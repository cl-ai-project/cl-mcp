;;;; src/test-runner.lisp --- Unified test runner with structured results

(defpackage #:cl-mcp/src/test-runner
  (:use #:cl)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:run-tests
           #:detect-test-framework))

(in-package #:cl-mcp/src/test-runner)

;;; ---------------------------------------------------------------------------
;;; Framework Detection
;;; ---------------------------------------------------------------------------

(defun detect-test-framework (system-name)
  "Detect which test framework SYSTEM-NAME uses.
Returns :ROVE, :FIVEAM, or :ASDF (fallback)."
  (declare (ignore system-name))
  ;; For now, detect by loaded packages
  ;; Future: inspect system dependencies
  (cond
    ((find-package :rove) :rove)
    ((find-package :fiveam) :fiveam)
    ((find-package :prove) :prove)
    (t :asdf)))

;;; ---------------------------------------------------------------------------
;;; Unified Result Format
;;; ---------------------------------------------------------------------------

(defun make-test-result (&key passed failed pending passed-tests failed-tests
                           framework duration)
  "Create a unified test result hash table."
  (let ((ht (make-ht "passed" passed
                     "failed" failed
                     "framework" (string-downcase (symbol-name framework)))))
    (when pending
      (setf (gethash "pending" ht) pending))
    (when passed-tests
      (setf (gethash "passed_tests" ht) (coerce passed-tests 'vector)))
    (when failed-tests
      (setf (gethash "failed_tests" ht) (coerce failed-tests 'vector)))
    (when duration
      (setf (gethash "duration_ms" ht) duration))
    ht))

(defun make-failure-detail (&key test-name description form values reason source)
  "Create a failure detail hash table."
  (let ((ht (make-ht "test_name" (if (stringp test-name)
                                     test-name
                                     (princ-to-string test-name)))))
    (when description
      (setf (gethash "description" ht) description))
    (when form
      (setf (gethash "form" ht) (princ-to-string form)))
    (when values
      (setf (gethash "values" ht)
            (coerce (mapcar #'princ-to-string values) 'vector)))
    (when reason
      (setf (gethash "reason" ht)
            (if (stringp reason)
                reason
                (princ-to-string reason))))
    (when source
      (setf (gethash "source" ht)
            (if (listp source)
                (make-ht "file" (first source)
                         "line" (second source))
                (princ-to-string source))))
    ht))

;;; ---------------------------------------------------------------------------
;;; Rove Backend
;;; ---------------------------------------------------------------------------

(defun %rove-extract-assertions (test-node)
  "Recursively extract failed assertions from a Rove test node."
  (let* ((pkg (find-package :rove/core/result))
         (test-failed-fn (fdefinition (find-symbol "TEST-FAILED-TESTS" pkg)))
         (assertion-form-fn (fdefinition (find-symbol "ASSERTION-FORM" pkg)))
         (assertion-desc-fn (fdefinition (find-symbol "ASSERTION-DESCRIPTION" pkg)))
         (assertion-reason-fn (fdefinition (find-symbol "ASSERTION-REASON" pkg)))
         (assertion-values-fn (fdefinition (find-symbol "ASSERTION-VALUES" pkg)))
         (assertion-source-fn (fdefinition (find-symbol "ASSERTION-SOURCE-LOCATION" pkg)))
         (failed-assertion-class (find-symbol "FAILED-ASSERTION" pkg))
         (children (funcall test-failed-fn test-node)))
    (loop for child in children
          if (typep child failed-assertion-class)
            collect (make-failure-detail
                     :form (funcall assertion-form-fn child)
                     :description (funcall assertion-desc-fn child)
                     :reason (funcall assertion-reason-fn child)
                     :values (funcall assertion-values-fn child)
                     :source (funcall assertion-source-fn child))
          else
            append (%rove-extract-assertions child))))

(defun %rove-extract-test-failures (stats &key single-test-p)
  "Extract all failure details from Rove stats.
When SINGLE-TEST-P is true, stats contain test results directly (no suite wrapper)."
  (let* ((pkg (find-package :rove/core/result))
         (stats-pkg (find-package :rove/core/stats))
         (test-name-fn (fdefinition (find-symbol "TEST-NAME" pkg)))
         (test-failed-fn (fdefinition (find-symbol "TEST-FAILED-TESTS" pkg)))
         (stats-failed-fn (fdefinition (find-symbol "STATS-FAILED-TESTS" stats-pkg)))
         (failed-tests (funcall stats-failed-fn stats))
         (results nil))
    (if single-test-p
        ;; Single test mode: failed-tests contains FAILED-TEST directly (deftest level)
        (loop for test-fail across failed-tests
              do (let ((test-name (funcall test-name-fn test-fail)))
                   (loop for testing-fail in (funcall test-failed-fn test-fail)
                         do (let* ((testing-desc (funcall test-name-fn testing-fail))
                                   (assertions (%rove-extract-assertions testing-fail)))
                              (dolist (assertion assertions)
                                (setf (gethash "test_name" assertion)
                                      (format nil "~A / ~A" test-name testing-desc))
                                (push assertion results))))))
        ;; Suite mode: failed-tests contains FAILED-SUITE → FAILED-TEST → assertions
        (loop for suite-fail across failed-tests
              do (loop for test-fail in (funcall test-failed-fn suite-fail)
                       do (loop for testing-fail in (funcall test-failed-fn test-fail)
                                do (let* ((test-name (funcall test-name-fn test-fail))
                                          (testing-desc (funcall test-name-fn testing-fail))
                                          (assertions (%rove-extract-assertions testing-fail)))
                                     (dolist (assertion assertions)
                                       (setf (gethash "test_name" assertion)
                                             (format nil "~A / ~A" test-name testing-desc))
                                       (push assertion results)))))))
    (nreverse results)))

(defun run-rove-single-test (test-name)
  "Run a single Rove test by name and return structured results.
TEST-NAME should be a fully qualified symbol name (e.g., 'pkg::test-name')."
  (log-event :info "test-runner" "framework" "rove" "test" (princ-to-string test-name))
  (let* ((suite-pkg (find-package :rove/core/suite))
         (stats-pkg (find-package :rove/core/stats))
         (reporter-pkg (find-package :rove/reporter))
         (get-test-fn (fdefinition (find-symbol "GET-TEST" suite-pkg)))
         (run-test-fns-fn (fdefinition (find-symbol "RUN-TEST-FUNCTIONS" suite-pkg)))
         (stats-passed-fn (fdefinition (find-symbol "STATS-PASSED-TESTS" stats-pkg)))
         (stats-failed-fn (fdefinition (find-symbol "STATS-FAILED-TESTS" stats-pkg)))
         (stats-pending-fn (fdefinition (find-symbol "STATS-PENDING-TESTS" stats-pkg)))
         ;; Dynamic symbol lookup to avoid compile-time package references
         (report-stream-sym (find-symbol "*REPORT-STREAM*" reporter-pkg))
         (stats-sym (find-symbol "*STATS*" stats-pkg))
         (with-reporter-sym (find-symbol "WITH-REPORTER" reporter-pkg))
         ;; Parse test name string to symbol
         (test-sym (if (symbolp test-name)
                       test-name
                       (let* ((name (string-upcase (string test-name)))
                              (colon-pos (search "::" name)))
                         (if colon-pos
                             (let ((pkg-name (subseq name 0 colon-pos))
                                   (sym-name (subseq name (+ colon-pos 2))))
                               (intern sym-name (find-package pkg-name)))
                             (error "Test name must be fully qualified (pkg::name): ~A" test-name)))))
         (test-fn (funcall get-test-fn test-sym))
         result-stats
         (start-time (get-internal-real-time)))
    (unless test-fn
      (error "No test found for ~A" test-sym))
    ;; Run test with structured result capture
    (setf result-stats
          (funcall
           (compile nil
                    `(lambda (run-fns-fn test-fn)
                       (let ((,report-stream-sym (make-broadcast-stream)))
                         (,with-reporter-sym :spec
                           (funcall run-fns-fn (list test-fn))
                           ,stats-sym))))
           run-test-fns-fn test-fn))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           (passed (length (funcall stats-passed-fn result-stats)))
           (failed (length (funcall stats-failed-fn result-stats)))
           (pending (length (funcall stats-pending-fn result-stats)))
           (failure-details (when (plusp failed)
                              (%rove-extract-test-failures result-stats
                                                           :single-test-p t))))
      (make-test-result :passed passed
                        :failed failed
                        :pending pending
                        :failed-tests failure-details
                        :framework :rove
                        :duration duration-ms))))
(defun run-rove-tests (system-name)
  "Run tests using Rove and return structured results."
  (log-event :info "test-runner" "framework" "rove" "system" system-name)
  (let* ((suite-pkg (find-package :rove/core/suite))
         (stats-pkg (find-package :rove/core/stats))
         (reporter-pkg (find-package :rove/reporter))
         (find-suite-fn (fdefinition (find-symbol "FIND-SUITE" suite-pkg)))
         (run-suite-fn (fdefinition (find-symbol "RUN-SUITE" suite-pkg)))
         (stats-passed-fn (fdefinition (find-symbol "STATS-PASSED-TESTS" stats-pkg)))
         (stats-failed-fn (fdefinition (find-symbol "STATS-FAILED-TESTS" stats-pkg)))
         (stats-pending-fn (fdefinition (find-symbol "STATS-PENDING-TESTS" stats-pkg)))
         ;; Dynamic symbol lookup to avoid compile-time package references
         (report-stream-sym (find-symbol "*REPORT-STREAM*" reporter-pkg))
         (stats-sym (find-symbol "*STATS*" stats-pkg))
         (with-reporter-sym (find-symbol "WITH-REPORTER" reporter-pkg))
         (suite (funcall find-suite-fn (intern (string-upcase system-name) :keyword)))
         result-stats
         (start-time (get-internal-real-time)))
    (unless suite
      (error "No test suite found for ~A" system-name))
    ;; Run with reporter and capture stats INSIDE the block
    ;; Use eval with dynamically looked-up symbols to avoid compile-time
    ;; package references (Rove may not be loaded when this file is compiled)
    (setf result-stats
          (funcall
           (compile nil
                    `(lambda (run-fn suite-obj)
                       (let ((,report-stream-sym (make-broadcast-stream)))
                         (,with-reporter-sym :spec
                           (funcall run-fn suite-obj)
                           ,stats-sym))))
           run-suite-fn suite))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           (passed (length (funcall stats-passed-fn result-stats)))
           (failed (length (funcall stats-failed-fn result-stats)))
           (pending (length (funcall stats-pending-fn result-stats)))
           (failure-details (when (plusp failed)
                              (%rove-extract-test-failures result-stats))))
      (make-test-result :passed passed
                        :failed failed
                        :pending pending
                        :failed-tests failure-details
                        :framework :rove
                        :duration duration-ms))))

;;; ---------------------------------------------------------------------------
;;; ASDF Fallback (text capture)
;;; ---------------------------------------------------------------------------

(defun run-asdf-fallback (system-name)
  "Run tests using asdf:test-system with text output capture."
  (log-event :info "test-runner" "framework" "asdf-fallback" "system" system-name)
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream))
        (start-time (get-internal-real-time))
        success)
    (handler-case
        (progn
          (let ((*standard-output* output)
                (*error-output* error-output))
            (asdf:test-system system-name))
          (setf success t))
      (error (c)
        (format error-output "~&Error: ~A~%" c)))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           (stdout (get-output-stream-string output))
           (stderr (get-output-stream-string error-output)))
      (let ((ht (make-ht "framework" "asdf"
                         "success" success
                         "duration_ms" duration-ms)))
        (when (plusp (length stdout))
          (setf (gethash "stdout" ht) stdout))
        (when (plusp (length stderr))
          (setf (gethash "stderr" ht) stderr))
        ht))))

;;; ---------------------------------------------------------------------------
;;; Main Entry Point
;;; ---------------------------------------------------------------------------

(defun run-tests (system-name &key framework test)
  "Run tests for SYSTEM-NAME using the specified or auto-detected FRAMEWORK.
If TEST is provided, run only that specific test (fully qualified symbol name).
Returns a hash table with structured results."
  (let ((fw (or (and framework (intern (string-upcase framework) :keyword))
                (detect-test-framework system-name))))
    (log-event :info "test-runner" "action" "run-tests" "system" system-name
               "framework" (string-downcase fw)
               "test" (or test "all"))
    (case fw
      (:rove
       (if (find-package :rove)
           (if test
               (run-rove-single-test test)
               (run-rove-tests system-name))
           (progn
             (log-event :warn "test-runner" "message"
                        "Rove not loaded, using ASDF fallback")
             (run-asdf-fallback system-name))))
      (:fiveam
       (log-event :warn "test-runner" "message"
                  "FiveAM support not yet implemented")
       (run-asdf-fallback system-name))
      (:prove
       (log-event :warn "test-runner" "message"
                  "Prove support not yet implemented")
       (run-asdf-fallback system-name))
      (t (run-asdf-fallback system-name)))))

;;; ---------------------------------------------------------------------------
;;; Tool Definition
;;; ---------------------------------------------------------------------------

(define-tool "run-tests"
  :description "Run tests for a system and return structured results.

Supports multiple test frameworks with automatic detection:
- Rove: Full structured results with failure details
- FiveAM: (planned) Structured results
- ASDF fallback: Text output capture

Returns:
- passed/failed counts
- failure details (test name, form, values, reason)
- execution duration

Examples:
  Run all tests: system='cl-mcp/tests/clhs-test'
  Run single test: system='cl-mcp/tests/clhs-test' test='cl-mcp/tests/clhs-test::clhs-lookup-symbol-returns-hash-table'"
  :args ((system :type :string :required t
                 :description "System name to test (e.g., 'my-project/tests')")
         (framework :type :string :required nil
                    :description "Force framework: 'rove', 'fiveam', or 'auto' (default: auto-detect)")
         (test :type :string :required nil
               :description "Run only this specific test (fully qualified: 'package::test-name')"))
  :body
  (let ((fw (when (and (boundp 'framework) framework)
              framework))
        (tst (when (and (boundp 'test) test)
               test)))
    (result id (run-tests system :framework fw :test tst))))
