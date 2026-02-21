;;;; src/test-runner-core.lisp
;;;;
;;;; Core test runner logic, shared between parent and worker processes.

(defpackage #:cl-mcp/src/test-runner-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:export #:run-tests
           #:detect-test-framework))

(in-package #:cl-mcp/src/test-runner-core)

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
  (let* ((normalized-failed-tests (if (vectorp failed-tests)
                                      failed-tests
                                      (coerce (or failed-tests '()) 'vector)))
         (ht (make-ht "passed" (or passed 0)
                      "failed" (or failed 0)
                      "framework" (string-downcase (symbol-name framework))
                      "failed_tests" normalized-failed-tests
                      "duration_ms" (or duration 0))))
    (when pending
      (setf (gethash "pending" ht) pending))
    (when passed-tests
      (setf (gethash "passed_tests" ht) (coerce passed-tests 'vector)))
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
        ;; Suite mode: failed-tests contains FAILED-SUITE -> FAILED-TEST -> assertions
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

(defun %coerce-test-symbol (test-name)
  "Convert TEST-NAME to a fully qualified test symbol."
  (cond
    ((null test-name)
     (error "Test name must not be NIL"))
    ((symbolp test-name)
     (unless (symbol-package test-name)
       (error "Test symbol must be package-qualified: ~S" test-name))
     test-name)
    ((stringp test-name)
     (let* ((name (string-upcase test-name))
            (colon-pos (search "::" name)))
       (unless colon-pos
         (error "Test name must be fully qualified (pkg::name): ~A" test-name))
       (let* ((pkg-name (subseq name 0 colon-pos))
              (sym-name (subseq name (+ colon-pos 2)))
              (pkg (find-package pkg-name)))
         (unless pkg
           (error "Test package not found: ~A" pkg-name))
         (intern sym-name pkg))))
    (t
     (error "Test name must be a string or symbol: ~S" test-name))))

(defun %normalize-tests-arg (tests)
  "Normalize TESTS to a non-empty list of fully-qualified test symbols."
  (let ((items (cond
                 ((null tests) nil)
                 ((vectorp tests) (coerce tests 'list))
                 ((listp tests) tests)
                 (t (error "tests must be an array of test names")))))
    (when (and tests (null items))
      (error "tests must contain at least one test name"))
    (mapcar #'%coerce-test-symbol items)))

(defun %resolve-framework (system-name framework)
  "Resolve FRAMEWORK argument to a backend keyword.
When FRAMEWORK is NIL or \"auto\", detect from SYSTEM-NAME."
  (cond
    ((null framework)
     (detect-test-framework system-name))
    ((and (stringp framework)
          (string-equal framework "auto"))
     (detect-test-framework system-name))
    ((stringp framework)
     (intern (string-upcase framework) :keyword))
    ((symbolp framework)
     (intern (string-upcase (symbol-name framework)) :keyword))
    (t
     (error "framework must be a string or symbol: ~S" framework))))

(defun %ensure-system-loaded (system-name)
  "Load SYSTEM-NAME to make test packages available for selective execution."
  (handler-case
      (asdf:load-system system-name)
    (error (c)
      (error "Failed to load test system ~A: ~A" system-name c))))

(defun %rove-extract-selected-failures (results)
  "Extract failure details from selected test RESULTS returned by rove:run-tests."
  (let* ((pkg (find-package :rove/core/result))
         (test-name-fn (fdefinition (find-symbol "TEST-NAME" pkg)))
         (test-failed-fn (fdefinition (find-symbol "TEST-FAILED-TESTS" pkg)))
         (failure-details nil))
    (dolist (test-result results)
      (let ((test-name (funcall test-name-fn test-result)))
        (dolist (testing-fail (funcall test-failed-fn test-result))
          (let* ((testing-desc (funcall test-name-fn testing-fail))
                 (assertions (%rove-extract-assertions testing-fail)))
            (dolist (assertion assertions)
              (setf (gethash "test_name" assertion)
                    (format nil "~A / ~A" test-name testing-desc))
              (push assertion failure-details))))))
    (nreverse failure-details)))

(defun run-rove-selected-tests (test-symbols)
  "Run Rove TEST-SYMBOLS and return structured results."
  (log-event :info "test-runner" "framework" "rove" "selected_tests"
             (format nil "~{~A~^, ~}" test-symbols))
  (let* ((result-pkg (find-package :rove/core/result))
         (reporter-pkg (find-package :rove/reporter))
         (rove-pkg (find-package :rove))
         (run-tests-fn (fdefinition (find-symbol "RUN-TESTS" rove-pkg)))
         (passed-tests-fn (fdefinition (find-symbol "PASSED-TESTS" result-pkg)))
         (failed-tests-fn (fdefinition (find-symbol "FAILED-TESTS" result-pkg)))
         (pending-tests-fn (fdefinition (find-symbol "PENDING-TESTS" result-pkg)))
         (report-stream-sym (find-symbol "*REPORT-STREAM*" reporter-pkg))
         (start-time (get-internal-real-time))
         successp
         results)
    (declare (ignore successp))
    (setf (values successp results)
          (funcall
           (compile nil
                    `(lambda (run-tests-fn tests)
                       (let ((,report-stream-sym (make-broadcast-stream))
                             (*standard-output* (make-broadcast-stream)))
                         (funcall run-tests-fn tests))))
           run-tests-fn test-symbols))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000
                                  (/ (- end-time start-time)
                                     internal-time-units-per-second))))
           (passed 0)
           (failed 0)
           (pending 0))
      (dolist (test-result results)
        (incf passed (length (funcall passed-tests-fn test-result)))
        (incf failed (length (funcall failed-tests-fn test-result)))
        (incf pending (length (funcall pending-tests-fn test-result))))
      (make-test-result :passed passed
                        :failed failed
                        :pending pending
                        :failed-tests (when (plusp failed)
                                        (%rove-extract-selected-failures results))
                        :framework :rove
                        :duration duration-ms))))

(defun run-rove-single-test (test-name)
  "Run a single Rove test by name and return structured results.
TEST-NAME should be a fully qualified symbol name (e.g., 'pkg::test-name')."
  (run-rove-selected-tests
   (list (%coerce-test-symbol test-name))))

(defun run-rove-tests (system-name)
  "Run tests using Rove and return structured results.
Uses rove:run to ensure any :around methods (e.g., test environment setup) are invoked."
  (log-event :info "test-runner" "framework" "rove" "system" system-name)
  (let* ((result-pkg (find-package :rove/core/result))
         (reporter-pkg (find-package :rove/reporter))
         (rove-pkg (find-package :rove))
         (run-fn (fdefinition (find-symbol "RUN" rove-pkg)))
         (passed-tests-fn (fdefinition (find-symbol "PASSED-TESTS" result-pkg)))
         (failed-tests-fn (fdefinition (find-symbol "FAILED-TESTS" result-pkg)))
         (pending-tests-fn (fdefinition (find-symbol "PENDING-TESTS" result-pkg)))
         (report-stream-sym (find-symbol "*REPORT-STREAM*" reporter-pkg))
         (last-report-sym (find-symbol "*LAST-SUITE-REPORT*" rove-pkg))
         (start-time (get-internal-real-time))
         successp results)
    ;; Run with suppressed output using rove:run (triggers :around methods)
    (setf (values successp results)
          (funcall
           (compile nil
                    `(lambda (run-fn system-key)
                       (let ((,report-stream-sym (make-broadcast-stream))
                             (*standard-output* (make-broadcast-stream)))
                         (funcall run-fn system-key))))
           run-fn (intern (string-upcase system-name) :keyword)))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           ;; Get stats from *last-suite-report*
           (suite-results (symbol-value last-report-sym))
           (passed 0)
           (failed 0)
           (pending 0)
           (failure-details nil))
      ;; Count passed/failed/pending from results
      ;; Structure: suite-results -> package-results -> deftest-results
      ;; We need to count at the deftest level (3rd level)
      (dolist (suite-result suite-results)
        ;; Level 2: package results (passed/failed packages)
        (dolist (pkg-result (funcall passed-tests-fn suite-result))
          ;; Level 3: deftest results
          (incf passed (length (funcall passed-tests-fn pkg-result)))
          (incf failed (length (funcall failed-tests-fn pkg-result)))
          (incf pending (length (funcall pending-tests-fn pkg-result))))
        ;; Also count from failed packages
        (dolist (pkg-result (funcall failed-tests-fn suite-result))
          (incf passed (length (funcall passed-tests-fn pkg-result)))
          (incf failed (length (funcall failed-tests-fn pkg-result)))
          (incf pending (length (funcall pending-tests-fn pkg-result)))))
      ;; Extract failure details if any
      (when (plusp failed)
        (dolist (suite-result suite-results)
          (dolist (pkg-result (append (funcall passed-tests-fn suite-result)
                                      (funcall failed-tests-fn suite-result)))
            (dolist (fail (funcall failed-tests-fn pkg-result))
              (push (make-failure-detail
                     :test-name (princ-to-string fail)
                     :reason "Test failed (see rove output for details)")
                    failure-details)))))
      (make-test-result :passed passed
                        :failed failed
                        :pending pending
                        :failed-tests (nreverse failure-details)
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
        (success nil)
        (condition-message nil))
    (handler-case
        (progn
          (let ((*standard-output* output)
                (*error-output* error-output))
            (asdf:test-system system-name))
          (setf success t))
      (error (c)
        (setf condition-message (princ-to-string c))
        (format error-output "~&Error: ~A~%" c)))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           (stdout (get-output-stream-string output))
           (stderr (get-output-stream-string error-output))
           (failure-reason (or condition-message
                               (and (plusp (length stderr)) stderr)
                               "asdf:test-system failed"))
           (failed-tests (if success
                             #()
                             (vector (make-failure-detail
                                      :test-name system-name
                                      :reason failure-reason))))
           (ht (make-ht "passed" 0
                        "failed" (if success 0 1)
                        "pending" 0
                        "framework" "asdf"
                        "duration_ms" duration-ms
                        "failed_tests" failed-tests
                        "success" success)))
      (when (plusp (length stdout))
        (setf (gethash "stdout" ht) stdout))
      (when (plusp (length stderr))
        (setf (gethash "stderr" ht) stderr))
      ht)))

;;; ---------------------------------------------------------------------------
;;; Main Entry Point
;;; ---------------------------------------------------------------------------

(defun run-tests (system-name &key framework test tests)
  "Run tests for SYSTEM-NAME using the specified or auto-detected FRAMEWORK.
If TEST is provided, run only that specific test.
If TESTS is provided, run those specific tests (array/list of fully qualified names).
Returns a hash table with structured results."
  (when (and test tests)
    (error "Specify either TEST or TESTS, not both"))
  (let* ((fw (%resolve-framework system-name framework))
         (selective-requested-p (or test tests)))
    (log-event :info "test-runner" "action" "run-tests" "system" system-name
               "framework" (string-downcase (symbol-name fw))
               "test" (cond
                         (test (princ-to-string test))
                         (tests "selected")
                         (t "all")))
    (case fw
      (:rove
       (when selective-requested-p
         (%ensure-system-loaded system-name))
       (if (find-package :rove)
           (if selective-requested-p
               (let ((selected-tests (if test
                                         (list (%coerce-test-symbol test))
                                         (%normalize-tests-arg tests))))
                 (run-rove-selected-tests selected-tests))
               (run-rove-tests system-name))
           (if selective-requested-p
               (error
                "Selective test execution with TEST/TESTS requires Rove for system ~A"
                system-name)
               (progn
                 (log-event :warn "test-runner" "message"
                            "Rove not loaded, using ASDF fallback")
                 (run-asdf-fallback system-name)))))
      (:fiveam
       (when selective-requested-p
         (error "Selective test execution is currently supported only with Rove"))
       (log-event :warn "test-runner" "message"
                  "FiveAM support not yet implemented")
       (run-asdf-fallback system-name))
      (:prove
       (when selective-requested-p
         (error "Selective test execution is currently supported only with Rove"))
       (log-event :warn "test-runner" "message"
                  "Prove support not yet implemented")
       (run-asdf-fallback system-name))
      (t
       (when selective-requested-p
         (error "Selective test execution is currently supported only with Rove"))
       (run-asdf-fallback system-name)))))
