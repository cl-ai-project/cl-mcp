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
           #:detect-test-framework
           #:*test-debug-output*
           #:*max-test-output-length*))

(in-package #:cl-mcp/src/test-runner-core)

(defvar *test-debug-output* (make-broadcast-stream)
  "Stream for intentional debug output during test execution.
Test code can write to this stream via (format cl-mcp/src/test-runner-core:*test-debug-output* ...).
Output is captured and included in the run-tests response content text.
Outside of test execution, this is a broadcast-stream (output discarded).")

(defvar *max-test-output-length* 50000
  "Maximum characters for stdout/stderr captured during test execution.
Matches *default-max-output-length* from repl-core.")

(defun %truncate-test-output (string)
  "Truncate STRING to *max-test-output-length* if it exceeds the limit."
  (if (> (length string) *max-test-output-length*)
      (format nil "~A~%... (truncated, ~D total chars)"
              (subseq string 0 *max-test-output-length*)
              (length string))
      string))

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
                         do (let ((testing-desc (funcall test-name-fn testing-fail))
                                   (assertions (%rove-extract-assertions testing-fail)))
                              (dolist (assertion assertions)
                                (setf (gethash "test_name" assertion)
                                      (format nil "~A / ~A" test-name testing-desc))
                                (push assertion results))))))
        ;; Suite mode: failed-tests contains FAILED-SUITE -> FAILED-TEST -> assertions
        (loop for suite-fail across failed-tests
              do (loop for test-fail in (funcall test-failed-fn suite-fail)
                       do (loop for testing-fail in (funcall test-failed-fn test-fail)
                                do (let ((test-name (funcall test-name-fn test-fail))
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

(defparameter *load-error-tail-max-lines* 40
  "Maximum number of trailing stderr lines attached to a test-system load failure.")

(defparameter *load-error-tail-max-chars* 4000
  "Maximum total characters of trailing stderr attached to a test-system load failure.")

(defun %tail-lines (text max-lines max-chars)
  "Return the last MAX-LINES lines of TEXT, capped at MAX-CHARS characters.
Returns NIL when TEXT is empty or contains only whitespace. Used to
extract the most actionable portion of captured compiler output for
inclusion in load-failure error messages."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (when (plusp (length trimmed))
      (let* ((lines (uiop:split-string trimmed :separator '(#\Newline)))
             (n (length lines))
             (start (max 0 (- n max-lines)))
             (tail (subseq lines start))
             (joined (format nil "~{~A~^~%~}" tail)))
        (if (> (length joined) max-chars)
            (let ((cut (max 0 (- (length joined) max-chars))))
              (format nil "... (truncated)~%~A" (subseq joined cut)))
            joined)))))

(defun %format-load-error (system-name condition stderr)
  "Format a test-system load-failure message.
When STDERR contains captured compiler output, the tail of it is
appended under a 'Compiler output (most recent):' header so the user
can see the underlying reason (e.g., SBCL package-variance warnings)
instead of just an opaque COMPILE-FILE-ERROR.
Shows the condition type explicitly to aid root-cause diagnosis."
  (let ((tail (%tail-lines stderr *load-error-tail-max-lines*
                           *load-error-tail-max-chars*))
        (ctype (type-of condition))
        (cmsg (or (ignore-errors (princ-to-string condition))
                  "unprintable condition")))
    (if tail
        (format nil
                "Failed to load test system ~A:~%  [~A] ~A~%~%Compiler output (most recent):~%~A"
                system-name ctype cmsg tail)
        (format nil "Failed to load test system ~A:~%  [~A] ~A"
                system-name ctype cmsg))))

(defun %extract-defpackage-names-from-file (pathname)
  "Return a list of package names mentioned in `(defpackage ...)' forms
of the Lisp source file at PATHNAME. Silently returns NIL on any error."
  (handler-case
      (with-open-file (stream pathname :direction :input)
        (let ((*read-eval* nil)
              (*package* (find-package :cl-user))
              (names nil))
          (loop
           (let ((form (handler-case (read stream nil :eof)
                         (error () :eof))))
             (when (eq form :eof) (return))
             (when (and (consp form)
                        (symbolp (car form))
                        (or (string= (symbol-name (car form)) "DEFPACKAGE")
                            (string= (symbol-name (car form)) "DEFINE-PACKAGE"))
                        (consp (cdr form)))
               (let ((name-form (second form)))
                 (push
                  (cond
                    ((stringp name-form) name-form)
                    ((symbolp name-form) (symbol-name name-form))
                    (t nil))
                  names)))))
          (remove-if-not #'stringp (nreverse names))))
    (error () nil)))

(defun %rove-purge-ghost-suites (system-name)
  "Remove Rove suite entries for every test package associated with
SYSTEM-NAME. This prevents `ghost deftests' — tests that were deleted
from source on disk but are still remembered by Rove's in-image
registry and therefore keep running (and possibly failing) after a
reload.

Rove stores its per-package suite in a hash-table keyed by package
object: ROVE/CORE/SUITE/PACKAGE::*PACKAGE-SUITES*. Clearing the entry
for a package makes Rove rebuild the suite from scratch on the next
deftest load, so forms that no longer exist in source disappear.

Two discovery strategies are combined to cover both
package-inferred-system and classic ASDF layouts:

  1. **Dependency names**: for package-inferred systems, every
     sub-system name like `foo/tests/bar-test' is also the package
     name it defines. Walk SYSTEM-NAME's transitive dependency list
     and clear packages matching each string name.
  2. **Component source files**: recursively walk the ASDF component
     tree under SYSTEM-NAME, read each source file, and clear
     packages named in its `(defpackage ...)' forms. Catches classic
     defsystems where the package name does not match any ASDF
     sub-system name."
  (let ((pkgs-var (find-symbol "*PACKAGE-SUITES*" :rove/core/suite/package)))
    (when (and pkgs-var (boundp pkgs-var)
               (hash-table-p (symbol-value pkgs-var)))
      (let ((suites (symbol-value pkgs-var))
            (visited-systems (make-hash-table :test #'equal))
            (visited-files (make-hash-table :test #'equal)))
        (labels ((clear-pkg (pkg-name)
                   (let ((pkg (and (stringp pkg-name) (find-package pkg-name))))
                     (when pkg (remhash pkg suites))))
                 (walk-components (component)
                   (when component
                     (cond
                       ((typep component 'asdf:cl-source-file)
                        (let ((path (ignore-errors
                                      (asdf:component-pathname component))))
                          (when (and path
                                     (not (gethash (namestring path)
                                                   visited-files))
                                     (probe-file path))
                            (setf (gethash (namestring path) visited-files) t)
                            (dolist (pkg-name
                                     (%extract-defpackage-names-from-file path))
                              (clear-pkg pkg-name)))))
                       ((ignore-errors (asdf:component-children component))
                        (dolist (child (asdf:component-children component))
                          (walk-components child))))))
                 (walk-system (name)
                   (when (and (stringp name)
                              (not (gethash name visited-systems)))
                     (setf (gethash name visited-systems) t)
                     (clear-pkg name)
                     (let ((sys (ignore-errors (asdf:find-system name nil))))
                       (when sys
                         (walk-components sys)
                         (dolist (dep (ignore-errors
                                        (asdf:system-depends-on sys)))
                           (cond
                             ((stringp dep) (walk-system dep))
                             ((and (consp dep) (stringp (second dep)))
                              (walk-system (second dep))))))))))
          (walk-system system-name))))))

(defun %ensure-system-loaded (system-name)
  "Force-reload SYSTEM-NAME so tests always run against the latest source.
Clears ASDF's loaded state for the system, then reloads.  This ensures
that files edited on disk (e.g., via lisp-edit-form in the parent process)
are recompiled before tests execute.  Dependencies whose source files
have not changed are skipped by ASDF's timestamp check (negligible overhead).

Captures compile-time diagnostics during the load phase so that on
failure the tail of the captured output is included in the error
message. Two complementary capture paths are used:
  1. HANDLER-BIND on WARNING collects individual warning conditions as
     they fire.
  2. The ASDF:LOAD-SYSTEM call runs inside WITH-COMPILATION-UNIT
     :OVERRIDE T with *ERROR-OUTPUT* bound to a string stream, so
     SBCL's compilation-unit summary (which enumerates caught fatal
     errors and non-fatal warnings) fires into our capture before
     exiting the scope.
Package-variance warnings and 'caught N ERROR condition' summaries
surface to the caller instead of being swallowed into an opaque
COMPILE-FILE-ERROR."
  (let ((captured-warnings (make-string-output-stream))
        (captured-stderr (make-string-output-stream)))
    (handler-case
        (handler-bind
            ((warning
              (lambda (c)
                (format captured-warnings "~A~%" c))))
          (progn
            (when (asdf:find-system system-name nil)
              (let ((asd-src
                      (ignore-errors
                        (asdf:system-source-file
                         (asdf:find-system system-name nil)))))
                ;; Purge Rove suite registry BEFORE clearing the system,
                ;; so any deftest forms deleted from source since the
                ;; previous load do not linger as ghost tests.
                (ignore-errors (%rove-purge-ghost-suites system-name))
                (asdf:clear-system system-name)
                ;; Always re-read the .asd so edits to :depends-on
                ;; and other system metadata are picked up.
                (when asd-src
                  (ignore-errors (asdf:load-asd asd-src)))))
            (let ((*error-output* captured-stderr))
              (with-compilation-unit (:override t)
                (asdf:load-system system-name)))))
      (error (c)
        (let* ((warnings-text (get-output-stream-string captured-warnings))
               (stderr-text (get-output-stream-string captured-stderr))
               (combined (concatenate 'string warnings-text stderr-text)))
          (error "~A"
                 (%format-load-error system-name c combined)))))))

(defun %rove-extract-selected-failures (results)
  "Extract failure details from selected test RESULTS returned by rove:run-tests.
Handles test nodes (with TEST-NAME) containing (testing ...) blocks.
Also includes defensive handling for bare FAILED-ASSERTION objects in case
a future Rove version returns them directly instead of crashing."
  (let* ((pkg (find-package :rove/core/result))
         (test-name-fn (fdefinition (find-symbol "TEST-NAME" pkg)))
         (failed-assertion-class (find-symbol "FAILED-ASSERTION" pkg))
         (assertion-form-fn (fdefinition (find-symbol "ASSERTION-FORM" pkg)))
         (assertion-desc-fn (fdefinition (find-symbol "ASSERTION-DESCRIPTION" pkg)))
         (assertion-reason-fn (fdefinition (find-symbol "ASSERTION-REASON" pkg)))
         (assertion-values-fn (fdefinition (find-symbol "ASSERTION-VALUES" pkg)))
         (assertion-source-fn (fdefinition (find-symbol "ASSERTION-SOURCE-LOCATION" pkg)))
         (failure-details nil))
    (dolist (test-result results)
      (if (typep test-result failed-assertion-class)
          ;; Direct assertion in deftest body (no testing wrapper)
          (push (make-failure-detail
                 :test-name (princ-to-string test-result)
                 :form (funcall assertion-form-fn test-result)
                 :description (funcall assertion-desc-fn test-result)
                 :reason (funcall assertion-reason-fn test-result)
                 :values (funcall assertion-values-fn test-result)
                 :source (funcall assertion-source-fn test-result))
                failure-details)
          ;; Test node — recurse via %rove-extract-assertions
          (let ((test-name (princ-to-string (funcall test-name-fn test-result)))
                (assertions (%rove-extract-assertions test-result)))
            (dolist (assertion assertions)
              (setf (gethash "test_name" assertion) test-name)
              (push assertion failure-details)))))
    (nreverse failure-details)))

(defun run-rove-selected-tests (test-symbols)
  "Run Rove TEST-SYMBOLS and return structured results.
Wraps rove:run-tests with error handling for Rove bugs (e.g., direct assertions
without testing wrappers crash Rove's internals with NO-APPLICABLE-METHOD)."
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
         (_ (unless report-stream-sym
              (error "Rove internal symbol *REPORT-STREAM* not found; incompatible Rove version?")))
         (start-time (get-internal-real-time))
         (stdout-stream (make-string-output-stream))
         (stderr-stream (make-string-output-stream))
         (debug-stream (make-string-output-stream))
         successp
         results
         rove-error)
    (declare (ignore successp _))
    (handler-case
        ;; progv: bind late-resolved Rove *REPORT-STREAM* to suppress output.
        ;; Cannot use regular let because the symbol is resolved at runtime.
        (setf (values successp results)
              (progv (list report-stream-sym)
                  (list (make-broadcast-stream))
                (let ((*standard-output* stdout-stream)
                      (*error-output* stderr-stream)
                      (*test-debug-output* debug-stream))
                  (funcall run-tests-fn test-symbols))))
      (error (c)
        (setf rove-error (princ-to-string c))
        (log-event :error "test-runner" "message"
                   "rove:run-tests crashed" "error" rove-error)))
    (let* ((end-time (get-internal-real-time))
           (duration-ms
            (round
             (* 1000
                (/ (- end-time start-time) internal-time-units-per-second))))
           (stdout (%truncate-test-output (get-output-stream-string stdout-stream)))
           (stderr (%truncate-test-output (get-output-stream-string stderr-stream)))
           (debug-output (get-output-stream-string debug-stream)))
      (if rove-error
          ;; Rove crashed (e.g., direct assertions without testing wrapper)
          (let ((ht (make-test-result
                     :passed 0 :failed (length test-symbols)
                     :failed-tests
                     (mapcar (lambda (sym)
                               (make-failure-detail
                                :test-name (princ-to-string sym)
                                :reason (format nil "Test runner crashed: ~A" rove-error)))
                             test-symbols)
                     :framework :rove :duration duration-ms)))
            (when (plusp (length stdout)) (setf (gethash "stdout" ht) stdout))
            (when (plusp (length stderr)) (setf (gethash "stderr" ht) stderr))
            (when (plusp (length debug-output))
              (setf (gethash "debug_output" ht) debug-output))
            ht)
          ;; Normal path
          (let ((passed 0) (failed 0) (pending 0))
            (dolist (test-result results)
              (incf passed (length (funcall passed-tests-fn test-result)))
              (incf failed (length (funcall failed-tests-fn test-result)))
              (incf pending (length (funcall pending-tests-fn test-result))))
            (let ((ht (make-test-result
                       :passed passed :failed failed :pending pending
                       :failed-tests
                       (when (plusp failed)
                         (%rove-extract-selected-failures results))
                       :framework :rove :duration duration-ms)))
              (when (plusp (length stdout)) (setf (gethash "stdout" ht) stdout))
              (when (plusp (length stderr)) (setf (gethash "stderr" ht) stderr))
              (when (plusp (length debug-output))
                (setf (gethash "debug_output" ht) debug-output))
              ht))))))

(defun run-rove-single-test (test-name)
  "Run a single Rove test by name and return structured results.
TEST-NAME should be a fully qualified symbol name (e.g., 'pkg::test-name')."
  (run-rove-selected-tests
   (list (%coerce-test-symbol test-name))))

(defun %find-rove-test-sub-systems (system-name)
  "Return test sub-system names from SYSTEM-NAME's ASDF :depends-on.
Filters string dependencies that have SYSTEM-NAME/ as a prefix.
Used to detect aggregate test systems (e.g., my-proj/tests) whose
sub-systems (my-proj/tests/foo-test) hold the actual Rove suites."
  (let* ((sys (ignore-errors (asdf:find-system system-name nil)))
         (deps (when sys
                 (ignore-errors (asdf:system-depends-on sys))))
         (prefix (concatenate 'string (string-downcase system-name) "/")))
    (remove-if-not (lambda (dep)
                     (and (stringp dep)
                          (uiop:string-prefix-p prefix dep)))
                   deps)))

(defun run-rove-tests (system-name)
  "Run tests using Rove and return structured results.
Uses rove:run to ensure any :around methods (e.g., test environment setup)
are invoked.  When the initial run returns zero counts (common with aggregate
test systems whose rove:run keyword does not map to registered suites),
detects test sub-systems from ASDF dependencies and runs each individually."
  (log-event :info "test-runner" "framework" "rove" "system" system-name)
  (let* ((result-pkg (find-package :rove/core/result))
         (reporter-pkg (find-package :rove/reporter))
         (rove-pkg (find-package :rove))
         (run-fn (fdefinition (find-symbol "RUN" rove-pkg)))
         (passed-tests-fn
          (fdefinition (find-symbol "PASSED-TESTS" result-pkg)))
         (failed-tests-fn
          (fdefinition (find-symbol "FAILED-TESTS" result-pkg)))
         (pending-tests-fn
          (fdefinition (find-symbol "PENDING-TESTS" result-pkg)))
         (test-name-fn (fdefinition (find-symbol "TEST-NAME" result-pkg)))
         (report-stream-sym (find-symbol "*REPORT-STREAM*" reporter-pkg))
         (last-report-sym (find-symbol "*LAST-SUITE-REPORT*" rove-pkg))
         (pkgs-var (find-symbol "*PACKAGE-SUITES*"
                                :rove/core/suite/package))
         (_ (unless (and report-stream-sym last-report-sym)
              (error "Rove internal symbols not found (~A ~A); incompatible Rove version?"
                     report-stream-sym last-report-sym)))
         (start-time (get-internal-real-time))
         (stdout-stream (make-string-output-stream))
         (stderr-stream (make-string-output-stream))
         (debug-stream (make-string-output-stream))
         successp
         results
         rove-error)
    (declare (ignore successp results _))
    (handler-case
     ;; progv: bind late-resolved Rove *REPORT-STREAM* to suppress output.
     ;; Cannot use regular let because the symbol is resolved at runtime.
     (setf (values successp results)
             (progv (list report-stream-sym)
                 (list (make-broadcast-stream))
               (let ((*standard-output* stdout-stream)
                     (*error-output* stderr-stream)
                     (*test-debug-output* debug-stream))
                 (funcall run-fn
                          (intern (string-upcase system-name) :keyword)))))
     (error (c) (setf rove-error (princ-to-string c))
            (log-event :error "test-runner" "message" "rove:run crashed"
                       "error" rove-error)))
    (let* ((end-time (get-internal-real-time))
           (duration-ms
            (round
             (* 1000
                (/ (- end-time start-time) internal-time-units-per-second))))
           (stdout
            (%truncate-test-output (get-output-stream-string stdout-stream)))
           (stderr
            (%truncate-test-output (get-output-stream-string stderr-stream)))
           (debug-output (get-output-stream-string debug-stream)))
      (if rove-error
          (let ((ht
                 (make-test-result :passed 0 :failed 1 :failed-tests
                                   (list
                                    (make-failure-detail :test-name system-name
                                                         :reason
                                                         (format nil
                                                                 "Test runner crashed: ~A"
                                                                 rove-error)))
                                   :framework :rove :duration duration-ms)))
            (when (plusp (length stdout)) (setf (gethash "stdout" ht) stdout))
            (when (plusp (length stderr)) (setf (gethash "stderr" ht) stderr))
            (when (plusp (length debug-output))
              (setf (gethash "debug_output" ht) debug-output))
            ht)
          (let ((suite-results (symbol-value last-report-sym))
                (passed 0)
                (failed 0)
                (pending 0)
                (failure-details nil))
            (labels
                ((%extract-suites (suites)
                   "Extract counts and failure details from SUITES into
the surrounding passed/failed/pending/failure-details bindings."
                   (dolist (suite-result suites)
                     (dolist
                         (pkg-result (funcall passed-tests-fn suite-result))
                       (incf passed
                             (length (funcall passed-tests-fn pkg-result)))
                       (incf failed
                             (length (funcall failed-tests-fn pkg-result)))
                       (incf pending
                             (length (funcall pending-tests-fn pkg-result))))
                     (dolist
                         (pkg-result (funcall failed-tests-fn suite-result))
                       (incf passed
                             (length (funcall passed-tests-fn pkg-result)))
                       (incf failed
                             (length (funcall failed-tests-fn pkg-result)))
                       (incf pending
                             (length (funcall pending-tests-fn pkg-result))))
                     (dolist
                         (pkg-result
                          (append (funcall passed-tests-fn suite-result)
                                  (funcall failed-tests-fn suite-result)))
                       (dolist
                           (test-fail (funcall failed-tests-fn pkg-result))
                         (let ((test-name (funcall test-name-fn test-fail))
                               (assertions
                                (%rove-extract-assertions test-fail)))
                           (dolist (a assertions)
                             (setf (gethash "test_name" a)
                                     (princ-to-string test-name))
                             (push a failure-details))))))))
              (%extract-suites suite-results)
              ;; Fallback: aggregate test systems (e.g., with a custom
              ;; :perform test-op that calls rove:run on sub-packages)
              ;; report 0 counts because rove:run with the aggregate
              ;; keyword finds no registered suites.  Detect sub-systems
              ;; and run each individually.
              (when (and (zerop (+ passed failed pending))
                         (null rove-error))
                (let ((sub-systems
                       (%find-rove-test-sub-systems system-name)))
                  (when sub-systems
                    (log-event :info "test-runner" "message"
                               "zero counts from aggregate system, retrying sub-systems"
                               "count" (length sub-systems))
                    (dolist (sub-sys sub-systems)
                      (let ((run-ok nil))
                        (handler-case
                            (progn
                              ;; Purge Rove suite for this sub-system so
                              ;; rove:run rebuilds from fresh deftests,
                              ;; producing a full 3-level result structure.
                              (when (and pkgs-var (boundp pkgs-var)
                                         (hash-table-p (symbol-value pkgs-var)))
                                (let ((pkg (find-package (string-upcase sub-sys))))
                                  (when pkg
                                    (remhash pkg (symbol-value pkgs-var)))))
                              ;; Clear ASDF loaded state so rove:run
                              ;; triggers a real reload of deftest forms.
                              (ignore-errors
                                (asdf/system-registry:clear-system sub-sys))
                              ;; progv: bind late-resolved Rove special
                              ;; (see M1 guard above for nil safety)
                              (progv (list report-stream-sym)
                                  (list (make-broadcast-stream))
                                (let ((*standard-output* (make-broadcast-stream))
                                      (*error-output* (make-broadcast-stream))
                                      (*test-debug-output* (make-broadcast-stream)))
                                  (funcall run-fn
                                           (intern (string-upcase sub-sys)
                                                   :keyword))))
                              (setf run-ok t))
                          (error (c)
                            (incf failed 1)
                            (push (make-failure-detail
                                   :test-name sub-sys
                                   :reason (format nil
                                                   "Sub-system crashed: ~A"
                                                   (princ-to-string c)))
                                  failure-details)
                            (log-event :warn "test-runner" "message"
                                       "sub-system test error"
                                       "sub-system" sub-sys
                                       "error"
                                       (princ-to-string c))))
                        ;; Extract outside handler-case so extraction
                        ;; bugs propagate instead of being swallowed.
                        (when run-ok
                          (%extract-suites
                           (symbol-value last-report-sym)))))))))
            (let ((ht
                   (make-test-result :passed passed :failed failed :pending
                                     pending :failed-tests
                                     (nreverse failure-details) :framework
                                     :rove :duration duration-ms)))
              (when (plusp (length stdout))
                (setf (gethash "stdout" ht) stdout))
              (when (plusp (length stderr))
                (setf (gethash "stderr" ht) stderr))
              (when (plusp (length debug-output))
                (setf (gethash "debug_output" ht) debug-output))
              ht))))))

;;; ---------------------------------------------------------------------------
;;; ASDF Fallback (text capture)
;;; ---------------------------------------------------------------------------

(defun run-asdf-fallback (system-name)
  "Run tests using asdf:test-system with text output capture."
  (log-event :info "test-runner" "framework" "asdf-fallback" "system" system-name)
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream))
        (debug-stream (make-string-output-stream))
        (start-time (get-internal-real-time))
        (success nil)
        (condition-message nil))
    (handler-case
        (progn
          (let ((*standard-output* output)
                (*error-output* error-output)
                (*test-debug-output* debug-stream))
            (asdf:test-system system-name))
          (setf success t))
      (error (c)
        (setf condition-message (princ-to-string c))
        (format error-output "~&Error: ~A~%" c)))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           (stdout (%truncate-test-output (get-output-stream-string output)))
           (stderr (%truncate-test-output (get-output-stream-string error-output)))
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
      (let ((debug-output (get-output-stream-string debug-stream)))
        (when (plusp (length debug-output))
          (setf (gethash "debug_output" ht) debug-output)))
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
  ;; Load the test system before framework detection so that framework
  ;; packages (e.g. :rove) are available for detect-test-framework.
  ;; Without this, a freshly started process without Rove pre-loaded
  ;; would fall back to the ASDF text-capture path that cannot report
  ;; individual test counts (always returns passed=0, failed=0).
  (%ensure-system-loaded system-name)
  (let ((fw (%resolve-framework system-name framework))
         (selective-requested-p (or test tests)))
    (log-event :info "test-runner" "action" "run-tests" "system" system-name
               "framework" (string-downcase (symbol-name fw))
               "test" (cond
                         (test (princ-to-string test))
                         (tests "selected")
                         (t "all")))
    (case fw
      (:rove
       ;; System already force-reloaded above; no second load needed.
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
