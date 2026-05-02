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
           #:make-load-failure-result
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

(defun %safe-test-name (test-name-fn node)
  "Call TEST-NAME-FN on NODE, falling back gracefully on error.
Some Rove result objects (e.g., FAILED-ASSERTION) lack a TEST-NAME method."
  (handler-case (funcall test-name-fn node)
    (error ()
      (let* ((pkg (find-package :rove/core/result))
             (desc-sym (find-symbol "ASSERTION-DESCRIPTION" pkg)))
        (or (and desc-sym
                 (ignore-errors (funcall (fdefinition desc-sym) node)))
            "<unknown test>")))))

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
              do (let ((test-name (%safe-test-name test-name-fn test-fail)))
                   (loop for testing-fail in (funcall test-failed-fn test-fail)
                         do (let ((testing-desc (%safe-test-name test-name-fn testing-fail))
                                   (assertions (%rove-extract-assertions testing-fail)))
                              (dolist (assertion assertions)
                                (setf (gethash "test_name" assertion)
                                      (format nil "~A / ~A" test-name testing-desc))
                                (push assertion results))))))
        ;; Suite mode: failed-tests contains FAILED-SUITE -> FAILED-TEST -> assertions
        (loop for suite-fail across failed-tests
              do (loop for test-fail in (funcall test-failed-fn suite-fail)
                       do (loop for testing-fail in (funcall test-failed-fn test-fail)
                                do (let ((test-name (%safe-test-name test-name-fn test-fail))
                                          (testing-desc (%safe-test-name test-name-fn testing-fail))
                                          (assertions (%rove-extract-assertions testing-fail)))
                                     (dolist (assertion assertions)
                                       (setf (gethash "test_name" assertion)
                                             (format nil "~A / ~A" test-name testing-desc))
                                       (push assertion results)))))))
    (nreverse results)))

(defun %normalize-tests-arg (tests)
  "Convert TESTS (list or array of strings/symbols) to a list of symbols."
  (mapcar (lambda (x)
            (if (stringp x)
                (let* ((pos (position #\: x))
                       (pkg-name (if pos (string-upcase (subseq x 0 pos)) "CL-USER"))
                       (sym-name (if pos (string-upcase (subseq x (1+ pos))) (string-upcase x)))
                       (pkg (find-package pkg-name)))
                  (if pkg
                      (multiple-value-bind (sym status) (find-symbol sym-name pkg)
                        (if status
                            sym
                            (error "Symbol ~A not found in package ~A" sym-name pkg-name)))
                      (error "Package ~A not found" pkg-name)))
                x))
          (if (vectorp tests) (coerce tests 'list) tests)))

(defun %coerce-test-symbol (test-name)
  "Coerce TEST-NAME (string or symbol) to a symbol."
  (if (stringp test-name)
      (first (%normalize-tests-arg (list test-name)))
      test-name))

(defun %resolve-framework (system-name framework-arg)
  "Resolve the test framework to use.
If FRAMEWORK-ARG is provided, use it. Otherwise detect from SYSTEM-NAME."
  (if (and framework-arg (not (string= (princ-to-string framework-arg) "")))
      (intern (string-upcase (princ-to-string framework-arg)) :keyword)
      (detect-test-framework system-name)))

(defun %format-load-error (system-name condition stderr)
  "Format a system load failure into a human-readable message."
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

(defun make-load-failure-result (system-name condition)
  "Convert a test-system load failure into a structured test-result so
RUN-TESTS returns a normal MCP response (failed:1 with a synthetic
SYSTEM-LOAD entry) instead of letting the condition propagate as an
opaque RPC-level error.  Mirrors the timeout pattern used by
%handle-run-tests in worker/handlers.lisp."
  (make-test-result
   :passed 0
   :failed 1
   :pending 0
   :framework :load-error
   :duration 0
   :failed-tests
   (vector
    (make-failure-detail
     :test-name "SYSTEM-LOAD"
     :description (format nil "Could not load test system ~A" system-name)
     :reason
     (format nil "~A~%~%Hint: the worker process may have a broken ~
                  package state. Use pool-kill-worker to get a fresh ~
                  worker, then retry run-tests."
             (or (ignore-errors (princ-to-string condition))
                 "unprintable condition"))))))

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

(defun %ensure-rove-test-name-method ()
  "Add a TEST-NAME method for FAILED-ASSERTION if Rove is loaded and the
method is missing.  Rove's TEST-NAME generic function has no method for
FAILED-ASSERTION, which causes NO-APPLICABLE-METHOD crashes inside
rove:run-tests when a deftest body contains bare (ok ...) assertions
without a (testing ...) wrapper.  This monkey-patch lets Rove's internal
iteration proceed instead of crashing."
  (let ((pkg (find-package :rove/core/result)))
    (when pkg
      (let ((test-name-gf (ignore-errors
                             (fdefinition (find-symbol "TEST-NAME" pkg))))
            (fa-class (find-class (find-symbol "FAILED-ASSERTION" pkg) nil))
            (desc-fn (ignore-errors
                       (fdefinition (find-symbol "ASSERTION-DESCRIPTION" pkg)))))
        (when (and test-name-gf fa-class desc-fn
                   (typep test-name-gf 'generic-function)
                   (null (ignore-errors
                           (find-method test-name-gf nil (list fa-class)))))
          (let ((method
                  (eval
                   `(defmethod ,(find-symbol "TEST-NAME" pkg)
                        ((obj ,(find-symbol "FAILED-ASSERTION" pkg)))
                      (or (ignore-errors (funcall ,desc-fn obj))
                          "<assertion>")))))
            (when method
              (log-event :info "test-runner"
                         "message" "added TEST-NAME method for FAILED-ASSERTION"))))))))

(defvar *load-error-tail-max-lines* 20)
(defvar *load-error-tail-max-chars* 2000)

(defun %tail-lines (string max-lines max-chars)
  "Return the last MAX-LINES or MAX-CHARS of STRING."
  (let* ((len (length string))
         (start (max 0 (- len max-chars)))
         (substring (subseq string start))
         (lines (uiop:split-string substring :separator '(#\Newline))))
    (format nil "~{~A~^~%~}"
            (last lines (min (length lines) max-lines)))))

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
                         (asdf:find-system system-name nil))))
                    ;; %find-rove-test-sub-systems already wraps its
                    ;; ASDF lookups in IGNORE-ERRORS internally.
                    (sub-systems (%find-rove-test-sub-systems system-name)))
                ;; Purge Rove suite registry BEFORE clearing the system,
                ;; so any deftest forms deleted from source since the
                ;; previous load do not linger as ghost tests.
                (ignore-errors (%rove-purge-ghost-suites system-name))
                (asdf:clear-system system-name)
                ;; Also clear test sub-systems so ASDF reloads them even
                ;; when source files are unchanged.  Without this, deftest
                ;; forms do not re-execute after ghost-suite purging and
                ;; Rove reports zero counts for those packages.
                (dolist (sub sub-systems)
                  (ignore-errors (asdf:clear-system sub)))
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

(defun run-rove-selected-tests (test-symbols &key (reload t))
  "Run Rove TEST-SYMBOLS and return structured results.
Wraps rove:run-tests with error handling for Rove bugs (e.g., direct assertions
without testing wrappers crash Rove's internals with NO-APPLICABLE-METHOD)."
  (%ensure-rove-test-name-method)
  (log-event :info "test-runner" "framework" "rove" "selected_tests"
             (format nil "~{~A~^, ~}" test-symbols) "reload" reload)
  (let* ((result-pkg (find-package :rove/core/result))
         (reporter-pkg (find-package :rove/reporter))
         (stats-pkg (find-package :rove/core/stats))
         (suite-pkg (find-package :rove/core/suite))
         (color-pkg (find-package :rove/misc/color))
         (rove-pkg (find-package :rove))
         (run-tests-fn (fdefinition (find-symbol "RUN-TESTS" rove-pkg)))
         (passed-tests-fn (fdefinition (find-symbol "PASSED-TESTS" result-pkg)))
         (failed-tests-fn (fdefinition (find-symbol "FAILED-TESTS" result-pkg)))
         (pending-tests-fn (fdefinition (find-symbol "PENDING-TESTS" result-pkg)))
         (report-stream-sym (find-symbol "*REPORT-STREAM*" reporter-pkg))
         (stats-sym (find-symbol "*STATS*" stats-pkg))
         (rove-stdout-sym (find-symbol "*ROVE-STANDARD-OUTPUT*" suite-pkg))
         (rove-stderr-sym (find-symbol "*ROVE-ERROR-OUTPUT*" suite-pkg))
         (last-report-sym (find-symbol "*LAST-SUITE-REPORT*" rove-pkg))
         (color-sym (find-symbol "*ENABLE-COLORS*" color-pkg))
         (with-context-sym (find-symbol "WITH-CONTEXT" stats-pkg))
         (_ (unless (and report-stream-sym stats-sym rove-stdout-sym rove-stderr-sym with-context-sym)
              (error "Rove internal symbols not found; incompatible Rove version?")))
         (start-time (get-internal-real-time))
         (stdout-stream (make-string-output-stream))
         (stderr-stream (make-string-output-stream))
         (debug-stream (make-string-output-stream))
         successp
         results
         rove-error)
    (declare (ignore successp _))
    (handler-case
        ;; Extreme isolation for nested Rove calls to fix the "Inception" bug.
        ;; rove/core/stats:with-context binds its first arg as a LET variable,
        ;; so we must pass a fresh gensym — passing NIL produces an illegal
        ;; (let ((nil ...)) ...) at compile time.
        (eval
         `(,(find-symbol "WITH-CONTEXT" stats-pkg) (,(gensym "ROVE-CTX"))
            (progv (list ',report-stream-sym
                         ',rove-stdout-sym
                         ',rove-stderr-sym
                         ',last-report-sym
                         ',color-sym)
                (list (make-broadcast-stream)
                      ,stdout-stream
                      ,stderr-stream
                      nil
                      nil)
              (let ((*standard-output* ,stdout-stream)
                    (*error-output* ,stderr-stream)
                    (*test-debug-output* ,debug-stream))
                (setf (values successp results)
                      (funcall ,run-tests-fn ',test-symbols))))))
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

(defun run-rove-single-test (test-name &key (reload t))
  "Run a single Rove test by name and return structured results.
TEST-NAME should be a fully qualified symbol name (e.g., 'pkg::test-name')."
  (run-rove-selected-tests
   (list (%coerce-test-symbol test-name))
   :reload reload))

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

(defun run-rove-tests (system-name &key (reload t))
  "Run tests using Rove and return structured results.
Uses rove:run to ensure any :around methods (e.g., test environment setup)
are invoked.  When the initial run returns zero counts (common with aggregate
test systems whose rove:run keyword does not map to registered suites),
detects test sub-systems from ASDF dependencies and runs each individually."
  (%ensure-rove-test-name-method)
  (log-event :info "test-runner" "framework" "rove" "system" system-name "reload" reload)
  (let* ((result-pkg (find-package :rove/core/result))
         (reporter-pkg (find-package :rove/reporter))
         (stats-pkg (find-package :rove/core/stats))
         (suite-pkg (find-package :rove/core/suite))
         (color-pkg (find-package :rove/misc/color))
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
         (stats-sym (find-symbol "*STATS*" stats-pkg))
         (rove-stdout-sym (find-symbol "*ROVE-STANDARD-OUTPUT*" suite-pkg))
         (rove-stderr-sym (find-symbol "*ROVE-ERROR-OUTPUT*" suite-pkg))
         (color-sym (find-symbol "*ENABLE-COLORS*" color-pkg))
         (with-context-sym (find-symbol "WITH-CONTEXT" stats-pkg))
         (pkgs-var (find-symbol "*PACKAGE-SUITES*"
                                :rove/core/suite/package))
         (_ (unless (and report-stream-sym last-report-sym stats-sym rove-stdout-sym rove-stderr-sym with-context-sym)
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
     ;; Deep isolation for nested Rove calls.  with-context binds its first
     ;; arg as a LET variable, so we pass a fresh gensym — NIL would produce
     ;; an illegal (let ((nil ...)) ...) at compile time.
     (eval
      ;; Note: do NOT progv-bind *last-suite-report* — rove:run writes into
      ;; it and the wrapper reads it after the eval completes.  A progv
      ;; binding would unwind on exit and discard rove's report.
      `(,(find-symbol "WITH-CONTEXT" stats-pkg) (,(gensym "ROVE-CTX"))
         (progv (list ',report-stream-sym
                      ',rove-stdout-sym
                      ',rove-stderr-sym
                      ',color-sym)
             (list (make-broadcast-stream)
                   ,stdout-stream
                   ,stderr-stream
                   nil)
           (let ((*standard-output* ,stdout-stream)
                 (*error-output* ,stderr-stream)
                 (*test-debug-output* ,debug-stream))
             (funcall ,run-fn
                      (intern (string-upcase ,system-name) :keyword))))))
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
                         (let ((test-name (%safe-test-name test-name-fn test-fail))
                               (assertions
                                (%rove-extract-assertions test-fail)))
                           (dolist (assertion assertions)
                             (setf (gethash "test_name" assertion)
                                     (princ-to-string test-name))
                             (push assertion failure-details))))))))
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
                              ;; progv: bind late-resolved Rove specials.
                              ;; gensym for with-context binding: NIL is illegal as a let var.
                              ;; See note above: do NOT progv-bind
                              ;; *last-suite-report*.  We read it after the
                              ;; eval to extract sub-system results.
                              (eval
                               `(,(find-symbol "WITH-CONTEXT" stats-pkg) (,(gensym "ROVE-CTX"))
                                  (progv (list ',report-stream-sym
                                               ',rove-stdout-sym
                                               ',rove-stderr-sym
                                               ',color-sym)
                                      (list (make-broadcast-stream)
                                            (make-broadcast-stream)
                                            (make-broadcast-stream)
                                            nil)
                                    (let ((*standard-output* (make-broadcast-stream))
                                          (*error-output* (make-broadcast-stream))
                                          (*test-debug-output* (make-broadcast-stream)))
                                      (funcall ,run-fn
                                               (intern (string-upcase ,sub-sys)
                                                       :keyword))))))
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

(defun run-tests (system-name &key framework test tests (reload t))
  "Run tests for SYSTEM-NAME using the specified or auto-detected FRAMEWORK.
If TEST is provided, run only that specific test.
If TESTS is provided, run those specific tests (array/list of fully qualified names).
If RELOAD is T (default), force-reload the system before running tests.
Returns a hash table with structured results.

If the implicit system-load step (%ENSURE-SYSTEM-LOADED) signals an
error, the failure is converted via MAKE-LOAD-FAILURE-RESULT so callers
always receive a structured hash.  Errors raised after the load step
(e.g., framework-internal errors) still propagate normally."
  (when (and test tests) (error "Specify either TEST or TESTS, not both"))
  ;; %ensure-system-loaded re-raises load failures via (error "~A" ...),
  ;; so simple-error is the precise contract.  Catching plain ERROR would
  ;; mask serious-condition subclasses or interactive-interrupt that
  ;; should bubble up as bugs rather than be silently bucketed into a
  ;; load-failure result.
  (when reload
    (handler-case (%ensure-system-loaded system-name)
      (simple-error (load-err)
        (return-from run-tests
          (make-load-failure-result system-name load-err)))))
  (let ((fw (%resolve-framework system-name framework))
        (selective-requested-p (or test tests)))
    (log-event :info "test-runner" "action" "run-tests" "system" system-name
               "framework" (string-downcase (symbol-name fw)) "test"
               (cond (test (princ-to-string test)) (tests "selected")
                     (t "all"))
               "reload" reload)
    (case fw
      (:rove
       (if (find-package :rove)
           (if selective-requested-p
               (let ((selected-tests
                      (if test
                          (list (%coerce-test-symbol test))
                          (%normalize-tests-arg tests))))
                 (run-rove-selected-tests selected-tests :reload reload))
               (run-rove-tests system-name :reload reload))
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
         (error
          "Selective test execution is currently supported only with Rove"))
       (log-event :warn "test-runner" "message"
                  "FiveAM support not yet implemented")
       (run-asdf-fallback system-name))
      (:prove
       (when selective-requested-p
         (error
          "Selective test execution is currently supported only with Rove"))
       (log-event :warn "test-runner" "message"
                  "Prove support not yet implemented")
       (run-asdf-fallback system-name))
      (t
       (when selective-requested-p
         (error
          "Selective test execution is currently supported only with Rove"))
       (run-asdf-fallback system-name)))))
