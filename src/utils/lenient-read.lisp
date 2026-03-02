;;;; src/utils/lenient-read.lisp
;;;;
;;;; Lenient package resolution for reader operations.
;;;; Creates ephemeral stub packages on demand when the reader encounters
;;;; unknown package-qualified symbols, and cleans them up afterward.

(defpackage #:cl-mcp/src/utils/lenient-read
  (:use #:cl)
  (:import-from #:eclector.reader
                #:package-does-not-exist
                #:symbol-does-not-exist)
  (:export #:call-with-lenient-packages))

(in-package #:cl-mcp/src/utils/lenient-read)

(defun %find-restart-by-name (name-string condition)
  "Find the first restart whose name matches NAME-STRING by string comparison.
Uses SYMBOL-NAME to avoid package-qualification issues with restart names
that may be internal to other packages (e.g., Eclector)."
  (dolist (r (compute-restarts condition))
    (when (and (restart-name r)
               (string= name-string
                        (symbol-name (restart-name r))))
      (return r))))

#+sbcl
(defun %find-restart-by-name-and-description (name-string desc-pattern
                                              condition)
  "Find a restart matching NAME-STRING whose description contains DESC-PATTERN.
Used for SBCL reader restarts where multiple restarts share the same name
but differ in scope (e.g., reader RETRY vs file-loader RETRY)."
  (dolist (r (compute-restarts condition))
    (when (and (restart-name r)
               (string= name-string
                        (symbol-name (restart-name r))))
      (let ((desc (format nil "~A" r)))
        (when (search desc-pattern desc :test #'char-equal)
          (return r))))))

(defun %handle-eclector-package-missing (condition stubs)
  "Handle Eclector's package-does-not-exist condition.
Creates a stub package and invokes the USE-PACKAGE restart."
  (let ((name (slot-value condition
                          'eclector.reader::%package-name)))
    (unless (find-package name)
      (push (make-package name :use nil) (car stubs)))
    (let ((r (%find-restart-by-name "USE-PACKAGE" condition)))
      (when r
        (invoke-restart r (find-package name))))))

(defun %handle-eclector-symbol-missing (condition)
  "Handle Eclector's symbol-does-not-exist condition.
Invokes the INTERN restart to create the symbol in the stub package."
  (let ((r (%find-restart-by-name "INTERN" condition)))
    (when r
      (invoke-restart r))))

#+sbcl
(defun %handle-sbcl-reader-package-error (condition stubs)
  "Handle SBCL's simple-reader-package-error condition.
Dispatches on the format control string to determine whether the error
is about a missing package or a missing symbol."
  (let* ((ctrl (simple-condition-format-control condition))
         (args (simple-condition-format-arguments condition)))
    (cond
      ;; "Package ~A does not exist."
      ((search "does not exist" ctrl :test #'char-equal)
       (let ((name (first args)))
         (when (stringp name)
           (unless (find-package name)
             (push (make-package name :use nil) (car stubs)))
           (let ((r (%find-restart-by-name-and-description
                     "RETRY" "finding" condition)))
             (when r (invoke-restart r))))))
      ;; "Symbol ~S not found in the ~A package."
      ((search "not found" ctrl :test #'char-equal)
       (let ((sym-name (first args))
             (pkg-name (second args)))
         (when (and (stringp sym-name) (stringp pkg-name))
           (let ((pkg (find-package pkg-name)))
             (when pkg
               (export (intern sym-name pkg) pkg)))
           (let ((r (%find-restart-by-name "CONTINUE" condition)))
             (when r (invoke-restart r)))))))))

(defun %cleanup-stub-packages (stubs)
  "Delete all stub packages, first uninterning their symbols.
Safe to call even if packages have already been deleted."
  (dolist (pkg stubs)
    (let ((name (ignore-errors (package-name pkg))))
      (when (and name (find-package name))
        (do-symbols (s pkg)
          (unintern s pkg))
        (delete-package pkg)))))

(defun call-with-lenient-packages (thunk)
  "Call THUNK with handler-bind that intercepts unknown-package conditions.

When the reader (Eclector or SBCL's CL reader) encounters a package-qualified
symbol whose package does not exist, this function creates an ephemeral stub
package so the read can proceed. After THUNK returns (or signals an error),
all stub packages are cleaned up via UNWIND-PROTECT.

Handles three condition types:
  1. ECLECTOR.READER:PACKAGE-DOES-NOT-EXIST -- invokes USE-PACKAGE restart
  2. ECLECTOR.READER:SYMBOL-DOES-NOT-EXIST -- invokes INTERN restart
  3. SB-INT:SIMPLE-READER-PACKAGE-ERROR (SBCL) -- creates stub, retries

Arguments:
  THUNK -- A function of zero arguments to call under lenient resolution.

Returns:
  The values returned by THUNK.

Examples:
  (call-with-lenient-packages
    (lambda ()
      (eclector.reader:read-from-string \"(unknown-pkg:sym)\")))
  => (UNKNOWN-PKG::SYM)"
  (let ((stubs-cell (list nil)))
    (unwind-protect
         (handler-bind
             ((package-does-not-exist
                (lambda (c)
                  (%handle-eclector-package-missing c stubs-cell)))
              (symbol-does-not-exist
                (lambda (c)
                  (%handle-eclector-symbol-missing c)))
              #+sbcl
              (sb-int:simple-reader-package-error
                (lambda (c)
                  (%handle-sbcl-reader-package-error c stubs-cell))))
           (funcall thunk))
      (%cleanup-stub-packages (car stubs-cell)))))
