;;;; src/system-loader-core.lisp
;;;;
;;;; Core system loading logic, shared between parent and worker processes.
;;;; Uses only ASDF (no Quicklisp dependency).

(defpackage #:cl-mcp/src/system-loader-core
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:thread-alive-p
                #:make-thread
                #:destroy-thread)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:export #:load-system
           #:*last-compiler-stderr*))

(in-package #:cl-mcp/src/system-loader-core)

(declaim (ftype (function (function (or number null)) (values t &rest t))
                %load-with-timeout))

(defun %load-with-timeout (thunk timeout-seconds)
  "Execute THUNK in a worker thread with TIMEOUT-SECONDS limit.
Returns (values result-list timed-out-p errored-p).
RESULT-LIST is a list of the thunk's multiple return values on success,
or a single-element list containing the error condition on failure.
TIMED-OUT-P is T if the worker was still running when the deadline passed.
ERRORED-P is T if the thunk signaled an error.

NOTE: This is a polling-based safety net, not a strict deadline enforcer.
The worker is polled every 50ms, so the effective granularity is 50ms.
If the worker completes during the final polling interval, the result is
returned as a success -- completed work is never discarded as a timeout."
  (if (and timeout-seconds (plusp timeout-seconds))
      (let* ((result-box nil)
             (error-box nil)
             (worker
               (make-thread
                (lambda ()
                  (handler-case
                      (setf result-box (multiple-value-list (funcall thunk)))
                    (error (c)
                      (setf error-box c))))
                :name "mcp-load-system")))
        (loop repeat (ceiling (/ timeout-seconds 0.05d0))
              when (not (thread-alive-p worker))
                do (return-from %load-with-timeout
                     (if error-box
                         (values (list error-box) nil t)
                         (values result-box nil nil)))
              do (sleep 0.05d0))
        ;; Worker may have completed during the last sleep window.
        ;; Re-check before declaring timeout.
        (cond
          ((not (thread-alive-p worker))
           (if error-box
               (values (list error-box) nil t)
               (values result-box nil nil)))
          (t
           (ignore-errors (destroy-thread worker))
           (values nil t nil))))
      (handler-case
          (values (multiple-value-list (funcall thunk)) nil nil)
        (error (c)
          (values (list c) nil t)))))

(defvar *last-compiler-stderr* nil
  "Captured compiler stderr from the most recent %call-with-suppressed-output call.
Always set via unwind-protect so it survives error unwinds.  When the call
completes normally this is set to NIL; on error it holds the stderr string
accumulated up to the point of failure.")

(defvar *auto-discovered-asd* nil
  "When non-NIL, holds the namestring of the .asd file that was auto-discovered
and registered during the most recent load-system call.  Bound dynamically so
that response builders can include an informational hint.")

(defun %redefinition-warning-p (warning)
  "Return T when WARNING is an SBCL \"redefining X in DEFUN/DEFMACRO/...\"
notification that is pure noise under force=true reloads. Uses the
condition class where available and falls back to a textual prefix match
on other implementations so the filter still works in portable images."
  (or #+sbcl
      (let ((cls (find-class 'sb-kernel:redefinition-warning nil)))
        (and cls (typep warning cls)))
      (let ((text (ignore-errors (princ-to-string warning))))
        (and (stringp text)
             (uiop:string-prefix-p "redefining " text)))))

(defun %discover-asd-in-project (system-name)
  "Search *project-root* for a .asd file matching SYSTEM-NAME.
For package-inferred subsystems like \"foo/tests\", searches for the
root system \"foo\" since .asd files are named after the root.
Returns the pathname of the shallowest match, or NIL if none found.
Wrapped in IGNORE-ERRORS for filesystem robustness."
  (when *project-root*
    (ignore-errors
     (let* ((root-name (string-downcase
                        (subseq system-name
                                0 (or (position #\/ system-name)
                                      (length system-name)))))
            (pattern (merge-pathnames
                      (make-pathname :directory '(:relative :wild-inferiors)
                                     :name root-name
                                     :type "asd")
                      *project-root*))
            (matches (directory pattern)))
       (when matches
         ;; Prefer shallowest path (closest to project root)
         (first
          (sort (copy-list matches)
                (lambda (a b)
                  (< (length (pathname-directory a))
                     (length (pathname-directory b)))))))))))

(defun %call-with-suppressed-output (thunk &key suppress-redefinition)
  "Call THUNK with compilation and load output suppressed.
Returns (values thunk-result warning-count warning-details compiler-stderr).
The stderr string is also saved to *last-compiler-stderr* via unwind-protect
so it survives error unwinds and can be retrieved by callers that catch the error.

When SUPPRESS-REDEFINITION is non-nil, warnings identified by
%REDEFINITION-WARNING-P are silently muffled and do not increment the
returned count.  Useful under force=true reloads where 'redefining X in
DEFUN' lines are noise that drown real warnings."
  (let ((warning-count 0)
        (warning-details (make-string-output-stream))
        (stderr (make-string-output-stream)))
    ;; Reset before each call so stale data from a previous run is not
    ;; mistakenly attributed to this invocation.
    (setf *last-compiler-stderr* nil)
    (flet ((handle-warning (w)
             (cond
               ((and suppress-redefinition (%redefinition-warning-p w))
                (when (find-restart 'muffle-warning)
                  (invoke-restart 'muffle-warning)))
               (t
                (incf warning-count)
                (format warning-details "~A~%" w)
                (when (find-restart 'muffle-warning)
                  (invoke-restart 'muffle-warning))))))
      #+sbcl
      (let ((err-sym (find-symbol "*COMPILER-ERROR-OUTPUT*" "SB-C"))
            (note-sym (find-symbol "*COMPILER-NOTE-STREAM*" "SB-C"))
            (trace-sym (find-symbol "*COMPILER-TRACE-OUTPUT*" "SB-C"))
            (syms nil)
            (vals nil))
        (when err-sym (push err-sym syms) (push stderr vals))
        (when note-sym (push note-sym syms) (push stderr vals))
        (when trace-sym (push trace-sym syms) (push nil vals))
        (let ((result nil)
              (completed-p nil))
          (unwind-protect
              (progn
                (setf result
                      (handler-bind ((warning #'handle-warning))
                        (let ((*compile-verbose* nil)
                              (*compile-print* nil)
                              (*load-verbose* nil)
                              (*load-print* nil)
                              (*standard-output* (make-string-output-stream))
                              (*trace-output* (make-string-output-stream))
                              (*error-output* stderr))
                          (if syms
                              (progv (nreverse syms) (nreverse vals)
                                (with-compilation-unit (:override t)
                                  (funcall thunk)))
                              (with-compilation-unit (:override t)
                                (funcall thunk))))))
                (setf completed-p t)
                (values result warning-count
                        (get-output-stream-string warning-details)
                        (get-output-stream-string stderr)))
            ;; Always capture stderr so it survives error unwind.
            (unless completed-p
              (setf *last-compiler-stderr*
                    (ignore-errors (get-output-stream-string stderr)))))))
      #-sbcl
      (let ((result nil)
            (completed-p nil))
        (unwind-protect
            (progn
              (setf result
                    (handler-bind ((warning #'handle-warning))
                      (let ((*compile-verbose* nil)
                            (*compile-print* nil)
                            (*load-verbose* nil)
                            (*load-print* nil)
                            (*standard-output* (make-string-output-stream))
                            (*trace-output* (make-string-output-stream))
                            (*error-output* stderr))
                        (funcall thunk))))
              (setf completed-p t)
              (values result warning-count
                      (get-output-stream-string warning-details)
                      (get-output-stream-string stderr)))
          ;; Always capture stderr so it survives error unwind.
          (unless completed-p
            (setf *last-compiler-stderr*
                  (ignore-errors (get-output-stream-string stderr)))))))))

(declaim (ftype (function (string &key (:force boolean)
                                       (:clear-fasls boolean)
                                       (:timeout-seconds (or null (real (0))))
                                       (:suppress-redefinition-warnings t))
                          (values hash-table &rest t))
                load-system))

(defun load-system
       (system-name &key (force t) (clear-fasls nil) (timeout-seconds 120)
                         (suppress-redefinition-warnings :auto))
  "Load ASDF system SYSTEM-NAME with structured result.

When FORCE is true (default), clears loaded state before loading so
changed files are picked up. When CLEAR-FASLS is true, forces full
recompilation from source. TIMEOUT-SECONDS must be a positive number
or NIL (no timeout). Default is 120 seconds.

SUPPRESS-REDEFINITION-WARNINGS controls whether SBCL
'redefining X in DEFUN' style notifications are dropped from the
captured warning stream.  Values:
  :auto  - suppress when FORCE is true (default, matches the
           force=true use case where redefinitions are expected).
  T      - always suppress.
  NIL    - never suppress (preserve pre-change behavior).

If ASDF signals MISSING-COMPONENT for the requested system, searches
*project-root* for a matching .asd file and retries once after
registering it."
  (check-type system-name string)
  (check-type timeout-seconds (or null (real (0))))
  ;; ASDF system names are canonically lowercase; normalize early so all
  ;; subsequent find-system / load-system / clear-system calls match.
  (let* ((system-name (string-downcase system-name))
         (start-time (get-internal-real-time))
         (suppress (cond
                     ((eq suppress-redefinition-warnings :auto) force)
                     (t suppress-redefinition-warnings))))
    (setf *auto-discovered-asd* nil)
    (log-event :info "load-system" "system" system-name "force" force
               "clear_fasls" clear-fasls "timeout" timeout-seconds
               "suppress_redefinition" suppress)
    (multiple-value-bind (result-list timed-out-p errored-p)
        (%load-with-timeout
         (lambda ()
           (flet ((%do-load ()
                    (when (and force (asdf:find-system system-name nil))
                      ;; Save .asd path before clearing: locally-registered
                      ;; systems cannot be rediscovered after clear-system
                      (let ((asd-src
                             (ignore-errors
                              (asdf:system-source-file
                               (asdf:find-system system-name nil)))))
                        (asdf:clear-system system-name)
                        ;; Re-read .asd so edits to :depends-on etc. are picked up
                        (when asd-src
                          (ignore-errors
                           (asdf:load-asd asd-src)))))
                    (%call-with-suppressed-output
                     (lambda ()
                       (asdf:load-system system-name
                                                 :force clear-fasls))
                     :suppress-redefinition suppress)))
             (handler-case (%do-load)
               (asdf/find-component:missing-component (c)
                 ;; Retry when the missing component is the system itself
                 ;; or its root (for package-inferred subsystems like "foo/tests")
                 (let* ((missing (princ-to-string
                                  (asdf/find-component:missing-requires c)))
                        (root-name (subseq system-name
                                           0 (or (position #\/ system-name)
                                                 (length system-name))))
                        (asd-path
                         (when (or (string-equal system-name missing)
                                   (string-equal root-name missing))
                           (%discover-asd-in-project system-name))))
                   (unless asd-path
                     (error c))
                   ;; Register discovered .asd and retry once
                   (log-event :info "load-system-auto-discover"
                              "system" system-name
                              "asd_path" (namestring asd-path))
                   (asdf/find-system:load-asd asd-path)
                   (setf *auto-discovered-asd* (namestring asd-path))
                   (%do-load))))))
         timeout-seconds)
      (let ((elapsed-ms
             (round
              (* 1000
                 (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second))))
            (ht (make-ht "system" system-name)))
        (cond
         (timed-out-p (setf (gethash "status" ht) "timeout")
          (setf (gethash "duration_ms" ht) elapsed-ms)
          (setf (gethash "message" ht)
                  (format nil "Load timed out after ~,2F seconds"
                          timeout-seconds))
          (log-event :warn "load-system-timeout" "system" system-name "timeout"
                     timeout-seconds))
         (errored-p
          (let ((err (first result-list))
                (compiler-stderr *last-compiler-stderr*))
            (setf (gethash "status" ht) "error")
            (setf (gethash "duration_ms" ht) elapsed-ms)
            (setf (gethash "message" ht)
                    (sanitize-for-json
                     (or (ignore-errors (princ-to-string err))
                         (format nil "~A" (type-of err)))))
            (when
                (and (stringp compiler-stderr)
                     (plusp (length compiler-stderr)))
              (setf (gethash "compiler_output" ht)
                      (sanitize-for-json compiler-stderr)))
            (log-event :error "load-system-error" "system" system-name "error"
                       (or (ignore-errors (princ-to-string err))
                           "unprintable error"))))
         (t
          (destructuring-bind
              (load-result warning-count warning-details
               &optional compiler-stderr)
              result-list
            (declare (ignore load-result compiler-stderr))
            (setf (gethash "status" ht) "loaded")
            (setf (gethash "duration_ms" ht) elapsed-ms)
            (setf (gethash "forced" ht) force)
            (setf (gethash "clear_fasls" ht) clear-fasls)
            (setf (gethash "warnings" ht) warning-count)
            (when (plusp warning-count)
              (setf (gethash "warning_details" ht)
                      (sanitize-for-json warning-details)))
            (log-event :info "load-system-complete" "system" system-name
                       "duration_ms" elapsed-ms "warnings" warning-count))))
        (when *auto-discovered-asd*
          (setf (gethash "auto_discovered_asd" ht) *auto-discovered-asd*))
        ht))))
