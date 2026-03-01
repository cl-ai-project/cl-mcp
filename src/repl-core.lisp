;;;; src/repl-core.lisp
;;;;
;;;; Pure REPL evaluation logic, independent of MCP tool definitions.
;;;; This module can be used by both the parent MCP server and child
;;;; worker processes.

(defpackage #:cl-mcp/src/repl-core
  (:use #:cl)
  (:import-from #:uiop #:print-backtrace)
  (:import-from #:bordeaux-threads
                #:thread-alive-p
                #:make-thread
                #:destroy-thread)
  (:import-from #:cl-mcp/src/frame-inspector #:capture-error-context)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:repl-eval #:*default-eval-package* #:*default-max-output-length*))

(in-package #:cl-mcp/src/repl-core)

(defparameter *default-eval-package* (find-package :cl-user)
  "Default package in which `repl-eval` evaluates forms.")

(defvar *default-max-output-length* 100000
  "Default maximum characters for repl-eval output when not specified by caller.
Prevents unbounded output from consuming excessive memory or bandwidth.")

(declaim (inline %read-all))

(defun %read-all (string allow-read-eval)
  "Read all top-level forms from STRING and return them as a list."
  (let ((*readtable* (copy-readtable))
        (*read-eval* allow-read-eval))
    (with-input-from-string (in string)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            collect form))))

(declaim (ftype (function (string
                           &key (:package (or package symbol string))
                                (:print-level (or null (integer 0)))
                                (:print-length (or null (integer 0)))
                                (:timeout-seconds (or null (real 0)))
                                (:max-output-length (or null (integer 0)))
                                (:safe-read (member t nil))
                                (:locals-preview-frames (or null (integer 0)))
                                (:locals-preview-max-depth (or null (integer 0)))
                                (:locals-preview-max-elements (or null (integer 0)))
                                (:locals-preview-skip-internal (member t nil)))
                           (values string t string string (or null list) &optional))
                 repl-eval))

(defun %sanitize-control-chars (string)
  "Remove control characters that are invalid in JSON strings.
Delegates to sanitize-for-json which also strips DEL (127)."
  (sanitize-for-json string))

(defun %truncate-output (string max-output-length)
  (let ((sanitized (%sanitize-control-chars string)))
    (if (and max-output-length
             (integerp max-output-length)
             (> (length sanitized) max-output-length))
        (concatenate 'string (subseq sanitized 0 max-output-length)
                     "...(truncated)")
        sanitized)))

(defun %resolve-eval-package (package)
  (let ((pkg (etypecase package
               (package package)
               (symbol (find-package package))
               (string (find-package package)))))
    (unless pkg
      (error "Package ~S does not exist" package))
    pkg))

(defun %call-with-compiler-streams (stdout stderr thunk)
  (declare (ignore stdout))
  #+sbcl
  (let ((err-sym (find-symbol "*COMPILER-ERROR-OUTPUT*" "SB-C"))
        (note-sym (find-symbol "*COMPILER-NOTE-STREAM*" "SB-C"))
        (trace-sym (find-symbol "*COMPILER-TRACE-OUTPUT*" "SB-C"))
        (syms '())
        (vals '()))
    ;; Route compiler errors/warnings and notes to STDERR.
    ;; Disable compiler trace output entirely, since it can include noisy diagnostics.
    (when err-sym
      (push err-sym syms)
      (push stderr vals))
    (when note-sym
      (push note-sym syms)
      (push stderr vals))
    (when trace-sym
      (push trace-sym syms)
      (push nil vals))
    (if syms
        (progv (nreverse syms) (nreverse vals)
          ;; Ensure warnings are emitted within this dynamic extent.
          (sb-ext::with-compilation-unit (:override t
                                          :source-namestring "repl-eval")
            (funcall thunk)))
        (sb-ext::with-compilation-unit (:override t
                                        :source-namestring "repl-eval")
          (funcall thunk))))
  #-sbcl
  (funcall thunk))

(defun %eval-forms (forms package stdout stderr safe-read)
  (let ((last-value nil))
    (let ((*package* package)
          (*read-eval* (not safe-read))
          (*print-readably* nil))
      (let ((*standard-output* stdout)
            (*error-output* stderr)
            (*compile-verbose* nil)
            (*compile-print* nil))
        (%call-with-compiler-streams
         stdout
         stderr
         (lambda ()
           (dolist (form forms)
             (setf last-value (eval form)))))))
    last-value))

(defun %do-repl-eval (input package safe-read print-level print-length max-output-length
                      &key locals-preview-frames locals-preview-max-depth
                           locals-preview-max-elements locals-preview-skip-internal)
  "Evaluate INPUT and return (values printed raw-value stdout stderr error-context).
ERROR-CONTEXT is a plist with structured error info when an error occurs, NIL otherwise."
  (let ((last-value nil)
        (error-context nil)
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (handler-bind ((warning (lambda (w)
                              (format stderr "~&Warning: ~A~%" w)
                              (when (find-restart 'muffle-warning)
                                (invoke-restart 'muffle-warning))))
                   (error (lambda (e)
                            ;; Capture structured error context
                            (setf error-context
                                  (capture-error-context e
                                                         :max-frames 20
                                                         :print-level (or print-level 3)
                                                         :print-length (or print-length 10)
                                                         :locals-preview-frames (or locals-preview-frames 0)
                                                         :preview-max-depth (or locals-preview-max-depth 1)
                                                         :preview-max-elements (or locals-preview-max-elements 5)
                                                         :locals-preview-skip-internal locals-preview-skip-internal))
                            ;; Also keep text representation for backward compatibility
                            (setf last-value
                                  (with-output-to-string (out)
                                    (let ((*print-readably* nil))
                                      (format out "~A~%" e)
                                      (uiop:print-backtrace :stream out
                                                            :condition e))))
                            (return-from %do-repl-eval
                              (values (%truncate-output last-value max-output-length)
                                      last-value
                                      (%truncate-output (get-output-stream-string stdout) max-output-length)
                                      (%truncate-output (get-output-stream-string stderr) max-output-length)
                                      error-context)))))
      (let ((pkg (%resolve-eval-package package)))
        (let ((forms (%read-all input (not safe-read))))
          (setf last-value (%eval-forms forms pkg stdout stderr safe-read)))))
    (let ((*print-level* print-level)
          (*print-length* print-length)
          (*print-readably* nil))
      (values (%truncate-output (prin1-to-string last-value) max-output-length)
              last-value
              (%truncate-output (get-output-stream-string stdout) max-output-length)
              (%truncate-output (get-output-stream-string stderr) max-output-length)
              nil))))

(defun %repl-eval-with-timeout (thunk timeout-seconds)
  "Execute THUNK with a polling-based timeout (50ms granularity).
If the worker completes during the final polling interval, returns
the result as success -- completed work is never discarded as a timeout."
  (if (and timeout-seconds (plusp timeout-seconds))
      (let* ((result-box nil)
             (worker (bordeaux-threads:make-thread
                      (lambda ()
                        (setf result-box (multiple-value-list (funcall thunk))))
                      :name "mcp-repl-eval")))
        (loop repeat (ceiling (/ timeout-seconds 0.05d0))
              when (not (bordeaux-threads:thread-alive-p worker))
              do (return-from %repl-eval-with-timeout (values-list result-box))
              do (sleep 0.05d0))
        ;; Worker may have completed during the last sleep window.
        ;; Re-check before declaring timeout.
        (cond
          ((not (bordeaux-threads:thread-alive-p worker))
           (values-list result-box))
          (t
           (ignore-errors (bordeaux-threads:destroy-thread worker))
           (values
            (format nil "Evaluation timed out after ~,2F seconds" timeout-seconds)
            :timeout "" "" nil))))
      (funcall thunk)))

(defun repl-eval (input &key (package *default-eval-package*)
                             (print-level nil) (print-length nil)
                             (timeout-seconds nil)
                             (max-output-length nil)
                             (safe-read nil)
                             (locals-preview-frames nil)
                             (locals-preview-max-depth nil)
                             (locals-preview-max-elements nil)
                             (locals-preview-skip-internal t))
  "Evaluate INPUT (a string of one or more s-expressions) in PACKAGE.

Forms are read as provided and evaluated sequentially; the last value is
returned as a printed string per `prin1-to-string`. The second return value is
the raw last value for callers that want it. The third and fourth values capture
stdout and stderr produced during evaluation. The fifth value is a structured
error context plist when an error occurred, NIL otherwise.

Options:
- TIMEOUT-SECONDS: abort evaluation after this many seconds, returning a timeout string.
- MAX-OUTPUT-LENGTH: truncate printed value/stdout/stderr to at most this many chars.
- SAFE-READ: when T, disables `*read-eval*` to block reader evaluation (#.).
- LOCALS-PREVIEW-FRAMES: number of top frames to include local variable previews (default: 0).
- LOCALS-PREVIEW-MAX-DEPTH: max nesting depth for local previews (default: 1).
- LOCALS-PREVIEW-MAX-ELEMENTS: max elements per collection in local previews (default: 5).
- LOCALS-PREVIEW-SKIP-INTERNAL: when T (default), skip internal frames when counting for preview."
  (let* ((effective-max-output-length
           (or max-output-length *default-max-output-length*))
         (thunk (lambda ()
                 (%do-repl-eval input
                                package
                                safe-read
                                print-level
                                print-length
                                effective-max-output-length
                                :locals-preview-frames locals-preview-frames
                                :locals-preview-max-depth locals-preview-max-depth
                                :locals-preview-max-elements locals-preview-max-elements
                                :locals-preview-skip-internal locals-preview-skip-internal))))
    (%repl-eval-with-timeout thunk timeout-seconds)))
