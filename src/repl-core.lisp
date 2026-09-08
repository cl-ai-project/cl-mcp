;;;; src/repl-core.lisp
;;;;
;;;; Pure REPL evaluation logic, independent of MCP tool definitions.
;;;; This module can be used by both the parent MCP server and child
;;;; worker processes.

(defpackage #:cl-mcp/src/repl-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread)
  (:import-from #:cl-mcp/src/utils/bounded-stream
                #:make-bounded-output-stream
                #:bounded-output-string)
  (:import-from #:cl-mcp/src/frame-inspector #:capture-error-context)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:repl-eval #:*default-eval-package* #:*default-max-output-length*))

(in-package #:cl-mcp/src/repl-core)

(defparameter *default-eval-package* (find-package :cl-user)
  "Default package in which `repl-eval` evaluates forms.")

(defvar *default-max-output-length* 50000
  "Default maximum characters for repl-eval output when not specified by caller.
Prevents unbounded output from consuming excessive memory or bandwidth.")

(declaim (inline %read-all))

(defun %read-all (string allow-read-eval)
  "Read all top-level forms from STRING and return them as a list.
Uses a gensym sentinel to avoid collision with user input of :eof."
  (let ((*readtable* (copy-readtable))
        (*read-eval* allow-read-eval)
        (eof-sentinel (gensym "EOF")))
    (with-input-from-string (in string)
      (loop for form = (read in nil eof-sentinel)
            until (eq form eof-sentinel)
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
  "Truncate STRING to MAX-OUTPUT-LENGTH then sanitize.
Truncating first avoids sanitizing data that will be discarded (5x speedup
on large outputs)."
  (if (and max-output-length
           (integerp max-output-length)
           (> (length string) max-output-length))
      (concatenate 'string
                   (%sanitize-control-chars (subseq string 0 max-output-length))
                   "...(truncated)")
      (%sanitize-control-chars string)))

(defun %make-capture-stream (max-output-length)
  "Return a stream for capturing evaluated code's output, bounded by
MAX-OUTPUT-LENGTH.

Bounded while writing rather than truncated afterwards.  A
STRING-OUTPUT-STREAM holds everything the form produced, so the limit governed
what was reported while the heap paid for the rest -- and the form here is
whatever the client sent.  Measured, `(dotimes (i 500000) (write-string ...))'
cost 313 MB of heap to report 50 KB of it, and under a 256 MB dynamic space it
died with HEAP-EXHAUSTED-ERROR while materialising the string, in a quarter of
a second.  The evaluation deadline is no protection at that speed.

Zero is a limit, not a missing one: max_output_length is declared (integer 0)
and asking for zero asks for the output to be suppressed, which the stream
does exactly.  Only a value that is not a usable limit at all falls back to
the default."
  (make-bounded-output-stream (if (and (integerp max-output-length)
                                       (not (minusp max-output-length)))
                                  max-output-length
                                  *default-max-output-length*)))

(defun %captured-output (stream)
  "Drain STREAM's bounded capture and sanitize it for JSON.

The bounding already happened on the way in, so only the sanitizing half of
%TRUNCATE-OUTPUT is left to do here.  Sanitizing runs on the retained text
alone, before the stream appends its note: capture cut mid-escape-sequence
ends in an introducer whose terminator was dropped, and SANITIZE-FOR-JSON then
consumes everything after it -- which, applied to the composed string, is the
note saying output went missing.  The client would be handed silently
shortened output with nothing to say so.

The note the stream appends differs from %TRUNCATE-OUTPUT's \"...(truncated)\":
the stream counted what it discarded and says how much, which is worth more to
a caller than knowing only that something was lost."
  (bounded-output-string stream :transform #'%sanitize-control-chars))

(define-condition %package-not-found-error (package-error)
  ()
  (:report (lambda (c stream)
             (format stream "Package ~S does not exist"
                     (package-error-package c)))))

(defun %resolve-eval-package (package)
  (let ((pkg (etypecase package
               (package package)
               (symbol (find-package package))
               (string (find-package (string-upcase package))))))
    (unless pkg
      (error '%package-not-found-error :package package))
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
          (with-compilation-unit (:override t
                                  :source-namestring "repl-eval")
            (funcall thunk)))
        (with-compilation-unit (:override t
                                :source-namestring "repl-eval")
          (funcall thunk))))
  #-sbcl
  (funcall thunk))

(defun %eval-forms (forms package stdout stderr safe-read)
  (let ((last-value nil))
    (let ((*package* package)
          (*read-eval* (not safe-read))
          (*print-readably* nil)
          ;; Rebind printer/reader variables to safe defaults so user code
          ;; like (setf *print-base* 16) does not corrupt JSON serialization
          ;; in the worker's response path.
          (*print-base* 10)
          (*print-radix* nil)
          (*print-case* :upcase)
          (*print-circle* t)
          (*print-escape* t)
          (*print-gensym* t)
          (*print-array* t)
          (*print-pretty* t)
          (*read-default-float-format* 'single-float))
      (let* ((interactive
               (make-two-way-stream (make-concatenated-stream) stdout))
             (*standard-output* stdout)
             (*error-output* stderr)
             ;; log4cl's console appender writes to a synonym stream for
             ;; *DEBUG-IO*, which in a worker resolves to the original stdout
             ;; fd whose read end the parent closed after the handshake --
             ;; any write there raises BROKEN-PIPE.  Rebinding these keeps
             ;; log4cl / interactive output captured instead of hitting the
             ;; dead pipe.
             ;;
             ;; A two-way stream rather than STDOUT itself: ANSI requires
             ;; these three to be bidirectional, and code that reads from
             ;; them -- a stray Y-OR-N-P in evaluated code, say -- would
             ;; otherwise get "not an input stream" rather than the empty
             ;; input it should see in a non-interactive process.
             (*debug-io* interactive)
             (*terminal-io* interactive)
             (*query-io* interactive)
             ;; (TIME ...) and TRACE write here, and its default is a synonym
             ;; for the process's stdout.  Captured rather than redirected, so
             ;; a caller who asks for timings gets them back in the result.
             (*trace-output* stdout)
             (*compile-verbose* nil)
             (*compile-print* nil))
        (%call-with-compiler-streams
         stdout
         stderr
         (lambda ()
           (dolist (form forms)
             (setf last-value (eval form)))))))
    last-value))

(defun %live-package-p (package)
  "Return true when PACKAGE is a package object that has not been deleted.
`delete-package` keeps the object's identity but sets its name to NIL, so a
NIL `package-name` is the reliable liveness test."
  (and (packagep package)
       (package-name package)
       t))

(defun %safe-prin1-to-string (value)
  "Print VALUE with `prin1-to-string`, degrading to a placeholder on any error.
The result-printing block of `%do-repl-eval` sits outside its HANDLER-BIND, so a
printer error signalled here would otherwise escape `repl-eval` entirely."
  (handler-case (prin1-to-string value)
    (serious-condition ()
      (or (ignore-errors (format nil "#<unprintable ~A>" (type-of value)))
          "#<unprintable>"))))

(defun %do-repl-eval (input package safe-read print-level print-length max-output-length
                      &key locals-preview-frames locals-preview-max-depth
                           locals-preview-max-elements locals-preview-skip-internal)
  "Evaluate INPUT and return (values printed raw-value stdout stderr error-context).
PRINTED is rendered relative to the resolved eval package, in lower case and
pretty-printed at a 100-column margin, so it reads as source.
ERROR-CONTEXT is a plist with structured error info when an error occurs, NIL otherwise."
  (let ((last-value nil)
        (error-context nil)
        (eval-package nil)
        (stdout (%make-capture-stream max-output-length))
        (stderr (%make-capture-stream max-output-length)))
    (handler-bind ((warning (lambda (w)
                              (format stderr "~&Warning: ~A~%" w)
                              (when (find-restart 'muffle-warning)
                                (invoke-restart 'muffle-warning))))
                   (package-error
                    (lambda (e)
                      (let* ((raw-msg (format nil "Package error: ~A" e))
                             (msg (%truncate-output raw-msg max-output-length)))
                        (return-from %do-repl-eval
                          (values msg msg
                                  (%captured-output stdout)
                                  (%captured-output stderr)
                                  (list :condition-type (princ-to-string (type-of e))
                                        :message msg
                                        :restarts nil
                                        :frames nil))))))
                   (reader-error
                    (lambda (e)
                      (let* ((raw-msg (format nil "Reader error: ~A" e))
                             (msg (%truncate-output raw-msg max-output-length)))
                        (return-from %do-repl-eval
                          (values msg msg
                                  (%captured-output stdout)
                                  (%captured-output stderr)
                                  (list :condition-type (princ-to-string (type-of e))
                                        :message msg
                                        :restarts nil
                                        :frames nil))))))
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
                            (setf last-value
                                  (let ((*print-readably* nil))
                                    (handler-case (format nil "~A" e)
                                      (error ()
                                        (format nil "<error formatting ~A>"
                                                (type-of e))))))
                            (return-from %do-repl-eval
                              (values (%truncate-output last-value max-output-length)
                                      last-value
                                      (%captured-output stdout)
                                      (%captured-output stderr)
                                      error-context)))))
      (let ((pkg (%resolve-eval-package package)))
        (setf eval-package pkg)
        (let ((forms (handler-case
                         (let ((*package* pkg)) (%read-all input (not safe-read)))
                       (end-of-file (e)
                         ;; Scope END-OF-FILE handling to the read phase only.
                         ;; Runtime EOF (e.g. reading from an empty stream in user
                         ;; code) must NOT be silently rewritten as an input error.
                         (declare (ignore e))
                         (let ((msg "Reader error: unexpected end of file -- check for unbalanced parentheses"))
                           (return-from %do-repl-eval
                             (values msg msg
                                     (%captured-output stdout)
                                     (%captured-output stderr)
                                     (list :condition-type "END-OF-FILE"
                                           :message msg
                                           :restarts nil
                                           :frames nil))))))))
          (setf last-value (%eval-forms forms pkg stdout stderr safe-read)))))
    ;; This block is a SIBLING of the HANDLER-BIND above, not nested inside
    ;; it, so PKG's binding has already unwound by the time we get here and
    ;; *PACKAGE* is whatever the caller had.  EVAL-PACKAGE carries the
    ;; resolved package across that boundary; without it every symbol prints
    ;; fully package-qualified relative to the caller.  Every error path
    ;; RETURN-FROMs before reaching here, so EVAL-PACKAGE is always set.
    ;; User code may have deleted it, though -- (delete-package :my-app) from
    ;; inside MY-APP -- and no handler is in scope here, so binding *PACKAGE*
    ;; to a dead package would let the printer signal straight out of
    ;; REPL-EVAL.  Fall back to the caller's package in that case.
    (let ((*package* (if (%live-package-p eval-package) eval-package *package*))
          (*print-level* print-level)
          (*print-length* print-length)
          (*print-readably* nil)
          (*print-case* :downcase)
          (*print-pretty* t)
          (*print-right-margin* 100)
          (*print-circle* t))
      (values (%truncate-output (%safe-prin1-to-string last-value) max-output-length)
              last-value
              (%captured-output stdout)
              (%captured-output stderr)
              nil))))

(defun %thunk-error-result (condition)
  "Build the five-element `repl-eval` result list describing CONDITION.
Shaped like the error returns of `%do-repl-eval`: printed value, raw value,
stdout, stderr, error-context."
  (let ((msg (or (ignore-errors (format nil "Evaluation error: ~A" condition))
                 "Evaluation error: <unprintable condition>"))
        (type-name (or (ignore-errors (princ-to-string (type-of condition)))
                       "SERIOUS-CONDITION")))
    (list msg msg "" ""
          (list :condition-type type-name
                :message msg
                :restarts nil
                :frames nil))))

(defun %repl-eval-with-timeout (thunk timeout-seconds)
  "Execute THUNK, enforcing TIMEOUT-SECONDS on a dedicated thread.
Without a usable deadline THUNK runs inline, exactly as before: there is
nothing to enforce, and the caller's handlers and backtrace stay intact.
If the evaluation completes while the deadline is being enforced, its real
result is returned -- completed work is never discarded as a timeout."
  (if (not (and timeout-seconds (realp timeout-seconds) (plusp timeout-seconds)))
      (funcall thunk)
      (multiple-value-bind (result status leaked)
          (call-with-deadline-thread
           (lambda ()
             ;; Nothing raised by THUNK -- evaluation *or* printing -- may
             ;; reach the debugger hook: a worker process runs under
             ;; SB-EXT:DISABLE-DEBUGGER, where an unhandled condition in this
             ;; thread aborts the whole process and destroys the session's
             ;; state.  Degrade to an error result instead.  The deadline
             ;; unwind is a THROW, not a condition, so this does not defeat it.
             (handler-case (funcall thunk)
               (serious-condition (e)
                 (ignore-errors
                  (cl-mcp/src/log:log-event
                   :warn "repl.eval.condition-escaped"
                   "type" (princ-to-string (type-of e))))
                 (values-list (%thunk-error-result e)))))
           timeout-seconds
           :name "mcp-repl-eval")
        (when leaked
          (ignore-errors
           (cl-mcp/src/log:log-event :warn "repl.timeout.thread-leaked"
                                     "name" "mcp-repl-eval"
                                     "timeout" timeout-seconds)))
        (ecase status
          (:ok (values-list result))
          ;; The wrapper above converts every SERIOUS-CONDITION, so :ERROR can
          ;; only mean the conversion itself failed.  Report it the same way.
          (:error (values-list (%thunk-error-result result)))
          (:timeout
           (values
            (if leaked
                ;; The evaluation is still running: it can still print, still
                ;; consume CPU, and still mutate this session's state under
                ;; later requests, so say so rather than implying it stopped.
                (format nil "Evaluation timed out after ~,2F seconds and ~
                             could not be stopped: it is still running in ~
                             this worker. Use pool-kill-worker to get a ~
                             fresh worker."
                        timeout-seconds)
                (format nil "Evaluation timed out after ~,2F seconds"
                        timeout-seconds))
            :timeout "" "" nil))))))

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
returned as a printed string per `prin1-to-string`, rendered relative to
PACKAGE in lower case and pretty-printed at a 100-column margin. The second return value is
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
