;;;; src/repl.lisp

(defpackage #:cl-mcp/src/repl
  (:use #:cl)
  (:import-from #:uiop #:print-backtrace)
  (:import-from #:bordeaux-threads #:thread-alive-p #:make-thread #:destroy-thread)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:repl-eval #:*default-eval-package*))

(in-package #:cl-mcp/src/repl)

(defparameter *default-eval-package* (find-package :cl-user)
  "Default package in which `repl-eval` evaluates forms.")

(declaim (inline %read-all))
(defun %read-all (string allow-read-eval)
  "Read all top-level forms from STRING and return them as a list."
  (let ((*readtable* (copy-readtable nil))
        (*read-eval* allow-read-eval))
    (with-input-from-string (in string)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            collect form))))

(declaim (ftype (function (string &key (:package (or package symbol string))
                                  (:print-level (or null (integer 0)))
                                  (:print-length (or null (integer 0)))
                                  (:timeout-seconds (or null (real 0)))
                                  (:max-output-length (or null (integer 0)))
                                  (:safe-read (member t nil)))
                          (values string t string string &optional))
                repl-eval))

(defun %truncate-output (string max-output-length)
  (if (and max-output-length
           (integerp max-output-length)
           (> (length string) max-output-length))
      (concatenate 'string (subseq string 0 max-output-length)
                   "...(truncated)")
      string))

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

(defun %do-repl-eval (input package safe-read print-level print-length max-output-length)
  (let ((last-value nil)
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (handler-bind ((warning (lambda (w)
                              (format stderr "~&Warning: ~A~%" w)
                              (when (find-restart 'muffle-warning)
                                (invoke-restart 'muffle-warning))))
                   (error (lambda (e)
                            (setf last-value
                                  (with-output-to-string (out)
                                    (let ((*print-readably* nil))
                                      (format out "~A~%" e)
                                      (uiop:print-backtrace :stream out
                                                            :condition e))))
                            (return-from %do-repl-eval
                              (values last-value
                                      last-value
                                      (get-output-stream-string stdout)
                                      (get-output-stream-string stderr))))))
      (let ((pkg (%resolve-eval-package package)))
        (let ((forms (%read-all input (not safe-read))))
          (setf last-value (%eval-forms forms pkg stdout stderr safe-read)))))
    (let ((*print-level* print-level)
          (*print-length* print-length)
          (*print-readably* nil))
      (values (%truncate-output (prin1-to-string last-value) max-output-length)
              last-value
              (%truncate-output (get-output-stream-string stdout) max-output-length)
              (%truncate-output (get-output-stream-string stderr) max-output-length)))))

(defun %repl-eval-with-timeout (thunk timeout-seconds)
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
        ;; timed out
        ;; Avoid destroying a thread that already exited while we were checking.
        (when (bordeaux-threads:thread-alive-p worker)
          (bordeaux-threads:destroy-thread worker))
        (values (format nil "Evaluation timed out after ~,2F seconds" timeout-seconds)
                :timeout
                ""
                ""))
      (funcall thunk)))

(defun repl-eval (input &key (package *default-eval-package*)
                             (print-level nil) (print-length nil)
                             (timeout-seconds nil)
                             (max-output-length nil)
                             (safe-read nil))
  "Evaluate INPUT (a string of one or more s-expressions) in PACKAGE.

Forms are read as provided and evaluated sequentially; the last value is
returned as a printed string per `prin1-to-string`. The second return value is
the raw last value for callers that want it. The third and fourth values capture
stdout and stderr produced during evaluation.

Options:
- TIMEOUT-SECONDS: abort evaluation after this many seconds, returning a timeout string.
- MAX-OUTPUT-LENGTH: truncate printed value/stdout/stderr to at most this many chars.
- SAFE-READ: when T, disables `*read-eval*` to block reader evaluation (#.)."
  (let ((thunk (lambda ()
                 (%do-repl-eval input
                                package
                                safe-read
                                print-level
                                print-length
                                max-output-length))))
    (%repl-eval-with-timeout thunk timeout-seconds)))

(define-tool "repl-eval"
  :description "Evaluate Common Lisp forms and return the last value as printed text.
Use this for testing, inspection, debugging, or loading systems (ql:quickload).
Provide an existing package (e.g., CL-USER) and set print_level/print_length when needed.
WARNING: Definitions created here are TRANSIENT and lost on server restart.
To modify code permanently, you MUST use 'lisp-edit-form' or 'fs-write-file'
to save changes to files."
  :args ((code :type :string :required t
               :description "Code string of one or more forms evaluated sequentially")
         (package :type :string
                  :description "Existing package name (e.g., CL-USER); forms are read/evaluated there")
         (print-level :type :integer :json-name "print_level"
                      :description "Integer to limit printed nesting depth (omit to print fully)")
         (print-length :type :integer :json-name "print_length"
                       :description "Integer to limit printed list length (omit to print fully)")
         (timeout-seconds :type :number :json-name "timeout_seconds"
                          :description "Seconds to wait before timing out evaluation")
         (max-output-length :type :integer :json-name "max_output_length"
                            :description "Maximum characters for printed result/stdout/stderr")
         (safe-read :type :boolean :json-name "safe_read"
                    :description "When true, disables #. reader evaluation for safety"))
  :body
  (multiple-value-bind (printed _ stdout stderr)
      (repl-eval code
                 :package (or package *package*)
                 :print-level print-level
                 :print-length print-length
                 :timeout-seconds timeout-seconds
                 :max-output-length max-output-length
                 :safe-read safe-read)
    (declare (ignore _))
    (result id
            (make-ht "content" (text-content printed)
                     "stdout" stdout
                     "stderr" stderr))))
