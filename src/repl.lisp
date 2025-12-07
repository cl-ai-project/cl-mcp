;;;; src/repl.lisp

(defpackage #:cl-mcp/src/repl
  (:use #:cl)
  (:import-from #:uiop #:print-backtrace)
  (:import-from #:bordeaux-threads #:thread-alive-p #:make-thread #:destroy-thread)
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
  (labels ((truncate-output (s)
             (if (and max-output-length
                      (integerp max-output-length)
                      (> (length s) max-output-length))
                 (concatenate 'string (subseq s 0 max-output-length) "...(truncated)")
                 s))
           (do-eval ()
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
                                      (return-from do-eval
                                        (values last-value
                                                last-value
                                                (get-output-stream-string stdout)
                                                (progn (write-string "" stderr)
                                                        (get-output-stream-string stderr)))))))
                 (let* ((pkg (etypecase package
                               (package package)
                               (symbol (find-package package))
                               (string (find-package package)))))
                   (unless pkg
                     (error "Package ~S does not exist" package))
                   (let ((*package* pkg)
                         (*read-eval* (not safe-read))
                         (*print-readably* nil))
                    (let ((forms (%read-all input (not safe-read))))
                       (let ((*standard-output* stdout)
                             (*error-output* stderr))
                         #+sbcl
                         (let* ((err-sym (find-symbol "*COMPILER-ERROR-OUTPUT*" "SB-C"))
                                (note-sym (find-symbol "*COMPILER-NOTE-STREAM*" "SB-C"))
                                (trace-sym (find-symbol "*COMPILER-TRACE-OUTPUT*" "SB-C"))
                                (old-err (and err-sym (boundp err-sym) (symbol-value err-sym)))
                                (old-note (and note-sym (boundp note-sym) (symbol-value note-sym)))
                                (old-trace (and trace-sym (boundp trace-sym) (symbol-value trace-sym))))
                           (unwind-protect
                                (progn
                                  (when (and err-sym (boundp err-sym))
                                    (setf (symbol-value err-sym) stderr))
                                  (when (and note-sym (boundp note-sym))
                                    (setf (symbol-value note-sym) stderr))
                                  (when (and trace-sym (boundp trace-sym))
                                    (setf (symbol-value trace-sym) stderr))
                                  (dolist (form forms)
                                    (setf last-value (eval form))))
                             (when (and err-sym (boundp err-sym))
                               (setf (symbol-value err-sym) old-err))
                             (when (and note-sym (boundp note-sym))
                               (setf (symbol-value note-sym) old-note))
                             (when (and trace-sym (boundp trace-sym))
                               (setf (symbol-value trace-sym) old-trace))))
                         #-sbcl
                         (dolist (form forms)
                           (setf last-value (eval form)))))))
               (let ((*print-level* print-level)
                     (*print-length* print-length)
                       (*print-readably* nil))
                   (values (truncate-output (prin1-to-string last-value))
                           last-value
                           (truncate-output (get-output-stream-string stdout))
                           (truncate-output (get-output-stream-string stderr))))))))
    (if (and timeout-seconds (plusp timeout-seconds))
        (let* ((result-box nil)
               (worker (bordeaux-threads:make-thread
                        (lambda ()
                          (setf result-box (multiple-value-list (do-eval))))
                        :name "mcp-repl-eval")))
          (loop repeat (ceiling (/ timeout-seconds 0.05))
                when (not (bordeaux-threads:thread-alive-p worker))
                  do (return-from repl-eval (values-list result-box))
                do (sleep 0.05))
          ;; timed out
          ;; Avoid destroying a thread that already exited while we were checking.
          (when (bordeaux-threads:thread-alive-p worker)
            (bordeaux-threads:destroy-thread worker))
          (values (format nil "Evaluation timed out after ~,2F seconds" timeout-seconds)
                  :timeout
                  ""
                  ""))
        (do-eval))))
