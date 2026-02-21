;;;; src/system-loader-core.lisp
;;;;
;;;; Core system loading logic, shared between parent and worker processes.

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
  (:export #:load-system))

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

(defun %call-with-suppressed-output (thunk)
  "Call THUNK with compilation and load output suppressed.
Returns (values thunk-result warning-count warning-details)."
  (let ((warning-count 0)
        (warning-details (make-string-output-stream))
        (stderr (make-string-output-stream)))
    #+sbcl
    (let ((err-sym (find-symbol "*COMPILER-ERROR-OUTPUT*" "SB-C"))
          (note-sym (find-symbol "*COMPILER-NOTE-STREAM*" "SB-C"))
          (trace-sym (find-symbol "*COMPILER-TRACE-OUTPUT*" "SB-C"))
          (syms nil)
          (vals nil))
      (when err-sym (push err-sym syms) (push stderr vals))
      (when note-sym (push note-sym syms) (push stderr vals))
      (when trace-sym (push trace-sym syms) (push nil vals))
      (let ((result
              (handler-bind
                  ((warning
                     (lambda (w)
                       (incf warning-count)
                       (format warning-details "~A~%" w)
                       (when (find-restart 'muffle-warning)
                         (invoke-restart 'muffle-warning)))))
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
                        (funcall thunk)))))))
        (values result warning-count
                (get-output-stream-string warning-details))))
    #-sbcl
    (let ((result
            (handler-bind
                ((warning
                   (lambda (w)
                     (incf warning-count)
                     (format warning-details "~A~%" w)
                     (when (find-restart 'muffle-warning)
                       (invoke-restart 'muffle-warning)))))
              (let ((*compile-verbose* nil)
                    (*compile-print* nil)
                    (*load-verbose* nil)
                    (*load-print* nil)
                    (*standard-output* (make-string-output-stream))
                    (*trace-output* (make-string-output-stream))
                    (*error-output* stderr))
                (funcall thunk)))))
      (values result warning-count
              (get-output-stream-string warning-details)))))

(declaim (ftype (function (string &key (:force boolean)
                                       (:clear-fasls boolean)
                                       (:timeout-seconds (or null (real (0)))))
                          (values hash-table &rest t))
                load-system))

(defun load-system (system-name &key (force t) (clear-fasls nil)
                                     (timeout-seconds 120))
  "Load ASDF system SYSTEM-NAME with structured result.

When FORCE is true (default), clears loaded state before loading so
changed files are picked up. When CLEAR-FASLS is true, forces full
recompilation from source. TIMEOUT-SECONDS must be a positive number
or NIL (no timeout). Default is 120 seconds."
  (check-type system-name string)
  (check-type timeout-seconds (or null (real (0))))
  (let ((start-time (get-internal-real-time)))
    (log-event :info "load-system"
               "system" system-name
               "force" force
               "clear_fasls" clear-fasls
               "timeout" timeout-seconds)
    (multiple-value-bind (result-list timed-out-p errored-p)
        (%load-with-timeout
         (lambda ()
           (when (and force (asdf:find-system system-name nil))
             (asdf:clear-system system-name))
           (%call-with-suppressed-output
            (lambda ()
              (if clear-fasls
                  (asdf:load-system system-name :force t)
                  (ql:quickload system-name :silent t)))))
         timeout-seconds)
      (let* ((elapsed-ms
               (round (* 1000 (/ (- (get-internal-real-time) start-time)
                                 internal-time-units-per-second))))
             (ht (make-ht "system" system-name)))
        (cond
          (timed-out-p
           (setf (gethash "status" ht) "timeout")
           (setf (gethash "duration_ms" ht) elapsed-ms)
           (setf (gethash "message" ht)
                 (format nil "Load timed out after ~,2F seconds"
                         timeout-seconds))
           (log-event :warn "load-system-timeout"
                      "system" system-name
                      "timeout" timeout-seconds))
          (errored-p
           (let ((err (first result-list)))
             (setf (gethash "status" ht) "error")
             (setf (gethash "duration_ms" ht) elapsed-ms)
             (setf (gethash "message" ht)
                   (sanitize-for-json
                    (or (ignore-errors (princ-to-string err))
                        (format nil "~A" (type-of err)))))
             (log-event :error "load-system-error"
                        "system" system-name
                        "error" (or (ignore-errors (princ-to-string err))
                                    "unprintable error"))))
          (t
           (destructuring-bind (load-result warning-count warning-details)
               result-list
             (declare (ignore load-result))
             (setf (gethash "status" ht) "loaded")
             (setf (gethash "duration_ms" ht) elapsed-ms)
             (setf (gethash "forced" ht) force)
             (setf (gethash "clear_fasls" ht) clear-fasls)
             (setf (gethash "warnings" ht) warning-count)
             (when (plusp warning-count)
               (setf (gethash "warning_details" ht)
                     (sanitize-for-json warning-details)))
             (log-event :info "load-system-complete"
                        "system" system-name
                        "duration_ms" elapsed-ms
                        "warnings" warning-count))))
        ht))))
