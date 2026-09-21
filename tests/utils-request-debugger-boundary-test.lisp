(defpackage #:cl-mcp/tests/utils-request-debugger-boundary-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:*request-debugger-boundary-active*
                #:call-with-request-debugger-boundary
                #:request-debugger-result-status
                #:request-debugger-result-values
                #:request-debugger-result-error
                #:request-debugger-deadline-interrupt
                #:request-debugger-escape-error-p
                #:request-debugger-escape-error-context
                #:request-debugger-escape-error-display-text))

(in-package #:cl-mcp/tests/utils-request-debugger-boundary-test)

(declaim (optimize (debug 3) (safety 3)))

(define-condition boundary-direct-condition (condition) ())

(define-condition boundary-simple-condition (simple-condition) ())

(defvar *report-invocations* 0)

(define-condition boundary-reentrant-report-condition (condition) ()
  (:report (lambda (condition stream)
             (declare (ignore condition stream))
             (incf *report-invocations*)
             (invoke-debugger (make-condition 'boundary-direct-condition)))))

(define-condition boundary-signalling-report-condition (condition) ()
  (:report (lambda (condition stream)
             (declare (ignore condition stream))
             (incf *report-invocations*)
             (signal 'boundary-direct-condition))))

(defun %boundary-result (thunk)
  (let ((*request-debugger-boundary-active* t))
    (call-with-request-debugger-boundary thunk)))

#+sbcl
(deftest direct-condition-becomes-a-debugger-outcome
  (let ((result (%boundary-result (lambda () (error 'boundary-direct-condition)))))
    (ok (eq :debugger (request-debugger-result-status result)))
    (ok (null (request-debugger-result-values result)))
    (let ((escape (request-debugger-result-error result)))
      (ok (request-debugger-escape-error-p escape))
      (ok (typep escape 'error))
      (ok (search "BOUNDARY-DIRECT-CONDITION"
                  (getf (request-debugger-escape-error-context escape)
                        :condition-type))))))

#+sbcl
(deftest simple-condition-becomes-a-debugger-outcome
  (let ((result (%boundary-result (lambda () (error 'boundary-simple-condition)))))
    (ok (eq :debugger (request-debugger-result-status result)))
    (ok (search "BOUNDARY-SIMPLE-CONDITION"
                (getf (request-debugger-escape-error-context
                       (request-debugger-result-error result))
                      :condition-type)))))

#+sbcl
(deftest explicit-invoke-debugger-becomes-a-debugger-outcome
  (let ((result (%boundary-result
                 (lambda ()
                   (invoke-debugger (make-condition 'boundary-direct-condition))))))
    (ok (eq :debugger (request-debugger-result-status result)))))

(deftest ordinary-condition-control-flow-is-not-intercepted
  (flet ((values-of (thunk)
           (let ((result (%boundary-result thunk)))
             (ok (eq :ok (request-debugger-result-status result)))
             (ok (null (request-debugger-result-error result)))
             (request-debugger-result-values result))))
    (ok (equal '(:signalled)
               (values-of (lambda ()
                            (signal 'boundary-direct-condition)
                            :signalled))))
    (ok (equal '(:muffled)
               (values-of (lambda ()
                            (handler-bind ((warning #'muffle-warning))
                              (warn "keep warning semantics")
                              :muffled)))))
    (ok (equal '(:handled)
               (values-of (lambda ()
                            (handler-case (error "handled")
                              (error () :handled))))))))

(deftest normal-multiple-values-are-not-escape-markers
  (flet ((values-of (thunk)
           (let ((result (%boundary-result thunk)))
             (ok (eq :ok (request-debugger-result-status result)))
             (request-debugger-result-values result))))
    (ok (null (values-of (lambda () (values)))))
    (ok (equal '(nil) (values-of (lambda () (values nil)))))
    (ok (equal '(:one :two) (values-of (lambda () (values :one :two)))))))

#+sbcl
(deftest debugger-escape-does-not-signal-through-user-error-handlers
  (let ((result (%boundary-result
                 (lambda ()
                   (handler-case (error 'boundary-direct-condition)
                     (error () :handled))))))
    (ok (eq :debugger (request-debugger-result-status result)))))

#+sbcl
(deftest report-debugger-reentry-preserves-original-degraded-record
  (let* ((*report-invocations* 0)
         (result (%boundary-result
                  (lambda () (error 'boundary-reentrant-report-condition))))
         (escape (request-debugger-result-error result))
         (context (request-debugger-escape-error-context escape)))
    (ok (eq :debugger (request-debugger-result-status result)))
    (ok (search "BOUNDARY-REENTRANT-REPORT-CONDITION" (getf context :condition-type)))
    (ok (getf context :diagnostic-capture-failed))
    (ok (null (getf context :frames)))
    (ok (null (getf context :restarts)))
    (ok (search "BOUNDARY-REENTRANT-REPORT-CONDITION"
                (request-debugger-escape-error-display-text escape)))
    (ok (equal (request-debugger-escape-error-display-text escape)
               (princ-to-string escape)))
    (ok (= 1 *report-invocations*) "saved display text never calls the original report")))

#+sbcl
(deftest diagnostic-signal-preserves-original-degraded-record
  (let* ((*report-invocations* 0)
         (result (%boundary-result
                  (lambda () (error 'boundary-signalling-report-condition))))
         (context (request-debugger-escape-error-context
                   (request-debugger-result-error result))))
    (ok (eq :debugger (request-debugger-result-status result)))
    (ok (search "BOUNDARY-SIGNALLING-REPORT-CONDITION" (getf context :condition-type)))
    (ok (getf context :diagnostic-capture-failed))
    (ok (= 1 *report-invocations*))))

#+sbcl
(deftest declining-handler-still-reaches-debugger-boundary
  (let* ((seen nil)
         (result (%boundary-result
                  (lambda ()
                    (handler-bind ((boundary-direct-condition
                                     (lambda (condition)
                                       (declare (ignore condition))
                                       (setf seen t))))
                      (error 'boundary-direct-condition))))))
    (ok seen)
    (ok (eq :debugger (request-debugger-result-status result)))))

(deftest inactive-policy-does-not-create-request-context
  (let* ((*request-debugger-boundary-active* nil)
         (result
           (call-with-request-debugger-boundary
            (lambda ()
              cl-mcp/src/utils/request-debugger-boundary::*request-debugger-context*))))
    (ok (eq :ok (request-debugger-result-status result)))
    (ok (equal '(nil) (request-debugger-result-values result)))))

(deftest user-restart-recovery-precedes-debugger-boundary
  (let ((result
          (%boundary-result
           (lambda ()
             (restart-case
                 (handler-bind ((boundary-direct-condition
                                  (lambda (condition)
                                    (declare (ignore condition))
                                    (invoke-restart 'recover-boundary))))
                   (error 'boundary-direct-condition))
               (recover-boundary () :recovered))))))
    (ok (eq :ok (request-debugger-result-status result)))
    (ok (equal '(:recovered) (request-debugger-result-values result)))))

#+sbcl
(deftest cleanup-debugger-entry-preserves-first-record
  (let* ((cleanup-ran nil)
         (*report-invocations* 0)
         (result
           (%boundary-result
            (lambda ()
              (unwind-protect
                   (error 'boundary-direct-condition)
                (setf cleanup-ran t)
                (invoke-debugger (make-condition 'boundary-reentrant-report-condition))))))
         (context (request-debugger-escape-error-context
                   (request-debugger-result-error result))))
    (ok cleanup-ran)
    (ok (eq :debugger (request-debugger-result-status result)))
    (ok (search "BOUNDARY-DIRECT-CONDITION" (getf context :condition-type)))
    (ok (zerop *report-invocations*) "cleanup debugger entry never restarts capture")))

#+sbcl
(deftest boundary-restores-caller-debugger-hook
  (let* ((hook (lambda (condition previous-hook)
                 (declare (ignore condition previous-hook))
                 (error "caller hook must not run")))
         (sb-ext:*invoke-debugger-hook* hook)
         (result (%boundary-result (lambda () (error 'boundary-direct-condition)))))
    (ok (eq :debugger (request-debugger-result-status result)))
    (ok (eq hook sb-ext:*invoke-debugger-hook*))))

#+sbcl
(deftest deadline-interrupt-transfers-to-its-live-inner-tag
  (let* ((tag (list :test-deadline))
         (marker (list :test-deadline-marker))
         (caught nil)
         (result (%boundary-result
                  (lambda ()
                    (setf caught
                          (catch tag
                            (request-debugger-deadline-interrupt tag marker)))))))
    (ok (eq marker caught))
    (ok (eq :timeout (request-debugger-result-status result)))
    (ok (null (request-debugger-result-values result)))
    (ok (null (request-debugger-result-error result)))))

#+sbcl
(deftest repeated-deadline-interrupt-does-not-rethrow-during-unwind
  (let* ((tag (list :test-deadline))
         (marker (list :test-deadline-marker))
         (cleanup-finished nil)
         (result
           (%boundary-result
            (lambda ()
              (catch tag
                (unwind-protect
                     (request-debugger-deadline-interrupt tag marker)
                  (request-debugger-deadline-interrupt tag marker)
                  (setf cleanup-finished t)))))))
    (ok cleanup-finished)
    (ok (eq :timeout (request-debugger-result-status result)))))

#+sbcl
(deftest deadline-during-debugger-unwind-selects-terminal-timeout
  (let* ((tag (list :test-deadline))
         (marker (list :test-deadline-marker))
         (inner-catch-resumed nil)
         (result
           (%boundary-result
            (lambda ()
              (catch tag
                (unwind-protect
                     (error 'boundary-direct-condition)
                  (request-debugger-deadline-interrupt tag marker)))
              (setf inner-catch-resumed t)))))
    (ok (not inner-catch-resumed) "the deadline does not target an unwound inner catch")
    (ok (eq :timeout (request-debugger-result-status result)))
    (ok (null (request-debugger-result-error result)))))

#+sbcl
(deftest debugger-during-deadline-unwind-preserves-timeout
  (let* ((tag (list :test-deadline))
         (marker (list :test-deadline-marker))
         (*report-invocations* 0)
         (result
           (%boundary-result
            (lambda ()
              (catch tag
                (unwind-protect
                     (request-debugger-deadline-interrupt tag marker)
                  (invoke-debugger
                   (make-condition 'boundary-reentrant-report-condition))))))))
    (ok (eq :timeout (request-debugger-result-status result)))
    (ok (null (request-debugger-result-error result)))
    (ok (zerop *report-invocations*))))

(deftest inactive-deadline-interrupt-keeps-guarded-transfer
  (let ((tag (list :test-deadline))
        (marker (list :test-deadline-marker)))
    (ok (eq marker (catch tag (request-debugger-deadline-interrupt tag marker))))
    (ok (null (request-debugger-deadline-interrupt tag marker))
        "an absent tag outside a request retains the guarded transfer behavior")))
