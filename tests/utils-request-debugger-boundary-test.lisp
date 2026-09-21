(defpackage #:cl-mcp/tests/utils-request-debugger-boundary-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok)
  (:import-from #:bordeaux-threads
                #:make-semaphore #:signal-semaphore #:wait-on-semaphore
                #:make-thread #:thread-alive-p #:destroy-thread #:join-thread)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:*request-debugger-boundary-active*
                #:*request-debugger-context* #:%context-state
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
         (*request-debugger-context* nil)
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
  ;; RUN-TESTS may itself be inside a managed request boundary. This fixture
  ;; specifically exercises calls outside any request context.
  (let ((*request-debugger-boundary-active* nil)
        (*request-debugger-context* nil)
        (tag (list :test-deadline))
        (marker (list :test-deadline-marker)))
    (ok (eq marker (catch tag (request-debugger-deadline-interrupt tag marker))))
    (ok (null (request-debugger-deadline-interrupt tag marker))
        "an absent tag outside a request retains the guarded transfer behavior")))

#+sbcl
(deftest deadline-after-debugger-escape-does-not-target-expired-catch
  (let* ((*request-debugger-context* nil)
         (constructor 'cl-mcp/src/utils/request-debugger-boundary::%make-result)
         (original (fdefinition constructor))
         (tag (list :expired-deadline))
         (marker (list :deadline-marker))
         (injected nil)
         (delivered nil)
         (result nil))
    ;; Inject at the first result-construction call after the debugger catch
    ;; has escaped. Keep the real constructor so the boundary still settles
    ;; its own result, and restore it even if the stale-tag throw fails.
    (unwind-protect
         (progn
           (setf (fdefinition constructor)
                 (lambda (status &rest arguments)
                   (when (eq :debugger status)
                     (setf injected t)
                     (sb-thread:interrupt-thread
                      sb-thread:*current-thread*
                      (lambda ()
                        (setf delivered t)
                        (request-debugger-deadline-interrupt tag marker))))
                   (apply original status arguments)))
           (setf result
                 (handler-case
                     (%boundary-result
                      (lambda ()
                        (catch tag
                          (error 'boundary-direct-condition))))
                   (control-error () :expired-catch))))
      (setf (fdefinition constructor) original))
    (ok injected "the deadline arrives between catch escape and result construction")
    (ok delivered "the interrupt is eventually delivered")
    (ok (not (eq :expired-catch result)) "the interrupt never throws to a dead catch")
    (unless (eq :expired-catch result)
      (ok (eq :debugger (request-debugger-result-status result))))))

#+sbcl
(defvar *boundary-interrupt-events* nil)

#+sbcl
(defun %probe-boundary-interrupts (phase)
  (sb-thread:interrupt-thread
   sb-thread:*current-thread*
   (lambda () (push (list phase :interrupt) *boundary-interrupt-events*)))
  (push (list phase :returned) *boundary-interrupt-events*))

#+sbcl
(define-condition boundary-interruptible-report-condition (condition) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (%probe-boundary-interrupts :diagnostics)
             (write-string "interruptible diagnostic report" stream))))

#+sbcl
(deftest boundary-keeps-execution-diagnostics-and-cleanup-interruptible
  (let ((*boundary-interrupt-events* nil))
    (let ((result
            (%boundary-result
             (lambda ()
               (%probe-boundary-interrupts :thunk)
               (unwind-protect
                    (error 'boundary-interruptible-report-condition)
                 (%probe-boundary-interrupts :cleanup))))))
      (ok (eq :debugger (request-debugger-result-status result)))
      (ok (equal '((:thunk :interrupt) (:thunk :returned)
                   (:diagnostics :interrupt) (:diagnostics :returned)
                   (:cleanup :interrupt) (:cleanup :returned))
                 (nreverse *boundary-interrupt-events*))
          "all user phases deliver interrupts immediately instead of deferring them"))))

#+sbcl
(deftest managed-deadline-children-inherit-policy-with-distinct-contexts
  (let ((*request-debugger-boundary-active* t)
        (outer-context nil)
        (inner-context nil)
        (inner-error nil)
        (inner-leaked nil))
    (multiple-value-bind (result status leaked)
        (call-with-deadline-thread
         (lambda ()
           (setf outer-context
                 cl-mcp/src/utils/request-debugger-boundary::*request-debugger-context*)
           (multiple-value-bind (result status leaked)
               (call-with-deadline-thread
                (lambda ()
                  (setf inner-context
                        cl-mcp/src/utils/request-debugger-boundary::*request-debugger-context*)
                  (error 'boundary-direct-condition))
                2 :name "nested-debugger-child")
             (setf inner-error result
                   inner-leaked leaked)
             status))
         5 :name "outer-debugger-child")
      (ok (eq :ok status))
      (ok (equal '(:error) result))
      (ok outer-context)
      (ok inner-context)
      (ok (not (eq outer-context inner-context)))
      (ok (request-debugger-escape-error-p inner-error))
      (ok (not inner-leaked))
      (ok (not leaked)))))

#+sbcl
(deftest managed-deadline-keeps-user-phases-interruptible
  (let ((*request-debugger-boundary-active* t)
        (events nil))
    (multiple-value-bind (result status leaked)
        (call-with-deadline-thread
         (lambda ()
           (let ((*boundary-interrupt-events* nil))
             (unwind-protect
                  (progn
                    (%probe-boundary-interrupts :thunk)
                    (error 'boundary-interruptible-report-condition))
               (%probe-boundary-interrupts :cleanup)
               (setf events (reverse *boundary-interrupt-events*)))))
         2 :name "interruptible-debugger-child")
      (ok (eq :error status))
      (ok (request-debugger-escape-error-p result))
      (ok (not leaked))
      (ok (equal '((:thunk :interrupt) (:thunk :returned)
                   (:diagnostics :interrupt) (:diagnostics :returned)
                   (:cleanup :interrupt) (:cleanup :returned))
                 events)
          "the deadline's publication protection permits interrupts in every user phase"))))

#+sbcl
(defun %assert-deadline-race (thunk entered &key (state :running))
  ;; Gate the actual deadline callback before it is queued. User execution,
  ;; diagnostics, and cleanup remain interruptible while the caller waits.
  (let* ((answer-ready (make-semaphore))
         (callback-ready (make-semaphore))
         (deliver-callback (make-semaphore))
         (name (symbol-name (gensym "debugger-cleanup-race-")))
         (interrupt-name 'bordeaux-threads:interrupt-thread)
         (original (fdefinition interrupt-name))
         (context nil)
         (delivered-state nil)
         (answer nil)
         (caller nil))
    (unwind-protect
         (progn
           (setf (fdefinition interrupt-name)
                 (lambda (thread callback)
                   (if (equal name (bordeaux-threads:thread-name thread))
                       (progn
                         (signal-semaphore callback-ready)
                         (wait-on-semaphore deliver-callback)
                         (funcall original thread
                                  (lambda ()
                                    (setf delivered-state (%context-state context))
                                    (funcall callback))))
                       (funcall original thread callback))))
           (setf caller
                 (make-thread
                  (lambda ()
                    (let ((*request-debugger-boundary-active* t))
                      (setf answer
                            (multiple-value-list
                             (call-with-deadline-thread
                              (lambda ()
                                (setf context *request-debugger-context*)
                                (funcall thunk))
                              0.25 :name name))))
                    (signal-semaphore answer-ready))
                  :name "deadline-debugger-race"))
           (ok (wait-on-semaphore entered :timeout 2)
               "the intended user phase started before callback delivery")
           (ok (wait-on-semaphore callback-ready :timeout 2)
               "the actual deadline callback is waiting at the gate")
           (ok (and context
                    (eq state (%context-state context)))
               "the intended boundary state is confirmed before callback delivery")
           (signal-semaphore deliver-callback)
           (ok (wait-on-semaphore answer-ready :timeout 5)
               "deadline terminates through the outer terminal tag")
           (ok (eq state delivered-state)
               "the real callback was delivered in the confirmed phase")
           (ok (eq :timeout (second answer)))
           (ok (eql 0.25 (first answer)))
           (ok (not (third answer))
               "the controlled timeout did not leak the deadline child"))
      (signal-semaphore deliver-callback)
      (when caller
        (unless (wait-on-semaphore answer-ready :timeout 0.1)
          (when (thread-alive-p caller)
            (destroy-thread caller)))
        (ignore-errors (join-thread caller)))
      (setf (fdefinition interrupt-name) original))))

#+sbcl
(deftest deadline-during-debugger-cleanup-prefers-timeout
  (let ((cleanup-started (make-semaphore)))
    (%assert-deadline-race
     (lambda ()
       (unwind-protect
            (error 'boundary-direct-condition)
         (signal-semaphore cleanup-started)
         (wait-on-semaphore (make-semaphore))))
     cleanup-started :state :debugger-unwinding)))

#+sbcl
(define-condition boundary-blocking-report-condition (condition)
  ((report-started :initarg :report-started :reader report-started)
   (cleanup-started :initarg :cleanup-started :reader cleanup-started)))

#+sbcl
(define-condition boundary-deadline-reentrant-report-condition
    (boundary-blocking-report-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore stream))
     (unwind-protect
          (progn
            (signal-semaphore (report-started condition))
            (wait-on-semaphore (make-semaphore)))
       (signal-semaphore (cleanup-started condition))
       (invoke-debugger (make-condition 'boundary-direct-condition))))))

#+sbcl
(define-condition boundary-deadline-signalling-report-condition
    (boundary-blocking-report-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore stream))
     (unwind-protect
          (progn
            (signal-semaphore (report-started condition))
            (wait-on-semaphore (make-semaphore)))
       (signal-semaphore (cleanup-started condition))
       (signal 'boundary-direct-condition)))))

#+sbcl
(deftest deadline-during-report-debugger-reentry-preserves-timeout
  (let ((report-started (make-semaphore))
        (cleanup-started (make-semaphore)))
    (%assert-deadline-race
     (lambda ()
       (error 'boundary-deadline-reentrant-report-condition
              :report-started report-started :cleanup-started cleanup-started))
     report-started)
    (ok (wait-on-semaphore cleanup-started :timeout 1)
        "deadline unwind entered the condition report's debugger cleanup")))

#+sbcl
(deftest deadline-during-report-signal-preserves-timeout
  (let ((report-started (make-semaphore))
        (cleanup-started (make-semaphore)))
    (%assert-deadline-race
     (lambda ()
       (error 'boundary-deadline-signalling-report-condition
              :report-started report-started :cleanup-started cleanup-started))
     report-started)
    (ok (wait-on-semaphore cleanup-started :timeout 1)
        "deadline unwind entered the condition report's signalling cleanup")))

#+sbcl
(defclass boundary-preview-printer ()
  ((entered :initarg :entered :reader preview-entered)
   (cleanup :initarg :cleanup :reader preview-cleanup)))

#+sbcl
(defmethod print-object ((object boundary-preview-printer) stream)
  (declare (ignore stream))
  (unwind-protect
       (progn
         (signal-semaphore (preview-entered object))
         (wait-on-semaphore (make-semaphore)))
    (funcall (preview-cleanup object))))

#+sbcl
(defvar *preview-kept-local* nil)

#+sbcl
(defun boundary-preview-user-frame (object)
  (declare (optimize (debug 3) (speed 0)))
  (unwind-protect
       (invoke-debugger (make-condition 'boundary-simple-condition
                                       :format-control "original preview condition"))
    (setf *preview-kept-local* object)))

#+sbcl
(deftest deadline-during-positive-preview-secondary-error-preserves-timeout
  (let* ((entered (make-semaphore))
         (object (make-hash-table))
         (capture-name 'cl-mcp/src/frame-inspector:capture-debugger-error-context)
         (capture (fdefinition capture-name))
         (observed nil))
    (setf (gethash :nested object)
          (make-instance 'boundary-preview-printer
                         :entered entered
                         :cleanup (lambda ()
                                    (error "secondary preview unwind failure"))))
    ;; Enable the supported optional preview branch at the existing capture
    ;; seam, while retaining the real hook, selector, and deadline callback.
    (unwind-protect
         (progn
           (setf (fdefinition capture-name)
                 (lambda (condition callback &rest options)
                   (declare (ignore options))
                   (funcall capture condition
                            (lambda (secondary)
                              (setf observed
                                    (list (type-of secondary)
                                          (princ-to-string secondary)
                                          (%context-state *request-debugger-context*)))
                              (funcall callback secondary))
                            :max-frames 1 :filter-internal t :locals-preview-frames 1
                            :preview-max-depth 2)))
           (%assert-deadline-race (lambda () (boundary-preview-user-frame object)) entered)
           (ok (equal '(simple-error "secondary preview unwind failure" :deadline-unwinding)
                      observed)
               "the diagnostic callback sees the preview cleanup error during deadline unwind"))
      (setf (fdefinition capture-name) capture))))
