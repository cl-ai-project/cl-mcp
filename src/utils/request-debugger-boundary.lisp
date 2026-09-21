(defpackage #:cl-mcp/src/utils/request-debugger-boundary
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary-protocol
                #:*request-debugger-boundary-active*
                #:call-with-request-debugger-boundary
                #:request-debugger-deadline-interrupt
                #:request-debugger-result-status
                #:request-debugger-result-error)
  (:import-from #:cl-mcp/src/frame-inspector
                #:capture-debugger-error-context)
  (:export #:*request-debugger-boundary-active*
           #:call-with-request-debugger-boundary
           #:request-debugger-result-status
           #:request-debugger-result-values
           #:request-debugger-result-error
           #:request-debugger-deadline-interrupt
           #:request-debugger-escape-error-p
           #:request-debugger-escape-error-context
           #:request-debugger-escape-error-display-text))

(in-package #:cl-mcp/src/utils/request-debugger-boundary)

(declaim (optimize (debug 3) (safety 3)))

(defvar *request-debugger-context* nil
  "Private same-thread state; never propagate this binding to a child thread.")

(defstruct (request-debugger-result
            (:constructor %make-result (status &key values error))
            (:conc-name %result-))
  "The settled request outcome, containing values or a saved debugger error."
  (status :ok :type (member :ok :debugger :timeout) :read-only t)
  (values nil :type list :read-only t)
  (error nil :read-only t))

(defmethod request-debugger-result-status ((result request-debugger-result))
  (%result-status result))

(defmethod request-debugger-result-error ((result request-debugger-result))
  (%result-error result))

(defun request-debugger-result-values (result)
  "Return the thunk's multiple-value list only for an :OK result."
  (%result-values result))

(define-condition request-debugger-escape-error (error)
  ((context :initarg :context :reader request-debugger-escape-error-context)
   (condition-type :initarg :condition-type)
   (message :initarg :message))
  (:documentation
   "A saved debugger outcome, constructed after the private throw has escaped.
No original condition is retained or printed, and no user restart is selected.")
  (:report
   (lambda (condition stream)
     (format stream "~A: ~A"
             (slot-value condition 'condition-type)
             (slot-value condition 'message)))))

(setf (documentation 'request-debugger-escape-error-context 'function)
      "Return the original diagnostic snapshot or its safe degraded record.")

(defun request-debugger-escape-error-p (object)
  "Return whether OBJECT is a saved request debugger escape error."
  (typep object 'request-debugger-escape-error))

(defun request-debugger-escape-error-display-text (condition)
  "Return CONDITION's saved type and message without printing its original."
  (format nil "~A: ~A"
          (slot-value condition 'condition-type)
          (slot-value condition 'message)))

#+sbcl
(defstruct (%request-debugger-context (:constructor %make-context) (:conc-name %context-))
  "Private tags and state belong to the thread executing one boundary."
  (state :running :type (member :running :debugger-unwinding :deadline-unwinding))
  (terminal-tag (list :request-terminal) :read-only t)
  (debugger-tag (list :request-debugger) :read-only t)
  pending)

#+sbcl
(defun %minimal-record (condition)
  "Describe CONDITION's type without invoking any condition or object printer."
  (let ((type (type-of condition)))
    (list :error t
          :condition-type (if (symbolp type) (symbol-name type) "CONDITION")
          :message "Debugger entered; diagnostic capture unavailable."
          :restarts nil
          :frames nil
          :diagnostic-capture-failed nil)))

#+sbcl
(defun %select-debugger-exit (context record degraded-p)
  "Commit the first debugger record, or preserve an already selected unwind."
  (sb-sys:without-interrupts
    (case (%context-state context)
      (:running
       (when degraded-p
         (setf (getf record :diagnostic-capture-failed) t))
       (setf (%context-state context) :debugger-unwinding
             (%context-pending context) record)
       :debugger)
      ((:debugger-unwinding :deadline-unwinding) :terminal))))

#+sbcl
(defun %escape-debugger (context record &optional degraded-p)
  "Throw after the small protected selection, without signalling a condition."
  (ecase (%select-debugger-exit context record degraded-p)
    (:debugger (throw (%context-debugger-tag context) nil))
    (:terminal (throw (%context-terminal-tag context) nil))))

#+sbcl
(defun %capture-debugger-entry (context condition)
  "Save the first condition, escaping immediately if diagnostics enter again."
  (let ((minimal
          (sb-sys:without-interrupts
            (when (eq :running (%context-state context))
              (setf (%context-pending context) (%minimal-record condition))))))
    (if minimal
        (flet ((diagnostic-condition (secondary)
                 (declare (ignore secondary))
                 (%escape-debugger context minimal t))
               (secondary-hook (secondary previous-hook)
                 ;; PREVIOUS-HOOK is the active hook before SBCL binds it to
                 ;; NIL, not a handle to the disabled debugger policy.
                 (declare (ignore secondary previous-hook))
                 (%escape-debugger context minimal t)))
          (let ((sb-ext:*invoke-debugger-hook* #'secondary-hook))
            (%escape-debugger
             context (capture-debugger-error-context condition #'diagnostic-condition))))
        (%escape-debugger context nil))))

#+sbcl
(defun %select-deadline-exit (context marker)
  "Choose a still-live transfer target without interrupting state publication."
  (sb-sys:without-interrupts
    (case (%context-state context)
      (:running
       (setf (%context-state context) :deadline-unwinding
             (%context-pending context) marker)
       :deadline)
      (:debugger-unwinding
       (setf (%context-state context) :deadline-unwinding
             (%context-pending context) marker)
       :terminal)
      (:deadline-unwinding nil))))

(defmethod request-debugger-deadline-interrupt :around (deadline-tag deadline-marker)
  "Transfer a deadline interrupt to a live tag on the executing request thread.
Repeated interrupts return during a deadline unwind. Outside a request, retain
the guarded transfer used by ordinary deadline callers."
  #+sbcl
  (when *request-debugger-context*
    (return-from request-debugger-deadline-interrupt
      (case (%select-deadline-exit *request-debugger-context* deadline-marker)
        (:deadline (throw deadline-tag deadline-marker))
        (:terminal
         (throw (%context-terminal-tag *request-debugger-context*) deadline-marker)))))
  (call-next-method))

(defmethod call-with-request-debugger-boundary (thunk)
  "Run THUNK and return a settled request debugger result with its values.
On SBCL, the authenticated request policy installs a dynamic debugger hook.
Ordinary signalling and user recovery are untouched until debugger entry."
  #+sbcl
  (when *request-debugger-boundary-active*
    (return-from call-with-request-debugger-boundary
      ;; Defer interrupts while exposing or retiring the context, including
      ;; result construction after its catches have gone. A pending interrupt
      ;; is delivered only after this context's binding has been unwound.
      (sb-sys:without-interrupts
        (let* ((context (%make-context))
               (*request-debugger-context* context)
               (normal-result nil))
          ;; The hook remains live while the debugger catch is unwinding. A
          ;; cleanup that enters the debugger then uses only the terminal tag.
          (catch (%context-terminal-tag context)
            (let ((sb-ext:*invoke-debugger-hook*
                    (lambda (condition previous-hook)
                      (declare (ignore previous-hook))
                      (%capture-debugger-entry context condition))))
              (catch (%context-debugger-tag context)
                (setf normal-result
                      (%make-result
                       :ok :values
                       (multiple-value-list
                        ;; User execution, diagnostics, and unwind cleanups
                        ;; all remain inside this interruptible extent.
                        (sb-sys:with-local-interrupts (funcall thunk))))))))
          ;; Only now materialize an ERROR subtype: user ERROR handlers cannot
          ;; intercept the debugger escape, and its report uses saved strings.
          (ecase (%context-state context)
            (:running normal-result)
            (:debugger-unwinding
             (let ((record (%context-pending context)))
               (%make-result :debugger
                             :error (make-condition 'request-debugger-escape-error
                                                    :context record
                                                    :condition-type
                                                    (getf record :condition-type)
                                                    :message (getf record :message)))))
            (:deadline-unwinding (%make-result :timeout)))))))
  (%make-result :ok :values (multiple-value-list (funcall thunk))))
