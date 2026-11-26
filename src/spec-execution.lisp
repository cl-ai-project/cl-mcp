;;;; Execute one requested spec check in its project's dynamic environment.

(defpackage #:cl-mcp/src/spec-execution
  (:use #:cl)
  (:import-from #:bordeaux-threads #:current-thread)
  (:import-from #:cl-mcp/src/object-registry #:*object-registry*)
  (:import-from #:cl-mcp/src/spec-adapter-core #:resolve-symbol-designator)
  (:import-from #:cl-mcp/src/utils/deadline #:call-with-deadline-thread)
  (:import-from #:cl-mcp/src/utils/bounded-stream
                #:make-bounded-output-stream #:bounded-output-string)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:request-debugger-escape-error-p #:request-debugger-escape-error-display-text)
  (:export #:resolve-check-wrapper #:call-with-check-deadline #:check-wrapper-message))

(in-package #:cl-mcp/src/spec-execution)

(declaim (optimize (debug 3) (safety 3)))

(defvar *wrapped-check-p* nil
  "True inside a wrapper's check thunk, on the thread that owns its deadline.")

(define-condition check-wrapper-error (error)
  ((message :initarg :message :reader check-wrapper-error-message))
  (:report (lambda (condition stream)
             (write-string (check-wrapper-error-message condition) stream)))
  (:documentation "A spec wrapper failed or did not invoke its thunk as required."))

(defun resolve-check-wrapper (name)
  "Return the loaded function named by NAME and NIL, or NIL and an argument error."
  (when (null name)
    (return-from resolve-check-wrapper (values nil nil)))
  (unless (and (stringp name) (let ((separator (position #\: name)))
                               (and separator (plusp separator))))
    (return-from resolve-check-wrapper
      (values nil "around_function must be a package-qualified function name.")))
  (let ((symbol (resolve-symbol-designator name)))
    (if (and symbol (fboundp symbol)
             (not (or (macro-function symbol) (special-operator-p symbol))))
        (values (fdefinition symbol) nil)
        (values nil (format nil "around_function ~S must name a loaded Lisp function." name)))))

(defun check-wrapper-message (condition)
  "Return the recorded wrapper diagnostic without invoking CONDITION's printer."
  (typecase condition
    (check-wrapper-error (check-wrapper-error-message condition))
    (t (if (request-debugger-escape-error-p condition)
           (request-debugger-escape-error-display-text condition)
           (format nil "around_function ended with a ~A condition." (type-of condition))))))

(defun %call-check-wrapper (wrapper thunk)
  (let ((owner (current-thread))
        (active t)
        (started nil)
        (completed nil)
        (results nil)
        (violation nil))
    (labels ((run-check ()
               (unless (and active (eq owner (current-thread)) (not started))
                 (setf violation t)
                 (error 'check-wrapper-error
                        :message "around_function must call its thunk once, synchronously."))
               (setf started t)
               (let ((*wrapped-check-p* t))
                 (setf results (multiple-value-list (funcall thunk))
                       completed t))
               (values-list results)))
      (unwind-protect
           (handler-case
               (progn
                 (funcall wrapper #'run-check)
                 (when (or violation (not completed))
                   (error 'check-wrapper-error
                          :message "around_function must complete its thunk once, synchronously."))
                 ;; The wrapper can perform cleanup after its call; that cleanup
                 ;; does not replace the check's result.
                 (values-list results))
             (check-wrapper-error (condition) (error condition))
             (serious-condition (condition)
               (error 'check-wrapper-error
                      :message (format nil "around_function failed ~A: ~A"
                                       (cond (completed "after the check")
                                             (started "during the check")
                                             (t "before the check"))
                                       (let ((stream (make-bounded-output-stream 1024)))
                                         (princ condition stream)
                                         (bounded-output-string stream))))))
        (setf active nil)))))

(defun %cl-spec-runtime-specials ()
  (let ((package (find-package "CL-SPEC")))
    (when package
      (loop for name in '("*REGISTRY*" "*GENERATOR-BACKEND*")
            for symbol = (find-symbol name package)
            when (and symbol (boundp symbol)) collect symbol))))

(defun call-with-check-deadline (thunk seconds &key name wrapper)
  "Run THUNK under a spec deadline, using WRAPPER on the owning thread when supplied.

The wrapper covers the request, including discovery, running and rendering.
Nested checks run in its dynamic extent under the same outer deadline."
  (cond
    (wrapper
     (let* ((specials (%cl-spec-runtime-specials))
            (settings (mapcar #'symbol-value specials))
            (registry *object-registry*))
       (call-with-deadline-thread
        (lambda ()
          (progv specials settings
            (let ((*object-registry* registry))
              (%call-check-wrapper wrapper thunk))))
        seconds :name name)))
    (*wrapped-check-p*
     (handler-case (values (multiple-value-list (funcall thunk)) :ok nil)
       (serious-condition (condition) (values condition :error nil))))
    (t (call-with-deadline-thread thunk seconds :name name))))
