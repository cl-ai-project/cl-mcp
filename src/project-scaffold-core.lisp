;;;; src/project-scaffold-core.lisp
;;;;
;;;; Pure helpers for project-scaffold: input validation, template rendering,
;;;; path math, and file manifest construction. No I/O. No worker interaction.
;;;; This module exists so that the effectful layer in project-scaffold.lisp
;;;; stays thin and the bulk of the logic is trivially unit-testable.

(defpackage #:cl-mcp/src/project-scaffold-core
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan
                #:regex-replace-all)
  (:export #:validate-project-name
           #:validate-destination
           #:validate-text-field
           #:render-template
           #:invalid-argument-error
           #:invalid-argument-field
           #:invalid-argument-value
           #:invalid-argument-reason))

(in-package #:cl-mcp/src/project-scaffold-core)

(define-condition invalid-argument-error (error)
  ((field :initarg :field :reader invalid-argument-field)
   (value :initarg :value :reader invalid-argument-value)
   (reason :initarg :reason :reader invalid-argument-reason))
  (:documentation "Signaled when a project-scaffold input argument is invalid.")
  (:report
   (lambda (condition stream)
     (format stream "Invalid argument ~A = ~S: ~A"
             (invalid-argument-field condition)
             (invalid-argument-value condition)
             (invalid-argument-reason condition)))))

(defparameter *project-name-regex* "^[a-z][a-z0-9-]*$"
  "Regular expression that valid project names must fully match.")

(defparameter *project-name-max-length* 64
  "Maximum allowed length for a project name.")

(defun validate-project-name (name)
  "Return NAME unchanged when valid, else signal INVALID-ARGUMENT-ERROR.
A valid name is a non-empty string of length at most *PROJECT-NAME-MAX-LENGTH*
that fully matches *PROJECT-NAME-REGEX*: a lower-case letter followed by
lower-case letters, digits, or hyphens."
  (unless (stringp name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must be a string"))
  (when (zerop (length name))
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must not be empty"))
  (when (> (length name) *project-name-max-length*)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must be at most ~D characters"
                           *project-name-max-length*)))
  (unless (cl-ppcre:scan *project-name-regex* name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must match ~A" *project-name-regex*)))
  name)

(defun validate-destination (destination)
  "Return DESTINATION when it is a safe relative path, else signal error.
A valid destination is a non-empty relative path with no absolute segment
and no '..' component. Empty strings and NIL are rejected."
  (unless (and (stringp destination) (plusp (length destination)))
    (error 'invalid-argument-error
           :field "destination" :value destination
           :reason "must be a non-empty string"))
  (when (char= (char destination 0) #\/)
    (error 'invalid-argument-error
           :field "destination" :value destination
           :reason "must be a relative path (no leading /)"))
  (dolist (segment (uiop:split-string destination :separator "/"))
    (when (string= segment "..")
      (error 'invalid-argument-error
             :field "destination" :value destination
             :reason "must not contain '..' path segments")))
  destination)

(defun validate-text-field (field-name value)
  "Return VALUE when it is an acceptable free-text field, else signal.
FIELD-NAME is included in the error for caller-side diagnostics. A valid
value is a string containing no newline (#\\Newline) or carriage return
(#\\Return) characters. Empty strings are allowed."
  (unless (stringp value)
    (error 'invalid-argument-error
           :field field-name :value value
           :reason "must be a string"))
  (when (or (find #\Newline value) (find #\Return value))
    (error 'invalid-argument-error
           :field field-name :value value
           :reason "must not contain newline characters"))
  value)

(defun render-template (template bindings)
  "Return TEMPLATE with each '{{key}}' substituted using BINDINGS.
BINDINGS is an alist of (KEY-STRING . VALUE-STRING). Unknown placeholders
are left intact. Values are substituted literally; regex metacharacters
in values (including backslash and dollar sign) are handled safely via
cl-ppcre's :simple-calls replacement callback."
  (cl-ppcre:regex-replace-all
   "\\{\\{([A-Za-z_-][A-Za-z0-9_-]*)\\}\\}"
   template
   (lambda (match &rest registers)
     (declare (ignore match))
     (let* ((key (first registers))
            (entry (assoc key bindings :test #'string=)))
       (if entry
           (cdr entry)
           (format nil "{{~A}}" key))))
   :simple-calls t))
