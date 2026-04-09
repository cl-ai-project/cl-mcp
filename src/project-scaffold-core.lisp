;;;; src/project-scaffold-core.lisp
;;;;
;;;; Pure helpers for project-scaffold: input validation, template rendering,
;;;; path math, and file manifest construction. No I/O. No worker interaction.
;;;; This module exists so that the effectful layer in project-scaffold.lisp
;;;; stays thin and the bulk of the logic is trivially unit-testable.

(defpackage #:cl-mcp/src/project-scaffold-core
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan)
  (:export #:validate-project-name
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
