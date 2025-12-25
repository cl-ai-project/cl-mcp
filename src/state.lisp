;;;; src/state.lisp
;;;; Server state class - extracted to avoid circular dependencies

(defpackage #:cl-mcp/src/state
  (:use #:cl)
  (:export
   #:server-state
   #:initialized-p
   #:client-info
   #:protocol-version
   #:make-state))

(in-package #:cl-mcp/src/state)

(defclass server-state ()
  ((initialized-p :initform nil :accessor initialized-p)
   (client-info :initform nil :accessor client-info)
   (protocol-version :initform nil :accessor protocol-version))
  (:documentation "Holds per-session state for an MCP connection."))

(defun make-state ()
  "Create a fresh server-state instance."
  (make-instance 'server-state))
