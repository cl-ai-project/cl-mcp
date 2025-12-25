;;;; src/tools/registry.lisp
;;;;
;;;; Tool registry for MCP server.
;;;; Provides centralized registration and lookup of tool descriptors and handlers.

(defpackage #:cl-mcp/src/tools/registry
  (:use #:cl)
  (:export #:register-tool
           #:get-tool-handler
           #:get-all-tool-descriptors
           #:clear-tool-registry))

(in-package #:cl-mcp/src/tools/registry)

(defstruct tool-entry
  "Entry for a registered tool."
  (name "" :type string)
  (descriptor nil :type (or null hash-table))
  (handler nil :type (or null function)))

(defparameter *tool-registry* (make-hash-table :test #'equal)
  "Registry mapping tool names to tool-entry structs.")

(defun register-tool (name descriptor handler)
  "Register a tool with its NAME, DESCRIPTOR (hash-table), and HANDLER (function).
NAME should be a string like \"repl-eval\" or \"fs-read-file\".
DESCRIPTOR should be a hash-table with keys like \"name\", \"description\", \"inputSchema\".
HANDLER should be a function of (state id args) that returns a JSON-RPC result."
  (setf (gethash name *tool-registry*)
        (make-tool-entry :name name
                         :descriptor descriptor
                         :handler handler))
  name)

(defun get-tool-handler (name)
  "Return the handler function for tool NAME, or NIL if not found."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (tool-entry-handler entry))))

(defun get-all-tool-descriptors ()
  "Return a vector of all registered tool descriptors."
  (let ((descriptors '()))
    (maphash (lambda (name entry)
               (declare (ignore name))
               (push (tool-entry-descriptor entry) descriptors))
             *tool-registry*)
    (coerce (nreverse descriptors) 'vector)))

(defun clear-tool-registry ()
  "Clear all registered tools. Mainly for testing."
  (clrhash *tool-registry*))
