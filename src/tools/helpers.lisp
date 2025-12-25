;;;; src/tools/helpers.lisp
;;;;
;;;; Common helper functions for MCP tool handlers.
;;;; These are used by tool implementations to format JSON-RPC responses.

(defpackage #:cl-mcp/src/tools/helpers
  (:use #:cl)
  (:export #:make-ht
           #:result
           #:rpc-error
           #:text-content
           #:tool-error))

(in-package #:cl-mcp/src/tools/helpers)

(defun make-ht (&rest kvs)
  "Create a hash-table from alternating key-value pairs.
Example: (make-ht \"name\" \"foo\" \"type\" \"string\")"
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k h) v))
    h))

(defun result (id payload)
  "Create a JSON-RPC 2.0 result response."
  (make-ht "jsonrpc" "2.0" "id" id "result" payload))

(defun rpc-error (id code message &optional data)
  "Create a JSON-RPC 2.0 error response."
  (let* ((err (make-ht "code" code "message" message))
         (obj (make-ht "jsonrpc" "2.0" "id" id "error" err)))
    (when data (setf (gethash "data" err) data))
    obj))

(defun text-content (text)
  "Return a one-element content vector with TEXT as a text part.
Used for MCP tool response content."
  (vector (make-ht "type" "text" "text" text)))

(defun tool-error (id message &key (protocol-version nil))
  "Return a tool input validation error in the appropriate format.
For protocol version 2025-11-25 and later, returns as Tool Execution Error.
For older versions, returns as JSON-RPC Protocol Error (-32602).
PROTOCOL-VERSION should be a string like \"2025-11-25\" or NIL."
  (if (and protocol-version (string>= protocol-version "2025-11-25"))
      (result id (make-ht "content" (text-content message) "isError" t))
      (rpc-error id -32602 message)))
