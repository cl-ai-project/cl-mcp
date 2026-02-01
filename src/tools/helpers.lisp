;;;; src/tools/helpers.lisp
;;;;
;;;; Common helper functions for MCP tool handlers.
;;;; These are used by tool implementations to format JSON-RPC responses.

(defpackage #:cl-mcp/src/tools/helpers
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/hash
                #:make-string-hash-table)
  (:export #:make-ht
           #:result
           #:rpc-error
           #:text-content
           #:tool-error
           ;; Argument extraction helpers
           #:arg-validation-error
           #:validation-message
           #:extract-arg
           #:extract-boolean-arg))

(in-package #:cl-mcp/src/tools/helpers)

(declaim (ftype (function (&rest t) hash-table) make-ht))
(defun make-ht (&rest kvs)
  "Create a hash-table from alternating key-value pairs.
This is an alias for make-string-hash-table from utils/hash.
Example: (make-ht \"name\" \"foo\" \"type\" \"string\")"
  (apply #'make-string-hash-table kvs))

(declaim (ftype (function (t t) hash-table) result))
(defun result (id payload)
  "Create a JSON-RPC 2.0 result response."
  (make-ht "jsonrpc" "2.0" "id" id "result" payload))

(declaim (ftype (function (t integer string &optional t) hash-table) rpc-error))
(defun rpc-error (id code message &optional data)
  "Create a JSON-RPC 2.0 error response."
  (let* ((err (make-ht "code" code "message" message))
         (obj (make-ht "jsonrpc" "2.0" "id" id "error" err)))
    (when data (setf (gethash "data" err) data))
    obj))

(declaim (ftype (function (string) simple-vector) text-content))
(defun text-content (text)
  "Return a one-element content vector with TEXT as a text part.
Used for MCP tool response content."
  (vector (make-ht "type" "text" "text" text)))

(declaim (ftype (function (t string &key (:protocol-version (or string null))) hash-table) tool-error))
(defun tool-error (id message &key (protocol-version nil))
  "Return a tool input validation error in the appropriate format.
For protocol version 2025-11-25 and later, returns as Tool Execution Error.
For older versions, returns as JSON-RPC Protocol Error (-32602).
PROTOCOL-VERSION should be a string like \"2025-11-25\" or NIL."
  (if (and protocol-version (string>= protocol-version "2025-11-25"))
      (result id (make-ht "content" (text-content message) "isError" t))
      (rpc-error id -32602 message)))

;;;; Argument Extraction Helpers
;;;;
;;;; These helpers simplify the common pattern of extracting and validating
;;;; arguments from the MCP tool call args hash-table.

(define-condition arg-validation-error (error)
  ((arg-name :initarg :arg-name :reader arg-name)
   (message :initarg :message :reader validation-message))
  (:report (lambda (c s)
             (format s "~A" (validation-message c))))
  (:documentation "Signaled when tool argument validation fails.
Use VALIDATION-MESSAGE to get the user-facing error message."))

(defun %check-type-match (value type arg-name)
  "Check if VALUE matches TYPE. Signal ARG-VALIDATION-ERROR if not.
TYPE can be :string, :integer, :number, :boolean, :array, :object, or NIL (any)."
  (when type
    (let ((valid (ecase type
                   (:string (stringp value))
                   (:integer (integerp value))
                   (:number (numberp value))
                   (:boolean (member value '(t nil)))
                   (:array (or (vectorp value) (listp value)))
                   (:object (hash-table-p value)))))
      (unless valid
        (error 'arg-validation-error
               :arg-name arg-name
               :message (format nil "~A must be ~A"
                                arg-name
                                (ecase type
                                  (:string "a string")
                                  (:integer "an integer")
                                  (:number "a number")
                                  (:boolean "boolean")
                                  (:array "an array")
                                  (:object "an object"))))))))

(declaim (ftype (function ((or hash-table null) string &key (:type (or keyword null)) (:required boolean)) t) extract-arg))
(defun extract-arg (args name &key type required)
  "Extract argument NAME from ARGS hash-table with optional validation.

Arguments:
  ARGS     - Hash-table of tool arguments (may be NIL)
  NAME     - String key to extract
  TYPE     - Expected type (:string :integer :number :boolean :array :object)
             If NIL, no type checking is performed.
  REQUIRED - If T, signal error when argument is missing

Returns the argument value, or NIL if optional and not provided.
Signals ARG-VALIDATION-ERROR on validation failure.

Example:
  (extract-arg args \"path\" :type :string :required t)
  (extract-arg args \"limit\" :type :integer)"
  (let ((value (and args (gethash name args))))
    (cond
      ;; Required but missing
      ((and required (null value))
       (error 'arg-validation-error
              :arg-name name
              :message (format nil "~A is required" name)))
      ;; Present - check type
      (value
       (%check-type-match value type name)
       value)
      ;; Optional and missing
      (t nil))))

(declaim (ftype (function ((or hash-table null) string &key (:default boolean)) boolean) extract-boolean-arg))
(defun extract-boolean-arg (args name &key (default nil))
  "Extract boolean argument NAME from ARGS with proper nil/false handling.

The challenge with boolean args is distinguishing between:
  - Not provided (should use DEFAULT)
  - Explicitly set to false/nil (should use NIL)
  - Explicitly set to true (should use T)

Arguments:
  ARGS    - Hash-table of tool arguments (may be NIL)
  NAME    - String key to extract
  DEFAULT - Value to use when argument is not provided (default: NIL)

Returns the boolean value.
Signals ARG-VALIDATION-ERROR if provided but not a boolean.

Example:
  (extract-boolean-arg args \"recursive\" :default t)
  (extract-boolean-arg args \"dry_run\")"
  (multiple-value-bind (value presentp)
      (and args (gethash name args))
    (cond
      ;; Not provided - use default
      ((not presentp) default)
      ;; Provided - validate it's boolean
      (t
       (unless (member value '(t nil))
         (error 'arg-validation-error
                :arg-name name
                :message (format nil "~A must be boolean" name)))
       value))))
