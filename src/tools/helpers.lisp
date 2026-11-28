;;;; src/tools/helpers.lisp
;;;;
;;;; Common helper functions for MCP tool handlers.
;;;; These are used by tool implementations to format JSON-RPC responses.

(defpackage #:cl-mcp/src/tools/helpers
  (:use #:cl)
  (:import-from #:cl-ppcre #:create-scanner #:scan-to-strings)
  (:import-from #:cl-mcp/src/utils/hash
                #:make-string-hash-table)
  (:import-from #:cl-mcp/src/utils/sanitize #:json-number-p)
  (:import-from #:yason
                #:false)
  ;; Every tool response passes through this module, parent and worker alike,
  ;; so loading the shim here covers all yason:true/false and :null uses.
  (:import-from #:cl-mcp/src/yason-compat)
  (:export #:make-ht
           #:result
           #:rpc-error
           #:text-content
           #:tool-error
           ;; JSON serialization helpers
           #:json-bool
           ;; Failures that leave the worker usable
           #:transient-error
           ;; Argument extraction helpers
           #:arg-validation-error
           #:validation-message
           #:extract-arg
           #:extract-array-arg
           #:extract-boolean-arg
           #:*wire-tool-arguments*
           #:*wire-scalar-source*))

(in-package #:cl-mcp/src/tools/helpers)

(declaim (ftype (function (&rest t) hash-table) make-ht))
(defun make-ht (&rest kvs)
  "Create a hash-table from alternating key-value pairs.
This is an alias for make-string-hash-table from utils/hash.
Example: (make-ht \"name\" \"foo\" \"type\" \"string\")"
  (apply #'make-string-hash-table kvs))

(declaim (ftype (function ((or null string integer) t) hash-table) result))
(defun result (id payload)
  "Create a JSON-RPC 2.0 result response."
  (make-ht "jsonrpc" "2.0" "id" id "result" payload))

(declaim (ftype (function ((or null string integer) integer string &optional t)
                           hash-table)
                rpc-error))
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

(declaim (ftype (function ((or null string integer) string
                           &key (:protocol-version (or null string)))
                           hash-table)
                tool-error))
(defun tool-error (id message &key (protocol-version nil))
  "Return a tool input validation error in the appropriate format.
For protocol version 2025-11-25 and later, returns as Tool Execution Error.
For older versions, returns as JSON-RPC Protocol Error (-32602).
PROTOCOL-VERSION should be a string like \"2025-11-25\" or NIL."
  (if (and protocol-version (string>= protocol-version "2025-11-25"))
      (result id (make-ht "content" (text-content message) "isError" t))
      (rpc-error id -32602 message)))

(declaim (inline json-bool))

(defun json-bool (value)
  "Convert a Common Lisp generalized boolean to a strict JSON boolean.
In CL, any non-NIL value is truthy, but JSON requires exactly true or
false.  Yason serializes T as true and YASON:FALSE as false; plain NIL
would serialize as null.  This helper normalizes the mismatch:

  (json-bool some-flag)  =>  T          when SOME-FLAG is non-NIL
  (json-bool nil)        =>  YASON:FALSE

Typical usage in MCP tool responses:
  (make-ht \"would_change\" (json-bool changed-p))"
  (if value t 'yason:false))

;;;; Argument Extraction Helpers
;;;;
;;;; These helpers simplify the common pattern of extracting and validating
;;;; arguments from the MCP tool call args hash-table.

(define-condition transient-error (simple-error)
  ()
  (:documentation "An error whose cause leaves this worker usable.

Signalled when an operation could not proceed for a reason that will pass on
its own -- a concurrent load holding a lock, say -- rather than because the
image is damaged.  Response builders check for it before appending their
standing advice to replace the worker: for these failures that advice is
actively harmful, because it destroys work that was about to finish.

A subtype of SIMPLE-ERROR so that handlers written for the ordinary case,
which is what most callers have, still catch it."))

(define-condition arg-validation-error (error)
  ((arg-name :initarg :arg-name :reader arg-name)
   (message :initarg :message :reader validation-message))
  (:report (lambda (c s)
             (format s "~A" (validation-message c))))
  (:documentation "Signaled when tool argument validation fails.
Use VALIDATION-MESSAGE to get the user-facing error message."))

(defvar *wire-tool-arguments* nil
  "The argument object of the current tool call when decoded from wire JSON.")

(defvar *wire-scalar-source* nil
  "Request text paired with the value positions of its decoded object keys.")

(defun %wire-argument-source (args name)
  "Return the request text and source position for the value of NAME in ARGS."
  (when *wire-scalar-source*
    (destructuring-bind (text . value-positions) *wire-scalar-source*
      (loop for key being the hash-keys of args
            when (equal key name)
              return (values text (gethash key value-positions))))))

(defparameter *json-decimal-pattern*
  (create-scanner "\\A(-?)(0|[1-9][0-9]*)(?:\\.([0-9]+))?(?:[eE]([+-]?[0-9]+))?\\z")
  "The JSON number grammar, with groups for the sign, digits, fraction and exponent.")

(defun %json-decimal-integer (text start value)
  "Return the exact integer of the JSON number at START, or NIL for a fraction."
  (unless (json-number-p value)
    (return-from %json-decimal-integer nil))
  (multiple-value-bind (rounded remainder) (truncate value)
    (unless (zerop remainder)
      (return-from %json-decimal-integer nil))
    (let ((end (position-if-not (lambda (char) (find char ".0123456789+-Ee"))
                                text :start start)))
      (multiple-value-bind (matched parts)
          (scan-to-strings *json-decimal-pattern* text :start start :end end)
        (when matched
          (destructuring-bind (sign whole fraction exponent) (coerce parts 'list)
            (let* ((digits (concatenate 'string whole fraction))
                   (last-nonzero (position-if (lambda (digit) (char/= digit #\0))
                                              digits :from-end t)))
              (unless last-nonzero
                (return-from %json-decimal-integer 0))
              (let* ((scale (- (if exponent (parse-integer exponent) 0) (length fraction)))
                     (cut (+ (length digits) (min 0 scale))))
                ;; Negative scale must discard only zeros. Positive scale cannot
                ;; exceed the bit length of a finite nonzero decoded number.
                (when (and (< last-nonzero cut) (<= scale (integer-length rounded)))
                  (let ((number (* (parse-integer digits :end cut)
                                   (expt 10 (max 0 scale)))))
                    (if (string= sign "-") (- number) number)))))))))))

(defun %check-type-match (value type arg-name &optional item-number type-error-hint wire-literal)
  "Check VALUE against TYPE. WIRE-LITERAL names a JSON false or null decoded as NIL."
  (when type
    (let ((valid (ecase type
                   (:any t)
                   (:string (stringp value))
                   (:integer (integerp value))
                   (:number (numberp value))
                   (:boolean (and (not (eq wire-literal :null)) (member value '(t nil))))
                   (:array (or (and (not wire-literal) (listp value))
                               (and (vectorp value) (not (stringp value)))))
                   (:object (hash-table-p value)))))
      (unless valid
        (error 'arg-validation-error
               :arg-name arg-name
               :message (format nil "~A~@[ item ~D~] must be ~A~@[. ~A~]"
                                arg-name item-number
                                (ecase type
                                  (:string "a string")
                                  (:integer "an integer")
                                  (:number "a number")
                                  (:boolean "boolean")
                                  (:array "an array")
                                  (:object "an object"))
                                type-error-hint))))))

(declaim (ftype (function ((or hash-table null) string
                           &key (:type (or keyword null)) (:required boolean) (:nullable boolean)
                                (:type-error-hint (or null string)))
                          t)
                extract-arg))
(defun extract-arg (args name &key type required nullable type-error-hint)
  "Extract NAME from the tool argument hash table ARGS and validate its TYPE.
TYPE is :STRING, :INTEGER, :NUMBER, :BOOLEAN, :ARRAY, :OBJECT, :ANY or NIL.
REQUIRED rejects omission. NULLABLE permits JSON null for the declared TYPE.
An omitted optional argument or a permitted null returns NIL. Lisp callers
may also use NIL for an optional argument. TYPE-ERROR-HINT augments a type error."
  (multiple-value-bind (value supplied-p) (and args (gethash name args))
    (let ((wire-value-p (and supplied-p (eq args *wire-tool-arguments*))))
      (when (and required
                 (not (or value (and supplied-p (or wire-value-p nullable (eq type :any))))))
        (error 'arg-validation-error :arg-name name
               :message (format nil "~A is required" name)))
      (when (or value wire-value-p)
        (multiple-value-bind (text start)
            (when (and wire-value-p (or (null value) (and (eq type :integer) (floatp value))))
              (%wire-argument-source args name))
          (let ((wire-literal (when (and wire-value-p (null value))
                                (if (and start (char= (char text start) #\n)) :null :false))))
            (when (and start (eq type :integer) (floatp value))
              (let ((integer (%json-decimal-integer text start value)))
                (when integer
                  ;; Some handlers forward the decoded argument table to the worker.
                  (setf value integer
                        (gethash name args) integer))))
            (unless (and nullable (eq wire-literal :null))
              (%check-type-match value type name nil type-error-hint wire-literal))
            value))))))

(defun extract-array-arg (args name item-type &key required nullable)
  "Extract the array argument NAME from ARGS and validate its items against ITEM-TYPE.
REQUIRED and NULLABLE have the same meaning as in EXTRACT-ARG.
Return the array unchanged."
  (let ((items (extract-arg args name :type :array :required required :nullable nullable))
        (item-number 0))
    (map nil (lambda (item)
               (%check-type-match item item-type name (incf item-number)))
         items)
    items))

(declaim (ftype (function ((or hash-table null) string
                           &key (:default boolean) (:nullable boolean))
                          boolean)
                extract-boolean-arg))
(defun extract-boolean-arg (args name &key (default nil) nullable)
  "Extract the Boolean argument NAME from ARGS.
Use DEFAULT when omitted, and return NIL for false or a permitted null."
  (if (and args (nth-value 1 (gethash name args)))
      (extract-arg args name :type :boolean :nullable nullable)
      default))
