;;;; src/tools/define-tool.lisp
;;;;
;;;; Macro for defining MCP tools with reduced boilerplate.
;;;; Generates descriptor, handler, and registration from a single definition.

(defpackage #:cl-mcp/src/tools/define-tool
  (:use #:cl)
  (:import-from #:cl-mcp/src/state
                #:protocol-version)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:rpc-error #:tool-error
                #:arg-validation-error #:validation-message
                #:extract-arg #:extract-boolean-arg)
  (:import-from #:cl-mcp/src/tools/registry
                #:register-tool)
  (:export #:define-tool))

(in-package #:cl-mcp/src/tools/define-tool)

;;;; Argument Specification Helpers
;;;;
;;;; Argument specs are plists or simple forms:
;;;;   Simple: name                     -> optional string arg
;;;;   Full:   (name &key json-name type required default description)
;;;;
;;;; Types: :string :integer :number :boolean :array :object

(defun %kebab-to-snake (string)
  "Convert kebab-case STRING to snake_case."
  (substitute #\_ #\- string))

(defun %symbol-to-json-name (sym)
  "Convert symbol SYM to a snake_case JSON key string."
  (%kebab-to-snake (string-downcase (symbol-name sym))))

(defun %parse-arg-spec (spec)
  "Parse an argument specification into a normalized plist."
  (etypecase spec
    (symbol
     (list :name spec
           :json-name (%symbol-to-json-name spec)
           :type :string
           :required nil
           :default nil
           :enum nil
           :description nil))
    (list
     (destructuring-bind (name &key json-name type required default enum description)
         spec
       (list :name name
             :json-name (or json-name (%symbol-to-json-name name))
             :type (or type :string)
             :required required
             :default default
             :enum enum
             :description description)))))

(defun %type-to-json-type (type)
  "Convert Lisp type keyword to JSON Schema type string."
  (ecase type
    (:string "string")
    (:integer "integer")
    (:number "number")
    (:boolean "boolean")
    (:array "array")
    (:object "object")))

(defun %generate-extraction-form (parsed-spec args-var)
  "Generate the extraction form for a parsed argument spec."
  (let ((name (getf parsed-spec :name))
        (json-name (getf parsed-spec :json-name))
        (type (getf parsed-spec :type))
        (required (getf parsed-spec :required))
        (default (getf parsed-spec :default)))
    (if (eq type :boolean)
        `(,name (extract-boolean-arg ,args-var ,json-name :default ,default))
        `(,name (extract-arg ,args-var ,json-name
                             :type ,type
                             :required ,required)))))

(defun %generate-schema-property (parsed-spec)
  "Generate schema property setter form for a parsed argument spec."
  (let ((json-name (getf parsed-spec :json-name))
        (type (getf parsed-spec :type))
        (enum (getf parsed-spec :enum))
        (description (getf parsed-spec :description)))
    `(setf (gethash ,json-name properties)
           (make-ht "type" ,(%type-to-json-type type)
                    ,@(when enum
                        `("enum" (vector ,@enum)))
                    ,@(when description
                        `("description" ,description))))))

(defun %collect-required-args (parsed-specs)
  "Return a list of JSON names for required arguments."
  (loop for spec in parsed-specs
        when (getf spec :required)
          collect (getf spec :json-name)))

;;;; Main Macro

(defmacro define-tool (name &key description args body)
  "Define an MCP tool with descriptor, handler, and registration.

NAME is the tool name string (e.g., \"fs-list-directory\").

DESCRIPTION is the tool description string for the schema.

ARGS is a list of argument specifications. Each spec can be:
  - A symbol: creates an optional string argument
  - A list: (name &key json-name type required default enum description)
    - name: Symbol used in BODY
    - json-name: JSON key (default: snake_case of name)
    - type: :string :integer :number :boolean :array :object
    - required: T if required
    - default: Default value (for :boolean only)
    - enum: List of allowed string values (e.g., '(\"a\" \"b\" \"c\"))
    - description: Schema description

BODY is the handler body. It has access to:
  - All argument names as local variables
  - ID: The request ID for result/error responses
  - STATE: The connection state

Example:
  (define-tool \"fs-list-directory\"
    :description \"List directory entries.\"
    :args ((path :type :string :required t :description \"Directory path\"))
    :body
    (let ((entries (fs-list-directory path)))
      (result id
              (make-ht \"entries\" entries
                       \"content\" (text-content \"OK\")))))"
  ;; Intern handler symbols in the caller's package for proper capture
  (let* ((parsed-specs (mapcar #'%parse-arg-spec args))
         (required-names (%collect-required-args parsed-specs))
         (handler-name (intern (format nil "~A-HANDLER" (string-upcase name)) *package*))
         (descriptor-name (intern (format nil "~A-DESCRIPTOR" (string-upcase name)) *package*))
         ;; Intern id/state/args in caller's package so body can reference them
         (id-sym (intern "ID" *package*))
         (state-sym (intern "STATE" *package*))
         (args-sym (intern "ARGS" *package*)))
    `(progn
       ;; Descriptor function
       (defun ,descriptor-name ()
         ,(format nil "Return the MCP tool descriptor for ~A." name)
         (let ((properties (make-hash-table :test #'equal)))
           ,@(mapcar #'%generate-schema-property parsed-specs)
           (make-ht "name" ,name
                    "description" ,description
                    "inputSchema"
                    (make-ht "type" "object"
                             "properties" properties
                             ,@(when required-names
                                 `("required" (vector ,@required-names)))))))

       ;; Handler function
       (defun ,handler-name (,state-sym ,id-sym ,args-sym)
         ,(format nil "Handle the ~A MCP tool call." name)
         (handler-case
             (let (,@(mapcar (lambda (spec)
                               (%generate-extraction-form spec args-sym))
                             parsed-specs))
               ,body)
           (arg-validation-error (e)
             (tool-error ,id-sym (validation-message e)
                         :protocol-version (protocol-version ,state-sym)))
           (error (e)
             (rpc-error ,id-sym -32603
                        (format nil "Internal error during ~A: ~A" ,name e)))))

       ;; Registration
       (register-tool ,name (,descriptor-name) #',handler-name)

       ;; Return the handler name for reference
       ',handler-name)))
