;;;; src/inspect.lisp

(defpackage #:cl-mcp/src/inspect
  (:use #:cl)
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p
                #:register-object
                #:lookup-object)
  (:import-from #:cl-mcp/src/utils/printing
                #:safe-prin1)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:inspect-object-by-id
           #:generate-result-preview))



(in-package #:cl-mcp/src/inspect)

(defun %type-name (object)
  "Return the type name of OBJECT as a string."
  (let ((type (type-of object)))
    (if (consp type)
        (symbol-name (car type))
        (symbol-name type))))

(defun %package-name-or-nil (symbol)
  "Return package name of SYMBOL or NIL if uninterned."
  (let ((pkg (symbol-package symbol)))
    (when pkg (package-name pkg))))

(defun %primitive-value-repr (object)
  "Return a hash-table representation of a primitive value."
  (cond
    ((numberp object)
     (make-ht "value" object
              "type" (%type-name object)))
    ((stringp object)
     (make-ht "value" object
              "type" (%type-name object)))
    ((characterp object)
     (make-ht "value" (string object)
              "type" "CHARACTER"))
    ((symbolp object)
     (let ((ht (make-ht "value" (symbol-name object)
                        "type" "SYMBOL")))
       (when (symbol-package object)
         (setf (gethash "package" ht) (package-name (symbol-package object))))
       ht))
    (t
     (make-ht "value" (safe-prin1 object)
              "type" (%type-name object)))))

(defun %make-object-ref (object visited-table)
  "Register OBJECT and return an object-ref hash-table.
VISITED-TABLE is used for circular reference detection."
  (let ((existing-id (gethash object visited-table)))
    (if existing-id
        ;; Circular reference
        (make-ht "kind" "circular-ref"
                 "ref_id" existing-id
                 "summary" (format nil "<circular to #~A>" existing-id))
        ;; New object reference
        (let ((id (register-object object)))
          (setf (gethash object visited-table) id)
          (make-ht "kind" "object-ref"
                   "id" id
                   "summary" (safe-prin1 object)
                   "type" (%type-name object))))))

(defun %value-repr (object visited-table depth max-depth max-elements)
  "Return representation of OBJECT, registering if inspectable.
Returns either a primitive value representation or an object-ref."
  (if (inspectable-p object)
      ;; DEPTH here is the parent depth. Expand nested objects only when the
      ;; child depth (DEPTH + 1) is still below MAX-DEPTH and we have not
      ;; already visited the object (to preserve circular-reference handling).
      (if (and (< (1+ depth) max-depth)
               (null (gethash object visited-table)))
          (%inspect-object-impl object visited-table (1+ depth) max-depth max-elements)
          (%make-object-ref object visited-table))
      (%primitive-value-repr object)))

;;; Type-specific inspection functions

(defun %inspect-cons (object visited-table depth max-depth max-elements)
  "Inspect a cons/list."
  (let ((elements '())
        (count 0)
        (truncated nil)
        (current object))
    ;; Handle proper lists and dotted lists
    (loop while (consp current)
          do (if (>= count max-elements)
                 (progn (setf truncated t)
                        (return))
                 (progn
                   (push (%value-repr (car current) visited-table depth max-depth max-elements)
                         elements)
                   (incf count)
                   (setf current (cdr current)))))
    ;; Handle dotted list tail
    (when (and current (not truncated))
      (push (make-ht "kind" "dotted-tail"
                     "value" (%value-repr current visited-table depth max-depth max-elements))
            elements))
    (let ((ht (make-ht "kind" "list"
                       "summary" (safe-prin1 object)
                       "elements" (nreverse elements))))
      (setf (gethash "meta" ht)
            (make-ht "length" (if truncated (format nil ">~A" max-elements) count)
                     "truncated" truncated
                     "max_elements" max-elements))
      ht)))

(defun %inspect-vector (object visited-table depth max-depth max-elements)
  "Inspect a vector."
  (let* ((len (length object))
         (limit (min len max-elements))
         (elements (loop for i from 0 below limit
                         collect (%value-repr (aref object i) visited-table depth max-depth max-elements)))
         (truncated (> len max-elements)))
    (let ((ht (make-ht "kind" "array"
                       "summary" (safe-prin1 object)
                       "element_type" (let ((et (array-element-type object)))
                                        (if (eq et t) "T" (prin1-to-string et)))
                       "dimensions" (list len)
                       "elements" elements)))
      (setf (gethash "meta" ht)
            (make-ht "total_elements" len
                     "truncated" truncated
                     "max_elements" max-elements))
      ht)))

(defun %inspect-array (object visited-table depth max-depth max-elements)
  "Inspect a multi-dimensional array."
  (let* ((dims (array-dimensions object))
         (total (array-total-size object))
         (limit (min total max-elements))
         (elements (loop for i from 0 below limit
                         collect (%value-repr (row-major-aref object i) visited-table depth max-depth max-elements)))
         (truncated (> total max-elements)))
    (let ((ht (make-ht "kind" "array"
                       "summary" (safe-prin1 object)
                       "element_type" (let ((et (array-element-type object)))
                                        (if (eq et t) "T" (prin1-to-string et)))
                       "dimensions" dims
                       "elements" elements)))
      (setf (gethash "meta" ht)
            (make-ht "total_elements" total
                     "truncated" truncated
                     "max_elements" max-elements))
      ht)))

(defun %inspect-hash-table (object visited-table depth max-depth max-elements)
  "Inspect a hash-table."
  (let ((entries '())
        (count 0)
        (truncated nil)
        (total (hash-table-count object)))
    (block collect
      (maphash (lambda (k v)
                 (when (>= count max-elements)
                   (setf truncated t)
                   (return-from collect))
                 (push (make-ht "key" (%value-repr k visited-table depth max-depth max-elements)
                                "value" (%value-repr v visited-table depth max-depth max-elements))
                       entries)
                 (incf count))
               object))
    (let ((ht (make-ht "kind" "hash-table"
                       "summary" (safe-prin1 object)
                       "test" (symbol-name (hash-table-test object))
                       "entries" (nreverse entries))))
      (setf (gethash "meta" ht)
            (make-ht "count" total
                     "truncated" truncated
                     "max_elements" max-elements))
      ht)))

(defun %inspect-function (object)
  "Inspect a function."
  (let ((name nil)
        (lambda-list nil))
    #+sbcl
    (handler-case
        (let ((fun-name-fn (find-symbol "%FUN-NAME" "SB-KERNEL"))
              (introspect-pkg (find-package "SB-INTROSPECT")))
          (when (and fun-name-fn (fboundp fun-name-fn))
            (setf name (funcall fun-name-fn object)))
          (when introspect-pkg
            (let ((lambda-list-fn (find-symbol "FUNCTION-LAMBDA-LIST" introspect-pkg)))
              (when (and lambda-list-fn (fboundp lambda-list-fn))
                (setf lambda-list (funcall lambda-list-fn object))))))
      (error () nil))
    (let ((ht (make-ht "kind" "function"
                       "summary" (safe-prin1 object))))
      (when name
        (setf (gethash "name" ht) (safe-prin1 name)))
      (when lambda-list
        (setf (gethash "lambda_list" ht) (safe-prin1 lambda-list)))
      ht)))

#+sbcl
(defun %sbcl-structure-p (object)
  "Check if OBJECT is an SBCL structure (not standard-object)."
  (and (not (typep object 'standard-object))
       (let ((class (class-of object)))
         (typep class 'structure-class))))

(defun %inspect-structure (object visited-table depth max-depth max-elements)
  "Inspect a structure."
  (let ((slots '())
        (class-name (%type-name object)))
    #+sbcl
    (handler-case
        ;; Use dynamic symbol lookup for SBCL internals to avoid reader errors
        (let* ((layout-of-fn (find-symbol "LAYOUT-OF" "SB-KERNEL"))
               (wrapper-info-fn (find-symbol "WRAPPER-INFO" "SB-KERNEL"))
               (wrapper-dd-fn (find-symbol "WRAPPER-DD" "SB-KERNEL"))
               (dd-slots-fn (find-symbol "DD-SLOTS" "SB-KERNEL"))
               (dsd-name-fn (find-symbol "DSD-NAME" "SB-KERNEL"))
               (dsd-accessor-fn (find-symbol "DSD-ACCESSOR-NAME" "SB-KERNEL")))
          (when (and layout-of-fn wrapper-info-fn wrapper-dd-fn
                     dd-slots-fn dsd-name-fn dsd-accessor-fn
                     (fboundp layout-of-fn) (fboundp wrapper-info-fn))
            (let ((layout (funcall wrapper-info-fn (funcall layout-of-fn object))))
              (when layout
                (let ((dd (funcall wrapper-dd-fn layout)))
                  (when dd
                    (dolist (dsd (funcall dd-slots-fn dd))
                      (let* ((slot-name (symbol-name (funcall dsd-name-fn dsd)))
                             (accessor (funcall dsd-accessor-fn dsd))
                             (value (handler-case
                                        (funcall accessor object)
                                      (error () :unbound))))
                        (push (make-ht "name" slot-name
                                       "value" (if (eq value :unbound)
                                                   (make-ht "kind" "unbound"
                                                            "summary" "#<unbound-slot>")
                                                   (%value-repr value visited-table depth max-depth max-elements)))
                              slots)))))))))
      (error () nil))  ; Silently ignore if internal API not available
    (let ((ht (make-ht "kind" "structure"
                       "class" class-name
                       "summary" (safe-prin1 object)
                       "slots" (nreverse slots))))
      (setf (gethash "meta" ht)
            (make-ht "slot_count" (length slots)))
      ht)))

(defun %inspect-instance (object visited-table depth max-depth max-elements)
  "Inspect a CLOS instance."
  (let ((slots '())
        (class (class-of object))
        (class-name (%type-name object)))
    #+sbcl
    (handler-case
        (let* ((mop-pkg (find-package "SB-MOP"))
               (class-slots-fn (when mop-pkg (find-symbol "CLASS-SLOTS" mop-pkg)))
               (slot-def-name-fn (when mop-pkg (find-symbol "SLOT-DEFINITION-NAME" mop-pkg))))
          (when (and class-slots-fn slot-def-name-fn
                     (fboundp class-slots-fn) (fboundp slot-def-name-fn))
            (dolist (slot (funcall class-slots-fn class))
              (let* ((slot-def-name (funcall slot-def-name-fn slot))
                     (slot-name (symbol-name slot-def-name))
                     (bound-p (slot-boundp object slot-def-name))
                     (value (if bound-p
                                (slot-value object slot-def-name)
                                :unbound)))
                (push (make-ht "name" slot-name
                               "value" (if (eq value :unbound)
                                           (make-ht "kind" "unbound"
                                                    "summary" "#<unbound-slot>")
                                           (%value-repr value visited-table depth max-depth max-elements)))
                      slots)))))
      (error () nil))
    #-sbcl
    nil  ; Non-SBCL fallback: no slot introspection
    (let ((ht (make-ht "kind" "instance"
                       "class" class-name
                       "summary" (safe-prin1 object)
                       "slots" (nreverse slots))))
      (setf (gethash "meta" ht)
            (make-ht "slot_count" (length slots)))
      ht)))

(defun %inspect-object-impl (object visited-table depth max-depth max-elements)
  "Internal implementation of object inspection."
  (cond
    ;; List/cons
    ((consp object)
     (%inspect-cons object visited-table depth max-depth max-elements))
    ;; Vector (1D array)
    ((and (arrayp object) (= 1 (array-rank object)))
     (%inspect-vector object visited-table depth max-depth max-elements))
    ;; Multi-dimensional array
    ((arrayp object)
     (%inspect-array object visited-table depth max-depth max-elements))
    ;; Hash-table
    ((hash-table-p object)
     (%inspect-hash-table object visited-table depth max-depth max-elements))
    ;; Function
    ((functionp object)
     (%inspect-function object))
    ;; Structure (SBCL-specific check)
    #+sbcl
    ((%sbcl-structure-p object)
     (%inspect-structure object visited-table depth max-depth max-elements))
    ;; CLOS instance
    ((typep object 'standard-object)
     (%inspect-instance object visited-table depth max-depth max-elements))
    ;; Fallback: other types
    (t
     (make-ht "kind" "other"
              "summary" (safe-prin1 object)
              "type" (%type-name object)))))

(defun inspect-object-by-id (id &key (max-depth 1) (max-elements 50))
  "Inspect object by ID from the registry.
Returns a hash-table with inspection results or error info."
  (let ((object (lookup-object id)))
    (if object
        (let ((visited (make-hash-table :test 'eq))
              (result nil))
          ;; Mark the root object as visited with its ID
          (setf (gethash object visited) id)
          (setf result (%inspect-object-impl object visited 0 max-depth max-elements))
          (setf (gethash "id" result) id)
          result)
        (make-ht "error" t
                 "code" "OBJECT_NOT_FOUND"
                 "message" (format nil "Object ID ~A not found (may have been evicted from cache)" id)))))


(defun generate-result-preview (object &key (max-depth 1) (max-elements 8))
  "Generate a lightweight preview of OBJECT for inclusion in repl-eval response.
Unlike inspect-object-by-id, this takes a raw object (not an ID) and registers it.
Returns a hash-table with:
  - id: the registered object ID
  - kind, type, summary, etc.: structural preview
  - truncated: T if elements were omitted due to max-elements limit
For nested non-primitive values, id fields are included for drill-down."
  (let* ((id (register-object object))
         (visited (make-hash-table :test 'eq))
         (result nil))
    (setf (gethash object visited) id)
    (setf result
            (%inspect-object-impl object visited 0 max-depth max-elements))
    (setf (gethash "id" result) id)
    result))

;;; MCP Tool Definition

(define-tool "inspect-object"
  :description "Inspect an object's internal structure by ID.
Objects are registered when repl-eval returns non-primitive values (result_object_id field).
Use this to drill down into complex data structures like CLOS instances, structures, lists, arrays, and hash-tables."
  :args ((object-id :type :integer :json-name "id" :required t
                    :description "Object ID from repl-eval result_object_id or previous inspection")
         (max-depth :type :integer :json-name "max_depth"
                    :description "Nesting depth for expansion (0=summary only, default=1)")
         (max-elements :type :integer :json-name "max_elements"
                       :description "Maximum elements for lists/arrays/hash-tables (default=50)"))
  :body
  (let ((inspection-result (inspect-object-by-id object-id
                                                  :max-depth (or max-depth 1)
                                                  :max-elements (or max-elements 50))))
    (if (gethash "error" inspection-result)
        ;; Return error using result function with isError flag
        (let ((error-ht (make-ht "content" (text-content (gethash "message" inspection-result))
                                 "isError" t)))
          (result id error-ht))
        ;; Return successful inspection with content field for display
        (let ((summary (format nil "[~A] ~A"
                               (gethash "kind" inspection-result)
                               (gethash "summary" inspection-result))))
          (setf (gethash "content" inspection-result) (text-content summary))
          (result id inspection-result)))))
