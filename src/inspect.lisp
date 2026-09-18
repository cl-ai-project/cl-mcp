;;;; src/inspect.lisp

(defpackage #:cl-mcp/src/inspect
  (:use #:cl)
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p
                #:register-object
                #:lookup-object)
  (:import-from #:cl-mcp/src/code-refs-core
                #:qualified-symbol-name)
  (:import-from #:cl-mcp/src/utils/printing
                #:safe-prin1)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:result)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:inspect-object-by-id
           #:generate-result-preview
           #:format-inspect-elements))

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

(defun %ensure-object-id (object seen-table)
  "Return OBJECT's ID from SEEN-TABLE, registering it if needed."
  (or (gethash object seen-table)
      (let ((id (register-object object)))
        (setf (gethash object seen-table) id)
        id)))

(defun %make-object-ref (object seen-table &key circular-p)
  "Return a reference hash-table for OBJECT.
When CIRCULAR-P is true, returns a circular reference marker."
  (let ((id (%ensure-object-id object seen-table)))
    (if circular-p
        (make-ht "kind" "circular-ref"
                 "ref_id" id
                 "summary" (format nil "<circular to #~A>" id))
        (make-ht "kind" "object-ref"
                 "id" id
                 "summary" (safe-prin1 object)
                 "type" (%type-name object)))))

(defun %value-repr (object seen-table active-table depth max-depth max-elements)
  "Return representation of OBJECT, registering if inspectable.
Returns either a primitive value representation, an object-ref, or a circular-ref.

An expanded result carries its \"id\" just as an object-ref does.  Without it,
raising MAX-DEPTH removed the very handles it was raised to reach: at depth 1 a
slot came back as an object-ref and showed [object-id: N], while at depth 2 the
same slot was expanded in place and showed none, so the deeper view was the one
you could not drill into."
  (if (inspectable-p object)
      (let ((child-depth (1+ depth)))
        (cond
          ;; Object appears on the active path: true cycle.
          ((gethash object active-table)
           (%make-object-ref object seen-table :circular-p t))
          ;; Respect depth boundary by returning a reference.
          ((>= child-depth max-depth)
           (%make-object-ref object seen-table))
          ;; Already seen elsewhere: shared reference, not a cycle.
          ((gethash object seen-table)
           (%make-object-ref object seen-table))
          ;; First time and depth allows expansion.
          (t
           (let ((id (%ensure-object-id object seen-table))
                 (expanded (%inspect-object-impl object seen-table active-table
                                                 child-depth max-depth
                                                 max-elements)))
             (when (hash-table-p expanded)
               (setf (gethash "id" expanded) id))
             expanded))))
      (%primitive-value-repr object)))

;;; Type-specific inspection functions

(defun %inspect-cons
       (object seen-table active-table depth max-depth max-elements)
  "Inspect a cons/list.  Detects circular CDR chains to prevent infinite loops."
  (let ((elements nil)
        (count 0)
        (truncated nil)
        (current object)
        (cdr-seen (make-hash-table :test #'eq)))
    (setf (gethash object cdr-seen) t)
    (loop while (consp current)
          do (if (>= count max-elements)
                 (progn (setf truncated t) (return))
                 (progn
                  (push
                   (%value-repr (car current) seen-table active-table depth
                                max-depth max-elements)
                   elements)
                  (incf count)
                  (setf current (cdr current))
                  (when (and (consp current) (gethash current cdr-seen))
                    (setf truncated t)
                    (return))
                  (when (consp current)
                    (setf (gethash current cdr-seen) t)))))
    (when (and current (not truncated))
      (push
       (make-ht "kind" "dotted-tail" "value"
                (%value-repr current seen-table active-table depth max-depth
                             max-elements))
       elements))
    (let ((ht
           (make-ht "kind" "list" "summary" (safe-prin1 object) "elements"
                    (nreverse elements))))
      (setf (gethash "meta" ht)
              (make-ht "length"
                       (if truncated
                           (format nil ">~A" max-elements)
                           count)
                       "truncated" truncated "max_elements" max-elements))
      ht)))

(defun %inspect-vector (object seen-table active-table depth max-depth max-elements)
  "Inspect a vector."
  (let* ((len (length object))
         (limit (min len max-elements))
         (elements (loop for i from 0 below limit
                         collect (%value-repr (aref object i) seen-table active-table
                                              depth max-depth max-elements)))
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

(defun %inspect-array (object seen-table active-table depth max-depth max-elements)
  "Inspect a multi-dimensional array."
  (let* ((dims (array-dimensions object))
         (total (array-total-size object))
         (limit (min total max-elements))
         (elements (loop for i from 0 below limit
                         collect (%value-repr (row-major-aref object i)
                                              seen-table active-table
                                              depth max-depth max-elements)))
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

(defun %inspect-hash-table (object seen-table active-table depth max-depth max-elements)
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
                 (push (make-ht "key" (%value-repr k seen-table active-table
                                                    depth max-depth max-elements)
                                "value" (%value-repr v seen-table active-table
                                                      depth max-depth max-elements))
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

(defun %inspect-structure
    (object seen-table active-table depth max-depth max-elements)
  "Inspect a structure."
  (let ((slots '())
        (class-name (%type-name object)))
    #+sbcl
    (let ((layout-of-fn (find-symbol "LAYOUT-OF" "SB-KERNEL"))
          (dd-slots-fn (find-symbol "DD-SLOTS" "SB-KERNEL"))
          (dsd-name-fn (find-symbol "DSD-NAME" "SB-KERNEL"))
          (dsd-accessor-fn (find-symbol "DSD-ACCESSOR-NAME" "SB-KERNEL")))
      ;; Modern SBCL path: layout-of → layout-info → dd-slots.
      ;; Isolated in its own handler-case so errors here do not
      ;; prevent the legacy fallback from running.
      (handler-case
          (let ((layout-info-fn (find-symbol "LAYOUT-INFO" "SB-KERNEL")))
            (when (and layout-of-fn layout-info-fn dd-slots-fn
                       dsd-name-fn dsd-accessor-fn
                       (fboundp layout-of-fn) (fboundp layout-info-fn))
              (let* ((layout (funcall layout-of-fn object))
                     (dd (funcall layout-info-fn layout)))
                (when dd
                  (dolist (dsd (funcall dd-slots-fn dd))
                    (let* ((slot-name (symbol-name (funcall dsd-name-fn dsd)))
                           (accessor (funcall dsd-accessor-fn dsd))
                           (value (handler-case (funcall accessor object)
                                    (error () :unbound))))
                      (push (make-ht "name" slot-name
                                     "value"
                                     (if (eq value :unbound)
                                         (make-ht "kind" "unbound"
                                                  "summary" "#<unbound-slot>")
                                         (%value-repr value seen-table
                                                      active-table depth
                                                      max-depth max-elements)))
                            slots)))))))
        (error () nil))
      ;; Legacy SBCL path: layout-of → wrapper-info → wrapper-dd → dd-slots.
      ;; Only tried when the modern path produced no slots.
      (when (null slots)
        (handler-case
            (let ((wrapper-info-fn (find-symbol "WRAPPER-INFO" "SB-KERNEL"))
                  (wrapper-dd-fn (find-symbol "WRAPPER-DD" "SB-KERNEL")))
              (when (and wrapper-info-fn wrapper-dd-fn layout-of-fn
                         dd-slots-fn dsd-name-fn dsd-accessor-fn
                         (fboundp layout-of-fn) (fboundp wrapper-info-fn))
                (let ((layout (funcall wrapper-info-fn
                                       (funcall layout-of-fn object))))
                  (when layout
                    (let ((dd (funcall wrapper-dd-fn layout)))
                      (when dd
                        (dolist (dsd (funcall dd-slots-fn dd))
                          (let* ((slot-name
                                   (symbol-name (funcall dsd-name-fn dsd)))
                                 (accessor (funcall dsd-accessor-fn dsd))
                                 (value
                                   (handler-case (funcall accessor object)
                                     (error () :unbound))))
                            (push (make-ht
                                   "name" slot-name
                                   "value"
                                   (if (eq value :unbound)
                                       (make-ht "kind" "unbound"
                                                "summary" "#<unbound-slot>")
                                       (%value-repr value seen-table
                                                    active-table depth
                                                    max-depth max-elements)))
                                  slots)))))))))
          (error () nil))))
    (let ((ht (make-ht "kind" "structure"
                       "class" class-name
                       "summary" (safe-prin1 object)
                       "slots" (nreverse slots))))
      (setf (gethash "meta" ht) (make-ht "slot_count" (length slots)))
      ht)))

(defun %inspect-instance (object seen-table active-table depth max-depth max-elements)
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
                                           (%value-repr value seen-table active-table
                                                        depth max-depth max-elements)))
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

(defun %inspect-object-impl (object seen-table active-table depth max-depth max-elements)
  "Internal implementation of object inspection."
  (setf (gethash object active-table) t)
  (unwind-protect
       (cond
         ;; List/cons
         ((consp object)
          (%inspect-cons object seen-table active-table depth max-depth max-elements))
         ;; Vector (1D array)
         ((and (arrayp object) (= 1 (array-rank object)))
          (%inspect-vector object seen-table active-table depth max-depth max-elements))
         ;; Multi-dimensional array
         ((arrayp object)
          (%inspect-array object seen-table active-table depth max-depth max-elements))
         ;; Hash-table
         ((hash-table-p object)
          (%inspect-hash-table object seen-table active-table depth max-depth max-elements))
         ;; Function
         ((functionp object)
          (%inspect-function object))
         ;; Structure (SBCL-specific check)
         #+sbcl
         ((%sbcl-structure-p object)
          (%inspect-structure object seen-table active-table depth max-depth max-elements))
         ;; CLOS instance
         ((typep object 'standard-object)
          (%inspect-instance object seen-table active-table depth max-depth max-elements))
         ;; Fallback: other types
         (t
          (make-ht "kind" "other"
                   "summary" (safe-prin1 object)
                   "type" (%type-name object))))
    (remhash object active-table)))

(defun %clos-hint (object)
  "Return the line pointing at clos-describe for OBJECT, or NIL.
OBJECT qualifies when it is the class its symbol names, or the generic function
its name -- a symbol or (SETF symbol) -- names.  inspect-object shows such an
object's internal representation; clos-describe describes the class or generic
function itself.  Anything else, an anonymous or replaced class included, gets
no hint, and so does an object whose check signals."
  (ignore-errors
   (cond
     ((typep object 'class)
      (let ((name (class-name object)))
        (when (and name (symbolp name) (eq (find-class name nil) object))
          (let ((qualified (qualified-symbol-name name)))
            (format nil "This is the class ~A; clos-describe ~A shows its slots, ~
superclasses, subclasses and methods with source lines."
                    qualified qualified)))))
     ((typep object 'generic-function)
      (let* ((name (sb-mop:generic-function-name object))
             (setf-p (and (consp name) (eq (first name) 'setf)))
             (base (if setf-p (second name) name)))
        (when (and base (symbolp base) (fboundp name) (eq (fdefinition name) object))
          (format nil "This is the generic function ~:[~A~;(SETF ~A)~]; clos-describe ~A ~
lists its methods with their specializers and source lines."
                  setf-p (qualified-symbol-name base) (qualified-symbol-name base))))))))

(defun inspect-object-by-id (id &key (max-depth 1) (max-elements 50))
  "Inspect object by ID from the registry.
Returns a hash-table with inspection results or error info."
  (let ((object (lookup-object id)))
    (if object
        (handler-case
            (let ((seen (make-hash-table :test 'eq))
                  (active (make-hash-table :test 'eq))
                  (result nil))
              ;; Seed root ID so child references to root point to the caller-visible ID.
              (setf (gethash object seen) id)
              (setf result (%inspect-object-impl object seen active 0 max-depth max-elements))
              (setf (gethash "id" result) id)
              ;; Only the object asked about gets a hint, never its elements.
              (let ((hint (%clos-hint object)))
                (when hint
                  (setf (gethash "hint" result) hint)))
              result)
          (serious-condition (e)
            (make-ht "error" t
                     "code" "INSPECTION_FAILED"
                     "message" (format nil
                                       "Cannot inspect object ID ~A: ~A (object may have been garbage-collected)"
                                       id e))))
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
  (let ((id (register-object object))
         (seen (make-hash-table :test 'eq))
         (active (make-hash-table :test 'eq))
         (result nil))
    (setf (gethash object seen) id)
    (setf result
          (%inspect-object-impl object seen active 0 max-depth max-elements))
    (setf (gethash "id" result) id)
    result))

(defparameter *inspect-format-max-depth* 8
  "How deep %WRITE-INSPECT-BODY will recurse into an already-generated preview.

The preview tree is finite -- %VALUE-REPR stops at its own MAX-DEPTH and emits
a circular-ref rather than revisiting an object on the active path -- so this
is not what terminates the walk.  It is a guard for a renderer that is also
handed data decoded from a worker's JSON, where nothing in this process
produced the shape.")

(defun %inspect-repr-text (repr)
  "Return readable text for REPR, a value representation from %VALUE-REPR.
A primitive carries its printed form under \"value\"; anything else -- an
expanded node, an object-ref, a circular-ref -- is named by its \"summary\".
A REPR that is not a hash-table at all is printed as it stands."
  (if (hash-table-p repr)
      (let ((v (gethash "value" repr)))
        (if (and v (not (hash-table-p v)))
            (princ-to-string v)
            (or (gethash "summary" repr) "?")))
      (princ-to-string repr)))

(defun %inspect-expandable-p (repr)
  "True when REPR is a node whose own contents were already expanded.

An object-ref and a circular-ref carry a summary and an id and nothing else,
so they answer false and the walk stops there -- which is exactly where
%VALUE-REPR decided the expansion should stop, whether for depth, for sharing
or for a cycle."
  (and (hash-table-p repr)
       (flet ((filled (key)
                (let ((v (gethash key repr)))
                  (and v (plusp (length v))))))
         (or (filled "elements") (filled "entries") (filled "slots")))))

(defun %write-inspect-body (stream node indent depth)
  "Write NODE's elements, entries, slots and truncation note to STREAM, each
line indented INDENT spaces, recursing into a child that was itself expanded.

The recursion is what makes a raised max_depth visible.  %VALUE-REPR already
builds the nested node -- a hash-table inside a hash-table comes back with its
own \"entries\" -- and this used to print only its summary, so asking for more
depth changed the JSON and nothing a reader could see.  Nothing is inspected
again here; only what was already generated is written out.

A composite hash-table KEY is named with its object-id rather than expanded:
its structure under the row it keys reads as the value's."
  (let ((pad (make-string indent :initial-element #\Space))
        (item-pad (make-string (+ indent 2) :initial-element #\Space)))
    (flet ((nested (repr)
             (when (and (< depth *inspect-format-max-depth*)
                        (%inspect-expandable-p repr))
               (%write-inspect-body stream repr (+ indent 4) (1+ depth)))))
      (let ((elements (gethash "elements" node)))
        (when (and elements (plusp (length elements)))
          (format stream "~&~AElements:" pad)
          (loop for el in (coerce elements 'list)
                for i from 0
                do (if (hash-table-p el)
                       ;; REF_ID as well as ID: a circular-ref carries its
                       ;; handle under "ref_id", and it is the one nested kind
                       ;; that would otherwise render with no marker at all --
                       ;; conspicuous now that its siblings have one.
                       (progn
                         (format stream "~&~A[~D] ~A~@[ [object-id: ~A]~]"
                                 item-pad i (%inspect-repr-text el)
                                 (or (gethash "id" el) (gethash "ref_id" el)))
                         (nested el))
                       (format stream "~&~A[~D] ~A" item-pad i el)))))
      (let ((entries (gethash "entries" node)))
        (when (and entries (plusp (length entries)))
          (format stream "~&~AEntries (~A test):" pad
                  (or (gethash "test" node) "EQL"))
          (loop for entry in (coerce entries 'list)
                do (when (hash-table-p entry)
                     (let ((k (gethash "key" entry))
                           (v (gethash "value" entry)))
                       (format stream
                               "~&~A~A~@[ [key-object-id: ~A]~] => ~A~@[ [object-id: ~A]~]"
                               item-pad
                               (%inspect-repr-text k)
                               (when (hash-table-p k)
                                 (or (gethash "id" k) (gethash "ref_id" k)))
                               (%inspect-repr-text v)
                               (when (hash-table-p v)
                                 (or (gethash "id" v) (gethash "ref_id" v))))
                       (nested v))))))
      (let ((slots (gethash "slots" node)))
        (when (and slots (plusp (length slots)))
          (format stream "~&~ASlots:" pad)
          (loop for slot in (coerce slots 'list)
                do (when (hash-table-p slot)
                     (let ((v (gethash "value" slot)))
                       (format stream "~&~A~A: ~A~@[ [object-id: ~A]~]"
                               item-pad
                               (gethash "name" slot "?")
                               (%inspect-repr-text v)
                               (when (hash-table-p v)
                                 (or (gethash "id" v) (gethash "ref_id" v))))
                       (nested v))))))
      (let ((meta (gethash "meta" node)))
        (when (and meta (hash-table-p meta) (gethash "truncated" meta))
          ;; Different inspectors use different keys for total count:
          ;; list="length", vector/array="total_elements", hash-table="count"
          (let ((total (or (gethash "total_elements" meta)
                           (gethash "count" meta)
                           (gethash "length" meta))))
            (format stream "~&~A... (truncated, ~A total)" item-pad total)))))))

(defun format-inspect-elements (inspection-result &key (header t))
  "Format structured inspection data as human-readable text lines.

HEADER, true by default, writes the leading kind/summary, object-id and hint
lines.  A caller that has already named the object -- repl-eval's backtrace,
which prints a local's name, value and object-id on its own line before
expanding it -- passes NIL and gets only the body: the elements, entries,
slots and truncation note.  The result is then the empty string for an object
whose preview has no body, so a caller must check before writing it.

A child that was itself expanded is written out under its own row
(%WRITE-INSPECT-BODY), so raising max_depth shows more here and not only in
the JSON."
  (with-output-to-string (s)
    (when header
      (format s "[~A] ~A"
              (gethash "kind" inspection-result)
              (gethash "summary" inspection-result))
      (when (gethash "id" inspection-result)
        (format s "~&[object-id: ~A]" (gethash "id" inspection-result)))
      (when (gethash "hint" inspection-result)
        (format s "~&Hint: ~A" (gethash "hint" inspection-result))))
    (%write-inspect-body s inspection-result 0 0)))

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
  (with-proxy-dispatch (id "worker/inspect-object"
                          (make-ht "id" object-id
                                   "max_depth" max-depth
                                   "max_elements" max-elements))
    (let ((inspection-result (inspect-object-by-id object-id
                                                    :max-depth (or max-depth 1)
                                                    :max-elements (or max-elements 50))))
      (if (gethash "error" inspection-result)
          (result id (make-ht "isError" t
                              "content" (text-content
                                         (gethash "message" inspection-result))))
          (progn
            (setf (gethash "content" inspection-result)
                  (text-content (format-inspect-elements inspection-result)))
            (result id inspection-result))))))
