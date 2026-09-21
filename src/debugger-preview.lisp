(defpackage #:cl-mcp/src/debugger-preview
  (:use #:cl)
  (:import-from #:cl-mcp/src/object-registry #:inspectable-p #:register-object)
  (:export #:generate-debugger-preview))

(in-package #:cl-mcp/src/debugger-preview)

(declaim (optimize (debug 3) (safety 3)))

(defun %table (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(defun %type-name (object)
  (let ((type (type-of object)))
    (symbol-name (if (consp type) (car type) type))))

(defun %print-preview (object)
  ;; Match ordinary preview bounds, but never recover around user printers.
  (let ((*print-level* 3) (*print-length* 10)
        (*print-readably* nil) (*print-circle* nil))
    (prin1-to-string object)))

(defun %primitive (object)
  (let ((result (%table "type" (if (characterp object) "CHARACTER" (%type-name object))
                        "value" (typecase object
                                  (symbol (symbol-name object))
                                  (character (string object))
                                  (t object)))))
    (when (symbolp object)
      (setf (gethash "type" result) "SYMBOL")
      (when (symbol-package object)
        (setf (gethash "package" result) (package-name (symbol-package object)))))
    result))

(defun %function-preview (object)
  (let ((result (%table "kind" "function" "summary" (%print-preview object))))
    #+sbcl
    (let* ((name (sb-kernel:%fun-name object))
           (package (find-package "SB-INTROSPECT"))
           (accessor (and package (find-symbol "FUNCTION-LAMBDA-LIST" package))))
      (when name
        (setf (gethash "name" result) (%print-preview name)))
      (when (and accessor (fboundp accessor))
        (let ((lambda-list (funcall accessor object)))
          (when lambda-list
            (setf (gethash "lambda_list" result) (%print-preview lambda-list))))))
    result))

(defun generate-debugger-preview (object &key (max-depth 1) (max-elements 8))
  "Collect an object preview only under a debugger diagnostic boundary.

This collector has no printer or inspector recovery handlers: every secondary
condition must reach the caller's diagnostic handler or secondary debugger hook.
It depends only on the registry, not on the ordinary inspector's tool graph."
  (let ((seen (make-hash-table :test #'eq))
        (active (make-hash-table :test #'eq)))
    (labels
        ((id-for (value)
           (or (gethash value seen)
               (setf (gethash value seen) (register-object value))))
         (reference (value &optional circular-p)
           (let ((id (id-for value)))
             (if circular-p
                 (%table "kind" "circular-ref" "ref_id" id
                         "summary" (format nil "<circular to #~A>" id))
                 (%table "kind" "object-ref" "id" id
                         "summary" (%print-preview value) "type" (%type-name value)))))
         (child (value depth)
           (cond
             ((not (inspectable-p value)) (%primitive value))
             ((gethash value active) (reference value t))
             ((or (>= (1+ depth) max-depth) (gethash value seen)) (reference value))
             (t (expand value (1+ depth)))))
         (list-preview (value depth)
           (let ((elements nil) (count 0) (truncated nil)
                 (current value) (cdr-seen (make-hash-table :test #'eq)))
             (loop while (consp current)
                   do (when (or (>= count max-elements) (gethash current cdr-seen))
                        (setf truncated t)
                        (return))
                      (setf (gethash current cdr-seen) t)
                      (push (child (car current) depth) elements)
                      (incf count)
                      (setf current (cdr current)))
             (when (and current (not truncated))
               (push (%table "kind" "dotted-tail" "value" (child current depth)) elements))
             (%table "kind" "list" "summary" (%print-preview value)
                     "elements" (nreverse elements)
                     "meta" (%table "length" (if truncated
                                                 (format nil ">~A" max-elements) count)
                                    "truncated" truncated "max_elements" max-elements))))
         (array-preview (value depth)
           (let* ((total (if (vectorp value) (length value) (array-total-size value)))
                  (limit (min total max-elements)))
             (%table "kind" "array" "summary" (%print-preview value)
                     "element_type" (prin1-to-string (array-element-type value))
                     "dimensions" (if (vectorp value) (list total) (array-dimensions value))
                     "elements" (loop for i below limit
                                      collect (child (row-major-aref value i) depth))
                     "meta" (%table "total_elements" total "truncated" (> total limit)
                                    "max_elements" max-elements))))
         (hash-preview (value depth)
           (let ((entries nil) (count 0))
             (block collect
               (maphash (lambda (key item)
                          (when (>= count max-elements) (return-from collect))
                          (push (%table "key" (child key depth) "value" (child item depth))
                                entries)
                          (incf count))
                        value))
             (%table "kind" "hash-table" "summary" (%print-preview value)
                     "test" (symbol-name (hash-table-test value)) "entries" (nreverse entries)
                     "meta" (%table "count" (hash-table-count value)
                                    "truncated" (> (hash-table-count value) count)
                                    "max_elements" max-elements))))
         (instance-preview (value depth structure-p)
           (let ((slots nil))
             #+sbcl
             (dolist (slot (sb-mop:class-slots (class-of value)))
               (let ((name (sb-mop:slot-definition-name slot)))
                 (push (%table "name" (symbol-name name)
                               "value" (if (slot-boundp value name)
                                           (child (slot-value value name) depth)
                                           (%table "kind" "unbound"
                                                   "summary" "#<unbound-slot>")))
                       slots)))
             (%table "kind" (if structure-p "structure" "instance")
                     "class" (%type-name value) "summary" (%print-preview value)
                     "slots" (nreverse slots) "meta" (%table "slot_count" (length slots)))))
         (expand (value depth)
           (let ((id (id-for value)))
             (setf (gethash value active) t)
             (unwind-protect
                  (let ((result
                          (cond
                            ((consp value) (list-preview value depth))
                            ((arrayp value) (array-preview value depth))
                            ((hash-table-p value) (hash-preview value depth))
                            ((functionp value) (%function-preview value))
                            ((typep value 'structure-object) (instance-preview value depth t))
                            ((typep value 'standard-object) (instance-preview value depth nil))
                            (t (%table "kind" "other" "summary" (%print-preview value)
                                       "type" (%type-name value))))))
                    (setf (gethash "id" result) id)
                    result)
               (remhash value active)))))
      (expand object 0))))
