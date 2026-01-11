;;;; src/object-registry.lisp
;;;;
;;;; FIFO cache for object inspection.
;;;; Stores objects with integer IDs for later inspection via inspect-object tool.

(defpackage #:cl-mcp/src/object-registry
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:*object-registry*
           #:inspectable-p
           #:register-object
           #:lookup-object
           #:clear-registry
           #:registry-count))

(in-package #:cl-mcp/src/object-registry)

;;; Configuration

(defconstant +max-registry-size+ 1000
  "Maximum number of objects to keep in the registry.")

;;; Registry Structure

(defstruct (object-registry (:constructor %make-object-registry))
  "FIFO cache for inspectable objects."
  (storage (make-hash-table :test 'eql) :type hash-table)
  (history (make-array +max-registry-size+ :initial-element nil) :type simple-vector)
  (head 0 :type fixnum)  ; Next position to write
  (count 0 :type fixnum) ; Number of items in history
  (next-id 1 :type integer)
  (lock (bt:make-lock "object-registry")))

(defun make-object-registry ()
  "Create a new object registry."
  (%make-object-registry))

(defvar *object-registry* (make-object-registry)
  "Global registry for inspectable objects.")

;;; Predicates

(defun inspectable-p (object)
  "Return T if OBJECT should be registered for inspection.
Primitives (numbers, strings, symbols, characters) are excluded."
  (not (or (numberp object)
           (stringp object)
           (symbolp object)
           (characterp object))))

;;; Registry Operations

(defun %evict-oldest (registry)
  "Remove the oldest entry from REGISTRY. Called with lock held."
  (let* ((history (object-registry-history registry))
         (storage (object-registry-storage registry))
         (head (object-registry-head registry))
         (count (object-registry-count registry)))
    (when (>= count +max-registry-size+)
      ;; Calculate position of oldest entry (FIFO)
      (let* ((oldest-pos (mod head +max-registry-size+))
             (oldest-id (aref history oldest-pos)))
        (when oldest-id
          (remhash oldest-id storage)
          (setf (aref history oldest-pos) nil))))))

(defun register-object (object &optional (registry *object-registry*))
  "Register OBJECT in REGISTRY and return its ID.
Returns NIL if OBJECT is a primitive (not inspectable).
Evicts oldest entry if registry is full."
  (unless (inspectable-p object)
    (return-from register-object nil))
  (bt:with-lock-held ((object-registry-lock registry))
    (let* ((storage (object-registry-storage registry))
           (history (object-registry-history registry))
           (id (object-registry-next-id registry))
           (head (object-registry-head registry))
           (count (object-registry-count registry)))
      ;; Evict if full
      (when (>= count +max-registry-size+)
        (%evict-oldest registry))
      ;; Register new object
      (setf (gethash id storage) object)
      (setf (aref history head) id)
      ;; Update registry state
      (setf (object-registry-head registry)
            (mod (1+ head) +max-registry-size+))
      (setf (object-registry-count registry)
            (min (1+ count) +max-registry-size+))
      (incf (object-registry-next-id registry))
      id)))

(defun lookup-object (id &optional (registry *object-registry*))
  "Look up object by ID in REGISTRY.
Returns two values: the object (or NIL) and a boolean indicating if found."
  (bt:with-lock-held ((object-registry-lock registry))
    (multiple-value-bind (object found-p)
        (gethash id (object-registry-storage registry))
      (values object found-p))))

(defun clear-registry (&optional (registry *object-registry*))
  "Remove all objects from REGISTRY."
  (bt:with-lock-held ((object-registry-lock registry))
    (clrhash (object-registry-storage registry))
    (fill (object-registry-history registry) nil)
    (setf (object-registry-head registry) 0
          (object-registry-count registry) 0)
    t))

(defun registry-count (&optional (registry *object-registry*))
  "Return the number of objects currently in REGISTRY."
  (bt:with-lock-held ((object-registry-lock registry))
    (object-registry-count registry)))
