;;;; src/object-registry.lisp
;;;;
;;;; FIFO cache for object inspection.
;;;; Stores objects under opaque string handles for later inspection via the
;;;; inspect-object tool.
;;;;
;;;; A handle names its registry's generation as well as the object:
;;;; "o-<generation>-<n>".  A generation is drawn at random the first time a
;;;; registry hands out a handle, and again whenever the registry is cleared,
;;;; so a handle from an earlier worker image -- a worker that crashed, was
;;;; killed or retired, and was replaced by a fresh one numbering its objects
;;;; from 1 again -- or from before a clear, is recognized as stale and
;;;; refused.  It never names whatever object the new image happens to hold
;;;; under the same number.

(defpackage #:cl-mcp/src/object-registry
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:cl-mcp/src/utils/random
                #:generate-random-hex-string)
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
  ;; Drawn when the first handle is handed out rather than when the registry
  ;; is made: a registry made at load time and saved in a core would
  ;; otherwise give every image started from that core the same generation.
  (generation nil)
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

;;; Handles

(defun %new-generation ()
  "Return a fresh generation token."
  (generate-random-hex-string 6))

(defun %generation (registry)
  "Return REGISTRY's generation, drawing one first if it has none.  Called
with the lock held."
  (or (object-registry-generation registry)
      (setf (object-registry-generation registry) (%new-generation))))

(defun %handle (generation number)
  "Return the handle naming object NUMBER of GENERATION."
  (format nil "o-~A-~D" generation number))

(defun %parse-handle (handle)
  "Return HANDLE's generation and number as two values, or NIL when HANDLE is
not a handle's shape."
  (when (and (stringp handle) (> (length handle) 2)
             (string= "o-" handle :end2 2))
    (let ((dash (position #\- handle :start 2)))
      (when (and dash (> dash 2))
        (let ((number (ignore-errors
                       (parse-integer handle :start (1+ dash)))))
          (when (and number (plusp number))
            (values (subseq handle 2 dash) number)))))))

;;; Registry Operations

(defun %evict-oldest (registry)
  "Remove the oldest entry from REGISTRY. Called with lock held."
  (let ((history (object-registry-history registry))
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
  "Register OBJECT in REGISTRY and return its handle, a string.
Returns NIL if OBJECT is a primitive (not inspectable).
Evicts oldest entry if registry is full."
  (unless (inspectable-p object)
    (return-from register-object nil))
  (bt:with-lock-held ((object-registry-lock registry))
    (let ((storage (object-registry-storage registry))
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
      (%handle (%generation registry) id))))

(defun lookup-object (handle &optional (registry *object-registry*))
  "Look up the object HANDLE names in REGISTRY.
Returns three values: the object (or NIL), a boolean saying whether it was
found, and why not when it was not:

  :STALE    HANDLE was issued by another generation -- an earlier worker
            image, or this registry before it was cleared.  The object it
            named is gone with that state; nothing here stands for it.
  :EVICTED  HANDLE is this generation's, and its object has since been
            dropped to make room.
  :INVALID  HANDLE is not a handle at all -- an integer id from a version
            that numbered objects, say."
  (multiple-value-bind (generation number) (%parse-handle handle)
    (unless generation
      (return-from lookup-object (values nil nil :invalid)))
    (bt:with-lock-held ((object-registry-lock registry))
      (unless (equal generation (object-registry-generation registry))
        (return-from lookup-object (values nil nil :stale)))
      (multiple-value-bind (object found-p)
          (gethash number (object-registry-storage registry))
        (if found-p
            (values object t nil)
            (values nil nil :evicted))))))

(defun clear-registry (&optional (registry *object-registry*))
  "Remove all objects from REGISTRY, and start a new generation: every handle
it has handed out is stale from now on, rather than free to name an object
registered later under the same number."
  (bt:with-lock-held ((object-registry-lock registry))
    (clrhash (object-registry-storage registry))
    (fill (object-registry-history registry) nil)
    (setf (object-registry-head registry) 0
          (object-registry-count registry) 0
          (object-registry-generation registry) nil)
    t))

(defun registry-count (&optional (registry *object-registry*))
  "Return the number of objects currently in REGISTRY."
  (bt:with-lock-held ((object-registry-lock registry))
    (object-registry-count registry)))
