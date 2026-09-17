;;;; tests/fixtures/clos-fixture.lisp
;;;;
;;;; Compiled and loaded by the clos-describe and code-find tests so that they
;;;; see real CLOS definitions with source locations.
;;;; 日本語のコメント: SBCL の位置情報はバイト単位なので、この行の後ろでも
;;;; 行番号がずれないことを確かめる。
;;;; The tests find each definition by the text of its first line, so keep those
;;;; lines unique.  Never MAKE-INSTANCE a class defined here: the tests need
;;;; CIRCLE and SQUARE unfinalized.  (Loading this file finalizes SHAPE: PCL
;;;; finalizes the class whose slot a method's SLOT-VALUE names.)

(defpackage #:cl-mcp-clos-fixture
  (:use #:cl)
  (:export #:shape #:circle #:square #:area #:label #:radius #:side
           #:shape-name #:combine #:describe-shape #:probe-error
           #:probe-error-code #:point #:pending #:widget #:widget-size
           #:gadget #:gadget-size #:override-error #:override-error-code
           #:guarded-error #:guarded-error-code #:box #:box-w))

(in-package #:cl-mcp-clos-fixture)

(defclass shape ()
  ((name :initarg :name :reader shape-name :initform "anon"
         :documentation "A label for the shape.")
   (registry :allocation :class :initform (make-hash-table)))
  (:documentation "The base of every shape."))

(defclass circle (shape)
  ((radius :initarg :radius :accessor radius :type real :initform (random 10))))

(defclass square (shape)
  ((side :initarg :side :accessor side))
  (:default-initargs :name "square"))

(defgeneric area (shape)
  (:documentation "Return the area of SHAPE.")
  (:method ((shape (eql :unit)))
    1))

(defmethod area ((shape circle))
  (* pi (radius shape) (radius shape)))

(defmethod area ((shape square))
  (* (side shape) (side shape)))

(defmethod area :around ((shape circle))
  (call-next-method))

(defgeneric (setf label) (value shape))

(defmethod (setf label) (value (shape shape))
  (setf (slot-value shape 'name) value))

(defgeneric combine (a b)
  (:method-combination +))

(defmethod combine + ((a integer) b)
  a)

(defmethod combine + ((a number) (b number))
  b)

(defmethod describe-shape ((shape shape) &optional (stream *standard-output*) verbose)
  (format stream "~A~@[ (verbose)~]" (shape-name shape) verbose))

(defmethod print-object ((shape square) stream)
  (print-unreadable-object (shape stream :type t :identity t)))

(define-condition probe-error (error)
  ((code :initarg :code :reader probe-error-code)
   ;; Two slots sharing one reader name: PROBE-ERROR-AMBIGUOUS cannot be
   ;; mapped to either slot alone, so its identity's class/slot/access must
   ;; stay unfilled (fail-closed), unlike PROBE-ERROR-CODE above.
   (left :initarg :left :reader probe-error-ambiguous)
   (right :initarg :right :reader probe-error-ambiguous)))

(defstruct point x (y 0))

(defclass pending (not-yet-defined) ())

(defclass widget ()
  ((size :initarg :size :accessor widget-size)))

;; A :before method sharing WIDGET-SIZE's generic function and sole
;; specializer with the real accessor above, but qualified -- it must never
;; be mistaken for that accessor (review fix, Task 10).  On an ordinary class
;; the condition restriction alone already settles this; GUARDED-ERROR-CODE
;; below is where the qualifier guard is the only thing deciding.
(defmethod widget-size :before ((w widget))
  (declare (ignore w))
  nil)

;; A hand-written primary method replacing GADGET's generated reader.  SBCL
;; keeps one method on the generic function and it is a plain STANDARD-METHOD,
;; so the image alone cannot call it an accessor -- and on an ordinary class it
;; never is one, because a genuine accessor would be a STANDARD-READER-METHOD.
(defclass gadget ()
  ((size :initarg :size :reader gadget-size)))

(defmethod gadget-size ((g gadget))
  (declare (ignore g))
  0)

;; The same override on a CONDITION, where a genuine reader is also a plain
;; STANDARD-METHOD: the image cannot tell the two apart, so the identity still
;; carries class/slot/access and only the source form settles which it was.
(define-condition override-error (error)
  ((code :initarg :code :reader override-error-code)))

(defmethod override-error-code ((e override-error))
  (declare (ignore e))
  0)

;; A :before method sharing a genuine CONDITION reader's generic function and
;; sole specializer.  This is the case the accessor fallback's qualifier guard
;; exists for: unlike WIDGET-SIZE above, a condition reader really does reach
;; that fallback, so only the guard keeps this qualified method out of it.
(define-condition guarded-error (error)
  ((code :initarg :code :reader guarded-error-code)))

(defmethod guarded-error-code :before ((e guarded-error))
  (declare (ignore e))
  nil)

;; A writer override: two specializers, so the accessor fallback's
;; one-specializer guard rejects it whatever class it is on.  BOX-W's reader
;; half stays a genuine STANDARD-READER-METHOD.
(defclass box ()
  ((w :initarg :w :accessor box-w)))

(defmethod (setf box-w) (value (b box))
  value)
