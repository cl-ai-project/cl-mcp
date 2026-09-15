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
           #:probe-error-code #:point #:pending))

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
  ((code :initarg :code :reader probe-error-code)))

(defstruct point x (y 0))

(defclass pending (not-yet-defined) ())
