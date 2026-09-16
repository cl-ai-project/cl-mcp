;;;; tests/fixtures/clos-identity-fixture.lisp
;;;;
;;;; Fixture for clos-core's structured "identity": two packages, each with a
;;;; class and generic function of the same name, an EQL-specialized method
;;;; per tagged datum kind (design spec 3.3), a SETF generic function, a
;;;; DEFGENERIC with an inline (:method ...), and a DEFCLASS accessor.
;;;; Nothing here is ever MAKE-INSTANCE'd.
;;;; 識別子は表示文字列ではなく構造（パッケージとシンボル名、タグ付き datum）で
;;;; 突き合わせることを確認するための固定データ。

(defpackage #:cl-mcp-identity-a
  (:use #:cl))

(defpackage #:cl-mcp-identity-b
  (:use #:cl))

(in-package #:cl-mcp-identity-a)

(defparameter *probe-var* 7
  "An EQL specializer's FORM can be a variable reference; its datum is the
evaluated value (an integer), never the variable's name.")

(defclass probe ()
  ((value :accessor probe-value :initarg :value)))

(defclass |Foo| () ())

(defclass |FOO| () ())

(defgeneric act (x)
  (:method ((x integer)) x))

(defgeneric (setf act) (new-value x))

(defmethod act ((x probe)) x)

(defmethod act ((x |Foo|)) x)

(defmethod act ((x |FOO|)) x)

(defmethod act ((x (eql :unit))) x)

(defmethod act ((x (eql 3))) x)

(defmethod act ((x (eql 1/3))) x)

(defmethod act ((x (eql #\A))) x)

(defmethod act ((x (eql #\B))) x)

(defmethod act ((x (eql t))) x)

(defmethod act ((x (eql nil))) x)

(defmethod act ((x (eql 'sym))) x)

(defmethod act ((x (eql "str"))) x)

(defmethod act ((x (eql *probe-var*))) x)

(defmethod (setf act) (new-value (x probe))
  (declare (ignore new-value))
  x)

(in-package #:cl-mcp-identity-b)

(defclass probe ()
  ((value :accessor probe-value :initarg :value)))

(defgeneric act (x)
  (:method ((x integer)) x))

(defmethod act ((x probe)) x)
