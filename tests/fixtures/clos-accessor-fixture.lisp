;;;; tests/fixtures/clos-accessor-fixture.lisp
;;;;
;;;; Compiled and loaded by the clos-describe verification tests so that they
;;;; see real slot accessors whose generic functions differ only in their SETF
;;;; flag: a :WRITER naming a plain function, a :WRITER naming a (SETF ...)
;;;; function, and an :ACCESSOR defining a plain reader and a (SETF ...)
;;;; writer at once.  CL makes these three different definitions, so matching
;;;; a live accessor against the slot option that defines it has to tell them
;;;; apart.
;;;; The tests find each definition by the text of its first line, so keep
;;;; those lines unique.  Never MAKE-INSTANCE a class defined here.

(defpackage #:cl-mcp-clos-accessor-fixture
  (:use #:cl)
  (:export #:gauge #:gauge-level #:dial #:dial-level #:meter #:meter-level))

(in-package #:cl-mcp-clos-accessor-fixture)

(defclass gauge ()
  ((level :initarg :level :writer gauge-level)))

(defclass dial ()
  ((level :initarg :level :writer (setf dial-level))))

(defclass meter ()
  ((level :initarg :level :accessor meter-level)))
