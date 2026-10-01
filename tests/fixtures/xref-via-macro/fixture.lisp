;;;; tests/fixtures/xref-via-macro/fixture.lisp
;;;;
;;;; PROBE-TARGET is reached from a test that only uses WITH-PROBE, whose
;;;; backquoted expansion names it.  TEST compiles its body while the file
;;;; loads, with COMPILE, as FiveAM's TEST does: xref then records the call
;;;; with no source location at all, so only the source can say which test
;;;; it sits in.

(defpackage #:cl-mcp-via-macro-fixture
  (:use #:cl))

(in-package #:cl-mcp-via-macro-fixture)

(defun probe-target (x)
  (1+ x))

(defmacro with-probe ((var value) &body body)
  `(let ((,var (probe-target ,value)))
     ,@body))

(defmacro test (name &body body)
  `(setf (get ',name 'test-body) (compile nil '(lambda () ,@body))))

(test probe-reached-through-the-macro
  (with-probe (v 1)
    (= v 2)))

(defun probe-direct-caller ()
  (probe-target 3))
