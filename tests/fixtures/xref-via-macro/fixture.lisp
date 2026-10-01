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

;; Only quotes the macro's name: nothing expands, so nothing reaches the target.
(test probe-only-quotes-the-macro
  (equal '(with-probe (v 1) v) (list 'with-probe)))

;; Compiled from the file, so xref locates its call too: listed once.
(defun probe-macro-caller ()
  (with-probe (v 5) v))

;; Uses a local WITH-PROBE whose expansion never names the target.
(test probe-shadowed-by-macrolet
  (macrolet ((with-probe ((var value) &body body)
               `(let ((,var ,value)) ,@body)))
    (with-probe (v 1) v)))

;; Names the target in one branch of its expansion only.
(defmacro maybe-probe (enabled)
  (if enabled `(probe-target 1) `(identity 1)))

;; Compiled while loading: no xref location, so it can only be listed as a
;; form that may reach the target.
(test probe-maybe-off
  (maybe-probe nil))

;; Compiled from the file: xref says this use does not reach the target.
(defun probe-maybe-caller ()
  (maybe-probe nil))

;; A class: xref records no use of a class name, so even a file-compiled use
;; of a macro naming it is only known from the source.
(defclass probe-class () ())

(defmacro with-probe-instance ((var) &body body)
  `(let ((,var (make-instance 'probe-class)))
     ,@body))

(defun probe-class-user ()
  (with-probe-instance (p)
    p))
