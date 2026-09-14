;;;; tests/fixtures/xref-feature/xref-feature-fixture.lisp
;;;;
;;;; Compiled and loaded by tests/code-test.lisp.  It pushes a feature while it
;;;; is compiled and gates one caller on that feature, so SBCL counts one more
;;;; top-level form than a source scan made without the feature.  The callers
;;;; after the gated form must still meet their own xref entries.
;;;; The tests find each call site by its text, so keep every (feature-callee N)
;;;; unique.  It lives in a directory of its own so a scan can cover it alone.

(defpackage #:cl-mcp-xref-feature-fixture
  (:use #:cl)
  (:export #:feature-callee))

(in-package #:cl-mcp-xref-feature-fixture)

(defun feature-callee (x)
  "The function every caller below refers to."
  (1+ x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cl-mcp-xref-feature-fixture-on *features*))

#+cl-mcp-xref-feature-fixture-on
(defun gated-caller ()
  (feature-callee 1))

(defun caller-a ()
  (feature-callee 2))

(defun caller-b ()
  (feature-callee 3))
