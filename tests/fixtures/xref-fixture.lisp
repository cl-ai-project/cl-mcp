;;;; tests/fixtures/xref-fixture.lisp
;;;;
;;;; Compiled and loaded by tests/code-test.lisp so that SBCL records real
;;;; cross references for code-find-references to merge with its source scan.
;;;; 日本語のコメント: SBCL の位置情報はバイト単位なので、この行の後ろでも
;;;; 行番号がずれないことを確かめる。
;;;; The tests find each call site by its text, so keep every (target N) unique.

(defpackage #:cl-mcp-xref-fixture
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok)
  (:export #:target #:with-target))

(defpackage #:cl-mcp-xref-fixture-other
  (:use #:cl)
  (:export #:target))

(defpackage #:cl-mcp-xref-fixture-nick
  (:use #:cl)
  (:local-nicknames (#:fx #:cl-mcp-xref-fixture)))

(in-package #:cl-mcp-xref-fixture)

(defun target (x)
  "The function every other form refers to."
  (1+ x))

(defun plain-caller ()
  (target 1))

#+sbcl
(defun feature-caller ()
  (target 2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun eval-when-caller ()
    (target 3)))

(defparameter *top-level-use* (target 4))

(defmacro with-target (&body body)
  `(progn (target 5) ,@body))

(defun macro-hidden-caller ()
  (with-target :done))

(defun quoted-and-macro-caller ()
  'target
  (with-target :quoted))

(defun funcall-caller ()
  (funcall 'target 10))

(defun shadowing-caller ()
  (flet ((target (x) (* x 10)))
    (target 6)))

(defgeneric shape-area (shape))

(defmethod shape-area ((shape integer))
  (target shape))

(deftest target-is-called-from-a-test
  (ok (= 8 (target 7))))

(in-package #:cl-mcp-xref-fixture-other)

(defun target (x)
  "Same name, different symbol: never a reference to the fixture's TARGET."
  x)

(defun other-caller ()
  (target 8))

(in-package #:cl-mcp-xref-fixture-nick)

(defun nickname-caller ()
  (fx:target 9))
