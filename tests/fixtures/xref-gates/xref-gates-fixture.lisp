;;;; tests/fixtures/xref-gates/xref-gates-fixture.lisp
;;;;
;;;; Compiled and loaded by tests/code-test.lisp.  Between some callers of
;;;; GATE-CALLEE sit things SBCL's source offset does not point past: a form
;;;; that is false on SBCL, and a comment block longer than 1024 characters.
;;;; Each caller must still meet its own xref entry.  The last caller follows
;;;; a form gated on a feature the test adds only while scanning, so the scan
;;;; reads a form the compiler skipped.
;;;; The tests find each call site by its text, so keep every (gate-callee N)
;;;; unique.  It lives in a directory of its own so a scan can cover it alone.

(defpackage #:cl-mcp-xref-gates-fixture
  (:use #:cl)
  (:export #:gate-callee))

(in-package #:cl-mcp-xref-gates-fixture)

(defun gate-callee (x)
  "The function every caller below refers to."
  (1+ x))

#+(or)
(defun commented-out ()
  :never-read)

(defun after-commented-out ()
  (gate-callee 1))

#-sbcl
(defun not-on-sbcl ()
  :never-read)

(defun after-not-sbcl ()
  (gate-callee 2))

;;; A comment block longer than 1024 characters, so the form after it starts
;;; well beyond any fixed look-ahead from the end of the previous form.
;;; Filler line 01 of the long comment block, which only takes up room here.
;;; Filler line 02 of the long comment block, which only takes up room here.
;;; Filler line 03 of the long comment block, which only takes up room here.
;;; Filler line 04 of the long comment block, which only takes up room here.
;;; Filler line 05 of the long comment block, which only takes up room here.
;;; Filler line 06 of the long comment block, which only takes up room here.
;;; Filler line 07 of the long comment block, which only takes up room here.
;;; Filler line 08 of the long comment block, which only takes up room here.
;;; Filler line 09 of the long comment block, which only takes up room here.
;;; Filler line 10 of the long comment block, which only takes up room here.
;;; Filler line 11 of the long comment block, which only takes up room here.
;;; Filler line 12 of the long comment block, which only takes up room here.
;;; Filler line 13 of the long comment block, which only takes up room here.
;;; Filler line 14 of the long comment block, which only takes up room here.
;;; Filler line 15 of the long comment block, which only takes up room here.
;;; Filler line 16 of the long comment block, which only takes up room here.
;;; Filler line 17 of the long comment block, which only takes up room here.
;;; Filler line 18 of the long comment block, which only takes up room here.
;;; Filler line 19 of the long comment block, which only takes up room here.
;;; Filler line 20 of the long comment block, which only takes up room here.
;;; Filler line 21 of the long comment block, which only takes up room here.
;;; Filler line 22 of the long comment block, which only takes up room here.
;;; Filler line 23 of the long comment block, which only takes up room here.
;;; Filler line 24 of the long comment block, which only takes up room here.
(defun after-long-comment ()
  (gate-callee 3))

#+cl-mcp-xref-gates-scan-only
(defun scan-only-caller ()
  (gate-callee 4))

(defun after-scan-only ()
  (gate-callee 5))
