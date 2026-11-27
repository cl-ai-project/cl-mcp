;;;; Uses in another file and package.

(defpackage #:cl-mcp-definer-refs-uses
  (:use #:cl)
  (:import-from #:cl-mcp-definer-refs-fixture #:define-checked))

(in-package #:cl-mcp-definer-refs-uses)

(define-checked cl-mcp-definer-refs-fixture:target)

(defun caller (argument)
  (cl-mcp-definer-refs-fixture:target argument))
