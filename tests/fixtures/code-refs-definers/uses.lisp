;;;; Uses in another file and package.

(defpackage #:cl-mcp/tests/fixtures/code-refs-definers/uses
  (:use #:cl)
  (:import-from #:cl-mcp/tests/fixtures/code-refs-definers/definitions
                #:define-checked))

(in-package #:cl-mcp/tests/fixtures/code-refs-definers/uses)

(define-checked cl-mcp/tests/fixtures/code-refs-definers/definitions:target)

(defun caller (argument)
  (cl-mcp/tests/fixtures/code-refs-definers/definitions:target argument))
