;;;; A reader position has to count characters after the λ in this comment.

(defpackage #:cl-mcp/tests/fixtures/code-refs-definers/same-line
  (:use #:cl)
  (:import-from #:cl-mcp/tests/fixtures/code-refs-definers/definitions
                #:define-checked))

(in-package #:cl-mcp/tests/fixtures/code-refs-definers/same-line)

(defun compact-target () 0) (define-checked compact-target)
