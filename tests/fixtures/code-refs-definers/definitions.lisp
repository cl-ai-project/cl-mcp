;;;; Definitions for the code-find-references definer-name tests.

(defpackage #:cl-mcp-definer-refs-fixture
  (:use #:cl)
  (:export #:define-checked #:target))

(in-package #:cl-mcp-definer-refs-fixture)

(defmacro define-checked (name)
  `(setf (get ',name 'checked) t))

(defmacro define-variable (name)
  `(defparameter ,name 0))

(defmacro define-structure (name)
  `(defstruct ,name))

(defmacro define-procedure (name)
  `(defun ,name () 0))

(defun target (argument)
  argument)

;; The function and variable namespaces can share a symbol.
(define-variable target)
(define-variable *only-variable*)
(define-structure only-structure)

(define-procedure macro-target)
(define-checked macro-target)
