;;;; src/core.lisp

(defpackage #:cl-mcp/src/core
  (:use #:cl)
  (:export #:version #:+server-version+))

(in-package #:cl-mcp/src/core)

(defparameter +server-version+
  "1.0.0"
  "Semantic version of cl-mcp.")

(declaim (ftype (function () simple-string) version))
(defun version ()
  "Return server version string."
  +server-version+)
