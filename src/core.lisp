;;;; src/core.lisp

(defpackage #:lisp-mcp-server/src/core
  (:use #:cl)
  (:export #:version #:+server-version+))

(in-package #:lisp-mcp-server/src/core)

(defparameter +server-version+
  "0.1.0"
  "Semantic version of lisp-mcp-server.")

(declaim (ftype (function () simple-string) version))
(defun version ()
  "Return server version string."
  +server-version+)
