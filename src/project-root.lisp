;;;; src/project-root.lisp
;;;;
;;;; Project root configuration.
;;;; This minimal module defines *project-root* to avoid circular dependencies.

(defpackage #:cl-mcp/src/project-root
  (:use #:cl)
  (:import-from #:bordeaux-threads #:make-lock)
  (:export #:*project-root*
           #:*project-root-lock*))

(in-package #:cl-mcp/src/project-root)

(defparameter *project-root*
  (let ((env-root (uiop/os:getenv "MCP_PROJECT_ROOT")))
    (when env-root
      (uiop/pathname:ensure-directory-pathname env-root)))
  "Absolute pathname of the project root.
Set via MCP_PROJECT_ROOT environment variable or fs-set-project-root tool.")

(defvar *project-root-lock* (bt:make-lock "project-root-lock")
  "Lock protecting multi-step mutations of *project-root* and related globals.")
