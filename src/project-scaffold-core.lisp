;;;; src/project-scaffold-core.lisp
;;;;
;;;; Pure helpers for project-scaffold: input validation, template rendering,
;;;; path math, and file manifest construction. No I/O. No worker interaction.
;;;; This module exists so that the effectful layer in project-scaffold.lisp
;;;; stays thin and the bulk of the logic is trivially unit-testable.

(defpackage #:cl-mcp/src/project-scaffold-core
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan
                #:regex-replace-all)
  (:export))

(in-package #:cl-mcp/src/project-scaffold-core)
