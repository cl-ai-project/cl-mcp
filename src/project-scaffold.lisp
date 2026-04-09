;;;; src/project-scaffold.lisp
;;;;
;;;; MCP tool entry for project-scaffold. Thin I/O layer on top of the pure
;;;; logic in project-scaffold-core. Runs in the parent (inline) process
;;;; alongside other fs-* tools. Registers itself with the tool registry
;;;; at load time via define-tool.

(defpackage #:cl-mcp/src/project-scaffold
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:result
                #:text-content)
  (:export))

(in-package #:cl-mcp/src/project-scaffold)
