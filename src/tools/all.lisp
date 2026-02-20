;;;; src/tools/all.lisp
;;;;
;;;; Tool loader module for MCP server.
;;;; This module exists solely to trigger loading of all tool modules,
;;;; which register themselves with the tool registry at load time.
;;;;
;;;; Design rationale:
;;;; - Tool modules import registry.lisp to use register-tool
;;;; - This module imports all tool modules for their load-time side effects
;;;; - protocol.lisp imports this module instead of individual tool modules
;;;; - This avoids circular dependencies and keeps imports semantically correct

(defpackage #:cl-mcp/src/tools/all
  (:use #:cl)
  (:documentation "Loader module that imports all tool modules for registration side effects.")
  ;; Import tool modules to trigger their load-time register-tool calls.
  ;; These symbols are not used directly; the import triggers ASDF loading.
  (:import-from #:cl-mcp/src/repl
                #:repl-eval)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory
                #:fs-get-project-info
                #:fs-set-project-root)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:import-from #:cl-mcp/src/code
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens)
  (:import-from #:cl-mcp/src/clgrep
                #:clgrep-search)
  (:import-from #:cl-mcp/src/inspect
                #:inspect-object-by-id)
  (:import-from #:cl-mcp/src/clhs
                #:clhs-lookup)
  (:import-from #:cl-mcp/src/test-runner
                #:run-tests)
  (:import-from #:cl-mcp/src/system-loader
                #:load-system))

(in-package #:cl-mcp/src/tools/all)

;; This package intentionally has no code.
;; Its purpose is to trigger loading of all tool modules via ASDF's
;; package-inferred-system, which causes their register-tool calls to execute.
