;;;; src/system-loader.lisp
;;;;
;;;; MCP tool for loading ASDF systems with structured output.
;;;; Solves three problems with raw ql:quickload via repl-eval:
;;;; 1. Staleness: force=true (default) clears loaded state before reloading
;;;; 2. Output noise: suppresses verbose compilation/load output
;;;; 3. Timeout: dedicated timeout with worker-thread pattern

(defpackage #:cl-mcp/src/system-loader
  (:use #:cl)
  (:import-from #:cl-mcp/src/system-loader-core
                #:load-system)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-load-system-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:load-system))

(in-package #:cl-mcp/src/system-loader)

(define-tool "load-system"
  :description
  "Load an ASDF system with structured output and reload support.

Solves three problems with using ql:quickload via repl-eval:
1. Staleness: force=true (default) clears loaded state before reloading
2. Output noise: suppresses verbose compilation/load output
3. Timeout: dedicated timeout prevents hanging on large systems

PREREQUISITE: The system must be findable by ASDF (in quicklisp paths or
registered via asdf:load-asd).

Examples:
  First-time load: system='my-project', force=false
  Reload after edits: system='my-project' (force=true is default)
  Full recompile: system='my-project', clear_fasls=true"
  :args
  ((system :type :string :required t
    :description "ASDF system name (e.g., 'my-project', 'cl-mcp/tests')")
   (force :type :boolean :default t
    :description "Clear loaded state before loading to pick up changes (default: true)")
   (clear-fasls :type :boolean :json-name "clear_fasls"
    :description "Force full recompilation from source (default: false)")
   (timeout-seconds :type :number :json-name "timeout_seconds"
    :description "Timeout for the operation in seconds (default: 120)"))
  :body
  (progn
    (when (and timeout-seconds (not (plusp timeout-seconds)))
      (error 'arg-validation-error
             :arg-name "timeout_seconds"
             :message "timeout_seconds must be a positive number"))
    (with-proxy-dispatch (id "worker/load-system"
                             (make-ht "system" system
                                      "force" force
                                      "clear_fasls" clear-fasls
                                      "timeout_seconds" timeout-seconds))
      (let ((ht (load-system system
                             :force force
                             :clear-fasls clear-fasls
                             :timeout-seconds (or timeout-seconds 120))))
        (result id (build-load-system-response system ht))))))
