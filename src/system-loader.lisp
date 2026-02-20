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
                #:text-content
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker
                #:*use-worker-pool*)
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
  (if *use-worker-pool*
      (progn
        (when (and timeout-seconds (not (plusp timeout-seconds)))
          (error 'arg-validation-error
                 :arg-name "timeout_seconds"
                 :message "timeout_seconds must be a positive number"))
        (result id
                (proxy-to-worker "worker/load-system"
                                 (make-ht "system" system
                                          "force" force
                                          "clear_fasls" clear-fasls
                                          "timeout_seconds" timeout-seconds))))
      ;; Fallback: inline execution
      (progn
        (when (and timeout-seconds (not (plusp timeout-seconds)))
          (error 'arg-validation-error
                 :arg-name "timeout_seconds"
                 :message "timeout_seconds must be a positive number"))
        (let* ((ht (load-system system
                              :force force
                              :clear-fasls clear-fasls
                              :timeout-seconds (or timeout-seconds 120)))
             (status (gethash "status" ht))
             (summary
               (with-output-to-string (s)
                 (cond
                   ((string= status "loaded")
                    (format s "System ~A loaded successfully in ~Dms"
                            system (gethash "duration_ms" ht))
                    (let ((wc (gethash "warnings" ht 0)))
                      (when (plusp wc)
                        (format s " (~D warning~:P)" wc))))
                   ((string= status "timeout")
                    (format s "~A" (gethash "message" ht)))
                   ((string= status "error")
                    (format s "Error loading ~A: ~A"
                            system (gethash "message" ht)))))))
          (setf (gethash "content" ht) (text-content summary))
          (result id ht)))))
