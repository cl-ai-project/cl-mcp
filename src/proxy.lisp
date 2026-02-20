;;;; src/proxy.lisp
;;;;
;;;; Proxy layer for routing tool calls to worker processes.
;;;;
;;;; When *use-worker-pool* is non-nil, tool handlers call
;;;; proxy-to-worker instead of executing inline.  The proxy
;;;; resolves the current session's dedicated worker via the
;;;; pool manager and forwards the call as a JSON-RPC request.
;;;;
;;;; Crash recovery: if the worker was recently restarted after
;;;; a crash, the proxy returns a one-time notification to the
;;;; AI agent before resuming normal operation.

(defpackage #:cl-mcp/src/proxy
  (:use #:cl)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:proxy-to-worker
           #:*use-worker-pool*
           #:*current-session-id*))

(in-package #:cl-mcp/src/proxy)

(defvar *use-worker-pool* nil
  "When non-nil, delegate eval/introspect operations to worker processes.
Defined here in proxy so tool files can import it without pulling in
the full pool/worker-client dependency chain at compile time.")

(defun %resolve (pkg-name sym-name)
  "Resolve a symbol at runtime from a package that may not be loaded yet."
  (let ((pkg (find-package pkg-name)))
    (unless pkg
      (error "Package ~A not loaded. Is the worker pool system loaded?" pkg-name))
    (let ((sym (find-symbol sym-name pkg)))
      (unless sym
        (error "Symbol ~A not found in ~A" sym-name pkg-name))
      sym)))

(defun proxy-to-worker (method params)
  "Proxy a tool call to the session's dedicated worker process.
Returns the worker's JSON-RPC result hash-table directly.

METHOD is the worker-side JSON-RPC method name (e.g. \"worker/eval\").
PARAMS is a hash-table of arguments to forward.

Pool and worker-client symbols are resolved at runtime via late-binding
so that this module compiles without requiring those systems to be loaded.
They are only needed when *use-worker-pool* is non-nil.

If the worker was recently restarted after a crash, returns a
one-time error notification instead of forwarding the call.
Subsequent calls proceed normally."
  (let* ((session-id *current-session-id*)
         (get-or-assign (fdefinition (%resolve "CL-MCP/SRC/POOL"
                                               "GET-OR-ASSIGN-WORKER")))
         (worker (funcall get-or-assign session-id))
         (needs-reset-fn (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                                "WORKER-NEEDS-RESET-NOTIFICATION")))
         (clear-reset-fn (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                                "CLEAR-RESET-NOTIFICATION")))
         (worker-rpc-fn (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                               "WORKER-RPC"))))
    (cond
      ;; Worker was recently restarted after a crash -- notify once
      ((funcall needs-reset-fn worker)
       (funcall clear-reset-fn worker)
       (log-event :info "proxy.crash-notification"
                  "session" session-id
                  "method" method)
       (let ((ht (make-ht)))
         (setf (gethash "content" ht)
               (text-content
                (concatenate 'string
                             "Worker process crashed and was restarted. "
                             "All Lisp state (loaded systems, defined "
                             "functions, package state) has been reset. "
                             "Please run load-system again to restore "
                             "your environment."))
               (gethash "isError" ht) t)
         ht))
      ;; Normal path -- forward to worker
      (t
       (log-event :debug "proxy.forward"
                  "session" session-id
                  "method" method)
       (funcall worker-rpc-fn worker method params)))))
