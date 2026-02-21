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
           #:*proxy-rpc-timeout*
           #:verify-proxy-bindings
           #:*current-session-id*))

(in-package #:cl-mcp/src/proxy)

(defvar *use-worker-pool* nil
  "When non-nil, delegate eval/introspect operations to worker processes.
Defined here in proxy so tool files can import it without pulling in
the full pool/worker-client dependency chain at compile time.")

(defvar *proxy-rpc-timeout* 300
  "Default timeout (seconds) for proxy-to-worker RPC calls.
Prevents requests from hanging indefinitely when a worker handler
is stuck.  300 seconds (5 minutes) accommodates long-running
operations like system compilation.")

(defun %resolve (pkg-name sym-name)
  "Resolve a symbol at runtime from a package that may not be loaded yet."
  (let ((pkg (find-package pkg-name)))
    (unless pkg
      (error "Package ~A not loaded. Is the worker pool system loaded?" pkg-name))
    (let ((sym (find-symbol sym-name pkg)))
      (unless sym
        (error "Symbol ~A not found in ~A" sym-name pkg-name))
      sym)))

(defparameter %proxy-bindings%
  '(("CL-MCP/SRC/POOL" . "GET-OR-ASSIGN-WORKER")
    ("CL-MCP/SRC/WORKER-CLIENT" . "CHECK-AND-CLEAR-RESET-NOTIFICATION")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-RPC")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-CRASHED"))
  "Package/symbol pairs used by proxy-to-worker at runtime.
Verified at pool initialization to detect stale strings early.")

(defun verify-proxy-bindings ()
  "Verify all late-bound proxy symbols are resolvable.
Call during initialize-pool to detect broken bindings immediately
instead of failing at first proxy call.  Signals an error listing
all unresolvable symbols."
  (let ((failures nil))
    (dolist (pair %proxy-bindings%)
      (let ((pkg (find-package (car pair))))
        (cond
         ((null pkg)
          (push (format nil "Package ~A not found" (car pair)) failures))
         ((null (find-symbol (cdr pair) pkg))
          (push (format nil "Symbol ~A:~A not found" (car pair) (cdr pair))
                failures)))))
    (when failures
      (error "Proxy binding verification failed:~%~{  - ~A~%~}" failures)))
  t)

(defun %crash-notification-result ()
  "Return a tool result hash-table with the crash notification message."
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

(defun proxy-to-worker (method params)
  "Proxy a tool call to the session's dedicated worker process.
Returns the worker's JSON-RPC result hash-table directly.
Uses atomic check-and-clear for crash notification to prevent
TOCTOU race with concurrent requests for the same session."
  (let* ((session-id *current-session-id*)
         (get-or-assign (fdefinition (%resolve "CL-MCP/SRC/POOL"
                                               "GET-OR-ASSIGN-WORKER")))
         (worker (funcall get-or-assign session-id))
         (check-and-clear-fn
           (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                  "CHECK-AND-CLEAR-RESET-NOTIFICATION")))
         (worker-rpc-fn
           (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT" "WORKER-RPC")))
         (worker-crashed-sym
           (%resolve "CL-MCP/SRC/WORKER-CLIENT" "WORKER-CRASHED")))
    (cond
      ;; Atomic check-and-clear: only one concurrent request gets the
      ;; crash notification; the rest proceed normally.
      ((funcall check-and-clear-fn worker)
       (log-event :info "proxy.crash-notification"
                  "session" session-id
                  "method" method)
       (%crash-notification-result))
      (t
       (log-event :debug "proxy.forward"
                  "session" session-id
                  "method" method)
       (let* ((user-timeout (and (hash-table-p params)
                                 (gethash "timeout_seconds" params)))
              (effective-timeout
                (if (and user-timeout (numberp user-timeout)
                         (plusp user-timeout))
                    (max *proxy-rpc-timeout*
                         (ceiling (+ user-timeout 30)))
                    *proxy-rpc-timeout*)))
         (handler-case
             (funcall worker-rpc-fn worker method params
                      :timeout effective-timeout)
           (error (e)
             (cond
               ((typep e worker-crashed-sym)
                (log-event :warn "proxy.worker-crashed-mid-request"
                           "session" session-id
                           "method" method
                           "error" (princ-to-string e))
                (%crash-notification-result))
               (t
                (log-event :debug "proxy.worker-rpc-error"
                           "session" session-id
                           "method" method
                           "error" (princ-to-string e))
                (error "~A" (princ-to-string e)))))))))))
