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
                #:make-ht #:text-content #:result)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message)
  (:export #:proxy-to-worker
           #:with-proxy-dispatch
           #:*use-worker-pool*
           #:*proxy-rpc-timeout*
           #:verify-proxy-bindings
           #:%invalidate-proxy-cache
           #:*current-session-id*))

(in-package #:cl-mcp/src/proxy)

(defvar *use-worker-pool* t
  "When non-nil, delegate eval/introspect operations to worker processes.
Defined here in proxy so tool files can import it without pulling in
the full pool/worker-client dependency chain at compile time.
Set MCP_NO_WORKER_POOL=1 to disable.")

(let ((env-val (uiop/os:getenv "MCP_NO_WORKER_POOL")))
  (when (and env-val (plusp (length env-val)))
    (setf *use-worker-pool* nil)))

(defvar *proxy-rpc-timeout* 300
  "Default timeout (seconds) for proxy-to-worker RPC calls.
Prevents requests from hanging indefinitely when a worker handler
is stuck.  300 seconds (5 minutes) accommodates long-running
operations like system compilation.")

(defmacro with-proxy-dispatch ((id method params-form) &body inline-body)
  "When *use-worker-pool* is non-nil, proxy the tool call to a worker
process and wrap the result for JSON-RPC.  Otherwise execute INLINE-BODY.
METHOD is a string like \"worker/eval\".  PARAMS-FORM builds the
arguments hash-table.  ID is the JSON-RPC request id."
  `(if *use-worker-pool*
       (result ,id (proxy-to-worker ,method ,params-form))
       (progn ,@inline-body)))

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
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-CRASHED")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-CRASHED-REASON"))
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

;;; ---------------------------------------------------------------------------
;;; Cached late-bound function references
;;; ---------------------------------------------------------------------------

(defvar %cached-get-or-assign% nil
  "Cached fdefinition for POOL:GET-OR-ASSIGN-WORKER.")
(defvar %cached-check-and-clear% nil
  "Cached fdefinition for WORKER-CLIENT:CHECK-AND-CLEAR-RESET-NOTIFICATION.")
(defvar %cached-worker-rpc% nil
  "Cached fdefinition for WORKER-CLIENT:WORKER-RPC.")
(defvar %cached-worker-crashed-sym% nil
  "Cached symbol WORKER-CLIENT:WORKER-CRASHED.")
(defvar %cached-worker-crashed-reason% nil
  "Cached fdefinition for WORKER-CLIENT:WORKER-CRASHED-REASON.")

(defun %ensure-cached-bindings ()
  "Populate the cached function bindings on first use.  Called once
per image after verify-proxy-bindings has validated the symbols."
  (unless %cached-get-or-assign%
    (setf %cached-get-or-assign%
          (fdefinition (%resolve "CL-MCP/SRC/POOL" "GET-OR-ASSIGN-WORKER"))
          %cached-check-and-clear%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "CHECK-AND-CLEAR-RESET-NOTIFICATION"))
          %cached-worker-rpc%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT" "WORKER-RPC"))
          %cached-worker-crashed-sym%
          (%resolve "CL-MCP/SRC/WORKER-CLIENT" "WORKER-CRASHED")
          %cached-worker-crashed-reason%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "WORKER-CRASHED-REASON")))))

(defun %invalidate-proxy-cache ()
  "Reset all cached late-bound function references to NIL.
Called by initialize-pool to ensure stale bindings from a previous
image or pool lifecycle are cleared before re-verification."
  (setf %cached-get-or-assign% nil
        %cached-check-and-clear% nil
        %cached-worker-rpc% nil
        %cached-worker-crashed-sym% nil
        %cached-worker-crashed-reason% nil))

(defun proxy-to-worker (method params)
  "Proxy a tool call to the session's dedicated worker process.
Returns the worker's JSON-RPC result hash-table directly.
Uses atomic check-and-clear for crash notification to prevent
TOCTOU race with concurrent requests for the same session."
  (let ((session-id *current-session-id*))
    (unless (and (stringp session-id) (plusp (length session-id)))
      (error "Cannot proxy tool call: no session ID bound."))
    (%ensure-cached-bindings)
    (let* ((worker (handler-case
                       (funcall %cached-get-or-assign% session-id)
                     (error (e)
                       (log-event :warn "proxy.pool-error"
                                  "session" session-id
                                  "method" method
                                  "error" (princ-to-string e))
                       (return-from proxy-to-worker
                         (make-ht "content" (text-content
                                             (format nil "Pool error: ~A"
                                                     (sanitize-error-message
                                                      (princ-to-string e))))
                                  "isError" t)))))
           (worker-crashed-sym %cached-worker-crashed-sym%))
      (cond
        ;; Atomic check-and-clear: only one concurrent request gets the
        ;; crash notification; the rest proceed normally.
        ((funcall %cached-check-and-clear% worker)
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
               (funcall %cached-worker-rpc% worker method params
                        :timeout effective-timeout)
             (error (e)
               (cond
                 ((typep e worker-crashed-sym)
                  (let ((reason (ignore-errors
                                  (funcall %cached-worker-crashed-reason% e))))
                    (cond
                      ((and reason (string= reason "timeout"))
                       (log-event :warn "proxy.worker-timeout"
                                  "session" session-id
                                  "method" method)
                       (make-ht "content" (text-content
                                           (concatenate 'string
                                             "Worker RPC timed out. "
                                             "The operation took too long and the worker "
                                             "was terminated. All Lisp state has been reset. "
                                             "Please run load-system again to restore "
                                             "your environment."))
                                "isError" t))
                      (t
                       (log-event :warn "proxy.worker-crashed-mid-request"
                                  "session" session-id
                                  "method" method
                                  "error" (princ-to-string e))
                       (%crash-notification-result)))))
                 (t
                  (log-event :debug "proxy.worker-rpc-error"
                             "session" session-id
                             "method" method
                             "error" (princ-to-string e))
                  ;; Return a tool-error result directly instead of re-signaling.
                  ;; This prevents define-tool's error handler from wrapping the
                  ;; message a third time ("Internal error during tool: ...").
                  (make-ht "content" (text-content
                                      (format nil "Worker error: ~A"
                                              (sanitize-error-message
                                               (princ-to-string e))))
                           "isError" t)))))))))))
