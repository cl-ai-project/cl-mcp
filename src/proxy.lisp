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
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held)
  (:export #:proxy-to-worker
           #:with-proxy-dispatch
           #:*use-worker-pool*
           #:*proxy-rpc-timeout*
           #:verify-proxy-bindings
           #:%invalidate-proxy-cache
           #:*current-session-id*
           #:cancel-request
           #:*active-requests*
           #:*active-requests-lock*))

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

(defvar *active-requests* (make-hash-table :test 'equal)
  "Maps MCP request-id (as string) to session-id for in-flight
proxied requests.  Used by cancel-request to find which worker to kill.")

(defvar *active-requests-lock* (make-lock "active-requests-lock")
  "Lock protecting *active-requests*.")

(defmacro with-proxy-dispatch ((id method params-form) &body inline-body)
  "When *use-worker-pool* is non-nil, proxy the tool call to a worker
process and wrap the result for JSON-RPC.  Otherwise execute INLINE-BODY.
METHOD is a string like \"worker/eval\".  PARAMS-FORM builds the
arguments hash-table.  ID is the JSON-RPC request id."
  `(if *use-worker-pool*
       (result ,id (proxy-to-worker ,id ,method ,params-form))
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
    ("CL-MCP/SRC/POOL" . "FIND-SESSION-WORKER")
    ("CL-MCP/SRC/WORKER-CLIENT" . "CHECK-AND-CLEAR-RESET-NOTIFICATION")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-RPC")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-CRASHED")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-CRASHED-REASON")
    ("CL-MCP/SRC/WORKER-CLIENT" . "KILL-WORKER")
    ("CL-MCP/SRC/WORKER-CLIENT" . "SIGNAL-WORKER-TERMINATE")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-LAST-CRASH-REASON")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-LAST-EXIT-STATUS")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-LAST-EXIT-CODE"))
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

(defun %crash-notification-result (&key reason exit-status exit-code)
  "Return a tool result hash-table with the crash notification message.
When crash details are provided, they are included for diagnostics."
  (let* ((detail-parts
          (remove nil
                  (list (when (and reason (not (equal reason "unknown")))
                          reason)
                        (when (and exit-status (not (equal exit-status "unknown")))
                          (format nil "exit_status=~A" exit-status))
                        (when (and exit-code (not (equal exit-code "unknown")))
                          (format nil "exit_code=~A" exit-code)))))
         (detail (when detail-parts
                   (format nil "~{~A~^, ~}" detail-parts)))
         (ht (make-ht)))
    (setf (gethash "content" ht)
            (text-content
             (format nil "Worker process crashed~@[ (~A)~] and was restarted. ~
                          All Lisp state (loaded systems, defined ~
                          functions, package state) has been reset. ~
                          Please run load-system again to restore ~
                          your environment."
                     detail))
          (gethash "isError" ht) t)
    ht))

;;; ---------------------------------------------------------------------------
;;; Cached late-bound function references
;;; ---------------------------------------------------------------------------

(defvar %cached-get-or-assign% nil
  "Cached fdefinition for POOL:GET-OR-ASSIGN-WORKER.")
(defvar %cached-find-session-worker% nil
  "Cached fdefinition for POOL:FIND-SESSION-WORKER.")
(defvar %cached-check-and-clear% nil
  "Cached fdefinition for WORKER-CLIENT:CHECK-AND-CLEAR-RESET-NOTIFICATION.")
(defvar %cached-worker-rpc% nil
  "Cached fdefinition for WORKER-CLIENT:WORKER-RPC.")
(defvar %cached-worker-crashed-sym% nil
  "Cached symbol WORKER-CLIENT:WORKER-CRASHED.")
(defvar %cached-worker-crashed-reason% nil
  "Cached fdefinition for WORKER-CLIENT:WORKER-CRASHED-REASON.")
(defvar %cached-kill-worker% nil
  "Cached fdefinition for WORKER-CLIENT:KILL-WORKER.")
(defvar %cached-signal-worker-terminate% nil
  "Cached fdefinition for WORKER-CLIENT:SIGNAL-WORKER-TERMINATE.")

(defvar %cached-worker-last-crash-reason% ()
  "Cached fdefinition for WORKER-CLIENT:WORKER-LAST-CRASH-REASON.")

(defvar %cached-worker-last-exit-status% ()
  "Cached fdefinition for WORKER-CLIENT:WORKER-LAST-EXIT-STATUS.")

(defvar %cached-worker-last-exit-code% ()
  "Cached fdefinition for WORKER-CLIENT:WORKER-LAST-EXIT-CODE.")

(defun %ensure-cached-bindings ()
  "Populate the cached function bindings on first use.  Called once
per image after verify-proxy-bindings has validated the symbols."
  (unless %cached-get-or-assign%
    (setf %cached-get-or-assign%
          (fdefinition (%resolve "CL-MCP/SRC/POOL" "GET-OR-ASSIGN-WORKER"))
          %cached-find-session-worker%
          (fdefinition (%resolve "CL-MCP/SRC/POOL" "FIND-SESSION-WORKER"))
          %cached-check-and-clear%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "CHECK-AND-CLEAR-RESET-NOTIFICATION"))
          %cached-worker-rpc%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT" "WORKER-RPC"))
          %cached-worker-crashed-sym%
          (%resolve "CL-MCP/SRC/WORKER-CLIENT" "WORKER-CRASHED")
          %cached-worker-crashed-reason%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "WORKER-CRASHED-REASON"))
          %cached-kill-worker%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT" "KILL-WORKER"))
          %cached-signal-worker-terminate%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "SIGNAL-WORKER-TERMINATE"))
          %cached-worker-last-crash-reason%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "WORKER-LAST-CRASH-REASON"))
          %cached-worker-last-exit-status%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "WORKER-LAST-EXIT-STATUS"))
          %cached-worker-last-exit-code%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "WORKER-LAST-EXIT-CODE")))))

(defun %invalidate-proxy-cache ()
  "Reset all cached late-bound function references to NIL.
Called by initialize-pool to ensure stale bindings from a previous
image or pool lifecycle are cleared before re-verification."
  (setf %cached-get-or-assign% nil
        %cached-find-session-worker% nil
        %cached-check-and-clear% nil
        %cached-worker-rpc% nil
        %cached-worker-crashed-sym% nil
        %cached-worker-crashed-reason% nil
        %cached-kill-worker% nil
        %cached-signal-worker-terminate% nil
        %cached-worker-last-crash-reason% nil
        %cached-worker-last-exit-status% nil
        %cached-worker-last-exit-code% nil))

(defun proxy-to-worker (id method params)
  "Proxy a tool call to the session's dedicated worker process.
Returns the worker's JSON-RPC result hash-table directly.
Registers the request in *active-requests* so notifications/cancelled
can map request-id to session and kill the worker.
Uses atomic check-and-clear for crash notification to prevent
TOCTOU race with concurrent requests for the same session."
  (let ((session-id *current-session-id*))
    (unless (and (stringp session-id) (plusp (length session-id)))
      (error "Cannot proxy tool call: no session ID bound."))
    (%ensure-cached-bindings)
    (let ((request-key (princ-to-string id)))
      (with-lock-held (*active-requests-lock*)
        (setf (gethash request-key *active-requests*) session-id))
      (unwind-protect
          (let ((worker
                  (handler-case
                      (funcall %cached-get-or-assign% session-id)
                    (error (e)
                      (log-event :warn "proxy.pool-error"
                                 "session" session-id
                                 "method" method
                                 "error" (princ-to-string e))
                      (return-from proxy-to-worker
                        (make-ht "content"
                                 (text-content
                                  (format nil "Pool error: ~A"
                                          (sanitize-error-message
                                           (princ-to-string e))))
                                 "isError" t)))))
                 (worker-crashed-sym %cached-worker-crashed-sym%))
            (cond
              ((funcall %cached-check-and-clear% worker)
               (let ((cr-reason (ignore-errors
                                  (funcall %cached-worker-last-crash-reason% worker)))
                     (cr-exit-status (ignore-errors
                                      (funcall %cached-worker-last-exit-status% worker)))
                     (cr-exit-code (ignore-errors
                                    (funcall %cached-worker-last-exit-code% worker))))
                 (log-event :info "proxy.crash-notification"
                            "session" session-id
                            "method" method "reason" cr-reason
                            "exit_status" cr-exit-status "exit_code" cr-exit-code)
                 (%crash-notification-result :reason cr-reason
                                             :exit-status cr-exit-status
                                             :exit-code cr-exit-code)))
              (t
               (log-event :debug "proxy.forward"
                          "session" session-id
                          "method" method)
               (let* ((user-timeout
                       (and (hash-table-p params)
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
                        (let ((reason
                               (ignore-errors
                                 (funcall %cached-worker-crashed-reason% e))))
                          (cond
                            ((and reason (string= reason "timeout"))
                             (log-event :warn "proxy.worker-timeout"
                                        "session" session-id
                                        "method" method)
                             (make-ht "content"
                                      (text-content
                                       (concatenate 'string
                                         "Worker RPC timed out. "
                                         "The operation took too long and the worker "
                                         "was terminated. All Lisp state has been reset. "
                                         "Please run load-system again to restore "
                                         "your environment."))
                                      "isError" t))
                            (t
                             (log-event :warn
                                        "proxy.worker-crashed-mid-request"
                                        "session" session-id
                                        "method" method
                                        "error" (princ-to-string e))
                             (%crash-notification-result
                              :reason (or reason "unknown"))))))
                       (t
                        (log-event :debug "proxy.worker-rpc-error"
                                   "session" session-id
                                   "method" method
                                   "error" (princ-to-string e))
                        (make-ht "content"
                                 (text-content
                                  (format nil "Worker error: ~A"
                                          (sanitize-error-message
                                           (princ-to-string e))))
                                 "isError" t)))))))))
        (with-lock-held (*active-requests-lock*)
          (remhash request-key *active-requests*))))))

(defun cancel-request (request-id &optional caller-session-id)
  "Cancel a proxied request by killing its worker.
Looks up REQUEST-ID in the active-requests map, validates the
caller's session matches (if CALLER-SESSION-ID provided), sends
SIGTERM to break in-flight RPC, then calls kill-worker for cleanup.
Returns T if found and cancelled, NIL otherwise."
  (let ((request-key (princ-to-string request-id))
        (session-id nil))
    ;; Look up without removing — validate ownership first.
    (with-lock-held (*active-requests-lock*)
      (setf session-id (gethash request-key *active-requests*)))
    (unless session-id
      (log-event :debug "proxy.cancel.not-found"
                 "request_id" request-key)
      (return-from cancel-request nil))
    ;; Cross-session validation: reject if caller doesn't own this request.
    (when (and caller-session-id
               (not (equal caller-session-id session-id)))
      (log-event :warn "proxy.cancel.session-mismatch"
                 "request_id" request-key
                 "caller_session" caller-session-id
                 "owner_session" (if (> (length session-id) 8)
                                     (subseq session-id 0 8)
                                     session-id))
      (return-from cancel-request nil))
    ;; Validation passed — remove entry before killing worker.
    (with-lock-held (*active-requests-lock*)
      (remhash request-key *active-requests*))
    (log-event :info "proxy.cancel.killing-worker"
               "request_id" request-key
               "session" session-id)
    (handler-case
        (progn
          (%ensure-cached-bindings)
          ;; M1: Read-only lookup, no spawn
          (let ((worker (funcall %cached-find-session-worker% session-id)))
            (unless worker
              (log-event :debug "proxy.cancel.no-worker"
                         "request_id" request-key
                         "session" session-id)
              (return-from cancel-request t))
            ;; C1: SIGTERM first to break blocked RPC read.
            ;; worker-rpc holds stream-lock for send+blocking-read.
            ;; kill-worker also acquires stream-lock -> deadlock.
            ;; SIGTERM breaks the TCP pipe, causing the blocked read
            ;; to fail with stream-error, which releases the lock.
            (funcall %cached-signal-worker-terminate% worker)
            ;; Then kill-worker for full cleanup (socket close, state, reap)
            (funcall %cached-kill-worker% worker)
            (log-event :info "proxy.cancel.worker-killed"
                       "request_id" request-key
                       "session" session-id)))
      (error (e)
        (log-event :warn "proxy.cancel.error"
                   "request_id" request-key
                   "session" session-id
                   "error" (princ-to-string e))))
    t))
