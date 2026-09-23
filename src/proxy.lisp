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
  (:import-from #:cl-mcp/src/utils/deadline
                #:*retired-leaked-thread-reason*)
  (:import-from #:cl-mcp/src/test-runner-core
                #:coerce-timeout-seconds)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message)
  (:import-from #:cl-mcp/src/request-lifecycle
                #:register-request
                #:unregister-request
                #:find-request
                #:note-request-phase
                #:note-request-worker
                #:cancellation-requested-p
                #:begin-send
                #:note-response
                #:cancel-request-record
                #:request-worker
                #:request-outcome)
  (:export #:proxy-to-worker
           #:with-proxy-dispatch
           #:*use-worker-pool*
           #:*proxy-rpc-timeout*
           #:verify-proxy-bindings
           #:%invalidate-proxy-cache
           #:*current-session-id*
           #:cancel-request))

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
  "Deadline (seconds) the proxy assumes the worker enforces when the caller
names none.

The tools that take a timeout_seconds argument all default to a server-side
deadline of their own: run-tests and repl-eval to this same 300, load-system
to 120.  Anything smaller than this figure is covered by waiting for it, so
one number is enough here -- but a tool that ever ran without a deadline would
not be, since the proxy would give up on a worker that is merely still
working, and a proxy timeout kills the worker rather than reporting a
timeout.")

(defvar *proxy-rpc-buffer* 30
  "Seconds the proxy waits beyond the deadline the worker enforces.

The worker owns the deadline; this is only the margin for its answer to
arrive.  The margin has to cover everything that happens *after* the worker's
own timer fires -- the cooperative unwind, DESTROY-THREAD's grace period,
building the response, and serializing it across the socket, which for
repl-eval can be max_output_length bytes.  Too small a margin and the proxy
gives up first, and a proxy timeout is not a timeout report: WORKER-RPC marks
the worker crashed and kills it, so the session's whole Lisp state is reset
for what was merely a slow answer.")

(defconstant +max-proxy-rpc-timeout+ 86400
  "Upper bound (seconds) on how long the proxy will wait for one worker call.
SB-EXT:WITH-TIMEOUT converts its argument to internal time units in a
(SIGNED-BYTE 64), so an unclamped caller-supplied value -- JSON's 1e20 reads
as a double -- becomes a bignum and a TYPE-ERROR, which WORKER-RPC reports as
a protocol error and kills the worker for.  A day is far beyond any legitimate
call.")

(defmacro with-proxy-dispatch ((id method params-form) &body inline-body)
  "When *use-worker-pool* is non-nil, proxy the tool call to a worker
process and wrap the result for JSON-RPC.  Otherwise execute INLINE-BODY.
METHOD is a string like \"worker/eval\".  PARAMS-FORM builds the
arguments hash-table.  ID is the JSON-RPC request id.

The worker's result goes to the client unread, so it is parsed with its JSON
types kept: a false the worker wrote stays false rather than turning into the
null an inline call would never have sent."
  `(if *use-worker-pool*
       (result ,id (proxy-to-worker ,id ,method ,params-form
                                    :preserve-json-types t))
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
    ("CL-MCP/SRC/WORKER-CLIENT" . "RPC-NOT-SENT")
    ("CL-MCP/SRC/WORKER-CLIENT" . "RPC-NOT-SENT-REASON")
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
                  (list (when (and reason
                              (not (equal reason "unknown"))
                              (or (not (stringp reason))
                                  (plusp (length reason))))
                          reason)
                        (when (and exit-status
                                   (not (equal exit-status "unknown"))
                                   (or (not (stringp exit-status))
                                       (plusp (length exit-status))))
                          (format nil "exit_status=~A" exit-status))
                        (when (and exit-code
                                   (not (equal exit-code "unknown"))
                                   (or (not (stringp exit-code))
                                       (plusp (length exit-code))))
                          (format nil "exit_code=~A" exit-code)))))
         (detail (when detail-parts
                   (format nil "~{~A~^, ~}" detail-parts)))
         (ht (make-ht)))
    (setf (gethash "content" ht)
            (text-content
             (if (equal reason *retired-leaked-thread-reason*)
                 ;; Not a crash, and saying so matters: the user is owed the
                 ;; connection between an earlier timeout they were told about
                 ;; and a reset they were not expecting.
                 (format nil "Worker process was replaced~@[ (~A)~]. An ~
                              earlier run exceeded its timeout and left a ~
                              thread that could not be stopped, so the worker ~
                              was retired rather than serve further requests ~
                              from it. All Lisp state (loaded systems, ~
                              defined functions, package state) has been ~
                              reset. Please run load-system again to restore ~
                              your environment."
                         detail)
                 (format nil "Worker process crashed~@[ (~A)~] and was ~
                              restarted. All Lisp state (loaded systems, ~
                              defined functions, package state) has been ~
                              reset. Please run load-system again to restore ~
                              your environment."
                         detail)))
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

(defvar %cached-rpc-not-sent-sym% nil
  "Cached symbol WORKER-CLIENT:RPC-NOT-SENT.")

(defvar %cached-rpc-not-sent-reason% nil
  "Cached fdefinition for WORKER-CLIENT:RPC-NOT-SENT-REASON.")

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
          %cached-rpc-not-sent-sym%
          (%resolve "CL-MCP/SRC/WORKER-CLIENT" "RPC-NOT-SENT")
          %cached-rpc-not-sent-reason%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "RPC-NOT-SENT-REASON"))
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
        %cached-rpc-not-sent-sym% nil
        %cached-rpc-not-sent-reason% nil
        %cached-kill-worker% nil
        %cached-signal-worker-terminate% nil
        %cached-worker-last-crash-reason% nil
        %cached-worker-last-exit-status% nil
        %cached-worker-last-exit-code% nil))

(defun %requested-worker-deadline (params)
  "The deadline the worker will enforce for a call carrying PARAMS, clamped.
The caller's timeout_seconds when it names one, the shared default otherwise."
  (min +max-proxy-rpc-timeout+
       (or (coerce-timeout-seconds
            (and (hash-table-p params)
                 (gethash "timeout_seconds" params)))
           *proxy-rpc-timeout*)))

(defun %effective-rpc-timeout (params)
  "Seconds to wait for the worker's answer to a call carrying PARAMS.

One rule, with no per-method knowledge: wait out the deadline the worker will
enforce, then the margin for its answer to arrive.  The caller's
timeout_seconds is that deadline when given; when the caller is silent the
worker falls back to a default of its own, and *PROXY-RPC-TIMEOUT* is the
largest of those.

Adding the margin in BOTH cases is what keeps the two from expiring together.
With the caller silent, worker and proxy would otherwise both sit on 300 s,
the proxy would give up a moment before the worker finished building its own
timeout result, and the graceful timeout report that path exists to deliver
would be replaced by a killed worker and a reset session.

A request beyond +MAX-PROXY-RPC-TIMEOUT+ is clamped in PARAMS as well, by
%CLAMP-TIMEOUT-PARAM, so the worker enforces the same figure this budget is
built on.  Clamping only here would invert the very ordering above: the proxy
would wait less than the worker's own deadline and kill it for still working."
  (ceiling (+ (%requested-worker-deadline params) *proxy-rpc-buffer*)))

(defun %clamp-timeout-param (params)
  "Lower PARAMS' timeout_seconds to +MAX-PROXY-RPC-TIMEOUT+ when it exceeds it.

The worker reads that key to set its own deadline, so clamping the proxy's
wait without clamping this would leave the worker enforcing the larger figure
and the proxy giving up first -- killing a worker that is still legitimately
working, which is the one outcome this whole budget exists to avoid.  PARAMS
is built fresh per call by WITH-PROXY-DISPATCH, so rewriting it is local to
this request."
  (let ((requested (and (hash-table-p params)
                        (coerce-timeout-seconds
                         (gethash "timeout_seconds" params)))))
    (when (and requested (> requested +max-proxy-rpc-timeout+))
      (setf (gethash "timeout_seconds" params) +max-proxy-rpc-timeout+))
    params))

(defun %with-execution-status (result outcome &optional note)
  "Mark RESULT, an error result the proxy built, with OUTCOME -- :NOT-EXECUTED
or :EXECUTION-UNKNOWN (see REQUEST-OUTCOME) -- as its execution_status, and
end its text with what that means for the caller, or with NOTE.

Said in the text as well as the field, because the text is all a client is
required to show: whether to send the request again depends on it, and a
request that may have run must not be sent again blindly."
  (let ((sentence (or note
                      (ecase outcome
                        (:not-executed
                         "This request was not run; send it again if you still need it.")
                        (:execution-unknown
                         "The request had reached the worker, so it may have run, partly or wholly; nothing it changed was undone, and it was not retried."))))
        (content (gethash "content" result)))
    (setf (gethash "execution_status" result)
          (string-downcase (symbol-name outcome)))
    (when (and (vectorp content) (plusp (length content)))
      (let ((item (aref content 0)))
        (setf (gethash "text" item)
              (format nil "~A ~A" (gethash "text" item) sentence))))
    result))

(defun %cancelled-before-run-result ()
  "Return the result for a request cancelled before it reached its worker."
  (%with-execution-status
   (make-ht "content" (text-content "Request cancelled before it was sent to the worker.")
            "isError" t)
   :not-executed
   "It was not run."))

(defun %proxied-request-failure (record session-id method worker condition)
  "Return the result for RECORD's call failing with CONDITION, classified by
how far the request got (REQUEST-OUTCOME), not by what the worker's failure
was called.

A worker found dead before this request was sent -- a request ahead of it
timed out, or was cancelled -- did not run this request, and is not reported
as this request timing out or crashing mid-run."
  (let ((outcome (request-outcome record)))
    (cond
      ((typep condition %cached-rpc-not-sent-sym%)
       (if (eq :cancelled (funcall %cached-rpc-not-sent-reason% condition))
           (%cancelled-before-run-result)
           (%with-execution-status
            (make-ht "content"
                     (text-content
                      "The worker this session was using was stopped to cancel another request before this one was sent to it.")
                     "isError" t)
            :not-executed)))
      ((typep condition %cached-worker-crashed-sym%)
       ;; Delivering the notification here is what settles the reset this
       ;; death owes the user, so consume the flag that records it.  The pool
       ;; hands an unconsumed one to the replacement worker instead -- which is
       ;; how a death nobody reported, during an internal RPC the pool makes
       ;; on its own behalf, still reaches the user rather than leaving them
       ;; talking to a fresh image that has lost their session.
       (ignore-errors (funcall %cached-check-and-clear% worker))
       (let ((reason (ignore-errors
                      (funcall %cached-worker-crashed-reason% condition)))
             (exit-status (ignore-errors
                           (funcall %cached-worker-last-exit-status% worker)))
             (exit-code (ignore-errors
                         (funcall %cached-worker-last-exit-code% worker))))
         (log-event :warn "proxy.worker-crashed"
                    "session" session-id "method" method
                    "reason" reason "outcome" (string-downcase (symbol-name outcome))
                    "exit_status" exit-status "exit_code" exit-code)
         (cond
           ((eq outcome :not-executed)
            (%with-execution-status
             (%crash-notification-result :reason (or reason "unknown")
                                         :exit-status exit-status
                                         :exit-code exit-code)
             :not-executed
             "The worker had stopped before this request was sent to it, so this request was not run; send it again if you still need it."))
           ((cancellation-requested-p record)
            (%with-execution-status
             (make-ht "content"
                      (text-content
                       "Request cancelled while it was running: the worker running it was stopped. All Lisp state (loaded systems, defined functions, package state) has been reset. Please run load-system again to restore your environment.")
                      "isError" t)
             :execution-unknown))
           ((equal reason "timeout")
            (%with-execution-status
             (make-ht "content"
                      (text-content
                       "Worker RPC timed out. The operation took too long and the worker was terminated. All Lisp state has been reset. Please run load-system again to restore your environment.")
                      "isError" t)
             :execution-unknown))
           (t
            (%with-execution-status
             (%crash-notification-result :reason (or reason "unknown")
                                         :exit-status exit-status
                                         :exit-code exit-code)
             :execution-unknown)))))
      (t
       (log-event :debug "proxy.worker-rpc-error"
                  "session" session-id "method" method
                  "error" (princ-to-string condition))
       (let ((result (make-ht "content"
                              (text-content
                               (format nil "Worker error: ~A"
                                       (sanitize-error-message
                                        (princ-to-string condition))))
                              "isError" t)))
         (if (eq outcome :completed)
             ;; The worker answered, with an error: the request ran.
             (progn (setf (gethash "execution_status" result) "completed")
                    result)
             (%with-execution-status result outcome)))))))

(defun %run-proxied-request (record session-id method params preserve-json-types)
  "Run the registered request RECORD: find its worker, and send it -- unless
it was cancelled first, which is checked at every boundary before the send
and, at the send itself, by WORKER-RPC's BEFORE-SEND hook."
  (when (cancellation-requested-p record)
    (return-from %run-proxied-request (%cancelled-before-run-result)))
  (note-request-phase record :acquiring)
  (let ((worker
          (handler-case (funcall %cached-get-or-assign% session-id)
            (error (e)
              (log-event :warn "proxy.pool-error"
                         "session" session-id
                         "method" method
                         "error" (princ-to-string e))
              (return-from %run-proxied-request
                (%with-execution-status
                 (make-ht "content"
                          (text-content
                           (format nil "Pool error: ~A"
                                   (sanitize-error-message (princ-to-string e))))
                          "isError" t)
                 :not-executed))))))
    ;; A cancellation that arrived while the worker was being found or
    ;; started: the worker stays the session's, the request does not run.
    (when (cancellation-requested-p record)
      (return-from %run-proxied-request (%cancelled-before-run-result)))
    (note-request-worker record worker)
    (when (funcall %cached-check-and-clear% worker)
      (let ((reason (ignore-errors
                     (funcall %cached-worker-last-crash-reason% worker)))
            (exit-status (ignore-errors
                          (funcall %cached-worker-last-exit-status% worker)))
            (exit-code (ignore-errors
                        (funcall %cached-worker-last-exit-code% worker))))
        (log-event :info "proxy.crash-notification"
                   "session" session-id
                   "method" method "reason" reason
                   "exit_status" exit-status "exit_code" exit-code)
        ;; The reset is reported in this request's place; the request itself
        ;; was never sent.
        (return-from %run-proxied-request
          (%with-execution-status
           (%crash-notification-result :reason reason
                                       :exit-status exit-status
                                       :exit-code exit-code)
           :not-executed))))
    (log-event :debug "proxy.forward" "session" session-id "method" method)
    (let ((effective-timeout (%effective-rpc-timeout (%clamp-timeout-param params))))
      (handler-case
          (apply %cached-worker-rpc% worker method params
                 :timeout effective-timeout
                 :before-send (lambda () (begin-send record))
                 :after-receive (lambda () (note-response record))
                 (when preserve-json-types
                   (list :preserve-json-types t)))
        (error (e)
          (%proxied-request-failure record session-id method worker e))))))

(defun proxy-to-worker (id method params &key preserve-json-types)
  "Proxy a tool call to the session's dedicated worker process.
Returns the worker's JSON-RPC result hash-table directly.
PRESERVE-JSON-TYPES asks for the result parsed so that it encodes back to the
JSON the worker wrote -- what a caller relaying it to the client needs.  A
caller that reads the result itself (clos-describe) leaves it off and gets
yason's defaults, which it was written against.

The request is registered under its session and ID for its whole life, first
of all -- before a worker is found for it -- so a cancellation that arrives
at any point finds it (src/request-lifecycle.lisp).  An error result this
builds carries execution_status: \"not-executed\" when the request never
reached the worker, \"execution-unknown\" when it did and no answer came.
Nothing is retried.

Uses atomic check-and-clear for crash notification to prevent
TOCTOU race with concurrent requests for the same session."
  (let ((session-id *current-session-id*))
    (unless (and (stringp session-id) (plusp (length session-id)))
      (error "Cannot proxy tool call: no session ID bound."))
    (%ensure-cached-bindings)
    (let ((record (register-request session-id id)))
      (unwind-protect
           (%run-proxied-request record session-id method params
                                 preserve-json-types)
        (unregister-request record)))))

(defun cancel-request (request-id &optional caller-session-id)
  "Cancel the request REQUEST-ID of CALLER-SESSION-ID, and return what that did,
or NIL when that session has no such request in flight.

The request is looked up under its session: another session's request with
the same id is a different request.  What happens depends on how far it got
(CANCEL-REQUEST-RECORD):

  :MARKED    it had not reached its worker; it stops at its next boundary
             without running, and its worker is left alone;
  :STOPPING  its worker was running it; the worker is signalled while the
             registry holds the request in :EXECUTING, then ended here;
  :TOO-LATE  it had already been answered; nothing is done.

A worker is only ever stopped for the request it is running at that moment,
never for a session's other requests."
  (let ((record (and caller-session-id
                     (find-request caller-session-id request-id))))
    (unless record
      (log-event :debug "proxy.cancel.not-found"
                 "request_id" (princ-to-string request-id))
      (return-from cancel-request nil))
    (%ensure-cached-bindings)
    (let ((verdict (cancel-request-record
                    record
                    (lambda (worker)
                      ;; SIGTERM first to break the RPC blocked in its read:
                      ;; it holds the stream lock KILL-WORKER needs.
                      (ignore-errors
                       (funcall %cached-signal-worker-terminate% worker))))))
      (log-event :info "proxy.cancel"
                 "request_id" (princ-to-string request-id)
                 "session" caller-session-id
                 "verdict" (string-downcase (symbol-name verdict)))
      (when (eq :stopping verdict)
        (ignore-errors (funcall %cached-kill-worker% (request-worker record))))
      verdict)))
