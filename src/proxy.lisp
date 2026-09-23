;;;; src/proxy.lisp
;;;;
;;;; Proxy layer for routing tool calls to worker processes.
;;;;
;;;; When *use-worker-pool* is non-nil, tool handlers call
;;;; proxy-to-worker instead of executing inline.  The proxy
;;;; resolves the current session's dedicated worker via the
;;;; pool manager and forwards the call as a JSON-RPC request.
;;;;
;;;; Reset notification: every worker a session loses is told to it in
;;;; exactly one response -- the failed request that met the loss, or the
;;;; session's next request, in its place (src/reset-events.lisp).

(defpackage #:cl-mcp/src/proxy
  (:use #:cl)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:result)
  (:import-from #:cl-mcp/src/log #:log-event)
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
  (:import-from #:cl-mcp/src/reset-events
                #:claim-session-resets
                #:worker-termination
                #:reset-event-worker-id
                #:reset-event-cause
                #:reset-event-reason
                #:reset-event-exit-status
                #:reset-event-exit-code)
  (:export #:proxy-to-worker
           #:with-proxy-dispatch
           #:*use-worker-pool*
           #:*proxy-rpc-timeout*
           #:verify-proxy-bindings
           #:%invalidate-proxy-cache
           #:*current-session-id*
           #:cancel-request
           #:termination-phrase
           #:reset-notice))

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
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-RPC")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-CRASHED")
    ("CL-MCP/SRC/WORKER-CLIENT" . "WORKER-CRASHED-REASON")
    ("CL-MCP/SRC/WORKER-CLIENT" . "RPC-NOT-SENT")
    ("CL-MCP/SRC/WORKER-CLIENT" . "RPC-NOT-SENT-REASON")
    ("CL-MCP/SRC/WORKER-CLIENT" . "RPC-ANSWER-WITHDRAWN")
    ("CL-MCP/SRC/WORKER-CLIENT" . "KILL-WORKER")
    ("CL-MCP/SRC/WORKER-CLIENT" . "SIGNAL-WORKER-TERMINATE")
    ("CL-MCP/SRC/WORKER-CLIENT" . "RECORD-WORKER-TERMINATION"))
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

(defun %exit-detail (event)
  "How EVENT's process ended, when that was observed -- \"exit code 1\" or
\"signal 9\" -- or NIL.  A process still running when its connection failed
has no exit to report, and saying \"running\" would describe nothing."
  (let ((status (reset-event-exit-status event))
        (code (reset-event-exit-code event)))
    (cond ((and (equal status "exited") (integerp code))
           (format nil "exit code ~D" code))
          ((and (equal status "signaled") (integerp code))
           (format nil "signal ~D" code)))))

(defun termination-phrase (event)
  "What happened to EVENT's worker, as a predicate: \"stopped unexpectedly
(eof, exit code 1)\", \"was stopped by pool-kill-worker\".  Said of the
cause the ledger recorded first, which for a worker the pool or a
cancellation stopped is that decision, not the EOF it produced."
  (let ((detail (%exit-detail event))
        (reason (reset-event-reason event)))
    (ecase (reset-event-cause event)
      (:crashed
       (let ((parts (remove nil (list (and (stringp reason) (plusp (length reason))
                                           reason)
                                      detail))))
         (format nil "stopped unexpectedly~@[ (~{~A~^, ~})~]" parts)))
      (:timeout "did not answer within its deadline and was abandoned")
      (:retired
       (format nil "retired itself~@[ (~A)~] rather than serve another ~
                    request: an earlier run exceeded its timeout and left a ~
                    thread that could not be stopped"
               detail))
      (:cancelled "was stopped to cancel the request it was running")
      (:killed "was stopped by pool-kill-worker")
      (:released "was stopped when its session was released")
      (:shutdown "was stopped when the worker pool shut down")
      (:stopped "was stopped by the worker pool"))))

(defun reset-notice (events &key worker-in-place)
  "Tell EVENTS -- state-loss events a session is being told for the first
time, oldest first -- as text: one sentence per worker, then what that cost.

Only what the caller knows is said.  WORKER-IN-PLACE says the session holds
another worker at this moment -- one was acquired for this very response --
and only then is that said.  Nothing is said about a worker still to come:
whether the next request gets one depends on capacity and on a spawn that
has not happened.

Each sentence begins \"Worker <id>\", so a worker's end is named once, in the
response that tells it, and nowhere else.  Every response that tells a reset
renders it here, the pool-kill-worker response included."
  (format nil "~{~A~^ ~} This session's Lisp state (loaded systems, defined ~
               functions, package state) was lost with ~:[it~;them~].~:[~; The ~
               session is now using another worker.~] Run load-system again to ~
               restore your environment."
          (mapcar (lambda (event)
                    (format nil "Worker ~A ~A."
                            (reset-event-worker-id event)
                            (termination-phrase event)))
                  events)
          (cdr events)
          worker-in-place))

;;; ---------------------------------------------------------------------------
;;; Cached late-bound function references
;;; ---------------------------------------------------------------------------

(defvar %cached-get-or-assign% nil
  "Cached fdefinition for POOL:GET-OR-ASSIGN-WORKER.")
(defvar %cached-find-session-worker% nil
  "Cached fdefinition for POOL:FIND-SESSION-WORKER.")
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

(defvar %cached-rpc-answer-withdrawn-sym% nil
  "Cached symbol WORKER-CLIENT:RPC-ANSWER-WITHDRAWN.")

(defvar %cached-kill-worker% nil
  "Cached fdefinition for WORKER-CLIENT:KILL-WORKER.")
(defvar %cached-signal-worker-terminate% nil
  "Cached fdefinition for WORKER-CLIENT:SIGNAL-WORKER-TERMINATE.")

(defvar %cached-record-worker-termination% ()
  "Cached fdefinition for WORKER-CLIENT:RECORD-WORKER-TERMINATION.")

(defun %ensure-cached-bindings ()
  "Populate the cached function bindings on first use.  Called once
per image after verify-proxy-bindings has validated the symbols."
  (unless %cached-get-or-assign%
    (setf %cached-get-or-assign%
          (fdefinition (%resolve "CL-MCP/SRC/POOL" "GET-OR-ASSIGN-WORKER"))
          %cached-find-session-worker%
          (fdefinition (%resolve "CL-MCP/SRC/POOL" "FIND-SESSION-WORKER"))
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
          %cached-rpc-answer-withdrawn-sym%
          (%resolve "CL-MCP/SRC/WORKER-CLIENT" "RPC-ANSWER-WITHDRAWN")
          %cached-kill-worker%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT" "KILL-WORKER"))
          %cached-signal-worker-terminate%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "SIGNAL-WORKER-TERMINATE"))
          %cached-record-worker-termination%
          (fdefinition (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                 "RECORD-WORKER-TERMINATION")))))

(defun %invalidate-proxy-cache ()
  "Reset all cached late-bound function references to NIL.
Called by initialize-pool to ensure stale bindings from a previous
image or pool lifecycle are cleared before re-verification."
  (setf %cached-get-or-assign% nil
        %cached-find-session-worker% nil
        %cached-worker-rpc% nil
        %cached-worker-crashed-sym% nil
        %cached-worker-crashed-reason% nil
        %cached-rpc-not-sent-sym% nil
        %cached-rpc-not-sent-reason% nil
        %cached-rpc-answer-withdrawn-sym% nil
        %cached-kill-worker% nil
        %cached-signal-worker-terminate% nil
        %cached-record-worker-termination% nil))

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

(defun %proxy-error-result (text outcome &key events note worker-in-place)
  "Return an error result saying TEXT and then telling EVENTS, when there are
any (RESET-NOTICE, with WORKER-IN-PLACE), marked with OUTCOME as its
execution_status.  For :NOT-EXECUTED and :EXECUTION-UNKNOWN the text ends
with what that means for the caller, or with NOTE (%WITH-EXECUTION-STATUS);
:COMPLETED, a request the worker answered with an error, needs no such
sentence.  With TEXT NIL, the notice is the whole message."
  (let* ((notice (and events (reset-notice events :worker-in-place worker-in-place)))
         (message (format nil "~@[~A~]~:[~; ~]~@[~A~]"
                          text (and text notice) notice))
         (result (make-ht "content" (text-content message) "isError" t)))
    (if (eq outcome :completed)
        (progn (setf (gethash "execution_status" result) "completed")
               result)
        (%with-execution-status result outcome note))))

(defun %cancelled-before-run-result (&optional events worker-in-place)
  "Return the result for a request cancelled before it reached its worker,
telling EVENTS as well when the session was owed any.  WORKER-IN-PLACE says
a worker was already acquired for the request, and stays the session's."
  (%proxy-error-result "Request cancelled before it was sent to the worker."
                       :not-executed :events events :note "It was not run."
                       :worker-in-place worker-in-place))

(defun %proxied-request-failure (record session-id method worker condition)
  "Return the result for RECORD's call failing with CONDITION, classified by
how far the request got (REQUEST-OUTCOME) and by why its worker ended as the
reset ledger recorded it (src/reset-events.lisp) -- not by what the transport
saw, which for a worker a cancellation or pool-kill-worker stopped is an EOF
like any crash.

A worker found dead before this request was sent -- a request ahead of it
timed out, or was cancelled -- did not run this request, and is not reported
as this request timing out or crashing mid-run.  Nor did a worker that
retired: it exits on receiving a request, before running it.

Whatever resets the session is owed and has not been told -- this worker's
end among them, unless another response told it first -- are claimed and
told here, once."
  (let* ((outcome (request-outcome record))
         (events (claim-session-resets session-id))
         (event (and worker (worker-termination worker)))
         (cause (and event (reset-event-cause event))))
    (flet ((fail (text outcome &optional note)
             (%proxy-error-result text outcome :events events :note note)))
      (cond
        ((typep condition %cached-rpc-not-sent-sym%)
         (if (eq :cancelled (funcall %cached-rpc-not-sent-reason% condition))
             (%cancelled-before-run-result events)
             (fail "The worker this session was using was stopped to cancel another request before this one was sent to it."
                   :not-executed)))
        ((typep condition %cached-rpc-answer-withdrawn-sym%)
         ;; The worker answered, but the cancellation reached the registry
         ;; first and stopped it: the cancellation is what is reported.  Its
         ;; stop left the session a fresh image, so the answer, delivered now,
         ;; would describe state that no longer exists.
         (fail "Request cancelled while it was running: the worker running it was stopped."
               :execution-unknown))
        ((typep condition %cached-worker-crashed-sym%)
         (let ((reason (ignore-errors
                        (funcall %cached-worker-crashed-reason% condition))))
           (log-event :warn "proxy.worker-crashed"
                      "session" session-id "method" method
                      "reason" reason
                      "cause" (and cause (string-downcase (symbol-name cause)))
                      "outcome" (string-downcase (symbol-name outcome)))
           (cond
             ((eq outcome :not-executed)
              (fail (format nil "The worker this session was using ~A before this request was sent to it."
                            (if event (termination-phrase event) "had stopped"))
                    :not-executed
                    "This request was not run; send it again if you still need it."))
             ((cancellation-requested-p record)
              (fail "Request cancelled while it was running: the worker running it was stopped."
                    :execution-unknown))
             ((eq cause :retired)
              (fail (format nil "The worker this request was sent to ~A; it did so on receiving this request, before running it."
                            (termination-phrase event))
                    :not-executed
                    "This request was not run; send it again if you still need it."))
             ((eq cause :timeout)
              (fail "Worker RPC timed out: the worker did not answer within its deadline, so the proxy stopped waiting for it and is stopping it."
                    :execution-unknown))
             (event
              (fail (format nil "The worker running this request ~A."
                            (termination-phrase event))
                    :execution-unknown))
             (t
              (fail (format nil "The worker running this request stopped unexpectedly~@[ (~A)~]."
                            reason)
                    :execution-unknown)))))
        (t
         (log-event :debug "proxy.worker-rpc-error"
                    "session" session-id "method" method
                    "error" (princ-to-string condition))
         ;; The worker answered, with an error: the request ran.
         (fail (format nil "Worker error: ~A"
                       (sanitize-error-message (princ-to-string condition)))
               (if (eq outcome :completed) :completed outcome)))))))

(defun %run-proxied-request (record session-id method params preserve-json-types)
  "Run the registered request RECORD: find its worker, and send it -- unless
it was cancelled first, which is checked at every boundary before the send
and, at the send itself, by WORKER-RPC's BEFORE-SEND hook.

A session owed resets it has not been told (src/reset-events.lisp) is told
them in this request's place, once its worker is found: the request is not
sent, because it was written against state the session no longer has."
  (when (cancellation-requested-p record)
    (return-from %run-proxied-request
      (%cancelled-before-run-result (claim-session-resets session-id))))
  (note-request-phase record :acquiring)
  (let ((worker
          (handler-case (funcall %cached-get-or-assign% session-id)
            (error (e)
              (log-event :warn "proxy.pool-error"
                         "session" session-id
                         "method" method
                         "error" (princ-to-string e))
              (return-from %run-proxied-request
                (%proxy-error-result
                 (format nil "Pool error: ~A"
                         (sanitize-error-message (princ-to-string e)))
                 :not-executed
                 :events (claim-session-resets session-id)))))))
    ;; A cancellation that arrived while the worker was being found or
    ;; started: the worker stays the session's, the request does not run.
    ;; That worker was acquired, so the notice may say the session has one.
    (when (cancellation-requested-p record)
      (return-from %run-proxied-request
        (%cancelled-before-run-result (claim-session-resets session-id) t)))
    (note-request-worker record worker)
    (let ((events (claim-session-resets session-id)))
      (when events
        (log-event :info "proxy.reset-notification"
                   "session" session-id "method" method
                   "workers" (format nil "~{~A~^,~}"
                                     (mapcar #'reset-event-worker-id events)))
        (return-from %run-proxied-request
          (%proxy-error-result nil :not-executed :events events
                              :worker-in-place t))))
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

Each reset the session is owed is told in exactly one response, by claiming
it from the reset ledger (src/reset-events.lisp)."
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
                      ;; Why the worker is ending is recorded before it is
                      ;; signalled, so the EOF the running request then meets
                      ;; is not taken for a crash (src/reset-events.lisp).
                      (ignore-errors
                       (funcall %cached-record-worker-termination%
                                worker :cancelled))
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
