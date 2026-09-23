;;;; src/pool.lisp
;;;;
;;;; Worker pool manager with strict session affinity.
;;;;
;;;; Design principles:
;;;; 1. Strict exclusive affinity: 1 session = 1 dedicated worker.
;;;;    No sharing, no fallback.
;;;; 2. Scale-out: When standbys exhausted, spawn new workers on demand.
;;;; 3. Warm standbys: Pool pre-spawns workers ready for immediate
;;;;    assignment.
;;;; 4. Crash recovery: Detect crash, restart worker, and record the
;;;;    state loss in the reset ledger (src/reset-events.lisp).
;;;; 5. Explicit reset notification: each worker a session loses is told
;;;;    to it exactly once, then normal operation.
;;;;
;;;; Thread safety uses a three-level lock hierarchy:
;;;;   *pool-lock* (global) -> placeholder.lock -> worker.stream-lock
;;;; Never acquire in reverse order.

(defpackage #:cl-mcp/src/pool
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held
                #:make-condition-variable #:condition-wait
                #:make-thread #:threadp #:thread-alive-p
                #:join-thread)
  (:import-from #:cl-mcp/src/worker-client
                #:worker
                #:spawn-worker #:worker-rpc #:kill-worker
                #:signal-worker-terminate
                #:worker-state #:worker-session-id
                #:record-worker-termination
                #:worker-leaked-threads
                #:worker-tcp-port
                #:worker-pid #:worker-id
                #:worker-process-info
                #:worker-crash-history-pushed-p
                #:worker-last-crash-reason
                #:worker-last-exit-status
                #:worker-last-exit-code
                #:worker-crashed
                #:worker-crashed-reason
                #:worker-retired-p
                #:worker-retirement-recorded
                #:exit-code-says-retired-p
                #:*reaper-threads* #:*reaper-threads-lock*
                #:*worker-startup-timeout*)
  (:import-from #:cl-mcp/src/utils/deadline
                #:*retired-leaked-thread-reason*)
  (:import-from #:cl-mcp/src/proxy
                #:verify-proxy-bindings
                #:%invalidate-proxy-cache)
  (:import-from #:cl-mcp/src/request-lifecycle
                #:clear-requests)
  (:import-from #:cl-mcp/src/reset-events
                #:amend-termination-exit
                #:claim-session-resets
                #:discard-session-resets
                #:discard-all-resets)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:*worker-pool-warmup*
           #:*max-pool-size*
           #:*worker-init-config*
           #:%warn-if-init-without-pool
           #:*health-check-interval-seconds*
           #:*shutdown-replenish-wait-seconds*
           #:initialize-pool
           #:shutdown-pool
           #:get-or-assign-worker
           #:release-session
           #:kill-session-worker
           #:broadcast-root-to-workers
           #:send-root-to-session-worker
           #:pool-worker-info
           #:pool-shutting-down
           #:pool-capacity-exceeded
           #:pool-spawn-cancelled
           #:*recovery-threads*
           #:find-session-worker
           #:pool-status-info))

(in-package #:cl-mcp/src/pool)

(define-condition pool-shutting-down (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Worker pool is shutting down"))))

(define-condition pool-capacity-exceeded (error)
  ((limit :initarg :limit :reader pool-capacity-exceeded-limit))
  (:report (lambda (c s)
             (format s "Pool size limit reached (~D workers). ~
                       Release unused sessions before creating new ones."
                     (pool-capacity-exceeded-limit c)))))

(define-condition pool-spawn-cancelled (error)
  ((session-id :initarg :session-id :reader pool-spawn-cancelled-session-id))
  (:report (lambda (c s)
             (format s "Session ~A was released during worker spawn."
                     (pool-spawn-cancelled-session-id c)))))

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defun %env-int (name default &key (min nil))
  "Read an integer from the environment variable NAME.  Return DEFAULT
when NAME is unset or empty.  When the value is unparseable -- or,
with MIN given, below MIN -- emit a warning and return DEFAULT.

Used to seed pool-tuning defvars from the environment so operators
can tune cl-mcp without editing source.  Each fresh SBCL process
reads the environment once at load time."
  (let ((s (uiop:getenv name)))
    (cond
      ((or (null s) (zerop (length s))) default)
      (t
       (handler-case
           (let ((n (parse-integer s)))
             (cond
               ((and min (< n min))
                (warn "~A=~A is below ~D; using default ~D"
                      name n min default)
                default)
               (t n)))
         (error ()
           (warn "~A=~S is not an integer; using default ~D"
                 name s default)
           default))))))

(defun %env-string (name)
  "Return the environment variable NAME as a string, or NIL when unset or
empty."
  (let ((s (uiop:getenv name)))
    (and s (plusp (length s)) s)))

(defvar *worker-pool-warmup*
  (%env-int "CL_MCP_WORKER_POOL_WARMUP" 1 :min 0)
  "Number of standby workers to pre-spawn and maintain.
Default 1; override with the CL_MCP_WORKER_POOL_WARMUP env var (must
be a non-negative integer).  Set to 0 to disable pre-spawning;
workers are then spawned on demand as sessions arrive.  This is the
right setting when an MCP client has a tight handshake-timeout
budget and no spare warmup time on a cold FASL cache.")

(defvar *max-pool-size*
  (%env-int "CL_MCP_MAX_POOL_SIZE" 16 :min 1)
  "Maximum total number of workers (bound + standby).  Prevents
unbounded resource consumption from unlimited session creation.
Each SBCL worker uses ~100-500MB memory.
Default 16; override with the CL_MCP_MAX_POOL_SIZE env var (must
be a positive integer).  Must be >= *worker-pool-warmup*.")

(defvar *health-check-interval-seconds* 10.0d0
  "Seconds between health monitor iterations while the pool is running.")

(defvar *shutdown-replenish-wait-seconds* 0.05d0
  "Polling interval (seconds) while waiting for replenish thread shutdown.")

(defvar *worker-init-config* nil
  "Parsed worker-init-hook config, or NIL when the feature is off.
A plist: (:system S :entry E :eval EV :package P :max-failures N :mode M).")

(defun %parse-worker-init-config ()
  "Read MCP_WORKER_INIT_* from the environment into a config plist, or NIL
when MCP_WORKER_INIT_SYSTEM is unset (feature off).  MCP_WORKER_INIT_SYSTEM
is the master gate, mirroring MCP_WORKER_SWANK."
  (let ((system (%env-string "MCP_WORKER_INIT_SYSTEM")))
    (when system
      (list :system system
            :entry (%env-string "MCP_WORKER_INIT_ENTRY")
            :eval (%env-string "MCP_WORKER_INIT_EVAL")
            :package (or (%env-string "MCP_WORKER_INIT_PACKAGE") "CL-USER")
            :max-failures (%env-int "MCP_WORKER_INIT_MAX_FAILURES" 1 :min 1)
            :mode (or (%env-string "MCP_WORKER_INIT_MODE") "singleton")))))

(defun %warn-if-init-without-pool (pool-enabled-p)
  "Warn (and log) when MCP_WORKER_INIT_SYSTEM is set while the worker pool
is disabled (POOL-ENABLED-P is NIL) -- the init hook is inert in that
configuration, so a silent no-op would look like a broken web server.
Called from the transport entry points after the pool-enabled decision."
  (when (and (%env-string "MCP_WORKER_INIT_SYSTEM") (not pool-enabled-p))
    (log-event :warn "pool.init-hook.inert"
               "reason" "MCP_WORKER_INIT_* set but worker pool disabled")
    (warn "MCP_WORKER_INIT_* is set but the worker pool is disabled ~
(MCP_NO_WORKER_POOL=1 or :worker-pool nil): the worker init hook is inert. ~
Enable the pool to use it.")))

;;; ---------------------------------------------------------------------------
;;; Global pool state
;;; ---------------------------------------------------------------------------

(defvar *pool-lock* (bt:make-lock "pool-lock")
  "Global mutex protecting pool state and the affinity map.")

(defvar *affinity-map* (make-hash-table :test 'equal)
  "Maps session-id (string) to worker or worker-placeholder.")

(defvar *standby-workers* nil
  "List of workers in :standby state ready for assignment.")

(defvar *all-workers* nil
  "List of all live worker structs (for shutdown and info).")

(defvar *health-thread* nil
  "Background thread for health monitoring.")

(defvar *health-monitor-lock* (bt:make-lock "pool-health-monitor-lock")
  "Lock protecting health monitor wait/signal coordination.")

(defvar *health-monitor-condvar*
  (bt:make-condition-variable :name "pool-health-monitor-condvar")
  "Condition variable used to wake the health monitor during shutdown.")

(defvar *pool-running* nil
  "Flag controlling the health monitor loop.  Set to NIL to stop.")

(defvar *replenish-running* nil
  "Flag preventing concurrent replenish threads.  Set under *pool-lock*.")

(defvar *replenish-thread* nil
  "The handle of the replenishment last started, or NIL.  SHUTDOWN-POOL waits
for it when it is a thread.  Set under *pool-lock*.")

(defvar *recovery-threads* nil
  "List of active crash recovery threads.  Maintained under *pool-lock*.")

(defvar *crash-history* (make-hash-table :test 'equal)
  "Maps session-id to list of crash timestamps (universal-time).
Used by the circuit breaker to detect repeated crash loops.")

(defparameter *crash-breaker-window* 300
  "Time window in seconds for the crash circuit breaker.")

(defparameter *crash-breaker-threshold* 3
  "Maximum crashes allowed within *crash-breaker-window* before
halting recovery for a session.")

(defparameter *max-concurrent-recoveries* 4
  "Maximum number of concurrent crash recovery threads.
Prevents resource exhaustion when many workers crash simultaneously.")

(defvar *runtime-owner* nil
  "The current runtime owner as (SESSION-ID . WORKER), or NIL.  The owner
is the single worker permitted to run a singleton init (bind the fixed
app port).  WORKER is NIL once the owner's worker has died: the session
keeps ownership until it is released, re-elected with a new worker, or
reclaimed.  Guarded by *pool-lock*.")

(defvar *runtime-init-failures* 0
  "Count of soft init failures for the current runtime.  Guarded by *pool-lock*.")

(defvar *runtime-init-disabled* nil
  "When T, init is not (re-)attempted until re-armed (pool-kill-worker /
config reload).  Guarded by *pool-lock*.")

(defvar *init-attributable-crashes* (make-hash-table :test 'eql)
  "Set of worker IDs whose crash happened during a cl-mcp-triggered init.
Such crashes are excluded from the crash circuit breaker.  Guarded by
*pool-lock*.")

;;; ---------------------------------------------------------------------------
;;; Worker lifecycle and background work
;;;
;;; The pool reaches a worker's process, and starts its background work,
;;; only through the four specials below.  Each names the function the
;;; production pool uses, as a symbol so a redefinition is picked up.  They
;;; exist so the ownership contracts (specs/pool-ownership.lisp) can run the
;;; real pool over generated operation sequences with workers that are not
;;; processes, and run its background work at chosen points.  Nothing in the
;;; pool rebinds them.
;;; ---------------------------------------------------------------------------

(defvar *spawn-worker-function* 'spawn-worker
  "Function of no arguments that starts a worker and returns it in :STANDBY.")

(defvar *kill-worker-function* 'kill-worker
  "Function of one worker that ends it: its process, its connection and its
state, which becomes :DEAD.")

(defvar *worker-alive-function* '%worker-process-alive-p
  "Function of one worker that answers whether its process is alive.")

(defvar *start-pool-thread-function* '%start-pool-thread
  "Function of a thunk and a name that runs the thunk as background work and
returns a handle for it.  The pool joins a handle only when it is a thread.")

(defun %spawn-worker ()
  "Start a worker through *SPAWN-WORKER-FUNCTION*."
  (funcall *spawn-worker-function*))

(defun %kill-worker (worker)
  "End WORKER through *KILL-WORKER-FUNCTION*."
  (funcall *kill-worker-function* worker))

(defun %worker-alive-p (worker)
  "Answer whether WORKER's process is alive, through *WORKER-ALIVE-FUNCTION*."
  (funcall *worker-alive-function* worker))

(defun %start-pool-thread (thunk name)
  "Run THUNK on a new thread called NAME and return the thread."
  (bt:make-thread thunk :name name))

(defun %start-pool-work (thunk name)
  "Start THUNK as background work called NAME, through
*START-POOL-THREAD-FUNCTION*, and return its handle."
  (funcall *start-pool-thread-function* thunk name))

(defvar *signal-worker-function* 'signal-worker-terminate
  "Function of one worker that asks its process to stop, so an RPC blocked on
it returns and releases the worker's stream -- which ending the worker needs.
The fifth lifecycle seam, beside the four above.")

(defun %signal-worker (worker)
  "Ask WORKER's process to stop, through *SIGNAL-WORKER-FUNCTION*."
  (funcall *signal-worker-function* worker))

;;; ---------------------------------------------------------------------------
;;; Work in flight outside the pool lock
;;;
;;; Spawning and ending a worker take seconds, so the pool does both outside
;;; *POOL-LOCK*.  In between, the worker is in none of the pool's lists: a
;;; worker being spawned is not registered yet, and one being ended has been
;;; taken out already.  A shutdown that snapshots the lists and returns would
;;; miss both, and return while a worker it was responsible for still lives.
;;;
;;; So each is accounted for from the moment it is decided, in the same
;;; critical section that decides it: a spawn operation is counted, and a
;;; worker taken out to be ended is listed.  SHUTDOWN-POOL waits until both
;;; are gone (%WAIT-FOR-WORK-IN-FLIGHT).
;;; ---------------------------------------------------------------------------

(defvar *spawns-in-flight* 0
  "Spawn operations decided and not yet finished: each ends with its worker
registered in the pool's lists, or ended.  Guarded by *POOL-LOCK*.")

(defvar *ending-workers* '()
  "Workers taken out of the pool's lists to be ended, and not ended yet.
Guarded by *POOL-LOCK*.")

(defvar *work-in-flight-condvar*
  (bt:make-condition-variable :name "pool-work-in-flight")
  "Broadcast, with *POOL-LOCK* held, whenever a spawn operation finishes or a
worker in *ENDING-WORKERS* has been ended.")

(defun %begin-spawn ()
  "Count a spawn operation as in flight.  Call with *POOL-LOCK* held, in the
critical section that decides the spawn, so no shutdown can look between the
decision and the count."
  (incf *spawns-in-flight*))

(defun %end-spawn ()
  "Count a spawn operation as finished -- its worker registered or ended.
Takes *POOL-LOCK*."
  (bt:with-lock-held (*pool-lock*)
    (decf *spawns-in-flight*)
    (%condition-broadcast *work-in-flight-condvar*)))

(defun %begin-ending (worker)
  "List WORKER as taken out of the pool's lists to be ended, and return it.
Call with *POOL-LOCK* held, in the critical section that takes it out."
  (push worker *ending-workers*)
  worker)

(defun %end-worker (worker)
  "End WORKER, listed by %BEGIN-ENDING, outside *POOL-LOCK*, and then take it
off the list however ending it went."
  (unwind-protect (ignore-errors (%kill-worker worker))
    (bt:with-lock-held (*pool-lock*)
      (setf *ending-workers* (remove worker *ending-workers*))
      (%condition-broadcast *work-in-flight-condvar*))))

(defun %wait-for-work-in-flight (seconds)
  "Wait up to SECONDS until no spawn operation is in flight and no worker is
waiting to be ended.  Returns true when that was reached; otherwise NIL, with
what is still in flight as the second and third values."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (bt:with-lock-held (*pool-lock*)
      (loop
        (when (and (zerop *spawns-in-flight*) (null *ending-workers*))
          (return t))
        (let ((remaining (/ (- deadline (get-internal-real-time))
                            internal-time-units-per-second)))
          (unless (plusp remaining)
            (return (values nil *spawns-in-flight* (length *ending-workers*))))
          (bt:condition-wait *work-in-flight-condvar* *pool-lock*
                             :timeout (min remaining 1)))))))

(defvar *pool-generation* 0
  "Which pool is running: INITIALIZE-POOL counts up.  Background work notes
the generation it was started for and adds nothing to a later one -- a
replenishment still spawning when its pool was shut down must not hand its
worker to the pool initialized after.  Guarded by *POOL-LOCK*.")

(defun %with-owner-reset (thunk)
  "Test helper: reset ownership/failure state under *pool-lock*, then run THUNK."
  (bt:with-lock-held (*pool-lock*)
    (setf *runtime-owner* nil
          *runtime-init-failures* 0
          *runtime-init-disabled* nil)
    (clrhash *init-attributable-crashes*))
  (funcall thunk))

(defun %elect-runtime-owner (worker session-id)
  "Grant runtime ownership to WORKER for SESSION-ID and return T, else NIL.
MUST be called with *pool-lock* held (reads/writes *runtime-owner*).
Grant iff the current owner is NIL, is the SAME session, or the current
owner's worker is dead AND its session is gone from *affinity-map*.
NEVER migrate to a different session while the OWNER SESSION is still
alive (present in *affinity-map*), even if that session's current worker
has crashed and is being recovered -- this keeps the app and the
developer's repl-eval/load-system in the same process and keeps exactly
one holder of the fixed port."
  (let ((current *runtime-owner*))
    (when (or (null current)
              (string= (car current) session-id)
              ;; Reclaim to a DIFFERENT session only when the previous owner's
              ;; worker is dead AND its session is gone from the affinity map.
              ;; Never migrate the runtime to another still-live session -- that
              ;; would land the developer's hot-reload in the wrong process (L4).
              ;; A NIL worker is one that died (%DROP-OWNER-WORKER).
              (and (not (and (cdr current) (%worker-alive-p (cdr current))))
                   (not (gethash (car current) *affinity-map*))))
      ;; A new runtime (no prior owner, or a different session) starts with a
      ;; clean soft-failure count; a same-session re-election preserves it.
      (unless (and current (string= (car current) session-id))
        (setf *runtime-init-failures* 0))
      (setf *runtime-owner* (cons session-id worker))
      (log-event :info "pool.runtime-owner.elected"
                 "session" session-id "worker_id" (worker-id worker))
      (return-from %elect-runtime-owner t))
    (log-event :info "pool.init.skipped-not-owner"
               "session" session-id "owner_session" (car current))
    nil))

(defun %release-runtime-owner-if (worker)
  "Clear *runtime-owner* if WORKER is the current owner.  Takes *pool-lock*.
Called (from paths NOT already holding the lock) when an owner worker is
released, killed, or removed on crash."
  (bt:with-lock-held (*pool-lock*)
    (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker))
      (log-event :info "pool.runtime-owner.released"
                 "worker_id" (worker-id worker))
      (setf *runtime-owner* nil))))

(defun %drop-owner-worker (worker)
  "Forget WORKER as the runtime owner's worker when it is, keeping the owner's
session.  Must be called with *pool-lock* held.

For a worker that died: the session keeps ownership, so the singleton runtime
is not taken over by another live session while this one is recovered (see
%ELECT-RUNTIME-OWNER), but nothing may go on naming a worker that is gone --
pool-status reported it as the owner, and a check of the owner's liveness
asked a process that no longer exists."
  (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker))
    (log-event :info "pool.runtime-owner.worker-lost"
               "session" (car *runtime-owner*) "worker_id" (worker-id worker))
    (setf *runtime-owner* (cons (car *runtime-owner*) nil))))

(defun %init-attributable-crash-p (worker)
  "T if WORKER's crash was attributed to a cl-mcp-triggered init.  Must be
called with *pool-lock* held (reads *init-attributable-crashes*).  Such
crashes are excluded from the crash circuit breaker so a bad web-server
init cannot brick a session's repl-eval/load-system."
  (gethash (worker-id worker) *init-attributable-crashes*))

(defun %init-params (config)
  "Build the worker/init-start params hash-table from CONFIG plist."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "system" ht) (getf config :system)
          (gethash "entry" ht) (getf config :entry)
          (gethash "eval" ht) (getf config :eval)
          (gethash "package" ht) (getf config :package))
    ht))

(defun %retirement-crash-p (condition)
  "True when CONDITION is a worker that retired deliberately rather than died.

A worker exits when it finds it is still carrying a thread a deadline could
not stop, and that reaches the parent as a crash like any other.  It is not
evidence about whatever the worker happened to be doing at the time, so
callers that draw conclusions from a crash have to exclude it.

Typed rather than wrapped in IGNORE-ERRORS: the first version of this guarded
the reader that way, and when the reader turned out not to be imported here
the swallowed undefined-function error made the whole exclusion silently
inert.  A TYPEP costs the same and cannot hide that."
  (and (typep condition 'worker-crashed)
       (equal *retired-leaked-thread-reason*
              (worker-crashed-reason condition))))

(defun %breaker-countable-crash-p (worker)
  "True when WORKER's death should count toward its session's circuit breaker.

Both push sites ask this rather than each spelling the exclusions out, because
they had drifted: one of them consulted the crash reason after the surrounding
function had already overwritten it, so the guard read as effective and could
never fire.

A deliberate retirement is excluded.  Three uninterruptible timeouts in five
minutes would otherwise trip the breaker and halt the session, where the same
three before this behaviour existed returned three timeouts and left the user
working.  WORKER-RETIRED-P is a slot recorded by whoever classified the death
rather than a question asked of the crash reason: the reason was once copied
onto the replacement worker, and deriving the answer from it made a live
worker report its predecessor's retirement as its own -- which would have
disabled this breaker for the rest of the session, exactly where a crash loop
is what the breaker is for.

An init-attributable crash is excluded for the reason it always was: the pool
counts those separately, against initialization rather than the session."
  (and (not (worker-retired-p worker))
       (not (%init-attributable-crash-p worker))))

(defun %record-worker-death (worker exit-status exit-code
                             &key already-classified)
  "Record what killed WORKER and return whether it was a deliberate retirement.

Must be called under *POOL-LOCK*, in the same critical section that moves the
worker to :CRASHED.  GET-OR-ASSIGN-WORKER takes that lock before asking the
breaker about a crashed worker, so publishing the state first would let a
request arriving in between count an unclassified retirement -- three of those
halt the session, which is the failure this whole exclusion exists to prevent.

The exit code is recorded before it is consulted because on this path -- the
health monitor reaching a dead worker before any RPC has seen the EOF --
nothing has classified the death and the code the worker left is the only
witness there is.

ALREADY-CLASSIFIED says the caller found this worker already marked crashed,
so its reason describes this worker's own death and is kept: an RPC that met
the death knows it was a \"timeout\" or a \"stream-error\", where this
function only ever knows the process is gone, and \"process-died\" tells the
user less than the truth it would overwrite.

The death is recorded in the reset ledger too, owed to the session while
WORKER is still :BOUND.  The ledger keeps the first record, so a worker whose
end was already decided -- cancelled, killed, released -- keeps that cause,
and only its exit details are filled in from this fresher reading."
  (let ((status (or exit-status
                    (and already-classified (worker-last-exit-status worker))))
        (code (or exit-code
                  (and already-classified (worker-last-exit-code worker)))))
    (setf (worker-last-exit-status worker) (or status "unknown")
          (worker-last-exit-code worker) (or code "unknown")))
  (let ((retired (or (worker-retired-p worker)
                     (exit-code-says-retired-p worker))))
    (setf (worker-retirement-recorded worker) retired
          (worker-last-crash-reason worker)
          (cond (retired *retired-leaked-thread-reason*)
                ((and already-classified (worker-last-crash-reason worker)))
                (t "process-died")))
    (record-worker-termination worker (if retired :retired :crashed)
                               :reason (worker-last-crash-reason worker))
    (amend-termination-exit worker exit-status exit-code)
    retired))

(defun %monitor-init (worker session-id max-failures)
  "Poll worker/init-status until terminal, updating failure/disable state.
Runs on a short-lived background thread with backoff (0.1s -> 2s cap; no
hard wall-clock deadline -- a slow cold-FASL init legitimately stays
:loading and is resolved by the operator via pool-kill-worker).  All state
mutations are guarded on this worker still being the runtime owner.

On terminal :running or (soft) :failed the worker is still alive, so the
eager init-attributable mark is cleared -- a later crash of it is a
workload crash that SHOULD feed the breaker.  On a soft :failed BELOW
max-failures, ownership is RETAINED (so no other live session can migrate
the singleton runtime in); it is released only once quarantined
(disabled=t), preserving the disabled=t => owner=nil invariant.  A crash
DURING init is init-attributable (excluded from the breaker) and disables
further init."
  (handler-case
      (let ((delay 0.1))
        (loop
          (sleep delay)
          (setf delay (min (* delay 1.5) 2.0))
          (let* ((st (worker-rpc worker "worker/init-status" nil :timeout 5))
                 (state (and (hash-table-p st) (gethash "init_state" st))))
            (cond
              ((equal state "running")
               (log-event :info "pool.init.running"
                          "session" session-id
                          "app_port" (gethash "app_port" st))
               (bt:with-lock-held (*pool-lock*)
                 (remhash (worker-id worker) *init-attributable-crashes*))
               (return))
              ((equal state "failed")
               (log-event :warn "pool.init.failed"
                          "session" session-id
                          "error" (gethash "last_init_error" st))
               (let ((release nil))
                 (bt:with-lock-held (*pool-lock*)
                   ;; init terminally soft-failed but the worker is alive -> a
                   ;; later crash is a workload crash: clear the eager mark.
                   (remhash (worker-id worker) *init-attributable-crashes*)
                   (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker))
                     (incf *runtime-init-failures*)
                     (when (>= *runtime-init-failures* max-failures)
                       (setf *runtime-init-disabled* t
                             release t)
                       (log-event :warn "pool.init.disabled"
                                  "failures" *runtime-init-failures*))))
                 ;; Release ownership ONLY once quarantined; below max we keep
                 ;; ownership so a different live session cannot take over.
                 (when release
                   (%release-runtime-owner-if worker)))
               (return))
              (t nil)))))
    (worker-crashed (c)
      ;; A worker that retired for carrying a leaked thread is not evidence
      ;; about init: it exited deliberately, and blaming init for it disables
      ;; initialization for every later worker in the pool.
      (bt:with-lock-held (*pool-lock*)
        (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker)
                   (not (%retirement-crash-p c)))
          (setf (gethash (worker-id worker) *init-attributable-crashes*) t
                *runtime-init-disabled* t)
          (log-event :warn "pool.init.hard-crash"
                     "session" session-id "worker_id" (worker-id worker))))
      (%release-runtime-owner-if worker))
    (serious-condition (e)
      (log-event :warn "pool.init.monitor-error"
                 "session" session-id "error" (princ-to-string e))
      (%release-runtime-owner-if worker))))

(defun %ensure-runtime-init (worker session-id)
  "Elect WORKER as runtime owner for SESSION-ID and, if elected, send a
fire-and-forget worker/init-start RPC (fast ack) and spawn a monitor
thread.  No-op when the feature is off or init is disabled.  Must be
called at the :bound transition, after the handshake.  Holds *pool-lock*
only across the election, not across the RPC.

On election, the worker is marked init-attributable EAGERLY (under the
same lock the crash path checks) so that a crash during init is excluded
from the circuit breaker even if the health monitor detects the dead
worker before the init monitor does."
  (let ((config nil) (granted nil))
    (bt:with-lock-held (*pool-lock*)
      (setf config *worker-init-config*)
      (when (and config (not *runtime-init-disabled*))
        (setf granted (%elect-runtime-owner worker session-id))
        (when granted
          (setf (gethash (worker-id worker) *init-attributable-crashes*) t))))
    (when granted
      (handler-case
          (progn
            (worker-rpc worker "worker/init-start" (%init-params config)
                        :timeout 5)
            (let ((max-failures (getf config :max-failures 1)))
              (bt:make-thread
               (lambda () (%monitor-init worker session-id max-failures))
               :name (format nil "pool-init-monitor-~A" (worker-id worker)))))
        (worker-crashed (c)
          ;; See %MONITOR-INIT: a deliberate retirement is not an init failure.
          (bt:with-lock-held (*pool-lock*)
            (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker)
                       (not (%retirement-crash-p c)))
              (setf (gethash (worker-id worker) *init-attributable-crashes*) t
                    *runtime-init-disabled* t)))
          (unless (%retirement-crash-p c)
            (log-event :warn "pool.init.hard-crash"
                       "session" session-id "worker_id" (worker-id worker)))
          (%release-runtime-owner-if worker))
        (error (e)
          ;; A non-crash init-start failure means init never ran; drop the
          ;; eager mark so a later crash of this (still-live) worker counts.
          (bt:with-lock-held (*pool-lock*)
            (remhash (worker-id worker) *init-attributable-crashes*))
          (log-event :warn "pool.init.start-failed"
                     "session" session-id "error" (princ-to-string e))
          (%release-runtime-owner-if worker))))))

;;; ---------------------------------------------------------------------------
;;; Placeholder struct -- coordinates concurrent spawn requests
;;; ---------------------------------------------------------------------------

(defun %condition-broadcast (condvar)
  "Wake ALL threads waiting on CONDVAR.
On SBCL, uses sb-thread:condition-broadcast.  On other
implementations, falls back to condition-notify (wakes at least one)."
  #+sbcl (sb-thread:condition-broadcast condvar)
  #-sbcl (bt:condition-notify condvar))

(defstruct worker-placeholder
  "Placeholder inserted into the affinity map while a worker is being
spawned.  Other threads requesting the same session wait on the
condition variable until the spawn completes or fails."
  (session-id nil)
  (lock (bt:make-lock "placeholder-lock"))
  (condvar (bt:make-condition-variable :name "placeholder-ready"))
  (state :spawning :type keyword)
  (worker nil)
  (error-message nil)
  (cancelled nil :type boolean))

(defun %effective-pool-size ()
  "Return the effective pool size: the workers the pool tracks plus the spawn
operations in flight, which the cap limits.  Every spawn is counted from the
decision to its end (%BEGIN-SPAWN) -- an on-demand spawn behind its
placeholder, a recovery's, a replenishment's without one -- and hands its
count to its worker in the critical section that registers it, so no worker
is counted twice or missed.  A worker taken out of the lists to be ended is
not counted: it is going.  Must be called with *pool-lock* held."
  (+ (length *all-workers*) *spawns-in-flight*))

;;; ---------------------------------------------------------------------------
;;; Internal -- spawn and bind
;;; ---------------------------------------------------------------------------

(defun %spawn-and-bind (session-id placeholder)
  "Spawn a worker, bind it to SESSION-ID, and notify waiting threads.
On failure, clean up the affinity map entry and notify waiters of
the failure.
Returns the worker on success.  Signals an error if the spawn was
cancelled (e.g. release-session during spawn) or failed.

Finishes the spawn operation its caller counted (%BEGIN-SPAWN) however it
ends: in the critical section that registers the worker, which then counts
in its place, or once the worker is ended."
  (let ((counted t))
    (unwind-protect
      (let ((new-worker nil))
        (unwind-protect
            (progn
              (setf new-worker (%spawn-worker))
              (setf (worker-state new-worker) :bound)
              (setf (worker-session-id new-worker) session-id)
              (let ((cancelled nil) (shut-down nil))
                (bt:with-lock-held (*pool-lock*)
                  (cond
                    ((worker-placeholder-cancelled placeholder)
                     (setf cancelled t)
                     (setf (worker-state new-worker) :released))
                    ;; The pool this spawn was started for is gone: a shutdown
                    ;; emptied the map while the process was starting, and a
                    ;; worker registered now would be in a pool nobody shuts down
                    ;; again -- the next INITIALIZE-POOL starts from an empty
                    ;; list, and the process is never killed.  The same holds
                    ;; when a new pool was initialized meanwhile: its map does
                    ;; not hold this placeholder.
                    ((or (not *pool-running*)
                         (not (eq (gethash session-id *affinity-map*) placeholder)))
                     (setf cancelled t
                           shut-down t)
                     (setf (worker-state new-worker) :released))
                    (t
                     (setf (gethash session-id *affinity-map*) new-worker)
                     (push new-worker *all-workers*)
                     ;; Tracked now, so no longer a spawn in flight: one
                     ;; critical section, so it is never counted twice.
                     (decf *spawns-in-flight*)
                     (setf counted nil)
                     (%condition-broadcast *work-in-flight-condvar*))))
                (cond
                  (cancelled
                   (bt:with-lock-held ((worker-placeholder-lock placeholder))
                     (setf (worker-placeholder-state placeholder) :failed
                           (worker-placeholder-error-message placeholder)
                           (if shut-down
                               "Pool shut down during spawn."
                               "Session released during spawn."))
                     (%condition-broadcast
                      (worker-placeholder-condvar placeholder)))
                   (log-event :info "pool.spawn.cancelled"
                              "session" session-id
                              "worker_id" (worker-id new-worker))
                   ;; Never bound to anyone: it held no session's state.
                   (record-worker-termination new-worker
                                              (if shut-down :shutdown :released)
                                              :owed nil)
                   (ignore-errors (%kill-worker new-worker))
                   ;; Nil out to prevent duplicate kill in unwind-protect cleanup
                   (setf new-worker nil)
                   (if shut-down
                       (error 'pool-shutting-down)
                       (error 'pool-spawn-cancelled :session-id session-id)))
                  (t
                   (bt:with-lock-held ((worker-placeholder-lock placeholder))
                     (setf (worker-placeholder-worker placeholder) new-worker
                           (worker-placeholder-state placeholder) :ready)
                     (%condition-broadcast
                      (worker-placeholder-condvar placeholder)))
                   (log-event :info "pool.worker.bound"
                              "session" session-id
                              "worker_id" (worker-id new-worker))
                   (%schedule-replenish)
                   new-worker))))
          (when (null (worker-placeholder-worker placeholder))
            (bt:with-lock-held (*pool-lock*)
              ;; Only remove if the affinity map still points to OUR placeholder.
              ;; A rapid release-session + new get-or-assign-worker could have
              ;; already replaced this entry; unconditional remhash would orphan
              ;; the newer binding.
              (when (eq (gethash session-id *affinity-map*) placeholder)
                (remhash session-id *affinity-map*)))
            (bt:with-lock-held ((worker-placeholder-lock placeholder))
              ;; Only a spawn nobody has accounted for failed to start.  One
              ;; cancelled above was told why, and "failed to start" would
              ;; replace that with something that did not happen.
              (when (eq :spawning (worker-placeholder-state placeholder))
                (setf (worker-placeholder-state placeholder) :failed
                      (worker-placeholder-error-message placeholder)
                      "Worker process failed to start."))
              (%condition-broadcast
               (worker-placeholder-condvar placeholder)))
            (when new-worker
              (record-worker-termination new-worker :stopped :owed nil)
              (ignore-errors (%kill-worker new-worker))))))
      (when counted (%end-spawn)))))

;;; ---------------------------------------------------------------------------
;;; Internal -- wait for placeholder
;;; ---------------------------------------------------------------------------

(defun %wait-for-placeholder (placeholder)
  "Wait for another thread to finish spawning a worker for the same
session.  Returns the worker on success, or signals an error on
failure or timeout.

The spawning thread uses condition-broadcast to wake ALL waiters
simultaneously.  Uses *worker-startup-timeout* + 15 seconds as the
deadline to ensure the waiter outlives the actual spawn attempt."
  (bt:with-lock-held ((worker-placeholder-lock placeholder))
    (let ((deadline (+ (get-internal-real-time)
                       (* (+ *worker-startup-timeout* 15)
                          internal-time-units-per-second))))
      (loop while (eq (worker-placeholder-state placeholder) :spawning)
            for remaining = (/ (max 0 (- deadline (get-internal-real-time)))
                               internal-time-units-per-second)
            when (zerop remaining) do (loop-finish)
            do (bt:condition-wait
                (worker-placeholder-condvar placeholder)
                (worker-placeholder-lock placeholder)
                :timeout remaining)))
    (case (worker-placeholder-state placeholder)
      (:ready
       (worker-placeholder-worker placeholder))
      (:failed
       (error "Worker spawn failed: ~A"
              (worker-placeholder-error-message placeholder)))
      (:spawning
       (error "Worker spawn timed out for session ~A"
              (worker-placeholder-session-id placeholder))))))

;;; ---------------------------------------------------------------------------
;;; Internal -- standby replenishment
;;; ---------------------------------------------------------------------------

(defun %replenish-standbys (&optional (generation *pool-generation*))
  "Spawn standby workers until the pool has *worker-pool-warmup*
standbys.  Respects *max-pool-size* to avoid growing the pool
beyond the configured cap.  Called in a background thread.
Exits early when *pool-running* becomes NIL (shutdown in progress).

GENERATION is the pool this replenishment was started for
(*POOL-GENERATION*): a worker it spawns is handed to that pool only, never
to one initialized after it was shut down.

Each spawn is counted from the decision (%BEGIN-SPAWN), so the cap, which
counts spawns in flight, holds while it runs, and a shutdown waits for it.
A worker it cannot register -- the pool stopped, or filled meanwhile -- is
ended outside *POOL-LOCK*: ending one takes up to two seconds, and every
session's next request goes through that lock."
  (unwind-protect
      (loop (unless *pool-running* (return))
            (let ((need-more nil))
              (bordeaux-threads:with-lock-held (*pool-lock*)
                (when (and *pool-running*
                           (eql generation *pool-generation*)
                           (< (length *standby-workers*) *worker-pool-warmup*)
                           (< (%effective-pool-size) *max-pool-size*))
                  (setf need-more t)
                  (%begin-spawn)))
              (unless need-more (return))
              (let ((counted t) (surplus nil) (stop nil))
                (unwind-protect
                     (handler-case
                         (let ((w (%spawn-worker)))
                           (bordeaux-threads:with-lock-held (*pool-lock*)
                             (cond
                               ((not (and *pool-running*
                                          (eql generation *pool-generation*)))
                                (setf surplus w stop t))
                               ;; Its own spawn is still counted in the size,
                               ;; so the cap is exceeded only if registering
                               ;; it would go past it.
                               ((> (%effective-pool-size) *max-pool-size*)
                                (log-event :info "pool.standby.cap-reached"
                                           "worker_id" (worker-id w))
                                (setf surplus w stop t))
                               (t
                                (push w *standby-workers*)
                                (push w *all-workers*)
                                ;; Tracked now: hand its count to it.
                                (decf *spawns-in-flight*)
                                (setf counted nil)
                                (%condition-broadcast *work-in-flight-condvar*))))
                           (unless surplus
                             (log-event :info "pool.standby.spawned"
                                        "worker_id" (worker-id w))))
                       (error (e)
                         (log-event :warn "pool.standby.spawn.failed"
                                    "error" (princ-to-string e))
                         (setf stop t)))
                  (when surplus
                    (ignore-errors (%kill-worker surplus)))
                  (when counted (%end-spawn)))
                (when stop (return)))))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (setf *replenish-running* nil))))

(defun %schedule-replenish ()
  "Spawn a background thread to replenish standby workers if needed.
Skips if a replenish thread is already running.  Captures the
caller's dynamic bindings for *worker-pool-warmup* and *max-pool-size*
and re-binds them inside the replenish thread, so callers (including
tests) can let-bind these vars and have the value honoured -- SBCL's
bt:make-thread does not propagate dynamic bindings to the new thread."
  (let ((should-start nil) (generation nil))
    (bt:with-lock-held (*pool-lock*)
      (when (and *pool-running*
                 (not *replenish-running*)
                 (< (length *standby-workers*) *worker-pool-warmup*))
        (setf *replenish-running* t
              should-start t
              generation *pool-generation*)))
    (when should-start
      (let ((warmup *worker-pool-warmup*)
            (max-size *max-pool-size*))
        (let ((handle (%start-pool-work
                       (lambda ()
                         (let ((*worker-pool-warmup* warmup)
                               (*max-pool-size* max-size))
                           (%replenish-standbys generation)))
                       "pool-replenish")))
          (bt:with-lock-held (*pool-lock*)
            (setf *replenish-thread* handle)))))))

;;; ---------------------------------------------------------------------------
;;; Internal -- crash recovery
;;; ---------------------------------------------------------------------------

(defun %count-crash-against-breaker (worker session-id)
  "Count WORKER's death against SESSION-ID's circuit breaker, once, and
return true when that trips it -- clearing the history, as a trip does.
Call with *POOL-LOCK* held, in the critical section that publishes the
worker as :CRASHED: GET-OR-ASSIGN-WORKER counts any :CRASHED worker it finds
whose death is not counted yet, and a death counted in a later section could
be counted by both."
  (when (and (not (worker-crash-history-pushed-p worker))
             (%breaker-countable-crash-p worker))
    (let* ((now (get-universal-time))
           (window-start (- now *crash-breaker-window*))
           (history (cons now (remove-if (lambda (ts) (< ts window-start))
                                         (gethash session-id *crash-history*)))))
      (setf (gethash session-id *crash-history*) history
            (worker-crash-history-pushed-p worker) t)
      (when (>= (length history) *crash-breaker-threshold*)
        ;; Cleared so a session id reused later starts from nothing.
        (remhash session-id *crash-history*)
        t))))

(defun %handle-worker-crash (crashed-worker)
  "Handle a crashed worker: take it out of the pool, end it, and when it was
bound to a session, bind that session a replacement.  The state the session
lost is recorded in the reset ledger, which the session's next response
tells it.  Skips workers whose state is not :bound, :standby, or :crashed.

Everything about the death is decided in one critical section, under the
same lock GET-OR-ASSIGN-WORKER takes: the worker is published :CRASHED, its
death counted against the circuit breaker, taken out of every list and
listed to be ended, and -- for a bound one -- the session's entry replaced by
a placeholder whose spawn is counted.  A request for the session arriving
meanwhile waits on that placeholder rather than spawning a second
replacement, the cap counts the replacement from the start, and a shutdown
waits for it.  The spawn itself is %SPAWN-AND-BIND's, outside the lock.

For workers already in :crashed state (detected by worker-rpc before
the health monitor), cleans up pool tracking and triggers replenishment
without attempting to spawn a replacement: the session's next request does.

Includes a circuit breaker: if a session's worker has crashed
*crash-breaker-threshold* times within *crash-breaker-window* seconds,
recovery is halted and the session is removed from the affinity map.

Exits immediately when *pool-running* is NIL (shutdown in progress):
the shutdown ends the worker."
  (let ((session-id nil)
        (was-standby nil)
        (bound-dropped nil)
        (retired-p nil)
        (exit-code nil)
        (exit-status nil)
        (placeholder nil)
        (tripped nil))
    ;; Read before the lock and recorded inside it: SB-EXT:PROCESS-STATUS is
    ;; only a struct read, but *POOL-LOCK* is the lock every session's next
    ;; request goes through, and there is nothing here it needs to see.
    (ignore-errors
      (let* ((proc (worker-process-info crashed-worker))
             (status (and proc (sb-ext:process-status proc))))
        (when status
          (setf exit-status (string-downcase (symbol-name status))))
        (when (member status '(:exited :signaled))
          (setf exit-code (sb-ext:process-exit-code proc)))))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (unless *pool-running*
        (return-from %handle-worker-crash))
      (case (worker-state crashed-worker)
        (:bound
         (setf session-id (worker-session-id crashed-worker))
         ;; Recorded before the state says there was a death, and under the
         ;; same lock: GET-OR-ASSIGN-WORKER asks the breaker about any
         ;; :CRASHED worker it finds, and between the two writes a
         ;; retirement is indistinguishable from a crash.
         ;; The reset it owes the session is recorded here too, while the
         ;; worker is still :BOUND and so still counts as holding its state.
         (setf retired-p (%record-worker-death crashed-worker
                                               exit-status exit-code))
         (%drop-owner-worker crashed-worker)
         (setf (worker-state crashed-worker) :crashed)
         (setf tripped (%count-crash-against-breaker crashed-worker session-id))
         ;; Only while this worker is still the session's: another thread
         ;; may have replaced it already.
         (when (eql (gethash session-id *affinity-map*) crashed-worker)
           (cond
             ;; The recovery stops here.  The session is not halted for
             ;; good -- the history is cleared, so its next request is
             ;; served, and told the reset from the ledger.
             (tripped (remhash session-id *affinity-map*))
             (t
              (setf placeholder (make-worker-placeholder :session-id session-id)
                    (gethash session-id *affinity-map*) placeholder)
              (%begin-spawn)))))
        (:standby
         (setf was-standby t)
         (setf retired-p (%record-worker-death crashed-worker
                                               exit-status exit-code))
         (setf (worker-state crashed-worker) :crashed))
        (:crashed
         ;; Already marked crashed (e.g. by worker-rpc EOF detection), so the
         ;; classification is already published; this adds the exit details
         ;; the RPC path could not see and keeps whatever it concluded.
         (setf session-id (worker-session-id crashed-worker))
         (setf retired-p (%record-worker-death crashed-worker
                                               exit-status exit-code
                                               :already-classified t))
         (%drop-owner-worker crashed-worker)
         ;; Dropped here with no replacement in hand -- the session's next
         ;; request makes one.  The reset it owes was recorded when it was
         ;; marked crashed, and waits in the ledger for the session.
         (when (eql (gethash session-id *affinity-map*) crashed-worker)
           (remhash session-id *affinity-map*)
           (setf bound-dropped t))
         ;; A crashed standby too: an RPC timeout marks a standby :CRASHED
         ;; before the monitor finds its process dead, and left on this list
         ;; it is a worker the pool has ended, still offered as a standby and
         ;; still counted as one when replenishing.
         (when (member crashed-worker *standby-workers*)
           (setf was-standby t)))
        (otherwise (return-from %handle-worker-crash)))
      (setf *standby-workers* (remove crashed-worker *standby-workers*)
            *all-workers* (remove crashed-worker *all-workers*))
      (%begin-ending crashed-worker))
    (log-event :warn "pool.worker.crashed" "worker_id"
               (worker-id crashed-worker) "session" session-id
               "was_standby" was-standby
               "retired" retired-p
               "exit_status" (or exit-status "unknown")
               "exit_code" (or exit-code "unknown"))
    (%end-worker crashed-worker)
    (when tripped
      (log-event :error "pool.circuit-breaker.tripped"
                 "session" session-id
                 "threshold" *crash-breaker-threshold*
                 "window_seconds" *crash-breaker-window*))
    (cond
      (placeholder
       (handler-case
           (let ((new-worker (%spawn-and-bind session-id placeholder)))
             (log-event :info "pool.worker.recovered"
                        "old_worker_id" (worker-id crashed-worker)
                        "new_worker_id" (worker-id new-worker)
                        "session" session-id)
             (when *worker-init-config*
               (ignore-errors (%ensure-runtime-init new-worker session-id))))
         (error (e)
           ;; The session was released, the pool shut down, or the spawn
           ;; failed: %SPAWN-AND-BIND ended its worker and took the
           ;; placeholder out, and the session's next request tries again.
           (log-event :error "pool.worker.recovery.failed"
                      "worker_id" (worker-id crashed-worker)
                      "session" session-id
                      "error" (princ-to-string e)))))
      ((or was-standby bound-dropped)
       (%schedule-replenish)))))

;;; ---------------------------------------------------------------------------
;;; Internal -- health monitor
;;; ---------------------------------------------------------------------------

(defun %worker-process-alive-p (worker)
  "Check if WORKER's OS process is still alive via sb-ext:process-alive-p.
Returns T if alive, NIL if dead.  Does not acquire any locks or perform I/O."
  (let ((process (worker-process-info worker)))
    (and process
         (ignore-errors (sb-ext:process-alive-p process)))))

(defun %wait-for-next-health-check ()
  "Wait until the next health check interval, or until shutdown wakes us."
  (bt:with-lock-held (*health-monitor-lock*)
    (when *pool-running*
      (bt:condition-wait *health-monitor-condvar*
                         *health-monitor-lock*
                         :timeout *health-check-interval-seconds*))))

(defun %check-worker-health ()
  "Check every bound and standby worker once.  Detect crashed ones using
OS-level process liveness, which acquires no lock and performs no I/O, and
start their recovery as background work; reap the processes of crashed
workers, and prune stale crash history.  One iteration of the health
monitor, separated so the ownership contracts can run it at a chosen point."
  (let ((workers nil))
    ;; Snapshot worker list under lock
    (bt:with-lock-held (*pool-lock*)
      (setf workers (copy-list *all-workers*)))
    ;; Check each bound/standby worker outside lock
    (dolist (w workers)
      (when (member (worker-state w) '(:bound :standby))
        (unless (%worker-alive-p w)
          ;; Cap concurrent recoveries to prevent resource
          ;; exhaustion when many workers crash at once.
          (let ((active-count
                  (bt:with-lock-held (*pool-lock*)
                    (length *recovery-threads*))))
            (if (>= active-count *max-concurrent-recoveries*)
                (log-event :warn "pool.monitor.recovery-deferred"
                           "worker_id" (worker-id w)
                           "active_recoveries" active-count
                           "max" *max-concurrent-recoveries*)
                ;; Queue crash recovery as separate work so one slow
                ;; recovery doesn't block checking other workers.  Register
                ;; under lock BEFORE the work can self-remove (both use
                ;; *pool-lock*).
                (let ((thread nil))
                  (bt:with-lock-held (*pool-lock*)
                    (setf thread
                          (%start-pool-work
                           (lambda ()
                             (unwind-protect
                                 (handler-case
                                     (%handle-worker-crash w)
                                   (error (e)
                                     (log-event :error
                                      "pool.monitor.recovery-error"
                                      "worker_id"
                                      (worker-id w) "error"
                                      (princ-to-string e))))
                               (bt:with-lock-held (*pool-lock*)
                                 (setf *recovery-threads*
                                       (remove thread
                                               *recovery-threads*)))))
                           (format nil "pool-recover-~A"
                                   (worker-id w))))
                    (push thread *recovery-threads*))))))))
    ;; Reap zombie workers: crashed workers whose OS process
    ;; is still tracked but no longer alive.
    (dolist (w workers)
      (when (eq (worker-state w) :crashed)
        (let ((process (worker-process-info w)))
          (when process
            (ignore-errors
              (sb-ext:process-close process))))))
    ;; Prune stale crash-history entries for sessions that
    ;; have no recent crashes (all timestamps outside window).
    (let ((window-start (- (get-universal-time)
                           *crash-breaker-window*)))
      (bt:with-lock-held (*pool-lock*)
        (let ((stale nil))
          (maphash
           (lambda (sid timestamps)
             (unless (some (lambda (ts) (>= ts window-start))
                          timestamps)
               (push sid stale)))
           *crash-history*)
          (dolist (sid stale)
            (remhash sid *crash-history*)))))))

(defun %health-monitor-loop ()
  "Run %CHECK-WORKER-HEALTH every health check interval until *pool-running*
becomes NIL.  The body is wrapped in handler-case to prevent transient errors
from killing the monitor thread."
  (loop while *pool-running*
        do (%wait-for-next-health-check)
           (when *pool-running*
             (handler-case (%check-worker-health)
               (error (e)
                 (log-event :error "pool.monitor.loop-error"
                            "error" (princ-to-string e)))))))

(defun %start-health-monitor ()
  "Start the background health monitor thread.
Captures the current value of *health-check-interval-seconds* so the
caller's dynamic binding (e.g. from with-pool in tests) is honoured
by the spawned thread."
  (setf *pool-running* t)
  (let ((interval *health-check-interval-seconds*))
    (setf *health-thread*
          (bt:make-thread
           (lambda ()
             (let ((*health-check-interval-seconds* interval))
               (%health-monitor-loop)))
           :name "pool-health-monitor"))))

;;; ---------------------------------------------------------------------------
;;; Public API -- initialize
;;; ---------------------------------------------------------------------------

(defvar *init-lock* (bt:make-lock "pool-init-lock")
  "Serializes concurrent calls to initialize-pool.")

(defun initialize-pool ()
  "Initialize the worker pool and start the health monitor.  Safe to
call multiple times (shuts down any existing pool first).  Registers
shutdown-pool in sb-ext:*exit-hooks* so that workers are cleaned up
if the parent process exits.

Returns once internal state is set up; warm standby workers spawn
asynchronously on a replenish thread so the caller is not blocked
on subprocess launches.  This lets cl-mcp:run :transport :stdio log
stdio.start and answer the MCP initialize handshake within
milliseconds, regardless of *worker-pool-warmup*.  The pool grows
to *worker-pool-warmup* standbys in the background.

Serialized by *init-lock* to prevent concurrent initialization."
  (bt:with-lock-held (*init-lock*)
    ;; Validate configuration
    (unless (and (integerp *max-pool-size*) (plusp *max-pool-size*))
      (error "Invalid *max-pool-size*: must be a positive integer, got ~S"
             *max-pool-size*))
    (unless (and (integerp *worker-pool-warmup*)
                 (>= *worker-pool-warmup* 0))
      (error "Invalid *worker-pool-warmup*: must be a non-negative integer, got ~S"
             *worker-pool-warmup*))
    (when (> *worker-pool-warmup* *max-pool-size*)
      (error "*worker-pool-warmup* (~D) exceeds *max-pool-size* (~D)"
             *worker-pool-warmup* *max-pool-size*))
    ;; Clear stale cached bindings before re-verifying
    (%invalidate-proxy-cache)
    ;; Verify late-bound proxy symbols are resolvable before starting
    (verify-proxy-bindings)
    ;; Shut down existing pool if running
    (when *pool-running*
      (shutdown-pool))
    ;; Clear stale request records from previous pool lifecycle
    (clear-requests)
    ;; Reset state under lock
    (bt:with-lock-held (*pool-lock*)
      (setf *affinity-map* (make-hash-table :test 'equal)
            *standby-workers* nil
            *all-workers* nil
            *recovery-threads* nil
            *worker-init-config* (%parse-worker-init-config))
      (setf *runtime-owner* nil
            *runtime-init-failures* 0
            *runtime-init-disabled* nil)
      (clrhash *init-attributable-crashes*)
      (clrhash *crash-history*)
      ;; A new pool: background work started for an earlier one adds
      ;; nothing to this one.
      (incf *pool-generation*))
    ;; Start health monitor.  Sets *pool-running* to T, which gates
    ;; the replenish thread below: %replenish-standbys exits early
    ;; if *pool-running* is NIL, so the monitor must come up first.
    (%start-health-monitor)
    ;; Register exit hook to prevent orphan workers on parent exit
    (pushnew 'shutdown-pool sb-ext:*exit-hooks*)
    ;; Hand off warmup to the existing standby-replenishment machinery,
    ;; which spawns workers on a background thread up to
    ;; *worker-pool-warmup* and respects *max-pool-size*.  Each spawn
    ;; logs pool.standby.spawned as it lands.
    (%schedule-replenish)
    (log-event :info "pool.initialized"
               "warmup_target" *worker-pool-warmup*)))

;;; ---------------------------------------------------------------------------
;;; Public API -- shutdown
;;; ---------------------------------------------------------------------------

(defun %shutdown-wait-seconds ()
  "How long SHUTDOWN-POOL waits for work in flight: long enough for a spawn
that has only just started to finish its handshake, or fail it."
  (+ *worker-startup-timeout* 15))

(defun shutdown-pool ()
  "Shut down all workers and clean up the pool.

When it returns, the pool owes nothing: every worker it was handed is ended,
none is left in its lists or on its way into them, and its background work
-- the health monitor, replenishment, crash recovery -- has finished.  In
order:

- the pool stops, under *POOL-LOCK*, so no acquire, replenishment or
  recovery decides anything after this;
- the health monitor is woken and joined;
- work in flight outside the lock is waited for (%WAIT-FOR-WORK-IN-FLIGHT):
  each spawn ends with its worker registered or ended -- a spawn finishing
  now sees the pool stopped and ends its own -- and each worker taken out to
  be ended is ended;
- the replenishment and recovery threads are joined;
- every worker still listed is recorded as ended by the shutdown, signalled
  first so an RPC blocked on it lets go of its stream, and ended.

A spawn that does not finish within %SHUTDOWN-WAIT-SECONDS is logged and
left to end its own worker, which it does on seeing the pool stopped."
  (log-event :info "pool.shutting-down")
  (bt:with-lock-held (*pool-lock*)
    (setf *pool-running* nil))
  ;; Clear stale request records so pool restart starts clean
  (clear-requests)
  ;; Wake health monitor immediately instead of waiting up to its poll interval.
  (bt:with-lock-held (*health-monitor-lock*)
    (%condition-broadcast *health-monitor-condvar*))
  ;; Join the health monitor thread.
  (when
      (and *health-thread* (bordeaux-threads:threadp *health-thread*)
           (bordeaux-threads:thread-alive-p *health-thread*))
    (handler-case (bordeaux-threads:join-thread *health-thread*)
                  (error () nil)))
  (setf *health-thread* nil)
  (let ((deadline (+ (get-internal-real-time)
                     (* (%shutdown-wait-seconds) internal-time-units-per-second))))
    (flet ((remaining ()
             (max 0 (/ (- deadline (get-internal-real-time))
                       internal-time-units-per-second))))
      (multiple-value-bind (quiet spawns ending)
          (%wait-for-work-in-flight (remaining))
        (unless quiet
          (log-event :warn "pool.shutdown.work-still-in-flight"
                     "spawns" spawns "ending" ending)))
      ;; The replenishment thread: its spawn is done, so it leaves at its
      ;; next check of *POOL-RUNNING*.  Only a thread is waited for: a
      ;; handle that is not one is work *START-POOL-THREAD-FUNCTION* ran some
      ;; other way, which sees the pool stopped whenever it runs.
      (let ((handle (bordeaux-threads:with-lock-held (*pool-lock*)
                      *replenish-thread*)))
        (when (bordeaux-threads:threadp handle)
          (loop while (and (plusp (remaining))
                           (bordeaux-threads:thread-alive-p handle))
                do (sleep *shutdown-replenish-wait-seconds*))))))
  ;; Wait for in-flight recovery threads
  (let ((threads (bordeaux-threads:with-lock-held (*pool-lock*)
                   (copy-list *recovery-threads*))))
    (dolist (th threads)
      ;; A handle that is not a thread is work *START-POOL-THREAD-FUNCTION*
      ;; ran some other way; there is nothing to join.
      (when (and (bordeaux-threads:threadp th)
                 (bordeaux-threads:thread-alive-p th))
        (handler-case (bordeaux-threads:join-thread th)
          (error () nil)))))
  ;; Snapshot and end all workers.
  (let ((workers nil))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (setf workers (copy-list *all-workers*))
      (setf *all-workers* nil
            *standby-workers* nil)
      (clrhash *affinity-map*)
      ;; No worker survives a shutdown to be the runtime's owner.
      (setf *runtime-owner* nil)
      ;; Recorded before any is signalled, so the EOF each in-flight request
      ;; then meets is not taken for a crash.  Nothing survives a shutdown
      ;; to be told anything.
      (dolist (w workers)
        (record-worker-termination w :shutdown :owed nil)
        (%begin-ending w))
      (discard-all-resets))
    ;; Signalled before it is ended, as RELEASE-SESSION does: ending a worker
    ;; takes its stream, which an RPC running on it holds for as long as the
    ;; request takes -- a shutdown behind a long load waited for all of it.
    (dolist (w workers) (ignore-errors (%signal-worker w)))
    (dolist (w workers) (%end-worker w)))
  ;; Wait for in-flight reaper threads (process cleanup from crashes).
  ;; bordeaux-threads 0.x join-thread does not support :timeout,
  ;; so poll thread-alive-p with a deadline instead.
  (let ((threads (bordeaux-threads:with-lock-held (*reaper-threads-lock*)
                   (copy-list *reaper-threads*)))
        (deadline (+ (get-internal-real-time)
                     (* 5 internal-time-units-per-second))))
    (dolist (th threads)
      (loop while (and (bordeaux-threads:thread-alive-p th)
                       (< (get-internal-real-time) deadline))
            do (sleep 0.1))
      (unless (bordeaux-threads:thread-alive-p th)
        (handler-case (bordeaux-threads:join-thread th)
          (error () nil)))))
  (log-event :info "pool.shutdown-complete"))

;;; ---------------------------------------------------------------------------
;;; Public API -- get-or-assign-worker
;;; ---------------------------------------------------------------------------

(defun find-session-worker (session-id)
  "Look up the worker bound to SESSION-ID without spawning.
Returns the worker struct if bound and alive, NIL otherwise.
Used by cancel-request to avoid spawning a worker just to kill it."
  (bt:with-lock-held (*pool-lock*)
    (let ((entry (gethash session-id *affinity-map*)))
      (when (and entry (typep entry 'worker)
                 (eq :bound (worker-state entry)))
        entry))))

(defun get-or-assign-worker (session-id)
  "Return the worker bound to SESSION-ID, assigning one if needed.
If a standby worker is available, it is assigned immediately.
Otherwise a new worker is spawned.  Multiple threads requesting
the same new session will coordinate via a placeholder so only
one spawn occurs.

Uses %effective-pool-size (which counts in-flight placeholders)
for the cap check so concurrent on-demand spawns cannot both
slip through.

Signals an error if the pool is shutting down or if the worker
cannot be created."
  (let ((entry nil) (need-spawn nil) (assigned-from-standby nil)
        (old-worker-to-kill nil) (standbys-to-kill '())
        (capacity-exceeded nil) (circuit-breaker-tripped nil))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (unless *pool-running*
        (error 'pool-shutting-down))
      (setf entry (gethash session-id *affinity-map*))
      (cond
        ;; Path 1: Existing bound worker — return immediately
        ((and entry (typep entry 'worker) (eq :bound (worker-state entry)))
         (return-from get-or-assign-worker entry))
        ;; Path 1b: Existing dead/crashed worker — remove and reassign.
        ;; Save reference for kill outside the lock (%kill-worker can
        ;; block for up to 2 seconds on SIGTERM→SIGKILL).
        ((and entry (typep entry 'worker))
         (setf old-worker-to-kill (%begin-ending entry))
         (remhash session-id *affinity-map*)
         (setf *all-workers* (remove entry *all-workers*))
         ;; Circuit breaker: record crash and check threshold.
         ;; Only for crashed workers (not :dead from normal kill).
         ;; Skip push if %handle-worker-crash already pushed for this
         ;; worker (prevents double-counting in the race window where
         ;; the health monitor detects the crash first).
         ;; A deliberate retirement is excluded: a user calling something
         ;; uninterruptible three times in five minutes would otherwise trip
         ;; the breaker and halt the session, where before this branch they
         ;; got three timeouts and kept working.  The worker is replaced each
         ;; time and the replacement starts clean, so it is not evidence of an
         ;; unstable pool.
         (when (and (eq :crashed (worker-state entry))
                    (not (worker-crash-history-pushed-p entry))
                    (%breaker-countable-crash-p entry))
           (let* ((now (get-universal-time))
                  (window-start (- now *crash-breaker-window*))
                  (history (gethash session-id *crash-history*)))
             (setf history
                   (remove-if (lambda (ts) (< ts window-start)) history))
             (push now history)
             (setf (gethash session-id *crash-history*) history)
             (when (>= (length history) *crash-breaker-threshold*)
               (setf circuit-breaker-tripped t))))
         ;; Check circuit breaker even if we didn't push (health monitor
         ;; may have already pushed enough to trip it).
         (when (and (not circuit-breaker-tripped)
                    (eq :crashed (worker-state entry))
                    (worker-crash-history-pushed-p entry))
           (let ((window-start (- (get-universal-time) *crash-breaker-window*))
                  (history (gethash session-id *crash-history*)))
             (setf history
                   (remove-if (lambda (ts) (< ts window-start)) history))
             (setf (gethash session-id *crash-history*) history)
             (when (>= (length history) *crash-breaker-threshold*)
               (setf circuit-breaker-tripped t))))
         ;; Nothing is said about the reset here: the ledger recorded it
         ;; when the worker ended, and the session's next response tells it.
         ;; A dead owner is forgotten as the runtime's worker.
         (%drop-owner-worker old-worker-to-kill)
         (setf entry nil))
        ;; Path 2: Placeholder — another thread is spawning
        ((and entry (typep entry 'worker-placeholder))
         nil))
      ;; Phase 2: assign standby or spawn
      ;; Skip when circuit breaker tripped — the error is raised after
      ;; the lock, but we must not leave a placeholder that nobody resolves.
      (when (and (null entry) (not circuit-breaker-tripped))
        ;; Try to assign a standby worker, skipping any that have died
        ;; between the last health check and now.
        (loop while *standby-workers*
              for w = (pop *standby-workers*)
              do (cond
                   ;; Its state as well as its process.  An RPC that timed out
                   ;; on a standby -- the project-root broadcast is one --
                   ;; marks it :CRASHED and closes its connection while the
                   ;; process lives on until the reaper gets to it, so a
                   ;; liveness check alone would lend a worker nobody can
                   ;; talk to.
                   ((and (eq :standby (worker-state w))
                         (%worker-alive-p w))
                    (setf (worker-state w) :bound
                          (worker-session-id w) session-id
                          (gethash session-id *affinity-map*) w)
                    (setf assigned-from-standby w)
                    (return))
                   (t
                    ;; Dead or unusable standby -- remove from tracking and
                    ;; end it below, outside the lock.  Dropped from the lists
                    ;; without being killed, its connection, its stderr thread
                    ;; and, for a crashed one, its still-running process were
                    ;; left to nobody.
                    (setf *all-workers* (remove w *all-workers*))
                    (push (%begin-ending w) standbys-to-kill)
                    (log-event :warn "pool.standby.dead-on-assign"
                               "worker_id" (worker-id w)
                               "pid" (worker-pid w)
                               "state" (string-downcase
                                        (symbol-name (worker-state w)))))))
        ;; If no live standby found, spawn on demand
        (when (and (null assigned-from-standby) (null entry))
          ;; Signalled after the lock, not here: a standby dropped above is
          ;; out of every list and not yet killed, and an exit from inside
          ;; the lock would leave its process to nobody.
          (if (>= (%effective-pool-size) *max-pool-size*)
              (setf capacity-exceeded t)
              (let ((ph (make-worker-placeholder :session-id session-id)))
                (setf (gethash session-id *affinity-map*) ph
                      entry ph
                      need-spawn t)
                ;; Counted here, where it is decided; %SPAWN-AND-BIND
                ;; finishes it.
                (%begin-spawn))))))
    ;; Kill orphaned worker outside the lock.  For timeout/stream-error
    ;; crashes the OS process may still be alive; without this it would
    ;; leak as an untracked SBCL process.
    (when old-worker-to-kill
      (%end-worker old-worker-to-kill))
    (dolist (standby standbys-to-kill)
      (%end-worker standby))
    (when capacity-exceeded
      (error 'pool-capacity-exceeded :limit *max-pool-size*))
    ;; Circuit breaker: halt recovery after too many crashes.
    ;; Checked outside the lock so the old worker is already cleaned up.
    (when circuit-breaker-tripped
      (log-event :error "pool.circuit-breaker.tripped"
                 "session" session-id
                 "threshold" *crash-breaker-threshold*
                 "window_seconds" *crash-breaker-window*)
      (error "Circuit breaker tripped for session ~A: ~
              worker crashed ~D times within ~Ds. Recovery halted."
             session-id *crash-breaker-threshold* *crash-breaker-window*))
    (let ((worker (cond
                    (assigned-from-standby
                     (%schedule-replenish)
                     assigned-from-standby)
                    (need-spawn
                     (%spawn-and-bind session-id entry))
                    (t (%wait-for-placeholder entry)))))
      ;; Sync the parent's *project-root* to a newly assigned worker.
      ;; At initialize time the worker doesn't exist yet, so the root
      ;; sent by handle-initialize is a no-op.  This ensures the worker
      ;; picks up the correct root on first actual use.
      ;;
      ;; IMPORTANT: send-root-to-session-worker calls worker-rpc, which
      ;; has the side effect of calling %mark-worker-crashed on ANY
      ;; stream/connection error.  If this happens, the worker's state
      ;; becomes :crashed and its socket is closed — the worker is
      ;; unusable.  We MUST detect this and not return a corrupted worker.
      (when (and (or assigned-from-standby need-spawn) *project-root*)
        (ignore-errors
          (send-root-to-session-worker session-id *project-root*))
        ;; Check if send-root corrupted the worker.  If so, clean up
        ;; and signal an error so the proxy returns a clean error message
        ;; instead of silently looping with a broken worker.
        (when (eq :crashed (worker-state worker))
          (log-event :warn "pool.send-root-corrupted-worker"
                     "session" session-id
                     "worker_id" (worker-id worker))
          (bordeaux-threads:with-lock-held (*pool-lock*)
            ;; Only while this worker is still the session's: recovery may
            ;; have replaced it already.  The crash that marked it recorded
            ;; the reset it owes in the ledger.
            (when (eql (gethash session-id *affinity-map*) worker)
              (%drop-owner-worker worker)
              (remhash session-id *affinity-map*))
            (setf *all-workers* (remove worker *all-workers*))
            (%begin-ending worker))
          (%end-worker worker)
          (%schedule-replenish)
          (error "Worker ~A crashed during project root setup for session ~A"
                 (worker-id worker) session-id)))
      ;; Fire the init hook only when THIS call newly bound a worker.
      (when (and (or assigned-from-standby need-spawn) *worker-init-config*)
        (ignore-errors (%ensure-runtime-init worker session-id)))
      worker)))

;;; ---------------------------------------------------------------------------
;;; Public API -- release-session
;;; ---------------------------------------------------------------------------

(defun release-session (session-id)
  "Release the worker bound to SESSION-ID.  Kills the worker and
removes it from the pool.

If a spawn is in progress (placeholder), marks it as cancelled so
the spawn thread will clean up the worker after it completes.

The worker's state is set to :released under the pool lock so
that the health monitor (which snapshots workers under the lock
then checks state outside it) will skip it and not treat the
impending kill as a crash."
  (let ((worker-to-kill nil))
    (bt:with-lock-held (*pool-lock*)
      ;; Outside the branches below, because a reset can be owed with no
      ;; worker in the map: recovery that dropped the worker, or a
      ;; replacement spawn that failed, leaves one behind.  The session is
      ;; going away, so a reset owed to it is owed to nobody -- and left in
      ;; the ledger it would greet a later session that reused the id with
      ;; someone else's crash.
      (discard-session-resets session-id)
      ;; Its ownership of the runtime goes with it, whichever worker holds
      ;; it now -- or none, when the owner's worker died.
      (when (and *runtime-owner* (equal (car *runtime-owner*) session-id))
        (setf *runtime-owner* nil))
      (let ((entry (gethash session-id *affinity-map*)))
        (cond
          ((and entry (typep entry 'worker))
           (setf worker-to-kill (%begin-ending entry))
           ;; Recorded before the signal, so the EOF an in-flight request
           ;; then meets is not taken for a crash.  Owed to nobody.
           (record-worker-termination worker-to-kill :released :owed nil)
           (setf (worker-state worker-to-kill) :released)
           (remhash session-id *affinity-map*)
           (remhash session-id *crash-history*)
           (setf *all-workers* (remove worker-to-kill *all-workers*)))
          ((and entry (typep entry 'worker-placeholder))
           (setf (worker-placeholder-cancelled entry) t)
           (remhash session-id *affinity-map*)
           (remhash session-id *crash-history*)
           (log-event :info "pool.session.cancelled-spawn"
                      "session" session-id)))))
    (when worker-to-kill
      (log-event :info "pool.session.released"
                 "session" session-id
                 "worker_id" (worker-id worker-to-kill))
      ;; Send SIGTERM first to break any in-flight RPC holding the
      ;; stream-lock, then kill-worker can acquire it without deadlock.
      (ignore-errors (%signal-worker worker-to-kill))
      (%end-worker worker-to-kill)
      (%schedule-replenish))))

(defun kill-session-worker (session-id)
  "Kill the worker bound to SESSION-ID without ending the session.
Unlike RELEASE-SESSION, the session remains active — the next tool
call that requires a worker will get a fresh one via
GET-OR-ASSIGN-WORKER.

Clears the session's crash history so the intentional kill does not
count toward the circuit breaker.

Returns :KILLED if a worker was found and killed, :NO-WORKER if no
worker was bound, :PLACEHOLDER if a spawn was in progress (cancelled).

The second value is every reset the session was owed and had not been told,
the kill's own included, oldest first (see src/reset-events.lisp): the
caller's response is what tells them.  The kill is recorded before the
worker is signalled, so a request it was running meets an EOF that is not
taken for a crash, and is told the worker was killed."
  (let ((worker-to-kill nil)
        (kill-result :no-worker)
        (told '()))
    (bt:with-lock-held (*pool-lock*)
      (let ((entry (gethash session-id *affinity-map*)))
        (cond
          ((and entry (typep entry 'worker))
           (setf worker-to-kill (%begin-ending entry)
                 kill-result :killed)
           ;; While it is still :BOUND, so the session is owed its state.
           (record-worker-termination worker-to-kill :killed)
           (setf (worker-state worker-to-kill) :released)
           (remhash session-id *affinity-map*)
           (remhash session-id *crash-history*)
           (setf *all-workers* (remove worker-to-kill *all-workers*))
           (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker-to-kill))
             (setf *runtime-owner* nil)))
          ((and entry (typep entry 'worker-placeholder))
           (setf (worker-placeholder-cancelled entry) t
                 kill-result :placeholder)
           (remhash session-id *affinity-map*)
           (remhash session-id *crash-history*)))
        ;; pool-kill-worker is the documented re-arm for a quarantined runtime.
        ;; It must fire even when the crashed owner worker was already reaped
        ;; from the affinity map (so kill found :no-worker).  disabled=t implies
        ;; owner=nil, so clearing the latch unconditionally (feature on) cannot
        ;; disturb a live healthy owner.
        (when *worker-init-config*
          (setf *runtime-init-disabled* nil
                *runtime-init-failures* 0))
        ;; Under the pool lock, so no request can be handed a worker between
        ;; the kill and the claim and tell the same reset a second time.
        (setf told (claim-session-resets session-id))))
    (when worker-to-kill
      (log-event :info "pool.session.worker-killed"
                 "session" session-id
                 "worker_id" (worker-id worker-to-kill))
      ;; Send SIGTERM first to break any in-flight RPC holding the
      ;; stream-lock, then kill-worker can acquire it without deadlock.
      (ignore-errors (%signal-worker worker-to-kill))
      (%end-worker worker-to-kill)
      (%schedule-replenish))
    (values kill-result told)))

;;; ---------------------------------------------------------------------------
;;; Public API -- pool-worker-info
;;; ---------------------------------------------------------------------------

(defun broadcast-root-to-workers (path)
  "Send worker/set-project-root RPC to all live workers in parallel.
Called when the parent's *project-root* changes to keep workers
synchronized.  No-op when the pool is empty.  Failures on
individual workers are logged but do not propagate."
  (let ((workers nil))
    (bt:with-lock-held (*pool-lock*)
      (setf workers (copy-list *all-workers*)))
    (when workers
      ;; NATIVE-NAMESTRING, paired with the worker's PARSE-UNIX-NAMESTRING in
      ;; %HANDLE-SET-PROJECT-ROOT: NAMESTRING escapes [ and ] for the pathname
      ;; reader, which only cancelled while the worker read it back with that
      ;; same reader. The two sides are one protocol and move together.
      (let ((path-string (if (pathnamep path)
                             (uiop:native-namestring path)
                             path)))
        (log-event :info "pool.broadcast-root"
                   "path" path-string
                   "worker_count" (length workers))
        (let ((targets (loop for w in workers
                             when (member (worker-state w) '(:bound :standby))
                             collect w)))
          (when targets
            (let ((threads
                    (mapcar
                     (lambda (w)
                       (let ((params (make-hash-table :test 'equal)))
                         (setf (gethash "path" params) path-string)
                         (bt:make-thread
                          (lambda ()
                            (handler-case
                                (worker-rpc w "worker/set-project-root" params
                                            :timeout 5)
                              (error (e)
                                (log-event :warn "pool.broadcast-root.failed"
                                           "worker_id" (worker-id w)
                                           "error" (princ-to-string e)))))
                          :name (format nil "broadcast-root-~A"
                                        (worker-id w)))))
                     targets)))
              ;; Wait for all broadcast threads to complete
              (dolist (th threads)
                (ignore-errors (bt:join-thread th))))))))))

(defun send-root-to-session-worker (session-id path)
  "Send worker/set-project-root RPC to the worker bound to SESSION-ID.
Only targets the calling session's worker, preserving per-session
isolation (unlike broadcast-root-to-workers which updates all workers).
No-op when SESSION-ID is NIL or no worker is bound to the session.
Failures are logged but do not propagate."
  (when session-id
    (let ((worker nil))
      (bt:with-lock-held (*pool-lock*)
        (let ((entry (gethash session-id *affinity-map*)))
          (when (and entry (typep entry 'worker)
                     (eq :bound (worker-state entry)))
            (setf worker entry))))
      (when worker
        ;; NATIVE-NAMESTRING: see BROADCAST-ROOT-TO-WORKERS above -- the
        ;; worker parses this natively, so it must not be reader-escaped.
        (let ((path-string (if (pathnamep path)
                               (uiop:native-namestring path)
                               path)))
          (let ((params (make-hash-table :test 'equal)))
            (setf (gethash "path" params) path-string)
            (handler-case
                (worker-rpc worker "worker/set-project-root" params
                            :timeout 5)
              (error (e)
                (log-event :warn "pool.send-root.failed"
                           "session" session-id
                           "worker_id" (worker-id worker)
                           "error" (princ-to-string e))))))))))

(defun pool-worker-info ()
  "Return a vector of worker info hash-tables suitable for inclusion
in fs-get-project-info output.  Includes diagnostic fields (id,
session, tcp_port, pid, state) but omits swank_port to prevent
unrestricted REPL access bypassing MCP security policies."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (bt:with-lock-held (*pool-lock*)
      (dolist (w *all-workers*)
        (let ((ht (make-hash-table :test 'equal)))
          (setf (gethash "id" ht) (worker-id w)
                (gethash "session" ht) (let ((sid (worker-session-id w)))
                                         (if (and (stringp sid)
                                                  (> (length sid) 8))
                                             (concatenate 'string
                                                          (subseq sid 0 8) "...")
                                             sid))
                (gethash "tcp_port" ht) (worker-tcp-port w)
                (gethash "pid" ht) (worker-pid w)
                (gethash "state" ht) (string-downcase
                                       (symbol-name (worker-state w)))
                ;; Threads a deadline could not stop, as of this worker's last
                ;; answer.  It retires itself before serving another request
                ;; while carrying one, so a non-zero count here means the
                ;; condition arose and the worker has not been asked for
                ;; anything since.
                (gethash "leaked_threads" ht) (worker-leaked-threads w))
          (vector-push-extend ht result))))
    result))

(defun pool-status-info ()
  "Return a hash-table with pool diagnostic information.
Keys: pool_running, total_workers, standby_count, bound_count,
max_pool_size, warmup_target, workers (vector of per-worker hashes)."
  (let* ((running *pool-running*)
         (workers (if running (pool-worker-info) (vector)))
         (standby-count 0)
         (bound-count 0))
    (when running
      (with-lock-held (*pool-lock*)
        (setf standby-count (length *standby-workers*)
              bound-count (- (length *all-workers*)
                             (length *standby-workers*)))))
    (let ((info (make-hash-table :test 'equal)))
      (setf (gethash "pool_running" info) (if running t nil)
            (gethash "total_workers" info) (length workers)
            (gethash "standby_count" info) standby-count
            (gethash "bound_count" info) bound-count
            (gethash "max_pool_size" info) *max-pool-size*
            (gethash "warmup_target" info) *worker-pool-warmup*
            (gethash "workers" info) workers)
      (with-lock-held (*pool-lock*)
        (setf (gethash "init_owner_session" info)
                (and *runtime-owner* (car *runtime-owner*))
              (gethash "init_owner_worker" info)
                (let ((owner-worker (cdr *runtime-owner*)))
                  (and owner-worker (worker-id owner-worker)))
              (gethash "init_disabled" info) (if *runtime-init-disabled* t nil)
              (gethash "init_failures" info) *runtime-init-failures*))
      info)))
