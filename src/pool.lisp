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
;;;; 4. Crash recovery: Detect crash, restart worker, set
;;;;    needs-reset-notification flag.
;;;; 5. Explicit crash notification: AI agent gets ONE notification
;;;;    about crash/reset, then normal operation.
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
                #:worker-state #:worker-session-id
                #:worker-needs-reset-notification
                #:worker-tcp-port
                #:worker-pid #:worker-id
                #:worker-process-info)
  (:import-from #:cl-mcp/src/proxy
                #:verify-proxy-bindings)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:*worker-pool-warmup*
           #:*max-pool-size*
           #:*health-check-interval-seconds*
           #:*shutdown-replenish-wait-seconds*
           #:initialize-pool
           #:shutdown-pool
           #:get-or-assign-worker
           #:release-session
           #:broadcast-root-to-workers
           #:send-root-to-session-worker
           #:pool-worker-info
           #:pool-shutting-down
           #:pool-capacity-exceeded
           #:pool-spawn-cancelled
           #:*recovery-threads*))

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

(defvar *worker-pool-warmup* 1
  "Number of standby workers to pre-spawn and maintain.")

(defvar *max-pool-size* 16
  "Maximum total number of workers (bound + standby).  Prevents
unbounded resource consumption from unlimited session creation.
Each SBCL worker uses ~100-500MB memory.")

(defvar *health-check-interval-seconds* 10.0d0
  "Seconds between health monitor iterations while the pool is running.")

(defvar *shutdown-replenish-wait-seconds* 0.05d0
  "Polling interval (seconds) while waiting for replenish thread shutdown.")

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
  "Return the effective pool size: live workers plus in-flight spawns.
Counts all entries in *all-workers* plus worker-placeholder entries
in *affinity-map* (representing on-demand spawns not yet complete).
Must be called with *pool-lock* held."
  (let ((pending 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (typep v 'worker-placeholder)
                 (incf pending)))
             *affinity-map*)
    (+ (length *all-workers*) pending)))

;;; ---------------------------------------------------------------------------
;;; Internal -- spawn and bind
;;; ---------------------------------------------------------------------------

(defun %spawn-and-bind (session-id placeholder &key need-reset)
  "Spawn a worker, bind it to SESSION-ID, and notify waiting threads.
On failure, clean up the affinity map entry and notify waiters of
the failure.  When NEED-RESET is true, sets the worker's
needs-reset-notification flag BEFORE making the worker visible in
the affinity map to prevent concurrent threads from missing the flag.
Returns the worker on success.  Signals an error if the spawn was
cancelled (e.g. release-session during spawn) or failed."
  (let ((new-worker nil))
    (unwind-protect
        (progn
          (setf new-worker (spawn-worker))
          (setf (worker-state new-worker) :bound)
          (setf (worker-session-id new-worker) session-id)
          (when need-reset
            (setf (worker-needs-reset-notification new-worker) t))
          (let ((cancelled nil))
            (bt:with-lock-held (*pool-lock*)
              (cond
                ((worker-placeholder-cancelled placeholder)
                 (setf cancelled t)
                 (setf (worker-state new-worker) :released))
                (t
                 (setf (gethash session-id *affinity-map*) new-worker)
                 (push new-worker *all-workers*))))
            (cond
              (cancelled
               (bt:with-lock-held ((worker-placeholder-lock placeholder))
                 (setf (worker-placeholder-state placeholder) :failed
                       (worker-placeholder-error-message placeholder)
                       "Session released during spawn.")
                 (%condition-broadcast
                  (worker-placeholder-condvar placeholder)))
               (log-event :info "pool.spawn.cancelled"
                          "session" session-id
                          "worker_id" (worker-id new-worker))
               (ignore-errors (kill-worker new-worker))
               (error 'pool-spawn-cancelled :session-id session-id))
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
          (setf (worker-placeholder-state placeholder) :failed
                (worker-placeholder-error-message placeholder)
                "Worker process failed to start.")
          (%condition-broadcast
           (worker-placeholder-condvar placeholder)))
        (when new-worker
          (ignore-errors (kill-worker new-worker)))))))

;;; ---------------------------------------------------------------------------
;;; Internal -- wait for placeholder
;;; ---------------------------------------------------------------------------

(defun %wait-for-placeholder (placeholder)
  "Wait for another thread to finish spawning a worker for the same
session.  Returns the worker on success, or signals an error on
failure or timeout.

The spawning thread uses condition-broadcast to wake ALL waiters
simultaneously.  Uses a 30-second deadline to avoid hanging
indefinitely if the spawn thread stalls."
  (bt:with-lock-held ((worker-placeholder-lock placeholder))
    (let ((deadline (+ (get-internal-real-time)
                       (* 30 internal-time-units-per-second))))
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

(defun %replenish-standbys ()
  "Spawn standby workers until the pool has *worker-pool-warmup*
standbys.  Respects *max-pool-size* to avoid growing the pool
beyond the configured cap.  Called in a background thread.
Exits early when *pool-running* becomes NIL (shutdown in progress).

Uses %effective-pool-size (which counts in-flight placeholders
from concurrent on-demand spawns) to prevent TOCTOU races.
Both the pre-spawn check and post-spawn addition re-verify
the effective size under *pool-lock*."
  (unwind-protect
      (loop (unless *pool-running* (return))
            (let ((need-more nil))
              (bordeaux-threads:with-lock-held (*pool-lock*)
                (when (and *pool-running*
                           (< (length *standby-workers*) *worker-pool-warmup*)
                           (< (%effective-pool-size) *max-pool-size*))
                  (setf need-more t)))
              (unless need-more (return))
              (handler-case
                  (let ((w (spawn-worker)))
                    ;; Re-check cap after spawn (concurrent on-demand
                    ;; spawns may have filled the pool during our spawn).
                    (bordeaux-threads:with-lock-held (*pool-lock*)
                      (cond
                        ((not *pool-running*)
                         (ignore-errors (kill-worker w))
                         (return))
                        ((>= (%effective-pool-size) *max-pool-size*)
                         (log-event :info "pool.standby.cap-reached"
                                    "worker_id" (worker-id w))
                         (ignore-errors (kill-worker w))
                         (return))
                        (t
                         (push w *standby-workers*)
                         (push w *all-workers*))))
                    (log-event :info "pool.standby.spawned"
                               "worker_id" (worker-id w)))
                (error (e)
                  (log-event :warn "pool.standby.spawn.failed"
                             "error" (princ-to-string e))
                  (return)))))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (setf *replenish-running* nil))))

(defun %schedule-replenish ()
  "Spawn a background thread to replenish standby workers if needed.
Skips if a replenish thread is already running."
  (let ((should-start nil))
    (bt:with-lock-held (*pool-lock*)
      (when (and (not *replenish-running*)
                 (< (length *standby-workers*) *worker-pool-warmup*))
        (setf *replenish-running* t
              should-start t)))
    (when should-start
      (bt:make-thread #'%replenish-standbys
                      :name "pool-replenish"))))

;;; ---------------------------------------------------------------------------
;;; Internal -- crash recovery
;;; ---------------------------------------------------------------------------

(defun %handle-worker-crash (crashed-worker)
  "Handle a crashed worker: spawn a replacement, bind it to the same
session, and set the needs-reset-notification flag.
Skips workers whose state is not :bound, :standby, or :crashed.
State check is performed under *pool-lock* to prevent races with
release-session.

For workers already in :crashed state (detected by worker-rpc before
the health monitor), cleans up pool tracking and triggers replenishment
without attempting to spawn a replacement.

Includes a circuit breaker: if a session's worker has crashed more
than *crash-breaker-threshold* times within *crash-breaker-window*
seconds, recovery is halted and the session is removed from the
affinity map.

Exits immediately when *pool-running* is NIL (shutdown in progress)
to prevent recovery threads from spawning orphan workers."
  (unless *pool-running* (return-from %handle-worker-crash))
  (let ((session-id nil)
        (was-bound nil)
        (was-standby nil)
        (was-already-crashed nil))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (case (worker-state crashed-worker)
        (:bound
         (setf was-bound t
               session-id (worker-session-id crashed-worker))
         (setf (worker-state crashed-worker) :crashed))
        (:standby
         (setf was-standby t)
         (setf *standby-workers* (remove crashed-worker *standby-workers*))
         (setf (worker-state crashed-worker) :crashed))
        (:crashed
         ;; Already marked crashed (e.g. by worker-rpc EOF detection).
         ;; Clean up pool tracking and trigger replenishment.
         (setf was-already-crashed t
               session-id (worker-session-id crashed-worker))
         (when (eql (gethash session-id *affinity-map*) crashed-worker)
           (remhash session-id *affinity-map*)
           (setf was-bound t))
         (setf *all-workers* (remove crashed-worker *all-workers*)))
        (otherwise (return-from %handle-worker-crash))))
    (log-event :warn "pool.worker.crashed" "worker_id"
               (worker-id crashed-worker) "session" session-id
               "was_standby" was-standby)
    (ignore-errors (kill-worker crashed-worker))
    ;; Already-crashed workers were cleaned up from tracking above.
    ;; Just schedule replenishment if needed and return.
    (when was-already-crashed
      (when was-bound (%schedule-replenish))
      (return-from %handle-worker-crash))
    (cond
      (was-bound
       (let ((now (get-universal-time))
             (window-start (- (get-universal-time) *crash-breaker-window*)))
         (bordeaux-threads:with-lock-held (*pool-lock*)
           (let ((history (gethash session-id *crash-history*)))
             (setf history
                   (remove-if (lambda (ts) (< ts window-start)) history))
             (push now history)
             (setf (gethash session-id *crash-history*) history)
             (when (>= (length history) *crash-breaker-threshold*)
               (log-event :error "pool.circuit-breaker.tripped"
                          "session" session-id
                          "crashes" (length history)
                          "window_seconds" *crash-breaker-window*)
               (when (eql (gethash session-id *affinity-map*) crashed-worker)
                 (remhash session-id *affinity-map*))
               (setf *all-workers* (remove crashed-worker *all-workers*))
               (return-from %handle-worker-crash)))))
       (handler-case
           (let ((new-worker nil) (registered nil))
             (unwind-protect
                 (progn
                   (setf new-worker (spawn-worker))
                   ;; Re-check pool state after potentially slow spawn
                   (unless *pool-running*
                     (ignore-errors (kill-worker new-worker))
                     (setf new-worker nil)
                     (return-from %handle-worker-crash))
                   (setf (worker-state new-worker) :bound)
                   (setf (worker-session-id new-worker) session-id)
                   (setf (worker-needs-reset-notification new-worker) t)
                   (bordeaux-threads:with-lock-held (*pool-lock*)
                     (cond
                       ((not *pool-running*)
                        (setf (worker-state new-worker) :released)
                        (setf *all-workers*
                              (remove crashed-worker *all-workers*)))
                       ((eql (gethash session-id *affinity-map*) crashed-worker)
                        (setf (gethash session-id *affinity-map*) new-worker)
                        (setf *all-workers*
                              (remove crashed-worker *all-workers*))
                        (push new-worker *all-workers*)
                        (setf registered t))
                       (t
                        (setf (worker-state new-worker) :released)
                        (setf *all-workers*
                              (remove crashed-worker *all-workers*)))))
                   (cond
                     ((not registered)
                      (log-event :info "pool.worker.recovery.session-gone"
                                 "worker_id" (worker-id new-worker)
                                 "session" session-id)
                      (ignore-errors (kill-worker new-worker)))
                     (t
                      (log-event :info "pool.worker.recovered"
                                 "old_worker_id" (worker-id crashed-worker)
                                 "new_worker_id" (worker-id new-worker)
                                 "session" session-id))))
               (when (and new-worker (not registered))
                 (ignore-errors (kill-worker new-worker)))))
         (error (e)
           (log-event :error "pool.worker.recovery.failed"
                      "worker_id" (worker-id crashed-worker)
                      "session" session-id
                      "error" (princ-to-string e))
           (bordeaux-threads:with-lock-held (*pool-lock*)
             (when (eql (gethash session-id *affinity-map*) crashed-worker)
               (remhash session-id *affinity-map*))
             (setf *all-workers* (remove crashed-worker *all-workers*))))))
      (was-standby
       (bordeaux-threads:with-lock-held (*pool-lock*)
         (setf *all-workers* (remove crashed-worker *all-workers*)))
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

(defun %health-monitor-loop ()
  "Periodically check all bound and standby workers.  Detect crashed
ones using OS-level process liveness (sb-ext:process-alive-p) which
does not acquire any locks or perform I/O, and recover asynchronously.
Runs until *pool-running* becomes NIL.  The outer loop body is
wrapped in handler-case to prevent transient errors from killing
the monitor thread."
  (loop while *pool-running*
        do (%wait-for-next-health-check)
           (when *pool-running*
             (handler-case
                 (let ((workers nil))
                   ;; Snapshot worker list under lock
                   (bt:with-lock-held (*pool-lock*)
                     (setf workers (copy-list *all-workers*)))
                   ;; Check each bound/standby worker outside lock
                   (dolist (w workers)
                     (when (member (worker-state w) '(:bound :standby))
                       (unless (%worker-process-alive-p w)
                         ;; Queue crash recovery to a separate thread
                         ;; so one slow recovery doesn't block checking
                         ;; other workers.
                         (let ((thread nil))
                           (setf thread
                                 (bt:make-thread
                                  (lambda ()
                                    (unwind-protect
                                        (handler-case (%handle-worker-crash w)
                                          (error (e)
                                            (log-event :error
                                             "pool.monitor.recovery-error"
                                             "worker_id"
                                             (worker-id w) "error"
                                             (princ-to-string e))))
                                      (bordeaux-threads:with-lock-held (*pool-lock*)
                                        (setf *recovery-threads*
                                              (remove thread *recovery-threads*)))))
                                  :name
                                  (format nil "pool-recover-~A"
                                          (worker-id w))))
                           (bordeaux-threads:with-lock-held (*pool-lock*)
                             (push thread *recovery-threads*)))))))
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
  "Initialize the worker pool, spawning warm standby workers and
starting the health monitor.  Safe to call multiple times (shuts
down any existing pool first).  Registers shutdown-pool in
sb-ext:*exit-hooks* so that workers are cleaned up if the parent
process exits.

Serialized by *init-lock* to prevent concurrent initialization.
Warmup spawns occur outside *pool-lock* to avoid holding the global
lock during potentially slow subprocess launches."
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
    ;; Verify late-bound proxy symbols are resolvable before starting
    (verify-proxy-bindings)
    ;; Shut down existing pool if running
    (when *pool-running*
      (shutdown-pool))
    ;; Reset state under lock
    (bt:with-lock-held (*pool-lock*)
      (setf *affinity-map* (make-hash-table :test 'equal)
            *standby-workers* nil
            *all-workers* nil
            *recovery-threads* nil)
      (clrhash *crash-history*))
    ;; Spawn warmup standbys OUTSIDE *pool-lock* to avoid blocking
    ;; the global lock during subprocess launches
    (let ((spawned nil))
      (dotimes (i *worker-pool-warmup*)
        (handler-case
            (let ((w (spawn-worker)))
              (push w spawned)
              (log-event :info "pool.standby.init"
                         "worker_id" (worker-id w)
                         "index" i))
          (error (e)
            (log-event :warn "pool.standby.init.failed"
                       "index" i
                       "error" (princ-to-string e)))))
      ;; Register all spawned workers under lock
      (bt:with-lock-held (*pool-lock*)
        (dolist (w spawned)
          (push w *standby-workers*)
          (push w *all-workers*))))
    ;; Start health monitor
    (%start-health-monitor)
    ;; Register exit hook to prevent orphan workers on parent exit
    (pushnew 'shutdown-pool sb-ext:*exit-hooks*)
    (log-event :info "pool.initialized"
               "warmup" *worker-pool-warmup*
               "standbys" (length *standby-workers*))))

;;; ---------------------------------------------------------------------------
;;; Public API -- shutdown
;;; ---------------------------------------------------------------------------

(defun shutdown-pool ()
  "Shut down all workers and clean up the pool.  Stops the health
monitor and waits for any in-flight replenish thread before
snapshotting and killing workers."
  (log-event :info "pool.shutting-down")
  (setf *pool-running* nil)
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
  ;; Wait for any in-flight replenish thread to finish.  The thread
  ;; checks *pool-running* each iteration and will exit promptly.
  (loop repeat 100
        while (bordeaux-threads:with-lock-held (*pool-lock*)
                *replenish-running*)
        do (sleep *shutdown-replenish-wait-seconds*))
  ;; Wait for in-flight recovery threads
  (let ((threads (bordeaux-threads:with-lock-held (*pool-lock*)
                   (copy-list *recovery-threads*))))
    (dolist (th threads)
      (when (bordeaux-threads:thread-alive-p th)
        (handler-case (bordeaux-threads:join-thread th)
          (error () nil)))))
  ;; Snapshot and kill all workers.
  (let ((workers nil))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (setf workers (copy-list *all-workers*))
      (setf *all-workers* nil
            *standby-workers* nil)
      (clrhash *affinity-map*))
    (dolist (w workers) (ignore-errors (kill-worker w))))
  (log-event :info "pool.shutdown-complete"))

;;; ---------------------------------------------------------------------------
;;; Public API -- get-or-assign-worker
;;; ---------------------------------------------------------------------------

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
        (need-reset nil) (old-worker-to-kill nil)
        (circuit-breaker-tripped nil))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (unless *pool-running*
        (error 'pool-shutting-down))
      (setf entry (gethash session-id *affinity-map*))
      (cond
       ;; Path 1: Existing bound worker — return immediately
       ((and entry (typep entry 'worker) (eq :bound (worker-state entry)))
        (return-from get-or-assign-worker entry))
       ;; Path 1b: Existing dead/crashed worker — remove and reassign.
       ;; Save reference for kill outside the lock (kill-worker can
       ;; block for up to 2 seconds on SIGTERM→SIGKILL).
       ((and entry (typep entry 'worker))
        (setf old-worker-to-kill entry)
        (remhash session-id *affinity-map*)
        (setf *all-workers* (remove entry *all-workers*))
        ;; Circuit breaker: record crash and check threshold.
        ;; Only for crashed workers (not :dead from normal kill).
        (when (eq :crashed (worker-state entry))
          (let* ((now (get-universal-time))
                 (window-start (- now *crash-breaker-window*))
                 (history (gethash session-id *crash-history*)))
            (setf history
                  (remove-if (lambda (ts) (< ts window-start)) history))
            (push now history)
            (setf (gethash session-id *crash-history*) history)
            (when (>= (length history) *crash-breaker-threshold*)
              (setf circuit-breaker-tripped t))))
        ;; Avoid double crash notification: when %mark-worker-crashed
        ;; (called by worker-rpc on EOF) set needs-reset-notification
        ;; on the old worker, the proxy's worker-crashed handler
        ;; already returned a notification to the client.
        (setf entry nil
              need-reset (not (worker-needs-reset-notification
                               old-worker-to-kill))))
       ;; Path 2: Placeholder — another thread is spawning
       ((and entry (typep entry 'worker-placeholder))
        nil))
      ;; Phase 2: assign standby or spawn
      (when (null entry)
        (if *standby-workers*
            (let ((w (pop *standby-workers*)))
              (setf (worker-state w) :bound
                    (worker-session-id w) session-id
                    (gethash session-id *affinity-map*) w)
              (when need-reset
                (setf (worker-needs-reset-notification w) t))
              (setf assigned-from-standby w))
            (progn
              (when (>= (%effective-pool-size) *max-pool-size*)
                (error 'pool-capacity-exceeded :limit *max-pool-size*))
              (let ((ph (make-worker-placeholder :session-id session-id)))
                (setf (gethash session-id *affinity-map*) ph
                      entry ph
                      need-spawn t))))))
    ;; Kill orphaned worker outside the lock.  For timeout/stream-error
    ;; crashes the OS process may still be alive; without this it would
    ;; leak as an untracked SBCL process.
    (when old-worker-to-kill
      (ignore-errors (kill-worker old-worker-to-kill)))
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
    (cond
     (assigned-from-standby
      (%schedule-replenish)
      assigned-from-standby)
     (need-spawn
      (%spawn-and-bind session-id entry :need-reset need-reset))
     (t (%wait-for-placeholder entry)))))

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
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (let ((entry (gethash session-id *affinity-map*)))
        (cond
         ;; Bound worker — release immediately
         ((and entry (typep entry 'worker))
          (setf worker-to-kill entry)
          (setf (worker-state worker-to-kill) :released)
          (remhash session-id *affinity-map*)
          (setf *all-workers* (remove worker-to-kill *all-workers*)))
         ;; Placeholder — mark cancelled so spawn thread cleans up
         ((and entry (typep entry 'worker-placeholder))
          (setf (worker-placeholder-cancelled entry) t)
          (remhash session-id *affinity-map*)
          (log-event :info "pool.session.cancelled-spawn"
                     "session" session-id)))))
    (when worker-to-kill
      (log-event :info "pool.session.released"
                 "session" session-id
                 "worker_id" (worker-id worker-to-kill))
      (ignore-errors (kill-worker worker-to-kill))
      (%schedule-replenish))))

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
      (let ((path-string (if (pathnamep path)
                             (namestring path)
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
        (let ((path-string (if (pathnamep path)
                               (namestring path)
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
                                       (symbol-name (worker-state w))))
          (vector-push-extend ht result))))
    result))
