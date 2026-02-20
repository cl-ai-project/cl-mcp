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
                #:condition-notify
                #:make-thread #:threadp #:thread-alive-p
                #:join-thread)
  (:import-from #:cl-mcp/src/worker-client
                #:worker
                #:spawn-worker #:worker-rpc #:kill-worker
                #:worker-state #:worker-session-id
                #:worker-needs-reset-notification
                #:worker-tcp-port #:worker-swank-port
                #:worker-pid #:worker-id)
  (:import-from #:cl-mcp/src/proxy
                #:verify-proxy-bindings)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:*worker-pool-warmup*
           #:*max-pool-size*
           #:initialize-pool
           #:shutdown-pool
           #:get-or-assign-worker
           #:release-session
           #:broadcast-root-to-workers
           #:pool-worker-info))

(in-package #:cl-mcp/src/pool)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defvar *worker-pool-warmup* 1
  "Number of standby workers to pre-spawn and maintain.")

(defvar *max-pool-size* 16
  "Maximum total number of workers (bound + standby).  Prevents
unbounded resource consumption from unlimited session creation.
Each SBCL worker uses ~100-500MB memory.")

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

(defvar *pool-running* nil
  "Flag controlling the health monitor loop.  Set to NIL to stop.")

(defvar *replenish-running* nil
  "Flag preventing concurrent replenish threads.  Set under *pool-lock*.")

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

;;; ---------------------------------------------------------------------------
;;; Internal -- spawn and bind
;;; ---------------------------------------------------------------------------

(defun %spawn-and-bind (session-id placeholder &key need-reset)
  "Spawn a worker, bind it to SESSION-ID, and notify waiting threads.
On failure, clean up the affinity map entry and notify waiters of
the failure.  When NEED-RESET is true, sets the worker's
needs-reset-notification flag BEFORE making the worker visible in
the affinity map to prevent concurrent threads from missing the flag.
Returns the worker on success."
  (let ((new-worker nil))
    (unwind-protect
         (progn
           (setf new-worker (spawn-worker))
           (setf (worker-state new-worker) :bound)
           (setf (worker-session-id new-worker) session-id)
           ;; Set reset notification BEFORE registering in affinity map
           ;; to prevent race where concurrent thread proxies before flag
           (when need-reset
             (setf (worker-needs-reset-notification new-worker) t))
           ;; Register in pool state under pool-lock.
           ;; Check cancellation: if release-session cancelled the
           ;; placeholder while we were spawning, kill the worker
           ;; instead of binding it.
           (let ((cancelled nil))
             (bt:with-lock-held (*pool-lock*)
               (cond
                ((worker-placeholder-cancelled placeholder)
                 ;; Session was released while spawning — orphan prevention
                 (setf cancelled t)
                 (setf (worker-state new-worker) :released))
                (t
                 (setf (gethash session-id *affinity-map*) new-worker)
                 (push new-worker *all-workers*))))
             (cond
              (cancelled
               ;; Notify waiters of cancellation
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
               nil)
              (t
               ;; Notify ALL waiters under placeholder lock.
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
      ;; Cleanup on failure -- only if worker was not successfully set
      (when (null (worker-placeholder-worker placeholder))
        ;; Remove placeholder from affinity map
        (bt:with-lock-held (*pool-lock*)
          (remhash session-id *affinity-map*))
        ;; Notify waiters of failure
        (bt:with-lock-held ((worker-placeholder-lock placeholder))
          (setf (worker-placeholder-state placeholder) :failed
                (worker-placeholder-error-message placeholder)
                "Worker process failed to start.")
          (%condition-broadcast
           (worker-placeholder-condvar placeholder)))
        ;; Kill partially started worker
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
standbys.  Called in a background thread.  Exits early when
*pool-running* becomes NIL (shutdown in progress)."
  (unwind-protect
      (loop
       (unless *pool-running* (return))
       (let ((need-more nil))
         (bordeaux-threads:with-lock-held (*pool-lock*)
           (when (and *pool-running*
                      (< (length *standby-workers*) *worker-pool-warmup*))
             (setf need-more t)))
         (unless need-more (return))
         (handler-case
          (let ((w (spawn-worker)))
            (bordeaux-threads:with-lock-held (*pool-lock*)
              (cond
               (*pool-running*
                (push w *standby-workers*)
                (push w *all-workers*))
               (t
                ;; Pool shutting down — kill the just-spawned worker
                ;; to avoid orphans.
                (ignore-errors (kill-worker w))
                (return))))
            (log-event :info "pool.standby.spawned"
                       "worker_id" (worker-id w)))
          (error (e)
                 (log-event :warn "pool.standby.spawn.failed"
                            "error" (princ-to-string e))
                 ;; Avoid tight retry loop on persistent failures
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
Skips workers whose state is not :bound or :standby (e.g. intentionally
released).  State check is performed under *pool-lock* to prevent
races with release-session."
  (let ((session-id nil)
        (was-bound nil)
        (was-standby nil))
    ;; Check and update state under *pool-lock* to prevent race with
    ;; release-session changing state between check and action.
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (case (worker-state crashed-worker)
        (:bound
         (setf was-bound t
               session-id (worker-session-id crashed-worker))
         ;; Mark crashed under lock so release-session won't touch it
         (setf (worker-state crashed-worker) :crashed))
        (:standby
         (setf was-standby t)
         ;; Remove dead standby from the standby list
         (setf *standby-workers* (remove crashed-worker *standby-workers*))
         (setf (worker-state crashed-worker) :crashed))
        (otherwise
         ;; Already released, dead, or crashed — skip
         (return-from %handle-worker-crash))))
    (log-event :warn "pool.worker.crashed" "worker_id"
     (worker-id crashed-worker) "session" session-id
     "was_standby" was-standby)
    (ignore-errors (kill-worker crashed-worker))
    (cond
     ;; Bound worker: spawn replacement for the same session
     (was-bound
      (handler-case
       (let ((new-worker (spawn-worker)))
         (setf (worker-state new-worker) :bound)
         (setf (worker-session-id new-worker) session-id)
         (setf (worker-needs-reset-notification new-worker) t)
         (bordeaux-threads:with-lock-held (*pool-lock*)
           (when session-id
             (setf (gethash session-id *affinity-map*) new-worker))
           (setf *all-workers* (remove crashed-worker *all-workers*))
           (push new-worker *all-workers*))
         (log-event :info "pool.worker.recovered" "old_worker_id"
          (worker-id crashed-worker) "new_worker_id" (worker-id new-worker)
          "session" session-id))
       (error (e)
              (log-event :error "pool.worker.recovery.failed" "worker_id"
               (worker-id crashed-worker) "session" session-id "error"
               (princ-to-string e))
              (bordeaux-threads:with-lock-held (*pool-lock*)
                (when session-id (remhash session-id *affinity-map*))
                (setf *all-workers*
                      (remove crashed-worker *all-workers*))))))
     ;; Standby worker: just remove and replenish
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
  (let ((process (cl-mcp/src/worker-client::worker-process-info worker)))
    (and process
         (ignore-errors (sb-ext:process-alive-p process)))))

(defun %health-monitor-loop ()
  "Periodically check all bound and standby workers.  Detect crashed
ones using OS-level process liveness (sb-ext:process-alive-p) which
does not acquire any locks or perform I/O, and recover asynchronously.
Runs until *pool-running* becomes NIL."
  (loop while *pool-running*
        do (sleep 10)
           (when *pool-running*
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
                     (bt:make-thread
                      (lambda ()
                        (handler-case
                            (%handle-worker-crash w)
                          (error (e)
                            (log-event :error "pool.monitor.recovery-error"
                                       "worker_id" (worker-id w)
                                       "error" (princ-to-string e)))))
                      :name (format nil "pool-recover-~A"
                                    (worker-id w))))))))))

(defun %start-health-monitor ()
  "Start the background health monitor thread."
  (setf *pool-running* t)
  (setf *health-thread*
        (bt:make-thread #'%health-monitor-loop
                        :name "pool-health-monitor")))

;;; ---------------------------------------------------------------------------
;;; Public API -- initialize
;;; ---------------------------------------------------------------------------

(defun initialize-pool ()
  "Initialize the worker pool, spawning warm standby workers and
starting the health monitor.  Safe to call multiple times (shuts
down any existing pool first)."
  ;; Verify late-bound proxy symbols are resolvable before starting
  (verify-proxy-bindings)
  ;; Shut down existing pool if running
  (when *pool-running*
    (shutdown-pool))
  ;; Reset state
  (setf *affinity-map* (make-hash-table :test 'equal)
        *standby-workers* nil
        *all-workers* nil)
  ;; Spawn warmup standbys
  (dotimes (i *worker-pool-warmup*)
    (handler-case
        (let ((w (spawn-worker)))
          (push w *standby-workers*)
          (push w *all-workers*)
          (log-event :info "pool.standby.init"
                     "worker_id" (worker-id w)
                     "index" i))
      (error (e)
        (log-event :warn "pool.standby.init.failed"
                   "index" i
                   "error" (princ-to-string e)))))
  ;; Start health monitor
  (%start-health-monitor)
  (log-event :info "pool.initialized"
             "warmup" *worker-pool-warmup*
             "standbys" (length *standby-workers*)))

;;; ---------------------------------------------------------------------------
;;; Public API -- shutdown
;;; ---------------------------------------------------------------------------

(defun shutdown-pool ()
  "Shut down all workers and clean up the pool.  Stops the health
monitor and waits for any in-flight replenish thread before
snapshotting and killing workers."
  (log-event :info "pool.shutting-down")
  (setf *pool-running* nil)
  ;; Join the health monitor thread.
  (when
      (and *health-thread* (bordeaux-threads:threadp *health-thread*)
           (bordeaux-threads:thread-alive-p *health-thread*))
    (handler-case (bordeaux-threads:join-thread *health-thread*)
                  (error nil nil)))
  (setf *health-thread* nil)
  ;; Wait for any in-flight replenish thread to finish.  The thread
  ;; checks *pool-running* each iteration and will exit promptly.
  (loop repeat 100
        while (bordeaux-threads:with-lock-held (*pool-lock*)
                *replenish-running*)
        do (sleep 0.05))
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

Signals an error if the worker cannot be created."
  (let ((entry nil) (need-spawn nil) (assigned-from-standby nil)
        (need-reset nil))
    (bordeaux-threads:with-lock-held (*pool-lock*)
      (setf entry (gethash session-id *affinity-map*))
      (cond
       ;; Path 1: Existing bound worker — return immediately
       ((and entry (typep entry 'worker) (eq :bound (worker-state entry)))
        (return-from get-or-assign-worker entry))
       ;; Path 1b: Existing dead/crashed worker — remove and reassign
       ((and entry (typep entry 'worker))
        (remhash session-id *affinity-map*)
        (setf *all-workers* (remove entry *all-workers*))
        (setf entry nil
              need-reset t))
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
              (when (>= (length *all-workers*) *max-pool-size*)
                (error "Pool size limit reached (~D workers). ~
                        Release unused sessions before creating new ones."
                       *max-pool-size*))
              (let ((ph (make-worker-placeholder :session-id session-id)))
                (setf (gethash session-id *affinity-map*) ph
                      entry ph
                      need-spawn t))))))
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
  "Send worker/set-project-root RPC to all live workers.
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
        (let ((params (make-hash-table :test 'equal)))
          (setf (gethash "path" params) path-string)
          (dolist (w workers)
            (when (member (worker-state w) '(:bound :standby))
              (handler-case
                  (worker-rpc w "worker/set-project-root" params
                              :timeout 5)
                (error (e)
                  (log-event :warn "pool.broadcast-root.failed"
                             "worker_id" (worker-id w)
                             "error" (princ-to-string e)))))))))))

(defun pool-worker-info ()
  "Return a vector of worker info hash-tables suitable for inclusion
in fs-get-project-info output."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (bt:with-lock-held (*pool-lock*)
      (dolist (w *all-workers*)
        (let ((ht (make-hash-table :test 'equal)))
          (setf (gethash "id" ht) (worker-id w)
                (gethash "session" ht) (worker-session-id w)
                (gethash "tcp_port" ht) (worker-tcp-port w)
                (gethash "swank_port" ht) (worker-swank-port w)
                (gethash "pid" ht) (worker-pid w)
                (gethash "state" ht) (string-downcase
                                      (symbol-name (worker-state w))))
          (vector-push-extend ht result))))
    result))
