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
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:*worker-pool-warmup*
           #:initialize-pool
           #:shutdown-pool
           #:get-or-assign-worker
           #:release-session
           #:pool-worker-info))

(in-package #:cl-mcp/src/pool)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defvar *worker-pool-warmup* 1
  "Number of standby workers to pre-spawn and maintain.")

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
  (error-message nil))

;;; ---------------------------------------------------------------------------
;;; Internal -- spawn and bind
;;; ---------------------------------------------------------------------------

(defun %spawn-and-bind (session-id placeholder)
  "Spawn a worker, bind it to SESSION-ID, and notify waiting threads.
On failure, clean up the affinity map entry and notify waiters of
the failure.  Returns the worker on success."
  (let ((new-worker nil))
    (unwind-protect
         (progn
           (setf new-worker (spawn-worker))
           (setf (worker-state new-worker) :bound)
           (setf (worker-session-id new-worker) session-id)
           ;; Register in pool state under pool-lock
           (bt:with-lock-held (*pool-lock*)
             (setf (gethash session-id *affinity-map*) new-worker)
             (push new-worker *all-workers*))
           ;; Notify ALL waiters under placeholder lock.
           ;; Uses condition-broadcast to wake all threads at once.
           (bt:with-lock-held ((worker-placeholder-lock placeholder))
             (setf (worker-placeholder-worker placeholder) new-worker
                   (worker-placeholder-state placeholder) :ready)
             (%condition-broadcast
              (worker-placeholder-condvar placeholder)))
           (log-event :info "pool.worker.bound"
                      "session" session-id
                      "worker_id" (worker-id new-worker))
           (%schedule-replenish)
           new-worker)
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
simultaneously."
  (bt:with-lock-held ((worker-placeholder-lock placeholder))
    (loop while (eq (worker-placeholder-state placeholder) :spawning)
          do (bt:condition-wait
              (worker-placeholder-condvar placeholder)
              (worker-placeholder-lock placeholder)
              :timeout 30))
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
standbys.  Called in a background thread."
  (loop
    (let ((need-more nil))
      (bt:with-lock-held (*pool-lock*)
        (when (< (length *standby-workers*) *worker-pool-warmup*)
          (setf need-more t)))
      (unless need-more
        (return))
      (handler-case
          (let ((w (spawn-worker)))
            (bt:with-lock-held (*pool-lock*)
              (push w *standby-workers*)
              (push w *all-workers*))
            (log-event :info "pool.standby.spawned"
                       "worker_id" (worker-id w)))
        (error (e)
          (log-event :warn "pool.standby.spawn.failed"
                     "error" (princ-to-string e))
          ;; Avoid tight retry loop on persistent failures
          (return))))))

(defun %schedule-replenish ()
  "Spawn a background thread to replenish standby workers if needed."
  (let ((need-more nil))
    (bt:with-lock-held (*pool-lock*)
      (when (< (length *standby-workers*) *worker-pool-warmup*)
        (setf need-more t)))
    (when need-more
      (bt:make-thread #'%replenish-standbys
                      :name "pool-replenish"))))

;;; ---------------------------------------------------------------------------
;;; Internal -- crash recovery
;;; ---------------------------------------------------------------------------

(defun %handle-worker-crash (crashed-worker)
  "Handle a crashed worker: spawn a replacement, bind it to the same
session, and set the needs-reset-notification flag."
  (let ((session-id (worker-session-id crashed-worker)))
    (log-event :warn "pool.worker.crashed"
               "worker_id" (worker-id crashed-worker)
               "session" session-id)
    ;; Kill the crashed worker (cleanup resources)
    (ignore-errors (kill-worker crashed-worker))
    ;; Attempt to spawn a replacement
    (handler-case
        (let ((new-worker (spawn-worker)))
          (setf (worker-state new-worker) :bound)
          (setf (worker-session-id new-worker) session-id)
          (setf (worker-needs-reset-notification new-worker) t)
          (bt:with-lock-held (*pool-lock*)
            ;; Replace in affinity map
            (when session-id
              (setf (gethash session-id *affinity-map*) new-worker))
            ;; Update all-workers: remove old, add new
            (setf *all-workers* (remove crashed-worker *all-workers*))
            (push new-worker *all-workers*))
          (log-event :info "pool.worker.recovered"
                     "old_worker_id" (worker-id crashed-worker)
                     "new_worker_id" (worker-id new-worker)
                     "session" session-id))
      (error (e)
        (log-event :error "pool.worker.recovery.failed"
                   "worker_id" (worker-id crashed-worker)
                   "session" session-id
                   "error" (princ-to-string e))
        ;; Remove the dead worker from pool state
        (bt:with-lock-held (*pool-lock*)
          (when session-id
            (remhash session-id *affinity-map*))
          (setf *all-workers*
                (remove crashed-worker *all-workers*)))))))

;;; ---------------------------------------------------------------------------
;;; Internal -- health monitor
;;; ---------------------------------------------------------------------------

(defun %health-monitor-loop ()
  "Periodically ping all bound workers.  Detect and recover crashed
ones.  Runs until *pool-running* becomes NIL."
  (loop while *pool-running*
        do (sleep 10)
           (when *pool-running*
             (let ((workers nil))
               ;; Snapshot worker list under lock
               (bt:with-lock-held (*pool-lock*)
                 (setf workers (copy-list *all-workers*)))
               ;; Ping each bound worker outside lock
               (dolist (w workers)
                 (when (eq (worker-state w) :bound)
                   (handler-case
                       (worker-rpc w "worker/ping" nil :timeout 5)
                     (error ()
                       (%handle-worker-crash w)))))))))

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
monitor thread."
  (log-event :info "pool.shutting-down")
  ;; Stop health monitor
  (setf *pool-running* nil)
  (when (and *health-thread*
             (bt:threadp *health-thread*)
             (bt:thread-alive-p *health-thread*))
    ;; Give it time to notice *pool-running* = nil
    (handler-case
        (bt:join-thread *health-thread*)
      (error ()
        ;; Thread may have already exited
        nil)))
  (setf *health-thread* nil)
  ;; Kill all workers
  (let ((workers nil))
    (bt:with-lock-held (*pool-lock*)
      (setf workers (copy-list *all-workers*))
      (setf *all-workers* nil
            *standby-workers* nil)
      (clrhash *affinity-map*))
    (dolist (w workers)
      (ignore-errors (kill-worker w))))
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
  (let ((entry nil)
        (need-spawn nil)
        (assigned-from-standby nil))
    ;; Phase 1: Check state under pool-lock
    (bt:with-lock-held (*pool-lock*)
      (setf entry (gethash session-id *affinity-map*))
      (cond
        ;; Path 1: Existing worker bound to this session
        ((and entry (typep entry 'worker))
         (return-from get-or-assign-worker entry))
        ;; Path 2: Another thread is already spawning for this session
        ((and entry (typep entry 'worker-placeholder))
         nil)
        ;; Path 3: No entry -- pop standby or create placeholder
        (t
         (if *standby-workers*
             ;; Standby available -- assign immediately
             (let ((w (pop *standby-workers*)))
               (setf (worker-state w) :bound
                     (worker-session-id w) session-id
                     (gethash session-id *affinity-map*) w)
               (setf assigned-from-standby w))
             ;; No standby -- we become the spawning thread
             (let ((ph (make-worker-placeholder
                        :session-id session-id)))
               (setf (gethash session-id *affinity-map*) ph
                     entry ph
                     need-spawn t))))))
    ;; Phase 2: Outside pool-lock
    (cond
      (assigned-from-standby
       ;; Replenish standbys in background (outside lock)
       (%schedule-replenish)
       assigned-from-standby)
      (need-spawn
       ;; We are the spawning thread
       (%spawn-and-bind session-id entry))
      (t
       ;; We are a waiting thread
       (%wait-for-placeholder entry)))))

;;; ---------------------------------------------------------------------------
;;; Public API -- release-session
;;; ---------------------------------------------------------------------------

(defun release-session (session-id)
  "Release the worker bound to SESSION-ID.  Kills the worker and
removes it from the pool.  No-op if the session has no bound
worker."
  (let ((worker-to-kill nil))
    (bt:with-lock-held (*pool-lock*)
      (let ((entry (gethash session-id *affinity-map*)))
        (when (and entry (typep entry 'worker))
          (setf worker-to-kill entry)
          (remhash session-id *affinity-map*)
          (setf *all-workers* (remove worker-to-kill *all-workers*)))))
    (when worker-to-kill
      (log-event :info "pool.session.released"
                 "session" session-id
                 "worker_id" (worker-id worker-to-kill))
      (ignore-errors (kill-worker worker-to-kill))
      (%schedule-replenish))))

;;; ---------------------------------------------------------------------------
;;; Public API -- pool-worker-info
;;; ---------------------------------------------------------------------------

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
