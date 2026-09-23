;;;; specs/concurrency-fixtures.lisp
;;;;
;;;; The real worker pool (src/pool.lisp) under operations that overlap in time
;;;; (Phase 4D), for the properties of specs/concurrency.lisp and the fixed
;;;; cases of tests/concurrency-test.lisp.  Needs no cl-spec, so the fixed cases
;;;; run in the default suite.
;;;;
;;;; Unlike specs/pool-fixtures.lisp, background work runs on real threads, and
;;;; the fake lifecycle is shared between them, so its ledger has a lock of its
;;;; own.  Lock order, everywhere: *POOL-LOCK* before the ledger's.
;;;;
;;;; As in 4A, three things are kept apart: the LEDGER, written by the fake
;;;; lifecycle alone -- every worker spawned and when, every worker ended and
;;;; when, which fake processes died, every thread the pool started; the pool's
;;;; own lists, which the invariants are about; and what the operations were
;;;; told, recorded by the threads that made them.
;;;;
;;;; Two kinds of run:
;;;;
;;;; - A SHUTDOWN SCENARIO fixes, with semaphores, what is in flight when
;;;;   SHUTDOWN-POOL is called -- a spawn behind an acquire, a replenishment or
;;;;   a recovery; a worker being ended; an RPC holding a worker's stream -- and
;;;;   opens those gates only after the shutdown has begun, in a drawn order.
;;;;   Deterministic: a scenario runs the same way every time.
;;;; - A CONCURRENT RUN lets client threads apply drawn operations at once,
;;;;   while an observer checks, again and again under the pool's lock, what
;;;;   must hold at every moment, and ends with a shutdown made while the
;;;;   clients are still running.  The operations are drawn; the interleaving
;;;;   is the scheduler's, so a seed replays the operations and not the run.

(defpackage #:cl-mcp/specs/concurrency-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/pool
                #:initialize-pool
                #:shutdown-pool
                #:get-or-assign-worker
                #:release-session
                #:kill-session-worker
                #:*worker-pool-warmup*
                #:*max-pool-size*)
  (:import-from #:cl-mcp/src/worker-client
                #:make-worker
                #:worker
                #:worker-id
                #:worker-state
                #:worker-session-id
                #:worker-stream-lock
                #:kill-worker)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log
                #:*log-level*)
  (:import-from #:bordeaux-threads)
  (:export #:with-concurrent-pool
           #:ended-p
           #:+in-flight-kinds+
           #:draw-shutdown-scenario
           #:run-shutdown-scenario
           #:shutdown-scenario-violations
           #:+late-kinds+
           #:draw-late-scenario
           #:run-late-scenario
           #:random-concurrent-plan
           #:run-concurrent-plan
           #:concurrency-violation-kinds))

(in-package #:cl-mcp/specs/concurrency-fixtures)

;;; ------------------------------------------------------------------------
;;; A. The ledger and the fake lifecycle

(defstruct (ledger (:conc-name ledger-))
  "What the fake lifecycle saw, written by it and by nothing in the pool."
  (lock (bt:make-lock "concurrency-ledger"))
  ;; Every worker spawned, oldest first, when its spawn completed, and the
  ;; pool generation that was running when it began.
  (spawned '())
  (spawned-at (make-hash-table :test 'eq))
  (spawned-in (make-hash-table :test 'eq))
  ;; Worker -> when the pool ended it.
  (ended-at (make-hash-table :test 'eq))
  ;; Workers whose fake process has died.
  (dead (make-hash-table :test 'eq))
  ;; A semaphore every spawn waits on before completing, or NIL.
  (spawn-gate nil)
  ;; How many spawns have begun.
  (spawns-begun 0)
  ;; Worker -> a semaphore its ending waits on before completing.
  (end-gates (make-hash-table :test 'eq))
  ;; Workers whose ending has begun.
  (ends-begun '())
  ;; Worker -> a semaphore that lets go of the RPC holding its stream.
  (holds (make-hash-table :test 'eq))
  ;; Every thread the pool started.
  (threads '())
  ;; Seconds a spawn and an ending take, at most, drawn per call.
  (spawn-delay 0)
  (end-delay 0)
  (next-id 800000))

(defmacro with-ledger ((ledger) &body body)
  "Run BODY with LEDGER's lock held."
  `(bt:with-lock-held ((ledger-lock ,ledger)) ,@body))

(defun %now ()
  "The current internal real time."
  (get-internal-real-time))

(defun %jitter (seconds)
  "Sleep a random time up to SECONDS, when SECONDS is positive."
  (when (plusp seconds)
    (sleep (random (float seconds 1d0)))))

(defun %fake-spawn (ledger)
  "Return a new standby worker with no process, once the spawn gate lets it."
  (let ((generation (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                      (cl-mcp/src/pool::generation-id cl-mcp/src/pool::*generation*)))
        (gate (with-ledger (ledger)
                (incf (ledger-spawns-begun ledger))
                (ledger-spawn-gate ledger))))
    (when gate (sb-thread:wait-on-semaphore gate :timeout 30))
    (%jitter (ledger-spawn-delay ledger))
    (with-ledger (ledger)
      (let ((worker (make-worker :id (incf (ledger-next-id ledger)) :state :standby)))
        (setf (ledger-spawned ledger) (append (ledger-spawned ledger) (list worker))
              (gethash worker (ledger-spawned-at ledger)) (%now)
              (gethash worker (ledger-spawned-in ledger)) generation)
        worker))))

(defun %fake-end (ledger worker)
  "End WORKER the way production does, once its end gate lets it, and record
when.  KILL-WORKER on a worker with no process takes its stream, closes
nothing and marks it :DEAD -- and so waits, as the real one does, for an RPC
holding that stream."
  (let ((gate (with-ledger (ledger)
                (push worker (ledger-ends-begun ledger))
                (gethash worker (ledger-end-gates ledger)))))
    (when gate (sb-thread:wait-on-semaphore gate :timeout 30))
    (%jitter (ledger-end-delay ledger))
    (kill-worker worker)
    (with-ledger (ledger)
      (unless (gethash worker (ledger-ended-at ledger))
        (setf (gethash worker (ledger-ended-at ledger)) (%now))))))

(defun %fake-alive-p (ledger worker)
  "True unless WORKER's fake process died or the pool ended it."
  (with-ledger (ledger)
    (not (or (gethash worker (ledger-dead ledger))
             (gethash worker (ledger-ended-at ledger))))))

(defun %fake-signal (ledger worker)
  "Let go of an RPC holding WORKER's stream, as SIGTERM breaks a real one."
  (let ((hold (with-ledger (ledger) (gethash worker (ledger-holds ledger)))))
    (when hold (sb-thread:signal-semaphore hold))))

(defun %start-thread (ledger thunk name)
  "Run THUNK on a new thread called NAME, recorded in LEDGER."
  (let ((thread (bt:make-thread thunk :name name)))
    (with-ledger (ledger) (push thread (ledger-threads ledger)))
    thread))

(defun ended-p (ledger worker)
  "True when the pool ended WORKER."
  (with-ledger (ledger) (and (gethash worker (ledger-ended-at ledger)) t)))

(defun %live-workers (ledger)
  "The workers LEDGER saw spawned and not ended.  Call with its lock held."
  (remove-if (lambda (worker) (gethash worker (ledger-ended-at ledger)))
             (ledger-spawned ledger)))

;;; ------------------------------------------------------------------------
;;; B. The harness

(defparameter +globals+
  '(cl-mcp/src/pool::*spawn-worker-function*
    cl-mcp/src/pool::*kill-worker-function*
    cl-mcp/src/pool::*worker-alive-function*
    cl-mcp/src/pool::*start-pool-thread-function*
    cl-mcp/src/pool::*signal-worker-function*
    cl-mcp/src/pool:*worker-pool-warmup*
    cl-mcp/src/pool:*max-pool-size*
    cl-mcp/src/pool::*health-check-interval-seconds*
    cl-mcp/src/pool::*shutdown-replenish-wait-seconds*
    cl-mcp/src/pool::*worker-init-config*
    cl-mcp/src/pool::*crash-breaker-threshold*
    cl-mcp/src/worker-client::*worker-startup-timeout*
    cl-mcp/src/project-root:*project-root*
    cl-mcp/src/log:*log-level*)
  "The specials the harness sets for a run and puts back afterwards.  Set, not
bound: the pool's threads do not see a binding made here.")

(defparameter *shutdown-bound-seconds* 8
  "How long a shutdown may take in these runs before it counts as stuck.  The
fake spawns and endings take a fraction of a second, and
*WORKER-STARTUP-TIMEOUT* is set to 2, so a correct shutdown waits for
nothing near this.")

(defun call-with-concurrent-pool (function &key (warmup 1) (max-size 4)
                                             (spawn-delay 0) (end-delay 0))
  "Initialize the real pool with fake workers whose background work runs on
real threads, call FUNCTION with the ledger, then shut the pool down and put
everything back.  Returns FUNCTION's values.

The teardown opens every gate, shuts the pool down if it is still running,
and waits for the pool's threads; only then does it end, itself, any worker
the pool left live -- which FUNCTION's own checks, made before, report."
  (when cl-mcp/src/pool::*pool-running*
    (error "A worker pool is running in this image; the concurrency fixture ~
would take it over.  Run it in a process with no pool."))
  (let ((ledger (make-ledger :spawn-delay spawn-delay :end-delay end-delay))
        (saved (mapcar #'symbol-value +globals+))
        (exit-hooks (copy-list sb-ext:*exit-hooks*)))
    (unwind-protect
         (progn
           (setf cl-mcp/src/pool::*spawn-worker-function*
                 (lambda () (%fake-spawn ledger))
                 cl-mcp/src/pool::*kill-worker-function*
                 (lambda (worker) (%fake-end ledger worker))
                 cl-mcp/src/pool::*worker-alive-function*
                 (lambda (worker) (%fake-alive-p ledger worker))
                 cl-mcp/src/pool::*start-pool-thread-function*
                 (lambda (thunk name) (%start-thread ledger thunk name))
                 cl-mcp/src/pool::*signal-worker-function*
                 (lambda (worker) (%fake-signal ledger worker))
                 *worker-pool-warmup* warmup
                 *max-pool-size* max-size
                 cl-mcp/src/pool::*health-check-interval-seconds* 3600
                 cl-mcp/src/pool::*shutdown-replenish-wait-seconds* 0.01d0
                 cl-mcp/src/pool::*worker-init-config* nil
                 cl-mcp/src/pool::*crash-breaker-threshold* 1000
                 cl-mcp/src/worker-client::*worker-startup-timeout* 2
                 *project-root* nil
                 *log-level* :error)
           (initialize-pool)
           (funcall function ledger))
      (%open-every-gate ledger)
      (ignore-errors
       (when cl-mcp/src/pool::*pool-running* (shutdown-pool)))
      (%join-threads (with-ledger (ledger) (copy-list (ledger-threads ledger))) 10)
      (dolist (worker (with-ledger (ledger) (%live-workers ledger)))
        (ignore-errors (kill-worker worker)))
      (loop for symbol in +globals+
            for value in saved
            do (setf (symbol-value symbol) value))
      (setf sb-ext:*exit-hooks* exit-hooks))))

(defmacro with-concurrent-pool ((ledger &rest options) &body body)
  "Run BODY with LEDGER bound, inside CALL-WITH-CONCURRENT-POOL."
  `(call-with-concurrent-pool (lambda (,ledger) ,@body) ,@options))

(defun %open-every-gate (ledger)
  "Let every gated spawn, ending and held RPC go."
  (with-ledger (ledger)
    (let ((gate (ledger-spawn-gate ledger)))
      (setf (ledger-spawn-gate ledger) nil)
      (when gate (sb-thread:signal-semaphore gate 100)))
    (maphash (lambda (worker gate)
               (declare (ignore worker))
               (sb-thread:signal-semaphore gate 10))
             (ledger-end-gates ledger))
    (maphash (lambda (worker hold)
               (declare (ignore worker))
               (sb-thread:signal-semaphore hold 10))
             (ledger-holds ledger))))

(defun %join-threads (threads seconds)
  "Wait up to SECONDS for THREADS to finish; return those still alive."
  (let ((deadline (+ (%now) (* seconds internal-time-units-per-second))))
    (loop for thread in threads
          do (loop while (and (bt:thread-alive-p thread) (< (%now) deadline))
                   do (sleep 0.01)))
    (remove-if-not #'bt:thread-alive-p threads)))

(defun %await (predicate &key (within 10))
  "Wait until PREDICATE is true, up to WITHIN seconds; return whether it got
there."
  (loop repeat (* 100 within)
        when (funcall predicate) return t
        do (sleep 0.01)
        finally (return nil)))

;;; ------------------------------------------------------------------------
;;; C. What the pool holds, and what must hold at every moment

(defun pool-state ()
  "The pool's lists and counts, read under its lock."
  (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
    (%pool-state-locked)))

(defun %pool-state-locked ()
  "POOL-STATE's reading, with *POOL-LOCK* already held."
  (list :all (copy-list cl-mcp/src/pool::*all-workers*)
        :standby (copy-list cl-mcp/src/pool::*standby-workers*)
        :map (let ((pairs '()))
               (maphash (lambda (session entry) (push (cons session entry) pairs))
                        cl-mcp/src/pool::*affinity-map*)
               pairs)
        :size (cl-mcp/src/pool::%effective-pool-size)
        :spawns (cl-mcp/src/pool::generation-spawns cl-mcp/src/pool::*generation*)
        :ending (copy-list (cl-mcp/src/pool::generation-ending
                            cl-mcp/src/pool::*generation*))
        :running (and cl-mcp/src/pool::*pool-running* t)))

(defun %duplicates (list)
  "Return the elements that occur more than once in LIST."
  (remove-duplicates (loop for (item . rest) on list when (member item rest) collect item)))

(defun moment-violations (ledger)
  "Return what the pool breaks at this moment, read in one piece: under
*POOL-LOCK*, then the ledger's lock, so nothing moves while it is read.

- No worker is held twice, or tracked twice, and every held one is tracked.
- No worker the ledger saw ended is held or tracked.
- A worker the map holds is bound to the session it is held for.
- The pool counts no more than its cap.
- No worker is lost track of: every live worker is tracked, being ended, or
  one of the spawns in flight -- of which there are no more live and
  untracked ones than the pool counts.  A worker nobody tracks, is ending
  or is spawning is a process nobody will end."
  (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
    (with-ledger (ledger)
      (let* ((state (%pool-state-locked))
             (all (getf state :all))
             (mapped (loop for (nil . entry) in (getf state :map)
                           when (typep entry 'worker) collect entry))
             (held (append mapped (getf state :standby)))
             (violations '()))
        (flet ((add (kind &rest detail) (push (list* :kind kind detail) violations))
               (ids (workers) (mapcar #'worker-id workers)))
          (let ((twice (%duplicates held)))
            (when twice (add :held-twice :workers (ids twice))))
          (let ((twice (%duplicates all)))
            (when twice (add :tracked-twice :workers (ids twice))))
          (let ((untracked (set-difference held all)))
            (when untracked (add :held-untracked :workers (ids untracked))))
          (let ((ended (remove-if-not (lambda (w) (gethash w (ledger-ended-at ledger)))
                                      (union held all))))
            (when ended (add :ended-but-held :workers (ids ended))))
          (loop for (session . entry) in (getf state :map)
                when (and (typep entry 'worker)
                          (not (equal session (worker-session-id entry))))
                  do (add :map-names-another-session :session session
                          :worker (worker-id entry)))
          (when (> (getf state :size) *max-pool-size*)
            (add :over-capacity :size (getf state :size) :cap *max-pool-size*))
          (let ((loose (set-difference (%live-workers ledger)
                                       (union all (getf state :ending)))))
            (when (> (length loose) (getf state :spawns))
              (add :lost-track :workers (ids loose) :spawns (getf state :spawns)))))
        (nreverse violations)))))

(defun after-shutdown-violations (ledger returned-at)
  "Return what a pool shut down at RETURNED-AT, an internal real time, still
owes: every worker it was handed is ended; no spawn or ending of its
completed after the shutdown returned; its lists, counts and runtime owner
are empty; and no thread it started is still running."
  (let ((violations '()))
    (flet ((add (kind &rest detail) (push (list* :kind kind detail) violations)))
      (let ((state (pool-state)))
        (when (or (getf state :all) (getf state :standby) (getf state :map))
          (add :holds-after-shutdown))
        (unless (and (zerop (getf state :spawns)) (null (getf state :ending)))
          (add :work-in-flight-after-shutdown :spawns (getf state :spawns)
               :ending (length (getf state :ending))))
        (when (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                cl-mcp/src/pool::*runtime-owner*)
          (add :owner-after-shutdown)))
      (with-ledger (ledger)
        (let ((live (%live-workers ledger)))
          (when live (add :live-after-shutdown :workers (mapcar #'worker-id live))))
        (let ((late-spawns (loop for worker in (ledger-spawned ledger)
                                 when (> (gethash worker (ledger-spawned-at ledger))
                                         returned-at)
                                   collect (worker-id worker)))
              (late-ends (loop for worker being the hash-keys of (ledger-ended-at ledger)
                                 using (hash-value at)
                               when (> at returned-at)
                                 collect (worker-id worker))))
          (when late-spawns (add :spawned-after-shutdown :workers late-spawns))
          (when late-ends (add :ended-after-shutdown :workers late-ends))))
      (let ((running (remove-if-not #'bt:thread-alive-p
                                    (with-ledger (ledger)
                                      (copy-list (ledger-threads ledger))))))
        (when running
          (add :pool-thread-after-shutdown
               :threads (mapcar #'bt:thread-name running)))))
    (nreverse violations)))

;;; ------------------------------------------------------------------------
;;; D. Shutdown scenarios

(defparameter +in-flight-kinds+
  '(:acquire-spawn :replenish-spawn :recovery-spawn :ending :rpc-held)
  "What can be in flight when a shutdown begins: a spawn made by an acquire,
by replenishment, or by crash recovery; a worker taken out to be ended and
not ended yet; and an RPC holding a worker's stream.")

(defun draw-shutdown-scenario ()
  "Draw a scenario with CL:RANDOM: a non-empty set of in-flight kinds, the
order their gates open in, and whether an acquire arrives once the shutdown
has begun."
  (let ((kinds (loop for kind in +in-flight-kinds+
                     when (zerop (random 2)) collect kind)))
    (unless kinds
      (setf kinds (list (nth (random (length +in-flight-kinds+)) +in-flight-kinds+))))
    (list :in-flight kinds
          :release-order (%shuffle (copy-list kinds))
          :late-acquire (zerop (random 2)))))

(defun %shuffle (list)
  "Return LIST in a random order drawn with CL:RANDOM."
  (let ((vector (coerce list 'vector)))
    (loop for i from (1- (length vector)) downto 1
          do (rotatef (aref vector i) (aref vector (random (1+ i)))))
    (coerce vector 'list)))

(defun %outcome (thunk)
  "Call THUNK and return (:value V) or (:error CONDITION-TYPE)."
  (handler-case (list :value (funcall thunk))
    (error (e) (list :error (type-of e)))))

(defun %gate-spawns (ledger)
  "Make every spawn from now on wait on a new gate, and return it."
  (let ((gate (sb-thread:make-semaphore)))
    (with-ledger (ledger) (setf (ledger-spawn-gate ledger) gate))
    gate))

(defun %spawns-begun (ledger)
  "How many spawns have begun."
  (with-ledger (ledger) (ledger-spawns-begun ledger)))

(defun run-shutdown-scenario (scenario)
  "Run SCENARIO and return what happened, as a plist of observations.

Sessions s2, s3 and s4 are given workers first, with nothing gated.  Then
every spawn is gated and each in-flight kind is started, and waited for
until it is in flight:

  :ACQUIRE-SPAWN    s1 is acquired on a thread of its own; its spawn waits;
  :REPLENISH-SPAWN  the warmup is raised to one and a replenishment started;
                    its spawn waits;
  :RECOVERY-SPAWN   s2's worker dies and a health check starts its recovery;
                    the replacement's spawn waits;
  :ENDING           s3 is released on a thread of its own; its worker's
                    ending waits;
  :RPC-HELD         a thread holds s4's worker's stream, as an RPC does,
                    until the worker is signalled.

SHUTDOWN-POOL is then called on a thread of its own.  Once it has begun, an
acquire of s5 is made when :LATE-ACQUIRE says so, and the gates are opened
in :RELEASE-ORDER -- the spawn gate for any of the spawn kinds, the ending's
for :ENDING; a held RPC is let go only by the shutdown's own signal.  If the
shutdown has not returned within *SHUTDOWN-BOUND-SECONDS*, everything is let
go and that is recorded."
  (destructuring-bind (&key in-flight release-order late-acquire) scenario
    (with-concurrent-pool (ledger :warmup 0 :max-size 8)
      (let ((workers (list :s2 (get-or-assign-worker "s2")
                           :s3 (get-or-assign-worker "s3")
                           :s4 (get-or-assign-worker "s4")))
            (acquire nil) (late nil) (release nil) (holder nil)
            (holding (sb-thread:make-semaphore))
            (end-gate (sb-thread:make-semaphore))
            (spawn-gate (%gate-spawns ledger))
            (spawns-expected 0))
        (flet ((expect-spawn () (incf spawns-expected)))
          (when (member :acquire-spawn in-flight)
            (expect-spawn)
            (setf acquire (bt:make-thread
                           (lambda () (%outcome (lambda () (get-or-assign-worker "s1"))))
                           :name "scenario-acquire"))
            (%await (lambda () (>= (%spawns-begun ledger) spawns-expected))))
          (when (member :replenish-spawn in-flight)
            (expect-spawn)
            (setf *worker-pool-warmup* 1)
            (cl-mcp/src/pool::%schedule-replenish)
            (%await (lambda () (>= (%spawns-begun ledger) spawns-expected))))
          (when (member :recovery-spawn in-flight)
            (expect-spawn)
            (with-ledger (ledger)
              (setf (gethash (getf workers :s2) (ledger-dead ledger)) t))
            (cl-mcp/src/pool::%check-worker-health)
            (%await (lambda () (>= (%spawns-begun ledger) spawns-expected))))
          (when (member :ending in-flight)
            (with-ledger (ledger)
              (setf (gethash (getf workers :s3) (ledger-end-gates ledger)) end-gate))
            (setf release (bt:make-thread (lambda () (release-session "s3"))
                                          :name "scenario-release"))
            (%await (lambda ()
                      (with-ledger (ledger)
                        (member (getf workers :s3) (ledger-ends-begun ledger))))))
          (when (member :rpc-held in-flight)
            (let ((worker (getf workers :s4))
                  (hold (sb-thread:make-semaphore)))
              (with-ledger (ledger) (setf (gethash worker (ledger-holds ledger)) hold))
              (setf holder
                    (bt:make-thread
                     (lambda ()
                       (bt:with-lock-held ((worker-stream-lock worker))
                         (sb-thread:signal-semaphore holding)
                         (sb-thread:wait-on-semaphore hold :timeout 30)))
                     :name "scenario-rpc"))
              (sb-thread:wait-on-semaphore holding :timeout 10))))
        ;; The shutdown, and what happens once it has begun.
        (let* ((returned-at nil)
               (shutdown (bt:make-thread
                          (lambda () (shutdown-pool) (setf returned-at (%now)))
                          :name "scenario-shutdown")))
          (%await (lambda () (not cl-mcp/src/pool::*pool-running*)))
          (when late-acquire
            (setf late (bt:make-thread
                        (lambda () (%outcome (lambda () (get-or-assign-worker "s5"))))
                        :name "scenario-late-acquire")))
          (dolist (kind release-order)
            (sleep 0.05)
            (case kind
              ((:acquire-spawn :replenish-spawn :recovery-spawn)
               (sb-thread:signal-semaphore spawn-gate 10))
              (:ending (sb-thread:signal-semaphore end-gate))))
          (let ((blocked (not (%await (lambda () returned-at)
                                      :within *shutdown-bound-seconds*))))
            (when blocked (%open-every-gate ledger))
            (%await (lambda () returned-at) :within *shutdown-bound-seconds*)
            (let ((after (and returned-at (after-shutdown-violations ledger returned-at))))
              ;; Everything the scenario started is let go and joined, so the
              ;; next scenario starts from nothing -- and asked again: a spawn
              ;; or an ending a wrong shutdown did not wait for may complete
              ;; only now, and its time says it came after the shutdown.
              (%open-every-gate ledger)
              (%join-threads (remove nil (list shutdown acquire late release holder)) 10)
              (%join-threads (with-ledger (ledger) (copy-list (ledger-threads ledger))) 5)
              (when returned-at
                (setf after (remove-duplicates
                             (append after (after-shutdown-violations ledger returned-at))
                             :test #'equal)))
              (list :scenario scenario
                    :blocked blocked
                    :returned (and returned-at t)
                    :after after
                    :acquire (and acquire (bt:join-thread acquire))
                    :late (and late (bt:join-thread late))))))))))

(defun shutdown-scenario-violations (observed)
  "Return what the shutdown in OBSERVED broke:

- it returned, and within *SHUTDOWN-BOUND-SECONDS* without anything but its
  own signal letting a held RPC go;
- once it returned, the pool owed nothing (AFTER-SHUTDOWN-VIOLATIONS);
- the acquire whose spawn it overtook, and any acquire made after it began,
  were refused rather than lent a worker."
  (let ((violations '()))
    (flet ((add (kind &rest detail) (push (list* :kind kind detail) violations)))
      (unless (getf observed :returned) (add :shutdown-did-not-return))
      (when (getf observed :blocked) (add :shutdown-blocked))
      (dolist (violation (getf observed :after)) (push violation violations))
      (dolist (key '(:acquire :late))
        (let ((outcome (getf observed key)))
          (when (and outcome (eq :value (first outcome)))
            (add :lent-during-shutdown :acquire key)))))
    (nreverse violations)))

;;; ------------------------------------------------------------------------
;;; E. Concurrent runs

(defparameter +concurrent-sessions+ '("c0" "c1" "c2" "c3" "c4" "c5")
  "The sessions a concurrent run acts for.")

(defun %random-concurrent-operation ()
  "Draw one operation with CL:RANDOM."
  (let ((session (nth (random (length +concurrent-sessions+)) +concurrent-sessions+))
        (roll (random 100)))
    (cond ((< roll 40) (list :acquire session))
          ((< roll 55) (list :release session))
          ((< roll 65) (list :kill-session session))
          ((< roll 75) (list :die (random 16)))
          ((< roll 82) (list :rpc-crash (random 16)))
          ((< roll 92) (list :health-check))
          (t (list :pause)))))

(defun random-concurrent-plan ()
  "Draw a plan with CL:RANDOM: 2 to 4 clients of 5 to 25 operations each, a
pool sizing, and after how many operations the shutdown begins."
  (let ((clients (+ 2 (random 3))))
    (list :clients (loop repeat clients
                         collect (loop repeat (+ 5 (random 21))
                                       collect (%random-concurrent-operation)))
          :warmup (random 3)
          :max-size (+ 2 (random 4))
          :shutdown-after (random 40))))

(defun %worker-at (ledger index)
  "The INDEXth worker LEDGER saw, modulo how many, or NIL."
  (with-ledger (ledger)
    (let ((spawned (ledger-spawned ledger)))
      (and spawned (nth (mod index (length spawned)) spawned)))))

(defun %apply-concurrent (ledger lent operation)
  "Apply OPERATION from a client thread.  LENT is a function recording that a
worker was lent to a session."
  (destructuring-bind (kind &optional argument) operation
    (ecase kind
      (:acquire
       (handler-case (funcall lent (get-or-assign-worker argument) argument)
         (error () nil)))
      (:release (release-session argument))
      (:kill-session (kill-session-worker argument))
      (:die
       (let ((worker (%worker-at ledger argument)))
         (when worker (with-ledger (ledger) (setf (gethash worker (ledger-dead ledger)) t)))))
      (:rpc-crash
       ;; As WORKER-RPC does it: with the worker's stream held.
       (let ((worker (%worker-at ledger argument)))
         (when (and worker (member (worker-state worker) '(:bound :standby)))
           (bt:with-lock-held ((worker-stream-lock worker))
             (cl-mcp/src/worker-client::%mark-worker-crashed worker "timeout")))))
      (:health-check
       (when cl-mcp/src/pool::*pool-running*
         (cl-mcp/src/pool::%check-worker-health)))
      (:pause (sleep 0.005)))))

(defun run-concurrent-plan (plan)
  "Run PLAN and return every violation.  A spawn and an ending take up to
:SPAWN-DELAY and :END-DELAY seconds each (0.003 when the plan does not say).

The clients run their operations at once.  An observer checks
MOMENT-VIOLATIONS continually, through the shutdown a separate thread makes
once the clients have applied :SHUTDOWN-AFTER operations between them, with
the clients still running.  Then: the pool owes nothing
(AFTER-SHUTDOWN-VIOLATIONS); no worker was lent to two sessions; and none
was lent after the shutdown returned."
  (destructuring-bind (&key clients warmup max-size shutdown-after
                         (spawn-delay 0.003) (end-delay 0.003))
      plan
    (with-concurrent-pool (ledger :warmup warmup :max-size max-size
                                  :spawn-delay spawn-delay :end-delay end-delay)
      (let* ((lock (bt:make-lock "concurrent-run"))
             (violations '())
             (lendings '())
             (applied 0)
             (done nil)
             (returned-at nil)
             (lent (lambda (worker session)
                     (bt:with-lock-held (lock)
                       (push (list worker session (%now)) lendings))))
             (observer
               (bt:make-thread
                (lambda ()
                  (loop until done
                        do (let ((found (moment-violations ledger)))
                             (when found
                               (bt:with-lock-held (lock)
                                 (setf violations (append violations found)))))
                           (sleep 0.001)))
                :name "concurrent-observer"))
             (threads
               (loop for operations in clients
                     for index from 0
                     collect (let ((operations operations))
                               (bt:make-thread
                                (lambda ()
                                  (dolist (operation operations)
                                    (ignore-errors
                                     (%apply-concurrent ledger lent operation))
                                    (bt:with-lock-held (lock) (incf applied))))
                                :name (format nil "concurrent-client-~D" index)))))
             (shutdown
               (bt:make-thread
                (lambda ()
                  (%await (lambda () (bt:with-lock-held (lock) (>= applied shutdown-after)))
                          :within 20)
                  ;; The observer goes on checking while the shutdown runs:
                  ;; what must hold at every moment holds during it too.
                  (shutdown-pool)
                  (setf returned-at (%now)
                        done t))
                :name "concurrent-shutdown")))
        (unless (%await (lambda () returned-at) :within 60)
          (push (list :kind :shutdown-did-not-return) violations))
        (%join-threads threads 30)
        (setf done t)
        (when returned-at
          (setf violations (append violations
                                   (after-shutdown-violations ledger returned-at))))
        (%join-threads (list shutdown observer) 10)
        ;; What the clients were told.
        (let ((owners (make-hash-table :test 'eq)))
          (loop for (worker session at) in (reverse lendings)
                do (let ((owner (gethash worker owners)))
                     (cond ((null owner) (setf (gethash worker owners) session))
                           ((not (equal owner session))
                            (push (list :kind :lent-to-two-sessions
                                        :worker (worker-id worker)
                                        :first owner :second session)
                                  violations))))
                   (when (and returned-at (> at returned-at))
                     (push (list :kind :lent-after-shutdown :worker (worker-id worker))
                           violations))))
        (remove-duplicates violations :test #'equal)))))

(defun concurrency-violation-kinds (violations)
  "Return the distinct kinds of VIOLATIONS."
  (remove-duplicates (mapcar (lambda (v) (getf v :kind)) violations)))

;;; ------------------------------------------------------------------------
;;; F. Work that outlives a shutdown's deadline

(defparameter +late-kinds+ '(:acquire-spawn :replenish-spawn :recovery-spawn)
  "Work that can still be spawning when a shutdown gives up waiting for it.")

(defun draw-late-scenario ()
  "Draw a late-work scenario with CL:RANDOM: which spawn outlives the
shutdown's deadline, the next pool's cap, and whether it keeps a standby."
  (list :late (nth (random (length +late-kinds+)) +late-kinds+)
        :max-size (1+ (random 2))
        :warmup (random 2)))

(defun %generation-spawns ()
  "The running generation's spawns in flight."
  (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
    (cl-mcp/src/pool::generation-spawns cl-mcp/src/pool::*generation*)))

(defun run-late-scenario (scenario)
  "Run SCENARIO and return its violations.

A spawn of kind :LATE is started and held on a gate that stays shut past the
shutdown's deadline, made one second here.  The shutdown returns, a new pool
is initialized with :MAX-SIZE and :WARMUP, and only then is the old spawn
let go.  What the new pool must not feel:

- it lends a session a worker at once, the old spawn notwithstanding, and
  keeps its standby when asked to;
- its account never holds the old spawn;
- the old spawn's worker never enters it, and is ended by the work that
  spawned it once it returns.

The new pool is then shut down, and owes nothing (AFTER-SHUTDOWN-VIOLATIONS)."
  (destructuring-bind (&key late max-size warmup) scenario
    (with-concurrent-pool (ledger :warmup 0 :max-size 4)
      (let ((violations '())
            (bound (and (eq late :recovery-spawn) (get-or-assign-worker "s2")))
            (stuck nil)
            (gate (%gate-spawns ledger))
            (base (%spawns-begun ledger)))
        (flet ((add (kind &rest detail) (push (list* :kind kind detail) violations)))
          (ecase late
            (:acquire-spawn
             (setf stuck (bt:make-thread
                          (lambda () (%outcome (lambda () (get-or-assign-worker "s1"))))
                          :name "late-acquire")))
            (:replenish-spawn
             (setf *worker-pool-warmup* 1)
             (cl-mcp/src/pool::%schedule-replenish))
            (:recovery-spawn
             (with-ledger (ledger) (setf (gethash bound (ledger-dead ledger)) t))
             (cl-mcp/src/pool::%check-worker-health)))
          (%await (lambda () (> (%spawns-begun ledger) base)))
          ;; A one-second deadline: the worker startup timeout plus 15.
          (setf cl-mcp/src/worker-client::*worker-startup-timeout* -14)
          (let ((start (%now)))
            (shutdown-pool)
            (when (> (- (%now) start) (* 5 internal-time-units-per-second))
              (add :shutdown-overran-its-deadline)))
          (let ((old (with-ledger (ledger)
                       (setf (ledger-spawn-gate ledger) nil)
                       (copy-list (ledger-spawned ledger))))
                (old-generation (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                                  (cl-mcp/src/pool::generation-id
                                   cl-mcp/src/pool::*generation*))))
            (setf *worker-pool-warmup* warmup
                  *max-pool-size* max-size
                  cl-mcp/src/worker-client::*worker-startup-timeout* 2)
            (initialize-pool)
            (unless (%outcome-lent-p (%outcome (lambda () (get-or-assign-worker "n1"))))
              (add :new-pool-refused))
            (when (and (plusp warmup) (< 1 max-size))
              (unless (%await (lambda () (getf (pool-state) :standby)) :within 3)
                (add :new-pool-not-replenished)))
            (%await (lambda () (zerop (%generation-spawns))) :within 3)
            (unless (zerop (%generation-spawns))
              (add :new-account-holds-old-work :spawns (%generation-spawns)))
            ;; Now the old spawn returns.
            (sb-thread:signal-semaphore gate 10)
            (when stuck (bt:join-thread stuck))
            (%await (lambda () (%late-workers ledger old-generation old)) :within 5)
            (let ((late-workers (%late-workers ledger old-generation old)))
              (dolist (worker late-workers)
                (unless (%await (lambda () (ended-p ledger worker)) :within 5)
                  (add :late-worker-left-live :worker (worker-id worker)))
                (let ((state (pool-state)))
                  (when (member worker (append (getf state :all) (getf state :standby)))
                    (add :late-worker-joined-new-pool :worker (worker-id worker)))))
              (unless late-workers (add :late-spawn-never-returned))))
          (let ((returned-at nil))
            (shutdown-pool)
            (setf returned-at (%now))
            (dolist (violation (after-shutdown-violations ledger returned-at))
              (push violation violations))))
        (nreverse violations)))))

(defun %outcome-lent-p (outcome)
  "True when OUTCOME, from %OUTCOME, is a worker lent."
  (and (eq :value (first outcome)) (typep (second outcome) 'worker)))

(defun %late-workers (ledger old-generation before)
  "The workers LEDGER saw spawned by work OLD-GENERATION started, other than
those in BEFORE: the spawns that outlived that generation's shutdown."
  (with-ledger (ledger)
    (loop for worker in (ledger-spawned ledger)
          when (and (eql old-generation (gethash worker (ledger-spawned-in ledger)))
                    (not (member worker before)))
            collect worker)))
