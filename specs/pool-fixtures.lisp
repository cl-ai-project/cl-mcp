;;;; specs/pool-fixtures.lisp
;;;;
;;;; The real worker pool (src/pool.lisp), run over operation sequences with
;;;; workers that are not processes, for the ownership properties
;;;; (specs/pool-ownership.lisp) and their fixed cases
;;;; (tests/pool-ownership-test.lisp).  Needs no cl-spec, so the fixed cases run
;;;; in the default suite.
;;;;
;;;; Three things here are kept apart on purpose, because each can only check
;;;; the others if it does not share their mistakes:
;;;;
;;;; - The LEDGER is written by the fake lifecycle, not by the pool: every
;;;;   worker the pool asked for, every worker it ended, and which fake
;;;;   processes died.  It is the independent record of what exists.  A pool
;;;;   that drops a worker from its lists without ending it looks clean from
;;;;   the lists alone; against the ledger it is a live worker nobody owns.
;;;; - The pool's own lists, read under its lock, are what the invariants
;;;;   are about.
;;;; - The MODEL records what the operations promise a caller -- which
;;;;   session was lent which worker, which workers were known unusable --
;;;;   and nothing about how the pool keeps its lists.  It is deliberately
;;;;   not a second pool: it says what must not happen (a worker lent to two
;;;;   sessions, an unusable worker newly lent, a released worker left
;;;;   running), not how the pool should get there.
;;;;
;;;; Background work -- standby replenishment, crash recovery -- is queued
;;;; rather than run on threads, and runs only when an operation says so.
;;;; Between operations with work still queued the pool is in an
;;;; INTERMEDIATE state; with nothing queued it is STABLE.  Some invariants
;;;; hold at every point between operations, some only at rest, and the
;;;; checks say which is which.

(defpackage #:cl-mcp/specs/pool-fixtures
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
                #:kill-worker)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log
                #:*log-level*)
  ;; Bare: the pool's lock is read through the BT nickname.
  (:import-from #:bordeaux-threads)
  (:export #:+sessions+
           #:ledger
           #:ledger-spawned
           #:ledger-kill-count
           #:ledger-tasks
           #:ledger-reaped-by-fixture
           #:killed-p
           #:call-with-fake-pool
           #:with-fake-pool
           #:fake-spawn
           #:run-pending-work
           #:pool-snapshot
           #:ownership-violations
           #:run-operation
           #:run-operation-sequence
           #:random-operation
           #:random-operation-sequence
           #:violation-kinds))

(in-package #:cl-mcp/specs/pool-fixtures)

(defparameter +sessions+ '("s0" "s1" "s2" "s3")
  "The sessions an operation sequence acts for.  Few, so sessions recur.")

;;; ------------------------------------------------------------------------
;;; A. The ledger and the fake lifecycle

(defstruct ledger
  "What the fake lifecycle saw, written by it and by nothing in the pool."
  ;; Every worker the pool was handed, oldest first.
  (spawned '())
  ;; Worker -> how many times the pool ended it.
  (kill-count (make-hash-table :test 'eq))
  ;; Workers whose fake process has died.
  (dead (make-hash-table :test 'eq))
  ;; Spawns still to fail, as an injected fault.
  (spawn-failures 0)
  ;; Background work the pool started and nobody has run, oldest first, as
  ;; (NAME . THUNK).
  (tasks '())
  ;; Workers the fixture had to end itself after the run, because the pool
  ;; left them live.  Reported as a violation: cleaning up after the pool
  ;; must not be what makes a leak disappear.
  (reaped-by-fixture '())
  (next-id 900000))

(defun killed-p (ledger worker)
  "True when the pool ended WORKER at least once."
  (plusp (gethash worker (ledger-kill-count ledger) 0)))

(defun fake-spawn (ledger)
  "Return a new standby worker with no process, recorded in LEDGER; or fail,
when a failure was injected."
  (when (plusp (ledger-spawn-failures ledger))
    (decf (ledger-spawn-failures ledger))
    (error "Injected spawn failure."))
  (let ((worker (make-worker :id (incf (ledger-next-id ledger)) :state :standby)))
    (setf (ledger-spawned ledger) (append (ledger-spawned ledger) (list worker)))
    worker))

(defun %fake-kill (ledger worker)
  "Record that the pool ended WORKER, then end it the way production does.
KILL-WORKER on a worker with no process or socket only marks it :DEAD."
  (incf (gethash worker (ledger-kill-count ledger) 0))
  (kill-worker worker))

(defun %fake-alive-p (ledger worker)
  "True unless WORKER's fake process died or the pool ended it."
  (not (or (gethash worker (ledger-dead ledger))
           (killed-p ledger worker))))

(defun %queue-work (ledger thunk name)
  "Queue THUNK as background work called NAME and return a handle that is
not a thread, so the pool records it without joining it."
  (setf (ledger-tasks ledger)
        (append (ledger-tasks ledger) (list (cons name thunk))))
  (list :queued name))

(defun run-pending-work (ledger)
  "Run the queued background work, oldest first, including work it queues,
until none is left.  Returns how many ran."
  (let ((count 0))
    (loop while (ledger-tasks ledger)
          do (let ((task (pop (ledger-tasks ledger))))
               (incf count)
               (funcall (cdr task))))
    count))

;;; ------------------------------------------------------------------------
;;; B. The harness

(defparameter +pool-globals+
  '(cl-mcp/src/pool::*spawn-worker-function*
    cl-mcp/src/pool::*kill-worker-function*
    cl-mcp/src/pool::*worker-alive-function*
    cl-mcp/src/pool::*start-pool-thread-function*
    cl-mcp/src/pool:*worker-pool-warmup*
    cl-mcp/src/pool:*max-pool-size*
    cl-mcp/src/pool::*health-check-interval-seconds*
    cl-mcp/src/pool::*shutdown-replenish-wait-seconds*
    cl-mcp/src/pool::*worker-init-config*
    cl-mcp/src/pool::*crash-breaker-threshold*
    cl-mcp/src/project-root:*project-root*
    cl-mcp/src/log:*log-level*)
  "The specials the harness sets for a run and puts back afterwards.  Set,
not bound: the pool's health monitor is a thread of its own and does not see
a binding made here.")

(defun call-with-fake-pool (function &key (warmup 1) (max-size 4))
  "Initialize the real pool with fake workers, call FUNCTION with the ledger,
then shut the pool down and put everything back.  Returns FUNCTION's values.

Refuses to run while a pool is running in this image: the pool's state is
global, and a real one would be shut down under its users.

After FUNCTION, the pool is shut down if it is still running and the queued
work drained.  Only then does the fixture end, itself, any worker the pool
left live, and it records each one in LEDGER-REAPED-BY-FIXTURE -- which
OWNERSHIP-VIOLATIONS reports -- so its own cleanup cannot hide a leak."
  (when cl-mcp/src/pool::*pool-running*
    (error "A worker pool is running in this image; the ownership fixture ~
would take it over.  Run it in a process with no pool."))
  (let ((ledger (make-ledger))
        (saved (mapcar #'symbol-value +pool-globals+))
        ;; INITIALIZE-POOL registers SHUTDOWN-POOL to run at exit; a fake
        ;; pool leaves nothing for it to do.
        (exit-hooks (copy-list sb-ext:*exit-hooks*)))
    (unwind-protect
         (progn
           (setf cl-mcp/src/pool::*spawn-worker-function*
                 (lambda () (fake-spawn ledger))
                 cl-mcp/src/pool::*kill-worker-function*
                 (lambda (worker) (%fake-kill ledger worker))
                 cl-mcp/src/pool::*worker-alive-function*
                 (lambda (worker) (%fake-alive-p ledger worker))
                 cl-mcp/src/pool::*start-pool-thread-function*
                 (lambda (thunk name) (%queue-work ledger thunk name))
                 *worker-pool-warmup* warmup
                 *max-pool-size* max-size
                 ;; The monitor thread is started, and sleeps; its checks run
                 ;; only when an operation asks for one.
                 cl-mcp/src/pool::*health-check-interval-seconds* 3600
                 cl-mcp/src/pool::*shutdown-replenish-wait-seconds* 0.0001d0
                 cl-mcp/src/pool::*worker-init-config* nil
                 ;; Enough that a generated sequence does not trip it; the
                 ;; breaker is 4C's.
                 cl-mcp/src/pool::*crash-breaker-threshold* 1000
                 *project-root* nil
                 *log-level* :error)
           (initialize-pool)
           (funcall function ledger))
      (ignore-errors
       (when cl-mcp/src/pool::*pool-running* (shutdown-pool)))
      (ignore-errors (run-pending-work ledger))
      (dolist (worker (ledger-spawned ledger))
        (unless (killed-p ledger worker)
          (push worker (ledger-reaped-by-fixture ledger))
          (ignore-errors (kill-worker worker))))
      (loop for symbol in +pool-globals+
            for value in saved
            do (setf (symbol-value symbol) value))
      (setf sb-ext:*exit-hooks* exit-hooks))))

(defmacro with-fake-pool ((ledger &rest options) &body body)
  "Run BODY with LEDGER bound, inside CALL-WITH-FAKE-POOL."
  `(call-with-fake-pool (lambda (,ledger) ,@body) ,@options))

;;; ------------------------------------------------------------------------
;;; C. What the pool holds

(defun pool-snapshot ()
  "Return the pool's lists as a plist, read under its lock:
:ALL (every tracked worker), :STANDBY, :MAP (session . entry) pairs, :SIZE
(the effective size the pool counts against its cap), :RUNNING."
  (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
    (list :all (copy-list cl-mcp/src/pool::*all-workers*)
          :standby (copy-list cl-mcp/src/pool::*standby-workers*)
          :map (let ((pairs '()))
                 (maphash (lambda (session entry) (push (cons session entry) pairs))
                          cl-mcp/src/pool::*affinity-map*)
                 (sort pairs #'string< :key #'car))
          :size (cl-mcp/src/pool::%effective-pool-size)
          :running (and cl-mcp/src/pool::*pool-running* t))))

(defun %map-workers (snapshot)
  "Return the workers the affinity map of SNAPSHOT holds."
  (loop for (nil . entry) in (getf snapshot :map)
        when (typep entry 'worker) collect entry))

(defun %duplicates (list)
  "Return the elements that occur more than once in LIST."
  (remove-duplicates
   (loop for (item . rest) on list when (member item rest) collect item)))

;;; ------------------------------------------------------------------------
;;; D. Invariants

(defun %violation (kind &rest detail)
  "Return a violation of KIND with DETAIL, a plist."
  (list* :kind kind detail))

(defun ownership-violations (ledger &key stable shut-down)
  "Return the ownership invariants the pool breaks now, as violations.

At every point between operations:
- no worker is held twice, in the map, the standby list or both;
- every worker in the map or the standby list is tracked, once;
- no worker the pool ended is still held or tracked;
- every worker the pool was handed and has not ended is tracked -- a live
  worker the pool does not know is a process nobody will end;
- a worker the map holds names the session it is held for;
- the pool counts no more than its cap.

STABLE -- nothing queued -- adds what holds only at rest: no spawn is in
flight, so no placeholder is left in the map, and the tracked list is
exactly the map's workers and the standbys, with nothing left over.

SHUT-DOWN adds what a stopped pool owes: it holds nothing, and every worker
it was handed has been ended.

The fixture's own reaping is always reported: a worker it had to end is one
the pool left live."
  (let* ((snapshot (pool-snapshot))
         (all (getf snapshot :all))
         (standby (getf snapshot :standby))
         (mapped (%map-workers snapshot))
         (held (append mapped standby))
         (violations '()))
    (flet ((add (kind &rest detail)
             (push (apply #'%violation kind detail) violations))
           (ids (workers) (mapcar #'worker-id workers)))
      (let ((twice (%duplicates held)))
        (when twice (add :held-twice :workers (ids twice))))
      (let ((twice (%duplicates all)))
        (when twice (add :tracked-twice :workers (ids twice))))
      (let ((untracked (set-difference held all)))
        (when untracked (add :held-untracked :workers (ids untracked))))
      (let ((ended (remove-if-not (lambda (w) (killed-p ledger w)) (union held all))))
        (when ended (add :ended-but-held :workers (ids ended))))
      (let ((orphans (remove-if (lambda (w) (or (killed-p ledger w) (member w all)))
                                (ledger-spawned ledger))))
        (when orphans (add :orphan :workers (ids orphans))))
      (loop for (session . entry) in (getf snapshot :map)
            when (and (typep entry 'worker)
                      (not (equal session (worker-session-id entry))))
              do (add :map-names-another-session :session session
                      :worker (worker-id entry)))
      (when (> (getf snapshot :size) *max-pool-size*)
        (add :over-capacity :size (getf snapshot :size) :cap *max-pool-size*))
      (when stable
        (let ((placeholders (loop for (session . entry) in (getf snapshot :map)
                                  unless (typep entry 'worker) collect session)))
          (when placeholders (add :placeholder-at-rest :sessions placeholders)))
        (let ((stray (set-difference all held)))
          (when stray (add :tracked-but-not-held :workers (ids stray)))))
      (when shut-down
        (when (or all standby (getf snapshot :map))
          (add :holds-after-shutdown :tracked (ids all)))
        (let ((live (remove-if (lambda (w) (killed-p ledger w)) (ledger-spawned ledger))))
          (when live (add :live-after-shutdown :workers (ids live)))))
      (when (ledger-reaped-by-fixture ledger)
        (add :reaped-by-fixture :workers (ids (ledger-reaped-by-fixture ledger)))))
    (nreverse violations)))

(defun violation-kinds (violations)
  "Return the distinct kinds of VIOLATIONS."
  (remove-duplicates (mapcar (lambda (v) (getf v :kind)) violations)))

;;; ------------------------------------------------------------------------
;;; E. Operations and the model

(defstruct model
  "What the operations promised a caller, and nothing about how the pool
keeps its lists."
  ;; Worker -> the session it was first lent to.
  (lent (make-hash-table :test 'eq))
  ;; Session -> the worker it was last handed.
  (holding (make-hash-table :test 'equal))
  ;; Workers known unusable: their process died, an RPC marked them
  ;; crashed, or the pool ended them.
  (unusable (make-hash-table :test 'eq)))

(defun %worker-at (ledger index)
  "Return the INDEXth worker LEDGER saw, modulo how many, or NIL."
  (let ((spawned (ledger-spawned ledger)))
    (when spawned (nth (mod index (length spawned)) spawned))))

(defun %forget-holdings (model)
  "Forget every session's holding: after a shutdown nothing is held."
  (clrhash (model-holding model)))

(defun %check-acquire (ledger model session worker)
  "Return the violations of lending WORKER to SESSION."
  (let* ((previous (gethash session (model-holding model)))
         (fresh (not (eq worker previous)))
         (violations '()))
    (flet ((add (kind &rest detail)
             (push (apply #'%violation kind :session session
                          :worker (worker-id worker) detail)
                   violations)))
      (unless (and (eq :bound (worker-state worker))
                   (equal session (worker-session-id worker)))
        (add :lent-unbound :state (worker-state worker)))
      (when (killed-p ledger worker)
        (add :lent-ended))
      (when fresh
        ;; A worker newly handed to a session must be one nobody knows to
        ;; be unusable.  An existing binding may be returned without a
        ;; check -- the pool learns of a death lazily -- but a new one may not.
        (when (gethash worker (model-unusable model))
          (add :lent-unusable))
        (let ((owner (gethash worker (model-lent model))))
          (when (and owner (not (equal owner session)))
            (add :lent-to-two-sessions :first owner))))
      ;; The same session gets the same worker while that worker is usable.
      (when (and previous fresh
                 (not (gethash previous (model-unusable model)))
                 (not (killed-p ledger previous)))
        (add :affinity-broken :previous (worker-id previous))))
    (setf (gethash session (model-holding model)) worker)
    (unless (gethash worker (model-lent model))
      (setf (gethash worker (model-lent model)) session))
    (nreverse violations)))

(defun %check-after-release (ledger model session)
  "Return the violations of a release of SESSION: the worker it held, if the
pool still bound it to SESSION, must have been ended."
  (let ((worker (gethash session (model-holding model))))
    (remhash session (model-holding model))
    (let ((violations '()))
      (when (and worker (not (killed-p ledger worker))
                 (eq (gethash worker (model-lent model)) session)
                 (not (member worker (getf (pool-snapshot) :all))))
        (push (%violation :released-but-live :session session
                          :worker (worker-id worker))
              violations))
      (when (assoc session (getf (pool-snapshot) :map) :test #'equal)
        (push (%violation :released-but-mapped :session session) violations))
      violations)))

(defun run-operation (ledger model operation)
  "Apply OPERATION to the pool and return (values OUTCOME VIOLATIONS), the
violations being the model's for this operation alone.

Operations:
  (:acquire SESSION)     get-or-assign-worker
  (:release SESSION)     release-session
  (:kill-session SESSION) kill-session-worker, the pool-kill-worker tool
  (:die INDEX)           the INDEXth worker's process dies
  (:rpc-crash INDEX)     an RPC to the INDEXth worker times out, marking it
                         crashed while its process lives on
  (:health-check)        one health monitor iteration
  (:run-work)            run the queued background work
  (:fail-next-spawn)     the next spawn fails
  (:stray-spawn)         a worker is made outside the pool (checks the checks)
  (:shutdown)            shutdown-pool, then the queued work
  (:restart)             initialize-pool, when the pool is stopped"
  (destructuring-bind (kind &optional argument) operation
    (ecase kind
      (:acquire
       (handler-case
           (let ((worker (get-or-assign-worker argument)))
             (values (list :lent (worker-id worker))
                     (%check-acquire ledger model argument worker)))
         (error (condition)
           (remhash argument (model-holding model))
           (values (list :refused (type-of condition))
                   (when (assoc argument (getf (pool-snapshot) :map) :test #'equal)
                     (list (%violation :refused-but-mapped :session argument)))))))
      ((:release :kill-session)
       (if (eq kind :release)
           (release-session argument)
           (kill-session-worker argument))
       (values (list kind argument)
               (%check-after-release ledger model argument)))
      (:die
       (let ((worker (%worker-at ledger argument)))
         (when worker
           (setf (gethash worker (ledger-dead ledger)) t
                 (gethash worker (model-unusable model)) t))
         (values (list :died (and worker (worker-id worker))) '())))
      (:rpc-crash
       (let ((worker (%worker-at ledger argument)))
         ;; Only a worker the pool still tracks is one an RPC can be sent to.
         (if (and worker (member worker (getf (pool-snapshot) :all))
                  (not (killed-p ledger worker)))
             (progn
               (cl-mcp/src/worker-client::%mark-worker-crashed worker "timeout")
               (setf (gethash worker (model-unusable model)) t)
               (values (list :rpc-crashed (worker-id worker)) '()))
             (values (list :rpc-crash-skipped) '()))))
      (:health-check
       (when cl-mcp/src/pool::*pool-running*
         (cl-mcp/src/pool::%check-worker-health))
       (values (list :health-checked) '()))
      (:run-work
       (values (list :ran (run-pending-work ledger)) '()))
      (:fail-next-spawn
       (incf (ledger-spawn-failures ledger))
       (values (list :spawn-will-fail) '()))
      (:stray-spawn
       ;; A worker made outside the pool, which the pool therefore never
       ;; ends: the leak a checker must report.  For checking the checks,
       ;; never drawn by RANDOM-OPERATION.
       (let ((worker (fake-spawn ledger)))
         (values (list :stray (worker-id worker)) '())))
      (:shutdown
       (shutdown-pool)
       (run-pending-work ledger)
       (%forget-holdings model)
       (values (list :shut-down) '()))
      (:restart
       (if cl-mcp/src/pool::*pool-running*
           (values (list :already-running) '())
           (progn
             (setf (ledger-spawn-failures ledger) 0)
             (initialize-pool)
             (values (list :restarted) '())))))))

(defun run-operation-sequence (operations &key (warmup 1) (max-size 4))
  "Run OPERATIONS against a fresh fake pool and return every violation, each
tagged with the index and operation it followed.

After each operation the invariants are checked -- the at-rest ones only
when nothing is queued -- and the model's promises for that operation.  The
sequence always ends with a shutdown, checked for what a stopped pool owes;
then the fixture's teardown, whose reaping is itself a violation."
  (let ((violations '())
        (model (make-model))
        (ledger-seen nil))
    (flet ((note (index operation found)
             (dolist (violation found)
               (push (list* :index index :operation operation violation)
                     violations))))
      (with-fake-pool (ledger :warmup warmup :max-size max-size)
        (setf ledger-seen ledger)
        (loop for operation in operations
              for index from 0
              do (multiple-value-bind (outcome found)
                     (run-operation ledger model operation)
                   (declare (ignore outcome))
                   (note index operation found))
                 (note index operation
                       (ownership-violations
                        ledger
                        :stable (and cl-mcp/src/pool::*pool-running*
                                     (null (ledger-tasks ledger)))
                        :shut-down (not cl-mcp/src/pool::*pool-running*))))
        (let ((index (length operations)))
          (run-operation ledger model '(:shutdown))
          (note index '(:shutdown)
                (ownership-violations ledger :shut-down t))))
      ;; After the fixture's own teardown: only its reaping is new.
      (when (ledger-reaped-by-fixture ledger-seen)
        (note (length operations) '(:teardown)
              (list (%violation :reaped-by-fixture
                                :workers (mapcar #'worker-id
                                                 (ledger-reaped-by-fixture
                                                  ledger-seen)))))))
    (nreverse violations)))

;;; ------------------------------------------------------------------------
;;; F. Generating sequences

(defun random-operation ()
  "Return one operation drawn with CL:RANDOM, weighted toward the ones that
move workers between owners."
  (let ((session (nth (random (length +sessions+)) +sessions+))
        (index (random 8))
        (roll (random 100)))
    (cond ((< roll 30) (list :acquire session))
          ((< roll 40) (list :release session))
          ((< roll 47) (list :kill-session session))
          ((< roll 55) (list :die index))
          ((< roll 62) (list :rpc-crash index))
          ((< roll 72) (list :health-check))
          ((< roll 84) (list :run-work))
          ((< roll 89) (list :fail-next-spawn))
          ((< roll 95) (list :shutdown))
          (t (list :restart)))))

(defun random-operation-sequence (&optional (length (+ 5 (random 26))))
  "Return LENGTH operations drawn with RANDOM-OPERATION."
  (loop repeat length collect (random-operation)))
