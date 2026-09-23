;;;; tests/pool-ownership-test.lisp
;;;;
;;;; Fixed cases for the pool's ownership contract (Phase 4A): which worker
;;;; the pool holds, lends, takes back and ends.  The generated operation
;;;; sequences are in specs/pool-ownership.lisp; these pin the cases that
;;;; must stay caught, run in the default suite and need no cl-spec.
;;;;
;;;; Most run the real pool with workers that are not processes
;;;; (specs/pool-fixtures.lisp), checked against a ledger the fake lifecycle
;;;; keeps.  Three things are pinned:
;;;; - the checks themselves find what they claim to, including a leak the
;;;;   fixture's own cleanup would otherwise hide, and wrong pools whose
;;;;   lists stay consistent;
;;;; - the four ownership faults fixed in 4A stay fixed: a standby an RPC
;;;;   marked crashed is not lent, a dead standby is ended rather than dropped,
;;;;   recovery takes a crashed standby off the standby list, and a spawn that
;;;;   completes after a shutdown is not registered.  The last needs two
;;;;   threads in a fixed order -- the one ordering test in 4A; the rest of
;;;;   concurrency is 4D;
;;;; - with real processes, what was ended is reaped.

(defpackage #:cl-mcp/tests/pool-ownership-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/specs/pool-fixtures
                #:with-fake-pool
                #:fake-spawn
                #:killed-p
                #:ledger-spawned
                #:ledger-reaped-by-fixture
                #:run-pending-work
                #:pool-snapshot
                #:ownership-violations
                #:run-operation-sequence
                #:violation-kinds)
  (:import-from #:cl-mcp/src/pool
                #:get-or-assign-worker
                #:release-session
                #:shutdown-pool
                #:initialize-pool
                #:pool-shutting-down)
  (:import-from #:cl-mcp/src/worker-client
                #:worker
                #:worker-state
                #:worker-pid)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool))

(in-package #:cl-mcp/tests/pool-ownership-test)

(defun %kinds (operations &rest options)
  "Return the violation kinds an operation sequence produces."
  (violation-kinds (apply #'run-operation-sequence operations options)))

(defun %kinds* (operations options)
  "Return the violation kinds of OPERATIONS run with the plist OPTIONS."
  (violation-kinds (apply #'run-operation-sequence operations options)))

;;; ------------------------------------------------------------------------
;;; The checks find what they claim to

(deftest a-leak-is-reported-and-not-hidden-by-the-fixture
  (testing "a worker made outside the pool is an orphan while the pool runs"
    (with-fake-pool (ledger :warmup 0)
      (let ((stray (fake-spawn ledger)))
        (ok (member :orphan (violation-kinds (ownership-violations ledger))))
        (ok (not (killed-p ledger stray)) "and nothing has ended it yet"))))
  (testing "the fixture ends it afterwards, and says it had to"
    (let ((kept nil))
      (with-fake-pool (ledger :warmup 0)
        (setf kept ledger)
        (fake-spawn ledger))
      (ok (= 1 (length (ledger-reaped-by-fixture kept))))))
  (testing "a sequence reports it at the shutdown and again at the teardown"
    (let ((kinds (%kinds '((:stray-spawn)) :warmup 0)))
      (ok (member :orphan kinds))
      (ok (member :live-after-shutdown kinds))
      (ok (member :reaped-by-fixture kinds))))
  (testing "while a clean sequence reports nothing at all"
    (ok (null (run-operation-sequence
               '((:acquire "s0") (:run-work) (:acquire "s1") (:acquire "s0")
                 (:release "s0") (:run-work) (:kill-session "s1") (:run-work)))))))

(deftest the-checks-tell-at-rest-from-in-between
  (with-fake-pool (ledger :warmup 1)
    ;; INITIALIZE-POOL queued a replenish; nothing has run it.
    (ok (plusp (length (cl-mcp/specs/pool-fixtures:ledger-tasks ledger))))
    (testing "an acquire with the replenish still queued spawns on demand"
      (ok (typep (get-or-assign-worker "s0") 'worker))
      (ok (null (ownership-violations ledger))))
    (testing "an RPC-crashed bound worker is legal in between and at rest"
      (let ((worker (get-or-assign-worker "s0")))
        (cl-mcp/src/worker-client::%mark-worker-crashed worker "timeout")
        (ok (null (ownership-violations ledger)))
        (run-pending-work ledger)
        (ok (null (ownership-violations ledger :stable t)))))
    (testing "a placeholder is legal in between and not at rest"
      (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
        (setf (gethash "s9" cl-mcp/src/pool::*affinity-map*)
              (cl-mcp/src/pool::make-worker-placeholder :session-id "s9")))
      (ok (null (ownership-violations ledger)))
      (ok (member :placeholder-at-rest
                  (violation-kinds (ownership-violations ledger :stable t))))
      (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
        (remhash "s9" cl-mcp/src/pool::*affinity-map*)))))

(deftest the-checks-catch-a-pool-that-agrees-with-itself
  ;; Wrong pools whose lists stay consistent, so only a check made from
  ;; outside them -- the model's promises, the ledger's count -- finds them.
  (flet ((swapped (symbol replacement operations &rest options)
           (let ((original (fdefinition symbol)))
             (unwind-protect
                  (progn (setf (fdefinition symbol) replacement)
                         (%kinds* operations options))
               (setf (fdefinition symbol) original)))))
    (testing "an acquire that refuses a pool with room is not a refusal"
      (ok (member :unexpected-refusal
                  (swapped 'get-or-assign-worker
                           (lambda (session) (error "Refusing ~A." session))
                           '((:acquire "s0")) :warmup 0))))
    (let ((real (fdefinition 'get-or-assign-worker)))
      (testing "an acquire that ends the session's healthy worker broke affinity"
        ;; The acquire ended the worker itself, so afterwards it looks like
        ;; one there was no need to keep; the decision is made before.
        (ok (member :affinity-broken
                    (swapped 'get-or-assign-worker
                             (lambda (session)
                               (cl-mcp/src/pool:kill-session-worker session)
                               (funcall real session))
                             '((:acquire "s0") (:acquire "s0"))))))
      (testing "and one that then fails to spawn had nothing to refuse"
        (ok (member :unexpected-refusal
                    (swapped 'get-or-assign-worker
                             (lambda (session)
                               (cl-mcp/src/pool:kill-session-worker session)
                               (funcall real session))
                             '((:acquire "s0") (:fail-next-spawn) (:acquire "s0"))
                             :warmup 0)))))
    (testing "a release that keeps the worker as a standby did not end it"
      (ok (member :released-but-live
                  (swapped 'release-session
                           (lambda (session)
                             (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                               (let ((worker (gethash session
                                                      cl-mcp/src/pool::*affinity-map*)))
                                 (when (typep worker 'worker)
                                   (remhash session cl-mcp/src/pool::*affinity-map*)
                                   (setf (worker-state worker) :standby
                                         (cl-mcp/src/worker-client:worker-session-id
                                          worker)
                                         nil)
                                   (push worker cl-mcp/src/pool::*standby-workers*)))))
                           '((:acquire "s0") (:release "s0"))))))
    (testing "a pool that undercounts itself is over its cap by the ledger's count"
      (let ((kinds (swapped 'cl-mcp/src/pool::%effective-pool-size (lambda () 0)
                            '((:run-work) (:acquire "s0") (:run-work))
                            :warmup 2 :max-size 2)))
        (ok (member :over-capacity kinds))
        (ok (member :size-miscounted kinds))))
    (testing "and the real pool passes all three sequences"
      (ok (null (run-operation-sequence '((:acquire "s0")) :warmup 0)))
      (ok (null (run-operation-sequence '((:acquire "s0") (:release "s0")))))
      (ok (null (run-operation-sequence '((:run-work) (:acquire "s0") (:run-work))
                                        :warmup 2 :max-size 2))))))

;;; ------------------------------------------------------------------------
;;; The four faults fixed in 4A

(deftest a-standby-an-rpc-marked-crashed-is-not-lent
  ;; The project-root broadcast is an RPC to every standby; a timeout there
  ;; marks the standby crashed and closes its connection, and its process
  ;; lives on until the reaper gets to it.
  (ok (null (run-operation-sequence '((:run-work) (:rpc-crash 0) (:acquire "s0")))))
  (with-fake-pool (ledger :warmup 1)
    (run-pending-work ledger)
    (let ((standby (first (getf (pool-snapshot) :standby))))
      (cl-mcp/src/worker-client::%mark-worker-crashed standby "timeout")
      (let ((lent (get-or-assign-worker "s0")))
        (ok (not (eq lent standby)) "the session gets another worker")
        (ok (killed-p ledger standby) "and the crashed standby is ended")
        (ok (not (member standby (getf (pool-snapshot) :all))))))))

(deftest a-dead-standby-is-ended-not-dropped
  (ok (null (run-operation-sequence '((:run-work) (:die 0) (:acquire "s0")))))
  (with-fake-pool (ledger :warmup 1)
    (run-pending-work ledger)
    (let ((standby (first (getf (pool-snapshot) :standby))))
      (setf (gethash standby (cl-mcp/specs/pool-fixtures::ledger-dead ledger)) t)
      (get-or-assign-worker "s0")
      (ok (killed-p ledger standby))
      (ok (null (ownership-violations ledger))))))

(deftest a-dropped-standby-is-ended-even-when-the-pool-is-full
  ;; The capacity refusal used to be signalled inside the lock, after the
  ;; dead standby had left every list and before anything ended it.
  (with-fake-pool (ledger :warmup 1 :max-size 1)
    (run-pending-work ledger)
    (let ((standby (first (getf (pool-snapshot) :standby))))
      (setf (gethash standby (cl-mcp/specs/pool-fixtures::ledger-dead ledger)) t)
      ;; Room for exactly that one standby: dropping it frees the slot, so
      ;; the acquire spawns.  Take the slot first, so the refusal fires.
      (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
        (setf (gethash "s9" cl-mcp/src/pool::*affinity-map*)
              (cl-mcp/src/pool::make-worker-placeholder :session-id "s9")))
      (ok (handler-case (progn (get-or-assign-worker "s0") nil)
            (cl-mcp/src/pool:pool-capacity-exceeded () t))
          "the pool is full")
      (ok (killed-p ledger standby) "and the dead standby it dropped was ended")
      (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
        (remhash "s9" cl-mcp/src/pool::*affinity-map*))
      (ok (null (ownership-violations ledger))))))

(deftest a-crashed-standby-recovery-ends-leaves-the-standby-list
  ;; Found by the generated sequences: a standby whose process died, then
  ;; marked crashed by an RPC before recovery ran.  Recovery's :CRASHED arm
  ;; took it out of the tracked list and ended it, and left it on the standby
  ;; list -- ended, still offered, and counted when replenishing.
  (ok (null (run-operation-sequence
             '((:run-work) (:die 4) (:health-check) (:rpc-crash 2) (:run-work))
             :warmup 2 :max-size 2)))
  (with-fake-pool (ledger :warmup 1)
    (run-pending-work ledger)
    (let ((standby (first (getf (pool-snapshot) :standby))))
      (setf (gethash standby (cl-mcp/specs/pool-fixtures::ledger-dead ledger)) t)
      (cl-mcp/src/pool::%check-worker-health)
      (cl-mcp/src/worker-client::%mark-worker-crashed standby "timeout")
      (run-pending-work ledger)
      (ok (killed-p ledger standby))
      (ok (not (member standby (getf (pool-snapshot) :standby)))
          "and it is no longer offered as a standby"))))

(deftest a-worker-recovery-bound-comes-back-without-a-check
  ;; The other finding was the model's: recovery binds a replacement to the
  ;; session on its own, and the session's next acquire may return it without
  ;; a liveness check, as it returns any existing binding.  Counted as a new
  ;; lending, a replacement that died afterwards read as an unusable worker
  ;; newly lent.
  (ok (null (run-operation-sequence
             '((:acquire "s2") (:die 6) (:health-check) (:run-work) (:die 5)
               (:acquire "s2"))
             :warmup 2 :max-size 2))))

(defun %spawn-racing-shutdown (&key restart)
  "Run an on-demand spawn that completes after a shutdown -- and, with
RESTART, after a new pool was initialized -- and return (values LEDGER
OUTCOME WHILE-SPAWNING), where OUTCOME is what GET-OR-ASSIGN-WORKER did and
WHILE-SPAWNING the violations seen while the spawn was in flight.

The order is fixed with two semaphores: the spawn announces that it started
and then waits; the shutdown runs; only then is the spawn let finish."
  (let ((started (sb-thread:make-semaphore))
        (release (sb-thread:make-semaphore))
        (outcome nil)
        (while-spawning :not-observed)
        (kept nil))
    (with-fake-pool (ledger :warmup 0)
      (setf kept ledger)
      (setf cl-mcp/src/pool::*spawn-worker-function*
            (lambda ()
              (sb-thread:signal-semaphore started)
              (sb-thread:wait-on-semaphore release :timeout 30)
              (fake-spawn ledger)))
      (let ((thread (bt:make-thread
                     (lambda ()
                       (setf outcome
                             (handler-case (list :lent (get-or-assign-worker "s0"))
                               (error (condition) (list :refused condition)))))
                     :name "spawn-racing-shutdown")))
        (sb-thread:wait-on-semaphore started :timeout 30)
        ;; In flight: the placeholder holds the session and counts against
        ;; the cap, and nothing else is owed yet.
        (setf while-spawning (ownership-violations ledger))
        (shutdown-pool)
        (when restart (initialize-pool))
        (sb-thread:signal-semaphore release)
        (bt:join-thread thread)))
    (values kept outcome while-spawning)))

(deftest a-spawn-finishing-after-shutdown-is-ended-not-registered
  (testing "after a shutdown"
    (multiple-value-bind (ledger outcome while-spawning) (%spawn-racing-shutdown)
      (ok (null while-spawning) "nothing is owed while the spawn is in flight")
      (ok (eq :refused (first outcome)))
      (ok (typep (second outcome) 'pool-shutting-down)
          "the caller is told the pool shut down")
      (ok (= 1 (length (ledger-spawned ledger))))
      (ok (killed-p ledger (first (ledger-spawned ledger)))
          "the worker it produced was ended by the pool")
      (ok (null (ledger-reaped-by-fixture ledger))
          "and not left for the fixture to find")))
  (testing "after a shutdown and a new pool"
    (multiple-value-bind (ledger outcome) (%spawn-racing-shutdown :restart t)
      (ok (eq :refused (first outcome)))
      (ok (killed-p ledger (first (ledger-spawned ledger))))
      (ok (null (ledger-reaped-by-fixture ledger))
          "the new pool did not adopt a worker the old one started"))))

;;; ------------------------------------------------------------------------
;;; Real processes

(defun %process-gone-p (pid &key (within 10))
  "True when no process PID exists, not even as a zombie, within WITHIN
seconds."
  (loop repeat (* 10 within)
        unless (probe-file (format nil "/proc/~D/" pid)) return t
        do (sleep 0.1)
        finally (return nil)))

(defun %await-standby (&key (within 60))
  "Return a standby worker once the real pool has one, or NIL."
  (loop repeat (* 10 within)
        do (let ((standby (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                            (first cl-mcp/src/pool::*standby-workers*))))
             (when standby (return standby)))
           (sleep 0.1)))

(deftest real-workers-the-pool-ends-are-reaped
  (if (not (spawn-available-p))
      (skip "no ros/sbcl available to spawn a worker")
      ;; WITH-POOL binds the warmup to 0; the standby cases need one, so it is
      ;; bound again inside, where each replenish the pool schedules reads it.
      (with-pool ()
        (let ((cl-mcp/src/pool:*worker-pool-warmup* 1))
          (cl-mcp/src/pool::%schedule-replenish)
          (testing "a released session's process is gone"
            (let* ((worker (get-or-assign-worker "reap-release"))
                   (pid (worker-pid worker)))
              (release-session "reap-release")
              (ok (%process-gone-p pid))))
          (testing "a crashed standby is ended when a session passes it over"
            (let ((standby (%await-standby)))
              (ok standby)
              (when standby
                (let ((pid (worker-pid standby)))
                  (cl-mcp/src/worker-client::%mark-worker-crashed standby "timeout")
                  (let ((lent (get-or-assign-worker "reap-crashed")))
                    (ok (not (eq lent standby))))
                  (ok (%process-gone-p pid) "its still-running process was ended")))))
          (testing "a dead standby is ended when a session passes it over"
            (let ((standby (%await-standby)))
              (ok standby)
              (when standby
                (let ((pid (worker-pid standby)))
                  (sb-posix:kill pid sb-posix:sigkill)
                  (loop repeat 50
                        while (cl-mcp/src/pool::%worker-process-alive-p standby)
                        do (sleep 0.1))
                  (get-or-assign-worker "reap-dead")
                  (ok (eq :dead (worker-state standby)) "the pool ended it")
                  (ok (%process-gone-p pid))))))
          (testing "a shutdown leaves no process behind"
            (let ((pids (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                          (mapcar #'worker-pid cl-mcp/src/pool::*all-workers*))))
              (ok (plusp (length pids)))
              (shutdown-pool)
              (ok (every #'%process-gone-p pids))))))))
