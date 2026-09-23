;;;; tests/concurrency-test.lisp
;;;;
;;;; Fixed cases for the pool under operations that overlap in time, and for
;;;; waiting on a worker (Phase 4D): each shutdown ordering on its own, the
;;;; checks of specs/concurrency-fixtures.lisp against deliberately wrong
;;;; implementations, and one ordering per defect 4D fixed, held in place with
;;;; semaphores.  The generated properties are specs/concurrency.lisp.

(defpackage #:cl-mcp/tests/concurrency-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/specs/concurrency-fixtures
                #:+in-flight-kinds+
                #:run-shutdown-scenario
                #:shutdown-scenario-violations
                #:run-late-scenario
                #:+late-kinds+
                #:run-concurrent-plan
                #:concurrency-violation-kinds
                #:with-concurrent-pool
                #:ended-p)
  (:import-from #:cl-mcp/specs/pool-fixtures
                #:with-fake-pool
                #:run-pending-work)
  (:import-from #:cl-mcp/specs/request-fixtures
                #:with-fake-worker
                #:start-request
                #:request-result
                #:hold
                #:release
                #:await-received
                #:await-phase
                #:received-p
                #:fake-server-dropped-p
                #:result-text
                #:execution-status)
  (:import-from #:cl-mcp/src/pool
                #:get-or-assign-worker
                #:shutdown-pool
                #:initialize-pool
                #:pool-capacity-exceeded)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker
                #:cancel-request)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/src/worker-client
                #:worker-state))

(in-package #:cl-mcp/tests/concurrency-test)

(defmacro %ledger-value (accessor ledger)
  "Read ACCESSOR of the concurrency LEDGER under its lock."
  `(bt:with-lock-held ((cl-mcp/specs/concurrency-fixtures::ledger-lock ,ledger))
     (,accessor ,ledger)))

(defun %gate-spawns (ledger)
  "Gate every spawn from now on; return the gate."
  (cl-mcp/specs/concurrency-fixtures::%gate-spawns ledger))

(defun %await (predicate &key (within 10))
  "Wait until PREDICATE holds, up to WITHIN seconds."
  (cl-mcp/specs/concurrency-fixtures::%await predicate :within within))

(defun %spawns-begun (ledger)
  "How many spawns LEDGER saw begin."
  (cl-mcp/specs/concurrency-fixtures::%spawns-begun ledger))

(defun %mark-dead (ledger worker)
  "WORKER's fake process dies."
  (bt:with-lock-held ((cl-mcp/specs/concurrency-fixtures::ledger-lock ledger))
    (setf (gethash worker (cl-mcp/specs/concurrency-fixtures::ledger-dead ledger)) t)))

(defun %history (session)
  "SESSION's crash history, as the circuit breaker counts it."
  (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
    (copy-list (gethash session cl-mcp/src/pool::*crash-history*))))

;;; ------------------------------------------------------------------------
;;; Shutdown with each kind of work in flight

(deftest a-shutdown-leaves-nothing-behind-whatever-is-in-flight
  (dolist (kind +in-flight-kinds+)
    (let ((violations (shutdown-scenario-violations
                       (run-shutdown-scenario
                        (list :in-flight (list kind) :release-order (list kind)
                              :late-acquire t)))))
      (ok (null violations) (format nil "~(~A~)~@[: ~S~]" kind violations))))
  (testing "and with all of it in flight at once, in either order"
    (dolist (order (list +in-flight-kinds+ (reverse +in-flight-kinds+)))
      (let ((violations (shutdown-scenario-violations
                         (run-shutdown-scenario
                          (list :in-flight +in-flight-kinds+ :release-order order
                                :late-acquire t)))))
        (ok (null violations) (format nil "~S~@[: ~S~]" order violations))))))

;;; ------------------------------------------------------------------------
;;; The checks catch wrong implementations

(defun %with-replaced (symbol replacement thunk)
  "Call THUNK with SYMBOL's function replaced by REPLACEMENT."
  (let ((original (fdefinition symbol)))
    (unwind-protect
         (progn (setf (fdefinition symbol) replacement)
                (funcall thunk))
      (setf (fdefinition symbol) original))))

(defun %kinds-of-scenario (kinds)
  "The violation kinds of a shutdown scenario with KINDS in flight."
  (concurrency-violation-kinds
   (shutdown-scenario-violations
    (run-shutdown-scenario (list :in-flight kinds :release-order kinds
                                 :late-acquire nil)))))

(deftest the-shutdown-checks-catch-a-wrong-shutdown
  (testing "a shutdown that does not signal a worker waits behind its RPC"
    ;; What SHUTDOWN-POOL did before 4D: it ended each worker without the
    ;; signal RELEASE-SESSION sends, and ending one takes the stream the RPC
    ;; holds.
    (let ((cl-mcp/specs/concurrency-fixtures::*shutdown-bound-seconds* 2))
      (ok (member :shutdown-blocked
                  (%with-replaced 'cl-mcp/src/pool::%signal-worker
                                  (lambda (worker) (declare (ignore worker)) nil)
                                  (lambda () (%kinds-of-scenario '(:rpc-held))))))))
  (testing "a shutdown that does not wait returns before a spawn it owes"
    (ok (member :spawned-after-shutdown
                (%with-replaced 'cl-mcp/src/pool::%wait-for-work-in-flight
                                (lambda (seconds &optional generation)
                                  (declare (ignore seconds generation))
                                  t)
                                (lambda () (%kinds-of-scenario '(:acquire-spawn)))))))
  (testing "a worker taken out to be ended and not accounted for outlives it"
    (ok (intersection '(:ended-after-shutdown :live-after-shutdown)
                      (%with-replaced 'cl-mcp/src/pool::%begin-ending
                                      (lambda (worker) worker)
                                      (lambda () (%kinds-of-scenario '(:ending)))))))
  (testing "and the real pool passes each of those"
    (dolist (kinds '((:rpc-held) (:acquire-spawn) (:ending)))
      (ok (null (%kinds-of-scenario kinds)) (format nil "~S" kinds)))))

(deftest the-moment-checks-catch-a-worker-nobody-tracks
  ;; A worker taken out of the lists and not listed as ending is, for the
  ;; moment it takes to end it, a process nobody accounts for.
  (let ((plan (list :clients (list (loop repeat 20
                                         append '((:acquire "c0") (:release "c0")))
                                   (loop repeat 20
                                         append '((:acquire "c1") (:kill-session "c1"))))
                    :warmup 1 :max-size 4 :shutdown-after 80)))
    (ok (null (run-concurrent-plan plan)) "the real pool keeps track")
    ;; Endings slow enough for the observer to see one in progress.
    (ok (member :lost-track
                (%with-replaced 'cl-mcp/src/pool::%begin-ending
                                (lambda (worker) worker)
                                (lambda () (concurrency-violation-kinds
                                            (run-concurrent-plan
                                             (append plan '(:end-delay 0.02))))))))))

;;; ------------------------------------------------------------------------
;;; One ordering per defect

(deftest a-crash-is-counted-once-against-the-breaker
  ;; The recovery used to publish the worker :CRASHED in one critical section
  ;; and count the crash in a later one; an acquire in between counted it
  ;; too.  Held here with the recovery's replacement spawn gated, so the
  ;; acquire arrives while the recovery is under way.
  (with-concurrent-pool (ledger :warmup 0 :max-size 4)
    (let ((worker (get-or-assign-worker "s0"))
          (base (%spawns-begun ledger))
          (gate (%gate-spawns ledger)))
      (%mark-dead ledger worker)
      (cl-mcp/src/pool::%check-worker-health)
      (%await (lambda () (>= (%spawns-begun ledger) (1+ base))))
      (let ((acquire (bt:make-thread (lambda () (get-or-assign-worker "s0"))
                                     :name "breaker-acquire")))
        (sleep 0.1)
        (ok (= 1 (length (%history "s0"))) "counted once while the recovery runs")
        (sb-thread:signal-semaphore gate 10)
        (let ((replacement (bt:join-thread acquire)))
          (ok (not (eq worker replacement)) "the acquire gets the replacement")
          (ok (= (1+ base) (%spawns-begun ledger)) "and no second one is spawned for it"))
        (ok (= 1 (length (%history "s0"))) "and still once afterwards")))))

(deftest a-recovery-counts-against-the-cap-from-the-start
  ;; The recovery spawned its replacement with no placeholder and no count,
  ;; so an acquire for the same session spawned a second one beside it, and
  ;; the pool briefly held more than its cap.
  (with-concurrent-pool (ledger :warmup 0 :max-size 2)
    (let ((worker (get-or-assign-worker "s0")))
      (get-or-assign-worker "s1")
      (let ((base (%spawns-begun ledger))
            (gate (%gate-spawns ledger)))
        (%mark-dead ledger worker)
        (cl-mcp/src/pool::%check-worker-health)
        (%await (lambda () (>= (%spawns-begun ledger) (1+ base))))
        (ok (= 2 (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                   (cl-mcp/src/pool::%effective-pool-size)))
            "the replacement is counted while it spawns")
        (ok (handler-case (progn (get-or-assign-worker "s2") nil)
              (pool-capacity-exceeded () t))
            "so a third session is refused, not spawned for")
        (let ((same (bt:make-thread (lambda () (get-or-assign-worker "s0"))
                                    :name "cap-acquire")))
          (sleep 0.1)
          (ok (= (1+ base) (%spawns-begun ledger)) "the session's own acquire waits for it")
          (sb-thread:signal-semaphore gate 10)
          (ok (eq (bt:join-thread same)
                  (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                    (gethash "s0" cl-mcp/src/pool::*affinity-map*)))
              "and gets the replacement the recovery bound"))))))

(deftest a-surplus-standby-is-ended-outside-the-pool-lock
  ;; Replenishment ended a worker it could not register while holding
  ;; *POOL-LOCK* -- up to two seconds in which no session could be served.
  (with-concurrent-pool (ledger :warmup 0 :max-size 4)
    (let ((gate (%gate-spawns ledger)))
      (setf cl-mcp/src/pool:*worker-pool-warmup* 1)
      (cl-mcp/src/pool::%schedule-replenish)
      (%await (lambda () (>= (%spawns-begun ledger) 1)))
      ;; The spawn in flight becomes a surplus: it was started for a pool
      ;; generation that is no longer the running one.  (Not by lowering the
      ;; cap: the replenishment thread has bound the value it started with.)
      (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
        (setf cl-mcp/src/pool::*generation* (cl-mcp/src/pool::%make-generation -1)))
      (let ((ending (sb-thread:make-semaphore))
            (waited nil))
        ;; End it slowly, and time how long the pool lock takes meanwhile.
        (setf cl-mcp/src/pool::*kill-worker-function*
              (let ((real cl-mcp/src/pool::*kill-worker-function*))
                (lambda (worker)
                  (sb-thread:signal-semaphore ending)
                  (sleep 0.5)
                  (funcall real worker))))
        (sb-thread:signal-semaphore gate 10)
        (ok (sb-thread:wait-on-semaphore ending :timeout 10) "the surplus is being ended")
        (let ((start (get-internal-real-time)))
          (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*) nil)
          (setf waited (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))
        (ok (< waited 0.2) (format nil "the lock was free while it ended: ~,2Fs" waited))
        (%await (lambda ()
                  (let ((spawned (%ledger-value
                                  cl-mcp/specs/concurrency-fixtures::ledger-spawned ledger)))
                    (and spawned (ended-p ledger (first spawned))))))
        (ok (ended-p ledger (first (%ledger-value
                                    cl-mcp/specs/concurrency-fixtures::ledger-spawned
                                    ledger)))
            "and it was ended")))))

(deftest a-replenishment-does-not-feed-a-later-pool
  ;; A shutdown that gave up waiting for a replenishment's spawn, and a pool
  ;; initialized after it: the spawn completes into the new pool, which
  ;; must not take the old one's worker.
  (with-concurrent-pool (ledger :warmup 0 :max-size 4)
    (let ((gate (%gate-spawns ledger)))
      (setf cl-mcp/src/pool:*worker-pool-warmup* 1)
      (cl-mcp/src/pool::%schedule-replenish)
      (%await (lambda () (>= (%spawns-begun ledger) 1)))
      ;; Give the shutdown one second to wait, then start the next pool.
      (setf cl-mcp/src/worker-client::*worker-startup-timeout* -14
            cl-mcp/src/pool:*worker-pool-warmup* 0)
      (shutdown-pool)
      (initialize-pool)
      (sb-thread:signal-semaphore gate 10)
      (%await (lambda ()
                (let ((spawned (%ledger-value
                                cl-mcp/specs/concurrency-fixtures::ledger-spawned ledger)))
                  (and spawned (ended-p ledger (first spawned))))))
      (let ((stale (first (%ledger-value
                           cl-mcp/specs/concurrency-fixtures::ledger-spawned ledger))))
        (ok (ended-p ledger stale) "the old pool's replenishment ended its worker")
        (ok (not (member stale (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                                 (copy-list cl-mcp/src/pool::*all-workers*))))
            "and the new pool never took it")))))

(deftest a-shutdown-waits-for-a-recovery-no-longer-than-its-deadline
  ;; Found in review: the recovery threads were joined after the deadline,
  ;; without one, so a recovery stuck in its spawn held the shutdown for as
  ;; long as the spawn took -- forever, for one that never returned.
  (with-concurrent-pool (ledger :warmup 0 :max-size 4)
    (let ((worker (get-or-assign-worker "s0"))
          (base (%spawns-begun ledger))
          (gate (%gate-spawns ledger)))
      (%mark-dead ledger worker)
      (cl-mcp/src/pool::%check-worker-health)
      (%await (lambda () (>= (%spawns-begun ledger) (1+ base))))
      ;; A one-second deadline (startup timeout + 15).
      (setf cl-mcp/src/worker-client::*worker-startup-timeout* -14)
      (let ((start (get-internal-real-time)))
        (shutdown-pool)
        (let ((took (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
          (ok (< took 3) (format nil "it gave up at its deadline: ~,1Fs" took))))
      ;; The recovery ends its own worker once its spawn returns.
      (sb-thread:signal-semaphore gate 10)
      (ok (%await (lambda ()
                    (every (lambda (w) (ended-p ledger w))
                           (%ledger-value
                            cl-mcp/specs/concurrency-fixtures::ledger-spawned ledger))))
          "and the late replacement is ended by the recovery itself"))))

(deftest a-replenishment-decided-before-a-shutdown-is-waited-for
  ;; Found in review: the replenishment was decided under the lock and its
  ;; thread started and published after it, so a shutdown in between saw
  ;; neither a spawn nor a handle, returned, and the thread started after.
  ;; Now the start and the handle are in the deciding critical section:
  ;; here the start is held there, so the shutdown can only follow it.
  (with-concurrent-pool (ledger :warmup 0 :max-size 4)
    (let ((starting (sb-thread:make-semaphore))
          (go (sb-thread:make-semaphore))
          (real cl-mcp/src/pool::*start-pool-thread-function*)
          (returned-at nil))
      (setf cl-mcp/src/pool::*start-pool-thread-function*
            (lambda (thunk name)
              (when (equal name "pool-replenish")
                (sb-thread:signal-semaphore starting)
                (sb-thread:wait-on-semaphore go :timeout 10))
              (funcall real thunk name)))
      (let ((scheduler (bt:make-thread
                        (lambda ()
                          (let ((cl-mcp/src/pool:*worker-pool-warmup* 1))
                            (cl-mcp/src/pool::%schedule-replenish)))
                        :name "replenish-scheduler")))
        (ok (sb-thread:wait-on-semaphore starting :timeout 10)
            "the replenishment is decided and about to start")
        (let ((shutdown (bt:make-thread
                         (lambda () (shutdown-pool)
                           (setf returned-at (get-internal-real-time)))
                         :name "replenish-shutdown")))
          (sleep 0.1)
          (ok (null returned-at) "the shutdown cannot pass the decision")
          (sb-thread:signal-semaphore go)
          (bt:join-thread scheduler)
          (bt:join-thread shutdown)
          (ok (null (cl-mcp/specs/concurrency-fixtures::after-shutdown-violations
                     ledger returned-at))
              "and once it returns, the replenishment has come and gone"))))))

(deftest a-pool-left-owing-work-does-not-weigh-on-the-next
  ;; Found in review: the spawn count was one global, so a spawn an old pool's
  ;; shutdown gave up on counted against the next pool's cap -- with a cap of
  ;; one, the new pool refused every session until the spawn returned.  So
  ;; did the old replenishment's flag, which kept the new pool from
  ;; replenishing.
  (with-concurrent-pool (ledger :warmup 0 :max-size 1)
    (let ((gate (%gate-spawns ledger))
          (base (%spawns-begun ledger)))
      (let ((stuck (bt:make-thread (lambda ()
                                     (ignore-errors (get-or-assign-worker "s0")))
                                   :name "stuck-acquire")))
        (setf cl-mcp/src/pool:*worker-pool-warmup* 1)
        (cl-mcp/src/pool::%schedule-replenish)
        (%await (lambda () (>= (%spawns-begun ledger) (1+ base))))
        (setf cl-mcp/src/worker-client::*worker-startup-timeout* -14)
        (shutdown-pool)
        ;; New spawns are not gated; the old ones still wait on the gate
        ;; they took.
        (bt:with-lock-held ((cl-mcp/specs/concurrency-fixtures::ledger-lock ledger))
          (setf (cl-mcp/specs/concurrency-fixtures::ledger-spawn-gate ledger) nil))
        (setf cl-mcp/src/pool:*worker-pool-warmup* 0)
        (initialize-pool)
        (ok (typep (get-or-assign-worker "s1") 'cl-mcp/src/worker-client:worker)
            "the new pool lends its one worker, the old spawn notwithstanding")
        (sb-thread:signal-semaphore gate 10)
        (bt:join-thread stuck)
        (ok (%await (lambda ()
                      (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                        (zerop (cl-mcp/src/pool::generation-spawns
                                cl-mcp/src/pool::*generation*)))))
            "and the new pool's account never held the old spawn")))))

(deftest late-work-stays-with-its-generation
  ;; What a shutdown that gave up on its deadline leaves behind is the
  ;; stopped generation's: a new pool of one worker neither refuses for it
  ;; nor takes its worker in.
  (dolist (kind +late-kinds+)
    (dolist (warmup '(0 1))
      (let ((violations (run-late-scenario
                         (list :late kind :max-size 1 :warmup warmup))))
        (ok (null violations)
            (format nil "~(~A~), warmup ~D~@[: ~S~]" kind warmup violations)))))
  (testing "and the check catches one account carried across pools"
    (ok (intersection '(:new-pool-refused :late-worker-joined-new-pool
                        :new-account-holds-old-work)
                      (concurrency-violation-kinds
                       (%with-replaced 'cl-mcp/src/pool::%make-generation
                                       (lambda (id)
                                         (declare (ignore id))
                                         cl-mcp/src/pool::*generation*)
                                       (lambda ()
                                         (run-late-scenario
                                          (list :late :acquire-spawn :max-size 1
                                                :warmup 0)))))))))

;;; ------------------------------------------------------------------------
;;; Waiting for a worker

(defun %request-thread (id params)
  "Make request ID of session \"owner\" with PARAMS on a thread of its own."
  (bt:make-thread (lambda ()
                    (let ((*current-session-id* "owner"))
                      (proxy-to-worker id "worker/q" params)))
                  :name (format nil "wait-request-~A" id)))

(deftest a-cancelled-request-stops-waiting-at-once
  ;; A request queued behind another of its session's used to wait for the
  ;; stream however long the other ran, cancelled or not.
  (with-fake-worker (server worker)
    (hold server "worker/r")
    (let ((r (start-request "owner" 7 "worker/r")))
      (await-received server "worker/r")
      (let ((q (%request-thread 8 (make-ht "timeout_seconds" 60))))
        (await-phase "owner" 8 :waiting-to-send)
        (sleep 0.1)
        (let ((start (get-internal-real-time)))
          (cancel-request 8 "owner")
          (let ((result (bt:join-thread q))
                (took (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)))
            (ok (< took 1) (format nil "it returned at once: ~,2Fs" took))
            (ok (equal "not-executed" (execution-status result)))
            (ok (search "cancelled before it was sent" (result-text result)))
            (ok (not (received-p server "worker/q")) "it was never sent")
            (ok (not (fake-server-dropped-p server)) "and the worker was kept")))
        (release server "worker/r")
        (ok (not (gethash "isError" (request-result r)))
            "the request it waited behind still gets its answer")))))

(deftest a-request-waits-no-longer-than-its-deadline
  ;; The deadline used to cover only reading the answer; waiting behind
  ;; another request had none.
  (let ((saved cl-mcp/src/proxy::*proxy-rpc-buffer*))
    (setf cl-mcp/src/proxy::*proxy-rpc-buffer* 0)
    (unwind-protect
         (with-fake-worker (server worker)
           (hold server "worker/r")
           (let ((r (start-request "owner" 7 "worker/r")))
             (await-received server "worker/r")
             (let* ((start (get-internal-real-time))
                    (result (bt:join-thread
                             (%request-thread 8 (make-ht "timeout_seconds" 1))))
                    (took (/ (- (get-internal-real-time) start)
                             internal-time-units-per-second)))
               (ok (< 0.8 took 3) (format nil "it gave up after its deadline: ~,2Fs" took))
               (ok (equal "not-executed" (execution-status result)))
               (ok (search "still running another request" (result-text result)))
               (ok (not (received-p server "worker/q")))
               (ok (eq :bound (worker-state worker)) "the worker is left alone"))
             (release server "worker/r")
             (ok (not (gethash "isError" (request-result r))))))
      (setf cl-mcp/src/proxy::*proxy-rpc-buffer* saved))))

;;; ------------------------------------------------------------------------
;;; A full pool

(deftest a-full-pool-refuses-at-once
  ;; The policy: no queue.  An acquire that finds no room is refused
  ;; immediately, and the request it was for reports that it did not run.
  (with-fake-pool (ledger :warmup 0 :max-size 1)
    (declare (ignorable ledger))
    (get-or-assign-worker "s0")
    (let ((start (get-internal-real-time)))
      (ok (handler-case (progn (get-or-assign-worker "s1") nil)
            (pool-capacity-exceeded () t)))
      (ok (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 0.2)
          "at once"))
    (let ((result (let ((*current-session-id* "s1"))
                    (proxy-to-worker 1 "worker/eval" (make-ht)))))
      (ok (gethash "isError" result))
      (ok (search "Pool size limit reached" (result-text result)))
      (ok (equal "not-executed" (execution-status result))))
    (run-pending-work ledger)))
