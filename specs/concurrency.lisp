;;;; specs/concurrency.lisp
;;;;
;;;; Properties of the worker pool when operations overlap in time (Phase 4D):
;;;; what a shutdown leaves behind when the work in flight finishes within its
;;;; deadline, what work that outlives the deadline may and may not touch, and
;;;; what must hold at every moment while clients act at once.  Both run the
;;;; real pool with fake workers and real threads
;;;; (specs/concurrency-fixtures.lisp); both judge by the fake lifecycle's own
;;;; ledger of spawns, endings and threads, never by the pool's lists alone.
;;;;
;;;; Verified domain: shutdowns begun with any combination of an acquire's
;;;; spawn, a replenishment's, a recovery's, a worker being ended and an RPC
;;;; holding a worker's stream in flight, released in any order, with an
;;;; acquire arriving after; and two to four clients of up to 25 operations
;;;; -- acquisitions, releases, pool-kill-worker, process deaths, RPC
;;;; timeouts, health checks -- over six sessions at pool sizes of two to
;;;; five, ended by a shutdown while they run.  Not covered: real processes
;;;; (tests/pool-test.lisp has a shutdown behind a running request), requests
;;;; through the proxy (tests/concurrency-test.lisp has the waits), and the
;;;; runtime-init owner.
;;;;
;;;; The first two properties are deterministic: a seed replays the scenario
;;;; and the run.  The third is not: a seed replays the operations each client
;;;; applies, and the scheduler decides how they interleave, so a failure
;;;; found once may not recur on replay.  Its fixed cases are what pin the
;;;; orderings down.

(defpackage #:cl-mcp/specs/concurrency
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/pool
                #:initialize-pool
                #:shutdown-pool
                #:get-or-assign-worker
                #:release-session
                #:kill-session-worker)
  (:import-from #:cl-mcp/specs/concurrency-fixtures
                #:draw-shutdown-scenario
                #:run-shutdown-scenario
                #:shutdown-scenario-violations
                #:draw-late-scenario
                #:run-late-scenario
                #:random-concurrent-plan
                #:run-concurrent-plan)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/concurrency)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none.  Both
properties are about runs, not calls."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(pool-shutdown-leaves-nothing-behind
    pool-late-work-stays-with-its-generation
    pool-holds-while-operations-overlap))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(shutdown-scenario late-scenario concurrent-plan))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(shutdown-scenario-generator late-scenario-generator concurrent-plan-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none.  Its fixed cases
are the Rove tests of tests/concurrency-test.lisp."
  '())

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator shutdown-scenario-generator ()
    "Draw what is in flight when a shutdown begins (DRAW-SHUTDOWN-SCENARIO)."
    (draw-shutdown-scenario))
  (defspec shutdown-scenario list (:generator shutdown-scenario-generator))
  (defgenerator late-scenario-generator ()
    "Draw a spawn that outlives a shutdown's deadline (DRAW-LATE-SCENARIO)."
    (draw-late-scenario))
  (defspec late-scenario list (:generator late-scenario-generator))
  (defgenerator concurrent-plan-generator ()
    "Draw clients' operations and a pool sizing (RANDOM-CONCURRENT-PLAN)."
    (random-concurrent-plan))
  (defspec concurrent-plan list (:generator concurrent-plan-generator))

  (defproperty pool-shutdown-leaves-nothing-behind
      ((scenario shutdown-scenario))
    "When the work in flight at a shutdown finishes within the shutdown's
deadline, the shutdown returns promptly -- an RPC holding a worker's stream
included, which its signal lets go -- and once it has returned the pool owes
nothing: every worker it was handed is ended, no spawn or ending of its
completes afterwards, its lists and counts are empty, and no thread it
started runs on.  An acquire it overtook, or one made after it began, is
refused, never lent a worker.  Work that outlives the deadline is another
matter: see POOL-LATE-WORK-STAYS-WITH-ITS-GENERATION."
    ;; The surfaces the scenarios drive, and every function a negative
    ;; control breaks for this property.
    (:about shutdown-pool
            get-or-assign-worker
            release-session
            cl-mcp/src/pool::%spawn-and-bind
            cl-mcp/src/pool::%schedule-replenish
            cl-mcp/src/pool::%replenish-standbys
            cl-mcp/src/pool::%handle-worker-crash
            cl-mcp/src/pool::%signal-worker
            cl-mcp/src/pool::%wait-for-work-in-flight
            cl-mcp/src/pool::%begin-ending
            cl-mcp/src/pool::%end-worker)
    (:kind :invariant)
    (:trials (:smoke 10 :normal 40))
    (null (shutdown-scenario-violations (run-shutdown-scenario scenario))))

  (defproperty pool-late-work-stays-with-its-generation
      ((scenario late-scenario))
    "A spawn -- an acquire's, a replenishment's or a recovery's -- that outlives
a shutdown's deadline does not hold the shutdown past it, and stays on the
stopped pool generation's account: a pool initialized after it lends a
session a worker at once whatever its cap, keeps its standby, and never
counts or receives the late spawn's worker, which the work that spawned it
ends once it returns.  The new pool, shut down in turn, owes nothing."
    (:about initialize-pool
            shutdown-pool
            get-or-assign-worker
            cl-mcp/src/pool::%make-generation
            cl-mcp/src/pool::%effective-pool-size
            cl-mcp/src/pool::%spawn-and-bind
            cl-mcp/src/pool::%schedule-replenish
            cl-mcp/src/pool::%replenish-standbys
            cl-mcp/src/pool::%handle-worker-crash)
    (:kind :invariant)
    (:trials (:smoke 3 :normal 10))
    (null (run-late-scenario scenario)))

  (defproperty pool-holds-while-operations-overlap
      ((plan concurrent-plan))
    "While clients acquire, release, kill and crash workers at once, at every
moment no worker is held or tracked twice, none ended is held, the pool
counts no more than its cap, and every live worker is tracked, being
spawned or being ended; no worker is lent to two sessions; and a shutdown
made while they run leaves nothing behind.  The interleaving is the
scheduler's: a seed replays the operations, not the run."
    (:about get-or-assign-worker
            release-session
            kill-session-worker
            shutdown-pool
            cl-mcp/src/pool::%check-worker-health
            cl-mcp/src/pool::%handle-worker-crash
            cl-mcp/src/pool::%effective-pool-size)
    (:kind :invariant)
    (:trials (:smoke 5 :normal 20))
    (null (run-concurrent-plan plan))))
