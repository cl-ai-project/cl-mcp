;;;; specs/pool-ownership.lisp
;;;;
;;;; Properties of the worker pool's ownership (Phase 4A): which worker the pool
;;;; holds, lends, takes back and ends, over generated operation sequences.
;;;; The real pool runs every operation -- GET-OR-ASSIGN-WORKER,
;;;; RELEASE-SESSION, KILL-SESSION-WORKER, the health check, crash recovery,
;;;; replenishment, SHUTDOWN-POOL, INITIALIZE-POOL -- with workers that are not
;;;; processes and its background work queued until an operation runs it
;;;; (specs/pool-fixtures.lisp).
;;;;
;;;; A sequence passes when, after every operation, the pool breaks none of
;;;; the invariants that hold between operations, none of those that hold at
;;;; rest when nothing is queued, and none of the promises the reference
;;;; model records for that operation; and when, after the shutdown every
;;;; sequence ends with, the pool holds nothing and every worker it was handed
;;;; has been ended -- checked against the fake lifecycle's own ledger, before
;;;; the fixture's teardown, whose reaping would itself be reported.
;;;;
;;;; Verified domain: sequential operation sequences of up to 30 operations
;;;; over four sessions, with process deaths, RPC timeouts marking a worker
;;;; crashed, injected spawn failures, shutdowns and restarts, at two pool
;;;; sizings.  Not covered: operations that overlap in time (4D; the one
;;;; ordering that 4A fixes, a spawn completing after a shutdown, is a fixed
;;;; case in tests/pool-ownership-test.lisp), real processes (fixed cases
;;;; there too), the runtime-init owner, reset notifications and the circuit
;;;; breaker (4C).

(defpackage #:cl-mcp/specs/pool-ownership
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/pool
                #:get-or-assign-worker
                #:release-session
                #:kill-session-worker
                #:shutdown-pool)
  (:import-from #:cl-mcp/specs/pool-fixtures
                #:run-operation-sequence
                #:random-operation-sequence)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/pool-ownership)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none.  Ownership
is a property of operation sequences, not of one call."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(pool-ownership-holds-over-operation-sequences
    pool-ownership-holds-when-the-pool-is-full))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(pool-operations))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(pool-operations-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none.  Its fixed cases
are the Rove tests of tests/pool-ownership-test.lisp."
  '())

(defun register-specifications ()
  "Install this file's generator, spec and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator pool-operations-generator ()
    "Draw 5 to 30 pool operations (RANDOM-OPERATION-SEQUENCE)."
    (random-operation-sequence))
  (defspec pool-operations list (:generator pool-operations-generator))

  (defproperty pool-ownership-holds-over-operation-sequences
      ((operations pool-operations))
    "Every worker the pool was handed is, after every operation, held in
exactly one place or ended; a worker is lent to one session only, never newly
lent once known unusable, and the same one is returned to its session while
it is usable; a released or killed session's worker is ended; and a shutdown
leaves the pool holding nothing and every worker ended.  One standby, room
for four."
    (:about get-or-assign-worker release-session kill-session-worker shutdown-pool)
    (:kind :invariant)
    (:trials (:smoke 10 :normal 60))
    (null (run-operation-sequence operations :warmup 1 :max-size 4)))

  (defproperty pool-ownership-holds-when-the-pool-is-full
      ((operations pool-operations))
    "The same invariants with two standbys and room for two, so the standbys
fill the pool, an acquire has to take one or be refused, and a refusal must
not leave a worker it dropped unended."
    (:about get-or-assign-worker release-session kill-session-worker shutdown-pool)
    (:kind :invariant)
    (:trials (:smoke 10 :normal 60))
    (null (run-operation-sequence operations :warmup 2 :max-size 2))))
