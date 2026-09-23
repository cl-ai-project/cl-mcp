;;;; specs/reset-events.lisp
;;;;
;;;; Properties of state-loss events (Phase 4C): each worker a session loses
;;;; is told to that session exactly once, with why it ended; nothing else is
;;;; told; and an object id never outlives the image that issued it.
;;;;
;;;; The reset property runs the real pool with fake workers and the real
;;;; proxy with a fake socket over generated sequences of pool operations and
;;;; requests (specs/reset-fixtures.lisp).  Its judge is the harness's own
;;;; account of which worker each session lost and how, and what the
;;;; responses said -- never the reset ledger it is checking.
;;;;
;;;; Verified domain: sequential sequences of up to 30 operations over four
;;;; sessions -- acquisitions, releases, pool-kill-worker, process deaths, RPC
;;;; timeouts, requests that answer and requests whose worker dies under them,
;;;; health checks, recovery, spawn failures, shutdowns and restarts -- at
;;;; two pool sizings.  Not covered: operations that overlap in time (4D),
;;;; cancellations (the request-lifecycle property checks that a cancelled
;;;; worker is told once), retirements and real processes (fixed cases in
;;;; tests/worker-leaked-thread-test.lisp and tests/pool-test.lisp), and the
;;;; runtime-init owner.

(defpackage #:cl-mcp/specs/reset-events
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker)
  (:import-from #:cl-mcp/src/pool
                #:kill-session-worker)
  (:import-from #:cl-mcp/src/object-registry
                #:lookup-object)
  (:import-from #:cl-mcp/specs/reset-fixtures
                #:run-reset-sequence
                #:random-reset-sequence
                #:run-handle-sequence
                #:random-handle-sequence)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/reset-events)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none.  Telling a
reset is a property of a sequence of operations, not of one call."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(resets-are-told-exactly-once
    resets-are-told-exactly-once-when-the-pool-is-full
    object-ids-never-outlive-their-image))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(reset-operations handle-operations))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(reset-operations-generator handle-operations-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none.  Its fixed cases
are the Rove tests of tests/reset-events-test.lisp."
  '())

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator reset-operations-generator ()
    "Draw 5 to 30 pool operations and requests (RANDOM-RESET-SEQUENCE)."
    (random-reset-sequence))
  (defspec reset-operations list (:generator reset-operations-generator))
  (defgenerator handle-operations-generator ()
    "Draw 5 to 40 object-registry operations over two images
(RANDOM-HANDLE-SEQUENCE)."
    (random-handle-sequence))
  (defspec handle-operations list (:generator handle-operations-generator))

  (defproperty resets-are-told-exactly-once
      ((operations reset-operations))
    "Every worker that ends while bound to a session is told to that session
in exactly one response -- the request that met its end, the session's next
request, or the pool-kill-worker call that ended it -- with a cause the
operations actually applied to it, and never once the session was released
or the pool shut down first.  No worker is told that was not lost, and once
every session has made a request, nothing is left untold.  The pool's
ownership invariants hold throughout.  One standby, room for four."
    (:about proxy-to-worker kill-session-worker)
    (:kind :invariant)
    (:trials (:smoke 10 :normal 60))
    (null (run-reset-sequence operations :warmup 1 :max-size 4)))

  (defproperty resets-are-told-exactly-once-when-the-pool-is-full
      ((operations reset-operations))
    "The same, with two standbys and room for two: requests are refused for
want of room, and a refusal tells what the session is owed as well."
    (:about proxy-to-worker kill-session-worker)
    (:kind :invariant)
    (:trials (:smoke 10 :normal 60))
    (null (run-reset-sequence operations :warmup 2 :max-size 2)))

  (defproperty object-ids-never-outlive-their-image
      ((operations handle-operations))
    "An object id is found only in the registry that issued it, until that
registry is cleared; in a replacement image, or after a clear, it is refused
as stale -- never answered with another object."
    (:about lookup-object)
    (:kind :invariant)
    (:trials (:smoke 30 :normal 200))
    (null (run-handle-sequence operations))))
