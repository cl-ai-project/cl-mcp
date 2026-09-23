;;;; specs/request-lifecycle.lisp
;;;;
;;;; Properties of a proxied request's lifecycle (Phase 4B): what a request is
;;;; reported to have done, which request a cancellation acts on, and that
;;;; nothing runs twice.  Each trial runs the real PROXY-TO-WORKER,
;;;; WORKER-RPC and CANCEL-REQUEST over a real socket to a fake worker whose
;;;; ledger of received requests is the independent account of what ran
;;;; (specs/request-fixtures.lisp).
;;;;
;;;; A drawn scenario fixes what the worker does with the request under test
;;;; -- answers, answers with an error, drops the connection, or holds it --
;;;; where its cancellation arrives -- never, while its worker is found,
;;;; while it waits behind another request, while it runs, after its answer
;;;; was read and before it was delivered, after its answer, or from another
;;;; session -- and whether a second request waits behind it.  The orderings
;;;; are fixed, not raced.
;;;;
;;;; Verified domain: one session's requests on one worker, three at most,
;;;; in those orderings.  Not covered: requests that race each other or a
;;;; cancellation at arbitrary instants (4D), the proxy's deadline firing
;;;; (the fake worker's hold stands in for a slow worker, not a timed-out
;;;; one), a real worker process (tests/cancel-test.lisp has one), and the
;;;; reset notice after a stopped worker (4C).

(defpackage #:cl-mcp/specs/request-lifecycle
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker
                #:cancel-request)
  (:import-from #:cl-mcp/specs/request-fixtures
                #:run-scenario
                #:scenario-violations
                #:draw-scenario)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/request-lifecycle)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none.  A request's
lifecycle is a property of a run, not of one call."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(request-lifecycle-keeps-its-promises))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(request-scenario))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(request-scenario-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none.  Its fixed cases
are the Rove tests of tests/request-lifecycle-test.lisp."
  '())

(defun register-specifications ()
  "Install this file's generator, spec and property in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator request-scenario-generator ()
    "Draw a behavior, a cancellation point and a queued request (DRAW-SCENARIO)."
    (draw-scenario))
  (defspec request-scenario list (:generator request-scenario-generator))

  (defproperty request-lifecycle-keeps-its-promises
      ((scenario request-scenario))
    "Every request gets exactly one result and is sent at most once.  A
request the worker never received reports not-executed; one it received
never does; one it answered gets the answer, or completed for a worker error;
one whose connection was dropped mid-run reports execution-unknown.  A
cancellation acts on the request it names -- before the request reaches the
worker it only withdraws it, and the worker is kept; while the worker runs it
the worker is stopped, and a request waiting behind is told it did not run;
between reading the answer and delivering it the cancellation stands and the
answer is withheld, never both; after the answer, or from another session,
nothing changes -- and nothing is left registered."
    (:about proxy-to-worker cancel-request)
    (:kind :invariant)
    (:trials (:smoke 12 :normal 48))
    (null (scenario-violations (run-scenario scenario)))))
