# Worker Debugger Observe-and-Abort Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Convert a debugger-reaching condition in a pooled SBCL worker request
into a structured failure for that execution, preserving the worker for its next
request.

**Architecture:** A request-local SBCL debugger boundary observes
`SB-EXT:*INVOKE-DEBUGGER-HOOK*`, captures a pre-unwind diagnostic snapshot, and
uses only private same-thread `THROW` targets to return an internal error
outcome. The shared deadline utility propagates an explicit request policy to
its managed child threads and arbitrates debugger and deadline unwinds with one
state machine; existing tool-specific failure formats remain the response
boundary.

**Tech Stack:** SBCL 2.5.x, Common Lisp conditions/restarts, SB-THREAD and
Bordeaux Threads, ASDF package-inferred systems, Rove, Yason.

**Spec:** [docs/superpowers/specs/2026-09-21-debugger-observe-and-abort-design.md](../specs/2026-09-21-debugger-observe-and-abort-design.md)

## Global Constraints

- Bind `SB-EXT:*INVOKE-DEBUGGER-HOOK*` only in an authenticated request's
  dynamic extent; do not globally replace hooks or call `ENABLE-DEBUGGER`.
- Do not wrap normal user execution in a `CONDITION` handler. `SIGNAL`,
  warnings, `MUFFLE-WARNING`, handled errors, and restart recovery keep their
  current semantics unless they actually reach the debugger path.
- The hook performs a private same-thread `THROW`. It never signals an internal
  `ERROR` and never selects `ABORT`, `CONTINUE`, or another user restart.
- The only cross-thread value is the boolean request-active policy. Each
  cl-mcp-managed deadline child creates its own tags, hook closure, state, and
  diagnostic snapshot, and dynamically rebinds the policy for nested children.
- Preserve `CALL-WITH-DEADLINE-THREAD`'s public three-value contract and its
  completion-wins, cleanup, destroy, and leaked-thread behavior.
- Use the approved exit-state ordering: controlled deadline unwind wins over
  debugger reentry during its cleanup; a deadline arriving during a pending
  debugger cleanup also wins and exits through the outer terminal tag.
- MCP cancellation remains `SIGTERM` plus `KILL-WORKER` and reap. It is not a
  cooperative deadline outcome and is outside the worker-survival guarantee.
- Snapshot original type/message, frames, locals when available, and restarts
  before unwind. Restarts are diagnostic snapshots, not callable later.
- Diagnostic collection must use a capture-specific `HANDLER-BIND` and
  secondary invoke-debugger hook. It must not reuse inner
  `HANDLER-CASE`/`IGNORE-ERRORS` fallbacks around a user-controlled report,
  printer, or inspector.
- Do not add an MCP tool, public debugger policy, JSON-RPC error code, live
  debugger session, restart invocation API, or rollback guarantee.
- SBCL-only hook behavior is conditionalized; non-SBCL retains the prior
  behavior. `SB-EXT:EXIT`, FFI crashes, forced process termination, and
  arbitrary user-created threads remain outside this change.
- Follow the repository rules: 2-space Common Lisp indentation, lines at most
  100 columns, docstrings on exported APIs, structural Lisp edits, and Rove
  tests registered in `tests.lisp`.

## Review Focus

- A direct `CONDITION` or `SIMPLE-CONDITION` passed to `ERROR` must produce an
  immediate structured worker/eval failure, retain its PID and state, and allow
  another eval; Task 5 pins this with fresh pooled workers.
- A diagnostic report that signals normally or directly invokes the debugger
  must retain the original safe type/message without recursion or a worker
  exit; Tasks 1 and 2 pin both paths.
- A deadline arriving while debugger cleanup is active must return timeout
  through the terminal tag, not a debugger error or a
  `thread-exited-without-result` failure; Task 3 uses semaphores to force it.
- Existing condition semantics and normal multiple values must stay intact,
  including nested deadline policy propagation; Tasks 2 and 3 pin signal,
  warning, handler/restart, zero-value, `NIL`-value, and nested-child cases.
- A real `SB-EXT:EXIT` and MCP cancellation must still take crash/cancel and
  reaper paths rather than being converted into a request-local rescue; Tasks
  5 and 6 pin those controls.

---

## File Structure

**Create**

| File | Responsibility |
|---|---|
| `src/utils/request-debugger-boundary.lisp` | SBCL-specific request-local debugger boundary, private state machine, safe internal error object, and deadline-interrupt selector. |
| `tests/utils-request-debugger-boundary-test.lisp` | Unit and controlled-unwind tests for the boundary and its state machine. |
| `tests/debugger-boundary-worker-test.lisp` | Fresh pooled-worker acceptance tests, including lifecycle controls. |
| `tests/debugger-boundary-run-tests-fixture.lisp` | An intentionally failing Rove fixture loaded only by the run-tests acceptance test. |

**Modify**

| File | Change |
|---|---|
| `src/frame-inspector.lisp` | Add a capture-specific, handler-bind-safe diagnostic entrypoint while preserving ordinary `capture-error-context` behavior. |
| `src/utils/deadline.lisp` | Propagate request-active into managed deadline children and publish debugger/deadline outcomes under the approved state table. |
| `src/worker/server.lisp` | Establish the request policy and direct dispatch boundary, then map its safe internal error through the existing `-32603` path. |
| `src/repl-core.lisp` | Use the saved pre-unwind snapshot for repl-eval's existing five-value error result. |
| `src/system-loader-core.lisp` | Render a debugger-escape error through saved text rather than a condition printer. |
| `src/tools/spec-entry.lisp` | Render the existing internal-error response with saved text for a debugger-escape error. |
| `src/spec-adapter-report.lisp` | Preserve original condition type/message in adapter error data instead of exposing the internal wrapper type. |
| `tests/frame-inspector-test.lisp` | Exercise normal and degraded capture behavior. |
| `tests/test-runner-deadline-test.lisp` | Cover deadline state-machine races and nested managed deadline children. |
| `tests/repl-error-context-test.lisp` | Assert repl-eval receives the pre-unwind `error_context` snapshot. |
| `tests/worker-test.lisp` | Exercise the deadline-free dispatch boundary and its existing JSON-RPC error route. |
| `tests/system-loader-test.lisp` | Assert load failure presentation uses saved text. |
| `tests/spec-adapter-report-test.lisp` | Assert adapter condition data preserves original type/message. |
| `tests/spec-tools-test.lisp` | Assert spec-entry's existing internal-error record renders saved debugger text. |
| `tests.lisp` | Register the two ordinary test packages, but not the intentionally failing run-tests fixture. |
| `README.md` | Explain request-level observe-and-abort versus a genuine worker crash. |
| `docs/tools.md` | Document snapshot-only restarts and the absence of rollback. |

**Read-only verification targets**

| File | Why it is checked |
|---|---|
| `src/worker/handlers.lisp` | The existing `run-tests :error` branch re-signals the safe internal error only after the hook/catch has exited. |
| `src/test-runner-core.lisp` | `%ensure-system-loaded`, the Rove selected-test path, and the FiveAM path retain their ERROR boundaries; Task 5 exercises the unhandled path through `worker/run-tests`. |
| `src/worker/main.lisp` | `DISABLE-DEBUGGER` remains the fallback policy outside a request boundary. |
| `src/proxy.lisp` | `cancel-request` remains the separate `SIGTERM`, `kill-worker`, and reap path. |

### Task 1: Add boundary-aware diagnostic capture

**Files:**
- Modify: `src/frame-inspector.lisp`
- Modify: `tests/frame-inspector-test.lisp`

**Interfaces:**
- Produces
  `(capture-debugger-error-context condition on-diagnostic-condition
     &key max-frames print-level print-length locals-preview-frames
          preview-max-depth preview-max-elements locals-preview-skip-internal
          filter-internal)`
  -> the existing error-context plist shape.
- `ON-DIAGNOSTIC-CONDITION` is a one-argument function. In production it must
  perform a non-local exit; this function does not treat a normal return from
  it as recovery.
- Keeps `(capture-error-context condition &key ...)` unchanged for all
  ordinary callers.

- [ ] **Step 1: Write the failing diagnostic-capture tests**

Add these definitions and tests to `tests/frame-inspector-test.lisp`. The first
test proves that the new entrypoint preserves the existing data shape. The
second proves that a condition printer's ordinary `ERROR` reaches the supplied
`HANDLER-BIND` callback rather than an inner fallback.

~~~lisp
(define-condition diagnostic-capture-report-error (condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition stream))
     (error "diagnostic report failed"))))

(deftest capture-debugger-error-context-keeps-live-restarts
  (let ((context
          (handler-case
              (restart-case
                  (error "snapshot source")
                (retry-snapshot () :report "Retry the snapshot" nil))
            (error (condition)
              (capture-debugger-error-context
               condition
               (lambda (secondary)
                 (declare (ignore secondary))
                 (error "unexpected diagnostic callback")))))))
    (ok (getf context :error))
    (ok (search "SIMPLE-ERROR" (getf context :condition-type)))
    (ok (search "snapshot source" (getf context :message)))
    (ok (find "RETRY-SNAPSHOT" (getf context :restarts)
              :key (lambda (restart) (getf restart :name))
              :test #'search))))

(deftest capture-debugger-error-context-transfers-on-secondary-condition
  (let ((tag (list :diagnostic-transfer)))
    (handler-case
        (error 'diagnostic-capture-report-error)
      (condition (original)
        (let ((result
                (catch tag
                  (capture-debugger-error-context
                   original
                   (lambda (secondary)
                     (throw tag (list :secondary (type-of secondary))))))))
          (ok (equal '(:secondary simple-error) result)
              "the report's ERROR reaches the caller's handler-bind exit"))))))
~~~

- [ ] **Step 2: Run the focused test to verify it fails**

Run: `run-tests system=cl-mcp/tests/frame-inspector-test`

Expected: FAIL while `CL-MCP/SRC/FRAME-INSPECTOR:CAPTURE-DEBUGGER-ERROR-CONTEXT`
does not exist.

- [ ] **Step 3: Implement the capture-specific entrypoint**

In `src/frame-inspector.lisp`, export the new entrypoint and add separate
unprotected collector variants. Keep the existing fallback-based functions for
ordinary `capture-error-context`; the debugger path must call the variants
below instead.

~~~lisp
(defun %debugger-condition-type-name (condition)
  "Return CONDITION's type text while the diagnostic handler-bind is active."
  (let ((type (type-of condition)))
    (if (symbolp type)
        (symbol-name type)
        (prin1-to-string type))))

(defun capture-debugger-error-context
    (condition on-diagnostic-condition
     &key (max-frames 20) (print-level 3) (print-length 10)
          (locals-preview-frames 0) (preview-max-depth 1)
          (preview-max-elements 5) (locals-preview-skip-internal t)
          (filter-internal nil))
  "Capture CONDITION while every secondary condition transfers through
ON-DIAGNOSTIC-CONDITION.

This is for the request debugger hook only. ON-DIAGNOSTIC-CONDITION is
expected to escape; it is deliberately not an ordinary recovery callback."
  (handler-bind
      ((condition on-diagnostic-condition))
    (list :error t
          :condition-type (%debugger-condition-type-name condition)
          :message (princ-to-string condition)
          :restarts (%collect-restarts-for-debugger)
          :frames (%collect-frames-for-debugger
                   max-frames print-level print-length
                   :locals-preview-frames locals-preview-frames
                   :preview-max-depth preview-max-depth
                   :preview-max-elements preview-max-elements
                   :locals-preview-skip-internal locals-preview-skip-internal
                   :filter-internal filter-internal))))
~~~

Implement `%collect-restarts-for-debugger`, `%collect-frames-for-debugger`,
and their frame/local/source helpers without `handler-case` or `ignore-errors`
around report text, restart descriptions, local printers, or SB-DI inspection.
They may reuse the same SB-DI primitives and output plist shape as the existing
collectors. A secondary condition must remain dynamically visible to the outer
`HANDLER-BIND`. Do not alter the existing fallback behavior of
`capture-error-context`.

- [ ] **Step 4: Run capture regressions**

Run: `run-tests system=cl-mcp/tests/frame-inspector-test`

Expected: PASS. Existing `capture-error-context` tests still tolerate
unavailable frame information; the new tests prove the debugger-only entrypoint
instead transfers immediately on a secondary condition.

- [ ] **Step 5: Check source structure and commit**

Run: `lisp-check-parens path=src/frame-inspector.lisp`

Expected: balanced parentheses.

~~~bash
git add src/frame-inspector.lisp tests/frame-inspector-test.lisp
git commit -m "frame-inspector: add debugger-safe diagnostic capture"
~~~

### Task 2: Establish the request-local debugger boundary

**Files:**
- Create: `src/utils/request-debugger-boundary.lisp`
- Create: `tests/utils-request-debugger-boundary-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes `capture-debugger-error-context` from Task 1.
- Produces:
  - `*request-debugger-boundary-active*`, a boolean policy special; callers
    bind it to true only for authenticated request execution.
  - `(call-with-request-debugger-boundary thunk)` -> one
    `request-debugger-result`.
  - `request-debugger-result-status` -> `:ok`, `:debugger`, or `:timeout`;
    `request-debugger-result-values` is a multiple-value list only for `:ok`;
    `request-debugger-result-error` is a
    `request-debugger-escape-error` only for `:debugger`.
  - `request-debugger-escape-error-p`,
    `request-debugger-escape-error-context`, and
    `request-debugger-escape-error-display-text`. The display text uses only
    saved original type/message.
  - `(request-debugger-deadline-interrupt deadline-tag deadline-marker)`,
    called only by Task 3's interrupt closure. It either transfers to a valid
    private tag or returns only when a deadline unwind is already in progress.
- The module keeps its context struct, tags, primary hook, secondary hook, and
  state selector private. Tests may inspect
  `cl-mcp/src/utils/request-debugger-boundary::*request-debugger-context*`
  solely to prove managed children do not reuse a context.

- [ ] **Step 1: Write failing boundary tests**

Create `tests/utils-request-debugger-boundary-test.lisp` with direct conditions
that deliberately do not inherit `ERROR` or `SERIOUS-CONDITION`.

~~~lisp
(define-condition boundary-direct-condition (condition) ())
(define-condition boundary-simple-condition (simple-condition) ())

(defun %boundary-result (thunk)
  (let ((cl-mcp/src/utils/request-debugger-boundary:*request-debugger-boundary-active*
          t))
    (cl-mcp/src/utils/request-debugger-boundary:call-with-request-debugger-boundary
     thunk)))

(deftest direct-condition-becomes-a-debugger-outcome
  (let ((result (%boundary-result
                 (lambda () (error 'boundary-direct-condition)))))
    (ok (eq :debugger
            (cl-mcp/src/utils/request-debugger-boundary:request-debugger-result-status
             result)))
    (let ((escape
            (cl-mcp/src/utils/request-debugger-boundary:request-debugger-result-error
             result)))
      (ok (cl-mcp/src/utils/request-debugger-boundary:request-debugger-escape-error-p
           escape))
      (ok (search "BOUNDARY-DIRECT-CONDITION"
                  (getf (cl-mcp/src/utils/request-debugger-boundary:request-debugger-escape-error-context
                         escape)
                        :condition-type))))))

(deftest simple-condition-becomes-a-debugger-outcome
  (let ((result (%boundary-result
                 (lambda () (error 'boundary-simple-condition)))))
    (ok (eq :debugger
            (cl-mcp/src/utils/request-debugger-boundary:request-debugger-result-status
             result)))
    (ok (search "BOUNDARY-SIMPLE-CONDITION"
                (getf
                 (cl-mcp/src/utils/request-debugger-boundary:request-debugger-escape-error-context
                  (cl-mcp/src/utils/request-debugger-boundary:request-debugger-result-error
                   result))
                 :condition-type)))))

(deftest ordinary-condition-control-flow-is-not-intercepted
  (flet ((values-of (thunk)
           (cl-mcp/src/utils/request-debugger-boundary:request-debugger-result-values
            (%boundary-result thunk))))
    (ok (equal '(:signalled)
               (values-of (lambda ()
                            (signal 'boundary-direct-condition)
                            :signalled))))
    (ok (equal '(:muffled)
               (values-of (lambda ()
                            (handler-bind ((warning #'muffle-warning))
                              (warn "keep warning semantics")
                              :muffled))))
    (ok (equal '(:handled)
               (values-of (lambda ()
                            (handler-case (error "handled")
                              (error () :handled))))))))

(deftest normal-multiple-values-are-not-escape-markers
  (flet ((values-of (thunk)
           (cl-mcp/src/utils/request-debugger-boundary:request-debugger-result-values
            (%boundary-result thunk))))
    (ok (null (values-of (lambda () (values)))))
    (ok (equal '(nil) (values-of (lambda () (values nil)))))
    (ok (equal '(:one :two) (values-of (lambda () (values :one :two)))))))
~~~

Add two more tests in this file:

1. `handler-case` specialized on `ERROR` around
   `(error 'boundary-direct-condition)` must not turn the boundary escape into
   `:handled`, because the boundary hook does not signal a wrapper through user
   handlers.
2. A condition whose `:report` directly calls `invoke-debugger` must return a
   `:debugger` outcome with the original condition's safe type and a degraded
   record, rather than recursively entering the disabled debugger.
3. A `handler-bind` handler that returns normally after seeing
   `boundary-direct-condition` must still produce `:debugger`; returning
   declines the condition rather than handling it.
4. With `*request-debugger-boundary-active*` false, an ordinary thunk returns
   `:ok` and observes
   `cl-mcp/src/utils/request-debugger-boundary::*request-debugger-context*` as
   `NIL`, proving request-external deadline callers do not acquire a boundary.
5. A handler that invokes a user restart returns `:recovered` as an `:ok`
   result, proving the boundary does not preempt recovery that completes before
   debugger entry.

- [ ] **Step 2: Register and run the failing test**

Add this import to `tests.lisp` next to the other utility test packages:

~~~lisp
  (:import-from #:cl-mcp/tests/utils-request-debugger-boundary-test)
~~~

Run: `run-tests system=cl-mcp/tests/utils-request-debugger-boundary-test`

Expected: FAIL because the request-debugger-boundary package and its exported
entrypoints do not yet exist.

- [ ] **Step 3: Implement the boundary module**

Create `src/utils/request-debugger-boundary.lisp` with this public surface:

~~~lisp
(defpackage #:cl-mcp/src/utils/request-debugger-boundary
  (:use #:cl)
  (:import-from #:cl-mcp/src/frame-inspector
                #:capture-debugger-error-context)
  (:export #:*request-debugger-boundary-active*
           #:call-with-request-debugger-boundary
           #:request-debugger-result-status
           #:request-debugger-result-values
           #:request-debugger-result-error
           #:request-debugger-deadline-interrupt
           #:request-debugger-escape-error-p
           #:request-debugger-escape-error-context
           #:request-debugger-escape-error-display-text))
~~~

Implement the private context with `:running`, `:debugger-unwinding`, and
`:deadline-unwinding` states, an outer terminal tag, a debugger tag, and a
pending minimal-or-full record. `call-with-request-debugger-boundary` uses this
fixed layout when the policy is true:

~~~text
terminal tag
  request-local SB-EXT:*INVOKE-DEBUGGER-HOOK*
    debugger tag
      THUNK
~~~

Its primary hook first constructs a non-printing minimal record with a fixed
fallback. It then dynamically installs the secondary invoke-debugger hook and
the Task 1 diagnostic callback before rendering the original condition or
walking frames/restarts. The state selector itself runs only under
`SB-SYS:WITHOUT-INTERRUPTS`:

~~~lisp
(case (%context-state context)
  (:running
   (setf (%context-state context) :debugger-unwinding
         (%context-pending context) record)
   :debugger)
  ((:debugger-unwinding :deadline-unwinding) :terminal))
~~~

The primary hook performs its selected `THROW` after that small protected
region. The secondary hook and Task 1 callback never retry capture, print a
condition, or invoke a restart; while `:running` they mark the existing minimal
record degraded and select the debugger tag, and otherwise select the terminal
tag. Both hook functions ignore SBCL's second argument: it is the active hook
value before SBCL temporarily binds that variable to `NIL`, not a callable
handle to the disabled debugger policy. The terminal catch materializes
`request-debugger-escape-error` only after the hook/catch escape. Its `:report`
must use only saved strings:

~~~lisp
(:report
 (lambda (condition stream)
   (format stream "~A: ~A"
           (slot-value condition 'condition-type)
           (slot-value condition 'message))))
~~~

On a non-SBCL implementation, return an `:ok` result containing
`(multiple-value-list (funcall thunk))`; do not introduce a substitute
condition handler.

- [ ] **Step 4: Run boundary and diagnostic tests**

Run: `run-tests system=cl-mcp/tests/utils-request-debugger-boundary-test`

Expected: PASS. A direct `CONDITION`, a direct `SIMPLE-CONDITION`, explicit
`INVOKE-DEBUGGER`, and diagnostic reentry produce debugger outcomes; `signal`,
warning, handled `ERROR`, restart recovery, and all normal multiple-value forms
remain ordinary returns.

Run: `run-tests system=cl-mcp/tests/frame-inspector-test`

Expected: PASS.

- [ ] **Step 5: Check source structure and commit**

Run: `lisp-check-parens path=src/utils/request-debugger-boundary.lisp`

Expected: balanced parentheses.

~~~bash
git add src/utils/request-debugger-boundary.lisp \
  tests/utils-request-debugger-boundary-test.lisp tests.lisp
git commit -m "worker: add request-local debugger boundary"
~~~

### Task 3: Integrate controlled deadline unwind arbitration

**Files:**
- Modify: `src/utils/deadline.lisp`
- Modify: `tests/utils-request-debugger-boundary-test.lisp`
- Modify: `tests/test-runner-deadline-test.lisp`

**Interfaces:**
- Consumes all Task 2 boundary interfaces.
- Preserves `(call-with-deadline-thread thunk timeout-seconds &key name)` ->
  `(values result status leaked)`, where `status` remains `:ok`, `:timeout`,
  or `:error`.
- When request policy is true, `:error` may carry a
  `request-debugger-escape-error`; normal serious-condition behavior remains
  unchanged.

- [ ] **Step 1: Write failing deadline-policy and race tests**

Add a request-active test to `tests/test-runner-deadline-test.lisp`:

~~~lisp
(define-condition deadline-direct-condition (condition) ())

(deftest deadline-thread-converts-a-debugger-escape-to-error
  (let ((cl-mcp/src/utils/request-debugger-boundary:*request-debugger-boundary-active*
          t))
    (multiple-value-bind (result status leaked)
        (cl-mcp/src/utils/deadline:call-with-deadline-thread
         (lambda () (error 'deadline-direct-condition))
         2 :name "deadline-debugger-test")
      (ok (eq :error status))
      (ok (not leaked))
      (ok (cl-mcp/src/utils/request-debugger-boundary:request-debugger-escape-error-p
           result)))))
~~~

Add a nested-child test in
`tests/utils-request-debugger-boundary-test.lisp`. It records the private
context objects inside an outer and an inner managed deadline child, asserts
both are non-NIL and not `EQ`, then makes the inner child raise a direct
`CONDITION`. The inner call must return `:error`; the outer call must remain a
normal `:ok` result that carries that inner status.

Add the following semaphore-controlled test to the same file. It confirms that
cleanup has started before the deadline acts; it is not a sleep-based race.

~~~lisp
(deftest deadline-during-debugger-cleanup-prefers-timeout
  (let ((cleanup-started (bt:make-semaphore))
        (answer-ready (bt:make-semaphore))
        (answer nil))
    (let ((caller
            (bt:make-thread
             (lambda ()
               (let ((cl-mcp/src/utils/request-debugger-boundary:*request-debugger-boundary-active*
                       t))
                 (setf answer
                       (multiple-value-list
                        (cl-mcp/src/utils/deadline:call-with-deadline-thread
                         (lambda ()
                           (unwind-protect
                               (error 'deadline-direct-condition)
                             (bt:signal-semaphore cleanup-started)
                             (bt:wait-on-semaphore (bt:make-semaphore))))
                         0.25 :name "debugger-cleanup-race"))))
               (bt:signal-semaphore answer-ready))
             :name "deadline-debugger-race")))
      (unwind-protect
           (progn
             (ok (bt:wait-on-semaphore cleanup-started :timeout 2)
                 "debugger escape entered user cleanup before deadline")
             (ok (bt:wait-on-semaphore answer-ready :timeout 5)
                 "deadline terminates through the outer terminal tag")
             (ok (eq :timeout (second answer)))
             (ok (not (third answer))
                 "the controlled timeout did not leak the deadline child"))
        (when (bt:thread-alive-p caller)
          (bt:destroy-thread caller))))))
~~~

Use two additional fixtures whose condition reports wait on a semaphore and
whose report cleanup respectively calls `invoke-debugger` and signals an
ordinary direct `CONDITION`. Run each under the same deadline pattern. Both
tests must finish with `:timeout`, not replace the pending timeout with a
debugger record.

- [ ] **Step 2: Run the focused deadline test to verify it fails**

Run: `run-tests system=cl-mcp/tests/test-runner-deadline-test`

Expected: FAIL because a request-active deadline child still has no boundary
outcome and the direct `CONDITION` follows the old debugger path.

- [ ] **Step 3: Refactor deadline child execution around the boundary**

Capture the boolean policy before `MAKE-THREAD` and dynamically rebind it in
the child. Only the true branch wraps the child run in
`call-with-request-debugger-boundary`; no-policy calls retain the current
inline and deadline behavior.

The true branch must establish the exact dynamic nesting required by the
specification:

~~~text
terminal tag
  request-local SB-EXT:*INVOKE-DEBUGGER-HOOK*
    debugger tag
      deadline tag
        user thunk
~~~

Use a private marker object for the inner deadline `CATCH`, not a user return
value. Publish all terminal results under the existing
`SB-SYS:WITHOUT-INTERRUPTS` result-publication region:

~~~lisp
(case (request-debugger-result-status boundary-result)
  (:ok
   ;; The normal run stored its multiple-value list in OUTCOME.
   nil)
  (:debugger
   (setf outcome
         (cons :error (request-debugger-result-error boundary-result))))
  (:timeout
   (setf outcome (cons :timeout timeout-seconds))))
~~~

Replace the interrupt closure's unconditional private `THROW` with
`request-debugger-deadline-interrupt`. Its state-table behavior is fixed:

~~~text
running             -> record timeout, THROW the inner deadline tag
debugger-unwinding  -> replace pending debugger record with timeout,
                       THROW the outer terminal tag
deadline-unwinding  -> preserve timeout and start no new transfer
no active context   -> retain the existing guarded deadline THROW
~~~

The primary/secondary diagnostic selector in Task 2 must consult this same
state. During deadline unwind it throws only to the terminal tag. During a
debugger unwind, a later deadline records timeout then throws only to the
terminal tag. Do not disable interrupts across diagnostics or user cleanup.
Retain the existing `ignore-errors` guard only for a context already gone
after an outer non-local exit; it is not the state-machine mechanism.

- [ ] **Step 4: Run deadline regressions**

Run: `run-tests system=cl-mcp/tests/test-runner-deadline-test`

Expected: PASS, including ordinary timeout, completion-wins, own
`SB-EXT:TIMEOUT` as error, leaked-thread accounting, the new debugger
conversion, and both cleanup races.

Run: `run-tests system=cl-mcp/tests/utils-request-debugger-boundary-test`

Expected: PASS, including nested request policy propagation and distinct child
contexts.

- [ ] **Step 5: Check source structure and commit**

Run: `lisp-check-parens path=src/utils/deadline.lisp`

Expected: balanced parentheses.

~~~bash
git add src/utils/deadline.lisp tests/test-runner-deadline-test.lisp \
  tests/utils-request-debugger-boundary-test.lisp
git commit -m "deadline: arbitrate debugger and timeout unwinds"
~~~

### Task 4: Connect boundary outcomes to existing response paths

**Files:**
- Modify: `src/worker/server.lisp`
- Modify: `src/repl-core.lisp`
- Modify: `src/system-loader-core.lisp`
- Modify: `src/tools/spec-entry.lisp`
- Modify: `src/spec-adapter-report.lisp`
- Modify: `tests/worker-test.lisp`
- Modify: `tests/repl-error-context-test.lisp`
- Modify: `tests/system-loader-test.lisp`
- Modify: `tests/spec-adapter-report-test.lisp`
- Modify: `tests/spec-tools-test.lisp`

**Interfaces:**
- Consumes Task 2's `request-debugger-result-*` and
  `request-debugger-escape-error-*` accessors and Task 3's unchanged deadline
  result contract.
- Produces no new public wire fields. Existing `repl-eval` gets its existing
  five values and `error_context`; other routes retain their current
  failure/status shapes.

- [ ] **Step 1: Write failing response-routing tests**

Add a direct-dispatch test to `tests/worker-test.lisp`. Mark the synthetic
server authenticated through its internal test accessor, register one method
that directly invokes the debugger, and call `%dispatch-request`. Define the
condition in this test package; individual test systems must not depend on
Task 2's test package.

~~~lisp
(define-condition dispatch-boundary-condition (condition) ())

(deftest dispatch-debugger-escape-uses-existing-internal-error
  (let ((server (make-worker-server :port 0)))
    (unwind-protect
         (progn
           (setf (cl-mcp/src/worker/server::worker-server-authenticated-p server) t)
           (register-method
            server "test/debugger-boundary"
            (lambda (params)
              (declare (ignore params))
              (invoke-debugger
               (make-condition 'dispatch-boundary-condition))))
           (let* ((line (cl-mcp/src/worker/server::%dispatch-request
                         server 17 "test/debugger-boundary" nil))
                  (response (yason:parse line))
                  (error (gethash "error" response)))
             (ok error)
             (ok (= -32603 (gethash "code" error)))
             (ok (search "DISPATCH-BOUNDARY-CONDITION"
                         (gethash "message" error)))))
      (stop-server server))))
~~~

Add a repl test which binds `*request-debugger-boundary-active*` true, evaluates
a direct `CONDITION` through a short deadline, and asserts:

~~~lisp
(ok (search "REPL-BOUNDARY-CONDITION" (getf error-context :condition-type)))
(ok (find-if (lambda (frame)
               (search "REPL-BOUNDARY-FRAME"
                       (or (getf frame :function) "")))
             (getf error-context :frames)))
(ok (find "REPL-BOUNDARY-RESTART" (getf error-context :restarts)
          :key (lambda (restart) (getf restart :name))
          :test #'search))
~~~

Add a system-loader test that invokes `%load-with-timeout` with an explicit
debugger under request policy and verifies its error message contains the saved
original type/message. Add a spec-adapter-report test that sends a
`request-debugger-escape-error` into `%condition-data` and verifies `:type` is
the original type, not `REQUEST-DEBUGGER-ESCAPE-ERROR`. Add a spec-entry test
for `%within-deadline` with a builder returning its record: its existing
`:internal-error` record must display saved original text.

- [ ] **Step 2: Run focused response tests to verify they fail**

Run: `run-tests system=cl-mcp/tests/worker-test`

Expected: FAIL because dispatch does not yet establish request policy or
recognize a debugger result.

Run: `run-tests system=cl-mcp/tests/repl-error-context-test`

Expected: FAIL because repl-eval's generic `%thunk-error-result` discards the
pre-unwind frames/restarts.

- [ ] **Step 3: Establish the dispatch boundary and preserve the repl snapshot**

In `%dispatch-request`, leave authentication, retirement, and the outer
`SERIOUS-CONDITION` handler in their existing order. Inside that handler, bind
the policy and run both handler invocation and JSON encoding through the
boundary:

~~~lisp
(let ((cl-mcp/src/utils/request-debugger-boundary:*request-debugger-boundary-active*
        t))
  (let ((outcome
          (call-with-request-debugger-boundary
           (lambda ()
             (%encode-response
              (%make-result id (funcall handler params)))))))
    (ecase (request-debugger-result-status outcome)
      (:ok (first (request-debugger-result-values outcome)))
      (:debugger
       (error (request-debugger-result-error outcome))))))
~~~

The private error is signaled only after the boundary has returned, so the
existing outer `SERIOUS-CONDITION` clause converts it to `-32603`. Do not add
a `CONDITION` clause.

In `repl-core`, make `%thunk-error-result` recognize
`request-debugger-escape-error-p` before its generic printer. Return the saved
context directly as the fifth repl result and construct the first two text
values from `request-debugger-escape-error-display-text`, not from the original
condition object.

For `system-loader-core` and `tools/spec-entry`, branch only for the private
error and use its display-text accessor before normal sanitizing/building. In
`spec-adapter-report`, make `%condition-data` use the snapshot's
`:condition-type` and `:message`; make `%classify-condition` use display text
when checking its fallback phrase. Existing conditions retain their current
printers and classifications.

- [ ] **Step 4: Run response-path regressions**

Run: `run-tests system=cl-mcp/tests/worker-test`

Expected: PASS. The synthetic deadline-free dispatch returns the existing
`-32603` JSON-RPC error instead of reaching the process debugger.

Run: `run-tests system=cl-mcp/tests/repl-error-context-test`

Expected: PASS. The new repl case contains original type/message, user frame,
and snapshot restart information.

Run: `run-tests system=cl-mcp/tests/system-loader-test`

Expected: PASS. The private error prints saved text and normal loader behavior
is unchanged.

Run: `run-tests system=cl-mcp/tests/spec-adapter-report-test`

Expected: PASS. Adapter condition data names the original condition.

- [ ] **Step 5: Check source structure and commit**

Run: `lisp-check-parens path=src/worker/server.lisp`

Expected: balanced parentheses.

~~~bash
git add src/worker/server.lisp src/repl-core.lisp src/system-loader-core.lisp \
  src/tools/spec-entry.lisp src/spec-adapter-report.lisp \
  tests/worker-test.lisp tests/repl-error-context-test.lisp \
  tests/system-loader-test.lisp tests/spec-adapter-report-test.lisp \
  tests/spec-tools-test.lisp
git commit -m "worker: return debugger escapes through existing failures"
~~~

### Task 5: Prove behavior in fresh pooled workers

**Files:**
- Create: `tests/debugger-boundary-worker-test.lisp`
- Create: `tests/debugger-boundary-run-tests-fixture.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes Task 4's existing response shapes through `proxy-to-worker`.
- The fixture system is package-inferred
  `cl-mcp/tests/debugger-boundary-run-tests-fixture`; do not import it from
  `tests.lisp`, because its tests intentionally fail.
- The ordinary worker acceptance package is
  `cl-mcp/tests/debugger-boundary-worker-test` and must be imported from
  `tests.lisp`.

- [ ] **Step 1: Create the intentionally failing run-tests fixture**

Create `tests/debugger-boundary-run-tests-fixture.lisp`:

~~~lisp
(defpackage #:cl-mcp/tests/debugger-boundary-run-tests-fixture
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok))

(in-package #:cl-mcp/tests/debugger-boundary-run-tests-fixture)

(define-condition run-tests-boundary-condition (condition) ())

(deftest ordinary-rove-failure
  (ok nil "ordinary Rove assertion failure"))

(deftest direct-condition-reaches-worker-boundary
  (error 'run-tests-boundary-condition))
~~~

Do not add this package to `tests.lisp`. It is selected explicitly over the
worker RPC, so the root suite never accidentally runs its two designed
failures.

- [ ] **Step 2: Write failing fresh-worker acceptance tests**

Create `tests/debugger-boundary-worker-test.lisp`. Import Rove, pool/proxy
helpers, `worker-pid`, `worker-state`, `worker-last-exit-status`,
`worker-last-exit-code`, and
`with-pool`/`spawn-available-p`. Use `proxy-to-worker` rather than a raw socket
so authentication works even when `MCP_WORKER_SECRET` is configured.

The main evaluation test has this structure:

~~~lisp
(deftest pooled-worker-survives-direct-condition-errors
  (unless (spawn-available-p) (skip "worker spawning unavailable"))
  (let ((*use-worker-pool* t)
        (*current-session-id* "debugger-boundary-eval"))
    (with-pool ()
      (let* ((worker (get-or-assign-worker *current-session-id*))
             (pid (worker-pid worker)))
        (%worker-eval "(defparameter *boundary-state* 73)")
        (let ((failed
                (%worker-eval
                 "(progn
                    (define-condition worker-direct-condition (condition) ())
                    (error 'worker-direct-condition))"
                 :timeout 5)))
          (ok (gethash "error_context" failed))
          (ok (search "WORKER-DIRECT-CONDITION"
                      (%context-type failed)))
          (ok (= pid (worker-pid worker)))
          (ok (eq :bound (worker-state worker)))
          (ok (not (gethash "isError" failed))
              "the result is not a proxy crash notification"))
        (ok (search "73" (%worker-text (%worker-eval "*boundary-state*"))))))))
~~~

Run that test separately for a direct `SIMPLE-CONDITION` with a distinct type
name, and for explicit `INVOKE-DEBUGGER`. Check elapsed time is below five
seconds so a 300-second request deadline cannot satisfy the assertion.

Add a diagnostics test that defines a named user function, a named restart,
and a direct condition. Assert the returned `error_context` carries original
type/message, the user function frame if SBCL exposes it, and the restart name.
Add separate report fixtures for a normal secondary `ERROR` and direct
`INVOKE-DEBUGGER`; each must retain the original type and leave the PID
unchanged.

Add non-regression RPC cases for:

~~~lisp
(signal 'worker-direct-condition)
(handler-bind ((warning #'muffle-warning)) (warn "muffled") :ok)
(handler-case (error "handled") (error () :handled))
(handler-bind ((error (lambda (c) (declare (ignore c))
                         (invoke-restart :recover))))
  (restart-case (error "recover") (:recover () :ok)))
~~~

The first three return normal values. The restart form returns `:ok`. A
`handler-case` specialized on `ERROR` around a direct `CONDITION` must not
report its success branch.

Add a run-tests RPC test which first selects `ordinary-rove-failure` and
asserts the ordinary Rove result has one failure. It then selects
`direct-condition-reaches-worker-boundary` and asserts the response is the
existing error form, includes `RUN-TESTS-BOUNDARY-CONDITION`, does not say the
worker crashed, retains its PID, and is followed by a successful `worker/eval`.
This calls `run-tests` through `%ensure-system-loaded` and the selected-Rove
path, so it verifies the boundary after both existing test-runner ERROR
boundaries have declined the direct condition.

Add an isolated real-exit control test. Send `(sb-ext:exit :code 71)` to the
fresh child worker; poll the original worker's reaper fields until its terminal
exit status/code are present, assert the proxy reports a crash/reset rather
than a structured debugger failure, then issue the documented retry sequence
until a replacement accepts an ordinary eval.

- [ ] **Step 3: Register and run the failing acceptance test**

Add this import to `tests.lisp`:

~~~lisp
  (:import-from #:cl-mcp/tests/debugger-boundary-worker-test)
~~~

Run: `run-tests system=cl-mcp/tests/debugger-boundary-worker-test`

Expected before the implementation is complete: FAIL assertions caused by the
isolated child worker's crash or by missing structured diagnostic information;
the test runner itself remains alive because the risky code executes in a
fresh nested pooled worker.

- [ ] **Step 4: Run the fresh-worker acceptance tests after Tasks 1–4**

Run: `run-tests system=cl-mcp/tests/debugger-boundary-worker-test`

Expected: PASS. Direct `CONDITION`, `SIMPLE-CONDITION`, and explicit
`INVOKE-DEBUGGER` yield request-local failures with an unchanged PID and
surviving state. The run-tests fixture separates ordinary Rove failure from a
debugger escape. The `SB-EXT:EXIT` control still follows EOF, crash recovery,
and reap.

- [ ] **Step 5: Check source structure and commit**

Run: `lisp-check-parens path=tests/debugger-boundary-worker-test.lisp`

Expected: balanced parentheses.

~~~bash
git add tests/debugger-boundary-worker-test.lisp \
  tests/debugger-boundary-run-tests-fixture.lisp tests.lisp
git commit -m "test(worker): cover debugger observe-and-abort"
~~~

### Task 6: Document the boundary and perform scoped verification

**Files:**
- Modify: `README.md`
- Modify: `docs/tools.md`

**Interfaces:**
- Documents existing behavior only; adds no tool, option, live debugger
  protocol, restart API, or wire-schema field.

- [ ] **Step 1: Write the documentation changes**

In `README.md`'s **Worker Pool Isolation** section, append a compact paragraph:

~~~text
For a pooled SBCL worker request, a condition that no existing handler resolves
and that reaches the debugger path is observed and aborted as that request's
structured failure. This preserves the worker for later requests, but it does
not roll back side effects made before the failure. A process exit such as
SB-EXT:EXIT remains a real worker crash and follows normal EOF/reaper recovery.
~~~

In `docs/tools.md`'s **repl-eval** `error_context` description, add:

~~~text
When this request-level debugger boundary is used, condition type/message,
frames, locals when available, and restarts are captured before unwind.
Restart entries are diagnostic snapshots only; they cannot be invoked after
the response. The worker remaining alive does not make the evaluation
transactional or restore mutated shared state.
~~~

- [ ] **Step 2: Verify documentation wording and no accidental schema change**

Run: `rg -n "observed and aborted|diagnostic snapshots|does not roll back|SB-EXT:EXIT" README.md docs/tools.md`

Expected: the new paragraphs appear exactly once each.

Run: `git diff --check`

Expected: no whitespace errors.

- [ ] **Step 3: Compile and run the focused regression matrix**

First force-reload the worktree root:

Run: `load-system system=cl-mcp force=true clear_fasls=true timeout_seconds=120`

Expected: load completes with no package cycle. Record any pre-existing warning
separately; do not accept a warning introduced by this branch.

Then run:

~~~text
run-tests system=cl-mcp/tests/frame-inspector-test
run-tests system=cl-mcp/tests/utils-request-debugger-boundary-test
run-tests system=cl-mcp/tests/test-runner-deadline-test
run-tests system=cl-mcp/tests/repl-error-context-test
run-tests system=cl-mcp/tests/worker-test
run-tests system=cl-mcp/tests/system-loader-test
run-tests system=cl-mcp/tests/spec-adapter-report-test
run-tests system=cl-mcp/tests/spec-tools-test
run-tests system=cl-mcp/tests/debugger-boundary-worker-test
run-tests system=cl-mcp/tests/cancel-test
run-tests system=cl-mcp/tests/pool-test
~~~

Expected: every selected system passes. The last two are the controls proving
that MCP cancellation still kills its worker and genuine process crashes still
use pool/reaper recovery. Do not claim the known unrelated long aggregate
run-tests timeout has been fixed; if a full aggregate is attempted and hits
that pre-existing timeout, record it separately with elapsed time and logs.

- [ ] **Step 4: Review the final diff and commit**

Run:

~~~bash
git status --short
git diff --check origin/main...HEAD
git log --oneline origin/main..HEAD
~~~

Expected: only the hardening implementation, tests, and the two scoped
documentation changes are present.

~~~bash
git add README.md docs/tools.md
git commit -m "docs: describe worker debugger observe-and-abort"
~~~

- [ ] **Step 5: Prepare the isolated hardening PR**

Run:

~~~bash
git status --short
git log --oneline origin/main..HEAD
gh pr create --base main --head debugger/observe-and-abort \
  --title "worker: observe and abort debugger escapes" \
  --body-file /tmp/cl-mcp-debugger-pr-body.md
~~~

Write `/tmp/cl-mcp-debugger-pr-body.md` with the exact sections **Problem**,
**Boundary**, **Non-goals**, **Verification**, and **Lifecycle controls**. The
body must state that the worker remains alive without rollback, that restart
data is snapshot-only, that MCP cancellation and `SB-EXT:EXIT` remain distinct,
and list every focused `run-tests` command from Step 3.

## Plan Self-Review

**Spec coverage:** Task 1 covers safe pre-unwind diagnostics; Task 2 covers the
primary/secondary hook, private throw, wrapper safety, and normal condition
semantics; Task 3 covers nested policy propagation, deadline ordering, cleanup,
and leaked-thread preservation; Task 4 covers each existing response family;
Task 5 proves fresh pooled-worker behavior, Rove separation, and real process
exit; Task 6 covers documentation, cancellation, compile, and scoped
regressions. No requested behavior is assigned to a generic `CONDITION`
boundary or a live debugger.

**Placeholder scan:** This plan has no deferred implementation markers. Every
task names its files, interfaces, failing test behavior, verification command,
and commit.

**Type consistency:** The only cross-task types are
`request-debugger-result`, `request-debugger-escape-error`, and their named
accessors defined in Task 2. Task 3 publishes the latter through the existing
deadline `:error` value; Task 4 consumes it; Task 5 observes only existing
wire responses.

**Review Focus coverage:** The five review-focus cases are exercised in Tasks
5, 1/2, 3, 2/3, and 5/6 respectively.
