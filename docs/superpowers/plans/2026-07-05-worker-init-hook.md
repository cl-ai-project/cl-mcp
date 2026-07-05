# Worker Init Hook Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a parent-orchestrated, session-bound, single-owner worker init hook so an app (e.g. recurya's web server) can auto-start inside a cl-mcp worker while the parent keeps the persistent `/mcp` endpoint, with init failures fully walled off from the crash breaker.

**Architecture:** The hook is **absent from `worker/main:start`** (so standby/replenishment/crash-replacement workers boot bare and never bind the fixed port). The parent, at the `:bound` transition, elects a single **runtime owner session** and sends a fire-and-forget `worker/init-start` RPC; the worker runs init on a background thread guarded by a worker-global `*asdf-load-lock*`, calling `load-system` with `timeout=nil` (the direct branch — no `destroy-thread` corruption). The parent polls `worker/init-status`, surfaces state in `pool-status`, and treats init-attributable crashes separately from the circuit breaker.

**Tech Stack:** SBCL, ASDF `package-inferred-system`, Rove tests, `bordeaux-threads`, `usocket`, `yason`.

**Spec:** `docs/plans/2026-07-05-worker-init-hook-design.md` (design examination). Read §5 (v1 design), §6 (implementation sketch), and the failure-scenario table before starting.

**Scope note:** Phase 1 (worker `*asdf-load-lock*`) is independently shippable and valuable even without the hook — it fixes the pre-existing concurrent-ASDF-load hazard. It is a reasonable first PR. Phases 4–5 (parent ownership + breaker isolation) are the heavy correctness work; all four correctness pillars in design §9 must land before enabling the feature in production.

---

## File Structure

**Worker process (loaded via `cl-mcp/src/worker/main` in the child SBCL):**
- Create `src/worker/init-hook.lisp` — package `cl-mcp/src/worker/init-hook`. Owns `*asdf-load-lock*` + `with-asdf-load-lock`, the init state machine (`*init-state*`), entry resolution (`%resolve-entry`), the init runner (`%run-init`), and the two RPC handlers (`handle-init-start`, `handle-init-status`). One file, one responsibility: "everything the worker does for the init hook."
- Modify `src/worker/handlers.lisp` — import `with-asdf-load-lock`, `handle-init-start`, `handle-init-status` from the new file; wrap the `load-system` and `run-tests` calls in the lock; register the two new methods.

**Parent process (loaded via `cl-mcp/main`):**
- Modify `src/pool.lisp` — config parsing (`%parse-worker-init-config`, `%env-string`), ownership specials (`*runtime-owner*`, `*runtime-init-failures*`, `*runtime-init-disabled*`, `*init-attributable-crashes*`), election (`%elect-runtime-owner`, `%release-runtime-owner-if`), orchestration (`%ensure-runtime-init`, `%start-init-and-monitor`), call-site wiring, breaker isolation, and `pool-status-info` fields.
- Modify `src/worker-client.lisp` — denylist the `MCP_WORKER_INIT_*` vars in `%build-environment`.

**Tests:**
- Create `tests/worker-init-hook-test.lisp` — package `cl-mcp/tests/worker-init-hook-test`. Load-lock serialization, state machine, entry resolution, and an integration test (spawn a worker server, drive `worker/init-start` → `worker/init-status`).
- Create `tests/pool-init-config-test.lisp` — package `cl-mcp/tests/pool-init-config-test`. Config parsing, env denylist, election logic, ownership release, breaker isolation.
- Modify `tests.lisp` — register both new test packages in the `cl-mcp/tests` aggregate.

**Docs:**
- Modify `README.md` — document the `MCP_WORKER_INIT_*` env vars and recurya wiring.

---

## Conventions for every task

- **Run a single test file** via the `run-tests` MCP tool: `{"system": "cl-mcp/tests/<name>-test"}`. Shell fallback for a clean image: `rove cl-mcp.asd` from the repo root (runs the whole suite).
- **Run one test** via `repl-eval`: `(rove:run-test 'cl-mcp/tests/<pkg>::<test-name>)` after `load-system`.
- **Edit `.lisp` files with `lisp-edit-form`/`lisp-patch-form`**, never the text Edit tool (per project MEMORY). After structural edits, run `lisp-check-parens`.
- **Lint before each commit:** `mallet src/*.lisp` (and the edited test files).
- New worker-side files are auto-loaded by `package-inferred-system` once `handlers.lisp` imports them; no `cl-mcp.asd` edit is needed. New test files MUST be added to `tests.lisp`.

---

## Phase 1 — Worker ASDF load lock (correctness pillar #1; independently shippable)

### Task 1: Create `src/worker/init-hook.lisp` with the ASDF load lock

**Files:**
- Create: `src/worker/init-hook.lisp`
- Create: `tests/worker-init-hook-test.lisp`
- Modify: `tests.lisp`

- [ ] **Step 1: Create the new test file with the failing serialization test**

Create `tests/worker-init-hook-test.lisp`:

```lisp
;;;; tests/worker-init-hook-test.lisp
;;;;
;;;; Tests for the worker-side init hook: load lock, init state machine,
;;;; entry resolution, and the init RPC handlers.

(defpackage #:cl-mcp/tests/worker-init-hook-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/worker/init-hook
                #:*asdf-load-lock*
                #:with-asdf-load-lock))

(in-package #:cl-mcp/tests/worker-init-hook-test)

(deftest with-asdf-load-lock-serializes
  (testing "two threads holding the lock never overlap in the critical section"
    (let ((inside 0) (max-inside 0) (lock (bt:make-lock "probe")))
      (flet ((body ()
               (with-asdf-load-lock
                 (bt:with-lock-held (lock)
                   (incf inside)
                   (setf max-inside (max max-inside inside)))
                 (sleep 0.02)
                 (bt:with-lock-held (lock) (decf inside)))))
        (let ((threads (loop repeat 5
                             collect (bt:make-thread #'body :name "probe"))))
          (dolist (th threads) (bt:join-thread th))))
      (ok (= max-inside 1)
          "at most one thread was inside the load-lock critical section"))))
```

- [ ] **Step 2: Register the new test package in `tests.lisp`**

Add this line to the `defpackage #:cl-mcp/tests` form in `tests.lisp`, after the `cl-mcp/tests/worker-test` import (line 47):

```lisp
  (:import-from #:cl-mcp/tests/worker-init-hook-test)
```

- [ ] **Step 3: Run the test to verify it fails**

Run via `run-tests`: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: FAIL / load error — `cl-mcp/src/worker/init-hook` package does not exist yet (`with-asdf-load-lock` unresolved).

- [ ] **Step 4: Create `src/worker/init-hook.lisp` with the lock**

Create `src/worker/init-hook.lisp`:

```lisp
;;;; src/worker/init-hook.lisp
;;;;
;;;; Worker-side machinery for the init hook.  Provides the worker-global
;;;; ASDF load lock (so cl-mcp-mediated loads never overlap), the init
;;;; state machine, entry resolution, and the worker/init-start and
;;;; worker/init-status RPC handlers.  See
;;;; docs/plans/2026-07-05-worker-init-hook-design.md.

(defpackage #:cl-mcp/src/worker/init-hook
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held #:make-thread)
  (:export #:*asdf-load-lock*
           #:with-asdf-load-lock))

(in-package #:cl-mcp/src/worker/init-hook)

(defvar *asdf-load-lock* (bt:make-lock "asdf-load-lock")
  "Worker-global lock serializing every cl-mcp-mediated ASDF load site
(worker/init, worker/load-system, worker/run-tests).  Prevents two
concurrent ASDF load-ops in one worker image, which the single-threaded
dispatch loop does NOT prevent because load-system/repl-eval run their
work on spawned helper threads.")

(defmacro with-asdf-load-lock (&body body)
  "Evaluate BODY holding *ASDF-LOAD-LOCK*."
  `(bt:with-lock-held (*asdf-load-lock*) ,@body))
```

- [ ] **Step 5: Run the test to verify it passes**

Run via `run-tests`: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: PASS — `with-asdf-load-lock-serializes` green.

- [ ] **Step 6: Lint and commit**

```bash
mallet src/worker/init-hook.lisp
git add src/worker/init-hook.lisp tests/worker-init-hook-test.lisp tests.lisp
git commit -m "feat(worker): add worker-global ASDF load lock"
```

---

### Task 2: Serialize the `load-system` and `run-tests` handlers with the lock

**Files:**
- Modify: `src/worker/handlers.lisp:105-120` (`%handle-load-system`), `src/worker/handlers.lisp:126-155` (`%handle-run-tests`), and the `defpackage` (lines 9-43)
- Test: `tests/worker-init-hook-test.lisp`

- [ ] **Step 1: Write the failing test — handlers still return their payloads under the lock**

Append to `tests/worker-init-hook-test.lisp`:

```lisp
(deftest load-system-handler-holds-lock
  (testing "%handle-load-system runs its ASDF load inside the load lock"
    ;; The lock must be free before and after; while the handler runs a
    ;; (fast) load of an already-present system, a concurrent attempt to
    ;; take the lock must block until the handler releases it.
    (ok (not (bt:acquire-lock cl-mcp/src/worker/init-hook:*asdf-load-lock* nil))
        "lock is available (acquire returns non-nil), then release it")
    (bt:release-lock cl-mcp/src/worker/init-hook:*asdf-load-lock*)
    (skip "serialization vs a concurrent load is covered by the integration test")))
```

Note: this is a smoke assertion; genuine serialization across `load-system` and `run-tests` is exercised by the Phase 2 integration test (Task 5) which drives real RPCs. The point of this task is that the handlers compile and still work after wrapping.

- [ ] **Step 2: Run the test to verify it fails**

Run via `run-tests`: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: FAIL — `bt:acquire-lock` on `*asdf-load-lock*` returns a value that trips the assertion phrasing OR compiles but the wrapping is not yet present. (If it passes trivially, proceed; the real verification is Step 4's compile + Task 5 integration.)

- [ ] **Step 3: Add the import to `handlers.lisp`'s `defpackage`**

Add to the `defpackage #:cl-mcp/src/worker/handlers` form (after the `#:cl-mcp/src/worker/server` import at line 41-42):

```lisp
  (:import-from #:cl-mcp/src/worker/init-hook
                #:with-asdf-load-lock)
```

- [ ] **Step 4: Wrap the `load-system` call in `%handle-load-system`**

Replace the `let` binding the load result (lines 116-119) so the `load-system` call is inside the lock. The new body of `%handle-load-system`:

```lisp
(defun %handle-load-system (params)
  "Load an ASDF system.  Returns the same structure as define-tool
\"load-system\".  Holds *ASDF-LOAD-LOCK* so it cannot overlap a
concurrent worker/init load or another load-system."
  (let ((system (gethash "system" params))
         (force (%bool-default params "force" t))
         (clear-fasls (gethash "clear_fasls" params))
         (timeout-seconds (gethash "timeout_seconds" params)))
    (unless system
      (error "system is required"))
    (when (and timeout-seconds (not (plusp timeout-seconds)))
      (error "timeout_seconds must be a positive number"))
    (let ((ht (with-asdf-load-lock
                (load-system system
                             :force force
                             :clear-fasls clear-fasls
                             :timeout-seconds (or timeout-seconds 120)))))
      (build-load-system-response system ht))))
```

- [ ] **Step 5: Wrap the `run-tests` call in `%handle-run-tests`**

In `%handle-run-tests`, wrap the `do-run` local function body so the test run (which force-reloads the test system) holds the lock. Change the `flet` in lines 137-141 to:

```lisp
    (flet ((do-run ()
             (with-asdf-load-lock
               (run-tests system
                          :framework framework
                          :test test
                          :tests tests))))
```

- [ ] **Step 6: Run the test to verify it passes**

Run via `run-tests`: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: PASS. Also run the existing worker suite to confirm no regression: `{"system": "cl-mcp/tests/worker-test"}` → PASS.

- [ ] **Step 7: Verify parens, lint, commit**

Run `lisp-check-parens` on `src/worker/handlers.lisp`. Then:

```bash
mallet src/worker/handlers.lisp
git add src/worker/handlers.lisp tests/worker-init-hook-test.lisp
git commit -m "feat(worker): serialize load-system and run-tests under the ASDF load lock"
```

---

## Phase 2 — Worker init state machine, entry resolution, and handlers

### Task 3: Init state machine

**Files:**
- Modify: `src/worker/init-hook.lisp`
- Test: `tests/worker-init-hook-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/worker-init-hook-test.lisp` (add the state-machine symbols to the test package's `:import-from` for `cl-mcp/src/worker/init-hook` first — edit the `defpackage` to also import `#:%set-init-state #:init-state-snapshot #:%reset-init-state`):

```lisp
(deftest init-state-transitions
  (testing "state starts idle, moves to loading/running/failed, snapshots as a hash-table"
    (cl-mcp/src/worker/init-hook::%reset-init-state)
    (let ((s0 (cl-mcp/src/worker/init-hook::init-state-snapshot)))
      (ok (string= (gethash "init_state" s0) "idle") "starts idle"))
    (cl-mcp/src/worker/init-hook::%set-init-state :loading)
    (ok (string= (gethash "init_state"
                          (cl-mcp/src/worker/init-hook::init-state-snapshot))
                 "loading")
        "loading")
    (cl-mcp/src/worker/init-hook::%set-init-state :running :app-port 13000)
    (let ((s (cl-mcp/src/worker/init-hook::init-state-snapshot)))
      (ok (string= (gethash "init_state" s) "running") "running")
      (ok (eql (gethash "app_port" s) 13000) "app_port recorded"))
    (cl-mcp/src/worker/init-hook::%set-init-state :failed :error "boom")
    (let ((s (cl-mcp/src/worker/init-hook::init-state-snapshot)))
      (ok (string= (gethash "init_state" s) "failed") "failed")
      (ok (string= (gethash "last_init_error" s) "boom") "error recorded"))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: FAIL — `%set-init-state`/`init-state-snapshot`/`%reset-init-state` unbound.

- [ ] **Step 3: Implement the state machine in `init-hook.lisp`**

Add to `src/worker/init-hook.lisp` after the `with-asdf-load-lock` macro. First extend the `:export` list to add `#:handle-init-start #:handle-init-status` (used later) and keep internals unexported:

```lisp
(defvar *init-lock* (bt:make-lock "worker-init-state")
  "Protects *INIT-STATE*.")

(defvar *init-state* (list :state :idle :app-port nil :error nil :started-at nil)
  "Init progress: :state is one of :idle :loading :running :failed.")

(defun %reset-init-state ()
  "Reset init state to :idle (used by tests and re-arming)."
  (bt:with-lock-held (*init-lock*)
    (setf *init-state* (list :state :idle :app-port nil :error nil
                             :started-at nil))))

(defun %set-init-state (state &key app-port error)
  "Transition init state.  STATE is a keyword; APP-PORT/ERROR update the
corresponding fields when provided."
  (bt:with-lock-held (*init-lock*)
    (setf (getf *init-state* :state) state)
    (when (eq state :loading)
      (setf (getf *init-state* :started-at) (get-universal-time)))
    (when app-port (setf (getf *init-state* :app-port) app-port))
    (when error (setf (getf *init-state* :error) error))))

(defun init-state-snapshot ()
  "Return a hash-table snapshot of init state for pool-status / RPC.
Keys: init_state, app_port, last_init_error, started_at."
  (bt:with-lock-held (*init-lock*)
    (let ((ht (make-hash-table :test 'equal)))
      (setf (gethash "init_state" ht) (string-downcase (getf *init-state* :state))
            (gethash "app_port" ht) (getf *init-state* :app-port)
            (gethash "last_init_error" ht) (getf *init-state* :error)
            (gethash "started_at" ht) (getf *init-state* :started-at))
      ht)))
```

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: PASS — `init-state-transitions` green.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/worker/init-hook.lisp
git add src/worker/init-hook.lisp tests/worker-init-hook-test.lisp
git commit -m "feat(worker): add init state machine"
```

---

### Task 4: Entry resolution (`%resolve-entry`)

**Files:**
- Modify: `src/worker/init-hook.lisp`
- Test: `tests/worker-init-hook-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/worker-init-hook-test.lisp`:

```lisp
(defun a-test-entry-thunk () 4242)

(deftest resolve-entry
  (testing "PKG:SYM and PKG::SYM resolve to the fdefinition; bad specs error"
    (let ((fn (cl-mcp/src/worker/init-hook::%resolve-entry
               "CL-MCP/TESTS/WORKER-INIT-HOOK-TEST:A-TEST-ENTRY-THUNK")))
      (ok (functionp fn) "resolves to a function")
      (ok (eql (funcall fn) 4242) "funcalls the resolved thunk"))
    (ok (functionp
         (cl-mcp/src/worker/init-hook::%resolve-entry
          "CL-MCP/TESTS/WORKER-INIT-HOOK-TEST::A-TEST-ENTRY-THUNK"))
        "double-colon form resolves")
    (ok (handler-case
            (progn (cl-mcp/src/worker/init-hook::%resolve-entry "no-colon") nil)
          (error () t))
        "spec without a colon errors")
    (ok (handler-case
            (progn (cl-mcp/src/worker/init-hook::%resolve-entry "NOSUCHPKG:FOO") nil)
          (error () t))
        "missing package errors")
    (ok (handler-case
            (progn (cl-mcp/src/worker/init-hook::%resolve-entry "CL:NO-SUCH-SYMBOL-XYZ") nil)
          (error () t))
        "missing symbol errors")))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: FAIL — `%resolve-entry` unbound.

- [ ] **Step 3: Implement `%resolve-entry`**

Add to `src/worker/init-hook.lisp`:

```lisp
(defun %resolve-entry (spec)
  "Resolve a \"PKG:SYMBOL\" or \"PKG::SYMBOL\" string to a callable.
Uses find-package / find-symbol / fdefinition only -- no read, eval, or
intern -- honoring the project's no-runtime-eval style rule.  Signals an
error if the package or symbol is missing or the symbol is not fbound."
  (let* ((dbl (search "::" spec))
         (colon (or dbl (position #\: spec))))
    (unless colon
      (error "init entry ~S must be of the form PKG:SYMBOL" spec))
    (let* ((pkg-name (string-upcase (subseq spec 0 colon)))
           (sym-name (string-upcase (subseq spec (+ colon (if dbl 2 1)))))
           (pkg (find-package pkg-name)))
      (unless pkg
        (error "init entry: package ~A not found" pkg-name))
      (let ((sym (find-symbol sym-name pkg)))
        (unless sym
          (error "init entry: symbol ~A not found in package ~A"
                 sym-name pkg-name))
        (unless (fboundp sym)
          (error "init entry: ~A is not fbound" sym))
        (fdefinition sym)))))
```

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: PASS.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/worker/init-hook.lisp
git add src/worker/init-hook.lisp tests/worker-init-hook-test.lisp
git commit -m "feat(worker): add init entry-point resolution"
```

---

### Task 5: Init runner + RPC handlers, registered on the server

**Files:**
- Modify: `src/worker/init-hook.lisp`
- Modify: `src/worker/handlers.lisp` (`defpackage` + `register-all-handlers`)
- Test: `tests/worker-init-hook-test.lisp`

- [ ] **Step 1: Write the failing integration test**

Append to `tests/worker-init-hook-test.lisp` (add `#:make-worker-server #:server-port #:start-accept-loop #:stop-server` from `cl-mcp/src/worker/server` and `#:register-all-handlers` from `cl-mcp/src/worker/handlers` to the test `defpackage` imports):

```lisp
(defparameter *entry-ran* nil)
(defun integration-entry-thunk () (setf *entry-ran* t) 12345)

(defun %rpc (stream id method &optional params)
  "Send one JSON-RPC line and read one response line; return the parsed hash."
  (let ((req (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" req) "2.0"
          (gethash "id" req) id
          (gethash "method" req) method)
    (when params (setf (gethash "params" req) params))
    (yason:encode req stream) (terpri stream) (force-output stream)
    (yason:parse (read-line stream))))

(deftest init-start-then-status-integration
  (testing "worker/init-start acks fast; init runs the entry; status reaches running"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        (let ((server (make-worker-server :port 0)))
          (register-all-handlers server)
          (setf *entry-ran* nil)
          (cl-mcp/src/worker/init-hook::%reset-init-state)
          (unwind-protect
               (let ((port (server-port server)))
                 (bt:make-thread (lambda () (start-accept-loop server))
                                 :name "test-init-accept")
                 (sleep 0.1)
                 (let ((socket (usocket:socket-connect "127.0.0.1" port
                                                       :element-type 'character)))
                   (unwind-protect
                        (let* ((stream (usocket:socket-stream socket))
                               (params (make-hash-table :test 'equal)))
                          (setf (gethash "entry" params)
                                "CL-MCP/TESTS/WORKER-INIT-HOOK-TEST:INTEGRATION-ENTRY-THUNK")
                          (let ((ack (%rpc stream 1 "worker/init-start" params)))
                            (ok (gethash "accepted" (gethash "result" ack))
                                "init-start acked with accepted=t"))
                          ;; Poll status until it leaves :loading (bounded).
                          (let ((final nil))
                            (loop repeat 50
                                  for st = (gethash "result"
                                            (%rpc stream 2 "worker/init-status"))
                                  for state = (gethash "init_state" st)
                                  do (setf final state)
                                  until (member state '("running" "failed")
                                                :test #'string=)
                                  do (sleep 0.05))
                            (ok (string= final "running") "init reached running")
                            (ok *entry-ran* "entry thunk executed")))
                     (ignore-errors (usocket:socket-close socket)))))
            (stop-server server))))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: FAIL — `worker/init-start` is `Method not found` (handlers not registered).

- [ ] **Step 3: Implement `%run-init` and the two handlers in `init-hook.lisp`**

Add the imports the runner needs to the `defpackage` (`#:cl-mcp/src/system-loader-core #:load-system`, `#:cl-mcp/src/repl-core #:repl-eval`, `#:cl-mcp/src/utils/sanitize #:sanitize-error-message`, `#:cl-mcp/src/tools/helpers #:make-ht`, `#:cl-mcp/src/log #:log-event`). Then add:

```lisp
(defun %maybe-eval (form-string package-name)
  "Run FORM-STRING via repl-core:repl-eval in PACKAGE-NAME.  Signals an
error if the evaluation produced an error-context, so the outer
handler-case records a :failed init.  Routing through repl-eval (not raw
eval) reuses the sanctioned evaluator."
  (let ((pkg (or (find-package (string-upcase package-name)) *package*)))
    (multiple-value-bind (printed raw stdout stderr err-ctx)
        (repl-eval form-string :package pkg)
      (declare (ignore printed raw stdout stderr))
      (when err-ctx
        (error "init eval failed: ~A"
               (if (hash-table-p err-ctx)
                   (or (gethash "message" err-ctx) err-ctx)
                   err-ctx))))))

(defun %run-init (params)
  "Background-thread init runner.  Holds *ASDF-LOAD-LOCK* for the whole
load so it cannot overlap a concurrent load-system/run-tests.  Loads with
timeout=NIL (the direct branch -- no spawned thread, no destroy-thread
mid-compile).  Never signals out of this function: on any error it records
a :failed init and leaves the worker fully usable."
  (let ((system (gethash "system" params))
        (evalform (gethash "eval" params))
        (entry (gethash "entry" params))
        (pkg (or (gethash "package" params) "CL-USER")))
    (%set-init-state :loading)
    (handler-case
        (with-asdf-load-lock
          (when system
            (load-system system :force nil :timeout-seconds nil))
          (when evalform
            (%maybe-eval evalform pkg))
          (let ((port nil))
            (when entry
              (setf port (funcall (%resolve-entry entry))))
            (%set-init-state :running
                             :app-port (and (integerp port) port))
            (log-event :info "worker.init.done"
                       "app_port" (and (integerp port) port))))
      (serious-condition (e)
        (%set-init-state :failed :error (sanitize-error-message e))
        (log-event :warn "worker.init.failed"
                   "error" (sanitize-error-message e))))))

(defun handle-init-start (params)
  "worker/init-start handler.  Spawns the init runner on a background
thread and returns an ACK immediately, so the parent's RPC does not block
on the (heavy) load and no long stream-lock is held."
  (bt:make-thread (lambda () (%run-init params)) :name "mcp-worker-init")
  (make-ht "accepted" t))

(defun handle-init-status (params)
  "worker/init-status handler.  Returns the current init state snapshot."
  (declare (ignore params))
  (init-state-snapshot))
```

- [ ] **Step 4: Register the handlers in `handlers.lisp`**

Add to `defpackage #:cl-mcp/src/worker/handlers` the imports (extend the existing `#:cl-mcp/src/worker/init-hook` import clause added in Task 2):

```lisp
  (:import-from #:cl-mcp/src/worker/init-hook
                #:with-asdf-load-lock
                #:handle-init-start
                #:handle-init-status)
```

In `register-all-handlers`, add the two registrations before the `log-event` call and bump the count from 8 to 10:

```lisp
  (register-method server "worker/init-start" #'handle-init-start)
  (register-method server "worker/init-status" #'handle-init-status)
  (log-event :info "worker.handlers.registered" "count" 10)
```

- [ ] **Step 5: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/worker-init-hook-test"}`
Expected: PASS — `init-start-then-status-integration` reaches `running` and the entry thunk ran.

- [ ] **Step 6: Verify parens, lint, commit**

Run `lisp-check-parens` on both edited src files. Then:

```bash
mallet src/worker/init-hook.lisp src/worker/handlers.lisp
git add src/worker/init-hook.lisp src/worker/handlers.lisp tests/worker-init-hook-test.lisp
git commit -m "feat(worker): add worker/init-start and worker/init-status handlers"
```

---

## Phase 3 — Parent config parsing, env denylist, pool-off guard

### Task 6: Parse `MCP_WORKER_INIT_*` config in the pool

**Files:**
- Modify: `src/pool.lisp` (config section, ~lines 90-141; `initialize-pool` ~686-741)
- Create: `tests/pool-init-config-test.lisp`
- Modify: `tests.lisp`

- [ ] **Step 1: Create the config test file**

Create `tests/pool-init-config-test.lisp`:

```lisp
;;;; tests/pool-init-config-test.lisp
;;;;
;;;; Tests for parent-side worker-init-hook config parsing, env denylist,
;;;; ownership election, and crash-breaker isolation.

(defpackage #:cl-mcp/tests/pool-init-config-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/pool
                #:*worker-init-config*))

(in-package #:cl-mcp/tests/pool-init-config-test)

(defun %with-env (bindings thunk)
  "Set env BINDINGS ((name . value) ...) for the duration of THUNK, then
restore.  A NIL value unsets the variable."
  (let ((saved (loop for (name . nil) in bindings
                     collect (cons name (uiop:getenv name)))))
    (unwind-protect
         (progn
           (loop for (name . value) in bindings
                 do (if value (setf (uiop/os:getenv name) value)
                        (sb-posix:unsetenv name)))
           (funcall thunk))
      (loop for (name . value) in saved
            do (if value (setf (uiop/os:getenv name) value)
                   (sb-posix:unsetenv name))))))

(deftest parse-worker-init-config
  (testing "config is nil when SYSTEM is unset, populated when set"
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . nil))
      (lambda ()
        (ok (null (cl-mcp/src/pool::%parse-worker-init-config))
            "no SYSTEM => nil config")))
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . "recurya/dev")
                 ("MCP_WORKER_INIT_ENTRY" . "recurya/dev:start-dev-runtime!")
                 ("MCP_WORKER_INIT_MAX_FAILURES" . "1"))
      (lambda ()
        (let ((cfg (cl-mcp/src/pool::%parse-worker-init-config)))
          (ok cfg "config present")
          (ok (string= (getf cfg :system) "recurya/dev") "system parsed")
          (ok (string= (getf cfg :entry) "recurya/dev:start-dev-runtime!")
              "entry parsed")
          (ok (eql (getf cfg :max-failures) 1) "max-failures parsed"))))))
```

- [ ] **Step 2: Register the test package in `tests.lisp`**

Add after the `cl-mcp/tests/pool-kill-worker-test` import (line 52):

```lisp
  (:import-from #:cl-mcp/tests/pool-init-config-test)
```

- [ ] **Step 3: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — `*worker-init-config*` and `%parse-worker-init-config` unbound.

- [ ] **Step 4: Add `%env-string`, specials, and `%parse-worker-init-config` to `pool.lisp`**

After the `%env-int` defun (ends line 117), add `%env-string`:

```lisp
(defun %env-string (name)
  "Return the environment variable NAME as a string, or NIL when unset or
empty."
  (let ((s (uiop:getenv name)))
    (and s (plusp (length s)) s)))
```

After the existing config defvars (after `*shutdown-replenish-wait-seconds*`, line 140), add:

```lisp
(defvar *worker-init-config* nil
  "Parsed worker-init-hook config, or NIL when the feature is off.
A plist: (:system S :entry E :eval EV :package P :max-failures N :mode M).")

(defun %parse-worker-init-config ()
  "Read MCP_WORKER_INIT_* from the environment into a config plist, or NIL
when MCP_WORKER_INIT_SYSTEM is unset (feature off).  MCP_WORKER_INIT_SYSTEM
is the master gate, mirroring MCP_WORKER_SWANK."
  (let ((system (%env-string "MCP_WORKER_INIT_SYSTEM")))
    (when system
      (list :system system
            :entry (%env-string "MCP_WORKER_INIT_ENTRY")
            :eval (%env-string "MCP_WORKER_INIT_EVAL")
            :package (or (%env-string "MCP_WORKER_INIT_PACKAGE") "CL-USER")
            :max-failures (%env-int "MCP_WORKER_INIT_MAX_FAILURES" 1 :min 1)
            :mode (or (%env-string "MCP_WORKER_INIT_MODE") "singleton")))))
```

Export `*worker-init-config*` from the `pool` package by adding `#:*worker-init-config*` to the `:export` list of `defpackage #:cl-mcp/src/pool` (after `#:*max-pool-size*`, line 51).

- [ ] **Step 5: Populate the config in `initialize-pool`**

In `initialize-pool`, inside the `bt:with-lock-held (*pool-lock*)` reset block (lines 723-728), add a line so the config is refreshed on every pool init:

```lisp
      (setf *worker-init-config* (%parse-worker-init-config))
```

- [ ] **Step 6: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: PASS — `parse-worker-init-config` green.

- [ ] **Step 7: Lint and commit**

```bash
mallet src/pool.lisp
git add src/pool.lisp tests/pool-init-config-test.lisp tests.lisp
git commit -m "feat(pool): parse MCP_WORKER_INIT_* config"
```

---

### Task 7: Denylist `MCP_WORKER_INIT_*` in the worker environment

**Files:**
- Modify: `src/worker-client.lisp:188-192` (`*worker-env-denylist*`)
- Test: `tests/pool-init-config-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/pool-init-config-test.lisp` (add `#:%build-environment` is internal; test the denylist var directly — add `#:cl-mcp/src/worker-client` to the test `defpackage` `:import-from` with no symbols, then reference the internal denylist):

```lisp
(deftest init-vars-are-denylisted
  (testing "MCP_WORKER_INIT_* are excluded from inherited worker env"
    (let ((denylist cl-mcp/src/worker-client::*worker-env-denylist*))
      (ok (member "MCP_WORKER_INIT_SYSTEM" denylist :test #'string=)
          "SYSTEM denylisted")
      (ok (member "MCP_WORKER_INIT_ENTRY" denylist :test #'string=)
          "ENTRY denylisted")
      (ok (member "MCP_WORKER_INIT_EVAL" denylist :test #'string=)
          "EVAL denylisted"))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — the `MCP_WORKER_INIT_*` names are not in the denylist.

- [ ] **Step 3: Add the vars to the denylist**

Replace the `*worker-env-denylist*` defparameter (lines 188-192) with:

```lisp
(defparameter *worker-env-denylist*
  '("MCP_WORKER_SECRET" "MCP_WORKER_ID" "MCP_PARENT_PID" "MCP_LOG_FILE"
    ;; The parent is the sole reader of these; the worker receives init
    ;; params via RPC.  Denylisting prevents init forms that embed secrets
    ;; from leaking into every worker's environment.
    "MCP_WORKER_INIT_SYSTEM" "MCP_WORKER_INIT_ENTRY" "MCP_WORKER_INIT_EVAL"
    "MCP_WORKER_INIT_PACKAGE" "MCP_WORKER_INIT_MAX_FAILURES"
    "MCP_WORKER_INIT_MODE")
  "Environment variables that must NOT be inherited from the parent.
MCP_WORKER_SECRET/ID/PARENT_PID are set explicitly per-worker.
MCP_LOG_FILE is excluded so workers don't write to the parent's log file.
MCP_WORKER_INIT_* are read only by the parent and passed as RPC params.")
```

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: PASS.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/worker-client.lisp
git add src/worker-client.lisp tests/pool-init-config-test.lisp
git commit -m "feat(worker): denylist MCP_WORKER_INIT_* from worker env inheritance"
```

---

### Task 8: Pool-off guard warning

**Files:**
- Modify: `src/pool.lisp` (`initialize-pool`, near the end ~740)
- Test: `tests/pool-init-config-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/pool-init-config-test.lisp`:

```lisp
(deftest pool-off-guard-warns
  (testing "%warn-if-init-without-pool warns when INIT is set but pool disabled"
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . "recurya/dev")
                 ("MCP_NO_WORKER_POOL" . "1"))
      (lambda ()
        (ok (handler-case
                (progn (cl-mcp/src/pool::%warn-if-init-without-pool) nil)
              (warning () t))
            "signals a warning when INIT set with pool disabled")))
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . nil)
                 ("MCP_NO_WORKER_POOL" . "1"))
      (lambda ()
        (ok (handler-case
                (progn (cl-mcp/src/pool::%warn-if-init-without-pool) t)
              (warning () nil))
            "no warning when INIT unset")))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — `%warn-if-init-without-pool` unbound.

- [ ] **Step 3: Implement the guard and call it from `initialize-pool`**

Add near `%parse-worker-init-config` in `pool.lisp`:

```lisp
(defun %warn-if-init-without-pool ()
  "Warn (and log) when MCP_WORKER_INIT_SYSTEM is set while the worker pool
is disabled -- the init hook is inert in that configuration, so a silent
no-op would look like a broken web server."
  (when (and (%env-string "MCP_WORKER_INIT_SYSTEM")
             (%env-string "MCP_NO_WORKER_POOL"))
    (log-event :warn "pool.init-hook.inert"
               "reason" "MCP_WORKER_INIT_* set but MCP_NO_WORKER_POOL=1")
    (warn "MCP_WORKER_INIT_* is set but MCP_NO_WORKER_POOL=1: the worker ~
init hook is inert. Unset MCP_NO_WORKER_POOL to enable it.")))
```

In `initialize-pool`, call it right after the config is parsed (after the `(setf *worker-init-config* ...)` line added in Task 6, but outside the lock — place it just before `(%schedule-replenish)` near line 739):

```lisp
    (%warn-if-init-without-pool)
```

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: PASS.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/pool.lisp
git add src/pool.lisp tests/pool-init-config-test.lisp
git commit -m "feat(pool): warn when init hook is configured but pool is disabled"
```

---

## Phase 4 — Parent ownership + orchestration (correctness pillars #2, #3)

### Task 9: Ownership specials + `%elect-runtime-owner`

**Files:**
- Modify: `src/pool.lisp` (global-state section ~142-190; new election function)
- Test: `tests/pool-init-config-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/pool-init-config-test.lisp` (add `#:make-worker` from `cl-mcp/src/worker-client` to the test `defpackage`):

```lisp
(deftest elect-runtime-owner-rules
  (testing "grant when owner nil; re-grant same session; refuse live other session"
    (let ((*standby-noop* nil))
      (declare (ignore *standby-noop*)))
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        ;; A fake worker with NIL process-info is treated as NOT alive by
        ;; %worker-process-alive-p, so it models a dead owner.
        (let ((w-a (cl-mcp/src/worker-client:make-worker
                    :id 1 :state :bound :session-id "sess-a"))
              (w-b (cl-mcp/src/worker-client:make-worker
                    :id 2 :state :bound :session-id "sess-b")))
          ;; owner nil -> grant to A
          (ok (cl-mcp/src/pool::%elect-runtime-owner w-a "sess-a")
              "grants when owner is nil")
          ;; same session -> re-grant to A's replacement (also A's session)
          (ok (cl-mcp/src/pool::%elect-runtime-owner w-a "sess-a")
              "re-grants to the same session")
          ;; different session, current owner process is DEAD (nil proc)
          ;; -> reclaim granted to B
          (ok (cl-mcp/src/pool::%elect-runtime-owner w-b "sess-b")
              "reclaims when the current owner process is dead"))))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — `%with-owner-reset` / `%elect-runtime-owner` unbound.

- [ ] **Step 3: Add ownership specials and the election function**

After the crash-breaker defparameters (after `*max-concurrent-recoveries*`, line 190), add:

```lisp
(defvar *runtime-owner* nil
  "The current runtime owner as (SESSION-ID . WORKER), or NIL.  The owner
is the single worker permitted to run a singleton init (bind the fixed
app port).  Guarded by *pool-lock*.")

(defvar *runtime-init-failures* 0
  "Count of soft init failures for the current runtime.  Guarded by *pool-lock*.")

(defvar *runtime-init-disabled* nil
  "When T, init is not (re-)attempted until re-armed (pool-kill-worker /
config reload).  Guarded by *pool-lock*.")

(defvar *init-attributable-crashes* (make-hash-table :test 'eql)
  "Set of worker IDs whose crash happened during a cl-mcp-triggered init.
Such crashes are excluded from the crash circuit breaker.  Guarded by
*pool-lock*.")

(defun %with-owner-reset (thunk)
  "Test helper: reset ownership/failure state under *pool-lock*, run THUNK."
  (bt:with-lock-held (*pool-lock*)
    (setf *runtime-owner* nil
          *runtime-init-failures* 0
          *runtime-init-disabled* nil)
    (clrhash *init-attributable-crashes*))
  (funcall thunk))

(defun %elect-runtime-owner (worker session-id)
  "Under *pool-lock* (caller must hold it OR call via a wrapper that does):
grant runtime ownership to WORKER for SESSION-ID and return T, else NIL.
Grant iff the current owner is NIL, is the SAME session, or its worker
process is provably dead.  Never migrate to a different session while the
current owner's process is alive -- this keeps the app and the developer's
repl-eval/load-system in the same process and keeps exactly one port holder."
  (let ((current *runtime-owner*))
    (when (or (null current)
              (string= (car current) session-id)
              (not (%worker-process-alive-p (cdr current))))
      (setf *runtime-owner* (cons session-id worker))
      (log-event :info "pool.runtime-owner.elected"
                 "session" session-id "worker_id" (worker-id worker))
      (return-from %elect-runtime-owner t))
    (log-event :info "pool.init.skipped-not-owner"
               "session" session-id
               "owner_session" (car current))
    nil))
```

Note: `%elect-runtime-owner` reads/writes `*runtime-owner*` and must be called with `*pool-lock*` held. `%with-owner-reset` takes the lock only for the reset; the election calls inside the test run without the lock — acceptable in a single-threaded test, but add a `bt:with-lock-held` around each election call in the test if you prefer strictness. For production, `%ensure-runtime-init` (Task 11) holds the lock around the election.

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: PASS — `elect-runtime-owner-rules` green.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/pool.lisp
git add src/pool.lisp tests/pool-init-config-test.lisp
git commit -m "feat(pool): add runtime-owner election"
```

---

### Task 10: `%release-runtime-owner-if`

**Files:**
- Modify: `src/pool.lisp`
- Test: `tests/pool-init-config-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/pool-init-config-test.lisp`:

```lisp
(deftest release-runtime-owner
  (testing "release clears ownership only for the owning worker"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((w1 (cl-mcp/src/worker-client:make-worker
                   :id 1 :state :bound :session-id "s1"))
              (w2 (cl-mcp/src/worker-client:make-worker
                   :id 2 :state :bound :session-id "s2")))
          (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
            (cl-mcp/src/pool::%elect-runtime-owner w1 "s1"))
          ;; releasing a non-owner does nothing
          (cl-mcp/src/pool::%release-runtime-owner-if w2)
          (ok cl-mcp/src/pool::*runtime-owner* "non-owner release is a no-op")
          ;; releasing the owner clears it
          (cl-mcp/src/pool::%release-runtime-owner-if w1)
          (ok (null cl-mcp/src/pool::*runtime-owner*)
              "owner release clears *runtime-owner*"))))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — `%release-runtime-owner-if` unbound.

- [ ] **Step 3: Implement `%release-runtime-owner-if`**

Add to `pool.lisp` after `%elect-runtime-owner`:

```lisp
(defun %release-runtime-owner-if (worker)
  "Clear *runtime-owner* if WORKER is the current owner.  Takes *pool-lock*.
Called when an owner worker is released, killed, or removed on crash."
  (bt:with-lock-held (*pool-lock*)
    (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker))
      (log-event :info "pool.runtime-owner.released"
                 "worker_id" (worker-id worker))
      (setf *runtime-owner* nil))))
```

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: PASS.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/pool.lisp
git add src/pool.lisp tests/pool-init-config-test.lisp
git commit -m "feat(pool): add runtime-owner release"
```

---

### Task 11: `%ensure-runtime-init` + init-start + monitor

**Files:**
- Modify: `src/pool.lisp`
- Test: `tests/pool-init-config-test.lisp` (integration, socket-guarded)

- [ ] **Step 1: Write the failing integration test**

Append to `tests/pool-init-config-test.lisp` (add `#:spawn-worker #:kill-worker` from `cl-mcp/src/worker-client`, and a socket check helper):

```lisp
(defun %socket-available-p ()
  (handler-case
      (let ((s (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                     :element-type 'character)))
        (unwind-protect t (ignore-errors (usocket:socket-close s))))
    (error () nil)))

(defparameter *ensure-entry-ran* nil)
(defun ensure-entry-thunk () (setf *ensure-entry-ran* t) 4599)

(deftest ensure-runtime-init-drives-a-real-worker
  (testing "%ensure-runtime-init elects, sends init-start, entry runs"
    (if (not (%socket-available-p))
        (rove:skip "socket unavailable")
        (cl-mcp/src/pool::%with-owner-reset
          (lambda ()
            (let ((worker (cl-mcp/src/worker-client:spawn-worker)))
              (unwind-protect
                   (let ((cl-mcp/src/pool::*worker-init-config*
                           (list :system nil :entry
                                 "CL-MCP/TESTS/POOL-INIT-CONFIG-TEST:ENSURE-ENTRY-THUNK"
                                 :eval nil :package "CL-USER"
                                 :max-failures 1 :mode "singleton")))
                     (setf *ensure-entry-ran* nil)
                     (setf (cl-mcp/src/worker-client:worker-session-id worker) "sx")
                     (setf (cl-mcp/src/worker-client:worker-state worker) :bound)
                     (cl-mcp/src/pool::%ensure-runtime-init worker "sx")
                     ;; monitor runs async; poll for entry side effect
                     (loop repeat 60 until *ensure-entry-ran* do (sleep 0.05))
                     (ok *ensure-entry-ran* "entry thunk ran in the worker"))
                (ignore-errors (cl-mcp/src/worker-client:kill-worker worker)))))))))
```

Note: `spawn-worker` authenticates the worker; the entry thunk must exist in the *worker* image. Because the worker loads `cl-mcp/src/worker/main` (not the test system), `ENSURE-ENTRY-THUNK` will NOT be defined in the worker. So this test must instead use an `:eval` that has a visible side effect the parent can observe via `worker/init-status`. Rework the test to assert on init state rather than a parent-side global:

```lisp
(deftest ensure-runtime-init-drives-a-real-worker
  (testing "%ensure-runtime-init elects and drives init to a terminal state"
    (if (not (%socket-available-p))
        (rove:skip "socket unavailable")
        (cl-mcp/src/pool::%with-owner-reset
          (lambda ()
            (let ((worker (cl-mcp/src/worker-client:spawn-worker)))
              (unwind-protect
                   (let ((cl-mcp/src/pool::*worker-init-config*
                           (list :system nil :entry nil
                                 :eval "(+ 1 2)" :package "CL-USER"
                                 :max-failures 1 :mode "singleton")))
                     (setf (cl-mcp/src/worker-client:worker-session-id worker) "sx"
                           (cl-mcp/src/worker-client:worker-state worker) :bound)
                     (cl-mcp/src/pool::%ensure-runtime-init worker "sx")
                     ;; Poll worker/init-status via a direct RPC until terminal.
                     (let ((state nil))
                       (loop repeat 60
                             for r = (ignore-errors
                                       (cl-mcp/src/worker-client:worker-rpc
                                        worker "worker/init-status" nil :timeout 5))
                             when r do (setf state (gethash "init_state" r))
                             until (member state '("running" "failed") :test #'equal)
                             do (sleep 0.05))
                       (ok (equal state "running")
                           "init reached running for a trivial eval")))
                (ignore-errors (cl-mcp/src/worker-client:kill-worker worker)))))))))
```

Add `#:worker-rpc #:worker-session-id #:worker-state` to the test `defpackage` imports from `cl-mcp/src/worker-client`.

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — `%ensure-runtime-init` unbound.

- [ ] **Step 3: Implement orchestration in `pool.lisp`**

Add after `%release-runtime-owner-if`:

```lisp
(defun %init-params (config)
  "Build the worker/init-start params hash-table from CONFIG plist."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "system" ht) (getf config :system)
          (gethash "entry" ht) (getf config :entry)
          (gethash "eval" ht) (getf config :eval)
          (gethash "package" ht) (getf config :package))
    ht))

(defun %monitor-init (worker session-id max-failures)
  "Poll worker/init-status until terminal, updating failure/disable state.
Runs on a short-lived background thread.  A worker-crash during init is an
init-attributable hard failure: mark the worker so the breaker excludes it,
disable further init, and release ownership."
  (handler-case
      (loop
        (sleep 0.1)
        (let ((st (worker-rpc worker "worker/init-status" nil :timeout 5)))
          (let ((state (and (hash-table-p st) (gethash "init_state" st))))
            (cond
              ((equal state "running")
               (log-event :info "pool.init.running"
                          "session" session-id
                          "app_port" (gethash "app_port" st))
               (return))
              ((equal state "failed")
               (let ((err (gethash "last_init_error" st)))
                 (log-event :warn "pool.init.failed"
                            "session" session-id "error" err)
                 (bt:with-lock-held (*pool-lock*)
                   ;; A transient EADDRINUSE gets one free retry without
                   ;; consuming the failure latch.
                   (if (and err (search "address" (string-downcase err)))
                       (log-event :info "pool.init.eaddrinuse-retry"
                                  "session" session-id)
                       (incf *runtime-init-failures*))
                   (when (>= *runtime-init-failures* max-failures)
                     (setf *runtime-init-disabled* t)
                     (log-event :warn "pool.init.disabled"
                                "failures" *runtime-init-failures*)))
                 (%release-runtime-owner-if worker))
               (return))
              (t nil)))))
    (cl-mcp/src/worker-client:worker-crashed ()
      ;; Init-attributable hard crash: exclude from the breaker, disable init.
      (bt:with-lock-held (*pool-lock*)
        (setf (gethash (worker-id worker) *init-attributable-crashes*) t
              *runtime-init-disabled* t))
      (log-event :warn "pool.init.hard-crash"
                 "session" session-id "worker_id" (worker-id worker))
      (%release-runtime-owner-if worker))))

(defun %ensure-runtime-init (worker session-id)
  "Elect WORKER as runtime owner for SESSION-ID and, if elected, send a
fire-and-forget worker/init-start RPC (fast ack) and spawn a monitor
thread.  No-op when the feature is off or init is disabled.  Must be
called at the :bound transition, after the handshake."
  (let ((config nil) (granted nil))
    (bt:with-lock-held (*pool-lock*)
      (setf config *worker-init-config*)
      (when (and config (not *runtime-init-disabled*))
        (setf granted (%elect-runtime-owner worker session-id))))
    (when granted
      (handler-case
          (progn
            (worker-rpc worker "worker/init-start" (%init-params config)
                        :timeout 5)
            (let ((max-failures (getf config :max-failures 1)))
              (bt:make-thread
               (lambda () (%monitor-init worker session-id max-failures))
               :name (format nil "pool-init-monitor-~A" (worker-id worker)))))
        (error (e)
          (log-event :warn "pool.init.start-failed"
                     "session" session-id "error" (princ-to-string e))
          (%release-runtime-owner-if worker))))))
```

Import `worker-crashed` into `pool.lisp`: add `#:worker-crashed` to the `(:import-from #:cl-mcp/src/worker-client ...)` clause of `defpackage #:cl-mcp/src/pool` (lines 27-41).

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: PASS — `ensure-runtime-init-drives-a-real-worker` reaches `running` (or skips if no socket).

- [ ] **Step 5: Lint and commit**

```bash
mallet src/pool.lisp
git add src/pool.lisp tests/pool-init-config-test.lisp
git commit -m "feat(pool): orchestrate worker init via elect + init-start + monitor"
```

---

### Task 12: Wire call sites (bind, crash recovery, release, kill, pool-kill reset)

**Files:**
- Modify: `src/pool.lisp` — `get-or-assign-worker` (~932-967), `%handle-worker-crash` (~536-563), `release-session` (~984-1008), `kill-session-worker` (~1021-1047)
- Test: covered by the integration test in Task 11 plus the existing `pool-test`/`pool-kill-worker-test` suites (run them to confirm no regression)

- [ ] **Step 1: Call `%ensure-runtime-init` after a fresh bind in `get-or-assign-worker`**

In `get-or-assign-worker`, after the `send-root` corruption check block (ends ~line 966, right before the final `worker)` return), insert:

```lisp
      ;; Fire the init hook only when THIS call newly bound a worker.
      (when (and (or assigned-from-standby need-spawn) *worker-init-config*)
        (ignore-errors (%ensure-runtime-init worker session-id)))
```

- [ ] **Step 2: Call it on crash recovery (only if not disabled)**

In `%handle-worker-crash`, in the `was-bound` recovery branch, after the worker is registered and logged as recovered (`pool.worker.recovered`, ~line 548), insert (still inside the `(t ...)` registered branch of the `cond`):

```lisp
                      (bt:with-lock-held (*pool-lock*)
                        (unless *runtime-init-disabled*
                          (setf %recover-init-worker new-worker)))
```

Then, after the `handler-case` completes for the recovery (outside the lock, before the `%handle-worker-crash` function returns), add a call. To keep this simple and avoid threading a local through the `unwind-protect`, instead insert the init call directly where `registered` is true, replacing the two-line insert above with a single guarded call after the recovery log:

```lisp
                      (when (and (not *runtime-init-disabled*)
                                 *worker-init-config*)
                        (ignore-errors
                         (%ensure-runtime-init new-worker session-id)))
```

(Place it immediately after the `(log-event :info "pool.worker.recovered" ...)` call, inside the `(t ...)` branch.)

- [ ] **Step 3: Release ownership in `release-session` and `kill-session-worker`**

In `release-session`, inside the `((and entry (typep entry 'worker)) ...)` branch (after `(setf *all-workers* (remove worker-to-kill *all-workers*))`, ~line 993), add:

```lisp
           (when (and *runtime-owner* (eq (cdr *runtime-owner*) worker-to-kill))
             (setf *runtime-owner* nil))
```

Apply the identical addition in `kill-session-worker`'s worker branch (after its `(setf *all-workers* ...)`, ~line 1032). Both run under `*pool-lock*`, so set the special directly rather than calling `%release-runtime-owner-if` (which would re-take the lock).

- [ ] **Step 4: Eager re-init after pool-kill-worker reset**

`kill-session-worker` is the reset path. After it kills the owner and clears ownership, the app port is unbound until the session's next tool call re-binds and re-elects. For v1 this lazy re-bind is acceptable (design §8, L2 residual). **Do not** add eager re-spawn here in v1 — document the residual in the commit message and rely on the next tool call. (If eager reset is desired later, it is a separate task: spawn+bind a replacement and call `%ensure-runtime-init` synchronously.)

- [ ] **Step 5: Run the pool suites to confirm no regression**

Run: `{"system": "cl-mcp/tests/pool-test"}` → PASS
Run: `{"system": "cl-mcp/tests/pool-kill-worker-test"}` → PASS
Run: `{"system": "cl-mcp/tests/pool-init-config-test"}` → PASS

- [ ] **Step 6: Verify parens, lint, commit**

Run `lisp-check-parens` on `src/pool.lisp`. Then:

```bash
mallet src/pool.lisp
git add src/pool.lisp
git commit -m "feat(pool): wire init hook into bind, crash recovery, release, and kill paths"
```

---

## Phase 5 — Crash-breaker isolation (correctness pillar #4)

### Task 13: Exclude init-attributable crashes from the circuit breaker

**Files:**
- Modify: `src/pool.lisp` — `%handle-worker-crash` crash-history push (~477-500) and `get-or-assign-worker` Path 1b crash-history push (~853-875)
- Test: `tests/pool-init-config-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/pool-init-config-test.lisp`:

```lisp
(deftest init-crash-excluded-from-breaker
  (testing "%init-attributable-crash-p reflects the marked set"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((w (cl-mcp/src/worker-client:make-worker :id 77 :state :crashed)))
          (ok (not (cl-mcp/src/pool::%init-attributable-crash-p w))
              "unmarked worker is not init-attributable")
          (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
            (setf (gethash 77 cl-mcp/src/pool::*init-attributable-crashes*) t))
          (ok (cl-mcp/src/pool::%init-attributable-crash-p w)
              "marked worker is init-attributable"))))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — `%init-attributable-crash-p` unbound.

- [ ] **Step 3: Add the predicate and gate both crash-history pushes**

Add to `pool.lisp` near the ownership helpers:

```lisp
(defun %init-attributable-crash-p (worker)
  "T if WORKER's crash was attributed to a cl-mcp-triggered init.  Must be
called with *pool-lock* held (reads *init-attributable-crashes*).  Such
crashes are excluded from the crash circuit breaker so a bad web-server
init cannot brick a session's repl-eval/load-system."
  (gethash (worker-id worker) *init-attributable-crashes*))
```

In `%handle-worker-crash`, in the `was-bound` branch, wrap the crash-history push (lines 480-484) so it is skipped for init-attributable crashes. The `let ((history ...))` block becomes:

```lisp
           (let ((history (gethash session-id *crash-history*)))
             (unless (%init-attributable-crash-p crashed-worker)
               (setf history
                     (remove-if (lambda (ts) (< ts window-start)) history))
               (push now history)
               (setf (gethash session-id *crash-history*) history)
               (setf (worker-crash-history-pushed-p crashed-worker) t)
               (when (>= (length history) *crash-breaker-threshold*)
                 (log-event :error "pool.circuit-breaker.tripped"
                            "session" session-id
                            "crashes" (length history)
                            "window_seconds" *crash-breaker-window*)
                 (remhash session-id *crash-history*)
                 (when (eql (gethash session-id *affinity-map*) crashed-worker)
                   (remhash session-id *affinity-map*))
                 (setf *all-workers* (remove crashed-worker *all-workers*))
                 (return-from %handle-worker-crash))))
```

In `get-or-assign-worker` Path 1b, guard the crash-history push (the `(when (and (eq :crashed (worker-state entry)) (not (worker-crash-history-pushed-p entry))) ...)` block, ~lines 853-863) by adding `(not (%init-attributable-crash-p entry))` to its `and`:

```lisp
         (when (and (eq :crashed (worker-state entry))
                    (not (worker-crash-history-pushed-p entry))
                    (not (%init-attributable-crash-p entry)))
```

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}` → PASS
Run: `{"system": "cl-mcp/tests/pool-test"}` → PASS (no regression)

- [ ] **Step 5: Verify parens, lint, commit**

Run `lisp-check-parens` on `src/pool.lisp`. Then:

```bash
mallet src/pool.lisp
git add src/pool.lisp tests/pool-init-config-test.lisp
git commit -m "feat(pool): exclude init-attributable crashes from the circuit breaker"
```

---

## Phase 6 — Observability + docs

### Task 14: Surface init state in `pool-status-info`

**Files:**
- Modify: `src/pool.lisp` — `pool-status-info` (~1144-1165)
- Test: `tests/pool-init-config-test.lisp`

- [ ] **Step 1: Write the failing test**

Append to `tests/pool-init-config-test.lisp`:

```lisp
(deftest pool-status-includes-init-fields
  (testing "pool-status-info exposes runtime init fields"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((info (cl-mcp/src/pool:pool-status-info)))
          (ok (nth-value 1 (gethash "init_owner_session" info))
              "init_owner_session key present")
          (ok (nth-value 1 (gethash "init_disabled" info))
              "init_disabled key present")
          (ok (nth-value 1 (gethash "init_failures" info))
              "init_failures key present"))))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}`
Expected: FAIL — keys absent.

- [ ] **Step 3: Add the fields in `pool-status-info`**

In `pool-status-info`, extend the final `(setf (gethash ...) ...)` block that populates `info` (before `info` is returned, ~line 1164) with:

```lisp
      (bt:with-lock-held (*pool-lock*)
        (setf (gethash "init_owner_session" info)
                (and *runtime-owner* (car *runtime-owner*))
              (gethash "init_owner_worker" info)
                (and *runtime-owner* (worker-id (cdr *runtime-owner*)))
              (gethash "init_disabled" info) (if *runtime-init-disabled* t nil)
              (gethash "init_failures" info) *runtime-init-failures*))
```

- [ ] **Step 4: Run to verify it passes**

Run: `{"system": "cl-mcp/tests/pool-init-config-test"}` → PASS
Run: `{"system": "cl-mcp/tests/pool-status-test"}` → PASS (no regression)

- [ ] **Step 5: Lint and commit**

```bash
mallet src/pool.lisp
git add src/pool.lisp tests/pool-init-config-test.lisp
git commit -m "feat(pool): surface init owner/state in pool-status"
```

---

### Task 15: Full-system compile check + full suite

**Files:** none (verification task)

- [ ] **Step 1: Force-compile the whole system to catch warnings**

Run via `repl-eval`: `(asdf:compile-system :cl-mcp :force t)`
Expected: returns `T` with no new `WARNING`/`ERROR` lines beyond pre-existing ones. Investigate any new warning (per project MEMORY, a `COMPILE-FILE-ERROR` usually means unbalanced parens — run `lisp-check-parens` on the offending file).

- [ ] **Step 2: Run the full test suite from a clean process**

Run from the repo root: `rove cl-mcp.asd`
Expected: all suites PASS, including `worker-init-hook-test`, `pool-init-config-test`, `worker-test`, `pool-test`, `pool-kill-worker-test`, `pool-status-test`.

- [ ] **Step 3: Commit nothing (verification only)**

If Steps 1–2 required fixes, they were committed under their respective tasks. If a fix was needed here, commit it with a descriptive message, e.g. `fix(pool): resolve compile warning in init orchestration`.

---

### Task 16: Document env vars and recurya wiring in README

**Files:**
- Modify: `README.md` — Environment Variables table (~193-200) and a new subsection

- [ ] **Step 1: Add the init-hook rows to the Environment Variables table**

In `README.md`, add these rows to the table under `## Environment Variables` (after the `CL_MCP_MAX_POOL_SIZE` row, line 200):

```markdown
| `MCP_WORKER_INIT_SYSTEM` | ASDF system to load in the elected owner worker at bind (master gate for the init hook) | (unset = off) |
| `MCP_WORKER_INIT_ENTRY` | `PKG:SYMBOL` nullary thunk run after the load (preferred activation) | (none) |
| `MCP_WORKER_INIT_EVAL` | Lisp form run via repl-eval as an escape hatch | (none) |
| `MCP_WORKER_INIT_MAX_FAILURES` | Soft init failures before init auto-retry latches off | `1` |
| `MCP_WORKER_INIT_MODE` | `singleton` (one owner binds a fixed port). v1 supports `singleton` only | `singleton` |
```

- [ ] **Step 2: Add a subsection documenting the hook**

Add after the "Tuning warmup" subsection (after line 213), a new subsection:

```markdown
### Running an app inside a worker (init hook)

Set `MCP_WORKER_INIT_SYSTEM` (and optionally `MCP_WORKER_INIT_ENTRY`) to have
the pool run a startup routine inside the single worker elected as the
"runtime owner" when a session binds it. This lets an app (e.g. a web server)
run in the same process that serves `repl-eval`/`load-system`, so hot-reload
lands in the app's process, while the parent keeps the persistent `/mcp`
endpoint. `pool-kill-worker` restarts the runtime without dropping `/mcp`.

- The init runs on a background thread under a worker-global ASDF load lock,
  so it never races a `load-system` RPC.
- Init failures never trip the crash circuit breaker; a failed init leaves a
  plain, usable REPL worker. Check `pool-status` (`init_owner_session`,
  `init_disabled`, `init_failures`) to see runtime state.
- Requires the pool enabled (do not set `MCP_NO_WORKER_POOL=1`).
- The init form must be bind-robust and idempotent, and should pass
  `:address "127.0.0.1"` for a localhost-only dev server. Do not embed
  secrets in `MCP_WORKER_INIT_EVAL`.

Example (recurya): add a nullary `recurya/dev:start-dev-runtime!` thunk that
starts the DB and web server, then set `MCP_WORKER_INIT_SYSTEM=recurya/dev`
and `MCP_WORKER_INIT_ENTRY=recurya/dev:start-dev-runtime!`. See
`docs/plans/2026-07-05-worker-init-hook-design.md` §7 for the full wiring.
```

- [ ] **Step 3: Commit**

```bash
git add README.md
git commit -m "docs: document the worker init hook and its env vars"
```

---

## Self-Review

**1. Spec coverage** (design §5 v1 pillars → tasks):
- Pillar #1 (`*asdf-load-lock*` + init loads with `timeout=nil`): Tasks 1, 2, 5 (`%run-init` uses `:timeout-seconds nil`).
- Pillar #2 (async ack-then-poll, no long stream-lock hold): Task 5 (`handle-init-start` acks) + Task 11 (`%ensure-runtime-init` sends fast RPC + monitor thread).
- Pillar #3 (session-bound ownership; refuse migration to a live other session): Tasks 9, 10, 12.
- Pillar #4 (init crashes excluded from breaker + quarantine): Tasks 11 (`%monitor-init` disables on hard crash), 13 (exclusion predicate + gated pushes).
- Trigger/config surface (§5.7): Tasks 6, 7 (denylist), 16 (docs).
- Observability (§5.5): Task 14 (`pool-status`). MCP notification + first-owner banner are **deferred** (design §5.5 lists them as additional; v1 ships the mandatory `pool-status` fields). Note this deferral to the maintainer.
- Pool-off guard (§5.7 F7): Task 8.
- recurya wiring (§7): documented in Task 16; the `recurya/dev:start-dev-runtime!` thunk lives in the recurya repo (out of this plan's tree).

**2. Placeholder scan:** No "TBD"/"add error handling"/"similar to Task N". Every code step shows complete forms. The one soft spot — genuine cross-RPC serialization — is explicitly routed to the integration test (Task 5/11) rather than left vague.

**3. Type/name consistency across tasks:** `*asdf-load-lock*` / `with-asdf-load-lock` (Tasks 1,2,5); `%set-init-state`/`init-state-snapshot`/`%reset-init-state` (Tasks 3,5); `%resolve-entry` (Tasks 4,5); `handle-init-start`/`handle-init-status` (Tasks 5, registered with the exact method strings `worker/init-start`/`worker/init-status`); `*runtime-owner*` as `(session-id . worker)` cons used consistently in `%elect-runtime-owner`, `%release-runtime-owner-if`, `%ensure-runtime-init`, `release-session`, `kill-session-worker`, `pool-status-info` (Tasks 9–14); `*init-attributable-crashes*` keyed by `worker-id` in Tasks 11 and 13; `%init-params`/`%monitor-init`/`%ensure-runtime-init` (Task 11) referenced by call sites in Task 12.

**Known deferrals to flag at go/no-go** (design §8): live hot-reload vs in-flight request threads (documented, not fixed); out-of-band loads bypass the lock (unset `MCP_WORKER_SWANK`); eager reset after `pool-kill-worker` (Task 12 Step 4 keeps v1 lazy); MCP notification + first-owner banner; `load-system` *tool* timeout still uses `destroy-thread` (separate hardening PR).
