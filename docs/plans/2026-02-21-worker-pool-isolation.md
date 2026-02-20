# Worker Pool Isolation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Isolate Lisp-image-dependent operations (repl-eval, load-system, run-tests, code-*) into child SBCL processes with automatic crash recovery, enabling fault-tolerant MCP server operation.

**Architecture:** Parent MCP server delegates eval/introspect operations to a pool of SBCL child processes via JSON-RPC over TCP. Each MCP session exclusively owns one worker. Workers are pre-warmed as standbys. Crashes trigger automatic restart with explicit state-reset notification to the AI agent.

**Tech Stack:** SBCL, bordeaux-threads, usocket, uiop (launch-program), yason, Swank (dynamic load)

**Design Doc:** `docs/plans/2026-02-21-worker-pool-isolation-design.md`

**Key Reference Files:**
- `prompts/repl-driven-development.md` — Tool usage guide (REPL-driven workflow)
- `agents/common-lisp-expert.md` — CL style, TDD with Rove, CLOS patterns

---

## Phase 1: Core Separation (Refactoring Only)

Extract pure logic from tool-definition files into `*-core.lisp` files. **No behavior change.** All existing tests must continue to pass after each task.

### Task 1: Extract repl-core.lisp

**Files:**
- Create: `src/repl-core.lisp`
- Modify: `src/repl.lisp`

**Step 1: Create `src/repl-core.lisp` with extracted logic**

Create a new file containing `%do-repl-eval`, `repl-eval`, and all their private helpers. The `define-tool` form stays in `repl.lisp`.

```lisp
;;;; src/repl-core.lisp
;;;;
;;;; Core REPL evaluation logic, shared between parent and worker processes.

(defpackage #:cl-mcp/src/repl-core
  (:use #:cl)
  (:import-from #:uiop #:print-backtrace)
  (:import-from #:bordeaux-threads
                #:thread-alive-p
                #:make-thread
                #:destroy-thread)
  (:import-from #:cl-mcp/src/frame-inspector #:capture-error-context)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:repl-eval
           #:*default-eval-package*))

(in-package #:cl-mcp/src/repl-core)

;; Move here from repl.lisp:
;;   *default-eval-package*
;;   %read-all
;;   ftype declaim for repl-eval
;;   %sanitize-control-chars
;;   %truncate-output
;;   %resolve-eval-package
;;   %call-with-compiler-streams
;;   %eval-forms
;;   %do-repl-eval
;;   %repl-eval-with-timeout
;;   repl-eval
```

Note: `repl-core.lisp` does NOT import from `cl-mcp/src/tools/helpers`, `cl-mcp/src/tools/define-tool`, `cl-mcp/src/object-registry`, or `cl-mcp/src/inspect`. Those are only needed by the tool definition wrapper in `repl.lisp`.

**Step 2: Modify `src/repl.lisp` to import from repl-core**

`repl.lisp` becomes a thin wrapper: it imports `repl-eval` from `repl-core` and contains only the `define-tool "repl-eval"` form plus the response-building logic (object registry, preview generation, error context formatting).

```lisp
(defpackage #:cl-mcp/src/repl
  (:use #:cl)
  (:import-from #:cl-mcp/src/repl-core
                #:repl-eval
                #:*default-eval-package*)
  (:import-from #:cl-mcp/src/object-registry #:inspectable-p #:register-object)
  (:import-from #:cl-mcp/src/inspect #:generate-result-preview)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:repl-eval #:*default-eval-package*))
```

The `define-tool "repl-eval"` body remains unchanged — it calls `repl-eval` (now imported from `repl-core`).

**Step 3: Run all tests**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
```

Expected: All PASS. No behavior change.

**Step 4: Lint**

```bash
mallet src/repl-core.lisp src/repl.lisp
```

**Step 5: Commit**

```bash
git add src/repl-core.lisp src/repl.lisp
git commit -m "refactor: extract repl-core.lisp from repl.lisp

Separate pure evaluation logic (repl-eval, %do-repl-eval, timeout wrapper)
from MCP tool definition. No behavior change."
```

---

### Task 2: Extract code-core.lisp

**Files:**
- Create: `src/code-core.lisp`
- Modify: `src/code.lisp`

**Step 1: Create `src/code-core.lisp` with extracted logic**

Move all sb-introspect logic and the three exported functions (`code-find-definition`, `code-describe-symbol`, `code-find-references`) plus all `%` private helpers.

```lisp
;;;; src/code-core.lisp
;;;;
;;;; Core code intelligence logic (sb-introspect), shared between parent and worker.

(defpackage #:cl-mcp/src/code-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/project-root #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:normalize-path-for-display #:path-inside-p)
  (:import-from #:uiop
                #:read-file-string #:ensure-pathname
                #:ensure-directory-pathname #:absolute-pathname-p)
  (:export #:code-find-definition
           #:code-describe-symbol
           #:code-find-references))

(in-package #:cl-mcp/src/code-core)

;; Move here from code.lisp:
;;   %ensure-package
;;   %parse-symbol
;;   %ensure-sb-introspect
;;   %sb-introspect-symbol
;;   %offset->line
;;   code-find-definition
;;   code-describe-symbol
;;   %path-inside-project-p
;;   %line-snippet
;;   %definition->path/line
;;   %finder->type
;;   code-find-references
```

**Step 2: Modify `src/code.lisp` to import from code-core**

```lisp
(defpackage #:cl-mcp/src/code
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-core
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:code-find-definition
           #:code-describe-symbol
           #:code-find-references))
```

Only the three `define-tool` forms remain in `code.lisp`.

**Step 3: Run tests, lint, commit**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
mallet src/code-core.lisp src/code.lisp
git add src/code-core.lisp src/code.lisp
git commit -m "refactor: extract code-core.lisp from code.lisp

Separate sb-introspect logic from MCP tool definitions. No behavior change."
```

---

### Task 3: Extract system-loader-core.lisp

**Files:**
- Create: `src/system-loader-core.lisp`
- Modify: `src/system-loader.lisp`

**Step 1: Create `src/system-loader-core.lisp`**

Move `%load-with-timeout`, `%call-with-suppressed-output`, and `load-system` function.

```lisp
(defpackage #:cl-mcp/src/system-loader-core
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:thread-alive-p #:make-thread #:destroy-thread)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:cl-mcp/src/utils/sanitize #:sanitize-for-json)
  (:export #:load-system))

;; Move: %load-with-timeout, %call-with-suppressed-output, load-system
```

**Step 2: Modify `src/system-loader.lisp`**

Import `load-system` from `system-loader-core`. Keep only the `define-tool "load-system"` form.

**Step 3: Run tests, lint, commit**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
mallet src/system-loader-core.lisp src/system-loader.lisp
git add src/system-loader-core.lisp src/system-loader.lisp
git commit -m "refactor: extract system-loader-core.lisp from system-loader.lisp"
```

---

### Task 4: Extract test-runner-core.lisp

**Files:**
- Create: `src/test-runner-core.lisp`
- Modify: `src/test-runner.lisp`

**Step 1: Create `src/test-runner-core.lisp`**

Move all functions except the `define-tool "run-tests"` form: `detect-test-framework`, `make-test-result`, `make-failure-detail`, all `%rove-*` helpers, `run-rove-*`, `run-asdf-fallback`, `run-tests`.

```lisp
(defpackage #:cl-mcp/src/test-runner-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:export #:run-tests #:detect-test-framework))

;; Move all logic here
```

**Step 2: Modify `src/test-runner.lisp`**

Import from `test-runner-core`. Keep only `define-tool "run-tests"`.

**Step 3: Run tests, lint, commit**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
mallet src/test-runner-core.lisp src/test-runner.lisp
git add src/test-runner-core.lisp src/test-runner.lisp
git commit -m "refactor: extract test-runner-core.lisp from test-runner.lisp"
```

---

### Task 5: Phase 1 verification

**Step 1: Run the full test suite**

```bash
rove cl-mcp.asd
```

**Step 2: Start the server and verify manual operation**

```lisp
(ql:quickload :cl-mcp)
(cl-mcp:start-http-server :port 3000)
;; Test repl-eval, code-find, load-system via curl or MCP client
```

**Step 3: Commit phase completion marker**

No code change. Just verify everything works.

---

## Phase 2: Worker Process

Build the SBCL child process that accepts JSON-RPC over TCP and executes Lisp operations.

### Task 6: Create worker TCP server

**Files:**
- Create: `src/worker/server.lisp`

**Step 1: Write test for worker server**

```lisp
;; tests/worker-test.lisp
(defpackage #:cl-mcp/tests/worker-test
  (:use #:cl #:rove))

(in-package #:cl-mcp/tests/worker-test)

(deftest worker-server-accepts-connection
  ;; Start a worker server on port 0, connect, send ping, get pong
  (let* ((server (cl-mcp/src/worker/server:start-server :port 0))
         (port (cl-mcp/src/worker/server:server-port server)))
    (unwind-protect
        (let ((socket (usocket:socket-connect "127.0.0.1" port)))
          (unwind-protect
              (let ((stream (usocket:socket-stream socket)))
                ;; Send a JSON-RPC ping
                (write-line "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"worker/ping\"}" stream)
                (force-output stream)
                (let ((response (yason:parse (read-line stream))))
                  (ok (gethash "result" response))
                  (ok (equal (gethash "id" response) 1))))
            (usocket:socket-close socket)))
      (cl-mcp/src/worker/server:stop-server server))))
```

**Step 2: Run test to verify it fails**

```bash
rove tests/worker-test.lisp
```
Expected: FAIL (package/file not found)

**Step 3: Implement worker server**

Create `src/worker/server.lisp` with:
- TCP listener on dynamic port (`:port 0`)
- Single-connection accept loop
- JSON-RPC line-delimited protocol (reuse yason for encode/decode)
- Method dispatch table
- `worker/ping` handler (returns `{"pong": true}`)

**Step 4: Run test to verify it passes**

```bash
rove tests/worker-test.lisp
```
Expected: PASS

**Step 5: Lint and commit**

```bash
mallet src/worker/server.lisp
git add src/worker/server.lisp tests/worker-test.lisp
git commit -m "feat: add worker TCP server with JSON-RPC protocol"
```

---

### Task 7: Create worker handlers

**Files:**
- Create: `src/worker/handlers.lisp`
- Modify: `src/worker/server.lisp` (register handlers)

**Step 1: Write tests for worker handlers**

Add to `tests/worker-test.lisp`:

```lisp
(deftest worker-eval-returns-result
  ;; Send worker/eval, expect evaluated result
  ;; {"method":"worker/eval","params":{"code":"(+ 1 2)","package":"CL-USER"}}
  ;; Expected: result contains "content" with "3"
  ...)

(deftest worker-eval-returns-error-on-bad-code
  ;; {"method":"worker/eval","params":{"code":"(error \"boom\")","package":"CL-USER"}}
  ;; Expected: result contains error info, isError true
  ...)

(deftest worker-code-describe-returns-info
  ;; First load cl via worker/load-system, then worker/code-describe for "cl:car"
  ...)

(deftest worker-set-project-root
  ;; {"method":"worker/set-project-root","params":{"path":"/tmp"}}
  ;; Verify *project-root* changed
  ...)
```

**Step 2: Implement handlers**

Create `src/worker/handlers.lisp`:
- `worker/eval` → calls `repl-eval` from `repl-core`
- `worker/load-system` → calls `load-system` from `system-loader-core`
- `worker/run-tests` → calls `run-tests` from `test-runner-core`
- `worker/code-find` → calls `code-find-definition` from `code-core`
- `worker/code-describe` → calls `code-describe-symbol` from `code-core`
- `worker/code-find-references` → calls `code-find-references` from `code-core`
- `worker/inspect-object` → calls `inspect-object-by-id` from `inspect`
- `worker/set-project-root` → sets `*project-root*`
- `worker/ping` → returns pong

Each handler:
1. Extracts params from the JSON-RPC request
2. Calls the core function
3. Builds a JSON-RPC response hash-table
4. Returns it for serialization

**Important:** The response format must match what the existing `define-tool` handlers produce. The worker handler for `worker/eval` must build the same hash-table structure (with `content`, `stdout`, `stderr`, `result_object_id`, `result_preview`, `error_context`) as the current `define-tool "repl-eval"` body does. Move that response-building logic into a shared helper or duplicate it in the worker handler.

**Step 3: Run tests, lint, commit**

```bash
rove tests/worker-test.lisp
mallet src/worker/handlers.lisp
git add src/worker/handlers.lisp tests/worker-test.lisp
git commit -m "feat: add worker method handlers for eval, code-*, load-system"
```

---

### Task 8: Create worker entry point with Swank

**Files:**
- Create: `src/worker/main.lisp`

**Step 1: Write test for worker startup and handshake**

Add to `tests/worker-test.lisp`:

```lisp
(deftest worker-process-starts-and-reports-ports
  ;; Launch worker via uiop:launch-program
  ;; Read stdout for JSON with tcp_port, swank_port, pid
  ;; Connect to tcp_port
  ;; Send ping, get pong
  ;; Kill process
  ...)
```

**Step 2: Implement worker main**

Create `src/worker/main.lisp`:
- `(defun start ()` — entry point
  1. Start TCP server on port 0
  2. Dynamically load Swank via `(ql:quickload :swank :silent t)` + `find-symbol`
  3. Start Swank on port 0 (graceful degradation if unavailable)
  4. Output JSON to stdout: `{"tcp_port": N, "swank_port": N, "pid": N}`
  5. `force-output`
  6. Accept connection and enter serve loop

- Read `MCP_PROJECT_ROOT` from environment and set `*project-root*`
- Install SIGTERM handler for graceful shutdown

**Step 3: Run test, lint, commit**

```bash
rove tests/worker-test.lisp
mallet src/worker/main.lisp
git add src/worker/main.lisp tests/worker-test.lisp
git commit -m "feat: add worker entry point with Swank and stdout port reporting"
```

---

## Phase 3: Pool Manager and Proxy

### Task 9: Create worker-client (parent→worker RPC)

**Files:**
- Create: `src/worker-client.lisp`

**Step 1: Write tests**

Create `tests/pool-test.lisp`:

```lisp
(deftest worker-client-sends-rpc-and-receives-response
  ;; Start a real worker process
  ;; Create a worker-client connection
  ;; Send worker/ping via worker-rpc
  ;; Verify pong response
  ...)

(deftest worker-client-handles-timeout
  ;; Send worker/eval with infinite loop and short timeout
  ;; Verify timeout error is returned
  ;; Verify worker process is killed
  ...)
```

**Step 2: Implement worker-client**

Create `src/worker-client.lisp`:

```lisp
(defpackage #:cl-mcp/src/worker-client
  (:use #:cl)
  (:import-from #:bordeaux-threads #:make-lock #:with-lock-held)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:worker
           #:make-worker-from-process
           #:worker-rpc
           #:worker-tcp-port
           #:worker-swank-port
           #:worker-pid
           #:worker-state
           #:worker-session-id
           #:worker-needs-reset-notification-p
           #:clear-reset-notification
           #:kill-worker
           #:spawn-worker))
```

Key functions:
- `spawn-worker` — `uiop:launch-program`, read stdout JSON, TCP connect, return worker struct
- `worker-rpc` — with `stream-lock`, send JSON-RPC, read response (timeout via socket options)
- `kill-worker` — SIGTERM, wait 2s, SIGKILL

Worker struct:
```lisp
(defstruct worker
  (id 0 :type fixnum)
  (state :dead :type keyword)
  (process-info nil)
  (stream nil)
  (socket nil)
  (stream-lock (bt:make-lock "worker-stream"))
  (tcp-port 0 :type fixnum)
  (swank-port nil)
  (pid 0 :type fixnum)
  (needs-reset-notification nil :type boolean)
  (session-id nil)
  (request-counter 0 :type fixnum))
```

**Step 3: Run tests, lint, commit**

```bash
rove tests/pool-test.lisp
mallet src/worker-client.lisp
git add src/worker-client.lisp tests/pool-test.lisp
git commit -m "feat: add worker-client for parent→worker JSON-RPC communication"
```

---

### Task 10: Create pool manager

**Files:**
- Create: `src/pool.lisp`

**Step 1: Write tests**

Add to `tests/pool-test.lisp`:

```lisp
(deftest pool-assigns-worker-to-session
  ;; Initialize pool with warmup=1
  ;; get-or-assign-worker for session "A"
  ;; Verify a worker is returned in :bound state
  ...)

(deftest pool-reuses-worker-for-same-session
  ;; get-or-assign-worker "A" twice
  ;; Verify same worker returned
  ...)

(deftest pool-assigns-different-workers-to-different-sessions
  ;; get-or-assign-worker "A" and "B"
  ;; Verify different workers
  ...)

(deftest pool-scales-out-when-standbys-exhausted
  ;; warmup=1, assign "A" (takes standby), assign "B" (must spawn new)
  ;; Verify both get workers
  ...)

(deftest pool-releases-worker-on-session-end
  ;; Assign worker, then release-session
  ;; Verify worker is killed and standby is replenished
  ...)

(deftest pool-crash-recovery-sets-reset-flag
  ;; Get worker, kill its process externally
  ;; Trigger health check or next RPC
  ;; Verify needs-reset-notification is set on restarted worker
  ...)
```

**Step 2: Implement pool manager**

Create `src/pool.lisp`:

```lisp
(defpackage #:cl-mcp/src/pool
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held
                #:make-condition-variable #:condition-wait #:condition-broadcast
                #:make-thread)
  (:import-from #:cl-mcp/src/worker-client ...)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:*use-worker-pool*
           #:*worker-pool-warmup*
           #:initialize-pool
           #:shutdown-pool
           #:get-or-assign-worker
           #:release-session
           #:pool-worker-info))
```

Key components:
- `*pool-lock*` — global mutex
- `*affinity-map*` — session-id → worker hash-table
- `*standby-workers*` — list of ready workers
- `worker-placeholder` struct with condvar (per design doc section 5)
- `get-or-assign-worker` — three-path: affinity hit, standby pop, spawn with placeholder
- `spawn-and-bind-worker` — with `unwind-protect` cleanup (per design doc)
- `release-session` — kill worker, replenish standby
- Health monitor thread (heartbeat ping)
- `pool-worker-info` — returns worker metadata for `fs-get-project-info`

**Step 3: Run tests, lint, commit**

```bash
rove tests/pool-test.lisp
mallet src/pool.lisp
git add src/pool.lisp tests/pool-test.lisp
git commit -m "feat: add worker pool manager with strict session affinity"
```

---

### Task 11: Add session ID to transports

**Files:**
- Modify: `src/protocol.lisp`
- Modify: `src/http.lisp`
- Modify: `src/tcp.lisp`
- Modify: `src/run.lisp`

**Step 1: Add `current-session-id` mechanism**

Create a dynamic variable `*current-session-id*` that each transport binds before calling `process-json-line`.

In `src/protocol.lisp` or a new small utility:
```lisp
(defvar *current-session-id* nil
  "Bound by each transport to the current session identifier.")
```

**Step 2: Bind in HTTP transport**

In `src/http.lisp`, `%handle-mcp-post-session` already has the session ID from the `Mcp-Session-Id` header. Bind `*current-session-id*` before calling `process-json-line`.

**Step 3: Bind in TCP transport**

In `src/tcp.lisp`, `%process-stream` generates a `conn-id`. Use `(format nil "tcp-~A" conn-id)` as the session ID. Bind `*current-session-id*`.

**Step 4: Bind in stdio transport**

In `src/run.lisp`, bind `*current-session-id*` to `"stdio"` at the start of the loop.

**Step 5: Run tests, lint, commit**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
mallet src/protocol.lisp src/http.lisp src/tcp.lisp src/run.lisp
git add src/protocol.lisp src/http.lisp src/tcp.lisp src/run.lisp
git commit -m "feat: thread session ID through all transports"
```

---

### Task 12: Convert tool definitions to proxies

**Files:**
- Modify: `src/repl.lisp`
- Modify: `src/code.lisp`
- Modify: `src/system-loader.lisp`
- Modify: `src/test-runner.lisp`
- Create: `src/proxy.lisp` (shared proxy helper)

**Step 1: Create proxy helper**

```lisp
;; src/proxy.lisp
(defpackage #:cl-mcp/src/proxy
  (:use #:cl)
  (:import-from #:cl-mcp/src/pool
                #:*use-worker-pool*
                #:get-or-assign-worker)
  (:import-from #:cl-mcp/src/worker-client
                #:worker-rpc
                #:worker-needs-reset-notification-p
                #:clear-reset-notification)
  (:import-from #:cl-mcp/src/tools/helpers #:tool-error)
  (:export #:proxy-to-worker))

(in-package #:cl-mcp/src/proxy)

(defvar *current-session-id* nil)  ;; or import from protocol

(defun proxy-to-worker (method params id)
  "Proxy a tool call to the session's dedicated worker."
  (let* ((session-id *current-session-id*)
         (worker (get-or-assign-worker session-id)))
    (cond
      ((worker-needs-reset-notification-p worker)
       (clear-reset-notification worker)
       (tool-error id "Worker process crashed and was restarted. All Lisp state (loaded systems, defined functions, package state) has been reset. Please run load-system again to restore your environment."))
      (t
       (worker-rpc worker method params)))))
```

**Step 2: Modify each tool file**

In each `define-tool` body, add the proxy branch:

```lisp
;; Example for repl.lisp:
(define-tool "repl-eval" ...
  :body
  (if *use-worker-pool*
      (proxy-to-worker "worker/eval"
                       (make-ht "code" code
                                "package" package
                                "print_level" print-level
                                ...)
                       id)
      ;; Original inline logic (fallback)
      (let ((... (repl-eval code ...)))
        ...)))
```

Apply the same pattern to `code.lisp` (3 tools), `system-loader.lisp`, `test-runner.lisp`.

**Step 3: Run tests with `*use-worker-pool*` = nil (fallback)**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
```
Expected: All PASS (using fallback path)

**Step 4: Lint and commit**

```bash
mallet src/proxy.lisp src/repl.lisp src/code.lisp src/system-loader.lisp src/test-runner.lisp
git add src/proxy.lisp src/repl.lisp src/code.lisp src/system-loader.lisp src/test-runner.lisp
git commit -m "feat: add proxy layer for worker delegation with fallback mode"
```

---

### Task 13: Update fs-get-project-info with worker metadata

**Files:**
- Modify: `src/fs.lisp`

**Step 1: Add worker info to project-info response**

When `*use-worker-pool*` is true, include a `"workers"` key in the `fs-get-project-info` response with each worker's `tcp_port`, `swank_port`, `pid`, `state`, `session`, `uptime_seconds`.

```lisp
;; In the define-tool "fs-get-project-info" body:
(when *use-worker-pool*
  (setf (gethash "workers" ht)
        (pool-worker-info)))  ;; from pool.lisp
```

**Step 2: Run tests, lint, commit**

```bash
rove tests/fs-test.lisp
mallet src/fs.lisp
git add src/fs.lisp
git commit -m "feat: expose worker pool info in fs-get-project-info"
```

---

### Task 14: Update main.lisp and http.lisp

**Files:**
- Modify: `main.lisp`
- Modify: `src/http.lisp`

**Step 1: Update main.lisp imports**

Add imports for pool.lisp exports and re-export the pool control symbols:

```lisp
(:import-from #:cl-mcp/src/pool
              #:*use-worker-pool*
              #:*worker-pool-warmup*
              #:initialize-pool
              #:shutdown-pool)
(:export ... #:*use-worker-pool* #:*worker-pool-warmup*
             #:initialize-pool #:shutdown-pool ...)
```

**Step 2: Update http.lisp for session cleanup**

In `delete-session` and `stop-http-server`, call `release-session` from pool.lisp:

```lisp
;; In delete-session:
(when *use-worker-pool*
  (release-session session-id))

;; In stop-http-server:
(when *use-worker-pool*
  (shutdown-pool))
```

**Step 3: Initialize pool on server start**

In `start-http-server` or as a separate call:
```lisp
(when *use-worker-pool*
  (initialize-pool))
```

**Step 4: Run tests, lint, commit**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
mallet main.lisp src/http.lisp
git add main.lisp src/http.lisp
git commit -m "feat: integrate pool lifecycle with HTTP server and main exports"
```

---

## Phase 4: Integration and Verification

### Task 15: End-to-end integration tests

**Files:**
- Modify: `tests/pool-test.lisp`

**Step 1: Write integration tests**

```lisp
(deftest e2e-repl-eval-through-worker
  ;; Initialize pool
  ;; Simulate an HTTP session (bind *current-session-id*)
  ;; Call the define-tool "repl-eval" handler
  ;; Verify result comes back correctly
  ...)

(deftest e2e-crash-recovery-returns-reset-notification
  ;; Assign worker to session
  ;; Kill worker process externally (SIGKILL)
  ;; Wait for health monitor to detect crash
  ;; Send repl-eval
  ;; Verify reset notification message
  ;; Send repl-eval again
  ;; Verify normal result
  ...)

(deftest e2e-timeout-kills-and-restarts-worker
  ;; Send worker/eval with (loop) and timeout 1s
  ;; Verify timeout error returned
  ;; Send worker/eval with (+ 1 2)
  ;; Verify reset notification
  ;; Send worker/eval with (+ 3 4)
  ;; Verify normal "7"
  ...)

(deftest e2e-fallback-mode-works
  ;; Set *use-worker-pool* to nil
  ;; Call repl-eval tool
  ;; Verify it works inline without workers
  ...)
```

**Step 2: Run integration tests**

```bash
rove tests/pool-test.lisp
```

**Step 3: Commit**

```bash
git add tests/pool-test.lisp
git commit -m "test: add end-to-end integration tests for worker pool"
```

---

### Task 16: Concurrent access tests

**Files:**
- Modify: `tests/pool-test.lisp`

**Step 1: Write concurrency tests**

```lisp
(deftest concurrent-sessions-get-different-workers
  ;; Spawn 3 threads, each with different session ID
  ;; Each calls get-or-assign-worker
  ;; Verify all get distinct workers
  ...)

(deftest concurrent-requests-same-session-serialize-on-stream-lock
  ;; Assign worker to session
  ;; Spawn 3 threads sending worker/eval simultaneously
  ;; Verify all get valid responses (no JSON corruption)
  ...)

(deftest placeholder-condvar-wakes-all-waiters
  ;; Set warmup=0 (no standbys)
  ;; Spawn 3 threads with same session ID calling get-or-assign-worker
  ;; First thread spawns worker (slow), other 2 wait on condvar
  ;; Verify all 3 threads get the same worker
  ...)
```

**Step 2: Run tests, commit**

```bash
rove tests/pool-test.lisp
git add tests/pool-test.lisp
git commit -m "test: add concurrency tests for pool manager"
```

---

### Task 17: Full suite verification and cleanup

**Step 1: Run complete test suite**

```bash
rove cl-mcp.asd
```

**Step 2: Lint all changed/new files**

```bash
mallet src/repl-core.lisp src/code-core.lisp src/system-loader-core.lisp src/test-runner-core.lisp src/pool.lisp src/worker-client.lisp src/proxy.lisp src/worker/main.lisp src/worker/server.lisp src/worker/handlers.lisp
```

**Step 3: Manual smoke test**

```lisp
(ql:quickload :cl-mcp)
(cl-mcp:start-http-server :port 3000)
;; From another terminal, use curl or Claude Code to:
;; 1. Initialize session
;; 2. Call repl-eval (+ 1 2) — verify "3"
;; 3. Call load-system — verify success
;; 4. Call code-describe for "cl:car" — verify response
;; 5. Check fs-get-project-info — verify workers array with swank_port
;; 6. Connect to swank_port via M-x slime-connect — verify works
```

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: worker pool isolation for eval stability

Delegate repl-eval, load-system, run-tests, and code-* operations
to isolated SBCL child processes. Each MCP session exclusively owns
one worker. Workers auto-restart on crash with explicit state-reset
notification. Swank servers in each worker enable human observation.

Includes fallback mode (*use-worker-pool* nil) for inline execution."
```

---

## Appendix: Risk Mitigation

| Risk | Mitigation |
|---|---|
| Worker startup is slow (~2-5s for SBCL+quickload) | Warm standby pool pre-starts workers |
| `uiop:launch-program` behavior varies by OS | Test on Linux (primary target); SBCL+Roswell specific |
| Swank load fails in minimal environments | Graceful degradation; worker runs without Swank |
| Port 0 assignment fails (extremely rare) | Retry logic in spawn-worker |
| `bordeaux-threads:condition-wait` spurious wakeup | Loop on state check (already in design) |
| Existing tests break after refactoring | Phase 1 is pure refactoring; run tests after every step |
