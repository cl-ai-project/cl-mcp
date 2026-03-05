# pool-status & notifications/cancelled Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a pool-status diagnostic MCP tool and implement notifications/cancelled to cancel running evaluations by killing the worker.

**Architecture:** Two independent features. pool-status is a simple define-tool wrapping existing pool-worker-info. notifications/cancelled adds an active-requests registry in proxy.lisp, extends protocol.lisp's notification handler, and threads the MCP request ID through proxy-to-worker so cancel can map request → session → worker → kill. The pool's existing crash recovery auto-spawns a replacement.

**Tech Stack:** Common Lisp, Rove (tests), ASDF package-inferred-system, bordeaux-threads

---

### Task 1: pool-status Tool — Tests

**Files:**
- Create: `tests/pool-status-test.lisp`

**Step 1: Create the test file with package definition and tests**

```lisp
;;;; tests/pool-status-test.lisp
;;;;
;;;; Tests for pool-status MCP tool.

(defpackage #:cl-mcp/tests/pool-status-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/pool
                #:*worker-pool-warmup*
                #:*health-check-interval-seconds*
                #:*shutdown-replenish-wait-seconds*
                #:initialize-pool
                #:shutdown-pool
                #:pool-worker-info)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-tool-handler)
  (:import-from #:cl-mcp/src/state
                #:make-state))

(in-package #:cl-mcp/tests/pool-status-test)

(defun spawn-available-p ()
  "Check if we can spawn worker processes."
  (ignore-errors
    (let ((p (sb-ext:run-program "ros" '("version")
               :search t :output :stream :wait nil)))
      (prog1 (sb-ext:process-alive-p p)
        (ignore-errors (sb-ext:process-kill p 15))
        (ignore-errors (sb-ext:process-close p))))))

(defmacro with-pool ((&key (health-check-interval 60.0d0)) &body body)
  `(let ((*worker-pool-warmup* 0)
         (*health-check-interval-seconds* ,health-check-interval)
         (*shutdown-replenish-wait-seconds* 0.01d0))
     (unwind-protect
         (progn (initialize-pool) ,@body)
       (shutdown-pool))))

(deftest pool-status-tool-registered
  (testing "pool-status tool is registered in the tool registry"
    (ok (functionp (get-tool-handler "pool-status")))))

(deftest pool-status-returns-structure-when-pool-disabled
  (testing "pool-status returns correct structure when pool is disabled"
    (let* ((*use-worker-pool* nil)
           (handler (get-tool-handler "pool-status"))
           (state (make-state))
           ;; handler signature: (state id args)
           (response (funcall handler state 1 nil)))
      ;; response is a JSON-RPC result wrapper
      (let ((result (gethash "result" response)))
        (ok (hash-table-p result))
        (ok (equal (gethash "pool_running" result) nil))
        (ok (equal (gethash "total_workers" result) 0))
        (ok (equal (gethash "standby_count" result) 0))
        (ok (equal (gethash "bound_count" result) 0))
        (ok (arrayp (gethash "workers" result)))
        (ok (zerop (length (gethash "workers" result))))))))

(deftest pool-status-returns-structure-when-pool-running
  (testing "pool-status returns correct structure with running pool"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (with-pool ()
      (let* ((handler (get-tool-handler "pool-status"))
             (state (make-state))
             (response (funcall handler state 1 nil)))
        (let ((result (gethash "result" response)))
          (ok (hash-table-p result))
          (ok (equal (gethash "pool_running" result) t))
          (ok (numberp (gethash "total_workers" result)))
          (ok (numberp (gethash "standby_count" result)))
          (ok (numberp (gethash "bound_count" result)))
          (ok (numberp (gethash "max_pool_size" result)))
          (ok (numberp (gethash "warmup_target" result)))
          (ok (arrayp (gethash "workers" result)))
          (ok (gethash "content" result)))))))
```

Write this file using `fs-write-file`. Then verify parens with `lisp-check-parens`.

**Step 2: Run test to verify it fails**

Run: `rove tests/pool-status-test.lisp`
Expected: FAIL — "pool-status" tool not yet registered, `get-tool-handler` returns NIL.

**Step 3: Commit failing tests**

```bash
git add tests/pool-status-test.lisp
git commit -m "test: add pool-status tool tests (red)"
```

---

### Task 2: pool-status Tool — Implementation

**Files:**
- Modify: `src/pool.lisp` (add define-tool after pool-worker-info, line ~1032)
- Modify: `src/pool.lisp` defpackage (add import for define-tool)

**Step 1: Add define-tool import to pool.lisp defpackage**

In `src/pool.lisp`, add to the defpackage form:

```lisp
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:result)
```

Use `lisp-edit-form` with form_type `defpackage` and form_name `#:cl-mcp/src/pool`.

**Step 2: Add the pool-status tool definition after pool-worker-info**

Insert after the `defun pool-worker-info` form:

```lisp
(define-tool "pool-status"
  :description "Return worker pool diagnostic information including per-worker
details and pool-level summary.  No arguments required.
Returns pool_running, total_workers, standby_count, bound_count,
max_pool_size, warmup_target, and a workers array."
  :args ()
  :body
  (let* ((running *pool-running*)
         (workers (if running (pool-worker-info) (vector)))
         (standby-count 0)
         (bound-count 0))
    (when running
      (with-lock-held (*pool-lock*)
        (setf standby-count (length *standby-workers*)
              bound-count (- (length *all-workers*)
                             (length *standby-workers*)))))
    (result id
            (make-ht "content"
                     (text-content
                      (format nil "Pool: ~D workers (~D bound, ~D standby)~@[ [stopped]~]"
                              (length workers) bound-count standby-count
                              (not running)))
                     "pool_running" (if running t nil)
                     "total_workers" (length workers)
                     "standby_count" standby-count
                     "bound_count" bound-count
                     "max_pool_size" *max-pool-size*
                     "warmup_target" *worker-pool-warmup*
                     "workers" workers))))
```

Use `lisp-edit-form` with operation `insert_after`, form_type `defun`, form_name `pool-worker-info`.

**Step 3: Verify parens**

Run `lisp-check-parens` on `src/pool.lisp`.

**Step 4: Run tests to verify they pass**

Run: `rove tests/pool-status-test.lisp`
Expected: ALL PASS

**Step 5: Run existing pool tests to verify no regressions**

Run: `rove tests/pool-test.lisp`
Expected: ALL PASS

**Step 6: Lint**

Run: `mallet src/pool.lisp`

**Step 7: Commit**

```bash
git add src/pool.lisp tests/pool-status-test.lisp
git commit -m "feat: add pool-status MCP diagnostic tool"
```

---

### Task 3: Active Request Registry — Tests

**Files:**
- Create: `tests/cancel-test.lisp`

These tests verify the active-requests registry and cancel-request function in isolation (no worker spawning needed for the unit tests).

**Step 1: Create the test file**

```lisp
;;;; tests/cancel-test.lisp
;;;;
;;;; Tests for notifications/cancelled handling.

(defpackage #:cl-mcp/tests/cancel-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:*active-requests*
                #:*active-requests-lock*
                #:cancel-request)
  (:import-from #:bordeaux-threads
                #:with-lock-held))

(in-package #:cl-mcp/tests/cancel-test)

;;; --- Unit tests for the active-requests registry ---

(deftest active-requests-initially-empty
  (testing "active-requests map starts empty"
    (with-lock-held (*active-requests-lock*)
      (ok (zerop (hash-table-count *active-requests*))))))

(deftest cancel-request-unknown-id-returns-nil
  (testing "cancel-request returns NIL for unknown request ID"
    (ok (null (cancel-request "nonexistent-id-999")))))

(deftest cancel-request-known-id-returns-t
  (testing "cancel-request returns T and removes entry for known request ID"
    ;; Manually insert an entry to simulate an in-flight request.
    ;; We use a fake session-id; cancel-request will call kill on the
    ;; worker but we expect it to gracefully handle the case where
    ;; no worker is bound (pool not running).
    (with-lock-held (*active-requests-lock*)
      (setf (gethash "test-req-42" *active-requests*) "fake-session-id"))
    (unwind-protect
        ;; cancel-request should find the entry.  The actual kill may
        ;; fail (no pool running) but cancel-request should still
        ;; return T after removing the entry and logging.
        (let ((result (cancel-request "test-req-42")))
          (ok (eq result t))
          ;; Verify entry was removed
          (with-lock-held (*active-requests-lock*)
            (ok (null (gethash "test-req-42" *active-requests*)))))
      ;; Cleanup in case test fails early
      (with-lock-held (*active-requests-lock*)
        (remhash "test-req-42" *active-requests*)))))
```

Write with `fs-write-file`, verify with `lisp-check-parens`.

**Step 2: Run test to verify it fails**

Run: `rove tests/cancel-test.lisp`
Expected: FAIL — symbols `*active-requests*`, `cancel-request` not exported from proxy.

**Step 3: Commit failing tests**

```bash
git add tests/cancel-test.lisp
git commit -m "test: add cancel-request unit tests (red)"
```

---

### Task 4: Active Request Registry — Implementation

**Files:**
- Modify: `src/proxy.lisp` defpackage — add exports
- Modify: `src/proxy.lisp` — add state vars, cancel-request function
- Modify: `src/proxy.lisp` — update with-proxy-dispatch macro
- Modify: `src/proxy.lisp` — update proxy-to-worker signature and body

**Step 1: Update proxy.lisp defpackage exports**

Add these exports to the defpackage form in `src/proxy.lisp`:

```lisp
           #:cancel-request
           #:*active-requests*
           #:*active-requests-lock*
```

Use `lisp-edit-form` on the defpackage.

**Step 2: Add active-requests state variables**

Insert after the `*proxy-rpc-timeout*` defvar (before the `with-proxy-dispatch` macro):

```lisp
(defvar *active-requests* (make-hash-table :test 'equal)
  "Maps MCP request-id (string or integer) to session-id for in-flight
proxied requests.  Used by cancel-request to find which worker to kill.")

(defvar *active-requests-lock* (make-lock "active-requests-lock")
  "Lock protecting *active-requests*.")
```

Use `lisp-edit-form` insert_after on defvar `*proxy-rpc-timeout*`.

**Step 3: Add bordeaux-threads import for make-lock/with-lock-held**

The proxy.lisp defpackage needs:

```lisp
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held)
```

Add to the defpackage via `lisp-edit-form`.

**Step 4: Update with-proxy-dispatch macro to pass id to proxy-to-worker**

Replace the macro to pass `id` as first argument:

```lisp
(defmacro with-proxy-dispatch ((id method params-form) &body inline-body)
  "When *use-worker-pool* is non-nil, proxy the tool call to a worker
process and wrap the result for JSON-RPC.  Otherwise execute INLINE-BODY.
METHOD is a string like \"worker/eval\".  PARAMS-FORM builds the
arguments hash-table.  ID is the JSON-RPC request id."
  `(if *use-worker-pool*
       (result ,id (proxy-to-worker ,id ,method ,params-form))
       (progn ,@inline-body)))
```

Use `lisp-edit-form` replace on defmacro `with-proxy-dispatch`.

**Step 5: Update proxy-to-worker signature and add request tracking**

Replace the entire `proxy-to-worker` defun. The key changes are:
1. New `id` parameter: `(defun proxy-to-worker (id method params)`
2. `unwind-protect` wrapping the RPC call to register/unregister in `*active-requests*`

```lisp
(defun proxy-to-worker (id method params)
  "Proxy a tool call to the session's dedicated worker process.
Returns the worker's JSON-RPC result hash-table directly.
Registers the request in *active-requests* so notifications/cancelled
can map request-id to session and kill the worker.
Uses atomic check-and-clear for crash notification to prevent
TOCTOU race with concurrent requests for the same session."
  (let ((session-id *current-session-id*))
    (unless (and (stringp session-id) (plusp (length session-id)))
      (error "Cannot proxy tool call: no session ID bound."))
    (%ensure-cached-bindings)
    ;; Register this request for cancellation support
    (let ((request-key (princ-to-string id)))
      (with-lock-held (*active-requests-lock*)
        (setf (gethash request-key *active-requests*) session-id))
      (unwind-protect
          (let* ((worker
                  (handler-case
                      (funcall %cached-get-or-assign% session-id)
                    (error (e)
                      (log-event :warn "proxy.pool-error"
                                 "session" session-id
                                 "method" method
                                 "error" (princ-to-string e))
                      (return-from proxy-to-worker
                        (make-ht "content"
                                 (text-content
                                  (format nil "Pool error: ~A"
                                          (sanitize-error-message
                                           (princ-to-string e))))
                                 "isError" t)))))
                 (worker-crashed-sym %cached-worker-crashed-sym%))
            (cond
              ((funcall %cached-check-and-clear% worker)
               (log-event :info "proxy.crash-notification"
                          "session" session-id
                          "method" method)
               (%crash-notification-result))
              (t
               (log-event :debug "proxy.forward"
                          "session" session-id
                          "method" method)
               (let* ((user-timeout
                       (and (hash-table-p params)
                            (gethash "timeout_seconds" params)))
                      (effective-timeout
                       (if (and user-timeout (numberp user-timeout)
                                (plusp user-timeout))
                           (max *proxy-rpc-timeout*
                                (ceiling (+ user-timeout 30)))
                           *proxy-rpc-timeout*)))
                 (handler-case
                     (funcall %cached-worker-rpc% worker method params
                              :timeout effective-timeout)
                   (error (e)
                     (cond
                       ((typep e worker-crashed-sym)
                        (let ((reason
                               (ignore-errors
                                 (funcall %cached-worker-crashed-reason% e))))
                          (cond
                            ((and reason (string= reason "timeout"))
                             (log-event :warn "proxy.worker-timeout"
                                        "session" session-id
                                        "method" method)
                             (make-ht "content"
                                      (text-content
                                       (concatenate 'string
                                         "Worker RPC timed out. "
                                         "The operation took too long and the worker "
                                         "was terminated. All Lisp state has been reset. "
                                         "Please run load-system again to restore "
                                         "your environment."))
                                      "isError" t))
                            (t
                             (log-event :warn
                                        "proxy.worker-crashed-mid-request"
                                        "session" session-id
                                        "method" method
                                        "error" (princ-to-string e))
                             (%crash-notification-result)))))
                       (t
                        (log-event :debug "proxy.worker-rpc-error"
                                   "session" session-id
                                   "method" method
                                   "error" (princ-to-string e))
                        (make-ht "content"
                                 (text-content
                                  (format nil "Worker error: ~A"
                                          (sanitize-error-message
                                           (princ-to-string e))))
                                 "isError" t)))))))))
        ;; Always unregister the request when done (normal or error)
        (with-lock-held (*active-requests-lock*)
          (remhash request-key *active-requests*))))))
```

Use `lisp-edit-form` replace on defun `proxy-to-worker`.

**Step 6: Add cancel-request function**

Insert after `proxy-to-worker`:

```lisp
(defun cancel-request (request-id)
  "Cancel a proxied request by killing its worker.
Looks up REQUEST-ID in the active-requests map, resolves the
session's worker, and kills it.  The pool's crash recovery
auto-spawns a replacement.
Returns T if a matching request was found, NIL otherwise."
  (let ((request-key (princ-to-string request-id))
        (session-id nil))
    (with-lock-held (*active-requests-lock*)
      (setf session-id (gethash request-key *active-requests*))
      (when session-id
        (remhash request-key *active-requests*)))
    (unless session-id
      (log-event :debug "proxy.cancel.not-found"
                 "request_id" request-key)
      (return-from cancel-request nil))
    (log-event :info "proxy.cancel.killing-worker"
               "request_id" request-key
               "session" session-id)
    (%ensure-cached-bindings)
    (handler-case
        (let ((worker (funcall %cached-get-or-assign% session-id)))
          (let ((kill-fn (fdefinition
                          (%resolve "CL-MCP/SRC/WORKER-CLIENT"
                                    "KILL-WORKER"))))
            (funcall kill-fn worker))
          (log-event :info "proxy.cancel.worker-killed"
                     "request_id" request-key
                     "session" session-id))
      (error (e)
        (log-event :warn "proxy.cancel.error"
                   "request_id" request-key
                   "session" session-id
                   "error" (princ-to-string e))))
    t))
```

Use `lisp-edit-form` insert_after on defun `proxy-to-worker`.

**Step 7: Verify parens and lint**

Run `lisp-check-parens` on `src/proxy.lisp`.
Run `mallet src/proxy.lisp`.

**Step 8: Run cancel tests**

Run: `rove tests/cancel-test.lisp`
Expected: ALL PASS

**Step 9: Run existing pool tests for regressions**

Run: `rove tests/pool-test.lisp`
Expected: ALL PASS

**Step 10: Commit**

```bash
git add src/proxy.lisp tests/cancel-test.lisp
git commit -m "feat: add active-request registry and cancel-request in proxy layer"
```

---

### Task 5: notifications/cancelled Protocol Handler — Tests

**Files:**
- Modify: `tests/cancel-test.lisp` — add protocol-level tests

**Step 1: Add protocol-level test to cancel-test.lisp**

Add to the existing `tests/cancel-test.lisp`:

```lisp
;;; --- Protocol-level notification tests ---

(deftest handle-cancelled-notification-dispatches
  (testing "notifications/cancelled calls cancel-request with requestId"
    ;; Seed an active request entry
    (with-lock-held (*active-requests-lock*)
      (setf (gethash "proto-req-7" *active-requests*) "fake-session"))
    (unwind-protect
        (let* ((params (let ((ht (make-hash-table :test 'equal)))
                         (setf (gethash "requestId" ht) "proto-req-7")
                         ht))
               ;; Call handle-notification directly
               (state (make-state)))
          (handle-notification state "notifications/cancelled" params)
          ;; Verify the entry was removed (cancel-request was called)
          (with-lock-held (*active-requests-lock*)
            (ok (null (gethash "proto-req-7" *active-requests*)))))
      (with-lock-held (*active-requests-lock*)
        (remhash "proto-req-7" *active-requests*)))))

(deftest handle-cancelled-notification-unknown-id-is-noop
  (testing "notifications/cancelled for unknown requestId is a no-op"
    (let* ((params (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "requestId" ht) "unknown-req-999")
                     ht))
           (state (make-state)))
      ;; Should not error
      (ok (null (handle-notification state "notifications/cancelled" params))))))
```

This requires adding imports for `handle-notification` and `make-state` to the test package. Update the defpackage:

```lisp
  (:import-from #:cl-mcp/src/protocol
                #:handle-notification)
  (:import-from #:cl-mcp/src/state
                #:make-state)
```

Use `lisp-edit-form` to update the defpackage and insert_after to add the tests.

**Step 2: Run tests to verify they fail**

Run: `rove tests/cancel-test.lisp`
Expected: FAIL — `handle-notification` doesn't handle "notifications/cancelled" yet.

**Step 3: Commit**

```bash
git add tests/cancel-test.lisp
git commit -m "test: add protocol-level notifications/cancelled tests (red)"
```

---

### Task 6: notifications/cancelled Protocol Handler — Implementation

**Files:**
- Modify: `src/protocol.lisp` defpackage — add import from proxy
- Modify: `src/protocol.lisp` — extend handle-notification
- Modify: `src/protocol.lisp` — add %handle-cancel-notification

**Step 1: Add proxy import to protocol.lisp defpackage**

Add to the defpackage:

```lisp
  (:import-from #:cl-mcp/src/proxy
                #:cancel-request)
```

Note: protocol.lisp already imports from `cl-mcp/src/pool`. Adding a proxy import follows the same dependency direction (protocol → proxy → pool).

Use `lisp-edit-form` on the defpackage.

**Step 2: Add %handle-cancel-notification helper**

Insert before `handle-notification`:

```lisp
(defun %handle-cancel-notification (params)
  "Handle a notifications/cancelled message from the MCP client.
Extracts requestId from PARAMS and calls cancel-request to kill
the worker handling that request."
  (let ((request-id (and (hash-table-p params)
                         (gethash "requestId" params))))
    (when request-id
      (log-event :info "protocol.cancel-notification"
                 "request_id" request-id)
      (cancel-request request-id)))
  nil)
```

Use `lisp-edit-form` insert_before on defun `handle-notification`.

**Step 3: Update handle-notification to dispatch cancelled**

Replace `handle-notification`:

```lisp
(defun handle-notification (state method params)
  "Handle incoming JSON-RPC notifications (no response expected).
Dispatches notifications/initialized and notifications/cancelled."
  (declare (ignore state))
  (cond
    ((string= method "notifications/initialized") nil)
    ((string= method "notifications/cancelled")
     (%handle-cancel-notification params))
    (t nil)))
```

Use `lisp-edit-form` replace on defun `handle-notification`.

**Step 4: Export handle-notification from protocol package**

It's already accessible via the package-inferred system. Check if the test can import it. The test imports `#:cl-mcp/src/protocol #:handle-notification` — this symbol needs to be exported.

Add `#:handle-notification` to protocol.lisp's `:export` list in the defpackage.

**Step 5: Verify parens and lint**

Run `lisp-check-parens` on `src/protocol.lisp`.
Run `mallet src/protocol.lisp`.

**Step 6: Run all cancel tests**

Run: `rove tests/cancel-test.lisp`
Expected: ALL PASS

**Step 7: Run all existing tests for regressions**

Run: `rove tests/pool-test.lisp tests/protocol-test.lisp`
Expected: ALL PASS

**Step 8: Commit**

```bash
git add src/protocol.lisp tests/cancel-test.lisp
git commit -m "feat: implement notifications/cancelled handler in protocol layer"
```

---

### Task 7: Integration Test — Cancel Kills Worker

**Files:**
- Modify: `tests/cancel-test.lisp` — add e2e test

**Step 1: Add integration test**

This test spawns a real worker, sends a long-running eval, cancels it, and verifies the worker is killed.

```lisp
;;; --- Integration test: full cancel flow ---

(deftest cancel-kills-running-worker-e2e
  (testing "Full cancel flow: send eval, cancel, verify worker killed"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (with-pool ()
      (let* ((session-id "cancel-test-session")
             (*current-session-id* session-id)
             (worker (get-or-assign-worker session-id))
             (worker-pid (worker-pid worker)))
        ;; Register a fake in-flight request
        (with-lock-held (*active-requests-lock*)
          (setf (gethash "e2e-req-1" *active-requests*) session-id))
        ;; Cancel the request — should kill the worker
        (cancel-request "e2e-req-1")
        ;; Give the kill process a moment
        (sleep 0.5)
        ;; Worker should be dead
        (ok (eq (worker-state worker) :dead)
            "Worker state should be :dead after cancel")
        ;; Entry should be removed from active-requests
        (with-lock-held (*active-requests-lock*)
          (ok (null (gethash "e2e-req-1" *active-requests*))
              "Active request entry should be removed"))))))
```

Add necessary imports to the test defpackage:

```lisp
  (:import-from #:cl-mcp/src/pool
                #:*worker-pool-warmup*
                #:*health-check-interval-seconds*
                #:*shutdown-replenish-wait-seconds*
                #:initialize-pool
                #:shutdown-pool
                #:get-or-assign-worker)
  (:import-from #:cl-mcp/src/worker-client
                #:worker-state #:worker-pid)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
```

Also add the `spawn-available-p` helper and `with-pool` macro (same pattern as pool-test.lisp).

**Step 2: Run test**

Run: `rove tests/cancel-test.lisp`
Expected: ALL PASS

**Step 3: Commit**

```bash
git add tests/cancel-test.lisp
git commit -m "test: add e2e integration test for cancel-kills-worker flow"
```

---

### Task 8: Final Verification

**Step 1: Run all tests**

```bash
rove tests/pool-status-test.lisp tests/cancel-test.lisp tests/pool-test.lisp tests/protocol-test.lisp
```

Expected: ALL PASS

**Step 2: Lint all changed files**

```bash
mallet src/proxy.lisp src/protocol.lisp src/pool.lisp
```

**Step 3: Verify pool-status tool works end-to-end via REPL**

```lisp
(ql:quickload :cl-mcp)
(cl-mcp:initialize-pool)
;; Use the pool-status tool handler directly:
(funcall (cl-mcp/src/tools/registry:get-tool-handler "pool-status")
         (cl-mcp/src/state:make-state) 1 nil)
(cl-mcp:shutdown-pool)
```

**Step 4: Final commit if any cleanup needed, then done**
