# Design: pool-status Tool & notifications/cancelled Handler

**Date:** 2026-03-05
**Branch:** feat/worker-pool-isolation
**Status:** Approved

## Overview

Two features for worker pool observability and control:

1. **pool-status** — MCP tool exposing pool diagnostics
2. **notifications/cancelled** — Cancel running evaluations by killing the worker

## Feature 1: pool-status Tool

### Summary

Expose existing `pool-worker-info` as an MCP tool with pool-level summary.
Runs in parent process (no proxy needed).

### Tool Schema

```json
{
  "name": "pool-status",
  "description": "Return worker pool diagnostic information including per-worker details and pool-level summary.",
  "inputSchema": {"type": "object", "properties": {}}
}
```

### Response Structure

```json
{
  "content": [{"type": "text", "text": "Pool: 2 workers (1 bound, 1 standby)"}],
  "pool_running": true,
  "total_workers": 2,
  "standby_count": 1,
  "bound_count": 1,
  "max_pool_size": 16,
  "warmup_target": 1,
  "workers": [
    {"id": 1, "session": "abc12345...", "tcp_port": 54321, "pid": 12345, "state": "bound"},
    {"id": 2, "session": null, "tcp_port": 54322, "pid": 12346, "state": "standby"}
  ]
}
```

### Behavior When Pool Disabled

When `MCP_NO_WORKER_POOL=1`: returns `pool_running: false, workers: []`.

### Files Changed

- `src/pool.lisp` — Add `define-tool "pool-status"` (colocated with `pool-worker-info`)

### Estimated Size

~30 lines.

## Feature 2: notifications/cancelled Handler

### Summary

When MCP client sends `notifications/cancelled` with a `requestId`, kill the
worker handling that request. The pool's existing crash recovery auto-spawns
a replacement. Client sees crash-notification on next tool call.

### Architecture

```
notifications/cancelled {requestId: 42}
  |
  v
handle-notification() in protocol.lisp
  | calls cancel-request(42)
  v
proxy.lisp: lookup *active-requests* map
  requestId=42 -> session="abc..."
  | resolve session -> worker
  v
worker-client:kill-worker(worker)
  |
  v
pool auto-recovery spawns replacement
  |
  v
client sees crash-notification on next call
```

### Component 1: Active Request Registry (src/proxy.lisp)

New state in proxy layer:

```lisp
(defvar *active-requests* (make-hash-table :test 'equal)
  "Maps MCP request-id to session-id for in-flight proxied requests.")
(defvar *active-requests-lock* (make-lock "active-requests-lock"))
```

- `proxy-to-worker` registers `(request-id -> session-id)` before RPC call
- `proxy-to-worker` unregisters on return (normal or error, via `unwind-protect`)
- Thread-safe: one lock, small critical section

New exported function:

```lisp
(defun cancel-request (request-id)
  "Cancel a proxied request by killing its worker.
Returns T if a matching request was found and cancelled, NIL otherwise."
  ...)
```

### Component 2: Request ID Threading

**Change:** `proxy-to-worker` signature from `(method params)` to `(id method params)`.

**Impact:**
- `with-proxy-dispatch` macro already binds `id` — just pass it through
- All tool handlers use the macro, so **no source changes** to repl.lisp, code.lisp, etc.
- Only `src/proxy.lisp` changes (function + macro)

### Component 3: Notification Handler (src/protocol.lisp)

Extend `handle-notification`:

```lisp
(defun handle-notification (state method params)
  (cond
    ((string= method "notifications/initialized") nil)
    ((string= method "notifications/cancelled")
     (%handle-cancel-notification params))
    (t nil)))
```

`%handle-cancel-notification`:
1. Extract `requestId` from params
2. Call `cancel-request` (exported from proxy)
3. Log the cancellation

### Dependency Direction

```
protocol.lisp -> proxy.lisp (cancel-request)   [new, same direction as existing]
proxy.lisp -> pool.lisp, worker-client.lisp     [existing]
```

No new reverse dependencies. Protocol already imports from proxy.

### Edge Cases

| Scenario | Behavior |
|----------|----------|
| Cancel after request completes | No-op (request-id not in map) |
| Cancel before worker assigned | Safe: proxy-to-worker handles worker-crashed on RPC |
| Multiple cancels for same ID | Second is no-op |
| Cancel for non-proxied tool | No-op (not in active-requests map) |
| Worker already dead | kill-worker is robust against dead workers |
| Pool disabled | No-op (no active requests registered) |

### Files Changed

- `src/proxy.lisp` — *active-requests*, cancel-request, proxy-to-worker signature, with-proxy-dispatch macro
- `src/protocol.lisp` — handle-notification extension, %handle-cancel-notification

### Estimated Size

~60 lines new code.

## Testing Strategy

### pool-status

- Unit test: pool-status returns correct structure with pool running
- Unit test: pool-status with pool disabled returns empty

### notifications/cancelled

- Unit test: active-requests registration/unregistration in proxy-to-worker
- Unit test: cancel-request finds and kills correct worker
- Unit test: cancel-request no-op for unknown request-id
- Integration test: full cancel flow (send tool call, send cancel notification, verify worker killed)
- Edge case: cancel after completion (no-op)
