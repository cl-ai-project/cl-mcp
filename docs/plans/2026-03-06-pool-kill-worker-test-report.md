# Comprehensive Test Report: pool-kill-worker

## Test Configuration
- **Perspectives used**: End User, Edge Case Explorer, Security, Error Handling, Maintainability
- **Date**: 2026-03-06
- **Target**: `pool-kill-worker` MCP tool and `kill-session-worker` pool function
- **Files under test**:
  - `src/pool.lisp` — `kill-session-worker` function
  - `src/tools/pool-kill-worker.lisp` — MCP tool definition
  - `src/tools/all.lisp` — tool registration wiring

## Summary

| Severity | Count | Fixed |
|----------|-------|-------|
| Critical | 0     | —     |
| Major    | 3     | 3/3   |
| Minor    | 9     | 5/9   |
| **Total**| **12**| **8/12** |

## Major Issues (All Fixed)

### M1. Missing SIGTERM before kill — deadlock on in-flight RPC
- **Flagged by**: Security, Maintainability
- **Location**: `src/pool.lisp:989-993`
- **Description**: `kill-session-worker` called `kill-worker` directly, which attempts to acquire `stream-lock`. If a concurrent `worker-rpc` holds the lock (blocked on worker response), `kill-worker` blocks up to 300s (`*proxy-rpc-timeout*`). The `cancel-request` function already solves this by calling `signal-worker-terminate` (SIGTERM) first to break the TCP pipe.
- **Fix**: Added `signal-worker-terminate` import and call before `kill-worker` in `kill-session-worker`, mirroring `cancel-request`'s pattern.

### M2. Not documented in agent-facing prompts
- **Flagged by**: End User, Maintainability
- **Location**: `prompts/repl-driven-development.md`, `CLAUDE.md`
- **Description**: The tool was absent from the Quick Reference cheat sheet, the Worker Pool Architecture section, and the Troubleshooting section. AI agents would never discover or use the tool.
- **Fix**: Added to:
  - Tool cheat sheet (`| Kill worker | pool-kill-worker | reset (optional) |`)
  - Parent process inline tools list
  - "Worker Crashed / State Lost" troubleshooting section
  - CLAUDE.md Tool Categories (new entry: Pool Management)

### M3. No test coverage
- **Flagged by**: End User, Edge Case Explorer, Error Handling, Maintainability
- **Location**: `tests/` (missing file)
- **Description**: Zero test coverage for both `kill-session-worker` and the MCP tool.
- **Fix**: Created `tests/pool-kill-worker-test.lisp` with 7 test cases (20 assertions):
  1. Tool registration
  2. Pool-disabled response
  3. No-session response
  4. No-bound-worker response
  5. Kill success (reset=false) — verifies worker removed from affinity map
  6. Kill + reset (reset=true) — verifies new worker has different PID
  7. Double-kill idempotency

## Minor Issues

### Fixed (5/9)

#### m1. No pool-disabled check
- **Flagged by**: End User, Error Handling
- **Fix**: Added `*use-worker-pool*` check at top of tool body; returns "Worker pool is disabled" message.

#### m2. Unsanitized error messages in reset failure path
- **Flagged by**: Security
- **Fix**: Removed raw error text from response; uses generic message with `isError` flag instead.

#### m3. Missing `isError` flag on error responses
- **Flagged by**: End User
- **Fix**: Added `"isError" t` to the reset-failure response path.

#### m4. `signal-worker-terminate` not imported in pool.lisp
- **Flagged by**: Security (as part of M1)
- **Fix**: Added to `:import-from #:cl-mcp/src/worker-client`.

#### m5. Pool Management not listed in CLAUDE.md Tool Categories
- **Flagged by**: Maintainability
- **Fix**: Added entry #8 to Tool Categories.

### Accepted / Deferred (4/9)

#### m6. `reset=true` silently ignored when no worker exists
- **Flagged by**: End User, Edge Case Explorer
- **Status**: Accepted trade-off. The tool description clearly states behavior. Spawning a worker without killing is semantically a different operation (use `repl-eval` or `load-system` instead).

#### m7. Circuit breaker bypass via intentional kill/crash alternation
- **Flagged by**: Security
- **Status**: Accepted. Impact bounded by spawn latency (~10-30s per cycle) and `*max-pool-size*`. The trusted-local security model means the agent already has `repl-eval` access.

#### m8. No rate limiting on pool-kill-worker invocations
- **Flagged by**: Security
- **Status**: Accepted. Same reasoning as m7. Spawn latency is a natural rate limiter.

#### m9. Placeholder condvar waiters not woken on cancellation
- **Flagged by**: Edge Case Explorer
- **Status**: Deferred. Requires concurrent MCP requests on the same session, which is unlikely in practice. The spawning thread broadcasts within seconds.

## Positive Findings (Consolidated)

### Architecture & Thread Safety
- **Atomic state transitions**: All mutations to `*affinity-map*`, `*all-workers*`, and `*crash-history*` occur within a single `*pool-lock*` critical section
- **Worker state set to `:released` before kill**: Prevents health monitor from treating intentional kill as crash
- **Structural parity with `release-session`**: Same lock discipline, different semantics (session stays active)
- **`%schedule-replenish` integration**: Standby pool correctly replenished after kills

### Security & Isolation
- **Session isolation robust**: 256-bit random session IDs; no cross-session access possible
- **`*max-pool-size*` enforcement consistent**: Both `%effective-pool-size` and `%replenish-standbys` double-check the cap
- **Auth token on HTTP transport**: `/dev/urandom` session IDs, localhost-only binding

### Error Handling
- **Reset failure handled gracefully**: `handler-case` around `get-or-assign-worker` returns informative message
- **Double-kill idempotent**: Returns "No worker is bound" without errors
- **No crash notification leak**: Fresh workers do not receive stale crash notifications
- **Placeholder cancellation correct**: Cancelled flag checked by spawning thread

### UX & Functionality
- **Happy path smooth**: Kill, kill+reset, and auto-respawn all work correctly
- **State isolation verified**: Variables, packages, loaded systems all gone after kill
- **Full recovery cycle verified**: Kill -> load-system -> run-tests works correctly
- **Tool description well-written**: Clear explanation of modes, trade-offs, and post-kill steps

## Recommended Actions (Completed)

1. ~~Add `signal-worker-terminate` before `kill-worker`~~ Done
2. ~~Update documentation (prompts, CLAUDE.md)~~ Done
3. ~~Create test suite~~ Done
4. ~~Add pool-disabled check~~ Done
5. ~~Sanitize error messages~~ Done

## Files Modified

| File | Change |
|------|--------|
| `src/pool.lisp` | Added `signal-worker-terminate` import and call in `kill-session-worker` |
| `src/tools/pool-kill-worker.lisp` | Added pool-disabled check, sanitized errors, `isError` flag |
| `src/tools/all.lisp` | Added `pool-kill-worker` import (done in initial implementation) |
| `prompts/repl-driven-development.md` | Added to cheat sheet, inline tools list, troubleshooting |
| `CLAUDE.md` | Added Pool Management to Tool Categories |
| `tests/pool-kill-worker-test.lisp` | New file: 7 test cases, 20 assertions |
