# Comprehensive Test Report — Cycle 3

> **Date**: 2026-03-20
> **Branch**: `refactor/comprehensive-fix-cycle-2026-03-20`
> **Perspectives**: Edge Case Explorer, Security, Error Handling
> **Note**: Cycle 3 focuses on verifying Cycle 1/2 fixes and finding remaining issues in changed code.

## Summary

| Severity | Count |
|----------|-------|
| Critical | 0     |
| Major    | 8     |
| Minor    | 10    |
| **Total**| **18**|

All Cycle 1 and 2 fixes verified correct. No Critical issues. The remaining Major issues are primarily in the `%sanitize-for-encoding` fallback path, JSON-RPC id handling, and run-tests timeout response formatting.

---

## Major Issues

### M1. Dotted Pairs Crash `%sanitize-for-encoding`
- **Perspectives**: Edge Case Explorer
- **Location**: `src/protocol.lisp`, `%sanitize-for-encoding`, cons branch
- **Description**: Uses `(loop for elt in obj ...)` which requires proper lists. Dotted pairs `(1 . 2)` and alists `((a . 1) (b . 2))` signal TYPE-ERROR. This crashes the sanitize fallback, causing the third-tier hardcoded error to fire, losing all response content.
- **Suggestion**: Replace `loop for elt in` with manual CDR-walking that handles dotted tails.

### M2. Circular Cons Lists Amplify to 10,000 Elements in `%sanitize-for-encoding`
- **Perspectives**: Edge Case Explorer
- **Location**: `src/protocol.lisp`, `%sanitize-for-encoding`, cons branch
- **Description**: Caps at `+sanitize-max-elements+` (10,000) but a 3-element circular list produces 10,000 output elements. Memory waste and oversized JSON responses.
- **Suggestion**: Add CDR-seen detection (like `%inspect-cons`) or reduce max-elements for the sanitizer.

### M3. Non-Standard JSON-RPC `id` Types Cause Cascading TYPE-ERROR
- **Perspectives**: Edge Case Explorer
- **Location**: `src/protocol.lisp`, `process-json-line` error handler
- **Description**: When a request has a float/boolean/array `id`, the error handler calls `(rpc-error id ...)` with the invalid id, causing a second TYPE-ERROR. Client gets no response.
- **Suggestion**: Coerce id to `(or null string integer)` in the error handler before calling `rpc-error`.

### M4. `run-tests` Timeout Produces Misleading "PASS" Response
- **Perspectives**: Error Handling
- **Location**: `src/worker/handlers.lisp:141-150`, `src/tools/response-builders.lisp`
- **Description**: The Cycle 2 `sb-ext:timeout` handler creates `(make-ht "error" t ...)`, but `build-run-tests-response` ignores the "error" key and reports "PASS" with 0/0 counts.
- **Suggestion**: Set "failed" to 1 and add "failed_tests" entry describing the timeout.

### M5. Inline `run-tests` Path Lacks `sb-ext:timeout` Handler
- **Perspectives**: Error Handling
- **Location**: `src/test-runner.lisp:65`
- **Description**: The Cycle 2 timeout fix was applied to the worker handler but not the inline (non-pool) code path. Timeout propagates as generic "Internal error".
- **Suggestion**: Add `handler-case` for `sb-ext:timeout` mirroring the worker handler fix.

### M6. `stop-tcp-server-thread` `join-thread` Can Hang Indefinitely
- **Perspectives**: Error Handling
- **Location**: `src/tcp.lisp:134`
- **Description**: `join-thread` blocks without timeout. If a client connection is active, the function hangs until the client disconnects or the request completes.
- **Suggestion**: Wrap `join-thread` in `handler-case` or add a polling loop with deadline.

### M7. No HTTP Session Count Limit
- **Perspectives**: Security
- **Location**: `src/http.lisp:137-143`
- **Description**: Sessions created unconditionally with no upper bound. Cleanup runs every 24 hours. Localhost DoS by flooding initialize requests.
- **Suggestion**: Add `*max-sessions*` cap and reject with 503 when full.

### M8. HTTP DELETE Deletes Any Session Without Ownership Validation
- **Perspectives**: Security
- **Location**: `src/http.lisp:378-389`
- **Description**: Any localhost process can terminate any session by guessing/sniffing the session ID.
- **Suggestion**: Validate caller owns the session, or accept as documented localhost-only behavior.

---

## Minor Issues

### m1. Root path blocklist not comprehensive against traversal
- **Perspectives**: Edge Case Explorer, Security
- **Location**: `src/fs.lisp`, `src/protocol.lisp`, `src/worker/handlers.lisp`

### m2. Worker `*project-root*` mutation not lock-protected
- **Perspectives**: Security
- **Location**: `src/worker/handlers.lisp:228-236`

### m3. No HTTP request body size limit
- **Perspectives**: Security
- **Location**: `src/http.lisp:249-257`

### m4. Non-constant-time token comparison
- **Perspectives**: Security
- **Location**: `src/http.lisp:90-92`

### m5. Log injection via unsanitized error messages
- **Perspectives**: Security
- **Location**: `src/worker/server.lisp:133-136`

### m6. `start-http-server` leaves `*http-server*` set on failure
- **Perspectives**: Error Handling
- **Location**: `src/http.lisp:510-514`

### m7. Object registry cannot distinguish evicted from never-registered
- **Perspectives**: Error Handling
- **Location**: `src/inspect.lisp:358`

### m8. Thread destruction verification uses fixed 1-second polling
- **Perspectives**: Error Handling
- **Location**: `src/repl-core.lisp:255-264`

### m9. `broadcast-root-to-workers` creates unbounded threads
- **Perspectives**: Error Handling
- **Location**: `src/pool.lisp:1029-1044`

### m10. Worker TCP port exposed in `pool-worker-info`
- **Perspectives**: Security
- **Location**: `src/pool.lisp:1085-1097`

---

## Positive Findings (Consolidated)

1. **All Cycle 1/2 fixes verified correct** — circular CDR detection, nested block comments, file read cap, root blocklist, thread-safe root init, env allowlist all working as designed
2. **Three-tier JSON encoding fallback** — robust defense against encoding failures
3. **Worker authentication with 256-bit secrets** from `/dev/urandom`
4. **Circuit breaker** — correctly prevents crash loop resource exhaustion
5. **Symlink-aware path traversal prevention** — truename on both target and root
6. **Atomic file writes** — write-to-temp-then-rename with unwind-protect cleanup
7. **Cross-session cancel validation** — prevents interference between sessions
8. **CORS origin validation** — validates boundary character after hostname prefix
9. **Proxy error sanitization** — strips internal object representations, truncates to 500 chars
10. **Worker lifecycle management** — SIGTERM → wait → SIGKILL sequencing

---

## Recommended Actions (Priority Order)

1. Fix M1 + M2 together — sanitizer cons branch needs CDR-walking and circular detection
2. Fix M3 — id coercion in error handler (quick fix)
3. Fix M4 + M5 together — run-tests timeout response formatting
4. Fix M6 — TCP stop hang (wrap join-thread)
5. M7, M8 — Defense-in-depth for HTTP sessions (lower priority)
