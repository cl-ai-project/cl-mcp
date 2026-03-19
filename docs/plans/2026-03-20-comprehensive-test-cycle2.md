# Comprehensive Test Report — Cycle 2

> **Date**: 2026-03-20
> **Branch**: `refactor/comprehensive-fix-cycle-2026-03-20`
> **Perspectives**: Edge Case Explorer, Security, Error Handling
> **Note**: Performance agent from Cycle 1 covered perf issues; Cycle 2 focuses on remaining issues after Cycle 1 fixes.

## Summary

| Severity | Count |
|----------|-------|
| Critical | 0     |
| Major    | 10    |
| Minor    | 15    |
| **Total**| **25**|

All Critical issues from Cycle 1 have been resolved. Cycle 2 found no new Critical issues. The remaining issues are defense-in-depth gaps, consistency problems, and edge case handling.

---

## Major Issues

### M1. Race Condition in `handle-initialize` Project Root Setting
- **Perspectives**: Security
- **Location**: `src/protocol.lisp:102-115`
- **Description**: `handle-initialize` sets `*project-root*` and calls `uiop/os:chdir` without acquiring `*project-root-lock*`. In contrast, `fs-set-project-root` properly uses `bt:with-lock-held`. Concurrent HTTP initializations with different `rootPath` values can produce inconsistent state.
- **Suggestion**: Wrap the mutation in `(bt:with-lock-held (*project-root-lock*) ...)` and also set `*default-pathname-defaults*`.

### M2. Backtrace Information Leakage in `repl-eval` Error Responses
- **Perspectives**: Security
- **Location**: `src/repl-core.lisp:134-141`
- **Description**: Full SBCL backtraces with absolute filesystem paths, memory addresses, and internal function names are returned in `content[].text`. Additionally, `error_context.frames[].source_file` returns unsanitized absolute paths.
- **Suggestion**: Strip or relativize absolute paths in backtrace text. Use `normalize-path-for-display` for `source_file` fields.

### M3. Raw JSON Input Logged on Parse Errors
- **Perspectives**: Security
- **Location**: `src/protocol.lisp:189-192`
- **Description**: When JSON parse fails, the entire raw input line is logged verbatim. If a client sends malformed JSON containing secrets/API keys, these are written to logs.
- **Suggestion**: Log only byte count and error message, not raw content.

### M4. Worker `set-project-root` Has Weaker Validation Than Parent
- **Perspectives**: Security
- **Location**: `src/worker/handlers.lisp:188-199`
- **Description**: Worker-side `%handle-set-project-root` only blocks `("/" "//" "///")`, while parent-side `fs-set-project-root` also blocks `("/tmp/" "/home/")`. Inconsistent guardrails.
- **Suggestion**: Extract shared validation into a utility function called by both.

### M5. `%repl-eval-with-timeout` and `%load-with-timeout` Don't Verify Thread Destruction
- **Perspectives**: Error Handling
- **Location**: `src/repl-core.lisp:155-170`, `src/system-loader-core.lisp:35-55`
- **Description**: Both use `(ignore-errors (bt:destroy-thread worker))` on timeout but never check if the thread actually died. If `destroy-thread` fails (thread in uninterruptible state), the thread runs indefinitely.
- **Suggestion**: After `destroy-thread`, poll `thread-alive-p` briefly and log a warning if thread survives.

### M6. `%handle-run-tests` Timeout Leaves ASDF/Rove State Inconsistent
- **Perspectives**: Error Handling
- **Location**: `src/worker/handlers.lisp:107-120`
- **Description**: Uses `sb-ext:with-timeout` which interrupts the same thread mid-compilation. Unlike `%repl-eval-with-timeout` and `%load-with-timeout` (which use separate threads), this can corrupt ASDF compilation state and Rove tracking state.
- **Suggestion**: Use the same polling-based timeout pattern with a separate thread, consistent with other timeout handlers.

### M7. `%inspect-cons` Has No Circular CDR Chain Detection
- **Perspectives**: Edge Case Explorer
- **Location**: `src/inspect.lisp`, `%inspect-cons`
- **Description**: The loop walks `(cdr current)` without checking for circular cdr chains. Although bounded by `max-elements`, a short circular list with large `max_elements` (user-controllable) wastes CPU creating millions of identical element representations.
- **Suggestion**: Add a `seen` hash-table check on each cdr cons cell within the loop.

### M8. `%read-file-string` Has No Upper Bound When `limit` Not Provided
- **Perspectives**: Edge Case Explorer
- **Location**: `src/fs.lisp`, `%read-file-string`
- **Description**: When no `limit` is given, `file-length` is used directly with no cap. A 10MB file allocates a 10MB buffer. The `*fs-read-max-bytes*` limit is only enforced when `limit` is explicitly provided.
- **Suggestion**: Apply `(min ... *fs-read-max-bytes*)` when limit is not provided.

### M9. `%sanitize-for-encoding` Depth Limit Passes Through Unsanitized Data
- **Perspectives**: Edge Case Explorer
- **Location**: `src/protocol.lisp`, `%sanitize-for-encoding`
- **Description**: When recursion depth exceeds 20, returns `obj` as-is without sanitization. Deeply nested strings with ANSI escapes bypass sanitization entirely.
- **Suggestion**: At depth limit, still sanitize leaf strings: `(if (stringp obj) (sanitize-for-json obj) obj)`.

### M10. `scan-toplevel-forms` Does Not Handle Nested Block Comments
- **Perspectives**: Edge Case Explorer
- **Location**: `src/utils/clgrep.lisp`, `scan-toplevel-forms`
- **Description**: Uses binary state (`:normal` vs `:block-comment`) instead of a depth counter. Nested `#| #| |# |#` comments cause premature exit from block comment state, corrupting paren tracking for subsequent forms. Compare with `%scan-parens` in validate.lisp which correctly uses `block-depth`.
- **Suggestion**: Replace binary state with block-comment depth counter.

---

## Minor Issues

### m1. Timing Side-Channel in HTTP Bearer Token Comparison
- **Perspectives**: Security
- **Location**: `src/http.lisp:94`
- **Description**: `string=` short-circuits on first mismatch. Low practical risk for local-only service.

### m2. Unbounded HTTP Session Table (DoS)
- **Perspectives**: Security
- **Location**: `src/http.lisp:39-42`
- **Description**: `*sessions*` hash-table has no maximum size limit.

### m3. Worker TCP Ports Exposed in `pool-status`
- **Perspectives**: Security, Edge Case Explorer
- **Location**: `src/pool.lisp:1039-1042`
- **Description**: `pool-worker-info` includes `tcp_port` for every worker.

### m4. `fs-set-project-root` Blocklist is Incomplete
- **Perspectives**: Security
- **Location**: `src/fs.lisp:143-148`
- **Description**: Only blocks `/`, `/tmp/`, `/home/`. Missing `/etc/`, `/var/`, `/usr/`, `/sys/`, `/proc/`.

### m5. Incomplete Error Sanitization in Worker Dispatch
- **Perspectives**: Security
- **Location**: `src/worker/server.lisp:115-119`
- **Description**: `sanitize-error-message` doesn't strip absolute filesystem paths or IP:port patterns.

### m6. `start-http-server` Leaves `*http-server*` Set on Failure
- **Perspectives**: Error Handling
- **Location**: `src/http.lisp:502-530`
- **Description**: If `hunchentoot:start` fails, `*http-server*` still references the failed acceptor.

### m7. `broadcast-root-to-workers` Blocks Indefinitely if `join-thread` Hangs
- **Perspectives**: Error Handling
- **Location**: `src/pool.lisp:1030-1065`
- **Description**: `join-thread` calls have no timeout. Can freeze the MCP server.

### m8. `inspect-object-by-id` Doesn't Use `found-p` from `lookup-object`
- **Perspectives**: Error Handling
- **Location**: `src/inspect.lisp:305-325`
- **Description**: Uses `(if object ...)` instead of the secondary `found-p` value.

### m9. `cancel-request` TOCTOU Window
- **Perspectives**: Error Handling
- **Location**: `src/proxy.lisp:230-280`
- **Description**: Lookup, validation, and removal not atomic. Late cancellation can kill a worker unnecessarily.

### m10. `%eval-forms` Doesn't Catch Conditions from `prin1-to-string`
- **Perspectives**: Error Handling
- **Location**: `src/repl-core.lisp:178-186`
- **Description**: If `print-object` signals an error, it propagates uncaught from `%do-repl-eval`.

### m11. `object-registry` `next-id` Counter Grows Unboundedly
- **Perspectives**: Edge Case Explorer
- **Location**: `src/object-registry.lisp`
- **Description**: Fixnum->bignum transition after ~2^62 registrations. JSON precision at 2^53.

### m12. `%handle-mcp-post-initialize` Doesn't Validate `id` Field
- **Perspectives**: Edge Case Explorer
- **Location**: `src/http.lisp`
- **Description**: Creates a session for initialize requests without `id` (notifications). Session leaked.

### m13. `lisp-check-parens` with Large `offset` Reads Empty Content
- **Perspectives**: Edge Case Explorer
- **Location**: `src/validate.lisp`
- **Description**: Reports "balanced" for zero-length slice of unbalanced file.

### m14. `%build-package-map` Doesn't Handle `in-package` Inside Block Comments
- **Perspectives**: Edge Case Explorer
- **Location**: `src/utils/clgrep.lisp`
- **Description**: `(in-package ...)` inside `#| ... |#` is incorrectly parsed as a package declaration.

### m15. `generate-session-id` Fails on Systems Without `/dev/urandom`
- **Perspectives**: Edge Case Explorer
- **Location**: `src/http.lisp`
- **Description**: No fallback for containerized environments without `/dev/urandom`.

---

## Positive Findings (Consolidated)

1. **Atomic file writes** — write-to-temp-then-rename pattern with proper `unwind-protect` cleanup
2. **Worker pool circuit breaker** — 3 crashes in 5 minutes halts recovery, preventing resource exhaustion
3. **Environment variable allowlist** — Only safe variables passed to workers (Cycle 1 fix)
4. **Lock hierarchy documentation** — 3-level hierarchy consistently followed
5. **Triple-fallback JSON encoding** — normal → sanitize+retry → hardcoded error
6. **Session ID cryptographic strength** — 256-bit entropy from `/dev/urandom`
7. **CORS origin validation** — Validates boundary character after hostname prefix
8. **Write error recovery** — Both stdio/TCP transports handle `stream-error` (Cycle 1 fix)
9. **Placeholder-based spawn coordination** — Prevents duplicate worker spawns
10. **Line length limits** — 16MB cap on all transports (Cycle 1 fix)
11. **Cross-session cancel validation** — Prevents one session canceling another's requests
12. **Health monitor resilience** — Uses condition variable for responsive shutdown
13. **Cached proxy bindings** — Avoids repeated `find-package`/`find-symbol` lookups

---

## Recommended Actions (Priority Order)

1. Fix M1 (race condition) and M3 (log truncation) — quick wins, high impact
2. Fix M6 (run-tests timeout pattern) — consistency with other handlers
3. Fix M10 (nested block comments) — correctness of clgrep
4. Fix M9 (sanitization depth) — defense in depth
5. Fix M8 (unbounded file reads) — resource control
6. Fix M2 (backtrace paths) — information leakage reduction
7. Fix M5 (thread destruction verification) — reliability improvement
8. Fix M4 (unified root validation) — consistency
9. Fix M7 (circular cons detection) — edge case protection
