# Worker Log Forwarding Design

**Date**: 2026-03-01
**Approach**: B (worker self-identifies, parent passthrough)

## Problem

Worker processes use `log-event` which writes structured JSON to stderr. The parent's `%start-stderr-drain` reads and discards this output to prevent OS pipe buffer blocking. All worker logs are lost.

## Design

### Data Flow

```
Worker: log-event -> stderr -> {"ts":..,"worker_id":"3","event":"worker.rpc",...}
    | (OS pipe)
Parent: %start-stderr-drain -> read line -> write-string to parent *log-stream*
```

### Changes

1. **`src/worker-client.lisp` - `%build-environment`**: Add `MCP_WORKER_ID=<id>` to the child environment.

2. **`src/worker/main.lisp` - `start`**: Read `MCP_WORKER_ID` env var at startup, set `*log-context*` to `("worker_id" <id>)`. All subsequent `log-event` calls from the worker automatically include `worker_id`.

3. **`src/worker-client.lisp` - `%start-stderr-drain`**: Change from read-and-discard to line-by-line forwarding to parent's `*log-stream*`.

### Notes

- Parent and worker writes interleave on the same stream, but `finish-output` per line prevents mid-line corruption.
- Non-JSON output (SBCL warnings etc.) is forwarded as-is.
- No JSON parse/re-encode needed on the parent side.
