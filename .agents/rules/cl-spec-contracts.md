---
trigger: model_decision
description: "Apply whenever using cl-spec tools (spec-list, spec-symbol, spec-describe, spec-check, spec-check-input, spec-check-batch), writing or modifying cl-spec contracts/properties, or editing cl-mcp code covered by cl-mcp/specs contracts (strings, paths, fs-write-file, spec-core-record, spec-adapter, spec-entry, spec-response-builders, worker-client, protocol, pool, proxy, request-lifecycle, reset-events, object-registry)."
---

# cl-spec Workflow & cl-mcp Self-Contracts

For the complete day-to-day `cl-spec` development procedure (writing declarations, baseline capture, failure localization, reload with `clear_fasls=true`, seed replay, verdict interpretation, and completion reports), read @prompts/cl-spec-driven-development.md in full before making contract-related changes.

## Core cl-spec Rules (Summary)

1. **Never weaken a contract to make a check pass.** Changing an existing contract requires the user's explicit confirmation unless the user's request itself changes the required behaviour.
2. **`symbol=` runs only Properties (`:about`), and `function=` runs only the Function Spec.** Run both (or use `spec-check-batch symbol=...`).
3. **Seeds are decimal strings** in JSON. Replay by copying the `Replay:` line (including `expect_definition_digest`).
4. **An edit reaches the worker only through `load-system`.** Before acceptance checks, reload the primary system with `clear_fasls=true`, then reload the contract system.
5. **`verified` covers only what that call ran.** Report `verification_gaps` verbatim without omitting or altering them.

## Contracts on cl-mcp's Own Code (`cl-mcp/specs`, opt-in)

See @docs/specs.md and `scripts/check-specs.lisp` (the CI entry point). When a change touches any covered function or invariant below, load `cl-spec/check-it` and `cl-mcp/specs`, inspect the contract before editing, take a baseline, and re-check after reloading (`load-system cl-mcp` with `clear_fasls=true`, then `cl-mcp/specs`). Elsewhere the bundle is not required.

### Function Specs + Properties
- **`ensure-trailing-newline`, `sanitize-for-json`, `sanitize-error-message`**:
  Read the contract with `spec-symbol`/`spec-describe` before editing, take a `spec-check function=` and `symbol=` baseline, and re-check after reload.

### Read/Write Path Boundaries (Properties only)
- **Read access** (`allowed-read-path`, `resolve-readable-path`):
  Take a `symbol=` baseline (no seed), re-check each property with `property=` and its own seed and digest, and run `utils-paths-test` and `path-specs-test`.
- **Write boundary** (`ensure-write-path`, `fs-write-file`):
  `symbol=` on each, then `property=`, and run `utils-paths-test`, `write-path-specs-test`, and `fs-test`.
  Never test a write-path change with the MCP write tools on the working tree; the properties and tests write only into scratch trees of their own.

### cl-spec Adapter & Tool Layers (Properties only)
- **Record layer** (`field-availability`, `validate-versioned-record`, `project-record`, `project-core-record` in `src/spec-core-record.lisp`):
  `symbol=` on the changed function, then `property=`, and run `spec-core-record-test` and (with cl-spec loaded) `core-record-specs-test`.
- **Verdict layer** (`%counts`, `%contract-plist`, `%verified-p`, `%verification-gaps` in `src/spec-adapter-report.lisp`; internal, qualify with `::`):
  `symbol=`, then `property=`, and run `check-verdict-test` and `spec-adapter-report-test`.
- **Routing** (`%target-argument-error`, `%resolve-profile`, `%select-properties`, `%trials-budget`, `%definition-match` in `src/spec-adapter-report.lisp`, and `parse-seed-string` in `src/tools/spec-entry.lisp`):
  `symbol=`, then `property=`, and run `check-routing-test`; run the real-cl-spec suites only in a process of their own (`CL_MCP_SPECS_MODE=integration`, see `docs/specs.md`).
- **Inspection layer** (`api-backend-available-p`, `definition-digest` in `src/spec-adapter-core.lisp`; `contract-operation-missing`, `list-report`, `symbol-report`, `describe-report`, `%describe-function-spec` in `src/spec-adapter-report.lisp`):
  `symbol=`, then `property=`, and run `spec-inspection-test`.
- **Response layer** (the four `build-spec-*-response` in `src/tools/spec-response-builders.lisp`):
  `symbol=`, then `property=`, and run `spec-responses-test` and `spec-response-builders-test`; a change to the replay line or to a JSON field also needs `spec-responses-specs-test` in its own process.

### Wire Path, Worker Pool, Lifecycle & Concurrency
- **Worker-result wire path** (parse in `src/worker-client.lisp`, `proxy-to-worker`/`with-proxy-dispatch`, encoder in `src/protocol.lisp`):
  Run `spec-wire-test` in its own process (drives spec tools over a real TCP server and workers and compares pooled answers with inline ones).
- **Pool ownership** (`get-or-assign-worker`, `release-session`, `kill-session-worker`, `shutdown-pool`, and what they call in `src/pool.lisp`):
  `symbol=` on the changed function, then `property=` for each of the two `pool-ownership-*` properties, and run `pool-ownership-test`; its real-process case and other pool suites spawn workers, so run them in a fresh process (`rove`/`ros`), not the MCP worker you are working in.
- **Request lifecycle** (`proxy-to-worker`, `cancel-request`, hooks `worker-rpc` calls, and `src/request-lifecycle.lisp`):
  `property=request-lifecycle-keeps-its-promises`, then run `request-lifecycle-test` and `cancel-test` (the latter spawns a worker; fresh process).
- **State-loss events & object ids** (`src/reset-events.lisp`, `%mark-worker-crashed`, `kill-worker`, `record-worker-termination`, pool crash/release/kill/shutdown paths, proxy failure results, and `src/object-registry.lisp`):
  `property=` for `resets-are-told-exactly-once`, `resets-are-told-exactly-once-when-the-pool-is-full`, and `object-ids-never-outlive-their-image`, then run `reset-events-test`, `request-lifecycle-test`, and (in a fresh process) `worker-leaked-thread-test` and `pool-test`.
  *Invariant*: A worker's end is recorded before the signal that carries it out, and only the first record counts; a reset is told only by claiming it (`claim-session-resets`), never by copying it to a replacement.
- **Pool under overlapping operations & generations** (`shutdown-pool`, `%begin-spawn`, `%begin-ending`, `%end-worker`, `%wait-for-work-in-flight`, `%handle-worker-crash`, `%replenish-standbys`, `%effective-pool-size`, `%call-with-stream-held`, `initialize-pool`, `%make-generation`, `%spawn-and-bind`, `%schedule-replenish`):
  `property=` for `pool-shutdown-leaves-nothing-behind`, `pool-late-work-stays-with-its-generation`, and `pool-holds-while-operations-overlap`, then run `concurrency-test` and (in a fresh process) `pool-ownership-test` and `pool-test`.
  *Invariant*: A spawn or an ending is accounted for in the critical section that decides it, and a spawn hands its count to its worker in the one that registers it.
