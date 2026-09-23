# CLAUDE.md

## Agent Guidelines

@prompts/repl-driven-development.md
@prompts/common-lisp-expert.md
@prompts/cl-spec-driven-development.md

## Project Overview

cl-mcp is a Model Context Protocol (MCP) server for Common Lisp, providing JSON-RPC 2.0 over stdio/TCP/HTTP. It exposes tools for REPL evaluation, file operations, code introspection, and structure-aware Lisp editing to AI agents.

## Self-Hosted Development

This project is developed using its own MCP tools. When working on cl-mcp:

- **Lisp code operations** (search, read, edit, eval): Use cl-mcp tools (`clgrep-search`, `lisp-read-file`, `lisp-edit-form`, `repl-eval`, etc.) per repl-driven-development.md
- **Shell commands**: Only for `git`, `mallet` (linting), `rove` (test fallback), and user-requested commands
- **Package naming**: Uses ASDF `package-inferred-system` — each file defines package `cl-mcp/src/<name>`. Add new files by updating `cl-mcp.asd` dependencies. Exports go in `main.lisp`

## Testing & Linting

**Run tests** via `run-tests` tool with system name `cl-mcp/tests/<name>-test`:
```lisp
;; Single test via repl-eval (fallback when package conflicts occur)
(rove:run-test 'cl-mcp/tests/integration-test::repl-eval-printlength)
```

**Fallback** (stale image / package conflicts): `rove cl-mcp.asd` from Bash for a clean process.

**Pre-PR**: `(asdf:compile-system :cl-mcp :force :all)` to catch warnings, then run full test suite.
(`:force t` recompiles nothing here: cl-mcp is a package-inferred system, so the work is in the
per-file subsystems that only `:force :all` reaches.)

**Linting** (required before commit; the same globs the Lint CI job runs, tests included):
```bash
mallet src/*.lisp src/*/*.lisp tests/*.lisp specs.lisp specs/*.lisp scripts/*.lisp
```

**Contracts on cl-mcp's own code** (`cl-mcp/specs`, opt-in, see `docs/specs.md`): when a change
touches a function the bundle covers (`ensure-trailing-newline`, `sanitize-for-json`,
`sanitize-error-message`), load `cl-spec/check-it` and `cl-mcp/specs`, read the contract with
`spec-symbol`/`spec-describe` before editing, take a `spec-check function=` and `symbol=`
baseline, and re-check after reloading (`load-system cl-mcp` with `clear_fasls`, then
`cl-mcp/specs`). The read-access functions `allowed-read-path` and `resolve-readable-path`
have properties only: take a `symbol=` baseline (no seed), re-check each property with
`property=` and its own seed and digest, and run `utils-paths-test` and `path-specs-test`.
The write boundary, `ensure-write-path` and `fs-write-file`, likewise: `symbol=` on each, then
`property=`, and run `utils-paths-test`, `write-path-specs-test` and `fs-test`. Never try a
write-path change with the MCP write tools on the working tree; the properties and tests write
only into scratch trees of their own. The record layer of the cl-spec adapter
(`field-availability`, `validate-versioned-record`, `project-record`, `project-core-record` in
`src/spec-core-record.lisp`) has properties too: `symbol=` on the one you change, then `property=`,
and run `spec-core-record-test` and, with cl-spec loaded, `core-record-specs-test`. So does the
verdict layer (`%counts`, `%contract-plist`, `%verified-p`, `%verification-gaps` in
`src/spec-adapter-report.lisp`; internal, so qualify them with `::`): `symbol=` on the one you
change, then `property=`, and run `check-verdict-test` and `spec-adapter-report-test`. So does
routing (`%target-argument-error`, `%resolve-profile`, `%select-properties`, `%trials-budget`,
`%definition-match` there, and `parse-seed-string` in `src/tools/spec-entry.lisp`): the same
steps, then `check-routing-test`; run the real-cl-spec suites only in a process of their own
(`CL_MCP_SPECS_MODE=integration`, see `docs/specs.md`). The same goes for the inspection layer
(`api-backend-available-p` and `definition-digest` in `src/spec-adapter-core.lisp`;
`contract-operation-missing`, `list-report`, `symbol-report`, `describe-report` and
`%describe-function-spec` in `src/spec-adapter-report.lisp`): `symbol=`, then `property=`, and
run `spec-inspection-test`. So does the response layer (the four
`build-spec-*-response` in `src/tools/spec-response-builders.lisp`): `symbol=`, then
`property=`, and run `spec-responses-test` and `spec-response-builders-test`; a change to the
replay line or to a JSON field also needs `spec-responses-specs-test` in its own process.
A change to how a worker's result reaches the client (the parse in `src/worker-client.lisp`,
`proxy-to-worker`/`with-proxy-dispatch`, or the encoder in `src/protocol.lisp`) needs
`spec-wire-test` in its own process: it drives the spec tools over a real TCP server and
workers and compares the pooled answers with the inline ones.
So does the pool's ownership (`get-or-assign-worker`, `release-session`,
`kill-session-worker`, `shutdown-pool` and what they call in `src/pool.lisp`): `symbol=` on the
function you change, then `property=` for each of the two `pool-ownership-*` properties, and run
`pool-ownership-test`; its real-process case and the other pool suites spawn workers, so run them
in a fresh process (`rove`/`ros`), not the MCP worker you are working in.
So does a request's lifecycle (`proxy-to-worker`, `cancel-request`, the hooks `worker-rpc` calls,
and `src/request-lifecycle.lisp`): `property=request-lifecycle-keeps-its-promises`, then run
`request-lifecycle-test` and `cancel-test` (the latter spawns a worker; fresh process).
So do state-loss events (`src/reset-events.lisp`; wherever a worker's end is recorded or told --
`%mark-worker-crashed`, `kill-worker`, `record-worker-termination`, the pool's crash, release,
kill and shutdown paths, the proxy's failure results) and object ids (`src/object-registry.lisp`):
`property=` for `resets-are-told-exactly-once`, `…-when-the-pool-is-full` and
`object-ids-never-outlive-their-image`, then run `reset-events-test`, `request-lifecycle-test`,
and, in a fresh process, `worker-leaked-thread-test` and `pool-test`. A worker's end is recorded
before the signal that carries it out, and only the first record counts; a reset is told only by
claiming it (`claim-session-resets`), never by copying it to a replacement.
Elsewhere the bundle is not required.

## Architecture

**Protocol** (`src/protocol.lisp`): JSON-RPC 2.0, MCP handshake (2025-06-18, 2025-03-26, 2024-11-05), tools dispatch
**Transports** (`src/tcp.lisp`, `src/http.lisp`, `src/run.lisp`): Stdio, TCP (multi-threaded), HTTP (Streamable HTTP via Hunchentoot)
**Tools:**

| Category | Files | Purpose |
|----------|-------|---------|
| REPL | `src/repl.lisp` | Form evaluation with package context, print controls, timeout |
| System Loader | `src/system-loader.lisp` | ASDF loading with force-reload, output suppression |
| File System | `src/fs.lisp` | Read/write/list with project root guardrails |
| Lisp Reading | `src/lisp-read-file.lisp` | Collapsed signatures, pattern-based expansion |
| Lisp Editing | `src/lisp-edit-form.lisp` | CST-based form replace/insert via Eclector |
| Lisp Patching | `src/lisp-patch-form.lisp` | Token-efficient sub-form text replacement |
| Code Intel | `src/code.lisp`, `src/code-refs-scan.lisp`, `src/code-refs-core.lisp` | Symbol lookup, describe; callers with call sites and tests (xref + source scan) |
| CLOS | `src/clos.lisp`, `src/clos-core.lisp`, `src/clos-verify-core.lisp`, `src/tools/clos-response-builders.lisp` | Generic function methods, class hierarchy, slots and specialized methods (clos-describe; worker reads the image, parent reads the source, worker re-verifies each token so a `form_type`/`form_name` is only handed out once source and image agree — see `clos-describe` in docs/tools.md) |
| Validation | `src/validate.lisp`, `src/parinfer.lisp` | Paren checking, auto-repair |
| Pool Mgmt | `src/tools/pool-status.lisp`, `src/tools/pool-kill-worker.lisp` | Worker diagnostics and lifecycle |
| cl-spec (opt-in) | `src/spec-adapter-core.lisp`, `src/spec-adapter-report.lisp`, `src/tools/spec-*.lisp` | Spec/Property listing, discovery and execution (`spec-list` / `spec-symbol` / `spec-describe` / `spec-check`). **Optional tool group `cl-spec`, off by default** |

**Optional tool groups:** a tool declared with `define-tool`'s `:group` stays
out of `tools/list` and refuses calls until its group is enabled, via
`MCP_ENABLE_TOOL_GROUPS=cl-spec` in the server's environment or the
`:tool-groups` argument of any server entry point (`run`, `start-http-server`,
`serve-tcp`, `start-tcp-server-thread`, `ensure-tcp-server-thread`). cl-mcp has no dependency on
cl-spec; the adapter resolves it at call time. See `docs/tools.md`.

## Code Style

- Follow Google Common Lisp Style Guide
- 2-space indent, <=100 columns
- Blank line between top-level forms
- Lower-case lisp-case: `my-function`, `*special*`, `+constant+`, `something-p`
- Docstrings required for public functions/classes
- Each file starts with `(in-package ...)`

## Repository Structure

```
src/              Core implementation (protocol, tools, transports)
tests/            Rove test suites (mirrored naming: *-test.lisp)
specs.lisp, specs/  Opt-in cl-spec contracts on cl-mcp's own functions, and their runner
scripts/          Helper clients, stdio<->TCP bridge, check-specs.lisp (CI entry for specs)
prompts/          System prompts for AI agents
.claude/skills/   Project-local Claude Code skills (auto-discovered)
```

## Dogfooding

For structured feedback-collection cycles ("build a throwaway project with
cl-mcp's own tools and record every rough edge"), use the `dogfooding-cl-mcp`
skill shipped in `.claude/skills/dogfooding-cl-mcp/SKILL.md`. Claude Code
auto-discovers project-local skills on session start, so invoking `/dogfooding-cl-mcp`
(or the `Skill` tool with `skill: "dogfooding-cl-mcp"`) loads the workflow.
The skill includes a pitfalls table of known issues and a P1/P2/P3 feedback
format; contributing new pitfalls back to the skill is welcomed via normal PRs.

For the `cl-spec` tool group specifically, use `dogfooding-cl-spec`
(`.claude/skills/dogfooding-cl-spec/SKILL.md`) instead. It measures something
narrower: whether an agent can read a contract, repair the code without
breaking it, and tell `verified` from `not falsified over the trials that ran`.
Its cycles plant a fault a contract can see — prepared outside the repairing
agent's context whenever diagnosis quality is being scored — forbid
implementing any new `spec-*` tool mid-cycle, and record every `repl-eval`
fallback with a class (A-D), so a proposal like `spec-check-call` is decided by
measured friction across three to five cycles rather than by one uncomfortable
moment.
