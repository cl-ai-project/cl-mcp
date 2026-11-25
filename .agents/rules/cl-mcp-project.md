---
trigger: always_on
description: "cl-mcp project overview, self-hosted development workflow, ASDF registration points, testing tiers, linting, architecture, and dogfooding conventions."
---

# cl-mcp Project Guide

## Project Overview

cl-mcp is a Model Context Protocol (MCP) server for Common Lisp, providing JSON-RPC 2.0 over stdio/TCP/HTTP. It exposes tools for REPL evaluation, file operations, code introspection, and structure-aware Lisp editing to AI agents.

## Self-Hosted Development

This project is developed using its own MCP tools. When working on cl-mcp:

- **Lisp code operations** (search, read, edit, eval): Use cl-mcp tools (`clgrep-search`, `lisp-read-file`, `lisp-edit-form`, `lisp-patch-form`, `repl-eval`, etc.) per `prompts/repl-driven-development.md`.
- **Shell commands**: Only for `git`, `mallet` (linting), `rove` (test fallback), and user-requested commands.
- **Package naming**: Uses ASDF `package-inferred-system` — each file defines package `cl-mcp/src/<path>` and is loaded because a loaded file `:import-from`s it; `cl-mcp.asd` needs no edit. Where a new file must be registered:
  - **Source file**: `:import-from` it in the files that use it.
  - **Tool**: Add its module to `src/tools/all.lisp` (if forgotten, the tool silently never loads and is missing from `tools/list`); a worker-side tool also needs a `"worker/<method>"` entry in `register-all-handlers` (`src/worker/handlers.lisp`).
  - **Test file**: Add `(:import-from #:cl-mcp/tests/<name>-test)` to the root `tests.lisp` (if forgotten, neither `rove cl-mcp.asd` nor CI runs it) — or, for a suite that starts worker processes or servers, add its name to `*process-tier-suites*` there instead (see Test Tiers). Suites that need `cl-spec` or a process of their own are not listed at all and run as CI steps.
  - **Public API**: Re-export from `main.lisp`.

## Testing & Linting

**Run tests** via `run-tests` tool with system name `cl-mcp/tests/<name>-test`:

```lisp
;; Single test via repl-eval (fallback when package conflicts occur)
(rove:run-test 'cl-mcp/tests/integration-test::repl-eval-printlength)
```

**Fallback** (stale image / package conflicts): `rove cl-mcp.asd` from shell for a clean process.

**Test tiers**: `rove cl-mcp.asd` and `run-tests` on `cl-mcp/tests` run the **quick tier** — every suite except those that start worker processes or servers or wait out real deadlines (`*process-tier-suites*` in `tests.lisp`, about 12 of a full run's 14 minutes; they are kept out of `cl-mcp/tests`' `:import-from` list, which `run-tests` follows). `CL_MCP_TEST_TIER=full rove cl-mcp.asd` runs everything (same as CI). A process-tier suite run by name (`run-tests`, or `rove:run` on it) always runs whole.
A change to the pool, workers, transports, timeouts, or cancellation needs the full tier before it is called green; a new suite that spawns workers or servers belongs in `*process-tier-suites*`.

**Pre-PR**: `(asdf:compile-system :cl-mcp :force :all)` to catch warnings, then run the full tier (`CL_MCP_TEST_TIER=full rove cl-mcp.asd`).
(`:force t` recompiles nothing here: cl-mcp is a package-inferred system, so the work is in the per-file subsystems that only `:force :all` reaches.)

**Linting** (required before commit; matches the Lint CI job):

```bash
mallet src/*.lisp src/*/*.lisp tests/*.lisp specs.lisp specs/*.lisp scripts/*.lisp
```

**Contracts on cl-mcp's own code (`cl-mcp/specs`, opt-in)**: Several core modules (strings, read/write paths, cl-spec adapter record/verdict/routing/inspection/response layers, worker wire path, pool ownership/concurrency, request lifecycle, reset events, object registry) carry `cl-spec` contracts and properties. When editing any of those modules, follow `.agents/rules/cl-spec-contracts.md` and `docs/specs.md`; `scripts/check-specs.lisp` runs the same checks CI does.

## Architecture

- **Protocol** (`src/protocol.lisp`): JSON-RPC 2.0, MCP handshake (`2025-11-25`, `2025-06-18`, `2025-03-26`, `2024-11-05`), tools dispatch
- **Transports** (`src/tcp.lisp`, `src/http.lisp`, `src/run.lisp`): Stdio, TCP (multi-threaded), HTTP (Streamable HTTP via Hunchentoot)

| Category | Files | Purpose |
|----------|-------|---------|
| REPL | `src/repl.lisp` | Form evaluation with package context, print controls, timeout |
| System Loader | `src/system-loader.lisp` | ASDF loading with force-reload, output suppression |
| File System | `src/fs.lisp` | Read/write/list with project root guardrails |
| Lisp Reading | `src/lisp-read-file.lisp` | Collapsed signatures, pattern-based expansion |
| Lisp Editing | `src/lisp-edit-form.lisp` | CST-based form replace/insert via Eclector |
| Lisp Patching | `src/lisp-patch-form.lisp` | Token-efficient sub-form text replacement |
| Code Intel | `src/code.lisp`, `src/code-refs-scan.lisp`, `src/code-refs-core.lisp` | Symbol lookup, describe; callers with call sites and tests (xref + source scan) |
| CLOS | `src/clos.lisp`, `src/clos-core.lisp`, `src/clos-verify-core.lisp`, `src/tools/clos-response-builders.lisp` | Generic function methods, class hierarchy, slots, and verified source/image edit guards (`clos-describe`) |
| Macroexpand | `src/lisp-macroexpand.lisp`, `src/macroexpand-core.lisp` | `lisp-macroexpand`: parent locates the form (CST), worker expands it |
| Search & Reference | `src/clgrep.lisp`, `src/utils/clgrep.lisp`, `src/clhs.lisp` | `clgrep-search` (Lisp-aware grep, no loading), `clhs-lookup` |
| Inspection | `src/inspect.lisp`, `src/object-registry.lisp`, `src/frame-inspector.lisp` | `inspect-object`, object ids, backtrace frames and locals |
| Tests | `src/test-runner.lisp`, `src/test-runner-core.lisp` | `run-tests`: Rove/FiveAM/prove detection, structured results |
| Scaffold | `src/project-scaffold.lisp` | `project-scaffold`: throwaway package-inferred projects |
| Validation | `src/validate.lisp`, `src/parinfer.lisp` | Paren checking, auto-repair |
| Pool Mgmt | `src/tools/pool-status.lisp`, `src/tools/pool-kill-worker.lisp` | Worker diagnostics and lifecycle |
| Worker Pool | `src/pool.lisp`, `src/proxy.lisp`, `src/worker-client.lisp`, `src/request-lifecycle.lisp`, `src/reset-events.lisp`, `src/worker/` | Session affinity, spawning, crash recovery, request routing, worker process |
| Tool Plumbing | `src/tools/define-tool.lisp`, `src/tools/registry.lisp`, `src/tools/all.lisp`, `src/tools/response-builders.lisp`, `src/server-instructions.lisp` | Tool definition/registration, optional groups, response text, `initialize` instructions |
| `cl-spec` (opt-in) | `src/spec-adapter-core.lisp`, `src/spec-adapter-report.lisp`, `src/tools/spec-*.lisp` | Registry discovery, contract inspection, single-input/generated checks, batch selection. **Optional tool group `cl-spec`, off by default** |

**Optional tool groups**: A tool declared with `define-tool`'s `:group` stays out of `tools/list` and refuses calls until its group is enabled via `MCP_ENABLE_TOOL_GROUPS=cl-spec` in the server's environment or the `:tool-groups` argument of any server entry point (`run`, `start-http-server`, `serve-tcp`, `start-tcp-server-thread`, `ensure-tcp-server-thread`). cl-mcp has no hard dependency on `cl-spec`; the adapter resolves it at call time. See `docs/tools.md`.

## Repository Structure

```text
src/                Core implementation (protocol, tools, transports)
tests/              Rove test suites (mirrored naming: *-test.lisp)
specs.lisp, specs/  Opt-in cl-spec contracts on cl-mcp's own functions, and their runner
scripts/            Helper clients, stdio<->TCP bridge, check-specs.lisp (CI entry for specs)
prompts/            System prompts for AI agents
.agents/            Project-local Antigravity rules, MCP config, and skill manifest
.claude/skills/     Project-local skills (shared with Antigravity via .agents/skills.json)
```

## Dogfooding

- For structured feedback-collection cycles ("build a throwaway project with cl-mcp's own tools and record every rough edge"), use the `dogfooding-cl-mcp` skill (`.claude/skills/dogfooding-cl-mcp/SKILL.md`).
- For the `cl-spec` tool group specifically, use `dogfooding-cl-spec` (`.claude/skills/dogfooding-cl-spec/SKILL.md`). It measures whether an agent can read a contract, repair code without breaking it, and distinguish `verified` from `not falsified over the trials that ran`, forbidding new `spec-*` tool implementations mid-cycle and recording every `repl-eval` fallback with class A–D.
