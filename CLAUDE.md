# CLAUDE.md

## Agent Guidelines

@prompts/repl-driven-development.md
@prompts/common-lisp-expert.md

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
mallet src/*.lisp src/*/*.lisp tests/*.lisp
```

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
| Code Intel | `src/code.lisp` | Symbol lookup, describe, xref via sb-introspect |
| Validation | `src/validate.lisp`, `src/parinfer.lisp` | Paren checking, auto-repair |
| Pool Mgmt | `src/tools/pool-status.lisp`, `src/tools/pool-kill-worker.lisp` | Worker diagnostics and lifecycle |
| cl-spec (opt-in) | `src/spec-adapter-core.lisp`, `src/spec-adapter-report.lisp`, `src/tools/spec-*.lisp` | Spec/Property discovery and execution. **Optional tool group `cl-spec`, off by default** |

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
scripts/          Helper clients and stdio<->TCP bridge
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
