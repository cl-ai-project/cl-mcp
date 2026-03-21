# CLAUDE.md

## Agent Guidelines

@prompts/repl-driven-development.md
@agents/common-lisp-expert.md

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

**Pre-PR**: `(asdf:compile-system :cl-mcp :force t)` to catch warnings, then run full test suite.

**Linting** (required before commit):
```bash
mallet src/*.lisp
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

## Code Style

- Follow Google Common Lisp Style Guide
- 2-space indent, <=100 columns
- Blank line between top-level forms
- Lower-case lisp-case: `my-function`, `*special*`, `+constant+`, `something-p`
- Docstrings required for public functions/classes
- Each file starts with `(in-package ...)`

## Repository Structure

```
src/          Core implementation (protocol, tools, transports)
tests/        Rove test suites (mirrored naming: *-test.lisp)
scripts/      Helper clients and stdio<->TCP bridge
prompts/      System prompts for AI agents (repl-driven-development.md)
agents/       Agent persona guidelines (common-lisp-expert.md)
```
