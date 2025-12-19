# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Agent Guidelines

@prompts/repl-driven-development.md
@agents/common-lisp-expert.md

## Project Overview

cl-mcp is a Model Context Protocol (MCP) server for Common Lisp, providing JSON-RPC 2.0 over stdio/TCP/HTTP. It enables AI agents to interact with Common Lisp environments through structured tools for REPL evaluation, file operations, code introspection, and structure-aware editing.

## Build & Development Commands

### Loading the System
```lisp
(ql:quickload :cl-mcp)
```

### Starting the Server
```lisp
;; HTTP server (recommended for Claude Code integration)
(cl-mcp:start-http-server :port 3000)
;; Server runs at http://127.0.0.1:3000/mcp
;; You can continue using your REPL while the server runs

;; TCP server (development)
(cl-mcp:start-tcp-server-thread :port 12345)

;; Stdio transport
ros run -s cl-mcp -e "(cl-mcp:run :transport :stdio)"
```

### Running Tests
```bash
# All tests (requires socket access)
rove cl-mcp.asd

# Core tests only (sandbox-safe, excludes TCP)
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp

# Individual test suite
rove tests/integration-test.lisp
```

### Test from REPL (via MCP tools)
```lisp
;; Run specific test with output capture
(with-output-to-string (*standard-output*)
  (rove:run-test 'cl-mcp/tests/integration-test::repl-eval-printlength))
```

### Linting
Always run mallet on changed Lisp files before committing:
```bash
# Lint a single file
mallet src/http.lisp

# Lint all source files
mallet src/*.lisp
```

### Environment Variables
```bash
# Project root (critical for file operations)
export MCP_PROJECT_ROOT=/path/to/project

# Logging level
export MCP_LOG_LEVEL=debug  # debug|info|warn|error
```

## Architecture Overview

### Core Components

**Protocol Layer** (`src/protocol.lisp`)
- JSON-RPC 2.0 message handling with newline-delimited framing
- MCP initialize handshake with protocol version negotiation (supports 2025-06-18, 2025-03-26, 2024-11-05)
- Tools API dispatch (tools/list, tools/call)
- State management for client sessions

**Transport Layer** (`src/tcp.lisp`, `src/http.lisp`, `src/run.lisp`)
- Stdio transport: single-threaded line-by-line processing
- TCP transport: multi-threaded server with per-connection handler threads
- HTTP transport: Streamable HTTP (MCP 2025-03-26) via Hunchentoot for Claude Code
- Bridge support via `scripts/stdio_tcp_bridge.py` for editor integration

**Tool Categories**
1. **REPL** (`src/repl.lisp`): Form evaluation with package context, print controls, timeout
2. **File System** (`src/fs.lisp`): Read/write/list with project root security policy
3. **Lisp-Aware Reading** (`src/lisp-read-file.lisp`): Collapsed signatures, pattern-based expansion
4. **Structure-Aware Editing** (`src/lisp-edit-form.lisp`): CST-based form replacement using Eclector
5. **Code Intelligence** (`src/code.lisp`): Symbol definition lookup, describe, xref via sb-introspect
6. **Validation** (`src/validate.lisp`, `src/parinfer.lisp`): Parenthesis checking, auto-repair

### Security Model

**File Access Policy**:
- Reads: Allowed under project root OR within `asdf:system-source-directory` of registered systems
- Writes: Restricted to project root only; absolute paths rejected
- No shell access or arbitrary path traversal

**Evaluation Safety**:
- Reader and runtime eval are ENABLED (trusted local development tool)
- Optional `safeRead` parameter to disable `#.` reader macro
- Timeout support to prevent infinite loops

### Key Design Patterns

**Package-Inferred System**: Uses ASDF `package-inferred-system` - each file defines its own package
- Add new files by updating `cl-mcp.asd` dependencies
- Export symbols explicitly in `main.lisp`

**Structured Editing Philosophy**:
- `lisp-edit-form` (CST-based) for existing Lisp code → preserves structure and comments
- `fs-write-file` only for NEW files or non-Lisp content
- Parinfer integration auto-repairs missing closing parens

**Token-Efficient Reading**:
- `lisp-read-file` with `collapsed=true` shows only signatures (like `(defun foo (args) ...)`)
- Use `name_pattern` regex to expand specific definitions
- Defaults to 2000 line limit for raw reads

**Project Root Synchronization**:
- Critical for all file operations
- Set via `MCP_PROJECT_ROOT` env var OR `fs-set-project-root` tool
- Server syncs `*project-root*` and current working directory on initialize if client provides `rootPath`/`rootUri`

## Important Patterns & Constraints

### When Editing Code
1. **Never overwrite Lisp files with `fs-write-file`** - use `lisp-edit-form` instead
2. For `defmethod`, include specializers in `form_name`: `"print-object (my-class t)"`
3. Operations: `replace`, `insert_before`, `insert_after`
4. Content should be the complete form including `(defun ...)` wrapper

### When Reading Code
1. **Prefer `lisp-read-file` over `fs-read-file`** for `.lisp`/`.asd` files
2. Start with `collapsed=true` to scan structure
3. Use `name_pattern` for targeted reads: `"^my-function$"`
4. Use `fs-read-file` only for plain text (README, JSON, YAML, config)

### Symbol Operations
1. **Always load systems first**: `(ql:quickload :system-name)`
2. Use package-qualified symbols: `"cl-mcp:run"` not `"run"`
3. Fallback to `lisp-read-file` with `name_pattern` if `code-find` fails (symbol not loaded)

### Test Workflow
- Tests use Rove framework
- Place in `tests/` with `*-test.lisp` suffix
- Update `cl-mcp.asd` test system dependencies
- Socket tests (`tests/tcp-test.lisp`) fail in sandboxed environments - run core suites individually

### Logging
- Structured JSON logs to `*error-output*`
- Control via `MCP_LOG_LEVEL` environment variable
- Never log sensitive data (secrets, tokens, credentials)

## Integration Notes

### For AI Agents

**Required Reading:**
- `prompts/repl-driven-development.md` - Comprehensive tool usage guide for REPL-driven development
- `agents/common-lisp-expert.md` - Production-quality CL architecture, TDD with Rove, and style guidelines

**REPL-Driven Workflow** (from `repl-driven-development.md`):
1. **EXPLORE**: Use introspection tools (`code-describe`, `code-find`) and `lisp-read-file` to understand context
2. **DEVELOP**: Evaluate small forms in the REPL (`repl-eval`) to verify correctness
3. **EDIT**: Use `lisp-edit-form` to persist changes safely. **NEVER** overwrite existing Lisp files with `fs-write-file`
4. **VERIFY**: Re-evaluate changed forms or run tests to ensure correctness

**Common Lisp Expert Guidelines** (from `common-lisp-expert.md`):
- Ship production quality from day one with strict style adherence
- TDD-first with Rove: write failing tests, implement minimally, refactor
- CLOS protocol-first design; minimize global specials; clear packages/exports
- No runtime `eval` or dynamic interning; prefer restarts over `signal`

### MCP Client Configuration

**HTTP Transport (Recommended for Claude Code)**

Start the server from your REPL:
```lisp
(ql:quickload :cl-mcp)
(cl-mcp:start-http-server :port 3000)
```

Configure Claude Code (in `~/.claude/settings.json` or project `.mcp.json`):
```json
{
  "mcpServers": {
    "cl-mcp": {
      "type": "url",
      "url": "http://127.0.0.1:3000/mcp"
    }
  }
}
```

**Stdio Transport (for subprocess-based MCP clients)**
```json
{
  "mcpServers": {
    "cl-mcp": {
      "command": "ros",
      "args": ["run", "-l", "cl-mcp", "-e", "(cl-mcp:run)"],
      "env": {
        "MCP_PROJECT_ROOT": "${workspaceFolder}"
      }
    }
  }
}
```

## Code Style

- Follow Google Common Lisp Style Guide
- 2-space indent, ≤100 columns
- Blank line between top-level forms
- Lower-case lisp-case: `my-function`, `*special*`, `+constant+`, `something-p`
- Docstrings required for public functions/classes
- Each file starts with `(in-package ...)`

## Repository Structure

```
src/          Core implementation (protocol, tools, transports)
tests/        Rove test suites (mirrored naming: *-test.lisp)
scripts/      Helper clients and stdio↔TCP bridge
prompts/      System prompts for AI agents (repl-driven-development.md)
agents/       Agent persona guidelines (common-lisp-expert.md)
```
