# cl-mcp
[![CI](https://github.com/cl-ai-project/cl-mcp/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/cl-ai-project/cl-mcp/actions/workflows/ci.yml)
[![Lint](https://github.com/cl-ai-project/cl-mcp/actions/workflows/lint.yml/badge.svg?branch=main)](https://github.com/cl-ai-project/cl-mcp/actions/workflows/lint.yml)

A Model Context Protocol (MCP) server for Common Lisp, providing JSON-RPC 2.0
over stdio, TCP, and HTTP (Streamable HTTP). It enables AI agents to interact
with Common Lisp environments through structured tools for REPL evaluation,
system loading, file operations, code introspection, and structure-aware editing.

## Features
- **REPL evaluation with object inspection** — evaluate forms, drill down into complex results (CLOS instances, hash-tables, etc.), and capture structured error context with stack frames and local variables
- **Sandboxed file operations** — read/write/list files restricted to the project root and ASDF system source directories, preventing accidental access outside the project
- **Structure-aware Lisp editing** — replace, insert, and patch top-level forms using Eclector CST parsing with automatic parinfer repair, preserving formatting and comments
- **Code intelligence** — symbol lookup, metadata, and cross-references via `sb-introspect`; Lisp-aware file viewing with collapsed signatures and pattern-based expansion
- **Structured test runner** — run Rove tests with pass/fail counts, failure details, and source locations
- **Worker pool isolation** — eval-dependent tools run in isolated child SBCL processes with automatic crash recovery, circuit breaker, and per-session affinity
- **Three transports** — stdio, TCP (multi-client), and Streamable HTTP (for Claude Code)
- **MCP protocol** — JSON-RPC 2.0 framing, capability discovery, cooperative request cancellation

## Requirements
- SBCL 2.x (developed with SBCL 2.5.x)
- Quicklisp (optional; pure ASDF also works)
- Dependencies (via ASDF/Quicklisp): runtime — `alexandria`, `cl-ppcre`, `yason`, `usocket`, `bordeaux-threads`, `eclector`, `hunchentoot`; tests — `rove`; optional — `clhs` (loaded on-demand by `clhs-lookup` tool).

## Quick Start

Load and run from an existing REPL:

```lisp
(asdf:load-system :cl-mcp)  ; or (ql:quickload :cl-mcp) if using Quicklisp

;; Start TCP transport on port 12345 in a new thread.
(cl-mcp:start-tcp-server-thread :port 12345)
```

Or run a minimal stdio loop (one JSON-RPC line per request):

```bash
ros run -s cl-mcp -e "(cl-mcp:run :transport :stdio)"
```

**Project root**: When `MCP_PROJECT_ROOT` is not set, the server uses the
directory where the MCP client (Claude Code, Codex, etc.) was launched as
the project root. You can also set it explicitly via the environment variable
or by calling the `fs-set-project-root` tool after connecting.

### Claude Code

#### HTTP transport (recommended)

Start the HTTP server from your REPL and keep using it alongside Claude Code:

```lisp
(asdf:load-system :cl-mcp)  ; or (ql:quickload :cl-mcp)

;; Start HTTP server on port 3000 (default)
(cl-mcp:start-http-server :port 3000)

;; Server is now running at http://127.0.0.1:3000/mcp
;; You can continue using your REPL normally
(+ 1 2)  ; => 3

;; Stop the server when done
(cl-mcp:stop-http-server)
```

Configure Claude Code to connect (in `~/.claude/settings.json` or project `.mcp.json`):

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

This approach lets both you and Claude Code share the same Lisp runtime.
You can inspect state from SLIME/Sly while Claude Code works through MCP.

#### Stdio transport

For a simpler setup where Claude Code manages the server process directly:

```json
{
  "mcpServers": {
    "cl-mcp": {
      "command": "ros",
      "args": ["run", "-s", "cl-mcp", "-e", "(cl-mcp:run)"]
    }
  }
}
```

Stdio is easy to configure but the Lisp process is owned by Claude Code,
making manual intervention from SLIME or another REPL difficult.

### Codex

#### Stdio transport

The simplest setup — Codex spawns and manages the server process:

```json
{
  "mcpServers": {
    "cl-mcp": {
      "command": "ros",
      "args": ["run", "-s", "cl-mcp", "-e", "(cl-mcp:run)"]
    }
  }
}
```

As with Claude Code's stdio mode, the Lisp process is owned by Codex and
not easily accessible from SLIME or another REPL.

#### TCP transport with stdio bridge

Start a TCP server from your REPL, then point Codex at it via the bundled
Python bridge that translates stdio ↔ TCP:

```lisp
;; In your REPL
(asdf:load-system :cl-mcp)
(cl-mcp:start-tcp-server-thread :port 12345)
```

```json
{
  "mcpServers": {
    "cl-mcp": {
      "command": "python3",
      "args": ["scripts/stdio_tcp_bridge.py", "--host", "127.0.0.1", "--port", "12345"]
    }
  }
}
```

This gives you the same shared-REPL workflow as Claude Code's HTTP mode —
you keep your SLIME/Sly session while Codex works through the bridge.

### Bundled test clients

For manual testing and debugging:

```bash
# Python TCP one-shot client (initialize)
python3 scripts/client_init.py --host 127.0.0.1 --port 12345 --method initialize --id 1

# Stdio↔TCP bridge (pipe JSON-RPC to TCP)
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | \
  python3 scripts/stdio_tcp_bridge.py --host 127.0.0.1 --port 12345
```

## Worker Pool Isolation

Eval-dependent tools (`repl-eval`, `load-system`, `run-tests`, `code-find`,
`code-describe`, `code-find-references`, `inspect-object`) run in **isolated
child SBCL processes** rather than the parent MCP server image.

- **Per-session affinity** — each MCP session is bound to a dedicated worker
  process. The worker persists for the session lifetime, preserving loaded
  systems and REPL state.
- **Automatic crash recovery** — if a worker crashes, a replacement is spawned
  and re-bound to the same session automatically.
- **Circuit breaker** — if a session's worker crashes repeatedly (3 times
  within 5 minutes by default), the pool stops respawning and reports the
  failure to the client.
- **Warm standby pool** — pre-spawned workers reduce first-request latency.
- **Dual launcher** — workers can be spawned via Roswell (`ros`) or bare SBCL,
  auto-detected at startup.

File-system tools (`fs-*`, `lisp-read-file`, `lisp-edit-form`, `lisp-patch-form`, `lisp-check-parens`,
`clgrep-search`, `clhs-lookup`) continue to run inline in the parent process.

Disable the worker pool to run all tools inline in the parent process
(single-image mode):

- **Environment variable**: `MCP_NO_WORKER_POOL=1` (applies at image load time)
- **Keyword argument**: all startup functions accept `:worker-pool`
  ```lisp
  (cl-mcp:run :transport :stdio :worker-pool nil)
  (cl-mcp:start-http-server :port 3000 :worker-pool nil)
  ```
  When `:worker-pool` is supplied it takes precedence over the environment variable.
  When omitted, the existing setting is used.

## Tools

All tools are self-describing via the MCP `tools/list` response.
For detailed input/output schemas and examples, see [docs/tools.md](docs/tools.md).

| Tool | Description |
|------|-------------|
| `repl-eval` | Evaluate forms with package context, print controls, and timeout |
| `load-system` | Load ASDF systems with force-reload and output suppression |
| `inspect-object` | Drill down into non-primitive objects by ID |
| `fs-read-file` | Read files within project root or ASDF system directories |
| `fs-write-file` | Write files under the project root |
| `fs-list-directory` | List directory entries (skips hidden/build artifacts) |
| `fs-get-project-info` | Report project root and working directory |
| `fs-set-project-root` | Set the server's project root |
| `lisp-read-file` | Lisp-aware file viewer with collapsed signatures |
| `lisp-edit-form` | Structure-aware form replace/insert via Eclector CST |
| `lisp-patch-form` | Scoped text replacement within a matched form |
| `lisp-check-parens` | Detect mismatched delimiters |
| `clgrep-search` | Semantic grep for Lisp files |
| `clhs-lookup` | Common Lisp HyperSpec reference |
| `code-find` | Find symbol definition location via sb-introspect |
| `code-describe` | Symbol metadata (type, arglist, documentation) |
| `code-find-references` | Cross-references for a symbol |
| `run-tests` | Structured test runner (Rove, ASDF fallback) |
| `pool-status` | Worker pool diagnostics |
| `pool-kill-worker` | Kill and optionally reset the session's worker |

## Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `MCP_PROJECT_ROOT` | Project root directory for file operations | client working directory |
| `MCP_LOG_LEVEL` | Log level: `debug`, `info`, `warn`, `error` | `info` |
| `MCP_LOG_FILE` | Log to file (timestamped with PID) | (stderr only) |
| `MCP_NO_WORKER_POOL` | Set to `1` to disable worker pool isolation | (not set = pool enabled) |

## Logging
- Structured JSON line logs to `*error-output*` by default.
- Control level via `MCP_LOG_LEVEL` with one of: `debug`, `info`, `warn`, `error`.
- Optionally log to a file via `MCP_LOG_FILE`. The value is a path template;
  the server inserts a timestamp and PID before the extension
  (e.g., `/tmp/cl-mcp.log` → `/tmp/cl-mcp-2026-03-01T09-15-30-12345.log`).
  When set, logs are written to both stderr and the file.

Example:

```bash
MCP_LOG_LEVEL=debug MCP_LOG_FILE=/tmp/cl-mcp.log sbcl --eval '(asdf:load-system :cl-mcp)' ...
```

## Protocol Support
- Protocol versions recognized: `2025-06-18`, `2025-03-26`, `2024-11-05`
  - On `initialize`, if the client's `protocolVersion` is supported it is echoed
    back; if it is **not** supported the server returns `error.code = -32602`
    with `data.supportedVersions`.

## Running Tests

```sh
ros install fukamachi/rove
rove cl-mcp.asd
```

CI / sandbox note:
- Socket-restricted environments may fail `tests/tcp-test.lisp`. Run core suites without TCP via:
  ```sh
  rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
  ```
- Run TCP-specific tests only where binding to localhost is permitted:
  ```sh
  rove tests/tcp-test.lisp
  ```

## Security Model

**cl-mcp is a trusted, local-only development tool.** It is designed to run
on localhost, serving a single developer or AI agent that has the same level
of trust as the user who started it. It is **not** suitable for multi-tenant,
network-facing, or online service deployments.

### Why local-only?

`repl-eval` executes arbitrary Common Lisp code in the host image. Any user
who can reach the MCP endpoint can read/write any file, spawn processes, and
modify the running Lisp image. No amount of application-level access control
can secure this; true multi-tenant isolation would require OS-level mechanisms
(containers, separate processes, Unix users) and is out of scope.

### Convenience guardrails (not security boundaries)

The following mechanisms exist to prevent **accidental** mistakes, not to
enforce security. They are all trivially bypassable via `repl-eval`:

- **Project root enforcement** — File tools restrict reads/writes to the
  project root (and ASDF system source directories for reads). This prevents
  accidentally writing to `/etc` or reading unrelated files, but
  `(with-open-file ...)` in the REPL has no such restriction.
- **Broad rootPath rejection** — `initialize` and `fs-set-project-root`
  reject overly broad roots like `"/"`. This is a safety default, not a
  security gate.
- **HTTP auth token** — `start-http-server` accepts an optional `:token`
  parameter (default: `nil`, no auth). When enabled, it gates HTTP access
  with a Bearer token. This can prevent accidental cross-process access on
  localhost but is not a substitute for network-level security.
- **safe_read** — Disables the `#.` reader macro to prevent read-time code
  execution. Useful when parsing untrusted Lisp syntax, but `eval` itself
  is always permitted.
- **Session management** — HTTP sessions track connection state and manage
  worker process lifetimes. With the worker pool enabled, each session gets
  a dedicated child SBCL process, providing OS-level process isolation.
  This is a resource management and stability mechanism (a crash in one
  session's worker does not affect others), not a security boundary.

## License
MIT
