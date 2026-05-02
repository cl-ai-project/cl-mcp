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

For the full list of tools with input/output schemas, see [docs/tools.md](docs/tools.md).

## Requirements
- SBCL 2.x (developed with SBCL 2.5.x)
- Quicklisp (optional; pure ASDF also works)
- Dependencies (via ASDF/Quicklisp): runtime — `alexandria`, `cl-ppcre`, `yason`, `usocket`, `bordeaux-threads`, `eclector`, `hunchentoot`; tests — `rove`; optional — `clhs` (loaded on-demand by `clhs-lookup` tool).

## Quick Start

### Install

```bash
# Install from Roswell
ros install cl-ai-project/cl-mcp

# or fetch from GitHub
cd ~/common-lisp
git clone https://github.com/cl-ai-project/cl-mcp.git
```
Load and run from an existing REPL:

```lisp
(asdf:load-system :cl-mcp)  ; or (ql:quickload :cl-mcp) if using Quicklisp

;; Start HTTP server on port 12345
(cl-mcp:start-http-server :port 12345)

;; Start TCP transport on port 12345
(cl-mcp:start-tcp-server-thread :port 12345)
```

Or run a minimal stdio loop (one JSON-RPC line per request):

```bash
;; When use with Roswell
ros run -s cl-mcp -e "(cl-mcp:run :transport :stdio)"

;; When use plain sbcl command
sbcl --eval '(require :asdf)' \
     --eval '(asdf:load-system :cl-mcp)' \
     --eval '(cl-mcp:run :transport :stdio)'
```

**Project root**: File operations require a project root to be set. If
`MCP_PROJECT_ROOT` is not set, the first file access will return an error
prompting you to call `fs-set-project-root`. AI agents handle this
automatically by setting it to their working directory.

### System Prompts

The `prompts/` directory contains system prompts that teach AI agents how to
use cl-mcp tools effectively:

- **`repl-driven-development.md`** — Tool selection guide, REPL-driven workflow
  (explore → experiment → persist → verify), editing patterns, and
  troubleshooting recipes
- **`common-lisp-expert.md`** — Production-quality Common Lisp coding standards,
  CLOS/conditions/restarts conventions, and TDD with Rove

Reference them from your project's `CLAUDE.md` (or equivalent) so the agent
loads them at conversation start:

```markdown
# CLAUDE.md
@prompts/repl-driven-development.md
@prompts/common-lisp-expert.md
```

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

Configure in `~/.codex/config.toml`.

#### Stdio transport

The simplest setup — Codex spawns and manages the server process:

```toml
[mcp_servers.cl-mcp]
command = "ros"
args = ["run", "-s", "cl-mcp", "-e", "(cl-mcp:run)"]
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

```toml
[mcp_servers.cl-mcp]
command = "python3"
args = ["scripts/stdio_tcp_bridge.py", "--host", "127.0.0.1", "--port", "12345"]
```

This gives you the same shared-REPL workflow as Claude Code's HTTP mode —
you keep your SLIME/Sly session while Codex works through the bridge.

## Worker Pool Isolation

Eval-dependent tools (`repl-eval`, `load-system`, `run-tests`, `code-*`,
`inspect-object`) run in isolated child SBCL processes. Each session gets
a dedicated worker with automatic crash recovery and circuit breaker protection.
File-system and editing tools run inline in the parent process.

Disable the worker pool with `MCP_NO_WORKER_POOL=1` or the `:worker-pool` keyword:

```lisp
(cl-mcp:run :transport :stdio :worker-pool nil)
```

## Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `MCP_PROJECT_ROOT` | Project root directory for file operations | client working directory |
| `MCP_LOG_LEVEL` | Log level: `debug`, `info`, `warn`, `error` | `info` |
| `MCP_LOG_FILE` | Log to file (timestamped with PID) | (stderr only) |
| `MCP_NO_WORKER_POOL` | Set to `1` to disable worker pool isolation | (not set = pool enabled) |
| `CL_MCP_WORKER_POOL_WARMUP` | Number of standby workers to maintain (non-negative integer) | `1` |
| `CL_MCP_MAX_POOL_SIZE` | Maximum total workers, bound + standby (positive integer) | `16` |

### Tuning warmup for cold-start handshakes

The worker pool warms its standbys asynchronously once `cl-mcp:run`
returns control to the MCP transport, so the stdio handshake is not
blocked on subprocess launches. If you still see your MCP client
report `Failed to connect` on a cold FASL cache — for example the
first cl-mcp invocation after a system upgrade — set
`CL_MCP_WORKER_POOL_WARMUP=0` to skip pre-spawning entirely. Workers
are then created on demand the first time a session needs one,
trading first-eval latency for the smallest possible startup
footprint. Once your client establishes the session you can ignore
the warmup; the pool will grow as sessions arrive.

## Security Model

**cl-mcp is a trusted, localhost-only development tool.** `repl-eval` executes
arbitrary Common Lisp code in the host image, so any client with MCP access has
full system privileges. Do not expose the endpoint beyond localhost. File
operation restrictions (project root enforcement) are convenience guardrails
to prevent accidental mistakes, not security boundaries.

## License
MIT
