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

## Attach Mode (opt-in)

By default, every `repl-eval` runs in a forked, hermetic worker. That is
the right default for most cl-mcp users — it isolates the AI agent from
the host image — but it makes one workflow awkward: live debugging or
UAT against a long-running application image (a strategy server, a
daemon, a game). The hermetic worker has none of that application's
state.

**Attach mode** is an opt-in escape hatch. When configured, cl-mcp
points `repl-eval` at a user-supplied, already-running [Slynk] listener
and routes the evaluation through `slynk-client:slime-eval` instead of
through the worker pool. Other tools and other sessions remain on the
hermetic-worker path; attach mode is per-session and covers `repl-eval`
only in v1.

[Slynk]: https://github.com/joaotavora/sly

### Configuring it

Two equivalent surfaces:

```bash
# Environment variable (read once on cl-mcp:run entry)
CL_MCP_SLYNK_ATTACH=127.0.0.1:4005 ros run -s cl-mcp -e '(cl-mcp:run)'
```

```lisp
;; Keyword argument on cl-mcp:run (takes precedence over the env var)
(cl-mcp:run :transport :stdio :slynk-attach "127.0.0.1:4005")
```

In your application image, start a Slynk listener on the same port
*before* starting cl-mcp:

```lisp
(asdf:load-system :slynk)
(slynk:create-server :port 4005 :dont-close t)
```

Now any `repl-eval` issued through cl-mcp evaluates inside that running
image. State persists across calls — exactly what live debugging and
UAT need.

### Invariants

- **Per-session, single connection.** Each MCP session opens one Slynk
  connection and reuses it for the session's lifetime. Cross-session
  sharing is not implemented.
- **`repl-eval` only.** Other tools (`load-system`, `run-tests`, the
  `code-*` family, `inspect-object`) still go through the hermetic
  worker pool. Routing them through attach mode is on the roadmap as
  per-tool follow-ups.
- **Hermetic-worker path unchanged when attach is unset.** With
  `CL_MCP_SLYNK_ATTACH` unset and `:slynk-attach` not passed, cl-mcp
  behaves bit-for-bit as before — no extra dispatch, no Slynk traffic.
- **Concurrency is serialised per connection.** `slime-eval` is
  synchronous, so cl-mcp serialises concurrent `repl-eval` calls on a
  single connection with a `bordeaux-threads` lock.
- **Network errors fail closed.** If the connection drops mid-call, the
  call returns an `isError` result and the cached connection is
  discarded; the next call reconnects on demand. There is no automatic
  retry inside a single call.

### Licensing note

Attach mode depends on the [`slynk-client`] library, which is **GPL
v2**. cl-mcp itself is MIT-licensed; the combined-work implication of
the dependency is a maintainer-review item. Until that review lands,
the dependency is a direct `:depends-on` entry in `cl-mcp.asd` —
i.e. building cl-mcp pulls in `slynk-client`. A follow-up may move
attach mode behind a separate ASDF system loaded only when configured,
in which case cl-mcp's MIT core stays GPL-clean. If the licensing
posture matters for your deployment, either avoid configuring attach
mode (the GPL code is loaded but never exercised) or watch the
upstream PR thread for the optional-system restructuring.

[`slynk-client`]: https://github.com/Shookakko/slynk-client

### Local-development setup

`slynk-client` is not on Quicklisp; the cl-mcp dev workflow expects it
to be checked out alongside cl-mcp under `$LISP_WORKSPACE` (or anywhere
ASDF's source registry can find it). `scripts/setup-dev.sh` documents
the layout and clones the pinned fork if it isn't already present:

```bash
./scripts/setup-dev.sh
```

CI configuration is out of scope for v1. If your CI pipeline needs to
build cl-mcp with attach support, replicate what the script does or add
a `flake.nix` input — the maintainer's preferred path will be settled
during PR review.

### Opting out

Unset `CL_MCP_SLYNK_ATTACH` and omit `:slynk-attach` from `cl-mcp:run`.
With both unset, attach mode is inert and the worker-pool path is
identical to a build that does not include attach.

## Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `MCP_PROJECT_ROOT` | Project root directory for file operations | client working directory |
| `MCP_LOG_LEVEL` | Log level: `debug`, `info`, `warn`, `error` | `info` |
| `MCP_LOG_FILE` | Log to file (timestamped with PID) | (stderr only) |
| `MCP_NO_WORKER_POOL` | Set to `1` to disable worker pool isolation | (not set = pool enabled) |
| `CL_MCP_WORKER_POOL_WARMUP` | Number of standby workers to maintain (non-negative integer) | `1` |
| `CL_MCP_MAX_POOL_SIZE` | Maximum total workers, bound + standby (positive integer) | `16` |
| `CL_MCP_SLYNK_ATTACH` | `host:port` of a running Slynk listener to route `repl-eval` to (see Attach Mode) | (not set = hermetic worker) |

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
