# cl-mcp
[![CI](https://github.com/masatoi/cl-mcp/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/masatoi/cl-mcp/actions/workflows/ci.yml)

A minimal Model Context Protocol (MCP) server for Common Lisp. It provides a
newline‑delimited JSON‑RPC 2.0 transport over stdio or TCP, a small protocol
layer (initialize, ping, tools/list, tools/call), and a REPL tool that evaluates
forms and returns the last value.

This repo is intentionally small and test-first. It’s designed for editor/agent
clients to drive Common Lisp development via MCP.

## Features
- JSON‑RPC 2.0 request/response framing (one message per line)
- MCP initialize handshake with capability discovery
- Tools API
  - `repl-eval` — evaluate forms
  - `fs-read-file` / `fs-write-file` / `fs-list-directory` — project-scoped file access with allow‑list
  - `lisp-read-file` — Lisp-aware file viewer with collapsed/expanded modes
  - `code-find` / `code-describe` — sb-introspect based symbol lookup/metadata
- Transports: `:stdio` and `:tcp`
- Structured JSON logs with level control via env var
- Rove test suite wired through ASDF `test-op`

## Protocol Support
- Protocol versions recognized: `2025-06-18`, `2025-03-26`, `2024-11-05`
  - On `initialize`, if the client’s `protocolVersion` is supported it is echoed
    back; if it is **not** supported the server returns `error.code = -32602`
    with `data.supportedVersions`.

## Requirements
- SBCL 2.x (developed with SBCL 2.5.x)
- Quicklisp (for dependencies)
- Dependencies (via ASDF/Quicklisp): `alexandria`, `yason`, `usocket`, `bordeaux-threads`, `rove` (tests)

Note: The repository currently uses `yason` for JSON.

## Quick Start
Load and run from an existing REPL:

```lisp
(ql:quickload :cl-mcp)

;; Start TCP transport on an ephemeral port, print chosen port. This make a new thread.
(cl-mcp:start-tcp-server-thread :port 12345)
```

Or run a minimal stdio loop (one JSON‑RPC line per request):

```lisp
(cl-mcp:run :transport :stdio)
```

### Try it with the bundled clients
- Python TCP one‑shot client (initialize):

```bash
python3 scripts/client_init.py --host 127.0.0.1 --port 12345 --method initialize --id 1
```

- Stdio↔TCP bridge (connect editor’s stdio to the TCP server):

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | \
  python3 scripts/stdio_tcp_bridge.py --host 127.0.0.1 --port 12345
```

The bridge uses a bounded connect timeout but disables read timeouts after
connecting, so it can stay idle indefinitely (until stdin closes).

## Tools
### `repl-eval`
Evaluate one or more forms and return the last value as a text item.

Input schema (JSON):
- `code` (string, required): one or more s‑expressions
- `package` (string, optional): package to evaluate in (default `CL-USER`)
- `printLevel` (integer|null): binds `*print-level*`
- `printLength` (integer|null): binds `*print-length*`
Output fields:
- `content`: last value as text (existing)
- `stdout`: concatenated standard output from evaluation
- `stderr`: concatenated standard error from evaluation

Example JSON‑RPC request:

```json
{"jsonrpc":"2.0","id":2,"method":"tools/call",
 "params":{"name":"repl-eval","arguments":{"code":"(+ 1 2)"}}}
```

Response (excerpt):

```json
{"result":{"content":[{"type":"text","text":"3"}]}}
```

### `fs-read-file`
Read text from an allow‑listed path.

Input:
- `path` (string, required): project‑relative or absolute inside a registered ASDF system’s source tree
- `offset` / `limit` (integer, optional): substring window

Policy: reads are allowed only when the resolved path is under the project root or under `asdf:system-source-directory` of a registered system.
Dependency libs: reading source in Quicklisp/ASDF dependencies is permitted **only via `fs-read-file`**; do not shell out for metadata (`wc`, `stat`, etc.). File length is intentionally not returned—page through content with `limit`/`offset` when needed.

### `fs-write-file`
Write text to a file under the project root (directories auto-created).

Input:
- `path` (string, required): **must be relative** to the project root
- `content` (string, required)

Policy: writes outside the project root are rejected.

### `fs-list-directory`
List entries in a directory (files/directories only, skips hidden and build artifacts).

Input:
- `path` (string, required): project root or an ASDF system source dir.

Returns: `entries` array plus human-readable `content`.

### `lisp-read-file`
Read a file with Lisp-aware collapsing and optional pattern-based expansion.

Inputs:
- `path` (string, required): absolute path or project-relative.
- `collapsed` (boolean, default `true`): when `true` and the file is Lisp source
  (`.lisp`, `.asd`, `.ros`, `.cl`, `.lsp`), return only top-level signatures
  (e.g., `(defun name (args) ...)`) while keeping `in-package` forms fully
  shown.
- `namePattern` (string, optional): CL-PPCRE regex; matching definition names are
  expanded even in collapsed mode.
- `contentPattern` (string, optional): CL-PPCRE regex applied to form bodies; if
  it matches, the full form is expanded. For non-Lisp files, this triggers a
  grep-like text filter with ±5 lines of context.
- `offset` / `limit` (integer, optional): slice window used when `collapsed` is
  `false`; defaults to `offset=0`, `limit=2000` lines.

Output fields:
- `content`: formatted text (collapsed Lisp view, raw slice, or filtered text).
- `path`: normalized native pathname.
- `mode`: one of `lisp-collapsed`, `raw`, `text-filtered`, `lisp-snippet`,
  `text-snippet` depending on inputs and file type.
- `meta`: includes `total_forms`/`expanded_forms` for collapsed Lisp, or
  `total_lines` plus `truncated` flag for slices/filters.

### `check-parens`
Check balanced parentheses/brackets in a file slice or provided code; returns the first mismatch position.

Input:
- `path` (string, optional): absolute path inside the project or registered ASDF system (mutually exclusive with `code`)
- `code` (string, optional): raw code string (mutually exclusive with `path`)
- `offset` / `limit` (integer, optional): window when reading from `path`

Output:
- `ok` (boolean)
- when not ok: `kind` (`extra-close` | `mismatch` | `unclosed` | `too-large`), `expected`, `found`, and `position` (`offset`, `line`, `column`).

Notes:
- Uses the same read allow-list and 2 MB cap as `fs-read-file`.
- Ignores delimiters inside strings, `;` line comments, and `#| ... |#` block comments.

### `code-find`
Return definition location (path, line) for a symbol using SBCL `sb-introspect`.

Input:
- `symbol` (string, required): prefer package-qualified, e.g., `"cl-mcp:version"`
- `package` (string, optional): used when `symbol` is unqualified; must exist

Output:
- `path` (relative when inside project, absolute otherwise)
- `line` (integer or null if unknown)

### `code-describe`
Return symbol metadata (name, type, arglist, documentation).

Input:
- `symbol` (string, required)
- `package` (string, optional): must exist when `symbol` is unqualified

Output:
- `type` ("function" | "macro" | "variable" | "unbound")
- `arglist` (string)
- `documentation` (string|null)

## Logging
- Structured JSON line logs to `*error-output*`.
- Control level via env var `MCP_LOG_LEVEL` with one of: `debug`, `info`, `warn`, `error`.

Example:

```bash
MCP_LOG_LEVEL=debug sbcl --eval '(ql:quickload :cl-mcp)' ...
```

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

What’s covered:
- Version/API surface sanity
- REPL evaluation semantics (reader eval enabled)
- Protocol handshake (`initialize`, `ping`, tools listing/calls)
- Logging of RPC dispatch/results
- TCP server accept/respond (newline‑delimited JSON)
- Stdio↔TCP bridge stays alive on idle and exits cleanly when stdin closes

Note: Running tests compiles FASLs into `~/.cache/...`. Ensure your environment
allows writing there or configure SBCL’s cache directory accordingly.

## Project Layout
- `src/` — packages, logging, REPL, protocol, TCP, run entrypoint
- `tests/` — Rove test suites invoked by ASDF `test-op`
- `scripts/` — helper clients and a stdio↔TCP bridge
- `cl-mcp.asd` — main and test systems (delegates `test-op` to Rove)

## Security Notes
- Reader and runtime evaluation are both enabled. Treat this as a trusted,
  local-development tool; untrusted input can execute arbitrary code in the
  host Lisp image.
- File access:
  - Reads: project root, or `asdf:system-source-directory` of registered systems.
  - Writes: project root only; absolute paths are rejected.
- If exposure beyond trusted usage is planned, add allowlists, resource/time
  limits, and output caps.

## Troubleshooting
- Bridge exits after a few seconds of inactivity: ensure you’re using the
  bundled `scripts/stdio_tcp_bridge.py` (it disables read timeouts after
  connect) and that your stdin remains open.
- Permission errors compiling FASLs during tests: allow writes under `~/.cache`
  or reconfigure SBCL’s cache path.
- No output on stdio: remember the protocol is one JSON‑RPC message per line.
  Each request must end with a newline and the server will answer with exactly
  one line (or nothing for notifications).

## Roadmap
- Error taxonomy as condition types mapped to JSON‑RPC errors
- Bounds/quotas for tool outputs (content length caps)

## License
MIT
