# cl-mcp
[![CI](https://github.com/cl-ai-project/cl-mcp/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/cl-ai-project/cl-mcp/actions/workflows/ci.yml)

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
  - `fs-get-project-info` — report project root and cwd info for path normalization
  - `fs-set-project-root` — set the server's project root and working directory
  - `lisp-read-file` — Lisp-aware file viewer with collapsed/expanded modes
  - `code-find` / `code-describe` / `code-find-references` — sb-introspect based symbol lookup/metadata/xref
  - `lisp-edit-form` — structure-aware edits to top-level forms using Eclector CST
  - `lisp-check-parens` — detect mismatched delimiters in code slices
  - `clgrep-search` / `clgrep-signatures` — semantic grep for Lisp files with structure awareness
- Transports: `:stdio`, `:tcp`, and `:http` (Streamable HTTP for Claude Code)
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
- Dependencies (via ASDF/Quicklisp): runtime — `alexandria`, `cl-ppcre`, `yason`, `usocket`, `bordeaux-threads`, `eclector`, `hunchentoot`; tests — `rove`.

## Quick Start

**IMPORTANT**: Set the `MCP_PROJECT_ROOT` environment variable before starting:

```bash
export MCP_PROJECT_ROOT=/path/to/your/project
```

Load and run from an existing REPL:

```lisp
(ql:quickload :cl-mcp)

;; Start TCP transport on port 12345 in a new thread.
(cl-mcp:start-tcp-server-thread :port 12345)
```

Or run a minimal stdio loop (one JSON‑RPC line per request):

```bash
# With environment variable
export MCP_PROJECT_ROOT=$(pwd)
ros run -s cl-mcp -e "(cl-mcp:run :transport :stdio)"
```

**Alternative**: If you don't set `MCP_PROJECT_ROOT`, you must call `fs-set-project-root`
tool immediately after connecting to initialize the project root.

### HTTP Transport (for Claude Code)

Start the HTTP server and continue using your REPL:

```lisp
(ql:quickload :cl-mcp)

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

This is the recommended approach for Common Lisp development:
1. Start your REPL as usual
2. Load cl-mcp and start the HTTP server
3. Configure Claude Code to connect
4. Both you and Claude Code can use the same Lisp runtime simultaneously

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
- `timeoutSeconds` (number|null): abort evaluation after this many seconds
- `maxOutputLength` (integer|null): truncate `content`/`stdout`/`stderr` to this many characters
- `safeRead` (boolean|null): when `true`, disables `*read-eval*` while reading forms
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

### ASDF tools (disabled)
`asdf-system-info` and `asdf-list-systems` are temporarily disabled.

<!--
### `asdf-system-info`
Return detailed information about an ASDF system, including dependencies and source locations.

Input:
- `system_name` (string, required): ASDF system name (e.g., `"cl-mcp"`, `"alexandria"`)

Output fields:
- `name` (string)
- `version` / `description` / `author` / `license` (string|null)
- `depends_on` (array): direct dependencies
- `defsystem_depends_on` (array): defsystem dependencies
- `source_file` / `source_directory` (string|null)
- `loaded` (boolean)

Example JSON‑RPC request:
```json
{"jsonrpc":"2.0","id":3,"method":"tools/call",
 "params":{"name":"asdf-system-info","arguments":{"system_name":"cl-mcp"}}}
```

### `asdf-list-systems`
List all registered ASDF systems (may be large).

Input: none

Output: array of lower-case system names.
-->

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

### `fs-get-project-info`
Report project root and current working directory information for clients that need
to normalize relative paths.

Output:
- `project_root` (string): resolved project root
- `cwd` (string|null): current working directory
- `project_root_source` (string): one of `env` or `explicit`
- `relative_cwd` (string|null): cwd relative to project root when inside it

### `fs-set-project-root`
Synchronize the server's project root and working directory with the client's location.

Input:
- `path` (string, required): path to the project root directory (absolute preferred; relative is resolved to an absolute directory)

This tool allows AI agents to explicitly set the server's working directory, ensuring
path resolution works correctly. The server updates both `*project-root*` and the
current working directory (via `uiop:chdir`).

Output:
- `project_root` (string): new project root path
- `cwd` (string): new current working directory
- `previous_root` (string): previous project root path
- `status` (string): confirmation message

**Best Practice for AI Agents:** Call `fs-set-project-root` at the beginning of your
session with your current working directory to ensure file operations work correctly.

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

### `lisp-check-parens`
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

### `lisp-edit-form`
Perform structure-aware edits to a top-level form using Eclector CST parsing while
preserving surrounding formatting and comments.

Input:
- `file_path` (string, required): absolute path or project-relative path
- `form_type` (string, required): form constructor to match, e.g., `defun`, `defmacro`, `defmethod`
- `form_name` (string, required): name/specializers to match; for `defmethod` include specializers such as `"print-object (my-class t)"`
- `operation` (string, required): one of `replace`, `insert_before`, `insert_after`
- `content` (string, required): full form text to insert

Output:
- `path`, `operation`, `form_type`, `form_name`
- `bytes`: size of the updated file content
- `content`: human-readable summary string of the applied change

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

### `code-find-references`
Return cross-reference locations for a symbol using SBCL `sb-introspect`.

Input:
- `symbol` (string, required)
- `package` (string, optional): package used when `symbol` is unqualified
- `projectOnly` (boolean, default `true`): limit results to files under the project root

Output:
- `refs` (array): each element includes `path`, `line`, `type` (`call`, `macro`, `bind`, `reference`, `set`), and `context`
- `count` (integer): number of references returned
- `symbol`: echoed symbol name
- `projectOnly`: whether results were filtered to the project
- `content`: newline-separated human-readable summary of references

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

## Recommended System Prompts

The `prompts/` directory contains recommended system prompts for AI agents working with cl-mcp.

### REPL-Driven Development Prompt

**File**: [`prompts/repl-driven-development.md`](prompts/repl-driven-development.md)

This comprehensive guide teaches AI agents how to effectively use cl-mcp's tools for interactive Common Lisp development. It covers:

- **Initial setup**: Project root configuration and session initialization
- **Core philosophy**: The "Tiny Steps with Rich Feedback" approach (EXPLORE → DEVELOP → EDIT → VERIFY)
- **Tool usage guidelines**: When to use `lisp-edit-form` vs `fs-write-file`, `lisp-read-file` vs `fs-read-file`, and `repl-eval` best practices
- **Common Lisp specifics**: Package handling, dependency loading, pathname resolution
- **Recommended workflows**: Step-by-step guides for common tasks (modifying functions, debugging, running tests, adding features)
- **Troubleshooting**: Diagnosis and solutions for common errors
- **Performance considerations**: Token-efficient strategies for large codebases

**Usage**:

1. **For Claude Code users**: Reference this prompt in your `CLAUDE.md` or `AGENTS.md`:
   ```markdown
   @/path/to/cl-mcp/prompts/repl-driven-development.md
   ```

2. **For other AI agents**: Include the prompt content in your agent's system instructions or configuration file.

3. **For MCP client configuration**: Add instructions field to your `mcp.json`:
   ```json
   {
     "mcpServers": {
       "cl-mcp": {
         "instructions": "Follow the guidelines in prompts/repl-driven-development.md for Common Lisp development with this server."
       }
     }
   }
   ```

The prompt is designed to help AI agents make optimal use of cl-mcp's structural editing and introspection capabilities, avoiding common pitfalls like overwriting files or working in the wrong package context.

## AI Agent Configuration

When using cl-mcp with AI agents like Claude Code, you should configure the agent to
synchronize the project root at the start of each session.

### Recommended Setup

Add server-specific instructions to your MCP client configuration. For Claude Code,
edit your `mcp.json` or configuration file:

```json
{
  "mcpServers": {
    "cl-mcp": {
      "command": "ros",
      "args": ["run", "-l", "cl-mcp", "-e", "(cl-mcp:run)"],
      "env": {
        "MCP_PROJECT_ROOT": "${workspaceFolder}"
      },
      "instructions": "IMPORTANT: At the start of your session, call fs-set-project-root with the absolute path of your current working directory (e.g., /home/user/project) to synchronize the server's project root. This ensures all file operations work correctly."
    }
  }
}
```

### Alternative: Environment Variable

You can also set `MCP_PROJECT_ROOT` environment variable before starting the server:

```bash
export MCP_PROJECT_ROOT=/path/to/your/project
ros run -s cl-mcp -e "(cl-mcp:run)"
```

The server will use this path during initialization, though calling `fs-set-project-root`
explicitly is still recommended for dynamic project switching.

## Roadmap
- Error taxonomy as condition types mapped to JSON‑RPC errors
- Bounds/quotas for tool outputs (content length caps)

## License
MIT
