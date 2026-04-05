# cl-mcp
[![CI](https://github.com/cl-ai-project/cl-mcp/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/cl-ai-project/cl-mcp/actions/workflows/ci.yml)
[![Lint](https://github.com/cl-ai-project/cl-mcp/actions/workflows/lint.yml/badge.svg?branch=main)](https://github.com/cl-ai-project/cl-mcp/actions/workflows/lint.yml)

A Model Context Protocol (MCP) server for Common Lisp, providing JSON-RPC 2.0
over stdio, TCP, and HTTP (Streamable HTTP). It enables AI agents to interact
with Common Lisp environments through structured tools for REPL evaluation,
system loading, file operations, code introspection, and structure-aware editing.

This repo is test-first and designed for editor/agent clients to drive Common
Lisp development via MCP.

## Features
- JSON‑RPC 2.0 request/response framing (one message per line)
- MCP initialize handshake with capability discovery
- `notifications/cancelled` support for cooperative request cancellation
- **Worker pool isolation** — eval-dependent tools run in an isolated child SBCL process, separating the MCP server from potentially crashing evaluations. Features automatic crash recovery, circuit breaker, and per-session affinity for multi-client setups
- Tools API
  - `repl-eval` — evaluate forms (returns `result_object_id` for non-primitive results)
  - `load-system` — load ASDF systems with force-reload, output suppression, and timeout support
  - `inspect-object` — drill down into complex objects (CLOS instances, hash-tables, lists, arrays) by ID
  - `fs-read-file` / `fs-write-file` / `fs-list-directory` — project-scoped file access with allow‑list
  - `fs-get-project-info` — report project root and cwd info for path normalization
  - `fs-set-project-root` — set the server's project root and working directory
  - `lisp-read-file` — Lisp-aware file viewer with collapsed/expanded modes
  - `code-find` / `code-describe` / `code-find-references` — sb-introspect based symbol lookup/metadata/xref
  - `lisp-edit-form` — structure-aware edits to top-level forms (replace/insert) using Eclector CST
  - `lisp-patch-form` — scoped text replacement within a matched form (token-efficient small edits)
  - `lisp-check-parens` — detect mismatched delimiters in code slices
  - `clgrep-search` — semantic grep for Lisp files with structure awareness
  - `clhs-lookup` — Common Lisp HyperSpec reference (symbols and sections)
  - `run-tests` — unified test runner with structured results (Rove, ASDF fallback)
  - `pool-status` — worker pool diagnostics (worker counts, states, pool health)
  - `pool-kill-worker` — kill and optionally reset the session's worker process
- Transports: `:stdio`, `:tcp`, and `:http` (Streamable HTTP for Claude Code)
- Structured JSON logs with level control via env var
- Rove test suite wired through ASDF `test-op`

## Protocol Support
- Protocol versions recognized: `2025-06-18`, `2025-03-26`, `2024-11-05`
  - On `initialize`, if the client’s `protocolVersion` is supported it is echoed
    back; if it is **not** supported the server returns `error.code = -32602`
    with `data.supportedVersions`.

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

Or run a minimal stdio loop (one JSON‑RPC line per request):

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

As with Claude Code’s stdio mode, the Lisp process is owned by Codex and
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

This gives you the same shared-REPL workflow as Claude Code’s HTTP mode —
you keep your SLIME/Sly session while Codex works through the bridge.

### Bundled test clients

For manual testing and debugging:

```bash
# Python TCP one-shot client (initialize)
python3 scripts/client_init.py --host 127.0.0.1 --port 12345 --method initialize --id 1

# Stdio↔TCP bridge (pipe JSON-RPC to TCP)
echo ‘{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}’ | \
  python3 scripts/stdio_tcp_bridge.py --host 127.0.0.1 --port 12345
```

## Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `MCP_PROJECT_ROOT` | Project root directory for file operations | client working directory |
| `MCP_LOG_LEVEL` | Log level: `debug`, `info`, `warn`, `error` | `info` |
| `MCP_LOG_FILE` | Log to file (timestamped with PID, e.g., `/tmp/cl-mcp.log` → `/tmp/cl-mcp-2026-03-01T09-15-30-12345.log`) | (stderr only) |
| `MCP_NO_WORKER_POOL` | Set to `1` to disable worker pool isolation | (not set = pool enabled) |

## Tools
### `repl-eval`
Evaluate one or more forms and return the last value as a text item.

Input schema (JSON):
- `code` (string, required): one or more s‑expressions
- `package` (string, optional): package to evaluate in (default `CL-USER`)
- `print_level` (integer|null): binds `*print-level*`
- `print_length` (integer|null): binds `*print-length*`
- `timeout_seconds` (number|null): abort evaluation after this many seconds
- `max_output_length` (integer|null): truncate `content`/`stdout`/`stderr` to this many characters
- `safe_read` (boolean|null): when `true`, disables `*read-eval*` while reading forms
Output fields:
- `content`: last value as text
- `stdout`: concatenated standard output from evaluation
- `stderr`: concatenated standard error from evaluation
- `result_object_id` (integer|null): when the result is a non-primitive object (list, hash-table, CLOS instance, etc.), this ID can be used with `inspect-object` to drill down into its internal structure
- `error_context` (object|null): when an error occurs, contains structured error info including `condition_type`, `message`, `restarts`, and `frames` with local variable inspection

Example JSON‑RPC request:

```json
{"jsonrpc":"2.0","id":2,"method":"tools/call",
 "params":{"name":"repl-eval","arguments":{"code":"(+ 1 2)"}}}
```

Response (excerpt):

```json
{"result":{"content":[{"type":"text","text":"3"}]}}
```

### `inspect-object`
Drill down into non-primitive objects by ID. Objects are registered when `repl-eval`
returns non-primitive results (the `result_object_id` field).

Input:
- `id` (integer, required): Object ID from `repl-eval`'s `result_object_id` or from a previous `inspect-object` call
- `max_depth` (integer, optional): Nesting depth for expansion (0=summary only, default=1)
- `max_elements` (integer, optional): Maximum elements for lists/arrays/hash-tables (default=50)

Output fields:
- `kind`: Object type (`list`, `hash-table`, `array`, `instance`, `structure`, `function`, `other`)
- `summary`: String representation of the object
- `id`: The object's registry ID
- Type-specific fields:
  - Lists: `elements` array with nested value representations
  - Hash-tables: `entries` array with `key`/`value` pairs, `test` function name
  - Arrays: `elements` array, `dimensions`, `element_type`
  - CLOS instances: `class` name, `slots` array with `name`/`value` pairs
  - Structures: `class` name, `slots` array
  - Functions: `name`, `lambda_list` (SBCL only)
- `meta`: Contains `truncated` flag, element counts, etc.

Nested objects are returned as `object-ref` with their own `id` for further inspection.
Circular references are detected and marked as `circular-ref`.

Example workflow:
```json
// 1. Evaluate code that returns a complex object
{"method":"tools/call","params":{"name":"repl-eval","arguments":{"code":"(make-hash-table)"}}}
// Response includes: "result_object_id": 42

// 2. Inspect the object
{"method":"tools/call","params":{"name":"inspect-object","arguments":{"id":42}}}
// Response: {"kind":"hash-table","test":"EQL","entries":[...],"id":42}
```

### `load-system`
Load an ASDF system with structured output and reload support. Preferred over
`(ql:quickload ...)` or `(asdf:load-system ...)` via `repl-eval` for AI agents.

Input:
- `system` (string, required): ASDF system name (e.g., `"cl-mcp"`, `"my-project/tests"`)
- `force` (boolean, default `true`): clear loaded state before loading to pick up file changes
- `clear_fasls` (boolean, default `false`): force full recompilation from source
- `timeout_seconds` (number, default `120`): timeout for the load operation

Output fields:
- `system` (string): echoed system name
- `status` (string): `"loaded"`, `"timeout"`, or `"error"`
- `duration_ms` (integer): load time in milliseconds
- `warnings` (integer): number of compiler warnings (when loaded)
- `warning_details` (string|null): warning text (when warnings > 0)
- `forced` (boolean): whether force-reload was applied
- `clear_fasls` (boolean): whether full recompilation was done
- `message` (string|null): error or timeout message

Solves three problems with using `ql:quickload` via `repl-eval`:
1. **Staleness**: `force=true` (default) clears loaded state before reloading
2. **Output noise**: suppresses verbose compilation/load output
3. **Timeout**: dedicated timeout prevents hanging on large systems

Example JSON-RPC request:
```json
{"jsonrpc":"2.0","id":3,"method":"tools/call",
 "params":{"name":"load-system","arguments":{"system":"cl-mcp"}}}
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
- `name_pattern` (string, optional): CL-PPCRE regex; matching definition names are
  expanded even in collapsed mode.
- `content_pattern` (string, optional): CL-PPCRE regex applied to form bodies; if
  it matches, the full form is expanded. For non-Lisp files, this triggers a
  grep-like text filter with ±5 lines of context.
- `offset` / `limit` (integer, optional): slice window used when `collapsed` is
  `false`; defaults to `offset=0`, `limit=500` lines. When truncated, a
  `[Showing lines A-B of N. Use offset=B to read more.]` footer is appended.

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
preserving surrounding formatting and comments. Supports replace, insert_before, and
insert_after operations with automatic parinfer repair for missing closing parentheses.

Input:
- `file_path` (string, required): absolute path or project-relative path
- `form_type` (string, required): form constructor to match, e.g., `defun`, `defmacro`, `defmethod`
- `form_name` (string, required): name/specializers to match; for `defmethod` include specializers such as `"print-object ((obj my-class) stream)"`
- `operation` (string, required): one of `replace`, `insert_before`, `insert_after`
- `content` (string, required): full form text to insert or replace with
- `dry_run` (boolean, default `false`): preview changes without writing to disk
- `normalize_blank_lines` (boolean, default `true`): normalize blank lines around edited forms
- `readtable` (string, optional): named-readtable designator for files using custom reader macros

Operations:
- **replace**: Replace the entire matched form with `content`
- **insert_before**: Insert `content` as a new form before the matched form
- **insert_after**: Insert `content` as a new form after the matched form

Output:
- `path`, `operation`, `form_type`, `form_name`
- `would_change` (boolean): whether the file was modified
- `bytes`: size of the updated file content
- `content`: human-readable summary string of the applied change

Dry-run output (when `dry_run` is true):
- `would_change` (boolean): whether the operation would modify the file
- `original` (string): the matched form text before changes
- `preview` (string): full file preview with changes applied
- `parinfer_warning` (string, optional): auto-repair warning when closing delimiters are added
- `content`: human-readable summary

### `lisp-patch-form`
Scoped text replacement within a matched top-level Lisp form. Finds `old_text` (exact,
whitespace-sensitive match) within the form and replaces it with `new_text`. Most
token-efficient way to make small changes to large forms.

Does NOT auto-repair parentheses — if the patch breaks form structure, it fails
immediately and no changes are written to disk. Use `lisp-edit-form` instead when
replacing or inserting entire forms.

Input:
- `file_path` (string, required): absolute path or project-relative path
- `form_type` (string, required): form constructor to match, e.g., `defun`, `defmacro`, `defmethod`
- `form_name` (string, required): name/specializers to match; for `defmethod` include specializers such as `"print-object ((obj my-class) stream)"`
- `old_text` (string, required): exact text to find within the matched form (whitespace-sensitive, must match exactly once)
- `new_text` (string, required): replacement text
- `dry_run` (boolean, default `false`): preview changes without writing to disk
- `readtable` (string, optional): named-readtable designator for files using custom reader macros

Output:
- `path`, `form_type`, `form_name`
- `would_change` (boolean): whether the file was modified
- `bytes`: size of the updated file content
- `delta` (integer, present only when `would_change` is true): character count difference (`new_text` length minus `old_text` length)
- `content`: human-readable summary string of the applied change

Dry-run output (when `dry_run` is true):
- `would_change` (boolean): whether the operation would modify the file
- `operation`: always `"patch"`
- `original` (string): the matched form text before changes
- `preview` (string): modified form text after replacement
- `content`: human-readable summary with original and preview form text

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
- `project_only` (boolean, default `true`): limit results to files under the project root

Output:
- `refs` (array): each element includes `path`, `line`, `type` (`call`, `macro`, `bind`, `reference`, `set`), and `context`
- `count` (integer): number of references returned
- `symbol`: echoed symbol name
- `project_only`: whether results were filtered to the project
- `content`: newline-separated human-readable summary of references

### `clhs-lookup`
Look up a symbol or section in the Common Lisp HyperSpec (ANSI standard documentation).

Input:
- `query` (string, required): either a symbol name (e.g., `"loop"`, `"handler-case"`) or a section number (e.g., `"22.3"`, `"3.1.2"`)
- `include_content` (boolean, default `true`): include extracted text content from local HyperSpec

Output:
- `symbol` or `section`: the query identifier (depends on query type)
- `url`: HyperSpec URL (`file://` for local, `http://` for remote fallback)
- `source`: `"local"` or `"remote"`
- `content`: extracted text content (when `include_content` is true and source is local)

The tool auto-detects whether the query is a section number (digits and dots only, starting with a digit) or a symbol name.

Example requests:
```json
{"method":"tools/call","params":{"name":"clhs-lookup","arguments":{"query":"loop"}}}
{"method":"tools/call","params":{"name":"clhs-lookup","arguments":{"query":"22.3"}}}
```

Notes:
- If the HyperSpec is not installed locally, the tool attempts auto-installation via `(clhs:install-clhs-use-local)`
- Section numbers map to filenames: `22.3` → `22_c.htm`, `22.3.1` → `22_ca.htm` (a=1, b=2, c=3, etc.)

### `run-tests`
Run tests for a system and return structured results with pass/fail counts and failure details.

Input:
- `system` (string, required): ASDF system name to test (e.g., `"my-project/tests"`)
- `framework` (string, optional): Force a specific framework (`"rove"`, `"fiveam"`, or `"auto"` for auto-detect)
- `test` (string, optional): Run only a specific test by fully qualified name (e.g., `"my-package::my-test-name"`)

Output:
- `passed` (integer): Number of passed tests
- `failed` (integer): Number of failed tests
- `pending` (integer): Number of pending/skipped tests (Rove only)
- `framework` (string): Framework used (`"rove"` or `"asdf"`)
- `duration_ms` (integer): Execution time in milliseconds
- `failed_tests` (array, when failures exist): Detailed failure information including:
  - `test_name`: Name of the failing test
  - `description`: Test description
  - `form`: The failing assertion form
  - `values`: Evaluated values
  - `reason`: Error message (string)
  - `source`: Source location (file and line)

Example requests:
```json
// Run all tests in a system
{"method":"tools/call","params":{"name":"run-tests","arguments":{"system":"cl-mcp/tests/clhs-test"}}}

// Run a single test
{"method":"tools/call","params":{"name":"run-tests","arguments":{"system":"cl-mcp/tests/clhs-test","test":"cl-mcp/tests/clhs-test::clhs-lookup-symbol-returns-hash-table"}}}
```

Notes:
- Auto-detects Rove framework when loaded; falls back to ASDF `test-system` for text capture
- Single test execution requires the test package to be loaded first
- Test names must be fully qualified with package prefix (e.g., `"package::test-name"`)

### `pool-status`
Return worker pool diagnostic information. No arguments required.

Output fields:
- `pool_running` (boolean): whether the pool health monitor is active
- `total_workers` (integer): total live workers (bound + standby)
- `bound_count` (integer): workers assigned to sessions
- `standby_count` (integer): idle workers available for assignment
- `max_pool_size` (integer): configured maximum worker count
- `warmup_target` (integer): target number of warm standby workers
- `workers` (array): per-worker details including `id`, `state` (`bound` or `standby`), `session` (string or null, truncated to 8 chars), `pid`, and `tcp_port`

Example request:
```json
{"jsonrpc":"2.0","id":4,"method":"tools/call",
 "params":{"name":"pool-status","arguments":{}}}
```

### `pool-kill-worker`
Kill the worker process bound to the current session. All Lisp state (loaded
systems, REPL definitions, packages) in the worker is lost. Use this when a
worker is stuck, has corrupted state, or you want a clean environment.

Input:
- `reset` (boolean, default `false`): when `true`, immediately spawn a replacement
  worker after killing the current one; when `false`, defer spawning until the next
  tool call that needs a worker

Output fields:
- `killed` (boolean): whether a worker was actually killed
- `reset` (boolean|null): whether a replacement was spawned (only present when `killed` is true)
- `cancelled_spawn` (boolean|null): true if a pending spawn was cancelled instead of killing a live worker
- `isError` (boolean|null): true if kill succeeded but replacement spawn failed

In both modes, you must call `load-system` again to restore previously loaded systems.

Example requests:
```json
// Kill worker, let next tool call spawn a fresh one
{"method":"tools/call","params":{"name":"pool-kill-worker","arguments":{}}}

// Kill and immediately spawn a replacement
{"method":"tools/call","params":{"name":"pool-kill-worker","arguments":{"reset":true}}}
```

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
- Worker pool lifecycle, crash recovery, circuit breaker, and pool-status
- Request cancellation (`notifications/cancelled`) with worker termination

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

## License
MIT
