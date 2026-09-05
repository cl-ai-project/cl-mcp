# Tools Reference

Detailed input/output schemas and examples for all cl-mcp tools.

## `repl-eval`
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

## `inspect-object`
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

## `load-system`
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

## `fs-read-file`
Read text from an allow‑listed path.

Input:
- `path` (string, required): project‑relative or absolute inside a registered ASDF system's source tree
- `offset` / `limit` (integer, optional): substring window

Policy: reads are allowed only when the resolved path is under the project root or under `asdf:system-source-directory` of a registered system.
Dependency libs: reading source in Quicklisp/ASDF dependencies is permitted **only via `fs-read-file`**; do not shell out for metadata (`wc`, `stat`, etc.). File length is intentionally not returned—page through content with `limit`/`offset` when needed.

## `fs-write-file`
Write text to a file under the project root (directories auto-created).

Input:
- `path` (string, required): **must be relative** to the project root
- `content` (string, required)
- `allow_unparseable_overwrite` (boolean, optional): permit overwriting an
  existing `.lisp`/`.asd` file that the structural tools cannot parse.

Policy: writes outside the project root are rejected. An existing `.lisp`/`.asd`
file is never overwritten by default (`existing_lisp_overwrite_forbidden`; use
`lisp-edit-form`). With `allow_unparseable_overwrite: true` the file is parsed
first, and the write is allowed only when the parse fails on a delimiter (a
missing or stray `)`, or an unterminated string or `#|` comment) that no
readtable could fix; a file that parses, a truncated read, or an unreadable
file is still refused. The intended recovery loop is `lisp-check-parens` →
`fs-read-file` → `fs-write-file` with the flag; `path` must be relative to the
project root, and the guidance the tools print gives it in that form. The
plain refusal (no flag) carries `allow_unparseable_overwrite_available: true`
in its error data so a client can discover the opt-in.

## `fs-list-directory`
List entries in a directory (files/directories only, skips hidden and build artifacts).

Input:
- `path` (string, required): project root or an ASDF system source dir.

Returns: `entries` array plus human-readable `content`.

## `fs-get-project-info`
Report project root and current working directory information for clients that need
to normalize relative paths.

Output:
- `project_root` (string): resolved project root
- `cwd` (string|null): current working directory
- `project_root_source` (string): one of `env` or `explicit`
- `relative_cwd` (string|null): cwd relative to project root when inside it

## `fs-set-project-root`
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

## `lisp-read-file`
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

An **expanded** form is echoed from the file verbatim, so its text — including
comments inside the form — is exactly what is on disk, and the `NNN:` prefix on
each line is that line of the file. Text copied out of an expanded form can
therefore be used directly as `lisp-patch-form`'s `old_text`, which matches raw,
whitespace-sensitive text. **Collapsed** signature lines are printed rather than
quoted (a signature is a summary, not a quotation), and so is the subject of
`content_pattern`: the pattern matches the printed form, so it describes
structure rather than the file's whitespace.

Output fields:
- `content`: formatted text (collapsed Lisp view, raw slice, or filtered text).
- `path`: normalized native pathname.
- `mode`: one of `lisp-collapsed`, `raw`, `text-filtered`, `lisp-snippet`,
  `text-snippet` depending on inputs and file type.
- `meta`: includes `total_forms`/`expanded_forms` for collapsed Lisp, or
  `total_lines` plus `truncated` flag for slices/filters.

## `lisp-check-parens`
Check balanced parentheses/brackets in a file slice or provided code; returns the first mismatch position.

Input:
- `path` (string, optional): absolute path inside the project or registered ASDF system (mutually exclusive with `code`)
- `code` (string, optional): raw code string (mutually exclusive with `path`)
- `offset` / `limit` (integer, optional): window when reading from `path`

Output:
- `ok` (boolean)
- when not ok: `kind` (`extra-close` | `mismatch` | `unclosed` |
  `unclosed-block-comment` | `unclosed-string` | `reader-error` | `too-large`),
  `expected`, `found`, and `position` (`offset`, `line`, `column`; absent for
  `too-large`, where nothing was scanned); for an `unclosed-string` or
  `unclosed-block-comment` the position is the opening `"` or `#|`.
- for paren failures (`extra-close`, `mismatch`, `unclosed`): `likely_fixes`, a
  vector of `{line, original, repaired, delta, added, removed, column,
  removed_columns, before_comment, truncated}` inferred by parinfer from
  indentation (at most 10 entries; the rest are counted in
  `likely_fixes_omitted`). `column` is the 1-based column of the first change
  (where an insertion goes), `removed_columns` the columns of the `)` a
  removal drops, `before_comment` says the change goes before a trailing `;`
  comment, and `truncated` says `original`/`repaired` were cut to 120
  characters and are then descriptive, not text to write back. Empty when no
  repair could be inferred.
- for `unclosed`: `next_top_level_line`, the line of the first column-0 `(`
  seen while the form was still open, when there is one.
- `false_positive` (`true`, present only then): the editing tools' reader
  accepts the input (a `path` or inline `code`), so the scan's finding is most
  likely a false positive (`a]`, `foo#|bar|`, a reader macro); no
  `likely_fixes` or instruction is attached in that case.
- The summary text repeats the diagnosis in prose: the unclosed form's line and
  head, a `Likely fix, inferred from indentation:` block, and the next top-level
  form hint. Each likely-fix line is written to be applied verbatim: `add N ")"`
  only when the closers go at the very end of the line, `remove N ")"` for a
  pure removal, `insert N ")" at column C (before the trailing ; comment)` when
  they go before a comment, and otherwise the resulting line; a line cut at the
  120-character bound is described by position, never offered as text to write.
  When a fix closes a form whose next code line sits at the same indentation
  (the shape of a body that was meant to stay inside it), a NOTE says the
  lines below have left that form; when the fix rests on an unclosed `[`/`{`,
  a reminder says the `)` fixes are wrong if that bracket was meant as `(`.
  A verdict the editing reader contradicts is headlined as a likely false
  positive, with no edit instruction.

- `next_tool` / `fix_code` / `required_args`: for inline `code`, `lisp-edit-form`
  (which repairs and writes a form). For a `path` that the edit tools' own
  parser (the same verdict `fs-write-file`'s guard uses) finds to fail on a
  delimiter no readtable can fix, `fs-write-file` with
  `allow_unparseable_overwrite`, since the structural tools cannot locate any
  form in such a file. A file that parses (a symbol such as `a[b`), one that
  fails for a reader-level reason (`#.`, an unknown `#?`), or a windowed read
  keeps the `lisp-edit-form` hint, so the hint never promises an overwrite the
  guard would refuse.

Notes:
- Uses the same read allow-list as `fs-read-file`. Input over 2 MB, or a file
  read that `fs-read-file` truncated at its 1 MB cap, is reported as
  `kind: too-large` rather than diagnosed from a prefix.
- When a `path` fails the scan, the file is also parsed with the editing tools'
  reader (`*read-eval*` off; an in-file `in-readtable` is honoured, so its
  reader macros run in the server process, as they do for `lisp-read-file`)
  to decide the next step; inline `code` that fails the scan is read the same
  way (an `in-readtable` inside the snippet is honoured too). When that reader
  accepts the input, the scan's
  finding is a false positive: the text says so first, no `likely_fixes` /
  `next_top_level_line` are returned, and no "Replace it with" / "Close it
  with" instruction is attached. The overwrite hint is offered only for a file
  under the project root, since that is all `fs-write-file` can write.
- A windowed read (`offset` > 0, or `limit` that the file fills) is a prefix
  too: `kind` and `position` are reported, but no `likely_fixes`,
  `next_top_level_line` or diagnosis text, because a valid file's slice looks
  unbalanced. `position.line`/`column` count from the start of the window,
  `position.offset` from the start of the file.
- Ignores delimiters inside strings, `;` line comments, `#| ... |#` block
  comments (nested), `#\x` character literals, `\`-escaped characters and
  `|...|` symbols.
- `[`/`{` are still tracked as delimiters, so a symbol such as `a[b` produces a
  `mismatch` that the text flags as a possible false positive.

## `lisp-edit-form`
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

Auto-repair: when `content` does not read, missing `)` are inferred from
**indentation** (parinfer indent mode) and the repaired form is written. The
inference can place a `)` on the wrong line when the indentation is not what
you meant, moving a sub-form in or out of its parent while still producing
readable code. The response therefore shows the changed lines and the repaired
form; check them, and use `dry_run: true` when the content is non-trivial. A
`]` or `}` left where `)` was meant, and any leftover the repair cannot make
readable, is refused with the same line-level diagnosis as `lisp-check-parens`
and nothing is written. A repair that would change text inside a string or a
comment is refused too, even when the result happens to read: a fix the tool
would not suggest is not written either. Content that reads but whose scan
finds a `]`/`}` where `)` was expected (`(list a] 1)`) is written and flagged
with `bracket_warning` (also in the summary and in dry-run), as
`lisp-patch-form` does. A `readtable` argument (or an `in-readtable` earlier in
the file) that actually changes the syntax switches these standard-syntax
verdicts off and leaves the verdict to the reader.

Output:
- `path`, `operation`, `form_type`, `form_name`
- `would_change` (boolean): whether the file was modified
- `bytes`: size of the updated file content
- `bracket_warning` (string, optional): the content reads, but its delimiter
  scan found a `]` or `}` where `)` was expected (a symbol character in
  standard syntax, so a `)` typo survives); the edit is applied and the
  summary repeats the warning. Also present in dry-run output.
- `content`: human-readable summary string of the applied change

Dry-run output (when `dry_run` is true):
- `would_change` (boolean): whether the operation would modify the file
- `original` (string): the matched form text before changes
- `preview` (string): full file preview with changes applied
- `preview_form` (string): just the edited form after changes, or `"(form removed)"` for `delete`. This is what the human-readable summary shows; `preview` holds the whole file and is not inlined into the summary, so a dry-run against a large file no longer returns the file twice.
- `parinfer_warning` (string, optional): auto-repair warning when closing delimiters are added
- `content`: human-readable summary

## `lisp-patch-form`
Scoped text replacement within a matched top-level Lisp form. Finds `old_text` (exact,
whitespace-sensitive match) within the form and replaces it with `new_text`. Most
token-efficient way to make small changes to large forms.

Does NOT auto-repair parentheses — if the patch breaks form structure, it fails
immediately and no changes are written to disk. Use `lisp-edit-form` instead when
replacing or inserting entire forms.

When the patched form no longer reads, the error explains the breakage: if
`new_text` opens and closes a different net number of `)` than `old_text`
(parentheses inside strings and comments do not count), the message says how
many `)` to add to or remove from `new_text`; otherwise it carries the same
line-level diagnosis as `lisp-check-parens`, with line numbers counted within
the patched form, plus the reader's own error when a `]`/`}` may be a symbol
character. Under a `readtable` (argument or `in-readtable` in the file) that
changes the syntax, only the reader's error is reported. If the file itself
does not parse, the error names the line to fix and the recovery path
(`lisp-check-parens` → `fs-read-file` → `fs-write-file` with
`allow_unparseable_overwrite`).

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
- `bracket_warning` (string, optional): set when the patched form reads but its
  delimiter scan stops at a `]` or `}` where `)` was expected (in standard
  syntax the bracket is part of a symbol, so a `)` typo survives silently); the
  patch is applied as asked, and the summary repeats the warning. Also present
  in dry-run output.
- `content`: human-readable summary string of the applied change

Dry-run output (when `dry_run` is true):
- `would_change` (boolean): whether the operation would modify the file
- `operation`: always `"patch"`
- `original` (string): the matched form text before changes
- `preview` (string): modified form text after replacement
- `content`: human-readable summary with original and preview form text

## `code-find`
Return definition location (path, line) for a symbol using SBCL `sb-introspect`.

Input:
- `symbol` (string, required): prefer package-qualified, e.g., `"cl-mcp:version"`
- `package` (string, optional): used when `symbol` is unqualified; must exist

Output:
- `path` (relative when inside project, absolute otherwise)
- `line` (integer or null if unknown)

## `code-describe`
Return symbol metadata (name, type, arglist, documentation).

Input:
- `symbol` (string, required)
- `package` (string, optional): must exist when `symbol` is unqualified

Output:
- `type` ("function" | "macro" | "variable" | "unbound")
- `arglist` (string)
- `documentation` (string|null)

## `code-find-references`
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

## `clhs-lookup`
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

## `run-tests`
Run tests for a system and return structured results with pass/fail counts and failure details.

Input:
- `system` (string, required): ASDF system name to test (e.g., `"my-project/tests"`)
- `framework` (string, optional): Force a specific framework (`"rove"`, `"fiveam"`, or `"auto"` for auto-detect). Auto-detection reads the test system's own `:depends-on`: a framework the system declares directly wins, then one reached transitively, and only for a system ASDF has not registered does it fall back to guessing from the loaded packages. Detection never loads anything.
- `test` (string, optional): Run only a specific test by fully qualified name (e.g., `"my-package::my-test-name"`)
- `tests` (array of strings, optional): Run only the listed fully qualified tests

Output:
- `passed` (integer): Number of passed tests
- `failed` (integer): Number of failed tests
- `pending` (integer): Number of pending/skipped tests (when reported by the framework)
- `framework` (string): Framework or outcome category used (`"rove"`, `"fiveam"`, `"asdf"`, `"load-error"`, `"unresolved"`, or `"timeout"`)
- `duration_ms` (integer): Execution time in milliseconds

The summary line in `content[].text` is `✓ PASS`, `✗ FAIL`, `✗ LOAD FAILED`, `✗ UNRESOLVED`, `✗ TIMEOUT`, or `⚠ NO TESTS RAN`. The last means the run completed but executed nothing — a system with no tests, or a selection that matched none. It is not a failure, but it is not a pass either.

- `failed_tests` (array, when failures exist): Detailed failure information including:
  - `test_name`: Name of the failing test
  - `description`: Rove — the assertion's description; FiveAM — the test's docstring
  - `form`: The failing assertion form
  - `values`: Evaluated values (Rove only)
  - `reason`: Error message (string). For FiveAM this is the framework's own
    message with its blank lines removed, so one mismatch stays one short block
  - `source`: Source location, file and line (**Rove only** — FiveAM records no
    source location for a test, so locate a FiveAM failure by `test_name` and
    `description`)

Example requests:
```json
// Run all tests in a system
{"method":"tools/call","params":{"name":"run-tests","arguments":{"system":"cl-mcp/tests/clhs-test"}}}

// Run a single test
{"method":"tools/call","params":{"name":"run-tests","arguments":{"system":"cl-mcp/tests/clhs-test","test":"cl-mcp/tests/clhs-test::clhs-lookup-symbol-returns-hash-table"}}}

// Force FiveAM and run selected tests
{"method":"tools/call","params":{"name":"run-tests","arguments":{"system":"my-project/tests","framework":"fiveam","tests":["my-project/tests::one-test","my-project/tests::another-test"]}}}
```

Notes:
- **Auto-reloads the test system** before execution (clears ASDF's loaded state and reloads from source). Files edited via `lisp-edit-form` are automatically picked up — no need to call `load-system` first.
- Auto-detects Rove or FiveAM when available; falls back to ASDF `test-system` for text capture
- Single test execution requires the test package to be loaded first
- Test names must be fully qualified with package prefix (e.g., `"package::test-name"`)

## `pool-status`
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

## `pool-kill-worker`
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

## `project-scaffold`
Generate a minimal Common Lisp project skeleton under the project root. The
generated project uses `package-inferred-system` and ships with
`CLAUDE.md` / `AGENTS.md` templates referencing cl-mcp's existing prompts via
relative `@`-include paths. Its tests are written with Rove by default, or with
FiveAM when `framework` says so. On success, returns the list of created files and
a `next_steps` array with concrete REPL commands the agent can invoke to
register the project with ASDF and run its tests.

Input:
- `name` (string, required): project name in lisp-case. Must match `^[a-z][a-z0-9-]*$` and be 1–64 chars.
- `description` (string, optional): one-line description for `.asd` and `README.md`. No newlines. Defaults to a generic placeholder.
- `author` (string, optional): `.asd` `:author`. No newlines. Defaults to `"Unknown"`.
- `license` (string, optional): `.asd` `:license`. No newlines. Defaults to `"MIT"`.
- `destination` (string, optional): parent directory under project root where `<name>/` is created. No absolute paths, no `..` traversal. Defaults to `"scaffolds"`.
- `framework` (string, optional): test framework the generated tests are written with — `"rove"` (default) or `"fiveam"`, matched case-insensitively. Any other name is rejected rather than silently falling back, so a typo cannot produce a Rove project labelled as something else.
- `overwrite` (boolean, optional): replace an existing scaffold directory. Defaults to `false`. Only directories cl-mcp itself generated may be replaced — see Behavior below.

`description`, `author` and `license` are interpolated into Lisp string literals in the generated `.asd`, so double quotes and backslashes are rejected along with newlines.

Output fields (on success):
- `created` (boolean): always `true` on success
- `path` (string): directory path relative to project root (e.g. `"scaffolds/foo-lib/"`)
- `absolute_path` (string): fully qualified path
- `files` (array of strings): relative file paths written, in manifest order
- `framework` (string): the resolved test framework, `"rove"` or `"fiveam"`
- `next_steps` (array of strings): human-readable REPL commands to register the system with ASDF, load it, run its tests, and edit it via `lisp-edit-form`

The generated directory also contains a `.cl-mcp-scaffold` marker file recording the generator, the project name and the manifest. It is what `overwrite` checks for.

Output on failure:
- `created` (boolean): `false`
- `error` (string): diagnostic message explaining which field was rejected

Behavior:
- Runs inline in the parent process alongside other `fs-*` tools.
- Atomically writes to a `.tmp-project-scaffold-<uuid>/` directory under the
  destination, then renames on success. Failed generations leave no artifact.
- `framework` drives the generated `.asd` `:depends-on` entry, its `test-op`
  hook and `tests/main-test.lisp`. A FiveAM project gets a root suite named
  after the primary system — `(def-suite :<name>)` — which is exactly the
  spelling `run-tests`' FiveAM suite matcher looks for, so `run-tests` finds
  and runs it with no further configuration; nest further suites under it with
  `:in :<name>`. `run-tests` detects the framework from the generated
  `:depends-on`, so no `framework` argument is needed there either.
- Fails if the target directory already exists and `overwrite` is false.
- With `overwrite: true`, replaces the target only if it carries the
  `.cl-mcp-scaffold` marker; any other directory is refused untouched, so the
  tool can never delete work it did not generate. The old tree is renamed aside
  and removed only after the new one is committed. Scaffolds generated before
  v2.3.0 have no marker and must be deleted manually.
- Does NOT load or register the generated `.asd`, and does not switch project
  root. Loading generated code in the parent process would escape worker
  isolation; `load-system` registers the system in the worker instead. The
  caller runs the returned `next_steps`.
- Generated scaffolds live under `<project-root>/scaffolds/<name>/` and are
  accessible to every cl-mcp tool (read, edit, grep, eval) without changing
  `project-root`.

Example request:
```json
{"jsonrpc":"2.0","id":5,"method":"tools/call",
 "params":{"name":"project-scaffold",
           "arguments":{"name":"demo-lib",
                        "description":"A scratch project for testing cl-mcp tools",
                        "author":"Satoshi Imai",
                        "license":"MIT"}}}
```

Response (excerpt):
```json
{"result":{"created":true,
           "path":"scaffolds/demo-lib/",
           "files":["demo-lib.asd","CLAUDE.md","AGENTS.md","README.md",
                    ".gitignore","src/main.lisp","tests/main-test.lisp"],
           "framework":"rove",
           "next_steps":["To register with ASDF: run repl-eval with (asdf:load-asd \"...\")",
                         "To load: run load-system with {\"system\": \"demo-lib\"}",
                         "To test: run run-tests with {\"system\": \"demo-lib/tests\"}",
                         "To edit: use lisp-edit-form with paths under scaffolds/demo-lib/"]}}
```
