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
- `max_output_length` (optional integer, non-negative): keep at most this many characters of
  `content`/`stdout`/`stderr`. `stdout` and `stderr` are bounded as the form
  writes them, so output past the limit is never held in memory, and a short
  note saying how many characters there were in total is appended past that
  limit. `0` suppresses the output. `content` is truncated after the fact and
  marked `...(truncated)`.
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
- `hint` (string, only when the object is the class its symbol names or a named generic function): points at `clos-describe`, which describes the class or generic function itself; `inspect-object` shows its internal representation. The text shows it as `Hint:`

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
  removed_columns, before_comment, truncated, crlf}` inferred by parinfer from
  indentation (at most 10 entries; the rest are counted in
  `likely_fixes_omitted`). `column` is the 1-based column of the first change
  (where an insertion goes), `removed_columns` the columns of the `)` a
  removal drops, `before_comment` says the change goes before a trailing `;`
  comment, `truncated` says `original`/`repaired` were cut to 120
  characters and are then descriptive, not text to write back, and `crlf`
  says the line ends in CRLF (`original`/`repaired` are shown without the
  carriage return, so `repaired` is not the line to write back either). Empty
  when no repair could be inferred.
- for `unclosed`: `next_top_level_line`, the line of the first column-0 `(`
  seen while the form was still open, when there is one and no likely fix
  lands on or after it (the field is set exactly when the summary prints the
  next top-level form hint).
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
  (the shape of a body that was meant to stay inside it), or whose next code
  line sits in column 1 below an indented fix line (a body that lost its
  indentation), a NOTE says the lines below have left that form; when the
  fix rests on an unclosed `[`/`{`, a reminder says the `)` fixes are wrong if
  that bracket was meant as `(`. The next top-level form hint names its
  evidence (a `(` in column 1 while a form is still open) and is dropped when
  a likely fix lands on or after that line, since the two would contradict.
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
  guard would refuse. A delimiter-broken file outside the project root gets
  none of the three: neither `fs-write-file` nor the structural tools can act
  on it, and the summary says to fix it outside cl-mcp.

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
- `guard` (object, optional): an edit_guard object (design doc
  `2026-09-16-clos-describe-fail-closed`, section 4.1) pinning the edit to the exact file and
  form an earlier read observed — `clos-describe`'s own `edit_guard` field, on a `matched`
  entry, is one. See "Edit guard" below.

Matching a `defmethod`: package prefixes (`pkg:`, `pkg::`) and line breaks in `form_name` are
ignored, so `"sb-gray:stream-write-char ((s my-pkg::sink)\n    character)"` matches
`(defmethod stream-write-char ((s sink) character) ...)`. A `form_name` equal to one method's whole
signature picks that method over others it only abbreviates: `"area ((s circle))"` picks the
primary method, not `area :around ((s circle))`. Same-name methods whose specializers differ only
in package (`((w a:widget))` and `((w b:widget))`) match the same `form_name`; the
`Multiple matches` error lists them, and a `[N]` suffix (`"render ((w widget))[1]"`, 0-based)
picks one. Other form types compare `form_name` as written, so a string name such as
`"/users/:id"` keeps its colon and spaces.

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

**Edit guard.** Without `guard`, `lisp-edit-form` matches `form_type`/`form_name` against
whatever is on disk right now — there is no guarantee that form is the same one an earlier
read (a `clos-describe` call, say) observed; something else may have replaced, moved or
deleted it since. Passing `guard` closes that gap: `replace`, `insert_before`, `insert_after`,
`delete` and `dry_run` all run the same six checks, in order, **before writing anything**:

1. `guard.version` is `1` (the only version this tool understands).
2. `guard.abs_path` names the same file `file_path` resolves to.
3. `guard.file_digest` matches an MD5 digest of the file's current bytes.
4. `guard.form_start`/`guard.form_end` (0-based characters, end exclusive) are a valid range
   in that same reading of the file.
5. The form `form_type`/`form_name` matches today has exactly that range — not a different
   definition that happens to share the name.
6. When present, `guard.form_digest` matches an MD5 digest of that range's text.

The file is read once for these checks, and the very same bytes are what gets edited — never a
second, possibly different, read. The first check that fails stops everything: **nothing is
written**, not even under `dry_run`, and the file is byte-for-byte unchanged. The failure is a
tool error whose JSON also carries `conflict`: `{"reason": "<one sentence>", "expected": ...,
"actual": ...}`. There is no fallback to a plain name match and no adopting the new digest to
continue — re-run whatever produced `guard` (a fresh `clos-describe`, for instance) and retry
with the new value. Calling without `guard` is unaffected and keeps working as before; it is
just not protected against this class of surprise.

Checks 1-4 run before the file is parsed; checks 5-6 run once `form_type`/`form_name` has
matched a form. That split is what a changed file gets: once `guard.file_digest` no longer
matches, the call reports the `conflict` even when the lookup could not have finished anyway —
the observed form renamed or deleted, its name now matching several forms, or the file no longer
parsing at all. The reason names the `file_digest` mismatch, which is the change to look at
first, rather than the `not found`, `Multiple matches` or unparseable-file error the lookup would
otherwise have raised. When the file is *unchanged*, none of those is a guard problem: a
`form_type`/`form_name` that names nothing still gets the ordinary `Form <type> <name> not found
in <path>` error, and an ambiguous one still gets `Multiple matches ... Specify an index:`, so a
guard never turns a caller's own mistake into a `conflict`. A path the read policy refuses, a
file over the read limit and a file that is not valid UTF-8 likewise stay plain errors.

What this does *not* protect against: `guard` is a precondition, not an access token or a
lock — the existing path validation and write limits still apply unchanged (a guarded call
reaches no file, and writes no file, that an unguarded call could not). Reading for a guarded
call is capped the same way an ordinary `lisp-edit-form` read is: it reads the whole file in one
pass, so the digest it checks and the text it edits always agree, and refuses — never
truncates — a file at or over the same read limit an unguarded call would refuse too; a guard
never lets this tool read more than an unguarded call could. A file that is not valid UTF-8 is
refused the same way, with a plain error rather than a `conflict`: its undecodable bytes would
be replaced by `?` on the way back to disk, and an unguarded call refuses it too (its decoder
signals). This is not compare-and-swap.
Between the check above and the write, no *other cl-mcp call* can slip in: `lisp-edit-form`,
`lisp-patch-form` and `fs-write-file` take one per-file lock, and `lisp-edit-form` holds it
from before it reads the file until after it writes, so read → check → build → write is one
critical section. A second concurrent edit of the same file therefore runs after this one
finishes, reads what it wrote, and — with the same `guard` — gets a `conflict` instead of
silently overwriting it. A writer *outside* cl-mcp (an external editor, another process) is
still not coordinated, and that window is not closed. What *is* caught: any change after
`guard` was built, reusing the same `guard` for a second edit after the first one already
succeeded, and a change anywhere else in the file (an edited `in-package`, say) even when the
target form's own text is untouched.

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

`form_name` matches a `defmethod` as in `lisp-edit-form`: package prefixes and line breaks in it
are ignored, the method whose whole signature equals `form_name` is preferred over one it
abbreviates, and same-name methods from different packages need a `[N]` suffix.

No `guard`: `lisp-patch-form` takes no guard argument, so it always patches whatever
`form_type`/`form_name` match on disk right now, with no check that it is the form an earlier
read observed. An edit built from a `clos-describe` result should therefore go through
`lisp-edit-form` with that entry's `edit_guard` passed as `guard` — that is the only path where
a file or form changed since the observation stops the write.

Concurrent cl-mcp calls: like `lisp-edit-form`, a patch holds one per-file lock from before it
reads the file until after it writes, so two patches to two different forms of one file both
land instead of the second silently dropping the first. This serialises cl-mcp's own writes
only; a writer outside cl-mcp is not coordinated, and — since there is no `guard` here — a
change made between your read and this patch is neither detected nor reported.

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
- `line` (integer or null if unknown): classes, conditions, structures and methods get one too,
  the line of the top-level form defining them

## `code-describe`
Return symbol metadata (name, type, arglist, documentation).

Input:
- `symbol` (string, required)
- `package` (string, optional): must exist when `symbol` is unqualified

Output:
- `type` (`function`, `generic-function`, `macro`, `variable`, `class`, `condition`, `structure`)
- `arglist` (string; for a class, its direct slot names)
- `documentation` (string|null)
- `path`, `line`: where it is defined; a class, condition or structure gets its line too

The text ends with a pointer to `clos-describe` for a generic function (with its method count) or a class.

## `code-find-references`
Find who calls or references a symbol — its callers, the exact call sites inside
them, and the tests involved — to judge what a change would affect. Combines SBCL
`sb-introspect` xref with a scan of the project's source.

Input:
- `symbol` (string, required): `pkg:name`, `pkg::name` or `name`; a single colon also finds internal symbols
- `package` (string, optional): package used when `symbol` is unqualified
- `project_only` (boolean, default `true`): limit xref results to files under the project root
- `limit` (integer, default `50`): most forms listed; `count` always gives the total

Output (the content text carries everything that matters for a decision):
- `symbol_status`: `found`, `not_found` or `package_not_found`; nothing is interned either way
- `resolved_symbol`, `symbol_kind` (`function`, `macro`, `generic-function`, `special-operator`, `variable`, `constant`, `unbound`), `lookup_package`, `lookup_name`
- `refs` (array): one element per top-level form, sorted by path and line
  - `path`, `line` (start of the form), `type` (first of `types`), `types`
  - `caller`, `caller_symbol` (package-qualified; null for lambdas and for forms xref did not see)
  - `form_type`, `form_name`: pass them straight to `lisp-edit-form` (`form_type` / `form_name`); for `lisp-read-file`'s `name_pattern`, a CL-PPCRE regex, regex-quote the name first (a `defmethod` name such as `area ((s integer))` does not match itself)
  - `origin`: `xref+source`; `xref` (the call exists only in a macro expansion, or the source was not scanned); `source` (a top-level use xref never records, or code not compiled since it was written; no note when every site is `quoted`, `template` or `method`, which xref usually does not record -- `WHO-CALLS` does record a function passed by name such as `(mapcar 'name xs)`, but then the form is not source-only)
  - `call_sites` (array): `line`, `column`, `kind` (`call`, `macro`, `function`, `quoted`, `template`, `bind`, `set`, `method`, `reference`), `context`, `shadowed_by`; `function` is `#'name` or a quoted `'name` passed as the function to `funcall`, `apply` or `multiple-value-call`; `set` is a `setf`/`setq` place or the variable `incf`, `decf`, `pop`, `push` or `pushnew` changes
  - `test`: `{name, framework}` when the form is a `deftest` (rove), `test`/`def-test` (fiveam) or `define-test` (parachute)
  - `stale`: the file changed after it was compiled; `note`: why a form lacks call sites or xref, or, on an `xref+source` form, which xref types (`call`, `set`, ...) no listed site shows -- say a call made by a macro expansion or through a function passed by name such as `(mapcar 'name xs)`; the sites are still listed with their own kind, and the form is not split
- `count`, `file_count`, `limit`, `truncated`
- `tests` (array): `name`, `path`, `line` of every test among the references
- `unresolved` (array): `path`, `package`, `count`, `tests` for matches in files whose package is not loaded
- `notes` (array), `xref_count`, `files_scanned`, `name_matches`, `scan_skipped`, `project_only`, `symbol`

Limits: matching is positional, not a code walker. A `flet`/`labels`/`macrolet` binding the same
name is flagged in `shadowed_by`; other lexical bindings are not. The name position of any
`def...` form is treated as a definition. Sites after an `in-readtable` switch are not found.
Only files `fs-read-file` may read are scanned (under the project root or a registered ASDF
system's source directory, symlinks resolved): a file the root reaches through a symlink leading
elsewhere is not read, and a note counts such files without naming them.

## `clos-describe`
Describe a CLOS class or generic function from the running image — the structure a source
search cannot see: a generic function's methods with their qualifiers, specializers and
source lines, and a class's superclasses, subclasses, precedence list, direct and effective
slots, default initargs and specialized methods.

Input:
- `symbol` (string, required): `pkg:name`, `pkg::name` or `name`; a single colon also finds internal symbols
- `package` (string, optional): package used when `symbol` is unqualified
- `limit` (integer, default `50`): most methods listed per generic function and per class; `method_count` always gives the total

Output (the content text carries everything that matters; names in it drop the symbol's own package and `COMMON-LISP:`):
- `symbol_status`: `found`, `not_found` or `package_not_found`; nothing is interned either way
- `resolved_symbol`, `symbol_kind`, `lookup_package`, `lookup_name`, `limit`, `notes`
- `generic_functions` (array, up to 2): the function `symbol` names and its `(setf symbol)` function, when generic
  - `name`, `lambda_list`, `documentation`, `method_combination` (`STANDARD`, `+ :MOST-SPECIFIC-FIRST`, ...)
  - `path`, `line`, `stale`, `identity`, `source_match`, `source_match_reason`, `form_type`, `form_name`, `edit_guard`, `note`: the `defgeneric`; `path` is null when no `defgeneric` created the generic function (a `defmethod` or a slot accessor did)
  - `method_count`, `truncated`, `methods`
- `class` (object or null):
  - `name`, `metaclass`, `documentation`, `finalized`, `path`, `line`, `stale`, `identity`, `source_match`, `source_match_reason`, `form_type`, `form_name`, `edit_guard`, `note`
  - `direct_superclasses`, `direct_subclasses`, `precedence_list` (null when a superclass is undefined), `undefined_superclasses`
  - `direct_slots`, `effective_slots` (null without a precedence list): `name`, `from` (effective slots: the most specific class defining it), `initargs`, `initform` (the code, never evaluated; null when there is none), `type`, `allocation` (`instance`, `class`), `readers`, `writers`, `documentation`
  - `default_initargs`: `initarg`, `form`, `from`
  - `method_count`, `truncated`, `methods`, `omitted_classes`: the methods specialized on the class and its superclasses, except superclasses in `COMMON-LISP` or an `SB-` package (the standard protocol), which `omitted_classes` names
- Method objects: `generic_function`, `qualifiers`, `specializers` (`PKG::CLASS`, `COMMON-LISP:T`, `(EQL :KEY)`), `kind` (`method`, `reader`, `writer`), `slot` (accessors), `via` (class methods: the class specialized), `path`, `line`, `stale`, `identity`, `source_match`, `source_match_reason`, `form_type`, `form_name`, `edit_unit`, `edit_guard`, `note`

**Observation vs. edit information.** Every definition's `path`/`line`/`stale`/`identity` come
straight from the running image — SBCL's own record of where each generic function, class or
method was compiled from, plus a structured `identity` (its name, qualifiers, specializers,
and — for accessors — class/slot/access, all as package+name pairs, never a display string).
That is *observation*: what the image believes about itself, always present when the image
records a source location at all, regardless of whether the source file still agrees.

`form_type` / `form_name` are *edit information*: they are handed out only once the source file
has been independently re-read and its form at that location confirmed to describe the very
same definition the image reported — and, further, only once `lisp-edit-form`'s own locator
resolves that `form_type`/`form_name` back to that exact form. Confirming the same definition's
*identity* is not the same as confirming the loaded code matches the source text byte for byte;
this tool does not attempt the latter. `source_match` names which of three states this
confirmation reached, and `source_match_reason` is an English sentence for the two states that
are not "matched" (null when it is):

- `matched`: the source form at the recorded location describes the same definition, and
  `lisp-edit-form` resolves `form_type`/`form_name` to that same form — the only state that
  carries `form_type`/`form_name` (and, for a container, `edit_unit`; see below) and an
  `edit_guard` (see "Edit guard" below). Every other state sends `form_type` and `form_name` as
  `null`, and omits `edit_unit` and `edit_guard` entirely, rather than a stale or unverifiable
  pair.
- `mismatched`: the source form there is a different definition (same generic function name but
  different specializers, a class of the same name but different superclass, and so on) — for
  example, a method whose `(eql :old)` specializer was edited to `(eql :new)` and reloaded:
  the old method survives in the image (redefinition never removes a differently-specialized
  method), so `clos-describe` reports it too, but that entry gets no edit information.
- `unverified`: not enough could be confirmed either way — an unsupported or unparseable form,
  a name or package that does not resolve in this image, an ambiguous match (more than one
  candidate at the line, or more than one of a `defgeneric`'s inline methods matching), a file
  that could not be read, or a file whose modification time is newer than what the image
  recorded (`stale`: true) — staleness never lets a would-be `matched` verdict stand, since the
  form the image last saw and the form on disk now may no longer be the same one.

When no form starts on the recorded line at all, `note` says why (the file changed since it was
loaded, or does not parse); that case is `unverified` too, with its own `source_match_reason`.
The content text mirrors this exactly: a `matched` entry's line ends with
`(form_type form_name)`, everything else ends with `[state: reason]` — the text never suggests
an edit the JSON does not back up.

**Specializer matching**, including `(eql ...)`, compares the *identity* the image reports
against the *unevaluated source text* at that location — never against a printed
representation, and never by evaluating the source form again. Supported `(eql ...)` values:
a keyword, an integer (including a bignum, carried as decimal text so it never becomes a
JSON float), a ratio, a character (case-sensitive), `t` and `nil` (tagged as a two-letter
string, `"T"` or `"NIL"`, so `nil`-the-value is never confused with a missing field), and a
symbol quoted with `'` or a confirmed `(quote ...)`. A variable reference, a function call, a
string, a list or array, an uninterned symbol, `#.`, a float or a complex number, or anything
whose printed form was truncated, is `unverified` — there is no way to confirm it without
evaluating source, which this tool never does.

**Accessor matching.** A slot accessor is confirmed against the slot option that actually
defines it, `(setf name)` included. `:reader x` and `:writer x` each define the plain function
`x`; `:writer (setf x)` defines `(setf x)`; `:accessor x` defines both, a plain `x` reader and a
`(setf x)` writer. So `:accessor x` and `:writer x` are not interchangeable: a live `(setf x)`
writer whose slot option now reads `:writer x` is `mismatched`, not `matched`, and so is a live
plain `x` writer whose option now reads `:accessor x`. A `:reader` or `:accessor` written with
anything but a bare symbol is not valid Common Lisp; rather than guess what it meant, that option
confirms nothing, so an accessor that depends on it is `unverified` rather than `matched`.

**Container edit units.** A method identified as a `defgeneric`'s inline `(:method ...)` option,
or a class's slot accessor (`:reader`/`:writer`/`:accessor`), is not itself a top-level form —
editing it means editing the `defgeneric` or the `defclass`/`define-condition` that contains it.
When that applies, a `matched` method carries `edit_unit` (`"defgeneric"`, `"defclass"` or
`"define-condition"`) alongside a `form_type`/`form_name` that names the *container*, not the
method by itself; `lisp-edit-form` on that form_type/form_name replaces the whole container, so
edit it with that in mind rather than expecting a single method's text back.

**Edit guard.** A `matched` entry's `edit_guard` is the same object `lisp-edit-form`'s `guard`
argument accepts (see that tool's "Edit guard" section for the six checks it runs): `version`,
`path` (display only; verification runs on `abs_path`), `abs_path`, `file_digest`, `form_start`,
`form_end` and `form_digest` — all computed from the exact same read and CST span that produced
this entry's `form_type`/`form_name`, never a second, possibly different, read. Pass it straight
through as `guard` on the `lisp-edit-form` call `form_type`/`form_name` heads toward; recommended
for every edit built from a `clos-describe` result, not just when a race seems likely. Doing so
catches a change to the target form, or anywhere else in the file, made after this
`clos-describe` call returned, and catches reusing the same `edit_guard` for a second edit after
the first one already consumed it. Another cl-mcp call cannot race between `lisp-edit-form`'s own
check and its write — cl-mcp serialises its own writes to one file, so the second call runs after
the first and sees the changed file — but a writer outside cl-mcp is not coordinated, and
`edit_guard` remains a precondition, not a lock or an access token. On a conflict, call
`clos-describe` again for a fresh `edit_guard` rather than retrying without one or falling back
to a plain `form_type`/`form_name` call against possibly-changed source. `edit_guard` is present
exactly when `form_type`/`form_name` carry values: an entry with no edit information (both of
them `null`) never carries one.

Order: a generic function's methods run `:around`, `:before`, primary, `:after` for the standard
method combination, project files before other files; a class's methods follow its precedence
list, then the generic function's name.

Reads only: a class is never finalized — an unfinalized class's precedence list is computed and
its effective slots merged from the direct slots the standard way, with a note — no initform is
evaluated, and nothing is interned.

Cost: confirming a definition means reading and parsing the file it was compiled from, so one
call reads every distinct file its answer names — once each, whole — and its cost scales with
that number of files, not with the number of definitions. A file the CST reader cannot read
(`#.` with `*read-eval*` off, a custom reader macro) yields no candidates at all, so every entry
in it comes back `unverified` with no edit information, even when the definitions themselves are
untouched.

Limits: structure accessors are not MOP readers, so a `defstruct` slot lists none, and every
structure slot shows an initform (`NIL` when none was written). A metaclass that customizes
`compute-slots` may finalize with other effective slots than an unfinalized class shows. Lines come
from SBCL's record of each file's top-level forms, or from reading the file once that record has
been garbage collected with the file's code; a file using a custom reader macro whose record is
gone gets no line, and without the record `stale` is not known.

Use `inspect-object` for one instance's slot values, `code-describe` for a plain function, macro or
variable, and `code-find-references` for who calls a generic function.

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

## Optional tool groups

A tool may belong to an optional group. Grouped tools are registered like any
other but stay out of `tools/list` and refuse calls until the group is switched
on, so a tool that only makes sense alongside another system does not cost
every other user a line in the tool list and a description in the model's
context.

Enable a group in the server's environment, which is how an MCP client
normally launches cl-mcp:

```json
{ "mcpServers": { "cl-mcp": {
    "command": "ros", "args": ["run", "--", "..."],
    "env": { "MCP_ENABLE_TOOL_GROUPS": "cl-spec" } } } }
```

Several groups are comma or space separated (`"cl-spec, other"`). For embedded
use, every server entry point takes the same setting, with the same
supplied-p semantics `worker-pool` has — omitting it leaves whatever the
environment set:

```lisp
(cl-mcp:run                      :transport :stdio :tool-groups (list :cl-spec))
(cl-mcp:start-http-server        :port 3000        :tool-groups (list :cl-spec))
(cl-mcp:serve-tcp                :port 4005        :tool-groups (list :cl-spec))
(cl-mcp:start-tcp-server-thread  :port 4005        :tool-groups (list :cl-spec))
(cl-mcp:ensure-tcp-server-thread :port 4005        :tool-groups (list :cl-spec))
```

Calling a tool whose group is off returns a JSON-RPC error naming the group and
how to enable it, rather than "tool not found" — the tool is real, the setting
is what is missing.

### Group `cl-spec` — `spec-list`, `spec-symbol`, `spec-describe`, `spec-check`

Fetch the [cl-spec](https://github.com/cl-ai-project/cl-spec) Spec, Property or
function spec registered about a symbol, read one in full, and run it for a
structured counterexample. cl-mcp does not depend on cl-spec: these tools
resolve it at call time and report `cl-spec-not-loaded` when it is absent, and
`unsupported` when the loaded revision lacks the API an operation needs.

With cl-spec's versioned Lisp API, `spec-describe` and completed per-result
`spec-check` records include `core_schema`. Its integer `schema_version` is
independent of the adapter's existing string `schema_version`. It carries
`record_kind`, `entity_kind`, `definition_digest`, `definition_digest_complete`,
`definition_digest_covers`, and `capabilities` (generation, shrinking,
instrumentation). An absent or unsupported core schema is `null`.
The digest covers declarations and registered spec/generator dependencies,
including whole-argument generators; it excludes target/helper implementations,
captured/external state and backend settings. Check results use metadata captured
by cl-spec before execution. Incomplete or unsupported versioned digests remain
unknown when compared. Only older cl-spec records without a schema version use
the legacy adapter digest. Capability `available` means generator/shrinker
construction is supported; it does not guarantee successful draws or reductions.

- `spec-list` — what is registered at all. The entry point when you do not yet
  know a name: the other three all take one you already have. Returns names,
  and for each property its kind, tags, `(:about ...)` targets and docstring —
  not bodies.
  - `kind` (`specs` | `properties` | `function-specs` | `both`, default
    `both`), `package`, `tag`, `limit` (positive integer, default 200),
    `timeout_seconds`
  - `specs_listable`, `properties_listable`, `function_specs_listable` and
    `tag_filterable` say whether this cl-spec can enumerate each half, and
    whether it can filter by tag at all — facts about the loaded revision, not
    about what this call asked for. `filters.tag_applied` says whether the tag
    actually narrowed this listing. False there is not
    "this project has none" — and the matching entry in `counts` is `null`,
    never `0`. `kind=both` lists the halves it can and reports the rest this
    way rather than failing the whole call.
  - `tag` names a keyword. A tag no loaded code mentions comes back as
    `tag_resolved: "no-such-keyword"` rather than as an empty result — "nothing
    carries this tag" and "this tag does not exist here" are different answers.
  - An empty listing is not evidence that a project has no contracts: it shows
    what is registered in *this worker*.
  - `counts` is `null` for a kind that was not requested, and the text omits
    its line. A `0` there would read as "the registry holds none" when it
    means "this call did not look".
- `spec-symbol` — what is registered about a symbol, joined with this image's
  signature, docstring and source location. Property bodies are summarized,
  not inlined.
  - `symbol` (string, required), `package`, `include_runtime` (boolean,
    default true), `timeout_seconds` (number, default 30)
- `spec-describe` — one definition in full.
  - `kind` (`property` | `spec` | `function-spec`, required), `name` (required),
    `package`, `max_chars` (positive integer, default 8000),
    `timeout_seconds` (number, default 30)
  - `function-spec` projects the contract: the spec of each argument, the spec
    of the return value, and the `:pre` / `:post` forms. It answers
    `unsupported` only when the loaded cl-spec exports no `function-spec-data`
    — a statement about that revision, not about whether a contract exists.
  - `:pre` and `:post` are cut at `max_chars` with the cut reported, like
    `body` and `source_form`.
- `spec-check` — run one property, every property registered `(:about
  <symbol>)`, or one function spec against its function.
  - `property` **or** `symbol` **or** `function` (exactly one), `package`,
    `profile` (default `normal`), `trials` (positive integer, max 1,000,000),
    `seed` (decimal digits **as a string**), `expect_definition_digest`,
    `timeout_seconds` (number, default 60 — the budget for the whole call),
    `max_value_chars` (positive integer, default 2000)
  - `function` runs the contract. A `symbol` selection does **not** include it
    — `:about` covers properties only — and the response names the contract it
    left alone rather than letting the verdict read as full coverage. The
    reverse holds too: `function=` runs the contract and none of the properties
    registered about the symbol. Both directions are reported in three places —
    the headline qualifier, `selection.contract_not_run` /
    `selection.properties_not_run`, and `verification_gaps`
    (`contract-not-run` / `properties-not-run`) — so neither has to be read
    out of prose.
  - `trials` sizes a contract run and `profile` sizes a property run; each is
    **refused**, not ignored, against the other. A contract has no `:trials`
    table for a profile to select from, and `run-property` takes no trial
    override, so honouring either would report a setting the run did not use.
  - A contract run reports `rejected` and `effective_trials`: cl-spec's checker
    refuses inputs `:pre` does not admit, and a trial count that includes them
    overstates the work. Raise `trials` — and `timeout_seconds` with it — when
    `effective_trials` comes back small. `rejection-counts-unmeasured` is left
    off a contract run only when the count is actually usable; a run that timed
    out, never started, or whose reader failed still carries it.
  - `effective_trials` is **null**, never 0, when it could not be derived.
    `rejected_measured` false means no refused count came back — this cl-spec
    exports no reader for it, or the reader signalled; `rejected_overcounted`
    true means cl-spec reported more refusals than trials (its counter keeps
    running when the function signals); `rejected_contradicted` true means
    refusals were reported for a contract with no `:pre`; `has_precondition`
    false means the contract has no `:pre`, so nothing could be refused, and
    `null` means the definition could not be read. `rejected_usable` is the
    single flag answering whether the count may be subtracted with. A 0 in
    `effective_trials` would read as "never called".
  - A contract run whose effective count is unknown is not evidence:
    `verified` is false and `verification_gaps` carries
    `effective-trials-unknown` (plus `rejection-counts-unmeasured` when the
    refusal count is the reason). The raw trial count is never used as a
    fallback — it is the number the refusal count exists to correct, and a
    `:pre` that admits nothing would otherwise read as a hundred trials.
  - `verification_gaps` values: `zero-trials`, `effective-trials-unknown`,
    `rejection-counts-unmeasured`, `input-coverage-unmeasured`,
    `contract-not-run`, `properties-not-run`, `related-properties-unknown`,
    `no-properties-selected`, and any result status that is not a verdict
    (`skipped`, `pending`, `timeout`, `not-run`, the `*-error` statuses). The tool's
    own description defines each one, and `tests/spec-tools-test.lisp` checks
    that description against the code's list so the two cannot drift.
  - `selection.properties_not_run` is `null`, not `[]`, for a selection that
    never looks — `property=` and `symbol=` leave the symbol's other
    registrations unrun without reporting which.
  - `verified` is true only when at least one property was selected, all of
    them passed, and each evaluated at least one trial. Zero properties,
    a timeout, a generator failure and a zero-trial budget are each reported
    as themselves. The full per-property and whole-call status sets are listed
    in the tool's own description rather than duplicated here — that
    description is checked against the code's status list by
    `tests/spec-tools-test.lisp`, and a second copy would only drift.
  - The seed is text because a cl-spec seed exceeds JSON's safe integer range;
    a JSON number is refused rather than silently ignored.

Prerequisite: `load-system` with `cl-spec/check-it` (execution) or `cl-spec`
(introspection only), plus the system defining the specs and properties. All
three tools run in the session's worker, so the definitions a `load-system`
put there are the ones they see.
