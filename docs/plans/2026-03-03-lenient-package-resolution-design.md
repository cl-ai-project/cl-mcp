# Lenient Package Resolution for lisp-edit-form and lisp-read-file

**Date**: 2026-03-03
**Status**: Approved
**Branch**: feat/worker-pool-isolation

## Problem

After multi-process isolation (PR #67), `lisp-edit-form` and `lisp-read-file`
run in the **parent process**, which does not have user project packages loaded
(those exist only in worker processes). The Eclector CST parser and
`cl:read-from-string` both fail when encountering package-qualified symbols
like `my-pkg:my-function`, local nicknames (`ad:derivative`), or `defmethod`
specializers with user-defined classes.

### Affected Tools

| Tool | Component | Failure Mode |
|------|-----------|-------------|
| `lisp-edit-form` | `parse-top-level-forms` (file parse) | `reader-error` on unknown package |
| `lisp-edit-form` | `%validate-and-repair-content` (content validation) | `reader-error` on unknown package |
| `lisp-read-file` | `%format-lisp-file` → `parse-top-level-forms` | Same as above (collapsed mode) |

### Unaffected Tools

- `lisp-check-parens` — character-level paren counter, no reader
- `clgrep-search` — text-based `scan-toplevel-forms`, no Eclector

## Approach: handler-bind + Eclector Restart Protocol

Use Eclector's documented error recovery restarts to create ephemeral stub
packages on demand during read. Clean up stubs after read completes.

### Why This Approach

- Eclector explicitly provides `use-package`, `intern`, and `use-anyway`
  restarts for this purpose (code/reader/tokens.lisp L533-601)
- Minimal change to existing logic: CST parse results remain valid
- `%find-target` form matching continues to work unchanged
- No dependency on Eclector internals beyond the public restart protocol

### Alternatives Considered

**Custom Eclector client (`interpret-symbol` override)**: Rejected because
`package-does-not-exist` is signaled *inside* `interpret-symbol` (L603-614),
so overriding the method still requires handler-bind style recovery.

**Text-based fallback (clgrep's `scan-toplevel-forms`)**: Rejected because
it cannot produce `cst-node-value` (parsed form objects), breaking
`%find-target` and `%definition-candidates`. Would require rewriting form
matching to regex-based, losing comment preservation.

## Design

### New File: `src/utils/lenient-read.lisp`

Exports `call-with-lenient-packages`.

Handles three condition types:

| Condition | Restart | Action |
|-----------|---------|--------|
| `eclector.reader:package-does-not-exist` | `use-package` | Create stub package, pass object to restart |
| `eclector.reader:symbol-does-not-exist` | `intern` | Force-intern symbol in stub |
| `sb-int:simple-reader-package-error` | `retry` | Create stub, retry read (SBCL-specific) |

Restart lookup uses `%find-restart-by-name` (string comparison on
`symbol-name`) to avoid package-qualification mismatches.

Cleanup in `unwind-protect`: `do-symbols` + `unintern` then `delete-package`
for each stub.

### Changes to `src/cst.lisp`

`parse-top-level-forms`: Wrap the Eclector read loop (non-readtable path)
with `call-with-lenient-packages`. The CL reader fallback path
(`%read-remaining-with-cl-reader`, triggered by `in-readtable` detection)
executes inside the same wrapper, so it is automatically protected.

This change also fixes `lisp-read-file` (collapsed mode) with no additional
changes, since it calls `parse-top-level-forms`.

### Changes to `src/lisp-edit-form.lisp`

**`%validate-and-repair-content`**: Wrap with `call-with-lenient-packages`
to handle `sb-int:simple-reader-package-error` from `cl:read-from-string`.

**`%normalize-string`**: Change from `princ-to-string` to `symbol-name`
for symbol arguments:

```lisp
;; Before:
(defun %normalize-string (thing)
  (string-downcase (princ-to-string thing)))

;; After:
(defun %normalize-string (thing)
  (string-downcase
   (if (symbolp thing)
       (symbol-name thing)
       (princ-to-string thing))))
```

This ensures form name matching works regardless of package qualification.
Without this fix, stub package symbols print as `"STUB-PKG::MY-FUNCTION"`
via `princ-to-string`, which does not match `"my-function"`.

### Files Changed

| File | Change |
|------|--------|
| **New** `src/utils/lenient-read.lisp` | Core mechanism |
| `src/cst.lisp` | Wrap parse loop |
| `src/lisp-edit-form.lisp` | Wrap content validation + fix `%normalize-string` |

`lisp-read-file.lisp` requires no changes.
`cl-mcp.asd` requires no changes (package-inferred-system auto-resolves).

## Test Plan

### New: `tests/lenient-read-test.lisp`

1. Single unknown package: parse `(defun foo () (unknown-pkg:bar))` succeeds
2. Multiple unknown packages: `(defun foo () (pkg-a:x pkg-b:y))` succeeds
3. Double-colon (internal symbol): `pkg::bar` works (internp=t path)
4. defmethod specializer: `(defmethod foo ((x unknown-pkg:my-class)) x)` parses
5. Stub cleanup: `find-package "UNKNOWN-PKG"` returns NIL after parse
6. Known packages unaffected: `cl:car` uses real CL package
7. `%validate-and-repair-content` with package-qualified content
8. `%normalize-string` returns `symbol-name` for stub symbols
9. E2E: `lisp-edit-form` replace on file with package-qualified symbols

## Edge Cases

- **Package already exists**: `find-package` succeeds, no stub created
- **Nested package references**: handler-bind fires repeatedly, accumulates stubs
- **`#.(...)` read-time eval**: `*read-eval*` is nil, separate reader-error (existing behavior)
- **Concurrency**: Parent process tool execution is single-threaded, no race
- **Restart not found**: Handler does nothing, condition propagates to existing error handler
