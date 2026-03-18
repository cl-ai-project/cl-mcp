# Comprehensive Test Report: cl-mcp (Cycle 1)

Date: 2026-03-18
Scope: PR #83 – form matching fixes and reader error detection (Issues 1-3)
Perspectives: End User, Edge Case Explorer, Error Handling, Integration

---

## Test Configuration

- Perspectives: End User, Edge Case Explorer, Error Handling, Integration
- Focus: Newly implemented features in `src/validate.lisp` and `src/lisp-edit-form-core.lisp`

## Summary

| Severity | Count |
|----------|-------|
| Critical | 0 |
| Major    | 6 (deduplicated) |
| Minor    | 8 (deduplicated) |
| **Total**| **14** |

---

## Critical Issues

None.

---

## Major Issues

### M1. EOF-type reader errors reported with wrong position

**Flagged by**: Edge Case Explorer
**Location**: `src/validate.lisp` — `%try-reader-check`
**Description**: In SBCL, `end-of-file` (and `sb-kernel:reader-eof-error`) are NOT subtypes of `reader-error`. They are subtypes of `stream-error`. When the CL reader encounters an incomplete dispatch character (`#`), an unclosed block comment (`#|`), or an unterminated string literal, it raises `end-of-file` — which falls through the `(reader-error (e) ...)` clause entirely and is caught by the generic `(error (e) ...)` handler. That handler reports `:offset base-offset :line nil :column nil` (the offset of the beginning of the entire input, not where the error occurred). The actual stream position is available via `(file-position stream)` but is not used.

**Reproduction**:
```lisp
;; Confirmed via REPL:
(%try-reader-check "(foo) #" 0)
;; => (:KIND "reader-error" :MESSAGE "..." :OFFSET 0 :LINE NIL :COLUMN NIL)
;; Expected: :OFFSET 6 :LINE 1 :COLUMN 7
```

**Suggestion**: Add a dedicated `(end-of-file (e) ...)` handler clause in `%try-reader-check`, before the generic `(error (e) ...)` clause, that captures `(file-position stream)` for position reporting.

---

### M2. False positives for files with unloaded packages

**Flagged by**: End User, Edge Case Explorer
**Location**: `src/validate.lisp` — `%try-reader-check`
**Description**: The CL reader raises `package-error` when it encounters a package-qualified symbol (`my-pkg:my-sym`) whose package is not loaded. The generic `(error (e) ...)` handler catches this and returns `ok: false, kind: "reader-error"`. A perfectly valid `.lisp` file that references an unloaded package will be incorrectly reported as having a reader error. This is a false positive.

**Reproduction**:
```lisp
;; File uses (cl-mcp/src/fs:read-file ...) — package not loaded in worker
;; => lisp-check-parens returns ok: false, kind: "reader-error"
```

**Suggestion**: Catch `package-error` in a dedicated clause before the generic handler and return `nil` (no reader error), or report it separately with a distinct `kind: "package-error"` that signals a different problem.

---

### M3. False positives for files using custom readtables

**Flagged by**: Integration
**Location**: `src/validate.lisp` — `%try-reader-check`
**Description**: `%try-reader-check` reads the entire file text using the standard CL readtable (no custom readtable is installed). Files that use `named-readtables:in-readtable` with a custom readtable (e.g., `cl-interpol`'s `#?"..."` syntax) will trigger an "unknown dispatch character" reader error in `%try-reader-check`, even though the paren scan (which uses Eclector) passes correctly. This produces `ok: false, kind: "reader-error"` on a valid file.

**Reproduction**:
1. Create file with `cl-interpol` syntax: `(in-readtable :interpol-syntax)(defun greet (x) #?"Hello ${x}")`
2. Call `lisp-check-parens` on the file.
3. Paren scan passes, but `%try-reader-check` returns reader error on `#?`.

**Suggestion**: Before running the reader check, scan the file text for `in-readtable`. If found, skip `%try-reader-check` and return `nil`. This avoids false positives on files using non-standard readtables.

---

### M4. `lisp-check-parens` tool description omits reader error detection

**Flagged by**: End User, Integration
**Location**: `src/validate.lisp` — `define-tool "lisp-check-parens"` description string
**Description**: The MCP tool description (the only text AI clients see when choosing tools) does not mention that `lisp-check-parens` now also detects reader errors and returns `kind: "reader-error"` with a `message` field. The plan document (`docs/plans/2026-03-18-form-matching-and-reader-error-design.md`, line 188) explicitly required this documentation update but it was omitted.

**Suggestion**: Add to the tool description: "Also detects reader errors (e.g. unknown dispatch characters, `#.` when read-eval is disabled) even when parentheses are balanced. In that case the result has `kind: \"reader-error\"` and a `message` field."

---

### M5. Missing file I/O error handler in `lisp-check-parens` define-tool body

**Flagged by**: Error Handling
**Location**: `src/validate.lisp` — `define-tool "lisp-check-parens"` body
**Description**: The `lisp-check-parens` define-tool body calls `(lisp-check-parens :path path ...)` without wrapping it in a `handler-case`. If the specified file does not exist, `fs-read-file` signals an error that propagates uncaught from the tool handler, producing an uncontrolled error response. Compare with `lisp-edit-form`'s define-tool, which wraps its body in `(handler-case ... (error (e) (ht-error "..." e)))`.

**Suggestion**: Wrap the define-tool body in `(handler-case ... (error (e) (ht-error ...)))` to return a clean MCP error response for file-not-found and other I/O errors.

---

### M6. `position` hash always includes `null` line/column for non-reader-errors

**Flagged by**: Error Handling
**Location**: `src/validate.lisp` — define-tool body `make-ht` call
**Description**: When the generic `(error (e) ...)` handler fires (e.g., for package errors, EOF, or custom readtable errors), `%try-reader-check` returns `:line nil :column nil`. The define-tool body always constructs a `position` hash with these nil values, resulting in `"position": {"offset": ..., "line": null, "column": null}`. This inconsistency confuses clients that expect integer values.

**Suggestion**: Omit the `position` key when line/column are nil, or set `offset` to a more meaningful value (e.g., the actual stream position if available).

---

## Minor Issues

### m1. `%strip-name-prefix` with `"#:"` returns empty string — confusing error message
**Location**: `src/lisp-edit-form-core.lisp` — `%strip-name-prefix`
`form_name: "#:"` strips to `""` → "Form defun  not found" (double space). Should validate non-empty after stripping.

### m2. `define-tool "lisp-edit-form"` `form_name` description missing `defstruct` and `#:` info
**Location**: `src/lisp-edit-form.lisp`
The `form_name` description does not mention `defstruct` options syntax or `#:` prefix stripping.

### m3. `define-tool "lisp-patch-form"` `form_name` description missing `#:` prefix info
**Location**: `src/lisp-patch-form.lisp`
Same gap as m2 for `lisp-patch-form`.

### m4. No unit test for `lisp-patch-form` with `#:` prefix `form_name`
**Location**: `tests/tools-test.lisp`
Test for `lisp-edit-form` + `#:` exists; equivalent for `lisp-patch-form` does not.

### m5. `validate-test.lisp` has no unit tests for reader error detection
**Location**: `tests/validate-test.lisp`
Only paren-balance tests; no direct unit test for `%try-reader-check` or `lisp-check-parens` with reader errors.

### m6. `message` key always present in MCP response even when null
**Location**: `src/validate.lisp` — define-tool `make-ht`
For paren errors, `"message": null` appears in the response. For reader errors, `"expected": null, "found": null` appear. Asymmetric response shape.

### m7. `lisp-check-parens` `:code ""` (empty string) behavior undocumented
Edge case: empty string returns `ok: true`, which is correct but undocumented.

### m8. `%definition-candidates` `defstruct` branch code comment slightly misleading
**Location**: `src/lisp-edit-form-core.lisp`
Comment says "only first element is the name" but the condition is `(listp name)` — worth clarifying.

---

## Positive Findings

- `%strip-name-prefix` correctly integrated — both `lisp-edit-form` and `lisp-patch-form` benefit via shared `%find-target` call chain
- `%definition-candidates` defstruct branch correctly placed (after `symbolp` check, before generic fallback) — simple defstruct names still work via `symbolp` branch
- `%try-reader-check` uses `*read-eval* nil` — prevents arbitrary code execution during checking
- Protocol-level tests added for all three new behaviors in `tests/tools-test.lisp`
- `prompts/repl-driven-development.md` updated with `#:` prefix stripping documentation

---

## Recommended Actions (Priority Order)

1. **Fix `%try-reader-check` false positives and wrong positions** (M1+M2+M3+M6 — all in same function)
   - Add `end-of-file` handler with `file-position` for accurate position
   - Catch `package-error` separately (return nil — not a file error)
   - Detect `in-readtable` in file text; skip reader check if found
   - Omit null line/column from position hash
2. **Add file I/O error handler in define-tool body** (M5)
3. **Update `lisp-check-parens` tool description** (M4)
4. **Update `form_name` descriptions in `lisp-edit-form` and `lisp-patch-form`** (m2, m3)
5. **Add missing test coverage** (m4, m5)
