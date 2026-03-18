# Comprehensive Test Report: cl-mcp (Cycle 2)

Date: 2026-03-18
Scope: Post-Cycle-1 fixes — `src/validate.lisp` (M1-M6 all applied), plus
       `src/lisp-edit-form-core.lisp` (defstruct + `#:` prefix fixes from PR #83)
Perspectives: End User, Edge Case Explorer, Error Handling, Integration

---

## Test Configuration

- Perspectives: End User, Edge Case Explorer, Error Handling, Integration
- Focus: Verification that Cycle 1 fixes hold; discovery of remaining issues

---

## Summary

| Severity | Count |
|----------|-------|
| Critical | 0 |
| Major    | 0 (all candidate "Major" issues cross-checked as invalid — Cycle 1 fixes confirmed) |
| Minor    | 7 (deduplicated) |
| **Total**| **7** |

**All 6 Cycle-1 Major issues (M1–M6) confirmed fixed.** The testers flagged several
apparent Major issues but each was invalidated by cross-checking against the actual
post-fix source code. The Cycle 1 fixes are intact and all 11 validate-test and
271 tools-test assertions pass.

---

## Critical Issues

None.

---

## Major Issues

None (post cross-check). Apparent majors flagged by testers and their resolution:

| Tester claim | Why invalidated |
|---|---|
| Tool description omits reader error detection | M4 fixed: description now includes reader-error shape |
| False positives on unloaded packages | M2 fixed: `package-error` handler + `typep` guard |
| No `handler-case` in `define-tool` body | M5 fixed: `handler-case` wraps `lisp-check-parens` call |
| `position` hash emits `null` line/column | M6 fixed: `when r-line` / `when r-col` guards |
| No `in-readtable` detection | M3 fixed: `%custom-readtable-p` skips reader check |
| `end-of-file` falls to generic `error` handler | M1 fixed: dedicated `end-of-file` handler added |

---

## Minor Issues

### m-c2-1. `%strip-name-prefix` with `"#:"` returns empty string — confusing error message

**Flagged by**: Edge Case Explorer, (also m1 in Cycle 1)
**Location**: `src/lisp-edit-form-core.lisp` — `%strip-name-prefix` and `%find-target`
**Description**: `form_name: "#:"` passes the `(>= (length name) 2)` check and
`(subseq name 2)` returns `""`. The empty string reaches `%find-target` as the lookup
target. No form candidate will ever equal `""`, so the error message becomes
`"Form defun  not found"` with a double-space (the type is followed by an empty name).
Same for `form_name: ":"` (→ `""`) and `form_name: "\"\""` (→ `""`).

**Suggestion**: Guard in `%find-target` (or `%strip-name-prefix`): if the stripped
target is empty, signal `arg-validation-error` with a helpful message:
`"form_name resolved to empty string after prefix stripping"`.

---

### m-c2-2. `lisp-edit-form` `form_name` description missing defstruct options and `#:` prefix info

**Flagged by**: End User, Integration (also m2 in Cycle 1)
**Location**: `src/lisp-edit-form.lisp` — `:description` for `form_name` argument
**Description**: The `form_name` description reads: "Form name to match; for defmethod
include specializers, e.g., `print-object ((obj my-class) stream)`". It omits:
(a) `defstruct` with options: `(defstruct (point (:conc-name pt-)) ...)` requires
`form_name: "point"` (just the car of the name list), not the full `(point ...)` form.
(b) `#:` / `:` prefix stripping: `"#:my-pkg"`, `":my-pkg"`, and `"my-pkg"` all match
`(defpackage #:my-pkg ...)` equivalently.

**Suggestion**: Extend description: "For `defstruct` with options
`(defstruct (name opts...) ...)`, use just the bare struct name. Reader macro prefixes
`#:` and `:` are stripped automatically, so `\"#:my-pkg\"` and `\"my-pkg\"` both work."

---

### m-c2-3. `lisp-patch-form` `form_name` description missing `#:` prefix info

**Flagged by**: Integration (also m3 in Cycle 1)
**Location**: `src/lisp-patch-form.lisp` — `:description` for `form_name` argument
**Description**: Same gap as m-c2-2 but for `lisp-patch-form`. The `form_name`
description has identical old text with no mention of `#:` prefix stripping.

**Suggestion**: Same as m-c2-2.

---

### m-c2-4. No unit test for `lisp-patch-form` with `#:` prefix `form_name`

**Flagged by**: Integration (also m4 in Cycle 1)
**Location**: `tests/tools-test.lisp`
**Description**: `tests/tools-test.lisp` has `tools-call-lisp-edit-form-defpackage-hash-colon-prefix`
verifying `lisp-edit-form` with `"form_name": "#:test-hash-colon-pkg"`. No equivalent
test exists for `lisp-patch-form`. The fix is in the shared `%locate-target-form` call
chain, so the behavior is correct — but a test would catch future regressions.

**Suggestion**: Add a protocol-level test for `lisp-patch-form` with a `#:` prefix
`form_name`.

---

### m-c2-5. `"message": null` in paren-error responses; `"expected"/"found": null` in reader-error responses

**Flagged by**: Integration, Error Handling (also m6 in Cycle 1)
**Location**: `src/validate.lisp` — define-tool body, `make-ht` call
**Description**: The `make-ht` in the define-tool body always populates all keys:
```lisp
(make-ht "content" ... "ok" ok "kind" kind
         "expected" expected "found" found
         "message" message ...)
```
For paren errors, `message` is nil → `"message": null` in the JSON response.
For reader errors, `expected`/`found` are nil → `"expected": null, "found": null`.
This creates an asymmetric response shape where clients cannot distinguish
"field not applicable" from "field missing".

**Suggestion**: Either omit nil keys from `make-ht` (conditional population), or
document that all keys are always present and may be null, making it a stable contract.

---

### m-c2-6. `%scan-parens` does not detect unclosed block comments at end of input

**Flagged by**: Edge Case Explorer
**Location**: `src/validate.lisp` — `%scan-parens`, post-loop check
**Description**: After the main scan loop, `%scan-parens` checks for unclosed
parentheses (via the `stack`) but never checks `(scan-state-block-depth state)`.
An unclosed `#|` block comment (e.g. `"(foo) #|"`) passes the paren scan as `:ok t`
and then falls to `%try-reader-check` to catch it. `%try-reader-check` does catch it
(as `end-of-file`), and with the M1 fix the position is now reported accurately.
However, having the paren scanner itself detect unclosed block comments would make the
system more consistent (paren scanner handles all structural issues, reader handles
semantic issues) and would allow a more specific `kind` value.

**Suggestion**: After the main loop, add:
```lisp
(when (plusp (scan-state-block-depth state))
  (return-from %scan-parens
    (list :ok nil :kind "unclosed-block-comment" ...)))
```
This requires tracking the opening position of the outermost `#|`.

---

### m-c2-7. Reader error messages contain verbose SBCL stream representation

**Flagged by**: Edge Case Explorer
**Location**: `src/validate.lisp` — `%try-reader-check`, `:message (format nil "~A" e)`
**Description**: In SBCL, `reader-error` condition's `print-object` output includes
the full stream object description, e.g.:
`"unexpected end of file on #<STRING-INPUT-STREAM (unavailable) from \"(secret input)\">"`.
This echoes the full input back in the message, which is verbose and may expose
sensitive content for large or private inputs.

**Suggestion**: Extract just the condition text without the stream portion. Options:
(a) `(format nil "~A" (simple-condition-format-control e))` if the condition supports it,
(b) truncate the message after a fixed length (e.g., 200 chars),
(c) use `slot-value` to extract just the reader error reason without the stream rendition.

---

## Positive Findings

- All 6 Cycle-1 Major issues (M1–M6) confirmed fixed and verified by independent testers
- `%custom-readtable-p` correctly skips reader checking for custom readtable files
- `package-error` false positives eliminated (both direct `package-error` and
  `SB-INT:SIMPLE-READER-PACKAGE-ERROR` via `reader-error` + `typep` guard)
- `end-of-file` handler captures accurate stream position
- `position` hash correctly omits nil `line`/`column` keys
- All 11 validate-test assertions pass; all 271 tools-test assertions pass
- Test coverage for reader errors added in Cycle 1 (4 unit tests + 1 protocol test)

---

## Recommended Actions (Priority Order)

1. **Fix empty-string guard in `%strip-name-prefix`** (m-c2-1) — confusing error message
2. **Update `form_name` descriptions in `lisp-edit-form` and `lisp-patch-form`** (m-c2-2, m-c2-3)
3. **Add `lisp-patch-form` `#:` prefix test** (m-c2-4) — regression prevention
4. **Omit null keys from MCP response** (m-c2-5) — cleaner protocol contract
5. **Detect unclosed block comments in `%scan-parens`** (m-c2-6) — quality improvement
6. **Truncate reader error messages** (m-c2-7) — information hygiene
