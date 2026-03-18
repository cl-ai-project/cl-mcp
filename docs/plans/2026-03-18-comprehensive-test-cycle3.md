# Comprehensive Test Report: cl-mcp (Cycle 3)

Date: 2026-03-18
Scope: Post-Cycle-2 fixes — all 6 minor issues (m-c2-1 through m-c2-7) applied
Perspectives: End User, Edge Case Explorer, Error Handling, Integration

---

## Test Configuration

- Perspectives: End User, Edge Case Explorer, Error Handling, Integration
- Focus: Verification that Cycle 2 fixes hold; discovery of remaining issues

---

## Summary

| Severity | Count |
|----------|-------|
| Critical | 0 |
| Major    | 2 (same root cause — `~@[` format arg-shift; all testers agree) |
| Minor    | 8 (deduplicated) |
| **Total**| **10** |

**All 6 Cycle-2 minor fixes confirmed working.** All 17 validate-test assertions and all 275+ tools-test assertions pass.

---

## Critical Issues

None.

---

## Major Issues

### M-C3-1. `~@[` arg-shift in paren-error summary string

**Flagged by**: Edge Case Explorer, Error Handling, Integration (all three agree)
**Location**: `src/validate.lisp` — `define-tool "lisp-check-parens"` body, `format nil` call for non-reader-error kinds

**Description**: The paren-error summary format string:
```
"Unbalanced parentheses: ~A~@[ (expected ~A, found ~A)~] at line ~D, column ~D~A"
```
uses `~@[...~]` with the pattern `:expected ~A, :found ~A` inside the directive body. Per CLHS 22.3.7.2, `~@[clause~]` **consumes** its test argument whether true or false. When `expected` is `NIL` (for `extra-close`, `unclosed-block-comment`, `too-large` kinds), `~@[` consumes `expected` as the condition and skips the clause — but `found` is NOT consumed inside the skipped block, shifting all subsequent positional arguments one slot left.

**Observed garbled output:**
- `kind="extra-close"` (expected=nil, found=")"): → `"Unbalanced parentheses: extra-close at line ), column 18"` (")` in line slot; "1" and "8" concatenated as "18")
- `kind="unclosed-block-comment"` (expected=nil, found=nil, line=1): → `"Unbalanced parentheses: unclosed-block-comment at line NIL, column 17"`
- `kind="too-large"` (expected=nil, found=nil): → `"Unbalanced parentheses: too-large at line NIL, column 11"`

`mismatch` and `unclosed` kinds are unaffected (their `expected` is always non-nil).

The structured JSON fields (`position`, `expected`, `found`) in the payload hash are correct — only `content[].text` (what AI agents see) is wrong.

**Fix**: Pre-compute the expected/found fragment outside the format call:
```lisp
(let ((ef (if (and expected found)
              (format nil " (expected ~A, found ~A)" expected found)
              "")))
  (format nil "Unbalanced parentheses: ~A~A at line ~D, column ~D~A"
          kind ef line col (if next-tool " Use lisp-edit-form for existing Lisp files." "")))
```

---

### M-C3-2. `~@[` arg-shift in reader-error summary when `line` is nil

**Flagged by**: Edge Case Explorer, Error Handling, Integration (all three agree)
**Location**: `src/validate.lisp` — `define-tool` body, `format nil "Reader error~@[ at line ~D, column ~D~]: ~A"` call

**Description**: The catch-all `error` handler in `%try-reader-check` returns `:line nil :column nil` (no reliable stream position). When `line` is nil, `~@[ at line ~D, column ~D~]` consumes `line` as the condition (false), skips the location clause, and then the `~A` after `~]` picks up `col` (nil) instead of `message`. The actual error message is never printed, yielding `"Reader error: NIL"` instead of `"Reader error: <message text>"`.

**Fix**: Split into two independent single-argument conditionals:
```lisp
(format nil "Reader error~@[ at line ~D~]~@[, column ~D~]: ~A"
        line col (or message "unknown"))
```
Each `~@[...~]` now only controls its own one argument, so a nil `line` (skipping the first clause) does not consume `col`, and `message` is always the final argument.

---

## Minor Issues

### m-c3-1. Summary text says "Unbalanced parentheses" for `unclosed-block-comment` and `too-large`

**Flagged by**: End User
**Location**: `src/validate.lisp` — define-tool body, `format nil` call
**Description**: Once the Major fix is applied, the summary will read `"Unbalanced parentheses: unclosed-block-comment at line 1, column 7"`. The phrase "Unbalanced parentheses" is misleading for an unclosed block comment (not a paren issue). Similarly for `too-large`.
**Suggestion**: Use `"Syntax error"` or kind-specific text for non-paren kinds.

---

### m-c3-2. Tool description for `lisp-check-parens` does not mention `unclosed-block-comment` kind

**Flagged by**: End User
**Location**: `src/validate.lisp` — define-tool `:description` string
**Description**: The description mentions `kind: "reader-error"` explicitly but is silent about `"unclosed-block-comment"`.
**Suggestion**: Add: "Also detects unclosed `#|` block comments (kind: `\"unclosed-block-comment\"`)."

---

### m-c3-3. `%maybe-add-lisp-edit-guidance` does not cover `unclosed-block-comment`

**Flagged by**: Integration
**Location**: `src/validate.lisp` — `%maybe-add-lisp-edit-guidance`
**Description**: `lisp-edit-form` guidance is only added for `extra-close`, `mismatch`, `unclosed`. `unclosed-block-comment` is missing, so agents get no `next_tool` hint for this new kind.
**Suggestion**: Add `"unclosed-block-comment"` to the `member` check.

---

### m-c3-4. `kind=null` in ok=true response (schema inconsistency)

**Flagged by**: Integration
**Location**: `src/validate.lisp` — define-tool body, base `make-ht` call
**Description**: `"kind": null` always appears in the response even when `ok=true` (no error), while `expected`, `found`, `message`, `position` are now conditionally omitted. Inconsistent: `kind` should also be absent for the success case.
**Suggestion**: Move `kind` to a conditional `setf` like the other fields.

---

### m-c3-5. Nested `#|` depth not tracked inside block comments

**Flagged by**: Edge Case Explorer
**Location**: `src/validate.lisp` — `%scan-handle-block-comment`
**Description**: `%scan-handle-block-comment` decrements `block-depth` on `|#` but does not increment it on nested `#|`. For `"#| outer #| inner |# still-outer"`, `%scan-parens` incorrectly returns `ok=t`. The `%try-reader-check` fallback rescues the verdict, but with `kind="reader-error"` instead of `"unclosed-block-comment"`.
**Suggestion**: Detect `#|` inside block comments and increment depth.

---

### m-c3-6. Whitespace-only `form_name` after prefix stripping bypasses empty-string guard

**Flagged by**: Edge Case Explorer
**Location**: `src/lisp-edit-form-core.lisp` — `%find-target`, empty-string guard
**Description**: `form_name="#: "` strips to `" "` (one space), which has `(zerop (length " ")) = nil`. The guard does not fire; a generic "Form defun   not found" error appears with invisible space in the name.
**Suggestion**: Use `(string= "" (string-trim '(#\Space #\Tab #\Newline) target))` instead of `(zerop (length target))`.

---

### m-c3-7. Stream representation still appears in reader error messages (minor exposure)

**Flagged by**: Integration, Error Handling
**Location**: `src/validate.lisp` — `%truncate-message`
**Description**: SBCL `reader-error` `~A` output includes `Stream: #<STRING-INPUT-STREAM...>` which exposes the input. The truncation fix caps length at 200 chars, but for short inputs the full stream repr still appears in the message field.
**Suggestion**: Strip at first `"\n  Stream:"` before the 200-char cap.

---

### m-c3-8. No protocol-level tests for `content[].text` correctness for `extra-close`, `unclosed-block-comment`

**Flagged by**: Integration
**Location**: `tests/tools-test.lisp`
**Description**: No tools-test exercises the `content[].text` summary format for `extra-close` or `unclosed-block-comment` kinds. The Major issues M-C3-1/M-C3-2 went undetected because test coverage only checked structured fields via validate-test (internal hash), not the MCP protocol text.
**Suggestion**: Add protocol-level tests that verify `content[].text` contains correct line/column for these kinds.

---

## Positive Findings

- All 6 Cycle-2 minor fixes (m-c2-1 through m-c2-7) confirmed working
- All validate-test (17 assertions) and tools-test (275+ assertions) pass
- Structured JSON fields (`position`, `expected`, `found`, `kind`) in the payload hash are correct
- `%truncate-message` truncation logic is correct (197 + 3 = ≤200 chars total)
- Empty form_name guard fires with clear, actionable error message
- `#:` prefix stripping works for both `lisp-edit-form` and `lisp-patch-form`
- `block-open-pos` tracking correctly identifies outermost `#|` position

---

## Recommended Actions (Priority Order)

1. **Fix `~@[` arg-shift in paren-error summary** (M-C3-1) — primary quality issue
2. **Fix `~@[` arg-shift in reader-error summary** (M-C3-2) — same root cause, easy fix
3. **Add protocol-level tests for summary text correctness** (m-c3-8) — prevent future regressions
4. **Add `unclosed-block-comment` to `%maybe-add-lisp-edit-guidance`** (m-c3-3)
5. **Update tool description to mention `unclosed-block-comment` kind** (m-c3-2)
6. **Fix whitespace-only form_name guard** (m-c3-6)
7. **Fix `kind=null` in ok=true response** (m-c3-4)
8. **Strip stream repr from reader error messages** (m-c3-7)
9. **Track nested `#|` depth inside block comments** (m-c3-5)
