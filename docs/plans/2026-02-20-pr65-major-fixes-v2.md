# PR #65 Major Fixes v2 — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the 3 remaining Major issues found in the v2 comprehensive test review of PR #65 (robust JSON error handling).

**Architecture:** Each fix is independent and can be committed separately. Tasks are ordered by risk: simple defensive wrappers first (Tasks 1-2), then the more complex parser rewrite (Task 3). TDD throughout — failing test first, minimal fix, verify.

**Tech Stack:** Common Lisp, Rove (testing), SBCL, ECMA-48 (terminal escape sequences)

**Design Doc:** `docs/plans/2026-02-20-pr65-major-fixes-design.md`

---

### Task 1: Add non-string type guard to `sanitize-for-json`

**Context:** `sanitize-for-json` has a nil guard but no non-string guard. Passing an integer (e.g., `42`) causes `TYPE-ERROR: The value 42 is not of type SEQUENCE` because `(length string)` fails. This function runs on error paths where unexpected types could arrive.

**Files:**
- Modify: `src/utils/sanitize.lisp` — `sanitize-for-json` function
- Modify: `tests/utils-sanitize-test.lisp` — add new test

**Step 1: Write the failing test**

Add after the last `deftest` in `tests/utils-sanitize-test.lisp`:

```lisp
(deftest sanitize-for-json-handles-non-string-input
  (testing "Integer input is converted to its string representation"
    (ok (string= "42" (sanitize-for-json 42))))
  (testing "Symbol input is converted to its string representation"
    (ok (stringp (sanitize-for-json :hello))))
  (testing "Float input is converted to its string representation"
    (ok (stringp (sanitize-for-json 3.14)))))
```

**Step 2: Run test to verify it fails**

Run: `rove tests/utils-sanitize-test.lisp`
Expected: FAIL with `TYPE-ERROR: The value 42 is not of type SEQUENCE`

**Step 3: Implement the type guard**

In `src/utils/sanitize.lisp`, in `sanitize-for-json`, add a non-string guard immediately after the existing nil guard. The full function top should become:

```lisp
(defun sanitize-for-json (string)
  "Remove characters that are invalid in JSON strings.
Strips ANSI escape sequences, control characters (codes 0-31 except tab,
newline, carriage return), and the DEL character (code 127).
Returns NIL when given NIL. Non-string inputs are converted via
princ-to-string then sanitized."
  (when (null string) (return-from sanitize-for-json nil))
  (unless (stringp string)
    (return-from sanitize-for-json
      (sanitize-for-json (princ-to-string string))))
  ;; ... rest of function unchanged
```

Only 2 lines of new code + docstring update. Do NOT modify anything below the `(let ((result ...` line.

**Step 4: Run test to verify it passes**

Run: `rove tests/utils-sanitize-test.lisp`
Expected: ALL tests PASS (11 tests including the new one).

**Step 5: Commit**

```bash
git add src/utils/sanitize.lisp tests/utils-sanitize-test.lisp
git commit -m "fix: sanitize-for-json handles non-string input defensively

Add a type guard after the nil check. Non-nil, non-string inputs are
converted via princ-to-string then recursively sanitized. This prevents
TYPE-ERROR crashes on error paths where unexpected types could arrive."
```

---

### Task 2: Wrap `log-event` calls in error handlers with `ignore-errors`

**Context:** Two `log-event` calls inside error handler clauses in `src/protocol.lisp` are NOT wrapped with `ignore-errors`. If `*log-stream*` is broken or yason:encode fails during logging, the exception escapes and can crash the connection. The `log-event` at the bottom of `process-json-line` (the outermost handler) IS already wrapped — these two are inconsistent.

**Files:**
- Modify: `src/protocol.lisp` — 2 locations

**Step 1: No separate failing test needed**

This is a defensive `ignore-errors` wrap. The existing tests verify that `process-json-line` and `handle-initialize` produce correct output. Testing broken `*log-stream*` would require mocking infrastructure that doesn't exist. The fix is 2 trivial wraps for consistency.

**Step 2: Wrap `log-event` in `process-json-line`'s `%decode-json` error handler**

In `src/protocol.lisp`, find the `handler-case` around `(%decode-json trimmed)` inside `process-json-line`. Change:

```lisp
                 (error (e)
                   (log-event :warn "rpc.parse-error"
                              "line" trimmed
                              "error" (princ-to-string e))
```

to:

```lisp
                 (error (e)
                   (ignore-errors
                     (log-event :warn "rpc.parse-error"
                                "line" trimmed
                                "error" (princ-to-string e)))
```

**Step 3: Wrap `log-event` in `handle-initialize`'s root-sync error handler**

In `src/protocol.lisp`, find the `handler-case` around root-path sync in `handle-initialize`. Change:

```lisp
          (error (e)
            (log-event :warn "initialize.sync-root-failed" "path" root
                       "error" (princ-to-string e)))
```

to:

```lisp
          (error (e)
            (ignore-errors
              (log-event :warn "initialize.sync-root-failed" "path" root
                         "error" (princ-to-string e))))
```

**Step 4: Run tests to verify no regression**

Run: `rove tests/protocol-test.lisp`
Expected: All 23 tests PASS.

**Step 5: Commit**

```bash
git add src/protocol.lisp
git commit -m "fix: protect log-event calls in error handlers with ignore-errors

Two log-event calls inside handler-case bodies were not wrapped with
ignore-errors, inconsistent with the pattern at the outer process-json-line
handler. If *log-stream* is broken or log data encoding fails, the
unprotected call could crash the TCP connection."
```

---

### Task 3: ECMA-48 compliant escape sequence parser

**Context:** The sanitizer only handles CSI (`ESC[`) sequences. Other escape types leave remnant bytes in the output. For example, OSC title-setting `ESC]0;title BEL` becomes `"]0;title"` — the ESC is stripped as a control char but the remaining content leaks through.

This task extends the ESC handler to dispatch on the byte following ESC per ECMA-48:

| ESC + byte | Sequence type | Action |
|------------|---------------|--------|
| `[` (0x5B) | CSI | Existing logic: consume params + final byte |
| `]` (0x5D) | OSC | Consume until BEL (0x07) or ST (ESC + `\`) |
| `P`, `X`, `^`, `_` | DCS/SOS/PM/APC | Consume until ST (ESC + `\`) |
| 0x40-0x5F range (other) | 2-byte sequence (SS2, SS3, etc.) | Consume ESC + next byte |
| Other/non-ASCII | Unknown | Strip ESC only (existing fallback) |

**Files:**
- Modify: `src/utils/sanitize.lisp` — `sanitize-for-json` function (rewrite the ESC handling `cond` branch)
- Modify: `tests/utils-sanitize-test.lisp` — add 4 new tests

**Step 1: Write the failing tests**

Add after the last `deftest` in `tests/utils-sanitize-test.lisp`:

```lisp
(deftest sanitize-for-json-strips-osc-sequence
  (testing "OSC sequence (ESC ] ... BEL) is fully stripped"
    (let ((esc (code-char 27))
          (bel (code-char 7)))
      (ok (string= "before after"
                    (sanitize-for-json
                     (format nil "before ~C]0;window title~C after" esc bel))))))
  (testing "OSC sequence terminated by ST (ESC \\) is fully stripped"
    (let ((esc (code-char 27)))
      (ok (string= "before after"
                    (sanitize-for-json
                     (format nil "before ~C]0;title~C\\ after" esc esc)))))))

(deftest sanitize-for-json-strips-ss3-sequence
  (testing "SS3 sequence (ESC O) is stripped as 2-byte sequence"
    (let ((esc (code-char 27)))
      ;; ESC O P is F1 key in SS3 mode
      (ok (string= "P after"
                    (sanitize-for-json
                     (format nil "~COP after" esc))))
      ;; ESC N is SS2
      (ok (string= "x after"
                    (sanitize-for-json
                     (format nil "~CNx after" esc)))))))

(deftest sanitize-for-json-strips-dcs-sequence
  (testing "DCS sequence (ESC P ... ST) is fully stripped"
    (let ((esc (code-char 27)))
      (ok (string= "before after"
                    (sanitize-for-json
                     (format nil "before ~CP1$r0m~C\\ after" esc esc)))))))

(deftest sanitize-for-json-preserves-non-ascii-after-esc
  (testing "Non-ASCII byte after bare ESC is preserved"
    (let ((esc (code-char 27)))
      ;; ESC followed by a non-ASCII char — ESC is stripped, char preserved
      (ok (search "日本語"
                  (sanitize-for-json
                   (format nil "~C日本語" esc)))))))
```

**Step 2: Run tests to verify they fail**

Run: `rove tests/utils-sanitize-test.lisp`
Expected: FAIL — OSC content leaks as `"]0;window title"`, SS3 outputs `"OP after"` instead of `"P after"`, DCS content leaks.

**Step 3: Implement the ECMA-48 parser**

In `src/utils/sanitize.lisp`, replace the entire `sanitize-for-json` function with:

```lisp
(defun sanitize-for-json (string)
  "Remove characters that are invalid in JSON strings.
Strips ANSI/ECMA-48 escape sequences (CSI, OSC, DCS, SOS, PM, APC, and
2-byte sequences like SS2/SS3), control characters (codes 0-31 except tab,
newline, carriage return), and the DEL character (code 127).
Returns NIL when given NIL. Non-string inputs are converted via
princ-to-string then sanitized."
  (when (null string) (return-from sanitize-for-json nil))
  (unless (stringp string)
    (return-from sanitize-for-json
      (sanitize-for-json (princ-to-string string))))
  (let ((result
         (make-array (length string) :element-type 'character :fill-pointer 0
                     :adjustable t))
        (i 0)
        (len (length string)))
    (loop while (< i len)
          do (let* ((char (char string i))
                    (code (char-code char)))
               (cond
                ;; ESC (0x1B) — dispatch on the byte following ESC
                ((= code 27)
                 (let ((next-i (1+ i)))
                   (if (>= next-i len)
                       ;; Bare ESC at end of string — strip it
                       (incf i)
                       (let* ((next-char (char string next-i))
                              (next-code (char-code next-char)))
                         (cond
                          ;; CSI: ESC [ — consume params + final byte (0x40-0x7E)
                          ((char= next-char #\[)
                           (setf i (+ next-i 1))
                           (loop while (and (< i len)
                                            (let ((c (char-code (char string i))))
                                              (and (< c #x80)
                                                   (not (<= #x40 c #x7e)))))
                                 do (incf i))
                           (when (and (< i len)
                                      (let ((c (char-code (char string i))))
                                        (<= #x40 c #x7e)))
                             (incf i)))
                          ;; OSC: ESC ] — consume until BEL (0x07) or ST (ESC \)
                          ((char= next-char #\])
                           (setf i (+ next-i 1))
                           (loop while (< i len)
                                 do (cond
                                     ((= (char-code (char string i)) 7)
                                      (incf i) (return))
                                     ((and (= (char-code (char string i)) 27)
                                           (< (1+ i) len)
                                           (char= (char string (1+ i)) #\\))
                                      (incf i 2) (return))
                                     (t (incf i)))))
                          ;; DCS/SOS/PM/APC: ESC P, ESC X, ESC ^, ESC _
                          ;; Consume until ST (ESC \)
                          ((member next-char '(#\P #\X #\^ #\_))
                           (setf i (+ next-i 1))
                           (loop while (< i len)
                                 do (cond
                                     ((and (= (char-code (char string i)) 27)
                                           (< (1+ i) len)
                                           (char= (char string (1+ i)) #\\))
                                      (incf i 2) (return))
                                     (t (incf i)))))
                          ;; Other 2-byte sequences: ESC + byte in 0x40-0x5F
                          ;; Includes SS2 (ESC N), SS3 (ESC O), etc.
                          ((<= #x40 next-code #x5f)
                           (setf i (+ next-i 1)))
                          ;; Non-ASCII after ESC — preserve the non-ASCII char,
                          ;; strip only the ESC
                          ((>= next-code #x80)
                           (incf i))
                          ;; Unknown ASCII after ESC — strip ESC only
                          (t (incf i)))))))
                ;; Preserve allowed whitespace
                ((member char '(#\Tab #\Newline #\Return))
                 (vector-push-extend char result) (incf i))
                ;; Strip control characters (0-31)
                ((< code 32) (incf i))
                ;; Strip DEL (127)
                ((= code 127) (incf i))
                ;; Pass through everything else
                (t (vector-push-extend char result) (incf i)))))
    (coerce result 'string)))
```

Key changes from original:
- Added non-string type guard (from Task 1 — already done if executing sequentially)
- ESC handler now dispatches on next byte instead of only checking `ESC[`
- New branches: OSC (`ESC]`), DCS/SOS/PM/APC (`ESC P/X/^/_`), 2-byte (`0x40-0x5F`)
- Non-ASCII bytes after ESC are preserved (not consumed)

**Step 4: Run tests to verify they pass**

Run: `rove tests/utils-sanitize-test.lisp`
Expected: ALL tests PASS (15 tests — 11 original + 4 new).

**Step 5: Run full test suite to check for regressions**

Run: `rove tests/protocol-test.lisp tests/repl-test.lisp tests/tools-test.lisp`
Expected: All PASS — `sanitize-for-json` is used across these modules.

**Step 6: Commit**

```bash
git add src/utils/sanitize.lisp tests/utils-sanitize-test.lisp
git commit -m "fix: ECMA-48 compliant escape sequence parser in sanitize-for-json

Extend the ESC handler to dispatch on the byte following ESC:
- CSI (ESC [): existing logic, consume params + final byte
- OSC (ESC ]): consume until BEL or ST (ESC \\)
- DCS/SOS/PM/APC (ESC P/X/^/_): consume until ST
- 2-byte sequences (0x40-0x5F): consume ESC + next byte (SS2, SS3, etc.)
- Non-ASCII after ESC: preserve the non-ASCII char, strip only ESC
- Unknown ASCII after ESC: strip ESC only

Previously only CSI was handled; other escape types left remnant bytes
in the output (e.g., OSC title-setting became \"]0;title\")."
```

---

### Task 4: Final verification — full test suite + lint

**Files:** None (verification only)

**Step 1: Run all core tests**

```bash
rove tests/core-test.lisp tests/protocol-test.lisp tests/tools-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp tests/utils-sanitize-test.lisp
```

Expected: All PASS.

**Step 2: Lint changed files**

```bash
mallet src/utils/sanitize.lisp src/protocol.lisp
```

Expected: No warnings.

**Step 3: If any failures, fix and re-run before proceeding.**

---

## Summary of Changes

| Task | File | Major # | Change |
|------|------|---------|--------|
| 1 | `src/utils/sanitize.lisp` | #1 | Non-string type guard |
| 1 | `tests/utils-sanitize-test.lisp` | #1 | Integer/symbol/float input test |
| 2 | `src/protocol.lisp` | #2 | `ignore-errors` around 2 `log-event` calls |
| 3 | `src/utils/sanitize.lisp` | #3 | ECMA-48 dispatch (OSC, DCS, SS2/SS3, etc.) |
| 3 | `tests/utils-sanitize-test.lisp` | #3 | OSC, SS3, DCS, non-ASCII tests |
| 4 | (all) | — | Full test suite + lint verification |

## Execution Notes

- **Task 1 and Task 2** are independent and can be done in parallel.
- **Task 3** builds on Task 1 (the type guard is included in the full function rewrite). If executing sequentially, Task 1's type guard change will be superseded by Task 3's full rewrite — that's fine, both commits are meaningful.
- **Task 4** must run last.
