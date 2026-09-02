# Paren Diagnostics Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** When `lisp-check-parens`, `lisp-edit-form`, or `lisp-patch-form` receives delimiter-broken input, tell the caller which line to fix and how, instead of "unclosed at line 1" or "end of file on".

**Architecture:** A new pure module `src/paren-diagnostics.lisp` owns the delimiter scanner (moved out of `src/validate.lisp`), a parinfer-based "likely fix" line diff, a column-0 heuristic, an open/close counter, and one formatter that all three tools call so the wording is identical. `lisp-edit-form` keeps auto-repair but shows the repaired form and refuses `]`/`}` leftovers; `lisp-patch-form` detects depth mismatch before reading the file; both tools convert an unparseable target file into a guided error.

**Tech Stack:** SBCL, ASDF package-inferred-system, Rove, existing `cl-mcp/src/parinfer:apply-indent-mode`, `uiop:split-string`.

Spec: `docs/superpowers/specs/2026-09-03-paren-diagnostics-design.md`

## Global Constraints

- SBCL only; do not spend effort on other implementations.
- Package-inferred-system: new source files need **no** `cl-mcp.asd` edit. A new test file is registered by adding `(:import-from #:cl-mcp/tests/<name>)` to the root `tests.lisp`.
- Edit `.lisp` files only with the cl-mcp tools (`lisp-edit-form`, `lisp-patch-form`, `fs-write-file` for new files). Never use text editors/sed on Lisp source. After editing, run `lisp-check-parens` on the file.
- Style: 2-space indent, <= 100 columns, blank line between top-level forms, docstrings on public functions, `(in-package ...)` at the top.
- Lint before every commit: `mallet src/<changed files>.lisp`.
- Tests: prefer the `run-tests` tool with `{"system": "cl-mcp/tests/<name>-test"}`. Before each PR, run the full suite with `rove cl-mcp.asd` from Bash (single-file runs can hide failures) and `(asdf:compile-system :cl-mcp :force t)` via `repl-eval` to catch warnings. About 427 UIOP "redefining" warnings are pre-existing noise.
- The parent MCP process goes stale when a new export is added: if `lisp-edit-form` cannot read a file that references a brand-new symbol, restart the MCP server or use `fs-write-file`/`rove` from a fresh process.
- Wording copied verbatim from the spec must be kept: `Likely fix, inferred from indentation:`, `Next top-level form begins at line N, so the missing ")" must come before it.`, `No changes were written to disk.`, `Automatic repair could not produce a readable form; fix the delimiters by hand.`
- Commit messages end with:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01G6botZcYV9w4LDpqGg9sU3
  ```
- Work on branch `design/paren-diagnostics` (already exists) or a branch off it; never commit to `main` directly.

---

## File Structure

| File | Responsibility | PR |
|---|---|---|
| `src/paren-diagnostics.lisp` (new) | Delimiter scanner (moved), `diagnose-delimiters`, `count-delimiter-depth`, `repair-line-differences`, `format-repair-lines`, `format-delimiter-diagnosis`. Depends only on `cl-mcp/src/parinfer` and `uiop`. | 1 |
| `tests/paren-diagnostics-test.lisp` (new) | Unit tests for every public function above. | 1 |
| `src/validate.lisp` | Loses the scanner; `lisp-check-parens` calls `diagnose-delimiters` and appends the diagnosis text to the summary. | 1 |
| `tests/validate-test.lisp` | New assertions for `likely_fixes`, `next_top_level_line`, summary text. | 1 |
| `tests.lisp` | Registers the new test package. | 1 |
| `src/lisp-edit-form-core.lisp` | Defines `file-unparseable-error`; `%locate-target-form` converts parse failures into it. | 2 |
| `src/lisp-edit-form.lisp` | Repair visibility, `content-unrepairable-error`, handler clauses. | 2 |
| `tests/lisp-edit-form-test.lisp` | Tests for repaired-form display, `]` refusal, dropped-paren wording, broken-file guidance. | 2 |
| `src/lisp-patch-form.lisp` | Depth pre-check, diagnosis in parse failure, `file-unparseable-error` clause. | 3 |
| `tests/lisp-patch-form-test.lisp` | Tests for depth mismatch (both directions), nesting-only breakage, broken-file guidance. | 3 |

---

# PR 1: Diagnostic core and `lisp-check-parens`

### Task 1: Create `src/paren-diagnostics.lisp` with the moved scanner

**Files:**
- Create: `src/paren-diagnostics.lisp`
- Create: `tests/paren-diagnostics-test.lisp`
- Modify: `tests.lisp` (add one `:import-from`)
- Reference: `src/validate.lisp:19-186` (the scanner to move; do not delete it yet — Task 4 does that)

**Interfaces:**
- Produces: `cl-mcp/src/paren-diagnostics:scan-delimiters (text &key (base-offset 0))` → plist `(:ok bool :kind string-or-nil :expected string-or-nil :found string-or-nil :offset int :line int :column int)`. Identical behaviour to today's `%scan-parens`.

- [ ] **Step 1: Write the failing test file**

Create `tests/paren-diagnostics-test.lisp` with `fs-write-file`:

```lisp
;;;; tests/paren-diagnostics-test.lisp

(defpackage #:cl-mcp/tests/paren-diagnostics-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:scan-delimiters
                #:diagnose-delimiters
                #:count-delimiter-depth
                #:repair-line-differences
                #:format-repair-lines
                #:format-delimiter-diagnosis))

(in-package #:cl-mcp/tests/paren-diagnostics-test)

;;; Fixtures: the four measured cases from the spec (section 2.3).

(defparameter +let-binding-unclosed+
  (format nil "(defun f (x)~%  (let ((y 1)~%    (+ x y)))")
  "Missing \")\" after the let binding on line 2.")

(defparameter +trailing-extra-close+
  (format nil "(defun f (x)~%  (let ((y 1))~%    (+ x y))))")
  "One \")\" too many at the end of line 3.")

(defparameter +when-body-unclosed+
  (format nil "(defun f (x)~%  (let ((y 1))~%    (when (> x 0)~%      (format t \"~~A\" y)~%    (+ x y)))")
  "The when form on line 3 is never closed; line 4 needs one more \")\".")

(defparameter +file-middle-form-unclosed+
  (format nil "(in-package #:cl-user)~%~%(defun probe-a (x)~%  \"Docstring.\"~%  (let ((y (* x 2)))~%    (if (> y 10)~%        (format t \"big ~~A~~%\" y)~%        (format t \"small ~~A~~%\" y)~%    y))~%~%(defun probe-b (x)~%  (list x x))~%")
  "probe-a (line 3) never closes; line 8 needs one more \")\"; probe-b starts at line 11.")

(defparameter +stray-bracket+
  (format nil "(defun f (x)~%  (let ((y 1]~%    (+ x y)))")
  "A \"]\" where \")\" was meant, on line 2 column 13.")

(deftest scan-delimiters-balanced
  (testing "balanced text returns :ok t"
    (ok (getf (scan-delimiters "(+ 1 2)") :ok))))

(deftest scan-delimiters-extra-close
  (testing "extra close reports kind, offset, line and column"
    (let ((res (scan-delimiters "(+ 1 2))")))
      (ng (getf res :ok))
      (ok (string= (getf res :kind) "extra-close"))
      (ok (= (getf res :offset) 7))
      (ok (= (getf res :line) 1))
      (ok (= (getf res :column) 8)))))

(deftest scan-delimiters-unclosed
  (testing "unclosed reports the innermost still-open opener"
    (let ((res (scan-delimiters +let-binding-unclosed+)))
      (ng (getf res :ok))
      (ok (string= (getf res :kind) "unclosed"))
      (ok (string= (getf res :expected) ")"))
      (ok (= (getf res :line) 1))
      (ok (= (getf res :column) 1)))))

(deftest scan-delimiters-mismatch
  (testing "] closing ( is a mismatch at its own position"
    (let ((res (scan-delimiters +stray-bracket+)))
      (ok (string= (getf res :kind) "mismatch"))
      (ok (string= (getf res :expected) ")"))
      (ok (string= (getf res :found) "]"))
      (ok (= (getf res :line) 2))
      (ok (= (getf res :column) 13)))))

(deftest scan-delimiters-base-offset
  (testing "base-offset shifts :offset only, never :line"
    (let ((res (scan-delimiters "(+ 1 2))" :base-offset 100)))
      (ok (= (getf res :offset) 107))
      (ok (= (getf res :line) 1)))))

(deftest scan-delimiters-ignores-strings-comments-char-literals
  (testing "parens inside strings, comments and #\\( are not counted"
    (ok (getf (scan-delimiters "(list \")\" #\\( #\\) ; )
 #| ) |# )") :ok))))
```

- [ ] **Step 2: Register the test package in `tests.lisp`**

Use `lisp-patch-form` on `tests.lisp`, `form_type` `defpackage`, `form_name` `cl-mcp/tests`:
- `old_text`: `  (:import-from #:cl-mcp/tests/parinfer-test)`
- `new_text`: `  (:import-from #:cl-mcp/tests/parinfer-test)
  (:import-from #:cl-mcp/tests/paren-diagnostics-test)`

- [ ] **Step 3: Run the test to verify it fails**

Run `run-tests` with `{"system": "cl-mcp/tests/paren-diagnostics-test"}`.
Expected: load failure, "package CL-MCP/SRC/PAREN-DIAGNOSTICS does not exist" (or the system cannot be found).

- [ ] **Step 4: Create `src/paren-diagnostics.lisp` with the scanner**

Create with `fs-write-file`. The scanner body is copied from `src/validate.lisp` with `%scan-parens` renamed to `scan-delimiters`; everything else is verbatim so behaviour cannot drift.

```lisp
;;;; src/paren-diagnostics.lisp
;;;;
;;;; Delimiter diagnostics shared by lisp-check-parens, lisp-edit-form and
;;;; lisp-patch-form: a balance scanner, a parinfer-based "likely fix" line
;;;; diff, a column-0 heuristic, an open/close counter, and one formatter so
;;;; all three tools describe the same breakage with the same words.

(defpackage #:cl-mcp/src/paren-diagnostics
  (:use #:cl)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:uiop
                #:split-string)
  (:documentation "Delimiter balance diagnostics with repair hints.")
  (:export #:scan-delimiters
           #:diagnose-delimiters
           #:count-delimiter-depth
           #:repair-line-differences
           #:format-repair-lines
           #:format-delimiter-diagnosis))

(in-package #:cl-mcp/src/paren-diagnostics)

;;; ---------------------------------------------------------------------------
;;; Balance scanner (moved verbatim from validate.lisp)
;;; ---------------------------------------------------------------------------

(defun %closing (opener)
  (ecase opener
    (#\( #\))
    (#\[ #\])
    (#\{ #\})))

(defun %scan-parens-push-open (stack line col base-offset ch idx)
  (cons (list ch line col (+ base-offset idx)) stack))

(defun %scan-parens-pop-open (stack line col base-offset ch idx)
  (if (null stack)
      (values stack
              (list :ok nil
                    :kind "extra-close"
                    :expected nil
                    :found (string ch)
                    :offset (+ base-offset idx)
                    :line line
                    :column col))
      (destructuring-bind (top-ch top-line top-col top-off) (car stack)
        (declare (ignore top-line top-col top-off))
        (let ((expected (%closing top-ch)))
          (if (char= expected ch)
              (values (cdr stack) nil)
              (values stack
                      (list :ok nil
                            :kind "mismatch"
                            :expected (string expected)
                            :found (string ch)
                            :offset (+ base-offset idx)
                            :line line
                            :column col)))))))

(defstruct scan-state
  (line 1 :type fixnum)
  (col 1 :type fixnum)
  (stack '() :type list)
  (in-string nil :type boolean)
  (escape nil :type boolean)
  (line-comment nil :type boolean)
  (block-depth 0 :type fixnum)
  (block-open-pos 0 :type fixnum))

(defun %scan-handle-line-comment (state ch)
  (when (char= ch #\Newline)
    (setf (scan-state-line-comment state) nil)))

(defun %scan-handle-string (state ch)
  (cond
    ((scan-state-escape state)
     (setf (scan-state-escape state) nil))
    ((char= ch #\\)
     (setf (scan-state-escape state) t))
    ((char= ch #\")
     (setf (scan-state-in-string state) nil))))

(defun %scan-handle-block-comment (state ch next)
  (when (and (char= ch #\|) next (char= next #\#))
    (decf (scan-state-block-depth state))
    t))

(defun %scan-handle-normal (state ch next idx base-offset text)
  "Handle a character in normal (non-string, non-comment) context.
Returns (VALUES err consumed) where CONSUMED is NIL or a positive integer
indicating how many additional characters past CH were consumed."
  (cond
   ((char= ch #\;) (setf (scan-state-line-comment state) t) (values nil nil))
   ((char= ch #\") (setf (scan-state-in-string state) t) (values nil nil))
   ;; Character literal: #\x or #\Space etc.  Skip past entirely so that
   ;; delimiter characters like #\( are not treated as open-parens.
   ((and (char= ch #\#) next (char= next #\\))
    (let ((skip 1))  ; at minimum skip the backslash
      (let ((char-pos (+ idx 2)))
        (when (< char-pos (length text))
          (incf skip)  ; skip the character after backslash
          ;; Named character literals: consume remaining alpha chars
          (when (alpha-char-p (char text char-pos))
            (loop for k from (1+ char-pos) below (length text)
                  while (alpha-char-p (char text k))
                  do (incf skip)))))
      (values nil skip)))
   ((and (char= ch #\#) next (char= next #\|))
    (when (zerop (scan-state-block-depth state))
      (setf (scan-state-block-open-pos state) (+ base-offset idx)))
    (incf (scan-state-block-depth state))
    (values nil 1))
   ((or (char= ch #\() (char= ch #\[) (char= ch #\{))
    (setf (scan-state-stack state)
            (%scan-parens-push-open (scan-state-stack state)
             (scan-state-line state) (scan-state-col state) base-offset ch
             idx))
    (values nil nil))
   ((or (char= ch #\)) (char= ch #\]) (char= ch #\}))
    (multiple-value-bind (new-stack err)
        (%scan-parens-pop-open (scan-state-stack state) (scan-state-line state)
         (scan-state-col state) base-offset ch idx)
      (setf (scan-state-stack state) new-stack)
      (values err nil)))
   (t (values nil nil))))

(defun %scan-advance-position (state ch)
  (cond
    ((char= ch #\Newline)
     (incf (scan-state-line state))
     (setf (scan-state-col state) 1))
    (t
     (incf (scan-state-col state)))))

(defun scan-delimiters (text &key (base-offset 0))
  "Return a plist describing balance of delimiters in TEXT.
Keys: :ok (boolean), :kind (string|nil), :expected, :found, :offset, :line, :column.
:kind is one of \"extra-close\", \"mismatch\", \"unclosed\",
\"unclosed-block-comment\". BASE-OFFSET is added to :offset only; :line and
:column are always relative to the start of TEXT."
  (let ((state (make-scan-state))
        (len (length text))
        (idx 0))
    (loop while (< idx len)
          for ch = (char text idx)
          for next = (and (< (1+ idx) len) (char text (1+ idx)))
          do
            (cond
              ((scan-state-line-comment state)
               (%scan-handle-line-comment state ch))
              ((scan-state-in-string state)
               (%scan-handle-string state ch))
              ((plusp (scan-state-block-depth state))
               (when (%scan-handle-block-comment state ch next)
                 (incf idx)
                 (incf (scan-state-col state))))
              (t
               (multiple-value-bind (err consumed)
                   (%scan-handle-normal state ch next idx base-offset text)
                 (when err
                   (return-from scan-delimiters err))
                 (when consumed
                   (let ((n (if (integerp consumed) consumed 1)))
                     (incf idx n)
                     (incf (scan-state-col state) n))))))
            (%scan-advance-position state ch)
            (incf idx))
    (when (plusp (scan-state-block-depth state))
      (let* ((open-pos  (scan-state-block-open-pos state))
             (local-pos (- open-pos base-offset))
             (pre       (subseq text 0 (min local-pos (length text))))
             (r-line    (1+ (count #\Newline pre)))
             (col-start (or (position #\Newline pre :from-end t) -1))
             (r-col     (- local-pos col-start)))
        (return-from scan-delimiters
          (list :ok nil
                :kind "unclosed-block-comment"
                :expected nil
                :found nil
                :offset open-pos
                :line r-line
                :column r-col))))
    (when (scan-state-stack state)
      (destructuring-bind (ch l c off) (pop (scan-state-stack state))
        (return-from scan-delimiters
          (list :ok nil
                :kind "unclosed"
                :expected (string (%closing ch))
                :found nil
                :offset off
                :line l
                :column c))))
    (list :ok t)))
```

Add temporary stubs for the other exported names so the test package loads (they are replaced in Tasks 2 and 3):

```lisp
(defun diagnose-delimiters (text &key (base-offset 0))
  "Stub; replaced in Task 2."
  (scan-delimiters text :base-offset base-offset))

(defun count-delimiter-depth (text)
  "Stub; replaced in Task 2."
  (declare (ignore text))
  (values 0 0))

(defun repair-line-differences (original repaired)
  "Stub; replaced in Task 2."
  (declare (ignore original repaired))
  nil)

(defun format-repair-lines (fixes)
  "Stub; replaced in Task 3."
  (declare (ignore fixes))
  "")

(defun format-delimiter-diagnosis (diagnosis &key (target "code"))
  "Stub; replaced in Task 3."
  (declare (ignore diagnosis target))
  "")
```

- [ ] **Step 5: Run the tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/paren-diagnostics-test"}`.
Expected: 6 tests pass.

- [ ] **Step 6: Lint and commit**

```bash
mallet src/paren-diagnostics.lisp
git add src/paren-diagnostics.lisp tests/paren-diagnostics-test.lisp tests.lisp
git commit -m "feat(paren-diagnostics): new module with the delimiter scanner moved from validate"
```

---

### Task 2: `count-delimiter-depth`, `repair-line-differences`, `diagnose-delimiters`

**Files:**
- Modify: `src/paren-diagnostics.lisp` (replace the three stubs, add private helpers)
- Modify: `tests/paren-diagnostics-test.lisp` (append tests)

**Interfaces:**
- Produces:
  - `count-delimiter-depth (text)` → `(values opens closes)` counting only `(` and `)` outside strings, line comments, block comments and character literals.
  - `repair-line-differences (original repaired)` → list of `(:line n :original "..." :repaired "..." :delta d)`, `d` = closing parens added on that line (negative = removed).
  - `diagnose-delimiters (text &key (base-offset 0))` → the `scan-delimiters` plist, plus, when not ok and kind is not `"unclosed-block-comment"`: `:likely-fixes list`, `:repair-failed bool`, `:next-top-level-line int-or-nil`; and when kind is `"unclosed"`: `:unclosed-form-line int`, `:unclosed-form-head string`.

- [ ] **Step 1: Append failing tests**

Append to `tests/paren-diagnostics-test.lisp` using `lisp-edit-form` `insert_after` on `deftest scan-delimiters-ignores-strings-comments-char-literals` (one form per call):

```lisp
(deftest count-delimiter-depth-basic
  (testing "counts only code parens"
    (multiple-value-bind (opens closes) (count-delimiter-depth "(if (> y 10)")
      (ok (= opens 2))
      (ok (= closes 1)))
    (multiple-value-bind (opens closes)
        (count-delimiter-depth "(list \")\" #\\( #\\) ; )
 #| ( |# )")
      (ok (= opens 1))
      (ok (= closes 1)))))
```

```lisp
(deftest repair-line-differences-reports-changed-lines
  (testing "only changed lines are listed, with the added count"
    (let ((diff (repair-line-differences
                 (format nil "(a~%  (b~%  c)")
                 (format nil "(a~%  (b)~%  c)"))))
      (ok (= (length diff) 1))
      (ok (= (getf (first diff) :line) 2))
      (ok (string= (getf (first diff) :original) "  (b"))
      (ok (string= (getf (first diff) :repaired) "  (b)"))
      (ok (= (getf (first diff) :delta) 1))))
  (testing "removed parens give a negative delta"
    (let ((diff (repair-line-differences "(a))" "(a)")))
      (ok (= (getf (first diff) :delta) -1)))))
```

```lisp
(deftest diagnose-let-binding-unclosed
  (testing "likely fix points at the let binding line"
    (let* ((d (diagnose-delimiters +let-binding-unclosed+))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ng (getf d :repair-failed))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 2))
      (ok (= (getf (first fixes) :delta) 1))
      (ok (= (getf d :unclosed-form-line) 1))
      (ok (string= (getf d :unclosed-form-head) "(defun f (x)"))
      (ng (getf d :next-top-level-line)))))
```

```lisp
(deftest diagnose-trailing-extra-close
  (testing "likely fix removes one paren from the last line"
    (let* ((d (diagnose-delimiters +trailing-extra-close+))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "extra-close"))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 3))
      (ok (= (getf (first fixes) :delta) -1)))))
```

```lisp
(deftest diagnose-when-body-unclosed
  (testing "likely fix points at the last line of the when body"
    (let* ((d (diagnose-delimiters +when-body-unclosed+))
           (fixes (getf d :likely-fixes)))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 4))
      (ok (= (getf (first fixes) :delta) 1)))))
```

```lisp
(deftest diagnose-file-middle-form-unclosed
  (testing "file-level diagnosis names the open form and the next top-level line"
    (let* ((d (diagnose-delimiters +file-middle-form-unclosed+))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ok (= (getf d :unclosed-form-line) 3))
      (ok (string= (getf d :unclosed-form-head) "(defun probe-a (x)"))
      (ok (= (getf d :next-top-level-line) 11))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 8))
      (ok (= (getf (first fixes) :delta) 1)))))
```

```lisp
(deftest diagnose-stray-bracket-is-repair-failed
  (testing "] cannot be repaired: no fixes, repair-failed t"
    (let ((d (diagnose-delimiters +stray-bracket+)))
      (ok (string= (getf d :kind) "mismatch"))
      (ok (getf d :repair-failed))
      (ng (getf d :likely-fixes)))))
```

```lisp
(deftest diagnose-ok-has-no-extra-keys
  (testing "balanced text returns the plain scan plist"
    (let ((d (diagnose-delimiters "(+ 1 2)")))
      (ok (getf d :ok))
      (ng (getf d :likely-fixes))
      (ng (getf d :next-top-level-line)))))
```

```lisp
(deftest diagnose-form-head-is-trimmed-and-bounded
  (testing "unclosed-form-head trims indentation and stops at 40 chars"
    (let* ((long-name (make-string 60 :initial-element #\a))
           (d (diagnose-delimiters (format nil "   (defun ~A (x)~%  x" long-name))))
      (ok (= (length (getf d :unclosed-form-head)) 40))
      (ok (string= (subseq (getf d :unclosed-form-head) 0 7) "(defun ")))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run `run-tests` with `{"system": "cl-mcp/tests/paren-diagnostics-test"}`.
Expected: the 9 new tests fail (stubs return 0/0, nil, plain scan plist).

- [ ] **Step 3: Implement the helpers and replace the three stubs**

Use `lisp-edit-form` `replace` on each stub (`defun count-delimiter-depth`, `defun repair-line-differences`, `defun diagnose-delimiters`) and `insert_before` `defun count-delimiter-depth` for the private helpers.

```lisp
;;; ---------------------------------------------------------------------------
;;; Code-character walker shared by the counter and the column-0 heuristic
;;; ---------------------------------------------------------------------------

(defun %map-code-characters (text function)
  "Call FUNCTION with (CH IDX LINE COL) for every character of TEXT that is
outside strings, line comments, block comments and character literals.
LINE and COL are 1-based."
  (let ((len (length text)) (idx 0) (line 1) (col 1)
        (in-string nil) (escape nil) (line-comment nil) (block-depth 0))
    (loop while (< idx len)
          do (let* ((ch (char text idx))
                    (next (and (< (1+ idx) len) (char text (1+ idx)))))
               (cond
                 (line-comment
                  (when (char= ch #\Newline) (setf line-comment nil)))
                 (in-string
                  (cond (escape (setf escape nil))
                        ((char= ch #\\) (setf escape t))
                        ((char= ch #\") (setf in-string nil))))
                 ((plusp block-depth)
                  (cond ((and (char= ch #\|) next (char= next #\#))
                         (decf block-depth) (incf idx) (incf col))
                        ((and (char= ch #\#) next (char= next #\|))
                         (incf block-depth) (incf idx) (incf col))))
                 ((char= ch #\;) (setf line-comment t))
                 ((char= ch #\") (setf in-string t))
                 ((and (char= ch #\#) next (char= next #\|))
                  (incf block-depth) (incf idx) (incf col))
                 ((and (char= ch #\#) next (char= next #\\))
                  ;; #\x or #\Name: skip the backslash and the literal itself.
                  (let ((skip 1))
                    (when (< (+ idx 2) len)
                      (incf skip)
                      (when (alpha-char-p (char text (+ idx 2)))
                        (loop for k from (+ idx 3) below len
                              while (alpha-char-p (char text k))
                              do (incf skip))))
                    (incf idx skip)
                    (incf col skip)))
                 (t (funcall function ch idx line col)))
               (if (char= ch #\Newline)
                   (setf line (1+ line) col 1)
                   (incf col))
               (incf idx)))))

(defun %next-top-level-line (text)
  "Return the 1-based line of the first \"(\" in column 1 that appears while an
earlier form is still open, or NIL. Such a line almost always means the
previous top-level form was never closed."
  (let ((depth 0))
    (%map-code-characters
     text
     (lambda (ch idx line col)
       (declare (ignore idx))
       (case ch
         (#\( (when (and (= col 1) (plusp depth))
                (return-from %next-top-level-line line))
              (incf depth))
         (#\) (when (plusp depth) (decf depth))))))
    nil))

(defun %line-text (text line)
  "Return the LINE-th (1-based) line of TEXT, or \"\" when out of range."
  (let ((lines (split-string text :separator '(#\Newline))))
    (if (<= 1 line (length lines))
        (nth (1- line) lines)
        "")))

(defun %form-head (text line)
  "Return the trimmed first 40 characters of LINE in TEXT, for naming a form."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return) (%line-text text line))))
    (if (> (length trimmed) 40)
        (subseq trimmed 0 40)
        trimmed)))

(defun %stray-bracket-p (text)
  "Return T when TEXT contains ] or } outside strings, comments and char literals."
  (%map-code-characters
   text
   (lambda (ch idx line col)
     (declare (ignore idx line col))
     (when (or (char= ch #\]) (char= ch #\}))
       (return-from %stray-bracket-p t))))
  nil)

(defun %likely-fixes (text)
  "Run parinfer on TEXT and return (VALUES fixes repair-failed-p).
FIXES is the line diff from REPAIR-LINE-DIFFERENCES. REPAIR-FAILED-P is T
when the repaired text still has a stray ] or }, or is still unbalanced;
FIXES is NIL in that case."
  (let ((repaired (apply-indent-mode text)))
    (if (or (%stray-bracket-p repaired)
            (not (getf (scan-delimiters repaired) :ok)))
        (values nil t)
        (values (repair-line-differences text repaired) nil))))
```

```lisp
(defun count-delimiter-depth (text)
  "Return two values: the number of \"(\" and the number of \")\" in TEXT
outside strings, comments and character literals. Only round parentheses
are counted; [ and { are constituent characters in Common Lisp."
  (let ((opens 0) (closes 0))
    (%map-code-characters
     text
     (lambda (ch idx line col)
       (declare (ignore idx line col))
       (case ch
         (#\( (incf opens))
         (#\) (incf closes)))))
    (values opens closes)))
```

```lisp
(defun repair-line-differences (original repaired)
  "Compare ORIGINAL and REPAIRED (parinfer output) line by line.
Return a list of (:line n :original str :repaired str :delta d) for every
line that changed, where D is the number of \")\" added (negative if removed).
Both texts must have the same number of lines, which parinfer guarantees."
  (loop for orig in (split-string original :separator '(#\Newline))
        for rep in (split-string repaired :separator '(#\Newline))
        for line from 1
        unless (string= orig rep)
          collect (list :line line
                        :original orig
                        :repaired rep
                        :delta (- (count #\) rep) (count #\) orig)))))
```

```lisp
(defun diagnose-delimiters (text &key (base-offset 0))
  "Scan TEXT like SCAN-DELIMITERS and, when it is unbalanced, add repair hints:
:likely-fixes (parinfer line diff), :repair-failed, :next-top-level-line,
and for kind \"unclosed\" also :unclosed-form-line and :unclosed-form-head.
A balanced TEXT or an unclosed block comment returns the plain scan plist."
  (let* ((scan (scan-delimiters text :base-offset base-offset))
         (kind (getf scan :kind)))
    (if (or (getf scan :ok)
            (string= kind "unclosed-block-comment"))
        scan
        (multiple-value-bind (fixes failed) (%likely-fixes text)
          (append scan
                  (list :likely-fixes fixes
                        :repair-failed failed
                        :next-top-level-line (%next-top-level-line text))
                  (when (string= kind "unclosed")
                    (let ((line (getf scan :line)))
                      (list :unclosed-form-line line
                            :unclosed-form-head (%form-head text line)))))))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/paren-diagnostics-test"}`.
Expected: all 15 tests pass. If `diagnose-when-body-unclosed` fails on the line number, check that `apply-indent-mode` closes `when` on line 4 (it did in the measured prototype: `"      (format t \"~A\" y))"`).

- [ ] **Step 5: Lint and commit**

```bash
mallet src/paren-diagnostics.lisp
git add src/paren-diagnostics.lisp tests/paren-diagnostics-test.lisp
git commit -m "feat(paren-diagnostics): diagnose-delimiters with parinfer likely-fix line diff"
```

---

### Task 3: `format-repair-lines` and `format-delimiter-diagnosis`

**Files:**
- Modify: `src/paren-diagnostics.lisp` (replace the two stubs)
- Modify: `tests/paren-diagnostics-test.lisp` (append tests)

**Interfaces:**
- Produces:
  - `format-repair-lines (fixes)` → string; one line per fix, each starting with a newline and two spaces: `\n  line 2: "  (let ((y 1)"  ->  add 1 ")"`. Empty string for NIL.
  - `format-delimiter-diagnosis (diagnosis &key (target "code"))` → multi-line string per spec section 4.3. `target` is the subject: `"code"`, `"content"`, `"new_text"`, `"the patched form"`, or a file path.

- [ ] **Step 1: Append failing tests**

```lisp
(deftest format-repair-lines-wording
  (testing "add/remove wording and quoting"
    (let ((text (format-repair-lines
                 (list (list :line 2 :original "  (let ((y 1)" :repaired "  (let ((y 1))" :delta 1)
                       (list :line 9 :original "  x))" :repaired "  x)" :delta -1)))))
      (ok (search (format nil "~%  line 2: \"  (let ((y 1)\"  ->  add 1 \")\"") text))
      (ok (search (format nil "~%  line 9: \"  x))\"  ->  remove 1 \")\"") text))))
  (testing "no fixes gives an empty string"
    (ok (string= (format-repair-lines nil) ""))))
```

```lisp
(deftest format-diagnosis-unclosed
  (testing "unclosed names the form, the likely fix and the next top-level line"
    (let ((text (format-delimiter-diagnosis
                 (diagnose-delimiters +file-middle-form-unclosed+)
                 :target "/tmp/probe.lisp")))
      (ok (search "Unbalanced parentheses in /tmp/probe.lisp: unclosed (form starting at line 3: \"(defun probe-a (x)\")." text))
      (ok (search "Likely fix, inferred from indentation:" text))
      (ok (search "line 8:" text))
      (ok (search "add 1 \")\"" text))
      (ok (search "Next top-level form begins at line 11, so the missing \")\" must come before it." text)))))
```

```lisp
(deftest format-diagnosis-unclosed-without-next-top-level
  (testing "single-form input omits the next-top-level sentence"
    (let ((text (format-delimiter-diagnosis (diagnose-delimiters +let-binding-unclosed+))))
      (ok (search "Unbalanced parentheses in code: unclosed (form starting at line 1: \"(defun f (x)\")." text))
      (ok (search "line 2:" text))
      (ng (search "Next top-level form" text)))))
```

```lisp
(deftest format-diagnosis-extra-close
  (testing "extra-close offers both readings and the parinfer removal"
    (let ((text (format-delimiter-diagnosis (diagnose-delimiters +trailing-extra-close+))))
      (ok (search "Unbalanced parentheses in code: extra \")\" at line 3, column 14." text))
      (ok (search "Either remove that \")\" or check for a form opened earlier that was never closed." text))
      (ok (search "line 3:" text))
      (ok (search "remove 1 \")\"" text)))))
```

```lisp
(deftest format-diagnosis-mismatch
  (testing "mismatch explains that ] is a symbol character"
    (let ((text (format-delimiter-diagnosis (diagnose-delimiters +stray-bracket+) :target "content")))
      (ok (search "Unbalanced parentheses in content: expected \")\" but found \"]\" at line 2, column 13." text))
      (ok (search "\"]\" and \"}\" are ordinary symbol characters in Common Lisp and cannot be auto-repaired." text))
      (ok (search "Replace it with \")\"." text))
      (ok (search "Automatic repair could not produce a readable form; fix the delimiters by hand." text))
      (ng (search "Likely fix" text)))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run `run-tests` with `{"system": "cl-mcp/tests/paren-diagnostics-test"}`.
Expected: the 5 new tests fail (stubs return "").

- [ ] **Step 3: Replace the two stubs**

```lisp
(defun format-repair-lines (fixes)
  "Render FIXES (from REPAIR-LINE-DIFFERENCES) as indented lines, each
preceded by a newline, e.g. \"  line 2: \\\"  (let ((y 1)\\\"  ->  add 1 \\\")\\\"\"."
  (with-output-to-string (s)
    (dolist (fix fixes)
      (let ((delta (getf fix :delta)))
        (format s "~%  line ~D: ~S  ->  ~A ~D \")\""
                (getf fix :line)
                (getf fix :original)
                (if (minusp delta) "remove" "add")
                (abs delta))))))
```

```lisp
(defun format-delimiter-diagnosis (diagnosis &key (target "code"))
  "Render DIAGNOSIS (from DIAGNOSE-DELIMITERS) as guidance text.
TARGET is the subject of the first sentence: \"code\", \"content\", \"new_text\",
or a file path. The likely-fix block is included only when parinfer produced
one; otherwise a repair-failed sentence is printed instead."
  (let ((kind (getf diagnosis :kind))
        (line (getf diagnosis :line))
        (column (getf diagnosis :column))
        (expected (getf diagnosis :expected))
        (found (getf diagnosis :found))
        (fixes (getf diagnosis :likely-fixes))
        (failed (getf diagnosis :repair-failed))
        (next-line (getf diagnosis :next-top-level-line)))
    (with-output-to-string (s)
      (cond
        ((string= kind "unclosed")
         (format s "Unbalanced parentheses in ~A: unclosed (form starting at line ~D: ~S)."
                 target (getf diagnosis :unclosed-form-line)
                 (getf diagnosis :unclosed-form-head)))
        ((string= kind "extra-close")
         (format s "Unbalanced parentheses in ~A: extra ~S at line ~D, column ~D.~%~
                    Either remove that ~S or check for a form opened earlier that was never closed."
                 target found line column found))
        ((string= kind "mismatch")
         (format s "Unbalanced parentheses in ~A: expected ~S but found ~S at line ~D, column ~D.~%~
                    \"]\" and \"}\" are ordinary symbol characters in Common Lisp and cannot be auto-repaired.~%~
                    Replace it with ~S."
                 target expected found line column expected))
        (t
         (format s "Unbalanced parentheses in ~A: ~A at line ~D, column ~D."
                 target kind line column)))
      (cond
        (fixes
         (format s "~%Likely fix, inferred from indentation:~A" (format-repair-lines fixes)))
        (failed
         (format s "~%Automatic repair could not produce a readable form; fix the delimiters by hand.")))
      (when (and next-line (string= kind "unclosed"))
        (format s "~%Next top-level form begins at line ~D, so the missing \")\" must come before it."
                next-line)))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/paren-diagnostics-test"}`.
Expected: all 20 tests pass.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/paren-diagnostics.lisp
git add src/paren-diagnostics.lisp tests/paren-diagnostics-test.lisp
git commit -m "feat(paren-diagnostics): shared diagnosis formatter"
```

---

### Task 4: `lisp-check-parens` uses the diagnostic core

**Files:**
- Modify: `src/validate.lisp` (defpackage, delete scanner lines 19-186, `lisp-check-parens`, define-tool summary/payload)
- Modify: `tests/validate-test.lisp` (append tests)

**Interfaces:**
- Consumes: `diagnose-delimiters`, `format-delimiter-diagnosis` from Task 2/3.
- Produces: `lisp-check-parens` hash gains `"likely_fixes"` (vector of hash with `"line"`, `"original"`, `"repaired"`, `"delta"`), `"next_top_level_line"` (integer or absent), `"diagnosis_text"` (string, internal; not copied into the MCP payload). The MCP summary text ends with the diagnosis text.

- [ ] **Step 1: Append failing tests to `tests/validate-test.lisp`**

Insert after `deftest lisp-check-parens-ok-field-is-json-bool`:

```lisp
(deftest lisp-check-parens-likely-fixes-field
  (testing "unbalanced result carries parinfer likely fixes and diagnosis text"
    (let* ((res (lisp-check-parens
                 :code (format nil "(defun f (x)~%  (let ((y 1)~%    (+ x y)))")))
           (fixes (gethash "likely_fixes" res))
           (first-fix (and fixes (plusp (length fixes)) (aref fixes 0))))
      (ok (not (%ok? res)))
      (ok (vectorp fixes) "likely_fixes should be a vector")
      (ok (= (length fixes) 1))
      (ok (= (gethash "line" first-fix) 2))
      (ok (= (gethash "delta" first-fix) 1))
      (ok (string= (gethash "original" first-fix) "  (let ((y 1)"))
      (ok (string= (gethash "repaired" first-fix) "  (let ((y 1))"))
      (ok (search "Likely fix" (gethash "diagnosis_text" res)))
      (ok (null (gethash "next_top_level_line" res))))))
```

```lisp
(deftest lisp-check-parens-next-top-level-line-field
  (testing "a file-shaped input reports the next top-level form line"
    (let ((res (lisp-check-parens
                :code (format nil "(defun a ()~%  (list 1)~%~%(defun b ()~%  2)~%"))))
      (ok (string= (%kind res) "unclosed"))
      (ok (= (gethash "next_top_level_line" res) 4)))))
```

```lisp
(deftest lisp-check-parens-summary-includes-diagnosis
  (testing "MCP summary text carries the likely-fix guidance"
    (let* ((state (cl-mcp/src/state:make-state))
           (args (cl-mcp/src/tools/helpers:make-ht
                  "code" (format nil "(defun f (x)~%  (let ((y 1)~%    (+ x y)))")))
           ;; define-tool generates <tool-name>-handler (see lisp-edit-form-handler
           ;; in tests/lisp-edit-form-test.lisp); check src/tools/define-tool.lisp
           ;; if this symbol is not found.
           (response (cl-mcp/src/validate::lisp-check-parens-handler state "cp-1" args))
           (result-obj (gethash "result" response))
           (text (gethash "text" (aref (gethash "content" result-obj) 0))))
      (ok (search "Unbalanced parentheses: unclosed at line 1, column 1" text)
          "existing first line is preserved")
      (ok (search "Likely fix, inferred from indentation:" text))
      (ok (search "line 2:" text))
      (ok (vectorp (gethash "likely_fixes" result-obj))
          "sibling likely_fixes field is present")
      (ok (null (gethash "diagnosis_text" result-obj))
          "internal diagnosis_text is not leaked into the payload"))))
```

```lisp
(deftest lisp-check-parens-balanced-has-no-fix-fields
  (testing "balanced input has no likely_fixes"
    (let ((res (lisp-check-parens :code "(+ 1 2)")))
      (ok (%ok? res))
      (ok (null (gethash "likely_fixes" res))))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run `run-tests` with `{"system": "cl-mcp/tests/validate-test"}`.
Expected: the 4 new tests fail; existing ones pass.

- [ ] **Step 3: Rewrite `src/validate.lisp`**

3a. Replace the `defpackage` (`lisp-edit-form` `replace`, `form_type` `defpackage`, `form_name` `cl-mcp/src/validate`):

```lisp
(defpackage #:cl-mcp/src/validate
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content
                #:arg-validation-error #:json-bool)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:lisp-check-parens
           #:*check-parens-max-bytes*))
```

3b. Delete the moved scanner with `lisp-edit-form` `delete`, one form each: `defun %closing`, `defun %scan-parens-push-open`, `defun %scan-parens-pop-open`, `defstruct scan-state`, `defun %scan-handle-line-comment`, `defun %scan-handle-string`, `defun %scan-handle-block-comment`, `defun %scan-handle-normal`, `defun %scan-advance-position`, `defun %scan-parens`. Keep `%maybe-add-lisp-edit-guidance`, `%custom-readtable-p`, `%truncate-message`, `%try-reader-check`.

3c. Replace `defun lisp-check-parens`:

```lisp
(defun %fix->hash (fix)
  "Convert one (:line :original :repaired :delta) plist into a string-keyed hash."
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "line" h) (getf fix :line)
          (gethash "original" h) (getf fix :original)
          (gethash "repaired" h) (getf fix :repaired)
          (gethash "delta" h) (getf fix :delta))
    h))

(defun lisp-check-parens (&key path code offset limit)
  "Check balanced parentheses/brackets in CODE or PATH slice.
Also checks for reader errors (e.g. unknown dispatch characters, #. with
*read-eval* nil) even when parentheses are balanced.
Returns a hash table with key \"ok\" and, when not ok, \"kind\", and
either \"expected\"/\"found\" (delimiter mismatch) or \"message\" (reader error),
plus a \"position\" hash with \"line\", \"column\", \"offset\".
Delimiter failures also carry \"likely_fixes\" (vector of line/original/
repaired/delta hashes inferred by parinfer), \"next_top_level_line\" when a
later top-level form was swallowed, and \"diagnosis_text\" (the guidance the
MCP summary appends; not part of the MCP payload)."
  (when (and path code)
    (error "Provide either PATH or CODE, not both"))
  (when (and (null path) (null code))
    (error "Either PATH or CODE is required"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (let ((text (or code (fs-read-file path :offset offset :limit limit)))
        (base-off (or offset 0)))
    (when (> (length text) *check-parens-max-bytes*)
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "ok" h) nil
              (gethash "kind" h) "too-large"
              (gethash "expected" h) nil
              (gethash "found" h) nil)
        (let ((pos (make-hash-table :test #'equal)))
          (setf (gethash "offset" pos) base-off
                (gethash "line" pos) 1
                (gethash "column" pos) 1)
          (setf (gethash "position" h) pos))
        (return-from lisp-check-parens h)))
    (let ((diagnosis (diagnose-delimiters text :base-offset base-off))
          (reader-info (%try-reader-check text base-off)))
      (destructuring-bind (&key ok kind expected found
                                (offset base-off) (line 1) (column 1)
                                likely-fixes next-top-level-line
                           &allow-other-keys)
          diagnosis
        (let ((h (make-hash-table :test #'equal)))
          (cond
            ((not ok)
             ;; Paren error takes priority
             (setf (gethash "ok" h) nil
                   (gethash "kind" h) kind
                   (gethash "expected" h) expected
                   (gethash "found" h) found)
             (let ((pos (make-hash-table :test #'equal)))
               (setf (gethash "offset" pos) offset
                     (gethash "line" pos) line
                     (gethash "column" pos) column)
               (setf (gethash "position" h) pos))
             (unless (string= kind "unclosed-block-comment")
               (setf (gethash "likely_fixes" h)
                     (map 'vector #'%fix->hash likely-fixes)
                     (gethash "diagnosis_text" h)
                     (format-delimiter-diagnosis diagnosis :target (or path "code")))
               (when next-top-level-line
                 (setf (gethash "next_top_level_line" h) next-top-level-line)))
             (%maybe-add-lisp-edit-guidance h kind))
            (reader-info
             ;; Parens OK but reader error detected
             (setf (gethash "ok" h) nil
                   (gethash "kind" h) (getf reader-info :kind)
                   (gethash "message" h) (getf reader-info :message))
             (let ((pos (make-hash-table :test #'equal))
                   (r-line (getf reader-info :line))
                   (r-col  (getf reader-info :column)))
               (setf (gethash "offset" pos) (getf reader-info :offset))
               (when r-line   (setf (gethash "line" pos) r-line))
               (when r-col    (setf (gethash "column" pos) r-col))
               (setf (gethash "position" h) pos)))
            (t
             ;; Both checks passed
             (setf (gethash "ok" h) t)))
          h)))))
```

Note: `%fix->hash` is a separate top-level form; insert it with `insert_before` `defun lisp-check-parens`.

3d. Patch the define-tool summary and payload with `lisp-patch-form` (`form_type` `define-tool`, `form_name` `lisp-check-parens`), two patches:

Patch 1, `old_text`:
```
                            (format nil
                                    "Unbalanced parentheses: ~A~A at line ~D, column ~D~A"
                                    kind ef line col
                                    (if next-tool
                                        " Use lisp-edit-form for existing Lisp files."
                                        ""))))))))
```
`new_text`:
```
                            (format nil
                                    "Unbalanced parentheses: ~A~A at line ~D, column ~D~A~@[~%~A~]"
                                    kind ef line col
                                    (if next-tool
                                        " Use lisp-edit-form for existing Lisp files."
                                        "")
                                    (gethash "diagnosis_text" check-result))))))))
```

Patch 2, `old_text`:
```
            (when required-args
              (setf (gethash "required_args" payload) required-args))
```
`new_text`:
```
            (when required-args
              (setf (gethash "required_args" payload) required-args))
            (let ((fixes (gethash "likely_fixes" check-result))
                  (next-line (gethash "next_top_level_line" check-result)))
              (when fixes
                (setf (gethash "likely_fixes" payload) fixes))
              (when next-line
                (setf (gethash "next_top_level_line" payload) next-line)))
```

3e. Run `lisp-check-parens` on `src/validate.lisp` to confirm it is balanced.

- [ ] **Step 4: Run tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/validate-test"}` and `{"system": "cl-mcp/tests/paren-diagnostics-test"}`.
Expected: all pass. Also run `{"system": "cl-mcp/tests/tools-test"}` (it exercises the check-parens `ok` json-bool path).

- [ ] **Step 5: Lint and commit**

```bash
mallet src/validate.lisp src/paren-diagnostics.lisp
git add src/validate.lisp tests/validate-test.lisp
git commit -m "feat(lisp-check-parens): report parinfer-inferred likely fix lines"
```

---

### Task 5: PR 1 verification

**Files:** none new.

- [ ] **Step 1: Full compile for warnings**

Via `repl-eval` (package `CL-USER`, `timeout_seconds` 300):
```lisp
(asdf:compile-system :cl-mcp :force t)
```
Expected: no new warnings mentioning `paren-diagnostics` or `validate` (UIOP redefinition noise is pre-existing).

- [ ] **Step 2: Full test suite from a fresh process**

```bash
rove cl-mcp.asd
```
Expected: all suites pass, including `validate-test`, `paren-diagnostics-test`, `tools-test`, `lisp-edit-form-test`, `lisp-patch-form-test`.

- [ ] **Step 3: Manual smoke test through MCP**

Call the `lisp-check-parens` tool with `code` = `"(defun f (x)\n  (let ((y 1)\n    (+ x y)))"`.
Expected summary contains `Likely fix, inferred from indentation:` and `line 2: "  (let ((y 1)"  ->  add 1 ")"`. (If the running server predates this change, restart it first.)

- [ ] **Step 4: Open PR 1**

```bash
git push -u origin design/paren-diagnostics
gh pr create --title "feat: parinfer-inferred likely-fix lines in lisp-check-parens" --body "$(cat <<'EOF'
## Summary
- New `src/paren-diagnostics.lisp`: delimiter scanner moved out of validate.lisp, plus `diagnose-delimiters` (parinfer line diff, column-0 heuristic), `count-delimiter-depth`, and a shared formatter
- `lisp-check-parens` now appends "Likely fix, inferred from indentation:" with the exact lines to change, and reports the next top-level form line when a form swallowed the rest of the file
- Spec: docs/superpowers/specs/2026-09-03-paren-diagnostics-design.md (PR 1 of 3)

## Test plan
- [ ] `rove cl-mcp.asd` green
- [ ] `mallet src/validate.lisp src/paren-diagnostics.lisp` clean

🤖 Generated with [Claude Code](https://claude.com/claude-code)

https://claude.ai/code/session_01G6botZcYV9w4LDpqGg9sU3
EOF
)"
```

---

# PR 2: `lisp-edit-form` visibility, refusal, and broken-file guidance

Branch: `feat/edit-form-repair-visibility` off `design/paren-diagnostics` (or off `main` after PR 1 merges).

### Task 6: `file-unparseable-error` in `lisp-edit-form-core`

**Files:**
- Modify: `src/lisp-edit-form-core.lisp` (defpackage imports/exports, new condition, `%locate-target-form`)
- Modify: `tests/lisp-edit-form-test.lisp` (append one test)

**Interfaces:**
- Consumes: `diagnose-delimiters`, `format-delimiter-diagnosis` (Task 2/3); `sanitize-error-message` from `cl-mcp/src/utils/sanitize`.
- Produces: condition `cl-mcp/src/lisp-edit-form-core:file-unparseable-error` with readers `file-unparseable-path`, `file-unparseable-diagnosis`, `file-unparseable-cause`; its report string is `(file-unparseable-message condition)`. `%locate-target-form` signals it whenever `parse-top-level-forms` signals any `error`.

- [ ] **Step 1: Append the failing test**

Insert after `deftest lisp-edit-form-handler-returns-tool-error` in `tests/lisp-edit-form-test.lisp`:

```lisp
(deftest lisp-edit-form-broken-file-gives-guidance
  (testing "editing a file that does not parse names the open form and the next tool"
    (with-temp-file "tests/tmp/edit-form-broken-file.lisp"
        (format nil "(in-package #:cl-user)~%~%(defun probe-a (x)~%  (let ((y (* x 2)))~%    (if (> y 10)~%        (format t \"big\")~%        (format t \"small\")~%    y))~%~%(defun probe-c (x)~%  (list x x x))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "probe-c"
                              :operation "replace"
                              :content "(defun probe-c (x) (list x))")
            (cl-mcp/src/lisp-edit-form-core:file-unparseable-error (e)
              (setf err (princ-to-string e))))
          (ok err "should signal file-unparseable-error")
          (ok (search "unclosed (form starting at line 3: \"(defun probe-a (x)\")" err))
          (ok (search "Likely fix, inferred from indentation:" err))
          (ok (search "line 7:" err))
          (ok (search "Next top-level form begins at line 10" err))
          (ok (search "The file itself does not parse, so no form can be located." err))
          (ok (search "Run lisp-check-parens with path=" err))
          (ok (search "starting at line 3" err))
          (ok (string= before (fs-read-file path)) "file untouched"))))))
```

- [ ] **Step 2: Run the test to verify it fails**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-edit-form-test", "test": "cl-mcp/tests/lisp-edit-form-test::lisp-edit-form-broken-file-gives-guidance"}`.
Expected: FAIL (symbol `file-unparseable-error` not found / a plain "end of file" error escapes).

- [ ] **Step 3: Implement**

3a. Replace the `defpackage` in `src/lisp-edit-form-core.lisp`:

```lisp
(defpackage #:cl-mcp/src/lisp-edit-form-core
  (:use #:cl)
  (:shadowing-import-from #:cl-mcp/src/cst
                          #:cst-node
                          #:cst-node-kind
                          #:cst-node-value
                          #:cst-node-start
                          #:cst-node-end)
  (:import-from #:cl-ppcre
                #:scan-to-strings)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms)
  (:import-from #:cl-mcp/src/package-context
                #:extract-in-package-name-from-text)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:enough-pathname
                #:native-namestring
                #:subpathp)
  (:export #:%normalize-string
           #:%defmethod-candidates
           #:%definition-candidates
           #:%normalize-paths
           #:%strip-name-prefix
           #:%find-target
           #:%resolve-named-readtable
           #:%parse-readtable-designator
           #:%detect-readtable-before-node
           #:%whitespace-char-p
           #:%locate-target-form
           #:file-unparseable-error
           #:file-unparseable-path
           #:file-unparseable-diagnosis
           #:file-unparseable-cause
           #:file-unparseable-message))
```

3b. Insert before `defun %locate-target-form`:

```lisp
(defun file-unparseable-message (condition)
  "Return the guidance text for CONDITION, a FILE-UNPARSEABLE-ERROR.
When the delimiter scan found the breakage, the text is the shared diagnosis
followed by the next steps; otherwise only the sanitized reader error."
  (let* ((path (file-unparseable-path condition))
         (diagnosis (file-unparseable-diagnosis condition))
         (line (getf diagnosis :unclosed-form-line)))
    (if (getf diagnosis :ok)
        (format nil "Cannot parse ~A: ~A" path (file-unparseable-cause condition))
        (format nil "~A~%The file itself does not parse, so no form can be located.~%~
                     Run lisp-check-parens with path=~S to see the full diagnosis, then ~
                     use lisp-edit-form (operation \"replace\") on the form~@[ starting at line ~D~]."
                (format-delimiter-diagnosis diagnosis :target path)
                path line))))

(define-condition file-unparseable-error (error)
  ((path :initarg :path :reader file-unparseable-path)
   (diagnosis :initarg :diagnosis :reader file-unparseable-diagnosis)
   (cause :initarg :cause :reader file-unparseable-cause))
  (:report (lambda (c s) (write-string (file-unparseable-message c) s)))
  (:documentation "Signaled when the target file cannot be parsed into top-level forms."))
```

3c. Replace `defun %locate-target-form`:

```lisp
(defun %locate-target-form (file-path form-type form-name readtable)
  "Shared prologue: resolve paths, read file, parse, find target, extract snippet.
Signals FILE-UNPARSEABLE-ERROR, carrying a delimiter diagnosis, when the file
cannot be parsed at all.
Returns eight values:
  ABS — absolute pathname
  REL — relative namestring for FS write
  ORIGINAL — full file text
  NODES — parsed CST nodes
  TARGET — matched CST node
  TARGET-SNIPPET — text of the matched form
  FORM-TYPE-STR — downcased form-type string
  FILE-PACKAGE-NAME — package named by the file's first IN-PACKAGE form"
  (let ((form-type-str (string-downcase form-type)))
    (multiple-value-bind (abs rel)
        (%normalize-paths file-path)
      (let* ((original (fs-read-file abs))
             (nodes (handler-case
                        (parse-top-level-forms original
                                               :readtable readtable
                                               :source-path abs)
                      (error (e)
                        (error 'file-unparseable-error
                               :path (namestring abs)
                               :diagnosis (diagnose-delimiters original)
                               :cause (sanitize-error-message (princ-to-string e))))))
             (target (%find-target nodes form-type-str form-name)))
        (unless target
          (error "Form ~A ~A not found in ~A" form-type form-name (namestring abs)))
        (let ((target-snippet (subseq original
                                     (cst-node-start target)
                                     (cst-node-end target))))
          (values abs rel original nodes target target-snippet form-type-str
                  (extract-in-package-name-from-text original)))))))
```

- [ ] **Step 4: Run the test to verify it passes**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-edit-form-test"}`.
Expected: all pass, including the new test. If the parent MCP process cannot read `lisp-edit-form-core.lisp` after the new export, use `rove tests/lisp-edit-form-test.lisp` from Bash then `rove cl-mcp.asd`.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/lisp-edit-form-core.lisp
git add src/lisp-edit-form-core.lisp tests/lisp-edit-form-test.lisp
git commit -m "feat(lisp-edit-form-core): file-unparseable-error with delimiter diagnosis"
```

---

### Task 7: `lisp-edit-form` repair warning wording and `]` refusal

**Files:**
- Modify: `src/lisp-edit-form.lisp` (defpackage, `%validate-and-repair-content`, new condition, `lisp-edit-form`)
- Modify: `tests/lisp-edit-form-test.lisp` (append tests)

**Interfaces:**
- Consumes: `diagnose-delimiters`, `format-delimiter-diagnosis`, `repair-line-differences`, `format-repair-lines` (PR 1).
- Produces:
  - `%validate-and-repair-content` → `(values content warning fixes)`; `fixes` is the `repair-line-differences` list (NIL when no repair).
  - condition `content-unrepairable-error` (report = diagnosis text with target `"content"`).
  - `lisp-edit-form` non-delete, non-dry-run → `(values updated warning changed-p fixes validated-content)`; dry-run hash gains `"repair_fixes"` (the fixes list) next to `"parinfer_warning"`.

- [ ] **Step 1: Append failing tests**

```lisp
(deftest lisp-edit-form-warning-distinguishes-added-and-dropped
  (testing "extra closing parens are reported as dropped, never as a negative count"
    (with-temp-file "tests/tmp/edit-form-dropped.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning changed-p fixes)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target (x)~%  (let ((y 1))~%    (+ x y))))"))
          (declare (ignore updated changed-p))
          (ok (search "1 extra closing delimiter dropped by parinfer" warning))
          (ng (search "-1" warning))
          (ok (= (length fixes) 1))
          (ok (= (getf (first fixes) :line) 3))
          (ok (= (getf (first fixes) :delta) -1)))))))
```

```lisp
(deftest lisp-edit-form-warning-added-wording
  (testing "missing closing parens are reported as added"
    (with-temp-file "tests/tmp/edit-form-added.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning changed-p fixes)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))"))
          (declare (ignore updated changed-p))
          (ok (search "1 closing delimiter added by parinfer" warning))
          (ok (= (getf (first fixes) :line) 2))
          (ok (search "(let ((y 1))" (fs-read-file path))
              "the binding list was closed on line 2, not at the end"))))))
```

```lisp
(deftest lisp-edit-form-refuses-stray-bracket
  (testing "content with ] where ) was meant is rejected and nothing is written"
    (with-temp-file "tests/tmp/edit-form-stray-bracket.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "target"
                              :operation "replace"
                              :content (format nil "(defun target (x)~%  (let ((y 1]~%    (+ x y)))"))
            (cl-mcp/src/lisp-edit-form::content-unrepairable-error (e)
              (setf err (princ-to-string e))))
          (ok err "should signal content-unrepairable-error")
          (ok (search "Unbalanced parentheses in content: expected \")\" but found \"]\" at line 2, column 13." err))
          (ok (search "Replace it with \")\"." err))
          (ok (string= before (fs-read-file path)) "file untouched"))))))
```

```lisp
(deftest lisp-edit-form-dry-run-carries-repair-fixes
  (testing "dry-run hash exposes the repair line diff"
    (with-temp-file "tests/tmp/edit-form-dry-run-fixes.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((res (lisp-edit-form :file-path path
                                   :form-type "defun"
                                   :form-name "target"
                                   :operation "replace"
                                   :dry-run t
                                   :content (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))"))))
          (ok (stringp (gethash "parinfer_warning" res)))
          (ok (= (length (gethash "repair_fixes" res)) 1)))))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-edit-form-test"}`.
Expected: the 4 new tests fail (negative wording, no fourth value, no condition, no `repair_fixes`).

- [ ] **Step 3: Implement**

3a. Patch the `defpackage` of `cl-mcp/src/lisp-edit-form` with `lisp-patch-form`:
`old_text`:
```
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
```
`new_text`:
```
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis
                #:repair-line-differences
                #:format-repair-lines)
```
and `old_text`:
```
                #:%whitespace-char-p
                #:%locate-target-form)
```
`new_text`:
```
                #:%whitespace-char-p
                #:%locate-target-form
                #:file-unparseable-error)
```
and `old_text`:
```
                #:make-ht #:result #:rpc-error #:text-content
                #:arg-validation-error #:json-bool)
```
`new_text`:
```
                #:make-ht #:result #:rpc-error #:text-content
                #:arg-validation-error #:json-bool #:tool-error)
```

3b. Insert after `defun %multiple-top-level-forms-error-data`:

```lisp
(define-condition content-unrepairable-error (error)
  ((message :initarg :message :reader content-unrepairable-message))
  (:report (lambda (c s) (write-string (content-unrepairable-message c) s)))
  (:documentation "Signaled when CONTENT is unbalanced and parinfer cannot make it readable."))

(defun %repair-warning (fixes)
  "Describe FIXES (from REPAIR-LINE-DIFFERENCES) as a parinfer warning string.
Added and dropped closing delimiters are reported separately; the count is
never negative."
  (let ((added (loop for fix in fixes
                     for delta = (getf fix :delta)
                     when (plusp delta) sum delta))
        (dropped (loop for fix in fixes
                       for delta = (getf fix :delta)
                       when (minusp delta) sum (- delta))))
    (format nil "~{~A~^; ~}"
            (remove nil
                    (list (when (plusp added)
                            (format nil "~D closing delimiter~:P added by parinfer" added))
                          (when (plusp dropped)
                            (format nil "~D extra closing delimiter~:P dropped by parinfer"
                                    dropped))
                          (when (and (zerop added) (zerop dropped))
                            "content repaired by parinfer"))))))
```

3c. In `%validate-and-repair-content`, replace the final `multiple-value-bind (result err) ...` block. Use `lisp-patch-form` with `old_text`:
```
      (multiple-value-bind (result err)
          (try-parse content)
        (if result
            (values result nil)
            (let ((repaired (apply-indent-mode content)))
              (multiple-value-bind (repaired-result repaired-err)
                  (try-parse repaired)
                (cond
                  (repaired-result
                   (log-event :info "lisp.edit.form" "auto-repair" "success"
                              "original-error" (princ-to-string err))
                   (let ((added-count (- (length repaired) (length content))))
                     (values repaired-result
                             (format nil "~D closing delimiter~:P ~
                                          ~[were~;was~:;were~] added by parinfer"
                                     added-count added-count))))
                  ((and (typep err 'multiple-top-level-forms-error)
                        (typep repaired-err 'multiple-top-level-forms-error))
                   (error err))
                  (t
                   (error "content parse error: ~A (repair also failed: ~A)"
                          err repaired-err))))))))))
```
`new_text`:
```
      (multiple-value-bind (result err)
          (try-parse content)
        (if result
            (values result nil nil)
            (let* ((diagnosis (diagnose-delimiters content))
                   (repaired (apply-indent-mode content)))
              (when (and (not (getf diagnosis :ok))
                         (getf diagnosis :repair-failed))
                (error 'content-unrepairable-error
                       :message (format-delimiter-diagnosis diagnosis
                                                            :target "content")))
              (multiple-value-bind (repaired-result repaired-err)
                  (try-parse repaired)
                (cond
                  (repaired-result
                   (log-event :info "lisp.edit.form" "auto-repair" "success"
                              "original-error" (princ-to-string err))
                   (let ((fixes (repair-line-differences content repaired)))
                     (values repaired-result (%repair-warning fixes) fixes)))
                  ((and (typep err 'multiple-top-level-forms-error)
                        (typep repaired-err 'multiple-top-level-forms-error))
                   (error err))
                  ((not (getf diagnosis :ok))
                   (error 'content-unrepairable-error
                          :message (format-delimiter-diagnosis diagnosis
                                                               :target "content")))
                  (t
                   (error "content parse error: ~A (repair also failed: ~A)"
                          err repaired-err))))))))))
```
Also update the docstring's first paragraph to say: "Returns three values: the validated (possibly repaired) content, a parinfer warning string or NIL, and the repair line diff or NIL."

3d. In `lisp-edit-form`, patch the non-delete branch. `old_text`:
```
          (multiple-value-bind (validated-content parinfer-warning)
              (%validate-and-repair-content content readtable file-package-name
                                            abs)
```
`new_text`:
```
          (multiple-value-bind (validated-content parinfer-warning repair-fixes)
              (%validate-and-repair-content content readtable file-package-name
                                            abs)
```
`old_text`:
```
                  (when parinfer-warning
                    (setf (gethash "parinfer_warning" result) parinfer-warning))
                  result))
               (would-change (fs-write-file rel updated)
                (values updated parinfer-warning t))
               (t (values updated parinfer-warning nil)))))))))
```
`new_text`:
```
                  (when parinfer-warning
                    (setf (gethash "parinfer_warning" result) parinfer-warning
                          (gethash "repair_fixes" result) repair-fixes))
                  result))
               (would-change (fs-write-file rel updated)
                (values updated parinfer-warning t repair-fixes validated-content))
               (t (values updated parinfer-warning nil repair-fixes
                          validated-content)))))))))
```
Update the `lisp-edit-form` docstring: "For non-delete operations without DRY-RUN, returns five values: the updated file text, the parinfer warning or NIL, whether the file changed, the repair line diff or NIL, and the validated content that was spliced in."

3e. Run `lisp-check-parens` on `src/lisp-edit-form.lisp`.

- [ ] **Step 4: Run tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-edit-form-test"}`.
Expected: all pass, including `lisp-edit-form-parinfer-warning-returned` (it only searches "closing delimiter" and "parinfer") and `lisp-edit-form-auto-repair-missing-parens`.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/lisp-edit-form.lisp
git add src/lisp-edit-form.lisp tests/lisp-edit-form-test.lisp
git commit -m "feat(lisp-edit-form): added/dropped repair wording, refuse stray ] and }"
```

---

### Task 8: `lisp-edit-form` handler shows the repaired form and routes the new errors

**Files:**
- Modify: `src/lisp-edit-form.lisp` (`define-tool "lisp-edit-form"` body, new helper `%repair-summary`)
- Modify: `tests/lisp-edit-form-test.lisp` (append tests)

**Interfaces:**
- Consumes: Task 7's five return values and `"repair_fixes"`; Task 6's `file-unparseable-error`; `tool-error` from helpers.
- Produces: success summary text containing `WARNING: ...`, `Changed lines:` + repair lines, and (non-dry-run only) `--- repaired form ---` + the form; `isError` results for `content-unrepairable-error` and `file-unparseable-error` at protocol >= 2025-11-25, `-32602` rpc-error otherwise.

- [ ] **Step 1: Append failing tests**

```lisp
(deftest lisp-edit-form-summary-shows-repaired-form
  (testing "non-dry-run summary shows what parinfer actually wrote"
    (with-temp-file "tests/tmp/edit-form-summary-repaired.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "operation" "replace"
                      "content" (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))")))
               (response (funcall handler state "repaired-1" args))
               (result-obj (gethash "result" response))
               (text (gethash "text" (aref (gethash "content" result-obj) 0))))
          (ok (search "Applied replace to defun target" text))
          (ok (search "WARNING: 1 closing delimiter added by parinfer" text))
          (ok (search "Changed lines:" text))
          (ok (search "line 2: \"  (let ((y 1)\"  ->  add 1 \")\"" text))
          (ok (search "--- repaired form ---" text))
          (ok (search "(let ((y 1))" text)))))))
```

```lisp
(deftest lisp-edit-form-dry-run-summary-shows-changed-lines
  (testing "dry-run summary lists the changed lines but not a second copy of the form"
    (with-temp-file "tests/tmp/edit-form-dry-run-changed-lines.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "operation" "replace"
                      "dry_run" t
                      "content" (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))")))
               (response (funcall handler state "repaired-dry" args))
               (result-obj (gethash "result" response))
               (text (gethash "text" (aref (gethash "content" result-obj) 0))))
          (ok (search "Changed lines:" text))
          (ok (search "--- preview ---" text))
          (ng (search "--- repaired form ---" text)))))))
```

```lisp
(deftest lisp-edit-form-handler-stray-bracket-is-tool-error
  (testing "unrepairable content is an isError result on the new protocol"
    (with-temp-file "tests/tmp/edit-form-handler-stray.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "operation" "replace"
                      "content" (format nil "(defun target (x)~%  (let ((y 1]~%    (+ x y)))"))))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "stray-1" args))
                 (result-obj (gethash "result" response))
                 (text (gethash "text" (aref (gethash "content" result-obj) 0))))
            (ng (gethash "error" response))
            (ok (gethash "isError" result-obj))
            (ok (search "found \"]\"" text))))))))
```

```lisp
(deftest lisp-edit-form-handler-broken-file-is-tool-error
  (testing "a file that does not parse yields guidance as an isError result"
    (with-temp-file "tests/tmp/edit-form-handler-broken.lisp"
        (format nil "(defun a ()~%  (list 1)~%~%(defun b ()~%  2)~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "b"
                      "operation" "replace"
                      "content" "(defun b () 3)")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "broken-1" args))
                 (result-obj (gethash "result" response))
                 (text (gethash "text" (aref (gethash "content" result-obj) 0))))
            (ng (gethash "error" response))
            (ok (gethash "isError" result-obj))
            (ok (search "Run lisp-check-parens with path=" text))
            (ok (search "Next top-level form begins at line 4" text))))))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-edit-form-test"}`.
Expected: the 4 new tests fail.

- [ ] **Step 3: Implement**

3a. Insert after `defun %preview-form-text`:

```lisp
(defun %repair-summary (warning fixes repaired-form &key include-form)
  "Return the text appended to a success summary when parinfer repaired the
content, or NIL when WARNING is NIL. Lists the changed lines and, when
INCLUDE-FORM is true, the repaired form itself (bounded by %TRUNCATE-SNIPPET)."
  (when warning
    (with-output-to-string (s)
      (format s "~%WARNING: ~A" warning)
      (when fixes
        (format s "~%Changed lines:~A" (format-repair-lines fixes)))
      (when include-form
        (format s "~%~%--- repaired form ---~%~A" (%truncate-snippet repaired-form))))))
```

3b. In the `define-tool "lisp-edit-form"` body, patch the `multiple-value-bind` and the two summaries with `lisp-patch-form` (`form_type` `define-tool`, `form_name` `lisp-edit-form`):

`old_text`:
```
        (multiple-value-bind (updated parinfer-warning changed-p)
            (lisp-edit-form :file-path file_path
```
`new_text`:
```
        (multiple-value-bind (updated parinfer-warning changed-p repair-fixes
                              repaired-form)
            (lisp-edit-form :file-path file_path
```

`old_text`:
```
                     (pw (gethash "parinfer_warning" updated))
                     (summary
                      (format nil "Dry-run ~A on ~A ~A in ~A (~:[no change~;would change~])~
                                   ~@[~%WARNING: ~A~]~@[~%~%--- original ---~%~A~]~
                                   ~@[~%~%--- preview ---~%~A~]"
                              operation form_type form_name file_path would-change pw
                              (%truncate-snippet original-form)
                              (%truncate-snippet preview-form))))
```
`new_text`:
```
                     (pw (gethash "parinfer_warning" updated))
                     (summary
                      (format nil "Dry-run ~A on ~A ~A in ~A (~:[no change~;would change~])~
                                   ~@[~A~]~@[~%~%--- original ---~%~A~]~
                                   ~@[~%~%--- preview ---~%~A~]"
                              operation form_type form_name file_path would-change
                              (%repair-summary pw (gethash "repair_fixes" updated)
                                               preview-form)
                              (%truncate-snippet original-form)
                              (%truncate-snippet preview-form))))
```

`old_text`:
```
                       ((not changed-p)
                        (format nil "No change to ~A ~A in ~A (content matches existing form)~@[~%WARNING: ~A~]"
                                form_type form_name file_path parinfer-warning))
                       (t
                        (format nil "Applied ~A to ~A ~A in ~A (~D chars)~@[~%WARNING: ~A~]"
                                operation form_type form_name file_path (length updated) parinfer-warning)))))
```
`new_text`:
```
                       ((not changed-p)
                        (format nil "No change to ~A ~A in ~A (content matches existing form)~@[~A~]"
                                form_type form_name file_path
                                (%repair-summary parinfer-warning repair-fixes
                                                 repaired-form :include-form t)))
                       (t
                        (format nil "Applied ~A to ~A ~A in ~A (~D chars)~@[~A~]"
                                operation form_type form_name file_path (length updated)
                                (%repair-summary parinfer-warning repair-fixes
                                                 repaired-form :include-form t))))))
```

3c. Add handler clauses. `old_text`:
```
      (multiple-top-level-forms-error ()
```
`new_text`:
```
      (content-unrepairable-error (e)
        (tool-error id (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
      (file-unparseable-error (e)
        (tool-error id (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
      (multiple-top-level-forms-error ()
```
Note: the delete branch of `lisp-edit-form` returns three values only; `repair-fixes` and `repaired-form` bind to NIL there, and `%repair-summary` returns NIL for a NIL warning, so the delete summary is unchanged.

3d. Run `lisp-check-parens` on `src/lisp-edit-form.lisp`.

- [ ] **Step 4: Run tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-edit-form-test"}`.
Expected: all pass, including `lisp-edit-form-dry-run-summary-excludes-whole-file` (summary stays under 2000 chars: no warning, so nothing is appended) and `lisp-edit-form-old-protocol-error-returns-rpc-error` (not-found still goes through the generic `-32603` clause).

- [ ] **Step 5: Lint and commit**

```bash
mallet src/lisp-edit-form.lisp
git add src/lisp-edit-form.lisp tests/lisp-edit-form-test.lisp
git commit -m "feat(lisp-edit-form): show repaired form and changed lines; guided errors for broken files"
```

---

### Task 9: PR 2 verification

- [ ] **Step 1: Compile and full suite**

Via `repl-eval`: `(asdf:compile-system :cl-mcp :force t)` — no new warnings in `lisp-edit-form*.lisp`.
Bash: `rove cl-mcp.asd` — all green.

- [ ] **Step 2: Manual smoke test through MCP (after restarting the server)**

Call `lisp-edit-form` replace with content `"(defun x ()\n  (let ((y 1)\n    y))"` on a scratch file. Expected summary shows `WARNING: 1 closing delimiter added by parinfer`, `Changed lines:` with `line 2`, and `--- repaired form ---`. Then call it with content containing `]`: expected `isError` with the mismatch text and the file unchanged.

- [ ] **Step 3: Open PR 2**

```bash
git push -u origin feat/edit-form-repair-visibility
gh pr create --title "feat(lisp-edit-form): show parinfer repairs, refuse stray brackets, guide on broken files" --body "$(cat <<'EOF'
## Summary
- Success summaries now show the repaired form and the exact lines parinfer changed; "added" and "dropped" are reported separately (no more "-1 closing delimiters")
- Content that still holds `]`/`}` after repair is refused with the shared diagnosis; nothing is written
- Editing a file that does not parse returns the delimiter diagnosis, the next top-level form line, and the next tool to call, instead of "end of file on"
- Spec: docs/superpowers/specs/2026-09-03-paren-diagnostics-design.md (PR 2 of 3)

## Test plan
- [ ] `rove cl-mcp.asd` green
- [ ] `mallet src/lisp-edit-form.lisp src/lisp-edit-form-core.lisp` clean

🤖 Generated with [Claude Code](https://claude.com/claude-code)

https://claude.ai/code/session_01G6botZcYV9w4LDpqGg9sU3
EOF
)"
```

---

# PR 3: `lisp-patch-form` depth pre-check and diagnosis

Branch: `feat/patch-form-depth-diagnosis` off PR 2's branch.

### Task 10: Depth mismatch detected before reading the file

**Files:**
- Modify: `src/lisp-patch-form.lisp` (defpackage, new `%check-depth-balance`, `lisp-patch-form`)
- Modify: `tests/lisp-patch-form-test.lisp` (append tests)

**Interfaces:**
- Consumes: `count-delimiter-depth` (PR 1).
- Produces: `%check-depth-balance (old-text new-text)` signals `patch-operation-error` when `(open - close)` differs between the two texts. Called first thing in `lisp-patch-form` after argument type checks.

- [ ] **Step 1: Append failing tests**

Insert after `deftest lisp-patch-form-unrepairable-structure`:

```lisp
(deftest lisp-patch-form-depth-mismatch-fewer-closes
  (testing "new_text missing a ) is refused before the file is read"
    (with-temp-file "tests/tmp/patch-depth-fewer.lisp"
        (format nil "(defun target (x)~%  (if (> x 0)~%      (print x)~%      nil))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(print x)"
                               :new-text "(print x")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg)
          (ok (search "new_text closes 1 fewer \")\" than old_text" err-msg))
          (ok (search "(old_text: 1 open / 1 close, new_text: 1 open / 0 close)" err-msg))
          (ok (search "The patch would leave the form unclosed." err-msg))
          (ok (search "Add 1 \")\" to new_text, or remove 1 \"(\"." err-msg))
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))
```

```lisp
(deftest lisp-patch-form-depth-mismatch-more-closes
  (testing "new_text with an extra ) is refused with the opposite advice"
    (with-temp-file "tests/tmp/patch-depth-more.lisp"
        (format nil "(defun target (x)~%  (if (> x 0)~%      (print x)~%      nil))~%")
      (lambda (path)
        (let ((err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(print x)"
                               :new-text "(print x))")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok (search "new_text closes 1 more \")\" than old_text" err-msg))
          (ok (search "The patch would add an extra closing parenthesis." err-msg))
          (ok (search "Remove 1 \")\" from new_text, or add 1 \"(\"." err-msg)))))))
```

```lisp
(deftest lisp-patch-form-depth-check-ignores-strings-and-char-literals
  (testing "parens inside strings and #\\( do not trip the depth check"
    (with-temp-file "tests/tmp/patch-depth-strings.lisp"
        (format nil "(defun target ()~%  (list 1))~%")
      (lambda (path)
        (lisp-patch-form :file-path path
                         :form-type "defun"
                         :form-name "target"
                         :old-text "(list 1)"
                         :new-text "(list \")\" #\\( 1)")
        (ok (search "(list \")\" #\\( 1)" (fs-read-file path)))))))
```

```lisp
(deftest lisp-patch-form-depth-mismatch-does-not-need-a-readable-file
  (testing "the depth check fires even when the file path does not exist"
    (let ((err-msg nil))
      (handler-case
          (lisp-patch-form :file-path (project-path "tests/tmp/does-not-exist-xyzzy.lisp")
                           :form-type "defun"
                           :form-name "target"
                           :old-text "(a)"
                           :new-text "(a")
        (error (e) (setf err-msg (princ-to-string e))))
      (ok (search "new_text closes 1 fewer" err-msg)))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-patch-form-test"}`.
Expected: the 4 new tests fail (messages are the old "invalid Lisp: end of file" / file-not-found ones).

- [ ] **Step 3: Implement**

3a. Patch the `defpackage` of `cl-mcp/src/lisp-patch-form`. `old_text`:
```
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context)
```
`new_text`:
```
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:count-delimiter-depth
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
```
and `old_text`:
```
                #:%whitespace-char-p
                #:%locate-target-form)
```
`new_text`:
```
                #:%whitespace-char-p
                #:%locate-target-form
                #:file-unparseable-error)
```

3b. Insert before `defun %apply-patch-operation`:

```lisp
(defun %check-depth-balance (old-text new-text)
  "Signal PATCH-OPERATION-ERROR when NEW-TEXT opens/closes a different net
number of parentheses than OLD-TEXT. Since a patch changes only one region,
a net difference guarantees the form will not parse, so this is checked
before the file is read."
  (multiple-value-bind (old-open old-close) (count-delimiter-depth old-text)
    (multiple-value-bind (new-open new-close) (count-delimiter-depth new-text)
      (let ((diff (- (- new-open new-close) (- old-open old-close))))
        (unless (zerop diff)
          (let ((n (abs diff)))
            (error 'patch-operation-error
                   :reason
                   (if (plusp diff)
                       (format nil "new_text closes ~D fewer \")\" than old_text ~
                                    (old_text: ~D open / ~D close, new_text: ~D open / ~D close). ~
                                    The patch would leave the form unclosed. ~
                                    Add ~D \")\" to new_text, or remove ~D \"(\". ~
                                    No changes were written to disk."
                               n old-open old-close new-open new-close n n)
                       (format nil "new_text closes ~D more \")\" than old_text ~
                                    (old_text: ~D open / ~D close, new_text: ~D open / ~D close). ~
                                    The patch would add an extra closing parenthesis. ~
                                    Remove ~D \")\" from new_text, or add ~D \"(\". ~
                                    No changes were written to disk."
                               n old-open old-close new-open new-close n n)))))))))
```

3c. In `lisp-patch-form`, patch:
`old_text`:
```
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (multiple-value-bind (abs rel original nodes target target-snippet _ file-package-name)
```
`new_text`:
```
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (%check-depth-balance old-text new-text)
  (multiple-value-bind (abs rel original nodes target target-snippet _ file-package-name)
```

3d. Run `lisp-check-parens` on `src/lisp-patch-form.lisp`.

- [ ] **Step 4: Run tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-patch-form-test"}`.
Expected: all pass after updating two existing tests in this same task, because their inputs now stop at the depth check with the new wording:
- `lisp-patch-form-breaks-structure` (old `"(+ x 1))"` → new `"(+ x 1)"`, net -1 vs 0): change `(ok (search "invalid Lisp" err-msg))` to `(ok (or (search "invalid Lisp" err-msg) (search "fewer \")\"" err-msg)))`. Its `"No changes were written"` assertion still holds.
- `lisp-patch-form-unrepairable-structure` (old `"(defun target (x)"` → new `"completely broken ((( stuff"`, net +1 vs +3): add `(search "fewer \")\"" err-msg)` as a fourth alternative inside its `(or ...)`.
Use `lisp-patch-form` on the `deftest` forms for both edits.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/lisp-patch-form.lisp
git add src/lisp-patch-form.lisp tests/lisp-patch-form-test.lisp
git commit -m "feat(lisp-patch-form): refuse depth-mismatched patches before reading the file"
```

---

### Task 11: Diagnosis for nesting-only breakage and broken files

**Files:**
- Modify: `src/lisp-patch-form.lisp` (`%validate-form-parseable`, `define-tool "lisp-patch-form"` handler clauses)
- Modify: `tests/lisp-patch-form-test.lisp` (append tests)

**Interfaces:**
- Consumes: `diagnose-delimiters`, `format-delimiter-diagnosis`, `file-unparseable-error`.
- Produces: parse failures after a depth-balanced patch carry `"patch operation produced invalid Lisp. "` + diagnosis with target `"the patched form"` + `" No changes were written to disk."`; `file-unparseable-error` is returned through `tool-error`.

- [ ] **Step 1: Append failing tests**

Background for the fixture: a patch whose old/new net depth is equal usually still *parses* (only the meaning changes), so the reader path is reached only when the running depth dips below the form's own level. The fixture below does exactly that: `old_text` has net depth +1 (3 open, 2 close) and `new_text` also +1 (5 open, 4 close), but its leading `))` closes `defun` early, leaving `) (let ((y 1)) (( ...` as trailing content after the first form.

```lisp
(deftest lisp-patch-form-nesting-breakage-gets-diagnosis
  (testing "equal depth but an early ) yields trailing content and a diagnosis"
    (with-temp-file "tests/tmp/patch-nesting.lisp"
        (format nil "(defun target (x)~%  (let ((y 1))~%    (print y)~%    y))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(let ((y 1))"
                               :new-text ")) (let ((y 1)) ((")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg)
          (ok (search "invalid Lisp" err-msg))
          (ok (search "Unbalanced parentheses in the patched form" err-msg))
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))
```

```lisp
(deftest lisp-patch-form-broken-file-gives-guidance
  (testing "patching a file that does not parse returns the shared guidance"
    (with-temp-file "tests/tmp/patch-broken-file.lisp"
        (format nil "(defun a ()~%  (list 1)~%~%(defun b ()~%  2)~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "b"
                     "old_text" "2"
                     "new_text" "3")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "patch-broken-1" args))
                 (result-obj (gethash "result" response))
                 (text (gethash "text" (aref (gethash "content" result-obj) 0))))
            (ng (gethash "error" response))
            (ok (gethash "isError" result-obj))
            (ok (search "unclosed (form starting at line 1: \"(defun a ()\")" text))
            (ok (search "Next top-level form begins at line 4" text))
            (ok (search "Run lisp-check-parens with path=" text))))))))
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-patch-form-test"}`.
Expected: both new tests fail (old messages; broken file gives a generic error text without the guidance).

- [ ] **Step 3: Implement**

3a. Insert `%diagnosed-reason` with `lisp-edit-form` `insert_before` `defun %validate-form-parseable`, then `replace` `defun %validate-form-parseable`:

```lisp
(defun %diagnosed-reason (form-text fallback)
  "Return the patch failure reason for FORM-TEXT. When the delimiter scan
finds the breakage, the shared diagnosis is used; otherwise FALLBACK."
  (let ((diagnosis (diagnose-delimiters form-text)))
    (if (getf diagnosis :ok)
        fallback
        (format nil "patch operation produced invalid Lisp. ~A ~
                     Line numbers are within the patched form. ~
                     No changes were written to disk."
                (format-delimiter-diagnosis diagnosis :target "the patched form")))))

(defun %validate-form-parseable (form-text &optional readtable-designator
                                           package-name source-path)
  "Validate that FORM-TEXT parses as a single complete Lisp form.
Does NOT attempt parinfer repair. Signals PATCH-OPERATION-ERROR, carrying a
delimiter diagnosis when one applies, if the text does not parse correctly."
  (let* ((*read-eval* nil)
         (custom-rt (%resolve-named-readtable readtable-designator))
         (*readtable*
           (if custom-rt
               custom-rt
               (copy-readtable nil))))
    (handler-case
        (call-with-package-context
         package-name
         (lambda ()
           (multiple-value-bind (form pos)
               (read-from-string form-text nil :eof)
             (when (eq form :eof)
               (error 'patch-operation-error
                      :reason "patch produced an empty form"))
             (let ((rest-start (or (position-if-not #'%whitespace-char-p
                                                    form-text :start pos)
                                   (length form-text))))
               (when (< rest-start (length form-text))
                 (error 'patch-operation-error
                        :reason (%diagnosed-reason
                                 form-text
                                 "patch produced malformed form text (trailing content after form). No changes were written to disk."))))
             form-text))
         :source-path source-path)
      (patch-operation-error (e)
        (error e))
      (error (e)
        (error 'patch-operation-error
               :reason (%diagnosed-reason
                        form-text
                        (format nil "patch operation produced invalid Lisp: ~A. ~
                                     The form could not be parsed after replacement. ~
                                     No changes were written to disk."
                                e)))))))
```

3b. Add the handler clause in `define-tool "lisp-patch-form"`. `old_text`:
```
      (patch-operation-error (e)
        (tool-error id
```
`new_text`:
```
      (file-unparseable-error (e)
        (tool-error id
                    (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
      (patch-operation-error (e)
        (tool-error id
```

3c. Run `lisp-check-parens` on `src/lisp-patch-form.lisp`.

- [ ] **Step 4: Run tests to verify they pass**

Run `run-tests` with `{"system": "cl-mcp/tests/lisp-patch-form-test"}`.
Expected: all pass (the two existing tests touched in Task 10 need no further change).

- [ ] **Step 5: Lint and commit**

```bash
mallet src/lisp-patch-form.lisp
git add src/lisp-patch-form.lisp tests/lisp-patch-form-test.lisp
git commit -m "feat(lisp-patch-form): delimiter diagnosis on parse failure and broken-file guidance"
```

---

### Task 12: PR 3 verification

- [ ] **Step 1: Compile and full suite**

Via `repl-eval`: `(asdf:compile-system :cl-mcp :force t)`.
Bash: `rove cl-mcp.asd` — all green.

- [ ] **Step 2: Manual smoke test through MCP (after restarting the server)**

`lisp-patch-form` with `new_text` missing a `)`: expected `isError` text starting `new_text closes 1 fewer ")" than old_text`. `lisp-patch-form` on a file with an unclosed form: expected the `Run lisp-check-parens with path=` guidance.

- [ ] **Step 3: Open PR 3**

```bash
git push -u origin feat/patch-form-depth-diagnosis
gh pr create --title "feat(lisp-patch-form): depth mismatch pre-check and delimiter diagnosis" --body "$(cat <<'EOF'
## Summary
- old_text/new_text net paren depth is compared before the file is read; a mismatch says exactly how many ")" to add or remove
- Parse failures after a balanced patch carry the shared delimiter diagnosis (likely line, add/remove count)
- Patching a file that does not parse returns the same guidance as lisp-edit-form
- Spec: docs/superpowers/specs/2026-09-03-paren-diagnostics-design.md (PR 3 of 3)

## Test plan
- [ ] `rove cl-mcp.asd` green
- [ ] `mallet src/lisp-patch-form.lisp` clean

🤖 Generated with [Claude Code](https://claude.com/claude-code)

https://claude.ai/code/session_01G6botZcYV9w4LDpqGg9sU3
EOF
)"
```
