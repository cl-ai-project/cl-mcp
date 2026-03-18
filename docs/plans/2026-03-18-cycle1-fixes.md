# Cycle 1 Major Issue Fixes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix 3 false-positive/wrong-position bugs in `%try-reader-check`, add file I/O error handling to the `lisp-check-parens` tool, and update the tool description.

**Architecture:** All changes are in `src/validate.lisp`. `%try-reader-check` gets three new handler clauses (`end-of-file`, `package-error`, in-readtable pre-check). The `lisp-check-parens` function omits nil line/column from the position hash. The `define-tool` body gains a `handler-case` wrapper. The tool description string is updated in-place.

**Tech Stack:** Common Lisp, SBCL, Rove (tests), `lisp-edit-form` / `lisp-patch-form` for all source edits, `lisp-check-parens` for validation, `rove cl-mcp.asd` for test runs.

---

## Context

**Source file:** `src/validate.lisp`
**Test files:** `tests/validate-test.lisp` (unit), `tests/tools-test.lisp` (protocol-level)

Root cause summary (from Cycle 1 comprehensive test):

| Bug | Location | Problem |
|-----|----------|---------|
| M1 | `%try-reader-check` | `end-of-file` is not a subtype of `reader-error` in SBCL → generic `error` handler fires → returns `base-offset` (0 or file start) instead of actual stream position |
| M2 | `%try-reader-check` | `package-error` (unloaded package) triggers generic `error` handler → returns `ok: false` on valid files |
| M3 | `%try-reader-check` | Standard CL reader has no custom readtables → `#?"..."` (cl-interpol) etc. trigger reader error on valid files |
| M6 | `lisp-check-parens` | When M1/M2's generic `error` fires, `reader-info` has `:line nil :column nil`; position hash is built with null values anyway |
| M5 | `define-tool` body | `(lisp-check-parens :path ...)` raises unhandled errors if file not found |
| M4 | `define-tool` description | AI clients don't know about `kind: "reader-error"` / `message` response fields |

---

## Task 1: Add unit tests for `%try-reader-check` bugs (TDD — write failing tests first)

**Files:**
- Modify: `tests/validate-test.lisp`

These tests call `lisp-check-parens` directly and will FAIL before the fixes.

**Step 1: Add 4 failing unit tests to `tests/validate-test.lisp`**

Use `lisp-edit-form` with `operation: "insert_after"` after the last deftest (`lisp-check-parens-too-large-returns-nil`).

```lisp
(deftest lisp-check-parens-eof-reader-error-has-position
  (testing "incomplete dispatch #X gives reader-error with non-nil position"
    ;; M1: end-of-file from incomplete #, should NOT report offset 0 / line nil
    (let ((res (lisp-check-parens :code "(valid-form) #")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (string= (%kind res) "reader-error") "kind should be reader-error")
      (ok (integerp (%pos res "line")) "line must be an integer, not nil")
      (ok (>= (%pos res "offset") 12) "offset must be past the valid form"))))

(deftest lisp-check-parens-package-error-no-false-positive
  (testing "package-qualified symbol for unloaded package is not a reader error"
    ;; M2: package-error on unloaded package should return ok: true
    (let ((res (lisp-check-parens :code "(nonexistent-package::my-sym arg)")))
      (ok (%ok? res) "valid file using unloaded package must return ok: true"))))

(deftest lisp-check-parens-in-readtable-no-false-positive
  (testing "file with in-readtable declaration is not falsely flagged"
    ;; M3: in-readtable present => skip reader check => ok: true
    (let ((res (lisp-check-parens
                :code "(named-readtables:in-readtable :interpol-syntax)
(defun greet (x) x)")))
      (ok (%ok? res) "file with in-readtable should return ok: true"))))

(deftest lisp-check-parens-eof-position-not-null
  (testing "position hash for EOF-type reader error has non-null line and column"
    ;; M6: position hash must not have nil line/column for EOF errors
    (let* ((res (lisp-check-parens :code "(foo) #"))
           (pos (gethash "position" res)))
      (ok (not (%ok? res)) "ok should be false")
      (ok (hash-table-p pos) "position must be a hash table")
      (ok (integerp (gethash "line" pos)) "position.line must be integer")
      (ok (integerp (gethash "column" pos)) "position.column must be integer"))))
```

**Step 2: Run tests to confirm they fail**

```bash
cd /home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp
rove tests/validate-test.lisp
```

Expected: 4 new tests FAIL (M1/M2/M3 bugs are present, M6 shows nil line/column).

**Step 3: Commit the failing tests**

```bash
git add tests/validate-test.lisp
git commit -m "test: add failing tests for %try-reader-check false-positive and EOF position bugs"
```

---

## Task 2: Fix `%try-reader-check` (M1 + M2 + M3 + M6)

**Files:**
- Modify: `src/validate.lisp`

All changes via `lisp-patch-form` or `lisp-edit-form` — never `Edit` or `fs-write-file` on .lisp files.

### Step 1: Add `%custom-readtable-p` helper before `%try-reader-check`

Use `lisp-edit-form` with `operation: "insert_before"` on `%try-reader-check`:

```lisp
(defun %custom-readtable-p (text)
  "Return T if TEXT contains a named-readtable activation.
When a custom readtable is active, the standard CL reader would produce
false-positive reader errors on valid custom syntax."
  (not (null (search "in-readtable" text))))
```

### Step 2: Replace the body of `%try-reader-check`

Use `lisp-edit-form` with `operation: "replace"` on `form_type: "defun"`, `form_name: "%try-reader-check"`.

New complete form:

```lisp
(defun %try-reader-check (text base-offset)
  "Attempt to fully read TEXT using the standard CL reader with *READ-EVAL* nil.
Returns a plist with reader error info if a genuine syntax error is detected,
or NIL if the text is clean (or if checking is skipped for known safe reasons).

Plist keys when non-nil: :KIND \"reader-error\", :MESSAGE string,
:OFFSET integer, :LINE integer, :COLUMN integer.

Skips the reader check (returns NIL) when TEXT contains \"in-readtable\",
because the standard CL reader does not know about custom readtables and would
produce false positives on valid custom syntax (e.g. cl-interpol #?\"...\").

Also returns NIL for package-not-found errors: a missing package is not a
syntax error in the file itself."
  ;; Skip reader check for files using custom readtables.
  (when (%custom-readtable-p text)
    (return-from %try-reader-check nil))
  (with-input-from-string (stream text)
    (handler-case
        (let ((*read-eval* nil))
          (loop (when (eq :eof (read stream nil :eof)) (return nil))))
      (reader-error (e)
        (let* ((pos       (or (ignore-errors (file-position stream)) 0))
               (safe-pos  (min pos (length text)))
               (pre       (subseq text 0 safe-pos))
               (line      (1+ (count #\Newline pre)))
               (nl-pos    (position #\Newline pre :from-end t))
               (col-start (or nl-pos -1))
               (col       (- safe-pos col-start)))
          (list :kind    "reader-error"
                :message (format nil "~A" e)
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (end-of-file (e)
        ;; end-of-file is NOT a subtype of reader-error in SBCL.
        ;; Capture stream position to give an accurate error location.
        (declare (ignore e))
        (let* ((pos      (or (ignore-errors (file-position stream)) (length text)))
               (safe-pos (min pos (length text)))
               (pre      (subseq text 0 safe-pos))
               (line     (1+ (count #\Newline pre)))
               (nl-pos   (position #\Newline pre :from-end t))
               (col      (- safe-pos (or nl-pos -1))))
          (list :kind    "reader-error"
                :message "unexpected end of file while reading"
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (package-error (e)
        ;; Package-not-found is not a syntax error in the file.
        ;; Return NIL to avoid false positives on valid files that reference
        ;; packages not loaded in the current image.
        (declare (ignore e))
        nil)
      (error (e)
        ;; Catch-all for unexpected non-reader errors.
        ;; Report without position since we have no reliable stream position.
        (list :kind    "reader-error"
              :message (format nil "~A" e)
              :offset  base-offset
              :line    nil
              :column  nil)))))
```

### Step 3: Fix position hash to omit null line/column (M6)

In `lisp-check-parens`, find the `reader-info` branch and use `lisp-patch-form` to update the position hash construction. The current code always sets line and column regardless of nil:

**Old text** (in `reader-info` branch):
```lisp
             (let ((pos (make-hash-table :test #'equal)))
               (setf (gethash "offset" pos) (getf reader-info :offset)
                     (gethash "line" pos) (getf reader-info :line)
                     (gethash "column" pos) (getf reader-info :column))
               (setf (gethash "position" h) pos)))
```

**New text**:
```lisp
             (let ((pos (make-hash-table :test #'equal))
                   (r-line (getf reader-info :line))
                   (r-col  (getf reader-info :column)))
               (setf (gethash "offset" pos) (getf reader-info :offset))
               (when r-line   (setf (gethash "line" pos) r-line))
               (when r-col    (setf (gethash "column" pos) r-col))
               (setf (gethash "position" h) pos)))
```

Use `lisp-patch-form` with `form_type: "defun"`, `form_name: "lisp-check-parens"`.

### Step 4: Run the 4 unit tests to confirm they pass

```bash
rove tests/validate-test.lisp
```

Expected: All tests pass, including the 4 new ones.

### Step 5: Run the full test suite

```bash
rove tests/validate-test.lisp tests/tools-test.lisp tests/core-test.lisp tests/protocol-test.lisp
```

Expected: All tests pass, no regressions.

### Step 6: Lint

```bash
mallet src/validate.lisp
```

Expected: No errors.

### Step 7: Commit

```bash
git add src/validate.lisp tests/validate-test.lisp
git commit -m "fix: correct %try-reader-check false positives and EOF position reporting

- Add %custom-readtable-p helper: skip reader check when in-readtable present
- Add end-of-file handler: capture accurate stream position for incomplete # etc.
- Add package-error handler: return nil (not a file syntax error)
- Fix position hash: omit nil line/column from reader-error position map"
```

---

## Task 3: Add `handler-case` to `define-tool` body (M5)

**Files:**
- Modify: `src/validate.lisp`

### Step 1: Write a failing test for file-not-found error handling

Add to `tests/tools-test.lisp` after `tools-call-lisp-check-parens-ok-no-reader-error`:

```lisp
(deftest tools-call-lisp-check-parens-file-not-found
  (testing "lisp-check-parens returns error result for nonexistent path, not crash"
    (let* ((req (concatenate 'string
                  "{\"jsonrpc\":\"2.0\",\"id\":5006,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"lisp-check-parens\","
                  "\"arguments\":{\"path\":\"/nonexistent/does-not-exist.lisp\"}}}"))
           (resp (%pjl req))
           (obj  (yason:parse resp)))
      ;; Should return a result with isError, not a JSON-RPC error
      (ok (hash-table-p (gethash "result" obj))
          "should return a result, not a JSON-RPC error")
      (ok (%tool-call-failed-p obj)
          "result should indicate failure"))))
```

Run:
```bash
rove tests/tools-test.lisp
```
Expected: `tools-call-lisp-check-parens-file-not-found` FAILS (unhandled error → JSON-RPC error, not `result`).

### Step 2: Wrap the define-tool body in `handler-case`

Use `lisp-edit-form` to replace the `define-tool "lisp-check-parens"` form. The `define-tool` macro takes keyword arguments; the `:body` value needs wrapping. Replace the entire `define-tool` call.

Find the current `:body` section. The body currently starts with `(progn ...)`. Wrap it:

**Old `:body` value:**
```lisp
  :body
  (progn
    (when (and path code)
      (error 'arg-validation-error ...))
    (when (and (null path) (null code))
      (error 'arg-validation-error ...))
    (let* ((check-result (lisp-check-parens :path path
                                            :code code
                                            :offset offset
                                            :limit limit))
           ...)
      ...
      (result id payload))))
```

Use `lisp-patch-form` with `form_type: "define-tool"`, `form_name: "lisp-check-parens"`.

**Old text** (first few lines of body):
```lisp
  (progn
    (when (and path code)
      (error 'arg-validation-error
             :arg-name "path/code"
             :message "Provide either path or code, not both"))
    (when (and (null path) (null code))
      (error 'arg-validation-error
             :arg-name "path/code"
             :message "Either path or code is required"))
    (let* ((check-result (lisp-check-parens :path path
```

**New text**:
```lisp
  (handler-case
    (progn
      (when (and path code)
        (error 'arg-validation-error
               :arg-name "path/code"
               :message "Provide either path or code, not both"))
      (when (and (null path) (null code))
        (error 'arg-validation-error
               :arg-name "path/code"
               :message "Either path or code is required"))
      (let* ((check-result (lisp-check-parens :path path
```

Also need to close the `handler-case`. Add at the very end of `:body`, after the final `(result id payload))))`:

**Old text** (end of body):
```lisp
        (result id payload)))))
```

**New text**:
```lisp
        (result id payload)))))
    (error (e)
      (result id (make-ht "content" (text-content (format nil "Error: ~A" e))
                          "isError" t))))
```

> **Note:** `lisp-patch-form` does NOT auto-repair parens. Count carefully. After editing, run `lisp-check-parens` on the file to verify balance.

### Step 3: Verify parens are balanced

```bash
# Via MCP tool or:
rove tests/validate-test.lisp
```

Or call `lisp-check-parens` on `src/validate.lisp` directly.

### Step 4: Run the failing test to confirm it passes

```bash
rove tests/tools-test.lisp
```

Expected: `tools-call-lisp-check-parens-file-not-found` passes.

### Step 5: Commit

```bash
git add src/validate.lisp tests/tools-test.lisp
git commit -m "fix: wrap lisp-check-parens define-tool body in handler-case for file I/O errors"
```

---

## Task 4: Update `lisp-check-parens` tool description (M4)

**Files:**
- Modify: `src/validate.lisp`

### Step 1: Update the `:description` string in `define-tool "lisp-check-parens"`

Use `lisp-patch-form` with `form_type: "define-tool"`, `form_name: "lisp-check-parens"`.

**Old text:**
```lisp
  :description "Check balanced parentheses/brackets in a file slice or provided code.
Use this to DIAGNOSE syntax errors in existing files or validate code snippets
before/after editing. Returns the first mismatch position if unbalanced, or
success if balanced. Unbalanced delimiter results include guidance to use
lisp-edit-form for existing Lisp files."
```

**New text:**
```lisp
  :description "Check balanced parentheses/brackets in a file slice or provided code.
Use this to DIAGNOSE syntax errors in existing files or validate code snippets
before/after editing. Returns the first mismatch position if unbalanced, or
success if balanced. Unbalanced delimiter results include guidance to use
lisp-edit-form for existing Lisp files.

Also detects reader errors (e.g. unknown dispatch characters, #. read-time eval
when *read-eval* is nil) even when parentheses are balanced. In that case the
result has kind: \"reader-error\" and a message field describing the error,
instead of expected/found fields. Files using named-readtables:in-readtable are
exempt from reader checking to avoid false positives."
```

### Step 2: Run full test suite to confirm no regressions

```bash
rove tests/validate-test.lisp tests/tools-test.lisp
```

Expected: All tests pass.

### Step 3: Lint

```bash
mallet src/validate.lisp
```

### Step 4: Commit

```bash
git add src/validate.lisp
git commit -m "docs: update lisp-check-parens tool description to mention reader error detection"
```

---

## Verification

After all tasks, run the full test suite:

```bash
rove tests/validate-test.lisp tests/tools-test.lisp tests/core-test.lisp tests/protocol-test.lisp tests/repl-test.lisp tests/fs-test.lisp tests/code-test.lisp tests/logging-test.lisp
```

All tests must pass.
