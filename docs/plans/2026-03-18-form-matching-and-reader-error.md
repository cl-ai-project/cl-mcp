# Form Matching Fixes and Reader Error Detection Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix three bugs in `lisp-edit-form` form matching and add reader error detection to `lisp-check-parens`, all in one PR.

**Architecture:** Issue 1 and 3 are isolated changes to `%definition-candidates` and `%find-target` in `src/lisp-edit-form-core.lisp`. Issue 2 adds a new `%try-reader-check` helper to `src/validate.lisp` and threads its result through `lisp-check-parens`. Tests go in `tests/tools-test.lisp` using the existing `%pjl` / `with-test-project-root` / `unwind-protect` patterns.

**Tech Stack:** Common Lisp, SBCL, Rove (test framework), JSON-RPC protocol layer (`%pjl`), `tests/tmp/` for temp files.

---

### Task 1: Failing test — Issue 1 (`defstruct` with options)

**Files:**
- Modify: `tests/tools-test.lisp` (append after the last `deftest`)

**Step 1: Write the failing test**

Append this `deftest` to `tests/tools-test.lisp` (after the last `deftest` in the file):

```lisp
(deftest tools-call-lisp-edit-form-defstruct-with-options
  (testing "lisp-edit-form matches defstruct with (name options...) syntax"
    (with-test-project-root
      (let* ((tmp-path "tests/tmp/defstruct-options.lisp")
             (abs-path (merge-pathnames tmp-path
                                        cl-mcp/src/project-root:*project-root*))
             (initial "(in-package #:cl-user)

(defstruct (my-node (:print-object t))
  value
  children)
"))
        (with-open-file (out abs-path :direction :output :if-exists :supersede)
          (write-string initial out))
        (unwind-protect
             (let* ((req (format nil
                           (concatenate
                            'string
                            "{\"jsonrpc\":\"2.0\",\"id\":5001,\"method\":\"tools/call\","
                            "\"params\":{\"name\":\"lisp-edit-form\","
                            "\"arguments\":{\"file_path\":\"~A\","
                            "\"form_type\":\"defstruct\","
                            "\"form_name\":\"my-node\","
                            "\"operation\":\"replace\","
                            "\"dry_run\":true,"
                            "\"content\":\"(defstruct (my-node (:print-object t)) value children next)\"}}}") tmp-path))
                    (resp (%pjl req))
                    (obj (yason:parse resp))
                    (result (gethash "result" obj)))
               (ok (not (%tool-call-failed-p obj))
                   "defstruct with options syntax should be found")
               (ok (eql t (gethash "would_change" result))
                   "replacement content differs so would_change should be true"))
          (ignore-errors (delete-file abs-path)))))))
```

**Step 2: Run test to confirm FAIL**

```bash
cd /home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp
rove tests/tools-test.lisp 2>&1 | grep -A5 "defstruct-with-options"
```

Expected: FAIL — the tool returns an error because `my-node` is not found (candidate is `"(my-node (:print-object t))"` not `"my-node"`).

**Step 3: Implement the fix**

In `src/lisp-edit-form-core.lisp`, use `lisp-patch-form` to update `%definition-candidates`:

Change this block:
```lisp
      ((symbolp name)
       (list (%normalize-string name)))
      (t (list (%normalize-string name))))))
```

To:
```lisp
      ((symbolp name)
       (list (%normalize-string name)))
      ;; defstruct: (defstruct (name &rest options) ...) — first element is the name
      ((string= form-type "defstruct")
       (if (and (listp name) (symbolp (car name)))
           (list (%normalize-string (car name)))
           (list (%normalize-string name))))
      (t (list (%normalize-string name))))))
```

Use `lisp-patch-form` with:
- `form_type`: `"defun"`
- `form_name`: `"%definition-candidates"`
- `old_text`: the two lines being replaced (exact whitespace matters — copy from file)
- `new_text`: the new four lines above

**Step 4: Run test to confirm PASS**

```bash
rove tests/tools-test.lisp 2>&1 | grep -A3 "defstruct-with-options"
```

Expected: PASS

**Step 5: Run related existing tests (regression)**

```bash
rove tests/tools-test.lisp 2>&1 | grep -E "(PASS|FAIL|ERROR)" | head -20
```

Expected: no new failures in existing `lisp-edit-form` tests.

**Step 6: Commit**

```bash
git add src/lisp-edit-form-core.lisp tests/tools-test.lisp
git commit -m "fix: match defstruct with options syntax in lisp-edit-form"
```

---

### Task 2: Failing test — Issue 3 (`#:` prefix in `form_name`)

**Files:**
- Modify: `tests/tools-test.lisp` (append)

**Step 1: Write the failing test**

Append to `tests/tools-test.lisp`:

```lisp
(deftest tools-call-lisp-edit-form-defpackage-hash-colon-prefix
  (testing "lisp-edit-form form_name with #: prefix matches defpackage"
    (with-test-project-root
      (let* ((tmp-path "tests/tmp/defpkg-hash-colon.lisp")
             (abs-path (merge-pathnames tmp-path
                                        cl-mcp/src/project-root:*project-root*))
             (initial "(defpackage #:test-hash-colon-pkg
  (:use #:cl))

(in-package #:test-hash-colon-pkg)
"))
        (with-open-file (out abs-path :direction :output :if-exists :supersede)
          (write-string initial out))
        (unwind-protect
             (progn
               ;; #: prefix: should match
               (let* ((req (format nil
                              (concatenate
                               'string
                               "{\"jsonrpc\":\"2.0\",\"id\":5002,\"method\":\"tools/call\","
                               "\"params\":{\"name\":\"lisp-edit-form\","
                               "\"arguments\":{\"file_path\":\"~A\","
                               "\"form_type\":\"defpackage\","
                               "\"form_name\":\"#:test-hash-colon-pkg\","
                               "\"operation\":\"replace\","
                               "\"dry_run\":true,"
                               "\"content\":\"(defpackage #:test-hash-colon-pkg (:use #:cl) (:export #:hello))\"}}}") tmp-path))
                      (resp (%pjl req))
                      (obj (yason:parse resp))
                      (result (gethash "result" obj)))
                 (ok (not (%tool-call-failed-p obj))
                     "form_name with #: prefix should match defpackage")
                 (ok (eql t (gethash "would_change" result))))
               ;; bare name: must still work (regression)
               (let* ((req2 (format nil
                               (concatenate
                                'string
                                "{\"jsonrpc\":\"2.0\",\"id\":5003,\"method\":\"tools/call\","
                                "\"params\":{\"name\":\"lisp-edit-form\","
                                "\"arguments\":{\"file_path\":\"~A\","
                                "\"form_type\":\"defpackage\","
                                "\"form_name\":\"test-hash-colon-pkg\","
                                "\"operation\":\"replace\","
                                "\"dry_run\":true,"
                                "\"content\":\"(defpackage #:test-hash-colon-pkg (:use #:cl) (:export #:hello))\"}}}") tmp-path))
                      (resp2 (%pjl req2))
                      (obj2 (yason:parse resp2)))
                 (ok (not (%tool-call-failed-p obj2))
                     "bare form_name without prefix must still match")))
          (ignore-errors (delete-file abs-path)))))))
```

**Step 2: Run test to confirm FAIL**

```bash
rove tests/tools-test.lisp 2>&1 | grep -A5 "defpackage-hash-colon"
```

Expected: FAIL on the `#:` prefix case — form not found error.

**Step 3: Implement the fix**

In `src/lisp-edit-form-core.lisp`:

**3a.** Add `%strip-name-prefix` before `%find-target`. Use `lisp-edit-form` with `operation: "insert_before"`, targeting `%find-target`:

```lisp
(defun %strip-name-prefix (name)
  "Strip reader macro prefixes (#: : \"...\") from NAME for form-name matching.
Handles uninterned symbols (#:pkg), keywords (:pkg), and string literals (\"pkg\")."
  (cond
    ((and (>= (length name) 2) (string= (subseq name 0 2) "#:"))
     (subseq name 2))
    ((and (plusp (length name)) (char= (char name 0) #\:))
     (subseq name 1))
    ((and (>= (length name) 2)
          (char= (char name 0) #\")
          (char= (char name (1- (length name))) #\"))
     (subseq name 1 (1- (length name))))
    (t name)))
```

**3b.** In `%find-target`, use `lisp-patch-form` to change:
```lisp
    (let ((target (string-downcase base-name))
```
to:
```lisp
    (let ((target (string-downcase (%strip-name-prefix base-name)))
```

**3c.** Export `%strip-name-prefix` — add `#:%strip-name-prefix` to the `(:export ...)` list in the `defpackage` at the top of the file. Use `lisp-patch-form` targeting the `defpackage` form.

**Step 4: Run test to confirm PASS**

```bash
rove tests/tools-test.lisp 2>&1 | grep -A3 "defpackage-hash-colon"
```

Expected: both sub-assertions PASS.

**Step 5: Run regression check**

```bash
rove tests/tools-test.lisp 2>&1 | grep -c "PASS"
```

Count should be ≥ previous count.

**Step 6: Commit**

```bash
git add src/lisp-edit-form-core.lisp tests/tools-test.lisp
git commit -m "fix: strip #: : and string-quote prefixes from form_name in lisp-edit-form"
```

---

### Task 3: Failing test — Issue 2 (reader error detection)

**Files:**
- Modify: `tests/tools-test.lisp` (append)

**Step 1: Write the failing test**

`#.` (read-time eval) is disabled in `%try-reader-check` via `*read-eval* nil`, making it a reliable reader error trigger. The code has balanced parens so the paren scan will report OK — the only failure will be from the reader.

Append to `tests/tools-test.lisp`:

```lisp
(deftest tools-call-lisp-check-parens-reader-error
  (testing "lisp-check-parens detects reader error when parens are balanced"
    ;; #. triggers a reader-error when *read-eval* is nil.
    ;; The parens in (+ 1 #.(+ 1 2)) are balanced, so the paren scan passes.
    ;; The reader check should catch the #. and return ok: false, kind: "reader-error".
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":5004,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"lisp-check-parens\","
                 "\"arguments\":{\"code\":\"(defun foo () nil)\\n(+ 1 #.(+ 1 2))\"}}}")))
      (let* ((resp (%pjl req))
             (obj (yason:parse resp))
             (result (gethash "result" obj))
             (pos (gethash "position" result)))
        (ok (not (gethash "ok" result))
            "ok should be false for reader error")
        (ok (string= (gethash "kind" result) "reader-error")
            "kind should be reader-error")
        (ok (stringp (gethash "message" result))
            "message field should be a string describing the error")
        (ok (hash-table-p pos)
            "position should be a hash table")
        (ok (and pos (integerp (gethash "line" pos)))
            "position.line should be an integer")
        (ok (>= (gethash "line" pos) 2)
            "error should be on line 2 or later (second form)")))))
```

Also add a regression test to confirm valid code still returns `ok: true`:

```lisp
(deftest tools-call-lisp-check-parens-ok-no-reader-error
  (testing "lisp-check-parens still returns ok: true for valid Lisp with no reader errors"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":5005,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"lisp-check-parens\","
                 "\"arguments\":{\"code\":\"(defun foo (x) (* x 2))\"}}}")))
      (let* ((resp (%pjl req))
             (obj (yason:parse resp))
             (result (gethash "result" obj)))
        (ok (eql t (gethash "ok" result))
            "ok should be true for valid code")))))
```

**Step 2: Run tests to confirm FAIL**

```bash
rove tests/tools-test.lisp 2>&1 | grep -A5 "reader-error"
```

Expected: `tools-call-lisp-check-parens-reader-error` FAIL (currently returns `ok: true`). The regression test `tools-call-lisp-check-parens-ok-no-reader-error` will also be written but should PASS immediately after implementation.

---

### Task 4: Implement Issue 2 — `%try-reader-check` and `lisp-check-parens` update

**Files:**
- Modify: `src/validate.lisp`

**Step 1: Add `%try-reader-check` helper**

Use `lisp-edit-form` with `operation: "insert_before"`, targeting `lisp-check-parens`:

```lisp
(defun %try-reader-check (text base-offset)
  "Attempt to fully read TEXT using the standard CL reader with *READ-EVAL* nil.
Returns a plist with reader error info if any error is detected, or NIL if clean.
The plist has keys: :KIND \"reader-error\", :MESSAGE string, :OFFSET integer,
:LINE integer-or-nil, :COLUMN integer-or-nil."
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
      (error (e)
        ;; Non-reader-error (e.g. package does not exist): report without position
        (list :kind    "reader-error"
              :message (format nil "~A" e)
              :offset  base-offset
              :line    nil
              :column  nil)))))
```

**Step 2: Update `lisp-check-parens` function**

Replace the existing `lisp-check-parens` defun body to:
1. Always run the paren scan
2. Always run `%try-reader-check`
3. Paren error takes priority; reader error is primary only when parens are OK

Use `lisp-edit-form replace` on `lisp-check-parens`:

```lisp
(defun lisp-check-parens (&key path code offset limit)
  "Check balanced parentheses/brackets in CODE or PATH slice.
Also checks for reader errors (e.g. unknown dispatch characters, #. with
*read-eval* nil) even when parentheses are balanced.
Returns a hash table with key \"ok\" and, when not ok, \"kind\", and
either \"expected\"/\"found\" (delimiter mismatch) or \"message\" (reader error),
plus a \"position\" hash with \"line\", \"column\", \"offset\"."
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
    (let ((paren-result (%scan-parens text :base-offset base-off))
          (reader-info  (%try-reader-check text base-off)))
      (destructuring-bind (&key ok kind expected found
                                (offset base-off) (line 1) (column 1))
          paren-result
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
             (%maybe-add-lisp-edit-guidance h kind))
            (reader-info
             ;; Parens OK but reader error detected
             (setf (gethash "ok" h) nil
                   (gethash "kind" h) (getf reader-info :kind)
                   (gethash "message" h) (getf reader-info :message))
             (let ((pos (make-hash-table :test #'equal)))
               (setf (gethash "offset" pos) (getf reader-info :offset)
                     (gethash "line" pos) (getf reader-info :line)
                     (gethash "column" pos) (getf reader-info :column))
               (setf (gethash "position" h) pos)))
            (t
             ;; Both checks passed
             (setf (gethash "ok" h) t)))
          h)))))
```

**Step 3: Update the `define-tool` body summary for reader errors**

In the `define-tool "lisp-check-parens"` body, the `summary` string is built from `check-result`. Update the `(if ok ...)` branch to handle `kind: "reader-error"`:

Use `lisp-patch-form` on `define-tool` (note: `define-tool` is not a standard form — use `lisp-edit-form` targeting the `define-tool` body, or use `lisp-patch-form` to replace the summary generation block).

The summary block to replace:
```lisp
            (let* ((kind (gethash "kind" check-result))
                   (expected (gethash "expected" check-result))
                   (found (gethash "found" check-result))
                   (pos (gethash "position" check-result))
                   (line (and pos (gethash "line" pos)))
                   (col (and pos (gethash "column" pos))))
              (format nil
                      "Unbalanced parentheses: ~A~@[ (expected ~A, found ~A)~] at line ~D, column ~D~A"
                      kind expected found line col
                      (if next-tool
                          " Use lisp-edit-form for existing Lisp files."
                          ""))))))
```

Replace with:
```lisp
            (let* ((kind     (gethash "kind" check-result))
                   (message  (gethash "message" check-result))
                   (expected (gethash "expected" check-result))
                   (found    (gethash "found" check-result))
                   (pos      (gethash "position" check-result))
                   (line     (and pos (gethash "line" pos)))
                   (col      (and pos (gethash "column" pos))))
              (if (string= kind "reader-error")
                  (format nil "Reader error~@[ at line ~D, column ~D~]: ~A"
                          line col (or message "unknown"))
                  (format nil
                          "Unbalanced parentheses: ~A~@[ (expected ~A, found ~A)~] at line ~D, column ~D~A"
                          kind expected found line col
                          (if next-tool
                              " Use lisp-edit-form for existing Lisp files."
                              "")))))))
```

Also add `"message"` to the `payload` hash in the `define-tool` body:

Find the line `(make-ht "content" (text-content summary)` and add `"message"` to the payload. The payload currently is:
```lisp
(make-ht "content" (text-content summary)
         "ok" ok
         "kind" (gethash "kind" check-result)
         "expected" (gethash "expected" check-result)
         "found" (gethash "found" check-result)
         "position" (gethash "position" check-result))
```

Add `"message"` after `"found"`:
```lisp
(make-ht "content" (text-content summary)
         "ok" ok
         "kind" (gethash "kind" check-result)
         "expected" (gethash "expected" check-result)
         "found" (gethash "found" check-result)
         "message" (gethash "message" check-result)
         "position" (gethash "position" check-result))
```

Use `lisp-patch-form` for this small change.

**Step 4: Run tests**

```bash
rove tests/tools-test.lisp 2>&1 | grep -A5 "reader-error\|parens-ok"
```

Expected: both new tests PASS, existing `lisp-check-parens` tests still PASS.

**Step 5: Run lint**

```bash
mallet src/validate.lisp
```

Expected: no warnings.

**Step 6: Commit**

```bash
git add src/validate.lisp tests/tools-test.lisp
git commit -m "feat: detect reader errors in lisp-check-parens"
```

---

### Task 5: Documentation update

**Files:**
- Modify: `prompts/repl-driven-development.md`

**Step 1: Add note about `(setf name)` form_name syntax**

In the "Form not matched" troubleshooting section of `prompts/repl-driven-development.md`, add a bullet after the existing specializer note:

Find this text:
```
2. **Check specializers:** For methods, include the full lambda-list: `"form_name": "my-method ((s string))"`
```

Add after it:
```
3. **`(setf name)` functions:** Use the full list as the form name including parentheses: `"form_name": "(setf my-accessor)"`. The `#:` reader prefix is stripped automatically, so `"form_name": "#:my-package"` and `"form_name": "my-package"` are equivalent.
```

Use the `Edit` tool (not `lisp-edit-form` — this is a Markdown file).

**Step 2: Commit**

```bash
git add prompts/repl-driven-development.md
git commit -m "docs: document (setf name) form_name syntax and #: prefix handling"
```

---

### Task 6: Full test suite and PR

**Step 1: Run the full test suite**

```bash
rove cl-mcp.asd 2>&1 | tail -20
```

Expected: all tests pass with no new failures.

**Step 2: Run lint on changed files**

```bash
mallet src/lisp-edit-form-core.lisp src/validate.lisp
```

Expected: no warnings.

**Step 3: Open PR**

```bash
git push origin HEAD
gh pr create \
  --title "fix: defstruct matching, #: prefix normalization, reader error detection" \
  --body "$(cat <<'EOF'
## Summary

- **Issue 1**: `lisp-edit-form` now matches `defstruct` forms that use the options syntax `(defstruct (name opts...) ...)`. Previously only `(defstruct simple-name ...)` was found.
- **Issue 3**: `form_name` values with reader macro prefixes (`#:pkg`, `:pkg`, `"pkg"`) are now normalized before matching. `form_name: "#:my-package"` is equivalent to `form_name: "my-package"`.
- **Issue 2**: `lisp-check-parens` now also runs a reader check. Balanced-paren files with reader errors (e.g. `#.` dispatch, unknown dispatch characters) return `ok: false` with `kind: "reader-error"`, `message`, and `position.line`.

## Test plan

- [ ] `rove tests/tools-test.lisp` — all existing tests pass, 5 new tests pass
- [ ] `mallet src/lisp-edit-form-core.lisp src/validate.lisp` — no warnings
- [ ] `rove cl-mcp.asd` — full suite green

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```
