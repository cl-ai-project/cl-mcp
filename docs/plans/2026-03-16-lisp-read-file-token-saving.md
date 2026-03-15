# lisp-read-file Token Saving Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reduce token usage in `lisp-read-file` raw mode by lowering the default line limit and embedding truncation info visibly in the returned content.

**Architecture:** Two targeted changes to `src/lisp-read-file.lisp`: lower `*default-line-limit*` from 2000 to 500, and append a footer line to the content text when truncated in `collapsed=false` mode. No other files require changes except the test file.

**Tech Stack:** Common Lisp, Rove (test framework), cl-ppcre

---

### Task 1: Write failing tests

**Files:**
- Modify: `tests/lisp-read-file-test.lisp`

The test file already has a `lisp-read-file-raw-text-mode` test. Add four new tests after it.
Read the end of the file first to find a good insertion point:

```bash
# Find the last deftest in the file to insert after
grep -n "^(deftest" tests/lisp-read-file-test.lisp | tail -5
```

**Step 1: Add four failing tests using `lisp-edit-form` with `insert_after` on the last deftest**

Insert after the last `deftest` in `tests/lisp-read-file-test.lisp`:

```lisp
(deftest lisp-read-file-raw-truncated-footer
  (testing "appends footer when content is truncated in raw mode"
    ;; Create a temp file with 20 lines and read only 5
    (with-temp-lisp-file
     "tests/tmp/footer-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 20 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :limit 5))
              (content (gethash "content" result)))
         (ok (search "[Showing lines 1-5 of 20." content)
             "footer should show range and total")
         (ok (search "Use offset=5 to read more.]" content)
             "footer should include next offset"))))))

(deftest lisp-read-file-raw-truncated-footer-with-offset
  (testing "footer reflects correct range when offset is used"
    (with-temp-lisp-file
     "tests/tmp/footer-offset-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 30 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :offset 10 :limit 5))
              (content (gethash "content" result)))
         (ok (search "[Showing lines 11-15 of 30." content)
             "footer should show offset-adjusted range")
         (ok (search "Use offset=15 to read more.]" content)
             "footer should include correct next offset"))))))

(deftest lisp-read-file-raw-no-footer-when-complete
  (testing "no footer when entire file fits within limit"
    (with-temp-lisp-file
     "tests/tmp/no-footer-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 5 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :limit 100))
              (content (gethash "content" result)))
         (ok (not (search "[Showing lines" content))
             "no footer when not truncated"))))))

(deftest lisp-read-file-default-limit-is-500
  (testing "default limit is 500 lines (not 2000)"
    ;; Create a 600-line file and read with no explicit limit
    (with-temp-lisp-file
     "tests/tmp/default-limit-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 600 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil))
              (content (gethash "content" result)))
         (ok (search "[Showing lines 1-500 of 600." content)
             "default limit should be 500"))))))
```

**Step 2: Run tests to confirm they fail**

```bash
rove tests/lisp-read-file-test.lisp 2>&1 | grep -E "✓|✗|FAIL|PASS|footer|default-limit"
```

Expected: 4 failures (footer/limit features not yet implemented).

**Step 3: Commit the failing tests**

```bash
git add tests/lisp-read-file-test.lisp
git commit -m "test: add failing tests for raw-mode truncation footer and default limit"
```

---

### Task 2: Implement default limit change

**Files:**
- Modify: `src/lisp-read-file.lisp` (the `*default-line-limit*` defparameter)

**Step 1: Change the default limit**

Use `lisp-patch-form` on `src/lisp-read-file.lisp`:

```
form_type: defparameter
form_name: *default-line-limit*
old_text: 2000
new_text: 500
```

**Step 2: Run the default-limit test only**

```bash
rove tests/lisp-read-file-test.lisp 2>&1 | grep -A3 "default-limit"
```

Expected: `lisp-read-file-default-limit-is-500` now passes.

---

### Task 3: Implement truncation footer

**Files:**
- Modify: `src/lisp-read-file.lisp` (the `%lisp-read-file-content` function)

**Step 1: Read the current `(not collapsed)` branch to get exact text**

```
lisp-read-file path=src/lisp-read-file.lisp
               name_pattern="%lisp-read-file-content"
               collapsed=false
```

**Step 2: Patch the `(not collapsed)` branch using `lisp-patch-form`**

```
form_type: defun
form_name: %lisp-read-file-content
old_text:
     ((not collapsed)
      (multiple-value-bind (text truncated total)
          (%read-lines-slice resolved (or offset 0) line-limit)
        (let ((meta (make-hash-table :test #'equal)))
          (setf (gethash "truncated" meta) truncated
                (gethash "total_lines" meta) total)
          (values text meta "raw"))))

new_text:
     ((not collapsed)
      (multiple-value-bind (text truncated total)
          (%read-lines-slice resolved (or offset 0) line-limit)
        (let* ((meta (make-hash-table :test #'equal))
               (start-line (1+ (or offset 0)))
               (end-line (+ (or offset 0) line-limit))
               (footer (when truncated
                         (format nil "[Showing lines ~D-~D of ~D. ~
                                      Use offset=~D to read more.]~%"
                                 start-line end-line total end-line)))
               (content (if footer
                            (concatenate 'string text footer)
                            text)))
          (setf (gethash "truncated" meta) truncated
                (gethash "total_lines" meta) total)
          (values content meta "raw"))))
```

**Step 3: Run all four new tests**

```bash
rove tests/lisp-read-file-test.lisp 2>&1 | grep -E "footer|default-limit|✓|✗"
```

Expected: all 4 new tests pass.

**Step 4: Run the full test suite to check for regressions**

```bash
rove tests/lisp-read-file-test.lisp 2>&1 | tail -10
```

Expected: all tests pass.

**Step 5: Run mallet lint**

```bash
mallet src/lisp-read-file.lisp
```

Expected: `No problems found.`

**Step 6: Commit**

```bash
git add src/lisp-read-file.lisp
git commit -m "feat: embed truncation footer in lisp-read-file raw mode content

- Reduce *default-line-limit* from 2000 to 500
- Append [Showing lines X-Y of Z. Use offset=Y to read more.] when
  content is truncated in collapsed=false mode so LLMs see it directly"
```

---

### Task 4: Update tool description

**Files:**
- Modify: `src/lisp-read-file.lisp` (the `define-tool` block, `limit` arg description)

**Step 1: Find the exact description text**

```
clgrep-search pattern="defaults to 2000" path=src/lisp-read-file.lisp
```

If found, patch it:

```
form_type: define-tool (or search for the arg block)
old_text: "Maximum lines to return; defaults to 2000"
new_text: "Maximum lines to return; defaults to 500"
```

If the description uses a different format, read the `define-tool` block and locate the `limit` arg description manually.

**Step 2: Verify with mallet**

```bash
mallet src/lisp-read-file.lisp
```

**Step 3: Commit**

```bash
git add src/lisp-read-file.lisp
git commit -m "docs: update lisp-read-file limit description to reflect new default of 500"
```

---

### Task 5: Final verification

**Step 1: Run full lisp-read-file test suite**

```bash
rove tests/lisp-read-file-test.lisp 2>&1 | tail -5
```

Expected: all tests pass, 0 failures.

**Step 2: Smoke test via repl-eval (optional but recommended)**

```lisp
;; Load the updated system
(ql:quickload :cl-mcp)

;; Call lisp-read-file with collapsed=nil on a known large file
(let* ((result (cl-mcp/src/lisp-read-file:lisp-read-file
                "src/lisp-read-file.lisp"
                :collapsed nil))
       (content (gethash "content" result)))
  (format t "~&Last 200 chars:~%~A~%"
          (subseq content (max 0 (- (length content) 200)))))
```

Expected output ends with `[Showing lines 1-500 of NNN. Use offset=500 to read more.]`

**Step 3: Push**

```bash
git push
```
