# Cycle 2 Minor Fixes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix 6 minor issues found in Cycle 2 comprehensive testing.

**Architecture:** All changes are localized to `src/lisp-edit-form-core.lisp`,
`src/validate.lisp`, `src/lisp-edit-form.lisp`, `src/lisp-patch-form.lisp`,
`tests/tools-test.lisp`, and `tests/validate-test.lisp`. No new files needed.

**Tech Stack:** Common Lisp, Rove test framework, ASDF package-inferred-system.
Use `lisp-edit-form` / `lisp-patch-form` for ALL Lisp source edits (never raw
`Edit` tool on `.lisp` files).

---

## Task 1: Empty-string guard in `%find-target` (m-c2-1)

**Files:**
- Modify: `src/lisp-edit-form-core.lisp` — `%find-target` (line ~173)
- Test: `tests/tools-test.lisp` — new protocol-level test

**Context:** `%strip-name-prefix` on `"#:"` or `":"` returns `""`. That empty
string never matches any form, so `%find-target` returns nil and the caller
reports `"Form defun  not found"` (double space — the empty name). Fix: signal
a clear error when `target` is empty.

### Step 1: Write the failing test

Add after `tools-call-lisp-edit-form-defpackage-hash-colon-prefix` (line 1371
in `tests/tools-test.lisp`), using `lisp-edit-form` with `insert_after`:

```lisp
(deftest tools-call-lisp-edit-form-empty-form-name-after-prefix
  (testing "lisp-edit-form with form_name '#:' returns helpful error mentioning 'empty'"
    (with-test-project-root
      (let* ((tmp-path "tests/tmp/empty-form-name-test.lisp")
             (abs-path (merge-pathnames tmp-path
                                        cl-mcp/src/project-root:*project-root*))
             (initial "(defun my-fn () nil)\n"))
        (with-open-file (out abs-path :direction :output :if-exists :supersede)
          (write-string initial out))
        (unwind-protect
             (let* ((req (format nil
                            (concatenate
                             'string
                             "{\"jsonrpc\":\"2.0\",\"id\":5010,\"method\":\"tools/call\","
                             "\"params\":{\"name\":\"lisp-edit-form\","
                             "\"arguments\":{\"file_path\":\"~A\","
                             "\"form_type\":\"defun\","
                             "\"form_name\":\"#:\","
                             "\"operation\":\"replace\","
                             "\"dry_run\":true,"
                             "\"content\":\"(defun my-fn () :replaced)\"}}}") tmp-path))
                    (resp (%pjl req))
                    (obj (yason:parse resp))
                    (msg (%tool-call-message obj)))
               (ok (%tool-call-failed-p obj)
                   "form_name '#:' should produce an error")
               (ok (and (stringp msg) (search "empty" (string-downcase msg)))
                   "error message should mention 'empty'"))
          (ignore-errors (delete-file abs-path)))))))
```

### Step 2: Run test to verify it fails

```bash
rove tests/tools-test.lisp 2>&1 | grep -A5 "empty-form-name"
```

Expected: FAIL — currently produces "not found" error without "empty" in message.

### Step 3: Add empty-string guard in `%find-target`

In `src/lisp-edit-form-core.lisp`, the `%find-target` function body starts at
line ~173 with:

```lisp
(let ((target (string-downcase (%strip-name-prefix base-name)))
      (matches nil))
```

Use `lisp-patch-form` to insert the guard immediately after this `let` binding:

```
file_path: src/lisp-edit-form-core.lisp
form_type: defun
form_name: %find-target
old_text:     (let ((target (string-downcase (%strip-name-prefix base-name)))
          (matches nil))
      (loop for node in nodes
new_text:     (let ((target (string-downcase (%strip-name-prefix base-name)))
          (matches nil))
      (when (zerop (length target))
        (error "form_name resolved to empty string after prefix stripping; ~
provide a non-empty name (e.g. \"my-pkg\" instead of \"#:\" alone)"))
      (loop for node in nodes
```

### Step 4: Run test to verify it passes

```bash
rove tests/tools-test.lisp 2>&1 | grep -A5 "empty-form-name"
```

Expected: PASS.

### Step 5: Commit

```bash
git add tests/tools-test.lisp src/lisp-edit-form-core.lisp
git commit -m "fix: guard against empty form_name after prefix stripping in %find-target"
```

---

## Task 2: Update `form_name` descriptions (m-c2-2 + m-c2-3)

**Files:**
- Modify: `src/lisp-edit-form.lisp` — `form_name` `:description` in `define-tool`
- Modify: `src/lisp-patch-form.lisp` — `form_name` `:description` in `define-tool`

**Context:** Both descriptions say only "for defmethod include specializers".
They omit: (a) `defstruct` options syntax, (b) `#:` / `:` prefix stripping.

### Step 1: Find the exact current text in lisp-edit-form.lisp

Use `lisp-read-file` with `name_pattern="define-tool"` to see the `form_name`
description. The current text is:

```
"Form name to match; for defmethod include specializers,\ne.g., \"print-object ((obj my-class) stream)\""
```

### Step 2: Patch lisp-edit-form.lisp

Use `lisp-patch-form`:

```
file_path: src/lisp-edit-form.lisp
form_type: define-tool
form_name: lisp-edit-form
old_text: "Form name to match; for defmethod include specializers,\ne.g., \"print-object ((obj my-class) stream)\""
new_text: "Form name to match; for defmethod include specializers,\ne.g., \"print-object ((obj my-class) stream)\". For defstruct\nwith options \"(defstruct (name opts...) ...)\", use just the bare struct\nname. Reader macro prefixes #: and : are stripped automatically, so\n\"#:my-pkg\" and \"my-pkg\" both match \"(defpackage #:my-pkg ...)\"."
```

### Step 3: Patch lisp-patch-form.lisp

Find the current `form_name` description in `src/lisp-patch-form.lisp` (same
text as lisp-edit-form.lisp). Apply the same replacement using `lisp-patch-form`.

### Step 4: Verify no test regression

```bash
rove tests/tools-test.lisp 2>&1 | tail -5
```

Expected: all tests pass (description changes do not affect behaviour).

### Step 5: Commit

```bash
git add src/lisp-edit-form.lisp src/lisp-patch-form.lisp
git commit -m "docs: add defstruct options and #: prefix info to form_name descriptions"
```

---

## Task 3: Add `lisp-patch-form` `#:` prefix regression test (m-c2-4)

**Files:**
- Test: `tests/tools-test.lisp` — new test after the lisp-edit-form hash-colon test

**Context:** The shared `%locate-target-form` already handles `#:` for
`lisp-patch-form` correctly (same code path as `lisp-edit-form`). This test
prevents future regressions.

### Step 1: Write the test

Add after `tools-call-lisp-edit-form-defpackage-hash-colon-prefix` (but after
Task 1's test if that was inserted there). Use `lisp-edit-form` `insert_after`
to add:

```lisp
(deftest tools-call-lisp-patch-form-hash-colon-prefix
  (testing "lisp-patch-form form_name with #: prefix matches defpackage"
    (with-test-project-root
      (let* ((tmp-path "tests/tmp/patch-form-hash-colon.lisp")
             (abs-path (merge-pathnames tmp-path
                                        cl-mcp/src/project-root:*project-root*))
             (initial "(defpackage #:test-patch-pkg
  (:use #:cl))

(in-package #:test-patch-pkg)
"))
        (with-open-file (out abs-path :direction :output :if-exists :supersede)
          (write-string initial out))
        (unwind-protect
             (progn
               ;; #: prefix: should match
               (let* ((req (format nil
                              (concatenate
                               'string
                               "{\"jsonrpc\":\"2.0\",\"id\":5020,\"method\":\"tools/call\","
                               "\"params\":{\"name\":\"lisp-patch-form\","
                               "\"arguments\":{\"file_path\":\"~A\","
                               "\"form_type\":\"defpackage\","
                               "\"form_name\":\"#:test-patch-pkg\","
                               "\"dry_run\":true,"
                               "\"old_text\":\"(:use #:cl)\","
                               "\"new_text\":\"(:use #:cl) (:export #:hello)\"}}}") tmp-path))
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
                                "{\"jsonrpc\":\"2.0\",\"id\":5021,\"method\":\"tools/call\","
                                "\"params\":{\"name\":\"lisp-patch-form\","
                                "\"arguments\":{\"file_path\":\"~A\","
                                "\"form_type\":\"defpackage\","
                                "\"form_name\":\"test-patch-pkg\","
                                "\"dry_run\":true,"
                                "\"old_text\":\"(:use #:cl)\","
                                "\"new_text\":\"(:use #:cl) (:export #:hello)\"}}}") tmp-path))
                      (resp2 (%pjl req2))
                      (obj2 (yason:parse resp2)))
                 (ok (not (%tool-call-failed-p obj2))
                     "bare form_name without prefix must still match")))
          (ignore-errors (delete-file abs-path)))))))
```

### Step 2: Run test to verify it passes

```bash
rove tests/tools-test.lisp 2>&1 | grep -A5 "patch-form-hash"
```

Expected: PASS (behaviour already works; this is a regression guard).

### Step 3: Commit

```bash
git add tests/tools-test.lisp
git commit -m "test: add regression test for lisp-patch-form with #: prefix form_name"
```

---

## Task 4: Omit null keys from `lisp-check-parens` MCP response (m-c2-5)

**Files:**
- Modify: `src/validate.lisp` — define-tool body payload construction
- Test: `tests/tools-test.lisp` — two new protocol-level tests

**Context:** The `make-ht` in the define-tool body always includes
`"expected"`, `"found"`, and `"message"`, producing `null` values when not
applicable. Paren errors get `"message": null`; reader errors get
`"expected": null, "found": null`. These should be omitted.

### Step 1: Write failing tests

Add to `tests/tools-test.lisp`:

```lisp
(deftest tools-call-lisp-check-parens-paren-error-no-null-message
  (testing "paren error response omits 'message' key entirely"
    (let* ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":5030,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"lisp-check-parens\","
                   "\"arguments\":{\"code\":\"(+ 1 2))\"}}}"))
           (resp (%pjl req))
           (obj (yason:parse resp))
           (result (gethash "result" obj)))
      (ok (not (gethash "ok" result)) "should be error")
      (ok (string= (gethash "kind" result) "extra-close"))
      (multiple-value-bind (val presentp)
          (gethash "message" result)
        (declare (ignore val))
        (ok (not presentp) "'message' key must be absent for paren errors")))))

(deftest tools-call-lisp-check-parens-reader-error-no-null-expected-found
  (testing "reader error response omits 'expected' and 'found' keys entirely"
    (let* ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":5031,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"lisp-check-parens\","
                   "\"arguments\":{\"code\":\"(valid-form) #\"}}}"))
           (resp (%pjl req))
           (obj (yason:parse resp))
           (result (gethash "result" obj)))
      (ok (not (gethash "ok" result)) "should be error")
      (ok (string= (gethash "kind" result) "reader-error"))
      (multiple-value-bind (val presentp)
          (gethash "expected" result)
        (declare (ignore val))
        (ok (not presentp) "'expected' key must be absent for reader errors"))
      (multiple-value-bind (val presentp)
          (gethash "found" result)
        (declare (ignore val))
        (ok (not presentp) "'found' key must be absent for reader errors")))))
```

### Step 2: Run tests to verify they fail

```bash
rove tests/tools-test.lisp 2>&1 | grep -A5 "null-message\|null-expected"
```

Expected: FAIL (currently `"message": null` / `"expected": null` appear).

### Step 3: Fix the define-tool body in `src/validate.lisp`

Locate the `let ((payload (make-ht ...)))` block in the define-tool body.
Current code (exact text):

```lisp
          (let ((payload
                   (make-ht "content" (text-content summary)
                            "ok" ok
                            "kind" (gethash "kind" check-result)
                            "expected" (gethash "expected" check-result)
                            "found" (gethash "found" check-result)
                            "message" (gethash "message" check-result)
                            "position" (gethash "position" check-result)))
                 (fix-code (gethash "fix_code" check-result))
                 (required-args (gethash "required_args" check-result)))
```

Replace using `lisp-patch-form` (form_type: `define-tool`, form_name:
`lisp-check-parens`):

```
old_text:           (let ((payload
                   (make-ht "content" (text-content summary)
                            "ok" ok
                            "kind" (gethash "kind" check-result)
                            "expected" (gethash "expected" check-result)
                            "found" (gethash "found" check-result)
                            "message" (gethash "message" check-result)
                            "position" (gethash "position" check-result)))
                 (fix-code (gethash "fix_code" check-result))
                 (required-args (gethash "required_args" check-result)))

new_text:           (let* ((payload
                    (make-ht "content" (text-content summary)
                             "ok" ok
                             "kind" (gethash "kind" check-result)
                             "position" (gethash "position" check-result)))
                  (expected      (gethash "expected" check-result))
                  (found         (gethash "found" check-result))
                  (message       (gethash "message" check-result))
                  (fix-code      (gethash "fix_code" check-result))
                  (required-args (gethash "required_args" check-result)))
             (when expected (setf (gethash "expected" payload) expected))
             (when found    (setf (gethash "found" payload) found))
             (when message  (setf (gethash "message" payload) message))
```

IMPORTANT: The old `let` had 3 bindings and no conditional `setf` for
expected/found/message. The new `let*` has 5 bindings, and adds 3 conditional
`setf` lines. The remaining `(when fix-code ...)`, `(when next-tool ...)`,
`(when required-args ...)` lines that already exist must be kept unchanged.

### Step 4: Run tests to verify they pass

```bash
rove tests/validate-test.lisp tests/tools-test.lisp 2>&1 | tail -10
```

Expected: all 11 validate-test + all tools-test assertions pass.

### Step 5: Commit

```bash
git add src/validate.lisp tests/tools-test.lisp
git commit -m "fix: omit null keys from lisp-check-parens MCP response"
```

---

## Task 5: Detect unclosed block comments in `%scan-parens` (m-c2-6)

**Files:**
- Modify: `src/validate.lisp` — `scan-state` struct, `%scan-handle-normal`,
  `%scan-parens`
- Test: `tests/validate-test.lisp` — new unit test

**Context:** After the scan loop, `%scan-parens` checks unclosed parens (via
stack) but not unclosed `#|` block comments (via `block-depth`). An unclosed
`#|` currently falls to `%try-reader-check` which reports `kind:
"reader-error"`. This task makes the paren scanner itself detect it with a
more specific `kind: "unclosed-block-comment"`.

### Step 1: Write the failing unit test

Add to `tests/validate-test.lisp` (after the last existing test):

```lisp
(deftest lisp-check-parens-unclosed-block-comment
  (testing "unclosed block comment #| detected with kind unclosed-block-comment"
    (let ((res (lisp-check-parens :code "(foo) #| unclosed")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (string= (%kind res) "unclosed-block-comment")
          "kind should be unclosed-block-comment, not reader-error")
      (ok (integerp (%pos res "offset"))
          "position.offset should be present and integer"))))
```

### Step 2: Run test to verify it fails

```bash
rove tests/validate-test.lisp 2>&1 | grep -A5 "unclosed-block"
```

Expected: FAIL — currently returns `kind: "reader-error"` (caught by
`%try-reader-check`).

### Step 3: Add `block-open-pos` to `scan-state`

Use `lisp-patch-form`:

```
file_path: src/validate.lisp
form_type: defstruct
form_name: scan-state
old_text:   (block-depth 0 :type fixnum))
new_text:   (block-depth 0 :type fixnum)
  (block-open-pos 0 :type fixnum))
```

### Step 4: Record `block-open-pos` in `%scan-handle-normal`

The current block-comment branch in `%scan-handle-normal` is:

```lisp
   ((and (char= ch #\#) next (char= next #\|))
    (incf (scan-state-block-depth state)) (values nil 1))
```

Replace using `lisp-patch-form`:

```
file_path: src/validate.lisp
form_type: defun
form_name: %scan-handle-normal
old_text:    ((and (char= ch #\#) next (char= next #\|))
    (incf (scan-state-block-depth state)) (values nil 1))
new_text:    ((and (char= ch #\#) next (char= next #\|))
    (when (zerop (scan-state-block-depth state))
      (setf (scan-state-block-open-pos state) (+ base-offset idx)))
    (incf (scan-state-block-depth state)) (values nil 1))
```

### Step 5: Add post-loop check in `%scan-parens`

The current tail of `%scan-parens` (after the loop) is:

```lisp
    (when (scan-state-stack state)
      (destructuring-bind
          (ch l c off)
          (pop (scan-state-stack state))
        (return-from %scan-parens
          (list :ok nil :kind "unclosed" :expected (string (%closing ch))
                :found nil :offset off :line l :column c))))
    (list :ok t)))
```

Use `lisp-patch-form` to insert a block-depth check between the stack check
and the final `(list :ok t)`:

```
file_path: src/validate.lisp
form_type: defun
form_name: %scan-parens
old_text:     (list :ok t)))
new_text:     (when (plusp (scan-state-block-depth state))
      (let* ((off      (scan-state-block-open-pos state))
             (local-off (- off base-offset))
             (pre      (subseq text 0 (min local-off (length text))))
             (line     (1+ (count #\Newline pre)))
             (nl-pos   (position #\Newline pre :from-end t))
             (col      (- local-off (or nl-pos -1))))
        (return-from %scan-parens
          (list :ok nil
                :kind "unclosed-block-comment"
                :expected "|#"
                :found nil
                :offset off
                :line line
                :column col))))
    (list :ok t)))
```

### Step 6: Run tests to verify they pass

```bash
rove tests/validate-test.lisp 2>&1 | tail -10
```

Expected: all 12 tests pass (11 original + 1 new).

```bash
rove tests/tools-test.lisp 2>&1 | tail -5
```

Expected: all tools-test assertions pass.

### Step 7: Commit

```bash
git add src/validate.lisp tests/validate-test.lisp
git commit -m "feat: detect unclosed block comments #| in %scan-parens"
```

---

## Task 6: Truncate verbose SBCL stream repr in reader error messages (m-c2-7)

**Files:**
- Modify: `src/validate.lisp` — `%try-reader-check`, both `reader-error` and
  catch-all `error` handlers
- Test: `tests/validate-test.lisp` — new unit test

**Context:** `(format nil "~A" e)` on SBCL reader-error conditions produces a
message that includes the full stream representation with the input text, e.g.:
`"no dispatch character for #\@ on #<STRING-INPUT-STREAM (unavailable) from
\"(the full input...)\">"`
This echoes private input back in diagnostics. Truncating at 200 chars prevents
the worst case while preserving useful information.

### Step 1: Write the failing test

Add to `tests/validate-test.lisp`:

```lisp
(deftest lisp-check-parens-reader-error-message-truncated
  (testing "reader error message is truncated to at most 200 chars"
    ;; Build input that will trigger a reader error late in a large string.
    ;; The SBCL stream repr echoes the full input, so a long input makes this
    ;; visible.
    (let* ((padding (make-string 150 :initial-element #\a))
           (code (concatenate 'string "(valid) " padding " #@"))
           (res (lisp-check-parens :code code)))
      (ok (not (%ok? res)) "should be error")
      (ok (string= (%kind res) "reader-error"))
      (let ((msg (gethash "message" res)))
        (ok (and (stringp msg) (<= (length msg) 200))
            "message must not exceed 200 chars")))))
```

### Step 2: Run test to verify it fails

```bash
rove tests/validate-test.lisp 2>&1 | grep -A5 "message-truncated"
```

Expected: FAIL if SBCL echoes the long input in the condition message (message
exceeds 200 chars). If the test passes already (SBCL truncates internally),
that is also acceptable — skip this task.

### Step 3: Truncate message in reader-error handler

The `reader-error` handler in `%try-reader-check` currently has:

```lisp
                :message (format nil "~A" e)
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (end-of-file (e)
```

Use `lisp-patch-form`:

```
file_path: src/validate.lisp
form_type: defun
form_name: %try-reader-check
old_text:                 :message (format nil "~A" e)
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (end-of-file (e)
new_text:                 :message (let ((m (format nil "~A" e)))
                            (if (> (length m) 200) (subseq m 0 200) m))
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (end-of-file (e)
```

### Step 4: Truncate message in catch-all error handler

The catch-all `error` handler currently has:

```lisp
        (list :kind    "reader-error"
              :message (format nil "~A" e)
              :offset  base-offset
              :line    nil
              :column  nil)))))
```

Use `lisp-patch-form`:

```
file_path: src/validate.lisp
form_type: defun
form_name: %try-reader-check
old_text:         (list :kind    "reader-error"
              :message (format nil "~A" e)
              :offset  base-offset
              :line    nil
              :column  nil)))))
new_text:         (list :kind    "reader-error"
              :message (let ((m (format nil "~A" e)))
                         (if (> (length m) 200) (subseq m 0 200) m))
              :offset  base-offset
              :line    nil
              :column  nil)))))
```

### Step 5: Run all validate-test and tools-test

```bash
rove tests/validate-test.lisp 2>&1 | tail -10
rove tests/tools-test.lisp 2>&1 | tail -5
```

Expected: all tests pass.

### Step 6: Commit

```bash
git add src/validate.lisp tests/validate-test.lisp
git commit -m "fix: truncate reader error messages to max 200 chars"
```

---

## Final: Full test suite verification

```bash
rove tests/validate-test.lisp tests/tools-test.lisp 2>&1 | tail -20
```

Expected: 0 failures. The combined suite has ~13 validate-tests and ~285+
tools-test assertions.
