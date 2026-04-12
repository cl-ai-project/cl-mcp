# Dogfooding Cycle 11 P2 Fixes — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix two P2 issues: (1) load-system auto-discovers .asd files under project root, (2) run-tests guards test-name extraction against FAILED-ASSERTION objects.

**Architecture:** Fix 1 adds a retry-with-discovery layer in `system-loader-core.lisp` that catches MISSING-COMPONENT errors and searches the project root for matching `.asd` files. Fix 2 wraps all unguarded `test-name-fn` calls in `test-runner-core.lisp` with `handler-case`, matching the existing pattern in `%rove-extract-selected-failures`.

**Tech Stack:** Common Lisp, ASDF, Rove, SBCL

---

### Task 1: Add .asd auto-discovery helper to system-loader-core

**Files:**
- Modify: `src/system-loader-core.lisp`

**Step 1: Write the helper function**

Add a new function `%discover-asd-in-project` after the existing `%call-with-suppressed-output` (around line 158). This function searches the project root for a `.asd` file matching a given system name.

```lisp
(defun %discover-asd-in-project (system-name)
  "Search project root for <SYSTEM-NAME>.asd and register it with ASDF.
Return the pathname if found and registered, NIL otherwise."
  (let ((project-root (ignore-errors
                        (cl-mcp/src/fs-core:get-project-root))))
    (when project-root
      (let* ((pattern (make-pathname :directory '(:relative :wild-inferiors)
                                     :name system-name
                                     :type "asd"))
             (candidates (ignore-errors
                           (directory (merge-pathnames pattern project-root)))))
        (when candidates
          ;; Prefer the shallowest path (closest to project root)
          (let ((best (first (sort (copy-list candidates)
                                   #'< :key (lambda (p)
                                              (length (pathname-directory p)))))))
            (ignore-errors (asdf/find-system:load-asd best))
            (log-event :info "load-system-auto-discover"
                       "system" system-name
                       "asd" (namestring best))
            best))))))
```

Note: The function must import `cl-mcp/src/fs-core:get-project-root`. Add it to the `defpackage` `:import-from` list. Check the exact export name first:

```
clgrep-search pattern="get-project-root" path="src/"
```

**Step 2: Verify the helper compiles**

```
repl-eval code='(compile-file "src/system-loader-core.lisp" :output-file "/dev/null")'
```

Expected: compiles without errors.

### Task 2: Integrate auto-discovery into load-system

**Files:**
- Modify: `src/system-loader-core.lisp` — `load-system` function (line 166)

**Step 1: Add retry logic**

In the `load-system` function, the `errored-p` branch (line 208) handles load failures. Add auto-discovery retry when the error is a MISSING-COMPONENT condition. Modify the lambda inside `%load-with-timeout` (lines 181-192) to catch the missing-component error and retry after discovery:

```lisp
;; Replace the inner lambda (lines 181-192) with:
(lambda ()
  (when (and force (asdf/system:find-system system-name nil))
    (let ((asd-src
           (ignore-errors
            (asdf/system:system-source-file
             (asdf/system:find-system system-name nil)))))
      (asdf/system-registry:clear-system system-name)
      (when asd-src
        (ignore-errors (asdf/find-system:load-asd asd-src)))))
  (handler-case
      (%call-with-suppressed-output
       (lambda ()
         (asdf/operate:load-system system-name :force clear-fasls)))
    (asdf/find-system:missing-component (c)
      ;; Try auto-discovering .asd under project root
      (let ((found (%discover-asd-in-project system-name)))
        (unless found
          (error c))
        ;; Retry once after registration
        (%call-with-suppressed-output
         (lambda ()
           (asdf/operate:load-system system-name :force clear-fasls)))))))
```

**Step 2: Add auto-discovery hint to response**

In `src/tools/response-builders.lisp`, find the `build-load-system-response` function. Check if there is a way to pass through an "auto-discovered" hint. If the `load-system` result hash table includes an extra key, the response builder can include it in the text.

Alternative (simpler): After auto-discovery succeeds, set a key in the result hash-table:

In the `load-system` function's success branch (line 225, `t` case), the result `ht` is already built. Instead, add the auto-discovery info to the ht from the lambda return. The simplest approach: use a dynamic variable `*auto-discovered-asd*` bound in `load-system`, set inside `%discover-asd-in-project`, and checked after success.

```lisp
(defvar *auto-discovered-asd* nil
  "Set by %discover-asd-in-project when an .asd file is auto-registered.")
```

Set it in `%discover-asd-in-project` (before returning `best`):
```lisp
(setf *auto-discovered-asd* (namestring best))
```

Read it in `load-system`'s success branch (after line 238):
```lisp
(when *auto-discovered-asd*
  (setf (gethash "auto_discovered" ht) *auto-discovered-asd*))
```

And bind it with `let` at the top of `load-system`:
```lisp
(let ((*auto-discovered-asd* nil)
      (start-time (get-internal-real-time)))
  ...)
```

**Step 3: Update response builder**

In `src/tools/response-builders.lisp` — `build-load-system-response`: when `"auto_discovered"` key is present in the hash table, append a line to the response text:

```
"Auto-registered <path> with ASDF"
```

Check the exact function with:
```
lisp-read-file path=src/tools/response-builders.lisp name_pattern="^build-load-system-response$"
```

**Step 4: Verify compilation**

```
repl-eval code='(asdf:compile-system :cl-mcp :force t)'
```

Expected: compiles cleanly (0 warnings or only pre-existing warnings).

**Step 5: Commit**

```bash
git add src/system-loader-core.lisp src/tools/response-builders.lisp
git commit -m "feat: load-system auto-discovers .asd files under project root

When load-system fails with MISSING-COMPONENT, search the project root
for a matching .asd file, register it with ASDF, and retry once.
Eliminates the manual asdf:load-asd step after project-scaffold or
worker restart."
```

### Task 3: Manual verification of auto-discovery

**Step 1: Kill worker and test auto-discovery**

```
pool-kill-worker reset=true
repl-eval code='(asdf:load-asd "/home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp/experiments/prio-queue/prio-queue.asd")'
```

Wait — that defeats the purpose. Instead:

```
pool-kill-worker reset=true
load-system system=prio-queue
```

Expected: Should succeed with an "Auto-registered ... with ASDF" hint in the response, without any manual `asdf:load-asd`.

**Step 2: Test negative case**

```
load-system system=nonexistent-system-that-does-not-exist
```

Expected: Should fail with the normal "Component not found" error (no crash, no infinite loop).

### Task 4: Guard test-name-fn calls in %rove-extract-test-failures

**Files:**
- Modify: `src/test-runner-core.lisp` — `%rove-extract-test-failures` (line 124)

**Step 1: Add safe-test-name helper**

Add a local helper inside `%rove-extract-test-failures` (or as a top-level private function before it) that wraps `test-name-fn` safely. This follows the existing pattern in `%rove-extract-selected-failures` (line 414) which uses `typep` to check for `FAILED-ASSERTION`.

Add before `%rove-extract-test-failures` (around line 122):

```lisp
(defun %safe-test-name (test-name-fn node)
  "Call TEST-NAME-FN on NODE, falling back gracefully on error.
Some Rove result objects (e.g., FAILED-ASSERTION) lack a TEST-NAME method."
  (handler-case (funcall test-name-fn node)
    (error ()
      (let* ((pkg (find-package :rove/core/result))
             (desc-sym (find-symbol "ASSERTION-DESCRIPTION" pkg)))
        (or (and desc-sym
                 (ignore-errors (funcall (fdefinition desc-sym) node)))
            "<unknown test>")))))
```

**Step 2: Replace unguarded calls in %rove-extract-test-failures**

Replace all `(funcall test-name-fn ...)` with `(%safe-test-name test-name-fn ...)` in `%rove-extract-test-failures` (lines 137, 140, 153, 155):

Line 137: `(let ((test-name (funcall test-name-fn test-fail)))` → `(let ((test-name (%safe-test-name test-name-fn test-fail)))`

Line 140: `(funcall test-name-fn testing-fail)` → `(%safe-test-name test-name-fn testing-fail)`

Line 153: `(funcall test-name-fn test-fail)` → `(%safe-test-name test-name-fn test-fail)`

Line 155: `(funcall test-name-fn testing-fail)` → `(%safe-test-name test-name-fn testing-fail)`

**Step 3: Guard the same pattern in run-rove-tests**

In `run-rove-tests` (line 528), the `%extract-suites` local function at line 635 also calls `test-name-fn` without protection:

Line 635: `(let ((test-name (funcall test-name-fn test-fail))` → `(let ((test-name (%safe-test-name test-name-fn test-fail))`

Note: `%safe-test-name` is a top-level defun so it's accessible from the labels form.

**Step 4: Verify compilation**

```
repl-eval code='(asdf:compile-system :cl-mcp :force t)'
```

Expected: compiles cleanly.

**Step 5: Commit**

```bash
git add src/test-runner-core.lisp
git commit -m "fix: guard test-name extraction against FAILED-ASSERTION objects

Add %safe-test-name wrapper with handler-case fallback for Rove result
objects that lack a TEST-NAME method (e.g., FAILED-ASSERTION).
Applied to %rove-extract-test-failures and run-rove-tests %extract-suites,
matching the existing defensive pattern in %rove-extract-selected-failures."
```

### Task 5: Manual verification of test-name guard

**Step 1: Create and run a deliberately failing test**

```
repl-eval code='(rove:deftest prio-queue/tests/task-test::deliberate-fail-2 (rove:ok (= 1 2) "This should fail"))' package=CL-USER
```

Then:
```
run-tests system=prio-queue/tests/task-test test=prio-queue/tests/task-test::deliberate-fail-2
```

Expected: Should return structured failure with `test_name` field (possibly a fallback name), NOT crash with "no applicable method for TEST-NAME".

**Step 2: Verify normal failure reporting still works**

```
run-tests system=prio-queue/tests
```

Expected: 14 passed, 0 failed (the transient test is not persisted to disk).

### Task 6: Run full cl-mcp test suite for regression

**Step 1: Run cl-mcp's own tests**

```bash
cd /home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp && rove cl-mcp.asd
```

Expected: All tests pass. No regressions.

**Step 2: Final commit if any adjustments were needed**

Only if Step 1 revealed issues that required fixes.

### Task 7: Update dogfooding skill pitfalls table

**Files:**
- Modify: `.claude/skills/dogfooding-cl-mcp/SKILL.md`

**Step 1: Update the pitfalls table**

Update the entry for `load-system` after `project-scaffold`:

```markdown
| `load-system system=<name>` fails with `Component "<name>" not found` immediately after `project-scaffold` | Fixed: `load-system` now auto-discovers `.asd` files under the project root | Should work now; manual `asdf:load-asd` only needed if `.asd` is outside the project root |
```

And add a new resolved entry for the test-name crash:

```markdown
| `run-tests` single-test mode crashes with `no applicable method for TEST-NAME` on `FAILED-ASSERTION` | Fixed: `%safe-test-name` wrapper falls back to assertion description | Should work now; if it still crashes, file a new issue |
```

**Step 2: Commit**

```bash
git add .claude/skills/dogfooding-cl-mcp/SKILL.md
git commit -m "docs: update dogfooding skill pitfalls for cycle 11 fixes"
```
