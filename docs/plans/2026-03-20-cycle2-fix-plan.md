# Cycle 2 Fix Plan — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix all 10 Major issues found in Cycle 2 comprehensive testing.

**Architecture:** Targeted fixes in protocol, worker handlers, clgrep, fs, repl-core, and inspect modules.

**Tech Stack:** Common Lisp, SBCL, bordeaux-threads, ASDF package-inferred-system

---

### Task 1: Fix race condition in `handle-initialize` project root setting

**Files:**
- Modify: `src/protocol.lisp:179` (handle-initialize root mutation)

**Step 1: Add lock around project root mutation**

In `handle-initialize`, wrap the `setf *project-root*` and `chdir` in `bt:with-lock-held`:

```lisp
;; Change lines 179-186 from:
(t (setf *project-root* root-dir) (uiop/os:chdir root-dir) ...)
;; To:
(t (bt:with-lock-held (*project-root-lock*)
     (setf *project-root* root-dir)
     (uiop/os:chdir root-dir)
     (setf *default-pathname-defaults* root-dir))
   ...)
```

Need to add imports for `*project-root-lock*` and `bt:with-lock-held` to the package.

**Step 2: Verify compilation**

Run: `mallet src/protocol.lisp`

**Step 3: Commit**

```
fix: acquire *project-root-lock* in handle-initialize for thread safety
```

---

### Task 2: Truncate raw JSON in parse error logs

**Files:**
- Modify: `src/protocol.lisp:309-311`

**Step 1: Truncate logged line**

Change the log call to truncate:

```lisp
;; From:
"line" trimmed
;; To:
"bytes" (length trimmed)
```

Remove the raw content entirely, log only byte count and error message.

**Step 2: Lint**

Run: `mallet src/protocol.lisp`

**Step 3: Commit**

```
security: remove raw JSON from parse error logs to prevent credential leakage
```

---

### Task 3: Fix `%sanitize-for-encoding` depth limit

**Files:**
- Modify: `src/protocol.lisp:72`

**Step 1: Sanitize leaf strings at depth limit**

```lisp
;; From:
(when (> depth 20) (return-from %sanitize-for-encoding obj))
;; To:
(when (> depth 20)
  (return-from %sanitize-for-encoding
    (typecase obj
      (string (sanitize-for-json obj))
      ((or number (member t nil)) obj)
      (t (or (ignore-errors (princ-to-string obj)) "#<depth-limit>")))))
```

**Step 2: Lint**

Run: `mallet src/protocol.lisp`

**Step 3: Commit**

```
fix: sanitize leaf values at depth limit in %sanitize-for-encoding
```

---

### Task 4: Handle `sb-ext:timeout` in `%handle-run-tests`

**Files:**
- Modify: `src/worker/handlers.lisp:138-142`

**Step 1: Wrap with-timeout in handler-case**

```lisp
;; From:
(let ((test-result (if timeout
                       (sb-ext:with-timeout timeout (do-run))
                       (do-run))))
;; To:
(let ((test-result (if timeout
                       (handler-case
                           (sb-ext:with-timeout timeout (do-run))
                         (sb-ext:timeout ()
                           (make-ht "error" t
                                    "message" (format nil "Tests timed out after ~A seconds" timeout))))
                       (do-run))))
```

**Step 2: Lint**

Run: `mallet src/worker/handlers.lisp`

**Step 3: Commit**

```
fix: catch sb-ext:timeout in %handle-run-tests for structured error response
```

---

### Task 5: Unify project root validation in worker handler

**Files:**
- Modify: `src/worker/handlers.lisp:218-219`

**Step 1: Move validation after truename resolution and expand blocklist**

```lisp
;; After the truename resolution (line 223-224), add:
(let ((root-str (namestring dir-path)))
  (when (member root-str '("/" "/tmp/" "/home/") :test #'string=)
    (error "Refusing to set project root to ~A -- too broad" root-str)))
```

Remove the old check at lines 218-219.

**Step 2: Lint**

Run: `mallet src/worker/handlers.lisp`

**Step 3: Commit**

```
security: unify worker project root validation with parent blocklist
```

---

### Task 6: Add thread destruction verification to timeout functions

**Files:**
- Modify: `src/repl-core.lisp:251`

**Step 1: Add post-destroy verification**

After `(ignore-errors (destroy-thread worker))`, add a brief poll:

```lisp
(ignore-errors (destroy-thread worker))
(loop repeat 20  ;; 1 second total
      while (thread-alive-p worker)
      do (sleep 0.05d0))
(when (thread-alive-p worker)
  (ignore-errors
   (log-event :warn "repl.timeout.thread-leaked"
              "name" "mcp-repl-eval")))
```

**Step 2: Lint**

Run: `mallet src/repl-core.lisp`

**Step 3: Commit**

```
fix: verify thread destruction after timeout in %repl-eval-with-timeout
```

---

### Task 7: Fix nested block comments in `scan-toplevel-forms`

**Files:**
- Modify: `src/utils/clgrep.lisp:63-120`

**Step 1: Replace binary state with depth counter**

Add `block-comment-depth` variable. On `#|`, increment depth and set state. On `|#`, decrement depth; only return to `:normal` when depth = 0.

**Step 2: Lint**

Run: `mallet src/utils/clgrep.lisp`

**Step 3: Run tests**

Run: `rove cl-mcp.asd`

**Step 4: Commit**

```
fix: support nested #| |# block comments in scan-toplevel-forms
```

---

### Task 8: Cap `%read-file-string` allocation

**Files:**
- Modify: `src/fs.lisp:62-63`

**Step 1: Apply max cap when limit not provided**

```lisp
;; From:
(let* ((len (or limit (ignore-errors (file-length in)) *fs-read-max-bytes*))
;; To:
(let* ((len (min (or limit (ignore-errors (file-length in)) *fs-read-max-bytes*)
                 *fs-read-max-bytes*))
```

**Step 2: Lint**

Run: `mallet src/fs.lisp`

**Step 3: Commit**

```
fix: cap %read-file-string allocation to *fs-read-max-bytes* regardless of limit
```

---

### Task 9: Add circular CDR detection in `%inspect-cons`

**Files:**
- Modify: `src/inspect.lisp:101-130`

**Step 1: Add seen-cdr hash table check**

```lisp
(let ((elements nil) (count 0) (truncated nil) (current object)
      (cdr-seen (make-hash-table :test #'eq)))
  (setf (gethash object cdr-seen) t)
  (loop while (consp current)
        do (if (>= count max-elements)
               (progn (setf truncated t) (return))
               (progn
                (push (%value-repr (car current) ...) elements)
                (incf count)
                (setf current (cdr current))
                (when (and (consp current) (gethash current cdr-seen))
                  (setf truncated t) (return))
                (when (consp current)
                  (setf (gethash current cdr-seen) t))))))
```

**Step 2: Lint**

Run: `mallet src/inspect.lisp`

**Step 3: Commit**

```
fix: detect circular CDR chains in %inspect-cons
```

---

### Task 10: Full test suite and lint

**Step 1: Run all tests**

```bash
rove cl-mcp.asd
```

**Step 2: Lint all changed files**

```bash
mallet src/protocol.lisp src/worker/handlers.lisp src/repl-core.lisp src/utils/clgrep.lisp src/fs.lisp src/inspect.lisp
```

**Step 3: Fix any failures**
