# Cycle 3 Fix Plan — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the 6 highest-impact Major issues found in Cycle 3 comprehensive testing.

**Architecture:** Targeted fixes in protocol, worker handlers, test-runner, and tcp modules.

**Tech Stack:** Common Lisp, SBCL, bordeaux-threads, ASDF package-inferred-system

---

### Task 1: Fix `%sanitize-for-encoding` cons branch for dotted pairs and circular lists

**Files:**
- Modify: `src/protocol.lisp` — `%sanitize-for-encoding`, cons branch

**Step 1: Replace loop-for-in with manual CDR-walk**

Replace the cons branch from:
```lisp
(cons (loop for elt in obj for i below +sanitize-max-elements+ collect ...))
```
To a manual traversal that:
1. Walks CDR chain manually (handles dotted tails)
2. Uses eq hash-table for circular CDR detection
3. Stops at +sanitize-max-elements+

**Step 2: Lint**

Run: `mallet src/protocol.lisp`

**Step 3: Commit**

```
fix: handle dotted pairs and circular lists in %sanitize-for-encoding
```

---

### Task 2: Fix non-standard JSON-RPC id type handling

**Files:**
- Modify: `src/protocol.lisp` — `process-json-line` error handler

**Step 1: Coerce id before error response**

In the handler-case error clause, coerce id before calling rpc-error:
```lisp
(let ((safe-id (typecase id
                 ((or null string integer) id)
                 (t nil))))
  (%encode-json (rpc-error safe-id -32603 "Internal error")))
```

**Step 2: Lint**

Run: `mallet src/protocol.lisp`

**Step 3: Commit**

```
fix: coerce non-standard JSON-RPC id types to nil in error handler
```

---

### Task 3: Fix run-tests timeout response to indicate failure

**Files:**
- Modify: `src/worker/handlers.lisp` — `%handle-run-tests` timeout handler

**Step 1: Build timeout result with proper keys**

Change the timeout handler from:
```lisp
(make-ht "error" t "message" ...)
```
To:
```lisp
(make-ht "passed" 0
         "failed" 1
         "framework" "timeout"
         "duration_ms" (* timeout 1000)
         "failed_tests" (vector (make-ht "test_name" "TIMEOUT"
                                          "reason" (format nil "Tests timed out after ~A seconds" timeout))))
```

**Step 2: Lint**

Run: `mallet src/worker/handlers.lisp`

**Step 3: Commit**

```
fix: run-tests timeout returns structured failure instead of misleading PASS
```

---

### Task 4: Add timeout handler to inline run-tests path

**Files:**
- Modify: `src/test-runner.lisp` — define-tool "run-tests" body

**Step 1: Wrap sb-ext:with-timeout in handler-case**

Mirror the worker handler fix for the inline path.

**Step 2: Lint**

Run: `mallet src/test-runner.lisp`

**Step 3: Commit**

```
fix: add sb-ext:timeout handler to inline run-tests path
```

---

### Task 5: Fix stop-tcp-server-thread join hang

**Files:**
- Modify: `src/tcp.lisp` — `stop-tcp-server-thread`

**Step 1: Wrap join-thread with handler-case**

Replace bare join-thread with:
```lisp
(handler-case (bordeaux-threads:join-thread *tcp-server-thread*)
  (error () nil))
```

**Step 2: Lint**

Run: `mallet src/tcp.lisp`

**Step 3: Commit**

```
fix: prevent stop-tcp-server-thread from hanging on join-thread
```

---

### Task 6: Full test suite and lint

**Step 1: Run all tests**

```bash
rove cl-mcp.asd
```

**Step 2: Lint all changed files**

```bash
mallet src/protocol.lisp src/worker/handlers.lisp src/test-runner.lisp src/tcp.lisp
```

**Step 3: Fix any failures**
