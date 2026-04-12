# Dogfooding Cycle 11 — P2 Fixes Design

Date: 2026-04-12
Status: Approved
Scope: 2 P2 fixes from dogfooding cycle 11 (prio-queue project)

## Problem Statement

Two P2 issues were identified during dogfooding cycle 11:

1. **scaffold → worker ASDF registration gap**: `project-scaffold` auto-registers
   the `.asd` in the parent process, but `load-system` runs in the worker process
   which has no knowledge of the registration. Every dogfooding cycle (and any
   general CL project with `.asd` files under the project root) requires a manual
   `repl-eval (asdf:load-asd ...)` before `load-system` works.

2. **run-tests crash on failed-assertion test-name**: When running a single failing
   test via `run-tests test=pkg::name`, the structured result extractor calls
   `test-name` on a `FAILED-ASSERTION` object, which lacks that method, crashing
   the result extraction instead of reporting the failure.

## Fix 1: `load-system` auto-discovers .asd files under project root

### Approach

Add fallback logic to `load-system` (in `src/system-loader.lisp`) that, upon
`MISSING-COMPONENT` failure, searches the project root for a matching `.asd` file.

### Design

1. `load-system` attempts `(asdf:load-system name)` as usual.
2. On `ASDF/FIND-SYSTEM:MISSING-COMPONENT` (or equivalent condition):
   - Recursively search project root for `<system-name>.asd` using
     `(directory (merge-pathnames "**/<name>.asd" project-root))`.
   - If multiple matches, prefer the one closest to the project root (shallowest path).
   - Call `(asdf:load-asd found-path)` to register, then retry `load-system` once.
   - If no match found, return the original error.
3. On successful auto-discovery, append a hint to the response:
   `"Auto-registered <path> with ASDF"`.

### Constraints

- Retry at most once to prevent infinite loops.
- Wrap `.asd` search in `ignore-errors` for filesystem robustness.
- Skip `.git/` and fasl cache directories during search.
- General-purpose: works for any project with `.asd` files under the project root,
  not just scaffold-generated projects.

### Files changed

- `src/system-loader.lisp`: Add auto-discovery fallback in the load path.

## Fix 2: `run-tests` guards test-name extraction with handler-case

### Approach

Wrap the `test-name` function call in `handler-case` so that when the Rove
result object doesn't support `test-name`, extraction falls back gracefully.

### Design

1. In `%rove-extract-test-failures` (around line 145 of `src/test-runner-core.lisp`):
   - Wrap `(funcall test-name-fn test-fail)` in `handler-case`.
   - On error, fall back to:
     1. The assertion's description string (if available).
     2. `"<unknown test>"` as last resort.
2. The `failed_tests` response array continues to return structured data;
   only the `test_name` field uses the fallback value.

### Files changed

- `src/test-runner-core.lisp`: Add handler-case around test-name extraction (~5-10 lines).

## Testing

### Fix 1
- Existing `load-system` tests must not regress.
- Manual verification: `pool-kill-worker` → `load-system` on an experiments/
  project → should auto-discover and load.

### Fix 2
- Define a transient failing test via `repl-eval`, run via `run-tests test=...`,
  verify structured failure is returned without crash.
- Existing test suite regression check.

## Out of Scope

- P3 items (code-find-references setf, scaffold main-test stub, clgrep typo,
  clhs-lookup include_content) — deferred to a future cycle.
