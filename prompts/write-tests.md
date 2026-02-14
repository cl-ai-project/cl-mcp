# Write Tests with Rove
TDD workflow: write failing tests first, implement minimally, verify, refactor.

## Goal

Follow a strict TDD cycle using the Rove test framework: write a failing test (RED), make it pass with minimum code (GREEN), then improve design (REFACTOR). Every feature starts with a test.

## Tools You'll Use

| Tool | Purpose |
|------|---------|
| `run-tests` | Execute Rove test suites with structured results |
| `repl-eval` | Quick-check expressions, load systems |
| `lisp-read-file` | Read existing code to understand what to test |
| `lisp-edit-form` | Edit test and source files |
| `fs-write-file` | Create new test files |
| `lisp-check-parens` | Verify syntax before running |

## Workflow

### Step 1: RED — Write a Failing Test

Create or edit a test file. Test files live in `tests/` and mirror source naming:

```lisp
;; tests/core-test.lisp
(defpackage #:my-project/tests/core-test
  (:use #:cl #:rove))

(in-package #:my-project/tests/core-test)

(deftest my-feature-computes-correctly
  (testing "returns doubled value for positive integers"
    (ok (= (my-project:compute 5) 10)))
  (testing "returns zero for zero input"
    (ok (= (my-project:compute 0) 0))))
```

### Step 2: Run the Test (Confirm Failure)
```json
{"name": "run-tests", "arguments": {"system": "my-project/tests"}}
```
Expect `failed > 0`. The `failed_tests` array shows exactly which assertions failed and why.

### Step 3: GREEN — Implement Minimally

Write the simplest code that makes the test pass:
```json
{"name": "lisp-edit-form", "arguments": {
  "file_path": "src/core.lisp",
  "form_type": "defun",
  "form_name": "compute",
  "operation": "replace",
  "content": "(defun compute (n)\n  \"Return double of N.\"\n  (* n 2))"
}}
```

### Step 4: Run Tests Again (Confirm Pass)
```json
{"name": "run-tests", "arguments": {"system": "my-project/tests"}}
```
All tests should pass now. If not, iterate on the implementation.

### Step 5: REFACTOR — Improve Under Green

Refactor the implementation or tests while keeping the suite green. Re-run after each change.

## Rove Assertion Reference

| Macro | Purpose | Example |
|-------|---------|---------|
| `ok` | Assert truthy | `(ok (listp result))` |
| `ng` | Assert falsy | `(ng (find :bad items))` |
| `ok (equal a b)` | Assert equality | `(ok (equal result '(1 2 3)))` |
| `ok (= a b)` | Assert numeric equal | `(ok (= (compute 5) 10))` |
| `signals` | Assert condition raised | `(signals error (compute nil))` |
| `outputs` | Assert printed output | `(outputs "hello" (greet))` |
| `expands` | Assert macro expansion | `(expands '(expected) '(my-macro x))` |

## Test File Template

When creating a new test file:
1. Create minimal file with `fs-write-file`
2. Verify with `lisp-check-parens`
3. Register the test in the project's test system:
   - package-inferred style: add the test package import/entry point expected by the project
   - explicit `:components` style: add the test file to the test system in `.asd`
4. Expand with `lisp-edit-form`

```lisp
(defpackage #:my-project/tests/feature-test
  (:use #:cl #:rove))

(in-package #:my-project/tests/feature-test)

(deftest feature-basic-case
  (testing "description of expected behavior"
    (ok (expected-result-p (function-under-test input)))))
```

## Tips

- **Run single tests** during development to save time:
  ```json
  {"name": "run-tests", "arguments": {
    "system": "my-project/tests",
    "test": "my-project/tests/core-test::my-feature-computes-correctly"
  }}
  ```
- **Test error paths**: Use `signals` to verify condition handling.
- **Check `failed_tests`**: The structured output includes `form`, `reason`, and `source` for each failure.
- **Load before single-test**: The test package must be loaded first (run system-level tests or `ql:quickload`).
