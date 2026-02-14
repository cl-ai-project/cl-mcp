# Refactor Common Lisp Code
Safely refactor across files: find all usages, plan changes, update systematically, verify with tests.

## Goal

Restructure existing code without changing behavior. Find every usage, plan the transformation, apply changes systematically, and verify correctness after each step. Never break what works.

## Tools You'll Use

| Tool | Purpose |
|------|---------|
| `clgrep-search` | Find definitions and usages project-wide |
| `code-find-references` | Trace symbol usage (requires loaded system) |
| `lisp-read-file` | Read current implementations |
| `lisp-edit-form` | Apply structural edits |
| `repl-eval` | Verify changes compile and behave correctly |
| `run-tests` | Confirm no regressions |

## Workflow

### Step 1: Identify All Usages
Before changing anything, find every place the target symbol appears:
```json
{"name": "clgrep-search", "arguments": {
  "pattern": "old-function-name",
  "limit": 50
}}
```
For loaded systems, use semantic references:
```json
{"name": "code-find-references", "arguments": {
  "symbol": "my-package:old-function-name",
  "project_only": true
}}
```

### Step 2: Read Current Implementations
Understand what you are changing and all callers:
```json
{"name": "lisp-read-file", "arguments": {
  "path": "src/core.lisp",
  "name_pattern": "^old-function-name$"
}}
```

### Step 3: Plan Changes
List every file and location that needs modification. Decide the order:
1. Update the definition first
2. Update callers file by file
3. Update exports if the public API changes
4. Update tests to match new behavior

### Step 4: Update the Definition
```json
{"name": "lisp-edit-form", "arguments": {
  "file_path": "src/core.lisp",
  "form_type": "defun",
  "form_name": "old-function-name",
  "operation": "replace",
  "content": "(defun new-function-name (x y)\n  \"Refactored: now takes two arguments.\"\n  (+ x y))"
}}
```

### Step 5: Update Each Caller
Apply `lisp-edit-form` to each calling site. Verify after each file:
```json
{"name": "repl-eval", "arguments": {
  "code": "(compile-file \"src/caller.lisp\")",
  "package": "CL-USER"
}}
```

### Step 6: Run Tests After Each File
```json
{"name": "run-tests", "arguments": {"system": "my-project/tests"}}
```
Fix any failures before moving to the next file.

### Step 7: Final Verification
Run the full test suite and compile the entire system:
```json
{"name": "repl-eval", "arguments": {
  "code": "(asdf:compile-system :my-project :force t)",
  "package": "CL-USER"
}}
```

## Common Refactoring Patterns

### Rename Symbol
1. `clgrep-search` to find all occurrences
2. Update `defun`/`defgeneric`/`defclass` definition
3. Update all callers
4. Update package `:export` list
5. Run tests

### Extract Function
1. Identify repeated code with `clgrep-search`
2. Write the new function (prototype in REPL first)
3. `insert_after` to add the new function
4. Replace each occurrence in callers
5. Run tests after each change

### Change Function Signature
1. `code-find-references` to find all call sites
2. Update the function definition
3. Update every caller to match new signature
4. Run tests after each file

## Tips

- **Always verify between steps** — catch problems early, not after 10 edits.
- **Tests are your safety net** — run them after every structural change.
- **Compile check** — `(compile-file ...)` catches undefined references faster than runtime.
- **Backward compatibility** — if others depend on the API, consider adding a wrapper that calls the new function.
- **Small commits** — commit working intermediate states so you can revert if needed.
