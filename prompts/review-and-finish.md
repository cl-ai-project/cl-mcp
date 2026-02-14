# Pre-Commit Review and Quality Check
Final quality gate: compile cleanly, pass all tests, verify style and documentation.

## Goal

Run a final quality check before committing code. Ensure zero compiler warnings, all tests pass, documentation is complete, and code follows the project style guide.

## Tools You'll Use

| Tool | Purpose |
|------|---------|
| `repl-eval` | Compile system, check warnings |
| `run-tests` | Run full test suite |
| `lisp-read-file` | Review code for style and docs |
| `lisp-check-parens` | Verify syntax in changed files |

## Workflow

### Step 1: Full System Compile
Force a complete recompile and check for warnings:
```json
{"name": "repl-eval", "arguments": {
  "code": "(asdf:compile-system :my-project :force t)",
  "package": "CL-USER"
}}
```
**Fix all compiler warnings.** Treat optimization notes and style-warnings as context-dependent, but ensure they are intentional.

### Step 2: Run All Tests
```json
{"name": "run-tests", "arguments": {"system": "my-project/tests"}}
```
All tests must pass. If any fail, fix before proceeding.

### Step 3: Review Changed Files
For each file you modified, check:
```json
{"name": "lisp-read-file", "arguments": {"path": "src/changed-file.lisp", "collapsed": true}}
```

**Documentation checklist:**
- [ ] All public `defun` / `defgeneric` / `defclass` / `defmacro` have docstrings
- [ ] Docstrings describe what, not how
- [ ] Package `:export` lists are up to date

**Style checklist (Google CL Style Guide):**
- [ ] 2-space indent, no tabs
- [ ] Lines ≤ 100 columns
- [ ] `lower-case-lisp-case` naming
- [ ] One blank line between top-level forms
- [ ] Predicates end with `-p` or `p`
- [ ] No `::` references (internal symbols)

### Step 4: Verify Syntax of Changed Files
```json
{"name": "lisp-check-parens", "arguments": {"path": "src/changed-file.lisp"}}
```

### Step 5: Smoke Test
Run a quick interactive verification of the main feature:
```json
{"name": "repl-eval", "arguments": {
  "code": "(my-project:main-entry-point sample-input)",
  "package": "CL-USER"
}}
```

## Quick Pre-Commit Checklist

```
[ ] asdf:compile-system :force t — zero warnings
[ ] run-tests — all green
[ ] Docstrings on all public APIs
[ ] Style guide compliance
[ ] lisp-check-parens on changed files
[ ] Smoke test passes
[ ] Build/test registration updated for project style (package-inferred imports or `.asd` components)
```
