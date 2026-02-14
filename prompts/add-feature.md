# Add New Feature or Module
Create new packages, files, and functions following ASDF package-inferred-system conventions.

## Goal

Add a new feature to a Common Lisp project using the safe "minimal create then expand" pattern. The result is a properly integrated module with package, exports, tests, and ASDF registration.

## Tools You'll Use

| Tool | Purpose |
|------|---------|
| `lisp-read-file` | Understand existing project structure |
| `fs-list-directory` | Check directory layout |
| `fs-write-file` | Create new minimal files |
| `lisp-check-parens` | Verify syntax of new files |
| `lisp-edit-form` | Expand files safely, update .asd |
| `repl-eval` | Load and test new code |
| `run-tests` | Run test suite |

## Workflow

### Step 1: Understand Existing Structure
Read the `.asd` to understand the system layout and naming conventions:
```json
{"name": "lisp-read-file", "arguments": {"path": "my-project.asd", "collapsed": true}}
```
Check directory organization:
```json
{"name": "fs-list-directory", "arguments": {"path": "src/"}}
```

### Step 2: Create Minimal Source File
Start with a small, syntactically valid file. Include a stub function as an anchor for `lisp-edit-form`:
```json
{"name": "fs-write-file", "arguments": {
  "path": "src/new-module.lisp",
  "content": "(defpackage #:my-project/src/new-module\n  (:use #:cl)\n  (:export #:process-data))\n\n(in-package #:my-project/src/new-module)\n\n(defun process-data (input)\n  \"Process INPUT and return result.\"\n  input)\n"
}}
```

### Step 3: Verify Syntax
```json
{"name": "lisp-check-parens", "arguments": {"path": "src/new-module.lisp"}}
```

### Step 4: Expand with lisp-edit-form
Replace the stub or add more functions safely:
```json
{"name": "lisp-edit-form", "arguments": {
  "file_path": "src/new-module.lisp",
  "form_type": "defun",
  "form_name": "process-data",
  "operation": "replace",
  "content": "(defun process-data (input)\n  \"Process INPUT and return transformed result.\"\n  (transform input))"
}}
```
Add helper functions:
```json
{"name": "lisp-edit-form", "arguments": {
  "file_path": "src/new-module.lisp",
  "form_type": "defun",
  "form_name": "process-data",
  "operation": "insert_after",
  "content": "(defun transform (data)\n  \"Apply transformation to DATA.\"\n  (mapcar #'process-item data))"
}}
```

### Step 5: Update .asd Dependencies
Add the new module to the system definition using `lisp-edit-form` on the `.asd` file.

### Step 6: Load and Test Interactively
```json
{"name": "repl-eval", "arguments": {
  "code": "(ql:quickload :my-project)",
  "package": "CL-USER"
}}
```
Test the new function:
```json
{"name": "repl-eval", "arguments": {
  "code": "(my-project/src/new-module:process-data '(1 2 3))",
  "package": "CL-USER"
}}
```

### Step 7: Add Tests
Create a test file following the same minimal-then-expand pattern, add to `.asd` test dependencies, and run:
```json
{"name": "run-tests", "arguments": {"system": "my-project/tests"}}
```

## Package-Inferred-System Template

For projects using `package-inferred-system`, each file defines its own package:

```lisp
(defpackage #:my-project/src/module-name
  (:use #:cl)
  (:import-from #:my-project/src/other-module
    #:needed-function)
  (:export #:public-function))

(in-package #:my-project/src/module-name)
```

- Package name matches file path: `src/foo.lisp` → `my-project/src/foo`
- Use `:import-from` for explicit inter-module dependencies
- Export only the public API

## Tips

- **Never write large files with `fs-write-file`** — create minimal, then expand with `lisp-edit-form`.
- **`lisp-check-parens`** after `fs-write-file` catches syntax errors before they cause confusion.
- **Prototype in REPL** before persisting: `repl-eval` → iterate → `lisp-edit-form`.
- **One function at a time**: Add and test each function before writing the next.
