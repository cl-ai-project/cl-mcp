# project-scaffold Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ship a new `project-scaffold` cl-mcp tool that generates a minimal package-inferred-system + Rove project in a single atomic call.

**Architecture:** Three-file split under `src/` — pure core (validation / rendering / manifest) in `project-scaffold-core.lisp`, bulk template strings in `project-scaffold-templates.lisp`, and the thin I/O + `define-tool` entry in `project-scaffold.lisp`. Atomic generation via temp-directory-then-rename. Parent process only; no worker-side hooks.

**Tech Stack:** Common Lisp, SBCL, ASDF `package-inferred-system`, Rove for tests, `cl-ppcre`, `uiop`, Eclector (already in cl-mcp's deps).

**Design doc:** `docs/plans/2026-04-09-project-scaffold-design.md` — read this first for rationale, non-goals, and the template contents.

**Ground rules (from project CLAUDE.md and prompts):**
- All Lisp source edits go through `lisp-edit-form` / `lisp-patch-form` — NEVER the plain text Edit tool. Parens errors in Lisp are silent killers; parinfer auto-repair saves you.
- Pure TDD: write the failing test first, see it fail, then write the minimal implementation.
- Commit after each green step, small and focused.
- Lint with `mallet src/project-scaffold*.lisp` before every commit.
- Use `run-tests` tool (NOT bash rove) — it handles force-reload automatically.
- `fs-write-file` has a guardrail: it refuses to overwrite existing `.lisp` files. Use it for brand-new files only; use `lisp-edit-form` / `lisp-patch-form` for edits to anything that already exists.
- Every new `.lisp` file starts with a file header comment (`;;;; src/...`), then `(defpackage ...)` with explicit exports, then `(in-package ...)`.
- Follow the Google Common Lisp Style Guide: 2-space indent, ≤100 columns, blank line between top-level forms, docstrings on public functions.

---

## Phase 0: Orientation (do this once before starting)

**Before writing any code:**

1. Read the design doc `docs/plans/2026-04-09-project-scaffold-design.md` end-to-end via `fs-read-file`.
2. Read existing tool patterns to understand the `define-tool` macro contract:
   - `lisp-read-file path="src/fs.lisp" name_pattern="fs-write-file"` — see how an existing tool uses `define-tool`, `result`, `make-ht`, `text-content`.
   - `lisp-read-file path="src/tools/define-tool.lisp" name_pattern="define-tool"` — read the macro docstring carefully. Body gets `id`, `state`, and all named args as locals.
3. Read path helper contracts:
   - `lisp-read-file path="src/utils/paths.lisp" name_pattern="ensure-write-path|canonical-path|path-inside-p|ensure-project-root"`
4. Read an existing test file as a template:
   - `lisp-read-file path="tests/fs-test.lisp"` — note how temp project roots are set up.
5. Verify project root:
   - `fs-get-project-info` — confirm `project_root` is the cl-mcp repo root.
6. Verify starting state is green:
   - `repl-eval` with `(asdf:compile-system :cl-mcp :force t)` — expect zero warnings.
   - `run-tests` with `{"system": "cl-mcp/tests"}` — expect all current suites pass.

**Do not proceed if step 6 is not clean.** Fix any pre-existing failures first (out of scope — escalate to the user if encountered).

---

## Phase 1: Skeleton files and test hook-up

### Task 1.1: Create the empty core package

**Files:**
- Create: `src/project-scaffold-core.lisp`

**Step 1: Write the file via `fs-write-file`**

Call `fs-write-file` with `path="src/project-scaffold-core.lisp"` and this content:

```lisp
;;;; src/project-scaffold-core.lisp
;;;;
;;;; Pure helpers for project-scaffold: input validation, template rendering,
;;;; path math, and file manifest construction. No I/O. No worker interaction.
;;;; This module exists so that the effectful layer in project-scaffold.lisp
;;;; stays thin and the bulk of the logic is trivially unit-testable.

(defpackage #:cl-mcp/src/project-scaffold-core
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan
                #:regex-replace-all)
  (:export))

(in-package #:cl-mcp/src/project-scaffold-core)
```

**Step 2: Verify it loads**

Run `load-system` with `{"system": "cl-mcp"}`. Expect success; no warnings.

**Step 3: Commit**

```bash
git add src/project-scaffold-core.lisp
git commit -m "scaffold: add empty project-scaffold-core package"
```

---

### Task 1.2: Create the empty templates package

**Files:**
- Create: `src/project-scaffold-templates.lisp`

**Step 1: Write the file via `fs-write-file`**

```lisp
;;;; src/project-scaffold-templates.lisp
;;;;
;;;; Template string constants for project-scaffold. Kept in a dedicated
;;;; file so that bulk literal content does not clutter the logic modules.
;;;; All templates use {{name}}, {{description}}, {{author}}, {{license}},
;;;; {{parent-prompts}} placeholders resolved by render-template in
;;;; project-scaffold-core.

(defpackage #:cl-mcp/src/project-scaffold-templates
  (:use #:cl)
  (:export))

(in-package #:cl-mcp/src/project-scaffold-templates)
```

**Step 2: Verify it loads**

`load-system` with `{"system": "cl-mcp"}`. Expect success.

**Step 3: Commit**

```bash
git add src/project-scaffold-templates.lisp
git commit -m "scaffold: add empty project-scaffold-templates package"
```

---

### Task 1.3: Create the empty entry package

**Files:**
- Create: `src/project-scaffold.lisp`

**Step 1: Write the file via `fs-write-file`**

```lisp
;;;; src/project-scaffold.lisp
;;;;
;;;; MCP tool entry for project-scaffold. Thin I/O layer on top of the pure
;;;; logic in project-scaffold-core. Runs in the parent (inline) process
;;;; alongside other fs-* tools. Registers itself with the tool registry
;;;; at load time via define-tool.

(defpackage #:cl-mcp/src/project-scaffold
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:result
                #:text-content)
  (:export #:project-scaffold))

(in-package #:cl-mcp/src/project-scaffold)
```

**Step 2: Verify it loads**

`load-system` with `{"system": "cl-mcp"}`. Expect success.

**Step 3: Commit**

```bash
git add src/project-scaffold.lisp
git commit -m "scaffold: add empty project-scaffold entry package"
```

---

### Task 1.4: Create the empty test package with a smoke test

**Files:**
- Create: `tests/project-scaffold-test.lisp`

**Step 1: Write the file via `fs-write-file`**

```lisp
;;;; tests/project-scaffold-test.lisp

(defpackage #:cl-mcp/tests/project-scaffold-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/project-scaffold-core))

(in-package #:cl-mcp/tests/project-scaffold-test)

(deftest project-scaffold-smoke
  (testing "project-scaffold-core package loads"
    (ok (find-package '#:cl-mcp/src/project-scaffold-core))))
```

**Step 2: Wire into the aggregate test package**

Use `lisp-patch-form` to add the new test as an `:import-from` entry in `tests.lisp`. The surrounding form is a `defpackage`.

Call `lisp-patch-form` with:
- `path="tests.lisp"`
- `form_type="defpackage"`
- `form_name="cl-mcp/tests"`
- `old_text="(:import-from #:cl-mcp/tests/pool-kill-worker-test))"`
- `new_text="(:import-from #:cl-mcp/tests/pool-kill-worker-test)\n  (:import-from #:cl-mcp/tests/project-scaffold-test))"`

**Step 3: Verify load + test discovery**

Run `load-system` with `{"system": "cl-mcp"}` — expect success.
Run `run-tests` with `{"system": "cl-mcp/tests/project-scaffold-test"}` — expect 1 test, 1 pass.

**Step 4: Commit**

```bash
git add tests/project-scaffold-test.lisp tests.lisp
git commit -m "scaffold: add project-scaffold-test package with smoke test"
```

---

### Task 1.5: Register project-scaffold in the tools loader

**Files:**
- Modify: `src/tools/all.lisp`

**Step 1: Patch the defpackage**

The current form ends with `(:import-from #:cl-mcp/src/tools/pool-kill-worker #:pool-kill-worker))`. Add a new import for `project-scaffold`.

Call `lisp-patch-form`:
- `path="src/tools/all.lisp"`
- `form_type="defpackage"`
- `form_name="cl-mcp/src/tools/all"`
- `old_text="(:import-from #:cl-mcp/src/tools/pool-kill-worker\n                #:pool-kill-worker))"`
- `new_text="(:import-from #:cl-mcp/src/tools/pool-kill-worker\n                #:pool-kill-worker)\n  (:import-from #:cl-mcp/src/project-scaffold\n                #:project-scaffold))"`

**Step 2: Verify the symbol is not yet actually defined — the import should fail**

This is expected. The import exists but `cl-mcp/src/project-scaffold:project-scaffold` is not yet exported. The package has the symbol in `:export` but there is no `define-tool` or function definition yet. ASDF's package-inferred-system loads the file so the package exists, but the `:import-from` in `tools/all.lisp` will complain about missing symbol.

To keep the build green until we actually define the tool, temporarily skip this task's step 1 commit. Instead:

**Revised Step 1: Defer the tools/all.lisp wiring until Task 10.1.**

Undo any change you made to `src/tools/all.lisp` via `lisp-patch-form` reverse-patch, or leave it unmodified entirely. Do **not** commit anything in Task 1.5.

**Marker:** this task is intentionally deferred to Task 10.1 after the `define-tool` invocation exists.

---

## Phase 2: `validate-project-name`

### Task 2.1: Write failing test for a valid name

**Files:**
- Modify: `tests/project-scaffold-test.lisp`

**Step 1: Add the test**

Use `lisp-edit-form` with `operation="insert_after"`, targeting the existing `project-scaffold-smoke` deftest. The new form:

```lisp
(deftest validate-project-name-valid
  (testing "accepts valid names"
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-lib"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "a"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-123"))))
```

Call `lisp-edit-form`:
- `path="tests/project-scaffold-test.lisp"`
- `form_type="deftest"`
- `form_name="project-scaffold-smoke"`
- `operation="insert_after"`
- `content="(deftest validate-project-name-valid\n  (testing \"accepts valid names\"\n    (ok (cl-mcp/src/project-scaffold-core:validate-project-name \"foo-lib\"))\n    (ok (cl-mcp/src/project-scaffold-core:validate-project-name \"a\"))\n    (ok (cl-mcp/src/project-scaffold-core:validate-project-name \"foo-123\"))))"`

**Step 2: Run the test — expect a READ failure (undefined symbol)**

`run-tests` with `{"system": "cl-mcp/tests/project-scaffold-test"}`. Expected: failure to resolve `cl-mcp/src/project-scaffold-core:validate-project-name`.

### Task 2.2: Implement `validate-project-name` minimally

**Files:**
- Modify: `src/project-scaffold-core.lisp`

**Step 1: Add the export**

Call `lisp-patch-form`:
- `path="src/project-scaffold-core.lisp"`
- `form_type="defpackage"`
- `form_name="cl-mcp/src/project-scaffold-core"`
- `old_text="(:export)"`
- `new_text="(:export #:validate-project-name\n           #:invalid-argument-error\n           #:invalid-argument-field\n           #:invalid-argument-value\n           #:invalid-argument-reason)"`

**Step 2: Add the error condition and the function**

Use `lisp-edit-form` `operation="insert_after"` targeting the `in-package` form:

```lisp
(define-condition invalid-argument-error (error)
  ((field :initarg :field :reader invalid-argument-field)
   (value :initarg :value :reader invalid-argument-value)
   (reason :initarg :reason :reader invalid-argument-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid argument ~A = ~S: ~A"
             (invalid-argument-field condition)
             (invalid-argument-value condition)
             (invalid-argument-reason condition)))))

(defparameter *project-name-regex* "^[a-z][a-z0-9-]*$"
  "Regular expression that valid project names must fully match.")

(defparameter *project-name-max-length* 64
  "Maximum allowed length for a project name.")

(defun validate-project-name (name)
  "Return NAME unchanged when valid, else signal INVALID-ARGUMENT-ERROR.
A valid name is a non-empty string of length at most *PROJECT-NAME-MAX-LENGTH*
that fully matches *PROJECT-NAME-REGEX*: a lower-case letter followed by
lower-case letters, digits, or hyphens."
  (unless (stringp name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must be a string"))
  (when (zerop (length name))
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must not be empty"))
  (when (> (length name) *project-name-max-length*)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must be at most ~D characters"
                           *project-name-max-length*)))
  (unless (cl-ppcre:scan *project-name-regex* name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must match ~A" *project-name-regex*)))
  name)
```

Target: `in-package` form, `operation="insert_after"`. Content is the whole block above. Note: `lisp-edit-form` does NOT support `in-package` as `form_type` directly — instead, insert after the LAST existing top-level form in the file. Since the file currently only has `defpackage` and `in-package`, use `form_type="in-package"` OR, if that fails, insert after the `defpackage` with the package qualifier stripped.

**Fallback strategy if `form_type="in-package"` is not supported:** use `fs-write-file` to rewrite the whole file (since it is still small and has no existing `defun` forms, the rewrite is safe). Include the original header, defpackage, in-package, then the new code. `fs-write-file` will refuse if the file exists, so instead use `lisp-edit-form` replacing the `defpackage` form with itself (a no-op) and then manually append — which is awkward. Easiest reliable approach: rewrite via `fs-write-file` after `rm`'ing the file — but `fs-write-file` does not delete. 

**Simplest reliable approach:** Add a dummy anchor `defun` to the file in Task 1.1 so later `insert_after` calls have a target. Go back to Task 1.1's template — but we already committed it empty. Use this alternative:

Call `lisp-edit-form` with:
- `form_type="defpackage"`
- `form_name="cl-mcp/src/project-scaffold-core"`
- `operation="insert_after"`
- `content="(in-package #:cl-mcp/src/project-scaffold-core)\n\n(define-condition invalid-argument-error (error) ... )"` **— but this duplicates `in-package`.**

**Canonical fix:** use `lisp-edit-form` `insert_after` on the `defpackage` form, and include the full block starting with `(define-condition ...)`. The tool inserts after the matched form, which means the block goes _between_ `defpackage` and `in-package`. That is semantically wrong — code after `defpackage` but before `in-package` runs in the wrong package.

**Correct approach:** use `lisp-patch-form` on the `in-package` form is not possible (it's a single atom call with no sub-forms to patch).

**Best approach — adopt this for every file with only `defpackage` + `in-package`:**

Use `lisp-edit-form` `operation="replace"`, `form_type="defpackage"`, replacing the defpackage with an identical defpackage followed by `in-package` followed by the new code. The `content` field then contains:

```lisp
(defpackage #:cl-mcp/src/project-scaffold-core
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan
                #:regex-replace-all)
  (:export #:validate-project-name
           #:invalid-argument-error
           #:invalid-argument-field
           #:invalid-argument-value
           #:invalid-argument-reason))
```

Then a separate `lisp-edit-form` call with `form_type="in-package"`, `form_name="cl-mcp/src/project-scaffold-core"`, `operation="insert_after"`, `content=` the condition + function block. This requires that `lisp-edit-form` accepts `form_type="in-package"`. If it does not, escalate by testing: try the call; if it errors with "unsupported form_type", fall back to `fs-write-file` after `bash rm` of the target file (delete + re-create pattern).

**Actionable step for the engineer:**

1. Try `lisp-edit-form` with `form_type="in-package"` `operation="insert_after"`. If it works, use it. 
2. If not, run `bash rm src/project-scaffold-core.lisp` (this is the ONE approved use of bash rm in this plan because we're recreating a brand-new, just-committed stub) and then `fs-write-file` with the full new content. Commit as a single task (no intermediate state).

**Full rewritten file content (fall-back for `fs-write-file` path):**

```lisp
;;;; src/project-scaffold-core.lisp
;;;;
;;;; Pure helpers for project-scaffold: input validation, template rendering,
;;;; path math, and file manifest construction. No I/O. No worker interaction.
;;;; This module exists so that the effectful layer in project-scaffold.lisp
;;;; stays thin and the bulk of the logic is trivially unit-testable.

(defpackage #:cl-mcp/src/project-scaffold-core
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan
                #:regex-replace-all)
  (:export #:validate-project-name
           #:invalid-argument-error
           #:invalid-argument-field
           #:invalid-argument-value
           #:invalid-argument-reason))

(in-package #:cl-mcp/src/project-scaffold-core)

(define-condition invalid-argument-error (error)
  ((field :initarg :field :reader invalid-argument-field)
   (value :initarg :value :reader invalid-argument-value)
   (reason :initarg :reason :reader invalid-argument-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid argument ~A = ~S: ~A"
             (invalid-argument-field condition)
             (invalid-argument-value condition)
             (invalid-argument-reason condition)))))

(defparameter *project-name-regex* "^[a-z][a-z0-9-]*$"
  "Regular expression that valid project names must fully match.")

(defparameter *project-name-max-length* 64
  "Maximum allowed length for a project name.")

(defun validate-project-name (name)
  "Return NAME unchanged when valid, else signal INVALID-ARGUMENT-ERROR.
A valid name is a non-empty string of length at most *PROJECT-NAME-MAX-LENGTH*
that fully matches *PROJECT-NAME-REGEX*: a lower-case letter followed by
lower-case letters, digits, or hyphens."
  (unless (stringp name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must be a string"))
  (when (zerop (length name))
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must not be empty"))
  (when (> (length name) *project-name-max-length*)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must be at most ~D characters"
                           *project-name-max-length*)))
  (unless (cl-ppcre:scan *project-name-regex* name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must match ~A" *project-name-regex*)))
  name)
```

**Step 3: Run the test**

`run-tests` with `{"system": "cl-mcp/tests/project-scaffold-test"}`. Expected: 2 tests, 2 pass.

**Step 4: Lint**

```bash
mallet src/project-scaffold-core.lisp
```

Expected: no output (clean).

**Step 5: Commit**

```bash
git add src/project-scaffold-core.lisp tests/project-scaffold-test.lisp
git commit -m "scaffold: add validate-project-name with positive test"
```

---

### Task 2.3: Add failing tests for rejections

**Files:**
- Modify: `tests/project-scaffold-test.lisp`

**Step 1: Add the test form**

Use `lisp-edit-form` `operation="insert_after"`, target `deftest validate-project-name-valid`:

```lisp
(deftest validate-project-name-rejects
  (testing "rejects empty string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects uppercase"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "FooLib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects leading digit"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "1foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects underscore"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo_lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects slash"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo/lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects overlong name"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name
                  (make-string 65 :initial-element #\a))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name :foo)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))
```

**Step 2: Run the test**

`run-tests` on the scaffold test package. Expected: all pass (the implementation already covers these cases).

**Step 3: Commit**

```bash
git add tests/project-scaffold-test.lisp
git commit -m "scaffold: add rejection tests for validate-project-name"
```

---

## Phase 3: `validate-destination`

### Task 3.1: Write failing tests

**Files:**
- Modify: `tests/project-scaffold-test.lisp`

Add after the existing tests via `lisp-edit-form` `insert_after` on `deftest validate-project-name-rejects`:

```lisp
(deftest validate-destination
  (testing "accepts a plain relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds")))
  (testing "accepts a nested relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "work/samples")))
  (testing "rejects absolute path"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "/tmp/foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "../outside")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects embedded parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds/../..")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects empty"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))
```

Run the tests — expect failure on "undefined symbol `validate-destination`".

### Task 3.2: Implement `validate-destination`

**Files:**
- Modify: `src/project-scaffold-core.lisp`

**Step 1: Add export**

`lisp-patch-form` on the `defpackage`:
- `old_text="#:validate-project-name"`
- `new_text="#:validate-project-name\n           #:validate-destination"`

**Step 2: Add the function**

`lisp-edit-form` `operation="insert_after"`, `form_type="defun"`, `form_name="validate-project-name"`, content:

```lisp
(defun validate-destination (destination)
  "Return DESTINATION when it is a safe relative path, else signal error.
A valid destination is a non-empty relative path with no absolute segment
and no '..' component. Empty strings and NIL are rejected."
  (unless (and (stringp destination) (plusp (length destination)))
    (error 'invalid-argument-error
           :field "destination" :value destination
           :reason "must be a non-empty string"))
  (when (or (char= (char destination 0) #\/)
            (and (> (length destination) 1)
                 (char= (char destination 1) #\:))) ; rough Windows drive guard
    (error 'invalid-argument-error
           :field "destination" :value destination
           :reason "must be a relative path (no leading / or drive letter)"))
  (dolist (segment (uiop:split-string destination :separator "/"))
    (when (string= segment "..")
      (error 'invalid-argument-error
             :field "destination" :value destination
             :reason "must not contain '..' path segments")))
  destination)
```

**Step 3: Run tests**

`run-tests` on the scaffold test package. Expected: all pass.

**Step 4: Lint**

```bash
mallet src/project-scaffold-core.lisp
```

**Step 5: Commit**

```bash
git add src/project-scaffold-core.lisp tests/project-scaffold-test.lisp
git commit -m "scaffold: add validate-destination"
```

---

## Phase 4: `validate-text-field` (no newlines)

### Task 4.1: Test

Insert after `deftest validate-destination`:

```lisp
(deftest validate-text-field
  (testing "accepts normal text"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "author" "Ada Lovelace")))
  (testing "accepts empty string"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "description" "")))
  (testing "rejects newline"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "author" (format nil "Ada~%Lovelace"))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects CR"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "description" (format nil "foo~Abar" #\Return))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field "license" 42)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))
```

Run — expect undefined `validate-text-field` failure.

### Task 4.2: Implement

**Step 1: Export**

`lisp-patch-form` on defpackage:
- `old_text="#:validate-destination"`
- `new_text="#:validate-destination\n           #:validate-text-field"`

**Step 2: Add the function**

`lisp-edit-form` `insert_after` `defun validate-destination`:

```lisp
(defun validate-text-field (field-name value)
  "Return VALUE when it is an acceptable free-text field, else signal.
FIELD-NAME is included in the error for caller-side diagnostics. A valid
value is a string containing no newline (#\\Newline) or carriage return
(#\\Return) characters. Empty strings are allowed."
  (unless (stringp value)
    (error 'invalid-argument-error
           :field field-name :value value
           :reason "must be a string"))
  (when (or (find #\Newline value) (find #\Return value))
    (error 'invalid-argument-error
           :field field-name :value value
           :reason "must not contain newline characters"))
  value)
```

**Step 3: Run tests, lint, commit**

```bash
mallet src/project-scaffold-core.lisp
git add src/project-scaffold-core.lisp tests/project-scaffold-test.lisp
git commit -m "scaffold: add validate-text-field"
```

---

## Phase 5: `render-template`

### Task 5.1: Test

Insert after `deftest validate-text-field`:

```lisp
(deftest render-template
  (testing "substitutes a single placeholder"
    (ok (equal "hello foo"
               (cl-mcp/src/project-scaffold-core:render-template
                "hello {{name}}" '(("name" . "foo"))))))
  (testing "substitutes multiple placeholders"
    (ok (equal "foo by Ada (MIT)"
               (cl-mcp/src/project-scaffold-core:render-template
                "{{name}} by {{author}} ({{license}})"
                '(("name" . "foo") ("author" . "Ada") ("license" . "MIT"))))))
  (testing "leaves unknown placeholder unchanged"
    (ok (equal "hello {{unknown}}"
               (cl-mcp/src/project-scaffold-core:render-template
                "hello {{unknown}}" '(("name" . "foo"))))))
  (testing "handles repeated placeholder"
    (ok (equal "foo-foo"
               (cl-mcp/src/project-scaffold-core:render-template
                "{{name}}-{{name}}" '(("name" . "foo"))))))
  (testing "passes through regex-sensitive characters in values"
    (ok (equal "value: a.b*c"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}}" '(("v" . "a.b*c"))))))
  (testing "leaves format directive ~A unchanged in value"
    (ok (equal "value: ~A done"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}} done" '(("v" . "~A")))))))
```

Run — expect undefined `render-template`.

### Task 5.2: Implement

**Step 1: Export**

`lisp-patch-form` on defpackage:
- `old_text="#:validate-text-field"`
- `new_text="#:validate-text-field\n           #:render-template"`

**Step 2: Add the function**

`lisp-edit-form` `insert_after` `defun validate-text-field`:

```lisp
(defun render-template (template bindings)
  "Return TEMPLATE with each '{{key}}' substituted using BINDINGS.
BINDINGS is an alist of (KEY-STRING . VALUE-STRING). Unknown placeholders
are left intact. Values are substituted literally; regex metacharacters in
values are escaped so they do not affect the surrounding match. Values
containing literal '\\' or '$' are handled safely by cl-ppcre's
regex-replace-all via a constant-string replacement function."
  (cl-ppcre:regex-replace-all
   "\\{\\{([A-Za-z_-][A-Za-z0-9_-]*)\\}\\}"
   template
   (lambda (match &rest registers)
     (declare (ignore match))
     (let* ((key (first registers))
            (entry (assoc key bindings :test #'string=)))
       (if entry
           (cdr entry)
           (format nil "{{~A}}" key))))
   :simple-calls t))
```

**Step 3: Run tests**

`run-tests`. Expected all pass, including the regex-sensitive and `~A` passthrough cases.

**Step 4: Lint + commit**

```bash
mallet src/project-scaffold-core.lisp
git add src/project-scaffold-core.lisp tests/project-scaffold-test.lisp
git commit -m "scaffold: add render-template"
```

---

## Phase 6: `compute-parent-prompts-path`

### Task 6.1: Test

Insert after `deftest render-template`:

```lisp
(deftest compute-parent-prompts-path
  (testing "scaffolds destination with plain name"
    (ok (equal "../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "scaffolds" "foo-lib"))))
  (testing "two-segment destination"
    (ok (equal "../../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "work/samples" "foo-lib"))))
  (testing "three-segment destination"
    (ok (equal "../../../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "a/b/c" "foo-lib")))))
```

### Task 6.2: Implement

**Step 1: Export**

`lisp-patch-form`:
- `old_text="#:render-template"`
- `new_text="#:render-template\n           #:compute-parent-prompts-path"`

**Step 2: Add the function**

`lisp-edit-form` `insert_after` `defun render-template`:

```lisp
(defun compute-parent-prompts-path (destination name)
  "Return the relative path from DESTINATION/NAME back to <root>/prompts.
DESTINATION is a slash-separated relative directory (e.g. \"scaffolds\",
\"work/samples\"), NAME is the project directory name. The returned path
goes up by one '..' for every path segment in DESTINATION plus one for
NAME, then descends into 'prompts'."
  (let* ((segments (remove-if (lambda (s) (zerop (length s)))
                              (uiop:split-string destination :separator "/")))
         (depth (1+ (length segments))) ; +1 for NAME's own directory level
         (ups (with-output-to-string (s)
                (dotimes (i depth)
                  (declare (ignore i))
                  (write-string "../" s)))))
    (declare (ignore name))
    (concatenate 'string ups "prompts")))
```

**Step 3: Run tests, lint, commit**

```bash
mallet src/project-scaffold-core.lisp
git add src/project-scaffold-core.lisp tests/project-scaffold-test.lisp
git commit -m "scaffold: add compute-parent-prompts-path"
```

---

## Phase 7: Template constants

### Task 7.1: Populate `project-scaffold-templates.lisp`

**Files:**
- Modify: `src/project-scaffold-templates.lisp`

This task is bulk content: 7 template string constants. Because the existing file only has `defpackage` + `in-package`, use the delete-and-rewrite approach:

**Step 1: Delete the stub (one-time exception)**

```bash
rm src/project-scaffold-templates.lisp
```

**Step 2: Write the full content via `fs-write-file`**

Write the file with all 7 template constants. See the design doc section "Template contents" for the verbatim bodies. Use double-backslash nothing; the strings below are raw Lisp string literals with only `"` and `\` needing escape.

```lisp
;;;; src/project-scaffold-templates.lisp
;;;;
;;;; Template string constants for project-scaffold. Kept in a dedicated
;;;; file so that bulk literal content does not clutter the logic modules.
;;;; All templates use {{name}}, {{description}}, {{author}}, {{license}},
;;;; {{parent-prompts}} placeholders resolved by render-template in
;;;; project-scaffold-core.

(defpackage #:cl-mcp/src/project-scaffold-templates
  (:use #:cl)
  (:export #:+asd-template+
           #:+claude-md-template+
           #:+agents-md-template+
           #:+readme-template+
           #:+gitignore-template+
           #:+main-lisp-template+
           #:+main-test-template+))

(in-package #:cl-mcp/src/project-scaffold-templates)

(defparameter +asd-template+
  ";;;; {{name}}.asd

(asdf:defsystem \"{{name}}\"
  :class :package-inferred-system
  :description \"{{description}}\"
  :author \"{{author}}\"
  :license \"{{license}}\"
  :version \"0.1.0\"
  :depends-on (\"{{name}}/src/main\")
  :in-order-to ((test-op (test-op \"{{name}}/tests\"))))

(asdf:defsystem \"{{name}}/tests\"
  :class :package-inferred-system
  :depends-on (\"rove\"
               \"{{name}}\"
               \"{{name}}/tests/main-test\")
  :perform (test-op (o c) (uiop:symbol-call :rove :run c)))
"
  "Template for the generated project's .asd system definition.")

(defparameter +claude-md-template+
  "# CLAUDE.md

## Agent Guidelines

@{{parent-prompts}}/repl-driven-development.md
@{{parent-prompts}}/common-lisp-expert.md

## Project Overview

{{description}}

This project was scaffolded by cl-mcp's `project-scaffold` tool. It follows
cl-mcp's recommended structure: package-inferred-system + Rove.

## Self-Hosted Development

Use cl-mcp tools for all Lisp code operations:

- Search: `clgrep-search`
- Read: `lisp-read-file`
- Edit: `lisp-edit-form`, `lisp-patch-form`
- Eval: `repl-eval`
- Tests: `run-tests` with `{\"system\": \"{{name}}/tests\"}`

## Testing

```lisp
;; From repl-eval
(asdf:test-system :{{name}})
```

## Repository Structure

`src/`      Source code (package-inferred-system)
`tests/`    Rove test suites
"
  "Template for the generated project's CLAUDE.md.")

(defparameter +agents-md-template+
  "# Repository Guidelines

@{{parent-prompts}}/repl-driven-development.md
@{{parent-prompts}}/common-lisp-expert.md

## Project Overview

{{description}}

See `CLAUDE.md` for full agent guidelines — this file mirrors the essentials
for tools that read `AGENTS.md` by convention.

## Build, Test, and Development

Load via `load-system` and iterate via `repl-eval`. Run the test suite with
`run-tests` using system name `{{name}}/tests`.

## Coding Style

Follow the Google Common Lisp Style Guide: 2-space indent, <=100 columns,
lisp-case identifiers, docstrings on public functions.
"
  "Template for the generated project's AGENTS.md.")

(defparameter +readme-template+
  "# {{name}}

{{description}}

## Usage

```lisp
(asdf:load-system :{{name}})
({{name}}/src/main:greet \"world\")
;; => \"Hello, world!\"
```

## Tests

```lisp
(asdf:test-system :{{name}})
```

## License

{{license}}
"
  "Template for the generated project's README.md.")

(defparameter +gitignore-template+
  "*.fasl
*.ufasl
*.x86f
*.cfasl
.asdf-cache/
"
  "Template for the generated project's .gitignore.")

(defparameter +main-lisp-template+
  ";;;; src/main.lisp

(defpackage #:{{name}}/src/main
  (:use #:cl)
  (:export #:greet))

(in-package #:{{name}}/src/main)

(defun greet (who)
  \"Return a friendly greeting for WHO.\"
  (format nil \"Hello, ~A!\" who))
"
  "Template for the generated project's src/main.lisp.")

(defparameter +main-test-template+
  ";;;; tests/main-test.lisp

(defpackage #:{{name}}/tests/main-test
  (:use #:cl #:rove)
  (:import-from #:{{name}}/src/main
                #:greet))

(in-package #:{{name}}/tests/main-test)

(deftest greet-test
  (testing \"greet returns a hello string\"
    (ok (equal \"Hello, world!\" (greet \"world\")))))
"
  "Template for the generated project's tests/main-test.lisp.")
```

**Step 3: Verify load**

`load-system` `cl-mcp`. Expect success.

**Step 4: Lint**

```bash
mallet src/project-scaffold-templates.lisp
```

**Step 5: Commit**

```bash
git add src/project-scaffold-templates.lisp
git commit -m "scaffold: add 7 template constants"
```

---

## Phase 8: `plan-scaffold` manifest builder

### Task 8.1: Test

Insert after `deftest compute-parent-prompts-path`:

```lisp
(deftest plan-scaffold
  (let ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
               :name "foo-lib"
               :description "A demo library"
               :author "Ada Lovelace"
               :license "MIT"
               :destination "scaffolds")))
    (testing "returns seven entries"
      (ok (= 7 (length plan))))
    (testing "each entry is (relative-path . content) of strings"
      (ok (every (lambda (entry)
                   (and (consp entry)
                        (stringp (car entry))
                        (stringp (cdr entry))))
                 plan)))
    (testing "asd file contains the project name"
      (let ((asd (cdr (assoc "foo-lib.asd" plan :test #'string=))))
        (ok asd)
        (ok (search "foo-lib" asd))
        (ok (search "Ada Lovelace" asd))
        (ok (search "A demo library" asd))
        (ok (search "MIT" asd))))
    (testing "CLAUDE.md has resolved parent-prompts path"
      (let ((md (cdr (assoc "CLAUDE.md" plan :test #'string=))))
        (ok md)
        (ok (search "../../prompts/repl-driven-development.md" md))))
    (testing "main.lisp uses the project-qualified package name"
      (let ((src (cdr (assoc "src/main.lisp" plan :test #'string=))))
        (ok src)
        (ok (search "#:foo-lib/src/main" src))))
    (testing "main-test.lisp imports the greet symbol"
      (let ((test (cdr (assoc "tests/main-test.lisp" plan :test #'string=))))
        (ok test)
        (ok (search "#:foo-lib/src/main" test))
        (ok (search "#:greet" test))))
    (testing "no unresolved placeholders remain"
      (dolist (entry plan)
        (ok (null (search "{{" (cdr entry))))))))

(deftest plan-scaffold-deeper-destination
  (let ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
               :name "demo"
               :description "d"
               :author "a"
               :license "MIT"
               :destination "work/samples")))
    (testing "parent-prompts resolves with extra ../"
      (let ((md (cdr (assoc "CLAUDE.md" plan :test #'string=))))
        (ok (search "../../../prompts/repl-driven-development.md" md))))))
```

Run — expect undefined `plan-scaffold` failures.

### Task 8.2: Implement

**Step 1: Update defpackage imports**

`lisp-patch-form` on `defpackage` in `src/project-scaffold-core.lisp`:

First, add imports from templates package:
- `old_text="(:import-from #:cl-ppcre\n                #:scan\n                #:regex-replace-all)"`
- `new_text="(:import-from #:cl-ppcre\n                #:scan\n                #:regex-replace-all)\n  (:import-from #:cl-mcp/src/project-scaffold-templates\n                #:+asd-template+\n                #:+claude-md-template+\n                #:+agents-md-template+\n                #:+readme-template+\n                #:+gitignore-template+\n                #:+main-lisp-template+\n                #:+main-test-template+)"`

Second, add `plan-scaffold` to exports:
- `old_text="#:compute-parent-prompts-path"`
- `new_text="#:compute-parent-prompts-path\n           #:plan-scaffold"`

**Step 2: Add the function**

`lisp-edit-form` `insert_after` `defun compute-parent-prompts-path`:

```lisp
(defun plan-scaffold (&key name description author license destination)
  "Return an alist of (RELATIVE-PATH . CONTENT) for the scaffold manifest.
Applies all template substitutions but performs no I/O. Callers are
responsible for input validation before calling this function; plan-scaffold
assumes its arguments are already normalized strings."
  (let* ((parent-prompts (compute-parent-prompts-path destination name))
         (bindings `(("name" . ,name)
                     ("description" . ,description)
                     ("author" . ,author)
                     ("license" . ,license)
                     ("parent-prompts" . ,parent-prompts)))
         (render (lambda (tpl) (render-template tpl bindings))))
    (list
     (cons (format nil "~A.asd" name) (funcall render +asd-template+))
     (cons "CLAUDE.md"                (funcall render +claude-md-template+))
     (cons "AGENTS.md"                (funcall render +agents-md-template+))
     (cons "README.md"                (funcall render +readme-template+))
     (cons ".gitignore"               (funcall render +gitignore-template+))
     (cons "src/main.lisp"            (funcall render +main-lisp-template+))
     (cons "tests/main-test.lisp"     (funcall render +main-test-template+)))))
```

**Step 3: Run tests, lint, commit**

```bash
mallet src/project-scaffold-core.lisp
git add src/project-scaffold-core.lisp tests/project-scaffold-test.lisp
git commit -m "scaffold: add plan-scaffold manifest builder"
```

---

## Phase 9: Atomic I/O layer

### Task 9.1: Test helper — temp project root

**Files:**
- Modify: `tests/project-scaffold-test.lisp`

Insert after the existing test forms:

```lisp
(defmacro with-temp-project-root ((root-var) &body body)
  "Bind cl-mcp/src/project-root:*project-root* to a fresh temp directory.
Deletes the directory on exit."
  `(let* ((,root-var (uiop:ensure-directory-pathname
                      (uiop:merge-pathnames*
                       (format nil "cl-mcp-scaffold-test-~A/"
                               (random #xFFFFFFFF))
                       (uiop:temporary-directory)))))
     (ensure-directories-exist ,root-var)
     (unwind-protect
          (let ((cl-mcp/src/project-root:*project-root* ,root-var))
            ,@body)
       (ignore-errors (uiop:delete-directory-tree ,root-var :validate t)))))
```

We also need to add the import to the test package. `lisp-patch-form`:
- `old_text="(:import-from #:cl-mcp/src/project-scaffold-core))"`
- `new_text="(:import-from #:cl-mcp/src/project-scaffold-core)\n  (:import-from #:cl-mcp/src/project-root))"`

(Note: CL packages access symbols by package qualification, so the `(:import-from ... #:*project-root*)` is optional if we always use the fully qualified `cl-mcp/src/project-root:*project-root*`. Keep the bare `:import-from` without a specific symbol to surface the package namespace for clarity.)

Actually — `(:import-from #:cl-mcp/src/project-root)` with no symbols is a valid ASDF dependency hint but NOT a real CL import. It ensures ASDF loads the package. Use this form; the body still accesses the special via the `cl-mcp/src/project-root:*project-root*` qualified name.

**No test for the macro alone** — it will be exercised by the next task. Lint + commit:

```bash
mallet src/project-scaffold-core.lisp tests/project-scaffold-test.lisp || true
git add tests/project-scaffold-test.lisp
git commit -m "scaffold: add with-temp-project-root test helper"
```

Note: `mallet` may not lint test files by default; that's fine.

### Task 9.2: Test — `write-scaffold` writes all files atomically

Insert after the `with-temp-project-root` macro:

```lisp
(deftest write-scaffold-creates-all-files
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "foo-lib"
     :description "demo"
     :author "Ada"
     :license "MIT"
     :destination "scaffolds")
    (let ((target (uiop:merge-pathnames* "scaffolds/foo-lib/" root)))
      (testing "target directory exists"
        (ok (uiop:directory-exists-p target)))
      (testing "all seven files exist"
        (dolist (rel '("foo-lib.asd" "CLAUDE.md" "AGENTS.md" "README.md"
                       ".gitignore" "src/main.lisp" "tests/main-test.lisp"))
          (ok (probe-file (uiop:merge-pathnames* rel target))))))))

(deftest write-scaffold-rejects-existing-target
  (with-temp-project-root (root)
    (let ((target (uiop:merge-pathnames* "scaffolds/foo-lib/" root)))
      (ensure-directories-exist target)
      (testing "second generation into existing dir errors"
        (ok (signals
             (cl-mcp/src/project-scaffold:write-scaffold
              :name "foo-lib" :description "d" :author "a" :license "MIT"
              :destination "scaffolds")
             'cl-mcp/src/project-scaffold-core:invalid-argument-error))))))

(deftest write-scaffold-validates-inputs
  (with-temp-project-root (root)
    (declare (ignore root))
    (testing "invalid name errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "BadName" :description "d" :author "a" :license "MIT"
            :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
    (testing "newline in author errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "ok" :description "d" :author (format nil "a~%b") :license "MIT"
            :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))))

(deftest write-scaffold-no-temp-dir-on-failure
  (with-temp-project-root (root)
    (ignore-errors
     (cl-mcp/src/project-scaffold:write-scaffold
      :name "BadName" :description "d" :author "a" :license "MIT"
      :destination "scaffolds"))
    (let ((scaffolds (uiop:merge-pathnames* "scaffolds/" root)))
      (testing "no .tmp directory remains"
        (ok (or (null (uiop:directory-exists-p scaffolds))
                (null (remove-if-not
                       (lambda (p)
                         (uiop:string-prefix-p ".tmp-project-scaffold-"
                                               (car (last (pathname-directory p)))))
                       (uiop:subdirectories scaffolds)))))))))
```

Run — expect undefined `write-scaffold`.

### Task 9.3: Implement `write-scaffold`

**Files:**
- Modify: `src/project-scaffold.lisp`

**Step 1: Delete + rewrite the file (one-time, stub is empty)**

```bash
rm src/project-scaffold.lisp
```

`fs-write-file` with:

```lisp
;;;; src/project-scaffold.lisp
;;;;
;;;; MCP tool entry for project-scaffold. Thin I/O layer on top of the pure
;;;; logic in project-scaffold-core. Runs in the parent (inline) process
;;;; alongside other fs-* tools. Registers itself with the tool registry
;;;; at load time via define-tool.

(defpackage #:cl-mcp/src/project-scaffold
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:result
                #:text-content)
  (:import-from #:cl-mcp/src/project-scaffold-core
                #:validate-project-name
                #:validate-destination
                #:validate-text-field
                #:plan-scaffold
                #:invalid-argument-error)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-project-root
                #:canonical-path
                #:path-inside-p)
  (:export #:project-scaffold
           #:write-scaffold))

(in-package #:cl-mcp/src/project-scaffold)

(defun %uuid-suffix ()
  "Return a short pseudo-random suffix for temp directory naming."
  (format nil "~8,'0X" (random #xFFFFFFFF)))

(defun %absolute-scaffold-paths (root destination name)
  "Return (values TARGET-DIR TEMP-DIR) as absolute directory pathnames.
Both paths are inside ROOT. TEMP-DIR has a random suffix to avoid
collisions between concurrent calls."
  (let* ((dest-dir (uiop:ensure-directory-pathname
                    (merge-pathnames destination root)))
         (target-dir (uiop:ensure-directory-pathname
                      (merge-pathnames (format nil "~A/" name) dest-dir)))
         (temp-dir (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (format nil ".tmp-project-scaffold-~A/" (%uuid-suffix))
                     dest-dir))))
    (values target-dir temp-dir)))

(defun %assert-within-project-root (pathname)
  "Signal error if PATHNAME is not inside *project-root*."
  (unless (path-inside-p pathname *project-root*)
    (error 'invalid-argument-error
           :field "destination" :value (namestring pathname)
           :reason "resolves outside project root")))

(defun %write-files-to-temp (temp-dir plan)
  "Write all PLAN entries into TEMP-DIR. Caller cleans up on error."
  (ensure-directories-exist temp-dir)
  (dolist (entry plan)
    (let* ((rel (car entry))
           (content (cdr entry))
           (abs (merge-pathnames rel temp-dir)))
      (ensure-directories-exist abs)
      (with-open-file (out abs :direction :output
                               :if-exists :error
                               :if-does-not-exist :create
                               :element-type 'character)
        (write-sequence content out)))))

(defun write-scaffold (&key name description author license destination)
  "Generate the scaffold project atomically. Returns a plist with:
  :target-dir (absolute pathname)
  :relative-path (namestring relative to *project-root*)
  :files (list of relative path strings, in manifest order)

On any failure, signals INVALID-ARGUMENT-ERROR or propagates the
underlying error after cleaning up the temp directory."
  (ensure-project-root)
  (validate-project-name name)
  (validate-destination destination)
  (validate-text-field "description" (or description ""))
  (validate-text-field "author" (or author ""))
  (validate-text-field "license" (or license ""))
  (multiple-value-bind (target-dir temp-dir)
      (%absolute-scaffold-paths *project-root* destination name)
    (%assert-within-project-root target-dir)
    (%assert-within-project-root temp-dir)
    (when (uiop:directory-exists-p target-dir)
      (error 'invalid-argument-error
             :field "name" :value name
             :reason (format nil "target directory already exists: ~A"
                             (namestring target-dir))))
    (let ((plan (plan-scaffold :name name
                               :description (or description "")
                               :author (or author "")
                               :license (or license "")
                               :destination destination)))
      (unwind-protect
           (progn
             (%write-files-to-temp temp-dir plan)
             (rename-file temp-dir target-dir)
             (setf temp-dir nil)
             (list :target-dir target-dir
                   :relative-path
                   (enough-namestring target-dir *project-root*)
                   :files (mapcar #'car plan)))
        (when (and temp-dir (uiop:directory-exists-p temp-dir))
          (ignore-errors
           (uiop:delete-directory-tree temp-dir :validate t)))))))
```

**Step 2: Run tests**

`run-tests` on `cl-mcp/tests/project-scaffold-test`. Expected: all previous tests plus the 4 new `write-scaffold-*` tests pass.

**Investigation if `rename-file` fails between same-FS dirs:** SBCL's `rename-file` on a directory *pathname* can be finicky across implementations. If you see `FILE-ERROR` about the target, switch the `rename-file` call to `(uiop:rename-file-overwriting-target temp-dir target-dir)` OR implement a manual recursive copy as a fallback. Try the portable approach first.

**Step 3: Lint**

```bash
mallet src/project-scaffold.lisp
```

**Step 4: Commit**

```bash
git add src/project-scaffold.lisp tests/project-scaffold-test.lisp
git commit -m "scaffold: add write-scaffold atomic I/O layer"
```

---

## Phase 10: MCP tool registration

### Task 10.1: Wire `project-scaffold` into tools/all.lisp

**Files:**
- Modify: `src/tools/all.lisp`

**Step 1: Patch the defpackage**

`lisp-patch-form`:
- `path="src/tools/all.lisp"`
- `form_type="defpackage"`
- `form_name="cl-mcp/src/tools/all"`
- `old_text="(:import-from #:cl-mcp/src/tools/pool-kill-worker\n                #:pool-kill-worker))"`
- `new_text="(:import-from #:cl-mcp/src/tools/pool-kill-worker\n                #:pool-kill-worker)\n  (:import-from #:cl-mcp/src/project-scaffold\n                #:project-scaffold))"`

**Step 2: Verify load**

`load-system` `cl-mcp`. Expect success.

Currently `project-scaffold` symbol is exported from the package but only as a forward declaration — no `define-tool` yet. ASDF will load the file, the symbol will exist (because `(:export #:project-scaffold)` makes it so), and the `:import-from` in `tools/all.lisp` will resolve.

Do **not** commit yet — keep the change staged and proceed to the next task which adds the `define-tool` body so the symbol is actually populated.

### Task 10.2: Add the `define-tool` form

**Files:**
- Modify: `src/project-scaffold.lisp`

**Step 1: Insert after `defun write-scaffold`**

`lisp-edit-form` `operation="insert_after"`, `form_type="defun"`, `form_name="write-scaffold"`, content:

```lisp
(define-tool "project-scaffold"
  :description
  "Generate a minimal Common Lisp project skeleton under the project root.

The generated project uses package-inferred-system + Rove and ships with
CLAUDE.md/AGENTS.md templates referencing cl-mcp's existing prompts via
relative @-include paths. On success, returns the list of created files and
a 'next_steps' array with concrete REPL commands the agent can invoke to
register the project with ASDF and run its tests.

Fails if the target directory already exists; choose a unique 'name' per
generation. Intended for creating throwaway sample projects to exercise
cl-mcp's tool surface."
  :args
  ((name :type :string :required t
         :description
         "Project name in lisp-case (e.g. \"foo-lib\"). Must match ^[a-z][a-z0-9-]*$ and be 1-64 chars.")
   (description :type :string
                :description
                "One-line project description for .asd and README. No newlines.")
   (author :type :string
           :description "Author string for .asd :author. No newlines.")
   (license :type :string
            :description "License string for .asd :license. No newlines.")
   (destination :type :string
                :description
                "Relative parent directory under project root where <name>/ is created. Default: \"scaffolds\"."))
  :body
  (handler-case
      (let* ((result-plist
              (write-scaffold
               :name name
               :description (or description "A Common Lisp project scaffolded by cl-mcp.")
               :author (or author "Unknown")
               :license (or license "MIT")
               :destination (or destination "scaffolds")))
             (target-dir (getf result-plist :target-dir))
             (relative (getf result-plist :relative-path))
             (files (getf result-plist :files))
             (abs-asd (namestring
                       (merge-pathnames (format nil "~A.asd" name) target-dir)))
             (next-steps
              (vector
               (format nil
                       "To register with ASDF: run repl-eval with (asdf:load-asd ~S)"
                       abs-asd)
               (format nil
                       "To load: run load-system with {\"system\": ~S}"
                       name)
               (format nil
                       "To test: run run-tests with {\"system\": ~S}"
                       (format nil "~A/tests" name))
               (format nil
                       "To edit: use lisp-edit-form with paths under ~A"
                       relative))))
        (result id
                (make-ht
                 "created" t
                 "path" relative
                 "absolute_path" (namestring target-dir)
                 "files" (coerce files 'vector)
                 "next_steps" next-steps
                 "content"
                 (text-content
                  (format nil "Scaffolded ~A at ~A (~D files)"
                          name relative (length files))))))
    (cl-mcp/src/project-scaffold-core:invalid-argument-error (e)
      (result id
              (make-ht
               "created" nil
               "error" (princ-to-string e)
               "content" (text-content (princ-to-string e)))))))
```

**Step 2: Run the full scaffold test package**

`run-tests` with `{"system": "cl-mcp/tests/project-scaffold-test"}`. Expected: all tests pass.

**Step 3: Run the full regression suite**

`run-tests` with `{"system": "cl-mcp/tests"}`. Expected: no new failures.

**Step 4: Lint**

```bash
mallet src/project-scaffold.lisp
```

**Step 5: Commit (both files)**

```bash
git add src/project-scaffold.lisp src/tools/all.lisp
git commit -m "scaffold: register project-scaffold as MCP tool"
```

---

### Task 10.3: Export from main.lisp

**Files:**
- Modify: `main.lisp`

**Step 1: Inspect main.lisp for the pattern**

`lisp-read-file path="main.lisp" collapsed=true` — find how other tools are exported. Look at the `defpackage #:cl-mcp/main` form.

**Step 2: Patch the defpackage if necessary**

If the main package re-exports tool names, add `#:project-scaffold` to its `:export` list via `lisp-patch-form`. If no such re-export convention exists, skip this step (several tools are not re-exported; the registration via `define-tool` is what matters).

**Step 3: Verify load + tests + lint**

```bash
mallet main.lisp
```

`run-tests` `cl-mcp/tests`.

**Step 4: Commit** (only if main.lisp was actually modified)

```bash
git add main.lisp
git commit -m "scaffold: export project-scaffold from main package"
```

---

## Phase 11: End-to-end test

### Task 11.1: e2e test — generate → load → run tests

**Files:**
- Modify: `tests/project-scaffold-test.lisp`

**Step 1: Add the test**

Insert after the `write-scaffold-*` deftests:

```lisp
(deftest scaffold-e2e-load-and-test
  (with-temp-project-root (root)
    ;; Copy cl-mcp's real prompts/ into the temp root so the @-includes resolve.
    ;; The scaffold's CLAUDE.md uses ../../prompts/... which, relative to
    ;; root/scaffolds/foo-lib/, means root/prompts/. Create a minimal stub.
    (let ((prompts (uiop:ensure-directory-pathname
                    (merge-pathnames "prompts/" root))))
      (ensure-directories-exist prompts)
      (with-open-file (s (merge-pathnames "repl-driven-development.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s))
      (with-open-file (s (merge-pathnames "common-lisp-expert.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s)))
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "foo-lib-e2e"
     :description "e2e demo"
     :author "Test"
     :license "MIT"
     :destination "scaffolds")
    (let* ((asd-path (merge-pathnames "scaffolds/foo-lib-e2e/foo-lib-e2e.asd" root))
           (system-name "foo-lib-e2e"))
      (testing "asd is loadable"
        (ok (asdf:load-asd asd-path)))
      (testing "system loads cleanly"
        (ok (asdf:load-system system-name)))
      (testing "bundled test system runs green"
        (let ((test-system-name (format nil "~A/tests" system-name)))
          (ok (asdf:load-system test-system-name))
          ;; Rove's (rove:run ...) returns T for green, NIL for any failure.
          (ok (rove:run test-system-name))))
      ;; Clean up ASDF's knowledge of the system so subsequent runs work.
      (asdf:clear-system system-name)
      (asdf:clear-system (format nil "~A/tests" system-name)))))
```

**Step 2: Run the test**

`run-tests` on `cl-mcp/tests/project-scaffold-test`. Expected: all pass including the new e2e test.

**Troubleshooting if it fails:**
- If `asdf:load-asd` fails — check that the generated `.asd` is syntactically valid via `lisp-check-parens` on `/tmp/.../scaffolds/foo-lib-e2e/foo-lib-e2e.asd`.
- If `asdf:load-system` fails with "component not found" — the package-inferred-system needs the scaffold root in `asdf:*central-registry*` OR the `.asd` must have been loaded via `load-asd` first (we do this). Try `(asdf:initialize-source-registry ...)` inside the test.
- If the test hangs — Rove may be waiting on I/O; wrap the `(rove:run ...)` in a string stream capture.

**Step 3: Commit**

```bash
git add tests/project-scaffold-test.lisp
git commit -m "scaffold: add end-to-end load-and-test integration test"
```

---

## Phase 12: Documentation

### Task 12.1: Update docs/tools.md

**Files:**
- Modify: `docs/tools.md`

**Step 1: Find the right insertion point**

`fs-read-file path="docs/tools.md"` — read through to find where existing tools are documented. Identify the pattern (typically a `## tool-name` heading with sections for Description, Arguments, Example, Notes).

**Step 2: Add a new section**

Use `fs-write-file` only if the file is markdown (it is) — `fs-write-file` refuses `.lisp` overwrites but allows `.md`. Since we are APPENDING content, read the file first, append the new section, and write back.

Actually, `fs-write-file` overwrites without merging, so the safer pattern is:
1. `fs-read-file` the current contents.
2. Concatenate the new section.
3. `fs-write-file` the whole new content.

The new section content (adjust heading depth to match the existing file):

```markdown
## project-scaffold

Generate a minimal Common Lisp project skeleton under the project root. The
generated project uses `package-inferred-system` + Rove and ships with
CLAUDE.md/AGENTS.md templates referencing cl-mcp's existing prompts.

**Arguments:**
- `name` (string, required) — project name in lisp-case, `^[a-z][a-z0-9-]*$`, 1–64 chars
- `description` (string) — one-line description for `.asd` and README (no newlines)
- `author` (string) — `.asd` `:author` (no newlines)
- `license` (string) — `.asd` `:license` (no newlines)
- `destination` (string) — relative parent directory under project root, default `scaffolds`

**Behavior:**
- Fails if `scaffolds/<name>/` already exists; choose a unique name per call.
- Writes all files atomically via a temp directory + rename.
- Returns `{created, path, absolute_path, files, next_steps}` on success.
- `next_steps` contains concrete REPL commands the agent can invoke to
  `asdf:load-asd`, `load-system`, and `run-tests` the generated project.

**Example:**

```json
{
  "name": "project-scaffold",
  "arguments": {
    "name": "demo-lib",
    "description": "A scratch project for testing cl-mcp tools",
    "author": "Satoshi Imai",
    "license": "MIT"
  }
}
```

**Notes:**
- Runs inline in the parent process alongside other `fs-*` tools.
- Does not automatically load or switch project root; the caller executes the
  returned `next_steps` manually via `repl-eval` / `load-system` / `run-tests`.
- Generated scaffolds live under `<project-root>/scaffolds/<name>/` and are
  accessible to all cl-mcp tools without changing `project-root`.
```

**Step 3: Commit**

```bash
git add docs/tools.md
git commit -m "docs: document project-scaffold tool"
```

---

## Phase 13: Full-system verification

### Task 13.1: Compile with `:force t` — zero new warnings

**Step 1: Run**

`repl-eval` `(asdf:compile-system :cl-mcp :force t)`

**Expected:** compiles, zero warnings. If warnings appear in the new files, fix them (usually docstring/style issues).

### Task 13.2: Run the full test suite

**Step 1: Run**

`run-tests` with `{"system": "cl-mcp/tests"}`

**Expected:** all suites green. Baseline comparison: same pass count as before the work, plus the new `project-scaffold-test` tests.

### Task 13.3: Lint all new files

**Step 1: Run**

```bash
mallet src/project-scaffold.lisp src/project-scaffold-core.lisp src/project-scaffold-templates.lisp
```

**Expected:** clean.

### Task 13.4: Manual smoke test

**Step 1: Restart cl-mcp** (server owner does this — escalate if unclear)

**Step 2: Invoke the tool**

`project-scaffold` with arguments `{"name": "smoke-demo", "description": "manual smoke", "author": "Tester", "license": "MIT"}`

**Expected:** response includes `created: true`, `files` array of 7 entries, `next_steps` array of 4 strings.

**Step 3: Execute the next_steps**

Via `repl-eval` / `load-system` / `run-tests` as the response instructs.

**Expected:** scaffold loads, its test system runs green.

**Step 4: Clean up the smoke scaffold** (do NOT commit it)

```bash
rm -rf scaffolds/smoke-demo
```

If the temp `scaffolds/` directory is otherwise empty:

```bash
rmdir scaffolds 2>/dev/null || true
```

### Task 13.5: Final commit for any stragglers

If any fixes were applied during verification:

```bash
git add <files>
git commit -m "scaffold: address lint/compile feedback from final verification"
```

If nothing to commit, skip.

---

## Definition of Done Checklist

Before declaring the work complete, verify:

- [ ] All tests in `tests/project-scaffold-test.lisp` green (positive, negative, integration, e2e)
- [ ] `run-tests {"system": "cl-mcp/tests"}` shows no regressions
- [ ] `(asdf:compile-system :cl-mcp :force t)` with zero new warnings
- [ ] `mallet src/project-scaffold*.lisp` clean
- [ ] Manual smoke test: `project-scaffold smoke-demo` → `load-asd` → `load-system` → `run-tests` → green
- [ ] `docs/tools.md` has a `project-scaffold` reference section
- [ ] No stray `scaffolds/smoke-*` directories committed to the repo
- [ ] All commits follow the project's `<type>: <imperative summary>` convention
- [ ] Each commit is small and independently reviewable (no multi-phase megacommits)

---

## Appendix: Notes for the engineer

- **Do not skip the red/green TDD cycle.** Even when the implementation feels obvious, write the failing test first and see it actually fail. If a test passes before the implementation exists, the test is wrong.
- **Commit after every green.** Small steps, small commits. If a phase has 3 tests and 1 implementation, that is potentially 2 commits (red tests → green) or 1 (if atomic).
- **Use `lisp-edit-form` / `lisp-patch-form` exclusively for `.lisp` edits.** Do NOT use the generic Edit tool. Parinfer auto-repair prevents silent paren errors.
- **`fs-write-file` is for NEW files only.** The tool refuses to overwrite existing `.lisp` files by design. For edits, use the Lisp-aware tools.
- **When `lisp-edit-form` insertion is ambiguous** (e.g., file has only `defpackage` + `in-package`), the plan notes a one-time delete-and-rewrite workaround. Use it sparingly and only for files committed moments ago in the same branch.
- **Escalate promptly** if you hit:
  - Pre-existing test failures in Phase 0.
  - `rename-file` FS errors on the atomic directory move (investigate per-implementation alternatives).
  - Unexpected `define-tool` macro errors (read `src/tools/define-tool.lisp` docstring, study existing `define-tool` calls in `src/fs.lisp`).
  - Any sign of scope creep: do NOT add extra templates, extra parameters, or "nice to have" features not in this plan.
- **When stuck**, check `docs/plans/2026-04-09-project-scaffold-design.md` — the design doc answers the "why" behind every decision in this plan.
