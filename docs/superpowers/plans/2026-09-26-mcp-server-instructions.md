# MCP Server Instructions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Return `InitializeResult.instructions` from cl-mcp so that enabling the server alone gives
an agent the minimum standing guidance, with the cl-spec part present exactly when the `cl-spec`
tool group is on.

**Architecture:** The registry, which already decides which tools `tools/list` shows from
`*enabled-tool-groups*`, also holds a short instructions text per group. A new module
`src/server-instructions.lisp` owns the base text and joins it with the texts of the enabled
groups. `handle-initialize` puts the result in `"instructions"`. The cl-spec text is registered
next to the cl-spec tools in `src/tools/spec-tools.lisp`, so a group's tools and its guidance live
together and are switched by the same predicate.

**Tech Stack:** SBCL, ASDF package-inferred system, Rove, Yason, cl-ppcre.

## Global Constraints

- Budget: the joined instructions are at most **2,048 characters** for every combination of
  groups. Claude Code truncates at 2,048 characters (CHANGELOG v2.1.84 / v2.1.280, docs "tool
  search"); ChatGPT's docs ask for the most important details in the first 512 characters.
- The three core rules (`fs-set-project-root`, Lisp-aware editing tools, `load-system` after an
  edit) must all start within the **first 512 characters** of the base text.
- Order: base text first, then group texts in group registration order. Truncation cuts from the
  end, so the base must never depend on anything after it.
- Instructions may name a tool only if that tool is in `tools/list` under the same
  `*enabled-tool-groups*`.
- Instructions are read once per `initialize` by every client surveyed (none re-reads on
  `list_changed`). Groups stay a startup-time setting; `set-enabled-tool-groups` says so.
- `prompts/*.md` stay: Cline, Continue and Zed ignore `instructions`, and the prompts remain the
  full reference. The instructions are a summary, not a copy.
- English text, no backslash-newline inside string literals (it embeds a newline, it does not
  continue the line). Lines of source <= 100 columns. Run `mallet` before each commit.
- Lisp source edits go through `lisp-edit-form` / `lisp-patch-form`; new files start with
  `fs-write-file` (in-package + stub) and grow with `lisp-edit-form`.
- Parent-process code changes: the running MCP server keeps the old image until restart; verify
  with `run-tests` (worker) and a fresh `rove` process, not by calling the live server.

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `src/tools/registry.lisp` | modify | per-group instructions registry: `register-tool-group-instructions`, `enabled-tool-group-instructions` |
| `src/server-instructions.lisp` | create | base text, budget constant, `server-instructions` (join) |
| `src/tools/spec-tools.lisp` | modify | register the `:cl-spec` text |
| `src/protocol.lisp` | modify | `"instructions"` in the initialize result |
| `tests/server-instructions-test.lisp` | create | registry, composition, budget, consistency, initialize |
| `tests.lisp` | modify | import the new test package |
| `docs/tools.md`, `README.md` | modify | document the behaviour and the relation to `prompts/` |

No `cl-mcp.asd` edit: `src/protocol.lisp` importing `cl-mcp/src/server-instructions` is enough
for the package-inferred system to load it.

---

### Task 1: Per-group instructions in the registry

**Files:**
- Modify: `src/tools/registry.lisp` (defpackage exports; new forms after `tool-group-enabled-p`;
  docstring of `set-enabled-tool-groups`)
- Create: `tests/server-instructions-test.lisp`
- Modify: `tests.lisp` (add `(:import-from #:cl-mcp/tests/server-instructions-test)` after
  `protocol-test`)

**Interfaces:**
- Produces:
  - `(register-tool-group-instructions group text) => group-name` — GROUP is a keyword or string
    (normalized with `normalize-tool-group`), TEXT a string. Re-registering a group replaces its
    text in place, keeping its position.
  - `(enabled-tool-group-instructions) => list of strings` — texts of the groups for which
    `tool-group-enabled-p` is true, in first-registration order.

- [ ] **Step 1: Write the failing test**

Create `tests/server-instructions-test.lisp` (via `fs-write-file`, then `lisp-edit-form`):

```lisp
;;;; tests/server-instructions-test.lisp

(defpackage #:cl-mcp/tests/server-instructions-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/registry
                #:*enabled-tool-groups*
                #:register-tool-group-instructions
                #:enabled-tool-group-instructions))

(in-package #:cl-mcp/tests/server-instructions-test)

(deftest group-instructions-follow-the-enabled-groups
  (register-tool-group-instructions :test-instructions-group "Test group text.")
  (testing "a group that is off contributes nothing"
    (let ((*enabled-tool-groups* '()))
      (ok (not (member "Test group text." (enabled-tool-group-instructions)
                       :test #'string=)))))
  (testing "a group that is on contributes its text"
    (let ((*enabled-tool-groups* (list "TEST-INSTRUCTIONS-GROUP")))
      (ok (member "Test group text." (enabled-tool-group-instructions)
                  :test #'string=))))
  (testing "re-registering replaces the text instead of adding a second one"
    (register-tool-group-instructions "test-instructions-group" "Replaced.")
    (let ((*enabled-tool-groups* (list "TEST-INSTRUCTIONS-GROUP")))
      (ok (equal '("Replaced.") (enabled-tool-group-instructions))))))
```

The last assertion holds only while no other group is enabled in the binding, which is why the
binding names the test group alone.

- [ ] **Step 2: Run it to verify it fails**

Run `run-tests` with `{"system": "cl-mcp/tests/server-instructions-test"}`.
Expected: load failure, `REGISTER-TOOL-GROUP-INSTRUCTIONS` not external in
`CL-MCP/SRC/TOOLS/REGISTRY`.

- [ ] **Step 3: Implement**

Add to the `:export` list of `#:cl-mcp/src/tools/registry`:

```lisp
           #:register-tool-group-instructions
           #:enabled-tool-group-instructions
```

Insert after `tool-group-enabled-p` (`lisp-edit-form`, `insert_after`, `form_type` `defun`,
`form_name` `tool-group-enabled-p`):

```lisp
(defvar *tool-group-instructions* '()
  "Instructions text of each optional tool group, as (GROUP-NAME . TEXT) in
first-registration order.

Kept beside the tools so that what tools/list shows and what the server tells
a client about using it are decided by the same TOOL-GROUP-ENABLED-P.")

(defun register-tool-group-instructions (group text)
  "Register TEXT as the instructions of the optional tool GROUP; return its name.

Registering the same group again replaces its text where it stands, so
reloading a file does not move its group behind the others or repeat it."
  (check-type text string)
  (let* ((name (normalize-tool-group group))
         (entry (assoc name *tool-group-instructions* :test #'equal)))
    (if entry
        (setf (cdr entry) text)
        (setf *tool-group-instructions*
              (append *tool-group-instructions* (list (cons name text)))))
    name))

(defun enabled-tool-group-instructions ()
  "Return the instructions texts of the enabled tool groups, in registration order."
  (loop for (name . text) in *tool-group-instructions*
        when (tool-group-enabled-p name)
          collect text))
```

Append to the docstring of `set-enabled-tool-groups` (`lisp-patch-form`):

```text
Call it before a client connects.  A client reads the server's instructions
once, at initialize, and none of the ones surveyed reads them again on
tools/list_changed, so changing the groups under a connected client leaves its
tool list and its instructions disagreeing.
```

- [ ] **Step 4: Run the test to verify it passes**

Run `run-tests` with `{"system": "cl-mcp/tests/server-instructions-test"}`. Expected: 1 test, all
assertions pass. Also run `{"system": "cl-mcp/tests/define-tool-test"}` (existing group tests).

- [ ] **Step 5: Lint and commit**

```bash
mallet src/tools/registry.lisp tests/server-instructions-test.lisp tests.lisp
git add src/tools/registry.lisp tests/server-instructions-test.lisp tests.lisp
git commit -m "registry: keep an instructions text per optional tool group"
```

---

### Task 2: The base text, the cl-spec text and their composition

**Files:**
- Create: `src/server-instructions.lisp`
- Modify: `src/tools/spec-tools.lisp` (a top-level form after `in-package`)
- Modify: `tests/server-instructions-test.lisp`

**Interfaces:**
- Consumes: `enabled-tool-group-instructions`, `register-tool-group-instructions` (Task 1).
- Produces (package `cl-mcp/src/server-instructions`):
  - `+instructions-budget+` — 2048
  - `+base-instructions+` — the base string
  - `(server-instructions) => string` — base, then each enabled group's text, separated by one
    blank line.

- [ ] **Step 1: Write the failing tests**

Add to the test package's imports:

```lisp
  (:import-from #:cl-mcp/src/server-instructions
                #:+instructions-budget+
                #:+base-instructions+
                #:server-instructions)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-all-tool-descriptors)
  ;; Loads every tool module, so the registry below is the one the server has.
  (:import-from #:cl-mcp/src/protocol
                #:process-json-line)
  (:import-from #:yason #:parse)
```

(Merge the second `cl-mcp/src/tools/registry` clause into the one from Task 1.)

Add the tests:

```lisp
(defparameter *group-settings* (list '() (list "CL-SPEC"))
  "Every combination of the groups cl-mcp ships, as *ENABLED-TOOL-GROUPS* values.")

(defun %mentions-tool-p (text name)
  "True when TEXT names tool NAME as a word of its own, not inside a longer name."
  (cl-ppcre:scan (format nil "(?<![a-z0-9-])~A(?![a-z0-9-])"
                         (cl-ppcre:quote-meta-chars name))
                 text))

(defun %listed-tool-names ()
  (map 'list (lambda (d) (gethash "name" d)) (get-all-tool-descriptors)))

(deftest base-instructions-only-without-groups
  (let ((*enabled-tool-groups* '()))
    (ok (string= +base-instructions+ (server-instructions)))
    (ok (not (search "spec-check" (server-instructions))))))

(deftest cl-spec-instructions-follow-the-group
  (let ((*enabled-tool-groups* (list "CL-SPEC")))
    (let ((text (server-instructions)))
      (ok (eql 0 (search +base-instructions+ text)) "the base text comes first")
      (ok (search "function=" text))
      (ok (search "Replay:" text)))))

(deftest instructions-fit-the-budget
  (dolist (groups *group-settings*)
    (let ((*enabled-tool-groups* groups))
      (ok (<= (length (server-instructions)) +instructions-budget+)
          (format nil "~S: ~D characters"
                  groups (length (server-instructions)))))))

(deftest core-rules-lead-the-base-text
  ;; ChatGPT asks for the essentials in the first 512 characters; Claude Code
  ;; cuts from the end.  Either way the opening has to stand alone.
  (dolist (tool '("fs-set-project-root" "lisp-edit-form" "load-system"))
    (let ((position (search tool +base-instructions+)))
      (ok (and position (< position 512))
          (format nil "~A must appear within the first 512 characters" tool)))))

(deftest instructions-name-only-listed-tools
  ;; The point of deriving both from *ENABLED-TOOL-GROUPS*: guidance never
  ;; recommends a tool the client cannot see.
  (let ((registered (loop for name being the hash-keys
                            of cl-mcp/src/tools/registry::*tool-registry*
                          collect name)))
    (dolist (groups *group-settings*)
      (let* ((*enabled-tool-groups* groups)
             (text (server-instructions))
             (listed (%listed-tool-names)))
        (dolist (name registered)
          (when (%mentions-tool-p text name)
            (ok (member name listed :test #'string=)
                (format nil "~S names ~A, which tools/list hides" groups name))))))))

(deftest instructions-name-real-tools
  ;; Guards the text against a misspelt or renamed tool.
  (let ((*enabled-tool-groups* (list "CL-SPEC")))
    (let ((listed (%listed-tool-names)))
      (dolist (name '("fs-set-project-root" "clgrep-search" "lisp-read-file"
                      "lisp-edit-form" "lisp-patch-form" "load-system" "repl-eval"
                      "run-tests" "clos-describe" "lisp-macroexpand"
                      "inspect-object" "lisp-check-parens" "spec-list"
                      "spec-symbol" "spec-describe" "spec-check"))
        (ok (member name listed :test #'string=) name)
        (ok (%mentions-tool-p (server-instructions) name)
            (format nil "the instructions name ~A" name))))))
```

- [ ] **Step 2: Run to verify they fail**

`run-tests` `{"system": "cl-mcp/tests/server-instructions-test"}`. Expected: load failure,
package `CL-MCP/SRC/SERVER-INSTRUCTIONS` does not exist.

- [ ] **Step 3: Create `src/server-instructions.lisp`**

Write the stub with `fs-write-file`, then replace the stub with `lisp-edit-form`. Final content:

```lisp
;;;; src/server-instructions.lisp
;;;;
;;;; The instructions cl-mcp returns from initialize: the standing guidance an
;;;; agent gets from enabling the server, before it reads any tool description.
;;;;
;;;; With tool search, Claude Code defers every tool description and keeps only
;;;; tool names and these instructions in context, so this text is often the
;;;; only cl-mcp guidance a model has.  It is also cut: Claude Code truncates it
;;;; at 2,048 characters, from the end.  So it holds cross-tool rules only --
;;;; what one tool's description cannot say -- and the full guides stay in
;;;; prompts/.

(defpackage #:cl-mcp/src/server-instructions
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/registry
                #:enabled-tool-group-instructions)
  (:export #:+instructions-budget+
           #:+base-instructions+
           #:server-instructions))

(in-package #:cl-mcp/src/server-instructions)

(defparameter +instructions-budget+ 2048
  "Most characters the joined instructions may have, for any set of groups.
Claude Code truncates server instructions past 2,048 characters.")

(defparameter +base-instructions+
  "cl-mcp is a Common Lisp development server: a live SBCL REPL, ASDF loading and
structure-aware editing of Lisp source.

1. Call fs-set-project-root with the project's absolute path before any file tool.
2. For .lisp/.asd files use clgrep-search, lisp-read-file, lisp-edit-form and
lisp-patch-form, never shell grep/cat/sed: they parse the code and keep its comments.
3. An edit changes the file only. Run load-system before repl-eval or code-* tools see
it; run-tests reloads its test system itself.

Loop: explore (clgrep-search; lisp-read-file collapsed, then name_pattern) -> try
(repl-eval with package) -> persist (lisp-edit-form) -> verify (load-system, run-tests).

With the worker pool (default), repl-eval, load-system, run-tests, code-*,
clos-describe, lisp-macroexpand and inspect-object run in this session's own worker
image. Definitions made in repl-eval live only there. When a response says the worker
was lost, its state is gone: load-system again; old object ids are refused.
A defmethod's form_name includes its specializers: \"print-object ((o point) stream)\".
If a file no longer parses, lisp-check-parens shows where and the likely fix."
  "Instructions every client gets, whatever groups are on.  The three numbered
rules come first because a client may read only the opening.")

(defun server-instructions ()
  "Return the instructions for initialize: the base text, then the text of each
enabled tool group, separated by a blank line.

Computed per call from *ENABLED-TOOL-GROUPS*, the same setting tools/list
reads, so the two cannot describe different tool sets."
  (format nil "~A~{~%~%~A~}" +base-instructions+ (enabled-tool-group-instructions)))
```

- [ ] **Step 4: Register the cl-spec text**

In `src/tools/spec-tools.lisp`, add `register-tool-group-instructions` to an
`(:import-from #:cl-mcp/src/tools/registry ...)` clause of the defpackage, and insert after the
`in-package` form (before `(define-tool "spec-list"`):

```lisp
(register-tool-group-instructions
 :cl-spec
 "cl-spec tools are on (spec-list, spec-symbol, spec-describe, spec-check):
- load-system cl-spec/check-it, the application and its contract system first.
- Read the contract (spec-symbol, spec-describe) and take a baseline before editing.
- spec-check function= runs only the Function Spec; symbol= only the Properties
about the symbol. Run both.
- seed is a decimal string. To replay, copy the Replay: line.
- After an edit: load-system the primary system with clear_fasls=true, then the
contract system.
- Never weaken a contract to make a check pass; ask the user first.
- verified covers only what that call ran; report verification_gaps as given.")
```

Measured size of the drafts: base 1,158 characters, cl-spec 652, joined with the separator
about 1,812, which leaves about 236 characters of headroom under 2,048.

- [ ] **Step 5: Run the tests to verify they pass**

`run-tests` `{"system": "cl-mcp/tests/server-instructions-test"}`: all pass, with
`instructions-fit-the-budget` reporting the two lengths in its descriptions. Then
`{"system": "cl-mcp/tests/spec-tools-test"}` and `{"system": "cl-mcp/tests/define-tool-test"}`.

- [ ] **Step 6: Lint and commit**

```bash
mallet src/server-instructions.lisp src/tools/spec-tools.lisp tests/server-instructions-test.lisp
git add src/server-instructions.lisp src/tools/spec-tools.lisp tests/server-instructions-test.lisp
git commit -m "server-instructions: base guidance plus the cl-spec group's, within 2,048 characters"
```

---

### Task 3: Return the instructions from initialize

**Files:**
- Modify: `src/protocol.lisp` (defpackage import; `handle-initialize`)
- Modify: `tests/server-instructions-test.lisp`

**Interfaces:**
- Consumes: `server-instructions` (Task 2), `process-json-line`, `+supported-protocol-versions+`.
- Produces: the initialize result has a string `"instructions"`.

- [ ] **Step 1: Write the failing test**

Add `#:+supported-protocol-versions+` to the `cl-mcp/src/protocol` import, and
`(:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)`, then:

```lisp
(defun %initialize-result (version)
  (let ((*use-worker-pool* nil))
    (gethash "result"
             (parse (process-json-line
                     (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",~
                                  \"params\":{\"protocolVersion\":\"~A\"}}"
                             version))))))

(deftest initialize-returns-the-instructions
  (dolist (groups *group-settings*)
    (let ((*enabled-tool-groups* groups))
      (dolist (version +supported-protocol-versions+)
        (let ((result (%initialize-result version)))
          (ok (equal (server-instructions) (gethash "instructions" result))
              (format nil "~A with groups ~S" version groups)))))))
```

The FORMAT directive `~` followed by a newline skips the newline and the indentation after it, so
the JSON stays on one line.

- [ ] **Step 2: Run to verify it fails**

`run-tests` `{"system": "cl-mcp/tests/server-instructions-test"}`. Expected:
`initialize-returns-the-instructions` fails with `NIL` for `"instructions"`.

- [ ] **Step 3: Implement**

In the `#:cl-mcp/src/protocol` defpackage add:

```lisp
  (:import-from #:cl-mcp/src/server-instructions
                #:server-instructions)
```

In `handle-initialize`, patch the result (`lisp-patch-form`, `form_type` `defun`, `form_name`
`handle-initialize`):

old_text:
```lisp
                             (make-ht "name" "cl-mcp" "version" (version))
                             "capabilities" caps))))))
```

new_text:
```lisp
                             (make-ht "name" "cl-mcp" "version" (version))
                             "capabilities" caps
                             "instructions" (server-instructions)))))))
```

`instructions` is optional in every supported version's `InitializeResult` (2024-11-05 onward),
so it is sent for all of them.

- [ ] **Step 4: Run the tests**

`run-tests` for `cl-mcp/tests/server-instructions-test` and `cl-mcp/tests/protocol-test`: all
pass.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/protocol.lisp tests/server-instructions-test.lisp
git add src/protocol.lisp tests/server-instructions-test.lisp
git commit -m "protocol: send the server instructions from initialize"
```

---

### Task 4: Documentation

**Files:**
- Modify: `docs/tools.md` (a new `## Server instructions` section before `## Optional tool groups`)
- Modify: `README.md` (`### System Prompts`, around line 67)

- [ ] **Step 1: `docs/tools.md`**

Insert before `## Optional tool groups`:

```markdown
## Server instructions

`initialize` returns an `instructions` string: the base guidance (set the project root first,
edit Lisp with the Lisp tools, `load-system` after an edit, the worker pool) and, for each
enabled tool group, that group's guidance. It is built from the same group setting as
`tools/list`, so it never names a tool the client cannot see, and it is at most 2,048
characters for any combination of groups — the length at which Claude Code truncates it.

Clients read it once per connection. Clients differ in what they do with it: Claude Code,
Gemini CLI (trusted folders only), VS Code/Copilot, Goose, opencode and ChatGPT put it in
the model's context; Codex uses it as the description of the server's tools; Cline,
Continue and Zed ignore it. It is a summary; the full guides are in `prompts/`.
```

- [ ] **Step 2: `README.md`**

Replace the paragraph starting `Reference them from your project's CLAUDE.md` with:

```markdown
cl-mcp also sends a short summary of the first and third as its MCP server
instructions (see [Server instructions](docs/tools.md#server-instructions)), so
a client that honours them gets the essentials without any setup. Reference
the full prompts from your project's `CLAUDE.md` (or equivalent) when you want
the complete guide, or when your client ignores server instructions:
```

(Keep the code blocks and the cl-spec paragraph that follow.)

- [ ] **Step 3: Commit**

```bash
git add docs/tools.md README.md
git commit -m "docs: describe the server instructions and how they relate to prompts/"
```

---

### Task 5: Whole-branch verification

- [ ] **Step 1:** `repl-eval` `(asdf:compile-system :cl-mcp :force :all)` — no new warnings in
  the three changed source files.
- [ ] **Step 2:** Full lint:
  `mallet src/*.lisp src/*/*.lisp tests/*.lisp specs.lisp specs/*.lisp scripts/*.lisp`.
- [ ] **Step 3:** Full suite in a fresh process: `rove cl-mcp.asd`; count `;; testing '` lines and
  failures rather than trusting the exit code or the last summary.
- [ ] **Step 4:** End-to-end check against a fresh server image, both settings:

```bash
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}' \
  | MCP_NO_WORKER_POOL=1 ros run -s cl-mcp -e "(cl-mcp:run :transport :stdio)"
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}' \
  | MCP_NO_WORKER_POOL=1 MCP_ENABLE_TOOL_GROUPS=cl-spec ros run -s cl-mcp -e "(cl-mcp:run :transport :stdio)"
```

Expected: the first `instructions` has no `spec-check`; the second ends with the cl-spec block.
- [ ] **Step 5:** After merge, restart the MCP server in Claude Code and confirm the text appears
  under "MCP Server Instructions" for `cl-mcp` without truncation.
