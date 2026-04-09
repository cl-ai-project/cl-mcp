# Design: `project-scaffold` Tool

**Date:** 2026-04-09
**Status:** Approved (brainstorming phase complete, pending implementation plan)
**Author:** Satoshi Imai (with cl-mcp assistant)

## Overview

Add a new cl-mcp tool, `project-scaffold`, that generates a minimal Common Lisp
project embodying cl-mcp's recommended structure in a single atomic call.
The generated project uses `package-inferred-system`, Rove for testing, and
ships with `CLAUDE.md` / `AGENTS.md` templates wired to cl-mcp's existing
prompts via relative `@`-include references.

The primary motivation is to accelerate a dog-fooding feedback loop: by making
it trivially cheap to generate sample projects, the team can exercise cl-mcp's
full tool surface (`lisp-edit-form`, `run-tests`, `clgrep-search`, etc.) across
many throwaway codebases and surface real usage issues faster.

## Goals

1. **Accelerate the feedback loop.** One call → a ready-to-develop project.
2. **Serve as the canonical reference.** A single template that embodies the
   "cl-mcp way" of structuring a project, documented implicitly by the code it
   emits.
3. **Respect YAGNI.** Single template, single atomic tool, minimal parameters.
   Leave room for future extension but do not pre-build it.

## Non-Goals (explicitly deferred)

- Multiple template variants (library / CLI / web-service).
- Caller-supplied extra `:depends-on` lists.
- Scaffolding into existing (non-empty) directories.
- Automatic `fs-set-project-root` after generation.
- Symbolic linking into `~/.roswell/local-projects/`.
- Automatic ASDF registration in the worker. The tool returns `next_steps`
  instructions; the agent executes them explicitly via `repl-eval` /
  `load-system`.

## Success Criteria

- Generated scaffolds live under `<project-root>/scaffolds/<name>/` and are
  fully accessible through the existing `fs-*` and Lisp-aware tools without
  changing `project-root`.
- Following the returned `next_steps` verbatim, an agent can go from
  `project-scaffold` → `asdf:load-asd` → `load-system` → `run-tests` with a
  green result.
- The scaffold's bundled Rove test suite is **green on first run** (green
  start, not red start).

---

## Architecture

### Process placement

`project-scaffold` runs **inline in the parent process**, alongside the other
filesystem tools (`fs-write-file`, `fs-list-directory`). Rationale:

- The tool's work is pure file creation; it does not need a Lisp image.
- It inherits the project-root guardrail from the existing `fs-*` machinery.
- Worker-side ASDF operations (`load-asd`, `load-system`) are intentionally
  left to the caller to preserve the existing parent/worker boundary. The
  tool returns explicit next-step instructions so the agent can continue
  unassisted.

### Source layout

```
src/
├── project-scaffold.lisp            # define-tool entry, atomic I/O, response building
├── project-scaffold-core.lisp       # pure: validation, rendering, manifest, path math
├── project-scaffold-templates.lisp  # template string constants (7 files)
└── tools/all.lisp                   # :import-from #:cl-mcp/src/project-scaffold

tests/
└── project-scaffold-test.lisp       # Rove: validators, renderer, manifest, integration, e2e
```

The three-file split isolates effectful code (`project-scaffold.lisp`) from
pure logic (`project-scaffold-core.lisp`) and bulk string literals
(`project-scaffold-templates.lisp`). Pure functions carry the bulk of the
testability; the I/O layer stays thin.

Template content is kept as Lisp string constants (not external `.tmpl`
files) because:

- cl-mcp is self-hosted through `lisp-edit-form` / `clgrep-search`, so Lisp
  source is the friendliest home for the templates.
- ASDF loading stays simple (no runtime file reads at scaffold time).
- YAGNI: with ~7 templates, a constants file is readable and maintainable.

### Data flow

```
agent
  ↓ tools/call { name: "project-scaffold", arguments: { name, description, author, license, destination } }
protocol.lisp dispatch
  ↓
project-scaffold.lisp::project-scaffold
  │  1. validate arguments  (validate-project-name, validate-destination, validate-text-fields)
  │  2. collision check     (target directory must not exist)
  │  3. plan-scaffold       (build [(relative-path . content)] manifest)
  │  4. atomic write        (temp directory → rename on success)
  │  5. build response      ({created, path, absolute_path, files, next_steps})
  ↓
protocol → agent
```

### Integration with existing wiring

- `cl-mcp.asd` — no change (`package-inferred-system` auto-discovers new
  `.lisp` files).
- `src/tools/all.lisp` — add `:import-from #:cl-mcp/src/project-scaffold
  #:project-scaffold` so load-time registration fires.
- `main.lisp` — export `project-scaffold` per the project's convention for
  public tool entry points.
- `tests.lisp` — add `cl-mcp/tests/project-scaffold-test` to the aggregate
  test package's `:import-from` list.
- `docs/tools.md` — add a reference section for the new tool.

---

## Input Parameters

| Parameter | Required | Description | Default |
|---|---|---|---|
| `name` | **yes** | Project name (lisp-case). Becomes ASDF system name, package prefix, filesystem folder. | — |
| `description` | no | One-line description. Embedded in `.asd` and `README.md`. | `"A Common Lisp project scaffolded by cl-mcp."` |
| `author` | no | `.asd` `:author` field. | `"Unknown"` |
| `license` | no | `.asd` `:license` field. | `"MIT"` |
| `destination` | no | Parent directory (relative to project root) into which `<name>/` is created. | `"scaffolds"` |

### Validation rules

| Field | Rule | Example failure |
|---|---|---|
| `name` | non-empty, `^[a-z][a-z0-9-]*$`, length 1–64 | `"FooLib"`, `"1foo"`, `"foo_lib"` |
| `destination` | relative path, no `..` traversal, resolves within project root | `"/tmp/foo"`, `"../outside"` |
| `author` / `description` / `license` | no newlines | `"Name\ninjected"` |

Non-newline special characters in string fields are passed through verbatim.
`project-scaffold` treats its caller as trusted (same trust boundary as
`fs-write-file` and `repl-eval`); newline rejection is solely to prevent
accidental `.asd` corruption.

### Collision handling

If `<project-root>/<destination>/<name>/` already exists, the tool returns an
error and does not touch the filesystem. There is **no `force` flag** and **no
auto-suffix**. Callers that want to generate many scaffolds choose distinct
names themselves.

---

## File Manifest

The generated project contains seven files:

```
<project-root>/<destination>/<name>/
├── <name>.asd
├── CLAUDE.md
├── AGENTS.md
├── README.md
├── .gitignore
├── src/
│   └── main.lisp
└── tests/
    └── main-test.lisp
```

### Placeholder variables

Templates use `{{var}}` markers, substituted via `cl-ppcre:regex-replace-all`:

| Variable | Source |
|---|---|
| `{{name}}` | `name` argument |
| `{{description}}` | `description` argument |
| `{{author}}` | `author` argument |
| `{{license}}` | `license` argument |
| `{{parent-prompts}}` | Computed relative path from `<destination>/<name>/` back to `<project-root>/prompts/` |

`{{parent-prompts}}` is computed dynamically from the number of path segments
in `<destination>/<name>`. For the default `destination="scaffolds"`, the
value is `"../../prompts"`. For deeper destinations (e.g., `"work/samples"`),
the path extends accordingly (`"../../../prompts"`).

### Template contents

#### `<name>.asd`

```lisp
;;;; {{name}}.asd

(asdf:defsystem "{{name}}"
  :class :package-inferred-system
  :description "{{description}}"
  :author "{{author}}"
  :license "{{license}}"
  :version "0.1.0"
  :depends-on ("{{name}}/src/main")
  :in-order-to ((test-op (test-op "{{name}}/tests"))))

(asdf:defsystem "{{name}}/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "{{name}}"
               "{{name}}/tests/main-test")
  :perform (test-op (o c) (uiop:symbol-call :rove :run c)))
```

#### `CLAUDE.md`

```markdown
# CLAUDE.md

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
- Tests: `run-tests` with `{"system": "{{name}}/tests"}`

## Testing

```lisp
;; From repl-eval
(asdf:test-system :{{name}})
```

## Repository Structure

`src/`      Source code (package-inferred-system)
`tests/`    Rove test suites
```

#### `AGENTS.md`

```markdown
# Repository Guidelines

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
```

#### `README.md`

```markdown
# {{name}}

{{description}}

## Usage

\`\`\`lisp
(asdf:load-system :{{name}})
({{name}}/src/main:greet "world")
;; => "Hello, world!"
\`\`\`

## Tests

\`\`\`lisp
(asdf:test-system :{{name}})
\`\`\`

## License

{{license}}
```

#### `.gitignore`

```
*.fasl
*.ufasl
*.x86f
*.cfasl
.asdf-cache/
```

#### `src/main.lisp`

```lisp
;;;; src/main.lisp

(defpackage #:{{name}}/src/main
  (:use #:cl)
  (:export #:greet))

(in-package #:{{name}}/src/main)

(defun greet (who)
  "Return a friendly greeting for WHO."
  (format nil "Hello, ~A!" who))
```

#### `tests/main-test.lisp`

```lisp
;;;; tests/main-test.lisp

(defpackage #:{{name}}/tests/main-test
  (:use #:cl #:rove)
  (:import-from #:{{name}}/src/main
                #:greet))

(in-package #:{{name}}/tests/main-test)

(deftest greet-test
  (testing "greet returns a hello string"
    (ok (equal "Hello, world!" (greet "world")))))
```

---

## Atomicity and Error Handling

### Atomic generation: temp-then-rename

```
1. temp = <destination>/.tmp-project-scaffold-<uuid>/
2. Write all files into temp.
3. If any step fails, recursively delete temp and return error.
4. Rename temp → <destination>/<name>/   (atomic on the same filesystem)
5. Build success response.
```

This mirrors the pattern already used by `fs-write-file` and ensures that a
partially-written scaffold never appears on disk. If cleanup itself fails
(permissions, etc.), the original error is still returned and a
`cleanup_warning` field is appended to the error payload.

### Error response format

Follows the existing JSON-RPC error conventions used by other tools. Example:

```json
{
  "error": {
    "code": -32602,
    "message": "Invalid project name",
    "data": { "field": "name", "got": "FooLib", "expected_pattern": "^[a-z][a-z0-9-]*$" }
  }
}
```

### Failure matrix

| Scenario | Behavior |
|---|---|
| Invalid `name` | Error before touching filesystem |
| Destination escapes project root | Error before touching filesystem |
| Target directory exists | Error before touching filesystem |
| File write fails mid-generation | Delete temp, return error; target directory never created |
| Rename fails | Delete temp, return error |
| Response construction fails | Files already renamed; log and return generic internal error |

### Success response

```json
{
  "created": true,
  "path": "scaffolds/foo-lib",
  "absolute_path": "/abs/.../cl-mcp/scaffolds/foo-lib",
  "files": [
    "foo-lib.asd",
    "CLAUDE.md",
    "AGENTS.md",
    "README.md",
    ".gitignore",
    "src/main.lisp",
    "tests/main-test.lisp"
  ],
  "next_steps": [
    "To register with ASDF: run repl-eval with (asdf:load-asd \"/abs/.../cl-mcp/scaffolds/foo-lib/foo-lib.asd\")",
    "To load: run load-system with {\"system\": \"foo-lib\"}",
    "To test: run run-tests with {\"system\": \"foo-lib/tests\"}",
    "To edit: use lisp-edit-form with paths under scaffolds/foo-lib/"
  ]
}
```

`next_steps` is a list of human-readable English strings. Agents parse them
as guidance, not as an RPC contract.

---

## Testing Strategy (Rove)

Test file: `tests/project-scaffold-test.lisp`
Package: `cl-mcp/tests/project-scaffold-test`
Approach: TDD. Pure-function tests first, side-effect tests second, end-to-end
last. Each layer's failures point at its specific concern.

### Categories

**1. Input validation (pure)**

- Accept: `"foo-lib"`, `"a"`, `"foo-123"`
- Reject: empty, `"FooLib"`, `"1foo"`, `"foo_lib"`, `"foo.lib"`, `"foo/lib"`,
  65-char name
- `destination` rejects absolute paths, parent traversal
- `author` / `description` / `license` reject newlines

**2. Template rendering (pure)**

- `{{name}}` substitution
- All placeholders resolved in each template
- Unknown `{{...}}` markers pass through unchanged (guards against typos)
- `compute-parent-prompts-path` for `"scaffolds"` → `"../../prompts"`,
  `"work/samples"` → `"../../../prompts"`, `"a/b/c"` → `"../../../../prompts"`

**3. Manifest generation (pure)**

- `plan-scaffold` returns 7 entries
- Entry paths relative to the target directory
- All contents are strings
- Key placeholders (name, author) visible in generated `.asd` via `search`

**4. Integration (filesystem)**

Each test sets up a temporary project root (under OS `/tmp/`) and tears it
down afterwards. Scaffolds are never written under the real `cl-mcp/scaffolds/`
during tests.

- `scaffold-creates-all-files` — all 7 files exist after success
- `scaffold-asd-is-readable` — generated `.asd` reads without error via
  `with-open-file` + `read`
- `scaffold-lisp-files-have-balanced-parens` — `lisp-check-parens` internal
  call succeeds on `src/main.lisp` and `tests/main-test.lisp`
- `scaffold-rejects-existing-target` — second call with same `name` errors
- `scaffold-cleans-temp-on-failure` — no `.tmp-project-scaffold-*` remnant
  after induced failure
- `scaffold-success-response-has-expected-keys`
- `scaffold-next-steps-is-non-empty-string-list`

**5. End-to-end**

- `scaffold-then-load-then-test-succeeds` — in a temp project root: generate →
  `(asdf:load-asd ...)` → `(asdf:load-system ...)` → `(asdf:test-system ...)`
  → assert green. Uses `asdf:clear-system` before/after to avoid polluting
  the host image.

### Isolation concerns

- Tests dynamically bind `cl-mcp/src/project-root` (or equivalent) to a temp
  directory; they never mutate the real project root.
- Temp directories include UUIDs to allow parallel Rove execution.
- Every test cleans up via Rove's teardown hooks even on failure.

### Pre-PR gates

- `(asdf:compile-system :cl-mcp :force t)` — zero new warnings
- `mallet src/project-scaffold*.lisp` — lint clean
- `run-tests {"system": "cl-mcp/tests"}` — no regressions
- Manual smoke test after server restart: `project-scaffold demo-lib` →
  `asdf:load-asd` → `load-system` → `run-tests` → all green

---

## Known Risks and Mitigations

| Risk | Mitigation |
|---|---|
| `{{parent-prompts}}` relative path wrong for deep `destination` values | Unit tests cover multiple depths (`scaffolds`, `work/samples`, `a/b/c`) |
| Thread safety around `cl-mcp/src/project-root` in concurrent tests | Use the same dynamic-binding pattern already used by existing tests |
| Future multi-template expansion breaking pure functions | `plan-scaffold` takes `:template-kind :default` keyword from day one so new variants slot in additively |
| ASDF state pollution from e2e test | `asdf:clear-system :<generated-name>` before and after |
| `format` directives (`~`) inside `description` fields | Renderer uses `cl-ppcre:regex-replace-all` exclusively; `format` never sees user text. Covered by an explicit test |
| Generated scaffolds accidentally committed to cl-mcp's own repo | Add `scaffolds/` to `.gitignore` discussion at implementation time (not forced by this design, caller decides) |

---

## Definition of Done

- [ ] All Rove tests in `tests/project-scaffold-test.lisp` green
- [ ] `run-tests {"system": "cl-mcp/tests"}` — no regressions
- [ ] `(asdf:compile-system :cl-mcp :force t)` — zero new warnings
- [ ] `mallet src/project-scaffold*.lisp` — lint clean
- [ ] e2e test confirms generate → load-asd → load-system → test-system pipeline
- [ ] `docs/tools.md` updated with a `project-scaffold` reference section
- [ ] Manual post-restart smoke test passes

---

## Out of scope for this design

This document captures the approved design. The implementation plan
(concrete step ordering, checkpoints, review gates) lives in a separate
plan document generated via the `writing-plans` skill.
