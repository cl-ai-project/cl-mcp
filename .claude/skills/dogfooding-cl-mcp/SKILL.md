---
name: dogfooding-cl-mcp
description: Use when you want to stress-test cl-mcp tools against a realistic Common Lisp development workflow and collect concrete improvement feedback by building a throwaway medium-size project end-to-end.
---

# Dogfooding cl-mcp

## Overview

Build a real mid-size Common Lisp project with cl-mcp's own tools, watching for every rough edge along the way. The point is not the project — it is the feedback. Every retry, every confusing error, every tool that surprises you goes into the feedback file.

**Core principle:** Cheap, disposable projects that exercise the full cl-mcp tool surface produce better feedback than abstract review. Build, notice friction, record it, throw it away.

## When to Use

- User asks for dogfooding, feedback collection, or "try cl-mcp on a real project"
- You want to verify a recent cl-mcp change works in practice, not just in unit tests
- You are looking for P1/P2/P3-level improvement candidates to feed into the next PR cycle

**Do NOT use** for: scaffolding a project the user actually wants to keep, or for unrelated CL work.

## Workflow

### 1. Workspace setup (isolate the throwaway project)

**Never scaffold inside cl-mcp's own git tree.** The generated files would show up in `git status` and could be committed by accident.

```
mkdir -p ~/cl-mcp-experiments          # one-time; outside cl-mcp checkout
fs-set-project-root path=~/cl-mcp-experiments
fs-get-project-info                    # confirm
```

Note the original cl-mcp project root before switching. You will restore it at the end.

**Hydrate deferred tool schemas** before any tool call with boolean/integer parameters.
Without this, calls like `load-system force=true` or `inspect-object id=N` will fail
with misleading `must be boolean`/`must be integer` errors (harness-side issue, not cl-mcp):

```
ToolSearch select:mcp__cl-mcp__lisp-read-file,mcp__cl-mcp__load-system,mcp__cl-mcp__repl-eval,mcp__cl-mcp__inspect-object,mcp__cl-mcp__lisp-edit-form,mcp__cl-mcp__clgrep-search,mcp__cl-mcp__code-find,mcp__cl-mcp__code-describe,mcp__cl-mcp__code-find-references,mcp__cl-mcp__pool-kill-worker
```

### 2. Scaffold with `project-scaffold`

Call `project-scaffold` once. Pick a `name` that does not exist yet under `scaffolds/`. Save the response — note the `absolute_path` and `files` list.

⚠️ **The text summary returned by `project-scaffold` currently omits the `next_steps` array** (the field exists in the structured response but is not rendered in the visible content). Do not wait for it. Go straight to step 3.

### 3. Register the scaffold with ASDF (before first load-system)

ASDF does not auto-discover projects under `~/cl-mcp-experiments/`. The `load-system` tool will fail with `Component "<name>" not found` unless you prime ASDF first:

```
repl-eval code='(asdf:load-asd "<absolute-path-from-step-2>/<name>.asd")'
```

Only AFTER that call does `load-system system=<name>` work. This is a one-time prime per scaffold.

### 4. Build out medium complexity

Target shape (medium = 15-30 minutes of work):

- 3-5 source files under `src/`
- 2-4 test files under `tests/`
- ~10 Rove tests across all test files
- At least one of: `defclass` + `defmethod`, `defstruct`, `define-condition`, a small `defmacro`, multi-file inter-package `:import-from`

Use `fs-write-file` for **new** files, `lisp-edit-form` / `lisp-patch-form` for **existing** files (parinfer-safe). Register extra test packages in the scaffold's `.asd` `:depends-on` list and re-`load-system` after each new file.

**Tool parameter gotchas** (easy to trip on):
- `lisp-edit-form` / `lisp-patch-form` use `file_path`, NOT `path`. Reading tools (`lisp-read-file`, `fs-read-file`) use `path`.
- `lisp-edit-form content` must contain **exactly one top-level form**. To insert multiple forms, chain `insert_after` calls.
- `code-find` requires `symbol`, NOT `name`. When the symbol is not in `CL-USER`, also pass `package`.
- `lisp-edit-form` / `lisp-patch-form` accept `form_type: "defsystem"` for `.asd` files, NOT `"asdf:defsystem"`.

### 5. Exercise the full tool surface

Deliberately try each tool at least once so friction surfaces:

`clgrep-search`, `code-find`, `code-describe`, `code-find-references`, `inspect-object` on a non-primitive result, `lisp-read-file` with `name_pattern`, `repl-eval` with an intentional error to see `error_context`, `run-tests` on both a passing and a deliberately-failing assertion.

### 6. Record feedback as you go

Keep a running list. Append to the feedback file at the end of the cycle, not at the end of the session.

**Feedback file location** — pick the variant that matches how you work:

- **Solo / private notes** (default for most contributors): `~/.claude/memory/cl-mcp-feedback.md`. Contains your personal observations; never committed to cl-mcp's git tree.
- **Team-shared** (if the project has agreed on a shared file): e.g. `<cl-mcp-repo>/claudedocs/dogfooding-feedback.md`. Visible to other contributors; avoid sensitive material and un-redacted paths.
- **Project-specific override** — if the user of this skill has said "record feedback to X", use X and skip the defaults.

In all cases: **append, never overwrite**. Create the file with `fs-write-file` if it does not exist; afterwards append via shell heredoc or `repl-eval`.

**Format:** add a new dated section (`## Session YYYY-MM-DD — <project-name>`). Categorize every item as P1/P2/P3:
- **P1** — real bugs, silent wrong results, data-loss risk, or features that block the workflow
- **P2** — rough edges, token waste, confusing error messages, docs mismatches
- **P3** — nits, scaffold template polish, nice-to-haves

For each item: Problem (one line), Reproduction or symptom, Suggested fix.

### 7. Cleanup

At the end of the cycle:

1. `fs-set-project-root path=<original-cl-mcp-path>` to restore the working context
2. Report generation stats (project name, location, test count, feedback items count)
3. Leave the throwaway project on disk — it is cheap storage and the next cycle can reuse `~/cl-mcp-experiments/` as the parent

Do **not** `git add` anything in cl-mcp's checkout.

## Known pitfalls (check before recording as new bugs)

These are documented pitfalls that have tripped previous dogfooding runs. If you hit them, you can cite the existing feedback instead of opening duplicates.

| Symptom | Cause | Workaround |
|---|---|---|
| `run-tests` on aggregate `<name>/tests` reports `Passed: 0, Failed: 0` with `✓ PASS` despite tests actually running | `run-tests` result extractor does not handle the scaffold's `:perform (test-op ...)` rove:run shape | Run each sub-package individually (`run-tests system=<name>/tests/foo-test`) OR verify via `(asdf:test-system :<name>)` in repl-eval |
| `run-tests` fails with opaque `COMPILE-FILE-ERROR while compiling ...` after you edited a `defpackage` | SBCL package-variance warning escalated to error; cached worker state | `pool-kill-worker` then `load-system` to get a fresh image |
| `lisp-edit-form` or `lisp-patch-form` on a `.asd` file rejects `form_type: "asdf:defsystem"` | Tool matches on unqualified symbol name | Use `form_type: "defsystem"` |
| `code-find` returns `symbol is required` when you pass `name:` | Parameter name is `symbol`, not `name` | Check the tool schema: the required key is `symbol` |
| `fs-list-directory` hides `.gitignore` and other dotfiles | Default behavior filters `*hidden-prefixes*` | Pass `show_hidden: true` (added in PR #94) |
| `lisp-edit-form` on a defmethod with `#:` specializers says "not found" with plain `form_name` | Was a bug before PR #94; fixed by `%strip-hash-colon` normalization | Should work now; if it still fails, file a new issue |
| `load-system system=<name>` fails with `Component "<name>" not found` immediately after `project-scaffold` | ASDF has not loaded the scaffold's `.asd` yet | `repl-eval '(asdf:load-asd "<absolute-path>/<name>.asd")'` once, then `load-system` works — see step 3 |
| `project-scaffold` text response does not contain the `next_steps` array | Response builder does not render `next_steps` in `content[].text` | Use the `absolute_path` from the structured response and prime ASDF manually per step 3 |
| `inspect-object id=<N>` returns `id must be an integer` even when N is clearly an integer | Deferred tool schema not hydrated (harness-side, not cl-mcp) | Run ToolSearch hydration batch from step 1 before first use |
| `lisp-edit-form content=<multi-form>` rejects with `content must contain exactly one top-level form` | Tool only accepts one form per call | Chain multiple `insert_after` calls, one form each |
| `clgrep-search form_types=[...]` filter returns `[]` even when matches exist in the un-filtered query | Filter is currently unreliable | Omit `form_types` and post-filter client-side, OR prefer `code-find` / `code-describe` for exact lookups |
| `clgrep-search` signature field is a 4KB blob with the whole form body | Fixed: results are now deduplicated by (file, form-start-byte) with `match_lines` array | Should be resolved; if still noisy, use `limit` param or targeted `lisp-read-file name_pattern=...` |
| `lisp-edit-form` has no way to remove a form from a file | Fixed: `operation: "delete"` is now available (content param not needed) | Use `lisp-edit-form` with `operation: "delete"` to remove scaffold stubs like `defun greet` |
| `load-system` after changing package exports shows noisy "also exports" warnings | SBCL package-variance; stale worker image | `pool-kill-worker` then `load-system` for a clean image. `load-system` now shows a hint when this happens |

## Success criteria

You are done with one cycle when:

- [ ] The throwaway project has all its generated Rove tests green (verified per-package OR via `asdf:test-system`, **not** just via the buggy aggregate `run-tests` path)
- [ ] At least one edited Lisp file was sanity-checked with `lisp-check-parens` (cheap and catches `lisp-patch-form` drift early)
- [ ] At least **5 feedback items** were **actually appended** to the chosen feedback file (verify with a `fs-read-file` or shell `tail` — the "I'll record it later" trap is real)
- [ ] Feedback is categorized P1/P2/P3 under a dated section heading
- [ ] Project root is restored to cl-mcp's original location
- [ ] Nothing in `git status` under cl-mcp references the throwaway project

## Anti-patterns

- **Scaffolding inside cl-mcp's checkout.** The generated files will taint `git status`. Always set project root to an outside directory first.
- **Building a project you intend to keep.** This is a feedback-gathering exercise; grab shallow breadth (lots of tool calls) over deep polish.
- **Trusting `✓ PASS / Passed: 0, Failed: 0` on the aggregate test system.** See the pitfalls table — this is a known silent bug.
- **Recording only tool bugs.** Capture UX friction too: confusing errors, missing defaults, unnecessary retries. Those become P2/P3 items.
- **Skipping the "try every tool" step.** If you only use `lisp-edit-form` and `run-tests`, you only produce feedback on those two tools.

## Output when asked to run a cycle

When a cycle completes, summarize:

1. **Project:** name + absolute path
2. **Size:** N src files, N test files, N Rove tests, what CL features exercised (defclass, defmethod, etc.)
3. **Test status:** per-package counts (avoid the aggregate trap)
4. **Feedback recorded:** total count, P1/P2/P3 breakdown
5. **Procedural pitfalls:** anything that took more than one try (these are usually the best P1/P2 candidates)
6. **Cleanup:** project root restored ✓, cl-mcp `git status` clean ✓
