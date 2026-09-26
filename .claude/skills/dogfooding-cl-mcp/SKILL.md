---
name: dogfooding-cl-mcp
description: Use when you want to stress-test cl-mcp tools against a realistic Common Lisp development workflow and collect concrete improvement feedback by building a throwaway medium-size project end-to-end.
---

# Dogfooding cl-mcp

## Overview

Build a real mid-size Common Lisp project with cl-mcp's own tools, watching for every rough edge along the way. The point is not the project — it is the feedback. Every retry, every confusing error, every tool that surprises you goes into the ledger; what survives triage goes into the feedback file.

**Core principle:** Cheap, disposable projects that exercise the full cl-mcp tool surface produce better feedback than abstract review. Build, notice friction, find out which layer it lives in, record what that produced, throw the project away.

## When to Use

- User asks for dogfooding, feedback collection, or "try cl-mcp on a real project"
- You want to verify a recent cl-mcp change works in practice, not just in unit tests
- You are looking for P1/P2/P3-level improvement candidates to feed into the next PR cycle

**Do NOT use** for: scaffolding a project the user actually wants to keep, or for unrelated CL work.

## Workflow

### 1. Workspace setup

Scaffold projects live under `experiments/` inside the cl-mcp checkout.
This directory is listed in `.gitignore`, so generated files never appear
in `git status` and cannot be committed by accident.  Build and edit with the
root left at the cl-mcp checkout; step 5 switches it to the experiment for the
reference scan alone, and switches it straight back.

```
fs-set-project-root path=.            # ensure project root is cl-mcp
fs-get-project-info                    # confirm
```

**Hydrate deferred tool schemas** before any tool call with boolean/integer parameters.
Without this, calls like `load-system force=true` or `inspect-object id=N` will fail
with misleading `must be boolean`/`must be integer` errors (harness-side issue, not cl-mcp):

```
ToolSearch select:mcp__cl-mcp__lisp-read-file,mcp__cl-mcp__load-system,mcp__cl-mcp__repl-eval,mcp__cl-mcp__inspect-object,mcp__cl-mcp__lisp-edit-form,mcp__cl-mcp__clgrep-search,mcp__cl-mcp__code-find,mcp__cl-mcp__code-describe,mcp__cl-mcp__code-find-references,mcp__cl-mcp__pool-kill-worker
```

### 2. Scaffold with `project-scaffold`

Call `project-scaffold` once with `destination: "experiments"`. Save the response — note the `absolute_path`, the `files` list, and the `framework` it echoes back.

**Pick a test framework.** The default is Rove; pass `framework: "fiveam"` for a
FiveAM project instead. Same layout either way — only the `.asd` `:depends-on`
entry, its `test-op` hook and `tests/main-test.lisp` differ. Alternate between
cycles: a FiveAM cycle drives `run-tests` code paths a Rove cycle never reaches
(suite discovery, FiveAM failure-detail extraction), and framework-specific
rough edges only surface there. See "Working in a FiveAM project" below for the
handful of things that differ once you start adding tests.

**Pick a name ASDF does not already know.** Before calling `project-scaffold`,
verify with ASDF itself, not with a directory listing:

```
repl-eval code='(asdf:find-system "<candidate-name>" nil)' package=CL-USER
```

If the result is anything other than `NIL`, pick a different name. Roswell
auto-registers every `.asd` under `~/.roswell/local-projects/**` (and some
external paths like `~/cl-mcp-experiments/`), which can include stale
scaffolds from prior dogfood sessions that a simple `ls experiments/` will
miss. If you create a collision, `project-scaffold` will still report success
and `load-system` will still return green, but ASDF will silently resolve to
the **stale** `.asd` — your brand-new tests will be invisible and the test
runner will report counts from ghost tests. The symptom is baffling; avoid it
by asking ASDF up front.

`project-scaffold` does **not** register the generated `.asd` with ASDF — it
only writes files. Registering would mean loading generated code in the cl-mcp
parent process, outside worker isolation, so `load-system` is what registers
the system, in the worker. Sanity-check the resolution after your first
`load-system`:

```
repl-eval code='(asdf:system-source-file (asdf:find-system "<name>"))'
```

The returned path MUST match the `absolute_path` from the scaffold response.
If it does not, you hit a collision despite the check above — rename and
re-scaffold.

### 3. First load (this is what registers the system)

After scaffolding, run `load-system system=<name>`; that call is what registers
the system with ASDF, in the worker. If it fails with `Component "<name>" not
found` — for instance because the scaffold landed outside a directory ASDF
scans, or after a worker restart discarded the registration — register it
explicitly:

```
repl-eval code='(asdf:load-asd "<absolute-path-from-step-2>/<name>.asd")'
```

### 4. Build out medium complexity

Target shape (medium = 15-30 minutes of work):

- 3-5 source files under `src/`
- 2-4 test files under `tests/`
- ~10 tests across all test files (Rove `deftest`, or FiveAM `test` forms)
- At least one of: `defclass` + `defmethod`, `defstruct`, `define-condition`, a small `defmacro`, multi-file inter-package `:import-from`

Use `fs-write-file` for **new** files, `lisp-edit-form` / `lisp-patch-form` for **existing** files (parinfer-safe). Register extra test packages in the scaffold's `.asd` `:depends-on` list and re-`load-system` after each new file.

**Tool parameter gotchas** (easy to trip on):
- `lisp-edit-form` / `lisp-patch-form` use `file_path`, NOT `path`. Reading tools (`lisp-read-file`, `fs-read-file`) use `path`.
- `lisp-edit-form content`: `replace` takes **exactly one top-level form**; `insert_before`/`insert_after` take one or more, inserted in order as one block. To replace one form with several, `replace` with the first and `insert_after` the rest in one call.
- `code-find` requires `symbol`, NOT `name`. When the symbol is not in `CL-USER`, also pass `package`.
- `lisp-edit-form` / `lisp-patch-form` accept `form_type: "defsystem"` for `.asd` files, NOT `"asdf:defsystem"`.

**Working in a FiveAM project** (skip if you scaffolded with Rove):
- Each new test file needs **two** things, and skipping either fails quietly:
  1. `(def-suite <file>-suite :in :<name>)` then `(in-suite <file>-suite)`.
     Without `:in`, the generated `test-op` never reaches the suite even though
     `run-tests` still does — see the pitfalls table.
  2. `(:import-from #:<name>/tests/main-test)` in the `defpackage`. `:in`
     resolves its parent at load time, so the root-suite file has to load
     first; this clause is what makes ASDF guarantee it. Relying on the `.asd`
     `:depends-on` order instead is what produces the vanishing-test-count row
     in the pitfalls table.
- After adding suites, cross-check the two runners once. Count *tests*, not
  checks — `(length (fiveam:run ...))` returns one entry per `is`, so it is
  larger than `run-tests`' number and comparing them directly is misleading:

  ```lisp
  (length (remove-duplicates
           (mapcar (lambda (r) (fiveam::test-case r))
                   (let ((fiveam:*test-dribble* (make-broadcast-stream)))
                     (fiveam:run :<name>)))))
  ```

  That is what `test-op` covers, and it must equal `run-tests`'
  `passed + failed + pending`.
- `lisp-edit-form` addresses a FiveAM test with `form_type: "test"` and the
  test's name; a suite with `form_type: "def-suite"` and the suite name minus
  its `:` (`form_name: "my-project"` matches `(def-suite :my-project ...)`).
- `run-tests` needs no `framework` argument — it reads the generated
  `:depends-on`. Selecting one test still works: `tests: ["<pkg>::<test-name>"]`.
- FiveAM's `is` wraps a whole form and takes the message afterwards:
  `(is (= 3 (add 1 2)) "adds")`, not Rove's `(ok ... "adds")` shape.

### 5. Exercise the full tool surface

Deliberately try each tool at least once so friction surfaces:

`clgrep-search`, `code-find`, `code-describe`, `code-find-references`, `inspect-object` on a non-primitive result, `lisp-read-file` with `name_pattern`, `repl-eval` with an intentional error to see `error_context`, `run-tests` on both a passing and a deliberately-failing assertion.

**The search tools cannot see `experiments/` from the cl-mcp root.**
`clgrep-search` and `code-find-references`' source scan collect files the same
way, and that collector honours the `.gitignore` **of the root it is handed**.
cl-mcp's own `.gitignore` lists `experiments/`, so a search rooted at the
cl-mcp checkout answers `0 matches` for a symbol that plainly exists in your
project. That is the layout working as designed, not a tool bug. Reach the
project like this:

| Tool | How to reach the throwaway project |
|---|---|
| `clgrep-search` | Pass `path: "experiments/<project>"`. The `.gitignore` that then applies is the scaffold's own, which lists only fasls |
| `code-find-references` | It has no scan-root parameter. `fs-set-project-root <absolute_path from step 2>` for the duration of step 5, then set it back to the cl-mcp checkout |

While the root is switched, every path argument (`lisp-edit-form file_path`,
`fs-read-file path`) is relative to the experiment and `clgrep-search` needs no
`path`. Restore the root before step 7, which reads `git status` in cl-mcp.
(`fs-set-project-root` asks for the directory you are working in; for the
length of this step that is the experiment, so the call is in scope.)

### 6. Triage each candidate, then record

Friction you notice is a **candidate**, not a feedback item. It becomes a
feedback item once you can name the layer it lives in. The number of candidates
is an observation; the number of promoted items is whatever survives triage,
and **zero promoted items is a complete, reportable cycle**.

Keep a running ledger from the first candidate on — one row each, written as
you go, not reconstructed at the end:

```text
| # | What I saw | Layer | Evidence | Promoted? |
```

**Assigning the layer.** Look at what the candidate is actually about:

- **cl-mcp's own surface** — an error message you could not act on, a parameter
  name the docs contradict, a missing default, a schema that refuses a
  reasonable value, output too long to use. Layer is `tool` by inspection.
  Promote it; no control run needed.
- **Lisp behaviour** — a form was rejected, a value came back wrong, a condition
  was not caught, ASDF or the test framework acted oddly. **Run the control
  before promoting.** Without one the row is `unverified` and stays in the
  ledger.

**The control run.** The same behaviour, the same forms, outside cl-mcp
entirely. This is a shell command by definition — it is how this step is
performed, and it is allowed here:

```bash
ros run --non-interactive --eval '(progn <the smallest form that shows it>)'
```

| Control run result | Layer | Promote? |
|---|---|---|
| Same as the worker | `ANSI CL` / `ASDF` / `Rove`/`FiveAM` / `project code` | No. Ledger row carrying the control output |
| Differs — plain SBCL is fine, the worker is not | `worker` | Yes. Paste both outputs |
| Cannot be built, because the repro needs a cl-mcp tool | `tool` | Yes |

Two more checks, both cheap:

- **Spec check.** If the claim is "CL should have accepted this", confirm with
  `clhs-lookup` and quote the line. `define-condition`'s options are
  `:default-initargs`, `:documentation` and `:report` — `:format-control` is not
  one of them, a bare slot name is legal, and `defstruct` generates
  `make-<name>` rather than a `make-instance` initarg protocol. A rejection the
  standard mandates is not a cl-mcp finding.
- **Fresh worker.** `pool-kill-worker reset=true`, then repeat. Reproduces only
  on the warm worker? That *is* the finding — say so in the row.

**This skill is a layer too.** If the instructions here sent you the wrong way —
a step that contradicts itself, a pitfalls row that has gone stale — record it
with layer `skill` and promote it. Those rows compound into the next cycle.

**Feedback file location**: `claudedocs/dogfooding-feedback.md` inside the cl-mcp checkout.
The `claudedocs/` directory is listed in `.gitignore` so it is never committed. If the user has said
"record feedback to X", use X and skip the default.

In all cases: **append, never overwrite**. Create the file with `fs-write-file` if it does not exist; afterwards append via shell heredoc or `repl-eval`.

**Format:** add a new dated section (`## Session YYYY-MM-DD — <project-name>`). Categorize every item as P1/P2/P3:
- **P1** — real bugs, silent wrong results, data-loss risk, or features that block the workflow
- **P2** — rough edges, token waste, confusing error messages, docs mismatches
- **P3** — nits, scaffold template polish, nice-to-haves

For each item: Problem (one line), Layer (`tool` / `worker` / `skill`),
Reproduction or symptom, the control-run output if one was needed,
Suggested fix. Write the ledger out too, dismissed rows included — the
dismissals are what stop the next cycle re-filing the same candidate.

### 7. Cleanup

At the end of the cycle:

1. Report generation stats (project name, location, test count, feedback items count)
2. Leave the throwaway project on disk under `experiments/` — it is cheap storage and gitignored
3. Verify `git status` shows no untracked experiment files (gitignore should handle this)

## Known pitfalls (check before recording as new bugs)

These are documented pitfalls that have tripped previous dogfooding runs. If you hit them, you can cite the existing feedback instead of opening duplicates.

| Symptom | Cause | Workaround |
|---|---|---|
| `run-tests` on aggregate `<name>/tests` reports `Passed: 0, Failed: 0` with `✓ PASS` despite tests actually running | Fixed in PR #98: fallback now purges Rove suites and clears ASDF state before sub-system runs | Resolved. Zero-count case is handled. See next row for non-zero undercount variant |
| `run-tests` aggregate reports partial counts (e.g., 6 instead of 13) after individual sub-packages were run first in the same worker session | Individual runs left Rove suites registered; `%ensure-system-loaded` only cleared the aggregate ASDF system, so ASDF skipped reloading sub-systems and deftest forms didn't re-register | Fixed: `%ensure-system-loaded` now also clears ASDF state for test sub-systems |
| `run-tests` fails with opaque `COMPILE-FILE-ERROR while compiling ...` after you edited a `defpackage` | SBCL package-variance warning escalated to error; cached worker state | `pool-kill-worker` then `load-system` to get a fresh image |
| `lisp-edit-form` or `lisp-patch-form` on a `.asd` file rejects `form_type: "asdf:defsystem"` | Tool matches on unqualified symbol name | Use `form_type: "defsystem"` |
| `code-find` returns `symbol is required` when you pass `name:` | Parameter name is `symbol`, not `name` | Check the tool schema: the required key is `symbol` |
| `fs-list-directory` hides `.gitignore` and other dotfiles | Default behavior filters `*hidden-prefixes*` | Pass `show_hidden: true` (added in PR #94) |
| `lisp-edit-form` on a defmethod with `#:` specializers says "not found" with plain `form_name` | Was a bug before PR #94; fixed by `%strip-hash-colon` normalization | Should work now; if it still fails, file a new issue |
| `load-system system=<name>` fails with `Component "<name>" not found` immediately after `project-scaffold` | Fixed: `load-system` now auto-discovers `.asd` files under the project root on MISSING-COMPONENT | Resolved. Verified in cycle 10 (2026-04-13). Manual `asdf:load-asd` only needed if `.asd` is outside the project root |
| `run-tests` single-test mode reports `Test runner crashed` with `no applicable method for TEST-NAME` on `FAILED-ASSERTION` | Rove internal bug: `rove:run-tests` calls `TEST-NAME` on `FAILED-ASSERTION` objects. Caught by handler-case in `run-rove-selected-tests`; `%safe-test-name` guards `run-rove-tests` path | Failure is reported gracefully (not a hard crash). The "Test runner crashed" reason text reflects Rove's internal error |
| `inspect-object id=<N>` returns `id must be an integer` even when N is clearly an integer | Deferred tool schema not hydrated (harness-side, not cl-mcp) | Run ToolSearch hydration batch from step 1 before first use |
| `lisp-edit-form operation=replace content=<multi-form>` rejects with `replace takes exactly one top-level form` | `replace` swaps one form for one form; only `insert_before`/`insert_after` take several | `replace` with the first form, then one `insert_after` with the rest |
| `clgrep-search form_types=[...]` filter returns fewer results than expected | Filter works for most cases but may miss forms with non-standard structure | Omit `form_types` and post-filter client-side if results seem incomplete, OR prefer `code-find` / `code-describe` for exact lookups |
| `clgrep-search` signature field is a 4KB blob with the whole form body | Fixed: results are now deduplicated by (file, form-start-byte) with `match_lines` array | Should be resolved; if still noisy, use `limit` param or targeted `lisp-read-file name_pattern=...` |
| `lisp-edit-form` has no way to remove a form from a file | Fixed: `operation: "delete"` is now available (content param not needed) | Use `lisp-edit-form` with `operation: "delete"` to remove scaffold stubs like `defun greet` |
| `load-system` after changing package exports shows noisy "also exports" warnings | SBCL package-variance; stale worker image | `pool-kill-worker` then `load-system` for a clean image. `load-system` now shows a hint when this happens |
| FiveAM project: `run-tests` is green with the full count, but `asdf:test-system` runs only the scaffold smoke test | A suite was declared without `:in :<name>`. The generated `test-op` runs the root suite alone, so it never reaches that suite — while `run-tests` still finds it, because its matcher also matches on *package* name and `<NAME>/TESTS/<FILE>-TEST` nests below the system name. **`run-tests` cannot detect this defect**, so a green run is not evidence | Nest every suite: `(def-suite <file>-suite :in :<name>)`. Verify with the tests-not-checks count under "Working in a FiveAM project" — it must match `run-tests`' `passed + failed + pending` |
| FiveAM project: the test count silently drops (e.g. 6 → 1) between two `run-tests` calls, still reporting `✓ PASS` | A sub-test file does not depend on the root-suite file, so its load order comes from the `.asd` `:depends-on` list. Wrong order on a *cold* worker is a loud `Unknown suite <NAME>`; on a *warm* one the sub-suite attaches to the previous run's root-suite object, is orphaned when the new root replaces it, and its tests just disappear | Give every sub-test `defpackage` an `(:import-from #:<name>/tests/main-test)` clause. That makes ASDF order the files regardless of the `:depends-on` order — verified by leaving the list deliberately reversed |
| `run-tests` aggregate reports a suspiciously high count, per-package `run-tests` on your brand-new sub-packages fails with `MISSING-COMPONENT`, and `find-package` on the new test package returns `NIL` | ASDF resolved the system name to a stale `.asd` elsewhere on the Roswell source registry (previous dogfood residue) | `repl-eval (asdf:system-source-file (asdf:find-system "<name>"))` — if the path does not match your scaffold's `absolute_path`, rename and re-scaffold. Follow the pre-scaffold `asdf:find-system ... nil` check in step 2 to avoid this entirely |
| `clgrep-search` or `code-find-references` finds nothing in your throwaway project, although the symbol is right there | Both honour the `.gitignore` of their scan root, and cl-mcp's lists `experiments/` | `clgrep-search path=experiments/<project>`; for references, switch the project root to the experiment for step 5 and back afterwards. Expected behaviour - do not record it as a tool bug |
| A plain CL form looks broken in the worker: `define-condition` with a bare slot name, `handler-case` with a `(type (var) ...)` clause | Almost always the form as written, not the worker. Both of those were run in the worker and in plain SBCL on 2026-09-20 and behaved identically | Control-run it before recording. `(:format-control ...)` as a `define-condition` option and `make-instance` on a `defstruct` are *supposed* to fail - neither is in the standard |
| `(format t ...)` inside a test prints nothing in `run-tests`' summary | Deliberate: raw stdout is kept in a structured-only field and the summary renders `debug_output` alone, so an MCP client shows neither | Write to `cl-mcp/src/test-runner-core:*test-debug-output*` instead — that stream reaches both the field and the summary text. Surfacing a `stdout` preview is already an open feedback item; do not re-file it |

## Success criteria

You are done with one cycle when:

- [ ] The throwaway project has all its tests green (Rove: verified per-package OR via aggregate `run-tests`, the aggregate undercount bug is now fixed. FiveAM: one aggregate run covers everything nested under the root suite)
- [ ] At least one edited Lisp file was sanity-checked with `lisp-check-parens` (cheap and catches `lisp-patch-form` drift early)
- [ ] Every candidate you noticed has a ledger row naming its layer. A cycle
      that promoted **zero** feedback items is complete — report it as such
- [ ] Every promoted item's layer is `tool`, `worker` or `skill`, and every
      Lisp-behaviour item carries the control-run output that put it there
- [ ] The ledger and any promoted items were **actually appended** to the
      feedback file, verified by reading the tail back — even when the ledger
      says "no promoted items". "I'll record it at the end" is how a cycle
      loses its findings
- [ ] Feedback is categorized P1/P2/P3 under a dated section heading
- [ ] Nothing in `git status` references the throwaway project (gitignore should handle this)

## Red flags — you are manufacturing feedback

| Thought | Reality |
|---|---|
| "Only two items so far, let me look harder for a third" | The deliverable is the ledger, not a count. Two promoted items with control runs are worth more than five without |
| "Probably a cl-mcp bug; I will write it up and let someone check" | An unverified candidate costs the reviewer more than it saved you. Ledger row, `unverified` |
| "My code was wrong, but the error message could have been better" | That is a real `tool` item — write *that* one, not the bug you caused |
| "The standard probably allows this" | `clhs-lookup` and quote it, or do not promote it |
| "It failed in the worker, that is enough" | Only against a control run. A warm worker fails for reasons of its own |
| "A cycle with nothing to report looks like I did not try" | It reports which tools carried the work with no friction. That is the result |

**All of these mean: finish the triage, then write down whatever it produced.**

## Anti-patterns

- **Scaffolding outside `experiments/`.** If you scaffold into a non-gitignored path inside cl-mcp, generated files will taint `git status`. Always use `destination: "experiments"`.
- **Building a project you intend to keep.** This is a feedback-gathering exercise; grab shallow breadth (lots of tool calls) over deep polish.
- **Trusting aggregate counts without cross-checking.** Both the zero-count and partial-count bugs are now fixed; aggregate `run-tests` should report correct totals. If counts seem wrong, verify with per-package runs.
- **Recording only tool bugs.** Capture UX friction too: confusing errors, missing defaults, unnecessary retries. Those become P2/P3 items.
- **Treating the feedback count as a target.** A count target is met by promoting your own mistakes, the CL standard and Rove's internals to P1. Those items then cost a reviewer a full investigation each to dismiss.
- **Promoting a candidate with no layer.** "Something felt off in `run-tests`" is a ledger row until a control run or an inspection of cl-mcp's own output says which layer it is.
- **Skipping the "try every tool" step.** If you only use `lisp-edit-form` and `run-tests`, you only produce feedback on those two tools.

## Output when asked to run a cycle

When a cycle completes, summarize:

1. **Project:** name + absolute path
2. **Size:** N src files, N test files, N tests and which framework, what CL features exercised (defclass, defmethod, etc.)
3. **Test status:** Rove — per-package counts (avoid the aggregate trap). FiveAM — the root-suite run, plus a note on any suite that turned out not to be nested under it
4. **Feedback recorded:** candidates observed, how many were promoted with the
   P1/P2/P3 breakdown, and the dismissed ones tallied by layer (`ANSI CL: 2`,
   `project code: 1`, ...). "0 promoted, 4 dismissed" is a valid line
5. **Procedural pitfalls:** anything that took more than one try — the best source of candidates, and each one still goes through step 6's triage before it is called a finding
6. **Cleanup:** project root restored ✓, cl-mcp `git status` clean ✓
