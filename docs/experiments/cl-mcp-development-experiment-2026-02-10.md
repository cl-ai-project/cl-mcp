# cl-mcp development experiment log (2026-02-10)

## Goal
Run a realistic feature development cycle while using `cl-mcp` features broadly, then record:
- which features were used often
- relative importance by task phase
- practical strengths/limitations

## Scenario
Feature theme: improve `run-tests` for practical CI/dev usage.

Implemented:
1. fixed single-test execution counting bug (`run-rove-single-test`)
2. added multiple-test selection (`:tests` array)
3. added strict validation for `:test` + `:tests` conflict
4. fixed `framework="auto"` behavior to truly auto-detect
5. extended MCP tool schema and handler for new `tests` input
6. added regression tests at function and tool API levels

## Development cycle
1. Explore
- used: `fs-set-project-root`, `lisp-read-file`, `clgrep-search`, `code-describe`, `code-find`, `fs-read-file`
- outcome: identified root cause around `rove:run-test` return-value interpretation

2. Experiment
- used: `repl-eval`, `clhs-lookup`, `inspect-object`, `code-find-references`
- outcome: validated Rove API behavior; found that `run-tests` selection should use `rove:run-tests`

3. Persist
- used: `lisp-edit-form` (main), `fs-write-file` (new markdown only)
- outcome: safe structural edits across `src/test-runner.lisp`, `tests/test-runner-test.lisp`, `tests/tools-test.lisp`

4. Verify
- used: `lisp-check-parens`, `repl-eval` with `rove:run` / `rove:run-test`, `run-tests`
- outcome: all added tests passed

## Feature usage and importance (this run)

| Tool | Frequency | Importance | Notes |
|---|---|---|---|
| `lisp-read-file` | High | Critical | Fast structural comprehension of large Lisp files |
| `lisp-edit-form` | High | Critical | Safe top-level form edits; essential for refactor-scale changes |
| `repl-eval` | High | Critical | Runtime validation and framework behavior checks |
| `lisp-check-parens` | Medium | High | Prevented malformed form commits during iterative edits |
| `run-tests` | Medium | High | Quick behavioral verification from MCP surface |
| `clgrep-search` | Medium | High | Dependency/usage discovery without loading systems |
| `code-describe` / `code-find` | Low-Med | Medium | Helpful when symbol-level runtime metadata is needed |
| `clhs-lookup` | Low | Medium | Useful when semantics are uncertain; authoritative reference |
| `inspect-object` | Low | Medium | Good for opaque runtime objects/debug states |
| `fs-*` tools | Low-Med | Medium | Setup and non-Lisp file operations |

## Key findings
1. `lisp-read-file` + `clgrep-search` is the fastest exploration pair.
2. For existing Lisp files, `lisp-edit-form` reduces syntax risk compared with raw patch edits.
3. `repl-eval` is indispensable for library behavior verification (this run: Rove execution model).
4. Tool-level tests can expose integration bugs not visible in function-level tests
   (example: previous `boundp` guard accidentally dropped optional args).
5. `lisp-check-parens` should be used proactively before large form replacements.

## Added/updated project artifacts
- `src/test-runner.lisp`
- `tests/test-runner-test.lisp`
- `tests/tools-test.lisp`

## Reusable accumulation template
Use this block for future runs:

```
Date:
Theme:
Complexity level:

Most-used tools:
Critical tools:
Unexpectedly useful tools:
Pain points:

Feature outcomes:
Test outcomes:
Open follow-ups:
```
