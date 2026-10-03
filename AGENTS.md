# Repository Guidelines

@prompts/repl-driven-development.md
@prompts/common-lisp-expert.md
@prompts/cl-spec-driven-development.md

## Project Structure & Module Organization
The core system lives under `src/`, grouped by responsibility (`protocol`, transports, tools, the worker pool; see the Architecture table in `CLAUDE.md`). `cl-mcp.asd` is a `package-inferred-system`: each file defines package `cl-mcp/src/<path>` and is loaded because another loaded file `:import-from`s it, so the `.asd` needs no edit. A new tool module is registered in `src/tools/all.lisp` (and, if it runs in the worker, in `register-all-handlers` in `src/worker/handlers.lisp`); public API is re-exported from `main.lisp`. Tests reside in `tests/` with mirrored filenames (`*-test.lisp`) for Rove. Helper clients and bridges are in `scripts/`. Keep assets such as sample transcripts or captures under `tests/fixtures/` if introduced.

## Build, Test, and Development Commands
Use cl-mcp REPL, invoke individual suites with `run-tests` so tests run inside the agent without shelling out.

### Temporary Debug Logging
- When cl-mcp shows unintended behavior or hits errors that are hard to diagnose, record each occurrence to a temp file immediately.
- Use a per-session temp log (for example, `/tmp/cl-mcp-debug-<timestamp>.log`) and append entries instead of overwriting.
- For every entry, include at least: timestamp, command/tool invocation, relevant inputs, observed error/output, and stack trace or restart information when available.
- Redact secrets before writing logs, and reference the temp log in follow-up debugging notes, issues, or PRs.

## Coding Style & Naming Conventions
Follow the Google Common Lisp Style Guide: 2-space indent, ≤100 columns, blank line between top-level forms. Each `*.lisp` begins with `(in-package ...)` then module-specific `declaim`. Use lower-case lisp-case for functions, `-p` predicates, `+constants+`, and `*specials*`. Avoid runtime `eval` and dynamic symbol interning; prefer restarts over `signal`. Public functions and classes require docstrings; document conditions and restarts in situ.

## Testing Guidelines
Write Rove tests before implementations and place them in the matching file under `tests/`. Name suites after the unit under test (e.g., `(deftest protocol-handshake ...)`). Register a new test file with an `(:import-from #:cl-mcp/tests/<name>-test)` in the root `tests.lisp` — or, if it starts worker processes or servers, add its name to `*process-tier-suites*` there (the full tier, which CI runs) — or neither `rove cl-mcp.asd` nor CI runs it, and keep coverage over initialize/ping, tool discovery, REPL evaluation, logging, and transport boundary cases. Tests must leave listener threads closed and assert JSON payloads explicitly.

A few of cl-mcp's own functions (`ensure-trailing-newline`, `sanitize-for-json`, `sanitize-error-message`, and, through properties only, `allowed-read-path`, `resolve-readable-path`, `ensure-write-path`, `fs-write-file`, the record layer's `field-availability`, `validate-versioned-record`, `project-record` and `project-core-record`, the verdict layer's internal `%counts`, `%contract-plist`, `%verified-p` and `%verification-gaps`, spec-check's routing: `parse-seed-string` and the internal `%target-argument-error`, `%resolve-profile`, `%select-properties`, `%trials-budget` and `%definition-match`, and the inspection layer: `api-backend-available-p`, `definition-digest`, `list-report`, `symbol-report`, `describe-report` and the internal `contract-operation-missing` and `%describe-function-spec`, and the response layer: the four `build-spec-*-response`) carry cl-spec contracts in the opt-in `cl-mcp/specs` system, as do the worker-result wire path, the pool's ownership, a request's lifecycle, state-loss events and object ids, and the pool under overlapping operations — `CLAUDE.md`'s Testing & Linting section names the functions, properties and suites for each. When a change touches one of them, read its contract before editing and check it before and after, as `docs/specs.md` describes; `scripts/check-specs.lisp` runs the same checks CI does. Elsewhere the bundle is not required.

## Commit & Pull Request Guidelines
Compose commits as imperative, concise summaries (`module: action`). Include context in a wrapped body when touching protocol or threading code. Each PR should describe capability changes, list test commands run (`run-tests`, or `rove cl-mcp.asd` in a fresh process, plus the CI's separate-process suites when touched), and link related issues or logs. Provide reproduction steps for transport bugs and include JSON snippets when adjusting protocol semantics.

## Security & Configuration Notes
Treat the REPL tool as trusted input only; untrusted callers can execute arbitrary code. Prefer `:tcp` during development and secure the port when exposing beyond localhost. When adjusting logging, ensure secrets are redacted before emitting structured lines.
