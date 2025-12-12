# Repository Guidelines

@prompts/repl-driven-development.md
@agents/common-lisp-expert.md

## Project Structure & Module Organization
The core system lives under `src/`, grouped by responsibility (`log`, `repl`, `protocol`, `tcp`, `core`, `run`). Each file is serially loaded via `cl-mcp.asd`; add new packages here and export symbols explicitly. Tests reside in `tests/` with mirrored filenames (`*-test.lisp`) for Rove. Helper clients and bridges are in `scripts/`. Keep assets such as sample transcripts or captures under `tests/fixtures/` if introduced.

## Build, Test, and Development Commands
Use cl-mcp REPL, invoke individual suites with `(rove:run 'cl-mcp/tests/<package>)` via `repl-eval` so tests run inside the agent without shelling out.

### Running Individual Tests
- From the MCP REPL, wrap the test invocation so stdout is captured:  
  `(with-output-to-string (*standard-output*) (rove:run-test 'cl-mcp/tests/integration-test::repl-eval-printlength))`
- The wrapper lets you see per-assertion output even when the REPL only returns the final value. Replace the test symbol to target other cases.
- In sandboxed environments where TCP bind is denied, expect `tests/tcp-test.lisp` to fail; run other suites individually to iterate.

## Coding Style & Naming Conventions
Follow the Google Common Lisp Style Guide: 2-space indent, ≤100 columns, blank line between top-level forms. Each `*.lisp` begins with `(in-package ...)` then module-specific `declaim`. Use lower-case lisp-case for functions, `-p` predicates, `+constants+`, and `*specials*`. Avoid runtime `eval` and dynamic symbol interning; prefer restarts over `signal`. Public functions and classes require docstrings; document conditions and restarts in situ.

## Testing Guidelines
Write Rove tests before implementations and place them in the matching file under `tests/`. Name suites after the unit under test (e.g., `(deftest protocol-handshake ...)`). Ensure new behaviours extend the `cl-mcp/tests` ASDF component list and keep coverage over initialize/ping, tool discovery, REPL evaluation, logging, and transport boundary cases. Tests must leave listener threads closed and assert JSON payloads explicitly.

## Commit & Pull Request Guidelines
Compose commits as imperative, concise summaries (`module: action`). Include context in a wrapped body when touching protocol or threading code. Each PR should describe capability changes, list test commands run (`asdf:test-system "cl-mcp"`), and link related issues or logs. Provide reproduction steps for transport bugs and include JSON snippets when adjusting protocol semantics.

## Security & Configuration Notes
Treat the REPL tool as trusted input only; untrusted callers can execute arbitrary code. Prefer `:tcp` during development and secure the port when exposing beyond localhost. When adjusting logging, ensure secrets are redacted before emitting structured lines.
