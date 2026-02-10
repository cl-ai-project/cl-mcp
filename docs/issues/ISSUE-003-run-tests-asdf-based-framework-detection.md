# [High] run-tests: make framework auto-detection ASDF-aware

## Background
Framework auto-detection currently relies mainly on loaded packages, which can be misleading across long-lived REPL sessions.

## Problem
- False positives when unrelated frameworks are already loaded.
- Behavior can vary by session history, not system configuration.
- `framework="auto"` does not always reflect the target ASDF system.

## Proposal
Implement framework detection based on target system metadata first:
1. inspect ASDF/test system dependencies,
2. infer framework from declared deps (`rove`, `fiveam`, etc.),
3. fallback to loaded-package heuristic only when metadata is insufficient.

Also return `framework_source` metadata (e.g., `asdf-dependency`, `loaded-package`, `fallback`).

## Acceptance Criteria
- `framework="auto"` is deterministic for the same system in a fresh or dirty image.
- ASDF-based inference is preferred when available.
- Detection path is observable via metadata/logging.
- Backward compatibility is preserved for explicit `framework` values.

## Test Plan
- Add detection tests with mocked/fixture systems.
- Verify consistent result across repeated runs in same image.
- Ensure unknown framework still falls back cleanly.

## Out of Scope
- Full implementation of non-Rove runners.

## Suggested Labels
`enhancement`, `high-priority`, `testing`, `reliability`
