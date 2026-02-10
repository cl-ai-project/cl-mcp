# [High] run-tests: return structured payload alongside summary text

## Background
`run-tests` currently returns a human-readable summary in `content`, which is easy for humans but brittle for automation.

## Problem
- Clients must parse free-form text to extract pass/fail counts.
- CI and editor integrations cannot reliably consume results.
- Failure details are available internally but not consistently exposed as a stable API contract.

## Proposal
Extend the `run-tests` tool response to always include machine-readable fields:
- `passed` (integer)
- `failed` (integer)
- `pending` (integer, optional)
- `framework` (string)
- `duration_ms` (integer)
- `failed_tests` (array of objects)
- keep existing `content` summary for backward compatibility

## Acceptance Criteria
- `tools/call run-tests` returns both structured fields and `content`.
- Existing clients that only read `content` continue to work unchanged.
- Failure entries include at least `test_name` and `reason` when available.
- Tool schema/docs reflect the new contract.

## Test Plan
- Add/extend protocol-level tests under `tests/tools-test.lisp` for response shape.
- Add test-runner-level tests asserting numeric fields and failure array shape.
- Validate with passing, failing, and mixed test suites.

## Out of Scope
- Changing test execution backend logic.

## Suggested Labels
`enhancement`, `high-priority`, `testing`, `tools`
