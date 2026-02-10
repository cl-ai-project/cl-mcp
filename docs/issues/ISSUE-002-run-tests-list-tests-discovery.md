# [High] run-tests: add test discovery API for selective execution

## Background
Selective execution (`test` / `tests`) is now supported, but users still need to know exact fully-qualified test symbols.

## Problem
- Discovering valid test names is manual and error-prone.
- Misspelled symbols fail late at execution time.
- Integrations cannot present test pickers without discovery support.

## Proposal
Introduce a discovery capability, either:
1. new tool `list-tests` (recommended), or
2. new optional mode in `run-tests` (e.g., `discover_only=true`)

Return:
- `system`
- `tests` (array of fully-qualified test names)
- optional grouping metadata (package/suite)

## Acceptance Criteria
- Given a valid test system, discovery returns non-empty test symbol list.
- Returned names are directly accepted by `run-tests :test` and `:tests`.
- Unknown systems return actionable validation errors.
- Tool docs include example flow: discover -> execute selected tests.

## Test Plan
- Unit tests for discovery helper.
- Tool tests for happy-path and invalid-system path.
- Integration test that discovers and immediately executes a subset.

## Out of Scope
- UI implementation for test picker.

## Suggested Labels
`enhancement`, `high-priority`, `developer-experience`, `tools`
