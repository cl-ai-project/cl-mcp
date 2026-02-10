# [High] lisp-edit-form: improve parse error diagnostics for invalid content

## Background
When `lisp-edit-form` receives malformed `content`, the current error often reports generic parse failure, requiring extra manual debugging.

## Problem
- Error messages are not specific enough (line/column/context absent).
- Users must rerun with external checks to locate syntax problems.
- Edit loop slows down, especially for larger replacement forms.

## Proposal
On `content` parse failure, return structured diagnostics in error data:
- `code` (e.g., `content_parse_error`)
- `line` / `column`
- short `snippet` around error
- suggested `next_tool` (`lisp-check-parens`)
- optional `required_args` guidance when applicable

## Acceptance Criteria
- Parse failures include location metadata when available.
- Error message is actionable without reading stack traces.
- Existing successful edit behavior remains unchanged.
- Error schema is documented and consistent with other tools.

## Test Plan
- Add tests for malformed content cases (unclosed paren, extra close paren, trailing garbage).
- Assert error `data.code` and location fields.
- Verify no file mutation on failure.

## Out of Scope
- Automatic correction of malformed content.

## Suggested Labels
`enhancement`, `high-priority`, `developer-experience`, `tools`
