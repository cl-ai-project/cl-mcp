# [High] lisp-edit-form: enrich dry_run output with diff summary and target metadata

## Background
`dry_run` is useful, but current output still requires manual scanning of full preview text to understand what changed.

## Problem
- Hard to confirm whether the intended form was matched.
- Large file previews are costly to read and compare manually.
- Automation cannot quickly decide if the planned edit is acceptable.

## Proposal
Extend `dry_run` response with compact change metadata:
- `target` object (`file_path`, `form_type`, `form_name`, `operation`)
- `matched_signature` (actual matched top-level form signature)
- `summary` object (`would_change`, `inserted_forms`, `replaced_forms`, `line_delta`)
- optional small unified `diff` excerpt (bounded length)

Keep existing `preview` and `original` fields for backward compatibility.

## Acceptance Criteria
- `dry_run` response includes stable metadata fields above.
- For no-op edits, summary clearly indicates no changes.
- For changed edits, metadata points to exact matched target.
- Existing clients consuming current fields remain unaffected.

## Test Plan
- Add tests for `replace`, `insert_before`, `insert_after`, and no-op cases.
- Assert summary metadata correctness and bounded diff length.
- Validate behavior on files with multiple similar definitions.

## Out of Scope
- Full semantic diffing across arbitrary Lisp AST nodes.

## Suggested Labels
`enhancement`, `high-priority`, `developer-experience`, `tools`
