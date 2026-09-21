# PR-166 Mallet lint-fix report

Date: 2026-09-21
Branch: `debugger/observe-and-abort`

## Scope

Test-only changes for the six Mallet warnings from GitHub Actions run
35587398811. No production source files were changed.

## RED baseline

Command:

```text
mallet src/*.lisp src/*/*.lisp tests/*.lisp
```

Source provenance: the checked-out worktree at
`/home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp-debugger-observe-and-abort`.

Observed result: exit 1, six warnings:

- `tests/debugger-boundary-worker-test.lisp`: unused `rove:testing` and three
  `needless-let*` warnings (lines 174, 360, and 399 in the baseline).
- `tests/frame-inspector-test.lisp`: one `needless-let*` warning (line 81).
- `tests/utils-request-debugger-boundary-test.lisp`: one `needless-let*` warning
  (line 341).

## Changes

- Removed the unused `testing` import.
- Replaced the three independent-binding `let*` forms in the worker test with
  `let`.
- Used nested `let` forms for `*package*` and
  `*boundary-interrupt-events*`, preserving their dynamic scope while making
  the dependent initializer a separate inner binding.

## Verification

Exact lint command after the edit:

```text
mallet src/*.lisp src/*/*.lisp tests/*.lisp
```

Result: exit 0, `No problems found` (0 warnings, 0 errors).

Focused project `run-tests` command (the source registry is explicitly set to
the current worktree so no stale sibling checkout is loaded):

```text
CL_SOURCE_REGISTRY="$PWD//" ros -Q -e '(ql:quickload :cl-mcp :silent t)' -e '(let ((r (cl-mcp/src/test-runner-core:run-tests "SYSTEM"))) (format t "result failed=~S passed=~S~%" (gethash "failed" r) (gethash "passed" r)) (uiop:quit (if (and (hash-table-p r) (zerop (gethash "failed" r 0))) 0 1)))'
```

Substituting each requested system produced:

- `cl-mcp/tests/frame-inspector-test`: 19 passed, 0 failed.
- `cl-mcp/tests/utils-request-debugger-boundary-test`: 26 passed, 0 failed.
- `cl-mcp/tests/debugger-boundary-worker-test`: 12 passed, 0 failed.

Additional verification:

```text
git diff --check
```

Result: exit 0.

An initial run without `CL_SOURCE_REGISTRY` resolved the sibling checkout at
`/home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp`; the two boundary
systems were consequently reported as missing components. That run was not
used as test evidence. All reported test results above compile and execute
the current worktree, as shown by their compiler paths and worker project-root
paths.

## Commit

`f38cd2fdb188abfcb78b59251b3bf56edc6a0923`
