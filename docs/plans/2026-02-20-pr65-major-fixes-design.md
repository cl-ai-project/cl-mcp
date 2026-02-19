# Design: PR #65 Major Issue Fixes (v2)

## Context

PR #65 introduced robust JSON error handling with a 3-level encoding fallback.
A comprehensive test review identified 3 remaining Major issues after the initial
fix commits. This design addresses all 3.

## Major #1: `sanitize-for-json` Non-String Type Guard

**Problem**: `sanitize-for-json` crashes with TYPE-ERROR on non-string input (e.g., integer).
The function is called in error handlers where unexpected types could arrive.

**Design**: Add a defensive type conversion after the existing nil guard.
Non-nil, non-string inputs are converted via `princ-to-string` then recursively sanitized.

```lisp
(when (null string) (return-from sanitize-for-json nil))
(unless (stringp string)
  (return-from sanitize-for-json
    (sanitize-for-json (princ-to-string string))))
```

**Rationale**: Defensive over strict -- this function runs on error paths where crashing
would cause secondary failures. Converting to string is safe and preserves information.

**Test**: Verify integer input returns its string representation.

## Major #2: Missing `ignore-errors` on `log-event` in Error Handlers

**Problem**: Two `log-event` calls inside error handler clauses are not wrapped with
`ignore-errors`, inconsistent with the pattern at line 317. If logging fails (broken
stream, encoding error), the exception escapes and can crash the connection.

**Design**: Wrap both calls with `ignore-errors`:

1. `src/protocol.lisp` -- `process-json-line`, `%decode-json` error handler (line ~282)
2. `src/protocol.lisp` -- `handle-initialize`, root-sync error handler (line ~190)

**Rationale**: Consistency with existing pattern. Logging failure should never crash
the server.

## Major #3: ECMA-48 Compliant Escape Sequence Parser

**Problem**: The sanitizer only handles CSI (`ESC[`) sequences. Other escape types
(OSC, DCS, SS2, SS3, etc.) leave remnant bytes in the output.

**Design**: Extend the ESC handler to dispatch on the byte following ESC:

| ESC + byte | Sequence type | Action |
|------------|---------------|--------|
| `[` | CSI | Existing logic: consume params + final byte |
| `]` | OSC | Consume until BEL (0x07) or ST (ESC + `\`) |
| `P`, `X`, `^`, `_` | DCS/SOS/PM/APC | Consume until ST (ESC + `\`) |
| `0x40-0x5F` range | 2-byte sequence | Consume ESC + next byte |
| Other | Unknown | Strip ESC only (existing fallback) |

Non-ASCII bytes (>= 0x80) after ESC are never consumed as sequence content,
preserving CJK/emoji text.

**Tests**:
- OSC sequence (`ESC]0;title BEL`) fully stripped
- SS3 sequence (`ESC O`) 2-byte stripped
- DCS sequence (`ESC P ... ST`) fully stripped
- Non-ASCII after ESC preserved

## Files Changed

| File | Change |
|------|--------|
| `src/utils/sanitize.lisp` | Type guard + ECMA-48 parser extension |
| `src/protocol.lisp` | `ignore-errors` wrappers (2 locations) |
| `tests/utils-sanitize-test.lisp` | New tests for type guard + escape sequences |
