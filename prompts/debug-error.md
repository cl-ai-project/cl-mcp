# Debug Common Lisp Error
Systematic debugging: reproduce errors, inspect stack frames and locals, trace references, apply fixes.

## Goal

Diagnose and fix a Common Lisp error using structured debugging: reproduce the error, analyze the stack trace and local variables, understand the call chain, apply a targeted fix, and verify with tests.

## Tools You'll Use

| Tool | Purpose |
|------|---------|
| `repl-eval` | Reproduce error, test fixes |
| `inspect-object` | Drill into complex runtime values |
| `code-describe` | Check symbol signatures and types |
| `code-find-references` | Trace how problematic symbols are used |
| `clgrep-search` | Find related definitions across files |
| `lisp-check-parens` | Diagnose syntax errors |
| `lisp-edit-form` | Apply the fix |
| `run-tests` | Verify the fix |

## Workflow

### Step 1: Reproduce the Error
Run the failing expression with `locals_preview_frames` to capture stack context:
```json
{"name": "repl-eval", "arguments": {
  "code": "(my-function bad-input)",
  "package": "MY-PACKAGE",
  "locals_preview_frames": 3
}}
```
The response includes `error_context` with:
- `condition_type` — error type (e.g., `TYPE-ERROR`)
- `message` — human-readable description
- `restarts` — available recovery options
- `frames` — stack frames with function names, source locations, and local variables

### Step 2: Analyze Error Context
Read the error type and message. Common patterns:

| Error Type | Typical Cause | First Action |
|-----------|---------------|--------------|
| `UNBOUND-VARIABLE` | Typo, missing binding, wrong package | Check symbol package with `code-describe` |
| `TYPE-ERROR` | Wrong argument type | Inspect the value via `inspect-object` |
| `UNDEFINED-FUNCTION` | System not loaded, typo | `(ql:quickload ...)` or `clgrep-search` |
| `PACKAGE-ERROR` | Missing package, wrong export | Check `.asd` dependencies |
| `SIMPLE-ERROR` | Explicit `(error ...)` call | Read the signaling function |

### Step 3: Inspect Runtime Values
If a local variable looks suspicious, drill into it using its `object_id` from the stack frame:
```json
{"name": "inspect-object", "arguments": {"id": 42}}
```

### Step 4: Read the Failing Code
```json
{"name": "lisp-read-file", "arguments": {
  "path": "src/core.lisp",
  "name_pattern": "^my-function$"
}}
```

### Step 5: Trace References
Understand how the problematic function is called:
```json
{"name": "code-find-references", "arguments": {
  "symbol": "my-package:my-function",
  "project_only": true
}}
```

### Step 6: Check Syntax (if suspected)
```json
{"name": "lisp-check-parens", "arguments": {"path": "src/core.lisp"}}
```

### Step 7: Apply the Fix
```json
{"name": "lisp-edit-form", "arguments": {
  "file_path": "src/core.lisp",
  "form_type": "defun",
  "form_name": "my-function",
  "operation": "replace",
  "content": "(defun my-function (input)\n  (fixed-implementation input))"
}}
```

### Step 8: Verify
Re-run the original failing expression, then run the test suite:
```json
{"name": "repl-eval", "arguments": {"code": "(my-function bad-input)", "package": "MY-PACKAGE"}}
```
```json
{"name": "run-tests", "arguments": {"system": "my-project/tests"}}
```

## Debugging Tips

- **Enable debug info**: Add `(declare (optimize (debug 3)))` to functions you need to debug. SBCL's default optimization does not preserve locals.
- **`locals_preview_skip_internal`**: Defaults to `true`, skipping CL-MCP/SBCL frames. Set to `false` if you need to see infrastructure frames.
- **Check `stderr`**: Compiler warnings often hint at the root cause (unused variables, type mismatches).
- **Package context matters**: Always specify `package` in `repl-eval` to avoid symbol resolution errors.
- **Bisect with REPL**: Break complex expressions into smaller parts and evaluate each to isolate the failure.
