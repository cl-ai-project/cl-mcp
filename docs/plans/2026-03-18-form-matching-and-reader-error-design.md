# Design: Form Matching Fixes and Reader Error Detection

Date: 2026-03-18  
PR scope: 3 issues, 1 PR  
Source: User feedback from 2026-03-17 sessions

## Background

Three high-priority issues were identified from real-world cl-mcp usage:

1. `lisp-edit-form` silently fails to match `defstruct` forms that use the
   options syntax `(defstruct (name opts...) ...)`.
2. `lisp-check-parens` reports balanced parentheses even when the file
   contains a reader error, leaving users to do manual text inspection.
3. `lisp-edit-form` and `lisp-patch-form` reject `form_name` values that
   include reader macro prefixes like `#:my-package`, while the equivalent
   bare name `my-package` works.

## Issue 1: `defstruct` Form Matching

### Root Cause

`%definition-candidates` in `src/lisp-edit-form-core.lisp` handles only two
cases for the name position (second element of the form):

- `symbolp name` → use `(symbol-name name)` as the candidate
- otherwise → use `(princ-to-string name)` as the candidate

For `(defstruct (tape-node (:include base)) ...)`, `(second form)` is the
list `(TAPE-NODE (:INCLUDE BASE))`. The fallback branch produces candidate
`"(tape-node (:include base))"`, which never matches the user-supplied
`form_name: "tape-node"`.

Note: `(defun (setf car) ...)` is a superficially similar case where the
name position is a list, but there the *entire* list `(SETF CAR)` is the
function name. The current fallback already handles this correctly:
candidate is `"(setf car)"`, matching `form_name: "(setf car)"`.
This behavior is intentional and must not be changed.

### Fix

Add a `defstruct`-specific branch before the generic fallback in
`%definition-candidates`:

```lisp
;; defstruct: (defstruct (name &rest options) ...) — only first element is the name
((string= form-type "defstruct")
 (if (listp name)
     (list (%normalize-string (car name)))
     (list (%normalize-string name))))
```

The `(defun (setf ...) ...)` case continues to fall into `(t ...)` and
returns the full list printed as a string, unchanged.

### Documentation Note

Add to the tool description and `repl-driven-development.md`:
`(defun (setf name) ...)` requires `form_name: "(setf name)"` with
enclosing parentheses.

## Issue 2: Reader Error Detection in `lisp-check-parens`

### Root Cause

`lisp-check-parens` runs a custom character-by-character paren scan only.
It does not invoke the Lisp reader, so files with valid paren balance but
invalid reader syntax (unknown dispatch characters, malformed tokens, etc.)
receive `ok: true`, which is misleading.

### Fix

Add a second check using the standard CL reader after the paren scan.

**New helper** `%try-reader-check` in `src/validate.lisp`:

```lisp
(defun %try-reader-check (text base-offset)
  "Attempt to fully read TEXT; return a plist with reader error info or NIL."
  (with-input-from-string (stream text)
    (handler-case
        (let ((*read-eval* nil))
          (loop (when (eq :eof (read stream nil :eof)) (return nil))))
      (reader-error (e)
        (let* ((pos       (file-position stream))
               (pre       (subseq text 0 (min pos (length text))))
               (line      (1+ (count #\Newline pre)))
               (col-start (or (position #\Newline pre :from-end t) -1))
               (col       (- pos col-start)))
          (list :kind    "reader-error"
                :message (format nil "~A" e)
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (error (e)
        ;; Non-reader-error (e.g. package error): report without position
        (list :kind    "reader-error"
              :message (format nil "~A" e)
              :offset  base-offset
              :line    nil
              :column  nil)))))
```

**Changes to `lisp-check-parens`**:

- Run paren scan (existing).
- Run `%try-reader-check`.
- If paren scan fails: report paren error (existing behavior), add
  `reader_error` sub-field if reader also failed.
- If paren scan passes but reader fails: report reader error as the primary
  result with `ok: false`.
- Only if both pass: `ok: true`.

**Response shape for reader errors** (new `kind` value):

```json
{
  "ok": false,
  "kind": "reader-error",
  "message": "no dispatch character defined for #\\@",
  "position": { "offset": 42, "line": 3, "column": 8 }
}
```

Reader errors use `message` instead of `expected`/`found` (those fields are
specific to delimiter mismatch).

### Design Notes

- Uses standard CL reader with `*read-eval* nil` — no new dependencies.
- Position is derived from `(file-position stream)` after the error, which
  points to approximately where the reader stopped.
- Custom reader macro files (e.g. using `cl-interpol`) may produce false
  positives when read without the appropriate readtable. This is acceptable
  for the diagnostic use case.

## Issue 3: `#:` Prefix Normalization in Form Name Matching

### Root Cause

`%find-target` uses the user-supplied `form_name` as a lowercase string for
matching against `%definition-candidates` output. When Eclector reads
`#:my-package` it creates an uninterned symbol whose `SYMBOL-NAME` is
`MY-PACKAGE`, so the candidate is `"my-package"`. But if the user passes
`form_name: "#:my-package"`, `target` becomes `"#:my-package"` and the
comparison fails.

### Fix

Add helper `%strip-name-prefix` in `src/lisp-edit-form-core.lisp` and
apply it when deriving `target` in `%find-target`.

```lisp
(defun %strip-name-prefix (name)
  "Strip reader macro prefixes (#: : \"...\") from NAME for form-name matching."
  (cond
    ((and (>= (length name) 2) (string= (subseq name 0 2) "#:"))
     (subseq name 2))
    ((and (plusp (length name)) (char= (char name 0) #\:))
     (subseq name 1))
    ((and (>= (length name) 2)
          (char= (char name 0) #\")
          (char= (char name (1- (length name))) #\"))
     (subseq name 1 (1- (length name))))
    (t name)))
```

In `%find-target`, change:

```lisp
;; Before
(let ((target (string-downcase base-name)))

;; After
(let ((target (string-downcase (%strip-name-prefix base-name))))
```

This change applies to both `lisp-edit-form` and `lisp-patch-form` because
both call `%find-target` via `%locate-target-form`.

## Files Changed

| File | Change |
|------|--------|
| `src/lisp-edit-form-core.lisp` | Add `%strip-name-prefix`; fix `%definition-candidates` for `defstruct` |
| `src/validate.lisp` | Add `%try-reader-check`; update `lisp-check-parens` |
| `tests/tools-test.lisp` | New test cases for all three issues |
| `prompts/repl-driven-development.md` | Document `(setf name)` form_name syntax |

## Test Cases

### Issue 1
- File containing `(defstruct (foo (:include bar)) field1 field2)`:
  `lisp-edit-form` with `form_type: "defstruct"`, `form_name: "foo"` succeeds.
- File containing `(defstruct simple-struct field1)`:
  existing simple-name case still works.

### Issue 2
- Code with balanced parens but unknown dispatch char (e.g. `#@`):
  `lisp-check-parens` returns `ok: false`, `kind: "reader-error"`, non-nil `line`.
- Code with both unbalanced parens AND reader error:
  paren error is returned as primary, reader error may appear as secondary info.
- Valid Lisp code: `ok: true` (regression check).

### Issue 3
- `form_name: "#:my-package"` matches `(defpackage #:my-package ...)`.
- `form_name: ":my-keyword"` matches `(defpackage :my-package ...)`.
- `form_name: "my-package"` (bare) continues to work.
- `form_name: "(setf car)"` continues to match `(defun (setf car) ...)`.
