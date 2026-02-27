# Paredit MCP — Specification

Structural S-expression editing operations exposed as MCP tools for LLM-driven Common Lisp development.

## 1. Problem Statement

Large language models struggle with Common Lisp (and other S-expression languages) because closing parentheses carry no semantic signal — they are determined entirely by structure. LLMs produce statistically plausible but structurally invalid parenthesization, especially in deeply nested forms. A single misplaced `)` cascades into broken code.

Existing mitigations in **cl-mcp**:

- `lisp-edit-form` — replaces/inserts entire top-level forms using Eclector CST. Works well for wholesale form replacement, but forces the LLM to regenerate the full form text (including all closing delimiters).
- `lisp-check-parens` — detects mismatched delimiters after the fact.
- **Parinfer** auto-repair — fixes missing closers based on indentation, but only works on well-indented input.

**Gap**: There is no way to perform *sub-form* structural edits — the operations that paredit provides in interactive editors. These operations are *structurally safe by construction*: they cannot produce unbalanced output because they manipulate the tree, not the text.

## 2. Solution Overview

Expose paredit-style structural editing operations as MCP tools. Each tool:

1. Parses the file into a CST (Concrete Syntax Tree) with source locations.
2. Identifies the target node(s) via an addressing scheme.
3. Applies a structural transformation on the text, guided by the CST.
4. Validates the result (balanced check).
5. Writes the result back (or returns a dry-run preview).

The LLM never needs to produce or count parentheses. Instead of "rewrite this `let` form with an extra binding", it says `sexp-slurp-forward` on the `let` — pull the next sibling expression into the `let` body.

## 3. Architecture

### 3.1 Deployment Model

**Standalone CL MCP server** (SBCL), distributable as:

- STDIO transport (primary — for editor integration)
- Streamable HTTP transport (for remote/container use)

Can be packaged as a **cl-mcp extension** (additional tool module loaded into cl-mcp) or as a **standalone server** using 40ants/mcp or cl-mcp's tool infrastructure. The spec is transport-agnostic.

### 3.2 Core Dependencies

| Dependency | Role |
|---|---|
| **Eclector** | S-expression parsing to CST with source locations |
| **cl-ppcre** | Text matching for addressing |
| **yason** | JSON serialization (MCP protocol) |
| **alexandria** | Utilities |

### 3.3 Parsing Pipeline

```
File on disk
  → read text
  → Eclector CST parse (preserves comments, whitespace, source locations)
  → tree of CST nodes, each with (start, end, kind, children)
  → addressing resolution (find target node)
  → structural transform (text surgery guided by CST positions)
  → balanced-check on result
  → write back / return preview
```

### 3.4 Relationship to cl-mcp

| Concern | cl-mcp `lisp-edit-form` | paredit-mcp |
|---|---|---|
| Granularity | Top-level forms only | Any nested sub-form |
| Operations | replace, insert_before, insert_after | wrap, unwrap, slurp, barf, raise, kill, split, join, transpose |
| Addressing | `form_type` + `form_name` | `form_type` + `form_name` + sub-form targeting |
| LLM writes parens? | Yes (full form text) | No (structural ops only) |
| Complementary? | Yes — use `lisp-edit-form` for adding/replacing whole top-level forms, paredit-mcp for restructuring internals | |

### 3.5 Reuse from Lem

Lem's `lem-paredit-mode` (639 LOC) implements all core operations on Lem's buffer/point abstraction. The *algorithms* (not the buffer code) can be ported:

- Sexp boundary detection → replaced by Eclector CST node spans
- Point movement → replaced by CST tree traversal
- Text insertion/deletion → replaced by string surgery at CST-derived offsets
- Indentation → `trivial-indent` or simple heuristic re-indentation post-transform

## 4. Addressing Scheme

Every mutating tool needs to identify a target node in the file. Three mechanisms are provided, in priority order:

### 4.1 `form_type` + `form_name` (Top-Level Anchor)

Same convention as cl-mcp. Identifies the top-level form to scope the search.

```json
{
  "form_type": "defun",
  "form_name": "process-request"
}
```

For `defmethod`, include specializers: `"form_name": "process-request (http-request stream)"`.

Index disambiguation: `"form_name": "resize[1]"` selects the second match.

### 4.2 `target` (Sub-Form Text Match)

**Primary mechanism for LLMs.** A string that matches the text of the target sub-expression. The server finds the CST node whose source text equals (or starts with) the target string, scoped within the top-level form.

```json
{
  "form_type": "defun",
  "form_name": "process-request",
  "target": "(let ((conn (get-connection)))"
}
```

Matching rules:

1. **Exact match**: target text equals the node's source text (whitespace-normalized).
2. **Prefix match**: target text matches the beginning of the node's source text (for long forms where the LLM quotes just the head). Minimum prefix: the opening delimiter + first child.
3. **Disambiguation**: If multiple nodes match, return an error listing all matches with their paths. The LLM can then use `path` or refine `target`.

### 4.3 `path` (Child Index Path)

A JSON array of zero-based child indices, navigating from the top-level form root into nested structure.

```json
{
  "form_type": "defun",
  "form_name": "process-request",
  "path": [3, 0, 1]
}
```

For `(defun process-request (req) (let ((conn (get-connection))) (handle conn req)))`:
- `[0]` → `process-request` (the name symbol)
- `[1]` → `(req)` (lambda list)
- `[2]` → `(let ...)` (body form)
- `[2, 0]` → `let`
- `[2, 1]` → `((conn (get-connection)))` (bindings)
- `[2, 1, 0]` → `(conn (get-connection))` (first binding)
- `[2, 2]` → `(handle conn req)` (let body)

### 4.4 `line` (Line Number Hint)

Optional 1-based line number. Used as a disambiguation hint when `target` matches multiple nodes — the match closest to `line` wins.

```json
{
  "target": "(if condition",
  "line": 42
}
```

### 4.5 Resolution Priority

1. If `path` is provided, use it (most precise).
2. If `target` is provided, find matching nodes within the scoped form.
3. If `line` is also provided, use it to disambiguate among matches.
4. If `form_type`/`form_name` are omitted, scope is the entire file (useful for small files or top-level operations).

## 5. Tool Catalog

### 5.1 Tier 1 — Structural Transforms

These are the highest-value tools for LLMs. Each one is structurally safe.

---

#### `sexp-wrap`

Wrap one or more consecutive sibling S-expressions in a new enclosing form.

**This is the single most important tool for LLMs** — it adds parentheses without the LLM ever writing a closing paren.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type for scoping |
| `form_name` | string | no | Top-level form name for scoping |
| `target` | string | cond. | Text of the first sexp to wrap |
| `path` | array | cond. | Child index path to first sexp |
| `count` | integer | no | Number of consecutive siblings to wrap (default: 1) |
| `wrapper` | string | no | Delimiter type: `"round"` (default), `"square"`, `"curly"`, `"double-quote"` |
| `head` | string | no | If provided, inserted as the first element of the new list (e.g., `"progn"`, `"let"`, `"when"`) |
| `dry_run` | boolean | no | Preview without writing |

**Examples:**

Wrap a single form in `progn`:
```json
{
  "file_path": "src/handler.lisp",
  "form_type": "defun",
  "form_name": "handle",
  "target": "(process-item item)",
  "head": "progn"
}
```
Before: `(defun handle (items) (process-item item))`
After: `(defun handle (items) (progn (process-item item)))`

Wrap 3 body forms in `let`:
```json
{
  "file_path": "src/handler.lisp",
  "form_type": "defun",
  "form_name": "handle",
  "path": [2],
  "count": 3,
  "head": "let ()"
}
```

---

#### `sexp-unwrap` (splice)

Remove the enclosing delimiters of a list, promoting its children into the parent. Inverse of `sexp-wrap`.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the list to unwrap |
| `path` | array | cond. | Path to the list to unwrap |
| `keep` | string | no | `"all"` (default) — keep all children. `"body"` — drop the operator (first child). |
| `dry_run` | boolean | no | Preview without writing |

**Example:**

Remove a `progn` wrapper:
```json
{
  "target": "(progn (step-1) (step-2))",
  "keep": "body"
}
```
Before: `(defun f () (progn (step-1) (step-2)))`
After: `(defun f () (step-1) (step-2))`

---

#### `sexp-raise`

Replace the parent list with a single child. The targeted sexp replaces its enclosing form.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the child sexp to raise |
| `path` | array | cond. | Path to the child sexp |
| `dry_run` | boolean | no | Preview without writing |

**Example:**

Simplify `(if condition (do-thing) nil)` to just `(do-thing)` by raising:
```json
{
  "target": "(do-thing)",
  "form_type": "defun",
  "form_name": "check"
}
```
Before: `(defun check (x) (if condition (do-thing) nil))`
After: `(defun check (x) (do-thing))`

Note: raises the target to replace its immediate parent. The parent of `(do-thing)` is `(if condition (do-thing) nil)`.

---

#### `sexp-slurp-forward`

Expand the targeted list by pulling the next sibling *into* it (moving the closing delimiter rightward past the next sexp).

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the list to grow |
| `path` | array | cond. | Path to the list |
| `count` | integer | no | Number of siblings to slurp (default: 1) |
| `dry_run` | boolean | no | Preview without writing |

**Example:**

```json
{
  "form_type": "defun",
  "form_name": "process",
  "target": "(let ((x 1)))"
}
```
Before: `(defun process () (let ((x 1))) (use-x x))`
After: `(defun process () (let ((x 1)) (use-x x)))`

---

#### `sexp-slurp-backward`

Expand the targeted list by pulling the previous sibling *into* it (moving the opening delimiter leftward past the previous sexp).

Same parameters as `sexp-slurp-forward`.

**Example:**

Before: `(defun f () (compute-value) (list result))`
After slurp-backward on `(list result)`: `(defun f () (list (compute-value) result))`

---

#### `sexp-barf-forward`

Shrink the targeted list by pushing its last child *out* (moving the closing delimiter leftward, ejecting the last child as a sibling).

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the list to shrink |
| `path` | array | cond. | Path to the list |
| `count` | integer | no | Number of children to barf out (default: 1) |
| `dry_run` | boolean | no | Preview without writing |

**Example:**

Before: `(let ((x 1)) (compute) (cleanup))`
After barf-forward: `(let ((x 1)) (compute)) (cleanup)`

---

#### `sexp-barf-backward`

Shrink the targeted list by pushing its first child (after the operator) *out* leftward.

Same parameters as `sexp-barf-forward`.

---

### 5.2 Tier 2 — Editing Operations

---

#### `sexp-kill`

Delete one or more S-expressions. Structurally safe — always removes complete sexps.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the sexp to kill |
| `path` | array | cond. | Path to the sexp |
| `count` | integer | no | Number of consecutive siblings to kill (default: 1) |
| `dry_run` | boolean | no | Preview without writing |

---

#### `sexp-transpose`

Swap two adjacent sibling S-expressions.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the first (left) sexp |
| `path` | array | cond. | Path to the first sexp |
| `dry_run` | boolean | no | Preview without writing |

**Example:**

Before: `(list alpha beta)` — transpose `alpha` and `beta`
After: `(list beta alpha)`

---

#### `sexp-split`

Split a list into two at the targeted child position. Children before the target stay in the first list; the target and children after it go into the second list. Both lists inherit the operator (first element) if `clone_head` is true.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the child where the split happens |
| `path` | array | cond. | Path to the split point child |
| `clone_head` | boolean | no | If true, both resulting lists get the operator symbol (default: false) |
| `dry_run` | boolean | no | Preview without writing |

**Example:**

Split a `progn` at `(step-3)`:
```json
{
  "target": "(step-3)",
  "clone_head": true
}
```
Before: `(progn (step-1) (step-2) (step-3) (step-4))`
After: `(progn (step-1) (step-2)) (progn (step-3) (step-4))`

---

#### `sexp-join`

Join two adjacent sibling lists into one. The second list's children are appended to the first.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the first list |
| `path` | array | cond. | Path to the first list |
| `drop_head` | boolean | no | If true, drop the operator of the second list before joining (default: false) |
| `dry_run` | boolean | no | Preview without writing |

**Example:**

Before: `(progn (a) (b)) (progn (c) (d))`
After (with `drop_head: true`): `(progn (a) (b) (c) (d))`

---

### 5.3 Tier 3 — Query / Inspection

---

#### `sexp-show-structure`

Return a tree view of the S-expression structure for a form. Essential for the LLM to understand the structure before performing edits.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type (omit for full file) |
| `form_name` | string | no | Top-level form name |
| `depth` | integer | no | Max depth to show (default: 4) |
| `show_paths` | boolean | no | Include `path` arrays in output (default: true) |
| `show_text` | boolean | no | Include source text snippets (default: true, truncated at 60 chars) |

**Output format:**

```json
{
  "form": "(defun process-request (req) ...)",
  "tree": [
    {"path": [0], "text": "process-request", "kind": "symbol"},
    {"path": [1], "text": "(req)", "kind": "list", "children": [
      {"path": [1, 0], "text": "req", "kind": "symbol"}
    ]},
    {"path": [2], "text": "(let ((conn (get-conn))) ...)", "kind": "list", "children": [
      {"path": [2, 0], "text": "let", "kind": "symbol"},
      {"path": [2, 1], "text": "((conn (get-conn)))", "kind": "list", "children": ["..."]},
      {"path": [2, 2], "text": "(handle conn req)", "kind": "list", "children": ["..."]}
    ]}
  ]
}
```

This gives the LLM exact `path` values and `target` text to use with mutation tools.

---

#### `sexp-get-enclosing`

Return the immediate enclosing form of a targeted node. Useful for understanding context before an operation like `sexp-raise` or `sexp-unwrap`.

| Parameter | Type | Required | Description |
|---|---|---|---|
| `file_path` | string | yes | Target file path |
| `form_type` | string | no | Top-level form type |
| `form_name` | string | no | Top-level form name |
| `target` | string | cond. | Text of the child node |
| `path` | array | cond. | Path to the child node |
| `levels` | integer | no | How many levels up to go (default: 1) |

**Output:**

```json
{
  "enclosing_text": "(if (valid-p req) (process req) (error-response 400))",
  "enclosing_path": [2],
  "enclosing_kind": "list",
  "head": "if",
  "child_index": 1,
  "sibling_count": 3
}
```

---

## 6. Input/Output Conventions

### 6.1 Common Input Parameters

All mutating tools share this base parameter set:

| Parameter | Type | Required | Notes |
|---|---|---|---|
| `file_path` | string | yes | Absolute or relative to project root |
| `form_type` | string | no | Top-level form type for scoping |
| `form_name` | string | no | Top-level form name for scoping |
| `target` | string | conditional | At least one of `target` or `path` required |
| `path` | array[int] | conditional | At least one of `target` or `path` required |
| `line` | integer | no | Disambiguation hint (1-based) |
| `dry_run` | boolean | no | Default false |

### 6.2 Success Response

All mutating tools return:

```json
{
  "content": [{"type": "text", "text": "Applied sexp-slurp-forward ..."}],
  "file_path": "src/handler.lisp",
  "operation": "sexp-slurp-forward",
  "bytes": 1842,
  "changed_region": {
    "start_line": 15,
    "end_line": 22,
    "text": "(let ((x 1))\n  (use-x x))"
  }
}
```

The `changed_region` provides the modified portion so the LLM can verify the result without re-reading the entire file.

### 6.3 Dry-Run Response

When `dry_run` is true:

```json
{
  "would_change": true,
  "original": "(let ((x 1)))",
  "preview": "(let ((x 1))\n  (use-x x))",
  "operation": "sexp-slurp-forward"
}
```

### 6.4 Error Responses

**Ambiguous target** (multiple matches):

```json
{
  "error": "Multiple matches for target",
  "matches": [
    {"path": [2, 1], "text": "(if (a) (b))", "line": 15},
    {"path": [3, 0], "text": "(if (c) (d))", "line": 22}
  ],
  "hint": "Use 'path' parameter or add 'line' to disambiguate"
}
```

**Target not found:**

```json
{
  "error": "No matching node found",
  "target": "(nonexistent-form)",
  "searched_within": "defun process-request"
}
```

**Structural impossibility** (e.g., slurp when there's no next sibling):

```json
{
  "error": "Cannot slurp forward: no sibling after target",
  "target_path": [2],
  "suggestion": "Target is the last child of its parent"
}
```

## 7. Indentation

After any structural transform, the affected region is re-indented following standard Lisp conventions:

- 2-space body indentation for known special forms (`defun`, `let`, `when`, `unless`, `lambda`, etc.)
- 1-space alignment for regular function calls (align with first argument)
- Configurable via a `style` parameter or indentation table

This is critical — without proper re-indentation, parinfer-style tools will misinterpret the structure on subsequent edits.

## 8. Development Environment

### 8.1 Implementation Language

Common Lisp (SBCL). Consistent with cl-mcp and the Lem ecosystem.

### 8.2 Project Structure

```
paredit-mcp/
├── paredit-mcp.asd          # ASDF system definition
├── src/
│   ├── package.lisp          # Package definition
│   ├── cst.lisp              # Eclector CST parsing (port/reuse from cl-mcp)
│   ├── addressing.lisp       # Target resolution (text match, path, line)
│   ├── transforms/
│   │   ├── wrap.lisp         # sexp-wrap, sexp-unwrap
│   │   ├── slurp-barf.lisp   # slurp/barf in both directions
│   │   ├── raise.lisp        # sexp-raise
│   │   ├── kill.lisp         # sexp-kill
│   │   ├── rearrange.lisp    # transpose, split, join
│   │   └── indent.lisp       # post-transform re-indentation
│   ├── tools.lisp            # MCP tool definitions
│   ├── server.lisp           # MCP server setup (STDIO + HTTP)
│   └── query.lisp            # sexp-show-structure, sexp-get-enclosing
├── tests/
│   ├── addressing-test.lisp
│   ├── transforms-test.lisp
│   └── fixtures/             # .lisp files used as test inputs
├── SPEC.md
└── README.md
```

### 8.3 Windows / Devcontainer Support

SBCL on Windows works but has edge cases. For consistent development:

- Use `egao1980/cl-devcontainer-templates` as a base
- Docker image with SBCL + Quicklisp + Ultralisp pre-configured
- Mount project directory into container
- STDIO transport works through Docker stdin/stdout piping

`.devcontainer/devcontainer.json`:
```json
{
  "name": "paredit-mcp",
  "image": "ghcr.io/egao1980/cl-devcontainer-templates:latest",
  "mounts": ["source=${localWorkspaceFolder},target=/workspace,type=bind"],
  "postCreateCommand": "cd /workspace && sbcl --load paredit-mcp.asd --eval '(ql:quickload :paredit-mcp)' --quit"
}
```

## 9. Implementation Phases

### Phase 1 — Foundation + Core Transforms

1. CST parsing with Eclector (reuse/port cl-mcp's `cst.lisp`)
2. Addressing resolution (target text matching, path navigation)
3. `sexp-show-structure` (query tool — needed for testing and LLM workflow)
4. `sexp-wrap` / `sexp-unwrap`
5. `sexp-slurp-forward` / `sexp-barf-forward`
6. Post-transform re-indentation
7. STDIO MCP server with tool registration

### Phase 2 — Complete Operations

8. `sexp-slurp-backward` / `sexp-barf-backward`
9. `sexp-raise`
10. `sexp-kill`
11. `sexp-transpose`
12. `sexp-get-enclosing`
13. `dry_run` support on all tools

### Phase 3 — Advanced + Polish

14. `sexp-split` / `sexp-join`
15. HTTP transport
16. Configurable indentation styles
17. Integration tests with cl-mcp (combined tool set)
18. Devcontainer packaging

## 10. Example LLM Workflow

A concrete scenario showing how an LLM would use these tools to refactor code.

**Task**: "Add error handling around the database call in `process-order`."

**Step 1** — Inspect structure:
```json
{"tool": "sexp-show-structure", "file_path": "src/orders.lisp", "form_type": "defun", "form_name": "process-order", "depth": 3}
```

Response reveals:
```
[0] process-order          (symbol)
[1] (order)                (list)
[2] (validate order)       (list)
[3] (db:insert order)      (list)    ← target
[4] (notify-success order) (list)
```

**Step 2** — Wrap the db call in `handler-case`:
```json
{"tool": "sexp-wrap", "file_path": "src/orders.lisp", "form_type": "defun", "form_name": "process-order", "target": "(db:insert order)", "head": "handler-case"}
```

Result: `(handler-case (db:insert order))` now exists where `(db:insert order)` was.

**Step 3** — The LLM can now use cl-mcp's `lisp-edit-form` to replace the `handler-case` form with the full version including error clauses — but it only needs to write one form, and the surrounding parens are already correct.

Alternatively, the LLM uses `sexp-slurp-forward` to pull `(notify-success order)` into the `handler-case` if that's part of the protected region.

**No parenthesis counting was needed at any step.**

## 11. Supported Languages

While designed for Common Lisp, the S-expression editing operations work on any language with balanced delimiters and sexp structure:

- **Common Lisp** (primary target)
- **Scheme** / **Racket**
- **Clojure** (with `[]` and `{}` support)
- **Emacs Lisp**
- **Fennel**
- **Janet**

Language-specific behavior (reader macros, dispatch characters) is handled by Eclector's readtable configuration. A `readtable` parameter on tools allows specifying custom reader syntax.
