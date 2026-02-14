# Explore Common Lisp Codebase
Systematically explore and understand a CL project structure, packages, and key abstractions.

## Goal

Build a mental map of an unfamiliar Common Lisp project: its system definition, package layout, key abstractions, and entry points. Output a concise architecture summary.

## Tools You'll Use

| Tool | Purpose |
|------|---------|
| `fs-set-project-root` | Anchor file operations to the project |
| `lisp-read-file` | Collapsed signatures for structure overview |
| `fs-list-directory` | Browse directory layout |
| `clgrep-search` | Find definitions and usages across files |
| `repl-eval` | Load system, inspect packages |
| `code-describe` | Symbol type, arglist, documentation |
| `code-find-references` | Trace how symbols connect |

## Workflow

### Step 1: Set Project Root
```json
{"name": "fs-set-project-root", "arguments": {"path": "."}}
```

### Step 2: Read the System Definition
Read the `.asd` file to understand dependencies, subsystems, and test configuration:
```json
{"name": "lisp-read-file", "arguments": {"path": "my-project.asd", "collapsed": true}}
```

### Step 3: Survey Directory Layout
```json
{"name": "fs-list-directory", "arguments": {"path": "src/"}}
```
Note the file organization: one file per module, package-inferred naming, test mirrors.

### Step 4: Scan Each Source File
Use collapsed view to see all top-level forms (functions, classes, macros) without reading bodies:
```json
{"name": "lisp-read-file", "arguments": {"path": "src/core.lisp", "collapsed": true}}
```

### Step 5: Load the System
```json
{"name": "repl-eval", "arguments": {"code": "(ql:quickload :my-project)", "package": "CL-USER"}}
```

### Step 6: Inspect Key Symbols
For important-looking functions or classes, get details:
```json
{"name": "code-describe", "arguments": {"symbol": "my-project:main-entry"}}
```

### Step 7: Trace Connections
Find how key abstractions are used across the codebase:
```json
{"name": "code-find-references", "arguments": {"symbol": "my-project:handle-request"}}
```

### Step 8: Search for Patterns
Find all definitions of a certain type:
```json
{"name": "clgrep-search", "arguments": {"pattern": "handle-", "form_types": ["defun"]}}
```

## Output

Summarize your findings as:
- **System structure**: Dependencies, subsystems, test setup
- **Package layout**: One-line description per package
- **Key abstractions**: Core classes, protocols, generic functions
- **Entry points**: How the system starts, main dispatch paths
- **Architecture notes**: Design patterns, conventions, notable choices

## Tips

- Start with `collapsed=true` — only expand definitions you need to understand.
- `clgrep-search` works without loading systems; `code-*` tools require `ql:quickload` first.
- Read `.asd` files with `lisp-read-file`, not `fs-read-file` — they are Lisp source.
- For large projects, explore breadth-first: all file signatures before any function bodies.
