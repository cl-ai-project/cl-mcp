# clgrep

[![CI](https://github.com/masatoi/clgrep/actions/workflows/ci.yml/badge.svg)](https://github.com/masatoi/clgrep/actions/workflows/ci.yml)
[![Lint](https://github.com/masatoi/clgrep/actions/workflows/lint.yml/badge.svg)](https://github.com/masatoi/clgrep/actions/workflows/lint.yml)

A semantic grep tool for Common Lisp codebases that understands Lisp structure and provides rich contextual information.

## Overview

`clgrep` is a powerful search tool designed specifically for Common Lisp projects. Unlike traditional grep, it:

- **Understands Lisp structure**: Returns complete top-level forms (functions, macros, classes, etc.) containing matches
- **Package-aware**: Identifies which package each match belongs to
- **Respects .gitignore**: Automatically excludes files and directories specified in .gitignore
- **Semantic search**: Provides structured results with file, line number, package, matching line, and complete form context
- **JSON output**: Machine-readable format for integration with AI agents, IDEs, and other tools

## Features

### Core Functionality

- **Pattern matching**: Regular expression search using `cl-ppcre`
- **Recursive directory traversal**: Search entire project trees
- **File filtering**: Automatically targets `.lisp`, `.asd`, and `.ros` files
- **Form extraction**: Extracts the complete top-level form containing each match
- **Package detection**: Tracks `(in-package ...)` declarations to identify package context
- **Smart truncation**: For very large forms (>2000 chars), shows context around the match

### Command-Line Interface

The `roswell/clgrep.ros` Roswell script provides a grep-like interface:

```bash
clgrep [OPTIONS] PATTERN [PATH]
```

#### Options

- `-r, --recursive`: Search recursively through subdirectories
- `-i, --ignore-case`: Case-insensitive search
- `-t, --form-type TYPES`: Filter by form types (comma-separated, e.g., `defun,defmethod`)
- `-s, --only-signature`: Output signature only, omit full form text (reduces output size for AI agents)
- `--json`: Output results in JSON format (for AI/tool integration)
- `-h, --help`: Display help message

**Available form types for filtering:**
- Functions/macros: `defun`, `defmethod`, `defgeneric`, `defmacro`, `define-compiler-macro`
- Variables: `defvar`, `defparameter`, `defconstant`
- Types: `defclass`, `defstruct`, `deftype`, `define-condition`
- Packages: `defpackage`, `in-package`
- Systems: `defsystem`, `deftest`

#### Output Formats

**Text (default)**: Human-readable format
```
/path/to/file.lisp:42: (defun foo (x)
/path/to/file.lisp:58: (defun bar (y)
```

**JSON**: Structured data for programmatic consumption
```json
[
  {
    "file": "/path/to/file.lisp",
    "line": 42,
    "match": "(defun foo (x)",
    "package": "MY-PACKAGE",
    "formType": "defun",
    "formName": "foo",
    "signature": "(foo x)",
    "form": "(defun foo (x)\n  (+ x 1))",
    "formStartLine": 42,
    "formEndLine": 43,
    "formStartByte": 1823,
    "formEndByte": 1856
  }
]
```

**JSON with `--only-signature`**: Compact output without full form text
```json
[
  {
    "file": "/path/to/file.lisp",
    "line": 42,
    "match": "(defun foo (x)",
    "package": "MY-PACKAGE",
    "formType": "defun",
    "formName": "foo",
    "signature": "(foo x)",
    "formStartLine": 42,
    "formEndLine": 43,
    "formStartByte": 1823,
    "formEndByte": 1856
  }
]
```

## Installation

### Prerequisites

- [Roswell](https://github.com/roswell/roswell) - Common Lisp implementation manager
- [Quicklisp](https://www.quicklisp.org/) - Common Lisp library manager
- SBCL or another Common Lisp implementation

### Install Dependencies

The script automatically loads required libraries via Quicklisp:
- `cl-ppcre` - Regular expression library
- `uiop` - Portability utilities (usually included)
- `yason` - JSON encoding/decoding

### Setup

1. Clone the repository:
```bash
git clone https://github.com/yourusername/clgrep.git
cd clgrep
```

2. Make the script executable:
```bash
chmod +x roswell/clgrep.ros
```

3. (Optional) Add to your PATH:
```bash
ln -s $(pwd)/roswell/clgrep.ros ~/bin/clgrep
# or
export PATH="$PATH:$(pwd)/roswell"
```

## Usage Examples

### Basic Search

Search for a pattern in the current directory:
```bash
./roswell/clgrep.ros "defun.*foo" .
```

### Recursive Search

Search through entire project tree:
```bash
./roswell/clgrep.ros -r "defclass.*widget" /path/to/project
```

### Case-Insensitive Search

Find matches regardless of case:
```bash
./roswell/clgrep.ros -r -i "error" /path/to/project
```

### JSON Output for Tools

Get structured output for processing by other tools:
```bash
./roswell/clgrep.ros -r --json "defmethod" /path/to/project | jq '.[] | select(.package=="MY-PACKAGE")'
```

### Compact Output for AI Agents

Use `--only-signature` to reduce output size (typically 60-70% smaller):
```bash
./roswell/clgrep.ros -r --json -s "defun" /path/to/project
```
This outputs signature information without the full form text, ideal for AI agents that only need to identify code locations before reading specific files.

### Search Specific Patterns

Find all exported functions:
```bash
./roswell/clgrep.ros -r "defun.*:export" /path/to/project
```

Find all test definitions:
```bash
./roswell/clgrep.ros -r "deftest" /path/to/project/tests
```

Find macro definitions:
```bash
./roswell/clgrep.ros -r "defmacro" /path/to/project
```

### Form Type Filtering

Find only function definitions containing "parse":
```bash
./roswell/clgrep.ros -r -t defun "parse" /path/to/project
```

Find functions and methods containing "validate":
```bash
./roswell/clgrep.ros -r -t defun,defmethod "validate" /path/to/project
```

Find all class definitions with JSON output:
```bash
./roswell/clgrep.ros -r -t defclass --json "." /path/to/project
```

## Library API

You can also use `clgrep` as a Common Lisp library by loading the system:

```lisp
(ql:quickload :clgrep)
```

### Main Functions

#### `semantic-grep`

Search across a directory tree with semantic understanding:

```lisp
(clgrep:semantic-grep root-directory pattern
                      &key (recursive t) case-insensitive form-types (include-form t))
```

**Parameters:**
- `root-directory`: Starting directory for search
- `pattern`: Regular expression pattern (string)
- `recursive`: If true (default), search subdirectories recursively
- `case-insensitive`: If true, ignore case (default: nil)
- `form-types`: List of form type strings to filter by (e.g., `'("defun" "defmethod")`)
- `include-form`: If true (default), include the full form text; if nil, omit it to reduce output size

**Returns:** List of alists, each containing:
- `:file` - Full path to file
- `:line` - Line number of the match (integer)
- `:match` - The matching line (string)
- `:package` - Package name at that location (string)
- `:form-type` - Type of the form (e.g., "defun", "defclass"), or nil
- `:form-name` - Name of the form (e.g., function name), or nil
- `:signature` - Compact signature of the form (e.g., "(foo x y)" for defun)
- `:form` - Complete top-level form (string, may be truncated if >2000 chars; omitted if `include-form` is nil)
- `:form-start-line` - Start line of the containing form
- `:form-end-line` - End line of the containing form
- `:form-start-byte` - Start byte offset of the containing form
- `:form-end-byte` - End byte offset of the containing form

**Example:**
```lisp
(let ((results (clgrep:semantic-grep "/path/to/project" "defun.*foo")))
  (dolist (result results)
    (format t "Found in ~A:~A (package: ~A)~%"
            (cdr (assoc :file result))
            (cdr (assoc :form-start-line result))
            (cdr (assoc :package result)))))
```

**Example with form-type filtering:**
```lisp
;; Find only defun forms containing "parse"
(clgrep:semantic-grep "/path/to/project" "parse"
                      :form-types '("defun"))
```

**Example with signature-only output (for AI agents):**
```lisp
;; Get compact results without full form text
(clgrep:semantic-grep "/path/to/project" "parse"
                      :include-form nil)
;; Each result has :signature but no :form key
```

#### `extract-toplevel-form`

Extract the top-level form containing a specific line:

```lisp
(clgrep:extract-toplevel-form content line-number &optional (context-lines 5))
```

**Parameters:**
- `content`: File content as a string
- `line-number`: Target line number
- `context-lines`: Number of context lines to keep if truncating (default: 5)

**Returns:** An alist with the following keys, or NIL if not found:
- `:text` - The form text (string, may be truncated if >2000 chars)
- `:start-line` - Start line of the form
- `:end-line` - End line of the form
- `:start-byte` - Start byte offset
- `:end-byte` - End byte offset

**Example:**
```lisp
(let* ((content (uiop:read-file-string "src/main.lisp"))
       (form-info (clgrep:extract-toplevel-form content 42)))
  (when form-info
    (format t "Form at lines ~A-~A:~%~A~%"
            (cdr (assoc :start-line form-info))
            (cdr (assoc :end-line form-info))
            (cdr (assoc :text form-info)))))
```

#### `extract-form-signature`

Extract a compact signature from a form:

```lisp
(clgrep:extract-form-signature form-text)
```

**Parameters:**
- `form-text`: A string containing a Lisp form (e.g., "(defun foo (x y) ...)")

**Returns:** A string containing the signature, or NIL if not a recognized form type

**Supported form types:**
- `defun`, `defmacro`, `defgeneric`, `define-compiler-macro`: Returns `(name args...)`
- `defmethod`: Returns `(name specializers... args...)`
- `defvar`, `defparameter`, `defconstant`: Returns the variable name
- `defclass`: Returns `(name superclasses...)` or just name if no superclasses
- `defstruct`: Returns the structure name

**Example:**
```lisp
(clgrep:extract-form-signature "(defun foo (x y &key z) (+ x y z))")
;; => "(foo x y &key z)"

(clgrep:extract-form-signature "(defmethod emit ((logger json-logger) event) body)")
;; => "(emit (logger json-logger) event)"

(clgrep:extract-form-signature "(defclass my-class (parent-class) ())")
;; => "(my-class parent-class)"
```

#### `collect-target-files`

Collect all Lisp source files in a directory:

```lisp
(clgrep:collect-target-files root-directory &key (recursive t))
```

**Parameters:**
- `root-directory`: Directory to search
- `recursive`: If true (default), search subdirectories recursively

**Returns:** List of pathnames for `.lisp`, `.asd`, and `.ros` files

**Features:**
- Respects `.gitignore` patterns
- Excludes `.git` directories
- Recursively searches subdirectories by default

**Example:**
```lisp
(let ((files (clgrep:collect-target-files "/path/to/project")))
  (format t "Found ~D Lisp files~%" (length files)))
```

#### `grep-file`

Simple grep-like search in a single file:

```lisp
(clgrep:grep-file pattern filepath)
```

**Parameters:**
- `pattern`: Regular expression pattern
- `filepath`: Path to file

**Returns:** Number of matches found

**Side effect:** Prints matching lines to standard output

**Example:**
```lisp
(clgrep:grep-file "defun" "src/main.lisp")
```

## How It Works

### Form Extraction Algorithm

1. **Character-by-character scanning**: The code doesn't use `READ` to avoid evaluation risks
2. **State machine**: Tracks whether currently in a string, comment, or normal code
3. **Balanced parentheses**: Counts opening and closing parens to identify form boundaries
4. **Escape handling**: Correctly handles escaped quotes in strings (`\"`)
5. **Line tracking**: Maintains line numbers throughout scanning

### Package Detection

Searches backwards from the match line to find the most recent `(in-package ...)` form:
- Supports `:package`, `#:package`, and `"package"` syntax
- Normalizes to uppercase package names
- Returns "UNKNOWN" if no package declaration found

### .gitignore Support

Implements gitignore glob pattern matching:
- `*` - Matches any characters except `/`
- `**` - Matches any directory depth
- `?` - Matches single character
- `/` prefix - Anchors to repository root
- `/` suffix - Matches directories

## Use Cases

### For Developers

- **Quick code navigation**: Find function definitions across large projects
- **Refactoring**: Locate all uses of a specific pattern
- **Code review**: Understand context around specific code patterns
- **Documentation**: Generate function listings with context

### For AI Agents

- **Code understanding**: Provide complete context for code snippets
- **Symbol resolution**: Identify package membership for symbols
- **Semantic search**: Find implementations rather than just text matches
- **Structured results**: JSON output for easy parsing
- **Token efficiency**: Use `--only-signature` for compact search results, then read specific files as needed
- **Two-stage workflow**: First search with signatures to locate, then fetch full forms for selected results

### For IDE Integration

- **Jump to definition**: With full form context
- **Symbol search**: Package-aware searching
- **Project-wide operations**: Using .gitignore-aware file collection

## Architecture

```
roswell/clgrep.ros (CLI)
    ↓
semantic-grep (orchestration)
    ↓
    ├→ collect-target-files (file discovery)
    │   ├→ parse-gitignore
    │   ├→ glob-to-regex
    │   └→ path-ignored-p
    │
    └→ search-in-file (per-file search)
        ├→ extract-package-for-line
        └→ extract-toplevel-form
            └→ scan-toplevel-forms
```

## Testing

Run the test suite:

```bash
ros run
```

```lisp
(ql:quickload :clgrep)
(asdf:test-system :clgrep)
```

The test suite covers:
- Form extraction with various edge cases
- Glob pattern conversion
- File collection with .gitignore
- Package extraction
- Complete semantic search workflows

## Exit Codes

- `0`: Success (matches found)
- `1`: No matches found (standard grep behavior)
- `2`: Invalid regular expression
- `3`: Other errors (file not found, etc.)

## Limitations

- **Memory usage**: Large files are read entirely into memory
- **Binary files**: May produce errors on non-text files (use .gitignore to exclude)
- **Complex reader macros**: Only handles standard syntax (strings, comments, parens)
- **Performance**: Character-by-character scanning is slower than compiled regex for large files

## Future Enhancements

- [ ] Parallel file processing for large projects
- [ ] Incremental reading for very large files
- [ ] Support for more reader macros (#+ #- etc.)
- [ ] Color output for terminal
- [ ] Context lines around matches (like `grep -C`)
- [ ] Performance optimizations
- [ ] MCP (Model Context Protocol) server implementation

## Contributing

Contributions are welcome! Please ensure:

1. All tests pass: `(asdf:test-system :clgrep)`
2. Code follows existing style
3. New features include tests
4. Update documentation

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Credits

Built with:
- [cl-ppcre](https://edicl.github.io/cl-ppcre/) - Portable regular expressions
- [UIOP](https://common-lisp.net/project/asdf/uiop.html) - Portability library
- [yason](https://github.com/phmarek/yason) - JSON encoding
- [Roswell](https://github.com/roswell/roswell) - Common Lisp scripting

## Related Projects

- [grep](https://www.gnu.org/software/grep/) - Traditional text search
- [ripgrep](https://github.com/BurntSushi/ripgrep) - Fast recursive search
- [ag (The Silver Searcher)](https://github.com/ggreer/the_silver_searcher) - Code-searching tool
- [semantic-grep](https://github.com/semgrep/semgrep) - Pattern matching for many languages
