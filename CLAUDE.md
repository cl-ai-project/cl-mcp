# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

@prompts/repl-driven-development.md

## Project Overview

This is a Common Lisp project called "clgrep" built using ASDF (Another System Definition Facility). The project is in early development with minimal functionality currently implemented.

## Project Structure

- `clgrep.asd` - ASDF system definition file defining the main system and test system
- `src/main.lisp` - Main source file containing the `clgrep` package
- `tests/main.lisp` - Test file using the Rove testing framework

## Development Commands

### Loading the System
```lisp
(asdf:load-system :clgrep)
```

### Running Tests
```lisp
(asdf:test-system :clgrep)
```

The test system uses Rove as the testing framework. Individual tests can be run by loading the test system and calling specific test functions.

### Linting with Mallet

Run mallet to check for code style issues:
```bash
mallet src/*.lisp tests/*.lisp roswell/*.ros
```

Common issues detected by mallet:
- Trailing whitespace

To fix trailing whitespace:
```bash
sed -i 's/[[:space:]]*$//' src/*.lisp tests/*.lisp roswell/*.ros
```

Always run mallet before committing to ensure code passes CI lint checks.

### Working with the REPL

Load the package in your REPL:
```lisp
(ql:quickload :clgrep)  ; If using Quicklisp
(in-package :clgrep)
```

## System Dependencies

- Main system: No external dependencies currently
- Test system: `rove` (testing framework)

## File Editing

This is a Common Lisp project. When editing `.lisp` files:
- Use `lisp-read-file` to read Lisp source files (provides collapsed view to save tokens)
- Use `lisp-edit-form` to edit existing Lisp forms (preserves structure and comments)
- Only use `fs-write-file` for non-Lisp files or creating new Lisp files

## Architecture Notes

The project uses UIOP for package definition (`uiop:define-package`). The main package is `clgrep` which uses only the `#:cl` (Common Lisp) package currently.
