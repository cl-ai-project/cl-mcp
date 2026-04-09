;;;; src/project-scaffold-templates.lisp
;;;;
;;;; Template string constants for project-scaffold. Kept in a dedicated
;;;; file so that bulk literal content does not clutter the logic modules.
;;;; All templates use {{name}}, {{description}}, {{author}}, {{license}},
;;;; {{parent-prompts}} placeholders resolved by render-template in
;;;; project-scaffold-core.

(defpackage #:cl-mcp/src/project-scaffold-templates
  (:use #:cl)
  (:export #:+asd-template+
           #:+claude-md-template+
           #:+agents-md-template+
           #:+readme-template+
           #:+gitignore-template+
           #:+main-lisp-template+
           #:+main-test-template+))

(in-package #:cl-mcp/src/project-scaffold-templates)

(defparameter +asd-template+
  ";;;; {{name}}.asd

(asdf:defsystem \"{{name}}\"
  :class :package-inferred-system
  :description \"{{description}}\"
  :author \"{{author}}\"
  :license \"{{license}}\"
  :version \"0.1.0\"
  :depends-on (\"{{name}}/src/main\")
  :in-order-to ((test-op (test-op \"{{name}}/tests\"))))

(asdf:defsystem \"{{name}}/tests\"
  :class :package-inferred-system
  :depends-on (\"rove\"
               \"{{name}}\"
               \"{{name}}/tests/main-test\")
  :perform (test-op (o c) (uiop:symbol-call :rove :run c)))
"
  "Template for the generated project's .asd system definition.")

(defparameter +claude-md-template+
  "# CLAUDE.md

## Agent Guidelines

@{{parent-prompts}}/repl-driven-development.md
@{{parent-prompts}}/common-lisp-expert.md

## Project Overview

{{description}}

This project was scaffolded by cl-mcp's `project-scaffold` tool. It follows
cl-mcp's recommended structure: package-inferred-system + Rove.

## Self-Hosted Development

Use cl-mcp tools for all Lisp code operations:

- Search: `clgrep-search`
- Read: `lisp-read-file`
- Edit: `lisp-edit-form`, `lisp-patch-form`
- Eval: `repl-eval`
- Tests: `run-tests` with {\"system\": \"{{name}}/tests\"}

## Testing

```lisp
;; From repl-eval
(asdf:test-system :{{name}})
```

## Repository Structure

`src/`      Source code (package-inferred-system)
`tests/`    Rove test suites
"
  "Template for the generated project's CLAUDE.md.")

(defparameter +agents-md-template+
  "# Repository Guidelines

@{{parent-prompts}}/repl-driven-development.md
@{{parent-prompts}}/common-lisp-expert.md

## Project Overview

{{description}}

See `CLAUDE.md` for full agent guidelines - this file mirrors the essentials
for tools that read `AGENTS.md` by convention.

## Build, Test, and Development

Load via `load-system` and iterate via `repl-eval`. Run the test suite with
`run-tests` using system name `{{name}}/tests`.

## Coding Style

Follow the Google Common Lisp Style Guide: 2-space indent, <=100 columns,
lisp-case identifiers, docstrings on public functions.
"
  "Template for the generated project's AGENTS.md.")

(defparameter +readme-template+
  "# {{name}}

{{description}}

## Usage

```lisp
(asdf:load-system :{{name}})
({{name}}/src/main:greet \"world\")
;; => \"Hello, world!\"
```

## Tests

```lisp
(asdf:test-system :{{name}})
```

## License

{{license}}
"
  "Template for the generated project's README.md.")

(defparameter +gitignore-template+
  "*.fasl
*.ufasl
*.x86f
*.cfasl
.asdf-cache/
"
  "Template for the generated project's .gitignore.")

(defparameter +main-lisp-template+
  ";;;; src/main.lisp

(defpackage #:{{name}}/src/main
  (:use #:cl)
  (:export #:greet))

(in-package #:{{name}}/src/main)

(defun greet (who)
  \"Return a friendly greeting for WHO.\"
  (format nil \"Hello, ~A!\" who))
"
  "Template for the generated project's src/main.lisp.")

(defparameter +main-test-template+
  ";;;; tests/main-test.lisp

(defpackage #:{{name}}/tests/main-test
  (:use #:cl #:rove)
  (:import-from #:{{name}}/src/main
                #:greet))

(in-package #:{{name}}/tests/main-test)

(deftest greet-test
  (testing \"greet returns a hello string\"
    (ok (equal \"Hello, world!\" (greet \"world\")))))
"
  "Template for the generated project's tests/main-test.lisp.")
