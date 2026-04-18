;;;; src/project-scaffold-templates.lisp
;;;;
;;;; Template string constants for project-scaffold. Kept in a dedicated
;;;; file so that bulk literal content does not clutter the logic modules.
;;;; All templates use {{name}}, {{description}}, {{author}}, {{license}},
;;;; {{parent-prompts}} placeholders resolved by render-template in
;;;; project-scaffold-core.

(defpackage #:cl-mcp/src/project-scaffold-templates
  (:use #:cl)
  (:export #:*asd-template*
           #:*claude-md-template*
           #:*agents-md-template*
           #:*readme-template*
           #:*gitignore-template*
           #:*main-lisp-template*
           #:*main-test-template*))

(in-package #:cl-mcp/src/project-scaffold-templates)

(defparameter *asd-template*
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
  :perform (test-op (o c)
                    (declare (ignore o))
                    (let ((test-packages
                           (remove-if-not
                            (lambda (dep)
                              (and (stringp dep)
                                   (uiop:string-prefix-p \"{{name}}/tests/\" dep)))
                            (asdf:system-depends-on c))))
                      (uiop:symbol-call :rove :run test-packages))))
"
  "Template for the generated project's .asd system definition.")

(defparameter *claude-md-template*
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

(defparameter *agents-md-template*
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

(defparameter *readme-template*
  "# {{name}}

{{description}}

## Usage

```lisp
(asdf:load-system :{{name}})
```

Add your code under `src/`; the scaffolded `src/main.lisp` ships empty
on purpose so you can define your own package exports without fighting
SBCL's package-variance checks on reload.

## Tests

```lisp
(asdf:test-system :{{name}})
```

## License

{{license}}
"
  "Template for the generated project's README.md.")

(defparameter *gitignore-template*
  "*.fasl
*.ufasl
*.x86f
*.cfasl
.asdf-cache/
"
  "Template for the generated project's .gitignore.")

(defparameter *main-lisp-template*
  ";;;; src/main.lisp

(defpackage #:{{name}}/src/main
  (:use #:cl))

(in-package #:{{name}}/src/main)
"
  "Template for the generated project's src/main.lisp.
Intentionally empty past (in-package ...): no stub defun or dangling
(:export ...) clauses so the first load-system does not pin symbols
into the worker image. Add your own defuns/defclasses below.")

(defparameter *main-test-template*
  ";;;; tests/main-test.lisp

(defpackage #:{{name}}/tests/main-test
  (:use #:cl #:rove))

(in-package #:{{name}}/tests/main-test)

(deftest scaffold-smoke
  (testing \"scaffold main package loads\"
    (ok (find-package :{{name}}/src/main))))
"
  "Template for the generated project's tests/main-test.lisp.
Exists so `run-tests` has at least one green assertion out of the box;
holds no reference to any symbol the main package does not define, so
deleting this file or replacing the test is free of cascading errors.")
