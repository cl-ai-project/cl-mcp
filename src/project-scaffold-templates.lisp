;;;; src/project-scaffold-templates.lisp
;;;;
;;;; Template string constants for project-scaffold. Kept in a dedicated
;;;; file so that bulk literal content does not clutter the logic modules.
;;;; All templates use {{name}}, {{description}}, {{author}}, {{license}},
;;;; {{parent-prompts}}, {{test-framework}} placeholders resolved by
;;;; render-template in project-scaffold-core.
;;;;
;;;; The .asd and tests/main-test.lisp templates come in one variant per
;;;; supported test framework; project-scaffold-core picks the pair and
;;;; supplies {{test-framework}} as the matching human-readable label.

(defpackage #:cl-mcp/src/project-scaffold-templates
  (:use #:cl)
  (:export #:*asd-template-rove*
           #:*asd-template-fiveam*
           #:*claude-md-template*
           #:*agents-md-template*
           #:*readme-template*
           #:*gitignore-template*
           #:*main-lisp-template*
           #:*main-test-template-rove*
           #:*main-test-template-fiveam*))

(in-package #:cl-mcp/src/project-scaffold-templates)

(defparameter *asd-template-rove*
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
  "Template for the generated project's .asd when the framework is Rove.
Unlike the Markdown templates, every placeholder here sits inside a Lisp
string literal, so the values must be escaped with
CL-MCP/SRC/PROJECT-SCAFFOLD-CORE:ESCAPE-LISP-STRING before substitution:
an unescaped double quote would close the literal and turn the rest of
the value into top-level forms. PLAN-SCAFFOLD does that; keep any new
placeholder added here inside a string literal, or revisit the escaping.
The same applies to *ASD-TEMPLATE-FIVEAM*.")

(defparameter *asd-template-fiveam*
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
  :depends-on (\"fiveam\"
               \"{{name}}\"
               \"{{name}}/tests/main-test\")
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :fiveam :run! :{{name}})))
"
  "Template for the generated project's .asd when the framework is FiveAM.
Runs the project's own root suite rather than every registered one:
FiveAM's suite registry is global, so a blanket run would also execute
the suites of every other system loaded into the image.  The suite is
named after the primary system, which is both the dominant idiom in the
wild and the spelling cl-mcp's own run-tests suite matcher looks for.
See *ASD-TEMPLATE-ROVE* for the escaping contract these placeholders
share.")

(defparameter *claude-md-template*
  "# CLAUDE.md

## Agent Guidelines

@{{parent-prompts}}/repl-driven-development.md
@{{parent-prompts}}/common-lisp-expert.md

## Project Overview

{{description}}

This project was scaffolded by cl-mcp's `project-scaffold` tool. It follows
cl-mcp's recommended structure: package-inferred-system + {{test-framework}}.

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
`tests/`    {{test-framework}} test suites
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
`run-tests` using system name `{{name}}/tests`. The tests are written with
{{test-framework}}.

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

Written with {{test-framework}}.

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

(defparameter *main-test-template-rove*
  ";;;; tests/main-test.lisp

(defpackage #:{{name}}/tests/main-test
  (:use #:cl #:rove))

(in-package #:{{name}}/tests/main-test)

(deftest scaffold-smoke
  (testing \"scaffold main package loads\"
    (ok (find-package :{{name}}/src/main))))
"
  "Template for the generated project's tests/main-test.lisp under Rove.
Exists so `run-tests` has at least one green assertion out of the box;
holds no reference to any symbol the main package does not define, so
deleting this file or replacing the test is free of cascading errors.")

(defparameter *main-test-template-fiveam*
  ";;;; tests/main-test.lisp

(defpackage #:{{name}}/tests/main-test
  (:use #:cl #:fiveam))

(in-package #:{{name}}/tests/main-test)

(def-suite :{{name}}
  :description \"Root suite for {{name}}. Nest further suites under it with
(def-suite <name> :in :{{name}}), so one run covers the whole project.\")

(in-suite :{{name}})

(test scaffold-smoke
  \"scaffold main package loads\"
  (is (find-package :{{name}}/src/main)))
"
  "Template for the generated project's tests/main-test.lisp under FiveAM.
Serves the same purpose as *MAIN-TEST-TEMPLATE-ROVE*, and additionally
establishes the project's root suite: FiveAM has no per-package test
discovery, so a suite the tooling can find has to be declared somewhere,
and the generated .asd's test-op runs exactly this one.")
