;;;; src/server-instructions.lisp
;;;;
;;;; The instructions cl-mcp returns from initialize: the standing guidance an
;;;; agent gets from enabling the server, before it reads any tool description.
;;;;
;;;; With tool search, Claude Code defers every tool description and keeps only
;;;; tool names and these instructions in context, so this text is often the
;;;; only cl-mcp guidance a model has.  It is also cut: Claude Code truncates it
;;;; at 2,048 characters, from the end.  So it holds cross-tool rules only --
;;;; what one tool's description cannot say -- and the full guides stay in
;;;; prompts/.

(defpackage #:cl-mcp/src/server-instructions
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/registry
                #:enabled-tool-group-instructions)
  (:export #:+instructions-budget+
           #:+base-instructions+
           #:server-instructions))

(in-package #:cl-mcp/src/server-instructions)

(defparameter +instructions-budget+ 2048
  "Most characters the joined instructions may have, for any set of groups.
Claude Code truncates server instructions past 2,048 characters.")

(defparameter +base-instructions+
  "cl-mcp is a Common Lisp development server: a live SBCL REPL, ASDF loading and
structure-aware editing of Lisp source.

1. Call fs-set-project-root with the project's absolute path before any file tool.
2. For .lisp/.asd files use clgrep-search, lisp-read-file, lisp-edit-form and
lisp-patch-form, never shell grep/cat/sed: they parse the code and keep its comments.
3. An edit changes the file only. Run load-system before repl-eval or code-* tools see
it; run-tests reloads its test system itself.

Loop: explore (clgrep-search; lisp-read-file collapsed, then name_pattern) -> try
(repl-eval with package) -> persist (lisp-edit-form) -> verify (load-system, run-tests).

With the worker pool (default), repl-eval, load-system, run-tests, code-*,
clos-describe, lisp-macroexpand and inspect-object run in this session's own worker
image. Definitions made in repl-eval live only there. When a response says the worker
was lost, its state is gone: load-system again; old object ids are refused.
A defmethod's form_name includes its specializers: \"print-object ((o point) stream)\".
If a file no longer parses, lisp-check-parens shows where and the likely fix."
  "Instructions every client gets, whatever groups are on.  The three numbered
rules come first because a client may read only the opening.")

(defun server-instructions ()
  "Return the instructions for initialize: the base text, then the text of each
enabled tool group, separated by a blank line.

Computed per call from *ENABLED-TOOL-GROUPS*, the same setting tools/list
reads, so the two cannot describe different tool sets."
  (format nil "~A~{~%~%~A~}" +base-instructions+ (enabled-tool-group-instructions)))
