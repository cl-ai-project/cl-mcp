;;;; src/system-loader.lisp
;;;;
;;;; MCP tool for loading ASDF systems with structured output.
;;;; Solves three problems with raw (asdf:load-system) via repl-eval:
;;;; 1. Staleness: force=true (default) clears loaded state before reloading
;;;; 2. Output noise: suppresses verbose compilation/load output
;;;; 3. Timeout: dedicated timeout with worker-thread pattern

(defpackage #:cl-mcp/src/system-loader
  (:use #:cl)
  (:import-from #:cl-mcp/src/system-loader-core
                #:load-system)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-load-system-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:load-system))

(in-package #:cl-mcp/src/system-loader)

(define-tool "load-system"
  :description
  "Load an ASDF system with structured output and reload support.

Solves three problems with using (asdf:load-system) via repl-eval:
1. Staleness: force=true (default) clears loaded state before reloading
2. Output noise: suppresses verbose compilation/load output
3. Timeout: dedicated timeout prevents hanging on large systems

PREREQUISITE: The system must be findable by ASDF (registered via
asdf:load-asd or on the ASDF source registry / Quicklisp search paths). When it
is not, a matching <name>.asd under the project root is found, registered and
loaded (the response then carries auto_discovered_asd).

force=true clears ASDF's loaded state, but ASDF still recompiles only the files
it judges stale by their timestamps (one-second resolution).  A file of the
project's tree written in the same second as its FASL would look current, so
that FASL is deleted first, and the file is recompiled when this load reaches
it (same_second_fasls_deleted counts the deletions).  clear_fasls=true is the
guaranteed rebuild, of every file.

Warning handling: a file that compiles with a full WARNING (wrong argument
count, duplicate definition, type conflict, package variance) fails the load,
exactly as it does under asdf:load-system and run-tests; the error gives that
warning and its place as the cause, apart from any warning that only came
before it.  A full warning signalled outside a compilation (while
a file loads, or an undefined variable reported at the end) is listed and the
load goes on.  STYLE-WARNINGs (unused variable, undefined function) never fail
a load: they are counted and summed up by kind.  SBCL 'redefining X
in DEFUN' notifications are dropped, on a first load and a reload alike:
redefining is ordinary Common Lisp development, and a reload exists to do it.
Every warning kept is in the JSON warning_records, with its severity, class
and message, fails_compile when it is a full warning signalled while a file
compiled, and, where the compiler has a place for it, file, line and form: the
enclosing definition as the compiler names it, such as (defun wrong-arity).
form says where the warning is; it is not an argument for lisp-edit-form.  For
a top-level defun, defmacro or defclass the two read alike, but a method is
named by its specializers alone, (defmethod area (circle)), and a definition
inside eval-when by itself rather than by the eval-when.  Read the form at
file and line to address it.

Examples:
  First-time load: system='my-project', force=false
  Reload after edits: system='my-project' (force=true is default)
  Full recompile: system='my-project', clear_fasls=true"
  :args
  ((system :type :string :required t
    :description "ASDF system name (e.g., 'my-project', 'cl-mcp/tests')")
   (force :type :boolean :default t
    :description "Clear loaded state before loading to pick up changes (default: true)")
   (clear-fasls :type :boolean :json-name "clear_fasls"
    :description "Delete the cached FASLs under this system's source tree (its primary system's, for
a package-inferred subsystem) before loading, forcing that tree to recompile; dependencies in other
projects are not touched (default: false)")
   (timeout-seconds :type :number :json-name "timeout_seconds"
    :description "Timeout for the operation in seconds; positive (default: 120)"))
  :body
  (progn
    (when (and timeout-seconds (not (plusp timeout-seconds)))
      (error 'arg-validation-error
             :arg-name "timeout_seconds"
             :message "timeout_seconds must be a positive number"))
    (with-proxy-dispatch (id "worker/load-system"
                             (make-ht "system" system
                                      "force" force
                                      "clear_fasls" clear-fasls
                                      "timeout_seconds" timeout-seconds))
      (let ((ht (load-system system
                             :force force
                             :clear-fasls clear-fasls
                             :timeout-seconds (or timeout-seconds 120))))
        (result id (build-load-system-response system ht))))))
