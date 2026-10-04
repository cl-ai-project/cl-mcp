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
it judges stale by their timestamps (one-second resolution): an edit made in the
same second as the last compile can be missed. clear_fasls=true is the
guaranteed rebuild.

Warning handling: SBCL 'redefining X in DEFUN' notifications are dropped
from the warnings count/details -- a reload redefines what it reloads, and a
dependency the worker already had is read again -- except a conflict the
project can act on: a file under the system's directory replacing what
another file defined (two files defining one name, or a DEFUN on a
library's symbol inherited by :use).  Such a warning stays, naming both
files.  A definition moved to another file is reported once, by the load
that moves it.  A name defined twice in one file is SBCL's separate 'Duplicate
definition' warning for a DEFUN or DEFMACRO; one DEFMETHOD written twice in
a file is not reported.
Real warnings (style, type, package variance) always pass through
unchanged.

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
