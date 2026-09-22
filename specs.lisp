;;;; specs.lisp
;;;;
;;;; cl-mcp/specs: executable contracts and properties for a few of cl-mcp's own
;;;; functions, written with cl-spec.  Opt-in: nothing in cl-mcp.asd, main.lisp
;;;; or tests.lisp refers to this system, so loading or running cl-mcp never
;;;; needs cl-spec.  The dependency points one way only:
;;;;
;;;;   cl-mcp/specs --> cl-mcp/src/utils/{strings,sanitize,paths}, cl-mcp/src/fs,
;;;;                    cl-mcp/src/spec-core-record,
;;;;                    cl-spec/main, cl-spec/src/backends/check-it
;;;;
;;;; (The default suite does load specs/path-fixtures.lisp,
;;;; specs/write-fixtures.lisp and specs/core-record-fixtures.lisp, for its fixed
;;;; read, write and record cases; those files need no cl-spec and load nothing
;;;; of this bundle.)
;;;;
;;;; Loading this system registers the bundle in CL-SPEC:*REGISTRY* and does
;;;; nothing else: no check runs, no function is instrumented, no server or
;;;; worker is started, no scratch file is created and no ASDF system is
;;;; registered.  Run the checks with spec-check from an MCP client, or
;;;; with CL-MCP/SPECS/RUNNER.  docs/specs.md walks through both.

(defpackage #:cl-mcp/specs
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:*registry*)
  ;; A bare :import-from declares the check-it generator backend as an ASDF
  ;; dependency without importing any symbol from it.
  (:import-from #:cl-spec/src/backends/check-it)
  (:import-from #:cl-mcp/specs/strings)
  (:import-from #:cl-mcp/specs/sanitize)
  (:import-from #:cl-mcp/specs/paths)
  (:import-from #:cl-mcp/specs/write-paths)
  (:import-from #:cl-mcp/specs/core-records)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs)

(defun contract-names ()
  "Return the functions this bundle puts a Function Spec on.  Each is the
production symbol itself, so a Function Spec is listed under the home package
of the function it describes, not under a CL-MCP/SPECS package."
  (append (cl-mcp/specs/strings:contract-names)
          (cl-mcp/specs/sanitize:contract-names)
          (cl-mcp/specs/paths:contract-names)
          (cl-mcp/specs/write-paths:contract-names)
          (cl-mcp/specs/core-records:contract-names)))

(defun property-names ()
  "Return the properties this bundle defines."
  (append (cl-mcp/specs/strings:property-names)
          (cl-mcp/specs/sanitize:property-names)
          (cl-mcp/specs/paths:property-names)
          (cl-mcp/specs/write-paths:property-names)
          (cl-mcp/specs/core-records:property-names)))

(defun spec-names ()
  "Return the named data specs this bundle defines."
  (append (cl-mcp/specs/strings:spec-names)
          (cl-mcp/specs/sanitize:spec-names)
          (cl-mcp/specs/paths:spec-names)
          (cl-mcp/specs/write-paths:spec-names)
          (cl-mcp/specs/core-records:spec-names)))

(defun generator-names ()
  "Return the custom generators this bundle defines."
  (append (cl-mcp/specs/strings:generator-names)
          (cl-mcp/specs/sanitize:generator-names)
          (cl-mcp/specs/paths:generator-names)
          (cl-mcp/specs/write-paths:generator-names)
          (cl-mcp/specs/core-records:generator-names)))

(defun call-examples ()
  "Return the bundle's concrete calls, as (FUNCTION ARGUMENTS [CASE]), which the
runner checks with CL-SPEC:CHECK-CALL apart from generated trials."
  (append (cl-mcp/specs/strings:call-examples)
          (cl-mcp/specs/sanitize:call-examples)
          (cl-mcp/specs/paths:call-examples)
          (cl-mcp/specs/write-paths:call-examples)
          (cl-mcp/specs/core-records:call-examples)))

(defun register-specifications (&optional (registry *registry*))
  "Install every definition of this bundle in REGISTRY, the current
CL-SPEC:*REGISTRY* by default, and return the contract and property names.

Registering again replaces each definition by name and leaves every other
registration alone, so calling this after an edit, or into a fresh registry
from CL-SPEC:MAKE-HASH-TABLE-REGISTRY, is safe.  The definitions stay in
REGISTRY after the call returns: this does not bind a registry of its own."
  (let ((*registry* registry))
    (cl-mcp/specs/strings:register-specifications)
    (cl-mcp/specs/sanitize:register-specifications)
    (cl-mcp/specs/paths:register-specifications)
    (cl-mcp/specs/write-paths:register-specifications)
    (cl-mcp/specs/core-records:register-specifications))
  (values (contract-names) (property-names)))

;;; Loading the bundle registers it where MCP's spec-list and spec-symbol look.
(register-specifications)
