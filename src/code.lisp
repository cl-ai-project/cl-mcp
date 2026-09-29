;;;; src/code.lisp

(defpackage #:cl-mcp/src/code
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-core
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references
                #:code-find-references-report
                #:generic-function-method-count)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:scan-project
                #:top-level-forms-at)
  (:import-from #:cl-mcp/src/code-refs-core
                #:place-references-in-source)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-code-find-response
                #:build-code-describe-response
                #:build-code-find-references-response)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:proxy-to-worker
                #:with-proxy-dispatch)
  (:export
   #:code-find-definition
   #:code-describe-symbol
   #:code-find-references))

(in-package #:cl-mcp/src/code)

(define-tool "code-find"
  :description "Locate the definition of a symbol (path and line) using sb-introspect.

PREREQUISITE: The defining system MUST be loaded first (load-system tool).
Prefer package-qualified symbols or supply the package argument.

NOTE: If the symbol is not found, the system might not be loaded yet.
For code exploration WITHOUT loading systems, use 'clgrep-search' instead.
Fallback: Use 'lisp-read-file' with 'name_pattern' to search the file system."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl:mapcar\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified; ensure the package exists
and is loaded"))
  :body
  (with-proxy-dispatch (id "worker/code-find"
                          (make-ht "symbol" symbol "package" package))
    (multiple-value-bind (path line on-disk)
        (code-find-definition symbol :package package)
      (result id (build-code-find-response symbol path line on-disk)))))

(define-tool "code-describe"
  :description "Describe a symbol: type, arglist, and documentation.

PREREQUISITE: The defining system MUST be loaded first (load-system tool).
Pass a package or a package-qualified symbol to avoid resolution errors.

NOTE: If the symbol is not found, the system might not be loaded yet.
For code exploration WITHOUT loading systems, use 'clgrep-search' instead.
Fallback: Use 'lisp-read-file' with 'name_pattern' to search the file system."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl:mapcar\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified; ensure the package exists
and is loaded"))
  :body
  (with-proxy-dispatch (id "worker/code-describe"
                          (make-ht "symbol" symbol "package" package))
    (multiple-value-bind (name type arglist doc path line)
        (code-describe-symbol symbol :package package)
      (result id (build-code-describe-response
                  name type arglist doc path line
                  :method-count (generic-function-method-count symbol :package package))))))

(defun %json-nulls->nil (value)
  "Return VALUE, a worker's answer parsed with its JSON types kept, with every
:NULL inside it turned into NIL -- the value an inline report holds there, and
what the report's text builder reads as absent.  Hash-tables and vectors are
changed in place.  Falses stay YASON:FALSE, which encodes as the false the
worker wrote."
  (typecase value
    (hash-table
     (maphash (lambda (key child)
                (setf (gethash key value) (%json-nulls->nil child)))
              value)
     value)
    (string value)
    (vector
     (dotimes (i (length value) value)
       (setf (aref value i) (%json-nulls->nil (aref value i)))))
    (t (if (eq value :null) nil value))))

(define-tool "code-find-references"
  :description "Find who calls or references a symbol - its callers, the exact call sites
inside them, and the tests that exercise it - to see what a change would affect
before making it (who-calls / who-references / impact analysis).

Combines SBCL xref (calls, macroexpands, binds, references, sets) with a scan of
the project's source, so it also reports:
- the exact line of every call site inside each caller ('call_sites')
- top-level uses xref never records, such as a defparameter initform or a macro
  used at top level (origin 'source')
- calls that exist only inside a macro expansion (origin 'xref'), still named
  by the form they sit in, and counted as tests when that form is one
- the deftest a reference sits in, and a 'Tests:' line listing them
Each result is one top-level form; its form_type and form_name can be passed
straight to lisp-edit-form.  lisp-read-file's name_pattern is a regex, so
regex-quote form_name there: a defmethod name such as 'area ((s integer))'
does not match itself.

PREREQUISITE: load the defining system first (load-system).  A symbol or package
that does not exist is reported as such, and nothing is interned.  'pkg:name'
also finds internal symbols.

LIMITS: matching inside a form is positional, not a code walker.  A flet, labels
or macrolet binding the same name is flagged as shadowing; other lexical bindings
are not.  Matches in files whose package is not loaded are listed as possible
matches instead of being resolved.

For plain text search without loading anything, use 'clgrep-search'."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl-mcp:run\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified")
         (project-only :type :boolean :json-name "project_only" :default t
                       :description
                       "When true (default), only include references under the project root")
         (limit :type :integer
                :description
                "Maximum number of forms listed (default 50); the total is always reported"))
  :body
  (progn
    ;; Checked here, before the scan and before any worker call, so a bad value
    ;; gets the same argument error with and without the worker pool.
    (when (and limit (not (and (integerp limit) (plusp limit))))
      (error 'arg-validation-error
             :arg-name "limit"
             :message "limit must be a positive integer"))
    ;; The scan runs in this (parent) process on both paths: it needs eclector,
    ;; which the worker image does not load.  It also validates SYMBOL.  So
    ;; does placing the references the scan could not see (a call made by a
    ;; macro expansion in a form that never names the symbol): the report
    ;; comes back with every reference, is placed here, and only then cut
    ;; to LIMIT, so tests covers all of them.
    (let* ((scan (scan-project symbol))
           (report (if *use-worker-pool*
                       (proxy-to-worker id "worker/code-find-references"
                                        (make-ht "symbol" symbol
                                                 "package" package
                                                 "project_only" project-only
                                                 "limit" nil
                                                 "scan" scan)
                                        :preserve-json-types t)
                       (code-find-references-report symbol
                                                    :package package
                                                    :project-only project-only
                                                    :limit nil
                                                    :scan scan))))
      (result id
              (if (member (gethash "isError" report) '(t yason:true))
                  ;; A proxy failure or a reset notice: relayed untouched.
                  report
                  (build-code-find-references-response
                   (place-references-in-source (%json-nulls->nil report)
                                               #'top-level-forms-at
                                               :limit (or limit 50))))))))
