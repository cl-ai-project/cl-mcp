;;;; src/code.lisp

(defpackage #:cl-mcp/src/code
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-core
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export
   #:code-find-definition
   #:code-describe-symbol
   #:code-find-references))

(in-package #:cl-mcp/src/code)

(define-tool "code-find"
  :description "Locate the definition of a symbol (path and line) using sb-introspect.

PREREQUISITE: The defining system MUST be loaded first (ql:quickload).
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
  (multiple-value-bind (path line)
      (code-find-definition symbol :package package)
    (if path
        (result id
                (make-ht "path" path
                         "line" line
                         "content" (text-content
                                    (format nil "~A defined in ~A at line ~D"
                                            symbol path line))))
        (rpc-error id -32004
                   (format nil "Definition not found for ~A" symbol)))))

(define-tool "code-describe"
  :description "Describe a symbol: type, arglist, and documentation.

PREREQUISITE: The defining system MUST be loaded first (ql:quickload).
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
  (multiple-value-bind (name type arglist doc path line)
      (code-describe-symbol symbol :package package)
    (result id
            (make-ht "name" name
                     "type" type
                     "arglist" arglist
                     "documentation" doc
                     "path" path
                     "line" line
                     "content" (text-content
                                (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]~@[~%Defined at ~A:~D~]"
                                        name type arglist doc path line))))))

(define-tool "code-find-references"
  :description "Find where a symbol is referenced using SBCL xref (calls, macroexpands, binds,
references, sets).

PREREQUISITE: The defining system MUST be loaded first (ql:quickload).
Use package-qualified symbols when possible; set project_only=false to include
external libs.

For simple text-based usage search WITHOUT loading systems, use 'clgrep-search' instead."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl-mcp:run\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified")
         (project-only :type :boolean :json-name "project_only" :default t
                       :description "When true (default), only include references under the project root"))
  :body
  (multiple-value-bind (refs count)
      (code-find-references symbol :package package :project-only project-only)
    (let* ((summary-lines
            (map 'list
                 (lambda (h)
                   (format nil "~A:~D ~A ~A"
                           (gethash "path" h)
                           (gethash "line" h)
                           (gethash "type" h)
                           (gethash "context" h)))
                 refs))
           (summary (if summary-lines
                        (format nil "~{~A~%~}" summary-lines)
                        "")))
      (result id
              (make-ht "refs" refs
                       "count" count
                       "symbol" symbol
                       "project_only" project-only
                       "content" (text-content summary))))))
