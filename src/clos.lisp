;;;; src/clos.lisp
;;;;
;;;; The clos-describe tool: the worker reads a class or generic function from
;;;; its image (clos-core), and the parent annotates that report from the source
;;;; files and renders its text (clos-response-builders).

(defpackage #:cl-mcp/src/clos
  (:use #:cl)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/tools/clos-response-builders
                #:build-clos-describe-response)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:proxy-to-worker)
  (:export #:clos-describe-report))

(in-package #:cl-mcp/src/clos)

(define-tool "clos-describe"
  :description "Describe a CLOS class or generic function from the running image -
the structure a source search cannot see:
- a generic function: its methods with their qualifiers (:around, :before,
  :after), specializers (classes, (eql ...)), method combination and source line
- a class: superclasses, subclasses, precedence list, direct and effective slots
  (initargs, initform as code, type, accessors, and the class each comes from),
  default initargs, and the methods specialized on it or its superclasses
A symbol naming a class and a generic function, or a SETF generic function,
gets every section.  Each definition's form_type and form_name can be passed
straight to lisp-edit-form.

Reads only: a class is never finalized, no initform is evaluated, and nothing
is interned.

PREREQUISITE: load the defining system first (load-system).

Use inspect-object for one instance's slot values, code-describe for a plain
function, macro or variable, and code-find-references for who calls a generic
function."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"my-pkg:shape\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified")
         (limit :type :integer
                :description
                "Maximum methods listed per generic function and per class (default 50);
the total is always reported"))
  :body
  (progn
    ;; Checked before any worker call, so a bad value gets the same argument
    ;; error with and without the worker pool.
    (when (and limit (not (and (integerp limit) (plusp limit))))
      (error 'arg-validation-error
             :arg-name "limit"
             :message "limit must be a positive integer"))
    (let ((limit (or limit 50)))
      ;; Not WITH-PROXY-DISPATCH: the worker returns only the report, and both
      ;; paths finish it here, in the process that can parse source files.
      (result id
              (build-clos-describe-response
               (if *use-worker-pool*
                   (proxy-to-worker id "worker/clos-describe"
                                    (make-ht "symbol" symbol
                                             "package" package
                                             "limit" limit))
                   (clos-describe-report symbol :package package :limit limit)))))))
