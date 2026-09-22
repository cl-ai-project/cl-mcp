;;;; tests/spec-api-resolution-test.lisp
;;;;
;;;; RESOLVE-CL-SPEC-API, against a CL-SPEC package built for each case.
;;;;
;;;; RUN THIS ONLY IN A PROCESS OF ITS OWN.  The resolver reads the real
;;;; CL-SPEC package, so the only way to check what it does with a cl-spec
;;;; that is missing a name is to be the one who made that package.  This file
;;;; therefore refuses to run at all when a CL-SPEC package already exists: it
;;;; will not rename or delete someone else's cl-spec, and a process that has
;;;; one is not a process where these answers mean anything.
;;;; scripts/check-specs.lisp runs it as CL_MCP_SPECS_MODE=integration, in a
;;;; process that loads no cl-spec.
;;;;
;;;; It needs cl-mcp's adapter core and Rove, and nothing else.  Each case
;;;; makes the package, resolves, and deletes it again; the names it interns
;;;; are its own, in its own package, and are gone with it.

(defpackage #:cl-mcp/tests/spec-api-resolution-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-cl-spec-api
                #:api-has-p
                #:api-fn
                #:api-special
                #:api-class
                #:api-backend-available-p
                #:cl-spec-api-missing))

(in-package #:cl-mcp/tests/spec-api-resolution-test)

;;; ------------------------------------------------------------------------
;;; The manifest, restated

(defparameter +required-functions+
  '("SEMANTIC-DATA" "SPEC-DATA" "PROPERTY-DATA" "PROPERTIES-FOR" "RUN-PROPERTY"
    "BACKEND-DEFAULT-TRIALS" "PROPERTY-RESULT-STATUS" "PROPERTY-RESULT-TRIALS"
    "PROPERTY-RESULT-SEED" "PROPERTY-RESULT-PROFILE" "PROPERTY-RESULT-COUNTEREXAMPLE"
    "PROPERTY-RESULT-SHRUNK-COUNTEREXAMPLE" "PROPERTY-RESULT-CONDITION"
    "PROPERTY-RESULT-ELAPSED")
  "The functions a cl-spec must export before the adapter calls itself usable:
what every path reads.  Written here so that dropping one from the adapter's
own table is a failure rather than a quiet agreement.")

(defparameter +required-specials+ '("*REGISTRY*" "*GENERATOR-BACKEND*")
  "The specials the adapter reads as zero-argument handles.")

(defparameter +optional-functions+ '("RESULT-DATA" "LIST-SPECS" "LIST-PROPERTIES"
                                     "LIST-FUNCTION-SPECS" "PROPERTIES-WITH-TAG"
                                     "FUNCTION-SPEC-DATA" "CHECK-FUNCTION")
  "Functions whose absence costs one operation, never the whole adapter.")

(defparameter +condition-names+ '("UNKNOWN-SPEC" "UNKNOWN-PROPERTY" "CL-SPEC-ERROR")
  "Condition classes whose absence costs a coarser classification only.")

(defvar *calls* '()
  "Every synthetic cl-spec function this process called, most recent first.")

;;; ------------------------------------------------------------------------
;;; Building one

(defun %make-cl-spec (&key (functions +required-functions+)
                        (specials +required-specials+)
                        unbound-specials nil-specials optional classes
                        (unfbound '()))
  "Make a CL-SPEC package holding the names given, and return it.

FUNCTIONS and OPTIONAL are fbound; UNFBOUND names are interned and left
without a definition.  SPECIALS are bound to a value of their own, those in
NIL-SPECIALS to NIL, and those in UNBOUND-SPECIALS are interned and left
unbound.  CLASSES become condition classes."
  (when (find-package "CL-SPEC")
    (error "A CL-SPEC package already exists; this suite must run in a process ~
of its own."))
  (let ((package (make-package "CL-SPEC" :use '())))
    (dolist (name (append functions optional))
      (let ((symbol (intern name package)))
        (export symbol package)
        (setf (fdefinition symbol)
              (let ((key name))
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (push key *calls*)
                  nil)))))
    (dolist (name unfbound)
      (export (intern name package) package))
    (dolist (name specials)
      (let ((symbol (intern name package)))
        (export symbol package)
        (setf (symbol-value symbol)
              (cond ((member name nil-specials :test #'string=) nil)
                    ((string= name "*GENERATOR-BACKEND*") (list :synthetic-backend))
                    (t (list :synthetic-registry))))))
    (dolist (name unbound-specials)
      (export (intern name package) package))
    (dolist (name classes)
      (eval `(define-condition ,(intern name package) (error) ())))
    package))

(defun %destroy-cl-spec ()
  "Delete the CL-SPEC package this suite made, with the classes it defined."
  (let ((package (find-package "CL-SPEC")))
    (when package
      (do-symbols (symbol package)
        (when (find-class symbol nil) (setf (find-class symbol) nil))
        (when (fboundp symbol) (fmakunbound symbol)))
      (delete-package package))))

(defmacro with-cl-spec ((&rest arguments) &body body)
  "Make a CL-SPEC package for ARGUMENTS, run BODY, and delete it again."
  `(progn
     (%make-cl-spec ,@arguments)
     (unwind-protect (progn ,@body)
       (%destroy-cl-spec))))

(defun %resolve ()
  "Return (values API STATUS MISSING) for the package as it stands."
  (multiple-value-bind (api status) (resolve-cl-spec-api)
    (values api status (and api (cl-spec-api-missing api)))))

;;; ------------------------------------------------------------------------
;;; Tests

(deftest this-process-has-no-cl-spec-of-its-own
  ;; The guard the rest of the file depends on.  A process holding a real
  ;; cl-spec must fail here rather than have it taken apart.
  (ok (null (find-package "CL-SPEC"))
      "no CL-SPEC package exists before this suite builds one"))

(deftest no-package-at-all-is-not-loaded
  (multiple-value-bind (api status) (%resolve)
    (ok (null api))
    (ok (eq :not-loaded status))))

(deftest a-missing-required-name-is-incomplete-and-named
  (testing "a function that is not there"
    (with-cl-spec (:functions (remove "SPEC-DATA" +required-functions+ :test #'string=))
      (multiple-value-bind (api status missing) (%resolve)
        (ok api "an API is still returned, so the caller can report the version")
        (ok (eq :incomplete status))
        (ok (equal '("SPEC-DATA") missing) "only the one that is missing"))))
  (testing "a name that is there but not fbound"
    (with-cl-spec (:functions (remove "RUN-PROPERTY" +required-functions+ :test #'string=)
                   :unfbound '("RUN-PROPERTY"))
      (multiple-value-bind (api status missing) (%resolve)
        (declare (ignore api))
        (ok (eq :incomplete status))
        (ok (equal '("RUN-PROPERTY") missing)))))
  (testing "a special that is not bound"
    (with-cl-spec (:specials '("*REGISTRY*") :unbound-specials '("*GENERATOR-BACKEND*"))
      (multiple-value-bind (api status missing) (%resolve)
        (declare (ignore api))
        (ok (eq :incomplete status))
        (ok (equal '("*GENERATOR-BACKEND*") missing))))))

(deftest a-special-bound-to-nil-is-not-an-unbound-one
  (with-cl-spec (:nil-specials '("*GENERATOR-BACKEND*"))
    (multiple-value-bind (api status missing) (%resolve)
      (ok (eq :ok status) "bound to NIL is bound")
      (ok (null missing))
      (ok (not (api-backend-available-p api))
          "and the backend is reported as absent, which is a different fact"))))

(deftest optional-names-cost-their-own-operation-only
  (with-cl-spec ()
    (multiple-value-bind (api status missing) (%resolve)
      (ok (eq :ok status) "no optional function, and the adapter is still usable")
      (ok (null missing))
      (dolist (key '(:list-specs :list-properties :function-spec-data :check-function))
        (ok (not (api-has-p api key)) (format nil "~(~A~) is absent" key)))
      (ok (null (api-class api :unknown-spec)) "and so is the condition class")))
  (testing "and they are picked up when they are there"
    (with-cl-spec (:optional +optional-functions+ :classes +condition-names+)
      (multiple-value-bind (api status) (%resolve)
        (ok (eq :ok status))
        (dolist (key '(:list-specs :list-properties :function-spec-data :check-function))
          (ok (api-has-p api key)))
        (ok (api-class api :unknown-spec))))))

(deftest a-complete-cl-spec-resolves-to-its-own-definitions
  (with-cl-spec ()
    (multiple-value-bind (api status) (%resolve)
      (ok (eq :ok status))
      (testing "each handle is that package's own function"
        (ok (eq (fdefinition (find-symbol "SEMANTIC-DATA" "CL-SPEC"))
                (api-fn api :semantic-data)))
        (ok (eq (fdefinition (find-symbol "RUN-PROPERTY" "CL-SPEC"))
                (api-fn api :run-property))))
      (testing "each special is named, and read through"
        (ok (eq (find-symbol "*REGISTRY*" "CL-SPEC") (api-special api :registry)))
        (ok (eq (symbol-value (find-symbol "*REGISTRY*" "CL-SPEC"))
                (funcall (api-fn api :registry)))))
      (testing "resolving called none of them"
        (ok (null *calls*))))))

(deftest resolving-again-sees-the-bindings-as-they-are
  (with-cl-spec ()
    (let ((registry (find-symbol "*REGISTRY*" "CL-SPEC"))
          (replacement (list :another-registry)))
      (multiple-value-bind (api) (%resolve)
        (ok (not (eq replacement (funcall (api-fn api :registry)))))
        (setf (symbol-value registry) replacement)
        (multiple-value-bind (again status) (%resolve)
          (ok (eq :ok status))
          (ok (eq replacement (funcall (api-fn again :registry)))
              "the new resolution reads the binding as it now is"))))))

(deftest resolving-interns-nothing-it-did-not-find
  (with-cl-spec ()
    (%resolve)
    (dolist (name (append +optional-functions+ +condition-names+))
      (ok (null (find-symbol name "CL-SPEC"))
          (format nil "~A was not interned by looking for it" name)))))

(deftest the-package-is-gone-again
  ;; The suite cleans up after itself: whatever ran above, no CL-SPEC package
  ;; is left in this image.
  (ok (null (find-package "CL-SPEC"))))
