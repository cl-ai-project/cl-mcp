;;;; tests/spec-inspection-specs-test.lisp
;;;;
;;;; Inspection against a real cl-spec.  Opt-in: it needs cl-spec with the
;;;; check-it backend, and it is not in tests.lisp.  scripts/check-specs.lisp
;;;; runs it in a process of its own (CL_MCP_SPECS_MODE=integration), where a
;;;; missing dependency, a missing test, a failure or a skip fails the step.
;;;;
;;;; The stub of specs/spec-inspection-fixtures.lisp is a model of what
;;;; cl-spec answers; this file checks the model against cl-spec itself.  Its
;;;; declarations are its own, in registries of its own, and each check binds
;;;; CL-SPEC:*REGISTRY* around the call.
;;;;
;;;; Reading a declaration must run none of it.  Every form a contract can
;;;; carry -- the target, the argument generator, :PRE, :CAPTURE, a case guard,
;;;; :POST and :STATE-POST -- increments a counter of its own here, and the
;;;; reads below compare the counters with a snapshot taken after
;;;; registration.  That is a statement about contract and application forms,
;;;; not about anything else an implementation may do while printing.

(defpackage #:cl-mcp/tests/spec-inspection-specs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-spec/main
                #:*registry*
                #:make-hash-table-registry
                #:*generator-backend*
                #:defspec
                #:defproperty
                #:defgenerator
                #:defspec-function)
  ;; A bare :import-from declares the check-it backend as a dependency.
  (:import-from #:cl-spec/src/backends/check-it)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-cl-spec-api
                #:api-backend-available-p)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:list-report
                #:symbol-report
                #:describe-report)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-list-response
                #:spec-symbol-response
                #:spec-describe-response))

(in-package #:cl-mcp/tests/spec-inspection-specs-test)

;;; ------------------------------------------------------------------------
;;; Counters: what a read must never run

(defparameter +counters+ '(:target :generator :pre :capture :guard :post :state-post)
  "Every place a contract can hold a form of the author's.")

(defvar *counts* (make-hash-table :test #'eq)
  "How often each of +COUNTERS+ has run.")

(defun note (counter)
  "Record that COUNTER's form ran, and return true so a clause holds."
  (incf (gethash counter *counts* 0))
  t)

(defun snapshot ()
  "Return the counters as they stand."
  (loop for counter in +counters+ collect (cons counter (gethash counter *counts* 0))))

;;; ------------------------------------------------------------------------
;;; The declarations

(defun inspection-real-f (x)
  "Return twice X, and record that the target ran."
  (note :target)
  (* 2 x))

(defvar *scripted* nil
  "Argument lists the generator hands out, so no draw is needed.")

(defun %register (registry &key (about t) (wording :original))
  "Register this file's declarations in REGISTRY: a spec, a property of the
subject's own name, a property (:about ...) it, a plain contract and one with
named cases and state.

WORDING picks which of three contracts is declared for the subject.  They are
written out rather than built from a string: a docstring is part of the
declaration, and this file evaluates nothing at run time to vary one."
  (let ((*registry* registry))
    (defspec inspection-small (range integer 0 100))
    (defgenerator inspection-arguments ()
      "Hand out a scripted argument list, and record that it ran."
      (note :generator)
      (or (pop *scripted*) (list 1)))
    (defproperty inspection-real-f ((x inspection-small))
      "Shares the subject's name and is about nothing."
      (:trials (:smoke 2 :normal 4))
      (evenp (inspection-real-f x)))
    (when about
      (defproperty inspection-about-f ((x inspection-small))
        "Twice anything is even."
        (:about inspection-real-f)
        (:trials (:smoke 2 :normal 4))
        (evenp (inspection-real-f x))))
    (ecase wording
      (:original
       (defspec-function inspection-real-f
         "Twice its argument."
         (:args (x inspection-small))
         (:pre (note :pre))
         (:returns (range integer 0 *))
         (:post (note :post))))
      (:other
       (defspec-function inspection-real-f
         "The other registry's wording."
         (:args (x inspection-small))
         (:pre (note :pre))
         (:returns (range integer 0 *))
         (:post (note :post))))
      (:again
       (defspec-function inspection-real-f
         "Declared again."
         (:args (x inspection-small))
         (:pre (note :pre))
         (:returns (range integer 0 *))
         (:post (note :post)))))
    (defspec-function inspection-cased-f
      "Two named cases, a capture and a state postcondition."
      (:args (x inspection-small))
      (:args-generator inspection-arguments)
      (:capture (before (note :capture)))
      (:cases
        (:small
          "The small case."
          (:when (note :guard))
          (:returns (type integer))
          (:state-post (note :state-post)))
        (:large
          (:when (not (note :guard)))
          (:signals (type error)))))
    registry))

(defun inspection-cased-f (x)
  "Return X, and record that the target ran."
  (note :target)
  x)

(defun %registry (&rest arguments)
  "Return a fresh registry holding this file's declarations."
  (apply #'%register (make-hash-table-registry) arguments))

(defun %api ()
  "Return cl-spec's own API, as spec-check resolves it."
  (multiple-value-bind (api status) (resolve-cl-spec-api)
    (unless (eq status :ok) (error "cl-spec is not usable here: ~S" status))
    api))

(defun %name (name)
  "Return NAME's package-qualified designator."
  (format nil "CL-MCP/TESTS/SPEC-INSPECTION-SPECS-TEST::~A" name))

(defun %params (&rest pairs)
  "Return a tool-arguments hash-table holding PAIRS."
  (let ((params (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key params) value))
    params))

(defmacro with-installed-registry ((registry) &body body)
  "Install REGISTRY as CL-SPEC:*REGISTRY*'s global value for BODY, and put the
previous one back however BODY exits.

The entry functions read the registry on a deadline thread of their own, which
does not see a dynamic binding made here -- so a LET would leave them reading
whatever registry the image holds.  Swapping the global value is what the
existing real-cl-spec suite does too, and it is why both run in a process of
their own rather than in an MCP worker someone is using."
  (let ((previous (gensym "PREVIOUS")))
    `(let ((,previous *registry*))
       (setf *registry* ,registry)
       (unwind-protect (progn ,@body)
         (setf *registry* ,previous)))))

;;; ------------------------------------------------------------------------
;;; Tests

(deftest real-registrations-of-one-name-stay-apart
  (let ((registry (%registry)))
    (let ((*registry* registry))
      (let* ((report (symbol-report (%api) :ok (%name "INSPECTION-REAL-F")
                                    :include-runtime nil))
             (registered (getf report :registry)))
        (ok (eq :ok (getf report :status)))
        (ok (equal "INSPECTION-REAL-F" (getf (getf registered :property) :name))
            "the property of that name")
        (ok (equal "INSPECTION-REAL-F" (getf (getf registered :function-spec) :name))
            "the contract of that name")
        (ok (equal '("INSPECTION-ABOUT-F")
                   (mapcar (lambda (plist) (getf plist :name))
                           (getf registered :properties-about)))
            "and only the property declared about it")
        (ok (not (getf report :nothing-registered))))
      (testing "a name registered as a spec only is not a property"
        (let ((registered (getf (symbol-report (%api) :ok (%name "INSPECTION-SMALL")
                                               :include-runtime nil)
                                :registry)))
          (ok (getf registered :spec))
          (ok (null (getf registered :property)))
          (ok (null (getf registered :function-spec))))))))

(deftest real-introspection-works-without-a-backend
  (let ((registry (%registry)))
    (let ((*registry* registry)
          (*generator-backend* nil))
      (ok (not (api-backend-available-p (%api))) "no backend is installed")
      (let ((listing (list-report (%api) :ok :kind "both")))
        (ok (eq :ok (getf listing :status)) "and the registry still reads")
        (ok (plusp (getf (getf listing :counts) :specs)))
        (ok (plusp (getf (getf listing :counts) :properties)))
        (ok (plusp (getf (getf listing :counts) :function-specs))))
      (let ((described (describe-report (%api) :ok "function-spec"
                                        (%name "INSPECTION-REAL-F"))))
        (ok (eq :ok (getf described :status)))))))

(deftest real-declarations-arrive-as-cl-spec-projects-them
  (let ((registry (%registry)))
    (let ((*registry* registry))
      (testing "a plain contract: its argument, clauses and returns"
        (let* ((described (describe-report (%api) :ok "function-spec"
                                           (%name "INSPECTION-REAL-F")))
               (argument (first (getf described :arguments))))
          (ok (eq :ok (getf described :status)))
          (ok (equal "Twice its argument." (getf described :documentation)))
          (ok (eql 1 (length (getf described :arguments))))
          (ok (equal "X" (getf (getf argument :variable) :name)))
          (ok (eq :required (getf argument :kind)) "required, which v1 omits")
          (ok (search "NOTE" (getf described :preconditions)))
          (ok (eq t (getf described :preconditions-complete)))
          (ok (search "NOTE" (getf described :postconditions)))
          (ok (getf described :returns))
          (ok (equal "*" (getf (getf described :returns) :max))
              "an open range end is *")))
      (testing "a contract with cases: their order, guards and outcomes"
        (let* ((described (describe-report (%api) :ok "function-spec"
                                           (%name "INSPECTION-CASED-F")))
               (cases (getf described :cases)))
          (ok (eq :ok (getf described :status)))
          (ok (eq :exclusive (getf described :case-selection)))
          (ok (equal '(:small :large) (mapcar (lambda (case) (getf case :name)) cases)))
          (ok (eq :returns (getf (first cases) :outcome)))
          (ok (eq :signals (getf (second cases) :outcome)))
          (ok (search "NOTE" (getf (first cases) :guard)))
          (ok (search "NOTE" (getf (first cases) :state-post)))
          (ok (equal "BEFORE" (getf (getf (first (getf described :capture)) :name) :name)))
          (ok (equal "INSPECTION-ARGUMENTS"
                     (getf (getf described :argument-generator) :name))))))))

(deftest real-reads-run-no-form-of-the-declaration
  (let ((registry (%registry)))
    (let ((*registry* registry))
      (let ((before (snapshot)))
        (list-report (%api) :ok :kind "both")
        (symbol-report (%api) :ok (%name "INSPECTION-REAL-F") :include-runtime nil)
        (describe-report (%api) :ok "function-spec" (%name "INSPECTION-REAL-F"))
        (describe-report (%api) :ok "function-spec" (%name "INSPECTION-CASED-F"))
        (describe-report (%api) :ok "property" (%name "INSPECTION-ABOUT-F"))
        (describe-report (%api) :ok "spec" (%name "INSPECTION-SMALL"))
        (spec-describe-response (%params "kind" "function-spec"
                                         "name" (%name "INSPECTION-CASED-F")))
        (ok (equal before (snapshot))
            "no target, generator, :pre, capture, guard, :post or :state-post ran")))))

(deftest real-redefinition-reaches-describe-and-leaves-another-registry-alone
  (let ((first-registry (%registry))
        (second-registry (%registry :wording :other)))
    (flet ((documentation-of (registry)
             (let ((*registry* registry))
               (getf (describe-report (%api) :ok "function-spec"
                                      (%name "INSPECTION-REAL-F"))
                     :documentation))))
      (ok (equal "Twice its argument." (documentation-of first-registry)))
      (ok (equal "The other registry's wording." (documentation-of second-registry)))
      (testing "declaring it again is what the description shows"
        (%register first-registry :wording :again)
        (ok (equal "Declared again." (documentation-of first-registry)))
        (ok (equal "The other registry's wording." (documentation-of second-registry))
            "and the other registry is untouched")))))

(deftest real-entries-keep-the-kind-name-and-bounds-they-were-given
  (let ((registry (%registry)))
    (with-installed-registry (registry)
      (testing "a listing keeps its kind and limit"
        (let ((response (spec-list-response (%params "kind" "properties" "limit" 1))))
          (ok (equal "ok" (gethash "status" response)))
          (ok (equal "properties" (gethash "kind" response)))
          (ok (eql 1 (length (gethash "properties" response))))
          (ok (eql 2 (gethash "properties" (gethash "counts" response)))
              "counted before the limit")
          ;; True is carried as T here and encoded as true; only a false is
          ;; spelled YASON:FALSE at this layer.
          (ok (not (eq yason:false (gethash "truncated" response))))
          (ok (gethash "truncated" response))))
      (testing "a symbol report keeps the registrations apart"
        (let ((response (spec-symbol-response (%params "symbol" (%name "INSPECTION-REAL-F")
                                                       "include_runtime" yason:false))))
          (ok (equal "ok" (gethash "status" response)))
          (ok (gethash "function_spec" (gethash "registry" response)))
          (ok (gethash "property" (gethash "registry" response)))
          (ok (eql 1 (length (gethash "properties_about" (gethash "registry" response)))))))
      (testing "a description keeps its name and its character budget"
        (let* ((response (spec-describe-response
                          (%params "kind" "function-spec"
                                   "name" (%name "INSPECTION-CASED-F")
                                   "max_chars" 12)))
               (cases (gethash "cases" response)))
          (ok (equal "ok" (gethash "status" response)))
          (ok (equal "INSPECTION-CASED-F" (gethash "name" (gethash "name" response))))
          (ok (eql 2 (length cases)))
          (ok (eq yason:false (gethash "guard_complete" (aref cases 0)))
              "the budget cut the guard, and it says so")
          (ok (plusp (gethash "guard_omitted_chars" (aref cases 0))))))
      (testing "a name that is not registered is not an error"
        (let ((response (spec-describe-response
                         (%params "kind" "property" "name" (%name "INSPECTION-SMALL")))))
          (ok (equal "not-registered" (gethash "status" response))))))))
