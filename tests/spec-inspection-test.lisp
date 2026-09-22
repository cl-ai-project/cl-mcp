;;;; tests/spec-inspection-test.lisp
;;;;
;;;; Fixed cases for what cl-mcp reports about cl-spec before anything runs:
;;;; which operations are available, what is registered, what a declaration
;;;; says, and which digest stands for it.  They sit beside the generated
;;;; properties of specs/spec-inspection.lisp, need no cl-spec, and run in the
;;;; default suite.
;;;;
;;;; Every case goes through the report layer with the recording stub of
;;;; specs/spec-inspection-fixtures.lisp, which answers from a described
;;;; registry and records each reader call.  The entry functions resolve the
;;;; cl-spec in the image rather than taking an API, so they are exercised
;;;; against a real cl-spec in the opt-in tests/spec-inspection-specs-test.lisp
;;;; instead of here.

(defpackage #:cl-mcp/tests/spec-inspection-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:api-backend-available-p
                #:definition-digest)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:contract-operation-missing
                #:list-report
                #:symbol-report
                #:describe-report
                #:%describe-function-spec)
  (:import-from #:cl-mcp/specs/spec-inspection-fixtures
                #:+required-handles+
                #:+listing-handles+
                #:+home-package-name+
                #:+other-package-name+
                #:definition-symbol
                #:definition-name
                #:inspection-api
                #:api-calls
                #:calls-of
                #:calls-carry-registry-p
                #:contract-descriptor
                #:contract-record
                #:clause-reads-back-p
                #:clause-forms
                #:long-form))

(in-package #:cl-mcp/tests/spec-inspection-test)

(defparameter +all-handles+ (append +required-handles+ +listing-handles+
                                    (list :check-function))
  "Every handle a usable cl-spec with a full listing API and a checker has.")

(defun %registry (&rest keys)
  "Return a registry descriptor holding KEYS."
  (list :definitions keys))

(defun %designator (key)
  "Return the package-qualified designator of definition KEY."
  (destructuring-bind (package name) (definition-name key)
    (format nil "~A::~A" package name)))

(defun %describe (record &key (max-chars 8000))
  "Return the description of the contract RECORD."
  (%describe-function-spec (inspection-api :handles +all-handles+
                                           :registry (%registry :contract-a :spec-a)
                                           :contract-record record)
                           (definition-symbol :contract-a) :inspection-registry max-chars))

;;; ------------------------------------------------------------------------
;;; A. Availability

(deftest an-operation-needs-only-its-own-handles
  (loop for (operation needed) in '((:describe (:function-spec-data))
                                    (:run (:function-spec-data :check-function)))
        do (testing (format nil "~(~A~) a contract" operation)
             (testing "with everything, nothing is missing"
               (ok (null (contract-operation-missing
                          (inspection-api :handles +all-handles+) operation))))
             (dolist (absent needed)
               (let* ((handles (remove absent +all-handles+))
                      (missing (contract-operation-missing
                                (inspection-api :handles handles) operation)))
                 (ok (equal (list absent) missing)
                     (format nil "only ~(~A~) is named" absent))))))
  (testing "a cl-spec that cannot run a contract can still describe one"
    (let ((api (inspection-api :handles (remove :check-function +all-handles+))))
      (ok (null (contract-operation-missing api :describe)))
      (ok (equal '(:check-function) (contract-operation-missing api :run)))))
  (testing "and one that cannot project a contract can do neither"
    (let ((api (inspection-api :handles (remove :function-spec-data +all-handles+))))
      (ok (equal '(:function-spec-data) (contract-operation-missing api :describe)))
      (ok (equal '(:function-spec-data) (contract-operation-missing api :run))))))

(deftest a-backend-is-available-only-as-an-object
  (ok (api-backend-available-p (inspection-api :backend :object)) "an object")
  (ok (not (api-backend-available-p (inspection-api :backend :none)))
      "the special bound to NIL")
  (ok (not (api-backend-available-p (inspection-api :backend :signals)))
      "a reader that signals")
  (ok (not (api-backend-available-p
            (inspection-api :handles (remove :generator-backend +required-handles+))))
      "no reader at all")
  (testing "and reading the registry needs none of it"
    (let ((report (list-report (inspection-api :backend :none :handles +all-handles+
                                               :registry (%registry :spec-a))
                               :ok :kind "specs")))
      (ok (eq :ok (getf report :status)))
      (ok (eql 1 (getf (getf report :counts) :specs))))))

;;; ------------------------------------------------------------------------
;;; B. Listings

(deftest a-count-of-none-is-not-the-same-as-not-looking
  (let ((registry (%registry :spec-a :property-a :contract-a)))
    (testing "a kind that was not asked for has no count"
      (let ((counts (getf (list-report (inspection-api :handles +all-handles+
                                                       :registry registry)
                                       :ok :kind "specs")
                          :counts)))
        (ok (eql 1 (getf counts :specs)))
        (ok (null (getf counts :properties)))
        (ok (null (getf counts :function-specs)))))
    (testing "a kind that is asked for and holds none counts 0"
      (let ((counts (getf (list-report (inspection-api :handles +all-handles+
                                                       :registry (%registry :spec-a))
                                       :ok :kind "both")
                          :counts)))
        (ok (eql 1 (getf counts :specs)))
        (ok (eql 0 (getf counts :properties)))
        (ok (eql 0 (getf counts :function-specs)))))
    (testing "a kind that cannot be listed has no count, and says so"
      (let ((report (list-report (inspection-api
                                  :handles (remove :list-properties +all-handles+)
                                  :registry registry)
                                 :ok :kind "both")))
        (ok (eq :ok (getf report :status)) "the halves it can list are still listed")
        (ok (eql 1 (getf (getf report :counts) :specs)))
        (ok (null (getf (getf report :counts) :properties)))
        (ok (not (getf report :properties-listable)))
        (ok (getf report :specs-listable))))
    (testing "nothing the request asked for can be listed: unsupported"
      (let ((report (list-report (inspection-api
                                  :handles (remove :list-properties +all-handles+)
                                  :registry registry)
                                 :ok :kind "properties")))
        (ok (eq :unsupported (getf report :status)))
        (ok (search "list-properties" (getf report :message)))))))

(deftest listable-flags-describe-the-revision-not-the-request
  (let ((report (list-report (inspection-api :handles +all-handles+
                                             :registry (%registry :spec-a :property-a
                                                                  :contract-a))
                             :ok :kind "function-specs")))
    (ok (getf report :specs-listable) "asked for contracts, specs are still listable")
    (ok (getf report :properties-listable))
    (ok (getf report :function-specs-listable))
    (ok (null (getf (getf report :counts) :specs)) "but not counted")))

(deftest a-tag-has-three-answers-and-interns-nothing
  (let ((registry (%registry :property-a :property-b)))
    (flet ((filters (tag &optional (handles +all-handles+))
             (getf (list-report (inspection-api :handles handles :registry registry)
                                :ok :kind "properties" :tag tag)
                   :filters)))
      (testing "no tag asked for"
        (ok (eq :not-requested (getf (filters nil) :tag-resolved)))
        (ok (null (getf (filters nil) :tag-applied))))
      (testing "a tag this image knows"
        (ok (eq t (getf (filters "fast") :tag-resolved)))
        (ok (getf (filters "fast") :tag-applied)))
      (testing "a tag no loaded code mentions is unresolved, not empty"
        (let ((name "cl-mcp-inspection-no-such-tag-xyzzy"))
          (ok (null (find-symbol (string-upcase name) "KEYWORD")))
          (ok (null (getf (filters name) :tag-resolved)))
          (ok (null (find-symbol (string-upcase name) "KEYWORD"))
              "asking did not intern it")))
      (testing "a cl-spec without the tag reader cannot answer at all"
        (let* ((handles (remove :properties-with-tag +all-handles+))
               (report (list-report (inspection-api :handles handles :registry registry)
                                    :ok :kind "properties" :tag "fast")))
          (ok (not (getf report :tag-filterable)))
          (ok (null (getf (getf report :counts) :properties))
              "no count, rather than none carrying the tag"))))))

(deftest a-package-filter-reads-the-home-package-of-the-name
  (let ((registry (%registry :spec-a :spec-elsewhere :property-a :property-elsewhere)))
    (flet ((counts (package)
             (getf (list-report (inspection-api :handles +all-handles+ :registry registry)
                                :ok :kind "both" :package package)
                   :counts)))
      (ok (eql 2 (getf (counts nil) :specs)) "no filter: both")
      (ok (eql 1 (getf (counts +home-package-name+) :specs)))
      (ok (eql 1 (getf (counts +other-package-name+) :specs)))
      (ok (eql 1 (getf (counts +other-package-name+) :properties))))))

(deftest a-limit-cuts-the-list-and-never-the-count
  (let* ((registry (%registry :spec-a :spec-b :spec-elsewhere))
         (report (list-report (inspection-api :handles +all-handles+ :registry registry)
                              :ok :kind "specs" :limit 2)))
    (ok (eql 3 (getf (getf report :counts) :specs)) "counted before the cut")
    (ok (eql 2 (length (getf report :specs))))
    (ok (getf report :truncated))
    (ok (eql 2 (getf report :limit)))))

(deftest every-reader-is-handed-the-registry-the-listing-is-about
  (multiple-value-bind (api calls registry-object)
      (inspection-api :handles +all-handles+
                      :registry (%registry :spec-a :property-a :contract-a))
    (let ((report (list-report api :ok :kind "both" :limit 200)))
      (ok (eq :ok (getf report :status))))
    (testing "the enumerators and the row readers alike"
      (ok (calls-of calls :list-specs))
      (ok (calls-of calls :property-data) "a row is read, not only enumerated")
      (ok (calls-carry-registry-p calls registry-object))
      (ok (every (lambda (call) (eq registry-object (third call)))
                 (api-calls calls))
          "and this listing hands one to every reader it calls"))
    (testing "a call handed none fails, however right its answer looked"
      ;; The stub answers out of the descriptor it closes over either way, so
      ;; the rows of such a listing still look correct.  The recorded call is
      ;; the only place the omission shows.
      (ok (not (calls-carry-registry-p
                (list (list (list :property-data 'name nil)))
                registry-object)))
      (ok (not (calls-carry-registry-p
                (list (list (list :list-specs nil (list :some-other-registry))))
                registry-object))))
    (testing "a reader that takes no registry is not asked to carry one"
      (ok (calls-carry-registry-p
           (list (list (list :run-property 'name '(:trials 5))))
           registry-object)))))

;;; ------------------------------------------------------------------------
;;; C. Registration against read failure

(deftest a-reader-that-broke-is-not-an-absent-registration
  (let ((registry (%registry :property-a :contract-a :spec-a)))
    (flet ((status (kind subject failure)
             (getf (describe-report (inspection-api :handles +all-handles+
                                                    :registry registry
                                                    :reader-failures failure)
                                    :ok kind (%designator subject))
                   :status)))
      (ok (eq :ok (status "property" :property-a nil)))
      (ok (eq :not-registered (status "property" :property-a '(:property-data :unknown)))
          "cl-spec says the name is unknown")
      (ok (eq :internal-error (status "property" :property-a '(:property-data :fails)))
          "the reader broke: a fault, not an absence")
      (ok (eq :internal-error
              (status "property" :property-a '(:property-data :undefined-function)))
          "a property reader calling something undefined is a fault here")
      (ok (eq :undefined-function
              (status "function-spec" :contract-a '(:function-spec-data :undefined-function)))
          "a contract's target not being defined is its own answer")
      (ok (eq :not-registered (status "spec" :property-a nil))
          "a property asked for as a spec is not registered as one"))))

(deftest nothing-registered-is-said-only-about-a-lookup-that-worked
  (let ((registry (%registry :property-about :contract-a)))
    (testing "a symbol with a contract and properties about it"
      (let* ((report (symbol-report (inspection-api :handles +all-handles+
                                                    :registry registry)
                                    :ok (%designator :contract-a) :include-runtime nil))
             (registered (getf report :registry)))
        (ok (eq :ok (getf report :status)))
        (ok (getf registered :function-spec))
        (ok (null (getf registered :property)) "the name is not itself a property")
        (ok (eql 1 (length (getf registered :properties-about))))
        (ok (not (getf report :nothing-registered)))))
    (testing "a symbol the registry does not hold"
      (let ((report (symbol-report (inspection-api :handles +all-handles+
                                                   :registry (%registry))
                                   :ok (%designator :contract-a) :include-runtime nil)))
        (ok (eq :ok (getf report :status)))
        (ok (getf report :nothing-registered) "about this symbol, after a lookup")))
    (testing "a reverse index that could not be read says so, and claims nothing"
      (let ((report (symbol-report (inspection-api :handles +all-handles+
                                                   :registry registry
                                                   :reader-failures '(:semantic-data :fails))
                                   :ok (%designator :contract-a) :include-runtime nil)))
        (ok (eq :internal-error (getf report :status)))
        (ok (null (getf report :nothing-registered)))))
    (testing "include_runtime false reads no runtime and says why"
      (let ((report (symbol-report (inspection-api :handles +all-handles+
                                                   :registry registry)
                                   :ok (%designator :contract-a) :include-runtime nil)))
        (ok (null (getf report :runtime)))
        (ok (equal "include_runtime was false"
                   (getf report :runtime-unavailable-reason)))))))

;;; ------------------------------------------------------------------------
;;; D. Declarations

(deftest a-declaration-arrives-as-it-was-written
  (let* ((descriptor (contract-descriptor
                      :arguments '((:required) (:optional :supplied-p t)
                                   (:key :keyword :size))
                      :pre :one :post :two :state-post :none
                      :cases '(:small :large)))
         (description (%describe (contract-record descriptor))))
    (ok (eq :ok (getf description :status)))
    (testing "the arguments, in order, with the kind version 1 omits"
      (ok (equal '("ARG-0" "ARG-1" "ARG-2")
                 (mapcar (lambda (argument) (getf (getf argument :variable) :name))
                         (getf description :arguments))))
      (ok (equal '(:required :optional :key)
                 (mapcar (lambda (argument) (getf argument :kind))
                         (getf description :arguments))))
      (ok (equal "ARG-1-P" (getf (getf (second (getf description :arguments))
                                       :supplied-p)
                                 :name)))
      (ok (eq :size (getf (third (getf description :arguments)) :keyword))))
    (testing "a range's open end is *, and a zero is 0"
      (let ((first-spec (getf (first (getf description :arguments)) :spec))
            (second-spec (getf (second (getf description :arguments)) :spec)))
        (ok (equal "0" (getf first-spec :min)))
        (ok (equal "*" (getf second-spec :min)))))
    (testing "one form prints as itself and several as an AND"
      (ok (clause-reads-back-p (clause-forms :one) (getf description :preconditions)))
      (ok (clause-reads-back-p (clause-forms :two) (getf description :postconditions))))
    (testing "a clause that is not there is not applicable, not empty"
      (ok (null (getf description :state-post)))
      (ok (eq :not-applicable (getf description :state-post-complete))))
    (testing "the cases keep their order and their own fields"
      (ok (eq :exclusive (getf description :case-selection)))
      (ok (equal '(:small :large)
                 (mapcar (lambda (case) (getf case :name)) (getf description :cases))))
      (ok (eq :returns (getf (first (getf description :cases)) :outcome)))
      (ok (eq :signals (getf (second (getf description :cases)) :outcome)))
      (ok (getf (first (getf description :cases)) :returns))
      (ok (getf (second (getf description :cases)) :signals)))))

(deftest a-clause-that-is-there-and-nil-is-not-a-clause-that-is-absent
  (let ((present (%describe (contract-record (contract-descriptor :pre :present-nil))))
        (absent (%describe (contract-record (contract-descriptor :pre :none)))))
    (testing "a clause holding NIL is a clause, printed and complete"
      (ok (clause-reads-back-p (clause-forms :present-nil) (getf present :preconditions)))
      (ok (eq t (getf present :preconditions-complete))))
    (testing "no clause at all is not applicable"
      (ok (null (getf absent :preconditions)))
      (ok (eq :not-applicable (getf absent :preconditions-complete))))))

(deftest a-clause-that-could-not-be-read-is-not-a-clause-that-means-nil
  (testing "the text of the clause reads back as the clause"
    (ok (clause-reads-back-p (clause-forms :present-nil) "NIL"))
    (ok (clause-reads-back-p (clause-forms :present-nil) "COMMON-LISP:NIL")
        "printed package-qualified, as the projector prints")
    (ok (clause-reads-back-p (clause-forms :one) "(> X 0)"))
    (ok (clause-reads-back-p (clause-forms :two) "(AND (> X 0) (< X 100))")))
  (testing "a read that failed answers NIL, which is not the clause (NIL)"
    ;; Both of these end the text before a form is complete.  Told apart from
    ;; a clause that means NIL only because the failure is caught: the second
    ;; value of a failed read is a condition, which is not a position.
    (ok (not (clause-reads-back-p (clause-forms :present-nil) "")))
    (ok (not (clause-reads-back-p (clause-forms :present-nil) "(")))
    (ok (not (clause-reads-back-p (clause-forms :one) "(> X"))))
  (testing "a form with something after it is the text of something else"
    (ok (not (clause-reads-back-p (clause-forms :present-nil) "NIL TRAILING-JUNK")))
    (ok (not (clause-reads-back-p (clause-forms :one) "(> X 0) (< X 100)")))
    (ok (not (clause-reads-back-p (clause-forms :present-nil) "NIL)"))))
  (testing "and what is not a string is not a clause"
    (ok (not (clause-reads-back-p (clause-forms :present-nil) nil)))))

(deftest a-cut-clause-says-it-was-cut
  (multiple-value-bind (form text) (long-form 60)
    (let ((record (contract-record (contract-descriptor :pre :none))))
      (setf (getf record :preconditions) (list form))
      (testing "whole at its own length"
        (let ((description (%describe record :max-chars 60)))
          (ok (equal text (getf description :preconditions)))
          (ok (eq t (getf description :preconditions-complete)))
          (ok (eql 0 (getf description :preconditions-omitted-chars))
              "nothing omitted")))
      (testing "cut one character under it"
        (let ((description (%describe record :max-chars 59)))
          (ok (stringp (getf description :preconditions)) "still there")
          (ok (null (getf description :preconditions-complete)))
          (ok (plusp (getf description :preconditions-omitted-chars))))))))

(deftest a-schema-this-adapter-cannot-read-is-not-an-empty-contract
  (testing "a version it does not know"
    (let ((description (%describe (contract-record (contract-descriptor :version 2)))))
      (ok (eq :unsupported (getf description :status)))
      (ok (search "version" (getf description :message)))
      (ok (null (getf description :arguments)) "and nothing is projected")))
  (testing "a version 1 record missing required metadata"
    (let ((description (%describe (contract-record
                                   (contract-descriptor :metadata :missing-digest)))))
      (ok (eq :unsupported (getf description :status)))))
  (testing "a record of the wrong kind"
    (let ((description (%describe (contract-record
                                   (contract-descriptor :entity-kind :property)))))
      (ok (eq :unsupported (getf description :status)))))
  (testing "a projection that came back NIL"
    (let ((description (%describe :answers-nil)))
      (ok (eq :unsupported (getf description :status)))
      (ok (null (getf description :arguments))
          "not a contract with no arguments and no returns"))))

;;; ------------------------------------------------------------------------
;;; E. Digests

(deftest a-digest-comes-from-the-record-when-the-record-has-one
  (flet ((digest (record &rest arguments)
           (multiple-value-bind (api calls)
               (inspection-api :handles +all-handles+
                               :registry (%registry :contract-a :spec-a)
                               :contract-record record)
             (multiple-value-bind (digest complete)
                 (apply #'definition-digest api (definition-symbol :contract-a)
                        :inspection-registry :data-key :function-spec-data arguments)
               (list digest complete (length (calls-of calls :spec-data)))))))
    (testing "a complete version 1 digest is used as it stands, reading nothing else"
      (ok (equal '("fnv1a64-v1:00000000000000dd" t 0)
                 (digest (contract-record (contract-descriptor :reference t))))))
    (testing "an incomplete one gives no digest, and does not fall back"
      (ok (equal '(nil nil 0)
                 (digest (contract-record (contract-descriptor :metadata :incomplete-digest
                                                               :reference t))))))
    (testing "a version this cl-mcp does not know gives no digest either"
      (ok (equal '(nil nil 0)
                 (digest (contract-record (contract-descriptor :version 7 :reference t))))))
    (testing "the old shape digests from the readers, which it does read"
      (let ((answer (digest (contract-record (contract-descriptor :metadata :no-version
                                                                  :reference t)))))
        (ok (stringp (first answer)))
        (ok (eq t (second answer)))
        (ok (eql 1 (third answer)) "the spec it references was read")))
    (testing "an explicitly unread definition is not fetched again"
      (ok (equal '(nil nil 0)
                 (digest (contract-record (contract-descriptor :metadata :no-version))
                         :property nil))))))
