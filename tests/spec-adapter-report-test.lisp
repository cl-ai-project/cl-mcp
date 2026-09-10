;;;; tests/spec-adapter-report-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/spec-adapter-report.  Every cl-spec call goes
;;;; through the CL-SPEC-API struct, so the whole layer is exercised here with
;;;; lambdas and no cl-spec in the image.  That is deliberate: the branches
;;;; that matter most -- cl-spec absent, backend absent, zero properties,
;;;; timeout -- are exactly the ones a suite depending on a healthy cl-spec
;;;; could never reach.

(defpackage #:cl-mcp/tests/spec-adapter-report-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:make-cl-spec-api)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:list-report
                #:environment-data
                #:symbol-report
                #:describe-report
                #:check-report)
  (:import-from #:cl-mcp/src/utils/deadline
                #:forget-leaked-threads))

(in-package #:cl-mcp/tests/spec-adapter-report-test)

(define-condition fixture-unknown-name (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No such name is registered.")))
  (:documentation "Stands in for cl-spec's UNKNOWN-SPEC / UNKNOWN-PROPERTY.

The adapter tells \"not registered\" from \"something went wrong reading it\"
by asking the API's condition classes, so a stub that signals a plain ERROR
exercises the internal-error path, not the not-registered one."))

(defun %fixture-package ()
  "Return a package holding the symbols these tests talk about."
  (let ((package (or (find-package "CL-MCP-SPEC-REPORT-FIXTURE")
                     (make-package "CL-MCP-SPEC-REPORT-FIXTURE" :use '()))))
    (dolist (name '("ADD" "SMALL-INT" "ADD-COMMUTES" "LONELY" "A"))
      (export (intern name package) package))
    package))

(defun %sym (name)
  "Return the fixture package's symbol named NAME."
  (find-symbol name (%fixture-package)))

(defun %stub-api (&rest overrides)
  "Return a CL-SPEC-API whose handles answer a small fixed registry.

OVERRIDES come first in the plist, so GETF finds them before the defaults."
  ;; Forced before any handle is built: the adapter resolves a symbol
  ;; designator with FIND-PACKAGE, so the fixture package has to exist by the
  ;; time a report runs, not merely by the time a stub lambda is called.
  (%fixture-package)
  (make-cl-spec-api
   :version "0.1.0"
   :system-directory "/tmp/cl-spec/"
   :classes (list :unknown-spec 'fixture-unknown-name
                  :unknown-property 'fixture-unknown-name)
   :functions
   (append
    overrides
    (list
     :registry (lambda () :stub-registry)
     :generator-backend (lambda () :stub-backend)
     :backend-default-trials (lambda (backend) (declare (ignore backend)) 100)
     :semantic-data
     (lambda (symbol &key registry)
       (declare (ignore registry))
       (list :symbol symbol
             :package (package-name (symbol-package symbol))
             :spec nil :function-spec nil :property nil
             :properties-about (when (eq symbol (%sym "ADD"))
                                 (list (%sym "ADD-COMMUTES")))))
     :properties-for
     (lambda (symbol &optional registry)
       (declare (ignore registry))
       (when (eq symbol (%sym "ADD")) (list (%sym "ADD-COMMUTES"))))
     :property-data
     (lambda (name &key registry)
       (declare (ignore registry))
       (unless (eq name (%sym "ADD-COMMUTES"))
         (error 'fixture-unknown-name))
       (list :name (%sym "ADD-COMMUTES")
             :kind :commutativity
             :targets (list (%sym "ADD"))
             :tags (list :math)
             :documentation "Addition commutes."
             :trials (list :normal 100)
             :arguments (list (list :variable (%sym "A")
                                    :spec (list :name nil :kind :reference
                                                :target (%sym "SMALL-INT")
                                                :source-form (%sym "SMALL-INT")
                                                :source-location nil)))
             :body '((= (add a a) (add a a)))
             :source-form '(defproperty add-commutes ((a small-int)) (= 1 1))
             :source-location (list :file "fixture.lisp" :package "FIXTURE")
             :metadata (list :shrink t)))
     :spec-data
     (lambda (name &key registry)
       (declare (ignore registry))
       (unless (eq name (%sym "SMALL-INT"))
         (error 'fixture-unknown-name))
       (list :name (%sym "SMALL-INT") :kind :range :base-type nil
             :min 0 :max 100
             :source-form '(range 0 100) :source-location nil))))))

(defun %result-stub (&key (status :passed) (trials 100) (seed 42)
                          counterexample shrunk condition (elapsed 0.01))
  "Return a stand-in for a cl-spec PROPERTY-RESULT as a plist.

The API struct reads a result through eight reader handles, so a plist plus
GETF readers is a complete substitute and no cl-spec class is needed."
  (list :status status :trials trials :seed seed :profile :normal
        :counterexample counterexample :shrunk-counterexample shrunk
        :condition condition :elapsed elapsed))

(defun %api-with-run (run-property &rest overrides)
  "Return a stub API whose RUN-PROPERTY is RUN-PROPERTY."
  (apply #'%stub-api
         (append
          overrides
          (list :run-property run-property
                :result-status (lambda (r) (getf r :status))
                :result-trials (lambda (r) (getf r :trials))
                :result-seed (lambda (r) (getf r :seed))
                :result-profile (lambda (r) (getf r :profile))
                :result-counterexample (lambda (r) (getf r :counterexample))
                :result-shrunk-counterexample
                (lambda (r) (getf r :shrunk-counterexample))
                :result-condition (lambda (r) (getf r :condition))
                :result-elapsed (lambda (r) (getf r :elapsed))))))

(defun %two-property-semantic-data ()
  "Return a :SEMANTIC-DATA handle reporting two properties about any symbol."
  (lambda (symbol &key registry)
    (declare (ignore registry))
    (list :symbol symbol
          :package (package-name (symbol-package symbol))
          :spec nil :function-spec nil :property nil
          :properties-about (list (%sym "ADD-COMMUTES") (%sym "ADD-COMMUTES")))))

;;; ---------------------------------------------------------------------------
;;; Environment and availability
;;; ---------------------------------------------------------------------------

(deftest environment-data-separates-not-loaded-from-incomplete
  (testing "cl-spec absent is reported as such, with no invented version"
    (let ((data (environment-data nil :not-loaded)))
      (ok (not (getf data :cl-spec-loaded)))
      (ok (null (getf data :cl-spec-version)))
      (ok (stringp (getf data :lisp)))))
  (testing "an incomplete API names what was missing"
    (let* ((api (make-cl-spec-api :missing (list "RUN-PROPERTY")))
           (data (environment-data api :incomplete)))
      (ok (not (getf data :cl-spec-loaded)))
      (ok (equal (list "RUN-PROPERTY") (getf data :missing))))))

(deftest symbol-report-without-cl-spec
  (testing "an absent cl-spec is not reported as an absent contract"
    (let ((report (symbol-report nil :not-loaded "cl:car")))
      (ok (eq :cl-spec-not-loaded (getf report :status)))
      (ok (search "load-system" (getf report :message)))
      (ok (search "NOT evidence" (getf report :message))))))

;;; ---------------------------------------------------------------------------
;;; spec-symbol
;;; ---------------------------------------------------------------------------

(deftest symbol-report-unresolved-symbol
  (testing "a name that denotes nothing is diagnosed, not guessed"
    (let ((report (symbol-report (%stub-api) :ok "NO-SUCH-PACKAGE-XYZ:FOO")))
      (ok (eq :unresolved-symbol (getf report :status)))
      (ok (eq :package-not-found (getf (getf report :reason) :reason))))))

(deftest symbol-report-lists-related-properties
  (testing "a symbol with an :about property reports it by package and name"
    (let ((report (symbol-report (%stub-api) :ok
                                 "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                                 :include-runtime nil)))
      (ok (eq :ok (getf report :status)))
      (ok (string= "ADD" (getf (getf report :symbol) :name)))
      (let ((properties (getf report :properties)))
        (ok (= 1 (length properties)))
        (let ((first-property (first properties)))
          (ok (string= "ADD-COMMUTES" (getf (getf first-property :name) :name)))
          (ok (eq :commutativity (getf first-property :kind)))
          (ok (getf first-property :shrink-enabled))
          (testing "the body is summarized, not inlined, and says so"
            (ok (getf first-property :body-omitted))
            (ok (= 1 (getf first-property :body-forms)))
            (ok (stringp (getf first-property :definition-digest)))))))))

(deftest symbol-report-refuses-a-risky-runtime-lookup
  (testing "a symbol whose name would need escaping gets no runtime join"
    ;; FIND-SYMBOL matches this name exactly, which is the point: resolution
    ;; never goes through the reader. The runtime join does, so it declines
    ;; rather than read back a name that would denote something else.
    (let ((package (%fixture-package)))
      (export (intern "ODD NAME" package) package))
    (let ((report (symbol-report (%stub-api) :ok
                                 "CL-MCP-SPEC-REPORT-FIXTURE:ODD NAME")))
      (ok (eq :ok (getf report :status)))
      (ok (string= "ODD NAME" (getf (getf report :symbol) :name)))
      (ok (null (getf report :runtime)))
      (ok (search "escaping" (getf report :runtime-unavailable-reason))))))

(deftest symbol-report-zero-properties-is-not-a-clean-bill
  (testing "no registration is reported as no registration"
    (let ((report (symbol-report (%stub-api) :ok
                                 "CL-MCP-SPEC-REPORT-FIXTURE:LONELY"
                                 :include-runtime nil)))
      (ok (eq :ok (getf report :status)))
      (ok (null (getf report :properties)))
      (ok (getf report :nothing-registered)))))

;;; ---------------------------------------------------------------------------
;;; spec-describe
;;; ---------------------------------------------------------------------------

(deftest describe-report-property-includes-body
  (testing "detail returns the body the summary omitted"
    (let ((report (describe-report (%stub-api) :ok "property"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES")))
      (ok (eq :ok (getf report :status)))
      (ok (search "ADD" (getf report :body)))
      (ok (getf report :body-complete))
      (ok (search "DEFPROPERTY" (string-upcase (getf report :source-form)))))))

(deftest describe-report-truncates-loudly
  (testing "a body past max-chars is cut and the cut is reported"
    (let ((report (describe-report (%stub-api) :ok "property"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                                   :max-chars 5)))
      (ok (not (getf report :body-complete)))
      (ok (plusp (getf report :body-omitted-chars))))))

(deftest describe-report-unknown-name
  (testing "an unregistered name is not-registered, not an error"
    (let ((report (describe-report (%stub-api) :ok "spec"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :not-registered (getf report :status))))))

(deftest describe-report-function-spec-is-unsupported
  (testing "function specs name the cl-spec API that is missing"
    (let ((report (describe-report (%stub-api) :ok "function-spec"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :unsupported (getf report :status)))
      (ok (search "function-spec-data" (getf report :message))))))

(deftest describe-report-rejects-unknown-kind
  (testing "an unrecognized kind is an argument error"
    (let ((report (describe-report (%stub-api) :ok "generator"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :invalid-arguments (getf report :status))))))

;;; ---------------------------------------------------------------------------
;;; spec-check
;;; ---------------------------------------------------------------------------

(defvar *fixture-backend* :global-backend
  "Stands in for CL-SPEC:*GENERATOR-BACKEND*, so a test can check that the run
thread sees the value the caller captured rather than the global one.")

(deftest check-report-binds-the-captured-backend-in-the-run-thread
  (testing "the run sees the backend the caller captured, not the global"
    ;; RUN-PROPERTY reads *GENERATOR-BACKEND* where it runs, and a deadline
    ;; thread does not inherit dynamic bindings. Without PROGV the thread
    ;; would read :GLOBAL-BACKEND here and the budget would have been derived
    ;; from a different object than the trials ran under.
    (let* ((api (%api-with-run
                 (lambda (name &key profile seed registry)
                   (declare (ignore name profile seed registry))
                   (%result-stub :status (if (eq *fixture-backend* :captured)
                                             :passed
                                             :failed)))
                 :generator-backend (lambda () :captured)))
           (with-specials (make-cl-spec-api
                           :functions (cl-mcp/src/spec-adapter-core:cl-spec-api-functions api)
                           :specials (list :generator-backend '*fixture-backend*)))
           (report (check-report with-specials :ok
                                 :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :global-backend *fixture-backend*))
      (ok (eq :passed (getf (first (getf report :results)) :status))))))

(deftest check-report-zero-trials-is-not-verified
  (testing "a passing status with nothing evaluated is not a verification"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub :status :passed :trials 0)))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :completed (getf report :status)))
      (ok (not (getf report :verified)))
      (ok (member :zero-trials (getf report :verification-gaps)))
      (testing "and the two unmeasurable gaps are always listed"
        (ok (member :rejection-counts-unmeasured (getf report :verification-gaps)))
        (ok (member :input-coverage-unmeasured (getf report :verification-gaps)))))))

(deftest check-report-empty-counterexample-is-present-not-missing
  (testing "a property that generates no arguments still has a counterexample"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub :status :failed :trials 1))
                                  :property-data
                                  (lambda (name &key registry)
                                    (declare (ignore name registry))
                                    (list :name (%sym "ADD-COMMUTES")
                                          :arguments nil
                                          :metadata (list :shrink t))))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES")))
      (let ((result (first (getf report :results))))
        (ok (eq :present (getf result :counterexample-status)))
        (ok (null (getf result :counterexample)))))))

(deftest check-report-disabled-shrinking-says-so
  (testing "(:shrink nil) is reported as disabled, not as nothing found"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub :status :failed :trials 1
                                                  :counterexample (list (%sym "A") 5)))
                                  :property-data
                                  (lambda (name &key registry)
                                    (declare (ignore name registry))
                                    (list :name (%sym "ADD-COMMUTES")
                                          :arguments (list (list :variable (%sym "A")
                                                                 :spec nil))
                                          :metadata (list :shrink nil))))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES")))
      (ok (eq :disabled (getf (first (getf report :results)) :shrink-status))))))

(deftest check-report-timeout-leaves-worker-state-unknown
  (testing "a stopped timeout is not evidence the image is safe to reuse"
    (unwind-protect
         (let ((report (check-report
                        (%api-with-run (lambda (&rest ignored)
                                         (declare (ignore ignored))
                                         (sleep 5)
                                         (%result-stub)))
                        :ok
                        :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                        :timeout-seconds 0.3)))
           (ok (eq :timeout (getf (first (getf report :results)) :status)))
           (ok (member (getf report :worker-reuse) '(:unknown :unsafe)))
           (ok (search "pool-kill-worker" (getf report :worker-reuse-message)))
           (testing "and the counterexample is unavailable with a reason"
             (let ((result (first (getf report :results))))
               (ok (eq :unavailable (getf result :counterexample-status)))
               (ok (stringp (getf result :counterexample-unavailable-reason))))))
      (forget-leaked-threads))))

(defun %listing-api (&rest overrides)
  "Return a stub API that can enumerate a two-name registry."
  (apply #'%stub-api
         (append
          overrides
          (list :list-specs
                (lambda (&optional registry)
                  (declare (ignore registry))
                  (list (%sym "SMALL-INT")))
                :list-properties
                (lambda (&optional registry)
                  (declare (ignore registry))
                  (list (%sym "ADD-COMMUTES")))
                :properties-with-tag
                (lambda (tag &optional registry)
                  (declare (ignore registry))
                  (when (eq tag :math) (list (%sym "ADD-COMMUTES"))))))))

(defun %contract-listing-api ()
  "Return a stub API that can enumerate contracts and nothing else.

Stands for a cl-spec whose function-spec half is present while the older
listing functions are not -- the shape the blanket listing guard refused."
  (%stub-api :list-function-specs
             (lambda (&optional registry)
               (declare (ignore registry))
               (list (%sym "ADD")))
             :function-spec-data
             (lambda (name &key registry)
               (declare (ignore registry))
               (list :name name
                     :documentation "ADD stays inside SMALL-INT."
                     :arguments (list (list :variable (%sym "A")
                                            :spec (list :kind :reference
                                                        :target (%sym "SMALL-INT"))))
                     :returns (list :kind :reference
                                    :target (%sym "SMALL-INT"))
                     :preconditions nil
                     :postconditions nil))))

(deftest list-report-enumerates-what-is-registered
  (testing "both kinds come back with the property's discovery fields"
    (let ((report (list-report (%listing-api) :ok :kind "both")))
      (ok (eq :ok (getf report :status)))
      (ok (= 1 (getf (getf report :counts) :specs)))
      (ok (= 1 (getf (getf report :counts) :properties)))
      (let ((property (first (getf report :properties))))
        (ok (string= "ADD-COMMUTES" (getf (getf property :name) :name)))
        (ok (eq :commutativity (getf property :kind)))
        (ok (equal (list :math) (getf property :tags)))
        (ok (string= "ADD" (getf (first (getf property :targets)) :name)))
        (testing "and no body or digest, which a listing does not need"
          (ok (null (getf property :body)))
          (ok (null (getf property :definition-digest))))))))

(deftest list-report-filters-by-kind-and-package
  (testing "kind narrows what is enumerated"
    (ok (null (getf (list-report (%listing-api) :ok :kind "specs") :properties)))
    (ok (null (getf (list-report (%listing-api) :ok :kind "properties") :specs))))
  (testing "an unknown package is diagnosed rather than answered as empty"
    (let ((report (list-report (%listing-api) :ok :kind "both"
                               :package "NO-SUCH-PACKAGE-FOR-LISTING")))
      (ok (eq :unresolved-package (getf report :status)))
      (ok (search "looked up, not created" (getf report :message)))))
  (testing "a package that exists but holds nothing gives an empty listing"
    (let ((report (list-report (%listing-api) :ok :kind "both"
                               :package "KEYWORD")))
      (ok (eq :ok (getf report :status)))
      (ok (null (getf report :specs)))
      (ok (null (getf report :properties))))))

(deftest list-report-does-not-count-a-kind-it-was-not-asked-for
  (testing "a kind that was not requested has no count, not a count of zero"
    ;; "0 specs" for kind=properties reads as "this registry has no specs",
    ;; which is the reading every other zero in these tools is careful to
    ;; rule out.
    (let ((properties-only (list-report (%listing-api) :ok :kind "properties"))
          (specs-only (list-report (%listing-api) :ok :kind "specs")))
      (ok (null (getf (getf properties-only :counts) :specs)))
      (ok (= 1 (getf (getf properties-only :counts) :properties)))
      (ok (null (getf (getf specs-only :counts) :properties)))
      (ok (= 1 (getf (getf specs-only :counts) :specs)))))
  (testing "and a requested kind that matched nothing still counts zero"
    (let ((report (list-report (%listing-api) :ok :kind "both"
                               :package "KEYWORD")))
      (ok (eql 0 (getf (getf report :counts) :specs)))
      (ok (eql 0 (getf (getf report :counts) :properties))))))

(deftest list-report-separates-an-absent-tag-from-an-unmatched-one
  (testing "a tag that exists but matches nothing is resolved and empty"
    (let ((report (list-report (%listing-api) :ok :kind "properties"
                               :tag "shrink")))
      (ok (eq :ok (getf report :status)))
      (ok (null (getf report :properties)))
      (ok (eq t (getf (getf report :filters) :tag-resolved)))))
  (testing "a tag no keyword exists for says so"
    (let ((report (list-report (%listing-api) :ok :kind "properties"
                               :tag "no-such-tag-in-this-image-xyz")))
      (ok (eq :ok (getf report :status)))
      (ok (null (getf report :properties)))
      (ok (null (getf (getf report :filters) :tag-resolved)))
      ;; The point: asking about it must not intern it.
      (ok (null (find-symbol "NO-SUCH-TAG-IN-THIS-IMAGE-XYZ" "KEYWORD"))))))

(deftest list-report-without-the-listing-api
  (testing "a cl-spec that cannot enumerate is unsupported, not empty"
    ;; The listing functions are optional: losing them costs this one
    ;; operation rather than the whole adapter.
    (let ((report (list-report (%stub-api) :ok :kind "both")))
      (ok (eq :unsupported (getf report :status)))
      (ok (search "list-specs" (getf report :message))))))

(deftest list-report-gates-on-what-the-kind-needs
  (testing "a contract listing does not wait on the property listing API"
    ;; kind=function-specs reads neither LIST-SPECS nor LIST-PROPERTIES, and
    ;; the blanket guard predating it refused a listing it could produce --
    ;; while reporting function_specs_listable true three keys later.
    (let ((report (list-report (%contract-listing-api) :ok
                               :kind "function-specs")))
      (ok (eq :ok (getf report :status)))
      (ok (getf report :function-specs-listable))
      (ok (= 1 (length (getf report :function-specs))))))
  (testing "while both still needs both"
    (let ((report (list-report (%contract-listing-api) :ok :kind "both")))
      (ok (eq :unsupported (getf report :status))))))

(deftest list-report-rejects-an-unknown-kind
  (testing "kind is constrained"
    (ok (eq :invalid-arguments
            (getf (list-report (%listing-api) :ok :kind "everything") :status)))))

(deftest check-report-zero-properties-is-never-success
  (testing "a symbol with no :about property reports no-properties"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (error "must not run")))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:LONELY")))
      (ok (eq :no-properties (getf report :status)))
      (ok (not (getf report :verified)))
      (ok (null (getf report :results)))
      (ok (zerop (getf (getf report :selection) :count))))))

(deftest check-report-passing-property
  (testing "a passing property is verified and carries its seed as text"
    (let ((report (check-report
                   (%api-with-run (lambda (name &key profile seed registry)
                                    (declare (ignore name profile seed registry))
                                    (%result-stub :seed 3963993791726803706)))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :completed (getf report :status)))
      (ok (getf report :verified))
      (let ((result (first (getf report :results))))
        (ok (eq :passed (getf result :status)))
        (ok (string= "3963993791726803706" (getf result :seed)))
        (ok (null (getf result :counterexample)))
        (ok (= 100 (getf (getf result :trials) :budget)))
        (ok (= 100 (getf (getf result :trials) :executed)))))))

(deftest check-report-failing-property-externalizes-the-counterexample
  (testing "a failure carries named, printed arguments and is not verified"
    (let ((report (check-report
                   (%api-with-run
                    (lambda (name &key profile seed registry)
                      (declare (ignore name profile seed registry))
                      (%result-stub :status :failed :trials 1
                                    :counterexample (list (%sym "A") 68)
                                    :shrunk (list (%sym "A") 0))))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :completed (getf report :status)))
      (ok (not (getf report :verified)))
      (let* ((result (first (getf report :results)))
             (counterexample (first (getf result :counterexample))))
        (ok (eq :failed (getf result :status)))
        (ok (string= "A" (getf (getf counterexample :variable) :name)))
        (ok (string= "68" (getf (getf counterexample :value) :printed)))
        (ok (string= "0" (getf (getf (first (getf result :shrunk-counterexample))
                                     :value)
                               :printed)))))))

(deftest check-report-generator-error-is-not-a-pass
  (testing "a backend condition is classified, not counted as success"
    ;; No cl-spec condition classes are on this stub API, which is the
    ;; interesting case: classification has to fall back to the message and
    ;; must still keep the result out of the pass column.
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (error "No generator backend is installed.")))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :incomplete (getf report :status)))
      (ok (not (getf report :verified)))
      (let ((result (first (getf report :results))))
        (ok (eq :generator-error (getf result :status)))
        (ok (search "generator" (string-downcase (getf (getf result :condition)
                                                       :message))))))))

(deftest check-report-timeout-and-budget
  (testing "a run past the budget is timeout, and the next one never starts"
    (unwind-protect
         (let ((report (check-report
                        (%api-with-run (lambda (&rest ignored)
                                         (declare (ignore ignored))
                                         (sleep 5)
                                         (%result-stub))
                                       :semantic-data
                                       (%two-property-semantic-data))
                        :ok
                        :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                        :timeout-seconds 0.3)))
           (ok (eq :incomplete (getf report :status)))
           (ok (not (getf report :verified)))
           (let ((results (getf report :results)))
             (ok (= 2 (length results)))
             (ok (eq :timeout (getf (first results) :status)))
             (ok (eq :not-run (getf (second results) :status)))
             (ok (eq :budget-exhausted (getf (second results) :reason)))))
      (forget-leaked-threads))))

(deftest check-report-seed-requires-a-single-property
  (testing "a seed across several properties is refused rather than reused"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub))
                                  :semantic-data (%two-property-semantic-data))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                   :seed 42)))
      (ok (eq :invalid-arguments (getf report :status)))
      (ok (search "single" (string-downcase (getf report :message)))))))

(deftest check-report-refuses-a-setting-the-run-cannot-honour
  (testing "profile with function= is refused, not reported unused"
    ;; cl-spec's CHECK-FUNCTION takes no profile and a contract has no :TRIALS
    ;; table for one to select from, so publishing the caller's profile would
    ;; name a setting the run never used -- the mirror of the trials refusal.
    (let ((report (check-report (%stub-api) :ok
                                :function "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                                :profile "thorough")))
      (ok (eq :invalid-arguments (getf report :status)))
      (ok (search "profile" (string-downcase (getf report :message))))))
  (testing "trials with symbol= is refused the same way"
    (let ((report (check-report (%stub-api) :ok
                                :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                                :trials 100)))
      (ok (eq :invalid-arguments (getf report :status)))
      (ok (search "trials" (string-downcase (getf report :message))))))
  (testing "and neither waits on cl-spec to be loaded first"
    ;; An argument that is wrong is wrong whatever cl-spec is doing; answering
    ;; "cl-spec is not loaded" sends the caller to fix the wrong thing, then
    ;; hands them the real complaint on the next call.
    (let ((report (check-report nil :not-loaded :symbol "CL:CAR" :trials 100)))
      (ok (eq :invalid-arguments (getf report :status)))
      (ok (search "trials" (string-downcase (getf report :message))))))
  (testing "while a profile on a property selection is honoured"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub)))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                   :profile "thorough")))
      (ok (eq :completed (getf report :status)))
      (ok (eq :thorough (getf report :profile))))))

(deftest check-report-digest-mismatch-is-loud
  (testing "an unexpected definition is reported as an unfaithful replay"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub)))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                   :expect-definition-digest "0000000000000000")))
      (ok (eq :false (getf report :reproduction-faithful)))
      (let ((result (first (getf report :results))))
        (ok (eq :false (getf result :definition-match)))))))

(deftest check-report-without-backend
  (testing "an absent generator backend is its own answer"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub))
                                  :generator-backend (lambda () nil))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES")))
      (ok (eq :backend-not-loaded (getf report :status)))
      (ok (not (getf report :verified)))
      (ok (search "cl-spec/check-it" (getf report :message))))))

(deftest check-report-unknown-property
  (testing "an unregistered property name is not-registered"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub)))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:LONELY")))
      (ok (eq :not-registered (getf report :status)))
      (ok (not (getf report :verified))))))

(deftest check-report-counts-every-status
  (testing "an adapter-level failure is counted, not dropped from the tally"
    ;; The named buckets alone lost :GENERATOR-ERROR entirely, so a selection
    ;; of one whose run blew up reported one selected and zero of everything.
    (let* ((report (check-report
                    (%api-with-run (lambda (&rest ignored)
                                     (declare (ignore ignored))
                                     (error "No generator backend is installed.")))
                    :ok
                    :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD"))
           (counts (getf report :counts)))
      (ok (= 1 (getf counts :selected)))
      (ok (= 1 (getf counts :other)))
      (ok (equal '(1) (mapcar #'cdr (getf counts :by-status))))
      (testing "and the tally sums to the selection"
        (ok (= (getf counts :selected)
               (reduce #'+ (getf counts :by-status) :key #'cdr)))))))

(deftest describe-report-tells-a-failure-from-an-absence
  (testing "an internal failure is not reported as an unregistered name"
    ;; A blanket handler turned every error into :NOT-REGISTERED -- the exact
    ;; false negative this module exists to prevent.
    (let ((api (%stub-api :spec-data
                          (lambda (name &key registry)
                            (declare (ignore name registry))
                            (error "something went wrong in here")))))
      (let ((report (describe-report api :ok "spec"
                                     "CL-MCP-SPEC-REPORT-FIXTURE:SMALL-INT")))
        (ok (eq :internal-error (getf report :status)))
        (ok (search "something went wrong" (getf report :message)))))))

(deftest check-report-unreadable-digest-is-unknown-not-mismatch
  (testing "a digest that could not be computed disagrees with nothing"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub))
                                  :property-data
                                  (lambda (name &key registry)
                                    (declare (ignore registry))
                                    ;; Readable for the facts, unreadable for
                                    ;; the digest is not expressible here, so
                                    ;; make it unreadable for both: the digest
                                    ;; then comes back NIL.
                                    (if (eq name (%sym "ADD-COMMUTES"))
                                        (error 'fixture-unknown-name)
                                        (error 'fixture-unknown-name))))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                   :expect-definition-digest "0000000000000000")))
      ;; The property cannot be read at all, so this is not-registered --
      ;; which is itself the point: it is not reported as a digest mismatch.
      (ok (eq :not-registered (getf report :status)))
      (ok (not (eq :false (getf report :reproduction-faithful)))))))

(deftest check-report-no-properties-does-not-claim-an-unfaithful-replay
  (testing "a selection of zero has not been checked, not found unfaithful"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub)))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:LONELY")))
      (ok (eq :no-properties (getf report :status)))
      (ok (eq :not-checked (getf report :reproduction-faithful)))
      (ok (eq :safe (getf report :worker-reuse))))))

(deftest symbol-report-answers-when-cl-spec-signals
  (testing "a drifted cl-spec is a status, not a condition out of the tool"
    (let* ((api (%stub-api :semantic-data
                           (lambda (symbol &key registry)
                             (declare (ignore symbol registry))
                             (error "SEMANTIC-DATA got an unexpected argument"))))
           (report (symbol-report api :ok "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                                  :include-runtime nil)))
      (ok (eq :internal-error (getf report :status)))
      (ok (search "unexpected argument" (getf report :message))))))

(deftest describe-report-rejects-a-negative-budget
  (testing "max-chars is clamped rather than fed to subseq as an end index"
    ;; The tool layer refuses a non-positive value outright; the report layer
    ;; clamps so a direct caller cannot signal a type error either.
    (let ((report (describe-report (%stub-api) :ok "property"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                                   :max-chars -1)))
      (ok (eq :ok (getf report :status)))
      (ok (not (getf report :body-complete))))))

(deftest spec-summary-and-tree-tolerate-a-missing-spec
  (testing "an argument with no spec yields no spec node"
    (let ((report (describe-report
                   (%stub-api :property-data
                              (lambda (name &key registry)
                                (declare (ignore name registry))
                                (list :name (%sym "ADD-COMMUTES")
                                      :arguments (list (list :variable (%sym "A")
                                                             :spec nil))
                                      :metadata (list :shrink t))))
                   :ok "property" "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES")))
      (ok (eq :ok (getf report :status)))
      (ok (null (getf (first (getf report :arguments)) :spec))))))

(deftest check-report-requires-exactly-one-target
  (testing "neither or both of property and symbol is an argument error"
    (let ((api (%api-with-run (lambda (&rest ignored)
                                (declare (ignore ignored))
                                (%result-stub)))))
      (ok (eq :invalid-arguments (getf (check-report api :ok) :status)))
      (ok (eq :invalid-arguments
              (getf (check-report api :ok
                                  :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                                  :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")
                    :status))))))
