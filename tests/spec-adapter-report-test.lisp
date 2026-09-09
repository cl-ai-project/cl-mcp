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
                #:environment-data
                #:symbol-report
                #:describe-report
                #:check-report)
  (:import-from #:cl-mcp/src/utils/deadline
                #:forget-leaked-threads))

(in-package #:cl-mcp/tests/spec-adapter-report-test)

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
         (error "No property named ~S is registered." name))
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
         (error "No spec named ~S is registered." name))
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

(deftest check-report-digest-mismatch-is-loud
  (testing "an unexpected definition is reported as an unfaithful replay"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub)))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                   :expect-definition-digest "0000000000000000")))
      (ok (not (getf report :reproduction-faithful)))
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
