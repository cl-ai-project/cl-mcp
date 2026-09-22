;;;; tests/check-routing-specs-test.lisp
;;;;
;;;; Routing against a real cl-spec.  Opt-in: it needs cl-spec with the
;;;; check-it backend, and it is not in tests.lisp.  scripts/check-specs.lisp
;;;; runs it in a process of its own (CL_MCP_SPECS_MODE=integration), where a
;;;; missing dependency, a missing test, a failure or a skip fails the step.
;;;;
;;;; The stub registry of specs/check-routing-fixtures.lisp is a model of how
;;;; cl-spec relates definitions; this file checks that model against
;;;; cl-spec itself, so the two cannot share a mistake.  Two registries of the
;;;; test's own, A and B, hold the same declarations except for one related
;;;; property.  Each check binds CL-SPEC:*REGISTRY* to one of them around the
;;;; call and never touches the global registry.  The API is cl-spec's own,
;;;; resolved as spec-check resolves it; the only change is that its two
;;;; runners are wrapped, in a copy of the API, to record their arguments on
;;;; the way to the real functions.  Nothing global is swapped.

(defpackage #:cl-mcp/tests/check-routing-specs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-spec/main
                #:*registry*
                #:make-hash-table-registry
                #:defproperty
                #:defspec-function)
  ;; A bare :import-from declares the check-it backend as a dependency.
  (:import-from #:cl-spec/src/backends/check-it)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-cl-spec-api
                #:make-cl-spec-api
                #:cl-spec-api-functions
                #:cl-spec-api-classes
                #:cl-spec-api-specials
                #:cl-spec-api-version
                #:cl-spec-api-system-directory
                #:cl-spec-api-missing)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:check-report)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-check-response))

(in-package #:cl-mcp/tests/check-routing-specs-test)

;;; ------------------------------------------------------------------------
;;; The declarations

(defun routing-real-f (x)
  "Return twice X: the function the declarations are about."
  (* 2 x))

(defun routing-real-g (x)
  "Return X plus one: the function the unrelated property is about."
  (1+ x))

(defun %registry (&key (with-p2 t))
  "Return a fresh registry holding this file's declarations: a Function Spec
for ROUTING-REAL-F, a property of the same name about nothing, P1 and -- when
WITH-P2 -- P2 (:about ROUTING-REAL-F), and U (:about ROUTING-REAL-G)."
  (let ((registry (make-hash-table-registry)))
    (let ((*registry* registry))
      (defspec-function routing-real-f
        "Returns twice its argument."
        (:args (x (range integer 0 100)))
        (:returns integer)
        (:post (evenp (routing-real-f x))))
      (defproperty routing-real-f ((x (range integer 0 100)))
        "Shares the function's name and is about nothing."
        (:trials (:smoke 2 :normal 4))
        (= (routing-real-f x) (+ x x)))
      (defproperty routing-real-p1 ((x (range integer 0 100)))
        "Twice anything is even."
        (:about routing-real-f)
        (:trials (:smoke 2 :normal 4))
        (evenp (routing-real-f x)))
      (when with-p2
        (defproperty routing-real-p2 ((x (range integer 0 100)))
          "Twice anything is at least itself."
          (:about routing-real-f)
          (:trials (:smoke 2 :normal 4))
          (>= (routing-real-f x) x)))
      (defproperty routing-real-u ((x (range integer 0 100)))
        "About another function."
        (:about routing-real-g)
        (:trials (:smoke 2 :normal 4))
        (> (routing-real-g x) x)))
    registry))

(defun %change-p1 (registry)
  "Declare P1 again in REGISTRY with another docstring, which its digest
covers."
  (let ((*registry* registry))
    (defproperty routing-real-p1 ((x (range integer 0 100)))
      "Twice anything is even -- declared again."
      (:about routing-real-f)
      (:trials (:smoke 2 :normal 4))
      (evenp (routing-real-f x)))))

(defun %name (name)
  "Return NAME's package-qualified designator."
  (format nil "CL-MCP/TESTS/CHECK-ROUTING-SPECS-TEST::~A" name))

;;; ------------------------------------------------------------------------
;;; The API, recording

(defun %recording-api ()
  "Return (values API CALLS): cl-spec's own API with its two runners wrapped
to record (KEY NAME ARGUMENTS) in CALLS' CAR, most recent first, before
calling the real function."
  (multiple-value-bind (api status) (resolve-cl-spec-api)
    (unless (eq status :ok)
      (error "cl-spec is not usable here: ~S" status))
    (let ((calls (list nil))
          (functions (copy-list (cl-spec-api-functions api))))
      (dolist (key '(:run-property :check-function))
        (let ((real (getf functions key))
              (key key))
          (setf (getf functions key)
                (lambda (name &rest arguments)
                  (push (list key name (copy-list arguments)) (car calls))
                  (apply real name arguments)))))
      (values (make-cl-spec-api :functions functions
                                :classes (cl-spec-api-classes api)
                                :specials (cl-spec-api-specials api)
                                :version (cl-spec-api-version api)
                                :system-directory (cl-spec-api-system-directory api)
                                :missing (cl-spec-api-missing api))
              calls))))

(defun %check (registry &rest arguments)
  "Return (values REPORT CALLS) for CHECK-REPORT with ARGUMENTS, run against
REGISTRY through the recording API."
  (multiple-value-bind (api calls) (%recording-api)
    (let ((*registry* registry))
      (values (apply #'check-report api :ok :timeout-seconds 60 arguments)
              (reverse (car calls))))))

(defun %ran (calls)
  "Return (KEY NAME) for each recorded call, sorted by name."
  (sort (mapcar (lambda (call) (list (first call) (symbol-name (second call)))) calls)
        #'string< :key #'second))

;;; ------------------------------------------------------------------------
;;; Tests

(deftest real-selections-run-only-what-they-name
  (let ((registry (%registry)))
    (testing "symbol= runs the properties about the function, and nothing else"
      (multiple-value-bind (report calls) (%check registry :symbol (%name "ROUTING-REAL-F"))
        (ok (eq :completed (getf report :status)))
        (ok (equal '((:run-property "ROUTING-REAL-P1") (:run-property "ROUTING-REAL-P2"))
                   (%ran calls)))
        (let ((selection (getf report :selection)))
          (ok (equal "ROUTING-REAL-F" (getf (getf selection :contract-not-run) :name)))
          (ok (equal "ROUTING-REAL-F" (getf (getf selection :own-property-not-run) :name))))
        (ok (every (lambda (call) (eq registry (getf (third call) :registry))) calls))))
    (testing "property= of the shared name runs that property only"
      (multiple-value-bind (report calls) (%check registry :property (%name "ROUTING-REAL-F"))
        (ok (eq :completed (getf report :status)))
        (ok (equal '((:run-property "ROUTING-REAL-F")) (%ran calls)))))
    (testing "function= runs the contract only, and names the properties it left"
      (multiple-value-bind (report calls) (%check registry :function (%name "ROUTING-REAL-F")
                                                  :trials 5)
        (ok (eq :completed (getf report :status)))
        (ok (equal '((:check-function "ROUTING-REAL-F")) (%ran calls)))
        (ok (equal '("ROUTING-REAL-P1" "ROUTING-REAL-P2")
                   (sort (mapcar (lambda (plist) (getf plist :name))
                                 (getf (getf report :selection) :properties-not-run))
                         #'string<)))))))

(deftest real-registries-answer-for-themselves
  (let ((a (%registry))
        (b (%registry :with-p2 nil)))
    (testing "the same request names what each registry holds"
      (ok (equal '((:run-property "ROUTING-REAL-P1") (:run-property "ROUTING-REAL-P2"))
                 (%ran (nth-value 1 (%check a :symbol (%name "ROUTING-REAL-F"))))))
      (ok (equal '((:run-property "ROUTING-REAL-P1"))
                 (%ran (nth-value 1 (%check b :symbol (%name "ROUTING-REAL-F")))))))
    (testing "a property only one registry holds is not found in the other"
      (multiple-value-bind (report calls) (%check b :property (%name "ROUTING-REAL-P2"))
        (ok (eq :not-registered (getf report :status)))
        (ok (null calls)))
      (multiple-value-bind (report calls) (%check a :property (%name "ROUTING-REAL-P2"))
        (ok (eq :completed (getf report :status)))
        (ok (equal '((:run-property "ROUTING-REAL-P2")) (%ran calls)))))))

(deftest real-budgets-and-seeds-reach-cl-spec
  (let ((registry (%registry)))
    (testing "a contract: trials 3 and seed 0"
      (multiple-value-bind (report calls)
          (%check registry :function (%name "ROUTING-REAL-F") :trials 3 :seed 0)
        (let* ((result (first (getf report :results)))
               (source (getf (getf result :core-record) :source)))
          (ok (equal '(:seed 0 :registry) (subseq (third (first calls)) 0 3)))
          (ok (eql 3 (getf (third (first calls)) :trials)))
          (ok (equal "0" (getf result :seed)) "reported as the text of the seed given")
          (ok (eql 0 (getf source :seed)) "and cl-spec's record says it ran with it")
          (ok (eql 3 (getf source :budget)))
          (ok (eql 3 (getf (getf result :trials) :budget)))
          (ok (equal "cl-spec result" (getf (getf result :trials) :budget-source))))))
    (testing "a property: the smoke profile and a seed past 2^62"
      (let ((seed (+ (expt 2 62) 123)))
        (multiple-value-bind (report calls)
            (%check registry :property (%name "ROUTING-REAL-P1") :profile "smoke" :seed seed)
          (let* ((result (first (getf report :results)))
                 (source (getf (getf result :core-record) :source)))
            (ok (eq :smoke (getf (third (first calls)) :profile)))
            (ok (eql seed (getf (third (first calls)) :seed)))
            (ok (equal (format nil "~D" seed) (getf result :seed)))
            (ok (eql seed (getf source :seed)))
            (ok (eql 2 (getf source :trials)) "the smoke entry of its :trials table")))))))

(deftest real-replay-and-a-changed-declaration
  (let ((registry (%registry)))
    (multiple-value-bind (first-run) (%check registry :property (%name "ROUTING-REAL-P1")
                                             :seed 11)
      (let* ((result (first (getf first-run :results)))
             (digest (getf result :definition-digest)))
        (ok (stringp digest))
        (testing "the same declaration, seed and budget: a faithful replay"
          (let* ((again (%check registry :property (%name "ROUTING-REAL-P1") :seed 11
                                         :expect-definition-digest digest))
                 (replayed (first (getf again :results))))
            (ok (eq :true (getf replayed :definition-match)))
            (ok (eq :true (getf again :reproduction-faithful)))
            (ok (eq (getf result :status) (getf replayed :status)))
            (ok (eql (getf (getf result :trials) :executed)
                     (getf (getf replayed :trials) :executed)))
            (ok (equal "11" (getf replayed :seed)))))
        (testing "the declaration changed: the digest no longer matches, and the run stands"
          (%change-p1 registry)
          (let* ((changed (%check registry :property (%name "ROUTING-REAL-P1") :seed 11
                                           :expect-definition-digest digest))
                 (rerun (first (getf changed :results))))
            (ok (eq :false (getf rerun :definition-match)))
            (ok (eq :false (getf changed :reproduction-faithful)))
            (ok (eq :passed (getf rerun :status)))
            (ok (not (equal digest (getf rerun :definition-digest))))))))))

(deftest real-entry-reads-seed-text-and-reports-what-ran
  (let ((registry (%registry)))
    (flet ((respond (&rest pairs)
             (let ((params (make-hash-table :test #'equal)))
               (loop for (key value) on pairs by #'cddr
                     do (setf (gethash key params) value))
               (let ((*registry* registry))
                 (spec-check-response params))))
           (field (response &rest keys)
             (let ((value (aref (gethash "results" response) 0)))
               (dolist (key keys value)
                 (setf value (gethash key value))))))
      (testing "\"00042\" runs seed 42 and is reported as 42"
        (let ((response (respond "property" (%name "ROUTING-REAL-P1")
                                 "seed" "00042" "profile" "smoke")))
          (ok (equal "completed" (gethash "status" response)))
          (ok (equal "42" (field response "seed")))
          (ok (equal "42" (field response "core_result" "data" "seed")))))
      (testing "no seed: the seed reported is the one cl-spec drew, not 0"
        (let* ((response (respond "property" (%name "ROUTING-REAL-P1") "profile" "smoke"))
               (reported (field response "seed")))
          (ok (stringp reported))
          (ok (equal reported (field response "core_result" "data" "seed"))))))))
