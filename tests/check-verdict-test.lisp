;;;; tests/check-verdict-test.lisp
;;;;
;;;; Fixed cases for the verdict layer of cl-mcp/src/spec-adapter-report: the
;;;; per-status counts, a contract's effective trials, verified and
;;;; verification_gaps.  They sit beside the generated properties of
;;;; specs/check-verdicts.lisp, need no cl-spec, and run in the default suite.
;;;;
;;;; Three layers:
;;;;   - every row of the fixtures' policy tables once, with fixed numbers and
;;;;     concrete expected values;
;;;;   - the joint between %CONTRACT-PLIST and the verdict, which the
;;;;     properties check one side at a time;
;;;;   - CHECK-REPORT itself, over a CL-SPEC-API stub that answers from v1
;;;;     result records and records every run it is asked for.  The record of
;;;;     calls lives in a closure, not a special variable: CHECK-REPORT runs
;;;;     each property in a thread of its own, which does not see this
;;;;     thread's dynamic bindings.  Nothing is swapped and no registry is
;;;;     touched; the stub hands out only atoms, so the object registry is
;;;;     left as it was, which the tests check.

(defpackage #:cl-mcp/tests/check-verdict-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:make-cl-spec-api)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:check-report
                #:%counts
                #:%contract-plist
                #:%verified-p
                #:%verification-gaps)
  (:import-from #:cl-mcp/src/object-registry
                #:registry-count)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:make-result-record
                #:record-with)
  (:import-from #:cl-mcp/specs/check-verdict-fixtures
                #:+all-statuses+
                #:result-kind
                #:result-kind-names
                #:selection-kind-names
                #:draw-result
                #:build-result
                #:build-selection
                #:expected-gaps
                #:expected-verified-p
                #:same-set-p
                #:rejection-inputs))

(in-package #:cl-mcp/tests/check-verdict-test)

(defmacro with-fixed-draws ((seed) &body body)
  "Run BODY with CL:RANDOM seeded from SEED, so every drawn number is fixed."
  `(let ((*random-state* (sb-ext:seed-random-state ,seed)))
     ,@body))

;;; ------------------------------------------------------------------------
;;; The policy tables, row by row

(deftest every-result-kind-is-worth-what-its-row-says
  (with-fixed-draws (1)
    (dolist (name (result-kind-names :domain :any))
      (dolist (runs '(:property :contract))
        (when (member (getf (result-kind name) :runs) (list runs :either))
          (let* ((descriptor (draw-result name runs))
                 (result (build-result descriptor))
                 (good (build-result (draw-result :property-passed)))
                 (verifiable (expected-verified-p (list descriptor))))
            (testing (format nil "~(~A~) as a ~(~A~) run" name runs)
              (ok (eq verifiable (%verified-p (list result))) "alone")
              (ok (eq verifiable (%verified-p (list good result))) "beside a good result")
              (unless (eq :unspecified (getf (result-kind name) :gaps))
                (ok (same-set-p (expected-gaps (list descriptor))
                                (%verification-gaps (list result))))))))))))

(deftest every-selection-kind-names-what-it-left-out
  (with-fixed-draws (2)
    (dolist (name (selection-kind-names))
      (let* ((runs (if (member name (selection-kind-names :contract)) :contract :property))
             (descriptor (draw-result (if (eq runs :contract)
                                          :contract-passed
                                          :property-passed)))
             (result (build-result descriptor))
             (gaps (%verification-gaps (list result) (build-selection name))))
        (testing (format nil "~(~A~)" name)
          (ok (same-set-p (expected-gaps (list descriptor) name) gaps))
          (testing "and the result is still verified: a gap is not a failed verdict"
            (ok (%verified-p (list result)))))))))

(deftest verified-results-still-carry-gaps
  (with-fixed-draws (3)
    (let ((property (build-result (draw-result :property-passed)))
          (contract (build-result (draw-result :contract-passed))))
      (testing "a verified property run: refusals unmeasured, coverage unmeasured"
        (ok (%verified-p (list property)))
        (ok (same-set-p '(:rejection-counts-unmeasured :input-coverage-unmeasured)
                        (%verification-gaps (list property)
                                            (build-selection :explicit-property)))))
      (testing "a verified contract run that left properties unrun"
        (ok (%verified-p (list contract)))
        (ok (same-set-p '(:properties-not-run :input-coverage-unmeasured)
                        (%verification-gaps (list contract)
                                            (build-selection :contract-properties-left)))))
      (testing "an :about run that left a contract unrun"
        (ok (%verified-p (list property)))
        (ok (same-set-p '(:contract-not-run :rejection-counts-unmeasured
                          :input-coverage-unmeasured)
                        (%verification-gaps (list property)
                                            (build-selection :about-contract-left))))))))

(deftest nothing-ran-is-not-verified-and-the-helper-still-names-its-gaps
  (ok (not (%verified-p '())))
  (let ((gaps (%verification-gaps '() (build-selection :about :count 0))))
    ;; No count was measured when nothing ran, and coverage never is.  The
    ;; no-properties-selected gap belongs to CHECK-REPORT's empty-selection
    ;; answer, tested below, not to this helper.
    (ok (member :rejection-counts-unmeasured gaps))
    (ok (member :input-coverage-unmeasured gaps))))

;;; ------------------------------------------------------------------------
;;; Counts

(defun %by-status-entry (counts status)
  "Return the BY-STATUS count COUNTS gives STATUS, or NIL for none."
  (cdr (assoc status (getf counts :by-status))))

(deftest counts-keep-every-status
  (with-fixed-draws (4)
    (testing "nothing selected: every field zero, no entries"
      (let ((counts (%counts '())))
        (ok (eql 0 (getf counts :selected)))
        (dolist (field '(:passed :failed :errored :timed-out :not-run :other))
          (ok (eql 0 (getf counts field)) (format nil "~(~A~)" field)))
        (ok (null (getf counts :by-status)))))
    (testing "every status once: five named fields, eight under other"
      (let ((counts (%counts
                     (loop for status in +all-statuses+
                           collect (build-result
                                    (draw-result (first (result-kind-names
                                                         :status status))))))))
        (ok (eql 13 (getf counts :selected)))
        (dolist (field '(:passed :failed :errored :timed-out :not-run))
          (ok (eql 1 (getf counts field)) (format nil "~(~A~)" field)))
        (ok (eql 8 (getf counts :other)))
        (ok (eql 13 (length (getf counts :by-status))))
        (dolist (status +all-statuses+)
          (ok (eql 1 (%by-status-entry counts status)) (format nil "~(~A~)" status)))))
    (testing "repeated statuses are added, not collapsed"
      (let ((counts (%counts
                     (mapcar (lambda (name) (build-result (draw-result name)))
                             '(:generator-error :property-passed :generator-error
                               :contract-passed :generator-error :internal-error)))))
        (ok (eql 6 (getf counts :selected)))
        (ok (eql 2 (getf counts :passed)))
        (ok (eql 4 (getf counts :other)))
        (ok (eql 3 (%by-status-entry counts :generator-error)))
        (ok (eql 1 (%by-status-entry counts :internal-error)))
        (ok (eql 2 (%by-status-entry counts :passed)))
        (ok (eql 3 (length (getf counts :by-status))))))))

;;; ------------------------------------------------------------------------
;;; Refusal counts

(defun %instance (row path executed rejected precondition
                  &key (rejected-form :value) (failure-reason :postcondition)
                    (failure-reason-form :value))
  "Return a refusal-count instance with fixed numbers, as the draw returns one."
  (list :row row :path path :executed executed :rejected rejected
        :rejected-form rejected-form :precondition precondition
        :failure-reason failure-reason :failure-reason-form failure-reason-form))

(defun %half (instance)
  "Return (values HALF CALLS) for INSTANCE read by %CONTRACT-PLIST."
  (multiple-value-bind (api source calls) (rejection-inputs instance)
    (values (%contract-plist api (list :fixture-result) (getf instance :executed) 2000
                             source (getf instance :precondition))
            calls)))

(deftest a-usable-count-is-subtracted-on-both-paths
  (dolist (path '(:record :legacy))
    (testing (format nil "~(~A~) path" path)
      (let ((half (%half (%instance :usable path 5 2 t))))
        (ok (eq :usable (getf half :rejection-status)))
        (ok (getf half :rejected-usable))
        (ok (eql 3 (getf half :effective-trials))))
      (let ((half (%half (%instance :usable path 0 0 t))))
        (ok (eql 0 (getf half :effective-trials)) "no trials, none refused"))
      (let ((half (%half (%instance :all-refused path 4 4 t))))
        (ok (getf half :rejected-usable))
        (ok (eql 0 (getf half :effective-trials)) "every input refused"))
      (let ((half (%half (%instance :no-precondition path 6 0 nil))))
        (ok (eq :no-precondition (getf half :rejection-status)))
        (ok (eql 6 (getf half :effective-trials)))))))

(deftest an-unusable-count-publishes-no-effective-trials
  (dolist (path '(:record :legacy))
    (testing (format nil "~(~A~) path" path)
      (loop for (row executed rejected precondition status) in
            '((:executed-uncounted nil 1 t :trials-uncounted)
              (:negative 5 -1 t :negative)
              (:overcounted 1 2 t :overcounted)
              (:contradicted 5 2 nil :contradicted)
              (:precondition-unknown 5 0 :unknown :precondition-unknown))
            do (let ((half (%half (%instance row path executed rejected precondition))))
                 (testing (format nil "~(~A~)" row)
                   (ok (eq status (getf half :rejection-status)))
                   (ok (not (getf half :rejected-usable)))
                   ;; Not zero, and not the raw trial count.
                   (ok (null (getf half :effective-trials)))
                   (ok (eq (eq row :overcounted) (getf half :rejected-overcounted)))
                   (ok (eq (eq row :contradicted) (getf half :rejected-contradicted))))))
      (testing "more refusals than trials, and no :pre: both flags, still no count"
        ;; Two causes at once.  Either may name the status; neither may let a
        ;; count through.
        (let ((half (%half (%instance :overcounted-without-pre path 1 2 nil))))
          (ok (member (getf half :rejection-status) '(:overcounted :contradicted)))
          (ok (not (getf half :rejected-usable)))
          (ok (null (getf half :effective-trials)))
          (ok (getf half :rejected-overcounted))
          (ok (getf half :rejected-contradicted)))))))

(deftest a-missing-count-is-unmeasured-and-says-why
  (loop for (path form rejected readable) in
        '((:record :absent nil nil) (:record :nil nil t) (:record :non-integer "3" t)
          (:legacy :no-reader nil nil) (:legacy :signals nil nil) (:legacy :nil nil t)
          (:legacy :non-integer 2.5 t))
        do (let ((half (%half (%instance :rejected-unreadable path 9 rejected t
                                         :rejected-form form))))
             (testing (format nil "~(~A~) ~(~A~)" path form)
               (ok (eq :unmeasured (getf half :rejection-status)))
               (ok (null (getf half :rejected)))
               (ok (null (getf half :effective-trials)))
               (ok (not (getf half :rejected-measured)))
               (ok (eq readable (getf half :rejected-readable)))))))

(deftest the-record-answers-and-the-readers-are-not-asked
  (testing "a present NIL stays NIL even when the reader would say a number"
    (multiple-value-bind (half calls)
        (%half (%instance :rejected-unreadable :record 9 nil t :rejected-form :nil))
      (ok (null (getf half :rejected)))
      (ok (getf half :rejected-readable) "the record declares the key")
      (ok (null (getf half :effective-trials)))
      (ok (null (car calls)) "no reader was called")))
  (testing "an absent key is not filled in from the reader"
    (multiple-value-bind (half calls)
        (%half (%instance :rejected-unreadable :record 9 nil t :rejected-form :absent))
      (ok (not (getf half :rejected-readable)))
      (ok (null (getf half :effective-trials)))
      (ok (null (car calls)))))
  (testing "a present NIL failure reason is the record's answer, and readable"
    (let ((half (%half (%instance :usable :record 5 2 t :failure-reason nil))))
      (ok (null (getf half :failure-reason)))
      (ok (getf half :failure-reason-readable))))
  (testing "on the legacy path the readers are what answer"
    (multiple-value-bind (half calls) (%half (%instance :usable :legacy 5 2 t))
      (ok (eql 2 (getf half :rejected)))
      (ok (eq :postcondition (getf half :failure-reason)))
      (ok (member :check-rejected (car calls))))))

;;; ------------------------------------------------------------------------
;;; From the contract half to the verdict

(defun %verdict-over (instance)
  "Return (values VERIFIED GAPS HALF RESULT) for a passing contract run whose
contract half is %CONTRACT-PLIST's own reading of INSTANCE."
  (let ((half (%half instance))
        (descriptor (with-fixed-draws (5) (draw-result :contract-passed))))
    (setf (getf descriptor :executed) (getf instance :executed))
    (let ((result (build-result descriptor :contract half)))
      (values (%verified-p (list result)) (%verification-gaps (list result)) half result))))

(deftest a-contract-half-reaches-the-verdict-without-raw-trials
  (testing "a hundred raw trials and no readable refusal count: not evidence"
    (multiple-value-bind (verified gaps half result)
        (%verdict-over (%instance :rejected-unreadable :legacy 100 nil t
                                  :rejected-form :no-reader))
      (ok (eql 100 (getf (getf result :trials) :executed)))
      (ok (null (getf half :effective-trials)))
      (ok (not verified))
      (ok (member :effective-trials-unknown gaps))
      (ok (member :rejection-counts-unmeasured gaps))
      (ok (not (member :zero-trials gaps)) "unknown is not zero")))
  (testing "more refusals than trials: not evidence either"
    (multiple-value-bind (verified gaps) (%verdict-over (%instance :overcounted :record 1 2 t))
      (ok (not verified))
      (ok (member :effective-trials-unknown gaps))))
  (testing "an unknown :pre is not guessed away"
    (multiple-value-bind (verified gaps)
        (%verdict-over (%instance :precondition-unknown :record 5 0 :unknown))
      (ok (not verified))
      (ok (member :effective-trials-unknown gaps))))
  (testing "every input refused: zero, and said so"
    (multiple-value-bind (verified gaps half)
        (%verdict-over (%instance :all-refused :record 4 4 t))
      (ok (eql 0 (getf half :effective-trials)))
      (ok (not verified))
      (ok (member :zero-trials gaps))
      (ok (not (member :effective-trials-unknown gaps)))))
  (testing "a usable count: verified, and no refusal gap"
    (multiple-value-bind (verified gaps half)
        (%verdict-over (%instance :usable :record 5 2 t))
      (ok (eql 3 (getf half :effective-trials)))
      (ok verified)
      (ok (equal '(:input-coverage-unmeasured) gaps))))
  (testing "no :pre and nothing refused: verified on every trial"
    (multiple-value-bind (verified gaps)
        (%verdict-over (%instance :no-precondition :legacy 5 0 nil))
      (ok verified)
      (ok (equal '(:input-coverage-unmeasured) gaps)))))

;;; ------------------------------------------------------------------------
;;; Through CHECK-REPORT

(define-condition verdict-unknown-name (error)
  ()
  (:report "No such name is registered with the stub.")
  (:documentation "Stands in for cl-spec's UNKNOWN-SPEC / UNKNOWN-PROPERTY."))

(defparameter +subject+ "CL-MCP/TESTS/CHECK-VERDICT-TEST::VERDICT-SUBJECT"
  "The designator of the symbol the stub's contract and properties are about.")

(defun %contract-definition (&key preconditions case-selection)
  "Return a v1 definition record for VERDICT-SUBJECT's contract."
  (list :schema-version 1 :record-kind :definition :entity-kind :function-spec
        :definition-digest "fnv1a64-v1:00000000000000aa" :definition-digest-complete t
        :definition-digest-covers :declaration-and-registered-dependencies
        :capabilities (list :generation :available :shrinking :available
                            :instrumentation :none)
        :name 'verdict-subject :arguments nil :preconditions preconditions
        :case-selection case-selection :cases nil))

(defun %recording-api (&key records about contract)
  "Return (values API CALLS): a CL-SPEC-API whose runs answer from RECORDS.

RECORDS is an alist from each name that may run to the v1 result record cl-spec
would return for it.  ABOUT lists the properties registered about
VERDICT-SUBJECT, and CONTRACT is its contract's definition record, or NIL for
none.  CALLS is a cons whose CAR collects every run, most recent first."
  (let ((calls (list nil)))
    (flet ((record-of (result) (copy-tree (cdr (assoc (second result) records))))
           (field (key) (lambda (result)
                          (getf (cdr (assoc (second result) records)) key))))
      (values
       (make-cl-spec-api
        :version "0.1.0"
        :system-directory "/tmp/cl-spec/"
        :classes (list :unknown-spec 'verdict-unknown-name
                       :unknown-property 'verdict-unknown-name)
        :functions
        (list :registry (lambda () :stub-registry)
              :generator-backend (lambda () :stub-backend)
              :backend-default-trials (lambda (backend) (declare (ignore backend)) 25)
              :semantic-data
              (lambda (symbol &key registry)
                (declare (ignore registry))
                (list :symbol symbol :package (package-name (symbol-package symbol))
                      :spec nil :property nil
                      :function-spec (and contract (eq symbol 'verdict-subject) symbol)
                      :properties-about (and (eq symbol 'verdict-subject)
                                             (copy-list about))))
              :property-data
              (lambda (name &key registry)
                (declare (ignore registry))
                (unless (member name about) (error 'verdict-unknown-name))
                (list :name name :kind :invariant :targets (list 'verdict-subject)
                      :tags nil :documentation "A fixture property."
                      :trials (list :normal 25) :arguments nil :body '(t)
                      :source-form '(defproperty) :source-location nil
                      :metadata (list :shrink t)))
              :function-spec-data
              (lambda (name &key registry)
                (declare (ignore registry))
                (unless (and contract (eq name 'verdict-subject))
                  (error 'verdict-unknown-name))
                (copy-tree contract))
              :run-property
              (lambda (name &key profile seed registry)
                (push (list :run-property name :profile profile :seed seed
                            :registry registry)
                      (car calls))
                (list :fixture-result name))
              :check-function
              (lambda (name &key seed registry trials)
                (push (list :check-function name :seed seed :registry registry
                            :trials trials)
                      (car calls))
                (list :fixture-result name))
              :result-data #'record-of
              :result-status (field :status)
              :result-trials (field :trials)
              :result-seed (field :seed)
              :result-profile (field :profile)
              :result-counterexample (field :counterexample)
              :result-shrunk-counterexample (field :shrunk-counterexample)
              :result-elapsed (field :elapsed)
              :result-condition (constantly nil)
              :check-rejected (field :rejected)
              :check-failure-reason (field :failure-reason)))
       calls))))

(defun %statuses (report)
  "Return the status of each of REPORT's results, in order."
  (mapcar (lambda (result) (getf result :status)) (getf report :results)))

(defun %gaps-are (expected report &optional (description "the gaps are as expected"))
  "Check that REPORT's gaps are EXPECTED as a set, and that each is listed once.

A set comparison alone passes a gap listed twice.  That is what the public
answer would carry if %VERIFICATION-GAPS and CHECK-REPORT both added the same
gap -- no-properties-selected on an empty selection, which CHECK-REPORT adds
itself -- and the gap property allows the helper either way."
  (let ((gaps (getf report :verification-gaps)))
    (ok (same-set-p expected gaps) description)
    (ok (= (length gaps) (length (remove-duplicates gaps))) "each gap is listed once")))

(defmacro with-registry-unchanged (() &body body)
  "Run BODY and check that the object registry holds as many objects after it."
  (let ((before (gensym "BEFORE")))
    `(let ((,before (registry-count)))
       (multiple-value-prog1 (progn ,@body)
         (ok (eql ,before (registry-count)) "the object registry is left as it was")))))

(deftest check-report-runs-each-selected-property-and-tallies-what-came-back
  (with-registry-unchanged ()
    (multiple-value-bind (api calls)
        (%recording-api :about '(verdict-holds verdict-breaks)
                        :records (list (cons 'verdict-holds
                                             (make-result-record :status :passed :trials 25))
                                       (cons 'verdict-breaks
                                             (make-result-record :status :failed :trials 3))))
      (let ((report (check-report api :ok :symbol +subject+)))
        (testing "the stub ran both, in order, as a property run"
          (ok (equal '((:run-property verdict-holds :profile :normal :seed nil
                        :registry :stub-registry)
                       (:run-property verdict-breaks :profile :normal :seed nil
                        :registry :stub-registry))
                     (reverse (car calls)))))
        (testing "completed, and not verified: one failed"
          (ok (eq :completed (getf report :status)))
          (ok (equal '(:passed :failed) (%statuses report)))
          (ok (not (getf report :verified))))
        (testing "the tally matches the results"
          (let ((counts (getf report :counts)))
            (ok (eql 2 (getf counts :selected)))
            (ok (eql 1 (getf counts :passed)))
            (ok (eql 1 (getf counts :failed)))
            (ok (eql 0 (getf counts :other)))))
        (testing "a failure is a verdict, not a gap"
          (%gaps-are '(:rejection-counts-unmeasured :input-coverage-unmeasured) report))))))

(deftest check-report-verifies-a-passing-property-and-keeps-its-gaps
  (with-registry-unchanged ()
    (multiple-value-bind (api calls)
        (%recording-api :about '(verdict-holds)
                        :records (list (cons 'verdict-holds
                                             (make-result-record :status :passed :trials 25))))
      (let ((report (check-report api :ok
                                  :property "CL-MCP/TESTS/CHECK-VERDICT-TEST::VERDICT-HOLDS")))
        (ok (eql 1 (length (car calls))))
        (ok (eq :completed (getf report :status)))
        (ok (getf report :verified))
        (ok (eql 1 (getf (getf report :counts) :passed)))
        (%gaps-are '(:rejection-counts-unmeasured :input-coverage-unmeasured) report
                   "verified, and the gaps are still there")))))

(deftest check-report-verifies-a-contract-by-its-effective-trials
  (with-registry-unchanged ()
    (multiple-value-bind (api calls)
        (%recording-api :about '(verdict-holds)
                        :contract (%contract-definition :preconditions '((> x 0)))
                        :records (list (cons 'verdict-subject
                                             (record-with
                                              (make-result-record :entity-kind :function-spec
                                                                  :status :passed :trials 5)
                                              :rejected 2))))
      (let* ((report (check-report api :ok :function +subject+ :trials 5))
             (contract (getf (first (getf report :results)) :contract)))
        (ok (equal '((:check-function verdict-subject :seed nil :registry :stub-registry
                      :trials 5))
                   (car calls)))
        (ok (eq :completed (getf report :status)))
        (ok (eql 3 (getf contract :effective-trials)) "five trials, two refused")
        (ok (getf report :verified))
        (%gaps-are '(:properties-not-run :input-coverage-unmeasured) report
                   "the counted refusals leave no refusal gap; the unrun property is one")))))

(deftest check-report-does-not-verify-a-contract-that-refused-everything
  (with-registry-unchanged ()
    (multiple-value-bind (api calls)
        (%recording-api :contract (%contract-definition :preconditions '((> x 0)))
                        :records (list (cons 'verdict-subject
                                             (record-with
                                              (make-result-record :entity-kind :function-spec
                                                                  :status :skipped :trials 4)
                                              :rejected 4))))
      (let* ((report (check-report api :ok :function +subject+ :trials 4))
             (contract (getf (first (getf report :results)) :contract)))
        (ok (eql 1 (length (car calls))))
        (testing "skipped is a completed run, and not a verification"
          (ok (eq :completed (getf report :status)))
          (ok (not (getf report :verified))))
        (ok (eql 0 (getf contract :effective-trials)))
        (ok (eql 1 (getf (getf report :counts) :other)))
        (ok (eql 1 (cdr (assoc :skipped (getf (getf report :counts) :by-status)))))
        (%gaps-are '(:skipped :input-coverage-unmeasured) report)))))

(deftest check-report-does-not-verify-a-contract-with-an-unreached-case
  (with-registry-unchanged ()
    (multiple-value-bind (api calls)
        (%recording-api
         :contract (%contract-definition :preconditions '((> x 0))
                                         :case-selection :exclusive)
         :records (list (cons 'verdict-subject
                              (record-with
                               (record-with
                                (make-result-record :entity-kind :function-spec
                                                    :status :passed :trials 5)
                                :rejected 1)
                               :case-report
                               (list :selection :exclusive :unit :normal-trials
                                     :declared-cases (list :small :large)
                                     :cases (list (list :name :small :documentation nil
                                                        :called 4 :passed 4 :failed 0
                                                        :error 0)
                                                  (list :name :large :documentation nil
                                                        :called 0 :passed 0 :failed 0
                                                        :error 0))
                                     :case-selection-errors 0 :capture-errors 0
                                     :never-called (list :large))))))
      (let ((report (check-report api :ok :function +subject+ :trials 5)))
        (ok (eql 1 (length (car calls))))
        (ok (eq :completed (getf report :status)))
        (ok (equal '(:passed) (%statuses report)) "cl-spec's passed stands")
        (ok (not (getf report :verified)) "but the unreached branch is not verified")
        (%gaps-are '(:cases-never-called :input-coverage-unmeasured) report)))))

(deftest check-report-with-nothing-selected-runs-nothing-and-verifies-nothing
  (with-registry-unchanged ()
    (multiple-value-bind (api calls) (%recording-api)
      (let ((report (check-report api :ok :symbol +subject+)))
        (ok (null (car calls)) "nothing was run")
        (ok (eq :no-properties (getf report :status)))
        (ok (not (getf report :verified)))
        (ok (null (getf report :results)))
        (ok (eql 0 (getf (getf report :counts) :selected)))
        ;; Listed once: CHECK-REPORT adds no-properties-selected itself, so the
        ;; helper must not have added it too.
        (%gaps-are '(:no-properties-selected :rejection-counts-unmeasured
                     :input-coverage-unmeasured)
                   report)))))
