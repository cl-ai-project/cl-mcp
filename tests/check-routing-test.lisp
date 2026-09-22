;;;; tests/check-routing-test.lisp
;;;;
;;;; Fixed cases for spec-check's routing: which target arguments it accepts,
;;;; how a seed and a trial count are read at the entry, which runner a
;;;; selection reaches with which raw arguments, and how a definition digest
;;;; is compared per result and overall.  They sit beside the generated
;;;; properties of specs/check-routing.lisp, need no cl-spec, and run in the
;;;; default suite.
;;;;
;;;; The runner-facing cases go through CHECK-REPORT with SPY-API
;;;; (specs/check-routing-fixtures.lisp): a stand-in cl-spec whose runners
;;;; take only the keywords the real ones take, signal on any other, and
;;;; record their raw argument lists and the registry and backend they see on
;;;; the thread CHECK-REPORT runs them on.  Nothing is swapped and no registry
;;;; is touched.  The entry cases call SPEC-CHECK-RESPONSE only with arguments
;;;; it refuses, or with a target that resolves to nothing, so no cl-spec in
;;;; the image is ever asked to run anything.

(defpackage #:cl-mcp/tests/check-routing-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:check-report
                #:%target-argument-error
                #:%run-one)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-check-response
                #:parse-seed-string)
  (:import-from #:cl-mcp/specs/check-routing-fixtures
                #:routing-f
                #:routing-p1
                #:routing-p2
                #:designator
                #:unknown-profile-name
                #:spy-api
                #:spy-calls
                #:spy-run-calls))

(in-package #:cl-mcp/tests/check-routing-test)

(defun %name (target)
  "Return TARGET's package-qualified designator."
  (values (designator target :qualified)))

(defun %params (&rest pairs)
  "Return a tool-arguments hash-table holding PAIRS."
  (let ((params (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key params) value))
    params))

(defun %property (name &rest options)
  "Return a SPY-API entry for a property NAME."
  (list* :name name :kind :property options))

(defun %contract (name &rest options)
  "Return a SPY-API entry for the contract of NAME."
  (list* :name name :kind :contract options))

(defun %check (api &rest arguments)
  "Return CHECK-REPORT's plist for ARGUMENTS against API, with a whole-call
budget no stub needs more than a moment of."
  (apply #'check-report api :ok :timeout-seconds 30 arguments))

(defun %argument-keys (call)
  "Return the keywords a runner CALL was given, in order."
  (loop for (key) on (getf call :arguments) by #'cddr collect key))

;;; ------------------------------------------------------------------------
;;; Target arguments

(deftest every-presence-of-the-three-targets
  ;; (property symbol function) given or not, and whether that is a target.
  (loop for (property symbol function valid)
          in '((nil nil nil nil) (t nil nil t) (nil t nil t) (nil nil t t)
               (t t nil nil) (t nil t nil) (nil t t nil) (t t t nil))
        do (let ((answer (%target-argument-error (and property "P") (and symbol "S")
                                                 (and function "F") nil nil)))
             (testing (format nil "property ~:[no~;yes~], symbol ~:[no~;yes~], ~
function ~:[no~;yes~]" property symbol function)
               (if valid
                   (ok (null answer))
                   (ok (eq :invalid-arguments (getf answer :status))))))))

(deftest trials-and-profile-go-with-their-own-targets
  (testing "trials: a contract only"
    (ok (eq :invalid-arguments (getf (%target-argument-error "P" nil nil 5 nil) :status)))
    (ok (eq :invalid-arguments (getf (%target-argument-error nil "S" nil 5 nil) :status)))
    (ok (null (%target-argument-error nil nil "F" 5 nil))))
  (testing "profile: a property or an :about selection only"
    (ok (eq :invalid-arguments (getf (%target-argument-error nil nil "F" nil "smoke") :status)))
    (ok (null (%target-argument-error "P" nil nil nil "smoke")))
    (ok (null (%target-argument-error nil "S" nil nil "smoke")))))

(deftest check-report-runs-nothing-for-a-bad-target
  (let ((p (%name :p1)) (f (%name :f)))
    (loop for (label . arguments)
            in `(("no target")
                 ("property and symbol" :property ,p :symbol ,f)
                 ("property and function" :property ,p :function ,f)
                 ("symbol and function" :symbol ,f :function ,f)
                 ("all three" :property ,p :symbol ,f :function ,f)
                 ("trials with property" :property ,p :trials 5)
                 ("trials with symbol" :symbol ,f :trials 5)
                 ("profile with function" :function ,f :profile "smoke"))
          do (multiple-value-bind (api calls)
                 (spy-api (list (%property 'routing-p1 :about t) (%contract 'routing-f)))
               (let ((report (apply #'%check api arguments)))
                 (testing label
                   (ok (eq :invalid-arguments (getf report :status)))
                   (ok (null (spy-run-calls calls)) "nothing was run")))))
    (testing "the same answer when cl-spec is not loaded at all"
      (let ((report (check-report nil :not-loaded :property p :symbol f)))
        (ok (eq :invalid-arguments (getf report :status)))))))

(deftest a-profile-is-resolved-before-anything-runs
  (flet ((run (profile)
           (multiple-value-bind (api calls) (spy-api (list (%property 'routing-p1)))
             (values (%check api :property (%name :p1) :profile profile) calls))))
    (testing "none given is normal"
      (multiple-value-bind (report calls) (run nil)
        (ok (eq :completed (getf report :status)))
        (ok (eq :normal (getf (getf (first (spy-run-calls calls)) :arguments) :profile)))))
    (testing "smoke is smoke"
      (multiple-value-bind (report calls) (run "smoke")
        (declare (ignore report))
        (ok (eq :smoke (getf (getf (first (spy-run-calls calls)) :arguments) :profile)))))
    (testing "a name nothing interns is refused, runs nothing and stays unknown"
      (let* ((name (unknown-profile-name))
             (key (string-upcase name)))
        (ok (null (find-symbol key "KEYWORD")))
        (multiple-value-bind (report calls) (run name)
          (ok (eq :invalid-arguments (getf report :status)))
          (ok (null (spy-run-calls calls))))
        (ok (null (find-symbol key "KEYWORD")) "trying it interned nothing")))))

;;; ------------------------------------------------------------------------
;;; Seeds and trials at the entry

(deftest seed-text-keeps-every-digit
  (loop for (text integer)
          in '(("0" 0) ("1" 1) ("00042" 42) ("0000" 0)
               ("9007199254740991" 9007199254740991)
               ("9007199254740992" 9007199254740992)
               ("9007199254740993" 9007199254740993)
               ("4611686018427387903" 4611686018427387903)
               ("4611686018427387904" 4611686018427387904)
               ("4611686018427387905" 4611686018427387905)
               ("18446744073709551617" 18446744073709551617)
               ("10000000000000000000000000000000000000007"
                10000000000000000000000000000000000000007))
        do (multiple-value-bind (seed message) (parse-seed-string text)
             (ok (eql integer seed) text)
             (ok (null message) text)))
  (testing "no seed is not seed 0"
    (multiple-value-bind (seed message) (parse-seed-string nil)
      (ok (null seed))
      (ok (null message)))))

(deftest seed-text-refuses-anything-but-digits
  (dolist (input (list 42 4.2 "" "+42" "-42" " 42" "42 " "4 2" "42e3" "4.2" "42.0"
                       "#x2A" "#b101" "#.(+ 1 2)" "1/2" "1,000" :seed (list "42")))
    (multiple-value-bind (seed message) (parse-seed-string input)
      (ok (null seed) (format nil "~S" input))
      (ok (stringp message) (format nil "~S is refused, not taken as no seed" input)))))

(deftest spec-check-refuses-a-bad-seed-or-trials-before-asking-cl-spec
  (dolist (params (list (%params "property" "X" "seed" 42)
                        (%params "property" "X" "seed" "")
                        (%params "property" "X" "seed" "-1")
                        (%params "property" "X" "seed" " 1")
                        (%params "property" "X" "seed" "1e3")
                        (%params "function" "F" "trials" 0)
                        (%params "function" "F" "trials" -1)
                        (%params "function" "F" "trials" 1000001)
                        (%params "function" "F" "trials" 1.5)
                        (%params "function" "F" "trials" "3")))
    (let ((response (spec-check-response params)))
      (testing (format nil "seed ~S, trials ~S" (gethash "seed" params)
                       (gethash "trials" params))
        (ok (equal "invalid-arguments" (gethash "status" response)))
        (ok (equal "not-consulted"
                   (gethash "cl_spec_status" (gethash "environment" response))))))))

(deftest spec-check-accepts-trials-at-both-bounds
  ;; A function that resolves to nothing, so whatever cl-spec is in the image
  ;; is never asked to run it: accepted here means the entry passed it on.
  (dolist (trials '(1 1000000))
    (let ((response (spec-check-response
                     (%params "function" "CL-MCP-CHECK-ROUTING-NO-SUCH-PACKAGE::F"
                              "trials" trials))))
      (ok (not (equal "invalid-arguments" (gethash "status" response)))
          (format nil "trials ~D reaches the report" trials)))))

;;; ------------------------------------------------------------------------
;;; What reaches the runner

(deftest the-seed-reaches-the-runner-exactly
  (dolist (seed (list 0 4611686018427387905 (+ (expt 10 40) 7)))
    (multiple-value-bind (api calls) (spy-api (list (%property 'routing-p1)))
      (let ((report (%check api :property (%name :p1) :seed seed))
            (call (first (spy-run-calls calls))))
        (testing (format nil "seed ~D" seed)
          (ok (eql seed (getf (getf call :arguments) :seed)))
          (ok (equal (format nil "~D" seed) (getf (first (getf report :results)) :seed)))))))
  (testing "no seed reaches the runner as none, and the report says what it used"
    (multiple-value-bind (api calls)
        (spy-api (list (%property 'routing-p1 :seed 4611686018427387905)))
      (let ((report (%check api :property (%name :p1)))
            (call (first (spy-run-calls calls))))
        (ok (member :seed (%argument-keys call)))
        (ok (null (getf (getf call :arguments) :seed)))
        (ok (equal "4611686018427387905" (getf (first (getf report :results)) :seed)))))))

(deftest a-seed-fans-out-to-one-property-only
  (flet ((run (entries &rest arguments)
           (multiple-value-bind (api calls) (spy-api entries)
             (values (apply #'%check api :symbol (%name :f) arguments) calls))))
    (testing "nothing related: nothing runs"
      (multiple-value-bind (report calls) (run (list (%contract 'routing-f)) :seed 5)
        (ok (eq :no-properties (getf report :status)))
        (ok (null (spy-run-calls calls)))))
    (testing "one related: it runs with the seed"
      (multiple-value-bind (report calls) (run (list (%property 'routing-p1 :about t)) :seed 5)
        (ok (eq :completed (getf report :status)))
        (ok (equal '(routing-p1) (mapcar (lambda (call) (getf call :name))
                                         (spy-run-calls calls))))
        (ok (eql 5 (getf (getf (first (spy-run-calls calls)) :arguments) :seed)))))
    (testing "two related and a seed, even 0: refused before either runs"
      (multiple-value-bind (report calls)
          (run (list (%property 'routing-p1 :about t) (%property 'routing-p2 :about t))
               :seed 0)
        (ok (eq :invalid-arguments (getf report :status)))
        (ok (null (spy-run-calls calls)))))
    (testing "two related and only an expected digest: both run"
      (multiple-value-bind (report calls)
          (run (list (%property 'routing-p1 :about t) (%property 'routing-p2 :about t))
               :expect-definition-digest "fnv1a64-v1:00000000000000aa")
        (ok (eq :completed (getf report :status)))
        (ok (equal '(routing-p1 routing-p2)
                   (mapcar (lambda (call) (getf call :name)) (spy-run-calls calls))))))))

(deftest each-runner-gets-its-own-keywords
  (testing "a property: profile, seed and registry"
    (multiple-value-bind (api calls) (spy-api (list (%property 'routing-p1)))
      (%check api :property (%name :p1) :seed 3)
      (let ((call (first (spy-run-calls calls))))
        (ok (eq :run-property (getf call :key)))
        (ok (equal '(:profile :seed :registry) (%argument-keys call))))))
  (testing "a contract with trials: seed, registry and trials, no profile"
    (multiple-value-bind (api calls) (spy-api (list (%contract 'routing-f)))
      (%check api :function (%name :f) :seed 3 :trials 7)
      (let ((call (first (spy-run-calls calls))))
        (ok (eq :check-function (getf call :key)))
        (ok (equal '(:seed :registry :trials) (%argument-keys call)))
        (ok (eql 7 (getf (getf call :arguments) :trials))))))
  (testing "a contract without trials gets the backend default"
    (multiple-value-bind (api calls) (spy-api (list (%contract 'routing-f)) :backend-default 25)
      (%check api :function (%name :f))
      (ok (eql 25 (getf (getf (first (spy-run-calls calls)) :arguments) :trials)))))
  (testing "a contract whose default cannot be read gets no trials keyword at all"
    (multiple-value-bind (api calls)
        (spy-api (list (%contract 'routing-f)) :backend-default :unreadable)
      (%check api :function (%name :f))
      (let ((call (first (spy-run-calls calls))))
        (ok call "it still ran")
        (ok (equal '(:seed :registry) (%argument-keys call))
            "absent, not :trials NIL")))))

(deftest the-budget-the-result-records-wins
  ;; Asked for 7, the result records a budget of 5 and 2 trials: an early
  ;; failure runs fewer trials than its budget, and neither number is the
  ;; request.
  (multiple-value-bind (api calls)
      (spy-api (list (%contract 'routing-f :budget 5 :executed 2 :status :failed)))
    (let* ((report (%check api :function (%name :f) :trials 7))
           (trials (getf (first (getf report :results)) :trials)))
      (ok (eql 7 (getf (getf (first (spy-run-calls calls)) :arguments) :trials)))
      (ok (eql 5 (getf trials :budget)))
      (ok (equal "cl-spec result" (getf trials :budget-source)))
      (ok (eql 2 (getf trials :executed))))))

(deftest the-run-sees-the-registry-and-backend-it-was-given
  (let ((registry (list :this-registry))
        (backend (list :this-backend)))
    (multiple-value-bind (api calls)
        (spy-api (list (%contract 'routing-f)) :registry registry :backend backend)
      (%check api :function (%name :f))
      (let ((run (first (spy-run-calls calls)))
            (default (first (spy-calls calls :backend-default-trials))))
        (ok (eq registry (getf (getf run :arguments) :registry)) "passed as the argument")
        (ok (eq registry (getf run :thread-registry)) "bound on the run thread")
        (ok (eq backend (getf run :thread-backend)) "bound on the run thread")
        (ok (eq backend (first (getf default :arguments)))
            "the budget came from the same backend")))))

;;; ------------------------------------------------------------------------
;;; Digests

(defparameter +digest+ "fnv1a64-v1:0123456789abcdef")
(defparameter +other-digest+ "fnv1a64-v1:0123456789abcdee")

(defun %replay (entries expected &rest arguments)
  "Return the report for a property= run of ROUTING-P1 over ENTRIES with
EXPECTED as the expected digest, or the other ARGUMENTS given."
  (let ((api (spy-api entries)))
    (apply #'%check api :expect-definition-digest expected
           (or arguments (list :property (%name :p1))))))

(deftest a-digest-is-compared-four-ways-and-changes-no-verdict
  (flet ((one (report) (first (getf report :results))))
    (testing "the same complete digest: true"
      (let ((report (%replay (list (%property 'routing-p1 :digest +digest+)) +digest+)))
        (ok (eq :true (getf (one report) :definition-match)))
        (ok (eq :true (getf report :reproduction-faithful)))))
    (testing "another complete digest: false, and the verdict stands"
      (let ((report (%replay (list (%property 'routing-p1 :digest +digest+)) +other-digest+)))
        (ok (eq :false (getf (one report) :definition-match)))
        (ok (eq :false (getf report :reproduction-faithful)))
        (ok (eq :passed (getf (one report) :status)))
        (ok (getf report :verified))))
    (testing "a failed run with the same digest is still a match"
      (let ((report (%replay (list (%property 'routing-p1 :digest +digest+ :status :failed))
                             +digest+)))
        (ok (eq :failed (getf (one report) :status)))
        (ok (eq :true (getf (one report) :definition-match)))))
    (testing "an incomplete digest: unknown, equal or not"
      (dolist (expected (list +digest+ +other-digest+))
        (let ((report (%replay (list (%property 'routing-p1 :digest +digest+ :complete nil))
                               expected)))
          (ok (eq :unknown (getf (one report) :definition-match)))
          (ok (eq :unknown (getf report :reproduction-faithful))))))
    (testing "nothing expected: not checked"
      (let ((report (%replay (list (%property 'routing-p1 :digest +digest+)) nil)))
        (ok (eq :not-checked (getf (one report) :definition-match)))
        (ok (eq :not-checked (getf report :reproduction-faithful)))))))

(deftest reproduction-is-judged-over-every-result
  (flet ((replay (second-digest second-complete)
           (%replay (list (%property 'routing-p1 :about t :digest +other-digest+)
                          (%property 'routing-p2 :about t :digest second-digest
                                                 :complete second-complete))
                    +digest+ :symbol (%name :f))))
    (testing "a mismatch and a match: not faithful"
      (let ((report (replay +digest+ t)))
        (ok (equal '(:false :true)
                   (mapcar (lambda (result) (getf result :definition-match))
                           (getf report :results))))
        (ok (eq :false (getf report :reproduction-faithful)))))
    (testing "a mismatch and an unknown: unknown, not a confirmed mismatch"
      (let ((report (replay +digest+ nil)))
        (ok (equal '(:false :unknown)
                   (mapcar (lambda (result) (getf result :definition-match))
                           (getf report :results))))
        (ok (eq :unknown (getf report :reproduction-faithful)))))
    (testing "nothing selected: not checked"
      (let ((report (%replay (list (%contract 'routing-f)) +digest+ :symbol (%name :f))))
        (ok (eq :no-properties (getf report :status)))
        (ok (eq :not-checked (getf report :reproduction-faithful)))))))

(deftest a-run-that-never-compared-is-not-a-mismatch
  (testing "a run with no budget left is not started, and not compared"
    (let* ((api (spy-api (list (%property 'routing-p1))))
           (result (%run-one api 'routing-p1 :stub-registry :property :normal 5
                             (list :budget 25) (list :value +digest+ :complete t)
                             +other-digest+ 0 2000 nil nil)))
      (ok (eq :not-run (getf result :status)))
      (ok (eq :not-checked (getf result :definition-match)))))
  (testing "a run that signals is not compared, and the whole replay is unknown"
    (let ((report (%replay (list (%property 'routing-p1 :digest +digest+
                                                        :signal "a controlled failure"))
                           +other-digest+)))
      (ok (eq :not-checked (getf (first (getf report :results)) :definition-match)))
      (ok (eq :unknown (getf report :reproduction-faithful))))))

(deftest a-property-and-a-contract-of-one-name-keep-their-own-digests
  (let ((entries (list (%property 'routing-f :digest +digest+)
                       (%contract 'routing-f :digest +other-digest+))))
    (flet ((match (kind expected)
             (getf (first (getf (%replay entries expected kind (%name :f)) :results))
                   :definition-match)))
      (ok (eq :true (match :property +digest+)))
      (ok (eq :false (match :property +other-digest+)))
      (ok (eq :true (match :function +other-digest+)))
      (ok (eq :false (match :function +digest+))))))

(deftest the-result-records-digest-wins-over-the-one-read-before
  ;; The definition read before the run carries one digest; the result
  ;; record carries the one cl-spec ran under.  The report compares the
  ;; second.
  (let* ((entries (list (%contract 'routing-f :definition-digest +other-digest+
                                              :digest +digest+)))
         (report (%replay entries +digest+ :function (%name :f)))
         (result (first (getf report :results))))
    (ok (equal +digest+ (getf result :definition-digest)))
    (ok (eq :true (getf result :definition-match)))))
