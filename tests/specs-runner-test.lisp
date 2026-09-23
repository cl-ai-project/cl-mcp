;;;; tests/specs-runner-test.lisp
;;;;
;;;; Tests of the cl-mcp/specs bundle's registration and of its runner's
;;;; verdicts.  Opt-in, like the bundle: tests.lisp does not load this file, so
;;;; the default suite never needs cl-spec.  Run it with
;;;;
;;;;   run-tests system=cl-mcp/tests/specs-runner-test
;;;;
;;;; or CL_MCP_SPECS_MODE=self-test through scripts/check-specs.lisp.
;;;;
;;;; Every fixture here is registered in a registry of its own, made fresh per
;;;; test, so none of the failing fixtures can reach CL-SPEC:*REGISTRY* or the
;;;; bundle.  Nothing here replaces a production function; the negative control
;;;; that does runs only as a separate process (see scripts/check-specs.lisp).

(defpackage #:cl-mcp/tests/specs-runner-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-spec/main
                #:defspec
                #:defspec-function
                #:defproperty
                #:defgenerator
                #:make-hash-table-registry
                #:find-property
                #:find-function-spec
                #:properties-for
                #:list-properties
                #:list-function-specs
                #:list-specs
                #:list-generators)
  (:import-from #:cl-mcp/specs
                #:register-specifications
                #:contract-names
                #:property-names
                #:spec-names
                #:generator-names)
  (:import-from #:cl-mcp/specs/path-fixtures
                #:with-read-fixture
                #:region-native)
  (:import-from #:cl-mcp/specs/runner
                #:bundle-targets
                #:run-checks
                #:run-bundle
                #:judge-entry
                #:judge-example
                #:bundle-consistency-problems
                #:covered-functions
                #:report-ok-p
                #:exit-code
                #:git-state
                #:print-report
                #:write-report))

(in-package #:cl-mcp/tests/specs-runner-test)

;;; ------------------------------------------------------------------------
;;; Fixtures

(defun fixture-double (x)
  "Fixture target: twice X."
  (* 2 x))

(defun fixture-sign (x)
  "Fixture target: :NEGATIVE or :NON-NEGATIVE."
  (if (minusp x) :negative :non-negative))

(defun fixture-identity (x)
  "Fixture target: X."
  x)

(defvar *strays* '()
  "Files FIXTURE-FAILS-AND-CANNOT-CLEAN-UP left for the test to remove.")

(defun %fixture-registry ()
  "Return a fresh registry holding every fixture below, and nothing else."
  (let ((cl-spec:*registry* (make-hash-table-registry)))
    (defproperty fixture-double-is-even ((x (range integer 0 100)))
      "Passes: twice anything is even."
      (:about fixture-double)
      (:trials (:normal 20 :zero 0))
      (evenp (fixture-double x)))
    (defproperty fixture-small ((x (range integer 0 100)))
      "Fails: not every X is below 50."
      (:about fixture-double)
      (:trials (:normal 50))
      (< x 50))
    (defspec-function fixture-double
      "Passes: the result is even."
      (:args (x (range integer 0 100)))
      (:returns integer)
      (:post (evenp result)))
    (defspec-function fixture-identity
      "Rejects every generated input."
      (:args (x (range integer 0 100)))
      (:pre (minusp x))
      (:returns integer))
    (defspec-function fixture-sign
      "Its :NEGATIVE case is never reached from (RANGE INTEGER 0 100)."
      (:args (x (range integer 0 100)))
      (:cases
       (:negative (:when (minusp x)) (:returns (member :negative)))
       (:non-negative (:when (not (minusp x))) (:returns (member :non-negative)))))
    (defgenerator fixture-broken-generator ()
      "Signals instead of drawing."
      (error "fixture generator broke"))
    (defspec fixture-broken-input integer
      (:generator fixture-broken-generator))
    (defproperty fixture-broken-generation ((x fixture-broken-input))
      "Never gets an input."
      (:trials (:normal 10))
      (integerp x))
    (defproperty fixture-slow ((x (range integer 0 1)))
      "Outlives any short deadline."
      (:trials (:normal 1))
      (sleep 3)
      (integerp x))
    (defproperty fixture-fails-and-cannot-clean-up ((x (range integer 0 1)))
      "Writes to stderr, then fails with its read fixture holding a file the
fixture did not create, so the cleanup fails while the body unwinds."
      (:trials (:normal 1))
      (:shrink nil)
      (format *error-output* "noise before the failure~%")
      (with-read-fixture (fixture)
        (let ((stray (concatenate 'string (region-native fixture :project) "stray.txt")))
          (push stray *strays*)
          (with-open-file (out (uiop:parse-native-namestring stray) :direction :output)
            (write-string "not recorded" out)))
        (error "body failed on purpose ~D" x)))
    cl-spec:*registry*))

(defun %run (targets &rest options)
  "Run TARGETS in a fresh fixture registry under one seed."
  (apply #'run-checks targets :registry (%fixture-registry) :seeds '(11) options))

(defun %only-entry (report)
  "Return the single entry of REPORT."
  (let ((entries (getf report :entries)))
    (assert (= 1 (length entries)))
    (first entries)))

(defun %same-names-p (a b)
  "True when the name lists A and B hold the same names."
  (and (null (set-difference a b)) (null (set-difference b a))))

;;; ------------------------------------------------------------------------
;;; Registration

(deftest bundle-registration-matches-its-listing
  (testing "a fresh registry holds exactly what the bundle lists"
    (let ((registry (make-hash-table-registry)))
      (register-specifications registry)
      (ok (%same-names-p (contract-names) (list-function-specs registry)))
      (ok (%same-names-p (property-names) (list-properties registry)))
      (ok (%same-names-p (spec-names) (list-specs registry)))
      (ok (%same-names-p (generator-names) (list-generators registry)))
      (ok (null (bundle-consistency-problems registry)))))
  (testing "the listing names exactly the bundle's contracts and properties"
    (ok (%same-names-p (contract-names)
                       '(cl-mcp/src/utils/strings:ensure-trailing-newline
                         cl-mcp/src/utils/sanitize:sanitize-for-json
                         cl-mcp/src/utils/sanitize:sanitize-error-message)))
    (ok (= 3 (length (contract-names))))
    (ok (= 46 (length (property-names))))
    (ok (= 46 (length (remove-duplicates (property-names)))))
    (ok (%same-names-p (remove-if-not (lambda (name)
                                        (string= "CL-MCP/SPECS/POOL-OWNERSHIP"
                                                 (package-name (symbol-package name))))
                                      (property-names))
                       (cl-mcp/specs/pool-ownership:property-names))
        "the pool properties are the two of specs/pool-ownership.lisp")
    (ok (= 2 (length (cl-mcp/specs/pool-ownership:property-names))))
    (ok (%same-names-p (remove-if-not (lambda (name)
                                        (string= "CL-MCP/SPECS/PATHS"
                                                 (package-name (symbol-package name))))
                                      (property-names))
                       (cl-mcp/specs/paths:property-names))
        "the read-path properties are the four of specs/paths.lisp")
    (ok (%same-names-p (remove-if-not (lambda (name)
                                        (string= "CL-MCP/SPECS/WRITE-PATHS"
                                                 (package-name (symbol-package name))))
                                      (property-names))
                       (cl-mcp/specs/write-paths:property-names))
        "the write-path properties are the five of specs/write-paths.lisp")
    (ok (member 'cl-mcp/specs/write-paths::write-preserves-safe-spellings (property-names))
        "the safe-spelling relation is listed under its own name")
    (ok (equal (sort (mapcar #'symbol-name
                             (remove-if-not (lambda (name)
                                              (string= "CL-MCP/SPECS/CORE-RECORDS"
                                                       (package-name (symbol-package name))))
                                            (property-names)))
                     #'string<)
               (sort (list "CORE-RECORD-AVAILABILITY-SEPARATES-ABSENCE-FROM-NIL"
                           "CORE-RECORD-PROJECTS-EACH-FIELD-BY-ITS-ROLE"
                           "CORE-RECORD-SEEDS-STAY-DECIMAL-TEXT"
                           "CORE-RECORD-IGNORES-ORDER-DUPLICATES-AND-UNKNOWN-KEYS"
                           "CORE-RECORD-REPORTS-EVERY-CUT"
                           "CORE-RECORD-VALIDATION-SEPARATES-OK-UNSUPPORTED-MALFORMED")
                     #'string<))
        "the record properties are exactly the six of specs/core-records.lisp")
    (ok (equal (sort (mapcar #'symbol-name
                             (remove-if-not (lambda (name)
                                              (string= "CL-MCP/SPECS/CHECK-VERDICTS"
                                                       (package-name (symbol-package name))))
                                            (property-names)))
                     #'string<)
               (sort (list "CHECK-VERDICT-COUNTS-KEEP-EVERY-STATUS"
                           "CHECK-VERDICT-EFFECTIVE-TRIALS-ONLY-FROM-A-USABLE-COUNT"
                           "CHECK-VERDICT-VERIFIED-NEEDS-EVIDENCE-FROM-EVERY-RESULT"
                           "CHECK-VERDICT-GAPS-NAME-EACH-SHORTFALL-AND-NOTHING-ELSE")
                     #'string<))
        "the verdict properties are exactly the four of specs/check-verdicts.lisp")
    (ok (equal (sort (mapcar #'symbol-name
                             (remove-if-not (lambda (name)
                                              (string= "CL-MCP/SPECS/CHECK-ROUTING"
                                                       (package-name (symbol-package name))))
                                            (property-names)))
                     #'string<)
               (sort (list "CHECK-ROUTING-TARGET-ARGUMENTS-ARE-EXCLUSIVE"
                           "CHECK-ROUTING-SEED-TEXT-KEEPS-EVERY-DIGIT"
                           "CHECK-ROUTING-SELECTION-NAMES-ONLY-WHAT-WAS-ASKED"
                           "CHECK-ROUTING-BUDGET-COMES-FROM-ITS-STATED-SOURCE"
                           "CHECK-ROUTING-DIGEST-COMPARISON-HAS-FOUR-ANSWERS")
                     #'string<))
        "the routing properties are exactly the five of specs/check-routing.lisp")
    (ok (equal (sort (mapcar #'symbol-name
                             (remove-if-not (lambda (name)
                                              (string= "CL-MCP/SPECS/SPEC-INSPECTION"
                                                       (package-name (symbol-package name))))
                                            (property-names)))
                     #'string<)
               (sort (list "SPEC-INSPECTION-OPERATIONS-NEED-THEIR-OWN-HANDLES"
                           "SPEC-INSPECTION-LISTING-SEPARATES-CAPABILITY-FROM-COUNT"
                           "SPEC-INSPECTION-REGISTRATION-IS-NOT-READ-FAILURE"
                           "SPEC-INSPECTION-CONTRACT-DECLARATION-SURVIVES-DESCRIBE"
                           "SPEC-INSPECTION-DIGEST-COMES-FROM-THE-RECORD-OR-THE-READERS")
                     #'string<))
        "the inspection properties are exactly the five of specs/spec-inspection.lisp"))
  (testing "the functions it covers include those checked by properties alone"
    (let ((registry (make-hash-table-registry)))
      (register-specifications registry)
      (ok (%same-names-p (covered-functions registry)
                         '(cl-mcp/src/utils/strings:ensure-trailing-newline
                           cl-mcp/src/utils/sanitize:sanitize-for-json
                           cl-mcp/src/utils/sanitize:sanitize-error-message
                           cl-mcp/src/utils/paths:allowed-read-path
                           cl-mcp/src/utils/paths:resolve-readable-path
                           cl-mcp/src/utils/paths:ensure-write-path
                           cl-mcp/src/fs:fs-write-file
                           cl-mcp/src/spec-core-record:field-availability
                           cl-mcp/src/spec-core-record:validate-versioned-record
                           cl-mcp/src/spec-core-record:project-record
                           cl-mcp/src/spec-core-record:project-core-record
                           cl-mcp/src/spec-adapter-report::%counts
                           cl-mcp/src/spec-adapter-report::%contract-plist
                           cl-mcp/src/spec-adapter-report::%verified-p
                           cl-mcp/src/spec-adapter-report::%verification-gaps
                           cl-mcp/src/spec-adapter-report::%target-argument-error
                           cl-mcp/src/spec-adapter-report::%resolve-profile
                           cl-mcp/src/tools/spec-entry:parse-seed-string
                           cl-mcp/src/spec-adapter-report::%select-properties
                           cl-mcp/src/spec-adapter-report::%trials-budget
                           cl-mcp/src/spec-adapter-report::%definition-match
                           cl-mcp/src/spec-adapter-core:api-backend-available-p
                           cl-mcp/src/spec-adapter-core:definition-digest
                           cl-mcp/src/spec-adapter-report::contract-operation-missing
                           cl-mcp/src/spec-adapter-report:list-report
                           cl-mcp/src/spec-adapter-report:symbol-report
                           cl-mcp/src/spec-adapter-report:describe-report
                           cl-mcp/src/spec-adapter-report::%describe-function-spec
                           cl-mcp/src/tools/spec-response-builders:build-spec-list-response
                           cl-mcp/src/tools/spec-response-builders:build-spec-symbol-response
                           cl-mcp/src/tools/spec-response-builders:build-spec-describe-response
                           cl-mcp/src/tools/spec-response-builders:build-spec-check-response
                           cl-mcp/src/pool:get-or-assign-worker
                           cl-mcp/src/pool:release-session
                           cl-mcp/src/pool:kill-session-worker
                           cl-mcp/src/pool:shutdown-pool)))))
  (testing "a definition missing from the listing is reported"
    (let ((registry (make-hash-table-registry)))
      (register-specifications registry)
      (let ((cl-spec:*registry* registry))
        (defproperty fixture-unlisted ((x (range integer 0 1)))
          "Registered next to the bundle but not in its listing."
          (integerp x)))
      (ok (find :unlisted (bundle-consistency-problems registry) :key #'first)))))

(deftest bundle-reregistration-is-stable
  (let ((registry (make-hash-table-registry)))
    (let ((cl-spec:*registry* registry))
      (defproperty fixture-foreign ((x (range integer 0 1)))
        "Someone else's property about a bundle function."
        (:about cl-mcp/src/utils/strings:ensure-trailing-newline)
        (integerp x)))
    (register-specifications registry)
    (let ((properties (list-properties registry))
          (contracts (list-function-specs registry))
          (about (properties-for 'cl-mcp/src/utils/strings:ensure-trailing-newline registry)))
      (register-specifications registry)
      (testing "registering twice adds no name and no association"
        (ok (equal properties (list-properties registry)))
        (ok (equal contracts (list-function-specs registry)))
        (let ((again (properties-for 'cl-mcp/src/utils/strings:ensure-trailing-newline
                                     registry)))
          (ok (equal about again))
          (ok (= (length again) (length (remove-duplicates again))))))
      (testing "another registration survives"
        (ok (nth-value 1 (find-property 'fixture-foreign registry)))
        (ok (member 'fixture-foreign
                    (properties-for 'cl-mcp/src/utils/strings:ensure-trailing-newline
                                    registry)))))))

(deftest bundle-is-visible-in-the-current-registry
  (testing "loading the bundle registered it where spec-list and spec-symbol look"
    (dolist (name (contract-names))
      (ok (nth-value 1 (find-function-spec name)) (format nil "~S" name)))
    (dolist (name (property-names))
      (ok (nth-value 1 (find-property name)) (format nil "~S" name))))
  (testing "registering into a fresh registry leaves the current one alone"
    (let ((before (list-properties)))
      (register-specifications (make-hash-table-registry))
      (ok (equal before (list-properties))))))

;;; ------------------------------------------------------------------------
;;; Verdicts on fixtures

(deftest runner-passes-a-sound-run
  (let ((report (%run '((:property fixture-double-is-even)
                        (:function-spec fixture-double)))))
    (ok (report-ok-p report))
    (ok (= 0 (exit-code report)))
    (dolist (entry (getf report :entries))
      (ok (eq :passed (getf entry :status)))
      (ok (plusp (getf entry :trials))))))

(deftest runner-fails-a-failing-property
  (let* ((report (%run '((:property fixture-small))))
         (entry (%only-entry report)))
    (ng (report-ok-p report))
    (ok (= 1 (exit-code report)))
    (ok (eq :failed (getf entry :status)))
    (ok (getf entry :counterexample))
    (ok (equal '((:status :failed)) (judge-entry entry)))))

(deftest runner-refuses-an-empty-selection
  (let ((report (%run '())))
    (ng (report-ok-p report))
    (ok (member '(:empty-selection) (getf report :problems) :test #'equal))))

(deftest runner-refuses-an-unregistered-target
  (dolist (target '((:property fixture-nowhere) (:function-spec fixture-nowhere)))
    (let ((report (%run (list target))))
      (ng (report-ok-p report))
      (ok (eq :not-registered (getf (%only-entry report) :status))))))

(deftest runner-refuses-a-run-with-no-trials
  (testing "a profile whose budget is zero"
    (let* ((report (%run '((:property fixture-double-is-even)) :profile :zero))
           (entry (%only-entry report)))
      (ng (report-ok-p report))
      (ok (eql 0 (getf entry :trials)))))
  (testing "a profile the property does not declare is refused, not run"
    (let* ((report (%run '((:property fixture-double-is-even)) :profile :nightly))
           (entry (%only-entry report)))
      (ng (report-ok-p report))
      (ok (eq :unknown-profile (getf entry :status)))
      (ok (null (getf entry :trials))))))

(deftest runner-refuses-a-contract-that-rejected-every-input
  (let* ((report (%run '((:function-spec fixture-identity))))
         (entry (%only-entry report)))
    (ng (report-ok-p report))
    (ok (eq :skipped (getf entry :status)))
    (ok (eql (getf entry :trials) (getf entry :rejected)))))

(deftest runner-refuses-a-case-no-trial-reached
  (let* ((report (%run '((:function-spec fixture-sign))))
         (entry (%only-entry report)))
    (ok (eq :passed (getf entry :status)) "cl-spec itself answers :PASSED")
    (ng (report-ok-p report) "but the runner does not")
    (ok (find-if (lambda (problem) (member (first problem)
                                           '(:cases-never-called :case-not-called)))
                 (judge-entry entry)))))

(deftest runner-refuses-a-generation-error
  (let* ((report (%run '((:property fixture-broken-generation))))
         (entry (%only-entry report)))
    (ng (report-ok-p report))
    (ok (eq :signalled (getf entry :status)))
    (ok (search "fixture generator broke" (getf (getf entry :condition) :report)))))

(deftest runner-refuses-a-timeout
  (let* ((report (%run '((:property fixture-slow)) :timeout-seconds 1))
         (entry (%only-entry report)))
    (ng (report-ok-p report))
    (ok (eq :timeout (getf entry :status)))))

(deftest runner-keeps-a-cleanup-failure-apart-from-stderr-noise
  ;; Ordinary stderr output comes first, then the body fails, then the
  ;; fixture's cleanup fails too.  The report must keep the body's failure as
  ;; the result and the cleanup failure in full, not just the first stderr line.
  (setf *strays* '())
  (let* ((report (%run '((:property fixture-fails-and-cannot-clean-up))))
         (entry (%only-entry report))
         (cleanup (find-if (lambda (row) (search "READ-FIXTURE-CLEANUP-WARNING"
                                                 (getf row :type)))
                           (getf entry :warnings))))
    (unwind-protect
         (progn
           (ng (report-ok-p report))
           (ok (eq :error (getf entry :status)) "the body's own failure is the result")
           (ok (search "body failed on purpose" (getf (getf entry :condition) :report)))
           (ok (search "noise before the failure" (getf entry :error-output-sample))
               "ordinary stderr output is still counted and sampled")
           (ok cleanup "the cleanup failure is kept as a structured warning")
           (ok (and cleanup (= 1 (getf cleanup :count))))
           (dolist (stray *strays*)
             (let ((project (subseq stray 0 (- (length stray) (length "stray.txt")))))
               (ok (and cleanup (some (lambda (text) (search project text))
                                      (getf cleanup :reports)))
                   "naming the directory the cleanup could not remove"))))
      (dolist (stray *strays*)
        (let ((project (subseq stray 0 (- (length stray) (length "stray.txt")))))
          (sb-posix:unlink stray)
          (sb-posix:rmdir project)
          (sb-posix:rmdir (subseq project 0 (- (length project) (length "project/")))))))))

(deftest judge-entry-defaults-to-failure
  (testing "any status but :PASSED is a problem"
    (dolist (status '(:failed :error :skipped :pending :something-new nil))
      (ok (judge-entry (list :kind :property :status status :trials 5 :rejected 0))
          (format nil "~S" status))))
  (testing ":PASSED still needs measured, effective trials"
    (ok (judge-entry '(:kind :property :status :passed :trials nil :rejected 0)))
    (ok (judge-entry '(:kind :property :status :passed :trials 0 :rejected 0)))
    (ok (judge-entry '(:kind :property :status :passed :trials 5 :rejected nil)))
    (ok (judge-entry '(:kind :property :status :passed :trials 5 :rejected 5)))
    (ok (judge-entry '(:kind :function-spec :status :passed :trials 5 :rejected -1))
        "a negative rejection count is not a measurement")
    (ok (judge-entry '(:kind :function-spec :status :passed :trials 5 :rejected 6))
        "nor is one above the trial count")
    (ok (null (judge-entry '(:kind :property :status :passed :trials 5 :rejected 0)))))
  (testing "a contract with cases needs a measured case report"
    (ok (judge-entry '(:kind :function-spec :status :passed :trials 5 :rejected 0
                       :declared-cases (:a) :case-report :not-collected)))))

(deftest judge-example-checks-status-and-case
  (ok (null (judge-example '(:status :passed :case :text :expected-case :text))))
  (ok (judge-example '(:status :passed :case :absent :expected-case :text)))
  (ok (judge-example '(:status :failed)))
  (ok (judge-example '(:status :no-observation))))

(deftest reports-print-and-read-back
  (let ((report (list* :mode :check
                       :environment '(:lisp "test")
                       :sources '()
                       (%run '((:property fixture-small))))))
    (testing "a failing report prints its verdict, problem and replay line"
      (let ((text (with-output-to-string (out) (print-report report out))))
        (ok (search "Result: FAILED" text))
        (ok (search ":STATUS :FAILED" text))
        (ok (search "replay (MCP):  spec-check property=" text))
        (ok (search "seed=\"11\"" text) "the MCP seed is a decimal string")))
    (testing "the report file is one form that reads back without the bundle's packages"
      (uiop:with-temporary-file (:pathname pathname :type "sexp")
        (write-report report pathname)
        (let ((form (with-open-file (in pathname :external-format :utf-8)
                      (with-standard-io-syntax
                        (let ((*read-eval* nil))
                          (read in))))))
          (ok (eq :check (getf form :mode)))
          (ok (stringp (getf (first (getf form :entries)) :name))))))))

(deftest git-state-tells-worktrees-apart
  (let ((directory (uiop:ensure-directory-pathname
                    (uiop:merge-pathnames* (format nil "cl-mcp-git-state-~D-~D/"
                                                   (get-universal-time) (random 100000))
                                           (uiop:temporary-directory)))))
    (flet ((git (&rest arguments)
             (uiop:run-program (list* "git" "-C" (uiop:native-namestring directory)
                                      "-c" "user.name=test" "-c" "user.email=test@example.com"
                                      "-c" "commit.gpgsign=false" arguments)
                               :output nil :error-output nil))
           (spit (name text)
             (with-open-file (out (merge-pathnames name directory)
                                  :direction :output :if-exists :supersede)
               (write-string text out))))
      (unwind-protect
           (progn
             (ensure-directories-exist directory)
             (git "init" "--quiet")
             (spit "a.lisp" "(a)")
             (spit "notes.txt" "not lisp")
             (git "add" "a.lisp" "notes.txt")
             (git "commit" "--quiet" "-m" "base")
             (testing "a clean tree has no Lisp changes and no fingerprint"
               (let ((state (git-state directory)))
                 (ok (null (getf state :lisp-changes)))
                 (ok (null (getf state :lisp-fingerprint)))))
             (spit "a.lisp" "(b)")
             (let ((first (git-state directory)))
               (spit "a.lisp" "(c)")
               (let ((second (git-state directory)))
                 (testing "one HEAD, one status line, two different contents"
                   (ok (equal (getf first :revision) (getf second :revision)))
                   (ok (equal (getf first :changes) (getf second :changes)))
                   (ok (equal '((" M" "a.lisp"))
                              (mapcar #'butlast (getf second :lisp-changes))))
                   (ng (equal (getf first :lisp-fingerprint)
                              (getf second :lisp-fingerprint))))))
             (spit "b.lisp" "(new)")
             (spit "notes.txt" "changed, but not Lisp")
             (testing "untracked Lisp files count; other files do not"
               (ok (equal '((" M" "a.lisp") ("??" "b.lisp"))
                          (mapcar #'butlast (getf (git-state directory) :lisp-changes))))))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

;;; ------------------------------------------------------------------------
;;; The bundle itself

(deftest bundle-run-passes-and-checks-where-cl-mcp-came-from
  (let ((root (asdf:system-source-directory "cl-mcp")))
    (testing "every target and example of the bundle passes under one seed"
      (let ((report (run-bundle :seeds '(3) :trials 50 :expected-root root)))
        (ok (report-ok-p report) (format nil "~S" (getf report :problems)))
        (ok (= (length (bundle-targets)) (length (getf report :entries))))
        (ok (plusp (length (getf report :examples))))))
    (testing "a cl-mcp loaded from another directory fails the run"
      (let ((report (run-bundle :seeds '(3) :trials 5
                                :expected-root (uiop:temporary-directory))))
        (ng (report-ok-p report))
        (ok (find :cl-mcp-loaded-from (getf report :problems) :key #'first))))))
