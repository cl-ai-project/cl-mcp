;;;; tests/spec-integration-test.lisp
;;;;
;;;; The adapter against a real cl-spec: fetch the contract, take a real
;;;; counterexample, re-run from the seed, and see a redefinition reflected.
;;;;
;;;; Skipped, loudly, when cl-spec cannot be resolved.  cl-mcp does not depend
;;;; on cl-spec and its suite must stay green without it -- but a silent skip
;;;; would let this file rot unnoticed, so the skip says why.

(defpackage #:cl-mcp/tests/spec-integration-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:yason
                #:false)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-symbol-response
                #:spec-describe-response
                #:spec-check-response
                #:spec-list-response)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht))

(in-package #:cl-mcp/tests/spec-integration-test)

(defvar *fixture-registry* nil
  "The registry the fixture definitions were registered in, once loaded.")

(defparameter +skip-reason+
  "cl-spec/check-it could not be loaded in this image. The adapter's
cl-spec-facing behaviour is covered by tests/spec-adapter-report-test.lisp
with stub API handles; this file needs the real system."
  "Printed instead of running, so an absent cl-spec is visible rather than silent.")

(defvar *backend-load-attempted* nil
  "Set once the cl-spec/check-it load has been tried in this image.")

(defun %backend-installed-p ()
  "Return true when CL-SPEC is present with a generator backend installed.

The backend, not the package: cl-spec loads without one, and introspection
works while nothing can be executed.  This file runs properties, so the
package alone is not the question it needs answered."
  (let ((package (find-package "CL-SPEC")))
    (when package
      (let ((backend (find-symbol "*GENERATOR-BACKEND*" "CL-SPEC")))
        (and backend (boundp backend) (symbol-value backend) t)))))

(defun %cl-spec-available-p ()
  "Return true when cl-spec/check-it is usable in this image.

The load is attempted whenever the backend is missing, not only when the
CL-SPEC package is absent.  Gated on the package, an image that had loaded
plain cl-spec -- no backend -- never reached the load, every test in this file
skipped, and the suite reported itself green: the file's own coverage depended
on which system happened to be quickloaded first, and a claim that these tests
ran was a claim about load order rather than about the code.

Called once per test and cheap after the first: ASDF answers a loaded system
without recompiling, and the answer here is the backend, which either got
installed or did not."
  (case *backend-load-attempted*
    ((:done) (%backend-installed-p))
    (t
     (handler-case
         (progn
           (unless (%backend-installed-p)
             (let ((*standard-output* (make-broadcast-stream))
                   (*error-output* (make-broadcast-stream)))
               (asdf:load-system "cl-spec/check-it")))
           (setf *backend-load-attempted* :done)
           (%backend-installed-p))
       (error ()
         ;; Remembered, so a machine without cl-spec/check-it pays one failed
         ;; ASDF resolution rather than one per test.  The success path is
         ;; cheap on its own; this is the path the rewrite was for.
         (setf *backend-load-attempted* :done)
         nil)))))

(defparameter +no-contracts-reason+
  "the cl-spec in this image predates function specs: it exports no
function-spec-data, so there is no contract to describe or run. The adapter's
own answer to that -- an unsupported status naming the missing API -- is
covered by tests/spec-adapter-report-test.lisp."
  "Printed instead of running the contract tests against a cl-spec that has none.")

(defun %contracts-available-p ()
  "Return true when the fixture registered its contracts in this image.

Asks the fixture rather than carrying a second copy of the discriminator, and
asks it the question these tests need answered: not whether this cl-spec has
the function-spec API, but whether the definitions are in the registry.  The
two differ when DEFSPEC-FUNCTION exists and signals -- the fixture reports
that and keeps its other half -- and a predicate that only checked the API
would then run contract tests against an empty registry and blame the adapter.

Loading the fixture first is safe whatever the answer: the contract half lives
in a file of its own precisely so that an older cl-spec leaves it unread."
  (and (%cl-spec-available-p)
       (progn
         (%ensure-fixture)
         ;; Asked about the shared registry by name.  These tests read the
         ;; answer outside WITH-FIXTURE-REGISTRY, where the installed registry
         ;; is the global one, and another test loads a private copy of the
         ;; fixture -- so the question has to name which registry it is about.
         (let ((registered (find-symbol "CONTRACTS-REGISTERED-P"
                                        "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE")))
           (and registered
                (fboundp registered)
                (funcall registered *fixture-registry*)
                t)))))

(defun %registry-symbol ()
  "Return the CL-SPEC:*REGISTRY* symbol."
  (find-symbol "*REGISTRY*" "CL-SPEC"))

(defun %load-fixture ()
  "Load the fixture into a registry of its own and return that registry.

The global registry is swapped rather than rebound: the run thread a deadline
spawns does not inherit dynamic bindings, and the adapter reads the registry
on the calling thread precisely so it can hand it across.  Swapping keeps the
two paths agreeing whichever one a test exercises."
  (let* ((registry-symbol (%registry-symbol))
         (make (find-symbol "MAKE-HASH-TABLE-REGISTRY" "CL-SPEC"))
         (previous (symbol-value registry-symbol))
         (fresh (funcall make)))
    (setf (symbol-value registry-symbol) fresh)
    (unwind-protect
         (load (merge-pathnames "tests/fixtures/spec-fixture.lisp"
                                (asdf:system-source-directory "cl-mcp")))
      (setf (symbol-value registry-symbol) previous))
    fresh))

(defun %ensure-fixture ()
  "Load the shared fixture registry, once."
  (unless *fixture-registry*
    (setf *fixture-registry* (%load-fixture))))

(defmacro with-fixture-registry (&body body)
  "Run BODY with the fixture's registry installed as CL-SPEC:*REGISTRY*."
  `(let* ((registry-symbol (%registry-symbol))
          (previous (symbol-value registry-symbol)))
     (setf (symbol-value registry-symbol) *fixture-registry*)
     (unwind-protect (progn ,@body)
       (setf (symbol-value registry-symbol) previous))))

(defun %fixture-name (name)
  "Return the qualified designator for a fixture symbol named NAME."
  (format nil "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE::~A" name))

(defun %first-result (response)
  "Return the first per-property result of a spec-check RESPONSE."
  (aref (gethash "results" response) 0))

(defun %text (response)
  "Return the text an MCP client would show for RESPONSE.

Asserted on rather than only the payload: a client that renders content[].text
sees this and nothing else, so a fact that reaches the payload alone has not
reached the caller."
  (let ((content (gethash "content" response)))
    (if (and content (plusp (length content)))
        (gethash "text" (aref content 0))
        "")))

(defun %ok-core-record-read-whole (result)
  "Assert RESULT's core_result was read whole, against a real cl-spec record.

Two questions no scenario in this file asked.  UNKNOWN_KEYS names every key
the record carries that no descriptor covers -- a key whose value is dropped
rather than projected -- and PROJECTION.COMPLETE says whether the projection
reached the end of the record without cutting or stumbling on a shape it did
not predict.

Both instruments existed and neither was pointed at a real run.  The one
UNKNOWN-KEYS assertion in the suite ran the describe path against a
hand-written stub, which by construction carries exactly the keys the
descriptor declares, so a descriptor that did not match what cl-spec actually
builds passed every test there was.  Called from each scenario below, this is
what would have caught a signature guard matching a form whose third element
is not failure data, and an explanation key landing in UNKNOWN_KEYS with its
value discarded."
  (let ((core (gethash "core_result" result)))
    (ok (hash-table-p core))
    (when core
      (ok (equalp #() (gethash "unknown_keys" core)))
      (ok (eq t (gethash "complete" (gethash "projection" core))))
      (ok (equalp #() (gethash "issues" (gethash "projection" core)))))))

(deftest real-function-core-schema-survives-check
  (if (or (not (%contracts-available-p))
          (not (find-symbol "SCHEMA-INFO" "CL-SPEC")))
      (skip "This integration check needs the versioned Function Spec schema.")
      (with-fixture-registry
        (let* ((name (%fixture-name "WIDEN"))
               (description (spec-describe-response
                             (make-ht "kind" "function-spec" "name" name)))
               (definition (gethash "core_schema" description))
               (checked (spec-check-response
                         (make-ht "function" name "seed" "42" "trials" 1)))
               (result (%first-result checked))
               (metadata (gethash "core_schema" result)))
          (ok (hash-table-p definition))
          (ok (hash-table-p metadata))
          (when (and definition metadata)
            (ok (equal "function-spec" (gethash "entity_kind" metadata)))
            (ok (equal "result" (gethash "record_kind" metadata)))
            (ok (equal (gethash "definition_digest" definition)
                       (gethash "definition_digest" metadata))))
          (ok (equal (gethash "definition_digest" description)
                     (gethash "definition_digest" result)))))))

(deftest real-core-schema-survives-describe-and-check
  (if (or (not (%cl-spec-available-p))
          (not (find-symbol "SCHEMA-INFO" "CL-SPEC")))
      (skip "This integration check needs cl-spec's versioned core schema.")
      (progn
        (%ensure-fixture)
        (with-fixture-registry
          (let* ((name (%fixture-name "CLAMP-IS-IDEMPOTENT"))
                 (description (spec-describe-response
                               (make-ht "kind" "property" "name" name)))
                 (definition (gethash "core_schema" description))
                 (checked (spec-check-response (make-ht "property" name "seed" "42")))
                 (result (%first-result checked))
                 (metadata (gethash "core_schema" result)))
            (ok (hash-table-p definition))
            (ok (hash-table-p metadata))
            (when (and definition metadata)
              (ok (equal "definition" (gethash "record_kind" definition)))
              (ok (equal "result" (gethash "record_kind" metadata)))
              (ok (equal "property" (gethash "entity_kind" metadata)))
              (ok (equal (gethash "definition_digest" definition)
                         (gethash "definition_digest" metadata))))
            (ok (equal (gethash "definition_digest" description)
                       (gethash "definition_digest" result))))
          (let* ((description (spec-describe-response
                               (make-ht "kind" "spec" "name" (%fixture-name "SMALL-INT"))))
                 (metadata (gethash "core_schema" description)))
            (ok (hash-table-p metadata))
            (when metadata
              (ok (equal "spec" (gethash "entity_kind" metadata)))))))))

(deftest cl-spec-adapter-discovers-and-describes
  (if (not (%cl-spec-available-p))
      (skip +skip-reason+)
      (progn
        (%ensure-fixture)
        (with-fixture-registry
          (testing "discovery finds the properties registered about CLAMP"
            (let* ((response (spec-symbol-response
                              (make-ht "symbol" (%fixture-name "CLAMP"))))
                   (properties (gethash "properties" response)))
              (ok (string= "ok" (gethash "status" response)))
              (ok (= 3 (length properties)))
              (ok (string= "CLAMP" (gethash "name" (gethash "symbol" response))))
              (testing "the runtime join carries the signature"
                (ok (search "VALUE"
                            (string-upcase
                             (or (gethash "arglist" (gethash "runtime" response))
                                 "")))))
              (testing "and each body is omitted with a pointer to the detail"
                (ok (every (lambda (property)
                             (eq t (gethash "body_omitted" property)))
                           properties)))))

          (testing "detail returns the body the listing omitted"
            (let ((response (spec-describe-response
                             (make-ht "kind" "property"
                                      "name" (%fixture-name "CLAMP-IS-IDEMPOTENT")))))
              (ok (string= "ok" (gethash "status" response)))
              (ok (search "CLAMP" (string-upcase (gethash "body" response))))
              (ok (eq t (gethash "body_complete" response)))))))))

(deftest cl-spec-adapter-runs-and-replays
  (if (not (%cl-spec-available-p))
      (skip +skip-reason+)
      (progn
        (%ensure-fixture)
        (with-fixture-registry
          (testing "a true property passes and is verified"
            (let ((response (spec-check-response
                             (make-ht "property"
                                      (%fixture-name "CLAMP-IS-WITHIN-BOUNDS")))))
              (ok (string= "completed" (gethash "status" response)))
              (ok (eq t (gethash "verified" response)))
              (let ((result (%first-result response)))
                (ok (string= "passed" (gethash "status" result)))
                (ok (= 100 (gethash "budget" (gethash "trials" result)))))))

          (let (seed digest)
            (testing "a false property yields a real counterexample"
              (let* ((response (spec-check-response
                                (make-ht "property"
                                         (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE"))))
                     (result (%first-result response)))
                (ok (string= "completed" (gethash "status" response)))
                (ok (eq yason:false (gethash "verified" response)))
                (ok (string= "failed" (gethash "status" result)))
                (ok (plusp (length (gethash "counterexample" result))))
                (setf seed (gethash "seed" result)
                      digest (gethash "definition_digest" result))
                (testing "and the seed is text rather than a JSON number"
                  (ok (stringp seed))
                  (ok (every #'digit-char-p seed)))))

            (testing "the same seed reproduces the same counterexample"
              (let* ((response (spec-check-response
                                (make-ht "property"
                                         (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE")
                                         "seed" seed
                                         "profile" "normal"
                                         "expect_definition_digest" digest)))
                     (result (%first-result response)))
                (ok (string= "failed" (gethash "status" result)))
                (ok (string= seed (gethash "seed" result)))
                (ok (string= "match" (gethash "definition_match" result)))
                (ok (string= "faithful"
                             (gethash "reproduction_faithful" response)))))

            (testing "a digest from a different definition is reported, not ignored"
              (let ((response (spec-check-response
                               (make-ht "property"
                                        (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE")
                                        "seed" seed
                                        "profile" "normal"
                                        "expect_definition_digest" "0000000000000000"))))
                (ok (string= "unfaithful"
                             (gethash "reproduction_faithful" response)))
                (ok (string= "mismatch"
                             (gethash "definition_match"
                                      (%first-result response)))))))

          (testing "selecting by symbol runs all three and is not verified"
            (let ((response (spec-check-response
                             (make-ht "symbol" (%fixture-name "CLAMP")))))
              (ok (= 3 (gethash "count" (gethash "selection" response))))
              (ok (eq yason:false (gethash "verified" response)))
              (ok (= 1 (gethash "failed" (gethash "counts" response))))
              (ok (= 2 (gethash "passed" (gethash "counts" response))))))

          (testing "a symbol with nothing registered is not a clean bill"
            (let ((response (spec-check-response (make-ht "symbol" "cl:car"))))
              (ok (string= "no-properties" (gethash "status" response)))
              (ok (eq yason:false (gethash "verified" response)))))))))

(deftest cl-spec-adapter-sees-a-redefinition
  (if (not (%cl-spec-available-p))
      (skip +skip-reason+)
      ;; A registry of its own, not the shared one.  This test replaces one of
      ;; the fixture's properties with a corrected version, and the shared
      ;; registry would carry that replacement into whichever test ran next --
      ;; which is what happens when the suite is run twice in one image: the
      ;; property written to fail passes, and four other tests fail with it.
      (let ((*fixture-registry* (%load-fixture)))
        (with-fixture-registry
          (testing "re-registering a property changes its digest and its verdict"
            (let* ((before (spec-check-response
                            (make-ht "property"
                                     (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE"))))
                   (before-result (%first-result before))
                   (before-digest (gethash "definition_digest" before-result)))
              (ok (string= "failed" (gethash "status" before-result)))
              ;; Re-register the property as a statement that holds, exactly
              ;; as editing the file and loading it again would.
              (funcall (find-symbol "REGISTER-CORRECTED-PROPERTY"
                                    "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
              (let* ((after (spec-check-response
                             (make-ht "property"
                                      (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE"))))
                     (after-result (%first-result after)))
                (ok (string= "passed" (gethash "status" after-result)))
                (ok (eq t (gethash "verified" after)))
                (testing "and the digest moved, so an old seed is not faithful"
                  (ok (not (string= before-digest
                                    (gethash "definition_digest" after-result))))))))))))

(deftest cl-spec-adapter-reads-a-contract
  (if (not (%contracts-available-p))
      (skip (if (%cl-spec-available-p) +no-contracts-reason+ +skip-reason+))
      (progn
        (%ensure-fixture)
        (with-fixture-registry
          (testing "spec-symbol says a contract exists and how to reach it"
            (let ((response (spec-symbol-response
                             (make-ht "symbol" (%fixture-name "CLAMP")))))
              (ok (string= "CLAMP"
                           (gethash "name"
                                    (gethash "function_spec"
                                             (gethash "registry" response)))))
              (ok (search "spec-describe kind=function-spec" (%text response)))))

          (testing "an open range end reads as the * the author wrote"
            ;; cl-spec spells it :UNBOUNDED internally. Printing the keyword
            ;; puts an IR detail in front of a reader who wrote * and will
            ;; write * again.
            (let ((text (%text (spec-describe-response
                                (make-ht "kind" "function-spec"
                                         "name" (%fixture-name "MAGNITUDE"))))))
              (ok (search "[0, *]" text))
              (ok (not (search "UNBOUNDED" text)))))

          (testing "spec-describe projects which inputs it takes and what it returns"
            (let* ((response (spec-describe-response
                              (make-ht "kind" "function-spec"
                                       "name" (%fixture-name "CLAMP"))))
                   (text (%text response)))
              (ok (string= "ok" (gethash "status" response)))
              (ok (= 3 (length (gethash "arguments" response))))
              (ok (gethash "returns" response))
              (ok (search "SMALL-INT" text))
              (testing "and the :pre and :post forms are shown, not only stored"
                (ok (search ":pre" text))
                (ok (search ":post" text))
                (ok (search "RESULT" text)))))))))

(deftest cl-spec-adapter-runs-a-contract
  (if (not (%contracts-available-p))
      (skip (if (%cl-spec-available-p) +no-contracts-reason+ +skip-reason+))
      (progn
        (%ensure-fixture)
        (with-fixture-registry
          (testing "a contract that holds is verified, with its rejections counted"
            (let* ((response (spec-check-response
                              (make-ht "function" (%fixture-name "CLAMP")
                                       "trials" 200)))
                   (result (%first-result response))
                   (contract (gethash "contract" result)))
              (ok (string= "completed" (gethash "status" response)))
              (ok (eq t (gethash "verified" response)))
              (ok (string= "contract" (gethash "kind" result)))
              (ok (= 200 (gethash "budget" (gethash "trials" result))))
              (testing "and the budget comes off cl-spec's own result"
                ;; Not the adapter's reconstruction: check-function records
                ;; the budget the run was given, so the derivation note does
                ;; not apply to a contract run.
                (ok (string= "cl-spec result"
                             (gethash "budget_source"
                                      (gethash "trials" result)))))
              (testing "the run says how many inputs :pre refused"
                (ok (eq t (gethash "rejected_measured" contract)))
                (ok (integerp (gethash "rejected" contract)))
                (ok (plusp (gethash "rejected" contract)))
                (ok (= (gethash "effective_trials" contract)
                       (- (gethash "executed" (gethash "trials" result))
                          (gethash "rejected" contract)))))
              (testing "and does not claim rejections are unmeasured"
                (ok (not (find "rejection-counts-unmeasured"
                               (gethash "verification_gaps" response)
                               :test #'string=))))))

          (testing "a contract that is broken names which half broke"
            (let* ((response (spec-check-response
                              (make-ht "function" (%fixture-name "WIDEN")
                                       "trials" 300)))
                   (result (%first-result response))
                   (contract (gethash "contract" result)))
              (ok (eq yason:false (gethash "verified" response)))
              (ok (string= "failed" (gethash "status" result)))
              (ok (plusp (length (gethash "counterexample" result))))
              (ok (string= "return-spec" (gethash "failure_reason" contract)))
              (testing "with cl-spec's account of the value that missed its spec"
                (ok (stringp (gethash "explanation" contract))))))

          (testing "a contract nothing could call is skipped, never verified"
            ;; The zero-count success: every generated input refused, so the
            ;; function was never called and there is nothing to have verified.
            (let* ((response (spec-check-response
                              (make-ht "function" (%fixture-name "NEVER-CALLABLE")
                                       "trials" 30)))
                   (result (%first-result response)))
              (ok (eq yason:false (gethash "verified" response)))
              (ok (string= "skipped" (gethash "status" result)))
              (ok (= 30 (gethash "rejected" (gethash "contract" result))))
              (ok (eql 0 (gethash "effective_trials" (gethash "contract" result))))))

          (testing "an :about selection does not quietly run the contract"
            (let* ((response (spec-check-response
                              (make-ht "symbol" (%fixture-name "CLAMP"))))
                   (text (%text response))
                   (headline (subseq text 0 (or (position #\Newline text)
                                                (length text)))))
              (ok (every (lambda (result) (string= "property" (gethash "kind" result)))
                         (gethash "results" response)))
              (testing "and the first line says so, where a reader who stops will see it"
                (ok (search "properties only" headline))
                (ok (search "NOT run" headline))
                (ok (string= "CLAMP"
                             (gethash "name"
                                      (gethash "contract_not_run"
                                               (gethash "selection" response))))))))

          (testing "trials without a contract is refused rather than ignored"
            (let ((response (spec-check-response
                             (make-ht "symbol" (%fixture-name "CLAMP")
                                      "trials" 10))))
              (ok (string= "invalid-arguments" (gethash "status" response)))
              (ok (search "function=" (%text response)))))

          (testing "the replay line for a contract is a call that would work"
            ;; Printed as spec-check property=<name>, following it verbatim
            ;; asks for a property that does not exist -- and without the
            ;; budget, a failure found at 300 trials need not reappear at the
            ;; backend default.
            (let* ((response (spec-check-response
                              (make-ht "function" (%fixture-name "WIDEN")
                                       "trials" 300)))
                   (text (%text response)))
              (ok (search "Replay: spec-check function=" text))
              (ok (not (search "Replay: spec-check property=" text)))
              (ok (search "trials=300" text))))

          (testing "the selection line calls a contract a contract"
            (let ((text (%text (spec-check-response
                                (make-ht "function" (%fixture-name "CLAMP")
                                         "trials" 20)))))
              (ok (search "Selected 1 contract" text))
              (ok (not (search "Selected 1 property" text)))))

          (testing "a symbol whose only registration is a contract still says so"
            ;; The caller asked about the symbol and got "nothing ran". If the
            ;; contract is not named here they have no reason to look further.
            (let* ((response (spec-check-response
                              (make-ht "symbol" (%fixture-name "NEVER-CALLABLE"))))
                   (text (%text response)))
              (ok (string= "no-properties" (gethash "status" response)))
              (ok (search "function spec is registered" text))
              (ok (search "function=" text))))

          (testing "spec-list enumerates contracts as their own kind"
            (let* ((response (spec-list-response (make-ht "kind" "function-specs")))
                   (text (%text response)))
              (ok (string= "ok" (gethash "status" response)))
              (ok (eq t (gethash "function_specs_listable" response)))
              ;; The count is what the registry holds, not a magic number:
              ;; asserted against the listing beside it so adding a fixture
              ;; contract does not make this a puzzle to re-derive.
              (ok (<= 4 (gethash "function_specs" (gethash "counts" response))))
              (ok (= (length (gethash "function_specs" response))
                     (gethash "function_specs" (gethash "counts" response))))
              (ok (search "function specs:" text))
              (ok (search "CLAMP" text))))))))

(deftest real-named-cases-report-the-one-never-reached
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                         "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
              (list '(10 2) '(20 5)))
        (let* ((response (spec-check-response
                          (make-ht "function" (%fixture-name "REMAINING-BALANCE")
                                   "trials" 2 "seed" "1")))
               (result (%first-result response))
               (record (gethash "data" (gethash "core_result" result)))
               (cases (gethash "case_report" record))
               (text (%text response)))
          (ok (equal "passed" (gethash "status" result)))
          (%ok-core-record-read-whole result)
          (testing "the report distinguishes reached from declared"
            (ok (= 2 (length (gethash "declared_cases" cases))))
            ;; EQUALP, not EQUAL: EQUAL compares general vectors by EQ, so it
            ;; never matches a freshly-consed one; EQUALP compares elements.
            (ok (equalp #("insufficient-funds") (gethash "never_called" cases))))
          (testing "and the verdict does not read as full coverage"
            ;; YASON:FALSE is a value, not a function -- (YASON:FALSE) is
            ;; undefined, and every other use in this file spells it bare.
            (ok (eq yason:false (gethash "verified" response)))
            (ok (find "cases-never-called"
                      (gethash "verification_gaps" response) :test #'equal))
            (ok (search "NEVER CALLED" text)))))))

(deftest real-state-post-failure-keeps-the-target-outcome
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (let ((purse (funcall (find-symbol "MAKE-PURSE"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE")
                              100 7)))
          (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
                (list (list purse 30)))
          (let* ((response (spec-check-response
                            (make-ht "function"
                                     (%fixture-name "WITHDRAW-WITHOUT-RECORDING!")
                                     "trials" 1 "seed" "1")))
                 (result (%first-result response))
                 (record (gethash "data" (gethash "core_result" result)))
                 (failure (gethash "failure" record))
                 (text (%text response)))
            (ok (equal "state-post" (gethash "failure_phase" record)))
            (ok (equal "state-postcondition" (gethash "failure_reason" record)))
            (%ok-core-record-read-whole result)
            (testing "the target returned normally and that is visible"
              (ok (equal "returned"
                         (gethash "kind" (gethash "outcome" failure)))))
            (testing "the captured pre-state survives"
              (ok (plusp (length (gethash "values"
                                          (gethash "capture"
                                                   (gethash "state" failure)))))))
            (testing "and state-post is reported as violated, not as a target bug"
              (ok (equal "violation"
                         (gethash "status" (gethash "state_post"
                                                    (gethash "state" failure)))))
              (ok (search "target WAS called" text))
              (ok (search "state-post: violation" text))))))))

(deftest real-case-selection-error-does-not-blame-the-target
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                         "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
              (list '(5 5)))
        (let* ((response (spec-check-response
                          (make-ht "function" (%fixture-name "OVERLAPPING-BALANCE")
                                   "trials" 1 "seed" "1")))
               (result (%first-result response))
               (record (gethash "data" (gethash "core_result" result)))
               (text (%text response)))
          (ok (equal "case-selection" (gethash "failure_phase" record)))
          (%ok-core-record-read-whole result)
          (testing "the target was never called"
            ;; Not a (:kind ...) object here: cl-spec's raw :OUTCOME on this
            ;; observation is the bare :NOT-COLLECTED keyword, and
            ;; PROJECT-RECORD's atom-for-container rule (spec-core-record.lisp)
            ;; projects an atom landing under an :object-shaped field as the
            ;; atom itself rather than inventing a {"kind": ...} wrapper --
            ;; confirmed against a real run before this assertion was written.
            (ok (equal "not-collected" (gethash "outcome" (gethash "failure" record)))))
          (testing "the structured selection evidence is preserved"
            (ok (eql 1 (gethash "case_selection_errors"
                                (gethash "case_report" record)))))
          (testing "and the text says which half broke"
            (ok (search "the target was NOT called" text))
            ;; A counterexample exists for this run, and it must not read as
            ;; an input the function failed on.  Asserted against what the
            ;; renderer can actually write: "the function failed" appears in
            ;; no format string under SRC/ and never could, so the old
            ;; spelling of this line could not fail.  The two lines
            ;; %FORMAT-CORE-EVIDENCE writes for a target that finished are
            ;; "target: returned" and "target: signalled", and this run has
            ;; neither -- the reason it prints "target: not called" instead.
            (ok (not (search "target: returned" text)))
            (ok (not (search "target: signalled" text)))
            ;; And the contract-error gloss, which is what says the finding
            ;; is about the contract rather than about the function.
            (ok (search "NOT about the function" text)))))))

(deftest real-generation-exhaustion-is-not-a-target-failure
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (let* ((response (spec-check-response
                          (make-ht "function"
                                   (%fixture-name "MAGNITUDE-OF-IMPOSSIBLE")
                                   "trials" 1 "seed" "1")))
               (result (%first-result response))
               (record (gethash "data" (gethash "core_result" result)))
               (generation (gethash "generation_report" record))
               (text (%text response)))
          (ok (gethash "termination" generation))
          (ok (equal "generation" (gethash "failure_phase" record)))
          (%ok-core-record-read-whole result)
          (ok (find "generation-incomplete"
                    (gethash "verification_gaps" response) :test #'equal))
          (ok (search "did NOT complete" text))))))

(deftest real-state-contract-says-why-it-was-not-shrunk
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (let ((purse (funcall (find-symbol "MAKE-PURSE"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE")
                              100 7)))
          (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
                (list (list purse 30)))
          (let* ((response (spec-check-response
                            (make-ht "function"
                                     (%fixture-name "WITHDRAW-WITHOUT-RECORDING!")
                                     "trials" 1 "seed" "1")))
                 (result (%first-result response))
                 (record (gethash "data" (gethash "core_result" result)))
                 (shrink (gethash "shrink_report" record))
                 (text (%text response)))
            (ok (equal "state-restoration-unavailable"
                       (gethash "termination" shrink)))
            (%ok-core-record-read-whole result)
            (testing "the original evidence is still there"
              (ok (gethash "failure" record)))
            (testing "and nothing claims a minimal counterexample"
              (ok (not (search "no smaller" text)))
              (ok (search "nothing restores" text))))))))

(deftest real-postcondition-failure-is-read-whole
  ;; The commonest contract failure there is, and the one no scenario here
  ;; covered.  WIDEN breaks its :RETURNS, which CLASSIFY-TARGET-OUTCOME
  ;; classifies before it ever calls the :POST
  ;; (cl-spec/src/function-spec.lisp), so every contract scenario in this file
  ;; exercised :RETURN-SPEC and none of them :POSTCONDITION.
  ;;
  ;; Three things this branch meets at once.  The signature is
  ;; (:RETURN-VALUE :POSTCONDITION <explanation>)
  ;; (cl-spec/src/function-spec.lisp:1335) -- three elements under the same
  ;; head as the :RETURN-SPEC form, whose third element IS failure-shape data
  ;; and whose third element here is not.  The explanation is
  ;; (:POST-FORM <index>) (function-spec.lisp:1437-1441), a key the shape
  ;; table did not declare.  And %FORMAT-CONTRACT's broken-half line names the
  ;; reason.
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (let* ((response (spec-check-response
                          (make-ht "function" (%fixture-name "GROW-BY-NOTHING")
                                   "trials" 5 "seed" "1")))
               (result (%first-result response))
               (record (gethash "data" (gethash "core_result" result)))
               (failure (gethash "failure" record))
               (text (%text response)))
          (ok (equal "failed" (gethash "status" result)))
          (ok (equal "postcondition" (gethash "failure_reason" record)))
          (testing "the signature keeps cl-spec's grammar, leading tag first"
            (let ((signature (gethash "signature" failure)))
              (ok (= 3 (length signature)))
              (ok (equal "return-value" (aref signature 0)))
              (ok (equal "postcondition" (aref signature 1)))))
          (testing "and the explanation names which :post form did not hold"
            ;; Undeclared, this key landed in unknown_keys and its value was
            ;; dropped -- which is the whole explanation for this failure.
            (ok (eql 0 (gethash "post_form" (gethash "explanation" failure)))))
          (%ok-core-record-read-whole result)
          (testing "and the text names the half that broke"
            (ok (search "broken half: postcondition" text)))))))

(defun %capture-tagged-union-p ()
  "Return true when this cl-spec's SCHEMA-INFO declares the v1 capture union.

The guard that keeps this file honest: a cl-spec checkout whose
state.capture.values is still the pre-release (NAME . VALUE) alist would let
the capture assertions below pass against a shape cl-mcp no longer reads, so
the test skips loudly instead of reporting a green it did not earn."
  (let ((schema-info (find-symbol "SCHEMA-INFO" "CL-SPEC")))
    (and schema-info
         (fboundp schema-info)
         (let ((info (funcall schema-info)))
           (and (equal '(:collected :unavailable)
                       (getf info :capture-value-states))
                (member :anonymous-class
                        (getf info :capture-value-type-forms)))))))

(defun %capture-entry (values-array name)
  "Return the capture-value entry named NAME, or NIL."
  (loop for entry across values-array
        when (equal name (gethash "name" (gethash "name" entry)))
          return entry))

(deftest real-capture-availability-records-survive-both-branches
  ;; cl-spec PR #34 (merge 08d3ada): a captured binding is an explicit
  ;; availability record, so cl-mcp performs no shape-based recognition.  This
  ;; is the upstream-collision regression the original P1 finding asked for: a
  ;; legal application value shaped like the old opaque marker must stay
  ;; :COLLECTED, and a genuinely unfreezable value must be :UNAVAILABLE with no
  ;; value and no object id.
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (if (not (%capture-tagged-union-p))
          (skip "This cl-spec predates the v1 capture-value tagged union (PR #34).")
          (with-fixture-registry
            (let* ((response (spec-check-response
                              (make-ht "function"
                                       (%fixture-name "DIAGNOSTIC-CAPTURE")
                                       "trials" 1 "seed" "1")))
                   (result (%first-result response))
                   (record (gethash "data" (gethash "core_result" result)))
                   (failure (gethash "failure" record))
                   (values-array (gethash "values"
                                          (gethash "capture"
                                                   (gethash "state" failure))))
                   (text (%text response))
                   (marker (%capture-entry values-array "DIAGNOSTIC-BEFORE"))
                   (opaque (%capture-entry values-array "TABLE-BEFORE")))
              (ok (equal "failed" (gethash "status" result)))
              (ok (equal "state-post" (gethash "failure_phase" record)))
              (%ok-core-record-read-whole result)
              (testing "the availability records themselves are the v1 union"
                (ok (hash-table-p marker))
                (ok (hash-table-p opaque)))
              (testing "the old-marker-shaped value is collected application data"
                (ok (equal "collected" (gethash "availability" marker)))
                (ok (hash-table-p (gethash "value" marker)))
                (ok (search "UNAVAILABLE"
                            (gethash "printed" (gethash "value" marker))))
                (ok (integerp (gethash "object_id" (gethash "value" marker))))
                ;; The unavailable-only key must not appear.
                (ok (null (gethash "reason" marker))))
              (testing "the hash table is structurally unavailable"
                (ok (equal "unavailable" (gethash "availability" opaque)))
                (ok (equal "opaque-value" (gethash "reason" opaque)))
                ;; :TYPE is ordinary data: a named type symbol here.
                (ok (equal "HASH-TABLE"
                           (gethash "name" (gethash "type" opaque))))
                (ok (null (gethash "value" opaque))))
              (testing "and the text tells the same story"
                (ok (search "DIAGNOSTIC-BEFORE = " text))
                (ok (search "UNAVAILABLE" text))
                (ok (search "TABLE-BEFORE = UNAVAILABLE -- opaque-value (type HASH-TABLE)"
                            text))))))))
