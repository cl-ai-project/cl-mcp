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

(defun %cl-spec-available-p ()
  "Return true when cl-spec/check-it can be loaded into this image."
  (handler-case
      (progn
        (unless (find-package "CL-SPEC")
          (let ((*standard-output* (make-broadcast-stream))
                (*error-output* (make-broadcast-stream)))
            (asdf:load-system "cl-spec/check-it")))
        (and (find-package "CL-SPEC")
             (symbol-value (find-symbol "*GENERATOR-BACKEND*" "CL-SPEC"))
             t))
    (error () nil)))

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
  (if (not (%cl-spec-available-p))
      (skip +skip-reason+)
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
  (if (not (%cl-spec-available-p))
      (skip +skip-reason+)
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
              (ok (string= "requested" (gethash "budget_source"
                                                (gethash "trials" result))))
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
