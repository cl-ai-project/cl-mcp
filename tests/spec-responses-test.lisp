;;;; tests/spec-responses-test.lisp
;;;;
;;;; Fixed cases for the four cl-spec response builders, checked through the
;;;; JSON a client receives and the text an MCP client renders.  They sit
;;;; beside the generated properties of specs/spec-responses.lisp, need no
;;;; cl-spec, and run in the default suite.
;;;;
;;;; tests/spec-response-builders-test.lisp reads the builders' hash-tables.
;;;; This file reads the document: in a hash-table YASON:FALSE and NIL are
;;;; both objects, and only after encoding is one of them false and the other
;;;; null.

(defpackage #:cl-mcp/tests/spec-responses-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/spec-response-builders
                #:build-spec-list-response
                #:build-spec-symbol-response
                #:build-spec-describe-response
                #:build-spec-check-response)
  ;; A bare :import-from declares the dependency without importing a symbol.
  ;; The documented status and gap sets are internal parameters, named in full
  ;; where they are read.
  (:import-from #:cl-mcp/src/spec-adapter-report)
  (:import-from #:cl-mcp/specs/spec-response-fixtures
                #:response-json
                #:parse-response
                #:json-at
                #:json-kind
                #:json-array-p
                #:json-object-p
                #:json-true-p
                #:json-false-p
                #:json-null-p
                #:json-text
                #:first-line
                #:line-starting-with
                #:claims-p
                #:qualified
                #:+check-cases+
                #:+check-robustness-cases+
                #:list-scenario
                #:symbol-scenario
                #:describe-scenario
                #:check-scenario))

(in-package #:cl-mcp/tests/spec-responses-test)

(defun %document (response)
  "Return RESPONSE as a client reads it."
  (parse-response (response-json response)))

(defun %list-document (state &key (limit 50))
  "Return the document a spec-list answer in STATE produces."
  (%document (build-spec-list-response (list-scenario state :limit limit))))

(defun %check (case)
  "Return (values DOCUMENT TEXT) for the spec-check answer CASE."
  (let ((response (build-spec-check-response (check-scenario case))))
    (values (%document response) (json-text response))))

;;; ------------------------------------------------------------------------
;;; A. The JSON layer itself

(deftest the-decoder-keeps-five-answers-apart
  ;; A guard on the helper every other test here reads through.  Under the
  ;; server's own settings -- YASON:PARSE with no arguments -- false and null
  ;; are both NIL, and a check written on top of that cannot fail.
  (let ((document (parse-response
                   "{\"f\":false,\"t\":true,\"n\":null,\"a\":[],\"s\":\"\",\"z\":0}")))
    (ok (json-false-p (json-at document "f")))
    (ok (json-true-p (json-at document "t")))
    (ok (json-null-p (json-at document "n")))
    (ok (json-array-p (json-at document "a")))
    (ok (equal "" (json-at document "s")))
    (ok (eql 0 (json-at document "z")))
    (testing "and an absent key is absent, not null"
      (ok (not (nth-value 1 (json-at document "missing"))))
      (ok (nth-value 1 (json-at document "n"))))
    (testing "which json-kind reports as seven answers"
      (ok (equal '(:boolean :boolean :null :array :string :number :absent)
                 (loop for key in '("f" "t" "n" "a" "s" "z" "missing")
                       collect (multiple-value-bind (value present)
                                   (json-at document key)
                                 (json-kind value present))))))
    (testing "an empty array is not an empty string"
      (ok (json-array-p (json-at document "a")))
      (ok (not (json-array-p (json-at document "s"))))
      ;; The reason JSON-ARRAY-P exists rather than VECTORP or a length
      ;; comparison: a string is a vector, and EQUALP says an empty one and an
      ;; empty array are the same object.
      (ok (vectorp (json-at document "s")) "a string is a vector")
      (ok (equalp (json-at document "a") (json-at document "s"))
          "and EQUALP cannot tell the two apart"))
    (testing "and a false is not a Lisp falsehood"
      ;; The trap this helper exists for: YASON:FALSE is a symbol, so a bare
      ;; (when value ...) treats a JSON false as a JSON true.
      (ok (json-at document "f") "non-NIL in Lisp")
      (ok (not (json-true-p (json-at document "f")))))))

(deftest two-names-in-two-packages-stay-two-registrations
  (let* ((document (%list-document :present))
         (entries (json-at document "properties")))
    (ok (eql 2 (length entries)))
    (ok (equal (list (qualified :property) (qualified :elsewhere))
               (loop for entry across entries
                     collect (json-at entry "name" "qualified"))))
    (testing "each carrying its package and its name apart"
      (ok (equal "PROBE" (json-at (aref entries 0) "name" "package")))
      (ok (equal "PROBE-ELSEWHERE" (json-at (aref entries 1) "name" "package")))
      (ok (equal (json-at (aref entries 0) "name" "name")
                 (json-at (aref entries 1) "name" "name"))
          "the same symbol name in both"))))

;;; ------------------------------------------------------------------------
;;; B. spec-list

(deftest a-count-nobody-took-is-null-and-not-zero
  (testing "a kind that was asked for and holds none counts zero"
    (let ((document (%list-document :none-registered)))
      (ok (eql 0 (json-at document "counts" "properties")))
      (ok (json-array-p (json-at document "properties")))
      (ok (zerop (length (json-at document "properties"))))))
  (dolist (state '(:not-requested :unlistable :tag-unfilterable))
    (testing (format nil "~(~A~) has no count at all" state)
      (let ((document (%list-document state)))
        (multiple-value-bind (value present) (json-at document "counts" "properties")
          (ok present "the key is there")
          (ok (json-null-p value) "and its value is null"))))))

(deftest a-kind-that-was-not-counted-prints-no-zero
  (let* ((response (build-spec-list-response (list-scenario :not-requested)))
         (text (json-text response)))
    (ok (not (claims-p text "0 propert")))
    (ok (not (claims-p text "0 properties")))))

(deftest a-limit-cuts-the-list-and-leaves-the-count
  (let ((document (%list-document :present :limit 1)))
    (ok (eql 2 (json-at document "counts" "properties")) "counted before the limit")
    (ok (eql 1 (length (json-at document "properties"))))
    (ok (json-true-p (json-at document "truncated")))
    (testing "and at a limit that cuts nothing, truncated is false, not null"
      (let ((whole (%list-document :present :limit 50)))
        (ok (json-false-p (json-at whole "truncated")))
        (ok (eql 2 (length (json-at whole "properties"))))))))

(deftest a-tag-that-could-not-be-applied-says-so
  (let* ((response (build-spec-list-response (list-scenario :tag-unfilterable)))
         (document (%document response))
         (text (json-text response)))
    (ok (json-false-p (json-at document "tag_filterable")))
    (ok (json-false-p (json-at document "filters" "tag_applied")))
    (ok (claims-p text "was NOT applied"))
    (ok (not (claims-p text "0 propert")) "and does not report an empty result")))

;;; ------------------------------------------------------------------------
;;; C. spec-symbol

(deftest nothing-registered-is-not-said-about-a-lookup-that-failed
  (testing "a lookup that worked and found nothing says so"
    (let* ((response (build-spec-symbol-response (symbol-scenario :nothing-registered)))
           (document (%document response)))
      (ok (json-true-p (json-at document "nothing_registered")))
      (ok (claims-p (json-text response) "Nothing is registered"))))
  (dolist (state '(:not-loaded :internal-error))
    (testing (format nil "~(~A~) carries no such claim" state)
      (let* ((response (build-spec-symbol-response (symbol-scenario state)))
             (document (%document response)))
        (ok (not (nth-value 1 (json-at document "nothing_registered"))))
        (ok (not (equal "ok" (json-at document "status"))))
        (ok (not (claims-p (json-text response) "Nothing is registered")))))))

(deftest a-runtime-that-was-not-read-says-why
  (let* ((response (build-spec-symbol-response (symbol-scenario :runtime-not-read)))
         (document (%document response))
         (text (json-text response)))
    (ok (json-null-p (json-at document "runtime")))
    (ok (stringp (json-at document "runtime_unavailable_reason")))
    (ok (claims-p text "runtime information unavailable"))
    (testing "while a symbol that was read carries its definition"
      (let ((read (%document (build-spec-symbol-response
                              (symbol-scenario :all-three)))))
        (ok (json-object-p (json-at read "runtime")))
        (ok (equal "function" (json-at read "runtime" "type")))))))

(deftest a-registration-that-is-absent-is-null-and-not-an-empty-object
  (let ((document (%document (build-spec-symbol-response
                              (symbol-scenario :spec-only)))))
    (ok (json-object-p (json-at document "registry" "spec")))
    (ok (json-null-p (json-at document "registry" "function_spec")))
    (ok (json-null-p (json-at document "registry" "property")))
    (ok (json-array-p (json-at document "registry" "properties_about")))))

;;; ------------------------------------------------------------------------
;;; D. spec-describe

(deftest a-clause-has-three-states-and-json-carries-all-three
  (testing "a clause that is there and whole"
    (let ((document (%document (build-spec-describe-response
                                (describe-scenario :contract :pre :whole)))))
      (ok (equal "(> AMOUNT 0)" (json-at document "preconditions")))
      (ok (json-true-p (json-at document "preconditions_complete")))
      (ok (eql 0 (json-at document "preconditions_omitted_chars")))))
  (testing "a clause whose text is NIL is still a clause"
    (let ((document (%document (build-spec-describe-response
                                (describe-scenario :contract :pre :present-nil)))))
      (ok (equal "NIL" (json-at document "preconditions")))
      (ok (json-true-p (json-at document "preconditions_complete")))))
  (testing "a definition with no such clause is null, never false"
    (let ((document (%document (build-spec-describe-response
                                (describe-scenario :contract :pre :absent)))))
      (ok (json-null-p (json-at document "preconditions")))
      (ok (json-null-p (json-at document "preconditions_complete"))
          "false would say the clause is there and was cut")))
  (testing "and a clause that was cut says how much is missing"
    (let* ((response (build-spec-describe-response
                      (describe-scenario :contract :pre :cut)))
           (document (%document response))
           (text (json-text response)))
      (ok (equal "(AND (> AMOUNT 0) (<= AMOUNT BALA"
                 (json-at document "preconditions")))
      (ok (json-false-p (json-at document "preconditions_complete")))
      (ok (eql 17 (json-at document "preconditions_omitted_chars")))
      (ok (claims-p text "truncated"))
      (ok (claims-p text "17")))))

(deftest a-key-that-is-not-part-of-this-kind-is-null
  (let ((contract (%document (build-spec-describe-response
                              (describe-scenario :contract))))
        (property (%document (build-spec-describe-response
                              (describe-scenario :property)))))
    (ok (json-null-p (json-at contract "shrink_enabled"))
        "a contract declares no shrinking, and false would say it is off")
    (ok (json-true-p (json-at property "shrink_enabled")))
    (ok (json-null-p (json-at contract "body_complete"))
        "and a contract has no body to have been cut")))

(deftest a-declaration-crosses-json-as-it-was-written
  (dolist (documentation (list "A contract of this fixture's own."
                               "引き落としは残高を超えない。"
                               (format nil "Quotes \"like this\", a backslash \\ and a~%second line.")
                               ""))
    (let* ((response (build-spec-describe-response
                      (describe-scenario :contract :documentation documentation)))
           (document (%document response)))
      (ok (equal documentation (json-at document "documentation"))
          (format nil "~S survives encoding" documentation))))
  (testing "with its arguments in order, each keeping its own kind"
    (let* ((document (%document (build-spec-describe-response
                                 (describe-scenario :contract))))
           (arguments (json-at document "arguments")))
      (ok (eql 2 (length arguments)))
      (ok (equal "required" (json-at (aref arguments 0) "kind")))
      (ok (equal "key" (json-at (aref arguments 1) "kind")))
      (ok (equal "force" (json-at (aref arguments 1) "keyword")))
      (ok (json-object-p (json-at (aref arguments 1) "supplied_p")))
      (ok (json-null-p (json-at (aref arguments 0) "supplied_p")))))
  (testing "and its cases in the order declared"
    (let* ((document (%document (build-spec-describe-response
                                 (describe-scenario :contract-cases))))
           (cases (json-at document "cases")))
      (ok (equal '("sufficient" "insufficient")
                 (loop for case across cases collect (json-at case "name")))))))

(deftest a-contract-that-signals-does-not-also-return
  (let ((returns (%document (build-spec-describe-response
                             (describe-scenario :contract))))
        (signals (%document (build-spec-describe-response
                             (describe-scenario :contract-signals)))))
    (ok (json-object-p (json-at returns "returns")))
    (ok (json-null-p (json-at returns "signals")))
    (ok (json-object-p (json-at signals "signals")))
    (ok (json-null-p (json-at signals "returns")))))

;;; ------------------------------------------------------------------------
;;; E. spec-check

(deftest the-headline-is-one-verdict-and-not-a-substring-of-another
  (multiple-value-bind (document text) (%check :passed-with-gaps)
    (declare (ignore document))
    (ok (equal "✓ VERIFIED" (first-line text))))
  (multiple-value-bind (document text) (%check :case-never-reached)
    (declare (ignore document))
    (ok (claims-p text "⚠ NOT VERIFIED"))
    (ok (not (claims-p text "✓ VERIFIED"))
        "the good news is not found inside the bad")
    (ok (claims-p text "insufficient") "and the case nobody reached is named"))
  (multiple-value-bind (document text) (%check :has-failure)
    (declare (ignore document))
    (ok (claims-p text "✗ FAILED"))
    (ok (not (claims-p text "✓ VERIFIED")))))

(deftest a-status-with-no-run-claims-no-verdict
  (multiple-value-bind (document text) (%check :timeout)
    (ok (equal "timeout" (json-at document "status")))
    (ok (json-false-p (json-at document "verified")))
    (ok (claims-p text "TIMEOUT"))
    (dolist (token '("✓ VERIFIED" "✗ FAILED" "⚠ NOT VERIFIED"))
      (ok (not (claims-p text token))
          (format nil "~A is not claimed about a run that did not happen" token)))
    (testing "and the keys that describe a run are not there to be read as one"
      (ok (not (nth-value 1 (json-at document "results"))))
      (ok (not (nth-value 1 (json-at document "counts")))))))

(deftest the-gaps-reach-the-text-as-well-as-the-payload
  (multiple-value-bind (document text) (%check :passed-with-gaps)
    (ok (json-true-p (json-at document "verified")))
    (ok (equal '("rejection-counts-unmeasured" "input-coverage-unmeasured")
               (coerce (json-at document "verification_gaps") 'list))
        "every run carries these two; there is no check with none")
    ;; Read as a line, not searched for in the whole text: one gap found
    ;; somewhere is not every gap listed.
    (let ((line (line-starting-with text "verification gaps: ")))
      (ok line)
      (ok (claims-p line "rejection-counts-unmeasured"))
      (ok (claims-p line "input-coverage-unmeasured")))
    (ok (claims-p text "verified: true")))
  (testing "a gap a result produced is listed before the two standing ones"
    (multiple-value-bind (document text) (%check :case-never-reached)
      (ok (equal '("cases-never-called" "rejection-counts-unmeasured"
                   "input-coverage-unmeasured")
                 (coerce (json-at document "verification_gaps") 'list)))
      (ok (claims-p (line-starting-with text "verification gaps: ")
                    "cases-never-called")))))

(deftest four-answers-about-a-counterexample-stay-four
  (dolist (row '((:empty-counterexample "present" 0)
                 (:no-counterexample "none" 0)
                 (:counterexample-not-collected "unavailable" 0)
                 (:generation-failed "not-applicable" 0)
                 (:has-failure "present" 2)))
    (destructuring-bind (case status count) row
      (multiple-value-bind (document text) (%check case)
        (declare (ignore text))
        (let* ((results (json-at document "results"))
               (result (aref results (1- (length results)))))
          (ok (equal status (json-at result "counterexample_status"))
              (format nil "~(~A~) is ~A" case status))
          (ok (json-array-p (json-at result "counterexample")))
          (ok (eql count (length (json-at result "counterexample"))))))))
  (testing "and the one that could not be read says why"
    (multiple-value-bind (document text) (%check :counterexample-not-collected)
      (let ((result (aref (json-at document "results") 0)))
        (ok (stringp (json-at result "counterexample_unavailable_reason"))))
      (ok (claims-p text "UNAVAILABLE"))
      (ok (not (claims-p text "none reported"))
          "which is a different answer from the backend reporting none"))))

(deftest a-captured-nil-is-a-value-and-an-unavailable-capture-is-not
  (testing "in the text, which is written from the record's raw source"
    (multiple-value-bind (document text) (%check :capture-collected-nil)
      (declare (ignore document))
      (ok (claims-p text "captured:"))
      (ok (claims-p text "BALANCE = NIL"))
      (ok (not (claims-p text "BALANCE = UNAVAILABLE"))))
    (multiple-value-bind (document text) (%check :capture-unavailable)
      (declare (ignore document))
      (ok (claims-p text "BALANCE = UNAVAILABLE"))
      (ok (claims-p text "not-restorable"))
      (ok (not (claims-p text "BALANCE = NIL"))
          "a value cl-mcp could not read is not a value the run produced")))
  ;; The text and the payload are written from different halves of the same
  ;; record, so evidence can reach one and not the other.
  (testing "and in the payload, which is written from its projection"
    (multiple-value-bind (document text) (%check :capture-collected-nil)
      (declare (ignore text))
      (let* ((result (aref (json-at document "results") 0))
             (entry (aref (json-at result "core_result" "data" "capture") 0)))
        (ok (equal "BALANCE" (json-at entry "name")))
        (ok (equal "collected" (json-at entry "availability")))
        (ok (equal "NIL" (json-at entry "value" "printed")))))
    (multiple-value-bind (document text) (%check :capture-unavailable)
      (declare (ignore text))
      (let* ((result (aref (json-at document "results") 0))
             (entry (aref (json-at result "core_result" "data" "capture") 0)))
        (ok (equal "unavailable" (json-at entry "availability")))
        (ok (equal "not-restorable" (json-at entry "reason")))
        (ok (not (nth-value 1 (json-at entry "value")))
            "and carries no value key to be mistaken for one")))))

(deftest a-digest-that-moved-is-not-a-verdict-about-the-code
  (multiple-value-bind (document text) (%check :digest-moved)
    ;; The documented word, not the keyword: "unfaithful" says what happened
    ;; where "false" would read as a boolean about the run.
    (ok (equal "unfaithful" (json-at document "reproduction_faithful")))
    ;; The run holds, and says so.  Whether it reproduced the run the caller
    ;; named is a different question, answered beside the verdict.
    (ok (json-true-p (json-at document "verified"))
        "a digest that disagrees does not falsify anything")
    (ok (claims-p text "✓ VERIFIED"))
    (ok (claims-p text "did NOT reproduce"))
    (let ((result (aref (json-at document "results") 0)))
      (ok (equal "passed" (json-at result "status")))
      (ok (equal "mismatch" (json-at result "definition_match"))
          "and the comparison is a word of its own, not a boolean"))))

(deftest a-seed-stays-a-decimal-string
  (multiple-value-bind (document text) (%check :has-failure)
    (let ((result (aref (json-at document "results") 1)))
      (ok (stringp (json-at result "seed")))
      (ok (equal "11" (json-at result "seed"))))
    (let ((first (aref (json-at document "results") 0)))
      (ok (equal "3963993791726803706" (json-at first "seed"))
          "nineteen digits, which a JSON number would round"))
    (ok (claims-p text "3963993791726803706"))))

(deftest the-raw-record-does-not-reach-the-payload
  (multiple-value-bind (document text) (%check :case-never-reached)
    (declare (ignore text))
    (let ((result (aref (json-at document "results") 0)))
      (ok (json-object-p (json-at result "core_result")))
      (ok (not (nth-value 1 (json-at result "core_result" "source")))
          "cl-spec's own record stays behind the projection")
      ;; Behind the projection, not withheld: what the text says about the
      ;; case nobody reached is in the payload as well.
      (ok (equal '("insufficient")
                 (coerce (json-at result "core_result" "data" "never_called")
                         'list))))))

(deftest every-scenario-speaks-the-report-layers-own-vocabulary
  ;; The descriptors claim to be states the report layer builds.  A value it
  ;; never emits -- a status of :ABSENT, a gap nobody appends -- would make a
  ;; scenario a positive example of nothing, and every check over it would
  ;; hold vacuously.  These are the documented sets, read from production.
  (let ((statuses cl-mcp/src/spec-adapter-report::+result-statuses+)
        (calls cl-mcp/src/spec-adapter-report::+call-statuses+)
        (gaps cl-mcp/src/spec-adapter-report::+verification-gap-values+)
        (counts (mapcar #'cdr cl-mcp/src/spec-adapter-report::+named-count-statuses+)))
    (testing "and the robustness case is the one it does not"
      ;; Stated rather than skipped: BUILD-SPEC-CHECK-RESPONSE answers a
      ;; whole-call timeout, and no spec-check call produces one, so the
      ;; documented set does not name it.  If that ever changes, this is
      ;; where it is noticed.
      (ok (equal '(:timeout) +check-robustness-cases+))
      (ok (not (member :timeout calls))))
    (dolist (case +check-cases+)
      (let ((report (check-scenario case)))
        (testing (format nil "~(~A~)" case)
          (ok (member (getf report :status) calls)
              (format nil "call status ~S is documented" (getf report :status)))
          (dolist (gap (getf report :verification-gaps))
            (ok (or (member gap gaps) (member gap statuses))
                (format nil "gap ~S is documented" gap)))
          (dolist (result (getf report :results))
            (ok (member (getf result :status) statuses)
                (format nil "result status ~S is documented" (getf result :status))))
          (dolist (key '(:passed :failed :errored :timed-out :not-run))
            (ok (or (null (getf report :counts)) (member key counts))
                (format nil "count field ~S has a status of its own" key))))))))

;;; ------------------------------------------------------------------------
;;; F. The replay line

(deftest the-replay-line-asks-for-the-failure-not-the-first-run
  (multiple-value-bind (document text) (%check :has-failure)
    (declare (ignore document))
    (let ((line (line-starting-with text "Replay:")))
      (ok line)
      (ok (claims-p line (format nil "property=~A" (qualified :other-property))))
      (ok (claims-p line "seed=11"))
      (ok (claims-p line "profile=normal"))
      ;; The two results carry different digests, so a line that took the
      ;; failure's name and seed and the other result's digest is visible.
      (ok (claims-p line "expect_definition_digest=fnv1a64-v1:00000000000000ee"))
      (ok (not (claims-p line "00000000000000dd"))
          "not the digest of the run that already holds")
      (ok (not (claims-p line "3963993791726803706"))
          "nor its seed"))))

(deftest a-contract-is-replayed-by-function-and-a-budget
  (multiple-value-bind (document text) (%check :contract-only)
    (declare (ignore document))
    (let ((line (line-starting-with text "Replay:")))
      (ok line)
      (ok (claims-p line (format nil "function=~A" (qualified :subject))))
      (ok (claims-p line "trials=25"))
      (ok (not (claims-p line "property=")) "property= names something that does not exist")
      (ok (not (claims-p line "profile=")) "and a contract run used no profile"))))

(deftest a-run-with-no-seed-prints-no-replay-line
  (dolist (case '(:no-properties :timeout))
    (multiple-value-bind (document text) (%check case)
      (declare (ignore document))
      (ok (null (line-starting-with text "Replay:"))
          (format nil "~(~A~) has nothing to replay" case))
      (ok (not (claims-p text "seed=NIL"))))))
