;;;; tests/spec-responses-specs-test.lisp
;;;;
;;;; The replay line against a real cl-spec.  Opt-in: this file needs cl-spec
;;;; loaded, so it is not in tests.lisp and the default suite never sees it.
;;;; CI runs it as its own step, in a process of its own
;;;; (CL_MCP_SPECS_MODE=integration, see docs/specs.md).
;;;;
;;;; specs/spec-responses.lisp checks the line against scenario descriptors:
;;;; that it names the result it reports, with that result's own seed and
;;;; digest.  What a descriptor cannot say is whether the line, handed back to
;;;; the tool it names, runs the same thing again.  That is this file: a real
;;;; run, its printed instruction read back by the same grammar an agent would
;;;; read it by, and the arguments it yields passed to SPEC-CHECK-RESPONSE.
;;;;
;;;; The definitions here are pure -- no state, no clock, no files -- so a
;;;; replay under the same seed is expected to reproduce the same trial.

(defpackage #:cl-mcp/tests/spec-responses-specs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-spec/main
                #:defproperty
                #:defspec-function
                #:make-hash-table-registry
                #:*registry*)
  ;; A bare :import-from declares the check-it backend as a dependency without
  ;; importing a symbol: a generated run needs it installed.
  (:import-from #:cl-spec/src/backends/check-it)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-check-response)
  (:import-from #:cl-mcp/specs/spec-response-fixtures
                #:response-json
                #:parse-response
                #:json-at
                #:json-text
                #:json-false-p
                #:line-starting-with
                #:replay-arguments
                #:replay-argument))

(in-package #:cl-mcp/tests/spec-responses-specs-test)

(defun responses-real-double (x)
  "Return twice X."
  (* 2 x))

(defun %register (registry)
  "Register this file's declarations in REGISTRY: one property that always
fails, and one contract whose :RETURNS no return value can satisfy.

Both fail on their first trial, and neither reads or writes anything outside
itself, so the failure a seed produces is the failure that seed reproduces."
  (let ((*registry* registry))
    (defproperty responses-real-never-holds ((x (range integer 0 10)))
      "Claims something false about a doubled number, for a run that fails."
      (:about responses-real-double)
      (:trials (:smoke 5 :normal 25))
      (minusp (responses-real-double x)))
    (defspec-function responses-real-double
      "Twice its argument, declared to return what it never returns."
      (:args (x (range integer 0 10)))
      (:returns (range integer 100 200)))
    registry))

(defmacro with-installed-registry ((registry) &body body)
  "Install REGISTRY as CL-SPEC:*REGISTRY*'s global value for BODY, and put the
previous one back however BODY exits.

SPEC-CHECK-RESPONSE reads the registry on a deadline thread of its own, which
does not see a dynamic binding made here, so a LET would leave it reading
whatever registry the image holds.  That is also why this suite runs in a
process of its own rather than in an MCP worker someone is using."
  (let ((previous (gensym "PREVIOUS")))
    `(let ((,previous *registry*))
       (setf *registry* ,registry)
       (unwind-protect (progn ,@body)
         (setf *registry* ,previous)))))

(defun %params (&rest pairs)
  "Return a tool-arguments hash-table holding PAIRS."
  (let ((params (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key params) value))
    params))

(defun %document (response)
  "Return RESPONSE as a client reads it."
  (parse-response (response-json response)))

(defun %replay-params (line)
  "Return the tool arguments the Replay LINE asks for.

Built from the line's own grammar: every value crosses as the text it is
printed as, except the trial budget, which the tool takes as a number.
Nothing here is read or evaluated."
  (let ((arguments (replay-arguments line))
        (params (make-hash-table :test #'equal)))
    (loop for (key value) on arguments by #'cddr
          do (setf (gethash key params)
                   (if (equal "trials" key)
                       (parse-integer value)
                       value)))
    params))

(defun %first-result (document)
  "Return the first per-result object of DOCUMENT."
  (let ((results (json-at document "results")))
    (when (plusp (length results)) (aref results 0))))

(defun %counterexample-text (result)
  "Return RESULT's counterexample as \"NAME=PRINTED\" pairs."
  (loop for entry across (json-at result "counterexample")
        collect (format nil "~A=~A"
                        (json-at entry "variable" "name")
                        (json-at entry "value" "printed"))))

;;; ------------------------------------------------------------------------
;;; Tests

(deftest a-property-replay-line-runs-the-property-it-names
  (with-installed-registry ((%register (make-hash-table-registry)))
    (let* ((response (spec-check-response
                      (%params "property"
                               "CL-MCP/TESTS/SPEC-RESPONSES-SPECS-TEST::RESPONSES-REAL-NEVER-HOLDS"
                               "profile" "normal"
                               "timeout_seconds" 60)))
           (document (%document response))
           (text (json-text response))
           (result (%first-result document))
           (line (line-starting-with text "Replay:")))
      (ok (equal "completed" (json-at document "status")))
      (ok (json-false-p (json-at document "verified")))
      (ok (equal "failed" (json-at result "status")))
      (ok line "the response printed an instruction")
      (testing "which names this property, its seed and its digest"
        (let ((arguments (replay-arguments line)))
          (ok (equal (json-at result "property" "qualified")
                     (replay-argument arguments "property")))
          (ok (equal (json-at result "seed") (replay-argument arguments "seed")))
          (ok (equal "normal" (replay-argument arguments "profile")))
          (ok (equal (json-at result "definition_digest")
                     (replay-argument arguments "expect_definition_digest")))
          (ok (null (replay-argument arguments "function")))))
      (testing "and running it again reproduces that run"
        (let* ((again (%document (spec-check-response (%replay-params line))))
               (replayed (%first-result again)))
          (ok (equal "completed" (json-at again "status")))
          (ok (equal (json-at result "property" "qualified")
                     (json-at replayed "property" "qualified")))
          (ok (equal (json-at result "seed") (json-at replayed "seed")))
          (ok (equal "failed" (json-at replayed "status")))
          (ok (equal "match" (json-at replayed "definition_match"))
              "against the digest the line carried")
          (ok (equal "faithful" (json-at again "reproduction_faithful")))
          (ok (equal (%counterexample-text result) (%counterexample-text replayed))
              "the same trial, not merely another failure"))))))

(deftest a-contract-replay-line-runs-the-contract-with-its-budget
  (with-installed-registry ((%register (make-hash-table-registry)))
    (let* ((response (spec-check-response
                      (%params "function"
                               "CL-MCP/TESTS/SPEC-RESPONSES-SPECS-TEST::RESPONSES-REAL-DOUBLE"
                               "trials" 7
                               "timeout_seconds" 60)))
           (document (%document response))
           (text (json-text response))
           (result (%first-result document))
           (line (line-starting-with text "Replay:")))
      (ok (equal "contract" (json-at result "kind")))
      (ok (equal "failed" (json-at result "status")))
      (ok line)
      (testing "asked for as function=, with the budget it ran under"
        (let ((arguments (replay-arguments line)))
          (ok (equal (json-at result "property" "qualified")
                     (replay-argument arguments "function")))
          (ok (null (replay-argument arguments "property"))
              "property= would name something that is not registered")
          (ok (null (replay-argument arguments "profile"))
              "a contract run used no profile")
          (ok (equal "7" (replay-argument arguments "trials")))
          (ok (equal (json-at result "seed") (replay-argument arguments "seed")))))
      (testing "and running it again reproduces that run"
        (let* ((again (%document (spec-check-response (%replay-params line))))
               (replayed (%first-result again)))
          (ok (equal "contract" (json-at replayed "kind")))
          (ok (equal "failed" (json-at replayed "status")))
          (ok (equal (json-at result "seed") (json-at replayed "seed")))
          (ok (eql 7 (json-at replayed "trials" "budget")))
          (ok (equal "match" (json-at replayed "definition_match")))
          (ok (equal (%counterexample-text result)
                     (%counterexample-text replayed))))))))
