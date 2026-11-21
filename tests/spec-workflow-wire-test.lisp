(defpackage #:cl-mcp/tests/spec-workflow-wire-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:cl-mcp/specs/spec-response-fixtures
                #:json-at #:json-true-p #:json-false-p)
  (:import-from #:cl-mcp/tests/spec-batch-test #:qualified)
  (:import-from #:cl-mcp/tests/spec-input-test #:with-self-specs)
  (:import-from #:cl-mcp/tests/spec-wire-test
                #:with-wire-server #:with-session #:%tool #:%load-fixture))

(in-package #:cl-mcp/tests/spec-workflow-wire-test)

(defun %registry-checks (client)
  "Exercise the fixture and caller-owned forms of the same function spec."
  (let* ((target "cl-spec:registry-register-property")
         (setup "(defparameter cl-user::*wire-registration*
                   (cl-spec/self-spec-fixtures:registration-scenario-arguments :new))")
         (registry-code "(first cl-user::*wire-registration*)")
         (property-code "(third cl-user::*wire-registration*)")
         (fixture (%tool client "spec-check-input" "function" target "recipe"
                         (make-ht "keyword" "new"))))
    (ok (equal "passed" (json-at fixture "status")))
    (ok (equal "new-registration" (json-at fixture "selected_case")))
    (ok (equal 2 (json-at fixture "core_result" "schema_version")))
    (ok (json-true-p (json-at fixture "lifecycle" "target_called")))
    (ok (equal "completed" (json-at fixture "lifecycle" "cleanup")))
    (ok (equal "unavailable" (json-at fixture "outcome" "availability")))
    (ok (equal "fixture-check" (json-at fixture "core_result" "data" "record_kind")))
    (%tool client "repl-eval" "code" setup)
    (flet ((fixture-symbol (name)
             (make-ht "symbol" (concatenate 'string "cl-spec/self-spec-fixtures::" name)))
           (handle (code)
             (multiple-value-bind (result text) (%tool client "repl-eval" "code" code)
               (let ((id (json-at result "result_object_id")))
                 (ok (stringp id) text)
                 (make-ht "object_id" id)))))
      (let* ((arguments
               (vector (handle registry-code)
                       (fixture-symbol "self-registration-subject") (handle property-code)
                       (make-ht "keyword" "targets")
                       (vector (fixture-symbol "self-new-target") (fixture-symbol "self-shared-target"))
                       (make-ht "keyword" "tags")
                       (vector (fixture-symbol "self-new-tag") (fixture-symbol "self-shared-tag"))))
             (call (%tool client "spec-check-input" "function" target "arguments" arguments)))
        (ok (equal "passed" (json-at call "status")) (json-at call "message"))
        (ok (equal "new-registration" (json-at call "selected_case")))
        (ok (equal 1 (json-at call "core_result" "schema_version")))
        (ok (equal "returned" (json-at call "outcome" "value" "kind")))
        (ok (equal "result" (json-at call "core_result" "data" "record_kind")))))))

(defun %generated-counts (client trials reached counts)
  "Compare case counts, verdict and reports at seed 42 in the running worker."
  (multiple-value-bind (response text)
      (%tool client "spec-check" "function" "cl-spec:registry-register-property"
             "trials" trials "seed" "42" "detail" "summary")
    (let* ((row (aref (json-at response "results") 0))
           (coverage (json-at row "case_coverage")))
      (ok (equal trials (json-at row "trials" "executed")))
      (ok (equal reached (json-at coverage "reached")))
      (ok (equal 3 (json-at coverage "declared")))
      (ok (equal counts (map 'list (lambda (case) (json-at case "called"))
                             (json-at coverage "cases"))))
      (ok (if (= reached 3)
              (json-true-p (json-at response "verified"))
              (json-false-p (json-at response "verified"))))
      (ok (search (format nil "~D/3 cases reached" reached) text))
      (ok (search "Input coverage: unmeasured" text)))))

(defun self-spec-names-match-p (symbols names)
  "Return true when NAMES contains each of the declared SYMBOLS exactly once."
  (and symbols
       (equal (sort (mapcar #'qualified symbols) #'string<)
              (sort (map 'list #'identity names) #'string<))))

(defun exercise-self-spec-workflows ()
  "Run the self-bundle acceptance examples in one client session."
  (with-wire-server (port :worker-pool t)
    (with-session (client port)
      ;; The wire fixture adds unrelated registrations to the worker.
      (ok (every (lambda (message) (search "loaded successfully" message))
                 (%load-fixture client)))
      (ok (search "loaded successfully"
                  (nth-value 1 (%tool client "load-system" "system" "cl-spec/specs"))))
      (let* ((external (%tool client "spec-list" "kind" "function-specs" "package" "cl-spec"
                              "package_mode" "external" "detail" "compact"))
             (home (%tool client "spec-list" "kind" "function-specs" "package" "cl-spec"))
             (laws (%tool client "spec-list" "kind" "properties" "package" "cl-spec/specs"
                          "detail" "compact"))
             (functions (json-at external "function_specs"))
             (properties (json-at laws "properties"))
             (expected-functions (uiop:symbol-call :cl-spec/specs :contract-names))
             (expected-properties (uiop:symbol-call :cl-spec/specs :property-names)))
        (testing "discovery can supply exact membership to one batch call"
          (ok (self-spec-names-match-p expected-functions functions)
              (format nil "Discover all ~D self contracts" (length expected-functions)))
          (ok (= 0 (length (json-at home "function_specs"))))
          (ok (self-spec-names-match-p expected-properties properties)
              (format nil "Discover all ~D self properties" (length expected-properties)))
          (let* ((response (%tool client "spec-check-batch" "functions" functions
                                  "properties" properties "trials" 50 "profile" "smoke"
                                  "seed" "42" "timeout_seconds" 180))
                 (rows (json-at response "results")))
            (ok (= (+ (length expected-functions) (length expected-properties)) (length rows)))
            (loop for kind in '("contract" "property")
                  for expected in (list expected-functions expected-properties)
                  for names = (loop for row across rows
                                    when (equal kind (json-at row "kind"))
                                      collect (json-at row "name"))
                  do (ok (self-spec-names-match-p expected names)))
            (ok (every (lambda (row)
                         (and (equal "passed" (json-at row "status"))
                              (equal "42" (json-at row "seed"))
                              (stringp (json-at row "definition_digest"))
                              (hash-table-p (json-at row "replay"))))
                       rows))))
        (testing "a combined symbol selection"
          (let ((response (%tool client "spec-check-batch" "symbol" "cl-spec:validate"
                                 "trials" 5 "profile" "smoke" "seed" "42")))
            (ok (= 3 (length (json-at response "results"))))))
        (testing "read a contract summary, then one case"
          (flet ((describe-view (&rest args)
                   (apply #'%tool client "spec-describe" "kind" "function-spec"
                          "name" "cl-spec:registry-register-property" args)))
            (let ((summary (describe-view "detail" "summary"))
                  (case (describe-view "case" "new-registration")))
              (ok (json-true-p (json-at summary "fixture_present")))
              (ok (= 3 (length (json-at summary "cases"))))
              (ok (equal "new-registration" (json-at case "selected_case")))
              (ok (= 1 (length (json-at case "cases")))))))
        (testing "one invocation and its fixture recipe"
          (%registry-checks client))
        (testing "named cases reached by generated trials"
          (%generated-counts client 1 1 '(1 0 0))
          (%generated-counts client 50 3 '(29 10 11)))))))

(deftest self-spec-workflows-in-a-worker
  (if (asdf:find-system "cl-spec/check-it" nil)
      (with-self-specs (exercise-self-spec-workflows))
      (skip "The optional cl-spec/check-it system is unavailable.")))
