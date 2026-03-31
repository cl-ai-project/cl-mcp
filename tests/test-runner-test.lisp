;;;; tests/test-runner-test.lisp

(defpackage #:cl-mcp/tests/test-runner-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:signals)
  (:import-from #:cl-mcp/src/test-runner
                #:run-tests
                #:detect-test-framework)
  ;; Load clhs-test system so we can use it as a test subject
  ;; NOTE: Do NOT import from helper test packages (test-runner-test-failures, etc.)
  ;; as that would register their intentionally-failing tests with Rove
  (:import-from #:cl-mcp/tests/clhs-test)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-run-tests-response))

(in-package #:cl-mcp/tests/test-runner-test)

;;; ---------------------------------------------------------------------------
;;; Framework Detection Tests
;;; ---------------------------------------------------------------------------

(deftest detect-test-framework-finds-rove
  (testing "detect-test-framework returns :rove when rove is loaded"
    ;; Rove is loaded since we're using it for tests
    (ok (eq :rove (detect-test-framework "any-system")))))

;;; ---------------------------------------------------------------------------
;;; Result Structure Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-returns-hash-table
  (testing "run-tests returns a hash table"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      (ok (hash-table-p result)))))

(deftest run-tests-contains-required-fields
  (testing "run-tests result contains required structured fields"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      (ok (integerp (gethash "passed" result)))
      (ok (integerp (gethash "failed" result)))
      (ok (integerp (gethash "duration_ms" result)))
      (ok (string= "rove" (gethash "framework" result)))
      (let ((failures (gethash "failed_tests" result)))
        (ok (vectorp failures) "failed_tests should be an array")
        (ok (= 0 (length failures))
            "successful suite should return empty failed_tests")))))

(deftest run-tests-contains-duration
  (testing "run-tests result contains duration_ms"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      (ok (gethash "duration_ms" result))
      (ok (numberp (gethash "duration_ms" result))))))

;;; ---------------------------------------------------------------------------
;;; Passing Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-reports-passing-tests
  (testing "run-tests correctly reports passing tests"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      ;; clhs-test should pass
      (ok (>= (gethash "passed" result) 0))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-captures-stdout
  (testing "run-tests includes stdout from test execution"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-stdout")))
      (ok (= 0 (gethash "failed" result)) "Helper test should pass")
      (let ((stdout (gethash "stdout" result)))
        (ok (stringp stdout) "stdout should be present as a string")
        (ok (search "DEBUG-MARKER-12345" stdout)
            "stdout should contain the debug output from the test")))))

(deftest run-tests-selected-captures-stdout
  (testing "run-tests with :test captures stdout"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-stdout"
                             :test "cl-mcp/tests/test-runner-test-stdout::stdout-capture-test")))
      (ok (= 0 (gethash "failed" result)))
      (let ((stdout (gethash "stdout" result)))
        (ok (stringp stdout) "stdout should be present")
        (ok (search "DEBUG-MARKER-12345" stdout)
            "stdout should contain the debug output")))))

(deftest run-tests-captures-debug-output
  (testing "run-tests includes debug_output from *test-debug-output* stream"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-debug-output")))
      (ok (= 0 (gethash "failed" result)) "Helper test should pass")
      (let ((debug-out (gethash "debug_output" result)))
        (ok (stringp debug-out) "debug_output should be present as a string")
        (ok (search "DEBUG-STREAM-MARKER-98765" debug-out)
            "debug_output should contain the debug stream output")))))

(deftest run-tests-selected-captures-debug-output
  (testing "run-tests with :test captures debug_output"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-debug-output"
                             :test "cl-mcp/tests/test-runner-test-debug-output::debug-output-capture-test")))
      (ok (= 0 (gethash "failed" result)))
      (let ((debug-out (gethash "debug_output" result)))
        (ok (stringp debug-out) "debug_output should be present")
        (ok (search "DEBUG-STREAM-MARKER-98765" debug-out)
            "debug_output should contain the debug stream output")))))

(deftest run-tests-content-text-excludes-stdout
  (testing "content text does not contain raw stdout (kept in structured field only)"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-stdout"))
           (resp (build-run-tests-response result))
           (text (gethash "text" (aref (gethash "content" resp) 0))))
      (ok (search "DEBUG-MARKER-12345" (gethash "stdout" resp))
          "stdout structured field should contain the marker")
      (ok (not (search "DEBUG-MARKER-12345" text))
          "content text should not contain raw stdout"))))

(deftest run-tests-content-text-includes-debug-output
  (testing "content text includes debug_output from *test-debug-output*"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-debug-output"))
           (resp (build-run-tests-response result))
           (text (gethash "text" (aref (gethash "content" resp) 0))))
      (ok (search "DEBUG-STREAM-MARKER-98765" text)
          "content text should include debug output"))))

;;; ---------------------------------------------------------------------------
;;; Failure Details Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-captures-failure-details
  (testing "run-tests captures failure details for failed tests"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-failures")))
      (ok (> (gethash "failed" result) 0) "Should have failures")
      (let ((failures (gethash "failed_tests" result)))
        (ok (vectorp failures) "failed_tests should be an array")
        (ok (> (length failures) 0) "Should have at least one failure")
        (let ((first-failure (aref failures 0)))
          (ok (gethash "test_name" first-failure)
              "Failure should include test_name")
          (multiple-value-bind (reason presentp)
              (gethash "reason" first-failure)
            (ok (or (not presentp) (stringp reason))
                "Failure reason should be absent or a string")))))))

(deftest run-tests-failure-reason-is-string
  (testing "run-tests converts error conditions to strings in failure reason"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-failures")))
      (let* ((failures (gethash "failed_tests" result))
             (failure (aref failures 0))
             (reason (gethash "reason" failure)))
        ;; reason may be nil for assertion failures, but if present must be string
        (ok (or (null reason) (stringp reason))
            "Reason should be nil or a string, not a condition object")))))

;;; ---------------------------------------------------------------------------
;;; Error Handling During Test Execution
;;; ---------------------------------------------------------------------------

(deftest run-tests-handles-error-during-execution
  (testing "run-tests captures errors signaled during test execution"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-error")))
      (ok (= 0 (gethash "passed" result)) "Should have no passed tests")
      (ok (= 1 (gethash "failed" result)) "Should have one failed test")
      (let* ((failures (gethash "failed_tests" result))
             (failure (aref failures 0))
             (reason (gethash "reason" failure)))
        (ok (stringp reason) "Reason should be a string, not a condition object")))))

(deftest run-tests-handles-undefined-function
  (testing "run-tests captures undefined function errors"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-undefined")))
      (ok (= 0 (gethash "passed" result)) "Should have no passed tests")
      (ok (= 1 (gethash "failed" result)) "Should have one failed test")
      (let* ((failures (gethash "failed_tests" result))
             (failure (aref failures 0))
             (reason (gethash "reason" failure)))
        (ok (stringp reason) "Reason should be a string")))))

;;; ---------------------------------------------------------------------------
;;; Error Handling Tests - Missing Suite
;;; ---------------------------------------------------------------------------

(deftest run-tests-errors-on-missing-suite
  (testing "run-tests signals error for non-existent suite"
    (ok (signals (run-tests "non-existent-test-suite-xyz")))))

;;; ---------------------------------------------------------------------------
;;; Framework Parameter Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-accepts-framework-parameter
  (testing "run-tests accepts framework parameter"
    ;; Force rove framework
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :framework "rove")))
      (ok (string= "rove" (gethash "framework" result))))))

(deftest run-tests-asdf-fallback
  (testing "run-tests falls back to asdf and keeps structured response fields"
    ;; Force unknown framework - should fall back to asdf
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :framework "unknown")))
      (ok (string= "asdf" (gethash "framework" result)))
      (ok (integerp (gethash "passed" result)))
      (ok (integerp (gethash "failed" result)))
      (ok (integerp (gethash "duration_ms" result)))
      (ok (vectorp (gethash "failed_tests" result)))
      (ok (member (gethash "success" result) '(t nil))))))

(deftest run-tests-single-test-runs-only-target
  (testing "run-tests runs only the specified single test"
    (let ((result (run-tests "cl-mcp/tests/clhs-test"
                             :test "cl-mcp/tests/clhs-test::clhs-lookup-symbol-with-hyphen")))
      (ok (= 1 (gethash "passed" result)))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-single-test-loads-target-system-package
  (testing "run-tests loads the target test system before selective execution"
    (let ((result (run-tests
                   "cl-mcp/tests/utils-strings-test"
                   :framework "rove"
                   :test
                   "cl-mcp/tests/utils-strings-test::ensure-trailing-newline-adds-newline")))
      (ok (= 1 (gethash "passed" result)))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-tests-array-runs-selected-tests
  (testing "run-tests runs only tests listed in :tests"
    (let ((result (run-tests "cl-mcp/tests/clhs-test"
                             :tests '("cl-mcp/tests/clhs-test::clhs-lookup-symbol-with-hyphen"
                                      "cl-mcp/tests/clhs-test::clhs-lookup-format-as-symbol"))))
      (ok (= 2 (gethash "passed" result)))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-framework-auto-detects
  (testing "run-tests treats framework=auto as automatic detection"
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :framework "auto")))
      (ok (string= "rove" (gethash "framework" result))))))

(deftest run-tests-rejects-test-and-tests-together
  (testing "run-tests signals error when test and tests are both provided"
    (ok (signals (run-tests "cl-mcp/tests/clhs-test"
                            :test "cl-mcp/tests/clhs-test::clhs-lookup-symbol-with-hyphen"
                            :tests '("cl-mcp/tests/clhs-test::clhs-lookup-format-as-symbol"))))))

(deftest run-tests-tests-array-rejects-nil-element
  (testing "run-tests rejects NIL entries in :tests"
    (ok (signals (run-tests "cl-mcp/tests/clhs-test"
                            :tests '(nil))))))

(deftest run-tests-failure-includes-assertion-details
  (testing "run-tests includes description, form, and values in failure details"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-failures"))
           (failures (gethash "failed_tests" result))
           (failure (aref failures 0)))
      (ok (> (length failures) 0) "Should have failures")
      (ok (gethash "test_name" failure) "Should have test_name")
      ;; These come from (ok (= 1 2) "1 should equal 2") in the helper
      (let ((desc (gethash "description" failure)))
        (ok (stringp desc) "Should include assertion description")
        (ok (search "1 should equal 2" desc)
            "Description should contain the ok message"))
      (let ((form (gethash "form" failure)))
        (ok (stringp form) "Should include assertion form")
        (ok (search "= 1 2" form)
            "Form should contain the assertion expression"))
      ;; reason may be NIL for simple (ok ...) assertions; just check it doesn't error
      (ok (or (null (gethash "reason" failure))
              (stringp (gethash "reason" failure)))
          "reason should be nil or a string"))))

(deftest run-tests-handles-direct-assertion-failures
  (testing "run-tests handles failures from direct assertions without (testing ...) wrapper"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-direct-assertion"))
           (failures (gethash "failed_tests" result)))
      (ok (> (gethash "failed" result) 0) "Should have failures")
      (ok (> (length failures) 0) "Should have failure details")
      (let ((failure (aref failures 0)))
        (ok (gethash "test_name" failure) "Should have test_name")
        (let ((desc (gethash "description" failure)))
          (ok (stringp desc) "Should include assertion description")
          (ok (search "3 should equal 4" desc)
              "Description should contain the ok message"))
        (let ((form (gethash "form" failure)))
          (ok (stringp form) "Should include assertion form"))))))
