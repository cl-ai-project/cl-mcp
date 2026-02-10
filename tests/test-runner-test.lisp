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
  (:import-from #:cl-mcp/tests/clhs-test))

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
  (testing "run-tests result contains passed, failed, framework fields"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      (ok (not (null (gethash "passed" result))))
      (ok (not (null (gethash "failed" result))))
      (ok (gethash "framework" result))
      (ok (string= "rove" (gethash "framework" result))))))

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

;;; ---------------------------------------------------------------------------
;;; Failure Details Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-captures-failure-details
  (testing "run-tests captures failure details for failed tests"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-failures")))
      (ok (> (gethash "failed" result) 0) "Should have failures")
      (let ((failures (gethash "failed_tests" result)))
        (ok failures "Should have failed_tests array")
        (ok (> (length failures) 0) "Should have at least one failure")
        (let ((first-failure (aref failures 0)))
          (ok (gethash "test_name" first-failure) "Failure should have test_name"))))))

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
  (testing "run-tests falls back to asdf for unknown framework"
    ;; Force unknown framework - should fall back to asdf
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :framework "unknown")))
      (ok (string= "asdf" (gethash "framework" result)))
      ;; ASDF fallback returns success boolean instead of counts
      (ok (gethash "success" result)))))

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
