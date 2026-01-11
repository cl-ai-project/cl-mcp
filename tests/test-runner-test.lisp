;;;; tests/test-runner-test.lisp

(defpackage #:cl-mcp/tests/test-runner-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:signals)
  (:import-from #:cl-mcp/src/test-runner
                #:run-tests
                #:detect-test-framework)
  ;; Load clhs-test system so we can use it as a test subject
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

;; Define a test suite that intentionally fails for testing failure reporting
(defpackage #:cl-mcp/tests/test-runner-test/failures
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok))

(in-package #:cl-mcp/tests/test-runner-test/failures)

(rove:deftest intentional-failure-for-test-runner
  (rove:testing "This test intentionally fails"
    (rove:ok (= 1 2) "1 should equal 2")))

(in-package #:cl-mcp/tests/test-runner-test)

(deftest run-tests-captures-failure-details
  (testing "run-tests captures failure details for failed tests"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test/failures")))
      (ok (> (gethash "failed" result) 0) "Should have failures")
      (let ((failures (gethash "failed_tests" result)))
        (ok failures "Should have failed_tests array")
        (ok (> (length failures) 0) "Should have at least one failure")
        (let ((first-failure (aref failures 0)))
          (ok (gethash "test_name" first-failure) "Failure should have test_name")
          (ok (gethash "form" first-failure) "Failure should have form")
          (ok (gethash "description" first-failure) "Failure should have description"))))))

;;; ---------------------------------------------------------------------------
;;; Error Handling Tests
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
