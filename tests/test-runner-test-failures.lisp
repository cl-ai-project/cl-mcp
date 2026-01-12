;;;; tests/test-runner-test-failures.lisp --- Helper test for testing failure capture
;;;; This test intentionally fails to test the test-runner's failure detection.

(defpackage #:cl-mcp/tests/test-runner-test-failures
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok))

(in-package #:cl-mcp/tests/test-runner-test-failures)

(deftest intentional-failure-for-test-runner
  (testing "This test intentionally fails"
    (ok (= 1 2) "1 should equal 2")))
