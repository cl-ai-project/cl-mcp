;;;; tests/test-runner-test-counts.lisp --- Helper tests for run-tests' count unit
;;;; Three tests holding several assertions each, one of them failing, so a
;;;; count of assertions and a count of tests give different numbers.

(defpackage #:cl-mcp/tests/test-runner-test-counts
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip))

(in-package #:cl-mcp/tests/test-runner-test-counts)

(deftest three-passing-assertions
  (ok (= 1 1) "first")
  (ok (= 2 2) "second")
  (ok (= 3 3) "third"))

(deftest two-passing-assertions
  (ok (= 1 1) "first")
  (ok (= 2 2) "second"))

(deftest one-of-three-assertions-fails
  (ok (= 1 1) "passes")
  (ok (= 1 2) "fails on purpose")
  (ok (= 3 3) "passes too"))

(deftest nested-testing-blocks
  (testing "outer"
    (ok (= 1 1) "outer assertion")
    (testing "inner"
      (ok (= 2 2) "inner assertion")
      (ok (= 3 3) "another inner assertion"))))

(deftest only-skipped
  (skip "nothing to check yet"))
