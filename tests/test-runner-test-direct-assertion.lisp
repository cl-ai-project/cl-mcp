;;;; tests/test-runner-test-direct-assertion.lisp --- Helper test with direct assertion (no testing block)
;;;; This test intentionally fails to test that run-tests handles
;;;; direct assertions in deftest bodies without (testing ...) wrappers.

(defpackage #:cl-mcp/tests/test-runner-test-direct-assertion
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:ok))

(in-package #:cl-mcp/tests/test-runner-test-direct-assertion)

(deftest direct-assertion-failure
  ;; No (testing ...) wrapper — assertion lives directly in deftest body
  (ok (= 3 4) "3 should equal 4"))
