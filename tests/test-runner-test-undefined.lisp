;;;; tests/test-runner-test-undefined.lisp --- Helper test for testing undefined function errors
;;;; This test calls an undefined function to test error capture.

(defpackage #:cl-mcp/tests/test-runner-test-undefined
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok))

(in-package #:cl-mcp/tests/test-runner-test-undefined)

(deftest test-with-undefined-function
  (testing "This test calls an undefined function"
    (ok (cl-mcp/tests/test-runner-test-undefined::nonexistent-fn-xyz))))
