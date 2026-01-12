;;;; tests/test-runner-test-error.lisp --- Helper test for testing error handling
;;;; This test signals an error during execution to test error capture.

(defpackage #:cl-mcp/tests/test-runner-test-error
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok))

(in-package #:cl-mcp/tests/test-runner-test-error)

(deftest test-that-signals-error
  (testing "This test signals an error"
    (error "Intentional error for testing error handling")
    (ok t "This should not be reached")))
