;;;; tests/test-runner-test-debug-output.lisp --- Helper test that writes to *test-debug-output*

(defpackage #:cl-mcp/tests/test-runner-test-debug-output
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/test-runner-core
                #:*test-debug-output*))

(in-package #:cl-mcp/tests/test-runner-test-debug-output)

(deftest debug-output-capture-test
  (testing "This test writes to *test-debug-output* for capture testing"
    (format *test-debug-output* "~&DEBUG-STREAM-MARKER-98765~%")
    (ok t "Always passes")))
