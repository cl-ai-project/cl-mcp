;;;; tests/test-runner-test-stdout.lisp --- Helper test that prints to stdout

(defpackage #:cl-mcp/tests/test-runner-test-stdout
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok))

(in-package #:cl-mcp/tests/test-runner-test-stdout)

(deftest stdout-capture-test
  (testing "This test prints to stdout for capture testing"
    (format t "~&DEBUG-MARKER-12345~%")
    (ok t "Always passes")))
