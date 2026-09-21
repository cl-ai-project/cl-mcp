(defpackage #:cl-mcp/tests/debugger-boundary-run-tests-fixture
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok))

(in-package #:cl-mcp/tests/debugger-boundary-run-tests-fixture)

(define-condition run-tests-boundary-condition (condition) ())

(deftest ordinary-rove-failure
  (ok nil "ordinary Rove assertion failure"))

(deftest direct-condition-reaches-worker-boundary
  (error 'run-tests-boundary-condition))
