(defpackage #:cl-mcp/tests/timeout-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:ok))

(in-package #:cl-mcp/tests/timeout-test)

(deftest slow-test
  (sleep 10)
  (ok t "this should not be reached if timeout fires"))
