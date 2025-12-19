;;;; tests/logging-test.lisp

(defpackage #:cl-mcp/tests/logging-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:cl-mcp/src/log #:*log-level* #:*log-stream*))

(in-package #:cl-mcp/tests/logging-test)

(deftest logging-rpc-dispatch
  (testing "process-json-line emits debug logs when level=debug"
    (let ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")
          (sink (make-string-output-stream)))
      (let ((*log-level* :debug)
            (*log-stream* sink))
        (ok (stringp (process-json-line req)))
        (let ((s (get-output-stream-string sink)))
          (ok (> (length s) 0))
          (ok (search "\"event\":\"rpc.dispatch\"" s))
          (ok (search "\"event\":\"rpc.result\"" s)))))))
