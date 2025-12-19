;;;; tests/protocol-test.lisp

(defpackage #:cl-mcp/tests/protocol-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:yason #:parse))

(in-package #:cl-mcp/tests/protocol-test)

(defparameter *init-req*
  (concatenate 'string
               "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\","
               "\"params\":{\"clientInfo\":{\"name\":\"test-client\","
               "\"version\":\"0.1\"}}}"))

(deftest initialize-handshake
  (testing "process-json-line responds with serverInfo and capabilities"
    (let ((resp (process-json-line *init-req*)))
      (ok (stringp resp))
      (let ((obj (parse resp)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (eql (gethash "id" obj) 1))
        (ok (gethash "result" obj))
        (let* ((result (gethash "result" obj))
               (server (gethash "serverInfo" result))
               (caps (gethash "capabilities" result)))
          (ok (stringp (gethash "name" server)))
          (ok (stringp (gethash "version" server)))
          (ok (gethash "tools" caps)))))))

(deftest initialized-notification
  (testing "notifications/initialized returns no response"
    (let ((line (concatenate
                 'string
                 "{\"jsonrpc\":\"2.0\","
                 "\"method\":\"notifications/initialized\","
                 "\"params\":{\"protocolVersion\":\"2025-06-18\"}}")))
      (ok (null (process-json-line line))))))

(deftest initialize-echo-version
  (testing "initialize echoes client protocolVersion when supported"
    (let* ((line (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"initialize\","
                  "\"params\":{\"protocolVersion\":\"2024-11-05\"}}"))
           (resp (process-json-line line))
           (obj (parse resp))
           (result (gethash "result" obj)))
      (ok (string= (gethash "protocolVersion" result) "2024-11-05")))))

(deftest initialize-unsupported-version
  (testing "initialize returns error for unsupported protocolVersion"
    (let* ((line (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"initialize\","
                  "\"params\":{\"protocolVersion\":\"1999-01-01\"}}"))
           (resp (process-json-line line))
           (obj (parse resp))
           (err (gethash "error" obj)))
      (ok (= (gethash "code" err) -32602))
      (ok (gethash "data" err)))))

(deftest ping-returns-empty
  (testing "ping returns empty result object"
    (let* ((resp (process-json-line "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"ping\"}"))
           (obj (parse resp))
           (result (gethash "result" obj)))
      (ok (hash-table-p result))
      (ok (= (hash-table-count result) 0)))))

(deftest blank-lines-are-ignored
  (testing "empty or whitespace-only lines are skipped without error"
    (ok (null (process-json-line "")))
    (ok (null (process-json-line "   ")))))

(deftest invalid-json-returns-parse-error
  (testing "malformed JSON returns -32700 Parse error"
    (let* ((resp (process-json-line "{bad json"))
           (obj (parse resp))
           (err (gethash "error" obj)))
      (ok (= (gethash "code" err) -32700)))))

(deftest unknown-method-returns-not-found
  (testing "unknown method returns -32601"
    (let* ((resp (process-json-line "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"nope\"}"))
           (obj (parse resp))
           (err (gethash "error" obj)))
      (ok (= (gethash "code" err) -32601)))))
