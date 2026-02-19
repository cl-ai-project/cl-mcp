;;;; tests/protocol-test.lisp

(defpackage #:cl-mcp/tests/protocol-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/protocol
                #:process-json-line
                #:protocol-version
                #:make-state)
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

;;; Protocol version 2025-11-25 tests

(deftest initialize-supports-2025-11-25
  (testing "initialize accepts and echoes protocol version 2025-11-25"
    (let* ((line (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"initialize\","
                  "\"params\":{\"protocolVersion\":\"2025-11-25\"}}"))
           (resp (process-json-line line))
           (obj (parse resp))
           (result (gethash "result" obj)))
      (ok (string= (gethash "protocolVersion" result) "2025-11-25")))))

(deftest protocol-version-stored-in-state
  (testing "initialize stores negotiated protocol version in server state"
    (let* ((state (make-state))
           (line (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"initialize\","
                  "\"params\":{\"protocolVersion\":\"2025-11-25\"}}"))
           (_ (process-json-line line state)))
      (declare (ignore _))
      (ok (string= (protocol-version state) "2025-11-25")))))

(deftest protocol-version-stored-for-older-version
  (testing "initialize stores older protocol version in server state"
    (let* ((state (make-state))
           (line (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"initialize\","
                  "\"params\":{\"protocolVersion\":\"2024-11-05\"}}"))
           (_ (process-json-line line state)))
      (declare (ignore _))
      (ok (string= (protocol-version state) "2024-11-05")))))

(deftest tool-error-old-protocol-returns-json-rpc-error
  (testing "tool input validation error returns JSON-RPC error for old protocol"
    (let* ((state (make-state))
           ;; Initialize with old protocol
           (init-line (concatenate
                       'string
                       "{\"jsonrpc\":\"2.0\",\"id\":20,\"method\":\"initialize\","
                       "\"params\":{\"protocolVersion\":\"2024-11-05\"}}"))
           (_ (process-json-line init-line state))
           ;; Call tool with invalid input (missing path)
           (tool-line (concatenate
                       'string
                       "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/call\","
                       "\"params\":{\"name\":\"fs-read-file\",\"arguments\":{}}}"))
           (resp (process-json-line tool-line state))
           (obj (parse resp))
           (err (gethash "error" obj)))
      (declare (ignore _))
      ;; Should return JSON-RPC Protocol Error (-32602)
      (ok err "Should have error object")
      (ok (= (gethash "code" err) -32602))
      (ok (search "path" (gethash "message" err))))))

(deftest tool-error-new-protocol-returns-tool-execution-error
  (testing "tool input validation error returns Tool Execution Error for 2025-11-25"
    (let* ((state (make-state))
           ;; Initialize with new protocol
           (init-line (concatenate
                       'string
                       "{\"jsonrpc\":\"2.0\",\"id\":30,\"method\":\"initialize\","
                       "\"params\":{\"protocolVersion\":\"2025-11-25\"}}"))
           (_ (process-json-line init-line state))
           ;; Call tool with invalid input (missing path)
           (tool-line (concatenate
                       'string
                       "{\"jsonrpc\":\"2.0\",\"id\":31,\"method\":\"tools/call\","
                       "\"params\":{\"name\":\"fs-read-file\",\"arguments\":{}}}"))
           (resp (process-json-line tool-line state))
           (obj (parse resp))
           (result (gethash "result" obj)))
      (declare (ignore _))
      ;; Should return Tool Execution Error (result with isError: true)
      (ok result "Should have result, not error")
      (ok (gethash "isError" result) "Should have isError flag")
      (ok (eq (gethash "isError" result) t) "isError should be true")
      (ok (gethash "content" result) "Should have content"))))

(deftest tool-error-2025-06-18-returns-json-rpc-error
  (testing "tool input validation error returns JSON-RPC error for 2025-06-18"
    (let* ((state (make-state))
           ;; Initialize with 2025-06-18 protocol (before breaking change)
           (init-line (concatenate
                       'string
                       "{\"jsonrpc\":\"2.0\",\"id\":40,\"method\":\"initialize\","
                       "\"params\":{\"protocolVersion\":\"2025-06-18\"}}"))
           (_ (process-json-line init-line state))
           ;; Call tool with invalid input
           (tool-line (concatenate
                       'string
                       "{\"jsonrpc\":\"2.0\",\"id\":41,\"method\":\"tools/call\","
                       "\"params\":{\"name\":\"fs-list-directory\",\"arguments\":{}}}"))
           (resp (process-json-line tool-line state))
           (obj (parse resp))
           (err (gethash "error" obj)))
      (declare (ignore _))
      ;; 2025-06-18 is before 2025-11-25, should use old format
      (ok err "Should have error object")
      (ok (= (gethash "code" err) -32602)))))

;;; JSON encoding resilience tests

(deftest process-json-line-valid-json-with-control-chars
  (testing "process-json-line produces valid JSON when tool response contains control chars"
    (let* ((code (format nil "(format nil \"result~~C~~Cend\" (code-char 7) (code-char 127))"))
           (req (format nil
                        (concatenate
                         'string
                         "{\"jsonrpc\":\"2.0\",\"id\":90,\"method\":\"tools/call\","
                         "\"params\":{\"name\":\"repl-eval\","
                         "\"arguments\":{\"code\":~S}}}")
                        code))
           (resp (process-json-line req))
           (obj (parse resp)))
      (ok obj "response should parse as valid JSON")
      (ok (null (gethash "error" obj)) "should not be an error response")
      (let* ((result (gethash "result" obj))
             (content (gethash "content" result))
             (text (and (vectorp content) (> (length content) 0)
                        (gethash "text" (aref content 0)))))
        (ok (stringp text) "should have text content")
        ;; Verify no control chars in the JSON text
        (ok (not (find-if (lambda (c)
                            (or (and (< (char-code c) 32)
                                     (not (member c '(#\Tab #\Newline #\Return))))
                                (= (char-code c) 127)))
                          text))
            "text should not contain control chars or DEL")))))

(deftest encode-json-fallback-produces-valid-json
  (testing "%encode-json returns hardcoded error JSON for unencodable objects"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           ;; Pass an object yason cannot encode: a hash-table with a symbol value
           (ht (make-hash-table :test 'equal))
           (_ (setf (gethash "data" ht) (make-condition 'simple-error)))
           (result (funcall encode-fn ht)))
      (declare (ignore _))
      (ok (stringp result) "should return a string")
      ;; Should be valid JSON (the hardcoded fallback)
      (let ((obj (parse result)))
        (ok obj "result should parse as valid JSON")
        ;; The fallback is a JSON-RPC error response
        (ok (gethash "error" obj)
            "fallback should be a JSON-RPC error response")
        (ok (= (gethash "code" (gethash "error" obj)) -32603)
            "error code should be -32603")))))

(deftest encode-json-fallback-preserves-request-id
  (testing "%encode-json fallback preserves integer id from the response object"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (ht (make-hash-table :test 'equal))
           (_ (progn
                (setf (gethash "jsonrpc" ht) "2.0")
                (setf (gethash "id" ht) 42)
                (setf (gethash "result" ht) (make-condition 'simple-error))))
           (result (funcall encode-fn ht)))
      (declare (ignore _))
      (ok (stringp result) "should return a string")
      (let ((obj (parse result)))
        (ok obj "result should parse as valid JSON")
        (ok (= (gethash "id" obj) 42)
            "fallback should preserve the integer request id")
        (ok (gethash "error" obj)
            "fallback should be a JSON-RPC error response")))))

(deftest encode-json-fallback-preserves-fractional-id
  (testing "%encode-json fallback preserves non-integer numeric id"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (ht (make-hash-table :test 'equal))
           (_ (progn
                (setf (gethash "jsonrpc" ht) "2.0")
                (setf (gethash "id" ht) 1.5)
                (setf (gethash "result" ht) (make-condition 'simple-error))))
           (result (funcall encode-fn ht)))
      (declare (ignore _))
      (ok (stringp result) "should return a string")
      (let ((obj (parse result)))
        (ok obj "result should parse as valid JSON")
        (ok (= (gethash "id" obj) 1.5)
            "fallback should preserve the fractional numeric id")
        (ok (gethash "error" obj)
            "fallback should be a JSON-RPC error response")))))

(deftest encode-json-fallback-preserves-string-id
  (testing "%encode-json fallback preserves string id from the response object"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (ht (make-hash-table :test 'equal))
           (_ (progn
                (setf (gethash "jsonrpc" ht) "2.0")
                (setf (gethash "id" ht) "req-7")
                (setf (gethash "result" ht) (make-condition 'simple-error))))
           (result (funcall encode-fn ht)))
      (declare (ignore _))
      (ok (stringp result) "should return a string")
      (let ((obj (parse result)))
        (ok obj "result should parse as valid JSON")
        (ok (string= (gethash "id" obj) "req-7")
            "fallback should preserve the string request id")
        (ok (gethash "error" obj)
            "fallback should be a JSON-RPC error response"))))
  (testing "string id with special characters is properly escaped"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (ht (make-hash-table :test 'equal))
           (_ (progn
                (setf (gethash "jsonrpc" ht) "2.0")
                (setf (gethash "id" ht) "req\"special\\id")
                (setf (gethash "result" ht) (make-condition 'simple-error))))
           (result (funcall encode-fn ht)))
      (declare (ignore _))
      (ok (stringp result) "should return a string")
      (let ((obj (parse result)))
        (ok obj "result with special chars should parse as valid JSON")
        (ok (string= (gethash "id" obj) "req\"special\\id")
            "fallback should preserve string id with special chars"))))
  (testing "string id with whitespace control characters is properly escaped"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (ht (make-hash-table :test 'equal))
           (id-with-ws (format nil "req~Ctab~Cnl~Ccr" #\Tab #\Newline #\Return))
           (_ (progn
                (setf (gethash "jsonrpc" ht) "2.0")
                (setf (gethash "id" ht) id-with-ws)
                (setf (gethash "result" ht) (make-condition 'simple-error))))
           (result (funcall encode-fn ht)))
      (declare (ignore _))
      (ok (stringp result) "should return a string")
      ;; Raw JSON must not contain literal control whitespace
      (ok (not (find #\Tab result))
          "raw JSON should not contain literal tab")
      (ok (not (find #\Newline result))
          "raw JSON should not contain literal newline")
      (ok (not (find #\Return result))
          "raw JSON should not contain literal CR")
      ;; Must contain proper escape sequences
      (ok (search "\\t" result)
          "raw JSON should contain \\t escape")
      (ok (search "\\n" result)
          "raw JSON should contain \\n escape")
      (ok (search "\\r" result)
          "raw JSON should contain \\r escape")
      (let ((obj (parse result)))
        (ok obj "result with whitespace control chars should parse as valid JSON")
        (ok (string= (gethash "id" obj) id-with-ws)
            "fallback should round-trip string id with whitespace chars")))))
