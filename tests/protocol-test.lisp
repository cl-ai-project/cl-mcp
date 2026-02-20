;;;; tests/protocol-test.lisp

(defpackage #:cl-mcp/tests/protocol-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/protocol
                #:process-json-line
                #:protocol-version
                #:make-state)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
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
  (testing "%encode-json level-2 produces valid JSON for unencodable objects"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           ;; Pass an object yason cannot encode: a hash-table with a condition value
           (ht (make-hash-table :test 'equal))
           (_ (setf (gethash "data" ht) (make-condition 'simple-error)))
           (result (funcall encode-fn ht)))
      (declare (ignore _))
      (ok (stringp result) "should return a string")
      ;; Should be valid JSON (level-2 sanitization succeeds now)
      (let ((obj (parse result)))
        (ok obj "result should parse as valid JSON")
        ;; Level-2 succeeded: the condition was converted to a string
        (ok (stringp (gethash "data" obj))
            "condition value should be converted to string by sanitizer")))))

(deftest encode-json-fallback-preserves-request-id
  (testing "%encode-json level-2 preserves structure with integer id"
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
            "should preserve the integer request id")
        (ok (gethash "result" obj)
            "should have result key (level-2 succeeded)")
        (ok (stringp (gethash "result" obj))
            "condition in result should be converted to string")))))

(deftest encode-json-fallback-preserves-fractional-id
  (testing "%encode-json level-2 preserves non-integer numeric id"
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
            "should preserve the fractional numeric id")
        (ok (gethash "result" obj)
            "should have result key (level-2 succeeded)")
        (ok (stringp (gethash "result" obj))
            "condition in result should be converted to string")))))

(deftest encode-json-fallback-preserves-string-id
  (testing "%encode-json level-2 preserves string id from the response object"
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
            "should preserve the string request id")
        (ok (gethash "result" obj)
            "should have result key (level-2 succeeded)"))))
  (testing "string id with special characters is preserved at level-2"
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
            "should preserve string id with special chars"))))
  (testing "string id with whitespace control characters is preserved at level-2"
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
      (let ((obj (parse result)))
        (ok obj "result with whitespace control chars should parse as valid JSON")
        (ok (string= (gethash "id" obj) id-with-ws)
            "should round-trip string id with whitespace chars")))))

(deftest sanitize-for-encoding-handles-cyclic-list
  (testing "%sanitize-for-encoding terminates on cyclic cons list"
    (let* ((sanitize-fn (find-symbol "%SANITIZE-FOR-ENCODING"
                                     :cl-mcp/src/protocol))
           (cell (list "a" "b" "c")))
      ;; Make it cyclic: (nconc cell cell) → infinite loop for mapcar
      (setf (cdr (last cell)) cell)
      ;; Must terminate and return a list (capped at +sanitize-max-elements+)
      (let ((result (funcall sanitize-fn cell)))
        (ok (listp result) "should return a list")
        (ok (<= (length result) 10000)
            "result should be bounded in length")))))

(deftest sanitize-for-encoding-converts-non-serializable-to-string
  (testing "%sanitize-for-encoding converts conditions to their string representation"
    (let* ((sanitize-fn (find-symbol "%SANITIZE-FOR-ENCODING"
                                     :cl-mcp/src/protocol))
           (cond-obj (make-condition 'simple-error
                                     :format-control "test error"))
           (result (funcall sanitize-fn cond-obj)))
      (ok (stringp result) "condition should be converted to string")))
  (testing "numbers, t, and nil pass through unchanged"
    (let ((sanitize-fn (find-symbol "%SANITIZE-FOR-ENCODING"
                                    :cl-mcp/src/protocol)))
      (ok (= 42 (funcall sanitize-fn 42)) "integer passes through")
      (ok (eq t (funcall sanitize-fn t)) "t passes through")
      (ok (null (funcall sanitize-fn nil)) "nil passes through"))))

(deftest encode-json-level2-succeeds-for-non-serializable
  (testing "%encode-json level-2 succeeds when sanitizer converts objects to strings"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (ht (make-hash-table :test 'equal)))
      (setf (gethash "jsonrpc" ht) "2.0")
      (setf (gethash "id" ht) 99)
      (setf (gethash "result" ht)
            (make-ht "data" (make-condition 'simple-error
                                            :format-control "oops")))
      (let* ((json (funcall encode-fn ht))
             (obj (parse json)))
        (ok (stringp json) "should return a string")
        (ok obj "should be valid JSON")
        ;; Level-2 succeeded: result key present, not the hardcoded error fallback
        (ok (gethash "result" obj)
            "should have result key (level-2 succeeded, not level-3 fallback)")
        (ok (null (gethash "error" obj))
            "should NOT be the hardcoded error fallback")))))

(deftest encode-json-preserves-string-id-with-control-chars
  (testing "level-2 fallback preserves string ID containing \\b and \\f"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (ht (make-hash-table :test 'equal))
           (id-with-ctrl (format nil "req~Cbs~Cff" #\Backspace #\Page)))
      (setf (gethash "jsonrpc" ht) "2.0")
      (setf (gethash "id" ht) id-with-ctrl)
      ;; Non-serializable in result forces level-1 to fail, level-2 sanitizes
      (setf (gethash "result" ht)
            (make-ht "data" (make-condition 'simple-error
                                            :format-control "oops")))
      (let ((json (funcall encode-fn ht)))
        (ok (stringp json) "should return a string")
        (let ((obj (parse json)))
          (ok obj "should be valid JSON")
          ;; ID must round-trip: yason encodes \b and \f as JSON escapes
          (ok (string= id-with-ctrl (gethash "id" obj))
              "ID must match original including \\b and \\f"))))))

(deftest encode-json-level3-fallback-produces-valid-error
  (testing "level-3 hardcoded error response when both level-1 and level-2 fail"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (sanitize-fn-sym (find-symbol "%SANITIZE-FOR-ENCODING"
                                         :cl-mcp/src/protocol))
           (orig-fn (fdefinition sanitize-fn-sym))
           (ht (make-hash-table :test 'equal)))
      (setf (gethash "jsonrpc" ht) "2.0")
      (setf (gethash "id" ht) 99)
      ;; Symbol value fails yason:encode at level-1 (not a JSON type).
      ;; %sanitize-for-encoding is replaced below, so level-2 also fails.
      (setf (gethash "result" ht) :not-json-encodable)
      ;; Temporarily replace %sanitize-for-encoding to force level-2 failure,
      ;; causing %encode-json to fall through to level-3 hardcoded response.
      (unwind-protect
          (progn
            (setf (fdefinition sanitize-fn-sym)
                  (lambda (obj &optional depth)
                    (declare (ignore obj depth))
                    (error "forced sanitize failure for testing")))
            (let ((json (funcall encode-fn ht)))
              (ok (stringp json) "should return a string")
              (let ((obj (parse json)))
                (ok obj "should parse as valid JSON")
                (ok (eql 99 (gethash "id" obj))
                    "should preserve the integer request id")
                (ok (gethash "error" obj) "should have error object")
                (ok (= -32603 (gethash "code" (gethash "error" obj)))
                    "should have internal error code -32603")
                (ok (search "encoding failed"
                            (gethash "message" (gethash "error" obj)))
                    "should have encoding failure message"))))
        (setf (fdefinition sanitize-fn-sym) orig-fn)))))

(deftest encode-json-level3-preserves-string-id
  (testing "level-3 hardcoded response preserves string ID with control chars"
    (let* ((encode-fn (find-symbol "%ENCODE-JSON" :cl-mcp/src/protocol))
           (sanitize-fn-sym (find-symbol "%SANITIZE-FOR-ENCODING"
                                         :cl-mcp/src/protocol))
           (orig-fn (fdefinition sanitize-fn-sym))
           (ht (make-hash-table :test 'equal))
           (id-with-ctrl (format nil "req~Cbs~Cff" #\Backspace #\Page)))
      (setf (gethash "jsonrpc" ht) "2.0")
      (setf (gethash "id" ht) id-with-ctrl)
      (setf (gethash "result" ht) :not-json-encodable)
      (unwind-protect
          (progn
            (setf (fdefinition sanitize-fn-sym)
                  (lambda (obj &optional depth)
                    (declare (ignore obj depth))
                    (error "forced sanitize failure for testing")))
            (let ((json (funcall encode-fn ht)))
              (ok (stringp json) "should return a string")
              (let ((obj (parse json)))
                (ok obj "should parse as valid JSON")
                ;; ID with \b and \f must survive level-3 escaper
                (ok (string= id-with-ctrl (gethash "id" obj))
                    "should preserve string ID including \\b and \\f")
                (ok (gethash "error" obj) "should have error object"))))
        (setf (fdefinition sanitize-fn-sym) orig-fn)))))
