;;;; tests/spec-tools-test.lisp
;;;;
;;;; The cl-spec tools driven the way a client drives them: a JSON-RPC line in,
;;;; a JSON-RPC line out, with the worker pool disabled so the call runs in
;;;; this image.  These checks are about the tool surface -- schema, argument
;;;; validation, and the shape of an answer that never reaches cl-spec -- not
;;;; about cl-spec itself, which tests/spec-adapter-report-test.lisp covers
;;;; with stubs and tests/spec-integration-test.lisp covers with the real
;;;; thing.

(defpackage #:cl-mcp/tests/spec-tools-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/tools/spec-entry #:parse-seed-string)
  (:import-from #:yason #:parse))

(in-package #:cl-mcp/tests/spec-tools-test)

(defvar *tools-loaded* nil
  "True once cl-mcp/main has been loaded into this image.")

(defun %ensure-tools ()
  "Load the tool definitions so process-json-line can dispatch to them."
  (unless *tools-loaded*
    (asdf:load-system "cl-mcp/main")
    (setf *tools-loaded* t)))

(defun %call (name arguments-json)
  "Call tool NAME with ARGUMENTS-JSON and return the parsed JSON-RPC object.

The whole object, not its result: argument validation on a protocol version
older than 2025-11-25 answers with a JSON-RPC error rather than a result, and
a helper that returned only the result would hide exactly those messages."
  (%ensure-tools)
  (let ((*use-worker-pool* nil)
        (line (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",~
\"params\":{\"name\":\"~A\",\"arguments\":~A}}" name arguments-json)))
    (parse (process-json-line line))))

(defun %text (object)
  "Return the caller-visible text of a JSON-RPC OBJECT.

A tool answers either with a result whose content carries text, or with a
JSON-RPC error carrying a message.  Both are what the caller reads, so both
are what a test about the message has to look at."
  (let ((result (gethash "result" object))
        (failure (gethash "error" object)))
    (cond
      (failure (or (gethash "message" failure) ""))
      (result (let ((content (gethash "content" result)))
                (or (when (and (vectorp content) (plusp (length content)))
                      (gethash "text" (aref content 0)))
                    "")))
      (t ""))))

(deftest spec-tools-are-registered
  (testing "all three tools appear in tools/list"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response))))
           (names (map 'list (lambda (tool) (gethash "name" tool)) tools)))
      (dolist (name '("spec-symbol" "spec-describe" "spec-check"))
        (ok (member name names :test #'string=)))
      (testing "and each carries an inputSchema"
        (loop for tool across tools
              when (member (gethash "name" tool)
                           '("spec-symbol" "spec-describe" "spec-check")
                           :test #'string=)
                do (ok (hash-table-p (gethash "inputSchema" tool))))))))

(deftest spec-symbol-requires-a-symbol
  (testing "a missing symbol argument is a validation error, not a crash"
    (let ((response (%call "spec-symbol" "{}")))
      (ok (search "symbol" (%text response)))
      (testing "and nothing was answered as if it had succeeded"
        (ok (null (gethash "result" response)))))))

(deftest spec-describe-rejects-an-unknown-kind
  (testing "kind is constrained and the message says what is allowed"
    (let ((result (%call "spec-describe"
                         "{\"kind\":\"generator\",\"name\":\"cl:car\"}")))
      (ok (search "property" (%text result)))
      (ok (search "spec" (%text result))))))

(deftest spec-check-refuses-both-targets
  (testing "property and symbol together is refused with a usable message"
    (let ((result (%call "spec-check"
                         "{\"property\":\"cl:car\",\"symbol\":\"cl:cdr\"}")))
      (ok (search "both" (string-downcase (%text result)))))))

(deftest spec-check-refuses-a-non-numeric-seed
  (testing "a seed that is not decimal digits is rejected before any run"
    (let ((result (%call "spec-check"
                         "{\"property\":\"cl:car\",\"seed\":\"not-a-number\"}")))
      (ok (search "seed" (string-downcase (%text result)))))))

(deftest spec-check-refuses-a-numeric-seed
  (testing "a JSON number is refused at the schema, before any run"
    (let ((response (%call "spec-check"
                           "{\"property\":\"cl:car\",\"seed\":12345}")))
      (ok (search "seed" (string-downcase (%text response))))
      (ok (search "string" (string-downcase (%text response))))))
  (testing "and the entry point refuses one too, for callers below the schema"
    ;; SPEC-CHECK-RESPONSE is exported and the worker handler calls it with a
    ;; params table directly, so the refusal cannot live only in the schema.
    (%ensure-tools)
    (let* ((params (make-hash-table :test #'equal))
           (response (progn (setf (gethash "property" params) "cl:car"
                                  (gethash "seed" params) 12345)
                            (funcall (find-symbol "SPEC-CHECK-RESPONSE"
                                                  "CL-MCP/SRC/TOOLS/SPEC-ENTRY")
                                     params))))
      (ok (string= "invalid-arguments" (gethash "status" response)))
      (ok (search "digits" (string-downcase (gethash "message" response)))))))

(deftest spec-check-refuses-an-empty-seed
  (testing "an empty seed is refused, not treated as absent"
    (let ((response (%call "spec-check"
                           "{\"property\":\"cl:car\",\"seed\":\"\"}")))
      (ok (search "seed" (string-downcase (%text response)))))))

(deftest spec-describe-refuses-a-negative-budget
  (testing "max_chars is validated before anything reads the registry"
    (let ((response (%call "spec-describe"
                           "{\"kind\":\"property\",\"name\":\"cl:car\",\"max_chars\":-1}")))
      (ok (search "max_chars" (%text response)))
      (ok (search "positive" (string-downcase (%text response)))))))

(deftest read-tools-accept-a-timeout
  (testing "spec-symbol and spec-describe take timeout_seconds"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response)))))
      (loop for tool across tools
            when (member (gethash "name" tool) '("spec-symbol" "spec-describe")
                         :test #'string=)
              do (let ((properties (gethash "properties"
                                            (gethash "inputSchema" tool))))
                   (ok (nth-value 1 (gethash "timeout_seconds" properties))))))))

(deftest parse-seed-string-round-trips-a-big-seed
  (testing "a seed beyond JSON's safe integer parses exactly"
    (multiple-value-bind (value message)
        (parse-seed-string "3963993791726803706")
      (ok (null message))
      (ok (= 3963993791726803706 value))))
  (testing "and anything else is refused rather than coerced"
    (dolist (bad '("-1" "1.5" "" "12a" "0x10"))
      (multiple-value-bind (value message) (parse-seed-string bad)
        (ok (null value))
        (ok (stringp message))))))
