;;;; tests/protocol-test.lisp

(defpackage #:cl-mcp/tests/protocol-test
  (:use #:cl #:rove)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:temporary-directory
                #:delete-directory-tree)
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
          (ok (hash-table-p (gethash "tools" caps)))
          (ok (hash-table-p (gethash "prompts" caps))))))))

(deftest initialized-notification
  (testing "notifications/initialized returns no response"
    (let ((line (concatenate
                 'string
                 "{\"jsonrpc\":\"2.0\","
                 "\"method\":\"notifications/initialized\","
                 "\"params\":{\"protocolVersion\":\"2025-06-18\"}}")))
      (ok (null (process-json-line line))))))

(deftest prompts-list-returns-known-prompt
  (testing "prompts/list returns prompt descriptors from prompts directory"
    (let* ((resp (process-json-line
                  "{\"jsonrpc\":\"2.0\",\"id\":50,\"method\":\"prompts/list\",\"params\":{}}"))
           (obj (parse resp))
           (result (gethash "result" obj))
           (prompts (gethash "prompts" result)))
      (ok (string= (gethash "jsonrpc" obj) "2.0"))
      (ok (eql (gethash "id" obj) 50))
      (ok (vectorp prompts))
      (ok (> (length prompts) 0))
      (ok (find "repl-driven-development"
                (coerce prompts 'list)
                :test #'string=
                :key (lambda (prompt)
                       (gethash "name" prompt)))))))

(deftest prompts-get-returns-user-message
  (testing "prompts/get returns prompt body as user text message"
    (let* ((resp (process-json-line
                  (concatenate
                   'string
                   "{\"jsonrpc\":\"2.0\",\"id\":51,\"method\":\"prompts/get\","
                   "\"params\":{\"name\":\"repl-driven-development\"}}")))
           (obj (parse resp))
           (result (gethash "result" obj))
           (messages (gethash "messages" result))
           (first-message (and (vectorp messages)
                               (> (length messages) 0)
                               (aref messages 0)))
           (content (and first-message (gethash "content" first-message))))
      (ok (string= (gethash "jsonrpc" obj) "2.0"))
      (ok (eql (gethash "id" obj) 51))
      (ok (stringp (gethash "description" result)))
      (ok (vectorp messages))
      (ok (string= (gethash "role" first-message) "user"))
      (ok (string= (gethash "type" content) "text"))
      (ok (search "Common Lisp REPL-Driven Development Assistant"
                  (gethash "text" content))))))

(deftest prompts-get-missing-name-returns-invalid-params
  (testing "prompts/get validates required name parameter"
    (let* ((resp (process-json-line
                  "{\"jsonrpc\":\"2.0\",\"id\":52,\"method\":\"prompts/get\",\"params\":{}}"))
           (obj (parse resp))
           (err (gethash "error" obj)))
      (ok (string= (gethash "jsonrpc" obj) "2.0"))
      (ok (eql (gethash "id" obj) 52))
      (ok (= (gethash "code" err) -32602))
      (ok (search "name" (gethash "message" err))))))

(deftest prompts-get-unknown-name-returns-invalid-params
  (testing "prompts/get returns error when prompt name is unknown"
    (let* ((resp (process-json-line
                  (concatenate
                   'string
                   "{\"jsonrpc\":\"2.0\",\"id\":53,\"method\":\"prompts/get\","
                   "\"params\":{\"name\":\"definitely-missing-prompt\"}}")))
           (obj (parse resp))
           (err (gethash "error" obj)))
      (ok (string= (gethash "jsonrpc" obj) "2.0"))
      (ok (eql (gethash "id" obj) 53))
      (ok (= (gethash "code" err) -32602))
      (ok (search "not found" (gethash "message" err))))))

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

(deftest prompts-directory-uses-bundled-path
  (testing "%prompts-directory resolves to cl-mcp bundled prompts"
    (let* ((actual (cl-mcp/src/protocol::%prompts-directory))
           (expected (merge-pathnames "prompts/"
                                      (ensure-directory-pathname
                                       (system-source-directory "cl-mcp")))))
      (ok (pathnamep actual))
      (ok (equal (namestring (truename actual))
                 (namestring (truename expected)))))))

(deftest prompts-list-ignores-project-root-prompts
  (testing "prompts/list does not include prompts from *project-root*"
    (let* ((tmp (merge-pathnames
                 (format nil "cl-mcp-test-~A/" (get-universal-time))
                 (temporary-directory)))
           (prompts-dir (merge-pathnames "prompts/" tmp))
           (test-file (merge-pathnames "zz-project-only.md" prompts-dir)))
      (ensure-directories-exist test-file)
      (unwind-protect
           (progn
             (with-open-file (s test-file :direction :output :if-exists :supersede)
               (write-string "# Project Only Prompt" s))
             (let* ((cl-mcp/src/project-root:*project-root* tmp)
                    (resp (process-json-line
                           "{\"jsonrpc\":\"2.0\",\"id\":60,\"method\":\"prompts/list\",\"params\":{}}"))
                    (obj (parse resp))
                    (result (gethash "result" obj))
                    (prompts (gethash "prompts" result))
                    (names (map 'list (lambda (p) (gethash "name" p)) prompts)))
               (ok (vectorp prompts))
               (ok (not (find "zz-project-only" names :test #'string=)))))
        (ignore-errors (delete-directory-tree tmp :validate t))))))

(deftest prompts-get-ignores-project-root-shadow
  (testing "prompts/get returns bundled prompt content even when *project-root* has shadow file"
    (let* ((tmp (merge-pathnames
                 (format nil "cl-mcp-test-~A/" (get-universal-time))
                 (temporary-directory)))
           (prompts-dir (merge-pathnames "prompts/" tmp))
           (shadow-file (merge-pathnames "repl-driven-development.md" prompts-dir)))
      (ensure-directories-exist shadow-file)
      (unwind-protect
           (progn
             (with-open-file (s shadow-file :direction :output :if-exists :supersede)
               (write-string "# Shadow Prompt\nPROJECT-OVERRIDE-MARKER" s))
             (let* ((cl-mcp/src/project-root:*project-root* tmp)
                    (resp (process-json-line
                           (concatenate
                            'string
                            "{\"jsonrpc\":\"2.0\",\"id\":61,\"method\":\"prompts/get\","
                            "\"params\":{\"name\":\"repl-driven-development\"}}")))
                    (obj (parse resp))
                    (result (gethash "result" obj))
                    (messages (gethash "messages" result))
                    (first-message (aref messages 0))
                    (content (gethash "content" first-message))
                    (text (gethash "text" content)))
               (ok (search "Common Lisp REPL-Driven Development Assistant" text))
               (ok (not (search "PROJECT-OVERRIDE-MARKER" text)))))
        (ignore-errors (delete-directory-tree tmp :validate t))))))

(deftest prompts-list-returns-all-prompt-files
  (testing "prompts/list names match bundled prompts/*.md file names"
    (let* ((resp (process-json-line
                  "{\"jsonrpc\":\"2.0\",\"id\":70,\"method\":\"prompts/list\",\"params\":{}}"))
           (obj (parse resp))
           (result (gethash "result" obj))
           (prompts (gethash "prompts" result))
           (names (sort (map 'list
                             (lambda (p) (gethash "name" p))
                             prompts)
                        #'string<))
           (prompts-dir (cl-mcp/src/protocol::%prompts-directory))
           (expected (sort (mapcar (lambda (pn)
                                     (string-downcase
                                      (or (pathname-name pn) "")))
                                   (directory (merge-pathnames "*.md" prompts-dir)))
                           #'string<)))
      (ok (= (length prompts) (length expected))
          (format nil "expected ~D prompts, got ~D"
                  (length expected)
                  (length prompts)))
      (ok (equal names expected)
          (format nil "prompt names: ~A" names)))))

(deftest removed-asdf-tools-return-not-found
  (testing "removed asdf tools return method-not-found at tool lookup"
    (dolist (tool-name '("asdf-system-info" "asdf-list-systems"))
      (let* ((req (format nil
                          (concatenate
                           'string
                           "{\"jsonrpc\":\"2.0\",\"id\":80,\"method\":\"tools/call\","
                           "\"params\":{\"name\":\"~A\",\"arguments\":{}}}")
                          tool-name))
             (resp (process-json-line req))
             (obj (parse resp))
             (err (gethash "error" obj)))
        (ok err)
        (ok (= (gethash "code" err) -32601))
        (ok (search tool-name (gethash "message" err)))))))

(deftest namespaced-tool-name-resolves-to-registered-tool
  (testing "tools/call normalizes namespaced tool names"
    (let ((root (ensure-directory-pathname (system-source-directory "cl-mcp"))))
      (let ((cl-mcp/src/project-root:*project-root* root))
        (dolist (tool-name '("ns.fs-read-file" "ns/fs-read-file"))
          (let* ((req (format nil
                              (concatenate
                               'string
                               "{\"jsonrpc\":\"2.0\",\"id\":81,\"method\":\"tools/call\","
                               "\"params\":{\"name\":\"~A\","
                               "\"arguments\":{\"path\":\"src/core.lisp\",\"limit\":16}}}")
                              tool-name))
                 (resp (process-json-line req))
                 (obj (parse resp))
                 (result (gethash "result" obj))
                 (content (and result (gethash "content" result)))
                 (first (and (vectorp content) (> (length content) 0) (aref content 0))))
            (ok (null (gethash "error" obj)))
            (ok (stringp (and first (gethash "text" first))))
            (ok (> (length (gethash "text" first)) 0))))))))

(deftest prompts-get-returns-content-for-each-prompt
  (testing "prompts/get returns valid content for every discovered prompt"
    (let* ((list-resp (process-json-line
                       "{\"jsonrpc\":\"2.0\",\"id\":71,\"method\":\"prompts/list\",\"params\":{}}"))
           (list-obj (parse list-resp))
           (prompts (gethash "prompts" (gethash "result" list-obj))))
      (loop for prompt across prompts
            for name = (gethash "name" prompt)
            for req = (format nil
                        "{\"jsonrpc\":\"2.0\",\"id\":72,\"method\":\"prompts/get\",\"params\":{\"name\":\"~A\"}}"
                        name)
            for resp = (process-json-line req)
            for obj = (parse resp)
            for result = (gethash "result" obj)
            for messages = (gethash "messages" result)
            for first-msg = (and (vectorp messages) (> (length messages) 0)
                                 (aref messages 0))
            for content = (and first-msg (gethash "content" first-msg))
            do (ok result
                   (format nil "~A: result present" name))
               (ok (vectorp messages)
                   (format nil "~A: messages is vector" name))
               (ok (string= (gethash "role" first-msg) "user")
                   (format nil "~A: role is user" name))
               (ok (string= (gethash "type" content) "text")
                   (format nil "~A: content type is text" name))
               (ok (> (length (gethash "text" content)) 100)
                   (format nil "~A: content is non-trivial" name))))))
