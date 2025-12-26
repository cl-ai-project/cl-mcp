;;;; tests/integration-test.lisp

(defpackage #:cl-mcp/tests/integration-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:yason #:parse))

(in-package #:cl-mcp/tests/integration-test)

(deftest repl-eval-debug-flow
  (testing "agent fixes wrong arity using code-find and fs-read-file"
    ;; 1) Intentionally call version with a wrong argument to get an error.
    (let* ((req1 (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":100,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"repl-eval\","
                  "\"arguments\":{\"code\":\"(cl-mcp:version :extra)\"}}}"))
           (resp1 (process-json-line req1))
           (obj1 (parse resp1))
           (result1 (gethash "result" obj1))
           (content1 (and result1 (gethash "content" result1)))
           (first1 (and (arrayp content1) (> (length content1) 0) (aref content1 0)))
           (text1 (and first1 (gethash "text" first1))))
      (ok (string= (gethash "jsonrpc" obj1) "2.0"))
      ;; repl-eval returns the error message as text content, not as JSON-RPC error.
      (ok (stringp text1))
      (ok (search "argument" text1)))
    ;; 2) Locate the definition of version.
    (let* ((req2 (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":101,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"code-find\","
                  "\"arguments\":{\"symbol\":\"cl-mcp:version\"}}}"))
           (resp2 (process-json-line req2))
           (obj2 (parse resp2))
           (result2 (gethash "result" obj2)))
      (ok (string= (gethash "jsonrpc" obj2) "2.0"))
      (ok (string= (gethash "path" result2) "src/core.lisp"))
      (ok (integerp (gethash "line" result2))))
    ;; 3) Read the file and confirm the definition has no arguments.
    (let* ((req3 (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":102,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"fs-read-file\","
                  "\"arguments\":{\"path\":\"src/core.lisp\",\"limit\":400}}}"))
           (resp3 (process-json-line req3))
           (obj3 (parse resp3))
           (result3 (gethash "result" obj3))
           (content3 (gethash "content" result3))
           (first3 (and (arrayp content3) (> (length content3) 0) (aref content3 0)))
           (text3 (and first3 (gethash "text" first3))))
      (ok (string= (gethash "jsonrpc" obj3) "2.0"))
      (ok (stringp text3))
      (ok (search "(defun version" text3)))
    ;; 4) Call version correctly (no arguments) and succeed.
    (let* ((req4 (concatenate
                  'string
                  "{\"jsonrpc\":\"2.0\",\"id\":103,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"repl-eval\","
                  "\"arguments\":{\"code\":\"(cl-mcp:version)\"}}}"))
           (resp4 (process-json-line req4))
           (obj4 (parse resp4))
           (result4 (gethash "result" obj4))
           (content4 (and result4 (gethash "content" result4)))
           (first4 (and (arrayp content4) (> (length content4) 0) (aref content4 0)))
           (text4 (and first4 (gethash "text" first4))))
      (ok (string= (gethash "jsonrpc" obj4) "2.0"))
      (ok (stringp text4))
      (ok (search "1.0.0" text4)))))

(deftest fs-write-then-readback
  (testing "write file then read it back via tools/call"
    (let* ((path "tmp-integration-write.txt")
           (req-write
             (format nil
                     (concatenate
                      'string
                      "{\"jsonrpc\":\"2.0\",\"id\":110,\"method\":\"tools/call\","
                      "\"params\":{\"name\":\"fs-write-file\","
                      "\"arguments\":{\"path\":\"~A\",\"content\":\"hello\"}}}")
                     path))
           (req-read
             (format nil
                     (concatenate
                      'string
                      "{\"jsonrpc\":\"2.0\",\"id\":111,\"method\":\"tools/call\","
                      "\"params\":{\"name\":\"fs-read-file\","
                      "\"arguments\":{\"path\":\"~A\"}}}")
                     path)))
      (unwind-protect
           (progn
             (let* ((resp-w (process-json-line req-write))
                    (obj-w (parse resp-w))
                    (res-w (gethash "result" obj-w)))
               (ok (gethash "success" res-w)))
             (let* ((resp-r (process-json-line req-read))
                    (obj-r (parse resp-r))
                    (res-r (gethash "result" obj-r))
                    (content (gethash "content" res-r))
                    (first (and (arrayp content) (> (length content) 0)
                                (aref content 0))))
               (ok (string= (gethash "jsonrpc" obj-r) "2.0"))
               (ok (string= (gethash "type" first) "text"))
               (ok (string= (gethash "text" first) "hello"))))
        (ignore-errors (delete-file path))))))

(deftest list-then-read
  (testing "list directory then read discovered source file"
    (let* ((req-list (concatenate
                      'string
                      "{\"jsonrpc\":\"2.0\",\"id\":120,\"method\":\"tools/call\","
                      "\"params\":{\"name\":\"fs-list-directory\","
                      "\"arguments\":{\"path\":\".\"}}}"))
           (resp-list (process-json-line req-list))
           (obj-list (parse resp-list))
           (entries (gethash "entries" (gethash "result" obj-list)))
           (src-dir (find-if (lambda (e) (string= (gethash "name" e) "src"))
                             entries))
           ;; read a known file after confirming directory listing works
           (req-read (concatenate
                      'string
                      "{\"jsonrpc\":\"2.0\",\"id\":121,\"method\":\"tools/call\","
                      "\"params\":{\"name\":\"fs-read-file\","
                      "\"arguments\":{\"path\":\"src/core.lisp\",\"limit\":120}}}")))
      (ok (arrayp entries))
      (ok src-dir)
      (let* ((resp (process-json-line req-read))
             (obj (parse resp))
             (res (gethash "result" obj))
             (content (gethash "content" res))
             (first (and (arrayp content) (> (length content) 0)
                         (aref content 0))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (string= (gethash "type" first) "text"))
        (ok (> (length (gethash "text" first)) 0))))))

(deftest namespaced-code-describe
  (testing "namespaced tool name with package parameter"
    (let ((req (concatenate
                'string
                "{\"jsonrpc\":\"2.0\",\"id\":130,\"method\":\"tools/call\","
                "\"params\":{\"name\":\"lisp_mcp.code-describe\","
                "\"arguments\":{\"symbol\":\"cl-mcp:version\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (res (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok res)
        (ok (string= (gethash "type" res) "function"))
        (ok (stringp (gethash "arglist" res)))
        (ok (search "(" (gethash "arglist" res)))))))

(deftest repl-eval-printlength
  (testing "repl-eval honors print_length for long lists"
    (let ((req (concatenate
                'string
                "{\"jsonrpc\":\"2.0\",\"id\":140,\"method\":\"tools/call\","
                "\"params\":{\"name\":\"repl-eval\","
                "\"arguments\":{\"code\":\"(list 1 2 3 4)\",\"print_length\":2}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (res (gethash "result" obj))
             (content (gethash "content" res))
             (first (and (arrayp content) (> (length content) 0)
                         (aref content 0)))
             (text (and first (gethash "text" first))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (stringp text))
        (ok (search "..." text))))))

(deftest fs-read-disallowed-path
  (testing "fs-read-file rejects paths outside allow-list"
    (let ((req (concatenate
                'string
                "{\"jsonrpc\":\"2.0\",\"id\":150,\"method\":\"tools/call\","
                "\"params\":{\"name\":\"fs-read-file\","
                "\"arguments\":{\"path\":\"/etc/hosts\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (err (gethash "error" obj)))
        (ok err)
        (ok (stringp (gethash "message" err)))))))
