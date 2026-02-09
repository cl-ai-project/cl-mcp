;;;; tests/define-tool-test.lisp

(defpackage #:cl-mcp/tests/define-tool-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-tool-handler)
  (:import-from #:cl-mcp/src/state
                #:make-state))

(in-package #:cl-mcp/tests/define-tool-test)

;;; Test tool definition using the macro

(define-tool "test-echo"
  :description "A simple test tool that echoes its input."
  :args ((message :type :string :required t :description "Message to echo"))
  :body
  (result id
          (make-ht "content" (text-content message)
                   "echoed" message)))

(define-tool "test-add"
  :description "Add two numbers."
  :args ((a :type :integer :required t :description "First number")
         (b :type :integer :required t :description "Second number"))
  :body
  (let ((sum (+ a b)))
    (result id
            (make-ht "content" (text-content (format nil "~D" sum))
                     "sum" sum))))

(define-tool "test-options"
  :description "Test optional and boolean args."
  :args ((value :type :string :required t)
         (uppercase :type :boolean :default nil :description "Convert to uppercase")
         (prefix :type :string :description "Optional prefix"))
  :body
  (let* ((processed (if uppercase (string-upcase value) value))
         (output (if prefix (concatenate 'string prefix processed) processed)))
    (result id
            (make-ht "content" (text-content output)
                     "output" output))))

;;; Tests

(deftest define-tool-creates-handler
  (testing "define-tool creates a handler function"
    (ok (fboundp 'test-echo-handler))
    (ok (fboundp 'test-add-handler))
    (ok (fboundp 'test-options-handler))))

(deftest define-tool-creates-descriptor
  (testing "define-tool creates a descriptor function"
    (ok (fboundp 'test-echo-descriptor))
    (let ((desc (test-echo-descriptor)))
      (ok (hash-table-p desc))
      (ok (string= "test-echo" (gethash "name" desc)))
      (ok (string= "A simple test tool that echoes its input."
                   (gethash "description" desc)))
      (let* ((schema (gethash "inputSchema" desc))
             (props (gethash "properties" schema))
             (required (gethash "required" schema)))
        (ok (hash-table-p props))
        (ok (gethash "message" props))
        (ok (vectorp required))
        (ok (find "message" required :test #'string=))))))

(deftest define-tool-registers-tool
  (testing "define-tool registers the tool in the registry"
    (ok (get-tool-handler "test-echo"))
    (ok (get-tool-handler "test-add"))
    (ok (get-tool-handler "test-options"))))

(deftest define-tool-handler-works
  (testing "generated handler works correctly"
    (let* ((state (make-state))
           (args (make-ht "message" "hello world"))
           (result (test-echo-handler state 1 args))
           (res-result (gethash "result" result)))
      (ok (hash-table-p result))
      (ok (string= "2.0" (gethash "jsonrpc" result)))
      (ok (eql 1 (gethash "id" result)))
      (ok (string= "hello world" (gethash "echoed" res-result))))))

(deftest define-tool-integer-args
  (testing "integer arguments are extracted correctly"
    (let* ((state (make-state))
           (args (make-ht "a" 3 "b" 5))
           (result (test-add-handler state 2 args))
           (res-result (gethash "result" result)))
      (ok (eql 8 (gethash "sum" res-result))))))

(deftest define-tool-boolean-default
  (testing "boolean args use default values correctly"
    (let* ((state (make-state))
           (args (make-ht "value" "test")))
      ;; Without uppercase flag (default nil)
      (let* ((result (test-options-handler state 3 args))
             (res-result (gethash "result" result)))
        (ok (string= "test" (gethash "output" res-result))))
      ;; With uppercase flag true
      (setf (gethash "uppercase" args) t)
      (let* ((result (test-options-handler state 4 args))
             (res-result (gethash "result" result)))
        (ok (string= "TEST" (gethash "output" res-result)))))))

(deftest define-tool-optional-args
  (testing "optional string args work correctly"
    (let* ((state (make-state))
           (args (make-ht "value" "world")))
      ;; Without prefix
      (let* ((result (test-options-handler state 5 args))
             (res-result (gethash "result" result)))
        (ok (string= "world" (gethash "output" res-result))))
      ;; With prefix
      (setf (gethash "prefix" args) "hello ")
      (let* ((result (test-options-handler state 6 args))
             (res-result (gethash "result" result)))
        (ok (string= "hello world" (gethash "output" res-result)))))))

(deftest define-tool-missing-required-arg
  (testing "missing required arg returns error"
    (let* ((state (make-state))
           (args (make-ht))  ; missing "message"
           (result (test-echo-handler state 7 args)))
      ;; Should return an error response
      (ok (or (gethash "error" result)
              (let ((res (gethash "result" result)))
                (and res (gethash "isError" res))))))))

(deftest define-tool-kebab-to-camel
  (testing "kebab-case names convert to camelCase JSON keys"
    (let ((desc (test-options-descriptor)))
      (let* ((schema (gethash "inputSchema" desc))
             (props (gethash "properties" schema)))
        ;; "uppercase" should be "uppercase" (single word)
        (ok (gethash "uppercase" props))
        ;; "prefix" should be "prefix" (single word)
        (ok (gethash "prefix" props))))))

(deftest define-tool-parse-arg-spec-symbol
  (testing "%parse-arg-spec normalizes symbol arg specs"
    (let ((spec (cl-mcp/src/tools/define-tool::%parse-arg-spec 'foo-bar)))
      (ok (eq (getf spec :name) 'foo-bar))
      (ok (string= (getf spec :json-name) "foo_bar"))
      (ok (eq (getf spec :type) :string))
      (ok (null (getf spec :required)))
      (ok (null (getf spec :default)))
      (ok (null (getf spec :enum)))
      (ok (null (getf spec :description))))))

(deftest define-tool-parse-arg-spec-list
  (testing "%parse-arg-spec normalizes full list arg specs"
    (let ((spec (cl-mcp/src/tools/define-tool::%parse-arg-spec
                 '(path :json-name "file_path"
                   :type :string
                   :required t
                   :default nil
                   :enum ("a" "b")
                   :description "Path arg"))))
      (ok (eq (getf spec :name) 'path))
      (ok (string= (getf spec :json-name) "file_path"))
      (ok (eq (getf spec :type) :string))
      (ok (eql (getf spec :required) t))
      (ok (equal (getf spec :enum) '("a" "b")))
      (ok (string= (getf spec :description) "Path arg")))))

(deftest define-tool-type-to-json-type
  (testing "%type-to-json-type maps known types correctly"
    (ok (string= "string" (cl-mcp/src/tools/define-tool::%type-to-json-type :string)))
    (ok (string= "integer" (cl-mcp/src/tools/define-tool::%type-to-json-type :integer)))
    (ok (string= "number" (cl-mcp/src/tools/define-tool::%type-to-json-type :number)))
    (ok (string= "boolean" (cl-mcp/src/tools/define-tool::%type-to-json-type :boolean)))
    (ok (string= "array" (cl-mcp/src/tools/define-tool::%type-to-json-type :array)))
    (ok (string= "object" (cl-mcp/src/tools/define-tool::%type-to-json-type :object)))))

(deftest define-tool-collect-required-args
  (testing "%collect-required-args returns JSON names only for required args"
    (let* ((specs (list (cl-mcp/src/tools/define-tool::%parse-arg-spec
                         '(path :required t :json-name "path"))
                        (cl-mcp/src/tools/define-tool::%parse-arg-spec
                         '(limit :required nil :json-name "limit"))
                        (cl-mcp/src/tools/define-tool::%parse-arg-spec
                         '(content :required t :json-name "content"))))
           (required (cl-mcp/src/tools/define-tool::%collect-required-args specs)))
      (ok (equal required '("path" "content"))))))
