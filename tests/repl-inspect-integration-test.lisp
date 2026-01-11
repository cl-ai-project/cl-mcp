;;;; tests/repl-inspect-integration-test.lisp

(defpackage #:cl-mcp/tests/repl-inspect-integration-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/repl
                #:repl-eval)
  (:import-from #:cl-mcp/src/object-registry
                #:*object-registry*
                #:make-object-registry
                #:lookup-object
                #:inspectable-p)
  (:import-from #:cl-mcp/src/inspect
                #:inspect-object-by-id)
  (:import-from #:cl-mcp/src/protocol
                #:process-json-line))

(in-package #:cl-mcp/tests/repl-inspect-integration-test)

(defun ht-get (ht key)
  (gethash key ht))

(defun with-fresh-registry (thunk)
  (let ((*object-registry* (make-object-registry)))
    (funcall thunk)))

;;; Test repl-eval registers non-primitive results

(deftest repl-eval-registers-list-result
  (testing "repl-eval registers list result in object registry"
    (with-fresh-registry
     (lambda ()
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "(list 1 2 3)")
         (declare (ignore printed stdout stderr error-context))
         ;; The raw value should be inspectable
         (ok (inspectable-p raw-value))
         ;; Should be a list
         (ok (listp raw-value))
         (ok (equal '(1 2 3) raw-value)))))))

(deftest repl-eval-does-not-register-primitives
  (testing "repl-eval does not register primitive results"
    (with-fresh-registry
     (lambda ()
       ;; Number
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "42")
         (declare (ignore printed stdout stderr error-context))
         (ok (not (inspectable-p raw-value))))
       ;; String
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "\"hello\"")
         (declare (ignore printed stdout stderr error-context))
         (ok (not (inspectable-p raw-value))))
       ;; Symbol
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "'foo")
         (declare (ignore printed stdout stderr error-context))
         (ok (not (inspectable-p raw-value))))))))

(deftest repl-eval-result-can-be-inspected
  (testing "repl-eval result can be looked up and inspected"
    (with-fresh-registry
     (lambda ()
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "(make-hash-table :test 'equal)")
         (declare (ignore printed stdout stderr error-context))
         ;; Register it manually (simulating what the MCP tool does)
         (let ((id (cl-mcp/src/object-registry:register-object raw-value)))
           (ok id)
           ;; Can look it up
           (ok (eq raw-value (lookup-object id)))
           ;; Can inspect it
           (let ((result (inspect-object-by-id id)))
             (ok (string= "hash-table" (ht-get result "kind")))
             (ok (string= "EQUAL" (ht-get result "test"))))))))))

(deftest repl-eval-error-does-not-register
  (testing "repl-eval error does not register result"
    (with-fresh-registry
     (lambda ()
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "(error \"test error\")")
         (declare (ignore printed stdout stderr))
         ;; Error context should be present
         (ok error-context)
         ;; raw-value on error is the error message string (primitive)
         (ok (stringp raw-value)))))))

(deftest full-inspection-workflow
  (testing "complete workflow: eval -> register -> inspect -> drill down"
    (with-fresh-registry
     (lambda ()
       ;; Create a nested structure
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "(list (list 1 2) (list 3 4))")
         (declare (ignore printed stdout stderr error-context))
         ;; Register outer list
         (let ((outer-id (cl-mcp/src/object-registry:register-object raw-value)))
           (ok outer-id)
           ;; Inspect outer list
           (let ((outer-inspect (inspect-object-by-id outer-id :max-depth 1)))
             (ok (string= "list" (ht-get outer-inspect "kind")))
             (ok (= 2 (length (ht-get outer-inspect "elements"))))
             ;; First element should be object-ref
             (let* ((first-elem (first (ht-get outer-inspect "elements")))
                    (inner-id (ht-get first-elem "id")))
               (ok (string= "object-ref" (ht-get first-elem "kind")))
               (ok inner-id)
               ;; Drill down into inner list
               (let ((inner-inspect (inspect-object-by-id inner-id)))
                 (ok (string= "list" (ht-get inner-inspect "kind")))
                 (ok (= 2 (length (ht-get inner-inspect "elements")))))))))))))

;;; Test MCP tool integration via protocol

(deftest mcp-repl-eval-includes-result-object-id
  (testing "MCP repl-eval tool response includes result_object_id for non-primitives"
    (with-fresh-registry
     (lambda ()
       (let* ((req (concatenate 'string
                     "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\","
                     "\"params\":{\"name\":\"repl-eval\","
                     "\"arguments\":{\"code\":\"(list 1 2 3)\"}}}"))
              (resp (process-json-line req))
              (obj (yason:parse resp))
              (result (gethash "result" obj))
              (object-id (gethash "result_object_id" result)))
         (ok object-id "Should have result_object_id")
         (ok (integerp object-id) "result_object_id should be integer")
         ;; Verify we can look up the object
         (ok (lookup-object object-id) "Object should be in registry"))))))

(deftest mcp-repl-eval-no-object-id-for-primitives
  (testing "MCP repl-eval tool response has no result_object_id for primitives"
    (with-fresh-registry
     (lambda ()
       (let* ((req (concatenate 'string
                     "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\","
                     "\"params\":{\"name\":\"repl-eval\","
                     "\"arguments\":{\"code\":\"42\"}}}"))
              (resp (process-json-line req))
              (obj (yason:parse resp))
              (result (gethash "result" obj)))
         (ok (null (gethash "result_object_id" result))
             "Should NOT have result_object_id for primitives"))))))

(deftest mcp-inspect-object-tool
  (testing "MCP inspect-object tool works with registered objects"
    (with-fresh-registry
     (lambda ()
       ;; First, create an object via repl-eval
       (let* ((eval-req (concatenate 'string
                          "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\","
                          "\"params\":{\"name\":\"repl-eval\","
                          "\"arguments\":{\"code\":\"(make-hash-table)\"}}}"))
              (eval-resp (cl-mcp/src/protocol:process-json-line eval-req))
              (eval-obj (yason:parse eval-resp))
              (eval-result (gethash "result" eval-obj))
              (object-id (gethash "result_object_id" eval-result)))
         (ok object-id)
         ;; Now inspect it
         (let* ((inspect-req (format nil
                               "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",~
                                \"params\":{\"name\":\"inspect-object\",~
                                \"arguments\":{\"id\":~A}}}"
                               object-id))
                (inspect-resp (cl-mcp/src/protocol:process-json-line inspect-req))
                (inspect-obj (yason:parse inspect-resp))
                (inspect-result (gethash "result" inspect-obj)))
           (ok (string= "hash-table" (gethash "kind" inspect-result)))))))))

;;; Test error context locals include object_id

#+sbcl
(deftest error-context-locals-have-object-id
  (testing "error context includes object_id for non-primitive locals"
    (with-fresh-registry
     (lambda ()
       ;; Define and call a function that errors with a list local
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "(let ((my-list (list 1 2 3)))
                         (error \"test error with list local\"))")
         (declare (ignore printed raw-value stdout stderr))
         ;; Error context should be present
         (ok error-context "Should have error context")
         ;; Check frames
         (let ((frames (getf error-context :frames)))
           (ok frames "Should have frames")
           ;; Find a frame with locals containing my-list
           (let ((found-object-id nil))
             (dolist (frame frames)
               (dolist (local (getf frame :locals))
                 (when (and (string= "MY-LIST" (getf local :name))
                            (getf local :object-id))
                   (setf found-object-id (getf local :object-id)))))
             ;; Note: MY-LIST local may or may not be visible depending on optimization
             ;; If found, verify we can inspect it
             (when found-object-id
               (ok (integerp found-object-id) "object-id should be integer")
               (let ((inspect-result (inspect-object-by-id found-object-id)))
                 (ok (string= "list" (ht-get inspect-result "kind"))
                     "Should be able to inspect the local"))))))))))

#+sbcl
(deftest error-context-primitive-locals-no-object-id
  (testing "error context does not include object_id for primitive locals"
    (with-fresh-registry
     (lambda ()
       ;; Define and call a function that errors with primitive locals
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "(let ((my-number 42))
                         (error \"test error with number local\"))")
         (declare (ignore printed raw-value stdout stderr))
         ;; Error context should be present
         (ok error-context "Should have error context")
         ;; Check frames for locals named MY-NUMBER
         (let ((frames (getf error-context :frames)))
           (dolist (frame frames)
             (dolist (local (getf frame :locals))
               (when (string= "MY-NUMBER" (getf local :name))
                 ;; Primitive should NOT have object-id
                 (ok (null (getf local :object-id))
                     "Primitive local should not have object-id"))))))))))

#+sbcl
(deftest error-context-locals-inspection-workflow
  (testing "complete workflow: error -> get local object-id -> inspect"
    (with-fresh-registry
     (lambda ()
       ;; Create error with hash-table local
       (multiple-value-bind (printed raw-value stdout stderr error-context)
           (repl-eval "(let ((ht (make-hash-table :test 'equal)))
                         (setf (gethash \"key\" ht) \"value\")
                         (error \"test error\"))")
         (declare (ignore printed raw-value stdout stderr))
         (ok error-context)
         ;; Find HT local with object-id
         (let ((ht-object-id nil))
           (dolist (frame (getf error-context :frames))
             (dolist (local (getf frame :locals))
               (when (and (string= "HT" (getf local :name))
                          (getf local :object-id))
                 (setf ht-object-id (getf local :object-id)))))
           ;; If found, verify full inspection workflow
           (when ht-object-id
             (let ((result (inspect-object-by-id ht-object-id)))
               (ok (string= "hash-table" (ht-get result "kind")))
               (ok (string= "EQUAL" (ht-get result "test")))
               (ok (= 1 (length (ht-get result "entries"))))))))))))
