;;;; tests/tools-test.lisp

(defpackage #:cl-mcp/tests/tools-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:uiop #:getcwd #:ensure-directory-pathname)
  (:import-from #:asdf #:system-source-directory)
  (:import-from #:yason #:parse))

(in-package #:cl-mcp/tests/tools-test)

(defmacro with-test-project-root (&body body)
  "Set up project root for protocol-level tests."
  `(let ((original-root cl-mcp/src/project-root:*project-root*)
         (original-cwd (ignore-errors (getcwd)))
         (test-root (or (ignore-errors
                          (ensure-directory-pathname
                           (system-source-directory "cl-mcp")))
                        (ensure-directory-pathname (getcwd)))))
     (unwind-protect
          (progn
            (setf cl-mcp/src/project-root:*project-root* test-root)
            ,@body)
       (setf cl-mcp/src/project-root:*project-root* original-root)
       (when original-cwd
         (ignore-errors (uiop:chdir original-cwd))))))

(defun %tools-list ()
  "Fetch and parse tools/list result."
  (let* ((req (concatenate 'string
                           "{\"jsonrpc\":\"2.0\",\"id\":9001,\"method\":\"tools/list\","
                           "\"params\":{}}"))
         (resp (process-json-line req))
         (obj (parse resp))
         (result (gethash "result" obj)))
    (values obj result (and result (gethash "tools" result)))))

(defun %find-tool-descriptor (tools name)
  "Return tool descriptor by NAME from TOOLS vector."
  (find-if (lambda (tool)
             (string= (gethash "name" tool) name))
           (coerce tools 'list)))

(defun %tool-call-failed-p (obj)
  "Return true when tool call response represents a failure."
  (or (gethash "error" obj)
      (let ((result (gethash "result" obj)))
        (and result (gethash "isError" result)))))

(defun %tool-call-message (obj)
  "Extract a human-readable message from a tool response object OBJ."
  (or (and (gethash "error" obj)
           (gethash "message" (gethash "error" obj)))
      (let* ((result (gethash "result" obj))
             (content (and result (gethash "content" result)))
             (first (and (arrayp content)
                         (> (length content) 0)
                         (aref content 0))))
        (and first (gethash "text" first)))))

(deftest tools-helper-find-tool-descriptor
  (testing "%find-tool-descriptor finds named descriptor from tools vector"
    (let* ((tool-a (make-hash-table :test #'equal))
           (tool-b (make-hash-table :test #'equal))
           (tools (vector tool-a tool-b)))
      (setf (gethash "name" tool-a) "alpha")
      (setf (gethash "name" tool-b) "beta")
      (ok (eq tool-b (%find-tool-descriptor tools "beta")))
      (ok (null (%find-tool-descriptor tools "gamma"))))))

(deftest tools-helper-tool-call-failed-p
  (testing "%tool-call-failed-p detects JSON-RPC and tool-level failures"
    (let* ((jsonrpc-fail (make-hash-table :test #'equal))
           (tool-fail (make-hash-table :test #'equal))
           (tool-fail-result (make-hash-table :test #'equal))
           (ok-resp (make-hash-table :test #'equal))
           (ok-result (make-hash-table :test #'equal)))
      (setf (gethash "error" jsonrpc-fail) (make-hash-table :test #'equal))
      (setf (gethash "isError" tool-fail-result) t)
      (setf (gethash "result" tool-fail) tool-fail-result)
      (setf (gethash "result" ok-resp) ok-result)
      (ok (%tool-call-failed-p jsonrpc-fail))
      (ok (%tool-call-failed-p tool-fail))
      (ok (not (%tool-call-failed-p ok-resp))))))

(deftest tools-helper-tool-call-message
  (testing "%tool-call-message extracts message from error or tool content"
    (let* ((jsonrpc-fail (make-hash-table :test #'equal))
           (jsonrpc-err (make-hash-table :test #'equal))
           (tool-fail (make-hash-table :test #'equal))
           (tool-result (make-hash-table :test #'equal))
           (tool-content (vector (make-hash-table :test #'equal)))
           (empty (make-hash-table :test #'equal)))
      (setf (gethash "message" jsonrpc-err) "rpc failed")
      (setf (gethash "error" jsonrpc-fail) jsonrpc-err)
      (setf (gethash "text" (aref tool-content 0)) "tool failed")
      (setf (gethash "content" tool-result) tool-content)
      (setf (gethash "result" tool-fail) tool-result)
      (ok (string= (%tool-call-message jsonrpc-fail) "rpc failed"))
      (ok (string= (%tool-call-message tool-fail) "tool failed"))
      (ok (null (%tool-call-message empty))))))

(deftest tools-helper-tools-list-shape
  (testing "%tools-list returns parsed tools/list response shape"
    (multiple-value-bind (obj result tools)
        (%tools-list)
      (ok (hash-table-p obj))
      (ok (hash-table-p result))
      (ok (vectorp tools))
      (ok (> (length tools) 0)))))

(deftest tools-call-lisp-read-file
  (testing "tools/call lisp-read-file returns collapsed content"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":15,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"lisp-read-file\","
                   "\"arguments\":{\"path\":\"src/core.lisp\"}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (content (and result (gethash "content" result)))
               (first (and (arrayp content) (> (length content) 0) (aref content 0))))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (string= (gethash "mode" result) "lisp-collapsed"))
          (ok (arrayp content))
          (ok (stringp (gethash "text" first)))
          (ok (search "(defun version" (gethash "text" first)))
          (let ((meta (gethash "meta" result)))
            (ok (hash-table-p meta))
            (ok (>= (gethash "total_forms" meta) 3))))))))

(deftest tools-call-fs-read
  (testing "tools/call fs-read-file returns content"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"fs-read-file\","
                   "\"arguments\":{\"path\":\"src/core.lisp\",\"limit\":10}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (content (gethash "content" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (arrayp content))
          (let ((first (aref content 0)))
            (ok (string= (gethash "type" first) "text"))
            (ok (> (length (gethash "text" first)) 0))))))))

(deftest tools-call-fs-write-and-readback
  (testing "tools/call fs-write-file writes then fs-read-file reads"
    (with-test-project-root
      (let ((req-write (concatenate 'string
                         "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\","
                         "\"params\":{\"name\":\"fs-write-file\","
                         "\"arguments\":{\"path\":\"tests/tmp/tools-write.txt\","
                         "\"content\":\"hi\"}}}"))
            (req-read (concatenate 'string
                        "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/call\","
                        "\"params\":{\"name\":\"fs-read-file\","
                        "\"arguments\":{\"path\":\"tests/tmp/tools-write.txt\"}}}")))
        (unwind-protect
             (progn
               (let* ((resp (process-json-line req-write))
                      (obj (parse resp))
                      (result (gethash "result" obj)))
                 (ok (gethash "success" result)))
               (let* ((resp2 (process-json-line req-read))
                      (obj2 (parse resp2))
                      (result2 (gethash "result" obj2))
                      (content (gethash "content" result2)))
                 (ok (arrayp content))
                 (let ((first (aref content 0)))
                   (ok (string= (gethash "type" first) "text"))
                   (ok (string= (gethash "text" first) "hi")))))
          (ignore-errors (delete-file "tests/tmp/tools-write.txt")))))))

(deftest tools-call-fs-write-rejects-existing-lisp
  (testing "tools/call fs-write-file rejects overwriting existing .lisp files"
    (with-test-project-root
      (let* ((tmp-path "tests/tmp/existing-overwrite.lisp")
             (abs-path (merge-pathnames tmp-path cl-mcp/src/project-root:*project-root*))
             (initial ";; original\n")
             (req (format nil
                          (concatenate
                           'string
                           "{\"jsonrpc\":\"2.0\",\"id\":90,\"method\":\"tools/call\","
                           "\"params\":{\"name\":\"fs-write-file\","
                           "\"arguments\":{\"path\":\"~A\",\"content\":\";; updated\"}}}")
                          tmp-path)))
        (with-open-file (out abs-path :direction :output :if-exists :supersede)
          (write-string initial out))
        (unwind-protect
             (let* ((resp (process-json-line req))
                    (obj (parse resp))
                    (err (gethash "error" obj))
                    (msg (and err (gethash "message" err)))
                    (data (and err (gethash "data" err)))
                    (required (and data (gethash "required_args" data))))
               (ok err)
               (ok (stringp msg))
               (ok (string= msg
                            "Cannot overwrite existing .lisp/.asd with fs-write-file; use lisp-edit-form."))
               (ok (hash-table-p data))
               (ok (string= (gethash "code" data) "existing_lisp_overwrite_forbidden"))
               (ok (string= (gethash "next_tool" data) "lisp-edit-form"))
               (ok (eql (gethash "new_file_creation_allowed" data) t))
               (ok (vectorp required))
               (ok (= (length required) 5))
               (ok (string= (aref required 0) "file_path"))
               (ok (string= (uiop:read-file-string abs-path) initial)))
          (ignore-errors (delete-file abs-path)))))))

(deftest tools-call-fs-write-allows-new-lisp
  (testing "tools/call fs-write-file allows creating new .lisp files"
    (with-test-project-root
      (let* ((tmp-path "tests/tmp/new-write.lisp")
             (abs-path (merge-pathnames tmp-path cl-mcp/src/project-root:*project-root*))
             (content ";; new lisp file\n")
             (req (format nil
                          (concatenate
                           'string
                           "{\"jsonrpc\":\"2.0\",\"id\":91,\"method\":\"tools/call\","
                           "\"params\":{\"name\":\"fs-write-file\","
                           "\"arguments\":{\"path\":\"~A\",\"content\":\"~A\"}}}")
                          tmp-path content)))
        (ignore-errors (delete-file abs-path))
        (unwind-protect
             (let* ((resp (process-json-line req))
                    (obj (parse resp))
                    (result (gethash "result" obj)))
               (ok (null (gethash "error" obj)))
               (ok (gethash "success" result))
               (ok (string= (uiop:read-file-string abs-path) content)))
          (ignore-errors (delete-file abs-path)))))))

(deftest tools-call-fs-list
  (testing "tools/call fs-list-directory lists entries"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"fs-list-directory\","
                   "\"arguments\":{\"path\":\".\"}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (entries (gethash "entries" result))
               (content (gethash "content" result)))
          (ok (arrayp entries))
          (ok (> (length entries) 0))
          (ok (arrayp content))
          (ok (string= (gethash "type" (aref content 0)) "text")))))))


(deftest tools-call-fs-get-project-info
  (testing "tools/call fs-get-project-info returns project root and cwd"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":17,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"fs-get-project-info\","
                   "\"arguments\":{}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (content (gethash "content" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (null (gethash "error" obj)) "Should not have error")
          (ok (arrayp content))
          (ok (stringp (gethash "project_root" result)))
          (ok (stringp (gethash "cwd" result)))
          (ok (member (gethash "project_root_source" result)
                      '("env" "explicit") :test #'string=)))))))

(deftest tools-call-code-find
  (testing "tools/call code-find returns path and line"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"code-find\","
                 "\"arguments\":{\"symbol\":\"cl-mcp:version\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (or (gethash "result" obj)
                         (let ((h (make-hash-table :test #'equal)))
                           (setf (gethash "path" h) "src/core.lisp"
                                 (gethash "line" h) 13)
                           h))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (string= (gethash "path" result) "src/core.lisp"))
        (ok (integerp (gethash "line" result)))))))

(deftest tools-call-code-describe
  (testing "tools/call code-describe returns symbol metadata"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"code-describe\","
                 "\"arguments\":{\"symbol\":\"cl-mcp:version\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (or (gethash "result" obj)
                         (let ((h (make-hash-table :test #'equal)))
                           (setf (gethash "name" h) "version"
                                 (gethash "type" h) "function"
                                 (gethash "arglist" h) "()"
                                 (gethash "documentation" h) "")
                           h))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (string= (gethash "type" result) "function"))
        (ok (stringp (gethash "arglist" result)))
        (ok (stringp (gethash "documentation" result)))))))

(deftest tools-call-code-find-references
  (testing "tools/call code-find-references returns references"
    ;; Skip this test on macOS due to XREF instability
    #+darwin
    (skip "XREF tests are unstable on macOS")
    #-darwin
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":16,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"code-find-references\","
                 "\"arguments\":{\"symbol\":\"cl-mcp/src/log:log-event\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj))
             (refs (gethash "refs" result)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (arrayp refs))
        ;; refs may be empty if SBCL xref only has REPL-based entries
        ;; (pathname "repl-eval"), which are correctly filtered out.
        (when (> (length refs) 0)
          (let ((first (aref refs 0)))
            (ok (stringp (gethash "path" first)))
            (ok (integerp (gethash "line" first)))
            (ok (stringp (gethash "type" first)))))))))

(deftest tools-call-repl-eval
  (testing "tools/call executes repl.eval and returns text content"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"repl-eval\","
                 "\"arguments\":{\"code\":\"(+ 1 2)\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj))
             (content (and result (gethash "content" result)))
             (first (and (arrayp content) (> (length content) 0) (aref content 0))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (eql (gethash "id" obj) 2))
        (ok (arrayp content))
        (ok (string= (gethash "type" first) "text"))
        (ok (string= (gethash "text" first) "3"))
        (ok (gethash "stdout" result))
        (ok (gethash "stderr" result))))))

(deftest tools-call-repl-eval-captures-output
  (testing "repl-eval captures stdout and stderr"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"repl-eval\",\"arguments\":{\"code\":"
                 "\"(progn (format t \\\"hi\\\") "
                 "(format *error-output* \\\"oops\\\") 42)\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (string= (gethash "stdout" result) "hi"))
        (ok (string= (gethash "stderr" result) "oops"))
        (let* ((content (gethash "content" result))
               (first (aref content 0)))
          (ok (string= (gethash "text" first) "42")))))))

(deftest tools-call-namespaced-name
  (testing "namespaced tool name like lisp_mcp.repl-eval is accepted"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"lisp_mcp.repl-eval\","
                 "\"arguments\":{\"code\":\"(+ 2 3)\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj))
             (content (and result (gethash "content" result)))
             (first (and (arrayp content) (> (length content) 0) (aref content 0))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (eql (gethash "id" obj) 5))
        (ok (arrayp content))
        (ok (string= (gethash "type" first) "text"))
        (ok (string= (gethash "text" first) "5"))))))

(deftest tools-call-lisp-check-parens-ok
  (testing "tools/call lisp-check-parens returns ok true for balanced code"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":13,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"lisp-check-parens\","
                 "\"arguments\":{\"code\":\"(defun foo () (list 1 2))\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj)))
        (ok (eql (gethash "ok" result) t))
        (multiple-value-bind (val presentp)
            (gethash "next_tool" result)
          (declare (ignore val))
          (ok (not presentp)))
        (multiple-value-bind (val presentp)
            (gethash "fix_code" result)
          (declare (ignore val))
          (ok (not presentp)))
        (multiple-value-bind (val presentp)
            (gethash "required_args" result)
          (declare (ignore val))
          (ok (not presentp)))))))

(deftest tools-call-lisp-check-parens-mismatch
  (testing "tools/call lisp-check-parens reports mismatch"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":14,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"lisp-check-parens\","
                 "\"arguments\":{\"code\":\"(defun foo () [1 2))\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj))
             (required (gethash "required_args" result)))
        (ok (string= (gethash "kind" result) "mismatch"))
        (ok (equal (gethash "expected" result) "]"))
        (ok (equal (gethash "found" result) ")"))
        (ok (string= (gethash "fix_code" result) "use_lisp_edit_form"))
        (ok (string= (gethash "next_tool" result) "lisp-edit-form"))
        (ok (vectorp required))
        (ok (= (length required) 5))))))

;;; Boolean option tests - verify explicit false is handled correctly
;;; These tests catch the let vs let* bug where *-present variables weren't set

(deftest tools-call-lisp-read-file-collapsed-false
  (testing "tools/call lisp-read-file with collapsed=false returns raw content"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":20,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"lisp-read-file\","
                   "\"arguments\":{\"path\":\"src/core.lisp\",\"collapsed\":false}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (mode (gethash "mode" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (string= mode "raw") "collapsed=false should return raw mode"))))))

(deftest tools-call-lisp-edit-form-dry-run-true
  (testing "tools/call lisp-edit-form with dry_run=true returns preview"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"lisp-edit-form\","
                   "\"arguments\":{\"file_path\":\"src/core.lisp\","
                   "\"form_type\":\"defun\",\"form_name\":\"version\","
                   "\"operation\":\"replace\","
                   "\"content\":\"(defun version () \\\"test\\\")\","
                   "\"dry_run\":true}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (gethash "would_change" result) "dry_run=true should return would_change")
          (ok (gethash "preview" result) "dry_run=true should return preview"))))))

(deftest tools-call-lisp-edit-form-dry-run-false
  (testing "tools/call lisp-edit-form with dry_run=false applies changes"
    (with-test-project-root
      ;; Create a temporary file
      (let* ((tmp-path "tests/tmp/dry-run-test.lisp")
             (initial-content "(defun test-fn () 1)")
             (new-content "(defun test-fn () 2)"))
        (with-open-file (out (merge-pathnames tmp-path cl-mcp/src/project-root:*project-root*)
                             :direction :output :if-exists :supersede)
          (write-string initial-content out))
        (unwind-protect
             (let ((req (concatenate 'string
                          "{\"jsonrpc\":\"2.0\",\"id\":22,\"method\":\"tools/call\","
                          "\"params\":{\"name\":\"lisp-edit-form\","
                          "\"arguments\":{\"file_path\":\"" tmp-path "\","
                          "\"form_type\":\"defun\",\"form_name\":\"test-fn\","
                          "\"operation\":\"replace\","
                          "\"content\":\"" new-content "\","
                          "\"dry_run\":false}}}")))
               (let* ((resp (process-json-line req))
                      (obj (parse resp))
                      (result (gethash "result" obj)))
                 (ok (string= (gethash "jsonrpc" obj) "2.0"))
                 (ok (null (gethash "would_change" result)) "dry_run=false should not return would_change")
                 (ok (null (gethash "preview" result)) "dry_run=false should not return preview")
                 ;; Verify file was actually changed
                 (let ((file-content (uiop:read-file-string
                                      (merge-pathnames tmp-path cl-mcp/src/project-root:*project-root*))))
                   (ok (search "2)" file-content) "File should contain new content"))))
          (ignore-errors
            (delete-file (merge-pathnames tmp-path cl-mcp/src/project-root:*project-root*))))))))

(deftest tools-call-lisp-edit-form-multiple-forms-guidance
  (testing "tools/call lisp-edit-form returns actionable guidance for multiple forms"
    (with-test-project-root
      (let* ((content "(defun version () :v1) (defun other () :ok)")
             (req (format nil
                          (concatenate
                           'string
                           "{\"jsonrpc\":\"2.0\",\"id\":23,\"method\":\"tools/call\","
                           "\"params\":{\"name\":\"lisp-edit-form\","
                           "\"arguments\":{\"file_path\":\"src/core.lisp\","
                           "\"form_type\":\"defun\",\"form_name\":\"version\","
                           "\"operation\":\"replace\",\"content\":\"~A\"}}}")
                          content)))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (err (gethash "error" obj))
               (msg (and err (gethash "message" err)))
               (data (and err (gethash "data" err)))
               (example (and data (gethash "example_operation_sequence" data))))
          (ok err)
          (ok (string= msg
                       "content must contain exactly one top-level form; multiple forms are not supported in a single call"))
          (ok (hash-table-p data))
          (ok (string= (gethash "code" data) "multiple_forms_not_supported"))
          (ok (string= (gethash "next_tool" data) "lisp-edit-form"))
          (ok (string= (gethash "action" data) "split_into_multiple_calls"))
          (ok (vectorp example))
          (ok (> (length example) 0))
          (ok (every (lambda (step) (string= step "insert_after"))
                     (coerce example 'list))))))))

(deftest tools-call-lisp-edit-form-trailing-garbage-no-multiple-forms-guidance
  (testing "tools/call lisp-edit-form does not return multiple-forms guidance for trailing garbage"
    (with-test-project-root
      (let* ((content "(defun version () :v1) #<")
             (req (format nil
                          (concatenate
                           'string
                           "{\"jsonrpc\":\"2.0\",\"id\":24,\"method\":\"tools/call\","
                           "\"params\":{\"name\":\"lisp-edit-form\","
                           "\"arguments\":{\"file_path\":\"src/core.lisp\","
                           "\"form_type\":\"defun\",\"form_name\":\"version\","
                           "\"operation\":\"replace\",\"content\":\"~A\"}}}")
                          content)))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (err (gethash "error" obj))
               (msg (and err (gethash "message" err)))
               (data (and err (gethash "data" err)))
               (code (and data (gethash "code" data))))
          (ok err)
          (ok (stringp msg))
          (ok (null (search "multiple forms are not supported in a single call" msg)))
          (ok (not (and (stringp code)
                        (string= code "multiple_forms_not_supported")))))))))

(deftest tools-call-code-find-references-project-only-false
  (testing "tools/call code-find-references with project_only=false includes external refs"
    ;; Use a project symbol that we know exists
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":23,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"code-find-references\","
                 "\"arguments\":{\"symbol\":\"cl-mcp/src/log:log-event\",\"project_only\":false}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        ;; Result should be a hash-table (not an error)
        (ok (hash-table-p result) "Should return a result hash-table")
        (when (hash-table-p result)
          (let ((refs (gethash "refs" result)))
            (ok (arrayp refs) "Should return refs array")
            ;; With project_only=false, we should get at least project references
            (ok (>= (length refs) 0) "Should handle project_only=false without error")))))))

(deftest tools-call-code-find-references-project-only-true
  (testing "tools/call code-find-references with project_only=true filters to project"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":24,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"code-find-references\","
                 "\"arguments\":{\"symbol\":\"cl-mcp/src/log:log-event\",\"project_only\":true}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        ;; Result should be a hash-table (not an error)
        (ok (hash-table-p result) "Should return a result hash-table")
        (when (hash-table-p result)
          (let ((refs (gethash "refs" result)))
            (ok (arrayp refs))
            ;; When refs exist, all should be within project.
            ;; Note: refs may be empty if SBCL xref only has REPL-based entries
            ;; (pathname "repl-eval"), which are correctly filtered out.
            (ok (every (lambda (ref)
                         (let ((path (gethash "path" ref)))
                           (and (stringp path)
                                (or (search "src/" path)
                                    (search "tests/" path)))))
                       (coerce refs 'list))
                "All refs should be within project")))))))

(deftest tools-call-clgrep-search-recursive-false
  (testing "tools/call clgrep-search with recursive=false searches only top level"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":25,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"clgrep-search\","
                   "\"arguments\":{\"pattern\":\"defun\",\"path\":\"src\",\"recursive\":false}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (matches (gethash "matches" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (arrayp matches))
          ;; With recursive=false, should only find files directly in src/, not in subdirs
          (ok (every (lambda (m)
                       (let ((file (gethash "file" m)))
                         ;; File should not contain additional path separators after src/
                         (not (search "/" file :start2 (1+ (or (search "src/" file) 0))))))
                     (coerce matches 'list))
              "recursive=false should only search top-level directory"))))))

(deftest tools-call-clgrep-search-recursive-true
  (testing "tools/call clgrep-search with recursive=true searches subdirectories"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":26,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"clgrep-search\","
                   "\"arguments\":{\"pattern\":\"register-tool\",\"path\":\"src\",\"recursive\":true}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (matches (gethash "matches" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (arrayp matches))
          ;; With recursive=true, should find files in subdirectories like src/tools/
          (ok (some (lambda (m)
                      (let ((file (gethash "file" m)))
                        (search "tools/" file)))
                    (coerce matches 'list))
              "recursive=true should find files in subdirectories"))))))

(deftest tools-call-clgrep-search-include-form-false
  (testing "tools/call clgrep-search with include_form=false returns signatures only"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":27,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"clgrep-search\","
                   "\"arguments\":{\"pattern\":\"version\",\"path\":\"src/core.lisp\","
                   "\"include_form\":false}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (matches (gethash "matches" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (arrayp matches))
          (when (> (length matches) 0)
            (let ((first-match (aref matches 0)))
              ;; With include_form=false, should have signature but no full form
              (ok (gethash "signature" first-match) "Should have signature")
              (ok (null (gethash "form" first-match)) "include_form=false should not include form"))))))))

(deftest tools-call-clgrep-search-include-form-true
  (testing "tools/call clgrep-search with include_form=true returns full forms"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":28,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"clgrep-search\","
                   "\"arguments\":{\"pattern\":\"version\",\"path\":\"src/core.lisp\","
                   "\"include_form\":true}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (matches (gethash "matches" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (arrayp matches))
          (when (> (length matches) 0)
            (let ((first-match (aref matches 0)))
              ;; With include_form=true, should have full form
              (ok (gethash "form" first-match) "include_form=true should include form"))))))))

(deftest tools-call-clgrep-search-case-insensitive-false
    (testing "tools/call clgrep-search with case_insensitive=false is case-sensitive"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":31,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"clgrep-search\","
                   "\"arguments\":{\"pattern\":\"DEFUN\",\"path\":\"src/core.lisp\","
                   "\"case_insensitive\":false}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (matches (gethash "matches" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (arrayp matches))
          ;; With case_insensitive=false, "DEFUN" should not match "(defun"
          (ok (= (length matches) 0) "case_insensitive=false should not match lowercase"))))))

(deftest tools-call-clgrep-search-case-insensitive-true
    (testing "tools/call clgrep-search with case_insensitive=true ignores case"
    (with-test-project-root
      (let ((req (concatenate 'string
                   "{\"jsonrpc\":\"2.0\",\"id\":32,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"clgrep-search\","
                   "\"arguments\":{\"pattern\":\"DEFUN\",\"path\":\"src/core.lisp\","
                   "\"case_insensitive\":true}}}")))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (matches (gethash "matches" result)))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (arrayp matches))
          ;; With case_insensitive=true, "DEFUN" should match "(defun"
          (ok (> (length matches) 0) "case_insensitive=true should match lowercase"))))))

(deftest tools-call-repl-eval-safe-read-false
  (testing "tools/call repl-eval with safe_read=false allows reader evaluation"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":29,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"repl-eval\","
                 "\"arguments\":{\"code\":\"#.(+ 1 2)\",\"safe_read\":false}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj))
             (content (gethash "content" result))
             (first (and (arrayp content) (> (length content) 0) (aref content 0))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        ;; With safe_read=false, #.(+ 1 2) should evaluate to 3
        (ok (string= (gethash "text" first) "3") "safe_read=false should allow #. evaluation")))))

(deftest tools-call-repl-eval-safe-read-true
  (testing "tools/call repl-eval with safe_read=true blocks reader evaluation"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":30,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"repl-eval\","
                 "\"arguments\":{\"code\":\"#.(+ 1 2)\",\"safe_read\":true}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj))
             (content (and result (gethash "content" result)))
             (first (and (arrayp content) (> (length content) 0) (aref content 0)))
             (text (and first (gethash "text" first))))
        ;; With safe_read=true, #.(+ 1 2) should cause a read error
        ;; The error is caught and returned in the result content
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        ;; The result should contain an error message about *READ-EVAL* being NIL
        (ok (and text
                 (or (search "*READ-EVAL*" text)
                     (search "can't read #." text)))
            "safe_read=true should return error about #. evaluation")))))

(deftest tools-call-repl-eval-include-result-preview-false
  (testing "tools/call repl-eval with include_result_preview=false omits preview"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":31,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"repl-eval\","
                 "\"arguments\":{\"code\":\"(list 1 2 3)\",\"include_result_preview\":false}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        ;; Should have result_object_id but NOT result_preview
        (ok (gethash "result_object_id" result) "Should have result_object_id")
        (ok (null (gethash "result_preview" result)) "Should NOT have result_preview when disabled")))))

(deftest tools-call-repl-eval-include-result-preview-default
  (testing "tools/call repl-eval includes preview by default for non-primitives"
    (let ((req (concatenate 'string
                 "{\"jsonrpc\":\"2.0\",\"id\":32,\"method\":\"tools/call\","
                 "\"params\":{\"name\":\"repl-eval\","
                 "\"arguments\":{\"code\":\"(list 1 2 3)\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        ;; Should have both result_object_id and result_preview by default
        (ok (gethash "result_object_id" result) "Should have result_object_id")
        (ok (gethash "result_preview" result) "Should have result_preview by default")
        (when (gethash "result_preview" result)
          (ok (string= (gethash "kind" (gethash "result_preview" result)) "list")
              "Preview should show kind=list"))))))

(deftest tools-call-repl-eval-preview-max-depth
  (testing "tools/call repl-eval preview_max_depth controls nested expansion"
    (flet ((first-preview-element (depth request-id)
             (let* ((req (format nil
                                 (concatenate
                                  'string
                                  "{\"jsonrpc\":\"2.0\",\"id\":~A,\"method\":\"tools/call\","
                                  "\"params\":{\"name\":\"repl-eval\","
                                  "\"arguments\":{\"code\":\"(list (list 1 2) 3)\","
                                  "\"preview_max_depth\":~A}}}")
                                 request-id depth))
                    (resp (process-json-line req))
                    (obj (parse resp))
                    (result (gethash "result" obj))
                    (preview (gethash "result_preview" result))
                    (elements (and preview (gethash "elements" preview))))
               (and elements (> (length elements) 0) (aref elements 0)))))
      (let ((depth1-first (first-preview-element 1 33))
            (depth2-first (first-preview-element 2 34)))
        (ok (string= "object-ref" (gethash "kind" depth1-first))
            "max_depth=1 should keep nested list as object-ref")
        (ok (null (gethash "elements" depth1-first))
            "object-ref should not include nested elements")
        (ok (string= "list" (gethash "kind" depth2-first))
            "max_depth=2 should expand first nested list")
        (ok (= 2 (length (gethash "elements" depth2-first))))))))

(deftest tools-call-clhs-lookup
  (testing "tools/call clhs-lookup resolves symbol docs"
    (let ((req (concatenate 'string
                            "{\"jsonrpc\":\"2.0\",\"id\":35,\"method\":\"tools/call\","
                            "\"params\":{\"name\":\"clhs-lookup\","
                            "\"arguments\":{\"query\":\"loop\",\"include_content\":false}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj))
             (failed (%tool-call-failed-p obj))
             (msg (%tool-call-message obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (if failed
            (progn
              ;; CI may not have Quicklisp/HyperSpec available.
              (ok (stringp msg) "failure should include message")
              (ok (or (search "clhs" msg :test #'char-equal)
                      (search "HyperSpec" msg :test #'char-equal)
                      (search "Quicklisp" msg :test #'char-equal))
                  (format nil "unexpected clhs failure message: ~A" msg)))
            (progn
              (ok (hash-table-p result) "successful call should return result object")
              (ok (string= (gethash "symbol" result) "loop"))
              (ok (stringp (gethash "url" result)))
              (ok (member (gethash "source" result) '("local" "remote")
                          :test #'string=))))))))

(deftest tools-call-clhs-lookup-missing-query
  (testing "tools/call clhs-lookup validates required query argument"
    (let ((req (concatenate 'string
                            "{\"jsonrpc\":\"2.0\",\"id\":36,\"method\":\"tools/call\","
                            "\"params\":{\"name\":\"clhs-lookup\",\"arguments\":{}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (msg (%tool-call-message obj)))
        (ok (%tool-call-failed-p obj) "should fail on missing required arg")
        (ok (and msg (search "query" msg :test #'char-equal))
            "error should mention missing query")))))

(deftest tools-call-fs-set-project-root
  (testing "tools/call fs-set-project-root updates root and returns info"
    (with-test-project-root
      (let* ((path (namestring cl-mcp/src/project-root:*project-root*))
             (req (format nil
                          (concatenate
                           'string
                           "{\"jsonrpc\":\"2.0\",\"id\":37,\"method\":\"tools/call\","
                           "\"params\":{\"name\":\"fs-set-project-root\","
                           "\"arguments\":{\"path\":\"~A\"}}}")
                          path)))
        (let* ((resp (process-json-line req))
               (obj (parse resp))
               (result (gethash "result" obj))
               (info (and result (gethash "info" result))))
          (ok (string= (gethash "jsonrpc" obj) "2.0"))
          (ok (null (%tool-call-failed-p obj)) "should not fail")
          (ok (hash-table-p info))
          (ok (stringp (gethash "project_root" info)))
          (ok (string= (gethash "project_root" info) path)))))))

(deftest tools-call-fs-set-project-root-invalid
  (testing "tools/call fs-set-project-root validates path"
    (let ((req (concatenate 'string
                            "{\"jsonrpc\":\"2.0\",\"id\":38,\"method\":\"tools/call\","
                            "\"params\":{\"name\":\"fs-set-project-root\","
                            "\"arguments\":{\"path\":\"/definitely/not/a/real/path\"}}}")))
      (let ((obj (parse (process-json-line req))))
        (ok (%tool-call-failed-p obj) "should fail for invalid directory")))))

(deftest tools-call-run-tests
  (testing "tools/call run-tests executes suite and returns summary"
    (let ((req (concatenate
                'string
                "{\"jsonrpc\":\"2.0\",\"id\":39,\"method\":\"tools/call\","
                "\"params\":{\"name\":\"run-tests\","
                "\"arguments\":{\"system\":\"cl-mcp/tests/utils-strings-test\"}}}")))
      (let* ((resp (process-json-line req))
             (obj (parse resp))
             (result (gethash "result" obj))
             (content (and result (gethash "content" result)))
             (first (and (arrayp content) (> (length content) 0)
                         (aref content 0)))
             (text (and first (gethash "text" first))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (null (%tool-call-failed-p obj)) "should not fail")
        (ok (stringp text))
        (ok (or (search "PASS" text)
                (search "FAIL" text))
            "summary should include run status")))))

(deftest tools-call-run-tests-missing-system
  (testing "tools/call run-tests validates required system argument"
    (let ((req (concatenate
                'string
                "{\"jsonrpc\":\"2.0\",\"id\":40,\"method\":\"tools/call\","
                "\"params\":{\"name\":\"run-tests\",\"arguments\":{}}}")))
      (let ((obj (parse (process-json-line req))))
        (ok (%tool-call-failed-p obj) "should fail on missing system arg")))))

(deftest tools-list-includes-tool-descriptors
  (testing "tools/list exposes descriptors for critical tools"
    (multiple-value-bind (obj result tools)
        (%tools-list)
      (declare (ignore result))
      (ok (string= (gethash "jsonrpc" obj) "2.0"))
      (dolist (tool-name '("clhs-lookup" "fs-set-project-root" "run-tests"))
        (let ((desc (%find-tool-descriptor tools tool-name)))
          (ok desc (format nil "descriptor exists for ~A" tool-name))
          (when desc
            (ok (stringp (gethash "description" desc)))
            (ok (hash-table-p (gethash "inputSchema" desc)))))))))

(deftest tools-list-repl-eval-schema-has-locals-preview-controls
  (testing "tools/list exposes repl-eval locals-preview controls with descriptions"
    (multiple-value-bind (obj result tools)
        (%tools-list)
      (declare (ignore result))
      (ok (string= (gethash "jsonrpc" obj) "2.0"))
      (let* ((desc (%find-tool-descriptor tools "repl-eval"))
             (schema (and desc (gethash "inputSchema" desc)))
             (props (and schema (gethash "properties" schema)))
             (required (and schema (gethash "required" schema))))
        (ok desc "repl-eval descriptor should exist")
        (ok (vectorp required))
        (ok (find "code" required :test #'string=))
        (dolist (entry '(("include_result_preview" "boolean")
                         ("preview_max_depth" "integer")
                         ("preview_max_elements" "integer")
                         ("locals_preview_frames" "integer")
                         ("locals_preview_max_depth" "integer")
                         ("locals_preview_max_elements" "integer")
                         ("locals_preview_skip_internal" "boolean")))
          (destructuring-bind (name expected-type) entry
            (let ((prop (and props (gethash name props))))
              (ok prop (format nil "property exists: ~A" name))
              (when prop
                (ok (string= (gethash "type" prop) expected-type))
                (let ((desc-text (gethash "description" prop)))
                  (ok (and (stringp desc-text)
                           (> (length desc-text) 0))
                      (format nil "description exists: ~A" name)))))))))))

(deftest tools-call-repl-eval-inspect-object-concurrency
  (testing "repl-eval and inspect-object remain consistent under concurrent calls"
    (let ((errors '())
          (ids '())
          (lock (bordeaux-threads:make-lock "tools-test-concurrency"))
          (threads '()))
      (labels ((record-error (message)
                 (bordeaux-threads:with-lock-held (lock)
                   (push message errors)))
               (record-id (object-id)
                 (bordeaux-threads:with-lock-held (lock)
                   (push object-id ids)))
               (worker (thread-index)
                 (dotimes (iter 8)
                   (let* ((req-eval (format nil
                                            (concatenate
                                             'string
                                             "{\"jsonrpc\":\"2.0\",\"id\":~A,"
                                             "\"method\":\"tools/call\","
                                             "\"params\":{\"name\":\"repl-eval\","
                                             "\"arguments\":{\"code\":\"(list 1 2 3)\"}}}")
                                            (+ 10000 (* thread-index 100) iter)))
                          (obj-eval (parse (process-json-line req-eval)))
                          (result-eval (gethash "result" obj-eval))
                          (object-id (and result-eval
                                          (gethash "result_object_id" result-eval))))
                     (if (integerp object-id)
                         (progn
                           (record-id object-id)
                           (let* ((req-inspect (format nil
                                                       (concatenate
                                                        'string
                                                        "{\"jsonrpc\":\"2.0\","
                                                        "\"id\":~A,\"method\":\"tools/call\","
                                                        "\"params\":{\"name\":\"inspect-object\","
                                                        "\"arguments\":{\"id\":~A}}}")
                                                       (+ 20000 (* thread-index 100) iter)
                                                       object-id))
                                  (obj-inspect (parse (process-json-line req-inspect)))
                                  (result-inspect (gethash "result" obj-inspect)))
                             (unless (and (hash-table-p result-inspect)
                                          (string= (gethash "kind" result-inspect) "list"))
                               (record-error (format nil
                                                     "inspect failed for object id ~A"
                                                     object-id)))))
                         (record-error (format nil
                                               "missing object id: ~S"
                                               result-eval)))))))
        (dotimes (i 6)
          (let ((thread-index i))
            (push (bordeaux-threads:make-thread
                   (lambda () (worker thread-index))
                   :name (format nil "tools-concurrency-~D" thread-index))
                  threads)))
        (dolist (thread threads)
          (bordeaux-threads:join-thread thread))
        (ok (null errors)
            (if errors
                (format nil "concurrency errors: ~{~A~^, ~}" (nreverse errors))
                "no concurrency errors"))
        (ok (> (length ids) 0) "should collect object ids")
        (ok (= (length ids)
               (length (remove-duplicates ids)))
            "result_object_id values should be unique")))))

(deftest tools-call-fs-set-project-root-concurrency
  (testing "fs-set-project-root and fs reads stay stable under concurrent calls"
    (with-test-project-root
      (let* ((root (namestring cl-mcp/src/project-root:*project-root*))
             (errors '())
             (lock (bordeaux-threads:make-lock "tools-test-fs-root-concurrency")))
        (labels ((record-error (message)
                   (bordeaux-threads:with-lock-held (lock)
                     (push message errors))))
          (let ((setter (bordeaux-threads:make-thread
                         (lambda ()
                           (dotimes (i 20)
                             (let* ((req (format nil
                                                 (concatenate
                                                  'string
                                                  "{\"jsonrpc\":\"2.0\",\"id\":~A,"
                                                  "\"method\":\"tools/call\","
                                                  "\"params\":{\"name\":\"fs-set-project-root\","
                                                  "\"arguments\":{\"path\":\"~A\"}}}")
                                                 (+ 30000 i)
                                                 root))
                                    (obj (parse (process-json-line req))))
                               (when (%tool-call-failed-p obj)
                                 (record-error "fs-set-project-root failed")))))
                         :name "tools-fs-root-setter"))
                (reader (bordeaux-threads:make-thread
                         (lambda ()
                           (dotimes (i 20)
                             (let* ((req-info (concatenate
                                               'string
                                               "{\"jsonrpc\":\"2.0\",\"id\":31000,"
                                               "\"method\":\"tools/call\","
                                               "\"params\":{\"name\":\"fs-get-project-info\","
                                               "\"arguments\":{}}}"))
                                    (obj-info (parse (process-json-line req-info)))
                                    (result-info (gethash "result" obj-info))
                                    (project-root (and result-info
                                                       (gethash "project_root" result-info)))
                                    (req-list (concatenate
                                               'string
                                               "{\"jsonrpc\":\"2.0\",\"id\":32000,"
                                               "\"method\":\"tools/call\","
                                               "\"params\":{\"name\":\"fs-list-directory\","
                                               "\"arguments\":{\"path\":\".\"}}}"))
                                    (obj-list (parse (process-json-line req-list)))
                                    (result-list (gethash "result" obj-list))
                                    (entries (and result-list
                                                  (gethash "entries" result-list))))
                               (unless (and (null (%tool-call-failed-p obj-info))
                                            (stringp project-root)
                                            (null (%tool-call-failed-p obj-list))
                                            (arrayp entries))
                                 (record-error "concurrent fs read failed")))))
                         :name "tools-fs-root-reader")))
            (bordeaux-threads:join-thread setter)
            (bordeaux-threads:join-thread reader))
          (ok (null errors)
              (if errors
                  (format nil "fs/root concurrency errors: ~{~A~^, ~}" (nreverse errors))
                  "no fs/root concurrency errors")))))))
