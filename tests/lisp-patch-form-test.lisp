;;;; tests/lisp-patch-form-test.lisp

(defpackage #:cl-mcp/tests/lisp-patch-form-test
  (:use #:cl)
    (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:ng
                #:skip)
  (:import-from #:cl-mcp/src/lisp-patch-form
                #:lisp-patch-form)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:merge-pathnames*
                #:native-namestring
                #:ensure-directories-exist))

(in-package #:cl-mcp/tests/lisp-patch-form-test)

(setf cl-mcp/src/project-root:*project-root*
      (uiop:ensure-directory-pathname (system-source-directory :cl-mcp)))

(defun project-path (relative)
  "Return an absolute namestring under the cl-mcp project for RELATIVE."
  (native-namestring
   (merge-pathnames* relative (system-source-directory :cl-mcp))))

(defun with-temp-file (relative initial thunk)
  "Create RELATIVE file with INITIAL content, call THUNK with absolute path,
then clean up."
  (let ((abs (project-path relative)))
    (ensure-directories-exist abs)
    (fs-write-file relative initial)
    (unwind-protect
         (funcall thunk abs)
      (ignore-errors (delete-file abs)))))

(defun %directory-writable-p (dir)
  "Return T when a probe file can be created inside DIR.
Used to detect processes (e.g. root) for which chmod does not deny writes."
  (let ((probe (merge-pathnames* ".cl-mcp-write-probe"
                                 (uiop:ensure-directory-pathname dir))))
    (handler-case
        (progn
          (with-open-file (stream probe :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
            (write-char #\x stream))
          (ignore-errors (delete-file probe))
          t)
      (error () nil))))

(defun with-readonly-temp-file (relative initial thunk)
  "Create RELATIVE with INITIAL content, make it and its directory read-only,
then call THUNK with the absolute path. Permissions are restored and the
fixture removed even when THUNK signals, so a failing test cannot leave the
tree unwritable. When permissions are not enforced for this process (e.g.
running as root), THUNK is skipped instead."
  (let* ((abs (project-path relative))
         (dir (native-namestring (uiop:pathname-directory-pathname abs)))
         (dir-pre-existed (probe-file dir)))
    (ensure-directories-exist abs)
    (fs-write-file relative initial)
    (unwind-protect
         (progn
           (uiop:run-program (list "chmod" "444" abs))
           (uiop:run-program (list "chmod" "555" dir))
           (if (%directory-writable-p dir)
               (skip "filesystem permissions are not enforced for this process")
               (funcall thunk abs)))
      (ignore-errors (uiop:run-program (list "chmod" "755" dir)))
      (ignore-errors (uiop:run-program (list "chmod" "644" abs)))
      (ignore-errors (delete-file abs))
      ;; Only remove the directory when this call created it, so a caller
      ;; passing a path directly under tests/tmp cannot rmdir the shared dir.
      (unless dir-pre-existed
        (ignore-errors (uiop:delete-empty-directory dir))))))

(defun %try-load (system)
  "Attempt to load SYSTEM via Quicklisp or ASDF. Returns T on success, NIL on failure."
  (handler-case
      (cond
        ((find-package :ql)
         (funcall (find-symbol "QUICKLOAD" :ql) system :silent t)
         t)
        ((asdf:find-system system nil)
         (asdf:load-system system)
         t)
        (t nil))
    (error () nil)))

;;; ============================================================
;;; Basic operations
;;; ============================================================

(deftest lisp-patch-form-basic
  (testing "patch replaces a sub-expression within a defun"
    (with-temp-file "tests/tmp/patch-basic.lisp"
        (format nil "(defun compute (x)~%  (+ x 1))~%")
      (lambda (path)
        (lisp-patch-form :file-path path
                         :form-type "defun"
                         :form-name "compute"
                         :old-text "(+ x 1)"
                         :new-text "(* x 2)")
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 2)" updated))
          (ok (null (search "(+ x 1)" updated))))))))

(deftest lisp-patch-form-preserves-surrounding
  (testing "patch only modifies target form, rest of file unchanged"
    (with-temp-file "tests/tmp/patch-preserve.lisp"
        (format nil "(defun before () :keep)~%~%(defun target (x)~%  (+ x 1))~%~%(defun after () :keep)~%")
      (lambda (path)
        (lisp-patch-form :file-path path
                         :form-type "defun"
                         :form-name "target"
                         :old-text "(+ x 1)"
                         :new-text "(- x 1)")
        (let ((updated (fs-read-file path)))
          (ok (search "(- x 1)" updated))
          (ok (search "(defun before () :keep)" updated))
          (ok (search "(defun after () :keep)" updated)))))))

(deftest lisp-patch-form-multiline
  (testing "patch replaces a multi-line block within a form"
    (with-temp-file "tests/tmp/patch-multiline.lisp"
        (format nil "(defun process (data)~%  (when data~%    (print data)~%    (+ 1 2)))~%")
      (lambda (path)
        (let ((old-block (format nil "(when data~%    (print data)~%    (+ 1 2))"))
              (new-block (format nil "(when data~%    (log-info data)~%    (+ 1 2))")))
          (lisp-patch-form :file-path path
                           :form-type "defun"
                           :form-name "process"
                           :old-text old-block
                           :new-text new-block)
          (let ((updated (fs-read-file path)))
            (ok (search "(log-info data)" updated))
            (ok (null (search "(print data)" updated)))))))))

;;; ============================================================
;;; Error handling
;;; ============================================================

(deftest lisp-patch-form-not-found-error
  (testing "patch signals error when old_text is not in the form"
    (with-temp-file "tests/tmp/patch-not-found.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-patch-form :file-path path
                                     :form-type "defun"
                                     :form-name "target"
                                     :old-text "nonexistent text"
                                     :new-text "replacement")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "old_text not found" err-msg))
          (ok (search "whitespace-sensitive" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-multiple-matches-error
  (testing "patch signals error when old_text matches multiple times"
    (with-temp-file "tests/tmp/patch-multi-match.lisp"
        (format nil "(defun target (x)~%  (+ (abs x) (abs x) (abs x)))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-patch-form :file-path path
                                     :form-type "defun"
                                     :form-name "target"
                                     :old-text "(abs x)"
                                     :new-text "(abs y)")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "3 times" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-breaks-structure
  (testing "patch that breaks form structure signals error, no changes written"
    (with-temp-file "tests/tmp/patch-breaks-structure.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    ;; Remove a closing paren, breaking the form
                    (lisp-patch-form :file-path path
                                     :form-type "defun"
                                     :form-name "target"
                                     :old-text "(+ x 1))"
                                     :new-text "(+ x 1)")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (or (search "invalid Lisp" err-msg)
                  (search "fewer \")\"" err-msg)))
          (ok (search "No changes were written" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-unrepairable-structure
  (testing "patch that completely destroys form structure gives clear error"
    (with-temp-file "tests/tmp/patch-unrepairable.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-patch-form :file-path path
                                     :form-type "defun"
                                     :form-name "target"
                                     :old-text "(defun target (x)"
                                     :new-text "completely broken ((( stuff")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (or (search "invalid Lisp" err-msg)
                  (search "trailing content" err-msg)
                  (search "malformed form text" err-msg)
                  (search "fewer \")\"" err-msg))
              "error message should describe the structural problem")
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-empty-old-text-error
  (testing "patch with empty old_text signals error immediately"
    (with-temp-file "tests/tmp/patch-empty-old-text.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-patch-form :file-path path
                                     :form-type "defun"
                                     :form-name "target"
                                     :old-text ""
                                     :new-text "replacement")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "old_text must not be empty" err-msg))
          (ok (string= before (fs-read-file path))))))))

;;; ============================================================
;;; Dry-run
;;; ============================================================

(deftest lisp-patch-form-dry-run
  (testing "patch dry-run returns preview without modifying file"
    (with-temp-file "tests/tmp/patch-dry-run.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (result (lisp-patch-form :file-path path
                                       :form-type "defun"
                                       :form-name "target"
                                       :old-text "(+ x 1)"
                                       :new-text "(* x 2)"
                                       :dry-run t)))
          (ok (hash-table-p result))
          (ok (gethash "would_change" result))
          (ok (search "(+ x 1)" (gethash "original" result)))
          (ok (search "(* x 2)" (gethash "preview" result)))
          (ok (string= "patch" (gethash "operation" result)))
          (ok (string= before (fs-read-file path))))))))

;;; ============================================================
;;; Defmethod matching
;;; ============================================================

(deftest lisp-patch-form-defmethod
  (testing "patch works with defmethod form matching"
    (let ((initial (concatenate 'string
                    "(defmethod render ((w widget) stream)" (string #\Newline)
                    "  (format stream \"<~A>\" (name w)))" (string #\Newline))))
      (with-temp-file "tests/tmp/patch-defmethod.lisp"
          initial
        (lambda (path)
          (lisp-patch-form :file-path path
                           :form-type "defmethod"
                           :form-name "render ((w widget) stream)"
                           :old-text "(name w)"
                           :new-text "(widget-name w)")
          (let ((updated (fs-read-file path)))
            (ok (search "(widget-name w)" updated))
            (ok (null (search "(name w)" updated)))))))))

(deftest lisp-patch-form-discovers-separate-package-definition-for-local-nicknames
  (testing "patch works when package local nicknames are defined in a separate file"
    (let* ((pkg-name "CL-MCP-TMP-LN-PATCH-USER")
           (target-name "CL-MCP-TMP-LN-PATCH-TARGET")
           (defs-relative "tests/tmp/patch-form-local-nicknames-package.lisp")
           (source-relative "tests/tmp/patch-form-local-nicknames-source.lisp")
           (defs-path (project-path defs-relative))
           (source-path (project-path source-relative))
           (defs-content
             (format nil
                     "(defpackage #:~A~%  (:use #:cl)~%  (:local-nicknames (#:ad #:~A)))~%"
                     pkg-name target-name))
           (source-content
             (format nil
                     "(in-package #:~A)~%~%~
(defun make-thing ()~%  (ad:make-dual 1.0 0.0))~%~%~
(defun other-thing ()~%  (ad:make-dual 2.0 1.0))~%"
                     pkg-name)))
      (when (find-package pkg-name)
        (delete-package pkg-name))
      (when (find-package target-name)
        (delete-package target-name))
      (ensure-directories-exist defs-path)
      (fs-write-file defs-relative defs-content)
      (fs-write-file source-relative source-content)
      (unwind-protect
           (progn
             (ok (null (find-package pkg-name))
                 "user package is not preloaded in parent")
             (ok (null (find-package target-name))
                 "nickname target package is not preloaded in parent")
             (lisp-patch-form :file-path source-path
                              :form-type "defun"
                              :form-name "make-thing"
                              :old-text "(ad:make-dual 1.0 0.0)"
                              :new-text "(ad:make-dual 88.0 0.0)")
             (let ((updated (fs-read-file source-path)))
               (ok (search "88.0" updated))
               (ok (search "other-thing" updated)))
             (ok (null (find-package pkg-name))
                 "synthesized user package is cleaned up")
             (ok (null (find-package target-name))
                 "synthesized target package is cleaned up"))
        (ignore-errors (delete-file defs-path))
        (ignore-errors (delete-file source-path))))))

;;; ============================================================
;;; Edge cases
;;; ============================================================

(deftest lisp-patch-form-changes-form-name
  (testing "patch can rename the function (matching happens before patch)"
    (with-temp-file "tests/tmp/patch-rename.lisp"
        (format nil "(defun my-func (x)~%  (+ x 1))~%")
      (lambda (path)
        (lisp-patch-form :file-path path
                         :form-type "defun"
                         :form-name "my-func"
                         :old-text "my-func"
                         :new-text "my-func-v2")
        (let ((updated (fs-read-file path)))
          (ok (search "my-func-v2" updated))
          ;; The original name should only appear as part of the new name
          (ok (null (search "(defun my-func " updated))))))))

(deftest lisp-patch-form-no-op
  (testing "patch with old_text == new_text reports would_change as false"
    (with-temp-file "tests/tmp/patch-no-op.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (result (lisp-patch-form :file-path path
                                       :form-type "defun"
                                       :form-name "target"
                                       :old-text "(+ x 1)"
                                       :new-text "(+ x 1)"
                                       :dry-run t)))
          (ok (hash-table-p result))
          (ok (null (gethash "would_change" result))
              "would_change should be nil for no-op")
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-no-op-non-dry-run
  (testing "patch with old_text == new_text does not write file"
    (with-temp-file "tests/tmp/patch-no-op-nodry.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path)))
          (multiple-value-bind (updated changed-p)
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(+ x 1)"
                               :new-text "(+ x 1)")
            (ok (stringp updated))
            (ok (null changed-p) "changed-p should be nil for no-op")
            (ok (string= before (fs-read-file path))
                "file should not have been rewritten")))))))

(deftest lisp-patch-form-old-text-in-string-literal
  (testing "patch matches old_text in code when it also appears in a string"
    (with-temp-file "tests/tmp/patch-string-literal.lisp"
        (format nil "(defun target ()~%  (error \"call (+ x 1) here\")~%  (+ x 1))~%")
      (lambda (path)
        ;; "(+ x 1)" appears twice: in the string and in code
        ;; Provide enough context to uniquely match the code occurrence
        (lisp-patch-form :file-path path
                         :form-type "defun"
                         :form-name "target"
                         :old-text (format nil "  (+ x 1))")
                         :new-text (format nil "  (* x 2))"))
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 2)" updated))
          ;; String literal should be preserved
          (ok (search "\"call (+ x 1) here\"" updated)))))))

;;; ============================================================
;;; Readtable support
;;; ============================================================

(deftest lisp-patch-form-auto-detected-readtable
  (testing "patch validates modified form using auto-detected readtable from in-readtable"
    (handler-case
        (progn
          (unless (%try-load :cl-interpol) (error "not available"))
          (with-temp-file "tests/tmp/patch-auto-readtable.lisp"
              (format nil
                      "(in-package :cl-user)~%(named-readtables:in-readtable :interpol-syntax)~%~%(defun greet (name)~%  #?\"Hello, ${name}!\")~%")
            (lambda (path)
              ;; Patch without explicit readtable parameter - should auto-detect
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "greet"
                               :old-text "Hello"
                               :new-text "Hi")
              (let ((updated (fs-read-file path)))
                (ok (search "#?\"Hi, ${name}!\"" updated))
                (ok (null (search "Hello" updated)))))))
      (error ()
        (skip "cl-interpol not available")))))

;;; ============================================================
;;; Schema validation
;;; ============================================================

(deftest lisp-patch-form-schema-validates
  (testing "inputSchema has correct required fields and no structural/edit params"
    (let* ((descriptor (cl-mcp/src/lisp-patch-form::lisp-patch-form-descriptor))
           (schema (gethash "inputSchema" descriptor)))
      (ok (string= "object" (gethash "type" schema))
          "inputSchema should be a top-level object schema")
      ;; Required fields
      (let ((required (gethash "required" schema)))
        (ok (find "file_path" required :test #'string=)
            "file_path should be required")
        (ok (find "form_type" required :test #'string=)
            "form_type should be required")
        (ok (find "form_name" required :test #'string=)
            "form_name should be required")
        (ok (find "old_text" required :test #'string=)
            "old_text should be required")
        (ok (find "new_text" required :test #'string=)
            "new_text should be required"))
      ;; Should NOT have structural operation params
      (let ((properties (gethash "properties" schema)))
        (ok (null (gethash "content" properties))
            "content property should not exist")
        (ok (null (gethash "operation" properties))
            "operation property should not exist")
        (ok (null (gethash "normalize_blank_lines" properties))
            "normalize_blank_lines property should not exist")
        ;; Should have patch-specific params
        (ok (gethash "old_text" properties)
            "old_text property should exist")
        (ok (gethash "new_text" properties)
            "new_text property should exist")
        (ok (gethash "dry_run" properties)
            "dry_run property should exist")
        (ok (gethash "readtable" properties)
            "readtable property should exist")))))

;;; ============================================================
;;; Handler integration
;;; ============================================================

(deftest lisp-patch-form-handler-returns-tool-error
  (testing "handler returns isError for patch operational errors, not -32603"
    (with-temp-file "tests/tmp/patch-handler-tool-error.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (_ (setf (cl-mcp/src/state:protocol-version state) "2025-11-25"))
               (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "old_text" "nonexistent text"
                      "new_text" "replacement")))
          (declare (ignore _))
          ;; old_text not found → should be tool-error, not -32603
          (let* ((response (funcall handler state "test-patch-1" args))
                 (result-obj (gethash "result" response))
                 (is-error (and result-obj (gethash "isError" result-obj)))
                 (content (and result-obj (gethash "content" result-obj)))
                 (text (and content (> (length content) 0)
                            (gethash "text" (aref content 0)))))
            ;; Should NOT be an rpc error (-32603)
            (ng (gethash "error" response)
                "patch old_text-not-found should not produce rpc error -32603")
            ;; Should have result with isError
            (ok result-obj "response should have result field")
            (ok is-error "result should have isError = true")
            (ok (and text (search "old_text not found" text))
                "error message should mention old_text not found")))))))

(deftest lisp-patch-form-old-protocol-returns-32602
  (testing "old protocol: old_text not found returns -32602 not -32603"
    (with-temp-file "tests/tmp/patch-old-proto-notfound.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "old_text" "nonexistent text"
                      "new_text" "replacement")))
          (let* ((response (funcall handler state "test-old-1" args))
                 (err (gethash "error" response)))
            (ok err "old protocol should produce rpc error")
            (ok (eql -32602 (gethash "code" err))
                "error code should be -32602 not -32603")
            (ok (search "old_text not found" (gethash "message" err))
                "error message should mention old_text not found"))))))
  (testing "old protocol: empty old_text returns -32602 not -32603"
    (with-temp-file "tests/tmp/patch-old-proto-empty.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "old_text" ""
                      "new_text" "replacement")))
          (let* ((response (funcall handler state "test-old-2" args))
                 (err (gethash "error" response)))
            (ok err "old protocol should produce rpc error for empty old_text")
            (ok (eql -32602 (gethash "code" err))
                "error code should be -32602 not -32603")))))))

(deftest lisp-patch-form-handler-form-not-found-tool-error
  (testing "form not found at 2025-11-25 returns isError, not an internal error"
    (with-temp-file "tests/tmp/patch-handler-form-not-found.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "no-such-form-xyzzy"
                     "old_text" "(+ x 1)"
                     "new_text" "(* x 2)")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "test-nf-1" args))
                 (result-obj (gethash "result" response))
                 (content (and result-obj (gethash "content" result-obj)))
                 (text (and content (> (length content) 0)
                            (gethash "text" (aref content 0)))))
            (ng (gethash "error" response)
                "form-not-found at 2025-11-25 should not produce an rpc error")
            (ok result-obj "response should have result field")
            (ok (and result-obj (gethash "isError" result-obj))
                "result should have isError = true")
            (ok (and text (search "not found" text))
                "message should say the form was not found")
            (ok (and text (null (search "Internal error during" text)))
                "message must not carry an internal-error prefix")))))))

(deftest lisp-patch-form-handler-form-not-found-legacy-protocol
  (testing "form not found on a legacy protocol returns -32603, as lisp-edit-form does"
    (with-temp-file "tests/tmp/patch-handler-form-not-found-legacy.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "no-such-form-xyzzy"
                     "old_text" "(+ x 1)"
                     "new_text" "(* x 2)")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-06-18")
          (let* ((response (funcall handler state "test-nf-2" args))
                 (err (gethash "error" response))
                 (message (and err (gethash "message" err))))
            (ok err "legacy protocol should produce an rpc error")
            (ok (eql -32603 (gethash "code" err))
                "form-not-found is an internal error, not invalid params")
            (ok (and message (search "not found" message))
                "message should say the form was not found")
            (ok (and message (null (search "Internal error during" message)))
                "message must not carry an internal-error prefix")))))))

(deftest lisp-patch-form-handler-post-prologue-failure-tool-error
  (testing "unwritable target at 2025-11-25 returns isError without an internal-error prefix"
    (with-readonly-temp-file "tests/tmp/patch-readonly/target.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "target"
                     "old_text" "(+ x 1)"
                     "new_text" "(* x 2)")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "test-pp-1" args))
                 (result-obj (gethash "result" response))
                 (content (and result-obj (gethash "content" result-obj)))
                 (text (and content (> (length content) 0)
                            (gethash "text" (aref content 0)))))
            (ng (gethash "error" response)
                "post-prologue failure at 2025-11-25 should not produce an rpc error")
            (ok result-obj "response should have result field")
            (ok (and result-obj (gethash "isError" result-obj))
                "result should have isError = true")
            (ok (and text (null (search "Internal error during" text)))
                "message must not carry an internal-error prefix")
            ;; Deliberately not asserting on the errno text: that comes from
            ;; glibc strerror and is localized by LC_MESSAGES. Assert on the
            ;; fixture path instead, which is ours and locale-stable.
            (ok (and text (search "patch-readonly" text))
                "message should name the file that could not be written")))))))

(deftest lisp-patch-form-handler-post-prologue-failure-legacy-protocol
  (testing "unwritable target on a legacy protocol returns a bare -32603"
    (with-readonly-temp-file "tests/tmp/patch-readonly-legacy/target.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "target"
                     "old_text" "(+ x 1)"
                     "new_text" "(* x 2)")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-06-18")
          (let* ((response (funcall handler state "test-pp-2" args))
                 (err (gethash "error" response))
                 (message (and err (gethash "message" err))))
            (ok err "legacy protocol should produce an rpc error")
            (ok (eql -32603 (gethash "code" err))
                "post-prologue failure is an internal error")
            (ok (and message (null (search "Internal error during" message)))
                "message must not carry an internal-error prefix")
            (ok (and message (search "patch-readonly-legacy" message))
                "message should name the file that could not be written")))))))

(deftest lisp-patch-form-depth-mismatch-fewer-closes
  (testing "new_text missing a ) breaks the form, so the depth message is reported"
    (with-temp-file "tests/tmp/patch-depth-fewer.lisp"
        (format nil "(defun target (x)~%  (if (> x 0)~%      (print x)~%      nil))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(print x)"
                               :new-text "(print x")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg)
          (ok (search "new_text closes 1 fewer \")\" than old_text" err-msg))
          (ok (search "(old_text: 1 open / 1 close, new_text: 1 open / 0 close)" err-msg))
          (ok (search "The patch would leave the form unclosed." err-msg))
          (ok (search "Add 1 \")\" to new_text, or remove 1 \"(\"." err-msg))
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-mismatch-more-closes
  (testing "new_text with an extra ) fails to parse and gets the opposite advice"
    (with-temp-file "tests/tmp/patch-depth-more.lisp"
        (format nil "(defun target (x)~%  (if (> x 0)~%      (print x)~%      nil))~%")
      (lambda (path)
        (let ((err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(print x)"
                               :new-text "(print x))")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok (search "new_text closes 1 more \")\" than old_text" err-msg))
          (ok (search "The patch would add an extra closing parenthesis." err-msg))
          (ok (search "Remove 1 \")\" from new_text, or add 1 \"(\"." err-msg)))))))

(deftest lisp-patch-form-depth-reason-respects-string-context
  (testing "a ) inside a string is not code, so an unrelated reader error is not blamed on depth"
    (with-temp-file "tests/tmp/patch-string-context.lisp"
        (format nil "(defun target ()~%  \"a)\")~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "a)"
                               :new-text "b\" #?")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the #? dispatch is invalid, so the patch must fail")
          (ok (null (search "fewer \")\"" err-msg))
              "no false net-parenthesis message")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-requires-matching-boundary-state
  (testing "new_text that opens a string past the replacement does not get a depth message"
    (with-temp-file "tests/tmp/patch-boundary-state.lisp"
        (format nil "(defun target ()~%  (foo))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              ;; (foo -> "( leaves an unterminated string that swallows the
              ;; unchanged suffix; the failure is the string, not the parens.
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(foo"
                               :new-text "\"(")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (null (search "more \")\"" err-msg))
              "no manufactured net-parenthesis message")
          (ok (null (search "fewer \")\"" err-msg)))
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-sees-pending-string-escape
  (testing "a trailing backslash inside the new string swallows the suffix quote"
    (with-temp-file "tests/tmp/patch-boundary-escape.lisp"
        (format nil "(defun target ()~%  (foo \"x\"))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              ;; (foo "x -> "(\ : both ends are inside a string, but the new
              ;; one has an escape pending that eats the following quote.
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(foo \"x"
                               :new-text "\"(\\")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (null (search "more \")\"" err-msg))
              "no manufactured net-parenthesis message")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-sees-pending-code-escape
  (testing "new_text ending in a code escape that eats the suffix ) gets no depth message"
    (with-temp-file "tests/tmp/patch-boundary-code-escape.lisp"
        (format nil "(defun target ()~%  foo)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              ;; foo -> (\ : the backslash escapes the original ), so adding
              ;; one ) to new_text would be escaped too and not repair it.
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "foo"
                               :new-text "(\\")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (null (search "fewer \")\"" err-msg))
              "no manufactured net-parenthesis message")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-sees-cut-off-character-literal
  (testing "new_text ending in #\\ turns the suffix ) into a character literal"
    (with-temp-file "tests/tmp/patch-boundary-char-literal.lisp"
        (format nil "(defun target ()~%  foo)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "foo"
                               :new-text "(#\\")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (null (search "fewer \")\"" err-msg))
              "no manufactured net-parenthesis message")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-suppressed-under-custom-readtable
  (testing "with an in-readtable declaration that changes the syntax, no net-parenthesis message"
    (if (%try-load "named-readtables")
        (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                           :cl-mcp-test-patch-depth-syntax :merge '(:standard))))
          (set-dispatch-macro-character
           #\# #\?
           (lambda (s c n) (declare (ignore c n)) (read-line s nil ""))
           rt)
          (unwind-protect
               (with-temp-file "tests/tmp/patch-custom-readtable-depth.lisp"
                   (format nil "(named-readtables:in-readtable ~
                                :cl-mcp-test-patch-depth-syntax)~%~
                                (defun target (x)~%  (print x))~%")
                 (lambda (path)
                   (let ((before (fs-read-file path))
                         (err-msg nil))
                     (handler-case
                         (lisp-patch-form :file-path path
                                          :form-type "defun"
                                          :form-name "target"
                                          :old-text "(print x)"
                                          :new-text "(print x")
                       (error (e) (setf err-msg (princ-to-string e))))
                     (ok err-msg "the patch must still fail")
                     (ok (null (search "fewer \")\"" err-msg))
                         "standard lexical rules are not trusted under a custom readtable")
                     (ok (search "No changes were written to disk." err-msg))
                     (ok (string= before (fs-read-file path))))))
            (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                     :cl-mcp-test-patch-depth-syntax)))
        (rove:skip "named-readtables not available")))
  (testing "an in-readtable :standard declaration keeps the net-parenthesis message"
    (with-temp-file "tests/tmp/patch-standard-readtable-depth.lisp"
        (format nil "(in-readtable :standard)~%(defun target (x)~%  (print x))~%")
      (lambda (path)
        (let ((err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(print x)"
                               :new-text "(print x")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok (and err-msg (search "fewer \")\"" err-msg))
              ":standard reads standard syntax, so the depth message applies"))))))

(deftest lisp-patch-form-diagnosis-suppressed-under-custom-readtable
  (testing "with an in-readtable that changes the syntax, a parse failure keeps the reader's message"
    (if (%try-load "named-readtables")
        (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                           :cl-mcp-test-patch-diag-syntax :merge '(:standard))))
          (set-dispatch-macro-character
           #\# #\?
           (lambda (s c n) (declare (ignore c n)) (read-line s nil ""))
           rt)
          (unwind-protect
               (with-temp-file "tests/tmp/patch-custom-readtable-diagnosis.lisp"
                   (format nil "(named-readtables:in-readtable ~
                                :cl-mcp-test-patch-diag-syntax)~%~
                                (defun target ()~%  1)~%")
                 (lambda (path)
                   (let ((before (fs-read-file path))
                         (err-msg nil))
                     (handler-case
                         ;; #. is disabled, so the reader fails; the standard
                         ;; delimiter scan must not replace that with a
                         ;; bracket diagnosis.
                         (lisp-patch-form :file-path path
                                          :form-type "defun"
                                          :form-name "target"
                                          :old-text "1"
                                          :new-text "#.(+ 1 1) [(]")
                       (error (e) (setf err-msg (princ-to-string e))))
                     (ok err-msg "the patch must fail")
                     (ok (search "invalid Lisp" err-msg))
                     (ok (null (search "Unbalanced parentheses" err-msg))
                         "no standard-syntax diagnosis under a custom readtable")
                     (ok (search "No changes were written to disk." err-msg))
                     (ok (string= before (fs-read-file path))))))
            (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                     :cl-mcp-test-patch-diag-syntax)))
        (rove:skip "named-readtables not available"))))

(deftest lisp-patch-form-ambiguous-bracket-keeps-reader-error
  (testing "a [ in a symbol plus a real reader error reports both, not only the hedge"
    (with-temp-file "tests/tmp/patch-ambiguous-bracket.lisp"
        (format nil "(defun target ()~%  1)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "1"
                               :new-text "foo[ #?")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (search "false positive" err-msg) "the bracket diagnosis is hedged")
          (ok (search "The reader itself reported" err-msg) "the reader error is kept")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-ambiguous-closing-bracket-keeps-reader-error
  (testing "a symbol ending in ] plus a real reader error keeps the reader error too"
    (with-temp-file "tests/tmp/patch-ambiguous-closing-bracket.lisp"
        (format nil "(defun target ()~%  1)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "1"
                               :new-text "foo] #?")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (search "The reader itself reported" err-msg) "the reader error is kept")
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-ignores-parens-in-multiple-escape
  (testing "a ( inside a |...| symbol does not manufacture a net-parenthesis message"
    (with-temp-file "tests/tmp/patch-multiple-escape.lisp"
        (format nil "(defun target ()~%  (list 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(list 1)"
                               :new-text "(list '|a(b| #?)")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the #? dispatch is invalid, so the patch must fail")
          (ok (null (search "fewer \")\"" err-msg))
              "new_text is balanced; the ( is symbol text")
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-bracket-typo-beats-depth-message
  (testing "a ] typed for ) is reported as the typo, not as a missing )"
    (with-temp-file "tests/tmp/patch-bracket-typo.lisp"
        (format nil "(defun target ()~%  (list 1 2))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(list 1 2)"
                               :new-text "(list 1 2]")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (null (search "fewer \")\"" err-msg))
              "no net-count instruction that would write (list 1 2])")
          (ok (search "found \"]\"" err-msg) "the ] is named")
          (ok (search "Replace it with \")\"" err-msg))
          (ok (search "The reader itself reported: unexpected end of input" err-msg)
              "the reader's own words are kept for the ambiguous bracket")
          (ok (null (search "invalid Lisp" err-msg
                            :start2 (1+ (search "invalid Lisp" err-msg))))
              "the framing sentence appears once, not inside the reader text too")
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-compares-block-comment-depth
  (testing "boundaries inside block comments at different nesting depths do not match"
    (with-temp-file "tests/tmp/patch-boundary-block-depth.lisp"
        (format nil "(defun target ()~%  (foo #| c |#))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              ;; (foo #| -> ((#| #| : the suffix's |# now closes only the
              ;; inner comment, so the outer one swallows the closing parens.
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(foo #|"
                               :new-text "((#| #|")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (null (search "fewer \")\"" err-msg))
              "no manufactured net-parenthesis message")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-reason-pending-constructs-at-boundary
  (flet ((patch-error (relative initial old new)
           (with-temp-file relative initial
             (lambda (path)
               (let ((before (fs-read-file path))
                     (err-msg nil))
                 (handler-case
                     (lisp-patch-form :file-path path
                                      :form-type "defun"
                                      :form-name "target"
                                      :old-text old
                                      :new-text new)
                   (error (e) (setf err-msg (princ-to-string e))))
                 (ok err-msg "the patch must fail")
                 (ok (null (search "fewer \")\"" err-msg))
                     "no manufactured net-parenthesis message")
                 (ok (search "No changes were written to disk." err-msg))
                 (ok (string= before (fs-read-file path))))))))
    (testing "new_text ending with | inside a block comment whose |# needs the suffix"
      (patch-error "tests/tmp/patch-boundary-block-close.lisp"
                   (format nil "(defun target ()~%  foo #\\))~%")
                   "foo " "(#|x|"))
    (testing "new_text ending with a quote prefix that needs a following form"
      (patch-error "tests/tmp/patch-boundary-quote.lisp"
                   (format nil "(defun target ()~%  foo)~%")
                   "foo" "('"))))

(deftest lisp-patch-form-depth-reason-never-trusts-pending-boundaries
  (testing "two pending boundaries of different kinds do not count as matching"
    (with-temp-file "tests/tmp/patch-boundary-pending-kinds.lisp"
        (format nil "(defun target ()~%  foo #|x|#)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              ;; old ends with | (closes the comment with the suffix #),
              ;; new ends with # (does not): both :pending, not equivalent.
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "foo #|x|"
                               :new-text "(foo #|x#")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg "the patch must fail")
          (ok (null (search "fewer \")\"" err-msg))
              "no manufactured net-parenthesis message")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-depth-check-ignores-strings-and-char-literals
  (testing "parens inside strings and #\\( do not trip the depth check"
    (with-temp-file "tests/tmp/patch-depth-strings.lisp"
        (format nil "(defun target ()~%  (list 1))~%")
      (lambda (path)
        (lisp-patch-form :file-path path
                         :form-type "defun"
                         :form-name "target"
                         :old-text "(list 1)"
                         :new-text "(list \")\" #\\( 1)")
        (ok (search "(list \")\" #\\( 1)" (fs-read-file path)))))))

(deftest lisp-patch-form-depth-mismatch-inside-docstring-is-allowed
  (testing "an unbalanced paren added inside a docstring still parses, so it is applied"
    (with-temp-file "tests/tmp/patch-depth-docstring.lisp"
        (format nil "(defun target (x)~%  \"Return (1-based index.\"~%  x)~%")
      (lambda (path)
        (lisp-patch-form :file-path path
                         :form-type "defun"
                         :form-name "target"
                         :old-text "(1-based index."
                         :new-text "(1-based index.)")
        (ok (search "(1-based index.)\"" (fs-read-file path)))))))

(deftest lisp-patch-form-empty-old-text-is-an-argument-error
  (testing "an empty old_text reports the argument problem, not a depth message"
    (with-temp-file "tests/tmp/patch-empty-old-text.lisp"
        (format nil "(defun target (x)~%  x)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text ""
                               :new-text "(x")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg)
          (ok (search "old_text must not be empty" err-msg))
          (ng (search "new_text closes" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-nesting-breakage-gets-diagnosis
  (testing "equal depth but an early ) yields trailing content and a diagnosis"
    (with-temp-file "tests/tmp/patch-nesting.lisp"
        (format nil "(defun target (x)~%  (let ((y 1))~%    (print y)~%    y))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (handler-case
              (lisp-patch-form :file-path path
                               :form-type "defun"
                               :form-name "target"
                               :old-text "(let ((y 1))"
                               :new-text ")) (let ((y 1)) ((")
            (error (e) (setf err-msg (princ-to-string e))))
          (ok err-msg)
          (ok (search "invalid Lisp" err-msg))
          (ok (search "Unbalanced parentheses in the patched form" err-msg))
          (ok (search "line 2" err-msg)
              "the inferred line of the early ) is named")
          (ok (search "No changes were written to disk." err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-patch-form-broken-file-gives-guidance
  (testing "patching a file that does not parse returns the shared guidance"
    (with-temp-file "tests/tmp/patch-broken-file.lisp"
        (format nil "(defun a ()~%  (list 1)~%~%(defun b ()~%  2)~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-patch-form::lisp-patch-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "b"
                     "old_text" "2"
                     "new_text" "3")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "patch-broken-1" args))
                 (result-obj (gethash "result" response))
                 (text (gethash "text" (aref (gethash "content" result-obj) 0))))
            (ng (gethash "error" response))
            (ok (gethash "isError" result-obj))
            (ok (search "unclosed (form starting at line 1: \"(defun a ()\")" text))
            (ok (search "Next top-level form begins at line 4" text))
            (ok (search "Run lisp-check-parens with path=" text))))))))
