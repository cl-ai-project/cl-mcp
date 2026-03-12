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
          (ok (search "invalid Lisp" err-msg))
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
                  (search "malformed form text" err-msg))
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
