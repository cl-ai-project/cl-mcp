;;;; tests/lisp-edit-form-test.lisp

(defpackage #:cl-mcp/tests/lisp-edit-form-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:merge-pathnames*
                #:native-namestring
                #:ensure-directories-exist))

(in-package #:cl-mcp/tests/lisp-edit-form-test)

(setf cl-mcp/src/fs:*project-root*
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

(deftest lisp-edit-form-replace-defun
  (testing "replace updates function body"
    (with-temp-file "tests/tmp/edit-form-replace.lisp"
        "(defun target (x)\n  (+ x 1))\n\n(defun untouched () :ok)\n"
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content "(defun target (x)\n  (* x 2))")
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 2)" updated))
          (ok (null (search "(+ x 1)" updated))))))))

(deftest lisp-edit-form-dry-run-preview
  (testing "dry-run returns preview without writing the file"
    (with-temp-file "tests/tmp/edit-form-dry-run.lisp"
        "(defun target () :old)\n"
      (lambda (path)
        (let ((before (fs-read-file path))
              (result (lisp-edit-form :file-path path
                                      :form-type "defun"
                                      :form-name "target"
                                      :operation "replace"
                                      :content "(defun target () :new)"
                                      :dry-run t)))
          (let ((after (fs-read-file path)))
            (ok (hash-table-p result))
            (ok (gethash "would_change" result))
            (ok (string= before after))
            (ok (string= "(defun target () :old)" (gethash "original" result)))
            (ok (search ":new" (gethash "preview" result)))))))))

(deftest lisp-edit-form-insert-before
  (testing "insert_before inserts helper before target defun"
    (with-temp-file "tests/tmp/edit-form-insert-before.lisp"
        "(defun target (x)\n  (+ x 1))\n"
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "insert_before"
                        :content "(defun helper (y)\n  (- y 1))")
        (let* ((text (fs-read-file path))
               (helper-pos (search "defun helper" text))
               (target-pos (search "defun target" text)))
          (ok helper-pos)
          (ok target-pos)
          (ok (< helper-pos target-pos)))))))

(deftest lisp-edit-form-insert-after-defmethod
  (testing "insert_after matches defmethod with specializers"
    (with-temp-file "tests/tmp/edit-form-insert-after.lisp"
        "(defmethod describe ((obj widget))\n  (list :widget obj))\n"
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "describe ((obj widget))"
                        :operation "insert_after"
                        :content (concatenate 'string
                                   "(defmethod describe :after ((obj widget))"
                                   (format nil "~%  (format t \"done\")")))
        (let* ((text (fs-read-file path))
               (primary (search "defmethod describe ((obj widget))" text))
               (after (search "defmethod describe :after ((obj widget))" text)))
          (ok primary)
          (ok after)
          (ok (< primary after)))))))

(deftest lisp-edit-form-insert-after-preserves-newlines
  (testing "insert_after keeps following whitespace so new form starts on its own line"
    (with-temp-file "tests/tmp/edit-form-insert-after-newlines.lisp"
        (format nil "(defun summarize-tasks ()~%  :ok)~%~%(defun next () :next)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "summarize-tasks"
                        :operation "insert_after"
                        :content "(defun open-tasks (tasks)
  \"Return tasks whose status is :open.\"
  (remove-if-not (lambda (task) (eql :open (task-status task))) tasks))")
        (let* ((text (fs-read-file path))
               (open-pos (search "defun open-tasks" text))
               (next-pos (search "defun next" text)))
          (ok (search (format nil ")~%~%(defun open-tasks") text))
          (ok (null (search (format nil ")~%(defun open-tasks") text)))
          (ok open-pos)
          (ok next-pos)
          (ok (< open-pos next-pos)))))))

(deftest lisp-edit-form-insert-after-adds-blank-line
  (testing "insert_after ensures a blank line when inserting after the final form"
    (with-temp-file "tests/tmp/edit-form-insert-after-blank-line.lisp"
        "(defun alpha () :a)\n"
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "alpha"
                        :operation "insert_after"
                        :content "(defun beta () :b)")
        (let* ((text (fs-read-file path))
               (alpha-pos (search "(defun alpha () :a)" text))
               (beta-pos (search "(defun beta () :b)" text))
               (after-alpha (and alpha-pos (+ alpha-pos (length "(defun alpha () :a)"))))
               (between (and after-alpha beta-pos (subseq text after-alpha beta-pos))))
          (ok alpha-pos)
          (ok beta-pos)
          (ok between)
          (ok (search (format nil "~C~C" #\Newline #\Newline) between))
          (ok (null (search (make-string 3 :initial-element #\Newline) between)))
        )))))

(deftest lisp-edit-form-insert-after-keeps-existing-blank-line
  (testing "insert_after does not add extra blank lines when whitespace already exists"
    (with-temp-file "tests/tmp/edit-form-insert-after-preserve-blank.lisp"
        "(defun alpha () :a)\n\n(defun gamma () :g)\n"
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "alpha"
                        :operation "insert_after"
                        :content "(defun beta () :b)")
        (let* ((text (fs-read-file path))
               (alpha-pos (search "(defun alpha () :a)" text))
               (beta-pos (search "(defun beta () :b)" text))
               (after-alpha (and alpha-pos (+ alpha-pos (length "(defun alpha () :a)"))))
               (between (and after-alpha beta-pos (subseq text after-alpha beta-pos))))
          (ok alpha-pos)
          (ok beta-pos)
          (ok between)
          (ok (search (format nil "~C~C" #\Newline #\Newline) between))
          (ok (null (search (make-string 3 :initial-element #\Newline) between)))
          (ok (search "(defun gamma () :g)" text)))
      ))))

(deftest lisp-edit-form-missing-form-errors
  (testing "missing form signals an error and leaves file unchanged"
    (with-temp-file "tests/tmp/edit-form-missing.lisp"
        "(defun present () :ok)\n"
      (lambda (path)
        (let ((before (fs-read-file path)))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "absent"
                                    :operation "replace"
                                    :content "(defun absent () nil)")
                    nil)
                (error () t)))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-invalid-content-errors
  (testing "invalid content is rejected before touching the file"
    (with-temp-file "tests/tmp/edit-form-invalid.lisp"
        (format nil "(defun sample () :ok)~%")
      (lambda (path)
        (let ((before (fs-read-file path)))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "sample"
                                    :operation "replace"
                                    ;; Multiple forms - cannot be single valid form
                                    :content (format nil "(defun sample () 1) (defun other () 2)"))
                    nil)
                (error () t)))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-read-eval-disabled
  (testing "read-time evaluation is disabled when parsing source"
    (let* ((flag-path (project-path "tests/tmp/read-eval-flag"))
           (content
             (format nil
                     (concatenate
                      'string
                      "#.(progn (with-open-file "
                      "(s \"~A\" :direction :output "
                      ":if-exists :supersede :if-does-not-exist :create) "
                      "(write-line \"executed\" s)) "
                      "'(defun target () :ok))~%"
                      "(defun target () :ok)~%")
                     flag-path)))
      (ignore-errors (delete-file flag-path))
      (unwind-protect
           (with-temp-file "tests/tmp/edit-form-read-eval.lisp"
             content
             (lambda (path)
               (let ((before (fs-read-file path)))
                 (ok (handler-case
                         (progn
                           (lisp-edit-form :file-path path
                                           :form-type "defun"
                                           :form-name "target"
                                           :operation "replace"
                                           :content "(defun target () :updated)")
                           nil)
                       (error () t)))
                 (ok (string= before (fs-read-file path)))
                 (ok (not (probe-file flag-path))))))
        (ignore-errors (delete-file flag-path))))))

(deftest lisp-edit-form-auto-repair-missing-parens
  (testing "missing closing parentheses are automatically added via parinfer"
    (with-temp-file "tests/tmp/edit-form-auto-repair.lisp"
        (format nil "(defun original (x)~%  (+ x 1))~%")
      (lambda (path)
        ;; Provide content with missing closing parens
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "original"
                        :operation "replace"
                        :content (format nil "(defun original (x)~%  (* x 2"))
        (let ((updated (fs-read-file path)))
          ;; Verify the function was replaced and parens were auto-completed
          (ok (search "(* x 2)" updated))
          (ok (null (search "(+ x 1)" updated)))
          ;; Verify the updated content is valid Lisp (can be read)
          (ok (handler-case
                  (let ((*read-eval* nil))
                    (read-from-string updated)
                    t)
                (error () nil))))))))

(deftest lisp-edit-form-auto-repair-nested-missing-parens
  (testing "nested forms with missing parens are auto-repaired"
    (with-temp-file "tests/tmp/edit-form-auto-repair-nested.lisp"
        (format nil "(defun helper () :ok)~%")
      (lambda (path)
        ;; Insert a function with multiple missing closing parens
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "helper"
                        :operation "insert_after"
                        :content (format nil
                                   "(defun process (data)~%  (when data~%~
                                    (print data)~%    (+ 1 2"))
        (let ((updated (fs-read-file path)))
          (ok (search "(defun helper () :ok)" updated))
          (ok (search "(defun process (data)" updated))
          ;; Verify all forms in the file are valid
          (ok (handler-case
                  (let ((*read-eval* nil)
                        (forms 0))
                    (with-input-from-string (s updated)
                      (loop for form = (read s nil :eof)
                            until (eq form :eof)
                            do (incf forms)))
                    (= forms 2))
                (error () nil))))))))
