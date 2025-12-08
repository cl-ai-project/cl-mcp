;;;; tests/edit-lisp-form-test.lisp

(defpackage #:cl-mcp/tests/edit-lisp-form-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/edit-lisp-form
                #:edit-lisp-form)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:merge-pathnames*
                #:native-namestring
                #:ensure-directories-exist))

(in-package #:cl-mcp/tests/edit-lisp-form-test)

(setf cl-mcp/src/fs:*project-root*
      (uiop:ensure-directory-pathname (system-source-directory :cl-mcp)))

(defun project-path (relative)
  "Return an absolute namestring under the cl-mcp project for RELATIVE."
  (native-namestring
   (merge-pathnames* relative (system-source-directory :cl-mcp))))

(defun with-temp-file (relative initial thunk)
  "Create RELATIVE file with INITIAL content, call THUNK with absolute path, then clean up."
  (let* ((abs (project-path relative)))
    (ensure-directories-exist abs)
    (fs-write-file relative initial)
    (unwind-protect
         (funcall thunk abs)
      (ignore-errors (delete-file abs)))))

(deftest edit-lisp-form-replace-defun
  (testing "replace updates function body"
    (with-temp-file "tests/tmp/edit-form-replace.lisp"
        "(defun target (x)\n  (+ x 1))\n\n(defun untouched () :ok)\n"
      (lambda (path)
        (edit-lisp-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content "(defun target (x)\n  (* x 2))")
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 2)" updated))
          (ok (null (search "(+ x 1)" updated))))))))

(deftest edit-lisp-form-insert-before
  (testing "insert_before inserts helper before target defun"
    (with-temp-file "tests/tmp/edit-form-insert-before.lisp"
        "(defun target (x)\n  (+ x 1))\n"
      (lambda (path)
        (edit-lisp-form :file-path path
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

(deftest edit-lisp-form-insert-after-defmethod
  (testing "insert_after matches defmethod with specializers"
    (with-temp-file "tests/tmp/edit-form-insert-after.lisp"
        "(defmethod describe ((obj widget))\n  (list :widget obj))\n"
      (lambda (path)
        (edit-lisp-form :file-path path
                        :form-type "defmethod"
                        :form-name "describe ((obj widget))"
                        :operation "insert_after"
                        :content "(defmethod describe :after ((obj widget))\n  (format t \"done\"))")
        (let* ((text (fs-read-file path))
               (primary (search "defmethod describe ((obj widget))" text))
               (after (search "defmethod describe :after ((obj widget))" text)))
          (ok primary)
          (ok after)
          (ok (< primary after)))))))

(deftest edit-lisp-form-insert-after-preserves-newlines
  (testing "insert_after keeps following whitespace so new form starts on its own line"
    (with-temp-file "tests/tmp/edit-form-insert-after-newlines.lisp"
        (format nil "(defun summarize-tasks ()~%  :ok)~%~%(defun next () :next)~%")
      (lambda (path)
        (edit-lisp-form :file-path path
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

(deftest edit-lisp-form-missing-form-errors
  (testing "missing form signals an error and leaves file unchanged"
    (with-temp-file "tests/tmp/edit-form-missing.lisp"
        "(defun present () :ok)\n"
      (lambda (path)
        (let ((before (fs-read-file path)))
          (ok (handler-case
                  (progn
                    (edit-lisp-form :file-path path
                                    :form-type "defun"
                                    :form-name "absent"
                                    :operation "replace"
                                    :content "(defun absent () nil)")
                    nil)
                (error () t)))
          (ok (string= before (fs-read-file path))))))))

(deftest edit-lisp-form-invalid-content-errors
  (testing "invalid content is rejected before touching the file"
    (with-temp-file "tests/tmp/edit-form-invalid.lisp"
        "(defun sample () :ok)\n"
      (lambda (path)
        (let ((before (fs-read-file path)))
          (ok (handler-case
                  (progn
                    (edit-lisp-form :file-path path
                                    :form-type "defun"
                                    :form-name "sample"
                                    :operation "replace"
                                    :content "(defun sample (")
                    nil)
                (error () t)))
          (ok (string= before (fs-read-file path))))))))

(deftest edit-lisp-form-read-eval-disabled
  (testing "read-time evaluation is disabled when parsing source"
    (let* ((flag-path (project-path "tests/tmp/read-eval-flag"))
           (content (format nil
                            "#.(progn (with-open-file (s \"~A\" :direction :output :if-exists :supersede :if-does-not-exist :create) (write-line \"executed\" s)) '(defun target () :ok))~%\
(defun target () :ok)~%"
                            flag-path)))
      (ignore-errors (delete-file flag-path))
      (unwind-protect
           (with-temp-file "tests/tmp/edit-form-read-eval.lisp"
             content
             (lambda (path)
               (let ((before (fs-read-file path)))
                 (ok (handler-case
                         (progn
                           (edit-lisp-form :file-path path
                                           :form-type "defun"
                                           :form-name "target"
                                           :operation "replace"
                                           :content "(defun target () :updated)")
                           nil)
                       (error () t)))
                 (ok (string= before (fs-read-file path)))
                 (ok (not (probe-file flag-path))))))
        (ignore-errors (delete-file flag-path))))))
