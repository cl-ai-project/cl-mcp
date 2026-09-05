;;;; tests/lisp-edit-form-test.lisp

(defpackage #:cl-mcp/tests/lisp-edit-form-test
  (:use #:cl)
    (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:ng
                #:skip)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%normalize-string
                #:file-unparseable-error)
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

(defun large-file-source (form-count)
  "Return Lisp source with a `target' defun followed by FORM-COUNT filler defuns.
Used to prove that a dry-run summary does not grow with the size of the file."
  (with-output-to-string (s)
    (format s "(defun target () :old)~%~%")
    (dotimes (i form-count)
      (format s "(defun filler-~D (x)~%  ;; padding to keep this fixture large~%  (+ x ~D))~%~%"
              i i))))

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

(deftest lisp-edit-form-replace-with-comment-only
  (testing "replace accepts comment-only content as a deletion marker"
    (with-temp-file "tests/tmp/edit-form-replace-comment.lisp"
        "(defun keep (x) x)

(defun to-delete () :gone)

(defun also-keep () :ok)
"
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "to-delete"
                        :operation "replace"
                        :content ";; to-delete was removed by dogfooding cleanup")
        (let ((updated (fs-read-file path)))
          (ok (search ";; to-delete was removed" updated))
          (ok (null (search "(defun to-delete" updated)))
          (ok (search "(defun keep" updated))
          (ok (search "(defun also-keep" updated)))))))

(deftest lisp-edit-form-insert-after-comment-only
  (testing "insert_after accepts a bare comment as content"
    (with-temp-file "tests/tmp/edit-form-insert-comment.lisp"
        "(defun keep (x) x)
"
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "keep"
                        :operation "insert_after"
                        :content ";; TODO: add more helpers below")
        (let ((updated (fs-read-file path)))
          (ok (search ";; TODO: add more helpers below" updated))
          (ok (search "(defun keep" updated)))))))

(deftest lisp-edit-form-rejects-truly-empty-content
  (testing "whitespace-only content is still rejected"
    (with-temp-file "tests/tmp/edit-form-empty.lisp"
        "(defun target () :ok)
"
      (lambda (path)
        (let ((raised nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "target"
                              :operation "replace"
                              :content (format nil "   ~%  "))
            (error () (setf raised t)))
          (ok raised))))))

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

(deftest lisp-edit-form-insert-before-normalizes-blank-lines
  (testing "insert_before normalizes blank lines around inserted form"
    (with-temp-file "tests/tmp/edit-form-insert-before-normalize-blank-lines.lisp"
        (format nil "(defun alpha () :a)~%(defun target () :t)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "insert_before"
                        :content "(defun beta () :b)")
        (let ((text (fs-read-file path)))
          (ok (search (format nil "(defun alpha () :a)~%~%(defun beta () :b)") text))
          (ok (search (format nil "(defun beta () :b)~%~%(defun target () :t)") text))
          (ok (null (search
                     (format nil "(defun alpha () :a)~%(defun beta () :b)")
                     text)))
          (ok (null (search
                     (format nil "(defun beta () :b)~%(defun target () :t)")
                     text))))))))

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
          (ok (null (search (make-string 3 :initial-element #\Newline) between))))))))

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
          (ok (search "(defun gamma () :g)" text)))))))

(deftest lisp-edit-form-replace-normalizes-blank-lines-around-target
  (testing "replace normalizes blank lines before and after the edited form"
    (with-temp-file "tests/tmp/edit-form-replace-normalize-blank-lines.lisp"
        (format nil
                "(defun alpha () :a)~%~%~%(defun target () :old)~%(defun omega () :z)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content "(defun target () :new)")
        (let ((text (fs-read-file path)))
          (ok (search (format nil "(defun alpha () :a)~%~%(defun target () :new)") text))
          (ok (null (search
                     (format nil "(defun alpha () :a)~%~%~%(defun target () :new)")
                     text)))
          (ok (search (format nil "(defun target () :new)~%~%(defun omega () :z)") text))
          (ok (null (search
                     (format nil "(defun target () :new)~%(defun omega () :z)")
                     text))))))))

(deftest lisp-edit-form-replace-preserves-spacing-when-normalization-disabled
  (testing "replace keeps existing spacing when normalize_blank_lines is nil"
    (with-temp-file "tests/tmp/edit-form-replace-preserve-spacing.lisp"
        (format nil
                "(defun alpha () :a)~%(defun target () :old)~%~%~%(defun omega () :z)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content "(defun target () :new)"
                        :normalize-blank-lines nil)
        (let ((text (fs-read-file path)))
          (ok (search (format nil "(defun alpha () :a)~%(defun target () :new)") text))
          (ok (search
               (format nil "(defun target () :new)~%~%~%(defun omega () :z)")
               text))
          (ok (null (search
                     (format nil "(defun alpha () :a)~%~%(defun target () :new)")
                     text))))))))

(deftest lisp-edit-form-insert-after-normalizes-following-boundary
  (testing "insert_after ensures one blank line before both adjacent forms"
    (with-temp-file "tests/tmp/edit-form-insert-after-following-boundary.lisp"
        (format nil "(defun alpha () :a)~%(defun omega () :z)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "alpha"
                        :operation "insert_after"
                        :content "(defun beta () :b)")
        (let ((text (fs-read-file path)))
          (ok (search (format nil "(defun alpha () :a)~%~%(defun beta () :b)") text))
          (ok (search (format nil "(defun beta () :b)~%~%(defun omega () :z)") text))
          (ok (null (search
                     (format nil "(defun beta () :b)~%(defun omega () :z)")
                     text))))))))

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
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "sample"
                                    :operation "replace"
                                    ;; Multiple forms - cannot be single valid form
                                    :content (format nil "(defun sample () 1) (defun other () 2)"))
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (stringp err-msg))
          (ok (search "content must contain exactly one top-level form" err-msg))
          (ok (search "multiple forms are not supported in a single call" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-trailing-garbage-errors
  (testing "trailing malformed content is not classified as multiple forms"
    (with-temp-file "tests/tmp/edit-form-trailing-garbage.lisp"
        (format nil "(defun sample () :ok)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "sample"
                                    :operation "replace"
                                    :content "(defun sample () :new) #<")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (stringp err-msg))
          (ok (search "trailing malformed characters" err-msg))
          (ok (null (search "multiple forms are not supported in a single call" err-msg)))
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

(deftest lisp-edit-form-auto-repair-extra-trailing-paren
  (testing "extra trailing close paren is auto-repaired"
    (with-temp-file "tests/tmp/edit-form-auto-repair-extra-close.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        ;; Content has an extra trailing ")" but should still be repairable.
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content "(defun target () :new))")
        (let ((updated (fs-read-file path)))
          (ok (search "(defun target () :new)" updated))
          (ok (null (search ":old" updated)))
          (ok (handler-case
                  (let ((*read-eval* nil))
                    (read-from-string updated)
                    t)
                (error () nil))))))))


(deftest lisp-edit-form-auto-repair-dry-run-extra-trailing-paren
  (testing "dry-run preview applies auto-repair but does not write"
    (with-temp-file "tests/tmp/edit-form-auto-repair-dry-run-extra-close.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (result (lisp-edit-form :file-path path
                                      :form-type "defun"
                                      :form-name "target"
                                      :operation "replace"
                                      :content "(defun target () :new))"
                                      :dry-run t)))
          (ok (hash-table-p result))
          (ok (gethash "would_change" result))
          (ok (search "(defun target () :new)" (gethash "preview" result)))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-auto-repair-insert-after-extra-trailing-paren
  (testing "insert_after with extra trailing close paren is auto-repaired"
    (with-temp-file "tests/tmp/edit-form-auto-repair-insert-after-extra-close.lisp"
        (format nil "(defun anchor () :ok)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "anchor"
                        :operation "insert_after"
                        :content "(defun repaired () :ok))")
        (let ((updated (fs-read-file path)))
          (ok (search "(defun anchor () :ok)" updated))
          (ok (search "(defun repaired () :ok)" updated))
          (ok (handler-case
                  (let ((*read-eval* nil)
                        (forms 0))
                    (with-input-from-string (s updated)
                      (loop for form = (read s nil :eof)
                            until (eq form :eof)
                            do (incf forms)))
                    (= forms 2))
                (error () nil))))))))

(deftest lisp-edit-form-auto-repair-preserves-neighbor-forms
  (testing "auto-repair only updates target form and keeps neighbors unchanged"
    (with-temp-file "tests/tmp/edit-form-auto-repair-preserve-neighbors.lisp"
        (format nil
                "(defun target (x)~%  (+ x 1))~%~%(defun neighbor () :keep)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content (format nil "(defun target (x)~%  (* x 3"))
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 3)" updated))
          (ok (search "(defun neighbor () :keep)" updated))
          (ok (null (search "(+ x 1)" updated))))))))

(deftest lisp-edit-form-auto-repair-crlf-missing-parens
  (testing "auto-repair handles CRLF content with missing close parens"
    (with-temp-file "tests/tmp/edit-form-auto-repair-crlf.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content (format nil
                                         "(defun target (x)~C~C  (* x 2"
                                         #\Return #\Newline))
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 2)" updated))
          (ok (handler-case
                  (let ((*read-eval* nil))
                    (read-from-string updated)
                    t)
                (error () nil))))))))

(deftest lisp-edit-form-content-with-trailing-whitespace
  (testing "single form content with trailing whitespace is accepted"
    (with-temp-file "tests/tmp/edit-form-trailing-whitespace.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content (format nil "(defun target () :new)~%~%  ~%"))
        (let ((updated (fs-read-file path)))
          (ok (search "(defun target () :new)" updated))
          (ok (null (search ":old" updated))))))))

(deftest lisp-edit-form-content-with-string-parens
  (testing "parenthesis-like characters inside strings do not confuse parsing"
    (with-temp-file "tests/tmp/edit-form-string-parens.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content "(defun target () \"(())\")")
        (let ((updated (fs-read-file path)))
          (ok (search "\"(())\"" updated))
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
        (let ((content (concatenate 'string
                         "(defun process (data)" (string #\Newline)
                         "  (when data" (string #\Newline)
                         "    (print data)" (string #\Newline)
                         "    (+ 1 2")))
          (lisp-edit-form :file-path path
                          :form-type "defun"
                          :form-name "helper"
                          :operation "insert_after"
                          :content content))
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

(deftest lisp-edit-form-with-custom-readtable
  (testing "readtable parameter enables editing files with custom reader macros"
    (handler-case
        (progn
          (unless (%try-load :cl-interpol) (error "not available"))
          (with-temp-file "tests/tmp/edit-form-interpol.lisp"
              (format nil "(in-package :cl-user)~%~%(defun greet (name)~%  #?\"Hello, ${name}!\")~%")
            (lambda (path)
              ;; Edit a file containing cl-interpol #? reader macro
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "greet"
                              :operation "replace"
                              :content (format nil "(defun greet (name)~%  #?\"Hi, ${name}!\")")
                              :readtable :interpol-syntax)
              (let ((updated (fs-read-file path)))
                (ok (search "#?\"Hi, ${name}!\"" updated))
                (ok (null (search "#?\"Hello, ${name}!\"" updated)))))))
      (error ()
        (skip "cl-interpol not available")))))

(deftest lisp-edit-form-ignores-blank-readtable
  (testing "blank readtable arguments are treated as omitted"
    (with-temp-file "tests/tmp/edit-form-blank-readtable.lisp"
        (format nil "(defun greet ()~%  :hello)~%")
      (lambda (path)
        (let ((args (make-hash-table :test #'equal)))
          (setf (gethash "file_path" args) path
                (gethash "form_type" args) "defun"
                (gethash "form_name" args) "greet"
                (gethash "operation" args) "replace"
                (gethash "content" args) (format nil "(defun greet ()~%  :hi)~%")
                (gethash "readtable" args) "")
          (cl-mcp/src/lisp-edit-form::lisp-edit-form-handler
           (cl-mcp/src/state:make-state) 1 args))
        (let ((updated (fs-read-file path)))
          (ok (search ":hi" updated))
          (ok (null (search ":hello" updated))))))))

(deftest lisp-edit-form-auto-detects-in-readtable
  (testing "in-readtable form triggers automatic readtable switching"
    (handler-case
        (progn
          (unless (%try-load :cl-interpol) (error "not available"))
          (with-temp-file "tests/tmp/edit-form-in-readtable.lisp"
              (format nil "(in-package :cl-user)~%(named-readtables:in-readtable :interpol-syntax)~%~%(defun greet (name)~%  #?\"Hello, ${name}!\")~%")
            (lambda (path)
              ;; Without explicit readtable parameter, in-readtable should be auto-detected
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "greet"
                              :operation "replace"
                              :content (format nil "(defun greet (name)~%  #?\"Hi, ${name}!\")"))
              (let ((updated (fs-read-file path)))
                (ok (search "#?\"Hi, ${name}!\"" updated))
                (ok (null (search "#?\"Hello, ${name}!\"" updated)))))))
      (error ()
        (skip "cl-interpol not available")))))

(deftest lisp-edit-form-defmethod-qualifier-only
  (testing "defmethod matches by name + qualifier without lambda-list"
    (with-temp-file "tests/tmp/edit-form-qualifier.lisp"
        (format nil "(defmethod resize ((s shape) factor)~%  (* (slot-value s 'size) factor))~%~%(defmethod resize :after ((s shape) factor)~%  (format t \"resized\"))~%")
      (lambda (path)
        ;; Match by "name :qualifier" pattern (Fix #1)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "resize :after"
                        :operation "replace"
                        :content (format nil "(defmethod resize :after ((s shape) factor)~%  (format t \"resize complete\"))"))
        (let ((updated (fs-read-file path)))
          ;; Verify the :after method was updated
          (ok (search "resize complete" updated))
          (ok (null (search "resized" updated)))
          ;; Verify the primary method is untouched
          (ok (search "(slot-value s 'size)" updated)))))))

(deftest lisp-edit-form-defmethod-uninterned-specializer
  (testing "defmethod with #: uninterned symbols in specializer matches plain form-name"
    (with-temp-file "tests/tmp/edit-form-uninterned.lisp"
        (format nil
                "(defmethod evaluate ((#:e #:binary-op-expr))~%  :binary)~%~%(defmethod evaluate ((#:e #:num-expr))~%  :num)~%")
      (lambda (path)
        ;; User typed form_name without #: prefixes, as in ordinary source
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "evaluate ((e binary-op-expr))"
                        :operation "replace"
                        :content
                        (format nil "(defmethod evaluate ((e binary-op-expr))~%  :binary-replaced)"))
        (let ((updated (fs-read-file path)))
          (ok (search ":binary-replaced" updated))
          (ok (null (search ":binary)" updated)))
          (ok (search ":num)" updated))))))
  (testing "defmethod with #: also matches when form-name is written with #:"
    (with-temp-file "tests/tmp/edit-form-uninterned-hash.lisp"
        (format nil
                "(defmethod evaluate ((#:e #:binary-op-expr))~%  :binary)~%~%(defmethod evaluate ((#:e #:num-expr))~%  :num)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "evaluate ((#:e #:num-expr))"
                        :operation "replace"
                        :content
                        (format nil "(defmethod evaluate ((e num-expr))~%  :num-replaced)"))
        (let ((updated (fs-read-file path)))
          (ok (search ":num-replaced" updated))
          (ok (null (search ":num)" updated)))
          (ok (search ":binary)" updated)))))))

(deftest lisp-edit-form-defmethod-preserves-hash-colon-in-strings
  (testing "#: inside an EQL string specializer is not stripped by the normalizer"
    (with-temp-file "tests/tmp/edit-form-string-hash-colon.lisp"
        (format nil
                "(defmethod tag ((x (eql \"#:keep\")))~%  :hash-keep)~%~%(defmethod tag ((x (eql \"keep\")))~%  :plain-keep)~%")
      (lambda (path)
        ;; Two methods with EQL string specializers differ only by whether
        ;; the literal begins with #:. If %strip-hash-colon blindly removes
        ;; every '#:' substring the two candidates collide and lisp-edit-form
        ;; cannot target either one unambiguously. Matching the quoted form
        ;; exactly must resolve to the '#:keep' variant only.
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "tag ((x (eql \"#:keep\")))"
                        :operation "replace"
                        :content
                        (format nil "(defmethod tag ((x (eql \"#:keep\")))~%  :hash-keep-replaced)"))
        (let ((updated (fs-read-file path)))
          (ok (search ":hash-keep-replaced" updated))
          (ok (null (search ":hash-keep)" updated)))
          ;; The plain "keep" method must be untouched.
          (ok (search ":plain-keep" updated)))))))

(deftest lisp-edit-form-with-package-qualified-readtable
  (testing "readtable parameter supports package-qualified symbol names (pkg:sym format)"
    (handler-case
        (progn
          (unless (%try-load :named-readtables) (error "not available"))
          (unless (%try-load :cl-interpol) (error "not available"))
          ;; Create a test package with a named readtable at runtime
          (let ((test-pkg-name "CL-MCP-EDIT-TEST-PKG-QUALIFIED-RT"))
            (when (find-package test-pkg-name)
              (delete-package test-pkg-name))
            (unwind-protect
                 (progn
                   ;; Create package and register readtable dynamically
                   (eval `(defpackage ,test-pkg-name
                            (:use :cl)))
                   (eval `(in-package ,test-pkg-name))
                   ;; Copy interpol-syntax to our test package's readtable
                   ;; Use find-symbol to avoid read-time package resolution
                   (let ((defreadtable-fn (find-symbol "DEFREADTABLE" :named-readtables)))
                     (eval `(,defreadtable-fn
                                ,(intern "TEST-INTERPOL" test-pkg-name)
                              (:merge :interpol-syntax))))
                   (in-package :cl-mcp/tests/lisp-edit-form-test)
                   ;; Now test editing with package-qualified readtable string
                   (with-temp-file "tests/tmp/edit-form-pkg-qualified-rt.lisp"
                       (format nil "(in-package :cl-user)~%~%(defun greet-pkg (name)~%  #?\"Hello, ${name}!\")~%")
                     (lambda (path)
                       ;; Use the package-qualified format: "pkg:sym"
                       (let ((rt-string (format nil "~A:~A" test-pkg-name "TEST-INTERPOL")))
                         (lisp-edit-form :file-path path
                                         :form-type "defun"
                                         :form-name "greet-pkg"
                                         :operation "replace"
                                         :content (format nil "(defun greet-pkg (name)~%  #?\"Hi, ${name}!\")")
                                         :readtable rt-string)
                         (let ((updated (fs-read-file path)))
                           (ok (search "#?\"Hi, ${name}!\"" updated))
                           (ok (null (search "#?\"Hello, ${name}!\"" updated))))))))
              ;; Cleanup: unregister readtable and delete package
              (ignore-errors
               (let ((rt-sym (find-symbol "TEST-INTERPOL" test-pkg-name))
                     (unregister-fn (find-symbol "UNREGISTER-READTABLE" :named-readtables)))
                 (when (and rt-sym unregister-fn)
                   (funcall unregister-fn rt-sym))))
              (ignore-errors (delete-package test-pkg-name)))))
      (error (e)
        (skip (format nil "Test dependencies not available: ~A" e))))))

(deftest lisp-edit-form-multiple-matches-error
  (testing "multiple matches without index signals descriptive error"
    (with-temp-file "tests/tmp/edit-form-multi-match.lisp"
        (format nil "(defmethod process ((x string))~%  (string-upcase x))~%~%(defmethod process ((x integer))~%  (* x 2))~%")
      (lambda (path)
        (let ((before (fs-read-file path)))
          ;; Matching just "process" should error since there are 2 methods
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defmethod"
                                    :form-name "process"
                                    :operation "replace"
                                    :content "(defmethod process ((x string)) :replaced)")
                    nil)
                (error (e)
                  ;; Error message should mention multiple matches and indices
                  (let ((msg (princ-to-string e)))
                    (and (search "Multiple matches" msg)
                         (search "[0]" msg)
                         (search "[1]" msg))))))
          ;; File should be unchanged
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-index-syntax-selects-match
  (testing "index syntax [N] selects specific match from multiple"
    (with-temp-file "tests/tmp/edit-form-index-select.lisp"
        (format nil "(defmethod process ((x string))~%  (string-upcase x))~%~%(defmethod process ((x integer))~%  (* x 2))~%")
      (lambda (path)
        ;; Use [1] to select the second match (integer specializer)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "process[1]"
                        :operation "replace"
                        :content "(defmethod process ((x integer))~%  (* x 10))")
        (let ((updated (fs-read-file path)))
          ;; Second method (integer) should be updated
          (ok (search "(* x 10)" updated))
          (ok (null (search "(* x 2)" updated)))
          ;; First method (string) should be untouched
          (ok (search "(string-upcase x)" updated)))))))

(deftest normalize-string-uses-symbol-name
  (testing "%normalize-string returns just the symbol name, not package-qualified"
    (let* ((pkg-name "NORMALIZE-TEST-PKG")
           (pkg (or (find-package pkg-name)
                    (make-package pkg-name :use nil))))
      (unwind-protect
           (let ((sym (intern "MY-FUNC" pkg)))
             (ok (string= "my-func"
                          (%normalize-string sym)))
             ;; Also verify non-symbol input still works
             (ok (string= "hello"
                          (%normalize-string "HELLO"))))
        (delete-package pkg-name)))))

(deftest validate-content-with-unknown-package
  (testing "%validate-and-repair-content handles unknown package-qualified symbols"
    (let ((content "(defun process (x) (unknown-val-pkg:transform x))"))
      (ok (stringp
           (cl-mcp/src/lisp-edit-form::%validate-and-repair-content content))))))

(deftest lisp-edit-form-synthesizes-same-file-local-nickname-context
  (testing "lisp-edit-form works when package local nicknames exist only in the file header"
    (let* ((pkg-name "CL-MCP-TMP-LN-SAME-FILE-USER")
           (target-name "CL-MCP-TMP-LN-SAME-FILE-TARGET")
           (relative "tests/tmp/edit-form-local-nickname-same-file.lisp")
           (content
             (format nil
                     "(defpackage #:~A~%  (:use #:cl)~%  (:local-nicknames (#:ad #:~A)))~%~%~
(in-package #:~A)~%~%~
(defun make-thing ()~%  (ad:make-dual 1.0 0.0))~%~%~
(defun other-thing ()~%  (ad:make-dual 2.0 1.0))~%"
                     pkg-name target-name pkg-name)))
      (when (find-package pkg-name)
        (delete-package pkg-name))
      (when (find-package target-name)
        (delete-package target-name))
      (with-temp-file relative content
        (lambda (path)
          (ok (null (find-package pkg-name))
              "user package is not preloaded in parent")
          (ok (null (find-package target-name))
              "nickname target package is not preloaded in parent")
          (lisp-edit-form :file-path path
                          :form-type "defun"
                          :form-name "make-thing"
                          :operation "replace"
                          :content "(defun make-thing ()
  (ad:make-dual 99.0 0.0))")
          (let ((updated (fs-read-file path)))
            (ok (search "99.0" updated))
            (ok (search "other-thing" updated)))
          (ok (null (find-package pkg-name))
              "synthesized user package is cleaned up")
          (ok (null (find-package target-name))
              "synthesized target package is cleaned up"))))))

(deftest lisp-edit-form-parinfer-warning-returned
  (testing "parinfer warning is returned when content is auto-repaired"
    ;; M-4 fix: lisp-edit-form returns parinfer warning as second value
    ;; when closing delimiters are added.
    (with-temp-file "tests/tmp/edit-form-parinfer-warning.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content "(defun target (x)
  (when x
    (print x")
          (declare (ignore updated))
          (ok (stringp warning)
              "should return a parinfer warning string")
          (ok (search "closing delimiter" warning)
              "warning should mention closing delimiters")
          (ok (search "parinfer" warning)
              "warning should mention parinfer")))))
  (testing "no parinfer warning when content is already balanced"
    (with-temp-file "tests/tmp/edit-form-no-parinfer-warning.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content "(defun target () :new)")
          (declare (ignore updated))
          (ok (null warning)
              "should not return warning for balanced content")))))
  (testing "dry-run includes parinfer_warning in result hash-table"
    (with-temp-file "tests/tmp/edit-form-parinfer-warning-dry-run.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((result (lisp-edit-form :file-path path
                                      :form-type "defun"
                                      :form-name "target"
                                      :operation "replace"
                                      :content "(defun target (x)
  (list x"
                                      :dry-run t)))
          (ok (hash-table-p result))
          (ok (gethash "parinfer_warning" result)
              "dry-run result should include parinfer_warning key")
          (ok (search "closing delimiter" (gethash "parinfer_warning" result))
              "dry-run warning should mention closing delimiters"))))))

(deftest lisp-edit-form-replace-no-op
  (testing "replace with identical content does not write file"
    (with-temp-file "tests/tmp/edit-form-replace-noop.lisp"
        (format nil "(defun target () :same)~%")
      (lambda (path)
        (let ((before (fs-read-file path)))
          (multiple-value-bind (updated pw changed-p)
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "target"
                              :operation "replace"
                              :content "(defun target () :same)")
            (declare (ignore pw))
            (ok (stringp updated))
            (ok (null changed-p) "changed-p should be nil for no-op replace")
            (ok (string= before (fs-read-file path))
                "file should not have been rewritten")))))))

;;; ============================================================
;;; Schema and handler tests
;;; ============================================================

(deftest lisp-edit-form-schema-avoids-top-level-combinators
  (testing "inputSchema has 4-value operation enum (incl delete), content optional, no old_text/new_text"
    (let* ((descriptor (cl-mcp/src/lisp-edit-form::lisp-edit-form-descriptor))
           (schema (gethash "inputSchema" descriptor)))
      (ok (string= "object" (gethash "type" schema))
          "inputSchema should remain a top-level object schema")
      (ok (null (gethash "oneOf" schema))
          "top-level oneOf should be absent")
      (ok (null (gethash "allOf" schema))
          "top-level allOf should be absent")
      (ok (null (gethash "anyOf" schema))
          "top-level anyOf should be absent")
      (let* ((properties (gethash "properties" schema))
             (operation (gethash "operation" properties))
             (content (gethash "content" properties))
             (required (gethash "required" schema)))
        (ok operation "operation property should exist")
        (ok content "content property should exist")
        ;; operation enum should have exactly 3 values
        (let ((enum (gethash "enum" operation)))
          (ok (= 4 (length enum))
              "operation enum should have exactly 4 values")
          (ok (find "replace" enum :test #'string=))
          (ok (find "insert_before" enum :test #'string=))
          (ok (find "insert_after" enum :test #'string=))
          (ok (find "delete" enum :test #'string=)))
        ;; content should be in required list
        (ok (find "file_path" required :test #'string=)
            "file_path should remain globally required")
        (ok (find "form_type" required :test #'string=)
            "form_type should remain globally required")
        (ok (find "form_name" required :test #'string=)
            "form_name should remain globally required")
        (ok (find "operation" required :test #'string=)
            "operation should remain globally required")
        (ok (not (find "content" required :test #'string=))
            "content should NOT be globally required (optional for delete)")))))

(deftest lisp-edit-form-handler-returns-tool-error
  (testing "handler returns isError for operational errors on new protocol"
    (with-temp-file "tests/tmp/edit-handler-tool-error.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (_ (setf (cl-mcp/src/state:protocol-version state) "2025-11-25"))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "nonexistent-function"
                      "operation" "replace"
                      "content" "(defun nonexistent-function () nil)")))
          (declare (ignore _))
          (let* ((response (funcall handler state "test-id-1" args))
                 (result-obj (gethash "result" response))
                 (is-error (and result-obj (gethash "isError" result-obj)))
                 (content (and result-obj (gethash "content" result-obj)))
                 (text (and content (> (length content) 0)
                            (gethash "text" (aref content 0)))))
            (ng (gethash "error" response)
                "replace error should not produce rpc error -32603")
            (ok result-obj "response should have result field")
            (ok is-error "result should have isError = true")
            (ok (and text (search "not found" text))
                "error message should mention not found")))))))

(deftest lisp-edit-form-broken-file-gives-guidance
  (testing "editing a file that does not parse names the open form and the next tool"
    (with-temp-file "tests/tmp/edit-form-broken-file.lisp"
        (format nil "(in-package #:cl-user)~%~%(defun probe-a (x)~%  (let ((y (* x 2)))~%    (if (> y 10)~%        (format t \"big\")~%        (format t \"small\")~%    y))~%~%(defun probe-c (x)~%  (list x x x))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "probe-c"
                              :operation "replace"
                              :content "(defun probe-c (x) (list x))")
            (file-unparseable-error (e)
              (setf err (princ-to-string e))))
          (ok err "should signal file-unparseable-error")
          (ok (search "unclosed (form starting at line 3: \"(defun probe-a (x)\")" err))
          (ok (search "Likely fix, inferred from indentation:" err))
          (ok (search "line 7:" err))
          (ok (search "Next top-level form begins at line 10" err))
          (ok (search "The file itself does not parse, so lisp-edit-form and lisp-patch-form" err))
          (ok (search "cannot locate any form in it." err))
          (ok (search "Run lisp-check-parens with path=" err))
          (ok (search "starting at line 3" err))
          (ok (search "write the whole file back with fs-write-file" err)
              "recovery path must be executable with cl-mcp tools alone")
          (ng (search "use lisp-edit-form (operation" err)
              "must not send the caller back into the tool that just failed")
          (ok (string= before (fs-read-file path)) "file untouched"))))))

(deftest file-unparseable-hook-denies-truncated-reads
  (testing "a valid file larger than the read cap is not classified as unparseable"
    (with-temp-file "tests/tmp/edit-form-large-valid.lisp"
        (format nil "(defun target ()~%  (list 1 2 3 4 5 6 7 8 9 10))~%")
      (lambda (path)
        (let ((cl-mcp/src/fs::*fs-read-max-bytes* 16))
          (ok (null (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                     (pathname path)))
              "a truncated read must keep the overwrite guard in place")))))
  (testing "editing such a file reports the read limit, not a paren diagnosis"
    (with-temp-file "tests/tmp/edit-form-large-valid-2.lisp"
        (format nil "(defun target ()~%  (list 1 2 3 4 5 6 7 8 9 10))~%")
      (lambda (path)
        (let ((cl-mcp/src/fs::*fs-read-max-bytes* 16)
              (err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "target"
                              :operation "replace"
                              :content "(defun target () 1)")
            (error (e) (setf err (princ-to-string e))))
          (ok (search "read limit" err)
              "message names the cause instead of guessing at parentheses")
          (ok (null (search "Likely fix" err))))))))

(deftest file-unparseable-hook-requires-delimiter-breakage
  (testing "custom syntax failing the default reader is not unparseable, even with odd parens"
    (with-temp-file "tests/tmp/edit-form-custom-syntax.lisp"
        (format nil "(defun f ()~%  #?[(])~%")
      (lambda (path)
        (ok (null (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                   (pathname path)))
            "the unknown #? macro may consume the ( as data, so the guard must hold"))))
  (testing "a missing ) and an extra ) are delimiter failures no readtable can fix"
    (with-temp-file "tests/tmp/edit-form-missing-close.lisp"
        (format nil "(defun a ()~%  (list 1)~%")
      (lambda (path)
        (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
             (pathname path)))))
    (with-temp-file "tests/tmp/edit-form-extra-close.lisp"
        (format nil "(defun a ()~%  (list 1)))~%")
      (lambda (path)
        (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
             (pathname path))))))
  (testing "the reader-level failure message points at the readtable parameter"
    (with-temp-file "tests/tmp/edit-form-custom-syntax-2.lisp"
        (format nil "(defun greet (name)~%  #?\"Hello ${name}\")~%")
      (lambda (path)
        (let ((err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "greet"
                              :operation "replace"
                              :content "(defun greet (name) name)")
            (file-unparseable-error (e)
              (setf err (princ-to-string e))))
          (ok err "without a readtable the file does not parse")
          (ok (search "readtable" err) "message mentions the readtable parameter")
          (ok (null (search "overwriting is allowed" err))
              "must not promise an overwrite that the guard will refuse"))))))

(deftest file-unparseable-message-for-open-block-comment
  (testing "a file ending inside #| gets closing guidance, not a reference to a likely fix"
    (with-temp-file "tests/tmp/edit-form-open-block-comment.lisp"
        (format nil "(defun target () 1)~%#| never closed~%")
      (lambda (path)
        (let ((err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "nonexistent"
                              :operation "replace"
                              :content "(defun nonexistent () 1)")
            (file-unparseable-error (e)
              (setf err (princ-to-string e))))
          (ok err "the open comment makes the file unparseable")
          (ok (search "Close it with |#" err))
          (ok (search "apply the change described above" err))
          (ok (null (search "Likely fix" err)) "no likely fix exists for a comment problem")
          (ok (search "allow_unparseable_overwrite=true" err)
              "an open comment is a delimiter failure, so the recovery path applies"))))))

(deftest file-unparseable-after-in-readtable-switch
  ;; After (in-readtable ...) the CST parser reads with the standard CL reader
  ;; and swallows read errors, returning the nodes it has. That path only
  ;; exists when named-readtables is loaded and the readtable resolves.
  (if (and (%try-load "named-readtables")
           (cl-mcp/src/cst::%try-switch-readtable :standard))
      (progn
        (testing "a form broken after an in-readtable switch still counts as unparseable"
          (with-temp-file "tests/tmp/edit-form-in-readtable-broken.lisp"
              (format nil "(in-readtable :standard)~%(defun a () 1)~%(defun b ()~%  (list 1)~%")
            (lambda (path)
              (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                   (pathname path))
                  "the swallowed read error must make the hook return T")
              (let ((err nil))
                (handler-case
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "b"
                                    :operation "replace"
                                    :content "(defun b () 2)")
                  (file-unparseable-error (e)
                    (setf err (princ-to-string e))))
                (ok err "editing the broken form signals file-unparseable-error, not not-found")
                (ok (search "write the whole file back with fs-write-file" err)
                    "recovery path is present")))))
        (testing "a stray ) after the switch is classified structurally, not by reader wording"
          (with-temp-file "tests/tmp/edit-form-in-readtable-stray.lisp"
              (format nil "(in-readtable :standard)~%(defun a () 1))~%(defun b () 2)~%")
            (lambda (path)
              (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                   (pathname path))
                  "the stray ) is a delimiter failure"))))
        (testing "a stray ) behind a line or block comment is still recognised"
          (with-temp-file "tests/tmp/edit-form-in-readtable-stray-comment.lisp"
              (format nil "(in-readtable :standard)~%(defun a () 1)~%;; note~%)~%(defun b () 2)~%")
            (lambda (path)
              (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                   (pathname path))
                  "line comment before the stray )")))
          (with-temp-file "tests/tmp/edit-form-in-readtable-stray-block.lisp"
              (format nil "(in-readtable :standard)~%(defun a () 1)~%#| x #| y |# |# )~%")
            (lambda (path)
              (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                   (pathname path))
                  "nested block comment before the stray )"))))
        (testing "an unterminated #| comment after the switch is a delimiter failure"
          (with-temp-file "tests/tmp/edit-form-in-readtable-open-block.lisp"
              (format nil "(in-readtable :standard)~%(defun a () 1)~%~
                           #| never closed~%(defun b () 2)~%")
            (lambda (path)
              (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                   (pathname path))
                  "EOF inside a block comment must not look like a clean end of file"))))
        (testing "a readtable that redefines ) is left to interpret it itself"
          ;; Bracket-list syntax: [ and ] read lists, ) is a plain constituent.
          (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                             :cl-mcp-test-bracket-lists :merge '(:standard))))
            (set-macro-character #\[ (get-macro-character #\( ) nil rt)
            (set-macro-character #\] (get-macro-character #\) ) nil rt)
            (set-syntax-from-char #\) #\a rt)
            (unwind-protect
                 (with-temp-file "tests/tmp/edit-form-bracket-readtable.lisp"
                     (format nil "(in-readtable :cl-mcp-test-bracket-lists)~%~
                                  [defun a [] 1]~%)foo~%")
                   (lambda (path)
                     (ok (null (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                                (pathname path)))
                         ")foo is a symbol under this readtable, not a stray paren")))
              (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                       :cl-mcp-test-bracket-lists))))
        (testing "a readtable that redefines ; keeps its own reading of it"
          ;; ; reads the following form (like quote), so ;(defun ...) is a defun.
          (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                             :cl-mcp-test-semicolon-reads :merge '(:standard))))
            (set-macro-character #\; (lambda (s c) (declare (ignore c)) (read s t nil t))
                                 nil rt)
            (unwind-protect
                 (with-temp-file "tests/tmp/edit-form-semicolon-readtable.lisp"
                     (format nil "(in-readtable :cl-mcp-test-semicolon-reads)~%~
                                  ;(defun target () 1)~%")
                   (lambda (path)
                     (ok (null (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                                (pathname path))))
                     (lisp-edit-form :file-path path
                                     :form-type "defun"
                                     :form-name "target"
                                     :operation "replace"
                                     :content "(defun target () 2)")
                     (ok (search "(defun target () 2)" (fs-read-file path))
                         "the form behind the redefined ; was located and replaced")))
              (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                       :cl-mcp-test-semicolon-reads))))
        (testing "a readtable that makes Newline a macro character keeps its forms"
          ;; Each newline reads as the keyword :nl, so it must reach READ.
          (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                             :cl-mcp-test-newline-macro :merge '(:standard))))
            (set-macro-character #\Newline (lambda (s c) (declare (ignore s c)) :nl) nil rt)
            (unwind-protect
                 (with-temp-file "tests/tmp/edit-form-newline-readtable.lisp"
                     ;; The newline ending the ; comment must reach the macro too.
                     (format nil "(in-readtable :cl-mcp-test-newline-macro)~%~
                                  (defun a () 1) ; trailing comment~%")
                   (lambda (path)
                     (let* ((nodes (cl-mcp/src/cst:parse-top-level-forms
                                    (fs-read-file path) :source-path (pathname path)))
                            (newline-forms (count :nl nodes
                                                  :key #'cl-mcp/src/cst:cst-node-value)))
                       ;; The newline right after the in-readtable form is
                       ;; consumed by Eclector's READ before the switch; the
                       ;; one after the defun reaches the CL-reader pass and
                       ;; must be handed to the macro (0 would mean discarded).
                       (ok (= newline-forms 1)
                           "the newline after the switch was read by the macro"))))
              (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                       :cl-mcp-test-newline-macro))))
        (testing "a stray ) reached through a zero-value Newline macro is still classified"
          ;; Newline reads as nothing, so READ itself runs into the ) and the
          ;; structural peek never sees it; the native error is normalised.
          (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                             :cl-mcp-test-newline-void :merge '(:standard))))
            (set-macro-character #\Newline (lambda (s c) (declare (ignore s c)) (values))
                                 nil rt)
            (unwind-protect
                 (with-temp-file "tests/tmp/edit-form-newline-void-stray.lisp"
                     (format nil "(in-readtable :cl-mcp-test-newline-void)~%(defun a () 1)~%)~%")
                   (lambda (path)
                     (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                          (pathname path))
                         "the stray ) behind the void macro counts as a delimiter failure")))
              (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                       :cl-mcp-test-newline-void))))
        (testing "a macro that consumes a balanced ) and then fails is not a stray paren"
          ;; #S(...) with an unknown structure reads its balanced list and then
          ;; signals; the stream stops right after ), which must not be
          ;; mistaken for an unmatched close.
          (with-temp-file "tests/tmp/edit-form-in-readtable-struct.lisp"
              (format nil "(in-readtable :standard)~%(defun a () 1)~%~
                           #S(cl-mcp-no-such-struct-xyz :a 1)~%")
            (lambda (path)
              (ok (null (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                         (pathname path)))
                  "a reader error after a balanced ) is not a delimiter failure"))))
        (testing "a stray ) behind a value-less macro that swallowed a ( is still classified"
          ;; #[ ... ] is a custom comment that returns no values; the ( it
          ;; contains must not make the following stray ) look balanced.
          (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                             :cl-mcp-test-bracket-comment :merge '(:standard))))
            (set-dispatch-macro-character
             #\# #\[
             (lambda (s c n)
               (declare (ignore c n))
               (loop for ch = (read-char s nil nil)
                     until (or (null ch) (char= ch #\])))
               (values))
             rt)
            (unwind-protect
                 (with-temp-file "tests/tmp/edit-form-bracket-comment-stray.lisp"
                     (format nil "(in-readtable :cl-mcp-test-bracket-comment)~%~
                                  (defun a () 1)~%#[ignored (]~%)~%")
                   (lambda (path)
                     (ok (cl-mcp/src/lisp-edit-form-core::%file-unparseable-by-edit-tools-p
                          (pathname path))
                         "the readtable itself reads nothing before the ), so it is stray")))
              (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                       :cl-mcp-test-bracket-comment))))
        (testing "a form before the breakage can still be edited"
          (with-temp-file "tests/tmp/edit-form-in-readtable-broken-2.lisp"
              (format nil "(in-readtable :standard)~%(defun a () 1)~%(defun b ()~%  (list 1)~%")
            (lambda (path)
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "a"
                              :operation "replace"
                              :content "(defun a () 2)")
              (ok (search "(defun a () 2)" (fs-read-file path)))))))
      (skip "named-readtables not available; in-readtable switch path not exercised")))

(deftest lisp-edit-form-old-protocol-error-returns-rpc-error
  (testing "old protocol errors return -32603 rpc-error, not isError"
    (with-temp-file "tests/tmp/edit-old-proto.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        ;; Test with nil protocol version (no initialize handshake)
        (let ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "nonexistent"
                      "operation" "replace"
                      "content" "(defun nonexistent () nil)")))
          ;; nil protocol → should get rpc error -32603
          (let* ((response (funcall handler state "test-nil-proto" args))
                 (err (gethash "error" response)))
            (ok err "nil protocol should produce rpc error")
            (ok (eql -32603 (gethash "code" err))
                "error code should be -32603 for nil protocol")))
        ;; Test with old protocol version
        (let* ((state (cl-mcp/src/state:make-state))
               (_ (setf (cl-mcp/src/state:protocol-version state) "2024-11-05"))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "nonexistent"
                      "operation" "replace"
                      "content" "(defun nonexistent () nil)")))
          (declare (ignore _))
          ;; old protocol → should get rpc error -32603
          (let* ((response (funcall handler state "test-old-proto" args))
                 (err (gethash "error" response)))
            (ok err "old protocol should produce rpc error")
            (ok (eql -32603 (gethash "code" err))
                "error code should be -32603 for old protocol")))))))

(deftest lisp-edit-form-dry-run-carries-preview-form
  (testing "dry-run result exposes the edited form separately from the whole file"
    (with-temp-file "tests/tmp/edit-form-dry-run-preview-form.lisp"
        "(defun keep () :keep)

(defun target () :old)
"
      (lambda (path)
        (let ((before (fs-read-file path))
              (replaced (lisp-edit-form :file-path path
                                        :form-type "defun"
                                        :form-name "target"
                                        :operation "replace"
                                        :content "(defun target () :new)"
                                        :dry-run t))
              (deleted (lisp-edit-form :file-path path
                                       :form-type "defun"
                                       :form-name "target"
                                       :operation "delete"
                                       :dry-run t))
              (inserted (lisp-edit-form :file-path path
                                        :form-type "defun"
                                        :form-name "target"
                                        :operation "insert_after"
                                        :content "(defun added () :added)"
                                        :dry-run t)))
          (ok (string= "(defun target () :new)" (gethash "preview_form" replaced))
              "replace preview_form is the new form only")
          (ok (string= "(form removed)" (gethash "preview_form" deleted))
              "delete preview_form is a marker, not the file")
          (ok (string= "(defun added () :added)" (gethash "preview_form" inserted))
              "insert_after preview_form is the inserted form only")
          (ok (search "(defun keep" (gethash "preview" replaced))
              "preview still holds the whole updated file (replace)")
          (ok (search "(defun keep" (gethash "preview" deleted))
              "preview still holds the whole updated file (delete)")
          (ok (string= before (fs-read-file path))
              "dry-run writes nothing to disk"))))))

(deftest lisp-edit-form-dry-run-summary-excludes-whole-file
  (testing "dry-run replace summary shows the edited form, not the updated file"
    (with-temp-file "tests/tmp/edit-form-dry-run-large.lisp"
        (large-file-source 500)
      (lambda (path)
        (let* ((before (fs-read-file path))
               (state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "operation" "replace"
                      "content" "(defun target () :new)"
                      "dry_run" t))
               (response (funcall handler state "dry-run-large" args))
               (result-obj (gethash "result" response))
               (text (gethash "text" (aref (gethash "content" result-obj) 0))))
          (ok (> (length before) 20000)
              "fixture file is large enough to make inlining obvious")
          (ok (search ":new" text)
              "summary shows the edited form")
          (ok (search "(defun target () :old)" text)
              "summary still shows the original form")
          (ok (null (search "filler-499" text))
              "summary does not inline the rest of the file")
          (ok (< (length text) 2000)
              "summary stays bounded regardless of file size")
          (ok (search "filler-499" (gethash "preview" result-obj))
              "sibling preview field still carries the whole updated file")
          (ok (string= "(defun target () :new)" (gethash "preview_form" result-obj))
              "sibling preview_form field carries the edited form")
          (ok (string= before (fs-read-file path))
              "dry-run writes nothing to disk"))))))

(deftest lisp-edit-form-dry-run-delete-summary-excludes-whole-file
  (testing "dry-run delete summary reports the removal without inlining the file"
    (with-temp-file "tests/tmp/edit-form-dry-run-delete-large.lisp"
        (large-file-source 500)
      (lambda (path)
        (let* ((before (fs-read-file path))
               (state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               ;; No content argument: delete must work without one.
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "operation" "delete"
                      "dry_run" t))
               (response (funcall handler state "dry-run-delete-large" args))
               (result-obj (gethash "result" response))
               (text (gethash "text" (aref (gethash "content" result-obj) 0))))
          (ng (gethash "error" response)
              "delete dry-run without content is not an error")
          (ok (search "Dry-run delete" text)
              "summary names the delete operation")
          (ok (search "(form removed)" text)
              "summary marks the form as removed")
          (ok (search "(defun target () :old)" text)
              "summary still shows the form being deleted")
          (ok (null (search "filler-499" text))
              "summary does not inline the rest of the file")
          (ok (< (length text) 2000)
              "summary stays bounded regardless of file size")
          (ok (search "filler-499" (gethash "preview" result-obj))
              "sibling preview field still carries the whole updated file")
          (ok (null (search "(defun target" (gethash "preview" result-obj)))
              "sibling preview field reflects the deletion")
          (ok (string= before (fs-read-file path))
              "dry-run writes nothing to disk"))))))

(deftest lisp-edit-form-warning-distinguishes-added-and-dropped
  (testing "extra closing parens are reported as dropped, never as a negative count"
    (with-temp-file "tests/tmp/edit-form-dropped.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning changed-p fixes)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target (x)~%  (let ((y 1))~%    (+ x y))))"))
          (declare (ignore updated changed-p))
          (ok (search "1 extra closing delimiter dropped by parinfer" warning))
          (ng (search "-1" warning))
          (ok (= (length fixes) 1))
          (ok (= (getf (first fixes) :line) 3))
          (ok (= (getf (first fixes) :delta) -1)))))))

(deftest lisp-edit-form-warning-counts-gross-edits
  (testing "a relocated ) reports one added and one dropped, not a net zero"
    (with-temp-file "tests/tmp/edit-form-relocated.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content ")(defun target () 1")
          (declare (ignore updated))
          (ok (search "1 closing delimiter added by parinfer" warning))
          (ok (search "1 extra closing delimiter dropped by parinfer" warning))
          (ng (search "content repaired by parinfer" warning)))))))

(deftest lisp-edit-form-repair-ignores-parens-in-multiple-escape
  (testing "a ( inside a |...| symbol is not counted, so the repair adds exactly one )"
    (with-temp-file "tests/tmp/edit-form-multiple-escape.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target ()~%  (list '|a(b| 1)"))
          (declare (ignore updated))
          (ok (search "1 closing delimiter added by parinfer" warning))
          (ok (search "(list '|a(b| 1))" (fs-read-file path))
              "one ) was added after the form, the symbol untouched"))))))

(deftest lisp-edit-form-warning-added-wording
  (testing "missing closing parens are reported as added"
    (with-temp-file "tests/tmp/edit-form-added.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning changed-p fixes)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))"))
          (declare (ignore updated changed-p))
          (ok (search "1 closing delimiter added by parinfer" warning))
          (ok (= (getf (first fixes) :line) 2))
          (ok (search "(let ((y 1))" (fs-read-file path))
              "the binding list was closed on line 2, not at the end"))))))

(deftest lisp-edit-form-refuses-stray-bracket
  (testing "content with ] where ) was meant is rejected and nothing is written"
    (with-temp-file "tests/tmp/edit-form-stray-bracket.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "target"
                              :operation "replace"
                              :content (format nil "(defun target (x)~%  (let ((y 1]~%    (+ x y)))"))
            (cl-mcp/src/lisp-edit-form::content-unrepairable-error (e)
              (setf err (princ-to-string e))))
          (ok err "should signal content-unrepairable-error")
          (ok (search "Unbalanced parentheses in content: expected \")\" but found \"]\" at line 2, column 13." err))
          (ok (search "Replace it with \")\"." err))
          (ok (string= before (fs-read-file path)) "file untouched"))))))

(deftest lisp-edit-form-repairs-content-with-bracket-symbols
  (testing "an unmatched [ that may be a symbol character does not block the repair"
    (with-temp-file "tests/tmp/edit-form-bracket-symbol-repair.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target (x)~%  (list a[b x)"))
          (declare (ignore updated))
          (ok (search "closing delimiter" warning)
              "the missing ) is added by parinfer as on main")
          (ok (search "(list a[b x))" (fs-read-file path))
              "a[b is left alone and the form is closed")))))
  (testing "an unmatched [ opener with no other breakage is not refused either"
    (with-temp-file "tests/tmp/edit-form-bracket-opener-repair.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "replace"
                        :content (format nil "(defun target (x)~%  (foo [bar x"))
        (ok (search "(foo [bar x))" (fs-read-file path)))))))

(deftest lisp-edit-form-repairs-content-ending-in-a-comment
  (testing "a missing ) on a line with a trailing comment is repaired before the comment"
    (with-temp-file "tests/tmp/edit-form-trailing-comment-repair.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target (x)~%  (list x) ; done"))
          (declare (ignore updated))
          (ok (search "closing delimiter" warning))
          (ok (search "(list x)) ; done" (fs-read-file path))
              "the ) is inserted before the comment, so the form reads"))))))

(deftest lisp-edit-form-summary-flags-a-relocating-repair
  (testing "a closer inserted before the last code line is called out in the summary"
    ;; Indentation puts (g x) and (h x) outside the when; parinfer closes the
    ;; when on line 2, which the changed-lines list alone would not make plain.
    (let ((summary (cl-mcp/src/lisp-edit-form::%repair-summary
                    "1 closing delimiter added by parinfer"
                    '((:line 2 :original "  (when x" :repaired "  (when x)"
                       :delta 1 :added 1 :removed 0))
                    (format nil "(defun t1 (x)~%  (when x)~%  (g x)~%  (h x))"))))
      (ok (search "NOTE: parinfer closed a form on line 2" summary))
      (ok (search "no longer inside that form" summary))))
  (testing "an append on the last line is not a relocation"
    (let ((summary (cl-mcp/src/lisp-edit-form::%repair-summary
                    "1 closing delimiter added by parinfer"
                    '((:line 2 :original "  (list 1)" :repaired "  (list 1))"
                       :delta 1 :added 1 :removed 0))
                    (format nil "(defun t1 ()~%  (list 1))~%"))))
      (ng (search "NOTE:" summary)))))

(deftest lisp-edit-form-refusal-hides-reader-internals
  (testing "the reader error kept in a refusal carries no SBCL stream object"
    (with-temp-file "tests/tmp/edit-form-refusal-sanitized.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((err nil))
          (handler-case
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "target"
                              :operation "replace"
                              :content (format nil "(defun target (x)~%  (let ((y 1]~%~
                                                    (+ x y)))"))
            (cl-mcp/src/lisp-edit-form::content-unrepairable-error (e)
              (setf err (princ-to-string e))))
          (ok err "should signal content-unrepairable-error")
          (ok (search "(reader: " err) "the reader's own error is still appended")
          (ok (null (search "#<" err)) "no #<...> object representation leaks")
          (ok (search "Replace it with \")\"." err)
              "the multi-line diagnosis itself is untouched"))))))

(deftest lisp-edit-form-content-honours-in-readtable-in-file
  (testing "content is validated under the readtable the file switched to"
    ;; #?[...] reads raw text through ]; without the file's readtable the
    ;; content would fail on #? and the scan would then blame the ].
    (if (%try-load "named-readtables")
        (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                           :cl-mcp-test-file-raw-bracket :merge '(:standard))))
          (set-dispatch-macro-character
           #\# #\?
           (lambda (s c n)
             (declare (ignore c n))
             (read-char s)
             (coerce (loop for ch = (read-char s nil nil)
                           until (or (null ch) (char= ch #\]))
                           collect ch)
                     'string))
           rt)
          (unwind-protect
               (with-temp-file "tests/tmp/edit-form-in-readtable-content.lisp"
                   (format nil "(named-readtables:in-readtable ~
                                :cl-mcp-test-file-raw-bracket)~%(defun b () 1)~%")
                 (lambda (path)
                   (lisp-edit-form :file-path path
                                   :form-type "defun"
                                   :form-name "b"
                                   :operation "replace"
                                   :content "(defun b () #?[(])")
                   (ok (search "(defun b () #?[(])" (fs-read-file path))
                       "the content was accepted as written under the file's readtable")))
            (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                     :cl-mcp-test-file-raw-bracket)))
        (skip "named-readtables not available"))))

(deftest lisp-edit-form-custom-readtable-skips-delimiter-verdicts
  (testing "under a readtable that changes the syntax, the scan neither refuses nor explains"
    ;; Under a custom readtable ] may be meaningful, so the ] refusal that
    ;; applies to plain content is not applied; the reader decides.
    (if (%try-load "named-readtables")
        (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                           :cl-mcp-test-bracket-syntax :merge '(:standard))))
          (set-dispatch-macro-character
           #\# #\?
           (lambda (s c n) (declare (ignore c n)) (read-line s nil ""))
           rt)
          (unwind-protect
               (with-temp-file "tests/tmp/edit-form-custom-readtable-bracket.lisp"
                   (format nil "(defun target () :old)~%")
                 (lambda (path)
                   (multiple-value-bind (updated warning)
                       (lisp-edit-form :file-path path
                                       :form-type "defun"
                                       :form-name "target"
                                       :operation "replace"
                                       :readtable :cl-mcp-test-bracket-syntax
                                       :content (format nil "(defun target ()~%  foo]"))
                     (declare (ignore updated))
                     (ok (search "closing delimiter" warning)
                         "parinfer still closes the form under the custom readtable")
                     (ok (search "foo]" (fs-read-file path))
                         "foo] was accepted as the readtable's business"))))
            (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                     :cl-mcp-test-bracket-syntax)))
        (skip "named-readtables not available")))
  (testing "readtable :standard is not a loophole: foo] is still refused"
    (if (%try-load "named-readtables")
        (with-temp-file "tests/tmp/edit-form-standard-readtable-bracket.lisp"
            (format nil "(defun target () :old)~%")
          (lambda (path)
            (let ((before (fs-read-file path))
                  (err nil))
              (handler-case
                  (lisp-edit-form :file-path path
                                  :form-type "defun"
                                  :form-name "target"
                                  :operation "replace"
                                  :readtable :standard
                                  :content (format nil "(defun target ()~%  foo]"))
                (cl-mcp/src/lisp-edit-form::content-unrepairable-error (e)
                  (setf err (princ-to-string e))))
              (ok err "content-unrepairable-error under :standard")
              (ok (search "Replace it with \")\"." err))
              (ok (string= before (fs-read-file path)) "file untouched"))))
        (skip "named-readtables not available"))))

(deftest lisp-edit-form-readtable-file-failure-gives-no-scan-verdict
  (testing "a file that fails under the caller's readtable gets no standard-syntax advice"
    ;; #?[...] reads raw text through ]; the ( inside is data. Line 3 is
    ;; genuinely missing a ), but the standard scanner would blame the ].
    (if (%try-load "named-readtables")
        (let ((rt (funcall (find-symbol "MAKE-READTABLE" "NAMED-READTABLES")
                           :cl-mcp-test-raw-bracket :merge '(:standard))))
          (set-dispatch-macro-character
           #\# #\?
           (lambda (s c n)
             (declare (ignore c n))
             (read-char s)
             (coerce (loop for ch = (read-char s nil nil)
                           until (or (null ch) (char= ch #\]))
                           collect ch)
                     'string))
           rt)
          (unwind-protect
               (with-temp-file "tests/tmp/edit-form-readtable-file-failure.lisp"
                   (format nil "(defun a () #?[(])~%(defun b ()~%  (list 1)~%")
                 (lambda (path)
                   (let ((err nil))
                     (handler-case
                         (lisp-edit-form :file-path path
                                         :form-type "defun"
                                         :form-name "b"
                                         :operation "replace"
                                         :readtable :cl-mcp-test-raw-bracket
                                         :content "(defun b () 2)")
                       (file-unparseable-error (e)
                         (setf err (princ-to-string e))))
                     (ok err "the missing ) on line 3 still makes the file unparseable")
                     (ok (null (search "Replace it with" err))
                         "no standard-syntax bracket advice under a custom readtable")
                     (ok (null (search "Likely fix" err)))
                     (ok (search "cl-mcp-test-raw-bracket" err)
                         "the message names the readtable that was tried")
                     (ok (search "unexpected end of input" err)
                         "the reader's end-of-file is worded, not a dangling clause"))))
            (funcall (find-symbol "UNREGISTER-READTABLE" "NAMED-READTABLES")
                     :cl-mcp-test-raw-bracket)))
        (skip "named-readtables not available"))))

(deftest lisp-edit-form-accepts-balanced-braces-with-missing-paren
  (testing "content using {...} reader-macro syntax is still auto-repaired"
    (with-temp-file "tests/tmp/edit-form-braces.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (multiple-value-bind (updated warning)
            (lisp-edit-form :file-path path
                            :form-type "defun"
                            :form-name "target"
                            :operation "replace"
                            :content (format nil "(defun target (x)~%  (foo {a b}~%  (bar x))"))
          (declare (ignore updated))
          (ok (search "1 closing delimiter added by parinfer" warning))
          (ok (search "(foo {a b})" (fs-read-file path))
              "the paren was added after the braces, and the braces were kept"))))))

(deftest lisp-edit-form-dry-run-carries-repair-fixes
  (testing "dry-run hash exposes the repair line diff"
    (with-temp-file "tests/tmp/edit-form-dry-run-fixes.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((res (lisp-edit-form :file-path path
                                   :form-type "defun"
                                   :form-name "target"
                                   :operation "replace"
                                   :dry-run t
                                   :content (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))"))))
          (ok (stringp (gethash "parinfer_warning" res)))
          (ok (= (length (gethash "repair_fixes" res)) 1)))))))

(deftest lisp-edit-form-summary-shows-repaired-form
  (testing "non-dry-run summary shows what parinfer actually wrote"
    (with-temp-file "tests/tmp/edit-form-summary-repaired.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "operation" "replace"
                      "content" (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))")))
               (response (funcall handler state "repaired-1" args))
               (result-obj (gethash "result" response))
               (text (gethash "text" (aref (gethash "content" result-obj) 0))))
          (ok (search "Applied replace to defun target" text))
          (ok (search "WARNING: 1 closing delimiter added by parinfer" text))
          (ok (search "Changed lines:" text))
          (ok (search "line 2: \"  (let ((y 1)\"  ->  add 1 \")\"" text))
          (ok (search "--- repaired form ---" text))
          (ok (search "(let ((y 1))" text)))))))

(deftest lisp-edit-form-dry-run-summary-shows-changed-lines
  (testing "dry-run summary lists the changed lines but not a second copy of the form"
    (with-temp-file "tests/tmp/edit-form-dry-run-changed-lines.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let* ((state (cl-mcp/src/state:make-state))
               (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
               (args (cl-mcp/src/tools/helpers:make-ht
                      "file_path" path
                      "form_type" "defun"
                      "form_name" "target"
                      "operation" "replace"
                      "dry_run" t
                      "content" (format nil "(defun target (x)~%  (let ((y 1)~%    (+ x y)))")))
               (response (funcall handler state "repaired-dry" args))
               (result-obj (gethash "result" response))
               (text (gethash "text" (aref (gethash "content" result-obj) 0))))
          (ok (search "Changed lines:" text))
          (ok (search "--- preview ---" text))
          (ng (search "--- repaired form ---" text)))))))

(deftest lisp-edit-form-handler-stray-bracket-is-tool-error
  (testing "unrepairable content is an isError result on the new protocol"
    (with-temp-file "tests/tmp/edit-form-handler-stray.lisp"
        (format nil "(defun target () :old)~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "target"
                     "operation" "replace"
                     "content" (format nil "(defun target (x)~%  (let ((y 1]~%    (+ x y)))"))))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "stray-1" args))
                 (result-obj (gethash "result" response))
                 (text (gethash "text" (aref (gethash "content" result-obj) 0))))
            (ng (gethash "error" response))
            (ok (gethash "isError" result-obj))
            (ok (search "found \"]\"" text))))))))

(deftest lisp-edit-form-handler-broken-file-is-tool-error
  (testing "a file that does not parse yields guidance as an isError result"
    (with-temp-file "tests/tmp/edit-form-handler-broken.lisp"
        (format nil "(defun a ()~%  (list 1)~%~%(defun b ()~%  2)~%")
      (lambda (path)
        (let ((state (cl-mcp/src/state:make-state))
              (handler #'cl-mcp/src/lisp-edit-form::lisp-edit-form-handler)
              (args (cl-mcp/src/tools/helpers:make-ht
                     "file_path" path
                     "form_type" "defun"
                     "form_name" "b"
                     "operation" "replace"
                     "content" "(defun b () 3)")))
          (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
          (let* ((response (funcall handler state "broken-1" args))
                 (result-obj (gethash "result" response))
                 (text (gethash "text" (aref (gethash "content" result-obj) 0))))
            (ng (gethash "error" response))
            (ok (gethash "isError" result-obj))
            (ok (search "Run lisp-check-parens with path=" text))
            (ok (search "Next top-level form begins at line 4" text))))))))
