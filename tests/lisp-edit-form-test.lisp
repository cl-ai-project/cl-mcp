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
                          (cl-mcp/src/lisp-edit-form::%normalize-string sym)))
             ;; Also verify non-symbol input still works
             (ok (string= "hello"
                          (cl-mcp/src/lisp-edit-form::%normalize-string "HELLO"))))
        (delete-package pkg-name)))))

(deftest validate-content-with-unknown-package
  (testing "%validate-and-repair-content handles unknown package-qualified symbols"
    (let ((content "(defun process (x) (unknown-val-pkg:transform x))"))
      (ok (stringp
           (cl-mcp/src/lisp-edit-form::%validate-and-repair-content content))))))

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

;;; ============================================================
;;; Edit operation tests
;;; ============================================================

(deftest lisp-edit-form-edit-basic
  (testing "edit replaces a sub-expression within a defun"
    (with-temp-file "tests/tmp/edit-op-basic.lisp"
        (format nil "(defun compute (x)~%  (+ x 1))~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "compute"
                        :operation "edit"
                        :old-text "(+ x 1)"
                        :new-text "(* x 2)")
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 2)" updated))
          (ok (null (search "(+ x 1)" updated))))))))

(deftest lisp-edit-form-edit-preserves-surrounding
  (testing "edit only modifies target form, rest of file unchanged"
    (with-temp-file "tests/tmp/edit-op-preserve.lisp"
        (format nil "(defun before () :keep)~%~%(defun target (x)~%  (+ x 1))~%~%(defun after () :keep)~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "edit"
                        :old-text "(+ x 1)"
                        :new-text "(- x 1)")
        (let ((updated (fs-read-file path)))
          (ok (search "(- x 1)" updated))
          (ok (search "(defun before () :keep)" updated))
          (ok (search "(defun after () :keep)" updated)))))))

(deftest lisp-edit-form-edit-multiline
  (testing "edit replaces a multi-line block within a form"
    (with-temp-file "tests/tmp/edit-op-multiline.lisp"
        (format nil "(defun process (data)~%  (when data~%    (print data)~%    (+ 1 2)))~%")
      (lambda (path)
        (let ((old-block (format nil "(when data~%    (print data)~%    (+ 1 2))"))
              (new-block (format nil "(when data~%    (log-info data)~%    (+ 1 2))")))
          (lisp-edit-form :file-path path
                          :form-type "defun"
                          :form-name "process"
                          :operation "edit"
                          :old-text old-block
                          :new-text new-block)
          (let ((updated (fs-read-file path)))
            (ok (search "(log-info data)" updated))
            (ok (null (search "(print data)" updated)))))))))

(deftest lisp-edit-form-edit-not-found-error
  (testing "edit signals error when old_text is not in the form"
    (with-temp-file "tests/tmp/edit-op-not-found.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "target"
                                    :operation "edit"
                                    :old-text "nonexistent text"
                                    :new-text "replacement")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "old_text not found" err-msg))
          (ok (search "whitespace-sensitive" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-multiple-matches-error
  (testing "edit signals error when old_text matches multiple times"
    (with-temp-file "tests/tmp/edit-op-multi-match.lisp"
        (format nil "(defun target (x)~%  (+ (abs x) (abs x) (abs x)))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "target"
                                    :operation "edit"
                                    :old-text "(abs x)"
                                    :new-text "(abs y)")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "3 times" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-content-rejected
  (testing "edit rejects content parameter"
    (with-temp-file "tests/tmp/edit-op-content-rejected.lisp"
        (format nil "(defun target () :ok)~%")
      (lambda (path)
        (let ((before (fs-read-file path)))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "target"
                                    :operation "edit"
                                    :old-text ":ok"
                                    :new-text ":new"
                                    :content "(defun target () :new)")
                    nil)
                (error (e)
                  (search "content must not be provided" (princ-to-string e)))))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-missing-params
  (testing "edit without old_text signals error"
    (with-temp-file "tests/tmp/edit-op-missing-params.lisp"
        (format nil "(defun target () :ok)~%")
      (lambda (path)
        (let ((before (fs-read-file path)))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "target"
                                    :operation "edit"
                                    :new-text "replacement")
                    nil)
                (error (e)
                  (search "old_text and new_text are required" (princ-to-string e)))))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-dry-run
  (testing "edit dry-run returns preview without modifying file"
    (with-temp-file "tests/tmp/edit-op-dry-run.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (result (lisp-edit-form :file-path path
                                      :form-type "defun"
                                      :form-name "target"
                                      :operation "edit"
                                      :old-text "(+ x 1)"
                                      :new-text "(* x 2)"
                                      :dry-run t)))
          (ok (hash-table-p result))
          (ok (gethash "would_change" result))
          (ok (search "(+ x 1)" (gethash "original" result)))
          (ok (search "(* x 2)" (gethash "preview" result)))
          (ok (string= "edit" (gethash "operation" result)))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-breaks-structure
  (testing "edit that breaks form structure signals error, no changes written"
    (with-temp-file "tests/tmp/edit-op-breaks-structure.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    ;; Remove a closing paren, breaking the form
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "target"
                                    :operation "edit"
                                    :old-text "(+ x 1))"
                                    :new-text "(+ x 1)")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "invalid Lisp" err-msg))
          (ok (search "No changes were written" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-defmethod
  (testing "edit works with defmethod form matching"
    (let ((initial (concatenate 'string
                    "(defmethod render ((w widget) stream)" (string #\Newline)
                    "  (format stream \"<~A>\" (name w)))" (string #\Newline))))
      (with-temp-file "tests/tmp/edit-op-defmethod.lisp"
          initial
        (lambda (path)
          (lisp-edit-form :file-path path
                          :form-type "defmethod"
                          :form-name "render ((w widget) stream)"
                          :operation "edit"
                          :old-text "(name w)"
                          :new-text "(widget-name w)")
          (let ((updated (fs-read-file path)))
            (ok (search "(widget-name w)" updated))
            (ok (null (search "(name w)" updated)))))))))

(deftest lisp-edit-form-edit-changes-form-name
  (testing "edit can rename the function (matching happens before edit)"
    (with-temp-file "tests/tmp/edit-op-rename.lisp"
        (format nil "(defun my-func (x)~%  (+ x 1))~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "my-func"
                        :operation "edit"
                        :old-text "my-func"
                        :new-text "my-func-v2")
        (let ((updated (fs-read-file path)))
          (ok (search "my-func-v2" updated))
          ;; The original name should only appear as part of the new name
          (ok (null (search "(defun my-func " updated))))))))

(deftest lisp-edit-form-edit-no-op
  (testing "edit with old_text == new_text reports would_change as false"
    (with-temp-file "tests/tmp/edit-op-no-op.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (result (lisp-edit-form :file-path path
                                      :form-type "defun"
                                      :form-name "target"
                                      :operation "edit"
                                      :old-text "(+ x 1)"
                                      :new-text "(+ x 1)"
                                      :dry-run t)))
          (ok (hash-table-p result))
          (ok (not (gethash "would_change" result)))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-old-text-in-string-literal
  (testing "edit matches old_text in code when it also appears in a string"
    (with-temp-file "tests/tmp/edit-op-string-literal.lisp"
        (format nil "(defun target ()~%  (error \"call (+ x 1) here\")~%  (+ x 1))~%")
      (lambda (path)
        ;; "(+ x 1)" appears twice: in the string and in code
        ;; Provide enough context to uniquely match the code occurrence
        (lisp-edit-form :file-path path
                        :form-type "defun"
                        :form-name "target"
                        :operation "edit"
                        :old-text (format nil "  (+ x 1))")
                        :new-text (format nil "  (* x 2))"))
        (let ((updated (fs-read-file path)))
          (ok (search "(* x 2)" updated))
          ;; String literal should be preserved
          (ok (search "\"call (+ x 1) here\"" updated)))))))

(deftest lisp-edit-form-edit-unrepairable-structure
  (testing "edit that completely destroys form structure gives clear error"
    (with-temp-file "tests/tmp/edit-op-unrepairable.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "target"
                                    :operation "edit"
                                    :old-text "(defun target (x)"
                                    :new-text "completely broken ((( stuff")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "invalid Lisp" err-msg))
          (ok (string= before (fs-read-file path))))))))

(deftest lisp-edit-form-edit-rpc-error-code
  (testing "handler uses arg-validation-error for missing edit params"
    ;; arg-validation-error is caught by define-tool and mapped to -32602
    ;; We test the CL-level validation which uses plain error
    (with-temp-file "tests/tmp/edit-op-rpc-error.lisp"
        (format nil "(defun target () :ok)~%")
      (lambda (path)
        ;; Missing old-text should signal an error
        (ok (handler-case
                (progn
                  (lisp-edit-form :file-path path
                                  :form-type "defun"
                                  :form-name "target"
                                  :operation "edit"
                                  :old-text nil
                                  :new-text "replacement")
                  nil)
              (error (e)
                (search "old_text and new_text are required" (princ-to-string e)))))
        ;; Missing new-text should signal an error
        (ok (handler-case
                (progn
                  (lisp-edit-form :file-path path
                                  :form-type "defun"
                                  :form-name "target"
                                  :operation "edit"
                                  :old-text "some text"
                                  :new-text nil)
                  nil)
              (error (e)
                (search "old_text and new_text are required" (princ-to-string e)))))
        ;; Content with edit should signal an error
        (ok (handler-case
                (progn
                  (lisp-edit-form :file-path path
                                  :form-type "defun"
                                  :form-name "target"
                                  :operation "edit"
                                  :old-text ":ok"
                                  :new-text ":new"
                                  :content "(defun target () :new)")
                  nil)
              (error (e)
                (search "content must not be provided" (princ-to-string e)))))))))

(deftest lisp-edit-form-edit-empty-old-text-error
  (testing "edit with empty old_text signals error immediately"
    (with-temp-file "tests/tmp/edit-op-empty-old-text.lisp"
        (format nil "(defun target (x)~%  (+ x 1))~%")
      (lambda (path)
        (let ((before (fs-read-file path))
              (err-msg nil))
          (ok (handler-case
                  (progn
                    (lisp-edit-form :file-path path
                                    :form-type "defun"
                                    :form-name "target"
                                    :operation "edit"
                                    :old-text ""
                                    :new-text "replacement")
                    nil)
                (error (e)
                  (setf err-msg (princ-to-string e))
                  t)))
          (ok (search "old_text must not be empty" err-msg))
          (ok (string= before (fs-read-file path)))))))
)

(deftest lisp-edit-form-edit-auto-detected-readtable
  (testing "edit validates modified form using auto-detected readtable from in-readtable"
    (handler-case
        (progn
          (unless (%try-load :cl-interpol) (error "not available"))
          (with-temp-file "tests/tmp/edit-op-auto-readtable.lisp"
              (format nil
                      "(in-package :cl-user)~%(named-readtables:in-readtable :interpol-syntax)~%~%(defun greet (name)~%  #?\"Hello, ${name}!\")~%")
            (lambda (path)
              ;; Edit without explicit readtable parameter - should auto-detect
              (lisp-edit-form :file-path path
                              :form-type "defun"
                              :form-name "greet"
                              :operation "edit"
                              :old-text "Hello"
                              :new-text "Hi")
              (let ((updated (fs-read-file path)))
                (ok (search "#?\"Hi, ${name}!\"" updated))
                (ok (null (search "Hello" updated)))))))
      (error ()
        (skip "cl-interpol not available")))))
