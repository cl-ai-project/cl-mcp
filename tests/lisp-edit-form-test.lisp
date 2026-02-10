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

(deftest lisp-edit-form-with-custom-readtable
  (testing "readtable parameter enables editing files with custom reader macros"
    (handler-case
        (progn
          (ql:quickload :cl-interpol :silent t)
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
          (ql:quickload :cl-interpol :silent t)
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
          (ql:quickload :named-readtables :silent t)
          (ql:quickload :cl-interpol :silent t)
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
