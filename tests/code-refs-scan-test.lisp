;;;; tests/code-refs-scan-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/code-refs-scan: finding where a symbol name is
;;;; written, classifying the position, and describing the enclosing form.

(defpackage #:cl-mcp/tests/code-refs-scan-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:target-name-from-designator
                #:scan-text
                #:scan-project))

(in-package #:cl-mcp/tests/code-refs-scan-test)

(defun %sites (text name)
  "Return (kind line shadowed-by token) for every site of NAME in TEXT."
  (loop for form in (scan-text text name)
        append (loop for site across (gethash "sites" form)
                     collect (list (gethash "kind" site)
                                   (gethash "line" site)
                                   (gethash "shadowed_by" site)
                                   (gethash "token" site)))))

(defun %kinds (text name)
  "Return the kind of every site of NAME in TEXT, in source order."
  (mapcar #'first (%sites text name)))

(deftest scan-text-classifies-positions
  (testing "call, function, quoted, reference, bind and set"
    (ok (equal '("call") (%kinds "(defun a () (foo 1))" "FOO")))
    (ok (equal '("function") (%kinds "(defun a () (mapcar #'foo xs))" "FOO")))
    (ok (equal '("function") (%kinds "(defun a () (mapcar (function foo) xs))" "FOO")))
    (ok (equal '("quoted") (%kinds "(defun a () (funcall 'foo))" "FOO")))
    (ok (equal '("quoted" "quoted") (%kinds "(defun a () '(foo (foo)))" "FOO")))
    (ok (equal '("reference") (%kinds "(defun a () (list foo))" "FOO")))
    (ok (equal '("bind" "reference") (%kinds "(defun a () (let ((foo 1)) foo))" "FOO")))
    (ok (equal '("bind") (%kinds "(defun a (foo) nil)" "FOO")))
    (ok (equal '("set" "set") (%kinds "(defun a () (setf foo 1) (setq foo 2))" "FOO")))
    (ok (equal '("call") (%kinds "(defun a () (setf (foo x) 1))" "FOO")))
    (ok (equal '("call") (%kinds "(defun a () (funcall #'(lambda (x) (foo x)) 1))" "FOO")))
    (ok (equal '("reference") (%kinds "(defclass a (foo) ())" "FOO")))))

(deftest scan-text-labels-backquote-templates
  (testing "a template is labelled, an unquoted island is ordinary code"
    (ok (equal '("template" "call") (%kinds "(defmacro m (x) `(foo ,(foo x)))" "FOO")))))

(deftest scan-text-excludes-definitions-and-packages
  (testing "definition names, package forms, keywords, strings and comments"
    (ok (null (%kinds "(defun foo () 1)" "FOO")))
    (ok (null (%kinds "(defvar foo 1)" "FOO")))
    (ok (null (%kinds "(defpackage #:p (:export #:foo foo))" "FOO")))
    (ok (null (%kinds "(defun a () :foo)" "FOO")) "keyword")
    (ok (null (%kinds "(defun a () (list '#:foo))" "FOO")) "uninterned")
    (ok (null (%kinds (format nil "(defun a () \"foo\") ; foo~%") "FOO")) "string and comment")
    (ok (equal '("call") (%kinds "(defun foo (n) (foo (1- n)))" "FOO"))
        "a recursive call is still a call")
    (ok (equal '("method") (%kinds "(defmethod foo ((x integer)) x)" "FOO")))))

(deftest scan-text-marks-flet-shadowing
  (testing "sites below a flet binding the name carry the operator"
    (let ((sites (%sites "(defun a () (flet ((foo (x) (foo x))) (foo 1)))" "FOO")))
      (ok (equal '("call" "call") (mapcar #'first sites))
          "the binding name itself is not a site")
      (ok (every (lambda (site) (equal "flet" (third site))) sites))))
  (testing "an flet of another name shadows nothing"
    (ok (equal '(nil) (mapcar #'third (%sites "(defun a () (flet ((bar () 1)) (foo)))" "FOO"))))))

(deftest scan-text-records-enclosing-form
  (testing "index, lines, type, name, test and in-package of each form"
    (let* ((text (format nil "(in-package #:p1)~%~
                              (defun a ()~%~
                              ~2@T(foo))~%~
                              #+sbcl~%~
                              (defmethod b ((x integer))~%~
                              ~2@T(foo))~%~
                              (in-package :p2)~%~
                              (deftest c-test~%~
                              ~2@T(foo))~%~
                              (eval-when (:execute) (foo))~%"))
           (forms (scan-text text "FOO")))
      (flet ((field (i key) (gethash key (nth i forms))))
        (ok (= 4 (length forms)))
        (ok (equal '(1 2 4 5) (mapcar (lambda (form) (gethash "index" form)) forms))
            "indexes count every top-level expression, in-package forms included")
        (ok (equal "defun" (field 0 "form_type")))
        (ok (equal "a" (field 0 "form_name")))
        (ok (equal "P1" (field 0 "in_package")))
        (ok (null (field 0 "test_name")))
        (ok (equal "b ((x integer))" (field 1 "form_name")))
        (ok (= 4 (field 1 "start_line")) "a reader conditional starts the form")
        (ok (= 6 (field 1 "end_line")))
        (ok (equal "c-test" (field 2 "test_name")))
        (ok (equal "rove" (field 2 "test_framework")))
        (ok (equal "P2" (field 2 "in_package")))
        (ok (equal "eval-when" (field 3 "form_type")))
        (ok (null (field 3 "form_name")))))))

(deftest scan-text-keeps-token-column-and-context
  (testing "the token as written, its 1-based column and its line"
    (let* ((forms (scan-text (format nil "(defun a ()~%  (fx:foo 1))") "FOO"))
           (site (aref (gethash "sites" (first forms)) 0)))
      (ok (equal "fx:foo" (gethash "token" site)))
      (ok (= 2 (gethash "line" site)))
      (ok (= 4 (gethash "column" site)))
      (ok (equal "(fx:foo 1))" (gethash "context" site))))))

(deftest scan-text-stops-at-max-sites
  (testing "collection stops and says so"
    (multiple-value-bind (forms count truncated)
        (scan-text "(defun a () (foo) (foo) (foo))" "FOO" :max-sites 2)
      (ok (= 2 count))
      (ok truncated)
      (ok (= 2 (length (gethash "sites" (first forms))))))))

(deftest scan-project-reports-files-and-failures
  (testing "files scanned, files matched, forms and parse failures"
    (let ((dir (uiop:ensure-directory-pathname
                (uiop:merge-pathnames* (format nil "cl-mcp-refs-scan-~D/" (random 1000000))
                                       (uiop:temporary-directory)))))
      (ensure-directories-exist dir)
      (unwind-protect
           (flet ((put (name text)
                    (with-open-file (s (merge-pathnames name dir)
                                       :direction :output :if-exists :supersede
                                       :external-format :utf-8)
                      (write-string text s))))
             (put "uses.lisp" "(defun a () (foo))")
             (put "silent.lisp" "(defun b () (bar))")
             (put "broken.lisp" "(defun c () (foo")
             (let ((scan (scan-project "foo" :root dir)))
               (ok (equal "FOO" (gethash "target_name" scan)))
               (ok (stringp (gethash "root" scan)))
               (ok (= 3 (gethash "files_scanned" scan)))
               (ok (= 2 (gethash "files_matched" scan)))
               (ok (= 1 (length (gethash "forms" scan))))
               (ok (= 1 (length (gethash "parse_failures" scan))))
               (ok (search "broken.lisp"
                           (gethash "abs_path" (aref (gethash "parse_failures" scan) 0))))
               (ok (null (gethash "truncated_at" scan)))
               (ok (null (gethash "skipped_reason" scan)))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest scan-project-without-root-is-skipped
  (testing "no root means no scan, with a reason"
    (let ((scan (scan-project "foo" :root nil)))
      (ok (stringp (gethash "skipped_reason" scan)))
      (ok (zerop (length (gethash "forms" scan)))))))

(deftest target-name-from-designator-validates
  (testing "the name as read, or an argument error"
    (ok (equal "FOO" (target-name-from-designator "pkg::foo")))
    (ok (handler-case (progn (target-name-from-designator ":foo") nil)
          (arg-validation-error () t)))))
