;;;; tests/code-refs-scan-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/code-refs-scan: finding where a symbol name is
;;;; written, classifying the position, and describing the enclosing form.

(defpackage #:cl-mcp/tests/code-refs-scan-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
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
             (let* ((scan (scan-project "foo" :root dir))
                    (uses-truename (namestring (truename (merge-pathnames "uses.lisp" dir))))
                    (silent-truename (namestring (truename (merge-pathnames "silent.lisp" dir))))
                    (broken-truename (namestring (truename (merge-pathnames "broken.lisp" dir))))
                    (scanned-files (coerce (gethash "scanned_files" scan) 'list)))
               (ok (equal "FOO" (gethash "target_name" scan)))
               (ok (stringp (gethash "root" scan)))
               (ok (= 3 (gethash "files_scanned" scan)))
               (ok (= 2 (gethash "files_matched" scan)))
               (ok (= 1 (length (gethash "forms" scan))))
               (ok (= 1 (length (gethash "parse_failures" scan))))
               (ok (search "broken.lisp"
                           (gethash "abs_path" (aref (gethash "parse_failures" scan) 0))))
               (ok (null (gethash "truncated_at" scan)))
               (ok (null (gethash "skipped_reason" scan)))
               (testing "scanned_files lists every considered file's truename"
                 (ok (= 3 (length scanned-files)))
                 (ok (member uses-truename scanned-files :test #'equal))
                 (ok (member silent-truename scanned-files :test #'equal))
                 (ok (member broken-truename scanned-files :test #'equal)))
               (testing "abs_path is a truename in both forms and parse_failures"
                 (ok (equal broken-truename
                            (gethash "abs_path" (aref (gethash "parse_failures" scan) 0))))
                 (ok (equal uses-truename
                            (gethash "abs_path" (aref (gethash "forms" scan) 0)))))))
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

(deftest scan-text-walks-non-name-def-prefixed-arguments
  (testing "a DEF...-prefixed head is only a definer when its first argument looks like a name"
    (ok (equal '("call") (%kinds "(defun a () (default-value (foo 1)))" "FOO"))
        "DEFAULT-VALUE is not a definer; its argument is ordinary code")
    (ok (equal '("call") (%kinds "(defun a () (deflate (foo x) out))" "FOO")))
    (ok (equal '("call") (%kinds "(defun a () (case k (default (foo 1))))" "FOO"))
        "a CASE clause key that reads as DEFAULT is not a definer name")
    (ok (equal '("call") (%kinds "(defun a (&key (x (default-value (foo)))) x)" "FOO"))
        "a lambda-list init form is walked the same way")
    (ok (null (%kinds "(defun foo () 1)" "FOO"))
        "a real definer's name is still excluded")
    (ok (null (%kinds "(defstruct (foo (:conc-name f-)) x)" "FOO"))
        "DEFSTRUCT's (name . options) is still skipped whole")))

(deftest scan-text-unwraps-reader-conditionals-around-atoms
  (testing "a #+feature/#-feature wrapper around a bare token is transparent"
    (let ((sites (%sites "(defun a () (list #+sbcl foo))" "FOO")))
      (ok (equal '("reference") (mapcar #'first sites)))
      (ok (equal '("foo") (mapcar #'fourth sites)) "the token excludes the #+sbcl prefix"))
    (ok (null (%kinds "(defun a () (list #+sbcl :foo))" "FOO"))
        "a wrapped keyword is still excluded")
    (ok (null (%kinds "(defun a () (list '#+sbcl #:foo))" "FOO"))
        "a wrapped uninterned symbol is still excluded")))

(deftest scan-project-tolerates-invalid-byte
  (testing "a file with an invalid byte in a comment is still read and scanned"
    (let ((dir (uiop:ensure-directory-pathname
                (uiop:merge-pathnames* (format nil "cl-mcp-refs-scan-badbyte-~D/" (random 1000000))
                                       (uiop:temporary-directory)))))
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             (with-open-file (s (merge-pathnames "badbyte.lisp" dir)
                                :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
               (flet ((w (str) (write-sequence (sb-ext:string-to-octets str :external-format :utf-8) s)))
                 (w "(defun a () (foo)) ; comment with a bad byte: ")
                 (write-byte #xE9 s)
                 (w (format nil " end~%"))))
             (let ((scan (scan-project "foo" :root dir)))
               (ok (= 1 (gethash "files_scanned" scan)))
               (ok (= 1 (gethash "files_matched" scan)))
               (ok (= 1 (length (gethash "forms" scan))))
               (ok (zerop (length (gethash "parse_failures" scan))))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest scan-text-reports-readtable-switch
  (testing "sites before an in-file readtable switch are kept, and the switch is reported"
    (if (not (asdf:find-system "named-readtables" nil))
        (skip "named-readtables is not available")
        (progn
          (asdf:load-system "named-readtables")
          (multiple-value-bind (forms count truncated reason)
              (scan-text (format nil "(defun a () (foo))~%(named-readtables:in-readtable :standard)~%(defun b () (foo))~%")
                         "FOO")
            (declare (ignore count truncated))
            (ok (= 1 (length forms)))
            (ok (stringp reason))
            (ok (search "readtable" reason)))))))

(deftest scan-project-reports-readtable-switch-as-failure
  (testing "scan-project keeps forms before the switch and records it as a parse failure"
    (if (not (asdf:find-system "named-readtables" nil))
        (skip "named-readtables is not available")
        (progn
          (asdf:load-system "named-readtables")
          (let ((dir (uiop:ensure-directory-pathname
                      (uiop:merge-pathnames* (format nil "cl-mcp-refs-scan-rt-~D/" (random 1000000))
                                             (uiop:temporary-directory)))))
            (ensure-directories-exist dir)
            (unwind-protect
                 (progn
                   (with-open-file (s (merge-pathnames "switches.lisp" dir)
                                      :direction :output :if-exists :supersede
                                      :external-format :utf-8)
                     (write-string
                      (format nil "(defun a () (foo))~%(named-readtables:in-readtable :standard)~%(defun b () (foo))~%")
                      s))
                   (let ((scan (scan-project "foo" :root dir)))
                     (ok (= 1 (length (gethash "forms" scan)))
                         "only the form before the switch is scanned")
                     (ok (= 1 (length (gethash "parse_failures" scan))))
                     (ok (search "readtable"
                                 (gethash "error" (aref (gethash "parse_failures" scan) 0))))))
              (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))))

(deftest scan-project-abs-path-resolves-symlinked-subdirectory
  (testing "a file reached through a symlinked subdirectory reports its truename as abs_path"
    (require :sb-posix)
    (let* ((base (uiop:ensure-directory-pathname
                  (uiop:merge-pathnames* (format nil "cl-mcp-refs-scan-sym-~D/" (random 1000000))
                                         (uiop:temporary-directory))))
           (real (uiop:ensure-directory-pathname (merge-pathnames "real/" base)))
           (root (uiop:ensure-directory-pathname (merge-pathnames "root/" base)))
           (link (merge-pathnames "linked" root)))
      (ensure-directories-exist real)
      (ensure-directories-exist root)
      (unwind-protect
           (handler-case
               (progn
                 (sb-posix:symlink (uiop:native-namestring real) (uiop:native-namestring link))
                 (with-open-file (s (merge-pathnames "a.lisp" real)
                                    :direction :output :if-exists :supersede
                                    :external-format :utf-8)
                   (write-string "(defun a () (foo))" s))
                 (let* ((scan (scan-project "foo" :root root))
                        (forms (gethash "forms" scan))
                        (expected (namestring (truename (merge-pathnames "a.lisp" real)))))
                   (ok (= 1 (length forms)))
                   (ok (equal expected (gethash "abs_path" (aref forms 0))))))
             (error ()
               (skip "could not create a symlink in this environment")))
        (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)))))
