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
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log
                #:*log-level*
                #:*log-stream*)
  (:import-from #:cl-mcp/src/utils/paths
                #:native-path-namestring)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:target-name-from-designator
                #:scan-text
                #:scan-project
                #:top-level-forms-at)
  (:import-from #:cl-mcp/src/source-snapshot
                #:read-source-snapshot
                #:snapshot-range-digest
                #:digest-string-octets))

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
    (ok (equal '("quoted") (%kinds "(defun a () (list 'foo))" "FOO")))
    (ok (equal '("quoted" "quoted") (%kinds "(defun a () '(foo (foo)))" "FOO")))
    (ok (equal '("reference") (%kinds "(defun a () (list foo))" "FOO")))
    (ok (equal '("bind" "reference") (%kinds "(defun a () (let ((foo 1)) foo))" "FOO")))
    (ok (equal '("bind") (%kinds "(defun a (foo) nil)" "FOO")))
    (ok (equal '("set" "set") (%kinds "(defun a () (setf foo 1) (setq foo 2))" "FOO")))
    (ok (equal '("call") (%kinds "(defun a () (setf (foo x) 1))" "FOO")))
    (ok (equal '("call") (%kinds "(defun a () (funcall #'(lambda (x) (foo x)) 1))" "FOO")))
    (ok (equal '("reference") (%kinds "(defclass a (foo) ())" "FOO")))))

(deftest scan-text-classifies-quoted-function-designators
  (testing "a quoted name as FUNCALL's, APPLY's or MULTIPLE-VALUE-CALL's function is a function"
    (ok (equal '("function") (%kinds "(defun a () (funcall 'foo 1))" "FOO")))
    (ok (equal '("function") (%kinds "(defun a (args) (apply 'foo args))" "FOO")))
    (ok (equal '("function") (%kinds "(defun a () (multiple-value-call 'foo (values)))" "FOO")))
    (ok (equal '("function") (%kinds "(defun a () (funcall (quote foo) 1))" "FOO"))
        "(quote foo) written out is the same designator")
    (ok (equal '("function") (%kinds "(defun a () (funcall #+sbcl 'foo 1))" "FOO"))
        "a reader conditional around the designator is transparent"))
  (testing "the other arguments, and quoted data elsewhere, are walked as before"
    (ok (equal '("quoted") (%kinds "(defun a () (list 'foo))" "FOO")))
    (ok (equal '("quoted") (%kinds "(defun a (bar) (funcall bar 'foo))" "FOO"))
        "a quoted name that is not the function argument is data")
    (ok (equal '("function" "quoted") (%kinds "(defun a () (apply 'foo 'foo nil))" "FOO")))
    (ok (equal '("quoted") (%kinds "(defun a () (funcall '(foo)))" "FOO"))
        "a quoted list is data, not a designator")
    (ok (equal '("template") (%kinds "(defmacro m () `(funcall 'foo))" "FOO"))
        "inside a backquote template it is still template")))

(deftest scan-text-classifies-place-modifying-macros
  (testing "the bare-symbol place of INCF, DECF, POP, PUSH and PUSHNEW is a set"
    (ok (equal '("set") (%kinds "(defun a () (incf foo))" "FOO")))
    (ok (equal '("set") (%kinds "(defun a () (decf foo 2))" "FOO")))
    (ok (equal '("set") (%kinds "(defun a () (pop foo))" "FOO")))
    (ok (equal '("set") (%kinds "(defun a () (push 1 foo))" "FOO")))
    (ok (equal '("set") (%kinds "(defun a () (pushnew 1 foo))" "FOO"))))
  (testing "the other arguments, and a compound place, are walked as before"
    (ok (equal '("reference") (%kinds "(defun a (list) (push foo list))" "FOO"))
        "the item PUSH adds is only read")
    (ok (equal '("set" "reference") (%kinds "(defun a () (incf foo foo))" "FOO"))
        "the place is a set, the delta a reference")
    (ok (equal '("call") (%kinds "(defun a (x) (incf (foo x)))" "FOO"))
        "a compound place is ordinary code")))

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

(deftest scan-text-form-name-is-one-line
  (testing "a defmethod's form_name has no line break however long its lambda list"
    (let* ((text (format nil "(defmethod write-out ((stream sink) string &optional (start 0) end ~
(fill-pointer-output nil) (element-type 'character))~%  (target stream))"))
           (form (first (scan-text text "TARGET"))))
      (ok (equal (concatenate 'string
                              "write-out ((stream sink) string &optional (start 0) end "
                              "(fill-pointer-output nil) (element-type 'character))")
                 (gethash "form_name" form))))))

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

(defun %make-temp-dir (label)
  "Create and return a fresh directory pathname under the temporary directory.
LABEL goes into its name, so a leftover directory says which test made it."
  (let ((dir (uiop:ensure-directory-pathname
              (uiop:merge-pathnames* (format nil "cl-mcp-refs-scan-~A-~D/"
                                             label (random 1000000))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun %put-file (dir name text)
  "Write TEXT as UTF-8 to the file NAME under the directory DIR."
  (with-open-file (s (merge-pathnames name dir)
                     :direction :output :if-exists :supersede
                     :external-format :utf-8)
    (write-string text s)))

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
             (let* ((scan (let ((*project-root* dir))
                            (scan-project "foo" :root dir)))
                    (uses-truename (namestring (truename (merge-pathnames "uses.lisp" dir))))
                    (silent-truename (namestring (truename (merge-pathnames "silent.lisp" dir))))
                    (broken-truename (namestring (truename (merge-pathnames "broken.lisp" dir))))
                    (scanned-files (coerce (gethash "scanned_files" scan) 'list)))
               (ok (equal "FOO" (gethash "target_name" scan)))
               (ok (stringp (gethash "root" scan)))
               (ok (= 3 (gethash "files_scanned" scan)))
               (ok (= 2 (gethash "files_matched" scan)))
               (ok (eql 0 (gethash "files_denied" scan)))
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
      (ok (zerop (length (gethash "forms" scan))))))
  (testing "a root given while the project root is unset is not scanned either"
    ;; The read policy is anchored at *PROJECT-ROOT*; without it nothing can
    ;; be vouched for, whatever ROOT says.
    (let ((dir (%make-temp-dir "unset")))
      (unwind-protect
           (progn
             (%put-file dir "a.lisp" "(defun a () (foo))")
             (let* ((scan (let ((*project-root* nil))
                            (scan-project "foo" :root dir)))
                    (reason (gethash "skipped_reason" scan)))
               (ok (and (stringp reason) (search "project root is not set" reason)))
               (ok (zerop (length (gethash "forms" scan))))
               (ok (zerop (gethash "files_scanned" scan)))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

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
               (flet ((w (str)
                        (write-sequence (sb-ext:string-to-octets str :external-format :utf-8) s)))
                 (w "(defun a () (foo)) ; comment with a bad byte: ")
                 (write-byte #xE9 s)
                 (w (format nil " end~%"))))
             (let ((scan (let ((*project-root* dir))
                           (scan-project "foo" :root dir))))
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
              (scan-text (format nil "(defun a () (foo))~%~
                                      (named-readtables:in-readtable :standard)~%~
                                      (defun b () (foo))~%")
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
                      (format nil "(defun a () (foo))~%~
                                   (named-readtables:in-readtable :standard)~%~
                                   (defun b () (foo))~%")
                      s))
                   (let ((scan (let ((*project-root* dir))
                                 (scan-project "foo" :root dir))))
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
                 ;; The link's target lies under BASE, the project root, so the
                 ;; read policy allows the file it leads to.
                 (let* ((scan (let ((*project-root* base))
                                (scan-project "foo" :root root)))
                        (forms (gethash "forms" scan))
                        (expected (namestring (truename (merge-pathnames "a.lisp" real)))))
                   (ok (= 1 (length forms)))
                   (ok (equal expected (gethash "abs_path" (aref forms 0))))))
             (error ()
               (skip "could not create a symlink in this environment")))
        (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)))))

(deftest scan-project-does-not-read-outside-the-readable-paths
  (testing "a file reached through a symlink that leaves the project root is denied, not read"
    (require :sb-posix)
    (let* ((base (%make-temp-dir "deny"))
           (root (uiop:ensure-directory-pathname (merge-pathnames "root/" base)))
           (outside (uiop:ensure-directory-pathname (merge-pathnames "outside/" base)))
           (link (merge-pathnames "linked" root)))
      (ensure-directories-exist root)
      (ensure-directories-exist outside)
      (unwind-protect
           (progn
             (%put-file outside "a.lisp" "(defun a () (foo))")
             (%put-file root "own.lisp" "(defun b () (foo))")
             (if (not (ignore-errors
                       (sb-posix:symlink (uiop:native-namestring outside)
                                         (uiop:native-namestring link))
                       t))
                 (skip "could not create a symlink in this environment")
                 (let* ((scan (let ((*project-root* root))
                                (scan-project "foo" :root root)))
                        (forms (coerce (gethash "forms" scan) 'list))
                        (scanned-files (coerce (gethash "scanned_files" scan) 'list))
                        (own (namestring (truename (merge-pathnames "own.lisp" root))))
                        (denied (namestring (truename (merge-pathnames "a.lisp" outside)))))
                   (ok (eql 1 (gethash "files_denied" scan)) "the outside file is counted")
                   (ok (= 1 (gethash "files_scanned" scan)) "only the root's own file counts")
                   (ok (= 1 (gethash "files_matched" scan)))
                   (ok (equal (list own) (mapcar (lambda (form) (gethash "abs_path" form))
                                                 forms))
                       "the root's own file is scanned; the outside file is not in forms")
                   (ok (equal (list own) scanned-files)
                       "the outside file is not claimed as scanned")
                   (ok (not (member denied scanned-files :test #'equal)))
                   (ok (zerop (length (gethash "parse_failures" scan)))
                       "a denied file is not a parse failure either"))))
        (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)))))

(deftest scan-project-skips-a-root-outside-the-readable-paths
  (testing "a root the read policy does not allow is skipped with a reason, reading nothing"
    (let* ((base (%make-temp-dir "outside-root"))
           (root (uiop:ensure-directory-pathname (merge-pathnames "root/" base)))
           (outside (uiop:ensure-directory-pathname (merge-pathnames "outside/" base))))
      (ensure-directories-exist root)
      (ensure-directories-exist outside)
      (unwind-protect
           (progn
             (%put-file outside "a.lisp" "(defun a () (foo))")
             (let* ((scan (let ((*project-root* root))
                            (scan-project "foo" :root outside)))
                    (reason (gethash "skipped_reason" scan)))
               (ok (and (stringp reason) (search "outside the readable paths" reason)))
               (ok (zerop (length (gethash "forms" scan))))
               (ok (zerop (gethash "files_scanned" scan)))
               (ok (zerop (length (gethash "scanned_files" scan))))))
        (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)))))

(deftest scan-project-looks-a-package-up-once-per-scan
  (testing "files of a package the parent lacks share one search for its definition"
    ;; Package discovery reads files through FS-READ-FILE, which logs an
    ;; fs.read.open event per read; scan-project's own reads go through
    ;; FS-READ-SOURCE-TEXT, which logs fs.read-source.open instead, and it
    ;; never parses sub/package.lisp, which does not mention FOO.  So each
    ;; fs.read.open of that file is one walk: three without the cache (one
    ;; per file using the package), one with it.
    (let ((dir (uiop:ensure-directory-pathname
                (uiop:merge-pathnames* (format nil "cl-mcp-refs-scan-pkg-~D/" (random 1000000))
                                       (uiop:temporary-directory)))))
      (ensure-directories-exist (merge-pathnames "sub/" dir))
      (unwind-protect
           (let ((root (truename dir)))
             (flet ((write-source (name text)
                      (with-open-file (s (merge-pathnames name root)
                                         :direction :output :if-exists :supersede
                                         :external-format :utf-8)
                        (write-string text s))))
               (dolist (name '("a" "b" "c"))
                 (write-source (format nil "~A.lisp" name)
                               (format nil "(in-package #:cl-mcp-refs-scan-absent-pkg)~%~
                                            (defun ~A () (foo))~%"
                                       name)))
               (write-source "sub/package.lisp"
                             "(defpackage #:cl-mcp-refs-scan-absent-pkg (:use #:cl))"))
             (let* ((log (make-string-output-stream))
                    (scan (let ((*project-root* root)
                                (*log-level* :debug)
                                (*log-stream* log))
                            (scan-project "foo" :root root)))
                    (defining (namestring (merge-pathnames "sub/package.lisp" root)))
                    (reads (with-input-from-string (in (get-output-stream-string log))
                             (loop for line = (read-line in nil)
                                   while line
                                   count (and (search "\"fs.read.open\"" line)
                                              (search defining line))))))
               (ok (= 3 (length (gethash "forms" scan))) "every file is scanned")
               (ok (null (find-package "CL-MCP-REFS-SCAN-ABSENT-PKG"))
                   "the package stays absent from the parent")
               (ok (= 1 reads) "the defining file is read by one walk, not one per file")))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defun %write-tmp (name text)
  "Write TEXT to tests/tmp/NAME in the cl-mcp source tree; return its truename namestring."
  (let ((file (asdf/system:system-relative-pathname :cl-mcp (format nil "tests/tmp/~A" name))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    (namestring (truename file))))

(defun %name-shapes (names)
  "Return each source name in NAMES as (TOKEN . SETF-FLAG): the shape a slot
option contributes to a signature's :readers or :writers.  A NIL name --
an option value this scanner refuses to guess at -- stays NIL."
  (mapcar (lambda (name)
            (and name (cons (getf name :token) (and (getf name :setf) t))))
          names))

(deftest top-level-forms-at-describes-forms-starting-on-lines
  (testing "form_type and form_name of the forms starting on the given lines"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at.lisp"
                            (format nil "(in-package #:cl-user)~%~%(defclass widget ()~%  ())~%~%~
(defmethod paint ((w widget) stream)~%  (list w stream))~%~%#+sbcl~%(defun gated () 1)~%"))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(3 6 9 10 4))
             (ok (null failure))
             (ok (equal '("defclass" "widget")
                        (list (getf (first (gethash 3 table)) :form-type)
                              (getf (first (gethash 3 table)) :form-name))))
             (ok (equal :defclass (getf (getf (first (gethash 3 table)) :signature) :kind)))
             (ok (equal '("defmethod" "paint ((w widget) stream)")
                        (list (getf (first (gethash 6 table)) :form-type)
                              (getf (first (gethash 6 table)) :form-name))))
             (ok (equal '("defun" "gated")
                        (list (getf (first (gethash 9 table)) :form-type)
                              (getf (first (gethash 9 table)) :form-name)))
                 "the #+sbcl line")
             (ok (eq (first (gethash 9 table)) (first (gethash 10 table)))
                 "the wrapped form's own line shares the same entry")
             (ok (null (gethash 4 table)) "a line inside a form"))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-gives-definition-signatures
  (let ((*project-root* (asdf:system-source-directory :cl-mcp))
        (path (%write-tmp "top-level-forms-at-signatures.lisp"
                          (format nil "(in-package #:cl-user)~%~
(defmethod fx:render :around ((w fx:widget) stream (mode (eql :fast))~%    ~
&optional (depth 0) &key k)~%  (list w stream mode depth k))~%~
(defmethod (setf title) (value (w widget))~%  value)~%~
(defmethod combine + ((a integer) b)~%  a)~%~
(defgeneric (setf title) (value w))~%~
(defclass fx:widget () ())~%~
(define-condition oops (error) ())~%~
(defstruct (point (:constructor mk)) x)~%~
(defstruct plain x)~%~
(defun helper () 1)~%"))))
    (unwind-protect
         (multiple-value-bind (table failure)
             (top-level-forms-at path '(2 5 7 9 10 11 12 13 14))
           (flet ((signature (line) (getf (first (gethash line table)) :signature))
                  (tokens (plists) (mapcar (lambda (p) (getf p :token)) plists)))
             (ok (null failure))
             (testing "a defmethod's tokens are the source text, unresolved"
               (let ((sig (signature 2)))
                 (ok (equal :defmethod (getf sig :kind)))
                 (ok (equal "fx:render" (getf (getf sig :name) :token)))
                 (ok (null (getf (getf sig :name) :setf)))
                 (ok (equal '(":around") (tokens (getf sig :qualifiers))))
                 (let ((specializers (getf sig :specializers)))
                   (ok (equal '(:class :class :eql)
                              (mapcar (lambda (s) (getf s :kind)) specializers))
                       "widget, T (an unspecialized parameter) and the EQL mode")
                   (ok (equal "fx:widget" (getf (first specializers) :token)))
                   (ok (equal "CL-USER" (getf (first specializers) :in-package))
                       "a specializer the source spells out keeps the file's own package")
                   (ok (equal "T" (getf (second specializers) :token)))
                   (ok (equal "COMMON-LISP" (getf (second specializers) :in-package))
                       "the synthesized T names COMMON-LISP:T, never the file package's T")
                   (ok (equal :keyword (getf (getf (third specializers) :datum) :kind)))
                   (ok (equal "FAST" (getf (getf (third specializers) :datum) :name))))))
             (testing "a (setf x) name"
               (let ((sig (signature 5)))
                 (ok (equal "title" (getf (getf sig :name) :token)))
                 (ok (eq t (getf (getf sig :name) :setf)))
                 (ok (equal '("T" "widget") (tokens (getf sig :specializers))))))
             (testing "a symbol qualifier"
               (let ((sig (signature 7)))
                 (ok (equal '("+") (tokens (getf sig :qualifiers))))
                 (ok (equal '("integer" "T") (tokens (getf sig :specializers))))))
             (testing "the name of other definitions"
               (ok (equal "title" (getf (getf (signature 9) :name) :token)) "defgeneric")
               (ok (equal :defgeneric (getf (signature 9) :kind)))
               (ok (equal "fx:widget" (getf (getf (signature 10) :name) :token)) "defclass")
               (ok (equal "oops" (getf (getf (signature 11) :name) :token)) "define-condition")
               (ok (equal "point" (getf (getf (signature 12) :name) :token))
                   "defstruct with options")
               (ok (equal "plain" (getf (getf (signature 13) :name) :token)) "defstruct"))
             (testing "no kind-specific fields for other forms"
               (ok (equal '("defun" "helper")
                          (list (getf (first (gethash 14 table)) :form-type)
                                (getf (first (gethash 14 table)) :form-name))))
               (ok (equal :other (getf (signature 14) :kind)))
               (ok (equal "defun" (getf (getf (signature 14) :head) :token))))))
      (ignore-errors (delete-file path)))))

(deftest top-level-forms-at-tags-the-eql-datum
  (testing "an EQL specializer's datum is tagged per spec 3.3, kind by kind"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-eql.lisp"
                            (format nil "~{~A~%~}"
                                   (list "(in-package #:cl-user)"
                                         "(defmethod area ((s (eql \"str\"))) 1)"
                                         "(defmethod area ((s (eql *v*))) 2)"
                                         "(defmethod area ((s (eql (f)))) 3)"
                                         "(defmethod area ((s (eql #\\A))) 4)"
                                         "(defmethod area ((s (eql 'foo))) 5)"
                                         "(defmethod area ((s (eql (quote foo)))) 6)"
                                         "(defmethod area ((s (eql :k))) 7)"
                                         "(defmethod area ((s (eql t))) 8)"
                                         "(defmethod area ((s (eql nil))) 9)"
                                         "(defmethod area ((s (eql 3))) 10)")))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(2 3 4 5 6 7 8 9 10 11))
             (ok (null failure))
             (flet ((eql-datum (line)
                      (getf (first (getf (getf (first (gethash line table)) :signature)
                                         :specializers))
                            :datum)))
               (testing "unverifiable data keep a specific reason"
                 (ok (equal :unverifiable (getf (eql-datum 2) :kind)) "a string")
                 (ok (search "string" (getf (eql-datum 2) :reason)) "the reason names the type")
                 (ok (equal :unverifiable (getf (eql-datum 3) :kind)) "a variable reference")
                 (ok (equal :unverifiable (getf (eql-datum 4) :kind)) "an arbitrary call"))
               (ok (equal '(:kind :character :value "A") (eql-datum 5)))
               (testing "'foo is confirmed by the reader macro character alone"
                 (ok (equal '(:kind :symbol :token "foo" :in-package "CL-USER" :quoted :reader)
                            (eql-datum 6))))
               (testing "(quote foo) carries the operator's own token for the worker to resolve"
                 (let ((datum (eql-datum 7)))
                   (ok (equal :symbol (getf datum :kind)))
                   (ok (equal "foo" (getf datum :token)))
                   (ok (equal "CL-USER" (getf datum :in-package)))
                   (ok (equal :operator (getf datum :quoted)))
                   (ok (equal '(:token "quote" :in-package "CL-USER") (getf datum :quote-token)))))
               (ok (equal '(:kind :keyword :name "K") (eql-datum 8)))
               (ok (equal '(:kind :boolean :value "T") (eql-datum 9)))
               (ok (equal '(:kind :boolean :value "NIL") (eql-datum 10)))
               (ok (equal '(:kind :integer :value "3") (eql-datum 11)))))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-tags-an-explicit-quote-as-operator-even-if-shadowable
  (testing "(quote x) stays :operator, with a token, when the package might shadow quote"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-quote-shadow.lisp"
                            (format nil "~{~A~%~}"
                                   (list "(in-package #:cl-mcp-refs-scan-quote-shadow)"
                                         "(defmethod area ((s (eql (quote foo)))) 1)")))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(2))
             (ok (null failure))
             (let ((datum (getf (first (getf (getf (first (gethash 2 table)) :signature)
                                             :specializers))
                                :datum)))
               (ok (equal :symbol (getf datum :kind)))
               (ok (equal :operator (getf datum :quoted))
                   "the parent never decides quote is shadowed; it just hands over the token")
               (ok (equal '(:token "quote" :in-package "CL-MCP-REFS-SCAN-QUOTE-SHADOW")
                          (getf datum :quote-token)))))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-returns-every-form-starting-on-a-line
  (testing "two top-level forms on the same line both come back, in source order"
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (source (format nil "~{~A~%~}"
                           (list "(in-package #:cl-user)"
                                 "(defun a () 1) (defmethod area ((s circle)) 2)")))
           (path (%write-tmp "top-level-forms-at-same-line.lisp" source)))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(2))
             (ok (null failure))
             (let ((forms (gethash 2 table)))
               (ok (equal '("a" "area ((s circle))")
                          (mapcar (lambda (form) (getf form :form-name)) forms)))
               (ok (equal "(defun a () 1)"
                          (subseq source (getf (first forms) :start) (getf (first forms) :end))))
               (ok (equal "(defmethod area ((s circle)) 2)"
                          (subseq source (getf (second forms) :start)
                                  (getf (second forms) :end))))))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-keeps-tokens-unresolved
  (testing "a token from an unknown package comes back exactly as written, not resolved"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-unknown-package.lisp"
                            (format nil "~{~A~%~}"
                                   (list "(in-package #:cl-user)"
                                         (concatenate 'string
                                          "(defmethod area :around ((s pkg-a:circle) "
                                          "(n (eql 3/4)) other &optional x) 1)"))))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(2))
             (ok (null failure))
             (let* ((sig (getf (first (gethash 2 table)) :signature))
                    (specializers (getf sig :specializers)))
               (ok (= 3 (length specializers)) "s, n and the unspecialized other; x is &optional")
               (ok (equal "pkg-a:circle" (getf (first specializers) :token)))
               (ok (equal :eql (getf (second specializers) :kind)))
               (ok (equal '(:kind :ratio :numerator "3" :denominator "4")
                          (getf (second specializers) :datum)))
               (ok (equal :class (getf (third specializers) :kind)))
               (ok (equal "T" (getf (third specializers) :token))
                   "an unspecialized parameter synthesizes the literal T")
               (ok (equal "CL-USER" (getf (first specializers) :in-package))
                   "an explicit specializer resolves in the package the file was read in")
               (ok (equal "COMMON-LISP" (getf (third specializers) :in-package))
                   "the synthesized T resolves in COMMON-LISP instead")))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-describes-defgeneric-methods-and-defclass-slots
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (testing "defgeneric's (:method ...) options carry qualifiers and specializers"
      (let ((path (%write-tmp
                   "top-level-forms-at-defgeneric-methods.lisp"
                   (format nil "~{~A~%~}"
                          (list "(in-package #:cl-user)"
                                (concatenate 'string
                                 "(defgeneric area (shape) (:method ((s circle)) 1) "
                                 "(:method :around ((s square)) 2))"))))))
        (unwind-protect
             (multiple-value-bind (table failure) (top-level-forms-at path '(2))
               (ok (null failure))
               (let* ((sig (getf (first (gethash 2 table)) :signature))
                      (methods (getf sig :methods)))
                 (ok (= 2 (length methods)))
                 (ok (null (getf (first methods) :qualifiers)))
                 (ok (equal "circle"
                            (getf (first (getf (first methods) :specializers)) :token)))
                 (ok (equal ":around"
                            (getf (first (getf (second methods) :qualifiers)) :token)))
                 (ok (equal "square"
                            (getf (first (getf (second methods) :specializers)) :token)))))
          (ignore-errors (delete-file path)))))
    (testing "a defclass slot's :reader, :writer and :accessor names"
      (let ((path (%write-tmp
                   "top-level-forms-at-defclass-slots.lisp"
                   (format nil "~{~A~%~}"
                          (list "(in-package #:cl-user)"
                                "(defclass widget ()"
                                "  ((name :reader widget-name :writer set-widget-name)"
                                "   (id :accessor widget-id)"
                                "   (tag :writer (setf widget-tag)) bare))")))))
        (unwind-protect
             (multiple-value-bind (table failure) (top-level-forms-at path '(2))
               (ok (null failure))
               (let* ((sig (getf (first (gethash 2 table)) :signature))
                      (slots (getf sig :slots)))
                 (ok (= 4 (length slots)))
                 (ok (equal "name" (getf (getf (first slots) :name) :token)))
                 (ok (equal '("widget-name")
                            (mapcar (lambda (tok) (getf tok :token))
                                    (getf (first slots) :readers))))
                 (ok (equal '("set-widget-name")
                            (mapcar (lambda (tok) (getf tok :token))
                                    (getf (first slots) :writers))))
                 (ok (equal '("widget-id")
                            (mapcar (lambda (tok) (getf tok :token))
                                    (getf (second slots) :readers))))
                 (ok (equal '("widget-id")
                            (mapcar (lambda (tok) (getf tok :token))
                                    (getf (second slots) :writers)))
                     "an accessor contributes to both readers and writers")
                 (ok (equal '(("widget-name")) (%name-shapes (getf (first slots) :readers)))
                     ":reader x defines the plain function x")
                 (ok (equal '(("set-widget-name"))
                            (%name-shapes (getf (first slots) :writers)))
                     ":writer x defines the plain function x, never (setf x)")
                 (ok (equal '(("widget-id")) (%name-shapes (getf (second slots) :readers))))
                 (ok (equal '(("widget-id" . t)) (%name-shapes (getf (second slots) :writers)))
                     ":accessor x defines the (setf x) writer, not a plain x writer")
                 (ok (equal "tag" (getf (getf (third slots) :name) :token)))
                 (ok (null (getf (third slots) :readers)))
                 (ok (equal '(("widget-tag" . t)) (%name-shapes (getf (third slots) :writers)))
                     ":writer (setf x) defines the setf function, named by the inner symbol")
                 (ok (equal "bare" (getf (getf (fourth slots) :name) :token)))
                 (ok (null (getf (fourth slots) :readers)))
                 (ok (null (getf (fourth slots) :writers)))))
          (ignore-errors (delete-file path)))))
    (testing "a :reader or :accessor value CL does not allow becomes an unresolvable name"
      (let ((path (%write-tmp
                   "top-level-forms-at-defclass-invalid-accessors.lisp"
                   (format nil "~{~A~%~}"
                          (list "(in-package #:cl-user)"
                                "(defclass gizmo ()"
                                "  ((a :accessor (setf gizmo-a))"
                                "   (b :reader 42)))")))))
        (unwind-protect
             (multiple-value-bind (table failure) (top-level-forms-at path '(2))
               (ok (null failure))
               (let* ((sig (getf (first (gethash 2 table)) :signature))
                      (slots (getf sig :slots)))
                 (ok (= 2 (length slots)))
                 (ok (equal '(nil) (getf (first slots) :readers))
                     "an :accessor that is not a bare symbol resolves to nothing")
                 (ok (equal '(nil) (getf (first slots) :writers)))
                 (ok (equal '(nil) (getf (second slots) :readers))
                     "neither does a :reader that is not a symbol at all")))
          (ignore-errors (delete-file path)))))))

(deftest top-level-forms-at-tracks-in-package-switches
  (testing "a token's in-package is the designator in effect where it is written"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-in-package.lisp"
                            (format nil "~{~A~%~}"
                                   (list "(in-package #:common-lisp-user)"
                                         "(defclass one () ())"
                                         "(in-package #:keyword)"
                                         "(defclass two () ())")))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(2 4))
             (ok (null failure))
             (ok (equal "COMMON-LISP-USER"
                        (getf (getf (getf (first (gethash 2 table)) :signature) :name)
                              :in-package)))
             (ok (equal "KEYWORD"
                        (getf (getf (getf (first (gethash 4 table)) :signature) :name)
                              :in-package))))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-reports-why-it-found-nothing
  (testing "a file that does not parse gives its reader error"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-bad.lisp"
                            (format nil "(in-package #:cl-user)~%~%~
(defparameter *x* #.(+ 1 2))~%"))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(3))
             (ok (zerop (hash-table-count table)))
             (ok (and (stringp failure) (search "*READ-EVAL*" failure)) failure))
        (ignore-errors (delete-file path)))))
  (testing "a file outside the readable paths is not opened"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (outside (merge-pathnames "cl-mcp-top-level-forms-at-outside.lisp"
                                    (uiop:temporary-directory))))
      (with-open-file (out outside :direction :output :if-exists :supersede)
        (write-string "(defun outside () 1)" out))
      (unwind-protect
           (ok (eq :denied (nth-value 1 (top-level-forms-at (namestring outside) '(1)))))
        (ignore-errors (delete-file outside))))))

(deftest top-level-forms-at-without-lines-reads-nothing
  (testing "no lines asked for is no failure and an empty table"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-empty.lisp" "(defun one () 1)")))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '())
             (ok (zerop (hash-table-count table)))
             (ok (null failure)))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-accepts-a-source-snapshots-text
  (testing "READ-SOURCE-SNAPSHOT's :TEXT drives the CST and the digest from one read"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-snapshot.lisp"
                            (format nil "(in-package #:cl-user)~%(defun widget () 1)~%"))))
      (unwind-protect
           (multiple-value-bind (snapshot snapshot-failure) (read-source-snapshot path)
             (ok (null snapshot-failure))
             (multiple-value-bind (table failure)
                 (top-level-forms-at "/does/not/exist.lisp" '(2) :text (getf snapshot :text))
               (ok (null failure) "the read policy is not consulted when TEXT is given")
               (let ((entry (first (gethash 2 table))))
                 (ok (equal '("defun" "widget")
                            (list (getf entry :form-type) (getf entry :form-name))))
                 (ok (equal "(defun widget () 1)"
                            (subseq (getf snapshot :text) (getf entry :start) (getf entry :end)))
                     "the form's [start, end) matches the snapshot's own text")
                 (ok (equal (snapshot-range-digest snapshot (getf entry :start) (getf entry :end))
                            (digest-string-octets "(defun widget () 1)"))
                     "the range digest covers exactly the CST span the snapshot's text produced"))))
        (ignore-errors (delete-file path))))))

(deftest scan-project-works-when-the-project-root-itself-holds-brackets
  (testing "a root named with [ ] is scanned, not refused"
    ;; The root reached COLLECT-TARGET-FILES as a string, which calls TRUENAME
    ;; on it -- parsing it with the pathname reader again, where [ ] are wild.
    ;; TRUENAME refuses a wild pathname outright, so the whole scan died with
    ;; "Can't find the TRUENAME of wild pathname".  Built natively here for the
    ;; same reason MERGE-PATHNAMES on such a string would not do.
    (let* ((dir (uiop:parse-native-namestring
                 (format nil "~Atests/tmp/scanroot[br]/"
                         (native-path-namestring
                          (asdf:system-source-directory :cl-mcp)))
                 :ensure-directory t))
           (file (merge-pathnames (uiop:parse-native-namestring "thing.lisp") dir)))
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             (with-open-file (out file :direction :output :if-exists :supersede)
               (format out "(defun thing () :here)~%"))
             ;; Nested rather than LET*: SCAN-PROJECT reads the special, so
             ;; it has to run inside the binding, not alongside it.
             (let ((*project-root* dir))
               (let ((report (scan-project "thing" :root dir)))
                 (ok (null (gethash "skipped_reason" report))
                     "the scan is not skipped")
                 (ok (plusp (gethash "files_scanned" report))
                     "and it actually reached the file")
                 (ok (search "scanroot[br]" (gethash "root" report))
                     "the root it reports is the one on disk, unescaped"))))
        (ignore-errors (delete-file file))
        (ignore-errors (uiop:delete-empty-directory dir))))))
