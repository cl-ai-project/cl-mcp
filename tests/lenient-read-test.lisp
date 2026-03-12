;;;; tests/lenient-read-test.lisp

(defpackage #:cl-mcp/tests/lenient-read-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node-kind
                #:cst-node-value))

(in-package #:cl-mcp/tests/lenient-read-test)

(deftest single-unknown-package
  (testing "parse succeeds with a single unknown package prefix"
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (eclector.reader:read-from-string
                "(defun foo () (unknown-pkg:bar))")))))
      (ok (consp result) "result is a list")
      (ok (eq (first result) 'defun) "head is DEFUN")
      (ok (eq (second result) 'foo) "name is FOO"))))

(deftest multiple-unknown-packages
  (testing "parse succeeds with multiple unknown package prefixes"
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (eclector.reader:read-from-string
                "(defun foo () (pkg-a:x pkg-b:y))")))))
      (ok (consp result) "result is a list")
      ;; Verify both symbols are present in the body
      (let ((body (fourth result)))
        (ok (= 2 (length body)) "body has two symbols")))))

(deftest double-colon-internal-symbol
  (testing "double-colon (internal symbol access) works"
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (eclector.reader:read-from-string
                "(unknown-pkg::bar)")))))
      (ok (consp result) "result is a list")
      (let ((sym (first result)))
        (ok (symbolp sym) "element is a symbol")
        (ok (string= "BAR" (symbol-name sym))
            "symbol name is BAR")))))

(deftest defmethod-specializer
  (testing "defmethod with unknown-package specializer parses"
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (eclector.reader:read-from-string
                "(defmethod foo ((x unknown-pkg:my-class)) x)")))))
      (ok (consp result) "result is a list")
      (ok (eq (first result) 'defmethod) "head is DEFMETHOD")
      ;; The specializer list should reference the unknown symbol
      (let* ((lambda-list (third result))
             (spec (first lambda-list)))
        (ok (consp spec) "specializer is a list")
        (ok (string= "MY-CLASS"
                      (symbol-name (second spec)))
            "specializer class name is MY-CLASS")))))

(deftest stub-cleanup
  (testing "stub packages are deleted after parse completes"
    (call-with-lenient-packages
     (lambda ()
       (eclector.reader:read-from-string
        "(stub-cleanup-test-pkg:sym)")))
    (ok (null (find-package "STUB-CLEANUP-TEST-PKG"))
        "stub package removed after call")))

(deftest known-packages-unaffected
  (testing "known packages like CL are used as-is"
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (eclector.reader:read-from-string "(cl:car '(1 2))")))))
      (ok (eq (first result) 'cl:car)
          "cl:car resolves to the real CL:CAR symbol"))))

(deftest cl-reader-handles-unknown-package
  (testing "SBCL CL reader handles unknown package via retry restart"
    #+sbcl
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (read-from-string "(sbcl-reader-test-pkg:baz)")))))
      (ok (consp result) "result is a list")
      (let ((sym (first result)))
        (ok (symbolp sym) "element is a symbol")
        (ok (string= "BAZ" (symbol-name sym))
            "symbol name is BAZ")))
    (ok (null (find-package "SBCL-READER-TEST-PKG"))
        "stub cleaned up after CL reader test")))

(deftest stubs-cleaned-on-error
  (testing "stub packages are cleaned up even when body signals an error"
    (ignore-errors
      (call-with-lenient-packages
       (lambda ()
         (eclector.reader:read-from-string "(err-cleanup-pkg:sym)")
         (error "deliberate error"))))
    (ok (null (find-package "ERR-CLEANUP-PKG"))
        "stub cleaned up after error in body")))

(deftest e2e-lisp-edit-form-with-package-qualified-symbols
  (testing "lisp-edit-form replace works on file with unknown package symbols"
    (let* ((project-root (asdf:system-source-directory :cl-mcp))
           (tmp-path (merge-pathnames "tests/tmp-lenient-e2e.lisp" project-root))
           (cl-mcp/src/project-root:*project-root* project-root))
      (unwind-protect
           (progn
             (with-open-file (out tmp-path :direction :output
                                           :if-exists :supersede)
               (write-string "(in-package :cl-user)

(defun old-func ()
  (fake-pkg:do-something 42))

(defun other-func ()
  (another-pkg:process (fake-pkg:make-thing)))
" out))
             (let ((result (cl-mcp/src/lisp-edit-form:lisp-edit-form
                            :file-path (namestring tmp-path)
                            :form-type "defun"
                            :form-name "old-func"
                            :operation "replace"
                            :content "(defun old-func ()
  (fake-pkg:do-something-else 99))")))
               (ok (stringp result) "result is a string")
               (ok (search "do-something-else" result)
                   "new content present")
               (ok (search "other-func" result)
                   "other function preserved")
               (ok (search "another-pkg:process" result)
                   "other package references preserved")))
        (when (probe-file tmp-path)
          (delete-file tmp-path))))))

(deftest no-intern-into-real-packages-eclector
  (testing "Eclector: typo in a real package is not silently interned"
    ;; If someone writes cl-user:nonexistent-sym, that should error,
    ;; not silently intern NONEXISTENT-SYM into CL-USER.
    (let ((unique-name "LENIENT-TEST-NO-INTERN-SYM"))
      (assert (null (find-symbol unique-name :cl-user))
              () "precondition: symbol must not exist in CL-USER")
      ;; The read should signal an error because CL-USER is a real package
      (handler-case
          (progn
            (call-with-lenient-packages
             (lambda ()
               (eclector.reader:read-from-string
                (format nil "(cl-user:~A)" unique-name))))
            ;; If we get here, the handler swallowed the error (bug)
            (ok nil "should have signaled an error for typo in real package"))
        (error ()
          (ok t "correctly signaled error for typo in real package")))
      ;; Verify the symbol was NOT interned
      (ok (null (find-symbol unique-name :cl-user))
          "symbol was not interned into CL-USER"))))

#+sbcl
(deftest no-intern-into-real-packages-sbcl
  (testing "SBCL: typo in a real package is not silently interned/exported"
    (let ((unique-name "LENIENT-TEST-NO-INTERN-SBCL-SYM"))
      (assert (null (find-symbol unique-name :cl-user))
              () "precondition: symbol must not exist in CL-USER")
      (handler-case
          (progn
            (call-with-lenient-packages
             (lambda ()
               (read-from-string
                (format nil "(cl-user:~A)" unique-name))))
            (ok nil "should have signaled an error for typo in real package"))
        (error ()
          (ok t "correctly signaled error for typo in real package")))
      (ok (null (find-symbol unique-name :cl-user))
          "symbol was not interned into CL-USER"))))

(deftest parse-top-level-forms-with-local-nicknames
  (testing "parse-top-level-forms succeeds when file uses package-local-nicknames"
    ;; Create target package with an exported symbol
    (let ((target-pkg (make-package "LENIENT-LN-TARGET" :use nil))
          (user-pkg nil))
      (export (intern "SOME-FUNC" target-pkg) target-pkg)
      (unwind-protect
           (progn
             ;; Create user package with a local nickname for target
             (setf user-pkg (make-package "LENIENT-LN-USER" :use '(:cl)))
             (sb-ext:add-package-local-nickname "LT" target-pkg user-pkg)
             ;; Parse text that uses the local nickname
             (let* ((text (format nil "(in-package :lenient-ln-user)~%~%(defun test-func ()~%  (lt:some-func 42))~%"))
                    (nodes (parse-top-level-forms text))
                    (expr-nodes (remove-if-not
                                 (lambda (n) (eq (cst-node-kind n) :expr))
                                 nodes)))
               (ok (= 2 (length expr-nodes))
                   (format nil "Expected 2 expr nodes (in-package + defun), got ~A"
                           (length expr-nodes)))
               ;; Second expr should be the defun
               (let ((defun-node (second expr-nodes)))
                 (ok (eq 'defun (first (cst-node-value defun-node)))
                     "second form is a defun"))))
        (when (find-package "LENIENT-LN-USER")
          (delete-package "LENIENT-LN-USER"))
        (when (find-package "LENIENT-LN-TARGET")
          (delete-package "LENIENT-LN-TARGET"))))))

(deftest e2e-lisp-edit-form-with-local-nicknames
  (testing "lisp-edit-form works on file whose package uses local-nicknames"
    (let ((target-pkg (make-package "LENIENT-LN-E2E-TARGET" :use nil))
          (user-pkg nil)
          (project-root (asdf:system-source-directory :cl-mcp))
          (tmp-path nil))
      (export (intern "MAKE-DUAL" target-pkg) target-pkg)
      (unwind-protect
           (progn
             (setf user-pkg (make-package "LENIENT-LN-E2E-USER" :use '(:cl)))
             (sb-ext:add-package-local-nickname "AD" target-pkg user-pkg)
             (setf tmp-path (merge-pathnames "tests/tmp-local-nicknames.lisp"
                                             project-root))
             (let ((cl-mcp/src/project-root:*project-root* project-root))
               (with-open-file (out tmp-path :direction :output
                                             :if-exists :supersede)
                 (write-string "(in-package :lenient-ln-e2e-user)

(defun make-thing ()
  (ad:make-dual 1.0 0.0))

(defun other-thing ()
  (ad:make-dual 2.0 1.0))
" out))
               (let ((result (lisp-edit-form
                              :file-path (namestring tmp-path)
                              :form-type "defun"
                              :form-name "make-thing"
                              :operation "replace"
                              :content "(defun make-thing ()
  (ad:make-dual 99.0 0.0))")))
                 (ok (stringp result) "result is a string")
                 (ok (search "99.0" result)
                     "new content present")
                 (ok (search "other-thing" result)
                     "other function preserved"))))
        (when (and tmp-path (probe-file tmp-path))
          (delete-file tmp-path))
        (when (find-package "LENIENT-LN-E2E-USER")
          (delete-package "LENIENT-LN-E2E-USER"))
        (when (find-package "LENIENT-LN-E2E-TARGET")
          (delete-package "LENIENT-LN-E2E-TARGET"))))))

(deftest repeated-single-colon-stub-symbol
  (testing "Eclector: repeated single-colon access to stub symbol succeeds"
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (eclector.reader:read-from-string
                "(progn (stub-repeat:sym) (stub-repeat:sym) (stub-repeat:other))")))))
      (ok (consp result) "result is a list")
      (ok (= 4 (length result))
          "progn has three sub-forms")
      ;; Verify stub cleanup
      (ok (null (find-package "STUB-REPEAT"))
          "stub package removed after call"))))

#+sbcl
(deftest repeated-single-colon-stub-symbol-sbcl
  (testing "SBCL reader: repeated single-colon access to stub symbol succeeds"
    (let ((result
            (call-with-lenient-packages
             (lambda ()
               (read-from-string
                "(progn (sbcl-stub-repeat:sym) (sbcl-stub-repeat:sym) (sbcl-stub-repeat:other))")))))
      (ok (consp result) "result is a list")
      (ok (= 4 (length result))
          "progn has three sub-forms")
      (ok (null (find-package "SBCL-STUB-REPEAT"))
          "stub package removed after call"))))

(deftest parse-top-level-forms-multi-use-unknown-pkg
  (testing "parse-top-level-forms: multiple uses of unknown-pkg:symbol"
    (let* ((text "(in-package #:totally-nonexistent-pkg)

(defun func-a ()
  (ad:make-dual 42))

(defun func-b ()
  (ad:make-dual 99)
  (ad:other-func)
  (bx:something))")
           (nodes (parse-top-level-forms text))
           (expr-nodes (remove-if-not
                         (lambda (n) (eq (cst-node-kind n) :expr))
                         nodes)))
      (ok (= 3 (length expr-nodes))
          (format nil "Expected 3 expr nodes, got ~A" (length expr-nodes)))
      (ok (eq 'defun (first (cst-node-value (second expr-nodes))))
          "second form is a defun")
      (ok (eq 'defun (first (cst-node-value (third expr-nodes))))
          "third form is a defun"))))
