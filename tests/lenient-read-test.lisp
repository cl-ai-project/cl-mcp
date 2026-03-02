;;;; tests/lenient-read-test.lisp

(defpackage #:cl-mcp/tests/lenient-read-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages))

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
        "stub cleaned up after CL reader test")
    #-sbcl
    (skip "SBCL-specific test")))

(deftest stubs-cleaned-on-error
  (testing "stub packages are cleaned up even when body signals an error"
    (ignore-errors
      (call-with-lenient-packages
       (lambda ()
         (eclector.reader:read-from-string "(err-cleanup-pkg:sym)")
         (error "deliberate error"))))
    (ok (null (find-package "ERR-CLEANUP-PKG"))
        "stub cleaned up after error in body")))
