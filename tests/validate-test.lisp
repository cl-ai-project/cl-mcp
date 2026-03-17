;;;; tests/validate-test.lisp

(defpackage #:cl-mcp/tests/validate-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens
                #:*check-parens-max-bytes*))

(in-package #:cl-mcp/tests/validate-test)

(defun %ok? (ht) (gethash "ok" ht))

(defun %kind (ht) (gethash "kind" ht))

(defun %pos (ht key)
  (let ((p (gethash "position" ht)))
    (and p (gethash key p))))

(deftest lisp-check-parens-ok-string
  (testing "balanced string returns ok"
    (let ((res (lisp-check-parens :code "(let ((x 1)) (+ x 2))")))
      (ok (%ok? res))
      (multiple-value-bind (val presentp)
          (gethash "next_tool" res)
        (declare (ignore val))
        (ok (not presentp))))))

(deftest lisp-check-parens-extra-close
  (testing "extra closing paren reported"
    (let ((res (lisp-check-parens :code "(+ 1 2))")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "extra-close"))
      ;; extra close is the second ')' at offset 7
      (ok (= (%pos res "offset") 7)))))

(deftest lisp-check-parens-mismatch
  (testing "mismatch reports expected/found"
    (let ((res (lisp-check-parens :code "( [ ) ]")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "mismatch"))
      (ok (string= (gethash "expected" res) "]"))
      (ok (string= (gethash "found" res) ")")))))

(deftest lisp-check-parens-mismatch-includes-guidance
  (testing "mismatch result includes lisp-edit-form guidance"
    (let* ((res (lisp-check-parens :code "( [ ) ]"))
           (required (gethash "required_args" res)))
      (ok (string= (gethash "fix_code" res) "use_lisp_edit_form"))
      (ok (string= (gethash "next_tool" res) "lisp-edit-form"))
      (ok (vectorp required))
      (ok (= (length required) 5))
      (ok (string= (aref required 0) "file_path")))))
(deftest lisp-check-parens-unclosed
  (testing "unclosed opener at end"
    (let ((res (lisp-check-parens :code "(let ((x 1)) (+ x 2)")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "unclosed"))
      (ok (= (%pos res "line") 1)))))

(deftest lisp-check-parens-ignores-strings-and-comments
  (testing "parens inside strings and comments are ignored"
    (let ((res (lisp-check-parens :code "(format nil \"(\") ; )\n(list 1 2)")))
      (ok (%ok? res)))))

(deftest lisp-check-parens-too-large-returns-nil
  (testing "too large input returns ok as nil (boolean false)"
    (let ((*check-parens-max-bytes* 1))
      (let ((res (lisp-check-parens :code "abcd")))
        (ok (null (%ok? res)))
        (ok (not (eq (%ok? res) :false)))
        (ok (string= (%kind res) "too-large"))))))

(deftest lisp-check-parens-eof-reader-error-has-position
  (testing "incomplete dispatch #X gives reader-error with non-nil position"
    ;; M1: end-of-file from incomplete #, should NOT report offset 0 / line nil
    (let ((res (lisp-check-parens :code "(valid-form) #")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (string= (%kind res) "reader-error") "kind should be reader-error")
      (ok (integerp (%pos res "line")) "line must be an integer, not nil")
      (ok (>= (%pos res "offset") 12) "offset must be past the valid form"))))

(deftest lisp-check-parens-package-error-no-false-positive
  (testing "package-qualified symbol for unloaded package is not a reader error"
    ;; M2: package-error on unloaded package should return ok: true
    (let ((res (lisp-check-parens :code "(nonexistent-package::my-sym arg)")))
      (ok (%ok? res) "valid file using unloaded package must return ok: true"))))

(deftest lisp-check-parens-in-readtable-no-false-positive
  (testing "file with in-readtable declaration is not falsely flagged"
    ;; M3: in-readtable present => skip reader check => ok: true
    (let ((res (lisp-check-parens
                :code "(named-readtables:in-readtable :interpol-syntax)
(defun greet (x) x)")))
      (ok (%ok? res) "file with in-readtable should return ok: true"))))

(deftest lisp-check-parens-eof-position-not-null
  (testing "position hash for EOF-type reader error has non-null line and column"
    ;; M6: position hash must not have nil line/column for EOF errors
    (let ((res (lisp-check-parens :code "(foo) #")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (integerp (%pos res "line")) "position.line must be integer")
      (ok (integerp (%pos res "column")) "position.column must be integer"))))

(deftest lisp-check-parens-paren-error-no-null-message
  (testing "paren error response omits message key (not null)"
    (let ((res (lisp-check-parens :code "(+ 1 2))")))
      (ok (not (%ok? res)) "should be not-ok")
      (multiple-value-bind (val presentp)
          (gethash "message" res)
        (declare (ignore val))
        (ok (not presentp) "message key should be absent for paren errors")))))

(deftest lisp-check-parens-reader-error-no-null-expected-found
  (testing "reader error response omits expected/found keys (not null)"
    (let ((res (lisp-check-parens :code "(foo) #@")))
      (ok (not (%ok? res)) "should be not-ok")
      (ok (string= (%kind res) "reader-error") "kind should be reader-error")
      (multiple-value-bind (val presentp)
          (gethash "expected" res)
        (declare (ignore val))
        (ok (not presentp) "expected key should be absent for reader errors"))
      (multiple-value-bind (val presentp)
          (gethash "found" res)
        (declare (ignore val))
        (ok (not presentp) "found key should be absent for reader errors")))))

(deftest lisp-check-parens-unclosed-block-comment
  (testing "unclosed block comment at end returns unclosed-block-comment kind"
    (let ((res (lisp-check-parens :code "(foo) #|")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (string= (%kind res) "unclosed-block-comment")
          "kind should be unclosed-block-comment"))))
