;;;; tests/validate-test.lisp

(defpackage #:cl-mcp/tests/validate-test
  (:use #:cl #:rove)
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
      (ok (%ok? res)))))

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
    (let ((*lisp-check-parens-max-bytes* 1))
      (let ((res (lisp-check-parens :code "abcd")))
        (ok (null (%ok? res)))
        (ok (not (eq (%ok? res) :false)))
        (ok (string= (%kind res) "too-large"))))))
