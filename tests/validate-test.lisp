;;;; tests/validate-test.lisp

(defpackage #:cl-mcp/tests/validate-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/validate
                #:check-parens))

(in-package #:cl-mcp/tests/validate-test)

(defun %ok? (ht) (gethash "ok" ht))
(defun %kind (ht) (gethash "kind" ht))
(defun %pos (ht key)
  (let ((p (gethash "position" ht)))
    (and p (gethash key p))))

(deftest check-parens-ok-string
  (testing "balanced string returns ok"
    (let ((res (check-parens :code "(let ((x 1)) (+ x 2))")))
      (ok (%ok? res)))))

(deftest check-parens-extra-close
  (testing "extra closing paren reported"
    (let ((res (check-parens :code "(+ 1 2))")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "extra-close"))
      ;; extra close is the second ')' at offset 7
      (ok (= (%pos res "offset") 7)))))

(deftest check-parens-mismatch
  (testing "mismatch reports expected/found"
    (let ((res (check-parens :code "( [ ) ]")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "mismatch"))
      (ok (string= (gethash "expected" res) "]"))
      (ok (string= (gethash "found" res) ")")))))

(deftest check-parens-unclosed
  (testing "unclosed opener at end"
    (let ((res (check-parens :code "(let ((x 1)) (+ x 2)")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "unclosed"))
      (ok (= (%pos res "line") 1)))))

(deftest check-parens-ignores-strings-and-comments
  (testing "parens inside strings and comments are ignored"
    (let ((res (check-parens :code "(format nil \"(\") ; )\n(list 1 2)")))
      (ok (%ok? res)))))
