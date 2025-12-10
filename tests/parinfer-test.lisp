;;;; tests/parinfer-test.lisp

(defpackage #:cl-mcp/tests/parinfer-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode))

(in-package #:cl-mcp/tests/parinfer-test)

(deftest indent-mode-basic
  (testing "closes simple forms based on indentation"
    (let* ((input (format nil "(defun foo (x)~%  (let ((y 1))~%    (+ x y"))
           (output (apply-indent-mode input)))
      (ok (search "(+ x y)))" output))
      (ok (= (count #\( output) (count #\) output))))))

(deftest indent-mode-dedent
  (testing "closes multiple levels when dedenting"
    (let* ((input (format nil "(a~%  (b~%    (c~%  (d"))
           (output (apply-indent-mode input)))
      (ok (search "(c))" output))
      (ok (search "(d))" output)))))

(deftest indent-mode-respects-existing
  (testing "respects existing closing parens"
    (let* ((input "(list 1 2)")
           (output (apply-indent-mode input)))
      (ok (string= (string-trim '(#\Newline) output) "(list 1 2)")))))

(deftest indent-mode-strings-comments
  (testing "ignores parens in strings and comments"
    (let* ((input (format nil "(defun foo ()~%  \"(\" ; (~%  :ok"))
           (output (apply-indent-mode input)))
      (ok (search ":ok)" output))
      (ok (= (count #\( output) 2)) ; defun, ( in string
      (ok (= (count #\) output) 2)))))
