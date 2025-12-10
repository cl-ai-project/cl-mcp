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
  (testing "ignores parens in strings and comments for stack tracking"
    (let* ((input (format nil "(defun foo ()~%  \"(\" ; (~%  :ok"))
           (output (apply-indent-mode input)))
      ;; Should close the defun form properly, ignoring parens in string/comment
      (ok (search ":ok)" output))
      ;; Physical parens: (defun, foo(), "(", ; ( = 4 open
      (ok (= (count #\( output) 4))
      ;; Physical parens: foo(), defun) = 2 close
      ;; String/comment parens don't affect code structure
      (ok (= (count #\) output) 2)))))

(deftest indent-mode-drops-excessive-parens
  (testing "drops excessive closing parens"
    (let* ((input "(defun foo () :ok))")
           (output (apply-indent-mode input)))
      (ok (string= (string-trim '(#\Newline) output) "(defun foo () :ok)"))
      (ok (= (count #\( output) (count #\) output))))))
