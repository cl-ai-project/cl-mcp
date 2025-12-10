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

(deftest indent-mode-idempotency
  (testing "applying twice produces same result"
    (let* ((input (format nil "(defun foo ()~%  (let ((x 1))~%    (+ x 1"))
           (output1 (apply-indent-mode input))
           (output2 (apply-indent-mode output1)))
      (ok (string= output1 output2))))
  (testing "idempotent on already balanced code"
    (let* ((input (format nil "(defun foo ()~%  (let ((x 1))~%    (+ x 1)))"))
           (output (apply-indent-mode input)))
      (ok (string= (string-trim '(#\Newline) input)
                   (string-trim '(#\Newline) output))))))

(deftest indent-mode-escaped-quotes
  (testing "handles escaped quotes inside strings"
    (let* ((input "(print \"Say \\\"hello\\\" (to me)\")")
           (output (apply-indent-mode input)))
      (ok (string= (string-trim '(#\Newline) output)
                   "(print \"Say \\\"hello\\\" (to me)\")"))
      (ok (= (count #\( output) (count #\) output)))))
  (testing "handles complex string escapes"
    (let* ((input "(print \"Path: C:\\\\Users\\\\test\\\"file.txt\\\"\")")
           (output (apply-indent-mode input)))
      (ok (= (count #\( output) (count #\) output))))))

(deftest indent-mode-inline-structures
  (testing "preserves inline structures correctly"
    (let* ((input "(when t (print 1) (print 2))")
           (output (apply-indent-mode input)))
      (ok (string= (string-trim '(#\Newline) output)
                   "(when t (print 1) (print 2))"))
      (ok (= (count #\( output) (count #\) output)))))
  (testing "handles nested inline expressions"
    (let* ((input "(foo (bar (baz)))")
           (output (apply-indent-mode input)))
      (ok (string= (string-trim '(#\Newline) output)
                   "(foo (bar (baz)))"))
      (ok (= (count #\( output) (count #\) output))))))

(deftest indent-mode-comment-unbalanced
  (testing "handles unbalanced parens in comments"
    (let* ((input (format nil "(defun foo ()~%  ;; (unbalanced~%  :ok"))
           (output (apply-indent-mode input)))
      (ok (search ":ok)" output))
      (ok (= (count #\( output) 3)))) ; (defun, foo(), "(unbalanced
  (testing "ignores multiple comment parens"
    (let* ((input (format nil "(defun bar ()~%  ; ) ) )~%  ; ( ( (~%  42"))
           (output (apply-indent-mode input)))
      (ok (search "42)" output)))))
