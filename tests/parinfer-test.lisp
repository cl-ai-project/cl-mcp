;;;; tests/parinfer-test.lisp

(defpackage #:cl-mcp/tests/parinfer-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
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

(deftest indent-mode-same-indent-preservation
  (testing "preserves forms with same-level continuation (idempotency critical)"
    ;; This test verifies the fix for >= -> > in dedent logic
    ;; Without the fix, (foo\n bar\n baz) would be corrupted to (foo)\n bar\n baz
    (let* ((input (format nil "(foo~% bar~% baz)"))
           (output1 (apply-indent-mode input))
           (output2 (apply-indent-mode output1))
           (output3 (apply-indent-mode output2)))
      (ok (string= input output1) "First application preserves valid code")
      (ok (string= output1 output2) "Idempotent on second application")
      (ok (string= output2 output3) "Idempotent on third application")))
  (testing "preserves one-space aligned arguments"
    ;; Common Lisp style with 1-space continuation
    (let* ((input (format nil "(function arg1~% arg2~% arg3)"))
           (output1 (apply-indent-mode input))
           (output2 (apply-indent-mode output1)))
      (ok (string= input output1) "Preserves 1-space alignment")
      (ok (string= output1 output2) "Idempotent with 1-space alignment")))
  (testing "still closes on actual dedent"
    ;; Verify we didn't break normal dedent behavior
    (let* ((input (format nil "(outer~%  (inner~%    content~%  more)"))
           (output (apply-indent-mode input)))
      (ok (search "(inner" output))
      (ok (search "content)" output) "Closes inner form on dedent")
      (ok (search "more)" output) "Closes outer form"))))

(deftest indent-mode-character-literals
  (testing "Character literal #\\( should not be counted as an opening paren"
    (let* ((input "(list #\\()")
           (output (apply-indent-mode input)))
      ;; Input is balanced: (list ...).  #\( is a char literal, not an open paren.
      ;; Without fix, #\( would be counted as open paren and parinfer would add
      ;; a spurious close.
      (ok (equal output input)
          "Balanced input with #\\( should be unchanged")))
  (testing "Character literal #\\) should not be counted as a closing paren"
    (let* ((input "(list #\\))")
           (output (apply-indent-mode input)))
      ;; Input is balanced: (list ...).  #\) is a char literal, not a close paren.
      ;; Without fix, #\) would prematurely close (list and drop the real close.
      (ok (equal output input)
          "Balanced input with #\\) should be unchanged"))))

(deftest indent-mode-escapes-symbols-and-block-comments
  (testing "a single-escaped paren is symbol text, not a delimiter"
    (let ((input (format nil "(defun f ()~%  (list 'a\\(b)")))
      (ok (equal (apply-indent-mode input) (format nil "(defun f ()~%  (list 'a\\(b))"))
          "one ) is added for the unclosed defun; the \\( is left alone")))
  (testing "parens inside |...| are symbol text"
    (let ((input "(list '|a(b| 1)"))
      (ok (equal (apply-indent-mode input) input))))
  (testing "parens inside a #| ... |# block comment are neither counted nor repaired"
    (let ((input (format nil "(defun f ()~%  #| (~%  |#~%  (bar 1)")))
      (ok (equal (apply-indent-mode input)
                 (format nil "(defun f ()~%  #| (~%  |#~%  (bar 1))"))
          "the defun is closed after (bar 1); the comment is untouched")))
  (testing "a nested block comment does not end at the inner |#"
    (let ((input "(a #| x #| y |# ( |# b)"))
      (ok (equal (apply-indent-mode input) input)))))

(deftest indent-mode-closes-on-the-last-code-line
  (testing "closers go on the last code line, not on the trailing empty line"
    (let ((input (format nil "(defun f ()~%  (list 1)~%")))
      (ok (equal (apply-indent-mode input) (format nil "(defun f ()~%  (list 1))~%")))))
  (testing "a balanced text ending in a newline keeps exactly one newline"
    (let ((input (format nil "(a)~%")))
      (ok (equal (apply-indent-mode input) input))))
  (testing "blank and comment-only lines at the end are skipped"
    (let ((input (format nil "(defun f ()~%  (list 1)~%;; tail~%~%")))
      (ok (equal (apply-indent-mode input)
                 (format nil "(defun f ()~%  (list 1))~%;; tail~%~%")))))
  (testing "a dedent after a comment line closes the code line before the comment"
    (let ((input (format nil "(defun f ()~%  (let ((y 1)~%  ;; about foo~%  (foo))")))
      (ok (equal (apply-indent-mode input)
                 (format nil "(defun f ()~%  (let ((y 1)))~%  ;; about foo~%  (foo))")))))
  (testing "CRLF: the closer goes before the carriage return, and blank CRLF lines are blank"
    (let* ((crlf (format nil "~C~%" #\Return))
           (input (format nil "(defun f (x)~A  (let ((y 1)~A~A    (+ x y)))" crlf crlf crlf)))
      (ok (equal (apply-indent-mode input)
                 (format nil "(defun f (x)~A  (let ((y 1))~A~A    (+ x y)))"
                         crlf crlf crlf))))))

(deftest indent-mode-ignores-indentation-of-non-code-lines
  (testing "a column-0 block comment inside a form is not a dedent"
    (let ((input (format nil "(defun f (x)~%  (foo x~%#|~%  (old-impl x)~%|#~%  (bar x))")))
      (ok (equal (apply-indent-mode input)
                 (format nil "(defun f (x)~%  (foo x)~%#|~%  (old-impl x)~%|#~%  (bar x))"))
          "only the ) missing after (foo x is added; (bar x) stays inside the defun")))
  (testing "a string continuation line at column 0 is not a dedent"
    (let ((input (format nil "(defun f ()~%  \"doc~%continued\"~%  (list 1")))
      (ok (equal (apply-indent-mode input)
                 (format nil "(defun f ()~%  \"doc~%continued\"~%  (list 1))")))))
  (testing "closers never land on a line inside a block comment or a string"
    (let ((input (format nil "(defun f ()~%  (list 1)~%#| trailing~%note |#")))
      (ok (equal (apply-indent-mode input)
                 (format nil "(defun f ()~%  (list 1))~%#| trailing~%note |#"))))))
