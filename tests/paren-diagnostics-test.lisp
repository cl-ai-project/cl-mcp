;;;; tests/paren-diagnostics-test.lisp

(defpackage #:cl-mcp/tests/paren-diagnostics-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:scan-delimiters
                #:diagnose-delimiters
                #:count-delimiter-depth
                #:repair-line-differences
                #:format-repair-lines
                #:format-delimiter-diagnosis))

(in-package #:cl-mcp/tests/paren-diagnostics-test)

;;; Fixtures: the four measured cases from the spec (section 2.3).

(defparameter +let-binding-unclosed+
  (format nil "(defun f (x)~%  (let ((y 1)~%    (+ x y)))")
  "Missing \")\" after the let binding on line 2.")

(defparameter +trailing-extra-close+
  (format nil "(defun f (x)~%  (let ((y 1))~%    (+ x y))))")
  "One \")\" too many at the end of line 3.")

(defparameter +when-body-unclosed+
  (format nil "(defun f (x)~%  (let ((y 1))~%    (when (> x 0)~%      (format t \"~~A\" y)~%    (+ x y)))")
  "The when form on line 3 is never closed; line 4 needs one more \")\".")

(defparameter +file-middle-form-unclosed+
  (format nil "(in-package #:cl-user)~%~%(defun probe-a (x)~%  \"Docstring.\"~%  (let ((y (* x 2)))~%    (if (> y 10)~%        (format t \"big ~~A~~%\" y)~%        (format t \"small ~~A~~%\" y)~%    y))~%~%(defun probe-b (x)~%  (list x x))~%")
  "probe-a (line 3) never closes; line 8 needs one more \")\"; probe-b starts at line 11.")

(defparameter +stray-bracket+
  (format nil "(defun f (x)~%  (let ((y 1]~%    (+ x y)))")
  "A \"]\" where \")\" was meant, on line 2 column 13.")

(deftest scan-delimiters-balanced
  (testing "balanced text returns :ok t"
    (ok (getf (scan-delimiters "(+ 1 2)") :ok))))

(deftest scan-delimiters-extra-close
  (testing "extra close reports kind, offset, line and column"
    (let ((res (scan-delimiters "(+ 1 2))")))
      (ng (getf res :ok))
      (ok (string= (getf res :kind) "extra-close"))
      (ok (= (getf res :offset) 7))
      (ok (= (getf res :line) 1))
      (ok (= (getf res :column) 8)))))

(deftest scan-delimiters-unclosed
  (testing "unclosed reports the innermost still-open opener"
    (let ((res (scan-delimiters +let-binding-unclosed+)))
      (ng (getf res :ok))
      (ok (string= (getf res :kind) "unclosed"))
      (ok (string= (getf res :expected) ")"))
      (ok (= (getf res :line) 1))
      (ok (= (getf res :column) 1)))))

(deftest scan-delimiters-mismatch
  (testing "] closing ( is a mismatch at its own position"
    (let ((res (scan-delimiters +stray-bracket+)))
      (ok (string= (getf res :kind) "mismatch"))
      (ok (string= (getf res :expected) ")"))
      (ok (string= (getf res :found) "]"))
      (ok (= (getf res :line) 2))
      (ok (= (getf res :column) 13)))))

(deftest scan-delimiters-base-offset
  (testing "base-offset shifts :offset only, never :line"
    (let ((res (scan-delimiters "(+ 1 2))" :base-offset 100)))
      (ok (= (getf res :offset) 107))
      (ok (= (getf res :line) 1)))))

(deftest scan-delimiters-ignores-strings-comments-char-literals
  (testing "parens inside strings, comments and #\\( are not counted"
    (ok (getf (scan-delimiters "(list \")\" #\\( #\\) ; )
 #| ) |# )") :ok))))
