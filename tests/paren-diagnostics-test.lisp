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

(deftest count-delimiter-depth-basic
  (testing "counts only code parens"
    (multiple-value-bind (opens closes) (count-delimiter-depth "(if (> y 10)")
      (ok (= opens 2))
      (ok (= closes 1)))
    (multiple-value-bind (opens closes)
        (count-delimiter-depth "(list \")\" #\\( #\\) ; )
 #| ( |# )")
      (ok (= opens 1))
      (ok (= closes 1)))))

(deftest single-escaped-delimiters-are-not-code
  (testing "a \\ outside a string escapes the next character in both walkers"
    (multiple-value-bind (opens closes) (count-delimiter-depth "(foo bar\\) baz)")
      (ok (= opens 1))
      (ok (= closes 1)))
    (ok (getf (scan-delimiters "(foo bar\\) baz)") :ok)
        "scanner does not treat the escaped ) as a delimiter")
    (multiple-value-bind (opens closes) (count-delimiter-depth "(foo \\( bar)")
      (ok (= opens 1))
      (ok (= closes 1)))))

(deftest escaped-newline-keeps-line-numbers
  (testing "a \\ before a physical newline still advances the scanner's line counter"
    (let ((res (scan-delimiters (format nil "(foo a\\~%b))"))))
      (ok (string= (getf res :kind) "extra-close"))
      (ok (= (getf res :line) 2) "the extra ) is on line 2, not line 1"))))

(deftest count-delimiter-depth-region-uses-lexical-context
  (testing "a region inside a string counts no parens"
    ;; positions 13-14 are the `a)` inside the string literal
    (multiple-value-bind (opens closes)
        (count-delimiter-depth "(defun f () \"a)\")" :start 13 :end 15)
      (ok (= opens 0))
      (ok (= closes 0))))
  (testing "a region in code counts only its own parens"
    ;; positions 6-8 are `(a)`
    (multiple-value-bind (opens closes)
        (count-delimiter-depth "(list (a) (b))" :start 6 :end 9)
      (ok (= opens 1))
      (ok (= closes 1)))))

(deftest repair-line-differences-reports-changed-lines
  (testing "only changed lines are listed, with the added count"
    (let ((diff (repair-line-differences
                 (format nil "(a~%  (b~%  c)")
                 (format nil "(a~%  (b)~%  c)"))))
      (ok (= (length diff) 1))
      (ok (= (getf (first diff) :line) 2))
      (ok (string= (getf (first diff) :original) "  (b"))
      (ok (string= (getf (first diff) :repaired) "  (b)"))
      (ok (= (getf (first diff) :delta) 1))))
  (testing "removed parens give a negative delta"
    (let ((diff (repair-line-differences "(a))" "(a)")))
      (ok (= (getf (first diff) :delta) -1)))))

(deftest repair-line-differences-bounds-long-lines
  (testing "a very long changed line is truncated in the stored fix, delta still exact"
    (let* ((filler (make-string 500 :initial-element #\x))
           (orig (format nil "(a ~A" filler))
           (rep (format nil "(a ~A)" filler))
           (fix (first (repair-line-differences orig rep))))
      (ok (= (getf fix :delta) 1))
      (ok (< (length (getf fix :original)) 200)
          "original line is bounded")
      (ok (< (length (getf fix :repaired)) 200)
          "repaired line is bounded")
      (ok (search "..." (getf fix :original))
          "truncation is marked"))))

(deftest repair-line-differences-strips-carriage-returns
  (testing "CRLF input does not leak a #\\Return into :original or :repaired"
    (let ((diff (repair-line-differences
                 (format nil "(a~C~%  (b~C~%  c)" #\Return #\Return)
                 (format nil "(a~C~%  (b)~C~%  c)" #\Return #\Return))))
      (ok (= (length diff) 1))
      (ok (= (getf (first diff) :line) 2))
      (ok (string= (getf (first diff) :original) "  (b"))
      (ok (string= (getf (first diff) :repaired) "  (b)"))
      (ng (find #\Return (getf (first diff) :original)))
      (ng (find #\Return (getf (first diff) :repaired))))))

(deftest repair-line-differences-skips-zero-delta-lines
  (testing "a whitespace-only difference yields no entry"
    (ok (null (repair-line-differences
               (format nil "(a  ~%  (b))")
               (format nil "(a~%  (b))"))))))

(deftest diagnose-let-binding-unclosed
  (testing "likely fix points at the let binding line"
    (let* ((d (diagnose-delimiters +let-binding-unclosed+))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ng (getf d :repair-failed))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 2))
      (ok (= (getf (first fixes) :delta) 1))
      (ok (= (getf d :unclosed-form-line) 1))
      (ok (string= (getf d :unclosed-form-head) "(defun f (x)"))
      (ng (getf d :next-top-level-line)))))

(deftest diagnose-trailing-extra-close
  (testing "likely fix removes one paren from the last line"
    (let* ((d (diagnose-delimiters +trailing-extra-close+))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "extra-close"))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 3))
      (ok (= (getf (first fixes) :delta) -1)))))

(deftest diagnose-when-body-unclosed
  (testing "likely fix points at the last line of the when body"
    (let* ((d (diagnose-delimiters +when-body-unclosed+))
           (fixes (getf d :likely-fixes)))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 4))
      (ok (= (getf (first fixes) :delta) 1)))))

(deftest diagnose-file-middle-form-unclosed
  (testing "file-level diagnosis names the open form and the next top-level line"
    (let* ((d (diagnose-delimiters +file-middle-form-unclosed+))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ok (= (getf d :unclosed-form-line) 3))
      (ok (string= (getf d :unclosed-form-head) "(defun probe-a (x)"))
      (ok (= (getf d :next-top-level-line) 11))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 8))
      (ok (= (getf (first fixes) :delta) 1)))))

(deftest diagnose-stray-bracket-is-repair-failed
  (testing "] cannot be repaired: no fixes, repair-failed t"
    (let ((d (diagnose-delimiters +stray-bracket+)))
      (ok (string= (getf d :kind) "mismatch"))
      (ok (getf d :repair-failed))
      (ng (getf d :likely-fixes)))))

(deftest diagnose-balanced-braces-are-repairable
  (testing "balanced {...} pairs (reader-macro syntax) do not block the likely fix"
    (let* ((d (diagnose-delimiters (format nil "(defun f ()~%  (foo {a b}~%  (bar 1))")))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ng (getf d :repair-failed))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 2))
      (ok (= (getf (first fixes) :delta) 1))))
  (testing "a ] closing a ( is still a mismatch, even though the result would read"
    (let ((d (diagnose-delimiters (format nil "(defun f ()~%  (list foo]"))))
      (ok (string= (getf d :kind) "mismatch"))
      (ok (getf d :repair-failed))
      (ng (getf d :likely-fixes)))))

(deftest diagnose-ok-has-no-extra-keys
  (testing "balanced text returns the plain scan plist"
    (let ((d (diagnose-delimiters "(+ 1 2)")))
      (ok (getf d :ok))
      (ng (getf d :likely-fixes))
      (ng (getf d :next-top-level-line)))))

(deftest diagnose-form-head-is-trimmed-and-bounded
  (testing "unclosed-form-head trims indentation and stops at 40 chars"
    (let* ((long-name (make-string 60 :initial-element #\a))
           (d (diagnose-delimiters (format nil "   (defun ~A (x)~%  x" long-name))))
      (ok (= (length (getf d :unclosed-form-head)) 40))
      (ok (string= (subseq (getf d :unclosed-form-head) 0 7) "(defun ")))))

(deftest format-repair-lines-wording
  (testing "add/remove wording and quoting"
    (let ((text (format-repair-lines
                 (list (list :line 2 :original "  (let ((y 1)" :repaired "  (let ((y 1))" :delta 1)
                       (list :line 9 :original "  x))" :repaired "  x)" :delta -1)))))
      (ok (search (format nil "~%  line 2: \"  (let ((y 1)\"  ->  add 1 \")\"") text))
      (ok (search (format nil "~%  line 9: \"  x))\"  ->  remove 1 \")\"") text))))
  (testing "no fixes gives an empty string"
    (ok (string= (format-repair-lines nil) ""))))

(deftest format-repair-lines-caps-at-ten-entries
  (testing "12 fixes render 10 lines plus a remainder sentence"
    (let* ((fixes (loop for n from 1 to 12
                        collect (list :line n :original "x" :repaired "x)" :delta 1)))
           (text (format-repair-lines fixes)))
      (ok (search (format nil "~%  line 10: ") text))
      (ng (search (format nil "~%  line 11: ") text))
      (ng (search (format nil "~%  line 12: ") text))
      (ok (search (format nil "~%  ... and 2 more changed lines") text))))
  (testing "exactly 10 fixes render in full with no remainder sentence"
    (let* ((fixes (loop for n from 1 to 10
                        collect (list :line n :original "x" :repaired "x)" :delta 1)))
           (text (format-repair-lines fixes)))
      (ok (search (format nil "~%  line 10: ") text))
      (ng (search "more changed lines" text)))))

(deftest format-diagnosis-unclosed
  (testing "unclosed names the form, the likely fix and the next top-level line"
    (let ((text (format-delimiter-diagnosis
                 (diagnose-delimiters +file-middle-form-unclosed+)
                 :target "/tmp/probe.lisp")))
      (ok (search "Unbalanced parentheses in /tmp/probe.lisp: unclosed (form starting at line 3: \"(defun probe-a (x)\")." text))
      (ok (search "Likely fix, inferred from indentation:" text))
      (ok (search "line 8:" text))
      (ok (search "add 1 \")\"" text))
      (ok (search "Next top-level form begins at line 11, so the missing \")\" must come before it." text)))))

(deftest format-diagnosis-unclosed-without-next-top-level
  (testing "single-form input omits the next-top-level sentence"
    (let ((text (format-delimiter-diagnosis (diagnose-delimiters +let-binding-unclosed+))))
      (ok (search "Unbalanced parentheses in code: unclosed (form starting at line 1: \"(defun f (x)\")." text))
      (ok (search "line 2:" text))
      (ng (search "Next top-level form" text)))))

(deftest format-diagnosis-extra-close
  (testing "extra-close offers both readings and the parinfer removal"
    (let ((text (format-delimiter-diagnosis (diagnose-delimiters +trailing-extra-close+))))
      (ok (search "Unbalanced parentheses in code: extra \")\" at line 3, column 14." text))
      (ok (search "Either remove that \")\" or check for a form opened earlier that was never closed." text))
      (ok (search "line 3:" text))
      (ok (search "remove 1 \")\"" text)))))

(deftest format-diagnosis-mismatch
  (testing "mismatch explains that ] is a symbol character"
    (let ((text (format-delimiter-diagnosis (diagnose-delimiters +stray-bracket+) :target "content")))
      (ok (search "Unbalanced parentheses in content: expected \")\" but found \"]\" at line 2, column 13." text))
      (ok (search "\"]\" and \"}\" are ordinary symbol characters in Common Lisp and cannot be auto-repaired." text))
      (ok (search "Replace it with \")\"." text))
      (ok (search "Automatic repair could not produce a readable form; fix the delimiters by hand." text))
      (ng (search "Likely fix" text)))))

(deftest format-diagnosis-mismatch-bracket-opener
  (testing "a [ opener does not get a \"replace it\" instruction"
    (let ((text (format-delimiter-diagnosis (diagnose-delimiters "(list [a b)"))))
      (ok (search "Unbalanced parentheses in code: expected \"]\" but found \")\" at line 1, column 11."
                  text))
      (ok (search "The \"[\" opened earlier is being treated as an opening delimiter; if it is part of a symbol name this diagnosis is a false positive."
                  text))
      (ng (search "Replace it with" text)))))

(deftest format-diagnosis-balanced-returns-nil
  (testing "a balanced diagnosis has nothing to explain"
    (ok (null (format-delimiter-diagnosis (diagnose-delimiters "(+ 1 2)"))))
    (ok (null (format-delimiter-diagnosis (list :ok t) :target "content")))))
