;;;; tests/paren-diagnostics-test.lisp

(defpackage #:cl-mcp/tests/paren-diagnostics-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:scan-delimiters
                #:diagnose-delimiters
                #:count-delimiter-depth
                #:lexical-state-at
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

(deftest multiple-escaped-symbols-are-not-code
  (testing "parentheses inside |...| are symbol text in both walkers"
    (ok (getf (scan-delimiters "(list '|a(b| 1)") :ok)
        "scanner does not count the ( inside the symbol")
    (multiple-value-bind (opens closes) (count-delimiter-depth "(list '|a(b| #?)")
      (ok (= opens 1))
      (ok (= closes 1)))
    (ok (eq (lexical-state-at "(a |b(" 6) :symbol) "state inside |...| is :symbol"))
  (testing "a \\| inside |...| does not end the symbol"
    (ok (getf (scan-delimiters "(list '|a\\|(b| 1)") :ok)))
  (testing "an unterminated |...| makes the diagnosis a repair failure, not a bogus fix"
    (let ((d (diagnose-delimiters (format nil "(defun f ()~%  (list '|a(b 1)"))))
      (ok (not (getf d :ok)))
      (ng (getf d :likely-fixes)))))

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

(deftest lexical-state-at-reports-context
  (testing "the state at a position reflects strings, comments and code"
    ;;            0123456789012345678901234
    (let ((text (format nil "(a \"b\" ; c~% #| d |# e)")))
      (ok (eq (lexical-state-at text 2) :code) "before the string")
      (ok (eq (lexical-state-at text 4) :string) "inside \"b\"")
      (ok (eq (lexical-state-at text 6) :code) "after the string")
      (ok (eq (lexical-state-at text 9) :line-comment) "inside ; c")
      (ok (eq (lexical-state-at text 15) :block-comment) "inside #| d |#")
      (ok (eq (lexical-state-at text 21) :code) "after the block comment")
      (ok (eq (lexical-state-at text (length text)) :code) "at the end"))))

(deftest lexical-state-at-distinguishes-pending-escape
  (testing "a backslash pending inside a string is its own state"
    (let ((text "(a \"b\\c\")"))
      ;; characters: ( a space " b \ c " ) -- the backslash is index 5, so the
      ;; state just before index 6 has the escape pending.
      (ok (eq (lexical-state-at text 5) :string) "before the backslash")
      (ok (eq (lexical-state-at text 6) :string-escape) "right after the backslash")
      (ok (eq (lexical-state-at text 7) :string) "escape consumed by c"))))

(deftest lexical-state-at-distinguishes-pending-symbol-escape
  (testing "a backslash pending inside |...| is its own state too"
    ;; characters: ( f space | a \   -- the backslash is index 5
    (ok (eq (lexical-state-at "(f |a\\" 5) :symbol) "before the backslash")
    (ok (eq (lexical-state-at "(f |a\\" 6) :symbol-escape) "right after it")
    (ok (eq (lexical-state-at "(f |a\\|b" 7) :symbol) "\\| consumed, still in the symbol")))

(deftest newline-character-literal-keeps-line-numbers
  (testing "a #\\<Newline> literal advances the line counter in both walkers"
    ;; line 1: (a #\<newline>   line 2: )   line 3: (b   -- (b is unclosed on line 3
    (let ((text (format nil "(a #\\~%)~%(b~%")))
      (let ((res (scan-delimiters text)))
        (ok (string= (getf res :kind) "unclosed"))
        (ok (= (getf res :line) 3) "scanner: the unclosed ( is on line 3"))
      (let ((seen nil))
        (cl-mcp/src/paren-diagnostics::%map-code-characters
         text (lambda (ch idx line col)
                (declare (ignore idx col))
                (when (char= ch #\() (push line seen))))
        (ok (equal (reverse seen) '(1 3)) "walker: the two ( are on lines 1 and 3")))))

(deftest block-comment-at-column-zero-does-not-evict-code
  (testing "a #| line at column 0 inside a form does not make parinfer close everything"
    (let* ((text (format nil "(defun f (x)~%  (foo x~%#|~%  (old-impl x)~%|#~%  (bar x))"))
           (d (diagnose-delimiters text))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ok (= (length fixes) 1) "exactly one line changes")
      (ok (= (getf (first fixes) :line) 2))
      (ok (= (getf (first fixes) :delta) 1) "add the one ) missing after (foo x")
      (ng (find 6 fixes :key (lambda (f) (getf f :line)))
          "(bar x) is not rewritten out of the function"))))

(deftest likely-fix-survives-a-trailing-comment
  (testing "the line that needs the ) is still named when it ends in a ; comment"
    (let* ((text (format nil "(defun compute (a b)~%  (let ((s (+ a b)))~%    ~
                              (* s 2) ; double it"))
           (d (diagnose-delimiters text))
           (fix (first (getf d :likely-fixes))))
      (ng (getf d :repair-failed))
      (ok (= (length (getf d :likely-fixes)) 1))
      (ok (= (getf fix :line) 3))
      (ok (= (getf fix :delta) 2))
      (ok (string= (getf fix :repaired) "    (* s 2))) ; double it"))))
  (testing "a # cut off at the end of the text gets no fix: (a #) does not read"
    (let ((d (diagnose-delimiters "(a #")))
      (ok (getf d :repair-failed))
      (ng (getf d :likely-fixes)))))

(deftest rendered-fix-shows-the-line-when-the-closer-is-not-appended
  (testing "a closer before a trailing comment is rendered by position, with the resulting line"
    (let* ((text (format nil "(defun compute (a b)~%  (let ((s (+ a b)))~%    ~
                              (* s 2) ; double it"))
           (d (diagnose-delimiters text))
           (fix (first (getf d :likely-fixes)))
           (rendered (format-repair-lines (getf d :likely-fixes))))
      (ng (getf fix :append-only))
      (ok (= (getf fix :column) 12) "the closers go before the space and the comment")
      (ok (getf fix :before-comment))
      (ok (search "insert 2 \")\" at column 12 (before the trailing ; comment)" rendered))
      (ok (search "\"    (* s 2))) ; double it\"" rendered)
          "the short resulting line is shown as well")
      (ng (search "add 2" rendered))))
  (testing "a plain append keeps the terse form"
    (let* ((d (diagnose-delimiters +let-binding-unclosed+))
           (fix (first (getf d :likely-fixes))))
      (ok (getf fix :append-only))
      (ok (search "add 1 \")\"" (format-repair-lines (getf d :likely-fixes))))))
  (testing "a truncated line is never offered as text to write, nor shown as X -> X"
    (let* ((long (make-string 110 :initial-element #\x))
           (text (format nil "(defun f (a)~%  (let ((s (g a)))~%    (list s ~S) ; trailing~%"
                         long))
           (d (diagnose-delimiters text))
           (fix (first (getf d :likely-fixes)))
           (rendered (format-repair-lines (getf d :likely-fixes))))
      (ok (getf fix :truncated))
      (ok (search "insert 2 \")\" at column" rendered))
      (ng (search "giving" rendered) "no resulting line for a truncated one")
      (ng (search "->  \"    (list s \\\"xxx" rendered)))))

(deftest unclosed-bracket-opener-is-a-possible-false-positive-not-unrepairable
  (testing "an unclosed [ gets parinfer's ) fixes and a caveat, not a ] instruction"
    (let* ((text (format nil "(defun f (x)~%  (foo [bar x~%(defun g () 1)"))
           (d (diagnose-delimiters text))
           (msg (format-delimiter-diagnosis d)))
      (ok (string= (getf d :kind) "unclosed"))
      (ok (string= (getf d :expected) "]"))
      (ng (getf d :repair-failed) "the rescan's only complaint is the [ opener")
      (ok (= (getf (first (getf d :likely-fixes)) :delta) 2)
          "the same two ) lisp-edit-form writes")
      (ok (search "if it is part of a symbol name this diagnosis is a false positive" msg))
      (ng (search "could not produce a readable form" msg))
      (ng (search "missing \"]\"" msg) "no instruction to insert a ]")))
  (testing "a removal names the columns of the ) it drops"
    (let* ((d (diagnose-delimiters +trailing-extra-close+))
           (fix (first (getf d :likely-fixes))))
      (ok (equal (getf fix :removed-columns) '(14)))
      (ok (search "remove 1 \")\" at column 14" (format-repair-lines (getf d :likely-fixes))))))
  (testing "a relocating fix carries a note in the diagnosis text too"
    (let* ((text (format nil "(defun f ()~%  (when x~%  (g x)~%  (h x))"))
           (msg (format-delimiter-diagnosis (diagnose-delimiters text))))
      (ok (search "NOTE: the fix on line 2 closes a form" msg)))))

(deftest repair-failed-reason-is-named
  (testing "a repair that would edit inside a string is withheld, not called unreadable"
    ;; The string's closing quote never comes, so parinfer's closer would land
    ;; inside the string; the text must say so rather than claim no repair reads.
    (let* ((text (format nil "(defun f ()~%  \"doc~%  (list 1)"))
           (d (diagnose-delimiters text)))
      (ok (string= (getf d :kind) "unclosed-string"))
      (ng (getf d :repair-failed) "an unclosed string gets no parinfer verdict at all")))
  (testing "the two failure reasons are distinguished"
    (let ((d (diagnose-delimiters (format nil "(defun f ()~%  (list 1) ; a (~%"))))
      ;; Balanced after repair: the closer goes before the comment.
      (ng (getf d :repair-failed)))
    (let ((d (diagnose-delimiters "(a (b #\\)")))
      ;; #\) is a character literal: appending ) after it works.
      (ng (getf d :repair-failed))
      (ok (= (getf (first (getf d :likely-fixes)) :delta) 2)))
    (let ((d (diagnose-delimiters "(a @")))
      ;; The text ends in an unfinished token, so no ) can follow it:
      ;; reported as :outside-code, not as unreadable.
      (ok (eq (getf d :repair-failed) :outside-code))
      (ok (search "unfinished token" (format-delimiter-diagnosis d))))
    (let ((d (diagnose-delimiters (format nil "(defun f (x)~%  (let ((y 1]~%    (+ x y)))"))))
      (ok (eq (getf d :repair-failed) :unbalanced)))
    (let* ((text (format nil "(defun f ()~%  (let ((y 1)  ; bind~%    (+ x y)))"))
           (d (diagnose-delimiters text))
           (msg (format-delimiter-diagnosis d)))
      ;; Dedent wants to close the binding list on line 2, which ends in a
      ;; comment: the closer now goes before the comment, so this repairs.
      (ng (getf d :repair-failed))
      (ok (search "Likely fix" msg)))))

(deftest false-positive-rendering-attaches-no-instruction
  (testing "every kind describes the finding but tells the caller to change nothing"
    (flet ((rendered (text)
             (format-delimiter-diagnosis (diagnose-delimiters text) :false-positive t)))
      (let ((extra (rendered "(a))"))
            (typo (rendered (format nil "(defun f (x)~%  (let ((y 1]~%    (+ x y)))")))
            (comment (rendered "(a #| b"))
            (string (rendered "(a \"b"))
            (unclosed (rendered +let-binding-unclosed+)))
        (ok (search "extra \")\"" extra))
        (ng (search "Either remove" extra))
        (ok (search "found \"]\"" typo))
        (ng (search "Replace it with" typo))
        (ng (search "Close it with" comment))
        (ng (search "Close it with" string))
        (ok (search "unclosed (form starting at line 1" unclosed))
        (ng (search "Likely fix" unclosed))
        (ng (search "Next top-level form" unclosed))
        (ng (search "fix the delimiters by hand" unclosed))))))

(deftest lexical-state-at-stops-before-a-pending-code-escape
  (testing "a \\ or # at the scan limit is reported as pending, not consumed past END"
    ;; characters: ( a space \ b )  -- the backslash is index 3
    (ok (eq (lexical-state-at "(a \\b)" 4) :pending) "\\ at the limit")
    (ok (eq (lexical-state-at "(a \\b)" 5) :code) "\\b consumed inside the range")
    ;; characters: ( a space # \ ) )  -- the # is index 3
    (ok (eq (lexical-state-at "(a #\\))" 4) :pending) "# at the limit")
    (ok (eq (lexical-state-at "(a #\\))" 6) :code) "#\\) consumed inside the range")))

(deftest lexical-state-at-stops-before-a-cut-off-character-literal
  (testing "a #\\ whose literal character lies past the scan limit is pending"
    ;; characters: ( a space # \ ) b  -- the # is index 3, the literal ) is index 5
    (ok (eq (lexical-state-at "(a #\\)b" 5) :pending) "#\\ at the limit")
    (ok (eq (lexical-state-at "(a #\\)b" 6) :code) "#\\) consumed inside the range")
    (multiple-value-bind (opens closes) (count-delimiter-depth "(a #\\)b" :end 5)
      (ok (= opens 1) "the #\\ did not swallow anything")
      (ok (= closes 0)))))

(deftest lexical-state-at-reports-block-comment-depth
  (testing "the second value is the block-comment nesting depth"
    (let ((text "(a #| x #| y |# z |# b)"))
      (multiple-value-bind (state depth) (lexical-state-at text 6)
        (ok (eq state :block-comment))
        (ok (= depth 1)))
      (multiple-value-bind (state depth) (lexical-state-at text 11)
        (ok (eq state :block-comment))
        (ok (= depth 2)))
      (multiple-value-bind (state depth) (lexical-state-at text (length text))
        (ok (eq state :code))
        (ok (= depth 0))))))

(deftest lexical-state-at-never-consults-text-past-end
  (testing "a | or # at the limit inside a block comment is pending"
    ;; characters: ( a space # | x | #  -- the closing | is index 6
    (ok (eq (lexical-state-at "(a #|x|#" 7) :pending))
    (ok (eq (lexical-state-at "(a #|x|#" 8) :code) "|# consumed inside the range"))
  (testing "a reader prefix at the limit is pending"
    (ok (eq (lexical-state-at "(a '" 4) :pending) "quote")
    (ok (eq (lexical-state-at "(a `" 4) :pending) "backquote")
    (ok (eq (lexical-state-at "(a ,@" 5) :pending) "unquote-splicing")
    (ok (eq (lexical-state-at "(a 'b)" 5) :code) "prefix consumed inside the range")))

(deftest scan-delimiters-handles-nested-block-comments
  (testing "a nested #| ... |# inside a block comment does not end it early"
    (ok (getf (scan-delimiters "(a #| outer #| inner |# ( |# b)") :ok)
        "the ( inside the outer comment is not code"))
  (testing "an outer comment left open is still reported"
    (let ((res (scan-delimiters "(a #| outer #| inner |# b)")))
      (ok (string= (getf res :kind) "unclosed-block-comment")))))

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

(deftest repair-line-differences-keeps-zero-net-changes
  (testing "a line that lost one ) and gained one is still reported, with both counts"
    (let ((diff (repair-line-differences ")(a" "(a)")))
      (ok (= (length diff) 1))
      (ok (= (getf (first diff) :delta) 0))
      (ok (= (getf (first diff) :added) 1))
      (ok (= (getf (first diff) :removed) 1))))
  (testing "a whitespace-only difference is still skipped"
    (ok (null (repair-line-differences "(a)" "(a) "))))
  (testing "such a fix is rendered as a replacement, and diagnosis offers it"
    (let ((d (diagnose-delimiters ")(a")))
      (ok (= (length (getf d :likely-fixes)) 1))
      (ok (search "->  \"(a)\"" (format-repair-lines (getf d :likely-fixes)))))))

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
    (let* ((original (format nil "(a~C~%  (b~C~%  c)" #\Return #\Return))
           ;; The real pipeline: parinfer's own output, not a hand-built pair.
           (repaired (cl-mcp/src/parinfer:apply-indent-mode original))
           (diff (repair-line-differences original repaired)))
      (ok (= (length diff) 1))
      (ok (= (getf (first diff) :line) 2))
      (ok (string= (getf (first diff) :original) "  (b"))
      (ok (string= (getf (first diff) :repaired) "  (b)"))
      (ng (find #\Return (getf (first diff) :original)))
      (ng (find #\Return (getf (first diff) :repaired)))))
  (testing "a CRLF file with a trailing newline gets its likely fix on the code line"
    (let* ((text (format nil "(defun f (x)~C~%  (let ((y 1)~C~%    (+ x y)))~C~%"
                         #\Return #\Return #\Return))
           (d (diagnose-delimiters text))
           (fix (first (getf d :likely-fixes))))
      (ok (= (length (getf d :likely-fixes)) 1))
      (ok (= (getf fix :line) 2))
      (ok (string= (getf fix :repaired) "  (let ((y 1))")))))

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

(deftest diagnose-repairs-around-block-comments
  (testing "a ( inside a multi-line #| |# comment is not code: the fix lands after the form"
    (let* ((d (diagnose-delimiters (format nil "(defun f ()~%  #| (~%  |#~%  (bar 1)")))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ng (getf d :repair-failed))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 4) "the defun is closed after (bar 1), not in the comment")
      (ok (= (getf (first fixes) :delta) 1))))
  (testing "a ) inside a one-line #| ) |# is left alone"
    (let* ((d (diagnose-delimiters (format nil "#| ) |#~%(defun f ()~%  x")))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "unclosed"))
      (ng (getf d :repair-failed))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :line) 3))))
  (testing "only the real extra ) is removed, not the one in the comment"
    (let* ((d (diagnose-delimiters "(foo)) #| note ) |#"))
           (fixes (getf d :likely-fixes)))
      (ok (string= (getf d :kind) "extra-close"))
      (ng (getf d :repair-failed))
      (ok (= (length fixes) 1))
      (ok (= (getf (first fixes) :removed) 1))
      (ok (string= (getf (first fixes) :repaired) "(foo) #| note ) |#")))))

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
                 (list (list :line 2 :original "  (let ((y 1)" :repaired "  (let ((y 1))"
                             :delta 1 :append-only t)
                       (list :line 9 :original "  x))" :repaired "  x)" :delta -1)))))
      (ok (search (format nil "~%  line 2: \"  (let ((y 1)\"  ->  add 1 \")\"") text))
      (ok (search (format nil "~%  line 9: \"  x))\"  ->  remove 1 \")\"") text))))
  (testing "no fixes gives an empty string"
    (ok (string= (format-repair-lines nil) ""))))

(deftest format-repair-lines-caps-at-ten-entries
  (testing "12 fixes render 10 lines plus a remainder sentence"
    (let* ((fixes (loop for n from 1 to 12
                        collect (list :line n :original "x" :repaired "x)" :delta 1
                                      :append-only t)))
           (text (format-repair-lines fixes)))
      (ok (search (format nil "~%  line 10: ") text))
      (ng (search (format nil "~%  line 11: ") text))
      (ng (search (format nil "~%  line 12: ") text))
      (ok (search (format nil "~%  ... and 2 more changed lines") text))))
  (testing "exactly 10 fixes render in full with no remainder sentence"
    (let* ((fixes (loop for n from 1 to 10
                        collect (list :line n :original "x" :repaired "x)" :delta 1
                                      :append-only t)))
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
      (ok (search "Next top-level form begins at line 11, so the missing \")\"" text))
      (ok (search "most likely belongs before it." text)))))

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
      (ng (search "fix the delimiters by hand." text)
          "the specific instruction stands alone; no generic sentence follows it")
      (ng (search "Likely fix" text)))))

(deftest format-diagnosis-unclosed-block-comment
  (testing "an open block comment names its position and the closing token"
    (let ((text (format-delimiter-diagnosis
                 (diagnose-delimiters (format nil "(foo)~%  #| open"))
                 :target "code")))
      (ok (search "Unterminated block comment in code: the #| opened at line 2, column 3" text))
      (ok (search "Close it with |#." text))
      (ng (search "Likely fix" text)))))

(deftest unterminated-string-is-its-own-kind
  (testing "input ending inside a string literal is not a paren problem"
    (let ((d (diagnose-delimiters (format nil "(defun f ()~%  \"oops)~%"))))
      (ok (string= (getf d :kind) "unclosed-string"))
      (ok (= (getf d :line) 2) "reported at the opening quote")
      (ok (= (getf d :column) 3))
      (ng (getf d :likely-fixes) "parinfer has nothing to say about a missing quote")
      (let ((text (format-delimiter-diagnosis d :target "code")))
        (ok (search "Unterminated string in code: the \" opened at line 2, column 3" text))
        (ok (search "Close it with \"." text))
        (ng (search "Unbalanced parentheses" text)))))
  (testing "a string that closes on a later line is still fine"
    (ok (getf (scan-delimiters (format nil "(a \"x~%y\" b)")) :ok))))

(deftest format-diagnosis-names-the-expected-delimiter
  (testing "an unclosed [ is a possible symbol character: the hint asks for ), with a caveat"
    (let ((text (format-delimiter-diagnosis
                 (diagnose-delimiters (format nil "(foo [~%(bar)")))))
      (ok (search "Next top-level form begins at line 2, so the missing \")\"" text))
      (ok (search "most likely belongs before it." text))
      (ok (search "if it is part of a symbol name this diagnosis is a false positive" text))
      (ng (search "missing \"]\"" text)))))

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
