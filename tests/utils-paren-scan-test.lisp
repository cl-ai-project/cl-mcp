;;;; tests/utils-paren-scan-test.lisp

(defpackage #:cl-mcp/tests/utils-paren-scan-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:scan-parens
                #:*max-nesting-depth*))

(in-package #:cl-mcp/tests/utils-paren-scan-test)

(deftest scan-parens-reports-max-depth
  (testing "a flat form is depth 1"
    (ok (= 1 (getf (scan-parens "(a b c)") :max-depth))))
  (testing "nesting is counted, not paren count"
    (ok (= 3 (getf (scan-parens "(a (b (c)) d)") :max-depth))))
  (testing "the deepest branch wins, not the last one"
    (ok (= 4 (getf (scan-parens "(((( )))) (a)") :max-depth))))
  (testing "an empty string has depth 0"
    (ok (= 0 (getf (scan-parens "") :max-depth)))))

(deftest scan-parens-max-depth-ignores-non-code
  (testing "parens inside a string literal do not count"
    (ok (= 1 (getf (scan-parens "(f \"(((((\")") :max-depth))))
  (testing "parens inside a line comment do not count"
    (ok (= 1 (getf (scan-parens "(f) ; ((((((") :max-depth))))
  (testing "parens inside a block comment do not count"
    (ok (= 1 (getf (scan-parens "(f) #| (((( |#") :max-depth))))
  (testing "a character literal open paren does not count"
    (ok (= 1 (getf (scan-parens "(f #\\()") :max-depth)))))

(deftest scan-parens-counts-prefix-reader-macros
  (testing "quote adds one level"
    (ok (= 1 (getf (scan-parens "'a") :max-depth))))
  (testing "backquote adds one level"
    (ok (= 1 (getf (scan-parens "`a") :max-depth))))
  (testing "unquote adds one level"
    (ok (= 1 (getf (scan-parens ",a") :max-depth))))
  (testing "unquote-splicing counts as one level, not two"
    (ok (= 1 (getf (scan-parens ",@a") :max-depth))))
  (testing "function-quote adds one level"
    (ok (= 1 (getf (scan-parens "#'car") :max-depth))))
  (testing "stacked quotes accumulate before their target"
    (ok (= 4 (getf (scan-parens "''''x") :max-depth))))
  (testing "a quote before a list adds to the list's own depth"
    (ok (= 2 (getf (scan-parens "'(a b)") :max-depth)))))

(deftest scan-parens-prefix-depth-resets-between-sibling-forms
  (testing "many shallow, independent quoted atoms do not accumulate depth"
    ;; If prefix depth failed to unwind once its target was read, 200
    ;; independent "'a " pairs would incorrectly report a max-depth around
    ;; 200 instead of the true peak of 1 -- exactly the false-positive
    ;; failure mode that would make ordinary files (which use quote freely)
    ;; start tripping the too-deep guard. Same property the audit named:
    ;; the depth after "''''x y" must equal the depth after "y" alone.
    (let ((source (with-output-to-string (s)
                    (dotimes (i 200)
                      (format s "'a ")))))
      (ok (= 1 (getf (scan-parens source) :max-depth))))))

(deftest scan-parens-max-depth-ignores-prefix-chars-in-non-code
  (testing "quote characters inside a string literal do not count"
    (ok (= 0 (getf (scan-parens "\"'''''\"") :max-depth))))
  (testing "quote characters inside a line comment do not count"
    (ok (= 1 (getf (scan-parens "(f) ; ''''''") :max-depth))))
  (testing "a backquote inside a block comment does not count"
    (ok (= 1 (getf (scan-parens "(f) #| ` |#") :max-depth))))
  (testing "a quote character literal does not count as a prefix macro"
    (ok (= 1 (getf (scan-parens "(f #\\')") :max-depth)))))

(deftest scan-parens-counts-feature-conditionals
  (testing "a bare feature-expression atom adds one level for the guarded form"
    (ok (= 1 (getf (scan-parens "#+t x") :max-depth))))
  (testing "the feature-expression's own nesting is counted too"
    (ok (= 2 (getf (scan-parens "#+(and) x") :max-depth))))
  (testing "the guarded form's own nesting stacks on top"
    (ok (= 2 (getf (scan-parens "#+t (a b)") :max-depth))))
  (testing "chained #+ blocks accumulate, not reset, between blocks"
    ;; Each block's own (and)/(or) closes its own bracket depth back to 0
    ;; -- a bracket-only scan would see 3 independent shallow pairs. The
    ;; real reader recurses once per block for the pending guarded-form
    ;; read, so depth must keep climbing across the chain instead.
    (ok (= 4 (getf (scan-parens "#+t#+t#+t(and)") :max-depth))))
  (testing "#- behaves the same as #+"
    (ok (= 1 (getf (scan-parens "#-t x") :max-depth))))
  (testing "a multi-character feature name is one token, not one resolve per character"
    ;; Regression for the Critical: #+t (a single-character feature name)
    ;; happened to still work even when %SCAN-RESOLVE-PENDING-PREFIXES fired
    ;; once per character instead of once per token, because there was only
    ;; one character to fire on. #+sbcl -- the ordinary, ubiquitous spelling
    ;; -- has three, and each one past the first used to pop or no-op the
    ;; marker again before the guarded form was ever reached.
    (ok (= 1 (getf (scan-parens "#+sbcl x") :max-depth))))
  (testing "chained #+ blocks with multi-character feature names accumulate, not collapse to 1"
    (let* ((prefix (with-output-to-string (s)
                     (dotimes (i 5)
                       (write-string "#+sbcl " s))))
           (source (concatenate 'string prefix ":x")))
      (ok (= 5 (getf (scan-parens source) :max-depth))))))

(deftest scan-parens-counts-sharp-equals-labels
  (testing "#n= adds one level, like quote"
    (ok (= 1 (getf (scan-parens "#1=x") :max-depth))))
  (testing "multi-digit labels are consumed as one token"
    (ok (= 1 (getf (scan-parens "#123=x") :max-depth))))
  (testing "chained #n= labels accumulate before their target"
    (ok (= 3 (getf (scan-parens "#1=#2=#3=x") :max-depth)))))

(deftest scan-parens-sharp-hash-references-do-not-recurse
  (testing "#n# is a leaf back-reference: it adds no depth of its own"
    (ok (= 1 (getf (scan-parens "#1=1 #1#") :max-depth))))
  (testing "#n# alone, with no enclosing form, is depth 0"
    (ok (= 0 (getf (scan-parens "#1#") :max-depth)))))

(deftest max-nesting-depth-is-far-above-real-code
  (testing "the limit leaves ordinary source far below it"
    ;; src/proxy.lisp と src/pool.lisp が実測 20 で、このリポジトリの最大。
    (ok (> *max-nesting-depth* 200)))
  (testing "the limit stays far below the measured CST break floor"
    ;; Nothing above catches *MAX-NESTING-DEPTH* being raised toward or past
    ;; the break floor, which would restore the original crash. Task 1
    ;; (.superpowers/sdd/2026-08-02-deep-nesting-control-stack-exhaustion/
    ;; task-1-report.md) measured the Eclector CST path surviving depth 1750
    ;; and dying at 1875; assert against the low end of that measured range.
    (ok (< *max-nesting-depth* 1750))))
