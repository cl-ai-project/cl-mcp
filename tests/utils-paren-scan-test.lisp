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

(deftest max-nesting-depth-is-far-above-real-code
  (testing "the limit leaves ordinary source far below it"
    ;; src/proxy.lisp と src/pool.lisp が実測 20 で、このリポジトリの最大。
    (ok (> *max-nesting-depth* 200))))
