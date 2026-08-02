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

(deftest max-nesting-depth-is-far-above-real-code
  (testing "the limit leaves ordinary source far below it"
    ;; src/proxy.lisp と src/pool.lisp が実測 20 で、このリポジトリの最大。
    (ok (> *max-nesting-depth* 200))))
