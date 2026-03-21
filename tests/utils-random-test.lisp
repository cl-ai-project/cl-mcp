(defpackage #:cl-mcp/tests/utils-random-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/random
                #:generate-random-hex-string))

(in-package #:cl-mcp/tests/utils-random-test)

(deftest generate-random-hex-string-length
  (testing "returns a hex string of exactly 2 * n-bytes characters"
    (ok (= (length (generate-random-hex-string 1)) 2))
    (ok (= (length (generate-random-hex-string 16)) 32))
    (ok (= (length (generate-random-hex-string 32)) 64))))

(deftest generate-random-hex-string-hex-chars
  (testing "returns only valid hexadecimal characters"
    (let ((hex (generate-random-hex-string 32)))
      (ok (every (lambda (c)
                   (or (digit-char-p c)
                       (find c "abcdef")))
                 hex)))))

(deftest generate-random-hex-string-uniqueness
  (testing "successive calls produce different values"
    (let ((a (generate-random-hex-string 32))
          (b (generate-random-hex-string 32)))
      (ok (not (string= a b))))))
