(defpackage #:cl-mcp/tests/utils-printing-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/printing
                #:safe-prin1))

(in-package #:cl-mcp/tests/utils-printing-test)

(deftest safe-prin1-simple-values
  (testing "prints simple values correctly"
    (ok (string= (safe-prin1 42) "42"))
    (ok (string= (safe-prin1 "hello") "\"hello\""))
    (ok (string= (safe-prin1 'foo) "FOO"))
    (ok (string= (safe-prin1 nil) "NIL"))))

(deftest safe-prin1-lists
  (testing "prints lists correctly"
    (ok (string= (safe-prin1 '(1 2 3)) "(1 2 3)"))
    (ok (string= (safe-prin1 '(a b c)) "(A B C)"))))

(deftest safe-prin1-respects-level
  (testing "respects print-level parameter"
    (let ((nested '((((deep))))))
      (ok (search "#" (safe-prin1 nested :level 2))))))

(deftest safe-prin1-respects-length
  (testing "respects print-length parameter"
    (let ((long-list '(1 2 3 4 5 6 7 8 9 10 11 12)))
      (ok (search "..." (safe-prin1 long-list :length 5))))))

(deftest safe-prin1-handles-errors
  (testing "returns error message on print failure"
    ;; Create an object that fails to print
    (let ((result (safe-prin1 (make-array 0 :element-type nil))))
      ;; Should return a string, not signal an error
      (ok (stringp result)))))

(deftest safe-prin1-default-parameters
  (testing "uses sensible defaults"
    ;; Default level is 3, length is 10
    (let ((deep '((((very deep nested structure))))))
      (ok (stringp (safe-prin1 deep))))))
