(defpackage #:cl-mcp/tests/utils-strings-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/strings
                #:ensure-trailing-newline))

(in-package #:cl-mcp/tests/utils-strings-test)

(deftest ensure-trailing-newline-adds-newline
  (testing "adds newline to string without trailing newline"
    (ok (string= (ensure-trailing-newline "hello")
                 (concatenate 'string "hello" (string #\Newline))))))

(deftest ensure-trailing-newline-preserves-existing
  (testing "preserves string that already has trailing newline"
    (let ((s (concatenate 'string "hello" (string #\Newline))))
      (ok (string= (ensure-trailing-newline s) s)))))

(deftest ensure-trailing-newline-empty-string
  (testing "adds newline to empty string"
    (ok (string= (ensure-trailing-newline "")
                 (string #\Newline)))))

(deftest ensure-trailing-newline-multiline
  (testing "handles multiline string without trailing newline"
    (let ((input (format nil "line1~%line2"))
          (expected (format nil "line1~%line2~%")))
      (ok (string= (ensure-trailing-newline input) expected)))))

(deftest ensure-trailing-newline-multiline-with-newline
  (testing "preserves multiline string with trailing newline"
    (let ((s (format nil "line1~%line2~%")))
      (ok (string= (ensure-trailing-newline s) s)))))

(deftest ensure-trailing-newline-boundaries
  (testing "a lone carriage return is not a newline"
    (ok (string= (coerce (list #\Return #\Newline) 'string)
                 (ensure-trailing-newline (string #\Return)))))
  (testing "a newline hidden past the fill pointer does not count"
    (let ((text (make-array 3 :element-type 'character
                              :initial-contents (format nil "ab~%")
                              :fill-pointer 2)))
      (ok (string= (format nil "ab~%") (ensure-trailing-newline text))))))
