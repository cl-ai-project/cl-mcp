(defpackage #:cl-mcp/tests/utils-hash-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/hash
                #:make-string-hash-table
                #:alist-to-hash-table))


(in-package #:cl-mcp/tests/utils-hash-test)

(deftest make-string-hash-table-empty
  (testing "creates empty hash table with no arguments"
    (let ((h (make-string-hash-table)))
      (ok (hash-table-p h))
      (ok (zerop (hash-table-count h)))
      (ok (eq (hash-table-test h) 'equal)))))

(deftest make-string-hash-table-single-pair
  (testing "creates hash table with single key-value pair"
    (let ((h (make-string-hash-table "name" "foo")))
      (ok (= (hash-table-count h) 1))
      (ok (string= (gethash "name" h) "foo")))))

(deftest make-string-hash-table-multiple-pairs
  (testing "creates hash table with multiple key-value pairs"
    (let ((h (make-string-hash-table "name" "foo" "type" "string" "count" 42)))
      (ok (= (hash-table-count h) 3))
      (ok (string= (gethash "name" h) "foo"))
      (ok (string= (gethash "type" h) "string"))
      (ok (= (gethash "count" h) 42)))))

(deftest make-string-hash-table-string-keys
  (testing "supports string keys with equal test"
    (let ((h (make-string-hash-table "key" "value")))
      (ok (gethash "key" h))
      (ok (null (gethash 'key h))))))

(deftest make-string-hash-table-nil-value
  (testing "supports nil as a value"
    (let ((h (make-string-hash-table "empty" nil)))
      (ok (= (hash-table-count h) 1))
      (multiple-value-bind (val present) (gethash "empty" h)
        (ok (null val))
        (ok present)))))

(deftest make-string-hash-table-nested
  (testing "supports nested hash tables as values"
    (let* ((inner (make-string-hash-table "inner-key" "inner-value"))
           (outer (make-string-hash-table "nested" inner)))
      (ok (hash-table-p (gethash "nested" outer)))
      (ok (string= (gethash "inner-key" (gethash "nested" outer))
                   "inner-value")))))

(deftest alist-to-hash-table-empty
  (testing "creates empty hash table from empty alist"
    (let ((h (alist-to-hash-table '())))
      (ok (hash-table-p h))
      (ok (zerop (hash-table-count h)))
      (ok (eq (hash-table-test h) 'equal)))))

(deftest alist-to-hash-table-symbol-keys
  (testing "converts symbol keys to lowercase strings"
    (let ((h (alist-to-hash-table '((:name . "foo") (:count . 42)))))
      (ok (= (hash-table-count h) 2))
      (ok (string= (gethash "name" h) "foo"))
      (ok (= (gethash "count" h) 42)))))

(deftest alist-to-hash-table-string-keys
  (testing "preserves string keys as-is"
    (let ((h (alist-to-hash-table '(("Name" . "foo") ("COUNT" . 42)))))
      (ok (= (hash-table-count h) 2))
      (ok (string= (gethash "Name" h) "foo"))
      (ok (= (gethash "COUNT" h) 42)))))
