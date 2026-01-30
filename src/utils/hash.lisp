(defpackage #:cl-mcp/src/utils/hash
  (:use #:cl)
  (:export #:make-string-hash-table
           #:alist-to-hash-table))


(in-package #:cl-mcp/src/utils/hash)

(declaim (ftype (function (&rest t) hash-table) make-string-hash-table))

(defun make-string-hash-table (&rest kvs)
  "Create a hash table with EQUAL test from key-value pairs.

Arguments:
  KVS -- A property list of alternating keys and values.
         Keys are typically strings for JSON compatibility.

Returns:
  A hash table with :TEST #'EQUAL containing the provided pairs.
  Returns an empty hash table if no arguments are provided.

Examples:
  (make-string-hash-table)
  => #<HASH-TABLE :TEST EQUAL :COUNT 0>

  (make-string-hash-table \"name\" \"foo\" \"count\" 42)
  => #<HASH-TABLE :TEST EQUAL :COUNT 2>"
  (declare (dynamic-extent kvs))
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k h) v))
    h))

(declaim (ftype (function (list) hash-table) alist-to-hash-table))

(defun alist-to-hash-table (alist)
  "Convert an association list to a hash table with EQUAL test.
Keys that are symbols are converted to lowercase strings.

Arguments:
  ALIST -- An association list where each element is (key . value).
           Symbol keys are converted via (string-downcase (symbol-name key)).

Returns:
  A hash table with :TEST #'EQUAL containing the converted pairs.

Examples:
  (alist-to-hash-table '((:name . \"foo\") (:count . 42)))
  => #<HASH-TABLE :TEST EQUAL :COUNT 2>
  ;; with keys \"name\" and \"count\""
  (declare (type list alist))
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair alist ht)
      (setf (gethash (if (symbolp (car pair))
                         (string-downcase (symbol-name (car pair)))
                         (car pair))
                     ht)
            (cdr pair)))))
