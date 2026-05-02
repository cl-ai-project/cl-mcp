;;;; tests/tools-helpers-test.lisp
;;;;
;;;; Tests for the argument-extraction helpers used by every MCP tool.
;;;; These guards are the first line of defense for tool input safety: a
;;;; broken `extract-arg` could let malformed JSON reach internal Lisp code,
;;;; so the suite covers required/optional, each declared :type, and the
;;;; tricky "missing vs explicitly-false" distinction in `extract-boolean-arg`.

(defpackage #:cl-mcp/tests/tools-helpers-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:extract-arg
                #:extract-boolean-arg
                #:arg-validation-error
                #:validation-message))

(in-package #:cl-mcp/tests/tools-helpers-test)

(defun caught-validation-error (thunk)
  "Run THUNK and return the validation message string if it raised
ARG-VALIDATION-ERROR, otherwise NIL."
  (handler-case (progn (funcall thunk) nil)
    (arg-validation-error (c) (validation-message c))))

(deftest extract-arg-returns-string-value
 (testing "string value passes :string type check and is returned as-is"
  (let ((args (make-ht "path" "/tmp/x")))
    (ok (string= "/tmp/x" (extract-arg args "path" :type :string))))))

(deftest extract-arg-returns-integer-value
 (testing "integer value passes :integer type check"
  (let ((args (make-ht "limit" 42)))
    (ok (= 42 (extract-arg args "limit" :type :integer))))))

(deftest extract-arg-required-missing-errors
 (testing "missing required arg signals validation error mentioning the name"
  (let ((msg (caught-validation-error
              (lambda ()
                (extract-arg (make-ht) "path"
                             :type :string :required t)))))
    (ok msg "an error was raised")
    (ok (search "path" msg) "message mentions the missing arg name"))))

(deftest extract-arg-optional-missing-returns-nil
 (testing "optional missing arg returns NIL without erroring"
  (ok (null (extract-arg (make-ht) "path" :type :string)))))

(deftest extract-arg-type-mismatch-errors
 (testing "type mismatch (string expected, integer given) signals an error"
  (let ((msg (caught-validation-error
              (lambda ()
                (extract-arg (make-ht "path" 7) "path" :type :string)))))
    (ok msg)
    (ok (search "path" msg))
    (ok (search "string" msg)))))

(deftest extract-arg-no-type-no-validation
 (testing "without :type, any value passes through unchanged"
  (let ((args (make-ht "x" 'symbol-value)))
    (ok (eq 'symbol-value (extract-arg args "x"))))))

(deftest extract-arg-nil-args-treated-as-missing
 (testing "NIL args is equivalent to an empty hash-table"
  (ok (null (extract-arg nil "anything")))
  (ok (caught-validation-error
       (lambda () (extract-arg nil "anything" :required t))))))

(deftest extract-arg-array-type-accepts-vector-and-list
 (testing ":array accepts both vector and list"
  (ok (extract-arg (make-ht "xs" #(1 2)) "xs" :type :array))
  (ok (extract-arg (make-ht "xs" '(1 2)) "xs" :type :array))))

(deftest extract-boolean-arg-explicit-true
 (testing "explicit T is returned as T"
  (ok (eq t (extract-boolean-arg (make-ht "flag" t) "flag")))))

(deftest extract-boolean-arg-explicit-false
 (testing "explicit NIL is returned as NIL (not promoted to default)"
  (ok (null (extract-boolean-arg (make-ht "flag" nil) "flag" :default t)))))

(deftest extract-boolean-arg-missing-uses-default
 (testing "missing key returns the default"
  (ok (eq t (extract-boolean-arg (make-ht) "flag" :default t)))
  (ok (null (extract-boolean-arg (make-ht) "flag")))))

(deftest extract-boolean-arg-non-boolean-errors
 (testing "non-boolean value (e.g. string) signals validation error"
  (let ((msg (caught-validation-error
              (lambda ()
                (extract-boolean-arg (make-ht "flag" "yes") "flag")))))
    (ok msg)
    (ok (search "flag" msg))
    (ok (search "boolean" msg)))))
