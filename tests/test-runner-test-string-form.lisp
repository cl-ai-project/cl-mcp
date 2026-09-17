;;;; tests/test-runner-test-string-form.lisp --- Helper test whose failing form is a string
;;;; This test intentionally fails, so that run-tests' rendering of a failure
;;;; whose recorded form is a bare string literal can be asserted end to end.
;;;; Not part of cl-mcp/tests: it is loaded by name from the test that uses it.

(defpackage #:cl-mcp/tests/test-runner-test-string-form
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:ng))

(in-package #:cl-mcp/tests/test-runner-test-string-form)

(deftest intentional-string-form-failure
  ;; A string is true, so this negative assertion fails, and the form Rove
  ;; records for it is the string "truthy" itself rather than a list.
  (ng "truthy"))
