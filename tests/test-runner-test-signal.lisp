;;;; tests/test-runner-test-signal.lisp --- Helper tests for an escaping error
;;;; One test whose body signals a condition of its own class from a helper,
;;;; and one whose failing form is NIL itself.

(defpackage #:cl-mcp/tests/test-runner-test-signal
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok))

(in-package #:cl-mcp/tests/test-runner-test-signal)

(define-condition queue-empty (error)
  ()
  (:report "Queue queue is empty."))

(defun dequeue-empty ()
  (error 'queue-empty))

(deftest error-escapes
  (testing "Raise an error while testing."
    (dequeue-empty)))

(deftest nil-form-fails
  (ok nil "a NIL form"))
