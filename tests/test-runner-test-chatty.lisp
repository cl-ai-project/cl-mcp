;;;; tests/test-runner-test-chatty.lisp --- Helper test that prints far more
;;;; than *max-test-output-length*, so the capture path can be checked for
;;;; bounding what it holds rather than only what it reports.

(defpackage #:cl-mcp/tests/test-runner-test-chatty
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok))

(in-package #:cl-mcp/tests/test-runner-test-chatty)

(deftest chatty-capture-test
  (testing "prints well past the reporting limit"
    ;; 400 000 characters against a 50 000 character limit.  Small enough to
    ;; stay quick, large enough that the difference between bounding on write
    ;; and truncating afterwards is unambiguous in the reported total.
    (let ((line (make-string 79 :initial-element #\x)))
      (dotimes (i 5000)
        (write-string line)
        (terpri)))
    (ok t)))
