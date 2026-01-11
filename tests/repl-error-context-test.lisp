;;;; tests/repl-error-context-test.lisp
;;;;
;;;; Tests for structured error context in repl-eval.

(defpackage #:cl-mcp/tests/repl-error-context-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/repl
                #:repl-eval))

(in-package #:cl-mcp/tests/repl-error-context-test)

(deftest repl-eval-returns-error-context
  (testing "repl-eval returns structured error context on error"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(error \"Test error\")" :package "CL-USER")
      (declare (ignore raw stdout stderr))
      ;; The printed output should contain the error message
      (ok (search "Test error" printed))
      ;; Error context should be a plist with :error t
      (ok error-context)
      (ok (getf error-context :error))
      (ok (stringp (getf error-context :condition-type)))
      (ok (stringp (getf error-context :message)))
      (ok (listp (getf error-context :restarts)))
      (ok (listp (getf error-context :frames))))))

(deftest repl-eval-no-error-context-on-success
  (testing "repl-eval returns nil error-context on success"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(+ 1 2)" :package "CL-USER")
      (declare (ignore printed raw stdout stderr))
      (ok (null error-context)))))

(deftest repl-eval-error-context-has-frames
  (testing "error context includes stack frames on SBCL"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(labels ((foo () (bar))
                            (bar () (error \"deep error\")))
                     (foo))"
                   :package "CL-USER")
      (declare (ignore printed raw stdout stderr))
      (ok error-context)
      #+sbcl
      (progn
        (ok (> (length (getf error-context :frames)) 0))
        ;; Frames should have function names
        (let ((first-frame (first (getf error-context :frames))))
          (ok (stringp (getf first-frame :function))))))))

(deftest repl-eval-error-context-respects-print-limits
  (testing "error context uses print-level and print-length"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(let ((x '((((deeply nested))))))
                     (error \"with deep local\"))"
                   :package "CL-USER"
                   :print-level 2
                   :print-length 3)
      (declare (ignore printed raw stdout stderr))
      (ok error-context)
      ;; Should complete without error even with complex locals
      (ok (getf error-context :error)))))

(deftest repl-eval-timeout-no-error-context
  (testing "timeout returns nil error-context"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(loop)" :package "CL-USER" :timeout-seconds 0.1)
      (declare (ignore printed stdout stderr))
      (ok (eq raw :timeout))
      (ok (null error-context)))))
