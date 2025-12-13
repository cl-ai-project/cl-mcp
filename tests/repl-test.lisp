;;;; tests/repl-test.lisp

(defpackage #:cl-mcp/tests/repl-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/repl #:repl-eval))

(in-package #:cl-mcp/tests/repl-test)

(defparameter *timeout-flag* nil)

(deftest repl-eval-simple
  (testing "(+ 1 2) returns 3 as a string"
    (multiple-value-bind (printed value)
        (repl-eval "(+ 1 2)")
      (ok (string= printed "3"))
      (ok (= value 3)))))

(deftest repl-eval-multiple-forms
  (testing "evaluates forms sequentially and returns last value"
    (let ((input "(defparameter *x* 10) (incf *x* 2) (* *x* 2)"))
      (multiple-value-bind (printed value)
          (repl-eval input)
        (ok (string= printed "24"))
        (ok (= value 24))))))

(deftest repl-eval-read-eval-enabled
  (testing "#.(...) is evaluated at read time"
    (multiple-value-bind (printed value)
        (repl-eval "#.(+ 1 2)")
      (ok (string= printed "3"))
      (ok (= value 3)))))

(deftest repl-eval-print-length
  (testing "respects print-length when printing result"
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(list 1 2 3 4)" :print-length 2)
      (ok (equal value '(1 2 3 4)))
      (ok (search "..." printed))
      (ok (string= stdout ""))
      (ok (string= stderr "")))))

(deftest repl-eval-captures-stdout-stderr
  (testing "captures stdout and stderr separately"
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(progn (format t \"hi\") (format *error-output* \"oops\") 99)")
      (ok (string= printed "99"))
      (ok (= value 99))
      (ok (string= stdout "hi"))
      (ok (string= stderr "oops")))))

(deftest repl-eval-invalid-package
  (testing "returns error string when package is missing"
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(+ 1 1)" :package "NO-SUCH-PACKAGE")
      (ok (search "does not exist" printed))
      (ok (string= printed value))
      (ok (string= stdout ""))
      (ok (string= stderr "")))))

(deftest repl-eval-timeout
  (testing "timeouts return a descriptive string without hanging"
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(loop)" :timeout-seconds 0.1)
      (ok (search "timed out" printed))
      (ok (eql value :timeout))
      (ok (string= stdout ""))
      (ok (string= stderr "")))))

(deftest repl-eval-timeout-kills-worker
  (testing "timeout stops the worker thread so side effects never run"
    (setf *timeout-flag* nil)
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(progn (sleep 2) (setf cl-mcp/tests/repl-test::*timeout-flag* :done))"
                   :timeout-seconds 0.1)
      (ok (search "timed out" printed))
      (ok (eql value :timeout))
      (ok (string= stdout ""))
      (ok (string= stderr ""))
      ;; give any stray worker time to run if it survived; it should not.
      (sleep 0.2)
      (ok (null *timeout-flag*)))))

(deftest repl-eval-safe-read-disables-reader-eval
  (testing "safe-read prevents #. reader evaluation"
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "#.(+ 1 2)" :safe-read t)
      (ok (or (search "#." printed) (search "read-eval" printed) (search "reader" printed)))
      (ok (string= printed value))
      (ok (string= stdout ""))
      (ok (string= stderr "")))))

(deftest repl-eval-max-output-length
  (testing "max-output-length truncates printed result"
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(make-string 20 :initial-element #\\a)" :max-output-length 5)
      (declare (ignore value))
      (ok (search "...(truncated)" printed))
      (ok (<= (length printed) (+ 5 (length "...(truncated)"))))
      (ok (string= stdout ""))
      (ok (string= stderr "")))))

(deftest repl-eval-captures-compiler-warnings
  (testing "compiler warnings are routed to stderr"
    (let* ((fn (gensym "WARN-FN-"))
           (var (gensym "UNDEFINED-VAR-"))
           (code (format nil "(defun ~A (x) (+ x ~A))" fn var)))
      (multiple-value-bind (_printed _value stdout stderr)
          (repl-eval code)
        (declare (ignore _printed _value stdout))
        (ok (search (symbol-name var)
                    stderr
                    :test #'char-equal))))))

(deftest repl-eval-captures-compiler-warnings-under-compilation-unit
  (testing "compiler warnings are captured even under an outer compilation unit"
    #+sbcl
    (sb-ext::with-compilation-unit ()
      (let* ((fn (gensym "WARN-FN-"))
             (var (gensym "UNDEFINED-VAR-"))
             (code (format nil "(defun ~A (x) (+ x ~A))" fn var)))
        (multiple-value-bind (printed value stdout stderr)
            (repl-eval code)
          (declare (ignore printed value stdout))
          (ok (search (symbol-name var) stderr :test #'char-equal)))))
    #-sbcl
    (skip "SBCL-only: SB-EXT::WITH-COMPILATION-UNIT")))

(deftest repl-eval-suppresses-compiler-trace-output
  (testing "compiler trace output is discarded"
    #+sbcl
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval
         "(let ((s (find-symbol \"*COMPILER-TRACE-OUTPUT*\" \"SB-C\")))
  (when s
    (format (symbol-value s) \"TRACE-OUT\"))
  :ok)")
      (declare (ignore printed value))
      (ok (string= stdout ""))
      (ok (not (search "trace-out" (string-downcase stderr) :test #'char-equal))))
    #-sbcl
    (skip "SBCL-only: SB-C::*COMPILER-TRACE-OUTPUT*")))
