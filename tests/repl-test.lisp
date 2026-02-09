;;;; tests/repl-test.lisp

(defpackage #:cl-mcp/tests/repl-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/repl #:repl-eval)
  (:import-from #:cl-mcp/src/inspect #:generate-result-preview)
  (:import-from #:cl-mcp/src/object-registry #:inspectable-p))


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
        (repl-eval "(loop)" :timeout-seconds 0.1d0)
      (ok (search "timed out" printed))
      (ok (eql value :timeout))
      (ok (string= stdout ""))
      (ok (string= stderr "")))))

(deftest repl-eval-timeout-kills-worker
  (testing "timeout stops the worker thread so side effects never run"
    (setf *timeout-flag* nil)
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(progn (sleep 2) (setf cl-mcp/tests/repl-test::*timeout-flag* :done))"
                   :timeout-seconds 0.1d0)
      (ok (search "timed out" printed))
      (ok (eql value :timeout))
      (ok (string= stdout ""))
      (ok (string= stderr ""))
      ;; give any stray worker time to run if it survived; it should not.
      (sleep 0.2d0)
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

(deftest repl-eval-sanitizes-ansi-escape-codes
  (testing "ANSI escape codes are stripped from stdout"
    (multiple-value-bind (printed value stdout stderr)
        ;; Output text with ANSI color codes (ESC[32m = green, ESC[0m = reset)
        (repl-eval "(progn (format t \"~C[32mgreen~C[0m\" (code-char 27) (code-char 27)) :ok)")
      (declare (ignore value stderr))
      (ok (string= printed ":OK"))
      ;; ANSI codes should be stripped, leaving just "green"
      (ok (string= stdout "green"))
      ;; Verify no ESC character remains
      (ok (not (find (code-char 27) stdout)))))) ; ESC

(deftest repl-eval-sanitizes-control-chars
  (testing "control characters are stripped from output"
    (multiple-value-bind (printed value stdout stderr)
        ;; Output with control characters (bell, backspace)
        (repl-eval "(progn (format t \"hello~Cworld~Ctest\" (code-char 7) (code-char 8)) :ok)")
      (declare (ignore value stderr))
      (ok (string= printed ":OK"))
      ;; Control chars should be removed
      (ok (string= stdout "helloworldtest"))
      ;; Verify no control chars remain (except allowed ones)
      (ok (not (find-if (lambda (c)
                          (and (< (char-code c) 32)
                               (not (member c '(#\Tab #\Newline #\Return)))))
                        stdout))))))

(defun %has-control-chars-p (string)
  "Check if STRING contains any disallowed control characters."
  (and string
       (find-if (lambda (c)
                  (and (< (char-code c) 32)
                       (not (member c '(#\Tab #\Newline #\Return)))))
                string)))

(deftest repl-eval-no-control-chars-in-printed
  (testing "printed result never contains control characters"
    ;; Test with a value that contains control chars when printed
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(format nil \"result~C~Cend\" (code-char 7) (code-char 27))")
      (declare (ignore value stdout stderr))
      (ok (not (%has-control-chars-p printed))
          "printed should not contain control characters"))))

(deftest repl-eval-no-control-chars-in-stderr
  (testing "stderr never contains control characters"
    (multiple-value-bind (printed value stdout stderr)
        ;; Output ANSI codes and control chars to stderr
        (repl-eval "(progn (format *error-output* \"~C[31merror~C[0m~Cbeep\" (code-char 27) (code-char 27) (code-char 7)) :done)")
      (declare (ignore printed value stdout))
      (ok (not (%has-control-chars-p stderr))
          "stderr should not contain control characters")
      (ok (not (find (code-char 27) stderr)) ; ESC
          "stderr should not contain ESC character")
      (ok (string= stderr "errorbeep")
          "ANSI codes and control chars should be stripped from stderr"))))

(deftest repl-eval-no-control-chars-in-error-output
  (testing "error messages never contain control characters"
    (multiple-value-bind (printed value stdout stderr)
        ;; Trigger an error - the error message should be sanitized
        (repl-eval "(error \"fail~Cbeep\" (code-char 7))")
      (declare (ignore value stdout stderr))
      (ok (not (%has-control-chars-p printed))
          "error output should not contain control characters"))))

(deftest repl-eval-all-outputs-sanitized
  (testing "all output streams are sanitized comprehensively"
    (multiple-value-bind (printed value stdout stderr)
        ;; Comprehensive test: control chars in stdout, stderr, and result
        (repl-eval
         "(progn
            (format t \"out~C\" (code-char 8))
            (format *error-output* \"err~C\" (code-char 7))
            (format nil \"res~C\" (code-char 27)))")
      (declare (ignore value))
      ;; All outputs should be free of control characters
      (ok (not (%has-control-chars-p printed))
          "printed should be sanitized")
      (ok (not (%has-control-chars-p stdout))
          "stdout should be sanitized")
      (ok (not (%has-control-chars-p stderr))
          "stderr should be sanitized")
      ;; Verify content is preserved (minus control chars)
      (ok (search "out" stdout) "stdout content preserved")
      (ok (search "err" stderr) "stderr content preserved")
      (ok (search "res" printed) "printed content preserved"))))

(deftest generate-result-preview-list
  (testing "generates preview for list with kind, summary, elements, id"
    (let ((preview (generate-result-preview (list 1 2 3))))
      (ok (hash-table-p preview))
      (ok (string= (gethash "kind" preview) "list"))
      (ok (gethash "id" preview) "should have object id")
      (ok (gethash "elements" preview) "should have elements")
      (ok (= 3 (length (gethash "elements" preview)))))))

(deftest generate-result-preview-hash-table
  (testing "generates preview for hash-table"
    (let ((ht (make-hash-table :test 'equal)))
      (setf (gethash "key" ht) "value")
      (let ((preview (generate-result-preview ht)))
        (ok (string= (gethash "kind" preview) "hash-table"))
        (ok (gethash "id" preview))
        (ok (gethash "entries" preview))))))

(deftest generate-result-preview-max-elements
  (testing "respects max-elements and sets truncated flag"
    (let ((preview (generate-result-preview (list 1 2 3 4 5 6 7 8 9 10)
                                            :max-elements 3)))
      (ok (string= (gethash "kind" preview) "list"))
      (let ((meta (gethash "meta" preview)))
        (ok meta "should have meta")
        (ok (gethash "truncated" meta) "should be truncated"))
      (ok (<= (length (gethash "elements" preview)) 3)))))

(deftest generate-result-preview-circular-reference
  (testing "handles circular references without infinite loop"
    (let ((lst (list 1 2 3)))
      (setf (cdr (last lst)) lst)
      (let ((preview (generate-result-preview lst :max-elements 10)))
        (ok (hash-table-p preview) "should return hash-table")
        (ok (gethash "id" preview) "should have id")))))

(deftest generate-result-preview-vector
  (testing "generates preview for vector (as array)"
    (let ((preview (generate-result-preview #(a b c d e))))
      (ok (string= (gethash "kind" preview) "array"))
      (ok (gethash "id" preview))
      (ok (gethash "elements" preview))
      (ok (= 5 (length (gethash "elements" preview)))))))


(deftest repl-eval-primitive-not-inspectable
  (testing "primitive values are not inspectable"
    (multiple-value-bind (printed raw-value)
        (repl-eval "42")
      (declare (ignore printed))
      (ok (not (inspectable-p raw-value)) "number should not be inspectable"))
    (multiple-value-bind (printed raw-value)
        (repl-eval "\"hello\"")
      (declare (ignore printed))
      (ok (not (inspectable-p raw-value)) "string should not be inspectable"))
    (multiple-value-bind (printed raw-value)
        (repl-eval ":keyword")
      (declare (ignore printed))
      (ok (not (inspectable-p raw-value)) "symbol should not be inspectable"))))

(deftest repl-eval-non-primitive-inspectable
  (testing "non-primitive values are inspectable and previewable"
    (multiple-value-bind (printed raw-value)
        (repl-eval "(list 1 2 3)")
      (declare (ignore printed))
      (ok (inspectable-p raw-value) "list should be inspectable")
      (let ((preview (generate-result-preview raw-value)))
        (ok (string= (gethash "kind" preview) "list"))
        (ok (gethash "id" preview))))))
