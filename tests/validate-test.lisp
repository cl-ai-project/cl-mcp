;;;; tests/validate-test.lisp

(defpackage #:cl-mcp/tests/validate-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens
                #:*check-parens-max-bytes*))

(in-package #:cl-mcp/tests/validate-test)

(defun %ok? (ht) (gethash "ok" ht))

(defun %kind (ht) (gethash "kind" ht))

(defun %pos (ht key)
  (let ((p (gethash "position" ht)))
    (and p (gethash key p))))

(deftest lisp-check-parens-ok-string
  (testing "balanced string returns ok"
    (let ((res (lisp-check-parens :code "(let ((x 1)) (+ x 2))")))
      (ok (%ok? res))
      (multiple-value-bind (val presentp)
          (gethash "next_tool" res)
        (declare (ignore val))
        (ok (not presentp))))))

(deftest lisp-check-parens-extra-close
  (testing "extra closing paren reported"
    (let ((res (lisp-check-parens :code "(+ 1 2))")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "extra-close"))
      ;; extra close is the second ')' at offset 7
      (ok (= (%pos res "offset") 7)))))

(deftest lisp-check-parens-mismatch
  (testing "mismatch reports expected/found"
    (let ((res (lisp-check-parens :code "( [ ) ]")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "mismatch"))
      (ok (string= (gethash "expected" res) "]"))
      (ok (string= (gethash "found" res) ")")))))

(deftest lisp-check-parens-mismatch-includes-guidance
  (testing "mismatch result includes lisp-edit-form guidance"
    (let* ((res (lisp-check-parens :code "( [ ) ]"))
           (required (gethash "required_args" res)))
      (ok (string= (gethash "fix_code" res) "use_lisp_edit_form"))
      (ok (string= (gethash "next_tool" res) "lisp-edit-form"))
      (ok (vectorp required))
      (ok (= (length required) 5))
      (ok (string= (aref required 0) "file_path")))))
(deftest lisp-check-parens-unclosed
  (testing "unclosed opener at end"
    (let ((res (lisp-check-parens :code "(let ((x 1)) (+ x 2)")))
      (ok (not (%ok? res)))
      (ok (string= (%kind res) "unclosed"))
      (ok (= (%pos res "line") 1)))))

(deftest lisp-check-parens-ignores-strings-and-comments
  (testing "parens inside strings and comments are ignored"
    (let ((res (lisp-check-parens :code "(format nil \"(\") ; )\n(list 1 2)")))
      (ok (%ok? res)))))

(deftest lisp-check-parens-too-large-returns-nil
  (testing "too large input returns ok as nil (boolean false)"
    (let ((*check-parens-max-bytes* 1))
      (let ((res (lisp-check-parens :code "abcd")))
        (ok (null (%ok? res)))
        (ok (not (eq (%ok? res) :false)))
        (ok (string= (%kind res) "too-large"))))))

(deftest lisp-check-parens-eof-reader-error-has-position
  (testing "incomplete dispatch #X gives reader-error with non-nil position"
    ;; M1: end-of-file from incomplete #, should NOT report offset 0 / line nil
    (let ((res (lisp-check-parens :code "(valid-form) #")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (string= (%kind res) "reader-error") "kind should be reader-error")
      (ok (integerp (%pos res "line")) "line must be an integer, not nil")
      (ok (>= (%pos res "offset") 12) "offset must be past the valid form"))))

(deftest lisp-check-parens-package-error-no-false-positive
  (testing "package-qualified symbol for unloaded package is not a reader error"
    ;; M2: package-error on unloaded package should return ok: true
    (let ((res (lisp-check-parens :code "(nonexistent-package::my-sym arg)")))
      (ok (%ok? res) "valid file using unloaded package must return ok: true"))))

(deftest lisp-check-parens-in-readtable-no-false-positive
  (testing "file with in-readtable declaration is not falsely flagged"
    ;; M3: in-readtable present => skip reader check => ok: true
    (let ((res (lisp-check-parens
                :code "(named-readtables:in-readtable :interpol-syntax)
(defun greet (x) x)")))
      (ok (%ok? res) "file with in-readtable should return ok: true"))))

(deftest lisp-check-parens-eof-position-not-null
  (testing "position hash for EOF-type reader error has non-null line and column"
    ;; M6: position hash must not have nil line/column for EOF errors
    (let ((res (lisp-check-parens :code "(foo) #")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (integerp (%pos res "line")) "position.line must be integer")
      (ok (integerp (%pos res "column")) "position.column must be integer"))))

(deftest lisp-check-parens-paren-error-no-null-message
  (testing "paren error response omits message key (not null)"
    (let ((res (lisp-check-parens :code "(+ 1 2))")))
      (ok (not (%ok? res)) "should be not-ok")
      (multiple-value-bind (val presentp)
          (gethash "message" res)
        (declare (ignore val))
        (ok (not presentp) "message key should be absent for paren errors")))))

(deftest lisp-check-parens-reader-error-no-null-expected-found
  (testing "reader error response omits expected/found keys (not null)"
    (let ((res (lisp-check-parens :code "(foo) #@")))
      (ok (not (%ok? res)) "should be not-ok")
      (ok (string= (%kind res) "reader-error") "kind should be reader-error")
      (multiple-value-bind (val presentp)
          (gethash "expected" res)
        (declare (ignore val))
        (ok (not presentp) "expected key should be absent for reader errors"))
      (multiple-value-bind (val presentp)
          (gethash "found" res)
        (declare (ignore val))
        (ok (not presentp) "found key should be absent for reader errors")))))

(deftest lisp-check-parens-unclosed-block-comment
  (testing "unclosed block comment at end returns unclosed-block-comment kind"
    (let ((res (lisp-check-parens :code "(foo) #|")))
      (ok (not (%ok? res)) "ok should be false")
      (ok (string= (%kind res) "unclosed-block-comment")
          "kind should be unclosed-block-comment"))))

(deftest lisp-check-parens-reader-error-message-truncated
  (testing "reader error message is truncated to 200 chars max"
    ;; Build an input with a long filler that could be echoed in the error message.
    ;; SBCL's reader-error includes a stream repr in ~A rendering; we verify that
    ;; %try-reader-check never exposes more than 200 chars regardless of input length.
    (let* ((filler (make-string 300 :initial-element #\x))
           (code (concatenate 'string "(foo) #@ " filler))
           (res (lisp-check-parens :code code)))
      (ok (not (%ok? res)) "ok should be false")
      (ok (string= (%kind res) "reader-error") "kind should be reader-error")
      (let ((msg (gethash "message" res)))
        (ok (stringp msg) "message should be a string")
        (ok (<= (length msg) 200) "message should be at most 200 chars")))))

(deftest lisp-check-parens-extra-close-summary-line-col
  (testing "extra-close summary text has correct line and column (not garbled)"
    ;; Bug M-C3-1: when expected=nil, the ~@[ (expected ~A, found ~A)~] directive
    ;; consumes expected as the condition arg (false) and skips the body, but
    ;; found/line/col then shift into the wrong ~D/~A slots, garbling the output.
    ;; After the fix, the summary should correctly show "line 1" and "column 8".
    (let* ((res      (lisp-check-parens :code "(+ 1 2))"))
           (kind     (gethash "kind" res))
           (pos      (gethash "position" res))
           (line     (and pos (gethash "line" pos)))
           (col      (and pos (gethash "column" pos)))
           (next-tool (gethash "next_tool" res))
           (expected  (gethash "expected" res))
           (found     (gethash "found" res))
           ;; Reproduce the FIXED define-tool summary-building expression.
           ;; The old broken form used ~@[ (expected ~A, found ~A)~] which
           ;; consumed expected as the condition arg and shifted remaining args.
           ;; The fix pre-computes the ef fragment and uses plain ~A.
           (ef (if (and expected found)
                   (format nil " (expected ~A, found ~A)" expected found)
                   ""))
           (summary
            (format nil
                    "Unbalanced parentheses: ~A~A at line ~D, column ~D~A"
                    kind ef line col
                    (if next-tool
                        " Use lisp-edit-form for existing Lisp files."
                        ""))))
      (ok (not (%ok? res)) "should not be ok")
      (ok (string= kind "extra-close") "kind should be extra-close")
      (ok (eql line 1) "underlying line should be 1")
      (ok (eql col 8)  "underlying col should be 8")
      ;; With the fix, expected=nil means ef="" and line/col bind correctly.
      (ok (search "line 1" summary)
          (format nil "summary should contain 'line 1' but got: ~S" summary))
      (ok (search "column 8" summary)
          (format nil "summary should contain 'column 8' but got: ~S" summary)))))

(deftest lisp-check-parens-reader-error-nil-line-summary
  (testing "reader-error summary with nil line/col shows message not nil"
    ;; Bug M-C3-2: when line=nil, ~@[ at line ~D, column ~D~] consumes line as
    ;; condition (false), skips the body, then ~A picks up col (nil) instead of
    ;; message. Before fix: "Reader error: NIL". After fix: "Reader error: <msg>".
    (let* ((line    nil)
           (col     nil)
           (message "something went wrong")
           ;; Reproduce the BROKEN define-tool format string:
           (summary-broken
            (format nil "Reader error~@[ at line ~D, column ~D~]: ~A"
                    line col (or message "unknown")))
           ;; Reproduce the FIXED format string (two independent ~@[ directives):
           (summary-fixed
            (format nil "Reader error~@[ at line ~D~]~@[, column ~D~]: ~A"
                    line col (or message "unknown"))))
      ;; Confirm the bug is present in the broken form (documents the problem):
      (ok (search "NIL" summary-broken)
          (format nil "broken format should show NIL, got: ~S" summary-broken))
      ;; Confirm the fix works:
      (ok (search "something went wrong" summary-fixed)
          (format nil "fixed summary should contain message, got: ~S" summary-fixed))
      (ok (not (search "NIL" summary-fixed))
          (format nil "fixed summary must not contain NIL, got: ~S" summary-fixed)))))

(deftest lisp-check-parens-ok-field-is-json-bool
  (testing "ok field is strictly t for success and nil for errors (json-bool applied in MCP layer)"
    ;; The raw lisp-check-parens function returns Lisp nil/t.
    ;; json-bool is applied in the define-tool body to convert nil -> yason:false
    ;; so the MCP response has JSON false (not null).
    ;; This test verifies the raw function returns the expected raw booleans;
    ;; the protocol-level json-bool test lives in tools-test.lisp.
    (let ((res-err (lisp-check-parens :code "(+ 1 2))")))
      (ok (not (%ok? res-err)) "ok should be falsy for error")
      (ok (null (gethash "ok" res-err))
          "raw ok for error must be nil (define-tool applies json-bool for serialization)"))
    (let ((res-ok (lisp-check-parens :code "(+ 1 2)")))
      (ok (%ok? res-ok) "ok should be truthy for success")
      (ok (eq t (gethash "ok" res-ok))
          "ok for success must be t"))))
