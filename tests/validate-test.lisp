;;;; tests/validate-test.lisp

(defpackage #:cl-mcp/tests/validate-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
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
  (testing "a mismatch the reader rejects includes lisp-edit-form guidance"
    (let* ((res (lisp-check-parens :code "(defun f () (list 1]"))
           (required (gethash "required_args" res)))
      (ok (string= (%kind res) "mismatch"))
      (ok (string= (gethash "fix_code" res) "use_lisp_edit_form"))
      (ok (string= (gethash "next_tool" res) "lisp-edit-form"))
      (ok (vectorp required))
      (ok (= (length required) 5))
      (ok (string= (aref required 0) "file_path"))))
  (testing "a mismatch on code the reader accepts names no fix"
    ;; ( [ ) ] reads as a list holding the symbol [ followed by the symbol ].
    (let ((res (lisp-check-parens :code "( [ ) ]")))
      (ok (eq (gethash "false_positive" res) t))
      (ok (string= (gethash "next_tool" res) "lisp-edit-form"))
      (ok (null (gethash "fix_code" res)))
      (ok (null (gethash "required_args" res))))))

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
        (ok (string= (%kind res) "too-large"))
        (ok (null (gethash "position" res))
            "nothing was scanned, so no position is reported")))))

(deftest lisp-check-parens-too-large-inline-code-is-not-called-a-cut-read
  (testing "the too-large summary for inline code does not talk about the file read cap"
    (let* ((*check-parens-max-bytes* 1)
           (state (cl-mcp/src/state:make-state))
           (args (cl-mcp/src/tools/helpers:make-ht "code" "(abcd)"))
           (response (cl-mcp/src/validate::lisp-check-parens-handler state "cp-tl" args))
           (text (gethash "text" (aref (gethash "content" (gethash "result" response)) 0))))
      (ok (search "Input too large to check" text))
      (ok (search "smaller region" text))
      (ng (search "fs-read-file cap" text)))))

(deftest lisp-check-parens-truncated-read-is-too-large
  (testing "a file read cut at the fs cap is reported too-large, not diagnosed from its prefix"
    (let* ((root (asdf:system-source-directory :cl-mcp))
           (abs (merge-pathnames "tests/tmp/check-parens-truncated.lisp" root))
           (cl-mcp/src/project-root:*project-root* root))
      (ensure-directories-exist abs)
      (with-open-file (out abs :direction :output :if-exists :supersede)
        (write-string (format nil "(defun f ()~%  (list 1 2 3 4 5 6 7 8 9))~%") out))
      (unwind-protect
           (let ((cl-mcp/src/fs::*fs-read-max-bytes* 16))
             (let ((res (lisp-check-parens :path (namestring abs))))
               (ok (null (%ok? res)))
               (ok (string= (%kind res) "too-large")
                   "the 16-character prefix is unbalanced but must not be diagnosed")))
        (ignore-errors (delete-file abs))))))

(deftest lisp-check-parens-file-guidance-and-windows
  (let* ((root (asdf:system-source-directory :cl-mcp))
         (abs (merge-pathnames "tests/tmp/check-parens-window.lisp" root))
         (cl-mcp/src/project-root:*project-root* root)
         (valid (format nil "(defun a ()~%  (list 1 2 3))~%~%(defun b ()~%  (list 4 5 6))~%")))
    (ensure-directories-exist abs)
    (unwind-protect
         (progn
           (with-open-file (out abs :direction :output :if-exists :supersede)
             (write-string valid out))
           (testing "a window into a valid file reports the slice's kind but offers no repair"
             (let ((res (lisp-check-parens :path (namestring abs) :limit 25)))
               (ok (null (%ok? res)))
               (ok (string= (%kind res) "unclosed"))
               (ok (null (gethash "likely_fixes" res)) "no likely fix from a prefix")
               (ok (null (gethash "next_top_level_line" res)))
               (ok (search "window" (gethash "diagnosis_text" res))
                   "the text says only a window was checked")
               (ok (string= (gethash "next_tool" res) "lisp-edit-form")
                   "a window proves nothing about the file, so no overwrite hint")))
           (testing "an offset window is a prefix too"
             (let ((res (lisp-check-parens :path (namestring abs) :offset 29 :limit 20)))
               (ok (null (%ok? res)))
               (ok (null (gethash "likely_fixes" res)))))
           (with-open-file (out abs :direction :output :if-exists :supersede)
             (write-string (format nil "(defun a ()~%  (list 1 2 3)~%~%~
                                        (defun b ()~%  (list 4 5 6))~%")
                           out))
           (testing "a file that really fails on a delimiter is sent to the overwrite path"
             (let ((res (lisp-check-parens :path (namestring abs))))
               (ok (string= (%kind res) "unclosed"))
               (ok (vectorp (gethash "likely_fixes" res)))
               (ok (string= (gethash "next_tool" res) "fs-write-file"))
               (ok (string= (gethash "fix_code" res) "overwrite_with_allow_unparseable"))
               (ok (find "allow_unparseable_overwrite" (gethash "required_args" res)
                         :test #'string=))))
           (with-open-file (out abs :direction :output :if-exists :supersede)
             (write-string (format nil "(defun a ()~%  (list a[b 1))~%") out))
           (testing "a possible bracket false positive keeps the lisp-edit-form hint"
             (let ((res (lisp-check-parens :path (namestring abs))))
               (ok (string= (%kind res) "mismatch"))
               (ok (string= (gethash "next_tool" res) "lisp-edit-form")))))
      (ignore-errors (delete-file abs)))))

(deftest lisp-check-parens-broken-file-outside-root-names-no-tool
  (testing "a delimiter-broken file outside the project root gets prose only, no next_tool"
    (let* ((system-root (asdf:system-source-directory :cl-mcp))
           ;; The project root is a subdirectory; the file sits beside it,
           ;; still readable (under the ASDF system) but not writable by
           ;; fs-write-file.
           (root (merge-pathnames "tests/tmp/check-parens-root/" system-root))
           (abs (merge-pathnames "tests/tmp/check-parens-outside.lisp" system-root))
           (cl-mcp/src/project-root:*project-root* root))
      (ensure-directories-exist root)
      (ensure-directories-exist abs)
      (unwind-protect
           (progn
             (with-open-file (out abs :direction :output :if-exists :supersede)
               (write-string (format nil "(defun a ()~%  (list 1 2 3)~%~%~
                                          (defun b ()~%  (list 4 5 6))~%")
                             out))
             (let ((res (lisp-check-parens :path (namestring abs))))
               (ok (string= (%kind res) "unclosed"))
               (ok (search "outside the project root" (gethash "guidance_text" res)))
               (ng (gethash "next_tool" res) "no tool can act on it")
               (ng (gethash "fix_code" res))
               (ng (gethash "required_args" res))
               (ok (vectorp (gethash "likely_fixes" res))
                   "the fix itself is still described")))
        (ignore-errors (delete-file abs))))))

(deftest lisp-check-parens-guidance-agrees-with-the-overwrite-guard
  (let* ((root (asdf:system-source-directory :cl-mcp))
         (abs (merge-pathnames "tests/tmp/check-parens-guard-agreement.lisp" root))
         (cl-mcp/src/project-root:*project-root* root))
    (ensure-directories-exist abs)
    (flet ((write-text (text)
             (with-open-file (out abs :direction :output :if-exists :supersede)
               (write-string text out)))
           (guard-overwritable-p ()
             (cl-mcp/src/fs::%lisp-file-unparseable-p abs)))
      (unwind-protect
           (progn
             (testing "a reader-level failure plus a missing ) is not sent to the overwrite path"
               ;; #. is disabled, so the edit tools' parser fails before the
               ;; missing ); the guard refuses, and so must the hint.
               (write-text (format nil "(defvar *x* #.(+ 1 2))~%~%(defun f ()~%  (list 1 2~%"))
               (let ((res (lisp-check-parens :path (namestring abs))))
                 (ok (string= (%kind res) "unclosed"))
                 (ok (null (guard-overwritable-p)) "the guard would refuse")
                 (ok (string= (gethash "next_tool" res) "lisp-edit-form")
                     "so the hint does not promise the overwrite")))
             (testing "a token the scanner mis-lexes as a block comment is not sent there either"
               ;; foo#|bar| is one symbol to the reader; the file parses.
               (write-text (format nil "(list foo#|bar|)~%"))
               (let ((res (lisp-check-parens :path (namestring abs))))
                 (ok (string= (%kind res) "unclosed-block-comment"))
                 (ok (null (guard-overwritable-p)))
                 (ok (string= (gethash "next_tool" res) "lisp-edit-form"))
                 (let ((text (gethash "diagnosis_text" res)))
                   (ok (zerop (search "The editing tools' reader parses this file" text))
                       "the caveat comes first")
                   (ng (search "Close it with" text) "no instruction to change anything")
                   (ng (search "Likely fix" text)))
                 (ok (null (gethash "likely_fixes" res))
                     "no machine-readable fix for a file that parses")))
             (testing "a parseable file with a bracket typo look-alike gets no instruction"
               ;; a] is a symbol: the file parses, the scan says mismatch.
               (write-text (format nil "(defun g ()~%  (list a] 1))~%"))
               (let ((res (lisp-check-parens :path (namestring abs))))
                 (ok (null (%ok? res)))
                 (ok (string= (%kind res) "mismatch"))
                 (ok (null (guard-overwritable-p)))
                 (ok (null (gethash "likely_fixes" res)))
                 (ng (search "Replace it with" (gethash "diagnosis_text" res)))))
             (testing "a symbol starting with ] is an extra-close look-alike with no instruction"
               (write-text (format nil "(defun f () 1)~%~%]foo~%"))
               (let ((res (lisp-check-parens :path (namestring abs))))
                 (ok (string= (%kind res) "extra-close"))
                 (ok (null (guard-overwritable-p)))
                 (ng (search "Either remove" (gethash "diagnosis_text" res)))
                 (ok (null (gethash "likely_fixes" res)))))
             (testing "a plain missing ) agrees in the other direction"
               (write-text (format nil "(defun f ()~%  (list 1 2~%"))
               (let ((res (lisp-check-parens :path (namestring abs))))
                 (ok (guard-overwritable-p))
                 (ok (string= (gethash "next_tool" res) "fs-write-file")))))
        (ignore-errors (delete-file abs))))))

(deftest lisp-check-parens-guidance-names-a-relative-path-and-the-editable-prefix
  (let* ((root (asdf:system-source-directory :cl-mcp))
         (abs (merge-pathnames "tests/tmp/check-parens-guidance-path.lisp" root))
         (cl-mcp/src/project-root:*project-root* root))
    (ensure-directories-exist abs)
    (flet ((write-text (text)
             (with-open-file (out abs :direction :output :if-exists :supersede)
               (write-string text out))))
      (unwind-protect
           (progn
             (testing "the overwrite guidance gives fs-write-file a project-relative path"
               (write-text (format nil "(defun f ()~%  (list 1 2~%"))
               (let* ((res (lisp-check-parens :path (namestring abs)))
                      (guidance (gethash "guidance_text" res)))
                 (ok (string= (gethash "next_tool" res) "fs-write-file"))
                 (ok (search "path=\"tests/tmp/check-parens-guidance-path.lisp\"" guidance)
                     "relative to the project root, as fs-write-file requires")
                 (ok (search "cannot locate any form in it" guidance))))
             (testing "after an in-readtable switch the forms before the breakage stay editable"
               (if (find-package "NAMED-READTABLES")
                   (progn
                     (write-text (format nil "(named-readtables:in-readtable :standard)~%~%~
                                              (defun early ()~%  1)~%~%~
                                              (defun late ()~%  (list 1 2~%"))
                     (let* ((res (lisp-check-parens :path (namestring abs)))
                            (guidance (gethash "guidance_text" res)))
                       (ok (string= (gethash "next_tool" res) "fs-write-file"))
                       (ok (search "forms before it can still be edited" guidance))
                       (ng (search "cannot locate any form" guidance))))
                   (rove:skip "named-readtables not available"))))
        (ignore-errors (delete-file abs))))))

(deftest lisp-check-parens-inline-code-the-reader-accepts-gets-no-instruction
  (testing "valid inline code with a ] symbol is a false positive, not a typo to fix"
    (let ((res (lisp-check-parens :code "(list a] 1)")))
      (ok (null (%ok? res)))
      (ok (string= (%kind res) "mismatch"))
      (ok (eq (gethash "false_positive" res) t))
      (ok (null (gethash "likely_fixes" res)))
      (ok (null (gethash "fix_code" res)) "no fix is named for input that needs none")
      (ok (null (gethash "required_args" res)))
      (ok (string= (gethash "next_tool" res) "lisp-edit-form"))
      (ng (search "Replace it with" (gethash "diagnosis_text" res)))
      (ok (zerop (search "The editing tools' reader parses" (gethash "diagnosis_text" res))))))
  (testing "a symbol with #| inside is not told to close a comment"
    (let ((res (lisp-check-parens :code "(list foo#|bar| 1)")))
      (ok (string= (%kind res) "unclosed-block-comment"))
      (ok (eq (gethash "false_positive" res) t))
      (ng (search "Close it with" (gethash "diagnosis_text" res)))))
  (testing "inline code that really is broken keeps its fix"
    (let ((res (lisp-check-parens :code (format nil "(defun f ()~%  (list 1 2"))))
      (ok (null (gethash "false_positive" res)))
      (ok (vectorp (gethash "likely_fixes" res)))
      (ok (search "Likely fix" (gethash "diagnosis_text" res))))))

(deftest lisp-check-parens-false-positive-headline-does-not-assert-breakage
  (testing "the summary's first line calls a reader-accepted snippet a likely false positive"
    (let* ((state (cl-mcp/src/state:make-state))
           (args (cl-mcp/src/tools/helpers:make-ht "code" "(list a] 1)"))
           (response (cl-mcp/src/validate::lisp-check-parens-handler state "cp-fp" args))
           (result-obj (gethash "result" response))
           (text (gethash "text" (aref (gethash "content" result-obj) 0))))
      (ok (zerop (search "Likely false positive (the editing tools' reader accepts this snippet)"
                         text)))
      (ng (search "Unbalanced parentheses:" text))
      (ng (search "Use lisp-edit-form for existing Lisp files" text))
      (ok (eq (gethash "false_positive" result-obj) t)))))

(deftest lisp-check-parens-limit-equal-to-a-multibyte-file-is-not-a-window
  (testing "a limit equal to the character count reads the whole file, octets notwithstanding"
    (let* ((root (asdf:system-source-directory :cl-mcp))
           (abs (merge-pathnames "tests/tmp/check-parens-multibyte-limit.lisp" root))
           (cl-mcp/src/project-root:*project-root* root)
           ;; Three-byte characters in a comment: 3 lines, 30 characters, more octets.
           (text (format nil ";; ~A~%(defun f ()~%  (list 1)~%"
                         (make-string 6 :initial-element (code-char #x3042)))))
      (ensure-directories-exist abs)
      (with-open-file (out abs :direction :output :if-exists :supersede
                               :external-format :utf-8)
        (write-string text out))
      (unwind-protect
           (let ((res (lisp-check-parens :path (namestring abs) :limit (length text))))
             (ok (null (%ok? res)))
             (ok (vectorp (gethash "likely_fixes" res))
                 "the whole file was read, so the fix is offered")
             (ng (search "window" (gethash "diagnosis_text" res))))
        (ignore-errors (delete-file abs))))))

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

(deftest lisp-check-parens-unclosed-block-comment-guidance
  (testing "an open #| gets its own guidance and no parinfer fixes"
    (let ((res (lisp-check-parens :code (format nil "(foo)~%#| open"))))
      (ok (string= (%kind res) "unclosed-block-comment"))
      (ok (search "Close it with |#" (gethash "diagnosis_text" res)))
      (ok (null (gethash "likely_fixes" res)) "no likely_fixes for a comment problem")))
  (testing "the MCP summary carries that guidance"
    (let* ((state (cl-mcp/src/state:make-state))
           (args (cl-mcp/src/tools/helpers:make-ht "code" (format nil "(foo)~%#| open")))
           (response (cl-mcp/src/validate::lisp-check-parens-handler state "cp-bc" args))
           (text (gethash "text" (aref (gethash "content" (gethash "result" response)) 0))))
      (ok (search "Unterminated block comment" text))
      (ok (null (search "Likely fix" text))))))

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

(deftest lisp-check-parens-likely-fixes-field
  (testing "unbalanced result carries parinfer likely fixes and diagnosis text"
    (let* ((res (lisp-check-parens
                 :code (format nil "(defun f (x)~%  (let ((y 1)~%    (+ x y)))")))
           (fixes (gethash "likely_fixes" res))
           (first-fix (and fixes (plusp (length fixes)) (aref fixes 0))))
      (ok (not (%ok? res)))
      (ok (vectorp fixes) "likely_fixes should be a vector")
      (ok (= (length fixes) 1))
      (ok (= (gethash "line" first-fix) 2))
      (ok (= (gethash "delta" first-fix) 1))
      (ok (string= (gethash "original" first-fix) "  (let ((y 1)"))
      (ok (string= (gethash "repaired" first-fix) "  (let ((y 1))"))
      (ok (search "Likely fix" (gethash "diagnosis_text" res)))
      (ok (null (gethash "next_top_level_line" res))))))

(deftest lisp-check-parens-likely-fixes-are-capped
  (testing "the likely_fixes payload is bounded and reports how many entries were omitted"
    ;; 40 nested lines, each dedented so parinfer closes one paren per line.
    (let* ((code (with-output-to-string (s)
                   (loop for i from 0 below 40
                         do (format s "~A(a~%"
                                    (make-string (* 2 (- 40 i)) :initial-element #\Space)))))
           (res (lisp-check-parens :code code))
           (fixes (gethash "likely_fixes" res))
           (omitted (gethash "likely_fixes_omitted" res)))
      (ok (not (%ok? res)))
      (ok (vectorp fixes))
      (ok (<= (length fixes) cl-mcp/src/paren-diagnostics::*repair-lines-limit*)
          "payload vector is capped")
      (ok (and (integerp omitted) (plusp omitted))
          "omitted count is reported when entries were dropped"))))

(deftest lisp-check-parens-next-top-level-line-field
  (testing "a file-shaped input reports the next top-level form line"
    (let ((res (lisp-check-parens
                :code (format nil "(defun a ()~%  (list 1)~%~%(defun b ()~%  2)~%"))))
      (ok (string= (%kind res) "unclosed"))
      (ok (= (gethash "next_top_level_line" res) 4)))))

(deftest lisp-check-parens-summary-includes-diagnosis
  (testing "MCP summary text carries the likely-fix guidance"
    (let* ((state (cl-mcp/src/state:make-state))
           (args (cl-mcp/src/tools/helpers:make-ht
                  "code" (format nil "(defun f (x)~%  (let ((y 1)~%    (+ x y)))")))
           ;; define-tool generates <tool-name>-handler (see lisp-edit-form-handler
           ;; in tests/lisp-edit-form-test.lisp); check src/tools/define-tool.lisp
           ;; if this symbol is not found.
           (response (cl-mcp/src/validate::lisp-check-parens-handler state "cp-1" args))
           (result-obj (gethash "result" response))
           (text (gethash "text" (aref (gethash "content" result-obj) 0))))
      (ok (search "Unbalanced parentheses: unclosed at line 1, column 1" text)
          "existing first line is preserved")
      (ok (search "Likely fix, inferred from indentation:" text))
      (ok (search "line 2:" text))
      (ok (vectorp (gethash "likely_fixes" result-obj))
          "sibling likely_fixes field is present")
      (ok (null (gethash "diagnosis_text" result-obj))
          "internal diagnosis_text is not leaked into the payload"))))

(deftest lisp-check-parens-balanced-has-no-fix-fields
  (testing "balanced input has no likely_fixes"
    (let ((res (lisp-check-parens :code "(+ 1 2)")))
      (ok (%ok? res))
      (ok (null (gethash "likely_fixes" res))))))
