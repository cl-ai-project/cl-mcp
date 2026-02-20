;;;; tests/utils-sanitize-test.lisp

(defpackage #:cl-mcp/tests/utils-sanitize-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/sanitize #:sanitize-for-json))

(in-package #:cl-mcp/tests/utils-sanitize-test)

(deftest sanitize-for-json-nil-returns-nil
  (ok (null (sanitize-for-json nil))))

(deftest sanitize-for-json-clean-string-passes-through
  (ok (string= "hello world" (sanitize-for-json "hello world"))))

(deftest sanitize-for-json-strips-control-chars
  (let ((bel (code-char 7))
        (bs (code-char 8))
        (nul (code-char 0)))
    (ok (string= "abc"
                  (sanitize-for-json
                   (format nil "a~Cb~Cc" bel bs))))
    (ok (string= "start"
                  (sanitize-for-json
                   (format nil "~Cstart" nul))))))

(deftest sanitize-for-json-preserves-whitespace
  (ok (string= (format nil "a~Cb~Cc" #\Tab #\Newline #\Return)
               (sanitize-for-json
                (format nil "a~Cb~Cc" #\Tab #\Newline #\Return)))))

(deftest sanitize-for-json-strips-del
  (ok (string= "ab"
               (sanitize-for-json
                (format nil "a~Cb" (code-char 127))))))

(deftest sanitize-for-json-strips-ansi-escape-sequences
  (let ((esc (code-char 27)))
    (ok (string= "red text"
                  (sanitize-for-json
                   (format nil "~C[31mred text~C[0m" esc esc))))
    (ok (string= "bold"
                  (sanitize-for-json
                   (format nil "~C[1mbold~C[22m" esc esc))))))

(deftest sanitize-for-json-mixed-content
  (let ((esc (code-char 27))
        (bel (code-char 7))
        (del (code-char 127)))
    ;; ANSI escape stripped, BEL stripped, DEL stripped, newline preserved
    (ok (string= (format nil "hello world~C" #\Newline)
                  (sanitize-for-json
                   (format nil "~C[31mhello~C world~C~C" esc bel del #\Newline))))
    (ok (string= (format nil "hello world~C" #\Newline)
                  (sanitize-for-json
                   (format nil "hello world~C" #\Newline))))))

(deftest sanitize-for-json-strips-csi-with-tilde-terminator
  (testing "CSI sequences ending with ~ (e.g. bracketed paste) are fully stripped"
    (let ((esc (code-char 27)))
      ;; ESC[200~ is bracketed paste mode start
      (ok (string= "pasted text"
                    (sanitize-for-json
                     (format nil "~C[200~~pasted text~C[201~~" esc esc))))
      ;; ESC[2~ is Insert key
      (ok (string= "after"
                    (sanitize-for-json
                     (format nil "~C[2~~after" esc))))
      ;; ESC[1;5C is Ctrl+Right (C is alpha, was already handled)
      (ok (string= "word"
                    (sanitize-for-json
                     (format nil "~C[1;5Cword" esc)))))))

(deftest sanitize-for-json-empty-string
  (ok (string= "" (sanitize-for-json ""))))

(deftest sanitize-for-json-preserves-non-ascii-after-csi
  (testing "Non-ASCII characters after partial CSI sequences are preserved"
    (let ((esc (code-char 27)))
      ;; Japanese text after a CSI sequence should survive
      (ok (string= "hello world"
                    (sanitize-for-json
                     (format nil "~C[31mhello~C[0m world" esc esc))))
      ;; Standalone non-ASCII text is never consumed
      (ok (string= "日本語テスト"
                    (sanitize-for-json "日本語テスト")))
      ;; Non-ASCII immediately after ESC[ should not be consumed as CSI parameter
      (ok (search "日本語"
                  (sanitize-for-json
                   (format nil "~C[日本語text" esc)))))))

(deftest sanitize-for-json-handles-non-string-input
  (testing "Integer input is converted to its string representation"
    (ok (string= "42" (sanitize-for-json 42))))
  (testing "Symbol input is converted to its string representation"
    (ok (stringp (sanitize-for-json :hello))))
  (testing "Float input is converted to its string representation"
    (ok (stringp (sanitize-for-json 3.14)))))

(deftest sanitize-for-json-strips-osc-sequence
  (testing "OSC sequence (ESC ] ... BEL) is fully stripped"
    (let ((esc (code-char 27))
          (bel (code-char 7)))
      (ok (string= "before after"
                    (sanitize-for-json
                     (format nil "before~C]0;window title~C after" esc bel))))))
  (testing "OSC sequence terminated by ST (ESC \\) is fully stripped"
    (let ((esc (code-char 27)))
      (ok (string= "before after"
                    (sanitize-for-json
                     (format nil "before~C]0;title~C\\ after" esc esc)))))))

(deftest sanitize-for-json-strips-ss3-sequence
  (testing "SS3 sequence (ESC O) is stripped as 2-byte sequence"
    (let ((esc (code-char 27)))
      ;; ESC O P is F1 key in SS3 mode
      (ok (string= "P after"
                    (sanitize-for-json
                     (format nil "~COP after" esc))))
      ;; ESC N is SS2
      (ok (string= "x after"
                    (sanitize-for-json
                     (format nil "~CNx after" esc)))))))

(deftest sanitize-for-json-strips-dcs-sequence
  (testing "DCS sequence (ESC P ... ST) is fully stripped"
    (let ((esc (code-char 27)))
      (ok (string= "before after"
                    (sanitize-for-json
                     (format nil "before~CP1$r0m~C\\ after" esc esc)))))))

(deftest sanitize-for-json-preserves-non-ascii-after-esc
  (testing "Non-ASCII byte after bare ESC is preserved"
    (let ((esc (code-char 27)))
      ;; ESC followed by a non-ASCII char — ESC is stripped, char preserved
      (ok (search "日本語"
                  (sanitize-for-json
                   (format nil "~C日本語" esc)))))))

(deftest sanitize-for-json-strips-bare-esc-at-end
  (testing "Bare ESC at end of string is stripped"
    (let ((esc (code-char 27)))
      (ok (string= "text" (sanitize-for-json (format nil "text~C" esc)))))))

(deftest sanitize-for-json-handles-unterminated-sequences
  (testing "Unterminated OSC consumes to end of string"
    (let ((esc (code-char 27)))
      (ok (string= "before"
                    (sanitize-for-json
                     (format nil "before~C]0;title with no terminator" esc))))))
  (testing "Unterminated DCS consumes to end of string"
    (let ((esc (code-char 27)))
      (ok (string= "before"
                    (sanitize-for-json
                     (format nil "before~CPunterminated dcs" esc)))))))
