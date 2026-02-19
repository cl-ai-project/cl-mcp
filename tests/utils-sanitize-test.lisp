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

(deftest sanitize-for-json-empty-string
  (ok (string= "" (sanitize-for-json ""))))
