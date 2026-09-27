;;;; tests/nesting-test.lisp

(defpackage #:cl-mcp/tests/nesting-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/nesting
                #:json-too-deep-p
                #:lisp-too-deep-p
                #:check-lisp-nesting
                #:+max-lisp-nesting+
                #:+max-json-nesting+)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens))

(in-package #:cl-mcp/tests/nesting-test)

(defun %opens (n &optional (char #\())
  (make-string n :initial-element char))

(deftest lisp-depth-counts-only-code-parens
  (testing "the limit itself is allowed and one more is not"
    (ok (not (lisp-too-deep-p (%opens +max-lisp-nesting+))))
    (ok (lisp-too-deep-p (%opens (1+ +max-lisp-nesting+)))))
  (testing "parens that are data do not count"
    (ok (not (lisp-too-deep-p (format nil "\"~A\"" (%opens 900)))) "in a string")
    (ok (not (lisp-too-deep-p (format nil "|~A|" (%opens 900)))) "in a |symbol|")
    (ok (not (lisp-too-deep-p (format nil "; ~A~%x" (%opens 900)))) "in a line comment")
    (ok (not (lisp-too-deep-p (format nil "#| #| |# ~A |# x" (%opens 900))))
        "in a nested block comment")
    (ok (not (lisp-too-deep-p (with-output-to-string (s)
                                (dotimes (i 900) (write-string "#\\( " s)))))
        "as character names")
    (ok (not (lisp-too-deep-p (with-output-to-string (s)
                                (dotimes (i 900) (write-string "\\(" s)))))
        "after a single escape"))
  (testing "a closed list gives its depth back"
    (ok (not (lisp-too-deep-p (with-output-to-string (s)
                                (dotimes (i 900) (write-string "(a)" s))))))))

(deftest check-lisp-nesting-refuses-with-an-error
  (ok (string= "(a)" (check-lisp-nesting "(a)")))
  (ok (handler-case (progn (check-lisp-nesting (%opens 20000)) nil)
        (error (e) (search "levels deep" (princ-to-string e))))))

(deftest json-depth-counts-only-structure
  (ok (not (json-too-deep-p (%opens +max-json-nesting+ #\[))))
  (ok (json-too-deep-p (%opens (1+ +max-json-nesting+) #\{)))
  (ok (not (json-too-deep-p (format nil "[\"\\\"~A\"]" (%opens 5000 #\[))))
      "brackets inside a string, after an escaped quote, are data"))

(deftest lisp-check-parens-reports-deep-code-instead-of-reading-it
  (let ((result (lisp-check-parens
                 :code (concatenate 'string (%opens 20000) (%opens 20000 #\))))))
    (ok (hash-table-p result) "an answer, not a stopped thread")))
