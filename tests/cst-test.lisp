;;;; tests/cst-test.lisp

(defpackage #:cl-mcp/tests/cst-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node-kind
                #:cst-node-start-line
                #:cst-node-end-line
                #:cst-node-value))

(in-package #:cl-mcp/tests/cst-test)

(deftest parse-top-level-forms-captures-comments-and-lines
  (testing "comment nodes are preserved with accurate line numbers"
    (let* ((text (format nil ";; heading~%~%(defun sample ()~%  :ok)~%"))
           (nodes (parse-top-level-forms text))
           (comment (find-if (lambda (n) (eq (cst-node-kind n) :skipped)) nodes))
           (expr (find-if (lambda (n) (eq (cst-node-kind n) :expr)) nodes)))
      (ok comment)
      (ok expr)
      (ok (= 1 (cst-node-start-line comment)))
      (ok (= 3 (cst-node-start-line expr)))
      (ok (<= (cst-node-end-line expr) 4))
      (ok (string= "sample" (string-downcase (second (cst-node-value expr))))))))

(deftest parse-top-level-forms-error-suggests-readtable
  (testing "reader error message suggests using readtable parameter"
    (let ((error-message
            (handler-case
                (progn
                  (parse-top-level-forms "(defun foo () #?\"test\")")
                  nil)
              (error (e)
                (format nil "~A" e)))))
      (ok error-message)
      (ok (search "readtable" error-message))
      (ok (search "interpol-syntax" error-message)))))

(deftest parse-top-level-forms-read-eval-error-message
  (testing "read-time evaluation error shows security message, not readtable suggestion"
    (let ((error-message
            (handler-case
                (progn
                  (parse-top-level-forms "#.(+ 1 2)")
                  nil)
              (error (e)
                (format nil "~A" e)))))
      (ok error-message)
      (ok (search "security" error-message))
      (ok (search "#." error-message))
      (ok (null (search "readtable" error-message))))))
