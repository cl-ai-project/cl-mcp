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
