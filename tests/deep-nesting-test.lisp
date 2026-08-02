;;;; tests/deep-nesting-test.lisp
;;;;
;;;; 2026-08-01 監査の Critical の回帰テスト。
;;;; 深さ 20,000 のネストが接続スレッドを永久停止させた。

(defpackage #:cl-mcp/tests/deep-nesting-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens)
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:*max-nesting-depth*))

(in-package #:cl-mcp/tests/deep-nesting-test)

(defun nested-source (depth)
  "DEPTH 重にネストした、構文的に正しいフォームの文字列を返す。"
  (concatenate 'string
               (make-string depth :initial-element #\()
               ":deep"
               (make-string depth :initial-element #\))))

(deftest check-parens-accepts-depth-at-the-limit
  (testing "exactly the limit is still accepted"
    (let ((res (lisp-check-parens :code (nested-source *max-nesting-depth*))))
      (ok (eq t (gethash "ok" res))
          "a form at the limit must not be rejected"))))

(deftest check-parens-rejects-depth-over-the-limit
  (testing "one past the limit is rejected as too-deep, not as a hang"
    (let ((res (lisp-check-parens :code (nested-source (1+ *max-nesting-depth*)))))
      (ok (null (gethash "ok" res)))
      (ok (string= "too-deep" (gethash "kind" res))))))

(deftest check-parens-survives-the-audit-reproduction
  (testing "depth 20000 returns an error instead of exhausting the stack"
    (let ((res (lisp-check-parens :code (nested-source 20000))))
      (ok (string= "too-deep" (gethash "kind" res))))))

(deftest check-parens-does-not-count-string-parens
  (testing "a long string literal of open parens is not too deep"
    (let* ((source (format nil "(f ~S)" (make-string 30000 :initial-element #\()))
           (res (lisp-check-parens :code source)))
      (ok (eq t (gethash "ok" res))
          "parens inside a string literal must not trip the depth limit"))))
