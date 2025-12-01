;;;; tests/lisp-read-file-test.lisp

(defpackage #:cl-mcp/tests/lisp-read-file-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file))

(in-package #:cl-mcp/tests/lisp-read-file-test)

(deftest lisp-read-file-collapses-lisp
  (testing "collapses Lisp definitions and reports meta"
    (let* ((result (lisp-read-file "src/core.lisp"))
           (content (gethash "content" result))
           (meta (gethash "meta" result)))
      (ok (string= (gethash "mode" result) "lisp-collapsed"))
      (ok (stringp content))
      (ok (search "(defun version () ..." content))
      (ok (hash-table-p meta))
      (ok (>= (gethash "total_forms" meta) 3))
      (ok (>= (gethash "expanded_forms" meta) 1)))))

(deftest lisp-read-file-expands-name-pattern
  (testing "expands matching form when name-pattern is provided"
    (let* ((result (lisp-read-file "src/core.lisp" :name-pattern "version"))
           (content (gethash "content" result)))
      (ok (stringp content))
      (ok (search "+server-version+" content))
      (ok (not (search "(defun version () ...)" content))))))

(deftest lisp-read-file-raw-text-mode
  (testing "raw mode slices text by offset and limit"
    (let* ((result (lisp-read-file "codex-todo.md" :collapsed nil :offset 0 :limit 3))
           (meta (gethash "meta" result)))
      (ok (string= (gethash "mode" result) "raw"))
      (ok (hash-table-p meta))
      (ok (gethash "truncated" meta))
      (ok (>= (gethash "total_lines" meta) 3)))))

(deftest lisp-read-file-content-pattern
  (testing "content-pattern filters non-Lisp text with context"
    (let* ((result (lisp-read-file "codex-todo.md" :content-pattern "lisp-read-file"))
           (content (gethash "content" result)))
      (ok (string= (gethash "mode" result) "text-filtered"))
      (ok (stringp content))
      (ok (search "lisp-read-file" content)))))
