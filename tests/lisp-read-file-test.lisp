;;;; tests/lisp-read-file-test.lisp

(defpackage #:cl-mcp/tests/lisp-read-file-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file
                #:fs-resolve-read-path)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:ensure-directory-pathname))

(in-package #:cl-mcp/tests/lisp-read-file-test)

(setf cl-mcp/src/project-root:*project-root*
      (uiop:ensure-directory-pathname (system-source-directory :cl-mcp)))

(defun with-temp-lisp-file (relative content thunk)
  "Create RELATIVE Lisp file with CONTENT, run THUNK, and clean up."
  (fs-write-file relative content)
  (unwind-protect
       (funcall thunk relative)
    (ignore-errors (delete-file (fs-resolve-read-path relative)))))

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
      (ok (>= (gethash "expanded_forms" meta) 1))
      (ok (gethash "comment_lines" meta))
      (ok (gethash "blank_lines" meta))
      (ok (gethash "source_lines" meta)))))

(deftest lisp-read-file-expands-name-pattern
  (testing "expands matching form when name-pattern is provided"
    (let* ((result (lisp-read-file "src/core.lisp" :name-pattern "version"))
           (content (gethash "content" result)))
      (ok (stringp content))
      (ok (search "+server-version+" content))
      (ok (search ": (defun version" content))
      (ok (not (search "(defun version () ...)" content))))))

(deftest lisp-read-file-raw-text-mode
  (testing "raw mode slices text by offset and limit"
    (let* ((result (lisp-read-file "README.md" :collapsed nil :offset 0 :limit 3))
           (meta (gethash "meta" result)))
      (ok (string= (gethash "mode" result) "raw"))
      (ok (hash-table-p meta))
      (ok (gethash "truncated" meta))
      (ok (>= (gethash "total_lines" meta) 3)))))

(deftest lisp-read-file-content-pattern
  (testing "content-pattern filters non-Lisp text with context"
    (let* ((result (lisp-read-file "README.md" :content-pattern "lisp-read-file"))
           (content (gethash "content" result)))
      (ok (string= (gethash "mode" result) "text-filtered"))
      (ok (stringp content))
      (ok (search "lisp-read-file" content)))))

(deftest lisp-read-file-includes-preceding-comments
  (testing "include-comments surfaces leading comments in collapsed output"
    (with-temp-lisp-file "tests/tmp/lisp-read-comments.lisp"
        (format nil ";; helper comment~%(defun foo ()~%  :ok)~%")
      (lambda (path)
        (let* ((result (lisp-read-file path :include-comments t))
               (content (gethash "content" result)))
          (ok (search ";; helper comment" content))
          (ok (search "(defun foo" content)))))))

(deftest lisp-read-file-comment-context-all
  (testing "comment-context=all preserves comment positions"
    (with-temp-lisp-file "tests/tmp/lisp-read-comments-all.lisp"
        (format nil "(defun first () :one)~%;; between~%(defun second () :two)~%")
      (lambda (path)
        (let* ((content (gethash "content"
                                 (lisp-read-file path :include-comments t
                                                 :comment-context "all")))
               (first-pos (search "(defun first" content))
               (comment-pos (search ";; between" content))
               (second-pos (search "(defun second" content)))
          (ok first-pos)
          (ok comment-pos)
          (ok second-pos)
          (ok (< first-pos comment-pos))
          (ok (< comment-pos second-pos)))))))

(deftest lisp-read-file-expanded-lines-are-numbered
  (testing "expanded forms include line numbers"
    (with-temp-lisp-file "tests/tmp/lisp-read-line-numbers.lisp"
        (format nil "(defun target ()~%  :ok)~%")
      (lambda (path)
        (let* ((content (gethash "content"
                                 (lisp-read-file path :name-pattern "target")))
               (line (with-input-from-string (s content)
                       (read-line s nil "")))
               (colon (position #\: line)))
          (ok colon)
          (ok (every #'digit-char-p (subseq line 0 colon)))
          (ok (search "(defun target" line)))))))
