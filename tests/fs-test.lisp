;;;; tests/fs-test.lisp

(defpackage #:cl-mcp/tests/fs-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory))

(in-package #:cl-mcp/tests/fs-test)

(deftest fs-read-file-project
  (testing "fs-read-file reads project file with content"
    (let ((txt (fs-read-file "src/core.lisp" :offset 0 :limit 40)))
      (ok (stringp txt))
      (ok (> (length txt) 0)))))

(deftest fs-write-file-project
  (testing "fs-write-file writes under project root"
    (let* ((rel "tests/tmp-fs-write.txt")
           (content "hello world\n"))
      (unwind-protect
           (progn
             (ok (fs-write-file rel content))
             (let ((read (fs-read-file rel)))
               (ok (string= read content))))
        (ignore-errors (delete-file rel))))))

(deftest fs-list-directory-project
  (testing "fs-list-directory lists entries and filters hidden"
    (let* ((entries (fs-list-directory "."))
           (names (map 'list (lambda (h) (gethash "name" h)) entries)))
      (ok (find "src" names :test #'string=))
      (ok (not (find ".git" names :test #'string=))))))

(deftest fs-list-directory-includes-files
  (testing "fs-list-directory returns files with type metadata"
    (let* ((entries (fs-list-directory "src/"))
           (core (find "core.lisp" entries :key (lambda (h) (gethash "name" h))
                                       :test #'string=)))
      (ok core)
      (ok (string= "file" (gethash "type" core))))))

(deftest fs-read-file-respects-limit-and-offset
  (testing "limit and offset trim content"
    (let ((txt (fs-read-file "src/core.lisp" :offset 1 :limit 5)))
      (ok (= (length txt) 5)))))

(deftest fs-read-file-rejects-negative-offset
  (testing "negative offset signals error"
    (ok (handler-case (progn (fs-read-file "src/core.lisp" :offset -1) nil)
          (error () t)))))

(deftest fs-read-file-rejects-huge-limit
  (testing "limit over max signals error"
    (let ((max cl-mcp/src/fs::*fs-read-max-bytes*))
      (ok (handler-case (progn (fs-read-file "src/core.lisp" :limit (1+ max)) nil)
            (error () t))))))

(deftest fs-write-file-prevents-traversal
  (testing "writing outside project root is rejected"
    (ok (handler-case (progn (fs-write-file "../outside.txt" "nope") nil)
          (error () t)))))
