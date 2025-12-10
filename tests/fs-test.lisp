;;;; tests/fs-test.lisp

(defpackage #:cl-mcp/tests/fs-test
  (:use #:cl #:rove)
  (:import-from #:uiop #:getcwd #:ensure-directory-pathname)
  (:import-from #:asdf #:system-source-directory)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory
                #:fs-resolve-read-path
                #:fs-get-project-info
                #:fs-set-project-root))

(in-package #:cl-mcp/tests/fs-test)

(defmacro with-test-project-root (&body body)
  `(let ((cl-mcp/src/fs:*project-root*
          (or (ignore-errors (ensure-directory-pathname (system-source-directory "cl-mcp")))
              (ensure-directory-pathname (getcwd)))))
     ,@body))

(deftest fs-read-file-project
  (testing "fs-read-file reads project file with content"
    (with-test-project-root
      (let ((txt (fs-read-file "src/core.lisp" :offset 0 :limit 40)))
        (ok (stringp txt))
        (ok (> (length txt) 0))))))

(deftest fs-write-file-project
  (testing "fs-write-file writes under project root"
    (with-test-project-root
      (let* ((rel "tests/tmp-fs-write.txt")
             (content "hello world\n"))
        (unwind-protect
             (progn
               (ok (fs-write-file rel content))
               (let ((read (fs-read-file rel)))
                 (ok (string= read content))))
          (ignore-errors (delete-file rel)))))))

(deftest fs-list-directory-project
  (testing "fs-list-directory lists entries and filters hidden"
    (with-test-project-root
      (let* ((entries (fs-list-directory "."))
             (names (map 'list (lambda (h) (gethash "name" h)) entries)))
        (ok (find "src" names :test #'string=))
        (ok (not (find ".git" names :test #'string=)))))))

(deftest fs-list-directory-includes-files
  (testing "fs-list-directory returns files with type metadata"
    (with-test-project-root
      (let* ((entries (fs-list-directory "src/"))
             (core (find "core.lisp" entries :key (lambda (h) (gethash "name" h))
                                         :test #'string=)))
        (ok core)
        (ok (string= "file" (gethash "type" core)))))))

(deftest fs-read-file-respects-limit-and-offset
  (testing "limit and offset trim content"
    (with-test-project-root
      (let ((txt (fs-read-file "src/core.lisp" :offset 1 :limit 5)))
        (ok (= (length txt) 5))))))

(deftest fs-read-file-rejects-negative-offset
  (testing "negative offset signals error"
    (with-test-project-root
      (ok (handler-case (progn (fs-read-file "src/core.lisp" :offset -1) nil)
            (error () t))))))

(deftest fs-read-file-rejects-huge-limit
  (testing "limit over max signals error"
    (with-test-project-root
      (let ((max cl-mcp/src/fs::*fs-read-max-bytes*))
        (ok (handler-case (progn (fs-read-file "src/core.lisp" :limit (1+ max)) nil)
              (error () t)))))))

(deftest fs-list-directory-error-includes-resolved-path
  (testing "error message shows resolved absolute path"
    (with-test-project-root
      (let* ((rel "no-such-dir-for-test")
             (resolved (namestring (fs-resolve-read-path rel))))
        (ok (handler-case
                 (progn (fs-list-directory rel) nil)
               (error (e)
                 (let ((msg (princ-to-string e)))
                   (and (search rel msg) (search resolved msg))))))))))

(deftest fs-get-project-info-returns-paths
  (testing "fs-get-project-info exposes project root and cwd"
    (with-test-project-root
      (let ((info (fs-get-project-info)))
        (ok (stringp (gethash "project_root" info)))
        (ok (stringp (gethash "cwd" info)))
        (ok (member (gethash "project_root_source" info)
                    '("env" "cwd" "asdf") :test #'string=))))))

(deftest fs-write-file-prevents-traversal
  (testing "writing outside project root is rejected"
    (with-test-project-root
      (ok (handler-case (progn (fs-write-file "../outside.txt" "nope") nil)
            (error () t))))))

(deftest fs-set-project-root-changes-root
  (testing "fs-set-project-root updates project root and cwd"
    (with-test-project-root
      (let* ((original-root cl-mcp/src/fs:*project-root*)
             (original-cwd (getcwd))
             (test-dir (namestring original-root)))
        (unwind-protect
             (let ((result (fs-set-project-root test-dir)))
               (ok (hash-table-p result))
               (ok (stringp (gethash "project_root" result)))
               (ok (stringp (gethash "cwd" result)))
               (ok (stringp (gethash "previous_root" result)))
               (ok (stringp (gethash "status" result)))
               (ok (string= (gethash "project_root" result) test-dir))
               (ok (string= (gethash "cwd" result) test-dir)))
          ;; Restore original state
          (setf cl-mcp/src/fs:*project-root* original-root)
          (ignore-errors (uiop:chdir original-cwd)))))))

(deftest fs-set-project-root-validates-directory
  (testing "fs-set-project-root rejects non-existent directory"
    (with-test-project-root
      (ok (handler-case
               (progn (fs-set-project-root "/nonexistent/directory/path") nil)
             (error () t))))))

(deftest fs-set-project-root-validates-string
  (testing "fs-set-project-root rejects non-string argument"
    (with-test-project-root
      (ok (handler-case
               (progn (fs-set-project-root 123) nil)
             (error () t))))))

(deftest fs-set-project-root-syncs-with-get-info
  (testing "fs-set-project-root result matches fs-get-project-info"
    (with-test-project-root
      (let* ((original-root cl-mcp/src/fs:*project-root*)
             (original-cwd (getcwd))
             (test-dir (namestring original-root)))
        (unwind-protect
             (progn
               (fs-set-project-root test-dir)
               (let ((info (fs-get-project-info)))
                 (ok (string= (gethash "project_root" info) test-dir))
                 (ok (string= (gethash "cwd" info) test-dir))))
          ;; Restore original state
          (setf cl-mcp/src/fs:*project-root* original-root)
          (ignore-errors (uiop:chdir original-cwd)))))))
