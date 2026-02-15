;;;; tests/prompts-test.lisp

(defpackage #:cl-mcp/tests/prompts-test
  (:use #:cl #:rove)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:temporary-directory
                #:delete-directory-tree
                #:subpathp)
  (:import-from #:cl-mcp/src/prompts
                #:prompts-directory
                #:discover-prompts
                #:find-prompt-by-name)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*))

(in-package #:cl-mcp/tests/prompts-test)

(defun %system-prompts-directory ()
  (merge-pathnames "prompts/"
                   (ensure-directory-pathname
                    (system-source-directory "cl-mcp"))))

(defun %prompt-names (prompts)
  (mapcar (lambda (prompt) (gethash "name" prompt))
          prompts))

(defmacro with-temp-project-prompts ((root-var) &body body)
  `(let* ((,root-var (merge-pathnames
                      (format nil "cl-mcp-prompts-test-~A/" (get-universal-time))
                      (temporary-directory)))
          (prompts-dir (merge-pathnames "prompts/" ,root-var)))
     (ensure-directories-exist (merge-pathnames "seed.md" prompts-dir))
     (unwind-protect
          (progn ,@body)
       (ignore-errors (delete-directory-tree ,root-var :validate t)))))

(deftest prompts-directory-uses-system-root
  (testing "prompts-directory resolves to cl-mcp bundled prompts path"
    (let* ((actual (prompts-directory))
           (expected (%system-prompts-directory)))
      (ok (pathnamep actual))
      (ok (equal (namestring (truename actual))
                 (namestring (truename expected)))))))

(deftest prompts-directory-ignores-project-root
  (testing "prompts-directory does not switch to *project-root* prompts"
    (with-temp-project-prompts (tmp-root)
      (let ((probe (merge-pathnames "project-only.md"
                                    (merge-pathnames "prompts/" tmp-root))))
        (with-open-file (s probe :direction :output :if-exists :supersede)
          (write-string "# Project Only Prompt" s))
        (let ((*project-root* tmp-root)
              (actual (prompts-directory))
              (expected (%system-prompts-directory)))
          (ok (equal (namestring (truename actual))
                     (namestring (truename expected))))
          (ok (not (subpathp actual tmp-root))))))))

(deftest discover-prompts-ignores-project-root-prompts
  (testing "discover-prompts returns only bundled prompt metadata"
    (with-temp-project-prompts (tmp-root)
      (let ((probe (merge-pathnames "zz-project-only.md"
                                    (merge-pathnames "prompts/" tmp-root))))
        (with-open-file (s probe :direction :output :if-exists :supersede)
          (write-string "# Project Override Prompt\nThis should not appear." s))
        (let* ((*project-root* tmp-root)
               (prompts (discover-prompts))
               (names (%prompt-names prompts)))
          (ok (find "repl-driven-development" names :test #'string=))
          (ok (not (find "zz-project-only" names :test #'string=))))))))

(deftest find-prompt-by-name-ignores-project-shadow
  (testing "find-prompt-by-name does not read project-local prompt overrides"
    (with-temp-project-prompts (tmp-root)
      (let ((shadow (merge-pathnames "repl-driven-development.md"
                                     (merge-pathnames "prompts/" tmp-root))))
        (with-open-file (s shadow :direction :output :if-exists :supersede)
          (write-string "# Project Shadow\nOVERRIDE-MARKER" s))
        (let* ((*project-root* tmp-root)
               (prompt (find-prompt-by-name "repl-driven-development"))
               (file-path (and prompt (pathname (gethash "file_path" prompt))))
               (system-prompts (%system-prompts-directory)))
          (ok prompt)
          (ok (pathnamep file-path))
          (ok (subpathp file-path system-prompts))
          (ok (not (subpathp file-path tmp-root))))))))
