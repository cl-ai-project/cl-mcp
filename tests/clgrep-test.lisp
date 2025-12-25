;;;; tests/clgrep-test.lisp

(defpackage #:cl-mcp/tests/clgrep-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/fs
                #:*project-root*)
  (:import-from #:cl-mcp/src/clgrep
                #:clgrep-search))

(in-package #:cl-mcp/tests/clgrep-test)

(deftest clgrep-search-returns-results
  (testing "clgrep-search returns list of alists with expected keys (default: no form)"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "defun" :path "src/" :recursive nil)))
        (ok (listp results))
        (ok (> (length results) 0))
        ;; Check that results have expected keys (no :form by default)
        (let ((first-result (first results)))
          (ok (assoc :file first-result))
          (ok (assoc :line first-result))
          (ok (assoc :match first-result))
          (ok (assoc :signature first-result))
          ;; :form should NOT be present by default
          (ok (null (assoc :form first-result)))))))
  (testing "clgrep-search with include-form returns :form key"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "defun" :path "src/" :recursive nil :include-form t)))
        (ok (listp results))
        (ok (> (length results) 0))
        ;; Check that results have :form when include-form is true
        (let ((first-result (first results)))
          (ok (assoc :file first-result))
          (ok (assoc :signature first-result))
          (ok (assoc :form first-result)))))))

(deftest clgrep-search-filters-by-form-type
  (testing "clgrep-search filters results by form-types"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "." :path "src/"
                                    :recursive nil
                                    :form-types '("defparameter"))))
        (ok (listp results))
        ;; All results should be defparameter forms
        (dolist (r results)
          (let ((form-type (cdr (assoc :form-type r))))
            (ok (or (null form-type)
                    (string-equal form-type "defparameter")))))))))

(deftest clgrep-search-filters-by-form-type-vector
  (testing "clgrep-search filters by form-types when passed as vector (JSON array)"
    ;; This test verifies the fix for the bug where JSON arrays (parsed as vectors
    ;; by yason) were not handled by %parse-form-types, causing the filter to be
    ;; silently ignored.
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      ;; Pass form-types as a vector (simulating JSON array from MCP client)
      (let ((results (clgrep-search "." :path "src/"
                                    :recursive nil
                                    :form-types #("defparameter"))))
        (ok (listp results))
        (ok (> (length results) 0) "Should find at least one defparameter")
        ;; All results should be defparameter forms only
        (dolist (r results)
          (let ((form-type (cdr (assoc :form-type r))))
            (ok (string-equal form-type "defparameter")
                (format nil "Expected defparameter but got ~A" form-type)))))))
  (testing "clgrep-search with vector form-types filters rare form types correctly"
    ;; Test with defmethod which has only 2 occurrences in the project
    ;; This would have returned thousands of results before the fix
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "." :path "."
                                    :recursive t
                                    :form-types #("defmethod"))))
        (ok (listp results))
        ;; Should find defmethod results (there are exactly 2 defmethod forms)
        ;; Each form may have multiple line matches, but all should be defmethod
        (ok (> (length results) 0) "Should find at least one defmethod")
        (ok (< (length results) 20) "Should not return excessive results")
        ;; All results must be defmethod
        (dolist (r results)
          (let ((form-type (cdr (assoc :form-type r))))
            (ok (string-equal form-type "defmethod")
                (format nil "Expected defmethod but got ~A" form-type))))))))

(deftest clgrep-search-case-insensitive
  (testing "clgrep-search with case-insensitive flag"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      ;; Search for "DEFUN" with case-insensitive should find defun forms
      (let ((results (clgrep-search "DEFUN" :path "src/"
                                    :recursive nil
                                    :case-insensitive t)))
        (ok (listp results))
        (ok (> (length results) 0))))))


(deftest clgrep-search-path-parameter
  (testing "clgrep-search with single file path returns only results from that file"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "defun" :path "src/http.lisp")))
        (ok (listp results))
        (ok (> (length results) 0) "Should find defun in http.lisp")
        ;; All results must be from http.lisp only
        (dolist (r results)
          (let ((file (cdr (assoc :file r))))
            (ok (search "http.lisp" file)
                (format nil "Expected http.lisp but got ~A" file)))))))
  (testing "clgrep-search with directory path returns results from multiple files"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let* ((results (clgrep-search "defun" :path "src/" :recursive nil :limit 50))
             (files (remove-duplicates
                     (mapcar (lambda (r) (cdr (assoc :file r))) results)
                     :test #'string=)))
        (ok (listp results))
        (ok (> (length results) 0) "Should find defun in src/")
        ;; Should have results from multiple files
        (ok (> (length files) 1)
            (format nil "Expected multiple files but got ~A" files)))))
  (testing "clgrep-search single file vs directory returns different result counts"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((single-file-results (clgrep-search "defparameter"
                                                :path "src/fs.lisp"
                                                :form-types '("defparameter")))
            (dir-results (clgrep-search "defparameter"
                                        :path "src/"
                                        :recursive nil
                                        :form-types '("defparameter"))))
        (ok (listp single-file-results))
        (ok (listp dir-results))
        ;; Directory search should find more or equal results
        (ok (>= (length dir-results) (length single-file-results))
            (format nil "Directory (~A) should have >= single file (~A) results"
                    (length dir-results) (length single-file-results)))
        ;; Single file results should all be from fs.lisp
        (dolist (r single-file-results)
          (ok (search "fs.lisp" (cdr (assoc :file r)))))))))
