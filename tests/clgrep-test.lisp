;;;; tests/clgrep-test.lisp

(defpackage #:cl-mcp/tests/clgrep-test
  (:use #:cl)
    (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:signals)
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

(deftest clgrep-search-empty-pattern-rejected
  (testing "clgrep-search signals error for empty pattern"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (ok (signals (clgrep-search "" :path "src/")))))
  (testing "clgrep-search signals error for whitespace-only pattern"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (ok (signals (clgrep-search "   " :path "src/"))))))

(deftest clgrep-search-default-limit
  (testing "clgrep-search applies default limit of 200 when limit is not specified"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      ;; Search for "." which matches everything — should be capped at 200
      (let ((results (clgrep-search "." :path "." :recursive t)))
        (ok (listp results))
        (ok (<= (length results) 200)
            (format nil "Expected at most 200 results without explicit limit, got ~A"
                    (length results))))))
  (testing "clgrep-search explicit limit overrides default"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "defun" :path "src/" :recursive nil :limit 3)))
        (ok (listp results))
        (ok (<= (length results) 3)
            (format nil "Expected at most 3 results with explicit limit, got ~A"
                    (length results)))))))

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

(deftest clgrep-search-accepts-registered-asdf-system-directory
  (testing "an absolute path inside a registered ASDF system outside the project root is searched"
    ;; clgrep-search is read-only, so it uses the same policy as the read tools
    ;; (lisp-read-file / fs-read-file): project root OR a registered system's
    ;; source directory. Without this, discovery on a dependency was blocked
    ;; while reading the very same files was allowed.
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (system-dir (asdf:system-source-directory :alexandria)))
      (ok (not (uiop:subpathp system-dir *project-root*))
          "alexandria must live outside the project root for this test to mean anything")
      (let ((results (clgrep-search "defun" :path (namestring system-dir)
                                    :recursive t :limit 5
                                    :form-types '("defun"))))
        (ok (listp results))
        (ok (> (length results) 0)
            "should find defun forms in the dependency's sources")
        (dolist (r results)
          (ok (string-equal "defun" (cdr (assoc :form-type r)))))))))

(deftest clgrep-search-rejects-path-outside-every-allowed-root
  (testing "a path in neither the project root nor a registered system still signals"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((message (handler-case (progn (clgrep-search "defun" :path "/etc") nil)
                       (error (e) (princ-to-string e)))))
        (ok message "/etc must be rejected")
        (ok (search "/etc" message)
            (format nil "the error must name the rejected path, got: ~A" message))))))

(deftest clgrep-search-path-default-and-relative-unchanged
  (testing "omitting path searches the project root"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "clgrep-search" :recursive t :limit 10)))
        (ok (listp results))
        (ok (> (length results) 0)
            "should find clgrep-search somewhere under the project root"))))
  (testing "a relative path under the project root still resolves"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "defun" :path "src/" :recursive nil :limit 5)))
        (ok (listp results))
        (ok (> (length results) 0) "should find defun under src/")))))
