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
