;;;; tests/clgrep-test.lisp

(defpackage #:cl-mcp/tests/clgrep-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/fs
                #:*project-root*)
  (:import-from #:cl-mcp/src/clgrep
                #:clgrep-search
                #:clgrep-signatures))

(in-package #:cl-mcp/tests/clgrep-test)

(deftest clgrep-search-returns-results
  (testing "clgrep-search returns list of alists with expected keys"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-search "defun" :path "src/" :recursive nil)))
        (ok (listp results))
        (ok (> (length results) 0))
        ;; Check that results have expected keys
        (let ((first-result (first results)))
          (ok (assoc :file first-result))
          (ok (assoc :line first-result))
          (ok (assoc :match first-result))
          (ok (assoc :signature first-result))
          (ok (assoc :form first-result)))))))

(deftest clgrep-signatures-omits-form-body
  (testing "clgrep-signatures returns results without :form key"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (let ((results (clgrep-signatures "defun" :path "src/" :recursive nil)))
        (ok (listp results))
        (ok (> (length results) 0))
        ;; Check that results have signature but no form body
        (let ((first-result (first results)))
          (ok (assoc :file first-result))
          (ok (assoc :line first-result))
          (ok (assoc :signature first-result))
          ;; :form should NOT be present in signatures-only mode
          (ok (null (assoc :form first-result))))))))

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

(deftest clgrep-search-case-insensitive
  (testing "clgrep-search with case-insensitive flag"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      ;; Search for "DEFUN" with case-insensitive should find defun forms
      (let ((results (clgrep-search "DEFUN" :path "src/"
                                    :recursive nil
                                    :case-insensitive t)))
        (ok (listp results))
        (ok (> (length results) 0))))))
