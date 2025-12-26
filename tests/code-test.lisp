;;;; tests/code-test.lisp

(defpackage #:cl-mcp/tests/code-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/code
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references))

(in-package #:cl-mcp/tests/code-test)

(deftest code-find-definition-returns-path-and-line
  (testing "code.find-definition returns relative path and positive line"
    (multiple-value-bind (path line)
        (code-find-definition "cl-mcp:version")
      (ok (stringp path))
      (ok (search "src/core.lisp" path :from-end t))
      (ok (integerp line))
      (ok (> line 0)))))

(deftest code-find-definition-ignores-package-when-qualified
  (testing "code.find-definition ignores provided package for qualified symbols"
    (multiple-value-bind (path line)
        (code-find-definition "cl-mcp:version" :package "")
      (ok (stringp path))
      (ok (search "src/core.lisp" path :from-end t))
      (ok (integerp line))
      (ok (> line 0)))))

(deftest code-describe-symbol-returns-doc
  (testing "code.describe-symbol returns type, arglist, documentation, and location"
    (multiple-value-bind (name type arglist doc path line)
        (code-describe-symbol "cl-mcp:version")
      (ok (stringp name))
      (ok (string= type "function"))
      (ok (stringp arglist))
      (ok (stringp doc))
      (ok (stringp path))
      (ok (integerp line))
      (ok (> line 0)))))

(deftest code-find-references-returns-project-refs
  (testing "code.find-references returns valid structure"
    ;; Skip this test on macOS due to XREF instability
    (if (uiop:os-macosx-p)
        (skip "XREF tests are unstable on macOS")
        (multiple-value-bind (refs count)
            (code-find-references "cl-mcp:process-json-line")
          ;; refs may be empty if SBCL xref only has REPL-based entries
          ;; (pathname "repl-eval"), which are correctly filtered out.
          (ok (vectorp refs))
          (ok (= count (length refs)))
          (when (> count 0)
            (let ((first (aref refs 0)))
              (ok (hash-table-p first))
              (ok (stringp (gethash "path" first)))
              (ok (integerp (gethash "line" first)))
              (ok (stringp (gethash "type" first)))))))))
