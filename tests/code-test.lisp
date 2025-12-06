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
      (ok (string= path "src/core.lisp"))
      (ok (integerp line))
      (ok (> line 0)))))

(deftest code-find-definition-ignores-package-when-qualified
  (testing "code.find-definition ignores provided package for qualified symbols"
    (multiple-value-bind (path line)
        (code-find-definition "cl-mcp:version" :package "")
      (ok (string= path "src/core.lisp"))
      (ok (integerp line))
      (ok (> line 0)))))

(deftest code-describe-symbol-returns-doc
  (testing "code.describe-symbol returns type, arglist, and documentation"
    (multiple-value-bind (name type arglist doc)
        (code-describe-symbol "cl-mcp:version")
      (ok (stringp name))
      (ok (string= type "function"))
      (ok (stringp arglist))
      (ok (stringp doc)))))

(deftest code-find-references-returns-project-refs
  (testing "code.find-references returns at least one project reference"
    ;; Ensure at least one known reference exists for the target symbol by
    ;; defining and compiling a tiny helper that calls it. This avoids xref
    ;; flakiness when the symbol has not been compiled elsewhere in the session.
    (unless (fboundp 'cl-mcp/tests/code-test::xref-anchor)
      (defun cl-mcp/tests/code-test::xref-anchor ()
        (cl-mcp:process-json-line "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}")))
    (compile 'cl-mcp/tests/code-test::xref-anchor)
    (multiple-value-bind (refs count)
        (code-find-references "cl-mcp:process-json-line")
      (ok (>= count 1))
      (ok (vectorp refs))
      (let ((first (aref refs 0)))
        (ok (hash-table-p first))
        (ok (stringp (gethash "path" first)))
        (ok (integerp (gethash "line" first)))
        (ok (stringp (gethash "type" first)))))))
