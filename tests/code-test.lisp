;;;; tests/code-test.lisp

(defpackage #:cl-mcp/tests/code-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/code
                #:code-find-definition
                #:code-describe-symbol))

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
  (testing "code.find-references returns at least one project reference"
    ;; Skip this test on macOS due to XREF instability
    #+darwin
    (skip "XREF tests are unstable on macOS")
    #-darwin
    (progn
      ;; Ensure at least one known reference exists for the target symbol by
      ;; defining and compiling a tiny helper that calls it. This avoids xref
      ;; flakiness when the symbol has not been compiled elsewhere in the session.
      (unless (fboundp 'cl-mcp/tests/code-test::xref-anchor)
        (defun cl-mcp/tests/code-test::xref-anchor ()
          (cl-mcp:process-json-line "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}")))
      (compile 'cl-mcp/tests/code-test::xref-anchor)
      (multiple-value-bind (refs count)
          (cl-mcp/src/code:code-find-references "cl-mcp:process-json-line")
        (ok (>= count 1))
        (ok (vectorp refs))
        (let ((first (aref refs 0)))
          (ok (hash-table-p first))
          (ok (stringp (gethash "path" first)))
          (ok (integerp (gethash "line" first)))
          (ok (stringp (gethash "type" first))))))))
