;;;; tests/code-refs-core-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/code-refs-core: reading a symbol as written
;;;; without interning it, and deciding which scan sites name a symbol.

(defpackage #:cl-mcp/tests/code-refs-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/code-refs-core
                #:parse-symbol-text
                #:parse-target-designator
                #:resolve-target
                #:qualified-symbol-name
                #:symbol-kind
                #:resolve-site-token))

(in-package #:cl-mcp/tests/code-refs-core-test)

;;; Packages with known contents, so resolution is checked against them
;;; rather than against whatever the image happens to hold.

(defpackage #:cl-mcp-refs-core-a
  (:use #:cl)
  (:export #:shared)
  (:intern #:inner))

(defpackage #:cl-mcp-refs-core-b
  (:use #:cl)
  (:intern #:shared))

(defpackage #:cl-mcp-refs-core-c
  (:use #:cl)
  (:local-nicknames (#:aa #:cl-mcp-refs-core-a)))

(defpackage #:cl-mcp-refs-core-d
  (:use #:cl #:cl-mcp-refs-core-a))

(defun %validation-error-p (thunk)
  "True when calling THUNK signals ARG-VALIDATION-ERROR."
  (handler-case (progn (funcall thunk) nil)
    (arg-validation-error () t)))

(deftest parse-symbol-text-splits-and-upcases
  (testing "unqualified, single and double colon, escapes and keywords"
    (flet ((parts (text) (multiple-value-list (parse-symbol-text text))))
      (ok (equal '("FOO" nil nil) (parts "foo")))
      (ok (equal '("FOO" "PKG" nil) (parts "pkg:foo")))
      (ok (equal '("FOO" "PKG" nil) (parts "pkg::foo")))
      (ok (equal '("Foo" nil nil) (parts "|Foo|")))
      (ok (equal '("aB" "P" nil) (parts "p::\\a|B|")))
      (ok (equal '("FOO" "KEYWORD" nil) (parts ":foo")))
      (ok (equal '("CL-MCP/SRC/X" nil nil) (parts "  cl-mcp/src/x "))))))

(deftest parse-symbol-text-reports-malformed-input
  (testing "text that is not a symbol name comes back with a reason"
    (flet ((problem (text) (nth-value 2 (parse-symbol-text text))))
      (ok (stringp (problem "")))
      (ok (stringp (problem "   ")))
      (ok (stringp (problem "pkg:")))
      (ok (stringp (problem "a:b:c")))
      (ok (stringp (problem "a:::b")))
      (ok (stringp (problem "#:foo")))
      (ok (stringp (problem "|open")))
      (ok (stringp (problem "foo\\")))
      (ok (null (nth-value 0 (parse-symbol-text "a:b:c")))))))

(deftest parse-target-designator-signals-validation-errors
  (testing "keywords and malformed text are argument errors"
    (ok (%validation-error-p (lambda () (parse-target-designator ":foo"))))
    (ok (%validation-error-p (lambda () (parse-target-designator ""))))
    (ok (equal '("FOO" "PKG") (multiple-value-list (parse-target-designator "pkg::foo"))))))

(deftest resolve-target-finds-without-interning
  (testing "found, internal through one colon, not found, missing package"
    (multiple-value-bind (symbol status package-name name)
        (resolve-target "cl-mcp-refs-core-a:shared")
      (ok (eq :found status))
      (ok (eq symbol (find-symbol "SHARED" "CL-MCP-REFS-CORE-A")))
      (ok (equal "CL-MCP-REFS-CORE-A" package-name))
      (ok (equal "SHARED" name)))
    (ok (eq :found (nth-value 1 (resolve-target "cl-mcp-refs-core-a:inner")))
        "a single colon reaches an internal symbol")
    (ok (eq :found (nth-value 1 (resolve-target "shared" :package "cl-mcp-refs-core-a"))))
    (ok (eq :found (nth-value 1 (resolve-target "aa:shared" :package "cl-mcp-refs-core-c")))
        "the package argument's local nicknames apply")
    (multiple-value-bind (symbol status package-name name)
        (resolve-target "cl-mcp-refs-core-a::never-interned-xyz")
      (ok (null symbol))
      (ok (eq :not-found status))
      (ok (equal "CL-MCP-REFS-CORE-A" package-name))
      (ok (equal "NEVER-INTERNED-XYZ" name)))
    (ok (null (nth-value 1 (find-symbol "NEVER-INTERNED-XYZ" "CL-MCP-REFS-CORE-A")))
        "looking a symbol up must not intern it")
    (ok (eq :package-not-found (nth-value 1 (resolve-target "no-such-package-xyz::foo"))))
    (ok (eq :package-not-found
            (nth-value 1 (resolve-target "foo" :package "no-such-package-xyz"))))
    (ok (null (find-package "NO-SUCH-PACKAGE-XYZ")))))

(deftest qualified-symbol-name-follows-the-reader
  (testing "external, internal, keyword and uninterned"
    (ok (equal "CL-MCP-REFS-CORE-A:SHARED"
               (qualified-symbol-name (find-symbol "SHARED" "CL-MCP-REFS-CORE-A"))))
    (ok (equal "CL-MCP-REFS-CORE-A::INNER"
               (qualified-symbol-name (find-symbol "INNER" "CL-MCP-REFS-CORE-A"))))
    (ok (equal ":TEST" (qualified-symbol-name :test)))
    (ok (equal "#:LOOSE" (qualified-symbol-name (make-symbol "LOOSE"))))))

(deftest symbol-kind-names-what-a-symbol-denotes
  (testing "most specific kind first"
    (ok (equal "special-operator" (symbol-kind 'if)))
    (ok (equal "macro" (symbol-kind 'deftest)))
    (ok (equal "generic-function" (symbol-kind 'print-object)))
    (ok (equal "function" (symbol-kind 'parse-symbol-text)))
    (ok (equal "constant" (symbol-kind 'most-positive-fixnum)))
    (ok (equal "variable" (symbol-kind '*package*)))
    (ok (equal "unbound" (symbol-kind (find-symbol "INNER" "CL-MCP-REFS-CORE-A"))))))

(deftest resolve-site-token-judges-each-spelling
  (testing "inherited, nicknamed, other symbol, and missing packages"
    (let ((shared (find-symbol "SHARED" "CL-MCP-REFS-CORE-A")))
      (ok (eq :match (resolve-site-token "shared" "CL-MCP-REFS-CORE-A" shared)))
      (ok (eq :match (resolve-site-token "shared" "cl-mcp-refs-core-d" shared))
          "inherited through :use, designator written in lower case")
      (ok (eq :match (resolve-site-token "aa:shared" "CL-MCP-REFS-CORE-C" shared))
          "package-local nickname of the site's package")
      (ok (eq :other (resolve-site-token "shared" "CL-MCP-REFS-CORE-B" shared))
          "same name, different symbol")
      (ok (eq :other (resolve-site-token "shared" "CL-MCP-REFS-CORE-C" shared))
          "absent from the site's package")
      (multiple-value-bind (verdict missing)
          (resolve-site-token "shared" "NOT-LOADED-PKG-XYZ" shared)
        (ok (eq :unresolved verdict))
        (ok (equal "NOT-LOADED-PKG-XYZ" missing)))
      (multiple-value-bind (verdict missing)
          (resolve-site-token "nope-xyz:shared" "CL-MCP-REFS-CORE-A" shared)
        (ok (eq :unresolved verdict))
        (ok (equal "NOPE-XYZ" missing)))
      (ok (null (nth-value 1 (find-symbol "SHARED" "CL-MCP-REFS-CORE-C")))
          "resolution must not intern into the site's package"))))
