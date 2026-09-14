;;;; tests/code-refs-core-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/code-refs-core: reading a symbol as written
;;;; without interning it, and deciding which scan sites name a symbol.

(defpackage #:cl-mcp/tests/code-refs-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:arg-validation-error
                #:make-ht)
  (:import-from #:cl-mcp/src/code-refs-core
                #:parse-symbol-text
                #:parse-target-designator
                #:resolve-target
                #:qualified-symbol-name
                #:symbol-kind
                #:resolve-site-token
                #:resolve-scan-forms
                #:merge-references
                #:build-references-report))

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

;;; Scan sites, merge and report

(defun %site (token &key (line 1) (kind "call") shadowed-by)
  "Return a scan site object as CL-MCP/SRC/CODE-REFS-SCAN:SCAN-TEXT builds it."
  (make-ht "line" line "column" 1 "kind" kind "token" token
           "context" token "shadowed_by" shadowed-by))

(defun %form (path index in-package sites &key test-name form-name)
  "Return a scan form object for PATH holding SITES."
  (make-ht "path" path "abs_path" (concatenate 'string "/abs/" path)
           "index" index "start_line" (* 10 index) "end_line" (+ 5 (* 10 index))
           "form_type" "defun" "form_name" form-name
           "test_name" test-name "test_framework" (and test-name "rove")
           "in_package" in-package "context" "(defun ..."
           "sites" (coerce sites 'vector)))

(defun %xref (truename index &key (type "call") (caller "c") caller-symbol (line 12)
                                  stale (scan-status :scanned))
  "Return an xref entry plist as CL-MCP/SRC/CODE-CORE collects it.
LINE defaults to one inside %RESOLVED's default span (lines 10 to 15), since
MERGE-REFERENCES only trusts an index whose form holds the entry's line."
  (list :type type :caller caller :caller-symbol caller-symbol
        :truename truename :path (subseq truename 5) :line line :context "ctx"
        :form-index index :stale stale :scan-status scan-status))

(defun %resolved (truename index &key (start-line 10) test-name form-name
                                      (sites (list (list :line 11 :column 3 :kind "call"
                                                         :context "(shared)"
                                                         :shadowed-by nil))))
  "Return a resolved form plist as RESOLVE-SCAN-FORMS returns it."
  (list :truename truename :path (subseq truename 5) :index index
        :start-line start-line :end-line (+ start-line 5)
        :form-type "defun" :form-name form-name
        :test-name test-name :test-framework (and test-name "rove")
        :context "(defun x" :sites sites))

(defun %with-origin (refs origin)
  "Return the references among REFS whose origin is ORIGIN."
  (remove-if-not (lambda (ref) (equal origin (gethash "origin" ref))) refs))

(deftest resolve-scan-forms-keeps-only-the-target
  (testing "matches kept, other symbols dropped, missing packages counted"
    (let* ((shared (find-symbol "SHARED" "CL-MCP-REFS-CORE-A"))
           (forms (list (%form "a.lisp" 1 "CL-MCP-REFS-CORE-A"
                               (list (%site "shared" :line 11)
                                     (%site "shared" :line 12 :kind "quoted")))
                        (%form "b.lisp" 2 "CL-MCP-REFS-CORE-B"
                               (list (%site "shared" :line 21)))
                        (%form "c.lisp" 3 "NOT-LOADED-PKG-XYZ"
                               (list (%site "shared") (%site "shared"))
                               :test-name "c-test"))))
      (multiple-value-bind (resolved unresolved)
          (resolve-scan-forms forms shared :macro-p t)
        (ok (= 1 (length resolved)))
        (ok (equal '("macro" "quoted")
                   (mapcar (lambda (site) (getf site :kind)) (getf (first resolved) :sites)))
            "a call becomes macro when the target is a macro")
        (ok (equal "/abs/a.lisp" (getf (first resolved) :truename)))
        (ok (= 1 (length unresolved)))
        (ok (= 2 (getf (first unresolved) :count)))
        (ok (equal '("c-test") (getf (first unresolved) :tests)))
        (ok (equal "NOT-LOADED-PKG-XYZ" (getf (first unresolved) :package)))))))

(deftest resolve-scan-forms-accepts-lists-from-json
  (testing "sites parsed back from JSON arrive as a list"
    (let ((form (%form "a.lisp" 1 "CL-MCP-REFS-CORE-A" '())))
      (setf (gethash "sites" form) (list (%site "shared")))
      (ok (= 1 (length (resolve-scan-forms (list form)
                                           (find-symbol "SHARED" "CL-MCP-REFS-CORE-A"))))))))

(deftest merge-references-three-outcomes
  (testing "xref and source together, xref alone, source alone"
    (let ((refs (merge-references
                 (list (%xref "/abs/a.lisp" 1 :caller "a" :caller-symbol "P::A")
                       (%xref "/abs/a.lisp" 2 :caller "hidden" :line 20)
                       (%xref "/abs/z.lisp" nil :line 40 :scan-status :not-scanned))
                 (list (%resolved "/abs/a.lisp" 1 :form-name "a")
                       (%resolved "/abs/a.lisp" 3 :start-line 30 :form-name "*top*")))))
      (ok (= 4 (length refs)))
      (let ((both (first (%with-origin refs "xref+source"))))
        (ok (equal "P::A" (gethash "caller_symbol" both)))
        (ok (= 1 (length (gethash "call_sites" both))))
        (ok (= 10 (gethash "line" both)) "line is the enclosing form's start")
        (ok (equal "a" (gethash "form_name" both)))
        (ok (null (gethash "note" both))))
      (let ((xref-only (%with-origin refs "xref")))
        (ok (= 2 (length xref-only)))
        (ok (find-if (lambda (ref) (search "macro expansion" (gethash "note" ref))) xref-only))
        (ok (find-if (lambda (ref) (search "not scanned" (gethash "note" ref))) xref-only))
        (ok (every (lambda (ref) (zerop (length (gethash "call_sites" ref)))) xref-only)))
      (let ((source-only (first (%with-origin refs "source"))))
        (ok (equal "*top*" (gethash "caller" source-only)))
        (ok (null (gethash "caller_symbol" source-only)))
        (ok (equal '("call") (coerce (gethash "types" source-only) 'list)))
        (ok (search "not in xref" (gethash "note" source-only)))))))

(deftest merge-references-groups-types-and-prefers-named-callers
  (testing "one form, two xref kinds, a lambda and a named caller"
    (let* ((refs (merge-references
                  (list (%xref "/abs/a.lisp" 1 :type "call" :caller "(lambda)")
                        (%xref "/abs/a.lisp" 1 :type "reference" :caller "named"
                                               :caller-symbol "P::NAMED" :stale t))
                  (list (%resolved "/abs/a.lisp" 1 :test-name "a-test"))))
           (ref (first refs)))
      (ok (= 1 (length refs)))
      (ok (equal '("call" "reference") (coerce (gethash "types" ref) 'list)))
      (ok (equal "call" (gethash "type" ref)))
      (ok (equal "named" (gethash "caller" ref)))
      (ok (eq t (gethash "stale" ref)))
      (ok (search "reload" (gethash "note" ref)))
      (ok (equal "a-test" (gethash "name" (gethash "test" ref)))))))

(deftest merge-references-validates-the-index-against-the-line-span
  (testing "an index within the form's span still meets that form"
    (let* ((refs (merge-references
                  (list (%xref "/abs/a.lisp" 5 :caller "b" :caller-symbol "P::B" :line 50))
                  (list (%resolved "/abs/a.lisp" 5 :start-line 50 :form-name "b"))))
           (ref (first refs)))
      (ok (= 1 (length refs)))
      (ok (equal "xref+source" (gethash "origin" ref)))
      (ok (equal "P::B" (gethash "caller_symbol" ref)))))
  (testing "a shifted index meets the form whose span holds the entry's line"
    ;; The parent skipped a #+feature form SBCL counted: SBCL's index for
    ;; each caller is one more than the parent's.
    (let ((refs (merge-references
                 (list (%xref "/abs/a.lisp" 5 :caller "a" :caller-symbol "P::A" :line 40)
                       (%xref "/abs/a.lisp" 6 :caller "b" :caller-symbol "P::B" :line 50))
                 (list (%resolved "/abs/a.lisp" 4 :start-line 40 :form-name "a")
                       (%resolved "/abs/a.lisp" 5 :start-line 50 :form-name "b")))))
      (ok (= 2 (length refs)))
      (ok (every (lambda (ref) (equal "xref+source" (gethash "origin" ref))) refs))
      (ok (equal '(("a" . "P::A") ("b" . "P::B"))
                 (mapcar (lambda (ref)
                           (cons (gethash "form_name" ref) (gethash "caller_symbol" ref)))
                         refs))
          "each caller keeps its own form")
      (ok (every (lambda (ref) (null (gethash "note" ref))) refs))))
  (testing "no form holding the line leaves the entry unmatched, grouped on its line"
    (let* ((refs (merge-references
                  (list (%xref "/abs/a.lisp" 5 :caller "gated" :caller-symbol "P::GATED"
                                               :line 70)
                        (%xref "/abs/a.lisp" 5 :type "reference" :caller "gated"
                                               :caller-symbol "P::GATED" :line 70)
                        (%xref "/abs/b.lisp" 5 :caller "elsewhere" :line 52))
                  (list (%resolved "/abs/a.lisp" 5 :start-line 50 :form-name "b"))))
           (xref-only (%with-origin refs "xref"))
           (gated (find "gated" xref-only
                        :key (lambda (ref) (gethash "caller" ref)) :test #'equal))
           (source-only (first (%with-origin refs "source"))))
      (ok (= 3 (length refs)))
      (ok (= 2 (length xref-only)))
      (ok (and gated (= 70 (gethash "line" gated))) "reported on the entry's own line")
      (ok (and gated (equal '("call" "reference") (coerce (gethash "types" gated) 'list)))
          "entries on the same line share one reference")
      (ok (and gated (null (gethash "form_name" gated))))
      (ok (find "elsewhere" xref-only :key (lambda (ref) (gethash "caller" ref)) :test #'equal)
          "a form in another file never holds the line")
      (ok (and source-only (equal "b" (gethash "form_name" source-only)))
          "the form the index named is left to the source scan"))))

(deftest build-references-report-sorts-limits-and-lists-tests
  (testing "sorted by path and line, cut to LIMIT, counted in full"
    (let* ((refs (merge-references
                  '()
                  (list (%resolved "/abs/b.lisp" 1 :test-name "b-test")
                        (%resolved "/abs/a.lisp" 2 :start-line 20)
                        (%resolved "/abs/a.lisp" 1 :start-line 10))))
           (report (build-references-report :symbol "p::x" :resolved-symbol "P::X"
                                            :status :found :kind "function"
                                            :project-only t :limit 2 :refs refs)))
      (ok (= 3 (gethash "count" report)))
      (ok (= 2 (gethash "file_count" report)))
      (ok (= 2 (length (gethash "refs" report))))
      (ok (eq t (gethash "truncated" report)))
      (ok (equal '("a.lisp" "a.lisp")
                 (map 'list (lambda (ref) (gethash "path" ref)) (gethash "refs" report))))
      (ok (equal '(10 20)
                 (map 'list (lambda (ref) (gethash "line" ref)) (gethash "refs" report))))
      (ok (equal '("b-test")
                 (map 'list (lambda (test) (gethash "name" test)) (gethash "tests" report)))
          "tests come from every reference, not only the ones shown")
      (ok (equal "found" (gethash "symbol_status" report)))))
  (testing "status spellings and an untruncated report"
    (ok (equal "not_found"
               (gethash "symbol_status" (build-references-report :symbol "x" :status :not-found))))
    (ok (equal "package_not_found"
               (gethash "symbol_status"
                        (build-references-report :symbol "x" :status :package-not-found))))
    (ok (eq yason:false (gethash "truncated" (build-references-report :symbol "x"))))))
