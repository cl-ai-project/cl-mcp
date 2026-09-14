;;;; tests/response-builders-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/tools/response-builders.
;;;; Each builder converts internal core results into the canonical MCP
;;;; tool-response shape (a hash-table with a `content` text-vector and
;;;; structured fields).  The suite checks branching shape: with/without
;;;; line numbers, with/without arglist or doc, error vs success, and
;;;; lambda-context handling.

(defpackage #:cl-mcp/tests/response-builders-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/src/code-refs-core
                #:build-references-report)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-code-find-response
                #:build-code-describe-response
                #:build-code-find-references-response
                #:build-inspect-response))

(in-package #:cl-mcp/tests/response-builders-test)

(defun first-text (response)
  "Pull the text of the first content part out of RESPONSE, or NIL."
  (let ((content (gethash "content" response)))
    (when (and (vectorp content) (plusp (length content)))
      (gethash "text" (aref content 0)))))

(defun %ref (&key (path "src/a.lisp") (line 10) (type "call") (caller "a") form-type
                  form-name (origin "xref+source") sites test note)
  "Return a reference object shaped like MERGE-REFERENCES' output."
  (make-ht "path" path "line" line "type" type "types" (vector type)
           "caller" caller "caller_symbol" nil "context" "(defun a"
           "form_type" form-type "form_name" form-name "origin" origin
           "call_sites" (coerce sites 'vector) "test" test "stale" nil "note" note))

(defun %site (line &key (kind "call") (context "(foo 1)") shadowed-by)
  "Return a call-site object shaped like MERGE-REFERENCES' output."
  (make-ht "line" line "column" 3 "kind" kind "context" context
           "shadowed_by" shadowed-by))

(defun %report (refs &rest overrides)
  "Return a found-symbol report for REFS; OVERRIDES win over the defaults."
  (apply #'build-references-report
         (append overrides
                 (list :symbol "p::foo" :resolved-symbol "P::FOO" :status :found
                       :kind "function" :project-only t :refs refs))))

(deftest build-code-find-references-response-shows-forms-sites-and-tests
  (testing "header, one line per form, its call sites, and the tests"
    (let* ((r (build-code-find-references-response
               (%report (list (%ref :form-type "defun" :form-name "a"
                                    :sites (list (%site 12)
                                                 (%site 13 :kind "quoted" :context "'foo")))
                              (%ref :path "tests/a-test.lisp" :line 5 :form-type "deftest"
                                    :form-name "a-test" :caller "(lambda)"
                                    :sites (list (%site 6))
                                    :test (make-ht "name" "a-test" "framework" "rove"))))))
           (text (first-text r)))
      (ok (search "P::FOO (function) — 2 forms in 2 files, 1 test" text))
      (ok (search "src/a.lisp:10 (defun a) [call]" text))
      (ok (search "  12: (foo 1)" text))
      (ok (search "  13 (quoted): 'foo" text))
      (ok (search "tests/a-test.lisp:5 (deftest a-test) [call] TEST" text))
      (ok (search "Tests: a-test" text))
      (ok (= 2 (gethash "count" r))))))

(deftest build-code-find-references-response-notes-and-caps
  (testing "notes follow the form line; call sites beyond five are counted"
    (let ((text (first-text
                 (build-code-find-references-response
                  (%report (list (%ref :form-type "defun" :form-name "a"
                                       :sites (loop for i from 1 to 7 collect (%site i)))
                                 (%ref :path "src/b.lisp" :origin "xref" :caller "hidden"
                                       :note (concatenate
                                              'string
                                              "call not visible in source "
                                              "(produced by a macro expansion)"))
                                 (%ref :path "src/c.lisp" :form-type "defun" :form-name "c"
                                       :sites (list (%site 3 :shadowed-by "flet")))))))))
      (ok (search "  +2 more" text))
      (ok (search "src/b.lisp:10 (used in hidden) [call] — call not visible in source" text))
      (ok (search "[shadowed by flet]" text)))))

(deftest build-code-find-references-response-hides-lambda-callers
  (testing "a lambda caller without a form falls back to path:line"
    (let ((text (first-text (build-code-find-references-response
                             (%report (list (%ref :caller "(lambda)" :origin "xref")))))))
      (ok (search "src/a.lisp:10 [call]" text))
      (ok (not (search "(lambda)" text))))))

(deftest build-code-find-references-response-truncates
  (testing "forms beyond the limit are counted, not listed"
    (let ((text (first-text (build-code-find-references-response
                             (%report (list (%ref :path "a.lisp") (%ref :path "b.lisp")
                                            (%ref :path "c.lisp"))
                                      :limit 2)))))
      (ok (search "— 3 forms in 3 files" text))
      (ok (search "… 1 more form (raise limit to see them)" text))
      (ok (not (search "c.lisp" text))))))

(deftest build-code-find-references-response-empty-and-missing
  (testing "no references says so and shows what was searched"
    (let ((text (first-text (build-code-find-references-response
                             (%report '() :files-scanned 147 :name-matches 0)))))
      (ok (search "P::FOO (function) — no references." text))
      (ok (search "xref: 0 entries   source scan: 147 files, 0 matches" text))))
  (testing "a missing symbol"
    (let ((text (first-text (build-code-find-references-response
                             (%report '() :status :not-found :resolved-symbol nil :kind nil
                                          :lookup-package "P" :lookup-name "NOPE"
                                          :name-matches 7)))))
      (ok (search "Symbol \"NOPE\" not found in P (nothing was interned)" text))
      (ok (search "7 textual matches" text))))
  (testing "a missing package"
    (let ((text (first-text (build-code-find-references-response
                             (%report '() :status :package-not-found
                                          :lookup-package "NOPE-PKG")))))
      (ok (search "Package \"NOPE-PKG\" not found" text))))
  (testing "unresolved matches and notes are listed"
    (let ((text (first-text (build-code-find-references-response
                             (%report (list (%ref))
                                      :unresolved (list (list :path "tests/x-test.lisp"
                                                              :package "X-TEST" :count 3
                                                              :tests '("x-test")))
                                      :notes '("source scan skipped: project root is not set"))))))
      (ok (search "+ 3 possible matches in files whose package is not loaded:" text))
      (ok (search "tests/x-test.lisp (X-TEST; tests: x-test)" text))
      (ok (search "Note: source scan skipped: project root is not set" text)))))

(deftest build-code-find-response-with-line
 (testing "successful find emits path, line and a content text mentioning both"
  (let ((r (build-code-find-response "FOO" "/no/such/path.lisp" 42)))
    (ok (string= "/no/such/path.lisp" (gethash "path" r)))
    (ok (= 42 (gethash "line" r)))
    (ok (null (gethash "isError" r)))
    (let ((text (first-text r)))
      (ok (search "FOO" text))
      (ok (search "42" text))
      (ok (search "/no/such/path.lisp" text))))))

(deftest build-code-find-response-without-line
 (testing "missing line still produces a non-error response without a number"
  (let ((r (build-code-find-response "BAR" "/no/such/path.lisp" nil)))
    (ok (null (gethash "isError" r)))
    (let ((text (first-text r)))
      (ok (search "BAR" text))
      (ok (not (search " line " text)) "no line phrase when line is NIL")))))

(deftest build-code-find-response-does-not-misreport-an-existing-file
  (testing "a file that exists is not annotated 'source not on disk'"
    ;; The annotation used to fire on every hit.  BUILD-CODE-FIND-RESPONSE
    ;; probed the path it was given, but by then CODE-FIND-DEFINITION had
    ;; already made it relative for display -- relative to *PROJECT-ROOT*,
    ;; which is not the process's working directory in a worker.  Every
    ;; existing file therefore probed as missing.  The caller now decides,
    ;; from the absolute pathname, and passes the answer in.
    ;;
    ;; The two cases above both use /no/such/path.lisp and assert the
    ;; annotation IS present, so neither could ever have caught this.
    (let* ((r (build-code-find-response "FOO" "src/core.lisp" 10 t))
           (text (gethash "text" (aref (gethash "content" r) 0))))
      (ok (search "src/core.lisp" text) "the path is still reported")
      (ok (search "line 10" text) "and the line")
      (ok (not (search "not on disk" text))
          "an on-disk file must not be annotated as missing"))
    (let* ((r (build-code-find-response "FOO" "src/core.lisp" 10 nil))
           (text (gethash "text" (aref (gethash "content" r) 0))))
      (ok (search "not on disk" text)
          "and a genuinely missing file still is"))))

(deftest build-code-find-response-keeps-the-two-value-contract
  (testing "a caller passing no on-disk flag still probes for itself"
    ;; The argument is optional so a caller that has not been updated keeps
    ;; the old behaviour rather than silently claiming every file is present.
    (let* ((r (build-code-find-response "BAZ" "/no/such/path.lisp" 7))
           (text (gethash "text" (aref (gethash "content" r) 0))))
      (ok (search "not on disk" text)
          "a missing file is still reported as missing without the flag"))))

(deftest build-code-find-response-not-found
 (testing "NIL path produces an isError payload"
  (let ((r (build-code-find-response "BAZ" nil nil)))
    (ok (eq t (gethash "isError" r)))
    (let ((text (first-text r)))
      (ok (search "Definition not found" text))
      (ok (search "BAZ" text))))))

(deftest build-code-describe-response-with-arglist-and-doc
 (testing "all fields are passed through and rendered into content text"
  (let ((r (build-code-describe-response "FOO" "function" "(X Y)"
                                         "doc-string"
                                         "/path/file.lisp" 7)))
    (ok (string= "FOO" (gethash "name" r)))
    (ok (string= "function" (gethash "type" r)))
    (ok (string= "(X Y)" (gethash "arglist" r)))
    (ok (string= "doc-string" (gethash "documentation" r)))
    (ok (string= "/path/file.lisp" (gethash "path" r)))
    (ok (= 7 (gethash "line" r)))
    (let ((text (first-text r)))
      (ok (search "FOO" text))
      (ok (search "function" text))
      (ok (search "doc-string" text))))))

(deftest build-code-describe-response-without-doc
 (testing "missing arglist/doc/line still produce a coherent text"
  (let ((r (build-code-describe-response "BAR" "macro" nil nil nil nil)))
    (ok (null (gethash "documentation" r)))
    (let ((text (first-text r)))
      (ok (search "BAR" text))
      (ok (search "macro" text))))))

(deftest build-inspect-response-success-attaches-content
 (testing "successful inspection result gets a content vector attached"
  (let* ((ir (make-ht "id" 1 "kind" "list" "summary" "(1 2 3)"
                      "elements" '() "meta" (make-ht)))
         (r (build-inspect-response ir)))
    (ok (null (gethash "isError" r)))
    (ok (vectorp (gethash "content" r)))
    (let ((text (first-text r)))
      (ok (search "list" text))))))

(deftest build-inspect-response-error-payload
 (testing "error inspection result becomes an isError envelope"
  (let* ((ir (make-ht "error" t "message" "object 42 not found"))
         (r (build-inspect-response ir)))
    (ok (eq t (gethash "isError" r)))
    (ok (search "not found" (first-text r))))))
