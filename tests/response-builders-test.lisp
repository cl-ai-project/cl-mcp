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

(deftest build-code-find-references-response-uses-caller
 (testing "summary text shows caller name when available"
  (let* ((ref (make-ht "path" "/p/a.lisp" "line" 10 "type" "call"
                       "caller" "MAIN" "context" "..."))
         (refs (vector ref))
         (r (build-code-find-references-response "FOO" refs 1 t))
         (text (first-text r)))
    (ok (= 1 (gethash "count" r)))
    (ok (eq t (gethash "project_only" r)))
    (ok (search "MAIN" text) "caller appears in summary")
    (ok (search "FOO" (or (gethash "symbol" r) ""))))))

(deftest build-code-find-references-response-empty
 (testing "empty refs vector yields an empty content text without erroring"
  (let* ((r (build-code-find-references-response "FOO" #() 0 nil))
         (text (first-text r)))
    (ok (= 0 (gethash "count" r)))
    (ok (or (null text) (zerop (length text))) "summary is empty"))))

(deftest build-code-find-references-response-lambda-caller-falls-back
 (testing "(lambda) caller is treated as missing and we fall back to path:line"
  (let* ((ref (make-ht "path" "/p/a.lisp" "line" 99 "type" "call"
                       "caller" "(lambda)" "context" "..."))
         (refs (vector ref))
         (r (build-code-find-references-response "FOO" refs 1 nil))
         (text (first-text r)))
    (ok (search "/p/a.lisp:99" text) "fallback path:line shown")
    (ok (not (search "(lambda)" text)) "lambda placeholder is suppressed"))))

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
