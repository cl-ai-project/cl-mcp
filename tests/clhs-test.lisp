;;;; tests/clhs-test.lisp

(defpackage #:cl-mcp/tests/clhs-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/clhs
                #:clhs-lookup))

(in-package #:cl-mcp/tests/clhs-test)

;;; ---------------------------------------------------------------------------
;;; Symbol Lookup Tests
;;; ---------------------------------------------------------------------------

(deftest clhs-lookup-symbol-returns-hash-table
  (testing "clhs-lookup returns hash-table for valid CL symbol"
    (let ((result (clhs-lookup "loop")))
      (ok (hash-table-p result))
      (ok (gethash "symbol" result))
      (ok (gethash "url" result))
      (ok (member (gethash "source" result) '("local" "remote") :test #'string=)))))

(deftest clhs-lookup-symbol-case-insensitive
  (testing "clhs-lookup is case-insensitive for symbols"
    (let ((result-lower (clhs-lookup "loop"))
          (result-upper (clhs-lookup "LOOP"))
          (result-mixed (clhs-lookup "Loop")))
      (ok (string= (gethash "symbol" result-lower)
                   (gethash "symbol" result-upper)))
      (ok (string= (gethash "symbol" result-lower)
                   (gethash "symbol" result-mixed))))))

(deftest clhs-lookup-symbol-with-hyphen
  (testing "clhs-lookup handles hyphenated symbols"
    (let ((result (clhs-lookup "handler-case")))
      (ok (hash-table-p result))
      (ok (string= (gethash "symbol" result) "handler-case")))))

(deftest clhs-lookup-symbol-content-extraction
  (testing "clhs-lookup extracts content when include_content is true"
    (let ((result (clhs-lookup "loop" :include-content t)))
      (when (string= (gethash "source" result) "local")
        (let ((content (gethash "content" result)))
          (ok content "Content should be present for local source")
          (when content
            ;; Content is MCP format: vector of hash-tables
            (ok (vectorp content) "Content should be a vector (MCP format)")
            (ok (> (length content) 0) "Content should have at least one element")
            (let ((first-item (aref content 0)))
              (ok (hash-table-p first-item) "First element should be hash-table")
              (ok (gethash "type" first-item) "Content item should have type")
              (ok (gethash "text" first-item) "Content item should have text"))))))))

(deftest clhs-lookup-symbol-no-content
  (testing "clhs-lookup omits content when include_content is false"
    (let ((result (clhs-lookup "loop" :include-content nil)))
      (ok (null (gethash "content" result))))))

(deftest clhs-lookup-unknown-symbol-errors
  (testing "clhs-lookup signals error for unknown symbol"
    (ok (signals (clhs-lookup "not-a-real-cl-symbol-xyz")))))

;;; ---------------------------------------------------------------------------
;;; Section Lookup Tests
;;; ---------------------------------------------------------------------------

(deftest clhs-lookup-section-returns-hash-table
  (testing "clhs-lookup returns hash-table for section number"
    (let ((result (clhs-lookup "22.3")))
      (ok (hash-table-p result))
      (ok (gethash "section" result))
      (ok (string= (gethash "section" result) "22.3"))
      (ok (gethash "url" result)))))

(deftest clhs-lookup-section-deep-nesting
  (testing "clhs-lookup handles deeply nested sections"
    (let ((result (clhs-lookup "22.3.1")))
      (ok (hash-table-p result))
      (ok (string= (gethash "section" result) "22.3.1")))))

(deftest clhs-lookup-section-chapter-only
  (testing "clhs-lookup handles chapter-only numbers"
    (let ((result (clhs-lookup "3")))
      (ok (hash-table-p result))
      (ok (string= (gethash "section" result) "3")))))

(deftest clhs-lookup-section-content-extraction
  (testing "clhs-lookup extracts content for sections when local"
    (let ((result (clhs-lookup "22.3" :include-content t)))
      (when (string= (gethash "source" result) "local")
        (let ((content (gethash "content" result)))
          (ok content "Content should be present for local section"))))))

;;; ---------------------------------------------------------------------------
;;; Auto-Detection Tests
;;; ---------------------------------------------------------------------------

(deftest clhs-lookup-distinguishes-section-from-symbol
  (testing "clhs-lookup correctly distinguishes section numbers from symbols"
    ;; Section: starts with digit, contains only digits and dots
    (let ((section-result (clhs-lookup "22.3")))
      (ok (gethash "section" section-result))
      (ok (null (gethash "symbol" section-result))))
    ;; Symbol: contains letters
    (let ((symbol-result (clhs-lookup "loop")))
      (ok (gethash "symbol" symbol-result))
      (ok (null (gethash "section" symbol-result))))))

(deftest clhs-lookup-format-as-symbol
  (testing "clhs-lookup treats 'format' as symbol not section"
    (let ((result (clhs-lookup "format")))
      (ok (gethash "symbol" result))
      (ok (string= (gethash "symbol" result) "format")))))
