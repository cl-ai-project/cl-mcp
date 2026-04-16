;;;; tests/clhs-test.lisp

(defpackage #:cl-mcp/tests/clhs-test
  (:use #:cl)
    (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:signals)
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

(deftest clhs-lookup-section-not-found-returns-error
  (testing "clhs-lookup returns isError for non-existent section"
    (let ((result (clhs-lookup "99.99")))
      (ok (hash-table-p result))
      (ok (gethash "isError" result)
          "should have isError flag for non-existent section")
      (let ((content (gethash "content" result)))
        (ok (vectorp content) "content should be MCP format vector")
        (when (and (vectorp content) (> (length content) 0))
          (ok (search "99.99" (gethash "text" (aref content 0)))
              "error message should mention the section number"))))))

(deftest clhs-lookup-format-as-symbol
  (testing "clhs-lookup treats 'format' as symbol not section"
    (let ((result (clhs-lookup "format")))
      (ok (gethash "symbol" result))
      (ok (string= (gethash "symbol" result) "format")))))

(deftest clhs-lookup-single-digit-chapter-section
  (testing "clhs-lookup correctly handles single-digit chapter sections with zero-padding"
    ;; M-6 fix: chapter numbers are zero-padded to 2 digits (e.g., 7 -> 07)
    ;; so the file is 07_a.htm, not 7_a.htm.
    (let ((result (clhs-lookup "7.1")))
      (ok (hash-table-p result))
      (when (gethash "section" result)
        (ok (string= (gethash "section" result) "7.1")))
      ;; If local, the URL should contain the zero-padded filename
      (when (gethash "url" result)
        (ok (search "07_a" (gethash "url" result))
            "URL should use zero-padded chapter number 07_a.htm")))
    ;; Also test chapter 3 subsection
    (let ((result (clhs-lookup "3.1")))
      (ok (hash-table-p result))
      (when (gethash "url" result)
        (ok (search "03_a" (gethash "url" result))
            "URL should use zero-padded chapter number 03_a.htm")))))

(deftest clhs-lookup-brief-trims-symbol-content
  (testing "brief mode returns shorter content than non-brief for symbol pages"
    (let ((brief (clhs-lookup "loop" :include-content t :brief t))
          (full (clhs-lookup "loop" :include-content t :brief nil)))
      (when (and (string= (gethash "source" brief) "local")
                 (string= (gethash "source" full) "local"))
        (let* ((brief-vec (gethash "content" brief))
               (full-vec (gethash "content" full))
               (brief-text (when (and (vectorp brief-vec) (plusp (length brief-vec)))
                             (gethash "text" (aref brief-vec 0))))
               (full-text (when (and (vectorp full-vec) (plusp (length full-vec)))
                            (gethash "text" (aref full-vec 0)))))
          (when (and brief-text full-text)
            (ok (< (length brief-text) (length full-text))
                "brief content is shorter than full content")
            (ok (null (search "Description:" brief-text))
                "brief content stops before the Description section")))))))

(deftest clhs-lookup-brief-caps-section-pages
  (testing "brief mode caps section page content to *brief-max-chars*"
    (let ((result (clhs-lookup "22.3" :include-content t :brief t)))
      (when (string= (gethash "source" result) "local")
        (let* ((content-vec (gethash "content" result))
               (text (when (and (vectorp content-vec) (plusp (length content-vec)))
                       (gethash "text" (aref content-vec 0)))))
          (when text
            (ok (<= (length text) cl-mcp/src/clhs::*brief-max-chars*)
                "section page content is capped to *brief-max-chars*")))))))
