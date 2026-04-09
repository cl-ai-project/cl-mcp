;;;; tests/code-test.lisp

(defpackage #:cl-mcp/tests/code-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok
                #:skip)
  (:import-from #:cl-mcp/main)  ; Ensure CL-MCP package nickname exists
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

(deftest code-describe-symbol-handles-classes
  (testing "code-describe-symbol handles CLOS classes"
    ;; STANDARD-OBJECT is a well-known CL class that exists in any
    ;; SBCL image. Before the fix, describe would error with
    ;; "not bound as a function or variable".
    (multiple-value-bind (name type arglist doc path line)
        (code-describe-symbol "common-lisp:standard-object")
      (declare (ignore doc path line))
      (ok (stringp name))
      (ok (string= type "class"))
      (ok (stringp arglist)
          "arglist should be a slot summary string"))))

(deftest code-describe-symbol-handles-conditions
  (testing "code-describe-symbol handles condition types"
    ;; SIMPLE-ERROR is a condition type — defined via define-condition.
    (multiple-value-bind (name type arglist doc path line)
        (code-describe-symbol "common-lisp:simple-error")
      (declare (ignore arglist doc path line))
      (ok (stringp name))
      (ok (string= type "condition")
          "simple-error should be described as a condition"))))

(deftest code-describe-symbol-handles-generic-functions
  (testing "code-describe-symbol reports generic-function for defgeneric bindings"
    ;; PRINT-OBJECT is a generic function in every SBCL image.
    (multiple-value-bind (name type arglist doc path line)
        (code-describe-symbol "common-lisp:print-object")
      (declare (ignore doc path line))
      (ok (stringp name))
      (ok (string= type "generic-function")
          "print-object should be described as a generic-function")
      (ok (stringp arglist)))))

(deftest code-find-definition-logical-pathname
  (testing "code-find-definition handles CL standard symbols with logical pathnames"
    ;; CL standard symbols like CL:CAR have source locations with logical pathnames
    ;; (e.g., SYS:SRC;CODE;LIST.LISP). Before the fix, this would crash with
    ;; "logical namestring is not valid as a native namestring".
    (multiple-value-bind (path line)
        (code-find-definition "cl:car")
      ;; path may be nil if SBCL source is not installed, but it should NOT crash
      (if path
          (progn
            (ok (stringp path) "path should be a string")
            ;; line may be nil if the translated source file doesn't exist on disk
            (ok (or (null line) (and (integerp line) (> line 0)))
                "line should be nil or a positive integer"))
          ;; Even if source not found, no crash occurred
          (ok t "code-find-definition did not crash on logical pathname")))))

(deftest code-find-definition-line-points-at-open-paren
  (testing "code-find-definition line number matches the (def... form's line"
    ;; Regression: SBCL's character-offset may point at a newline just
    ;; *before* the opening paren (the newline after the preceding form).
    ;; The reported line must match the line containing `(def...' in the
    ;; file as a human would count lines, not SBCL's raw offset.
    (multiple-value-bind (path line)
        (code-find-definition "cl-mcp:version")
      (ok (stringp path))
      (ok (integerp line))
      (when (stringp path)
        (let* ((abs-path
                 (or (probe-file path)
                     (merge-pathnames
                      path
                      (asdf:system-source-directory :cl-mcp))))
               (content (and abs-path (uiop:read-file-string abs-path))))
          (when content
            (let* ((lines (cl-ppcre:split "\\n" content :limit nil))
                   (line-text (and lines
                                   (< (1- line) (length lines))
                                   (nth (1- line) lines))))
              (ok (stringp line-text))
              (ok line-text
                  "reported line should exist in the source file")
              (when (stringp line-text)
                (ok (or (search "(defun version" line-text)
                        (search "(defmacro version" line-text))
                    "reported line should contain the version def form")))))))))

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

(deftest code-find-references-includes-caller
  (testing "each reference has a caller field identifying the enclosing function"
    (if (uiop/os:os-macosx-p)
        (skip "XREF tests are unstable on macOS")
        (multiple-value-bind (refs count)
            (code-find-references "cl-mcp:process-json-line")
          (when (> count 0)
            (let ((first (aref refs 0)))
              (ok (hash-table-p first))
              ;; New field added in P1#3 fix: every reference carries
              ;; the caller's fully-qualified name so users can locate
              ;; the actual usage even when xref's line number points
              ;; at the enclosing form's start.
              (ok (stringp (gethash "caller" first)))
              (ok (plusp (length (gethash "caller" first))))))))))

(deftest code-format-xref-caller-normalizes-shapes
  (testing "%format-xref-caller turns SBCL internal shapes into readable names"
    (let ((fmt #'cl-mcp/src/code-core::%format-xref-caller))
      ;; Regular symbol caller
      (ok (string= "my-func" (funcall fmt 'my-func))
          "symbol should print downcased")
      ;; Fast method caller — the SBCL-internal shape user
      ;; previously saw leaked through as "(fast-method NAME ...)"
      (ok (search "defmethod"
                  (funcall fmt
                           (cons (intern "FAST-METHOD" "SB-PCL")
                                 (list 'contains-p '(bloom-filter string)))))
          "fast-method should be rendered as (defmethod ...)")
      (ok (not (search "fast-method"
                       (funcall fmt
                                (cons (intern "FAST-METHOD" "SB-PCL")
                                      (list 'contains-p
                                            '(bloom-filter string))))))
          "fast-method token must not leak to user")
      ;; Lambda with absolute file path should collapse
      (ok (string= "(lambda)" (funcall fmt '(lambda () :in "/abs/path.lisp")))
          "lambda + file path should collapse to (lambda)")
      (ok (not (search "/abs/path" (funcall fmt '(lambda () :in "/abs/path.lisp"))))
          "absolute file paths must not appear in caller output"))))
