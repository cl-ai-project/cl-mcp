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
                #:code-find-references)
  (:import-from #:cl-mcp/src/code-core
                #:code-find-references-report
                #:definition-source-line
                #:definition-source-location
                #:%read-form-starts
                #:%offset->line
                #:generic-function-method-count)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:scan-project)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht))

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

(deftest code-offset-to-line-skips-reader-conditionals
  (testing "%offset->line lands on (def...) even when preceded by #+ / #- / #|...|#"
    (let* ((tmp (uiop:merge-pathnames*
                 (format nil "cl-mcp-offset-test-~A.lisp" (get-universal-time))
                 (uiop:temporary-directory)))
           (path (namestring tmp))
           (text
            (format nil
                    "(in-package :cl-user)~%~
                     ~%~
                     (defun before () :ok)~%~
                     ~%~
                     #|~%~
                      block comment~%~
                     |#~%~
                     (defun after-block () :ok)~%~
                     ~%~
                     #+(or sbcl ccl)~%~
                     (defun after-list-cond () :ok)~%~
                     ~%~
                     #+sbcl~%~
                     (defun after-atom-cond () :ok)~%")))
      (unwind-protect
           (progn
             (with-open-file (s path :direction :output :if-exists :supersede)
               (write-string text s))
             (labels ((line-of-marker (needle)
                        (1+ (count #\Newline text :end (search needle text))))
                      (probe-before (needle)
                        ;; Simulate SBCL's offset landing a character or two
                        ;; before the directive.
                        (let ((pos (search needle text)))
                          (cl-mcp/src/code-core::%offset->line
                           path (max 0 (- pos 1))))))
               (ok (= (probe-before "#|")
                      (line-of-marker "(defun after-block"))
                   "offset just before #|...|# should report the defun's line")
               (ok (= (probe-before "#+(or")
                      (line-of-marker "(defun after-list-cond"))
                   "offset just before #+(or ...) should report the defun's line")
               (ok (= (probe-before "#+sbcl")
                      (line-of-marker "(defun after-atom-cond"))
                   "offset just before a true #+sbcl should report the gated defun's line")))
        (ignore-errors (delete-file path))))))

(deftest code-offset-to-line-skips-forms-false-in-this-image
  (testing "%offset->line evaluates reader conditionals and walks comments of any length"
    (let ((path (namestring (uiop:merge-pathnames*
                             (format nil "cl-mcp-offset-false-~A.lisp" (get-universal-time))
                             (uiop:temporary-directory))))
          (text
           (with-output-to-string (s)
             (flet ((line (control &rest args)
                      (apply #'format s control args)
                      (terpri s)))
               (line "(in-package :cl-user)")
               (line "(defun before () :ok)")
               (line "#+(or)")
               (line "(defun gated-or () :never)")
               (line "(defun after-or () :ok)")
               (line "#-sbcl")
               (line "(defun gated-not-sbcl () :never)")
               (line "(defun after-not-sbcl () :ok)")
               (line "#+(or) (defun gated-stacked-1 () :never)")
               (line "#-sbcl (defun gated-stacked-2 () :never)")
               (line "(defun after-stacked () :ok)")
               (line "#+(and sbcl (not sbcl))")
               (line "(defun gated-and-not () :never)")
               (line "(defun after-and-not () :ok)")
               (line "#-(OR :SBCL ccl) (defun gated-keyword () :never)")
               (line "(defun after-keyword () :ok)")
               (line "#-cl-user::sbcl")
               (line "(defun gated-prefixed () :read-on-sbcl)")
               (line "#+#:nil")
               (line "(defun gated-uninterned () :never)")
               (line "(defun after-uninterned () :ok)")
               (line "#+nil (defun gated-nil () :never)")
               (line "(defun after-nil () :ok)")
               (line "#+cl-mcp-offset-absent-top-xyz")
               (line "(defun gated-absent ()")
               (line "  #+cl-mcp-offset-absent-nested-xyz (car '(x)) \")\" #\\) :never)")
               (line "(defun after-absent () :ok)")
               (line "#+(or) #+sbcl (defun gated-false-over-true () :never)")
               (line "(defun after-false-over-true () :ok)")
               (line "#-sbcl #-ccl (defun gated-false-over-true-too () :never)")
               (line "(defun after-false-over-true-too () :ok)")
               (line "#+nil #+cl-mcp-offset-absent-nested-xyz (defun gated-first () :never)")
               (line "(defun gated-second () :never)")
               (line "(defun after-false-over-false () :ok)")
               (line "~C" #\Page)
               (line "(defun after-page () :ok)")
               (line "#-cl-mcp-offset-absent-top-xyz")
               (line "(defun gated-true-minus () :ok)")
               (line ";; A line comment block longer than 1024 characters.")
               (dotimes (i 30)
                 (line ";; filler line ~2,'0D of a comment block longer than 1024 characters" i))
               (line "(defun after-long-line-comment () :ok)")
               (line "#| An outer block comment")
               (line "   #| with a nested one |#")
               (dotimes (i 30)
                 (line "   filler line ~2,'0D of a block comment longer than 1024 characters" i))
               (line "|#")
               (line "(defun after-long-block-comment () :ok)")
               (line "#+sbcl")
               (line "(defun gated-sbcl () :ok)")
               (line "#+(version>= 9)")
               (line "(defun gated-unparsed () :ok)")
               (line "#-sbcl")
               (line "(defun gated-unreadable ()")))))
      (unwind-protect
           (progn
             (with-open-file (s path :direction :output :if-exists :supersede)
               (write-string text s))
             (labels ((line-of (needle)
                        (1+ (count #\Newline text :end (search needle text))))
                      (line-after (previous)
                        ;; SBCL's offset points just past the previous form.
                        (cl-mcp/src/code-core::%offset->line
                         path (+ (search previous text) (length previous))))
                      (lands (previous expected description)
                        (ok (eql (line-of expected) (line-after previous)) description)))
               (lands "(defun before () :ok)" "(defun after-or"
                      "a #+(or) form is skipped with its conditional")
               (lands "(defun after-or () :ok)" "(defun after-not-sbcl"
                      "a #-sbcl form is skipped with its conditional")
               (lands "(defun after-not-sbcl () :ok)" "(defun after-stacked"
                      "stacked false conditionals are all skipped")
               (lands "(defun after-stacked () :ok)" "(defun after-and-not"
                      "#+(and sbcl (not sbcl)) is false")
               (lands "(defun after-and-not () :ok)" "(defun after-keyword"
                      "unprefixed and keyword atoms match case-insensitively")
               (lands "(defun after-keyword () :ok)" "(defun gated-prefixed"
                      "an atom in a package other than KEYWORD never matches, as in SBCL")
               (lands "(defun gated-prefixed () :read-on-sbcl)" "(defun after-uninterned"
                      "#+#:nil is false: an uninterned symbol is never a feature")
               (lands "(defun after-uninterned () :ok)" "(defun after-nil"
                      "#+nil is false")
               (lands "(defun after-nil () :ok)" "(defun after-absent"
                      "a skipped form's strings, characters and conditionals are read past")
               (lands "(defun after-absent () :ok)" "(defun after-false-over-true ()"
                      "#+(or) over a true #+sbcl skips one form, not the next one too")
               (lands "(defun after-false-over-true () :ok)" "(defun after-false-over-true-too ()"
                      "#-sbcl over a true #-ccl skips one form, not the next one too")
               (lands "(defun after-false-over-true-too () :ok)" "(defun after-false-over-false"
                      "a false conditional over a false one skips two forms, as the reader does")
               (lands "(defun after-false-over-false () :ok)" "(defun after-page"
                      "a form feed is whitespace")
               (lands "(defun after-page () :ok)" "(defun gated-true-minus"
                      "a true #- lands on the gated defun")
               (lands "(defun gated-true-minus () :ok)" "(defun after-long-line-comment"
                      "a line comment block over 1024 characters is walked past")
               (lands "(defun after-long-line-comment () :ok)" "(defun after-long-block-comment"
                      "a nested block comment over 1024 characters is walked past")
               (lands "(defun after-long-block-comment () :ok)" "(defun gated-sbcl"
                      "a true #+sbcl still lands on the gated defun")
               (lands "(defun gated-sbcl () :ok)" "(defun gated-unparsed"
                      "an expression the parser does not understand counts as true")
               (lands "(defun gated-unparsed () :ok)" "(defun gated-unreadable"
                      "a gated form that cannot be read is where the walk stops"))
             (ok (null (find-symbol "CL-MCP-OFFSET-ABSENT-TOP-XYZ" "KEYWORD"))
                 "evaluating a feature expression interns nothing")
             (ok (null (find-symbol "CL-MCP-OFFSET-ABSENT-NESTED-XYZ" "KEYWORD"))
                 "skipping a form interns nothing, not even its conditionals' features")
             (ok (null (find-symbol "GATED-ABSENT" "COMMON-LISP-USER"))))
        (ignore-errors (delete-file path))))))

(deftest code-offset-to-line-counts-octets
  (testing "%offset->line reads SBCL's octet offset after a multibyte comment"
    (let* ((tmp (uiop:merge-pathnames*
                 (format nil "cl-mcp-offset-octets-~A.lisp" (get-universal-time))
                 (uiop:temporary-directory)))
           (path (namestring tmp))
           (text (format nil "(in-package :cl-user)~%~
                              ;; 日本語のコメントでバイト数と文字数がずれる~%~
                              (defun before-mb () :ok)~%~
                              ~%~
                              (defun after-mb () :ok)~%~
                              (defun filler-1 () :ok)~%~
                              (defun filler-2 () :ok)~%~
                              (defun filler-3 () :ok)~%")))
      (unwind-protect
           (progn
             (with-open-file (s path :direction :output :if-exists :supersede
                                     :external-format :utf-8)
               (write-string text s))
             (let* ((char-pos (search "(defun after-mb" text))
                    ;; SBCL records the octet position just past the previous
                    ;; form, i.e. at the whitespace before this one.
                    (octet-pos (length (sb-ext:string-to-octets
                                        text :end (1- char-pos)
                                        :external-format :utf-8))))
               (ok (= (1+ (count #\Newline text :end char-pos))
                      (cl-mcp/src/code-core::%offset->line path octet-pos))
                   "the defun after the comment is reported on its own line")))
        (ignore-errors (delete-file path))))))

(deftest code-find-definition-line-after-multibyte-comment
  (testing "code-find-definition reports the defun's own line in a UTF-8 file"
    (let* ((dir (uiop:ensure-directory-pathname
                 (uiop:merge-pathnames* (format nil "cl-mcp-octets-~D/" (random 1000000))
                                        (uiop:temporary-directory))))
           (src (merge-pathnames "octets.lisp" dir))
           (fasl (merge-pathnames "octets.fasl" dir))
           (text (format nil "(defpackage #:cl-mcp-octets-fixture (:use #:cl))~%~
                              (in-package #:cl-mcp-octets-fixture)~%~
                              ;; 日本語のコメントでバイト数と文字数がずれる~%~
                              (defun one () 1)~%~
                              (defun two () 2)~%~
                              (defun three () 3)~%~
                              (defun four () 4)~%")))
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             (with-open-file (s src :direction :output :if-exists :supersede
                                    :external-format :utf-8)
               (write-string text s))
             (handler-bind ((warning #'muffle-warning))
               (load (compile-file src :output-file fasl :verbose nil :print nil)))
             (ok (eql (1+ (count #\Newline text :end (search "(defun three" text)))
                      (nth-value 1 (code-find-definition "cl-mcp-octets-fixture::three")))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

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

;;; code-find-references-report against real xref data

(defparameter *xref-fixture*
  (asdf:system-relative-pathname :cl-mcp "tests/fixtures/xref-fixture.lisp")
  "Fixture compiled so that SBCL records cross references for it.")

(defun %load-xref-fixture (&optional (fixture *xref-fixture*))
  "Compile and load FIXTURE; xref needs COMPILE-FILE, not LOAD of source."
  (uiop:with-temporary-file (:pathname fasl :type "fasl")
    (handler-bind ((warning #'muffle-warning))
      (load (compile-file fixture :output-file fasl :verbose nil :print nil)))))

(defun %xref-fixture-report (&optional (designator "cl-mcp-xref-fixture:target"))
  "Return the report for DESIGNATOR, the fixture's TARGET unless given, scanning the
fixture directory."
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (%load-xref-fixture)
    (code-find-references-report
     designator
     :limit 1000
     :scan (scan-project designator
                         :root (uiop:pathname-directory-pathname *xref-fixture*)))))

(defun %fixture-line (needle &optional (fixture *xref-fixture*))
  "Return the 1-based line of FIXTURE on which NEEDLE starts."
  (let ((text (uiop:read-file-string fixture)))
    (1+ (count #\Newline text :end (search needle text)))))

(defun %ref-named (report form-name)
  "Return the reference in REPORT whose form_name is FORM-NAME."
  (find form-name (gethash "refs" report)
        :key (lambda (ref) (gethash "form_name" ref)) :test #'equal))

(defun %site-lines (ref)
  "Return the call-site lines of REF."
  (map 'list (lambda (site) (gethash "line" site)) (gethash "call_sites" ref)))

(deftest code-find-references-report-exact-call-sites
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (let ((report (%xref-fixture-report)))
        (testing "the symbol resolves and is described"
          (ok (equal "found" (gethash "symbol_status" report)))
          (ok (equal "CL-MCP-XREF-FIXTURE:TARGET" (gethash "resolved_symbol" report)))
          (ok (equal "function" (gethash "symbol_kind" report))))
        (testing "a plain caller carries its exact call line and qualified name"
          (let ((ref (%ref-named report "plain-caller")))
            (ok ref)
            (when ref
              (ok (equal "xref+source" (gethash "origin" ref)))
              (ok (equal (list (%fixture-line "(target 1)")) (%site-lines ref)))
              (ok (equal "CL-MCP-XREF-FIXTURE::PLAIN-CALLER" (gethash "caller_symbol" ref)))
              (ok (= (%fixture-line "(defun plain-caller") (gethash "line" ref))))))
        (testing "#+sbcl and eval-when wrappers still meet their xref entries"
          (let ((feature (%ref-named report "feature-caller"))
                (eval-when (find (%fixture-line "(target 3)") (gethash "refs" report)
                                 :key (lambda (ref) (first (%site-lines ref))))))
            (ok (and feature (equal "xref+source" (gethash "origin" feature))))
            (ok (and feature (equal (list (%fixture-line "(target 2)")) (%site-lines feature))))
            (ok (and eval-when (equal "xref+source" (gethash "origin" eval-when))))
            (ok (and eval-when (equal "eval-when" (gethash "form_type" eval-when))))))
        (testing "a method is named the way lisp-edit-form addresses it"
          (let ((ref (%ref-named report "shape-area ((shape integer))")))
            (ok ref)
            (ok (and ref (equal "xref+source" (gethash "origin" ref))))
            (ok (and ref (equal "CL-MCP-XREF-FIXTURE::SHAPE-AREA"
                                (gethash "caller_symbol" ref))))))
        (testing "a package-local nickname resolves; a same-named symbol does not"
          (ok (%ref-named report "nickname-caller"))
          (ok (null (%ref-named report "other-caller")))))))

(deftest code-find-references-report-finds-what-xref-cannot
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (let ((report (%xref-fixture-report)))
        (testing "a top-level use comes from the source scan alone"
          (let ((ref (%ref-named report "*top-level-use*")))
            (ok ref)
            (ok (and ref (equal "source" (gethash "origin" ref))))
            (ok (and ref (equal "defparameter" (gethash "form_type" ref))))
            (ok (and ref (search "not in xref" (gethash "note" ref))))))
        (testing "a macro template is labelled; the call it expands into comes from xref"
          (let ((template (%ref-named report "with-target"))
                (hidden (find-if (lambda (ref)
                                   (search "macro-hidden-caller" (gethash "caller" ref)))
                                 (gethash "refs" report))))
            (ok (and template
                     (equal '("template")
                            (map 'list (lambda (site) (gethash "kind" site))
                                 (gethash "call_sites" template)))))
            (ok hidden)
            (ok (and hidden (equal "xref" (gethash "origin" hidden))))
            (ok (and hidden (zerop (length (gethash "call_sites" hidden)))))
            (ok (and hidden (search "macro expansion" (gethash "note" hidden))))))
        (testing "a flet of the same name is flagged as shadowing"
          (let ((ref (%ref-named report "shadowing-caller")))
            (ok ref)
            (ok (and ref
                     (plusp (length (gethash "call_sites" ref)))
                     (every (lambda (site) (equal "flet" (gethash "shadowed_by" site)))
                            (gethash "call_sites" ref))))))
        (testing "a call inside a deftest is attributed to the test"
          (let ((ref (%ref-named report "target-is-called-from-a-test")))
            (ok ref)
            (ok (and ref (equal "xref+source" (gethash "origin" ref))))
            (ok (and ref (equal "target-is-called-from-a-test"
                                (gethash "name" (gethash "test" ref)))))
            (ok (find "target-is-called-from-a-test" (gethash "tests" report)
                      :key (lambda (test) (gethash "name" test)) :test #'equal)))))))

(deftest code-find-references-report-flags-xref-kinds-no-site-shows
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (flet ((mentions (refs name)
               ;; Every reference that is about NAME: by its form, or by its caller.
               (remove-if-not (lambda (ref)
                                (or (equal name (gethash "form_name" ref))
                                    (string-equal name (gethash "caller" ref))))
                              refs))
             (kinds (ref)
               (map 'list (lambda (site) (gethash "kind" site)) (gethash "call_sites" ref))))
        (let ((refs (coerce (gethash "refs" (%xref-fixture-report)) 'list)))
          (testing "a macro-made call beside a quoted name merges, and the note names the call"
            (let* ((mine (mentions refs "quoted-and-macro-caller"))
                   (ref (first mine))
                   (note (and ref (gethash "note" ref))))
              (ok (= 1 (length mine)) "one reference, not split")
              (ok (and ref (equal "xref+source" (gethash "origin" ref))))
              (ok (and ref (equal '("quoted") (kinds ref))))
              (ok (and (stringp note) (search "xref records a call" note)))))
          (testing "a quoted name given to funcall is a function site with no note"
            (let* ((mine (mentions refs "funcall-caller"))
                   (ref (first mine)))
              (ok (= 1 (length mine)))
              (ok (and ref (equal "xref+source" (gethash "origin" ref))))
              (ok (and ref (equal '("function") (kinds ref))))
              (ok (and ref (equal (list (%fixture-line "(funcall 'target 10)"))
                                  (%site-lines ref))))
              (ok (and ref (null (gethash "note" ref))))))
          (testing "a function passed to mapcar by name is not split into an xref-only reference"
            (let* ((mine (mentions refs "mapcar-caller"))
                   (ref (first mine))
                   (note (and ref (gethash "note" ref))))
              (ok (null (find "xref" mine :key (lambda (ref) (gethash "origin" ref))
                                          :test #'equal))
                  "no xref-only reference for mapcar-caller")
              (ok (= 1 (length mine)))
              (ok (and ref (equal "xref+source" (gethash "origin" ref))))
              (ok (and ref (equal '("quoted") (kinds ref))))
              (ok (and (stringp note) (search "xref records a call" note))))))
        (testing "a special variable changed by incf needs no note"
          (let* ((refs (coerce (gethash "refs" (%xref-fixture-report
                                                 "cl-mcp-xref-fixture::*target-count*"))
                               'list))
                 (mine (mentions refs "incf-caller"))
                 (ref (first mine)))
            (ok (= 1 (length mine)))
            (ok (and ref (equal "xref+source" (gethash "origin" ref))))
            (ok (and ref (equal '("reference" "set") (coerce (gethash "types" ref) 'list)))
                "SBCL records both a read and a write")
            (ok (and ref (equal '("set") (kinds ref))))
            (ok (and ref (null (gethash "note" ref)))))))))

(defparameter *xref-feature-fixture*
  (asdf:system-relative-pathname :cl-mcp
                                 "tests/fixtures/xref-feature/xref-feature-fixture.lisp")
  "Fixture that pushes a feature while it is compiled and gates one caller on it.")

(defun %scanned-fixture-report (fixture designator &key (scan-features #'identity))
  "Compile and load FIXTURE, scan its directory alone, and return DESIGNATOR's report.
The scan runs with *FEATURES* bound to SCAN-FEATURES applied to *FEATURES* as it
is after loading, standing in for a parent whose features differ from the
compiling image's; the report, which evaluates reader conditionals the way the
worker does, runs with *FEATURES* as it is."
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (%load-xref-fixture fixture)
    (let ((scan (let ((*features* (funcall scan-features *features*)))
                  (scan-project designator
                                :root (uiop:pathname-directory-pathname fixture)))))
      (code-find-references-report designator :limit 1000 :scan scan))))

(defun %ok-caller-meets-its-own-form (report fixture package-name name call)
  "Check that the function NAME in REPORT is one xref+source reference of its own.
FIXTURE holds its one call site, whose text is CALL; PACKAGE-NAME is the package
its caller_symbol is qualified with.  A caller whose xref entry missed its form
shows up twice -- once from xref, once from the scan -- or on another form."
  (let ((mentions (remove-if-not (lambda (ref)
                                   (or (equal name (gethash "form_name" ref))
                                       (string-equal name (gethash "caller" ref))))
                                 (coerce (gethash "refs" report) 'list)))
        (ref (%ref-named report name)))
    (ok (= 1 (length mentions)) (format nil "~A is one reference, not split" name))
    (ok (and ref (equal "xref+source" (gethash "origin" ref)))
        (format nil "~A meets its xref entry" name))
    (ok (and ref (equal (list (%fixture-line call fixture)) (%site-lines ref)))
        (format nil "~A carries its own call site" name))
    (ok (and ref (equal (format nil "~A::~:@(~A~)" package-name name)
                        (gethash "caller_symbol" ref)))
        (format nil "~A's caller_symbol names its own form" name))
    (ok (and ref (null (gethash "note" ref)))
        (format nil "~A carries no note" name))))

(defun %xref-feature-fixture-report ()
  "Return the report for the feature fixture's FEATURE-CALLEE, scanned without its feature.
The fixture is compiled with its feature pushed, as the image that loads a
system sees it; the scan runs with *FEATURES* lacking the feature, as a parent
that never loaded the system does, so the scan counts one top-level form fewer
than SBCL.  The feature is removed again afterwards unless it was already there."
  (let ((feature :cl-mcp-xref-feature-fixture-on))
    (let ((had-feature (member feature *features*)))
      (unwind-protect
           (%scanned-fixture-report *xref-feature-fixture*
                                    "cl-mcp-xref-feature-fixture:feature-callee"
                                    :scan-features (lambda (features)
                                                     (remove feature features)))
        (unless had-feature
          (setf *features* (remove feature *features*)))))))

(deftest code-find-references-report-survives-a-feature-the-scan-lacks
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (let ((report (%xref-feature-fixture-report)))
        (testing "each caller after the gated form meets its own xref entry"
          (%ok-caller-meets-its-own-form report *xref-feature-fixture*
                                         "CL-MCP-XREF-FEATURE-FIXTURE"
                                         "caller-a" "(feature-callee 2)")
          (%ok-caller-meets-its-own-form report *xref-feature-fixture*
                                         "CL-MCP-XREF-FEATURE-FIXTURE"
                                         "caller-b" "(feature-callee 3)")))))

(defparameter *xref-gates-fixture*
  (asdf:system-relative-pathname :cl-mcp "tests/fixtures/xref-gates/xref-gates-fixture.lisp")
  "Fixture whose callers follow forms false on SBCL, a long comment, and a scan-only form.")

(deftest code-find-references-report-sees-past-skipped-forms-and-long-comments
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (let* ((feature :cl-mcp-xref-gates-scan-only)
             (report (%scanned-fixture-report *xref-gates-fixture*
                                              "cl-mcp-xref-gates-fixture:gate-callee"
                                              :scan-features (lambda (features)
                                                               (cons feature features)))))
        (flet ((check (name call)
                 (%ok-caller-meets-its-own-form report *xref-gates-fixture*
                                                "CL-MCP-XREF-GATES-FIXTURE" name call)))
          (testing "a form false on SBCL before a caller is skipped with its conditional"
            (check "after-commented-out" "(gate-callee 1)")
            (check "after-not-sbcl" "(gate-callee 2)"))
          (testing "a false conditional over a true one skips one form, not the caller after it"
            (check "after-false-over-true" "(gate-callee 6)"))
          (testing "a comment longer than 1024 characters before a caller is walked past"
            (check "after-long-comment" "(gate-callee 3)"))
          (testing "a form only the scan read does not take the next caller's xref entry"
            (ok (null (member feature *features*)) "the feature was added for the scan only")
            (check "after-scan-only" "(gate-callee 5)")
            (let ((scan-only (%ref-named report "scan-only-caller")))
              (ok (and scan-only (equal "source" (gethash "origin" scan-only)))
                  "the form the compiler skipped is reported from the scan alone")
              (ok (and scan-only (null (gethash "caller_symbol" scan-only))))))))))

(deftest code-find-references-report-never-interns
  (testing "a missing symbol is reported and left uninterned"
    (let ((report (code-find-references-report
                   "cl-mcp/src/code-core::%no-such-function-xyz")))
      (ok (equal "not_found" (gethash "symbol_status" report)))
      (ok (null (nth-value 1 (find-symbol "%NO-SUCH-FUNCTION-XYZ" "CL-MCP/SRC/CODE-CORE"))))))
  (testing "a single colon reaches an internal symbol"
    (ok (equal "found"
               (gethash "symbol_status"
                        (code-find-references-report "cl-mcp/src/code-core:%parse-symbol"))))))

(deftest scan-status-classifies-entries
  (testing "a file under root but outside scanned_files is not-scanned"
    (let ((scan (make-ht "root" "/proj/"
                         "scanned_files" (vector "/proj/a.lisp")
                         "parse_failures" #()
                         "skipped_reason" nil
                         "truncated_at" nil)))
      (ok (eq :not-scanned
              (cl-mcp/src/code-core::%scan-status (list :truename "/proj/b.lisp") scan)))))
  (testing "a file in scanned_files is scanned"
    (let ((scan (make-ht "root" "/proj/"
                         "scanned_files" (vector "/proj/a.lisp")
                         "parse_failures" #()
                         "skipped_reason" nil
                         "truncated_at" nil)))
      (ok (eq :scanned
              (cl-mcp/src/code-core::%scan-status (list :truename "/proj/a.lisp") scan)))))
  (testing "a file in parse_failures is parse-failed"
    (let ((scan (make-ht "root" "/proj/"
                         "scanned_files" #()
                         "parse_failures" (vector (make-ht "abs_path" "/proj/c.lisp"))
                         "skipped_reason" nil
                         "truncated_at" nil)))
      (ok (eq :parse-failed
              (cl-mcp/src/code-core::%scan-status (list :truename "/proj/c.lisp") scan)))))
  (testing "no scan at all is not-scanned"
    (ok (eq :not-scanned
            (cl-mcp/src/code-core::%scan-status (list :truename "/proj/a.lisp") nil)))))

(deftest scan-notes-count-denied-files
  (flet ((notes (denied)
           (cl-mcp/src/code-core::%scan-notes
            (make-ht "root" "/proj/"
                     "files_denied" denied
                     "scanned_files" #()
                     "parse_failures" #()
                     "skipped_reason" nil
                     "truncated_at" nil))))
    (testing "files the read policy denied are counted, without their paths"
      (ok (equal '("2 files outside the readable paths were not scanned") (notes 2)))
      (ok (equal '("1 file outside the readable paths was not scanned") (notes 1))))
    (testing "no note when nothing was denied, or the scan predates the count"
      (ok (null (notes 0)))
      (ok (null (notes nil))))))

(defparameter *clos-fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-fixture.lisp")
  "CLOS definitions compiled so that SBCL records their source locations.")

(defun %compile-and-load-under-own-name (file)
  "Compile and load FILE with its truename as the source namestring.
repl-eval wraps evaluation in a compilation unit that names every file it
compiles \"repl-eval\"; overriding the unit keeps FILE's own path when the
tests run from there."
  (let ((truename (truename file)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %find-definition-sources (package-name name kind)
  "Return SB-INTROSPECT's definition sources of kind KIND for PACKAGE-NAME::NAME."
  (uiop:symbol-call :sb-introspect :find-definition-sources-by-name
                    (find-symbol name package-name) kind))

(deftest code-find-definition-returns-lines-of-classes
  (testing "defclass, define-condition and defstruct get the line of their form"
    (%compile-and-load-under-own-name *clos-fixture*)
    (dolist (case '(("cl-mcp-clos-fixture:circle" "(defclass circle")
                    ("cl-mcp-clos-fixture:probe-error" "(define-condition probe-error")
                    ("cl-mcp-clos-fixture:point" "(defstruct point")))
      (destructuring-bind (designator needle) case
        (multiple-value-bind (path line) (code-find-definition designator)
          (ok (search "tests/fixtures/clos-fixture.lisp" path) designator)
          (ok (eql (%fixture-line needle *clos-fixture*) line) designator))))))

(deftest definition-source-line-resolves-methods-and-accessors
  (testing "a method and a slot accessor resolve to their own top-level form"
    (%compile-and-load-under-own-name *clos-fixture*)
    (flet ((method-lines (name)
             (sort (mapcar (lambda (method)
                             (definition-source-line
                              (uiop:symbol-call :sb-introspect :find-definition-source method)))
                           (sb-mop:generic-function-methods (fdefinition name)))
                   #'<)))
      (ok (equal (sort (list (%fixture-line "(defgeneric area" *clos-fixture*)
                             (%fixture-line "(defmethod area ((shape circle" *clos-fixture*)
                             (%fixture-line "(defmethod area ((shape square" *clos-fixture*)
                             (%fixture-line "(defmethod area :around" *clos-fixture*))
                       #'<)
                 (method-lines (find-symbol "AREA" "CL-MCP-CLOS-FIXTURE"))))
      (ok (equal (list (%fixture-line "(defclass circle" *clos-fixture*))
                 (method-lines (find-symbol "RADIUS" "CL-MCP-CLOS-FIXTURE")))))))

(deftest definition-source-location-reports-path-line-and-staleness
  (testing "absolute truename, display path and line for a file definition"
    (%compile-and-load-under-own-name *clos-fixture*)
    (multiple-value-bind (abs-path path line stale)
        (definition-source-location
         (first (%find-definition-sources "CL-MCP-CLOS-FIXTURE" "SQUARE" :class)))
      (ok (equal (namestring (truename *clos-fixture*)) abs-path))
      (ok (search "tests/fixtures/clos-fixture.lisp" path))
      (ok (eql (%fixture-line "(defclass square" *clos-fixture*) line))
      (ok (null stale))))
  (testing "no source means no location"
    (ok (equal '(nil nil nil nil) (multiple-value-list (definition-source-location nil)))))
  (testing "a file written after it was compiled is stale"
    (let ((file (asdf/system:system-relative-pathname
                 :cl-mcp "tests/tmp/clos-stale-fixture.lisp")))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out "(defpackage #:cl-mcp-clos-stale-fixture (:use #:cl))~%~
(in-package #:cl-mcp-clos-stale-fixture)~%~
(defclass stale-probe () ())~%~
(defun stale-probe-function () 1)~%"))
      (unwind-protect
           (progn
             (%compile-and-load-under-own-name file)
             ;; utimes takes Unix time; FILE-WRITE-DATE is universal time.
             (let ((later (+ (- (file-write-date file) 2208988800) 100)))
               (uiop:symbol-call :sb-posix :utimes (namestring (truename file)) later later))
             (ok (nth-value 3 (definition-source-location
                               (first (%find-definition-sources "CL-MCP-CLOS-STALE-FIXTURE"
                                                                "STALE-PROBE" :class))))))
        (ignore-errors (delete-file file))))))

(deftest read-form-starts-counts-forms-as-the-compiler-does
  (testing "a form a reader conditional excludes leaves no position"
    (let ((file (asdf/system:system-relative-pathname :cl-mcp "tests/tmp/read-form-starts.lisp")))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
        (format out ";;; 日本語のコメント~%(defun one () 1)~%#+(or) (defun never () 0)~%~
#-sbcl (defun not-sbcl () 0)~%#+sbcl~%(defun two () 2)~%#| block~%comment |#~%~
(defparameter *three* #.(+ 1 2))~%#+(or) #+sbcl (defun stacked () 0)~%~
(defun four () (list #\\) \"str)ing\" '|a b|))~%"))
      (unwind-protect
           (let ((starts (%read-form-starts file)))
             (ok (= 4 (length starts)))
             (ok (equal '(2 6 9 11)
                        (map 'list (lambda (start) (%offset->line file start)) starts))))
        (ignore-errors (delete-file file))))))

(deftest definition-source-line-survives-collected-code
  (testing "a file holding only a class still yields a line after a full GC"
    (let ((file (asdf/system:system-relative-pathname :cl-mcp "tests/tmp/clos-classes-only.lisp")))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out "(defpackage #:cl-mcp-clos-classes-only (:use #:cl))~%~
(in-package #:cl-mcp-clos-classes-only)~%~%~
(defclass only-probe ()~%  ((a :initarg :a :accessor only-a)))~%"))
      (unwind-protect
           (progn
             (%compile-and-load-under-own-name file)
             ;; Once collected, the file's debug source and its recorded form
             ;; positions are gone, and the line has to come from reading it.
             (uiop:symbol-call :sb-ext :gc :full t)
             (ok (eql 4 (definition-source-line
                         (first (%find-definition-sources "CL-MCP-CLOS-CLASSES-ONLY"
                                                          "ONLY-PROBE" :class))))))
        (ignore-errors (delete-file file))))))

(deftest generic-function-method-count-counts-methods
  (testing "a generic function's method count, and NIL for anything else"
    (%compile-and-load-under-own-name *clos-fixture*)
    (ok (eql 4 (generic-function-method-count "cl-mcp-clos-fixture:area")))
    (ok (null (generic-function-method-count "cl-mcp-clos-fixture:circle")))
    (ok (null (generic-function-method-count "cl:car")))
    (ok (null (generic-function-method-count "cl-mcp-clos-fixture::no-such-counted-name")))
    (ok (null (find-symbol "NO-SUCH-COUNTED-NAME" "CL-MCP-CLOS-FIXTURE")))))

(deftest generic-function-method-count-tolerates-unresolvable-names
  (testing "a keyword or an unparseable designator counts as no generic function"
    (ok (null (handler-case (generic-function-method-count ":test")
                (error (e) e))))
    (ok (null (handler-case (generic-function-method-count "#:uninterned")
                (error (e) e))))
    (multiple-value-bind (name type)
        (code-describe-symbol ":test")
      (declare (ignore name))
      (ok (equal "variable" type)))))
