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
                #:code-find-references-report)
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
                     #-sbcl~%~
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
               (ok (= (probe-before "#-sbcl")
                      (line-of-marker "(defun after-atom-cond"))
                   "offset just before #-sbcl should report the defun's line")))
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

(defun %xref-fixture-report ()
  "Return the report for the fixture's TARGET, scanning the fixture directory."
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (%load-xref-fixture)
    (code-find-references-report
     "cl-mcp-xref-fixture:target"
     :limit 1000
     :scan (scan-project "cl-mcp-xref-fixture:target"
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

(defparameter *xref-feature-fixture*
  (asdf:system-relative-pathname :cl-mcp
                                 "tests/fixtures/xref-feature/xref-feature-fixture.lisp")
  "Fixture that pushes a feature while it is compiled and gates one caller on it.")

(defun %xref-feature-fixture-report ()
  "Return the report for the feature fixture's FEATURE-CALLEE, scanned without its feature.
The fixture is compiled with its feature pushed, as the image that loads a
system sees it; the scan runs with *FEATURES* lacking the feature, as a parent
that never loaded the system does, so the scan counts one top-level form fewer
than SBCL.  The feature is removed again afterwards unless it was already there."
  (let ((*project-root* (asdf:system-source-directory :cl-mcp))
        (feature :cl-mcp-xref-feature-fixture-on)
        (designator "cl-mcp-xref-feature-fixture:feature-callee"))
    (let ((had-feature (member feature *features*)))
      (unwind-protect
           (progn
             (%load-xref-fixture *xref-feature-fixture*)
             (let ((scan (let ((*features* (remove feature *features*)))
                           (scan-project designator
                                         :root (uiop:pathname-directory-pathname
                                                *xref-feature-fixture*)))))
               (code-find-references-report designator :limit 1000 :scan scan)))
        (unless had-feature
          (setf *features* (remove feature *features*)))))))

(deftest code-find-references-report-survives-a-feature-the-scan-lacks
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (let ((report (%xref-feature-fixture-report)))
        (testing "each caller after the gated form meets its own xref entry"
          (dolist (caller '(("caller-a" "(feature-callee 2)")
                            ("caller-b" "(feature-callee 3)")))
            (destructuring-bind (name call) caller
              (let ((ref (%ref-named report name)))
                (ok ref (format nil "~A is reported" name))
                (when ref
                  (ok (equal "xref+source" (gethash "origin" ref))
                      (format nil "~A meets its xref entry" name))
                  (ok (equal (list (%fixture-line call *xref-feature-fixture*))
                             (%site-lines ref))
                      (format nil "~A carries its own call site" name))
                  (ok (equal (format nil "CL-MCP-XREF-FEATURE-FIXTURE::~:@(~A~)"
                                     (gethash "form_name" ref))
                             (gethash "caller_symbol" ref))
                      (format nil "~A's caller_symbol names its own form" name))
                  (ok (null (gethash "note" ref))
                      (format nil "~A carries no note" name)))))))
        (testing "no caller's xref entry is left over as a reference of its own"
          (ok (notany (lambda (ref)
                        (and (equal "xref" (gethash "origin" ref))
                             (member (gethash "caller" ref) '("caller-a" "caller-b")
                                     :test #'string-equal)))
                      (gethash "refs" report)))))))

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
