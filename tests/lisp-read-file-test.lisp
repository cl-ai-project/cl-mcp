;;;; tests/lisp-read-file-test.lisp

(defpackage #:cl-mcp/tests/lisp-read-file-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok
                #:skip)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file
                #:fs-resolve-read-path)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:ensure-directory-pathname)
  (:import-from #:cl-ppcre
                #:scan))

(in-package #:cl-mcp/tests/lisp-read-file-test)

(setf cl-mcp/src/project-root:*project-root*
      (uiop:ensure-directory-pathname (system-source-directory :cl-mcp)))

(defun with-temp-lisp-file (relative content thunk)
  "Create RELATIVE Lisp file with CONTENT, run THUNK, and clean up."
  (fs-write-file relative content)
  (unwind-protect
       (funcall thunk relative)
    (ignore-errors (delete-file (fs-resolve-read-path relative)))))

(deftest lisp-read-file-collapses-lisp
  (testing "collapses Lisp definitions and reports meta"
    (let* ((result (lisp-read-file "src/core.lisp"))
           (content (gethash "content" result))
           (meta (gethash "meta" result)))
      (ok (string= (gethash "mode" result) "lisp-collapsed"))
      (ok (stringp content))
      ;; Collapsed forms now include line numbers: "  N: (defun version () ..."
      (ok (cl-ppcre:scan "\\d+: \\(defun version \\(\\)" content))
      (ok (hash-table-p meta))
      (ok (>= (gethash "total_forms" meta) 3))
      (ok (>= (gethash "expanded_forms" meta) 1))
      (ok (gethash "comment_lines" meta))
      (ok (gethash "blank_lines" meta))
      (ok (gethash "source_lines" meta)))))

(deftest lisp-read-file-expands-name-pattern
  (testing "expands matching form when name-pattern is provided"
    (let* ((result (lisp-read-file "src/core.lisp" :name-pattern "version"))
           (content (gethash "content" result)))
      (ok (stringp content))
      (ok (search "+server-version+" content))
      ;; Symbols from packages synthesized during parsing may become
      ;; homeless after the temporary package is deleted. The display
      ;; suppresses the `#:' prefix via *PRINT-GENSYM* so output is
      ;; consistent regardless of whether the package is preloaded.
      (ok (search ": (defun version" content))
      (ok (not (search "#:version" content)))
      (ok (not (search "(defun version () ...)" content))))))

(deftest lisp-read-file-no-hash-colon-prefix-pollution
  (testing "expanded body text has no bogus #: prefixes on normal symbols"
    (with-temp-lisp-file
     "tests/tmp/hash-colon-dogfood.lisp"
     (format nil "~A~%~A~%~A~%~A~%"
             "(defpackage #:dogfood-hash-colon-scratch (:use #:cl))"
             "(in-package #:dogfood-hash-colon-scratch)"
             "(defun demo (x y)"
             "  (loop for i from 0 below x always (< i y)))")
     (lambda (path)
       (let* ((result (lisp-read-file path :name-pattern "demo"))
              (content (gethash "content" result)))
         (ok (stringp content))
         (ok (search "(defun demo" content))
         ;; Body references (loop, for, always, i, x, y) must NOT be
         ;; rendered with a spurious `#:' prefix, even though the
         ;; synthesized dogfood-hash-colon-scratch package has been
         ;; deleted by the unwind-protect in call-with-package-context.
         (ok (not (search "#:for" content)))
         (ok (not (search "#:always" content)))
         (ok (not (search "#:demo" content)))
         (ok (not (search "#:x" content)))
         (ok (not (search "#:i" content))))))))

(deftest lisp-read-file-preserves-genuinely-uninterned-symbols
  (testing "#:foo in source survives as #:foo in expanded output"
    (with-temp-lisp-file
     "tests/tmp/dogfood-hash-colon-preserve.lisp"
     (format nil "~A~%~A~%~A~%~A~%~A~%~A~%"
             ";; A defpackage with #: prefixes uses uninterned symbols"
             "(defpackage #:dogfood-preserve-pkg"
             "  (:use #:cl)"
             "  (:export #:greet))"
             "(in-package #:dogfood-preserve-pkg)"
             "(defun greet (who) (format nil \"Hello, ~A\" who))")
     (lambda (path)
       (let* ((result (lisp-read-file path
                                      :name-pattern "greet"
                                      :content-pattern "dogfood-preserve-pkg"))
              (content (gethash "content" result)))
         (ok (stringp content))
         ;; Genuine #: symbols from source must survive as #: in display.
         (ok (search "#:dogfood-preserve-pkg" content)
             "genuinely uninterned package name should keep #: prefix")
         (ok (search "#:cl" content)
             "#:cl in :use should keep #: prefix")
         (ok (search "#:greet" content)
             "#:greet in :export should keep #: prefix")
         ;; But symbols that became homeless by teardown should NOT have it.
         (ok (not (search "#:who" content))
             "greet parameter 'who' should NOT acquire a spurious #: prefix")
         (ok (search "(defun greet" content)
             "defun name should render without #: prefix"))))))

(deftest lisp-read-file-raw-text-mode
  (testing "raw mode slices text by offset and limit"
    (let* ((result (lisp-read-file "README.md" :collapsed nil :offset 0 :limit 3))
           (meta (gethash "meta" result)))
      (ok (string= (gethash "mode" result) "raw"))
      (ok (hash-table-p meta))
      (ok (gethash "truncated" meta))
      (ok (>= (gethash "total_lines" meta) 3)))))

(deftest lisp-read-file-content-pattern
  (testing "content-pattern filters non-Lisp text with context"
    (with-temp-lisp-file "tests/tmp/content-pattern-test.txt"
        (format nil "line one~%this has search-target in it~%line three~%")
      (lambda (path)
        (let* ((result (lisp-read-file path :content-pattern "search-target"))
               (content (gethash "content" result)))
          (ok (string= (gethash "mode" result) "text-filtered"))
          (ok (stringp content))
          (ok (search "search-target" content)))))))

(deftest lisp-read-file-includes-preceding-comments
  (testing "include-comments surfaces leading comments in collapsed output"
    (with-temp-lisp-file "tests/tmp/lisp-read-comments.lisp"
        (format nil ";; helper comment~%(defun foo ()~%  :ok)~%")
      (lambda (path)
        (let* ((result (lisp-read-file path :include-comments t))
               (content (gethash "content" result)))
          (ok (search ";; helper comment" content))
          (ok (search "(defun foo" content)))))))

(deftest lisp-read-file-comment-context-all
  (testing "comment-context=all preserves comment positions"
    (with-temp-lisp-file "tests/tmp/lisp-read-comments-all.lisp"
        (format nil "(defun first () :one)~%;; between~%(defun second () :two)~%")
      (lambda (path)
        (let* ((content (gethash "content"
                                 (lisp-read-file path :include-comments t
                                                 :comment-context "all")))
               (first-pos (search "(defun first" content))
               (comment-pos (search ";; between" content))
               (second-pos (search "(defun second" content)))
          (ok first-pos)
          (ok comment-pos)
          (ok second-pos)
          (ok (< first-pos comment-pos))
          (ok (< comment-pos second-pos)))))))

(deftest lisp-read-file-expanded-lines-are-numbered
  (testing "expanded forms include line numbers"
    (with-temp-lisp-file "tests/tmp/lisp-read-line-numbers.lisp"
        (format nil "(defun target ()~%  :ok)~%")
      (lambda (path)
        (let* ((content (gethash "content"
                                 (lisp-read-file path :name-pattern "target")))
               (line (with-input-from-string (s content)
                       (read-line s nil "")))
               (colon (position #\: line)))
          (ok colon)
          (ok (every #'digit-char-p (subseq line 0 colon)))
          (ok (search "(defun target" line)))))))

(deftest lisp-read-file-collapsed-forms-have-line-numbers
  (testing "collapsed (non-expanded) forms include line numbers"
    (with-temp-lisp-file "tests/tmp/lisp-read-collapsed-lineno.lisp"
        (format nil "(in-package :cl-user)~%~%(defun alpha ()~%  :a)~%~%(defvar *beta* 42)~%~%(defclass gamma () ())~%")
      (lambda (path)
        (let* ((content (gethash "content"
                                 (lisp-read-file path :collapsed t)))
               (lines (with-input-from-string (s content)
                        (loop for line = (read-line s nil nil)
                              while line collect line))))
          ;; in-package is always expanded (with line numbers via %add-line-numbers)
          (ok (find-if (lambda (l) (search "(in-package" l)) lines)
              "in-package present")
          ;; All def-forms should have line numbers in collapsed mode
          (ok (find-if (lambda (l)
                         (and (search "(defun alpha" l)
                              (scan "^\\s*\\d+:" l)))
                       lines)
              "defun alpha should have line number")
          (ok (find-if (lambda (l)
                         (and (search "(defvar" l)
                              (scan "^\\s*\\d+:" l)))
                       lines)
              "defvar should have line number")
          (ok (find-if (lambda (l)
                         (and (search "(defclass" l)
                              (scan "^\\s*\\d+:" l)))
                       lines)
              "defclass should have line number"))))))

(defun %collapsed-lines (relative source)
  "Write SOURCE to RELATIVE, read it back collapsed, and return the display lines.
A collapsed definition is meant to be exactly one line, so the length of the
returned list is the assertion the signature-layout tests below are built on."
  (with-temp-lisp-file
   relative source
   (lambda (path)
     (with-input-from-string (in (gethash "content" (lisp-read-file path :collapsed t)))
       (loop for line = (read-line in nil nil)
             while line collect line)))))

(deftest lisp-read-file-collapsed-let-initializer-stays-on-one-line
  (testing "a LET initial value does not break the collapsed signature"
    ;; The dispatch table %COLLAPSE-DEF-FORM prints under is derived from
    ;; (COPY-PPRINT-DISPATCH NIL), which retains SBCL's standard layouts for
    ;; IF, LET, LET* and LOOP.  Those emit *mandatory* newlines that no
    ;; *PRINT-RIGHT-MARGIN* can suppress, and %FORMAT-LISP-FORM numbers only
    ;; the first line, so a spill leaves unnumbered continuation lines behind.
    (let* ((lines (%collapsed-lines
                   "tests/tmp/sig-let-initializer.lisp"
                   (format nil "~A~%~A~%"
                           "(defparameter *sig-let-demo*"
                           "  (let ((env \"root\")) (if env env \"fallback\")))")))
           (line (or (first lines) "")))
      (ok (= 1 (length lines))
          "the collapsed signature must occupy exactly one line")
      (ok (search "(defparameter *sig-let-demo*" line)
          "the signature names the variable")
      (ok (search "(if env" line)
          "the whole initial value stays on that line"))))

(deftest lisp-read-file-collapsed-if-default-stays-on-one-line
  (testing "an IF default in a lambda list does not break the collapsed signature"
    (let* ((lines (%collapsed-lines
                   "tests/tmp/sig-if-default.lisp"
                   (format nil "~A~%~A~%"
                           "(defun sig-if-demo (&optional (x (if alpha beta gamma)))"
                           "  x)")))
           (line (or (first lines) "")))
      (ok (= 1 (length lines))
          "the collapsed signature must occupy exactly one line")
      (ok (search "(if alpha beta gamma)" line)
          "the default form stays on that line"))))

(deftest lisp-read-file-collapsed-loop-default-stays-on-one-line
  (testing "a LOOP default in a lambda list does not break the collapsed signature"
    (let* ((lines (%collapsed-lines
                   "tests/tmp/sig-loop-default.lisp"
                   (format nil "~A~%~A~%"
                           "(defun sig-loop-demo (&key (x (loop for i below 3 collect i)))"
                           "  x)")))
           (line (or (first lines) "")))
      (ok (= 1 (length lines))
          "the collapsed signature must occupy exactly one line")
      (ok (search "(loop for i below 3 collect i)" line)
          "the loop stays on that line"))))

(deftest lisp-read-file-collapsed-keeps-quote-abbreviations
  (testing "a collapsed signature still abbreviates QUOTE and FUNCTION"
    ;; The one-line layout is obtained by routing every cons through
    ;; PPRINT-FILL.  On its own that entry also swallows the standard QUOTE
    ;; printer, regressing 'X to (quote x) and #'F to (function f), so the
    ;; standard handler is re-registered at a higher priority.  This is the
    ;; test that fails if that re-registration is ever dropped.
    (let* ((lines (%collapsed-lines
                   "tests/tmp/sig-quote-abbrev.lisp"
                   (format nil "~A~%~A~%"
                           "(defun sig-quote-demo (&optional (x '(1 2)) (y #'sig-quote-helper))"
                           "  (list x y))")))
           (line (or (first lines) "")))
      (ok (= 1 (length lines))
          "the collapsed signature must occupy exactly one line")
      (ok (search "'(1 2)" line)
          "QUOTE must still print as an apostrophe")
      (ok (not (search "(quote " line))
          "QUOTE must not fall back to its list form")
      (ok (search "#'sig-quote-helper" line)
          "FUNCTION must still print as #'")
      (ok (not (search "(function " line))
          "FUNCTION must not fall back to its list form"))))

(deftest lisp-read-file-collapsed-survives-standard-reader-backquote
  (testing "a CL:READ backquote does not exhaust the control stack"
    ;; Files read through the READTABLE argument go through CL:READ rather than
    ;; Eclector, so their backquotes arrive as SB-INT:QUASIQUOTE conses wrapping
    ;; SB-IMPL::COMMA structures -- not the ECLECTOR.READER:QUASIQUOTE heads the
    ;; table overrides by name.  The blanket PPRINT-FILL cons entry that gives a
    ;; signature its single line then descends into those structures and recurses
    ;; until the control stack is exhausted, which SBCL reports as a fatal error
    ;; rather than a catchable condition -- it takes the process down, so no
    ;; HANDLER-CASE here could soften it.  SBCL's initial table maps QUOTE,
    ;; FUNCTION and SB-INT:QUASIQUOTE to one PPRINT-QUOTE entry, so the priority-2
    ;; re-registration must name all three.  This test dies with the process if
    ;; SB-INT:QUASIQUOTE is ever dropped from that list.
    (let ((rendered
           (let ((*print-pprint-dispatch*
                  cl-mcp/src/lisp-read-file::*signature-pprint-dispatch*)
                 (*print-right-margin* 200)
                 (*print-case* :downcase))
             (write-to-string (read-from-string "`(a ,b ,@c)") :pretty t))))
      (ok (search "`(a" rendered)
          "a standard-reader backquote must still render as a backquote")
      (ok (search ",b" rendered)
          "an unquote must still render as a comma")
      (ok (not (search "quasiquote" rendered))
          "the SB-INT:QUASIQUOTE head must not leak into the signature"))))

(deftest lisp-read-file-collapsed-keeps-backquote-rendering
  (testing "a collapsed signature still renders Eclector backquote conses"
    ;; The backquote rendering lives in *SOURCE-PPRINT-DISPATCH* at priority 0,
    ;; where a blanket CONS entry would outrank it.  The signature table has to
    ;; re-register those three heads above the blanket entry, or an initial
    ;; value written with a backquote would show the raw Eclector symbols.
    (let* ((lines (%collapsed-lines
                   "tests/tmp/sig-backquote.lisp"
                   (format nil "~A~%~A~%"
                           "(defparameter *sig-bq-demo*"
                           "  `(let ((,a ,b)) ,@forms))")))
           (line (or (first lines) "")))
      (ok (= 1 (length lines))
          "the collapsed signature must occupy exactly one line")
      (ok (search "`(let ((,a ,b)) ,@forms)" line)
          "backquote, comma and comma-at must all survive")
      (ok (not (search "eclector.reader" line))
          "no eclector.reader symbol may leak into the signature"))))

(deftest lisp-read-file-collapsed-view-numbers-every-line
  (testing "no line of a collapsed view is left without a line number"
    ;; The property the per-form tests above imply, asserted once directly:
    ;; %FORMAT-LISP-FORM prefixes only the first line of a collapsed signature,
    ;; so an unnumbered line in the output means some signature spilled.
    ;;
    ;; The fixture deliberately holds no multi-line string literal.  A newline
    ;; inside a literal is content, not layout: no pretty-printer setting can
    ;; remove it and stripping it would corrupt the source, so a def form whose
    ;; third subform is such a string still spans several lines by design.
    (let ((lines (%collapsed-lines
                  "tests/tmp/sig-every-line-numbered.lisp"
                  (format nil "~{~A~%~}"
                          '("(in-package :cl-user)"
                            ""
                            "(defparameter *sig-numbered-a* (let ((q 1)) q))"
                            ""
                            "(defun sig-numbered-b (&optional (x (if alpha beta gamma)))"
                            "  x)"
                            ""
                            "(defvar *sig-numbered-c* (loop for i below 3 collect i))")))))
      (ok (>= (length lines) 4)
          "every top-level form contributes a line")
      (ok (every (lambda (line) (scan "^ *\\d+: " line)) lines)
          "an unnumbered line means a signature spilled onto a continuation line"))))

(defun %try-load (system)
  "Attempt to load SYSTEM via Quicklisp or ASDF. Returns T on success, NIL on failure."
  (handler-case
      (cond
        ((find-package :ql)
         (funcall (find-symbol "QUICKLOAD" :ql) system :silent t)
         t)
        ((asdf:find-system system nil)
         (asdf:load-system system)
         t)
        (t nil))
    (error () nil)))

(deftest lisp-read-file-with-custom-readtable
  (testing "readtable parameter enables reading files with custom reader macros"
    (handler-case
        (progn
          (unless (%try-load :cl-interpol) (error "not available"))
          (with-temp-lisp-file "tests/tmp/lisp-read-interpol.lisp"
              (format nil "(in-package :cl-user)~%~%(defun greet (name)~%  #?\"Hello, ${name}!\")~%")
            (lambda (path)
              ;; Without readtable parameter, this would fail
              (let* ((result (lisp-read-file path :readtable :interpol-syntax))
                     (content (gethash "content" result)))
                (ok (stringp content))
                (ok (search "(defun greet" content))))))
      (error ()
        (skip "cl-interpol not available")))))

(deftest lisp-read-file-ignores-blank-readtable
  (testing "blank readtable arguments are treated as omitted"
    (with-temp-lisp-file "tests/tmp/lisp-read-blank-readtable.lisp"
        (format nil "(defun greet ()~%  :hello)~%")
      (lambda (path)
        (let ((args (make-hash-table :test #'equal)))
          (setf (gethash "path" args) path
                (gethash "readtable" args) "")
          (let* ((response (cl-mcp/src/lisp-read-file::lisp-read-file-handler
                            (cl-mcp/src/state:make-state) 1 args))
                 (result (gethash "result" response))
                 (content (and result (gethash "content" result)))
                 (text (and (vectorp content)
                            (> (length content) 0)
                            (gethash "text" (aref content 0)))))
            (ok (stringp text))
            (ok (search "(defun greet" text))))))))

(deftest lisp-read-file-auto-detects-in-readtable
  (testing "in-readtable form triggers automatic readtable switching"
    (handler-case
        (progn
          (unless (%try-load :cl-interpol) (error "not available"))
          (with-temp-lisp-file "tests/tmp/lisp-read-in-readtable.lisp"
              (format nil "(in-package :cl-user)~%(named-readtables:in-readtable :interpol-syntax)~%~%(defun greet (name)~%  #?\"Hello, ${name}!\")~%")
            (lambda (path)
              ;; Without explicit readtable parameter, in-readtable should be auto-detected
              (let* ((result (lisp-read-file path))
                     (content (gethash "content" result)))
                (ok (stringp content))
                (ok (search "(defun greet" content))))))
      (error ()
        (skip "cl-interpol not available")))))

(deftest lisp-read-file-read-eval-disabled-in-package-context-scan
  (testing "package-context header scan does not execute #. forms"
    (let* ((flag-path (merge-pathnames "tests/tmp/lisp-read-read-eval-flag"
                                       (system-source-directory :cl-mcp)))
           (content
             (format nil
                     (concatenate
                      'string
                      "#.(progn "
                      "(with-open-file (s ~S :direction :output "
                      ":if-exists :supersede :if-does-not-exist :create) "
                      "(write-line \"executed\" s)) "
                      "'(in-package #:cl-user))~%"
                      "~%(defun target () :ok)~%")
                     (namestring flag-path))))
      (ignore-errors (delete-file flag-path))
      (unwind-protect
           (with-temp-lisp-file "tests/tmp/lisp-read-read-eval.lisp"
               content
             (lambda (path)
               (let ((error-message
                       (handler-case
                           (progn
                             (lisp-read-file path)
                             nil)
                         (error (e)
                           (format nil "~A" e)))))
                 (ok error-message)
                 (ok (or (search "Read-time evaluation (#.) is disabled for security"
                                 error-message)
                         (search "*READ-EVAL*" error-message)
                         (search "can't read #." error-message)))
                 (ok (not (probe-file flag-path)))))))
        (ignore-errors (delete-file flag-path)))))

(deftest lisp-read-file-collapsed-shows-method-qualifiers
  (testing "collapsed view includes method qualifiers like :before, :after, :around"
    (with-temp-lisp-file "tests/tmp/lisp-read-qualifiers.lisp"
        (format nil "(defmethod resize ((s shape) factor)~%  (* (size s) factor))~%~%(defmethod resize :before ((s shape) factor)~%  (validate s))~%~%(defmethod resize :after ((s shape) factor)~%  (notify s))~%~%(defmethod resize :around ((s shape) factor)~%  (call-next-method))~%")
      (lambda (path)
        (let* ((result (lisp-read-file path :collapsed t))
               (content (gethash "content" result)))
          ;; Primary method should show without qualifier
          (ok (search "(defmethod resize ((s shape) factor) ...)" content))
          ;; Qualified methods should show their qualifiers
          (ok (search "(defmethod resize :before ((s shape) factor) ...)" content))
          (ok (search "(defmethod resize :after ((s shape) factor) ...)" content))
          (ok (search "(defmethod resize :around ((s shape) factor) ...)" content)))))))

(deftest lisp-read-file-discovers-separate-package-definition-for-local-nicknames
  (testing "collapsed read works when package local nicknames are defined in another source file"
    (let* ((pkg-name "CL-MCP-TMP-LN-READ-USER")
           (target-name "CL-MCP-TMP-LN-READ-TARGET")
           (defs-relative "tests/tmp/lisp-read-local-nicknames-package.lisp")
           (source-relative "tests/tmp/lisp-read-local-nicknames-source.lisp")
           (defs-content
             (format nil
                     "(defpackage #:~A~%  (:use #:cl)~%  (:local-nicknames (#:ad #:~A)))~%"
                     pkg-name target-name))
           (source-content
             (format nil
                     "(in-package #:~A)~%~%~
(defun make-thing ()~%  (ad:make-dual 1.0 0.0))~%~%~
(defun other-thing ()~%  (ad:make-dual 2.0 1.0))~%"
                     pkg-name)))
      (when (find-package pkg-name)
        (delete-package pkg-name))
      (when (find-package target-name)
        (delete-package target-name))
      (fs-write-file defs-relative defs-content)
      (fs-write-file source-relative source-content)
      (unwind-protect
           (let* ((result (lisp-read-file source-relative))
                  (content (gethash "content" result)))
             (ok (null (find-package pkg-name))
                 "user package is not preloaded in parent")
             (ok (null (find-package target-name))
                 "nickname target package is not preloaded in parent")
             (ok (stringp content))
             (ok (search "(defun make-thing () ...)" content))
             (ok (search "(defun other-thing () ...)" content))
             (ok (null (find-package pkg-name))
                 "synthesized user package is cleaned up")
             (ok (null (find-package target-name))
                 "synthesized target package is cleaned up"))
        (ignore-errors (delete-file (fs-resolve-read-path defs-relative)))
        (ignore-errors (delete-file (fs-resolve-read-path source-relative)))))))

(deftest lisp-read-file-discovers-multiple-separate-local-nicknames
  (testing "collapsed read works when another file defines multiple local nicknames"
    (let* ((pkg-name "CL-MCP-TMP-LN-READ-MULTI-USER")
           (ad-target-name "CL-MCP-TMP-LN-READ-MULTI-AD-TARGET")
           (math-target-name "CL-MCP-TMP-LN-READ-MULTI-MATH-TARGET")
           (defs-relative "tests/tmp/lisp-read-local-nicknames-multi-package.lisp")
           (source-relative "tests/tmp/lisp-read-local-nicknames-multi-source.lisp")
           (defs-content
             (format nil
                     "(defpackage #:~A~%  (:use #:cl)~%  (:local-nicknames (#:ad #:~A) (#:math #:~A)))~%"
                     pkg-name ad-target-name math-target-name))
           (source-content
             (format nil
                     "(in-package #:~A)~%~%~
(defun make-thing ()~%  (ad:make-dual 1.0 0.0))~%~%~
(defun scale-thing ()~%  (math:scale 2.0 4.0))~%"
                     pkg-name)))
      (when (find-package pkg-name)
        (delete-package pkg-name))
      (when (find-package ad-target-name)
        (delete-package ad-target-name))
      (when (find-package math-target-name)
        (delete-package math-target-name))
      (fs-write-file defs-relative defs-content)
      (fs-write-file source-relative source-content)
      (unwind-protect
           (let* ((result (lisp-read-file source-relative))
                  (content (gethash "content" result)))
             (ok (null (find-package pkg-name))
                 "user package is not preloaded in parent")
             (ok (null (find-package ad-target-name))
                 "ad target package is not preloaded in parent")
             (ok (null (find-package math-target-name))
                 "math target package is not preloaded in parent")
             (ok (stringp content))
             (ok (search "(defun make-thing () ...)" content))
             (ok (search "(defun scale-thing () ...)" content))
             (ok (null (find-package pkg-name))
                 "synthesized user package is cleaned up")
             (ok (null (find-package ad-target-name))
                 "synthesized ad target package is cleaned up")
             (ok (null (find-package math-target-name))
                 "synthesized math target package is cleaned up"))
        (ignore-errors (delete-file (fs-resolve-read-path defs-relative)))
        (ignore-errors (delete-file (fs-resolve-read-path source-relative)))))))


(deftest lisp-read-file-with-package-qualified-readtable
  (testing "readtable parameter supports package-qualified symbol names (pkg:sym format)"
    (handler-case
        (progn
          (unless (%try-load :named-readtables) (error "not available"))
          (unless (%try-load :cl-interpol) (error "not available"))
          ;; Create a test package with a named readtable at runtime
          ;; to avoid parse-time errors with non-existent package-qualified symbols
          (let ((test-pkg-name "CL-MCP-TEST-PKG-QUALIFIED-RT"))
            (when (find-package test-pkg-name)
              (delete-package test-pkg-name))
            (unwind-protect
                 (progn
                   ;; Create package and register readtable dynamically
                   (eval `(defpackage ,test-pkg-name
                            (:use :cl)))
                   (eval `(in-package ,test-pkg-name))
                   ;; Copy interpol-syntax to our test package's readtable
                   ;; Use funcall to avoid read-time package resolution
                   (let ((defreadtable-fn (find-symbol "DEFREADTABLE" :named-readtables)))
                     (eval `(,defreadtable-fn
                                ,(intern "TEST-INTERPOL" test-pkg-name)
                              (:merge :interpol-syntax))))
                   (in-package :cl-mcp/tests/lisp-read-file-test)
                   ;; Now test reading with package-qualified readtable string
                   (with-temp-lisp-file "tests/tmp/lisp-read-pkg-qualified-rt.lisp"
                       (format nil "(in-package :cl-user)~%~%(defun greet-pkg (name)~%  #?\"Hello, ${name}!\")~%")
                     (lambda (path)
                       ;; Use the package-qualified format: "pkg:sym"
                       (let* ((rt-string (format nil "~A:~A" test-pkg-name "TEST-INTERPOL"))
                              (result (lisp-read-file path :readtable rt-string))
                              (content (gethash "content" result)))
                         (ok (stringp content))
                         (ok (search "(defun greet-pkg" content))))))
              ;; Cleanup: unregister readtable and delete package
              (ignore-errors
               (let ((rt-sym (find-symbol "TEST-INTERPOL" test-pkg-name))
                     (unregister-fn (find-symbol "UNREGISTER-READTABLE" :named-readtables)))
                 (when (and rt-sym unregister-fn)
                   (funcall unregister-fn rt-sym))))
              (ignore-errors (delete-package test-pkg-name)))))
      (error (e)
        (skip (format nil "Test dependencies not available: ~A" e))))))

(deftest lisp-read-file-raw-truncated-footer
  (testing "appends footer when content is truncated in raw mode"
    (with-temp-lisp-file "tests/tmp/footer-test.lisp"
        (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 20 collect i))
      (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :limit 5))
              (content (gethash "content" result)))
         (ok (search "[Showing lines 1-5 of 20." content)
             "footer should show range and total")
         (ok (search "Use offset=5 to read more.]" content)
             "footer should include next offset"))))))

(deftest lisp-read-file-raw-truncated-footer-with-offset
  (testing "footer reflects correct range when offset is used"
    (with-temp-lisp-file "tests/tmp/footer-offset-test.lisp"
        (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 30 collect i))
      (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :offset 10 :limit 5))
              (content (gethash "content" result)))
         (ok (search "[Showing lines 11-15 of 30." content)
             "footer should show offset-adjusted range")
         (ok (search "Use offset=15 to read more.]" content)
             "footer should include correct next offset"))))))

(deftest lisp-read-file-raw-no-footer-when-complete
  (testing "no footer when entire file fits within limit"
    (with-temp-lisp-file "tests/tmp/no-footer-test.lisp"
        (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 5 collect i))
      (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :limit 100))
              (content (gethash "content" result)))
         (ok (not (search "[Showing lines" content))
             "no footer when not truncated"))))))

(deftest lisp-read-file-default-limit-is-500
  (testing "default limit is 500 lines (not 2000)"
    (with-temp-lisp-file "tests/tmp/default-limit-test.lisp"
        (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 600 collect i))
      (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil))
              (content (gethash "content" result)))
         (ok (search "[Showing lines 1-500 of 600." content)
             "default limit should be 500"))))))

(deftest lisp-read-file-raw-no-footer-on-last-chunk
  (testing "no footer when last paginated chunk exhausts the file"
    (with-temp-lisp-file
     "tests/tmp/last-chunk-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 1200 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :offset 1000 :limit 500))
              (content (gethash "content" result)))
         (ok (not (search "[Showing lines" content))
             "no footer on last chunk when remaining lines fit within limit"))))))

(deftest lisp-read-file-raw-no-footer-on-exact-size
  (testing "no footer when file is exactly limit lines"
    (with-temp-lisp-file
     "tests/tmp/exact-size-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 500 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil))
              (content (gethash "content" result)))
         (ok (not (search "[Showing lines" content))
             "no footer when file is exactly 500 lines (= default limit"))))))

(deftest lisp-read-file-raw-no-footer-offset-beyond-eof
  (testing "no footer when offset is beyond end of file"
    (with-temp-lisp-file
     "tests/tmp/beyond-eof-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 10 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :offset 9999))
              (content (gethash "content" result)))
         (ok (not (search "[Showing lines" content))
             "no pagination footer when offset is beyond EOF")
         (ok (search "[Offset" content)
             "EOF message is shown when offset is beyond EOF"))))))

(deftest lisp-read-file-raw-limit-zero-signals-error
  (testing "limit=0 signals an error"
    (with-temp-lisp-file
     "tests/tmp/limit-zero-test.lisp"
     ";;; test"
     (lambda (path)
       (let ((e (handler-case (lisp-read-file path :collapsed nil :limit 0)
                  (error (e) e))))
         (ok (typep e 'cl-mcp/src/tools/helpers:arg-validation-error)
             "limit=0 should signal arg-validation-error, not a generic internal error"))))))

(deftest lisp-read-file-raw-eof-offset-message
  (testing "EOF message content when offset is past end of file"
    (with-temp-lisp-file
     "tests/tmp/eof-msg-test.lisp"
     (format nil "~{;;; line ~A~%~}" (loop for i from 1 to 10 collect i))
     (lambda (path)
       (let* ((result (lisp-read-file path :collapsed nil :offset 20))
              (content (gethash "content" result)))
         (ok (search "Offset 20" content)
             "EOF message should include the requested offset")
         (ok (search "10 total line" content)
             "EOF message should include the total line count"))))))

(deftest lisp-read-file-invalid-name-pattern-signals-error
  (testing "invalid regex in name_pattern signals arg-validation-error"
    (let ((e (handler-case (lisp-read-file "src/core.lisp" :name-pattern "(")
               (cl-mcp/src/tools/helpers:arg-validation-error (e) e))))
      (ok (typep e 'cl-mcp/src/tools/helpers:arg-validation-error)
          "invalid name_pattern regex should signal arg-validation-error")))
  (testing "invalid regex in content_pattern signals arg-validation-error"
    (let ((e (handler-case (lisp-read-file "src/core.lisp" :content-pattern "[unclosed")
               (cl-mcp/src/tools/helpers:arg-validation-error (e) e))))
      (ok (typep e 'cl-mcp/src/tools/helpers:arg-validation-error)
          "invalid content_pattern regex should signal arg-validation-error"))))

(deftest lisp-read-file-empty-path-signals-error
  (testing "empty string path signals arg-validation-error"
    (let ((e (handler-case (lisp-read-file "")
               (cl-mcp/src/tools/helpers:arg-validation-error (e) e))))
      (ok (typep e 'cl-mcp/src/tools/helpers:arg-validation-error)
          "empty path should signal arg-validation-error, not a filesystem error"))))

(deftest lisp-read-file-unbalanced-parens-helpful-error
  (testing "unbalanced parens produce a message mentioning lisp-check-parens"
    (with-temp-lisp-file "tests/tmp/unbalanced-parens.lisp"
        "(defun broken ("
      (lambda (path)
        (let ((msg (handler-case (lisp-read-file path)
                     (error (e) (format nil "~A" e)))))
          (ok msg "should signal an error on unbalanced parens")
          (ok (search "lisp-check-parens" msg)
              "error message should mention lisp-check-parens"))))))

(defun %strip-line-numbers (content)
  "Return CONTENT with the \"NNN: \" prefix that expanded forms carry removed."
  (with-output-to-string (out)
    (with-input-from-string (in content)
      (loop for line = (read-line in nil nil)
            while line
            do (multiple-value-bind (start end) (scan "^ *\\d+: " line)
                 (declare (ignore start))
                 (write-line (if end (subseq line end) line) out))))))

(deftest lisp-read-file-renders-backquote-reader-syntax
  (testing "macro bodies render with backquote, comma and comma-at"
    (with-temp-lisp-file
     "tests/tmp/backquote-render.lisp"
     (format nil "~A~%~A~%"
             "(defmacro bq-render-demo (a b &body forms)"
             "  `(let ((,a ,b)) ,@forms))")
     (lambda (path)
       (let ((content (gethash "content"
                               (lisp-read-file path
                                               :name-pattern "^bq-render-demo$"))))
         (ok (search "`(let" content)
             "quasiquote must render as a backquote")
         (ok (search "(,a ,b)" content)
             "unquote must render as a comma")
         (ok (search ",@forms" content)
             "unquote-splicing must render as ,@")
         (ok (not (search "eclector.reader" content))
             "no eclector.reader symbol may leak into the display"))))))

(deftest lisp-read-file-renders-nested-backquote
  (testing "a nested quasiquote renders with reader syntax at both levels"
    (with-temp-lisp-file
     "tests/tmp/backquote-nested.lisp"
     (format nil "~A~%~A~%"
             "(defmacro bq-nested-demo (c)"
             "  `(a `(b ,c)))")
     (lambda (path)
       (let ((content (gethash "content"
                               (lisp-read-file path
                                               :name-pattern "^bq-nested-demo$"))))
         (ok (search "`(a `(b ,c))" content)
             "the inner quasiquote must also use reader syntax")
         (ok (not (search "eclector.reader" content))
             "no eclector.reader symbol may leak into the display"))))))

(deftest lisp-read-file-backquote-round-trips
  (testing "rendered macro text reads back to the object the source reads to"
    (let ((source (format nil "~A~%~A~%~A~%"
                          "(defmacro bq-round-trip-demo (a b &body forms)"
                          "  `(let ((,a ,b))"
                          "     ,@forms))")))
      (with-temp-lisp-file
       "tests/tmp/backquote-round-trip.lisp" source
       (lambda (path)
         (let* ((content (gethash "content"
                                  (lisp-read-file
                                   path :name-pattern "^bq-round-trip-demo$")))
                (rendered (%strip-line-numbers content)))
           ;; Eclector's reader carries the primary assertion because it
           ;; yields plain conses that EQUAL can compare. CL:READ builds
           ;; SB-INT:COMMA structures, which EQUAL compares by identity,
           ;; so the CL:READ leg uses EQUALP instead.
           (ok (equal (eclector.reader:read-from-string rendered)
                      (eclector.reader:read-from-string source))
               "rendered text must read back to the same form as the source")
           (ok (equalp (read-from-string rendered)
                       (read-from-string source))
               "rendered text must read back identically under CL:READ too")))))))

(deftest lisp-read-file-malformed-backquote-cons-does-not-signal
  (testing "a literal list headed by a backquote symbol prints as a list"
    (let ((source (format nil "~A~%~A~%~A~%~A~%~A~%"
                          "(defparameter *bq-malformed-demo*"
                          "  '((eclector.reader:quasiquote)"
                          "    (eclector.reader:quasiquote a b)"
                          "    (eclector.reader:unquote . tail)"
                          "    (eclector.reader:unquote-splicing a b)))")))
      (with-temp-lisp-file
       "tests/tmp/backquote-malformed.lisp" source
       (lambda (path)
         (let ((result (handler-case (lisp-read-file
                                      path :name-pattern "bq-malformed-demo")
                         (error (e) e))))
           (ok (hash-table-p result)
               "malformed backquote conses must not signal")
           (when (hash-table-p result)
             (let ((rendered (%strip-line-numbers (gethash "content" result))))
               (ok (search "quasiquote" rendered)
                   "malformed conses fall back to ordinary list printing")
               (ok (equal (eclector.reader:read-from-string rendered)
                          (eclector.reader:read-from-string source))
                   "malformed conses must still round-trip")))))))))

(defun %render-definition (relative source name-pattern)
  "Write SOURCE to RELATIVE, expand NAME-PATTERN, and return the rendered text.
Line-number prefixes are stripped so the result can be read back."
  (with-temp-lisp-file
   relative source
   (lambda (path)
     (%strip-line-numbers
      (gethash "content" (lisp-read-file path :name-pattern name-pattern))))))

(defparameter *literal-unquote-source*
  "(defparameter *bq-literal-unquote-demo*
  '(eclector.reader:unquote value))
"
  "A top-level literal whose head is an Eclector reader symbol but which is
not reader output: no backquote encloses it, so it must print as a list.")

(deftest lisp-read-file-literal-unquote-list-is-not-converted
  (testing "an unquote list outside any backquote keeps its list form"
    (let ((rendered (%render-definition "tests/tmp/backquote-literal-unquote.lisp"
                                        *literal-unquote-source*
                                        "^\\*bq-literal-unquote-demo\\*$")))
      (ok (search "eclector.reader:unquote" rendered)
          "the literal list must still show the Eclector symbol")
      (ok (not (search "'," rendered))
          "',VALUE is (QUOTE (UNQUOTE VALUE)) and must never be emitted"))))

(deftest lisp-read-file-literal-unquote-list-reads-back
  (testing "the rendered literal unquote list is readable"
    ;; This is the property the bug violated: the tool emitted ',VALUE, which
    ;; CL:READ rejects with "Comma not inside a backquote". Asserting on the
    ;; text alone would not have caught a different unreadable spelling, so
    ;; readability is asserted directly.
    (let ((rendered (%render-definition "tests/tmp/backquote-literal-readback.lisp"
                                        *literal-unquote-source*
                                        "^\\*bq-literal-unquote-demo\\*$")))
      (ok (handler-case (progn (read-from-string rendered) t)
            (error () nil))
          "rendered text must read back without signalling")
      (ok (equal (eclector.reader:read-from-string rendered)
                 (eclector.reader:read-from-string *literal-unquote-source*))
          "and must read back to the object the source reads to"))))

(deftest lisp-read-file-unquote-converts-only-inside-a-backquote
  (testing "commas convert inside a backquote and not in data below one"
    ;; The same macro exercises both sides of the rule: `,A', `,B' and
    ;; `,@FORMS' sit at depth 1 and must convert, while the quoted list under
    ;; `,(list ...)' sits back at depth 0 -- one comma consumed one level --
    ;; and must not.
    (let* ((source "(defmacro bq-scope-demo (a b &body forms)
  `(let ((,a ,b))
     ,@forms
     ,(list '(eclector.reader:unquote raw))))
")
           (rendered (%render-definition "tests/tmp/backquote-scope.lisp"
                                         source "^bq-scope-demo$")))
      (ok (search "`(let" rendered)
          "quasiquote must render as a backquote")
      (ok (search "(,a ,b)" rendered)
          "unquote inside the backquote must render as a comma")
      (ok (search ",@forms" rendered)
          "unquote-splicing inside the backquote must render as ,@")
      (ok (search "'(eclector.reader:unquote raw)" rendered)
          "the literal below a comma is back at depth 0 and must not convert")
      (ok (equal (eclector.reader:read-from-string rendered)
                 (eclector.reader:read-from-string source))
          "the whole rendering must read back to the source object"))))

(deftest lisp-read-file-nested-quasiquote-tracks-depth
  (testing "a doubly nested quasiquote converts at both levels"
    ;; ``(A ,,C) reaches depth 2, so both commas are inside a backquote and
    ;; both must convert. A boolean "inside a quasiquote" flag would also pass
    ;; this; the depth is what also makes the depth-0 case above pass.
    (let* ((source "(defmacro bq-depth-demo (c)
  ``(a ,,c))
")
           (rendered (%render-definition "tests/tmp/backquote-depth.lisp"
                                         source "^bq-depth-demo$")))
      (ok (search "``(a ,,c)" rendered)
          "both quasiquote levels and both commas must use reader syntax")
      (ok (not (search "eclector.reader" rendered))
          "no eclector.reader symbol may leak into the display")
      (ok (equal (eclector.reader:read-from-string rendered)
                 (eclector.reader:read-from-string source))
          "the rendering must read back to the source object"))))

(deftest source-pprint-dispatch-is-rebuilt-on-reload
  (testing "reloading the source file re-registers the dispatch entries"
    ;; *SOURCE-PPRINT-DISPATCH* registers every entry in its initializer, so it
    ;; must not be a DEFVAR: DEFVAR leaves an already-bound variable alone, and
    ;; an image that had loaded an older version of the file would keep the old
    ;; table and silently ignore the new entries until a full restart.
    ;;
    ;; No other test can see this, because every test run starts from a fresh
    ;; image where the variable is unbound and DEFVAR and DEFPARAMETER behave
    ;; identically. So the pre-reload state is set up explicitly: install a
    ;; table that lacks the backquote entries -- standing in for the older
    ;; version -- and then reload the source file.  *SIGNATURE-PPRINT-DISPATCH*
    ;; is checked alongside it because it is built from the same entries and
    ;; carries exactly the same hazard.
    ;;
    ;; The reload is a plain LOAD rather than (ASDF:LOAD-SYSTEM ... :FORCE T).
    ;; The umbrella suite runs inside ASDF:OPERATE (test-op), and ASDF refuses
    ;; :FORCE in a nested OPERATE call, so the load-system spelling passes
    ;; under `rove tests/lisp-read-file-test.lisp' and errors under
    ;; `rove cl-mcp.asd'.  LOAD re-evaluates every top-level form in the file,
    ;; which is precisely the DEFPARAMETER-vs-DEFVAR distinction under test.
    (let ((saved cl-mcp/src/lisp-read-file::*source-pprint-dispatch*)
          (saved-signature cl-mcp/src/lisp-read-file::*signature-pprint-dispatch*)
          (form (list 'eclector.reader:quasiquote
                      (list 'a (list 'eclector.reader:unquote 'b)))))
      (unwind-protect
           (progn
             (setf cl-mcp/src/lisp-read-file::*source-pprint-dispatch*
                   (copy-pprint-dispatch nil)
                   cl-mcp/src/lisp-read-file::*signature-pprint-dispatch*
                   (copy-pprint-dispatch nil))
             (let ((stale (cl-mcp/src/lisp-read-file::%form->string form))
                   (stale-signature
                    (cl-mcp/src/lisp-read-file::%collapse-def-form
                     (list 'defparameter '*stale-demo* form))))
               (ok (search "eclector.reader" stale)
                   "precondition: the stale table renders the raw cons")
               (ok (search "eclector.reader" stale-signature)
                   "precondition: the stale signature table renders it too"))
             (let ((*standard-output* (make-broadcast-stream))
                   (*error-output* (make-broadcast-stream)))
               (handler-bind ((warning
                                (lambda (c)
                                  (let ((restart (find-restart 'muffle-warning c)))
                                    (when restart (invoke-restart restart))))))
                 (load (asdf:system-relative-pathname
                        "cl-mcp" "src/lisp-read-file.lisp"))))
             (let ((reloaded (cl-mcp/src/lisp-read-file::%form->string form))
                   (reloaded-signature
                    (cl-mcp/src/lisp-read-file::%collapse-def-form
                     (list 'defparameter '*stale-demo* form))))
               (ok (not (search "eclector.reader" reloaded))
                   "the reload must re-register the backquote entries")
               (ok (search "`" reloaded)
                   "quasiquote must render as a backquote again")
               (ok (search "," reloaded)
                   "unquote must render as a comma again")
               (ok (not (search "eclector.reader" reloaded-signature))
                   "the same reload must rebuild the signature table")
               (ok (search "`" reloaded-signature)
                   "and a signature must render the backquote again too")))
        (setf cl-mcp/src/lisp-read-file::*source-pprint-dispatch* saved
              cl-mcp/src/lisp-read-file::*signature-pprint-dispatch*
              saved-signature)))))

(deftest lisp-read-file-expanded-form-echoes-the-file
  ;; Expanded forms used to be re-printed from the CST rather than echoed, and
  ;; two things broke because of it.  lisp-patch-form matches old_text as raw,
  ;; whitespace-sensitive text, so an agent copying an expanded line straight
  ;; out of this tool got "not found" whenever the printer had reflowed it.
  ;; And each printed line still carried a number that read as a file line, so
  ;; a form the printer reflowed shifted every number after it -- silently.
  (with-temp-lisp-file
   "tests/tmp/raw-echo-demo.lisp"
   (format nil "~{~A~%~}"
           (list "(defgeneric raw-echo-demo (x)"
                 "  (:documentation \"Doc.\"))"
                 ""
                 "(defun raw-echo-caller (x)"
                 "  ;; an inner comment"
                 "  (restart-case (raw-echo-demo x)"
                 "    (:give-up ()"
                 "      :report \"stop\""
                 "      nil)))"))
   (lambda (path)
     (let ((content (gethash "content"
                             (lisp-read-file path
                                             :name-pattern "^raw-echo-caller$"))))
       (testing "the file's own text is echoed, not a re-print of it"
         (ok (search "(:give-up ()" content)
             "an empty lambda list stays () rather than becoming NIL")
         (ok (null (search "(:give-up nil" content))))
       (testing "a comment inside the form survives expansion"
         (ok (search ";; an inner comment" content)))
       (testing "each numbered line is that line of the file"
         (ok (search "4: (defun raw-echo-caller (x)" content))
         (ok (search "5:   ;; an inner comment" content))
         (ok (search "7:     (:give-up ()" content))
         (ok (search "9:       nil)))" content)))))))

(deftest lisp-read-file-expanded-form-keeps-numbers-aligned-with-the-file
  ;; The regression that motivated the previous test: a defgeneric the printer
  ;; splits across three lines where the file uses two.  Every number after the
  ;; split pointed one line too far -- here at a line the file leaves blank.
  (with-temp-lisp-file
   "tests/tmp/raw-echo-defgeneric.lisp"
   (format nil "~{~A~%~}"
           (list "(defgeneric spanning-demo (x)"
                 "  (:documentation \"Doc.\"))"
                 ""
                 "(defun after-the-generic () 42)"))
   (lambda (path)
     (let ((content (gethash "content"
                             (lisp-read-file path
                                             :name-pattern "^spanning-demo$"))))
       (testing "the two source lines stay two lines, numbered 1 and 2"
         (ok (search "1: (defgeneric spanning-demo (x)" content))
         (ok (search "2:   (:documentation \"Doc.\"))" content)))
       (testing "nothing is reported on line 3, which the file leaves blank"
         (ok (null (search "3:   (:documentation" content))))))))

(deftest lisp-read-file-content-pattern-sees-backquote-reader-syntax
  ;; Now that expanded forms are echoed rather than printed, the display no
  ;; longer exercises the source printer -- the three tests above pass by
  ;; construction. content_pattern still matches against the *printed* form,
  ;; so that a pattern describes structure rather than the file's whitespace,
  ;; and that is the path a quasiquote-printing regression would break: the
  ;; body would render as eclector.reader internals and stop matching.
  (with-temp-lisp-file
   "tests/tmp/backquote-content-match.lisp"
   (format nil "~{~A~%~}"
           (list "(defmacro bq-content-demo (a b &body forms)"
                 "  `(let ((,a ,b)) ,@forms))"
                 ""
                 "(defun bq-content-other () :unrelated)"))
   (lambda (path)
     (testing "a comma-at in a macro body is matchable through content_pattern"
       (let ((content (gethash "content"
                               (lisp-read-file path :content-pattern ",@forms"))))
         (ok (search "bq-content-demo" content))
         (ok (search ",@forms" content)
             "the matched form is echoed with its reader syntax intact")))
     (testing "a pattern naming eclector internals matches nothing"
       (let ((content (gethash "content"
                               (lisp-read-file path
                                               :content-pattern "eclector"))))
         (ok (null (search "`(let" content))
             "no form expanded, so the macro body is not shown"))))))
