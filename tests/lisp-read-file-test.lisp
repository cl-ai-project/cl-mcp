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
                #:ensure-directory-pathname))

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
      (ok (search "(defun version () ..." content))
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
      ;; Symbol print form depends on whether cl-mcp/src/core package is loaded.
      ;; If not preloaded, the temporary package is deleted after parsing, making
      ;; symbols uninterned so write prints them as #:version.
      (ok (or (search ": (defun version" content)
              (search ": (defun #:version" content)))
      (ok (not (search "(defun version () ...)" content))))))

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
    (let* ((result (lisp-read-file "README.md" :content-pattern "lisp-read-file"))
           (content (gethash "content" result)))
      (ok (string= (gethash "mode" result) "text-filtered"))
      (ok (stringp content))
      (ok (search "lisp-read-file" content)))))

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
