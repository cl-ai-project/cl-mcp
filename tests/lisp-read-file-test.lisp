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
      (ok (search ": (defun version" content))
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
