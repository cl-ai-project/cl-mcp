;;;; tests/package-context-test.lisp
;;;;
;;;; Unit tests for the parent-side package-context synthesis layer.
;;;; These functions reconstruct enough package metadata from source files to
;;;; activate package-local nicknames in the parent process while the worker
;;;; remains isolated. The suite covers:
;;;;   - text-level IN-PACKAGE extraction (various designator forms)
;;;;   - DISCOVER-PACKAGE-SPEC walking source files via *project-root*
;;;;   - CALL-WITH-PACKAGE-CONTEXT binding *package* and cleaning up synthesized
;;;;     packages on exit (both normal and non-local)

(defpackage #:cl-mcp/tests/package-context-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/package-context
                #:*package-spec-discovery-cache*
                #:extract-in-package-name-from-text
                #:discover-package-spec
                #:call-with-package-context
                #:call-with-file-package-context
                #:package-spec-name
                #:package-spec-nicknames
                #:package-spec-use
                #:package-spec-local-nicknames
                #:package-spec-source-path))

(in-package #:cl-mcp/tests/package-context-test)

(defun call-with-temp-project (thunk)
  "Bind *project-root* to a fresh temp dir and pass it to THUNK."
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "/tmp/cl-mcp-pkgctx-~A/" (random 1000000)))))
    (ensure-directories-exist dir)
    (let ((real-dir (truename dir)))
      (unwind-protect
           (let ((*project-root* real-dir))
             (funcall thunk real-dir))
        (uiop:delete-directory-tree real-dir
                                    :validate t
                                    :if-does-not-exist :ignore)))))

(defun write-source (root rel-path content)
  "Write CONTENT to ROOT/REL-PATH and return the absolute pathname."
  (let ((path (merge-pathnames rel-path root)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string content s))
    path))

(deftest extract-in-package-keyword
 (testing "extracts the symbol name from a keyword IN-PACKAGE form"
  (let ((name (extract-in-package-name-from-text
               "(in-package :my-app)~%(defun foo () 1)")))
    (ok (and (stringp name) (string-equal name "MY-APP"))))))

(deftest extract-in-package-string
 (testing "extracts from a string IN-PACKAGE form"
  (let ((name (extract-in-package-name-from-text
               "(in-package \"my-app/sub\")")))
    (ok (and (stringp name) (string-equal name "MY-APP/SUB"))))))

(deftest extract-in-package-uninterned-symbol
 (testing "extracts from a #: uninterned symbol designator"
  (let ((name (extract-in-package-name-from-text
               "(in-package #:my-app)")))
    (ok (and (stringp name) (string-equal name "MY-APP"))))))

(deftest extract-in-package-missing
 (testing "returns NIL when no IN-PACKAGE form is present"
  (ok (null (extract-in-package-name-from-text
             ";; just a comment~%(defun foo () 1)")))))

(deftest discover-package-spec-finds-defpackage
 (testing "discover-package-spec walks project sources to find a defpackage"
  (call-with-temp-project
   (lambda (root)
     (write-source root "src/foo.lisp"
                   "(defpackage #:my-discover-pkg
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package #:my-discover-pkg)")
     (let ((spec (discover-package-spec "MY-DISCOVER-PKG")))
       (ok spec "spec found")
       (when spec
         (ok (string-equal (package-spec-name spec) "MY-DISCOVER-PKG"))
         (ok (consp (package-spec-local-nicknames spec))
          "local nicknames preserved")))))))

(deftest discover-package-spec-returns-nil-when-absent
 (testing "discover-package-spec returns NIL when no defpackage matches"
  (call-with-temp-project
   (lambda (root)
     (write-source root "src/empty.lisp" "(defun foo () 1)")
     (ok (null (discover-package-spec "NOT-DEFINED-ANYWHERE-PKG")))))))

(defun %spec-summary (spec)
  "Return SPEC's name, nicknames, use list, local nicknames and source path, or NIL.
The source form is left out: it holds uninterned symbols, fresh on every read."
  (and spec
       (list (package-spec-name spec)
             (package-spec-nicknames spec)
             (package-spec-use spec)
             (package-spec-local-nicknames spec)
             (namestring (package-spec-source-path spec)))))

(deftest discover-package-spec-memoizes-its-walk-when-the-cache-is-bound
  (testing "cached results equal uncached ones, and a repeat lookup reads no file again"
    (call-with-temp-project
     (lambda (root)
       (let* ((defining (write-source root "src/package.lisp"
                                      "(defpackage #:cl-mcp-test-cached-pkg
  (:use #:cl)
  (:nicknames #:cl-mcp-test-cached-nick)
  (:local-nicknames (#:a #:alexandria)))"))
              (user-a (write-source root "src/a.lisp"
                                    "(in-package #:cl-mcp-test-cached-pkg)
(defun a () 1)"))
              (user-b (write-source root "src/b.lisp"
                                    "(in-package #:cl-mcp-test-cached-pkg)
(defun b () 2)"))
              (uncached (discover-package-spec "CL-MCP-TEST-CACHED-PKG" :source-path user-a))
              (cache (make-hash-table :test #'equal)))
         (ok uncached "the package is found without the cache")
         (let ((*package-spec-discovery-cache* cache))
           (let ((cached (discover-package-spec "CL-MCP-TEST-CACHED-PKG" :source-path user-a)))
             (ok (equal (%spec-summary uncached) (%spec-summary cached))
                 "the cached lookup finds what the uncached one does")
             (ok (null (discover-package-spec "CL-MCP-TEST-MISSING-PKG" :source-path user-a))
                 "a package defined nowhere is still NIL")
             (ok (= 2 (hash-table-count cache)) "the found spec and the miss are both kept")
             ;; With the defining file gone, only a lookup that skips the walk
             ;; can still return the spec; with a definition added, only one
             ;; that skips the walk can still miss it.
             (delete-file defining)
             (write-source root "src/late.lisp" "(defpackage #:cl-mcp-test-missing-pkg)")
             (ok (eq cached (discover-package-spec "CL-MCP-TEST-CACHED-PKG" :source-path user-b))
                 "another file of the same package reuses the walk's result")
             (ok (null (discover-package-spec "CL-MCP-TEST-MISSING-PKG" :source-path user-b))
                 "a miss is reused too")
             (call-with-temp-project
              (lambda (other-root)
                (write-source other-root "src/package.lisp"
                              "(defpackage #:cl-mcp-test-cached-pkg (:use #:cl))")
                (let ((elsewhere (discover-package-spec "CL-MCP-TEST-CACHED-PKG")))
                  (ok (and elsewhere (not (eq cached elsewhere)))
                      "a different project root walks its own sources")
                  (ok (and elsewhere (null (package-spec-local-nicknames elsewhere)))
                      "and finds its own definition"))))))
         (ok (null (discover-package-spec "CL-MCP-TEST-CACHED-PKG" :source-path user-b))
             "without the cache the walk sees the defining file is gone")
         (ok (discover-package-spec "CL-MCP-TEST-MISSING-PKG" :source-path user-b)
             "without the cache the walk sees the added definition"))))))

(deftest call-with-package-context-existing-package
 (testing "binds *package* when target package already exists"
  (let ((seen-package nil))
    (call-with-package-context "CL-USER"
                               (lambda ()
                                 (setf seen-package *package*)))
    (ok (eq seen-package (find-package :cl-user))))))

(deftest call-with-package-context-cleans-synthesized
 (testing "synthesized packages are deleted after the body returns"
  (call-with-temp-project
   (lambda (root)
     (write-source root "src/synth.lisp"
                   "(defpackage #:cl-mcp-test-synth-pkg (:use #:cl))
(in-package #:cl-mcp-test-synth-pkg)")
     (let ((before-pkg (find-package "CL-MCP-TEST-SYNTH-PKG")))
       (when before-pkg (delete-package before-pkg)))
     (call-with-package-context "CL-MCP-TEST-SYNTH-PKG"
                                (lambda ()
                                  (ok (find-package "CL-MCP-TEST-SYNTH-PKG")
                                   "package exists during body")))
     (ok (null (find-package "CL-MCP-TEST-SYNTH-PKG"))
      "package deleted after exit")))))

(deftest call-with-package-context-cleans-on-non-local-exit
 (testing "synthesized packages are still cleaned when the body unwinds"
  (call-with-temp-project
   (lambda (root)
     (write-source root "src/synth2.lisp"
                   "(defpackage #:cl-mcp-test-synth-unwind (:use #:cl))
(in-package #:cl-mcp-test-synth-unwind)")
     (let ((before-pkg (find-package "CL-MCP-TEST-SYNTH-UNWIND")))
       (when before-pkg (delete-package before-pkg)))
     (handler-case
         (call-with-package-context "CL-MCP-TEST-SYNTH-UNWIND"
                                    (lambda ()
                                      (error "intentional")))
       (error () nil))
     (ok (null (find-package "CL-MCP-TEST-SYNTH-UNWIND"))
      "package deleted after non-local exit")))))

(deftest discovery-survives-unreadable-vendored-file
 (testing "discovery does not abort when a vendored file signals a plain error on read"
  (call-with-temp-project
   (lambda (root)
     ;; The poison file must live OUTSIDE *PACKAGE-CONTEXT-SKIP-DIRECTORIES*,
     ;; or the walk never reads it and this test passes with the catch-all
     ;; clause deleted. `vendor/` is a real vendoring convention that the
     ;; skip list deliberately does not enumerate -- the point of the
     ;; catch-all is that the list cannot enumerate them all.
     ;;
     ;; No matching defpackage exists anywhere in this project either, so
     ;; DISCOVER-PACKAGE-SPEC's FIND-IF cannot short-circuit before reaching
     ;; the poison file.
     (write-source root "vendor/fake-lib/impl-allegro.lisp"
                   "#+(version>= 9) (defun guarded () nil)")
     (ok (null (discover-package-spec "FD010-NOT-DEFINED-ANYWHERE-PKG"))
      "returns NIL instead of signaling on the poisoned vendored file")))))

(deftest package-specs-in-file-tolerates-feature-expr-error
 (testing "%package-specs-in-file returns NIL instead of signaling on a bad feature expr"
  (call-with-temp-project
   (lambda (root)
     (let ((path (write-source root ".bundle-libs/software/fake-lib/impl-allegro.lisp"
                                "#+(version>= 9) (defun guarded () nil)")))
       (ok (null (cl-mcp/src/package-context::%package-specs-in-file path))
        "poison file yields NIL, no signal"))))))

(deftest call-with-file-package-context-uses-text
 (testing "infers package context from inline text without re-reading disk"
  (call-with-temp-project
   (lambda (root)
     (declare (ignore root))
     (let ((seen nil))
       (call-with-file-package-context "/nonexistent/foo.lisp"
                                       (lambda () (setf seen *package*))
                                       :text "(in-package :cl-user)")
       (ok (eq seen (find-package :cl-user))))))))

(defun %extract-within (seconds text)
  "Return EXTRACT-IN-PACKAGE-NAME-FROM-TEXT of TEXT, or :TIMEOUT when it has
not answered within SECONDS; the thread doing it is destroyed on the way out
whenever it is still running."
  (let* ((result nil)
         (thread (bordeaux-threads:make-thread
                  (lambda ()
                    (setf result (handler-case (extract-in-package-name-from-text text)
                                   (serious-condition (c) c))))
                  :name "package-context-test-extract-within")))
    (unwind-protect
         (progn
           (loop repeat (round (* seconds 100))
                 while (bordeaux-threads:thread-alive-p thread)
                 do (sleep 0.01d0))
           (if (bordeaux-threads:thread-alive-p thread) :timeout result))
      (when (bordeaux-threads:thread-alive-p thread)
        (ignore-errors (bordeaux-threads:destroy-thread thread))))))

(deftest extract-in-package-skips-a-circular-backquote-template
  ;; Issue #285: SBCL's reader never returns from `(a . #1=(b . #1#)) -- its
  ;; backquote reader walks the list to its last cons -- and the header scan
  ;; read every form of a file with that reader.
  (dolist (text (list (format nil "(defmacro q () `(a . #1=(b . #1#)))~%~
                                   (in-package :my-app)")
                      (format nil "(in-package :my-app)~%~
                                   (defmacro q () `(a . #1=(b . #1#)))")
                      (format nil "#+sbcl (defmacro q () `(a . #1=(b . #1#)))~%~
                                   (in-package :my-app)")
                      (format nil "#-sbcl (defmacro q () `(a . #1=(b . #1#)))~%~
                                   (in-package :my-app)")))
    (let ((name (%extract-within 2 text)))
      (ok (and (stringp name) (string-equal name "MY-APP"))
          (format nil "answered ~S for ~S" name text)))))

(deftest extract-in-package-still-finds-headers-written-other-ways
  (testing "headers behind comments, qualified, quoted or conditional still count"
    (dolist (case (list (format nil ";; header~%#| block #| nested |# |#~%~
                                     (cl:in-package :my-app)")
                        "#| c |#(in-package :my-app)"
                        "#||#(in-package :my-app)"
                        (format nil "(;c~% in-package :my-app)")
                        "(#|c|# in-package :my-app)"
                        "(|IN-PACKAGE| :my-app)"
                        (format nil "(defpackage :my-app (:use :cl))~%~
                                     (in-package :my-app)")
                        (format nil "#+sbcl (in-package :my-app)~%(defun f () 1)")
                        (format nil "#+(or) (in-package :other)~%(in-package :my-app)")))
      (let ((name (%extract-within 2 case)))
        (ok (and (stringp name) (string-equal name "MY-APP"))
            (format nil "answered ~S for ~S" name case)))))
  (testing "a form the reader would reject before the header no longer hides it"
    ;; Forms that are not headers are skipped without being read, so an
    ;; unknown package prefix in one no longer ends the scan.
    (let ((name (%extract-within 2 (format nil "(defun f () (nopkg:bar))~%~
                                                (in-package :my-app)"))))
      (ok (and (stringp name) (string-equal name "MY-APP"))
          (format nil "answered ~S" name)))))

(deftest extract-in-package-judges-feature-expressions
  (testing "nested conditionals are judged as the reader would"
    (dolist (case (list (format nil "#+sbcl #-sbcl (foo) (in-package :my-app)")
                        (format nil "#-sbcl #+sbcl (in-package :other) (in-package :my-app)")
                        "#+(and sbcl (not windows)) (in-package :my-app)"
                        "#+sb-thread (in-package :my-app)"
                        ;; In a place that takes one object: after a quote.
                        (format nil "(defun f () (foo '#+sbcl a #-sbcl b))~%~
                                     (in-package :my-app)")
                        (format nil "(defun f () (list #'#+sbcl a #-sbcl b))~%~
                                     (in-package :my-app)")))
      (let ((name (%extract-within 2 case)))
        (ok (and (stringp name) (string-equal name "MY-APP"))
            (format nil "answered ~S for ~S" name case)))))
  (testing "a feature expression FEATUREP rejects is no header and no failure"
    (dolist (pair (list (cons (format nil "#+(version>= 9) (defun g ())~%~
                                           (in-package :my-app)")
                              ;; Skipped like any form the reader would reject
                              ;; before a header, so the header is found.
                              "MY-APP")
                        (cons (format nil "(in-package :my-app)~%~
                                           #+(version>= 9) (defun g ())")
                              "MY-APP")
                        ;; Inside a form whether it yields an object decides
                        ;; where the form ends, so the scan stops there, as
                        ;; the reader would.
                        (cons (format nil "(defun f () #+(version>= 9) 1)~%~
                                           (in-package :my-app)")
                              nil)
                        (cons "#+(and . sbcl) (in-package :my-app)" nil)
                        (cons "#+" nil)))
      (let ((name (%extract-within 2 (car pair))))
        (ok (if (cdr pair)
                (and (stringp name) (string-equal name (cdr pair)))
                (null name))
            (format nil "answered ~S for ~S" name (car pair))))))
  (testing "a backquote template in a feature expression does not hang the scan"
    ;; At top level, and inside a form that is skipped: there SBCL's #+ reads
    ;; its feature expression with *READ-SUPPRESS* off.  At top level the
    ;; conditional and its object end where they end whether it holds or not;
    ;; inside a form they may not, so the scan stops.
    (dolist (pair (list (cons (format nil "#+`(a . #1=(b . #1#)) (defun f ())~%~
                                           (in-package :my-app)")
                              "MY-APP")
                        (cons (format nil "#-`(a . #1=(b . #1#)) (in-package :other)~%~
                                           (in-package :my-app)")
                              "MY-APP")
                        (cons (format nil "(defun f () #+`(a . #1=(b . #1#)) 1)~%~
                                           (in-package :my-app)")
                              nil)
                        (cons (format nil "#+sbcl (defun f () #-`(a . #1=(b . #1#)) 1)~%~
                                           (in-package :my-app)")
                              nil)))
      (let ((name (%extract-within 2 (car pair))))
        (ok (if (cdr pair)
                (and (stringp name) (string-equal name (cdr pair)))
                (null name))
            (format nil "answered ~S for ~S" name (car pair)))))))

(deftest package-specs-in-file-survives-a-feature-expression-featurep-rejects
  (testing "a bad conditional after the header keeps the package's spec"
    (call-with-temp-project
     (lambda (root)
       (let* ((file (write-source root "probe.lisp"
                                  (format nil "(defpackage :probe-pkg (:use :cl)~%  ~
                                                 (:local-nicknames (:a :alexandria)))~%~
                                               (in-package :probe-pkg)~%~
                                               #+(version>= 8 2)~%(defun g () 1)~%")))
              (specs (cl-mcp/src/package-context::%package-specs-in-file file)))
         (ok (find "PROBE-PKG" specs
                   :key #'cl-mcp/src/package-context::package-spec-name
                   :test #'string-equal)
             (format nil "the defpackage's spec is kept: ~S" specs)))))))

(deftest header-scan-refuses-circular-or-backquoted-header-forms
  (testing "a header form whose read would walk a cycle is no header and no hang"
    ;; Labels are an error in what the scan reads for real, so neither the
    ;; backquote reader nor #+, #( or #C ever meets a cycle.
    (dolist (pair '(("(in-package `(a . #1=(b . #1#)))" . nil)
                    ("(in-package #+(and . #1=(sbcl . #1#)) :p)" . nil)
                    ;; The feature expression is X or Y as the inner #+
                    ;; holds or not: no boundary can be trusted after it.
                    ("#+#+(and . #1=(sbcl . #1#)) x y (in-package :p)" . nil)
                    ("(defpackage :p (:use #+(or . #1=(nope . #1#)) :cl))" . nil)
                    ("(in-package #(a . #1=(b . #1#)))" . nil)
                    ("(in-package #C#1=(1 2 . #1#))" . nil)))
      (let ((name (%extract-within 2 (car pair))))
        (ok (if (cdr pair)
                (and (stringp name) (string-equal name (cdr pair)))
                (null name))
            (format nil "answered ~S for ~S" name (car pair))))))
  (testing "a circular header form is not walked for its package spec"
    (call-with-temp-project
     (lambda (root)
       (let* ((file (write-source root "circular-clauses.lisp"
                                  (format nil "(defpackage :p3 . #1=((:use :cl) . #1#))~%~
                                               (in-package :p3)~%")))
              (specs :timeout)
              (thread (bordeaux-threads:make-thread
                       (lambda ()
                         (setf specs
                               (cl-mcp/src/package-context::%package-specs-in-file file)))
                       :name "package-context-test-circular-clauses")))
         (unwind-protect
              (loop repeat 200
                    while (bordeaux-threads:thread-alive-p thread)
                    do (sleep 0.01d0))
           (when (bordeaux-threads:thread-alive-p thread)
             (ignore-errors (bordeaux-threads:destroy-thread thread))))
         (ok (null specs) (format nil "no spec, promptly: ~S" specs)))))))

(defun %header-heads (text)
  "Return the heads of the header forms the scan reads from TEXT, as strings,
or :TIMEOUT."
  (let ((forms :timeout)
        (thread nil))
    (unwind-protect
         (progn
           (setf thread (bordeaux-threads:make-thread
                         (lambda ()
                           (setf forms (cl-mcp/src/package-context::%read-header-forms-from-text
                                        text)))
                         :name "package-context-test-header-heads"))
           (loop repeat 200
                 while (bordeaux-threads:thread-alive-p thread)
                 do (sleep 0.01d0)))
      (when (and thread (bordeaux-threads:thread-alive-p thread))
        (ignore-errors (bordeaux-threads:destroy-thread thread))))
    (if (listp forms)
        (mapcar (lambda (form) (string-downcase (symbol-name (car form)))) forms)
        forms)))

(deftest header-scan-keeps-headers-real-libraries-write
  ;; Shapes from Quicklisp libraries the rewritten scan lost and main found.
  (testing "a top-level conditional that does not hold is passed over"
    (dolist (pair (list
                   ;; drakma.asd
                   (cons (format nil "(in-package :cl-user)~%~
                                      #+:lispworks (unless (find-symbol \"X\") (y))~%~
                                      (defpackage :drakma-asd (:use :cl :asdf))~%~
                                      (in-package :drakma-asd)")
                         '("in-package" "defpackage" "in-package"))
                   ;; cl+ssl's src/package.lisp
                   (cons (format nil "(in-package :cl-user)~%~
                                      #+(or) (in-package #:cl+ssl)~%~
                                      (defpackage :cl+ssl (:use :cl))")
                         '("in-package" "defpackage"))
                   ;; moptilities.lisp
                   (cons (format nil "(defpackage :m (:use :cl))~%~
                                      #+Ignore (eval-when (:compile-toplevel) (x))~%~
                                      (in-package :m)")
                         '("defpackage" "in-package"))))
      (ok (equal (%header-heads (car pair)) (cdr pair))
          (format nil "~S for ~S" (%header-heads (car pair)) (car pair)))))
  (testing "a defpackage that shares a clause through a label is read"
    ;; Eclector's code/base/package.lisp.
    (let ((text (format nil "(defpackage :e (:use :cl)~%  ~
                              (:shadow . #1=(#:end-of-file #:read-char))~%  ~
                              (:export . #1#))~%(in-package :e)")))
      (ok (equal (%header-heads text) '("defpackage" "in-package"))
          (format nil "~S" (%header-heads text)))))
  (testing "a suppressed branch of a header form may hold any dispatch"
    (dolist (branch '("(:export #.(x) #'y)" "(:export #(x y))" "(:export #())"
                      "(:export #3(x))" "(:export #* )" "(:export `(x ,y))"))
      (let* ((text (format nil "(defpackage :p (:use :cl) #-sbcl ~A (:nicknames :q))~%~
                                (in-package :p)"
                           branch))
             (forms (cl-mcp/src/package-context::%read-header-forms-from-text text)))
        (ok (and (= 2 (length forms))
                 (find '(:nicknames :q) (first forms) :test #'equal))
            (format nil "~S for ~A" forms branch))))))

(deftest header-scan-judges-shared-feature-expressions-quickly
  (testing "a feature expression built of shared labels is not walked exponentially"
    ;; #30=(and #29# #29#) and so on down: thirty labels, 2^30 paths.
    (let ((dag (let ((text "#1=(and)"))
                 (loop for k from 2 to 30
                       do (setf text (format nil "#~D=(and ~A #~D#)" k text (1- k))))
                 text)))
      (dolist (pair (list (cons (format nil "#+~A (in-package :q)~%(in-package :p)" dag)
                                "P")
                          (cons (format nil "(in-package #+~A :q :p)" dag) nil)))
        (let ((name (%extract-within 2 (car pair))))
          (ok (if (cdr pair)
                  (and (stringp name) (string-equal name (cdr pair)))
                  (null name))
              (format nil "answered ~S" name))))))
  (testing "a doubly false conditional in a header form reads as the reader does"
    (let ((forms (cl-mcp/src/package-context::%read-header-forms-from-text
                  "(defpackage :p #-sbcl #-sbcl (:use :a) (:use :b))")))
      (ok (equal forms '((defpackage :p))) (format nil "~S" forms)))))

(deftest header-scan-keeps-the-readers-form-boundaries
  ;; A #+/#- that does not hold yields no object, so in a place that takes
  ;; one object -- after a quote, say -- the reader takes the next one.
  (testing "a skipped form ends where the reader ends it"
    (dolist (pair (list
                   ;; The quote takes the in-package: no header in the file.
                   (cons (format nil "'#-sbcl ignored (in-package #:keyword)~%~
                                      (defun pr314-foo () 1)")
                         nil)
                   (cons (format nil "'#-sbcl #-sbcl a b (in-package #:keyword)~%~
                                      (defun pr314-foo () 1)")
                         nil)
                   (cons (format nil "(defun f () '#-sbcl x (in-package :other))~%~
                                      (in-package :my-app)")
                         "MY-APP")
                   (cons (format nil "'#+sbcl x~%(in-package :my-app)") "MY-APP")))
      (let ((name (%extract-within 2 (car pair))))
        (ok (if (cdr pair)
                (and (stringp name) (string-equal name (cdr pair)))
                (null name))
            (format nil "answered ~S for ~S" name (car pair))))))
  (testing "a chain of top-level conditionals takes what the reader takes"
    (dolist (pair (list
                   ;; #+sbcl holds; #-sbcl a yields nothing, so it takes the in-package.
                   (cons "#+sbcl #-sbcl a (in-package :my-app)" "MY-APP")
                   ;; The outer #-sbcl takes #-sbcl a (in-package :other).
                   (cons (format nil "#-sbcl #-sbcl a (in-package :other)~%~
                                      (in-package :my-app)")
                         "MY-APP")
                   (cons (format nil "#+sbcl #+sbcl (in-package :my-app)") "MY-APP")
                   ;; SBCL ignores a numeric argument to #+ and #|.
                   (cons "#1+sbcl (in-package :my-app) (in-package :other)" "MY-APP")
                   (cons "#1| c |# (in-package :my-app) (in-package :other)" "MY-APP")))
      (let ((name (%extract-within 2 (car pair))))
        (ok (and (stringp name) (string-equal name (cdr pair)))
            (format nil "answered ~S for ~S" name (car pair))))))
  (testing "a conditional inside a skipped form that cannot be judged ends the scan"
    ;; Whether it yields an object decides where the form ends.
    (dolist (case (list (format nil "'#+(version>= 9) a (in-package #:keyword)~%~
                                     (in-package :my-app)")
                        (format nil "(defun f () '#-(and . #1=(sbcl . #1#)) a)~%~
                                     (in-package :my-app)")))
      (let ((name (%extract-within 2 case)))
        (ok (null name) (format nil "answered ~S for ~S" name case))))))

(deftest header-scan-reads-radix-integers
  (testing "a defpackage holding #x, #b, #o or #r integers is a header"
    (let ((text (format nil "(defpackage #:pr314-size~%  ~
                             (:use #:cl)~%  ~
                             (:size #x20)~%  ~
                             (:local-nicknames (#:a #:alexandria)))~%~%~
                             (in-package #:pr314-size)~%~%~
                             (defun pr314-ensure (x)~%  (a:ensure-list x))~%"))
          (radixes (format nil "(defpackage :p (:size #X20) (:size #b101) (:size #O17) ~
                                (:size #3r12))~%(in-package :p)")))
      (ok (equal "PR314-SIZE" (%extract-within 2 text))
          (format nil "the in-package after it is found: ~S" (%extract-within 2 text)))
      (let ((forms (cl-mcp/src/package-context::%read-header-forms-from-text text)))
        (ok (find '(:size 32) (first forms) :test #'equal)
            (format nil "the size is read as an integer: ~S" (first forms)))
        (ok (equal '(("A" . "ALEXANDRIA"))
                   (cl-mcp/src/package-context::package-spec-local-nicknames
                    (cl-mcp/src/package-context::%extract-package-spec (first forms) nil)))
            "its local nickname is kept"))
      (let ((forms (cl-mcp/src/package-context::%read-header-forms-from-text radixes)))
        (ok (equal '(defpackage :p (:size 32) (:size 5) (:size 15) (:size 5))
                   (first forms))
            (format nil "every radix: ~S" forms))))))

(defun %chain-of (n piece)
  "Return PIECE written N times."
  (with-output-to-string (out)
    (dotimes (i n)
      (write-string piece out))))

(deftest header-scan-labels-do-not-walk-what-they-label
  (testing "a long labelled clause is read without recursing along it"
    ;; SBCL's #n= walks the labelled object along its cdrs, even with no
    ;; #n# in it: 200000 elements exhausted the stack.
    (dolist (pair (list (cons (format nil "(defpackage :p #1=(:export ~A))~%(in-package :p)"
                                      (%chain-of 200000 "a "))
                              "P")
                        ;; The reference inside the labelled object would make
                        ;; the form circular: no header.
                        (cons (format nil "(defpackage :p #1=(:nicknames ~A#1#))~%~
                                           (in-package :p)"
                                      (%chain-of 200000 "a "))
                              nil)))
      (let ((name (%extract-within 5 (car pair))))
        (ok (equal name (cdr pair))
            (format nil "answered ~S, expected ~S" name (cdr pair))))))
  (testing "a shared clause still reads, and a bad label is no header"
    (let ((forms (cl-mcp/src/package-context::%read-header-forms-from-text
                  (format nil "(defpackage :e (:shadow . #1=(#:a #:b)) (:export . #1#))~%~
                               (in-package :e)"))))
      (ok (= 2 (length forms)) (format nil "~S" forms))
      (ok (eq (cdr (third (first forms))) (cdr (fourth (first forms))))
          "the two clauses share their tail, as the text says"))
    (dolist (text (list "(in-package #1#)" "(in-package #1=a #1=b)"
                        (format nil "(defpackage :p (:use . #1=(:cl . #1#)))~%~
                                     (in-package :p)")))
      (ok (null (%extract-within 2 text)) (format nil "no header in ~S" text))))
  (testing "a suppressed #n= yields nothing, as SBCL's does"
    (let ((name (%extract-within 2 (format nil "(defpackage :p #-sbcl (a #1=) (:use :x))~%~
                                                (in-package :p)"))))
      (ok (equal "P" name) (format nil "answered ~S" name)))))
