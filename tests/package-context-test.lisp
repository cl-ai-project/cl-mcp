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
