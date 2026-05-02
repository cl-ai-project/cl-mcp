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
                #:extract-in-package-name-from-text
                #:discover-package-spec
                #:call-with-package-context
                #:call-with-file-package-context
                #:package-spec-name
                #:package-spec-local-nicknames))

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
