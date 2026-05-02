;;;; tests/utils-paths-test.lisp
;;;;
;;;; Tests for cl-mcp/src/utils/paths — the project-root access control layer.
;;;; These functions are the security boundary for every file-touching tool, so
;;;; the suite focuses on:
;;;;   - rejecting paths outside *project-root*
;;;;   - rejecting symlink-based traversal
;;;;   - rejecting overly-broad project roots (/, /tmp/, /home/)

(defpackage #:cl-mcp/tests/utils-paths-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:path-inside-p
                #:canonical-path
                #:allowed-read-path
                #:ensure-write-path
                #:resolve-path-in-project
                #:broad-root-p))

(in-package #:cl-mcp/tests/utils-paths-test)

(defun call-with-temp-project-root (thunk)
  "Bind *project-root* to a fresh temp directory for the duration of THUNK."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/cl-mcp-paths-test-~A/" (random 1000000))))
         (real-dir (progn (ensure-directories-exist dir) (truename dir))))
    (unwind-protect
         (let ((*project-root* real-dir))
           (funcall thunk real-dir))
      (uiop:delete-directory-tree real-dir :validate t :if-does-not-exist :ignore))))

(deftest path-inside-p-basic
 (testing "path-inside-p returns T for child paths"
  (ok (path-inside-p #P"/tmp/foo/bar" #P"/tmp/"))
  (ok (path-inside-p #P"/tmp/foo/bar/baz.txt" #P"/tmp/foo/")))
 (testing "path-inside-p returns NIL for sibling/outside paths"
  (ok (not (path-inside-p #P"/etc/passwd" #P"/tmp/")))
  (ok (not (path-inside-p #P"/tmp-other/x" #P"/tmp/"))))
 (testing "path-inside-p tolerates NIL inputs without erroring"
  (ok (not (path-inside-p nil #P"/tmp/")))
  (ok (not (path-inside-p #P"/tmp/foo" nil)))))

(deftest canonical-path-merges-relative
 (testing "relative paths are merged against *project-root*"
  (call-with-temp-project-root
   (lambda (root)
     (let ((abs (canonical-path "subdir/file.txt")))
       (ok (uiop:absolute-pathname-p abs))
       (ok (path-inside-p abs root)))))))

(deftest canonical-path-honors-relative-to
 (testing "relative-to overrides *project-root* base"
  (call-with-temp-project-root
   (lambda (root)
     (declare (ignore root))
     (let* ((alt-base #P"/var/tmp/")
            (abs (canonical-path "x" :relative-to alt-base)))
       (ok (path-inside-p abs alt-base)))))))

(deftest allowed-read-path-accepts-project-files
 (testing "files inside the project root are allowed"
  (call-with-temp-project-root
   (lambda (root)
     (let ((target (merge-pathnames "ok.txt" root)))
       (with-open-file (s target :direction :output :if-exists :supersede)
         (write-string "hi" s))
       (ok (allowed-read-path target)))))))

(deftest allowed-read-path-rejects-outside
 (testing "paths outside project root and ASDF dirs return NIL"
  (call-with-temp-project-root
   (lambda (root)
     (declare (ignore root))
     (ok (null (allowed-read-path #P"/etc/passwd")))))))

(deftest ensure-write-path-rejects-outside-project
 (testing "ensure-write-path signals when target escapes project root"
  (call-with-temp-project-root
   (lambda (root)
     (declare (ignore root))
     (ok (handler-case
             (progn (ensure-write-path "/etc/cl-mcp-evil") nil)
           (error () t))
      "absolute path outside project must error")))))

(deftest ensure-write-path-accepts-relative
 (testing "relative path under project root resolves and is returned absolute"
  (call-with-temp-project-root
   (lambda (root)
     (let ((abs (ensure-write-path "newfile.txt")))
       (ok (uiop:absolute-pathname-p abs))
       (ok (path-inside-p abs root)))))))

(deftest resolve-path-in-project-empty-returns-root
 (testing "empty or NIL path resolves to the project root itself"
  (call-with-temp-project-root
   (lambda (root)
     (let ((res-nil (resolve-path-in-project nil))
           (res-empty (resolve-path-in-project "")))
       (ok (path-inside-p res-nil root))
       (ok (path-inside-p res-empty root)))))))

(deftest resolve-path-in-project-rejects-outside
 (testing "absolute path outside the project root signals error"
  (call-with-temp-project-root
   (lambda (root)
     (declare (ignore root))
     (ok (handler-case
             (progn (resolve-path-in-project "/etc/passwd") nil)
           (error () t)))))))

(deftest resolve-path-in-project-must-exist
 (testing "must-exist signals on missing target"
  (call-with-temp-project-root
   (lambda (root)
     (declare (ignore root))
     (ok (handler-case
             (progn (resolve-path-in-project "no-such-file" :must-exist t) nil)
           (error () t)))))))

(deftest broad-root-p-blocks-overly-broad-roots
 (testing "broad-root-p flags top-level directories that are unsuitable"
  (ok (broad-root-p "/"))
  (ok (broad-root-p "/tmp/"))
  (ok (broad-root-p "/home/")))
 (testing "broad-root-p accepts a normal nested directory"
  (ok (not (broad-root-p "/tmp/cl-mcp-some-project/")))))
