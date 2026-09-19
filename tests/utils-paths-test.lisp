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
                #:resolve-readable-path
                #:broad-root-p
                #:native-path-namestring
                #:normalize-path-for-display))

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

(deftest resolve-readable-path-follows-the-read-policy
 (testing "empty or NIL path resolves to the project root itself"
  (call-with-temp-project-root
   (lambda (root)
     (ok (path-inside-p (resolve-readable-path nil) root))
     (ok (path-inside-p (resolve-readable-path "") root)))))
 (testing "a registered ASDF system source directory outside the root is allowed"
  (call-with-temp-project-root
   (lambda (root)
     (let ((system-dir (asdf:system-source-directory :alexandria)))
       (ok (not (path-inside-p system-dir root)))
       (ok (resolve-readable-path (namestring system-dir) :must-exist t))))))
 (testing "a path outside both the root and every registered system signals by name"
  (call-with-temp-project-root
   (lambda (root)
     (declare (ignore root))
     (let ((message (handler-case (progn (resolve-readable-path "/etc/passwd") nil)
                      (error (e) (princ-to-string e)))))
       (ok message)
       (ok (search "/etc/passwd" message))))))
 (testing "must-exist signals on a missing target"
  (call-with-temp-project-root
   (lambda (root)
     (declare (ignore root))
     (ok (handler-case
             (progn (resolve-readable-path "no-such-file" :must-exist t) nil)
           (error () t)))))))

(deftest broad-root-p-blocks-overly-broad-roots
 (testing "broad-root-p flags top-level directories that are unsuitable"
  (ok (broad-root-p "/"))
  (ok (broad-root-p "/tmp/"))
  (ok (broad-root-p "/home/")))
 (testing "broad-root-p accepts a normal nested directory"
  (ok (not (broad-root-p "/tmp/cl-mcp-some-project/")))))

(deftest normalize-path-for-display-untranslatable-logical-pathname
  (testing "a logical pathname with no matching translation rule returns NIL, not an error"
    ;; Regression test: NORMALIZE-PATH-FOR-DISPLAY used to call
    ;; TRANSLATE-LOGICAL-PATHNAME unprotected.  Some environments' SBCL xref
    ;; data references logical pathnames (e.g. SYS:OBJ;...) with no
    ;; translation rule at all, which signals FILE-ERROR instead of
    ;; returning gracefully and aborted the whole caller.  This reproduces
    ;; that shape without depending on this machine's own SYS: translations,
    ;; via a private logical host whose only rule does not cover the
    ;; directory used below.  The translations must be defined before the
    ;; pathname is built, or MAKE-PATHNAME itself signals for an unknown host.
    (setf (logical-pathname-translations "CLMCPPATHTEST")
          '(("SRC;**;*.*.*" "/tmp/")))
    (let* ((pn (make-pathname :host "CLMCPPATHTEST" :directory '(:absolute "OBJ")
                              :name "X" :type "LISP"))
           (signaled nil)
           (result (handler-case (normalize-path-for-display pn)
                     (condition (c) (setf signaled c) :signaled))))
      (ok (not signaled)
          (format nil "must not signal a condition; got ~A"
                  (and signaled (list (type-of signaled) signaled))))
      (ok (null result) "must return NIL for an untranslatable logical pathname"))))

(deftest native-path-namestring-does-not-escape-for-the-pathname-reader
  (testing "a path holding the reader's wild characters comes back usable"
    ;; NAMESTRING escapes [ and ] so its result round-trips through the
    ;; pathname READER; what a caller needs is a path that opens.  The escaped
    ;; form names nothing on disk, and cl-mcp printed it, compared against it
    ;; and tried to read it.
    (let ((pn (uiop:parse-native-namestring "/tmp/demo[old]/x.lisp")))
      (ok (search "demo[old]" (native-path-namestring pn))
          "the brackets survive unescaped")
      (ok (not (find #\\ (native-path-namestring pn)))
          "and nothing was escaped into the path")))
  (testing "NIL in, NIL out"
    (ok (null (native-path-namestring nil))))
  (testing "a genuinely wild pathname falls back instead of signalling"
    ;; It has no native form at all, and a function whose job is to describe a
    ;; path should not turn that into an error.
    (let ((wild (make-pathname :name :wild :type "lisp" :directory '(:absolute "tmp"))))
      (ok (stringp (native-path-namestring wild))))))

(deftest normalize-path-for-display-returns-a-path-that-can-be-read-back
  (testing "the displayed path is one the read policy accepts"
    ;; A path cl-mcp prints is a path a caller hands back, so the two have to
    ;; agree; under the old NAMESTRING they did not for a bracketed directory.
    ;; Bound explicitly: this asks whether the read policy accepts the path,
    ;; so the root it is judged against must be this suite's, not whatever a
    ;; previously-run suite happened to leave behind.
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           ;; Built natively on purpose: MERGE-PATHNAMES on a string with
           ;; brackets parses them as wild, the same confusion under test.
           (dir (uiop:parse-native-namestring
                 (format nil "~Atests/tmp/disp[br]/"
                         (native-path-namestring *project-root*))
                 :ensure-directory t))
           (file (merge-pathnames (uiop:parse-native-namestring "x.lisp") dir)))
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             (with-open-file (out file :direction :output :if-exists :supersede)
               (write-string "(defun x () 1)" out))
             (let ((shown (normalize-path-for-display (truename file))))
               (ok (search "disp[br]" shown) "the brackets are shown as they are")
               (ok (allowed-read-path shown)
                   "and the shown path is accepted by the read policy")))
        (ignore-errors (delete-file file))
        (ignore-errors (uiop:delete-empty-directory dir))))))
