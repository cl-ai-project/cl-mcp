;;;; tests/source-snapshot-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/source-snapshot: reading a file exactly once
;;;; and digesting it, so an edit guard's file and form digests come from the
;;;; same bytes a CST parse saw (design doc
;;;; 2026-09-16-clos-describe-fail-closed-design.md, section 4.1).

(defpackage #:cl-mcp/tests/source-snapshot-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/source-snapshot
                #:read-source-snapshot
                #:snapshot-range-digest
                #:digest-string-octets))

(in-package #:cl-mcp/tests/source-snapshot-test)

(defun %make-temp-dir (label)
  "Create and return a fresh directory pathname under the temporary directory.
LABEL goes into its name, so a leftover directory says which test made it."
  (let ((dir (uiop:ensure-directory-pathname
              (uiop:merge-pathnames* (format nil "cl-mcp-source-snapshot-~A-~D/"
                                             label (random 1000000))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun %put-file (dir name text)
  "Write TEXT as UTF-8 to the file NAME under the directory DIR."
  (with-open-file (s (merge-pathnames name dir)
                     :direction :output :if-exists :supersede
                     :external-format :utf-8)
    (write-string text s)))

(defun %octet-count (path)
  "Return the number of octets on disk at PATH."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (file-length s)))

(deftest digest-string-octets-hashes-utf-8-bytes
  (testing "matches the well-known md5 of the UTF-8 bytes of \"hello\""
    (ok (equal "md5:5d41402abc4b2a76b9719d911017c592" (digest-string-octets "hello"))))
  (testing "differs for differing input"
    (ok (not (equal (digest-string-octets "hello") (digest-string-octets "hellp"))))))

(deftest read-source-snapshot-same-content-same-digest
  (testing "two files with byte-identical content produce the same digest"
    (let ((dir (%make-temp-dir "same")))
      (unwind-protect
           (let ((*project-root* dir))
             (%put-file dir "a.lisp" "(defun a () 1)")
             (%put-file dir "b.lisp" "(defun a () 1)")
             (multiple-value-bind (snap-a fail-a)
                 (read-source-snapshot (merge-pathnames "a.lisp" dir))
               (multiple-value-bind (snap-b fail-b)
                   (read-source-snapshot (merge-pathnames "b.lisp" dir))
                 (ok (null fail-a))
                 (ok (null fail-b))
                 (ok (stringp (getf snap-a :digest)))
                 (ok (equal (getf snap-a :digest) (getf snap-b :digest))))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest read-source-snapshot-one-byte-difference-changes-digest
  (testing "a single differing byte produces a different digest"
    (let ((dir (%make-temp-dir "diff")))
      (unwind-protect
           (let ((*project-root* dir))
             (%put-file dir "a.lisp" "(defun a () 1)")
             (%put-file dir "b.lisp" "(defun a () 2)")
             (multiple-value-bind (snap-a fail-a)
                 (read-source-snapshot (merge-pathnames "a.lisp" dir))
               (multiple-value-bind (snap-b fail-b)
                   (read-source-snapshot (merge-pathnames "b.lisp" dir))
                 (ok (null fail-a))
                 (ok (null fail-b))
                 (ok (not (equal (getf snap-a :digest) (getf snap-b :digest)))))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest read-source-snapshot-detects-content-change-under-same-mtime-and-size
  (testing "digest changes even when mtime and size are held constant"
    (require :sb-posix)
    (let ((dir (%make-temp-dir "mtime")))
      (unwind-protect
           (let ((*project-root* dir)
                 (path (merge-pathnames "f.lisp" dir)))
             (%put-file dir "f.lisp" "(defun a () 1)")
             (multiple-value-bind (snap1 fail1) (read-source-snapshot path)
               (ok (null fail1))
               (let* ((stat (sb-posix:stat path))
                      (atime (sb-posix:stat-atime stat))
                      (mtime (sb-posix:stat-mtime stat))
                      (size (sb-posix:stat-size stat)))
                 ;; Same length as the original, so size is unaffected.
                 (%put-file dir "f.lisp" "(defun b () 1)")
                 (funcall (symbol-function (find-symbol "UTIME" "SB-POSIX")) path atime mtime)
                 (let ((stat2 (sb-posix:stat path)))
                   (ok (= size (sb-posix:stat-size stat2)) "size unchanged on disk")
                   (ok (= mtime (sb-posix:stat-mtime stat2)) "mtime unchanged on disk"))
                 (multiple-value-bind (snap2 fail2) (read-source-snapshot path)
                   (ok (null fail2))
                   (ok (not (equal (getf snap1 :digest) (getf snap2 :digest)))
                       "digest differs despite identical mtime and size")))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest read-source-snapshot-denies-a-path-outside-the-read-policy
  (testing "a path the read policy refuses is never opened"
    (let ((dir (%make-temp-dir "deny")))
      (unwind-protect
           (let ((*project-root* dir))
             (multiple-value-bind (snap failure) (read-source-snapshot #P"/etc/passwd")
               (ok (null snap) "no partial snapshot on denial")
               (ok (eq :denied failure))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest read-source-snapshot-reports-failure-for-a-missing-file
  (testing "a readable-but-absent path fails closed instead of signalling"
    (let ((dir (%make-temp-dir "missing")))
      (unwind-protect
           (let ((*project-root* dir))
             (multiple-value-bind (snap failure)
                 (read-source-snapshot (merge-pathnames "nope.lisp" dir))
               (ok (null snap))
               (ok (stringp failure))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest read-source-snapshot-decodes-invalid-utf-8-bytes
  (testing "a stray invalid byte is read as #\\?, matching FS-READ-SOURCE-TEXT"
    (let ((dir (%make-temp-dir "badbyte")))
      (unwind-protect
           (let ((*project-root* dir)
                 (path (merge-pathnames "bad.lisp" dir)))
             (ensure-directories-exist path)
             (with-open-file (out path :direction :output :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
               (write-sequence (sb-ext:string-to-octets "(defun a () 1) ; "
                                                        :external-format :utf-8)
                               out)
               (write-byte #xE9 out)
               (write-sequence (sb-ext:string-to-octets (format nil " end~%")
                                                        :external-format :utf-8)
                               out))
             (multiple-value-bind (snap failure) (read-source-snapshot path)
               (ok (null failure))
               (ok (equal (format nil "(defun a () 1) ; ? end~%") (getf snap :text)))
               (ok (stringp (getf snap :digest)))
               (ok (= (%octet-count path) (getf snap :octet-count)))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest snapshot-range-digest-covers-exactly-start-end
  (testing "the range digest covers [start, end) of the snapshot text, no more, no less"
    (let ((dir (%make-temp-dir "range")))
      (unwind-protect
           (let* ((*project-root* dir)
                  (path (merge-pathnames "f.lisp" dir))
                  (first-form "(defun a () 1)")
                  (second-form "(defun b () 2)")
                  (text (concatenate 'string first-form second-form))
                  (split (length first-form)))
             (%put-file dir "f.lisp" text)
             (multiple-value-bind (snap failure) (read-source-snapshot path)
               (ok (null failure))
               (ok (equal text (getf snap :text)))
               (let ((digest-1 (snapshot-range-digest snap 0 split))
                     (digest-2 (snapshot-range-digest snap split (length text))))
                 (ok (equal digest-1 (digest-string-octets first-form)))
                 (ok (equal digest-2 (digest-string-octets second-form)))
                 (ok (not (equal digest-1 digest-2)))
                 ;; END is exclusive: one character short or one character
                 ;; long must not match the exact-range digest.
                 (ok (not (equal digest-1 (snapshot-range-digest snap 0 (1- split)))))
                 (ok (not (equal digest-1 (snapshot-range-digest snap 0 (1+ split))))))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
