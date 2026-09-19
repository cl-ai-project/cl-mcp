;;;; tests/fs-test.lisp

(defpackage #:cl-mcp/tests/fs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:uiop #:getcwd #:ensure-directory-pathname
                #:merge-pathnames* #:native-namestring)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread
                #:thread-alive-p
                #:destroy-thread
                #:make-semaphore
                #:signal-semaphore
                #:wait-on-semaphore)
  (:import-from #:asdf #:system-source-directory)
  ;; FILE-LOCK-KEY-COLLAPSES-SPELLINGS-OF-ONE-FILE calls ENSURE-WRITE-PATH
  ;; directly (package-qualified below only reached this file transitively,
  ;; through CL-MCP/SRC/FS), so it is imported explicitly like every other
  ;; symbol this file uses.
  (:import-from #:cl-mcp/src/utils/paths #:ensure-write-path)
  ;; Named so ASDF loads it: fs-write-file's post-write warning and its
  ;; overwrite guard both follow *lisp-file-unparseable-hook*, which this
  ;; system installs at load time.  Without the dependency the hook is NIL
  ;; here and neither behaviour can be observed.
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%file-unparseable-by-edit-tools-p)
  ;; The fs-write-file tool's overwrite decision, its write and its post-write
  ;; check must exclude a concurrent structural edit of the same file, so the
  ;; tests below drive a real lisp-edit-form against it.
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-read-source-text
                #:fs-read-source-octets
                #:fs-write-file
                #:file-lock-key
                #:file-lock
                #:fs-window-start
                #:fs-list-directory
                #:fs-resolve-read-path
                #:fs-get-project-info
                #:fs-set-project-root))

(in-package #:cl-mcp/tests/fs-test)

(defmacro with-test-project-root (&body body)
  `(let ((original-root cl-mcp/src/project-root:*project-root*)
         (original-cwd (ignore-errors (getcwd)))
         ;; fs-set-project-root and handle-initialize both move
         ;; *default-pathname-defaults* along with the root. Restoring only the
         ;; root leaves it pointing at the test's directory -- at a deleted one
         ;; when the test cleans up after itself -- and every later test that
         ;; resolves a relative pathname inherits that.
         (original-defaults *default-pathname-defaults*)
         (test-root (or (ignore-errors
                          (ensure-directory-pathname
                           (system-source-directory "cl-mcp")))
                        (ensure-directory-pathname (getcwd)))))
     (unwind-protect
          (progn
            ;; Set project root explicitly for the test
            (setf cl-mcp/src/project-root:*project-root* test-root)
            ,@body)
       ;; Restore original state
       (setf cl-mcp/src/project-root:*project-root* original-root
             *default-pathname-defaults* original-defaults)
       (when original-cwd
         (ignore-errors (uiop:chdir original-cwd))))))

(deftest fs-read-file-project
  (testing "fs-read-file reads project file with content"
    (with-test-project-root
      (let ((txt (fs-read-file "src/core.lisp" :offset 0 :limit 40)))
        (ok (stringp txt))
        (ok (> (length txt) 0))))))

(deftest fs-read-file-multibyte-over-cap-in-bytes-is-not-truncated
  (testing "a file over the cap in octets but under it in characters is read whole"
    (with-test-project-root
      (let ((abs (merge-pathnames "tests/tmp/multibyte-big.lisp"
                                  cl-mcp/src/project-root:*project-root*)))
        (ensure-directories-exist abs)
        ;; 400 000 three-byte characters: 1.2 MB on disk, 400 000 characters.
        (with-open-file (out abs :direction :output :if-exists :supersede
                                 :external-format :utf-8)
          (write-string ";; " out)
          (loop repeat 400000 do (write-char (code-char #x3042) out))
          (terpri out))
        (unwind-protect
             (multiple-value-bind (text truncated)
                 (cl-mcp/src/fs::%read-file-string abs nil nil)
               (ok (= (length text) 400004) "every character was read")
               (ok (not truncated) "and the read is not reported as truncated"))
          (ignore-errors (delete-file abs)))))))

(deftest fs-read-file-undecodable-byte-past-cap-is-truncated-not-an-error
  (testing "an invalid byte just past the read cap does not turn the read into an error"
    (with-test-project-root
      (let ((abs (merge-pathnames "tests/tmp/bad-byte-past-cap.lisp"
                                  cl-mcp/src/project-root:*project-root*))
            (cap cl-mcp/src/fs::*fs-read-max-bytes*))
        (ensure-directories-exist abs)
        (with-open-file (out abs :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (loop repeat cap do (write-byte 97 out))
          (write-byte #xff out)
          (write-byte #xfe out))
        (unwind-protect
             (multiple-value-bind (text truncated)
                 (cl-mcp/src/fs::%read-file-string abs nil nil)
               (ok (= (length text) cap) "the prefix up to the cap is returned")
               (ok truncated "and it is reported as truncated"))
          (ignore-errors (delete-file abs)))))))

(deftest fs-read-source-text-reads-whole-files-within-the-read-policy
  (testing "an invalid UTF-8 byte is replaced, not an error"
    (with-test-project-root
      (let ((abs (merge-pathnames "tests/tmp/source-text-bad-byte.lisp"
                                  cl-mcp/src/project-root:*project-root*)))
        (ensure-directories-exist abs)
        (with-open-file (out abs :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (write-sequence (sb-ext:string-to-octets "(defun a () 1) ; " :external-format :utf-8)
                          out)
          (write-byte #xE9 out)
          (write-sequence (sb-ext:string-to-octets (format nil " end~%") :external-format :utf-8)
                          out))
        (unwind-protect
             (ok (equal (format nil "(defun a () 1) ; ? end~%") (fs-read-source-text abs)))
          (ignore-errors (delete-file abs))))))
  (testing "a file past fs-read-file's cap is read whole"
    (with-test-project-root
      (let ((abs (merge-pathnames "tests/tmp/source-text-over-cap.lisp"
                                  cl-mcp/src/project-root:*project-root*))
            (size (+ cl-mcp/src/fs::*fs-read-max-bytes* 10)))
        (ensure-directories-exist abs)
        (with-open-file (out abs :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (loop repeat size do (write-byte 97 out)))
        (unwind-protect
             (ok (= size (length (fs-read-source-text abs))))
          (ignore-errors (delete-file abs))))))
  (testing "a path outside the readable paths signals, even when the file exists"
    (with-test-project-root
      (let ((outside (merge-pathnames (format nil "cl-mcp-source-text-~D.lisp" (random 1000000))
                                      (uiop:temporary-directory))))
        (with-open-file (out outside :direction :output :if-exists :supersede)
          (write-string "(defun secret () 1)" out))
        (unwind-protect
             (ok (handler-case (progn (fs-read-source-text outside) nil)
                   (error (e) (and (search "not permitted" (princ-to-string e)) t))))
          (ignore-errors (delete-file outside)))))))

(deftest fs-read-source-octets-returns-the-files-exact-bytes
  (testing "an invalid UTF-8 byte survives the read, unlike fs-read-source-text"
    (with-test-project-root
      (let ((abs (merge-pathnames "tests/tmp/source-octets-bad-byte.lisp"
                                  cl-mcp/src/project-root:*project-root*))
            (expected (concatenate '(vector (unsigned-byte 8))
                                   (sb-ext:string-to-octets "(defun a () 1) ; "
                                                            :external-format :utf-8)
                                   (vector #xE9)
                                   (sb-ext:string-to-octets (format nil " あ end~%")
                                                            :external-format :utf-8))))
        (ensure-directories-exist abs)
        (with-open-file (out abs :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (write-sequence expected out))
        (unwind-protect
             (let ((octets (fs-read-source-octets abs)))
               (ok (equalp expected octets))
               ;; The digest an edit guard takes is over these bytes; the
               ;; decoded text replaces #xE9 with #\? and could not reproduce
               ;; them.
               (ok (find #xE9 octets)))
          (ignore-errors (delete-file abs))))))
  (testing "a file past fs-read-file's cap is read whole"
    (with-test-project-root
      (let ((abs (merge-pathnames "tests/tmp/source-octets-over-cap.lisp"
                                  cl-mcp/src/project-root:*project-root*))
            (size (+ cl-mcp/src/fs::*fs-read-max-bytes* 10)))
        (ensure-directories-exist abs)
        (with-open-file (out abs :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (loop repeat size do (write-byte 97 out)))
        (unwind-protect
             (ok (= size (length (fs-read-source-octets abs))))
          (ignore-errors (delete-file abs))))))
  (testing "a path outside the readable paths signals, even when the file exists"
    (with-test-project-root
      (let ((outside (merge-pathnames (format nil "cl-mcp-source-octets-~D.lisp" (random 1000000))
                                      (uiop:temporary-directory))))
        (with-open-file (out outside :direction :output :if-exists :supersede)
          (write-string "(defun secret () 1)" out))
        (unwind-protect
             (ok (handler-case (progn (fs-read-source-octets outside) nil)
                   (error (e) (and (search "not permitted" (princ-to-string e)) t))))
          (ignore-errors (delete-file outside)))))))

(deftest fs-write-file-project
  (testing "fs-write-file writes under project root"
    (with-test-project-root
      (let ((rel "tests/tmp/fs-write.txt")
            (content "hello world\n"))
        (unwind-protect
             (progn
               (ok (fs-write-file rel content))
               (let ((read (fs-read-file rel)))
                 (ok (string= read content))))
          (ignore-errors (delete-file rel)))))))

(deftest fs-list-directory-project
  (testing "fs-list-directory lists entries and filters hidden"
    (with-test-project-root
      (let* ((entries (fs-list-directory "."))
             (names (map 'list (lambda (h) (gethash "name" h)) entries)))
        (ok (find "src" names :test #'string=))
        (ok (not (find ".git" names :test #'string=)))))))

(deftest fs-list-directory-show-hidden
  (testing "show-hidden=nil omits dotfiles (default)"
    (with-test-project-root
      (let* ((entries (fs-list-directory "."))
             (names (map 'list (lambda (h) (gethash "name" h)) entries)))
        (ok (not (find-if (lambda (n) (and n (plusp (length n))
                                           (char= (char n 0) #\.)))
                          names))))))
  (testing "show-hidden=t includes dotfiles but keeps extension filter"
    (with-test-project-root
      (let* ((entries (fs-list-directory "." :show-hidden t))
             (names (map 'list (lambda (h) (gethash "name" h)) entries)))
        ;; The cl-mcp repo has a .gitignore; with show-hidden it must appear.
        ;; (If the repo layout changes this assertion tells us.)
        (ok (or (find ".gitignore" names :test #'string=)
                (find ".git" names :test #'string=)))
        ;; Extensions in *skip-extensions* are still filtered out.
        (ok (not (find-if (lambda (n)
                            (and n (search ".fasl" n :from-end t)
                                 (not (char= (char n 0) #\.))))
                          names)))))))

(deftest fs-list-directory-includes-files
  (testing "fs-list-directory returns files with type metadata"
    (with-test-project-root
      (let* ((entries (fs-list-directory "src/"))
             (core (find "core.lisp" entries :key (lambda (h) (gethash "name" h))
                                         :test #'string=)))
        (ok core)
        (ok (string= "file" (gethash "type" core)))))))

(deftest fs-read-file-respects-limit-and-offset
  (testing "limit and offset trim content"
    (with-test-project-root
      (let ((txt (fs-read-file "src/core.lisp" :offset 1 :limit 5)))
        (ok (= (length txt) 5))))))

(deftest fs-read-file-rejects-negative-offset
  (testing "negative offset signals error"
    (with-test-project-root
      (ok (handler-case (progn (fs-read-file "src/core.lisp" :offset -1) nil)
            (error () t))))))

(deftest fs-read-file-rejects-huge-limit
  (testing "limit over max signals error"
    (with-test-project-root
      (let ((max cl-mcp/src/fs::*fs-read-max-bytes*))
        (ok (handler-case (progn (fs-read-file "src/core.lisp" :limit (1+ max)) nil)
              (error () t)))))))

(deftest fs-list-directory-error-includes-resolved-path
  (testing "error message shows resolved absolute path"
    (with-test-project-root
      (let* ((rel "no-such-dir-for-test")
             (resolved (namestring (fs-resolve-read-path rel))))
        (ok (handler-case
                 (progn (fs-list-directory rel) nil)
               (error (e)
                 (let ((msg (princ-to-string e)))
                   (and (search rel msg) (search resolved msg))))))))))

(deftest fs-get-project-info-returns-paths
  (testing "fs-get-project-info exposes project root and cwd"
    (with-test-project-root
      (let ((info (fs-get-project-info)))
        (ok (stringp (gethash "project_root" info)))
        (ok (stringp (gethash "cwd" info)))
        (ok (member (gethash "project_root_source" info)
                    '("env" "explicit") :test #'string=))))))

(deftest fs-write-file-prevents-traversal
  (testing "writing outside project root is rejected"
    (with-test-project-root
      (ok (handler-case (progn (fs-write-file "../outside.txt" "nope") nil)
            (error () t))))))

(deftest fs-set-project-root-changes-root
  (testing "fs-set-project-root updates project root and cwd"
    (with-test-project-root
      (let* ((original-root cl-mcp/src/project-root:*project-root*)
             (original-cwd (getcwd))
             (test-dir (namestring original-root)))
        (unwind-protect
             (let ((result (fs-set-project-root test-dir)))
               (ok (hash-table-p result))
               (ok (stringp (gethash "project_root" result)))
               (ok (stringp (gethash "cwd" result)))
               (ok (stringp (gethash "previous_root" result)))
               (ok (stringp (gethash "status" result)))
               (ok (string= (gethash "project_root" result) test-dir))
               (ok (string= (gethash "cwd" result) test-dir)))
          ;; Restore original state
          (setf cl-mcp/src/project-root:*project-root* original-root)
          (ignore-errors (uiop:chdir original-cwd)))))))

(deftest fs-set-project-root-validates-directory
  (testing "fs-set-project-root rejects non-existent directory"
    (with-test-project-root
      (ok (handler-case
               (progn (fs-set-project-root "/nonexistent/directory/path") nil)
             (error () t))))))

(deftest fs-set-project-root-validates-string
  (testing "fs-set-project-root rejects non-string argument"
    (with-test-project-root
      (ok (handler-case
               (progn (fs-set-project-root 123) nil)
             (error () t))))))

(deftest fs-set-project-root-syncs-with-get-info
  (testing "fs-set-project-root result matches fs-get-project-info"
    (with-test-project-root
      (let* ((original-root cl-mcp/src/project-root:*project-root*)
             (original-cwd (getcwd))
             (test-dir (namestring original-root)))
        (unwind-protect
             (progn
               (fs-set-project-root test-dir)
               (let ((info (fs-get-project-info)))
                 (ok (string= (gethash "project_root" info) test-dir))
                 (ok (string= (gethash "cwd" info) test-dir))))
          ;; Restore original state
          (setf cl-mcp/src/project-root:*project-root* original-root)
          (ignore-errors (uiop:chdir original-cwd)))))))

(deftest fs-operations-require-project-root
  (testing "file operations fail with helpful error when project root is not set"
    (let ((cl-mcp/src/project-root:*project-root* nil))
      ;; Test that fs-read-file fails
      (ok (handler-case
               (progn (fs-read-file "src/core.lisp") nil)
             (error (e)
               (let ((msg (princ-to-string e)))
                 (and (search "Project root is not set" msg)
                      (search "fs-set-project-root" msg))))))
      ;; Test that fs-write-file fails
      (ok (handler-case
               (progn (fs-write-file "test.txt" "content") nil)
             (error (e)
               (let ((msg (princ-to-string e)))
                 (and (search "Project root is not set" msg)
                      (search "fs-set-project-root" msg))))))
      ;; Test that fs-list-directory fails
      (ok (handler-case
               (progn (fs-list-directory ".") nil)
             (error (e)
               (let ((msg (princ-to-string e)))
                 (and (search "Project root is not set" msg)
                      (search "fs-set-project-root" msg))))))
      ;; Test that fs-get-project-info fails
      (ok (handler-case
               (progn (fs-get-project-info) nil)
             (error (e)
               (let ((msg (princ-to-string e)))
                 (and (search "Project root is not set" msg)
                      (search "fs-set-project-root" msg)))))))))

(deftest fs-set-project-root-converts-relative-to-absolute
  (testing "fs-set-project-root converts relative paths to absolute paths"
    (let ((original-root cl-mcp/src/project-root:*project-root*)
          (original-cwd (getcwd))
          ;; Use getcwd to match what fs-set-project-root does internally
          (expected-absolute (truename (getcwd))))
      (unwind-protect
           (let ((result (fs-set-project-root ".")))
             ;; Verify the result contains an absolute path
             (ok (hash-table-p result))
             (ok (stringp (gethash "project_root" result)))

             ;; The returned path should be absolute (starts with /)
             (ok (uiop:absolute-pathname-p
                  (uiop:ensure-pathname (gethash "project_root" result))))

             ;; The returned path should match the expected absolute path
             (ok (string= (gethash "project_root" result)
                         (namestring expected-absolute)))

             ;; Verify *project-root* is also absolute
             (ok (uiop:absolute-pathname-p cl-mcp/src/project-root:*project-root*))

             ;; Verify file operations work with the absolute path
             (let ((info (fs-get-project-info)))
               (ok (stringp (gethash "project_root" info)))
               (ok (uiop:absolute-pathname-p
                    (uiop:ensure-pathname (gethash "project_root" info))))))
        ;; Restore original state
        (setf cl-mcp/src/project-root:*project-root* original-root)
        (when original-cwd
          (ignore-errors (uiop:chdir original-cwd)))))))

(deftest fs-set-project-root-relative-path-subdirectory
  (testing "fs-set-project-root handles relative subdirectory paths"
    (with-test-project-root
      (let* ((original-root cl-mcp/src/project-root:*project-root*)
             (original-cwd (getcwd))
             ;; Assume "src" directory exists in the project
             (relative-path "src")
             (expected-absolute (truename (ensure-directory-pathname relative-path))))
        (unwind-protect
             (let ((result (fs-set-project-root relative-path)))
               ;; Verify the result contains an absolute path
               (ok (hash-table-p result))
               (ok (stringp (gethash "project_root" result)))

               ;; The returned path should be absolute
               (ok (uiop:absolute-pathname-p
                    (uiop:ensure-pathname (gethash "project_root" result))))

               ;; The returned path should match the expected absolute path
               (ok (string= (gethash "project_root" result)
                           (namestring expected-absolute)))

               ;; Verify *project-root* is also absolute
               (ok (uiop:absolute-pathname-p cl-mcp/src/project-root:*project-root*)))
          ;; Restore original state
          (setf cl-mcp/src/project-root:*project-root* original-root)
          (when original-cwd
            (ignore-errors (uiop:chdir original-cwd))))))))

(deftest fs-list-directory-trailing-slash-normalization
  (testing "fs-list-directory accepts paths with and without trailing slashes"
    (with-test-project-root
      (let* ((project-root (namestring cl-mcp/src/project-root:*project-root*))
             ;; Remove trailing slash if present
             (path-without-slash (string-right-trim "/" project-root))
             ;; Ensure trailing slash
             (path-with-slash
               (if (char= (char project-root (1- (length project-root))) #\/)
                   project-root
                   (concatenate 'string project-root "/"))))
        ;; Test without trailing slash
        (let ((entries-no-slash (fs-list-directory path-without-slash)))
          (ok (vectorp entries-no-slash))
          (ok (> (length entries-no-slash) 0)))
        ;; Test with trailing slash
        (let ((entries-with-slash (fs-list-directory path-with-slash)))
          (ok (vectorp entries-with-slash))
          (ok (> (length entries-with-slash) 0)))
        ;; Both should return the same entries
        (let ((entries-no-slash (fs-list-directory path-without-slash))
              (entries-with-slash (fs-list-directory path-with-slash)))
          (ok (= (length entries-no-slash) (length entries-with-slash)))
          ;; Compare entry names
          (let ((names-no-slash
                  (sort (map 'list
                             (lambda (h) (gethash "name" h))
                             entries-no-slash)
                        #'string<))
                (names-with-slash
                  (sort (map 'list
                             (lambda (h) (gethash "name" h))
                             entries-with-slash)
                        #'string<)))
            (ok (equal names-no-slash names-with-slash))))))))

(deftest fs-resolve-read-path-trailing-slash-normalization
  (testing "fs-resolve-read-path normalizes paths with and without trailing slashes"
    (with-test-project-root
      (let* ((project-root (namestring cl-mcp/src/project-root:*project-root*))
             (path-without-slash (string-right-trim "/" project-root))
             (path-with-slash (if (char= (char project-root (1- (length project-root))) #\/)
                                  project-root
                                  (concatenate 'string project-root "/"))))
        ;; Both should resolve successfully
        (let ((resolved-no-slash (fs-resolve-read-path path-without-slash))
              (resolved-with-slash (fs-resolve-read-path path-with-slash)))
          (ok resolved-no-slash)
          (ok resolved-with-slash)
          ;; Both should resolve to the same directory pathname
          (ok (uiop:pathname-equal
               (uiop:ensure-directory-pathname resolved-no-slash)
               (uiop:ensure-directory-pathname resolved-with-slash))))))))

(defun %call-fs-write (path content &key allow)
  "Call the fs-write-file tool handler and return (VALUES text payload error):
the summary text, the result hash, and the JSON-RPC error hash, if any."
  (let ((args (cl-mcp/src/tools/helpers:make-ht "path" path "content" content)))
    (when allow
      (setf (gethash "allow_unparseable_overwrite" args) t))
    (let* ((response (cl-mcp/src/fs::fs-write-file-handler
                      (cl-mcp/src/state:make-state) 1 args))
           (payload (gethash "result" response))
           (content (and payload (gethash "content" payload))))
      (values (and content (plusp (length content)) (gethash "text" (aref content 0)))
              payload
              (gethash "error" response)))))

(defmacro with-scratch-file ((relative) &body body)
  "Run BODY under the test project root, then delete RELATIVE if it exists."
  `(with-test-project-root
     (unwind-protect
          (progn ,@body)
       (ignore-errors
        (delete-file (merge-pathnames ,relative cl-mcp/src/project-root:*project-root*))))))

(deftest fs-test-process-has-the-overwrite-guards-verdict-installed
  (testing "loading this test system installs the edit tools' parser as the hook"
    (ok (eq (fdefinition '%file-unparseable-by-edit-tools-p)
            cl-mcp/src/fs:*lisp-file-unparseable-hook*)
        "the post-write warning and the overwrite guard follow the real verdict here")))

(deftest fs-window-start-measures-the-prefix-before-a-window
  (testing "newlines before the window and characters since the last one"
    (with-scratch-file ("tests/tmp/window-start.lisp")
      ;; "(defun a ()" plus its newline is 12 characters, so offset 15 sits
      ;; three characters into line 2.
      (fs-write-file "tests/tmp/window-start.lisp" (format nil "(defun a ()~%  (list 1))~%"))
      (multiple-value-bind (lines col)
          (fs-window-start "tests/tmp/window-start.lisp" 15)
        (ok (= 1 lines))
        (ok (= 3 col)))
      (multiple-value-bind (lines col)
          (fs-window-start "tests/tmp/window-start.lisp" 12)
        (ok (= 1 lines) "an offset at a line start has seen its newline")
        (ok (= 0 col) "and nothing of the new line yet"))
      (multiple-value-bind (lines col)
          (fs-window-start "tests/tmp/window-start.lisp" 0)
        (ok (= 0 lines))
        (ok (= 0 col)))))
  (testing "the read policy applies, as for fs-read-file"
    (with-test-project-root
      (ok (handler-case (progn (fs-window-start "/etc/passwd" 5) nil)
            (error () t))
          "a path outside the project and every registered system is refused"))))

(deftest fs-write-file-warns-when-the-written-lisp-does-not-parse
  (with-scratch-file ("tests/tmp/write-warn-new.lisp")
    (multiple-value-bind (text payload err)
        (%call-fs-write "tests/tmp/write-warn-new.lisp"
                        (format nil "(defun a (x)~%  (list x)~%~%(defun b (y)~%  (list y))~%"))
      (testing "the write itself succeeds"
        (ok (null err))
        (ok (eq t (gethash "success" payload)))
        (ok (search "Wrote tests/tmp/write-warn-new.lisp" text))
        (ok (probe-file (merge-pathnames "tests/tmp/write-warn-new.lisp"
                                         cl-mcp/src/project-root:*project-root*))))
      (testing "the text says the file does not parse and shows the diagnosis"
        (ok (search "WARNING: the file was written but does not parse." text))
        (ok (search "unclosed (form starting at line 1" text))
        (ok (search "Likely fix" text)))
      (testing "and it says the next write needs the flag"
        (ok (search "allow_unparseable_overwrite=true" text))
        (ok (eq t (gethash "unparseable" payload)))))
    (testing "the second write without the flag is refused, which is why the warning says so"
      (multiple-value-bind (text payload err)
          (%call-fs-write "tests/tmp/write-warn-new.lisp"
                          (format nil "(defun a (x)~%  (list x))~%~%(defun b (y)~%  (list y))~%"))
        (declare (ignore text payload))
        (ok err "an existing unparseable .lisp needs the opt-in")))
    (testing "the write the warning asked for succeeds and warns no more"
      (multiple-value-bind (text payload err)
          (%call-fs-write "tests/tmp/write-warn-new.lisp"
                          (format nil "(defun a (x)~%  (list x))~%~%(defun b (y)~%  (list y))~%")
                          :allow t)
        (ok (null err))
        (ng (search "WARNING" text))
        (ok (null (gethash "unparseable" payload)))))))

(deftest fs-write-file-does-not-warn-for-parseable-or-non-lisp-content
  (testing "a balanced .lisp gets the plain summary"
    (with-scratch-file ("tests/tmp/write-warn-ok.lisp")
      (multiple-value-bind (text payload)
          (%call-fs-write "tests/tmp/write-warn-ok.lisp" (format nil "(defun a () 1)~%"))
        (ng (search "WARNING" text))
        (ok (null (gethash "unparseable" payload))))))
  (testing "a .md file is never parsed"
    (with-scratch-file ("tests/tmp/write-warn-notes.md")
      (multiple-value-bind (text payload)
          (%call-fs-write "tests/tmp/write-warn-notes.md" (format nil "# Notes~%(((~%"))
        (ng (search "WARNING" text))
        (ok (null (gethash "unparseable" payload))))))
  (testing "custom reader syntax that only fails the default reader is not called broken"
    (with-scratch-file ("tests/tmp/write-warn-custom.lisp")
      (multiple-value-bind (text payload)
          (%call-fs-write "tests/tmp/write-warn-custom.lisp"
                          (format nil "(defun f ()~%  #?[(])~%"))
        (ng (search "WARNING" text) "the hook says nil for a reader-level failure")
        (ok (null (gethash "unparseable" payload)))))))

(deftest fs-write-file-warning-follows-the-hook
  (testing "without a hook there is no verdict, so no warning and no error"
    (with-scratch-file ("tests/tmp/write-warn-nohook.lisp")
      (let ((cl-mcp/src/fs:*lisp-file-unparseable-hook* nil))
        (multiple-value-bind (text payload err)
            (%call-fs-write "tests/tmp/write-warn-nohook.lisp" (format nil "(defun a ()~%"))
          (ok (null err))
          (ng (search "WARNING" text))
          (ok (null (gethash "unparseable" payload)))))))
  (testing "a hook verdict on balanced-looking text still warns, with a plain sentence"
    (with-scratch-file ("tests/tmp/write-warn-stub.lisp")
      (let ((cl-mcp/src/fs:*lisp-file-unparseable-hook*
              (lambda (pn text) (declare (ignore pn text)) t)))
        (multiple-value-bind (text payload)
            (%call-fs-write "tests/tmp/write-warn-stub.lisp" (format nil "(defun a () 1)~%"))
          (ok (search "WARNING" text))
          (ok (search "cannot parse the file as written" text))
          (ok (search "allow_unparseable_overwrite=true" text))
          (ok (eq t (gethash "unparseable" payload))))))))

(deftest fs-write-file-survives-a-hook-that-errors
  (testing "an error from the hook is no verdict: the write succeeds and says nothing"
    (with-scratch-file ("tests/tmp/write-warn-boom.lisp")
      (let ((cl-mcp/src/fs:*lisp-file-unparseable-hook*
              (lambda (pn text) (declare (ignore pn text)) (error "boom"))))
        (multiple-value-bind (text payload err)
            (%call-fs-write "tests/tmp/write-warn-boom.lisp" (format nil "(defun a ()~%"))
          (ok (null err) "the file is on disk, so this must not be reported as an error")
          (ok (eq t (gethash "success" payload)))
          (ok (search "Wrote tests/tmp/write-warn-boom.lisp" text))
          (ng (search "WARNING" text))
          (ok (null (gethash "unparseable" payload))))))))

(deftest fs-write-file-does-not-promise-an-overwrite-the-guard-would-refuse
  (testing "content past the read cap is warned about without the flag instruction"
    (with-scratch-file ("tests/tmp/write-warn-huge.lisp")
      ;; The guard re-reads the file on the next write and treats a read cut at
      ;; the cap as parseable, so it would refuse the overwrite the flag promises.
      (let ((cl-mcp/src/fs::*fs-read-max-bytes* 16))
        (multiple-value-bind (text payload err)
            (%call-fs-write "tests/tmp/write-warn-huge.lisp"
                            (format nil "(defun a (x)~%  (list x)~%"))
          (ok (null err))
          (ok (eq t (gethash "success" payload)))
          (ok (search "WARNING: the file was written but does not parse." text)
              "the breakage is still reported")
          (ok (search "larger than the fs read cap" text))
          (ok (search "split the file or fix it outside cl-mcp" text))
          (ng (search "allow_unparseable_overwrite=true" text)
              "no promise the overwrite guard cannot keep")
          (ok (eq t (gethash "unparseable" payload))))))))

(defparameter *parallel-wait-seconds* 30
  "Seconds RUN-IN-PARALLEL waits at the barrier and for each thread to finish.
Long enough that a loaded machine never trips it, short enough that a deadlock
regression fails the suite instead of hanging it.")

(defun run-in-parallel (thunk-a thunk-b)
  "Run THUNK-A and THUNK-B in two threads released together and return their
two primary values as a list.

The release is a two-party semaphore barrier -- each thread signals its own
semaphore and then waits for the other's -- so neither thunk starts until both
threads are running. No SLEEP and no timing assumption: the barrier is the
synchronisation.

A thunk that fails contributes the SERIOUS-CONDITION it signalled, not only an
ERROR, so a non-ERROR failure in one thread cannot leave the other unjoined.
Each thread signals a shared completion semaphore from an UNWIND-PROTECT and
this function waits on that instead of blocking in JOIN-THREAD, so a thread
still running after *PARALLEL-WAIT-SECONDS* leaves :DID-NOT-FINISH in its slot
and is then destroyed and joined, so a deadlock regression fails the calling
test rather than hanging the suite, and no thread outlives this call. Callers
must treat :DID-NOT-FINISH as a failure."
  (let ((a-ready (make-semaphore))
        (b-ready (make-semaphore))
        (finished (make-semaphore))
        (results (make-array 2 :initial-element :did-not-finish)))
    (flet ((runner (index mine theirs thunk)
             (lambda ()
               (unwind-protect
                    (progn
                      (signal-semaphore mine)
                      (wait-on-semaphore theirs :timeout *parallel-wait-seconds*)
                      (setf (aref results index)
                            (handler-case (funcall thunk)
                              (serious-condition (c) c))))
                 (signal-semaphore finished)))))
      (let ((threads (list (make-thread (runner 0 a-ready b-ready thunk-a)
                                        :name "cl-mcp-file-lock-test-a")
                           (make-thread (runner 1 b-ready a-ready thunk-b)
                                        :name "cl-mcp-file-lock-test-b"))))
        (let ((finished-p
                (and (wait-on-semaphore finished :timeout *parallel-wait-seconds*)
                     (wait-on-semaphore finished :timeout *parallel-wait-seconds*))))
          ;; Reap every thread before returning, whichever way the wait ended:
          ;; a thread still running would keep writing files into the rest of
          ;; the suite.  One that timed out is destroyed first, which unwinds
          ;; it and releases any lock it holds, and JOIN-THREAD on a destroyed
          ;; thread signals, so the join is guarded.
          (dolist (thread threads)
            (unless finished-p
              (when (thread-alive-p thread)
                (ignore-errors (destroy-thread thread))))
            (ignore-errors (join-thread thread))))))
    (coerce results 'list)))

(deftest file-lock-key-collapses-spellings-of-one-file
  (testing "relative, absolute and dot-dot spellings of one file share a lock"
    (with-test-project-root
      (let* ((root cl-mcp/src/project-root:*project-root*)
             (relative "src/fs.lisp")
             (absolute (native-namestring (merge-pathnames* relative root)))
             (round-trip (native-namestring
                          (merge-pathnames* "src/../src/fs.lisp" root))))
        (ok (string= (file-lock-key relative) (file-lock-key absolute))
            "a relative and an absolute spelling give one key")
        (ok (string= (file-lock-key relative) (file-lock-key round-trip))
            "a .. component is resolved away")
        (ok (eq (file-lock relative) (file-lock absolute))
            "and one key means one lock object"))))
  (testing "a symlink and its target share a lock"
    (with-test-project-root
      (let* ((root cl-mcp/src/project-root:*project-root*)
             (link (native-namestring
                    (merge-pathnames* "tests/tmp/fs-lock-link.lisp" root)))
             (target (native-namestring (merge-pathnames* "src/fs.lisp" root))))
        (ensure-directories-exist link)
        (ignore-errors (delete-file link))
        (unwind-protect
             (progn
               (uiop:run-program (list "ln" "-s" target link))
               (ok (string= (file-lock-key link) (file-lock-key target))
                   "the link resolves to its target's key")
               (ok (eq (file-lock link) (file-lock target))
                   "so both spellings take the same lock"))
          (ignore-errors (delete-file link))))))
  (testing "a file that does not exist yet keys on its unresolved absolute path"
    (with-test-project-root
      (let ((key (file-lock-key "tests/tmp/fs-lock-no-such-file.lisp")))
        (ok (search "tests/tmp/fs-lock-no-such-file.lisp" key)
            "the key is still absolute and still names the file"))))
  (testing "the fs-write-file tool's outer key is the one the function computes"
    ;; The tool locks on (ensure-write-path path) and fs-write-file locks on it
    ;; again underneath; the recursive lock only nests if both spellings key the
    ;; same, for a file that exists and for one about to be created.
    (with-test-project-root
      (let ((existing "src/fs.lisp")
            (absent "tests/tmp/fs-lock-key-absent.lisp"))
        (ok (string= (file-lock-key (ensure-write-path existing))
                     (file-lock-key existing))
            "an existing file: the outer and inner keys agree")
        (ok (string= (file-lock-key (ensure-write-path absent))
                     (file-lock-key absent))
            "and so do they for a path that does not exist yet")))))

(deftest fs-write-file-uses-a-temp-name-unique-to-the-call
  (testing "no two writes of one file can share, interleave or delete one temp"
    (with-test-project-root
      (let* ((pn (merge-pathnames* "tests/tmp/fs-lock-temp-name.txt"
                                   cl-mcp/src/project-root:*project-root*))
             (temps (loop repeat 8
                          collect (cl-mcp/src/fs::%temp-pathname-for pn)))
             (names (mapcar #'native-namestring temps)))
        (ok (= (length names)
               (length (remove-duplicates names :test #'string=)))
            "every temp name is distinct")
        (ok (every (lambda (p) (equal (pathname-directory p)
                                      (pathname-directory pn)))
                   temps)
            "each temp stays in the target's own directory, so the rename is atomic")
        (ok (every (lambda (n) (search "/.fs-lock-temp-name." n)) names)
            "and each keeps the leading dot that hides it from listings")
        (ok (every (lambda (p) (string= "tmp" (pathname-type p))) temps)
            "the extension is tmp, not the target's, so a leftover is not source")))))

(deftest fs-write-file-serialises-concurrent-writes-of-one-file
  (testing "two threads writing one file leave exactly one of the two, and no temp"
    (with-test-project-root
      (let* ((root cl-mcp/src/project-root:*project-root*)
             ;; A directory of this test's own, so "what is left behind" can be
             ;; asserted exactly rather than filtered out of the shared tmp dir.
             (relative "tests/tmp/fs-lock-write/concurrent.txt")
             (abs (merge-pathnames* relative root))
             (dir (uiop:pathname-directory-pathname abs))
             (content-a (make-string 40000 :initial-element #\a))
             (content-b (make-string 40000 :initial-element #\b))
             (bad-round nil))
        (ensure-directories-exist abs)
        (unwind-protect
             (dotimes (round 10)
               (when bad-round (return))
               (let ((outcomes
                       (run-in-parallel
                        (lambda () (fs-write-file relative content-a))
                        (lambda () (fs-write-file relative content-b)))))
                 (when (member :did-not-finish outcomes)
                   (setf bad-round (list round :did-not-finish))
                   (return)))
               (let ((after (fs-read-file (native-namestring abs)))
                     (files (uiop:directory-files dir)))
                 (unless (or (string= after content-a) (string= after content-b))
                   (setf bad-round (or bad-round (list round :mixed-content
                                                       (length after)))))
                 (unless (= 1 (length files))
                   (setf bad-round (or bad-round
                                       (list round :files-left
                                             (mapcar #'native-namestring files)))))))
          (ignore-errors (delete-file abs))
          (ignore-errors (uiop:delete-empty-directory dir)))
        (ok (null bad-round)
            (if bad-round
                (format nil "round ~S is not one writer's content alone" bad-round)
                "every round left one writer's content whole, with no temp behind"))))))

(deftest fs-write-file-tool-decides-and-writes-under-one-lock
  (testing "two threads creating one new .lisp file: one creates, one is refused"
    ;; The overwrite guard allows a write to a .lisp path that does not exist
    ;; yet. Read outside the lock, that verdict is stale the moment the other
    ;; thread creates the file, and the loser's whole-file write lands on an
    ;; existing Lisp source -- exactly what the guard exists to prevent.
    (with-test-project-root
      (let* ((relative "tests/tmp/fs-lock-create.lisp")
             (abs (merge-pathnames* relative cl-mcp/src/project-root:*project-root*))
             (bad nil))
        (ensure-directories-exist abs)
        (unwind-protect
             (dotimes (round 15)
               (when bad (return))
               (ignore-errors (delete-file abs))
               (let* ((outcomes
                        (run-in-parallel
                         (lambda ()
                           (multiple-value-list
                            (%call-fs-write relative "(defun a () 1)")))
                         (lambda ()
                           (multiple-value-list
                            (%call-fs-write relative "(defun b () 2)")))))
                      (wrote (count-if
                              (lambda (o) (and (consp o)
                                               (hash-table-p (second o))
                                               (eq t (gethash "success" (second o)))))
                              outcomes))
                      (refusals (remove-if-not
                                 (lambda (o) (and (consp o) (hash-table-p (third o))))
                                 outcomes)))
                 (cond ((member :did-not-finish outcomes)
                        (setf bad (list round :did-not-finish)))
                       ((/= wrote 1)
                        (setf bad (list round :writers wrote)))
                       ((/= (length refusals) 1)
                        (setf bad (list round :refusals (length refusals))))
                       ((not (equal "existing_lisp_overwrite_forbidden"
                                    (gethash "code"
                                             (gethash "data"
                                                      (third (first refusals))))))
                        (setf bad (list round :wrong-refusal))))))
          (ignore-errors (delete-file abs)))
        (ok (null bad)
            (if bad
                (format nil "round ~S: the create decision was not made under the write's lock"
                        bad)
                "exactly one creates; the other is refused as an existing .lisp overwrite"))))))

(defparameter *overwrite-gate-seconds* 0.3
  "Per-round bound on how long the overwrite-guard hook below holds the
decision open for the concurrent edit thread. On the code path under test
(FS-WRITE-FILE's tool body: decide, then write, under one lock) this ALWAYS
times out -- the edit thread is blocked on that very lock and cannot signal
back until the decide-and-write span finishes and releases it, so hitting
this bound is expected on every round and proves nothing by itself; only the
file left on disk, and which side reports success, is evidence. Without the
lock the edit is not blocked and normally finishes and signals back well
inside this bound, but a slow or loaded machine could still make one round
miss it by scheduling luck alone -- which is why no single round is trusted;
see the dotimes below.")

(deftest fs-write-file-tool-excludes-a-concurrent-lisp-edit-form
  (testing "a structural edit cannot land between the overwrite check and the write"
    (with-test-project-root
      (let* ((relative "tests/tmp/fs-lock-overwrite-race.lisp")
             (abs (merge-pathnames* relative cl-mcp/src/project-root:*project-root*))
             (path (native-namestring abs))
             (original (format nil "(defun alpha () :alpha-old)~%~%(defun beta () :beta-old)~%"))
             (overwrite (format nil "(defun gamma () :gamma)~%"))
             (bad nil))
        (ensure-directories-exist abs)
        (unwind-protect
             ;; A single round's final state does not prove the lock works: on
             ;; a slow or loaded machine an unlocked edit could lose the race
             ;; by scheduling luck alone, inside the very window a working
             ;; lock also spends waiting out *OVERWRITE-GATE-SECONDS* every
             ;; time (see its docstring). Repeating the race from a freshly
             ;; written file is what makes a broken lock fail this test: a
             ;; broken lock only has to win once across all the rounds to be
             ;; caught, so it is the odds of every round happening to look
             ;; correct by chance that vanish, not the odds of any one round
             ;; doing so.
             (dotimes (round 15)
               (when bad (return))
               (fs-write-file relative original)
               (let* ((checked (make-semaphore))
                      (edited (make-semaphore))
                      (fired nil)
                      (outcomes
                        (run-in-parallel
                         (lambda ()
                           ;; The hook IS the overwrite guard's verdict, and it is
                           ;; handed the exact text the decision is made from. It
                           ;; publishes "the check has read the file", gives the
                           ;; other thread its chance, and only then answers from
                           ;; that text -- so the decision provably predates
                           ;; whatever the edit did. Bound inside the thread: a
                           ;; binding made in the parent would not be visible here.
                           (let ((cl-mcp/src/fs:*lisp-file-unparseable-hook*
                                   (lambda (pn text)
                                     (declare (ignore pn))
                                     (unless fired
                                       (setf fired t)
                                       (signal-semaphore checked)
                                       (wait-on-semaphore edited
                                                          :timeout *overwrite-gate-seconds*))
                                     (and (search ":alpha-old" text) t))))
                             (multiple-value-list
                              (%call-fs-write relative overwrite :allow t))))
                         (lambda ()
                           ;; Unlike the hook's wait above, a timeout HERE is never
                           ;; expected on either path: CHECKED is the hook's very
                           ;; first act, so a miss within *PARALLEL-WAIT-SECONDS*
                           ;; means the hook was never reached at all -- a broken
                           ;; race setup, not evidence about the lock -- and is
                           ;; reported as its own failure below rather than let
                           ;; through as a silent, unsynchronised attempt.
                           (if (wait-on-semaphore checked :timeout *parallel-wait-seconds*)
                               (unwind-protect
                                    (handler-case
                                        (progn
                                          (lisp-edit-form
                                           :file-path path :form-type "defun"
                                           :form-name "alpha" :operation "replace"
                                           :content "(defun alpha () :alpha-new)")
                                          :edited)
                                      (error () :refused))
                                 (signal-semaphore edited))
                               :checked-timeout))))
                      (write-outcome (first outcomes))
                      (wrote (and (consp write-outcome)
                                  (hash-table-p (second write-outcome))
                                  (eq t (gethash "success" (second write-outcome)))))
                      (edit-won (eq (second outcomes) :edited))
                      (after (fs-read-file path)))
                 (cond
                   ((member :did-not-finish outcomes)
                    (setf bad (list round :did-not-finish outcomes)))
                   ((eq (second outcomes) :checked-timeout)
                    (setf bad (list round :checked-never-signalled)))
                   ((and wrote edit-won)
                    (setf bad (list round :both-succeeded
                                    "overwrite landed on top of a successful edit")))
                   ((not (or wrote edit-won))
                    (setf bad (list round :neither-succeeded outcomes)))
                   (wrote
                    (unless (string= after overwrite)
                      (setf bad (list round :overwrite-but-wrong-content after))))
                   (t
                    (unless (search ":alpha-new" after)
                      (setf bad (list round :edit-but-wrong-content after)))))))
          (ignore-errors (delete-file abs)))
        (ok (null bad)
            (if bad
                (format nil "round ~S: ~S" (first bad) (rest bad))
                "every round left exactly one side's decision on disk, and the file agrees"))))))

(deftest project-root-survives-a-round-trip-through-its-own-reported-path
  (testing "the path fs-get-project-info reports is one fs-set-project-root takes"
    ;; This is the most direct round trip cl-mcp offers, and it was broken for a
    ;; root holding [ or ]: the tool returned a native path, while its input side
    ;; still handed the string to the CL pathname reader, which read [br] as
    ;; wildcard syntax and rejected the result as a wild pathname.
    (let* ((original-root cl-mcp/src/project-root:*project-root*)
           (original-cwd (ignore-errors (getcwd)))
           ;; fs-set-project-root moves this too, and the cleanup below deletes
           ;; the directory it would be left pointing at.
           (original-defaults *default-pathname-defaults*)
           (base (ensure-directory-pathname (system-source-directory "cl-mcp")))
           ;; Built natively: MERGE-PATHNAMES on a string with brackets parses
           ;; them as wild, which is the confusion under test.
           (dir (uiop:parse-native-namestring
                 (format nil "~Atests/tmp/fsroot[br]/" (native-namestring base))
                 :ensure-directory t)))
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             (ok (fs-set-project-root (native-namestring dir))
                 "the bracketed root is accepted at all")
             (let ((reported (gethash "project_root" (fs-get-project-info))))
               (ok (search "fsroot[br]" reported)
                   (format nil "the reported root keeps its brackets, got ~S" reported))
               (ok (not (find #\\ reported))
                   (format nil "and is not escaped for the reader, got ~S" reported))
               (let ((again (fs-set-project-root reported)))
                 (ok (string= (gethash "project_root" again) reported)
                     "feeding the reported root back in lands on the same root"))))
        (setf cl-mcp/src/project-root:*project-root* original-root
              *default-pathname-defaults* original-defaults)
        (when original-cwd (ignore-errors (uiop:chdir original-cwd)))
        (ignore-errors (uiop:delete-empty-directory dir))))))
