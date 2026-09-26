;;;; tests/session-project-root-test.lisp
;;;;
;;;; A session's project root is its own (#129): setting one, by
;;;; fs-set-project-root or by initialize's rootPath, must not move the root
;;;; another session's parent-side tools resolve paths against.

(defpackage #:cl-mcp/tests/session-project-root-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/protocol
                #:process-json-line)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*
                #:session-project-root
                #:forget-session-project-root)
  ;; A bare :import-from declares the dependency; the session table it clears
  ;; is internal and found by name where it is used.
  (:import-from #:cl-mcp/src/http)
  (:import-from #:yason #:parse))

(in-package #:cl-mcp/tests/session-project-root-test)

(defun %scratch-dir (name)
  "Return a fresh directory NAME under tests/tmp/ of the cl-mcp source tree."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "tests/tmp/session-root-~A/" name)
                               (asdf:system-source-directory :cl-mcp)))))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist dir)
    (truename dir)))

(defun %call (session method params)
  "Send METHOD with PARAMS (a hash-table) as SESSION, the way a transport does,
and return the parsed response."
  (let ((*current-session-id* session)
        (msg (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" msg) "2.0"
          (gethash "id" msg) 1
          (gethash "method" msg) method
          (gethash "params" msg) params)
    (parse (process-json-line
            (with-output-to-string (s) (yason:encode msg s))))))

(defun %tool (session name &rest args)
  "Call tool NAME as SESSION with ARGS, a plist of JSON keys and values, and
return the response's result, or its error when it has no result."
  (let ((params (make-hash-table :test #'equal))
        (arguments (make-hash-table :test #'equal)))
    (loop for (k v) on args by #'cddr
          do (setf (gethash k arguments) v))
    (setf (gethash "name" params) name
          (gethash "arguments" params) arguments)
    (let ((resp (%call session "tools/call" params)))
      (or (gethash "result" resp) (gethash "error" resp)))))

(defun %root-of (session)
  "The project root SESSION's parent-side tools resolve against, as a native
namestring, or NIL when it has none."
  (gethash "project_root" (%tool session "fs-get-project-info")))

(defun %native (pathname)
  (uiop:native-namestring pathname))

(defun call-with-isolated-roots (thunk sessions)
  "Call THUNK without the worker pool, then put back the global root, the
default pathname, the working directory, forget SESSIONS' roots and delete the
scratch directories %SCRATCH-DIR made."
  (let ((root *project-root*)
        (defaults *default-pathname-defaults*)
        (cwd (ignore-errors (uiop:getcwd)))
        (*use-worker-pool* nil))
    (unwind-protect (funcall thunk)
      (mapc #'forget-session-project-root sessions)
      (setf *project-root* root
            *default-pathname-defaults* defaults)
      (when cwd (ignore-errors (uiop:chdir cwd)))
      (dolist (dir (directory (merge-pathnames "tests/tmp/session-root-*/"
                                               (asdf:system-source-directory :cl-mcp))))
        (ignore-errors
          (uiop:delete-directory-tree dir :validate t))))))

(defmacro with-isolated-roots ((&rest sessions) &body body)
  `(call-with-isolated-roots (lambda () ,@body) (list ,@sessions)))

(deftest a-session-root-does-not-move-another-session
  (testing "fs-set-project-root in one session leaves the other and the default alone"
    (with-isolated-roots ("tA" "tB")
      (let ((default (%scratch-dir "default"))
            (a (%scratch-dir "a")))
        (setf *project-root* default)
        (%tool "tA" "fs-set-project-root" "path" (%native a))
        (ok (equal (%root-of "tA") (%native a))
            "the session that set the root works under it")
        (ok (equal (%root-of "tB") (%native default))
            "another session still works under the server default")
        (ok (equal (%native *project-root*) (%native default))
            "the global default is untouched")
        (ok (equal (gethash "project_root_source"
                            (%tool "tA" "fs-get-project-info"))
                   "session")
            "fs-get-project-info says the root is the session's own")))))

(deftest a-relative-write-lands-in-the-writing-sessions-tree
  (testing "the #129 hazard: a relative path must not resolve under the other root"
    (with-isolated-roots ("tA" "tB")
      (let ((a (%scratch-dir "write-a"))
            (b (%scratch-dir "write-b")))
        (%tool "tA" "fs-set-project-root" "path" (%native a))
        ;; B sets its root after A, which used to re-point A as well.
        (%tool "tB" "fs-set-project-root" "path" (%native b))
        (%tool "tA" "fs-write-file" "path" "note.txt" "content" "from A")
        (ok (probe-file (merge-pathnames "note.txt" a))
            "A's relative write lands in A's tree")
        (ok (not (probe-file (merge-pathnames "note.txt" b)))
            "and not in B's, which B set last")
        (let ((listing (%tool "tB" "fs-list-directory" "path" ".")))
          (ok (not (search "note.txt" (with-output-to-string (s)
                                         (yason:encode listing s))))
              "B's listing is of B's tree"))))))

(deftest initialize-sets-only-the-connecting-sessions-root
  (testing "a client connecting with rootPath does not re-point the sessions already there"
    (with-isolated-roots ("tA" "tB")
      (let ((a (%scratch-dir "init-a"))
            (b (%scratch-dir "init-b"))
            (params (make-hash-table :test #'equal)))
        (%tool "tA" "fs-set-project-root" "path" (%native a))
        (setf (gethash "protocolVersion" params) "2025-06-18"
              (gethash "rootPath" params) (%native b))
        (ok (gethash "result" (%call "tB" "initialize" params))
            "initialize succeeds")
        (ok (equal (%root-of "tB") (%native b))
            "the connecting session works under its rootPath")
        (ok (equal (%root-of "tA") (%native a))
            "the session already connected keeps its own root")))))

(deftest a-root-set-outside-any-session-is-the-default
  (testing "with no session id the call sets the global default, as before"
    (with-isolated-roots ("tA")
      (let ((d (%scratch-dir "global")))
        (%tool nil "fs-set-project-root" "path" (%native d))
        (ok (equal (%native *project-root*) (%native d))
            "the global default moved")
        (ok (null (session-project-root "tA"))
            "no session entry was made")
        (ok (equal (%root-of "tA") (%native d))
            "a session with no root of its own works under the new default")))))

(deftest an-ended-session-forgets-its-root
  (testing "deleting an HTTP session drops its root, so a reused id starts from the default"
    (with-isolated-roots ("tA")
      (let ((default (%scratch-dir "forget-default"))
            (a (%scratch-dir "forget-a")))
        (setf *project-root* default)
        (%tool "tA" "fs-set-project-root" "path" (%native a))
        (ok (session-project-root "tA") "the session has a root")
        (cl-mcp/src/http::delete-session "tA")
        (ok (null (session-project-root "tA")) "the ended session's root is gone")
        (ok (equal (%root-of "tA") (%native default))
            "the id now resolves against the default")))))
