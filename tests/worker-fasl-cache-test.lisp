;;;; tests/worker-fasl-cache-test.lisp
;;;;
;;;; A worker compiles its project root's files into a FASL cache of its own
;;;; (issue #317).  Process tier: the last test spawns two real workers.

(defpackage #:cl-mcp/tests/worker-fasl-cache-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/worker/fasl-cache
                #:*worker-fasl-cache*
                #:sweep-dead-worker-caches
                #:prepare-worker-fasl-cache
                #:isolate-project-fasls)
  (:import-from #:cl-mcp/src/worker-client
                #:spawn-worker
                #:worker-rpc
                #:worker-pid
                #:kill-worker)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p))

(in-package #:cl-mcp/tests/worker-fasl-cache-test)

(defun %scratch-directory (name)
  "Return a fresh, empty directory NAME-<random> under tests/tmp/."
  (let ((directory (asdf:system-relative-pathname
                    :cl-mcp (format nil "tests/tmp/~A-~36R/" name (random (expt 36 8))))))
    (ensure-directories-exist directory)
    directory))

(defun %write-text (path text)
  "Write TEXT to PATH, creating its directory first."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string text out)))

(defun %ended-pid ()
  "Return the PID of a process that has already ended."
  (let ((process (sb-ext:run-program "true" '() :search t :wait t)))
    (prog1 (sb-ext:process-pid process)
      (sb-ext:process-close process))))

(defmacro with-saved-translations (&body body)
  "Run BODY, then put ASDF's output translations back as they were."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (asdf/output-translations:output-translations)))
       (unwind-protect (progn ,@body)
         (setf (asdf/output-translations:output-translations) ,saved)))))

(deftest isolate-project-fasls-does-nothing-outside-a-worker
  (testing "without a worker cache, the translations are left alone"
    (with-saved-translations
      (let ((*worker-fasl-cache* nil)
            (before (asdf/output-translations:output-translations)))
        (ok (null (isolate-project-fasls (asdf:system-source-directory :cl-mcp)))
            "nothing is isolated")
        (ok (equal before (asdf/output-translations:output-translations))
            "the translations are unchanged")))))

(deftest isolate-project-fasls-moves-the-root-s-files-alone
  (let ((base (%scratch-directory "fasl-cache-base"))
        (root (%scratch-directory "fasl-cache-root")))
    (unwind-protect
         (with-saved-translations
           (let* ((*worker-fasl-cache* nil)
                  (outside (asdf:system-relative-pathname :cl-mcp "src/fs.fasl"))
                  (outside-before (asdf:apply-output-translations outside))
                  (own (prepare-worker-fasl-cache base)))
             (testing "the worker's own cache is BASE/<pid>/"
               (ok (equal (namestring own)
                          (namestring (merge-pathnames (format nil "~D/" (sb-posix:getpid))
                                                       base)))
                   "named by the PID")
               (ok (uiop:directory-exists-p own) "and made"))
             (let ((cache (isolate-project-fasls root))
                   (count (length (asdf/output-translations:output-translations))))
               (testing "a file under the root compiles into the worker's cache"
                 (ok (uiop:subpathp (asdf:apply-output-translations
                                     (merge-pathnames "src/a.fasl" (truename root)))
                                    own)
                     "inside the worker's cache")
                 (ok (uiop:subpathp cache own) "the root's place is inside it too"))
               (testing "a file outside the root keeps the shared cache"
                 (ok (equal outside-before (asdf:apply-output-translations outside))
                     "unchanged"))
               (testing "setting the same root again adds nothing"
                 (isolate-project-fasls root)
                 (ok (= count (length (asdf/output-translations:output-translations)))
                     "one entry per root")))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(deftest sweep-dead-worker-caches-deletes-only-ended-workers
  (let* ((base (%scratch-directory "fasl-cache-sweep"))
         (ended (merge-pathnames (format nil "~D/" (%ended-pid)) base))
         (own (merge-pathnames (format nil "~D/" (sb-posix:getpid)) base))
         (parent (merge-pathnames (format nil "~D/" (sb-posix:getppid)) base))
         (not-a-pid (merge-pathnames "notes/" base))
         (outside (%scratch-directory "fasl-cache-outside"))
         (link (merge-pathnames "999999999" base)))
    (unwind-protect
         (progn
           (dolist (directory (list ended own parent not-a-pid))
             (%write-text (merge-pathnames "a.fasl" directory) "x"))
           (%write-text (merge-pathnames "keep.fasl" outside) "x")
           ;; A link named like the PID of no process, pointing out of the base.
           (sb-posix:symlink (uiop:native-namestring outside)
                             (uiop:native-namestring link))
           (let ((deleted (sweep-dead-worker-caches base)))
             (testing "an ended worker's cache is deleted"
               (ok (not (uiop:directory-exists-p ended)) "gone")
               (ok (= 1 deleted) "and only it is counted"))
             (testing "the rest is kept"
               (ok (uiop:directory-exists-p own) "this process's own")
               (ok (uiop:directory-exists-p parent) "a running process's")
               (ok (uiop:directory-exists-p not-a-pid) "a directory not named by a PID")
               (ok (probe-file (merge-pathnames "keep.fasl" outside))
                   "and nothing behind a link out of the base"))))
      (ignore-errors (sb-posix:unlink (uiop:native-namestring link)))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree outside :validate t :if-does-not-exist :ignore))))

(defun %eval-in (worker code)
  "Evaluate CODE in WORKER's CL-USER and return the response's text."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "code" params) code
          (gethash "package" params) "CL-USER")
    (let* ((result (worker-rpc worker "worker/eval" params :timeout 180))
           (content (and (hash-table-p result) (gethash "content" result))))
      (if (and (vectorp content) (plusp (length content)))
          (or (gethash "text" (aref content 0)) "")
          ""))))

(defun %set-worker-root (worker root)
  "Make ROOT the project root of WORKER, as fs-set-project-root does."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "path" params) (uiop:native-namestring root))
    (worker-rpc worker "worker/set-project-root" params :timeout 60)))

(defparameter *macro-first-a*
  "(defpackage #:fasl-cache-fixture (:use #:cl))
(in-package #:fasl-cache-fixture)
(defmacro twice (x) `(* 2 ,x))
(defun run () (twice 21))
"
  "a.lisp before the edit: the macro is defined before its use.")

(defparameter *macro-moved-a*
  "(defpackage #:fasl-cache-fixture (:use #:cl))
(in-package #:fasl-cache-fixture)
(defun run () (twice 21))
"
  "a.lisp after the edit: the macro moved to b.lisp, which loads after it, so
compiled from source RUN calls TWICE as a function.")

(defparameter *macro-moved-b*
  "(in-package #:fasl-cache-fixture)
(defmacro twice (x) `(* 2 ,x))
")

(defparameter *run-code*
  "(handler-case (progn (asdf:load-system \"fasl-cache-fixture\")
                       (format nil \"RESULT=~A\" (uiop:symbol-call \"FASL-CACHE-FIXTURE\" \"RUN\")))
     (error () \"RESULT=FAILED\"))"
  "Load the fixture with plain ASDF -- no same-second FASL handling -- and call RUN.")

(deftest a-fresh-worker-builds-the-project-from-source
  ;; Issue #317: a worker that had the macro in its image compiled a.lisp with
  ;; it expanded, into the cache every worker shared.  The FASL was newer than
  ;; a.lisp, so the next worker loaded it and passed, while a clean build
  ;; calls TWICE as a function and fails.
  (unless (spawn-available-p)
    (skip "Cannot spawn workers"))
  (let* ((root (%scratch-directory "fasl-cache-project"))
         (a (merge-pathnames "a.lisp" root))
         (b (merge-pathnames "b.lisp" root))
         (shared-a-fasl (asdf:apply-output-translations (compile-file-pathname (merge-pathnames "a.lisp" (truename root)))))
         (first-worker nil)
         (second-worker nil))
    (unwind-protect
         (progn
           (%write-text (merge-pathnames "fasl-cache-fixture.asd" root)
                        "(asdf:defsystem \"fasl-cache-fixture\" :serial t
  :components ((:file \"a\") (:file \"b\")))
")
           (%write-text a *macro-first-a*)
           (%write-text b "(in-package #:fasl-cache-fixture)
(defun unused () nil)
")
           (setf first-worker (spawn-worker))
           (%set-worker-root first-worker root)
           (testing "the first worker builds and runs it"
             (ok (search "RESULT=42" (%eval-in first-worker *run-code*)) "42 before the edit"))
           ;; The macro moves to the later file; this worker still has it.
           (%write-text a *macro-moved-a*)
           (%write-text b *macro-moved-b*)
           (testing "the first worker still runs it after the edit"
             (ok (search "RESULT=42" (%eval-in first-worker *run-code*))
                 "its image holds the macro"))
           (testing "and compiled nothing into the shared cache"
             (ok (not (probe-file shared-a-fasl)) "no a.fasl there"))
           (kill-worker first-worker)
           (setf first-worker nil)
           (setf second-worker (spawn-worker))
           (%set-worker-root second-worker root)
           (testing "a fresh worker compiles the saved source and fails as a clean build does"
             (ok (search "RESULT=FAILED" (%eval-in second-worker *run-code*))
                 "TWICE is called as a function"))
           (testing "into a cache of its own"
             (ok (search (format nil "/worker-fasls/~D/" (worker-pid second-worker))
                         (%eval-in second-worker
                                   (format nil "(namestring (asdf:apply-output-translations ~S))"
                                           (namestring (merge-pathnames "a.fasl" (truename root))))))
                 "named by its PID")))
      (when first-worker (kill-worker first-worker))
      (when second-worker (kill-worker second-worker))
      (ignore-errors (delete-file shared-a-fasl))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
