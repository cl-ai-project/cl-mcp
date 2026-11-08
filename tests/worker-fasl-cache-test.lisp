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
                #:*worker-cache-lock-fd*
                #:*isolated-roots*
                #:sweep-dead-worker-caches
                #:prepare-worker-fasl-cache
                #:isolate-project-fasls
                #:ensure-project-fasls-isolated
                #:%try-lock)
  (:import-from #:cl-mcp/src/worker-client
                #:spawn-worker
                #:worker-rpc
                #:worker-pid
                #:kill-worker)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p))

(in-package #:cl-mcp/tests/worker-fasl-cache-test)

(defun %scratch-directory (name)
  "Return a fresh, empty directory NAME-<pid>-<random> under tests/tmp/."
  (let ((directory (asdf:system-relative-pathname
                    :cl-mcp (format nil "tests/tmp/~A-~D-~36R/" name (sb-posix:getpid)
                                    (random (expt 36 8) (make-random-state t))))))
    (ensure-directories-exist directory)
    directory))

(defun %write-text (path text)
  "Write TEXT to PATH, creating its directory first."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string text out)))

(defun %hold-lock-in-another-process (path)
  "Start a process that takes a POSIX record lock on PATH, as a live worker holds
its cache's, and return it once the lock is held; NIL when python3 is missing."
  (let ((process (ignore-errors
                  (sb-ext:run-program
                   "python3"
                   (list "-c" "import fcntl,sys,time
f=open(sys.argv[1],'a')
fcntl.lockf(f,fcntl.LOCK_EX)
print('locked',flush=True)
time.sleep(120)"
                         (uiop:native-namestring path))
                   :search t :wait nil :output :stream))))
    (when process
      (if (equal "locked" (ignore-errors (read-line (sb-ext:process-output process) nil)))
          process
          (progn (ignore-errors (sb-ext:process-kill process 9))
                 nil)))))

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
                  (*worker-cache-lock-fd* nil)
                  (*isolated-roots* '())
                  (outside (asdf:system-relative-pathname :cl-mcp "src/fs.fasl"))
                  (outside-before (asdf:apply-output-translations outside))
                  (own (prepare-worker-fasl-cache base))
                  (inside (merge-pathnames "src/a.fasl" (truename root))))
             (unwind-protect
                  (progn
                    (testing "the worker's own cache is BASE/<pid>-<token>/, claimed by its lock"
                      (ok (uiop:string-prefix-p
                           (format nil "~D-" (sb-posix:getpid))
                           (car (last (pathname-directory own))))
                          "named by the PID and a token")
                      (ok (uiop:directory-exists-p own) "made")
                      (ok (integerp *worker-cache-lock-fd*) "and locked"))
                    (let ((cache (isolate-project-fasls root))
                          (count (length (asdf/output-translations:output-translations))))
                      (testing "a file under the root compiles into the worker's cache"
                        (ok (uiop:subpathp (asdf:apply-output-translations inside) own)
                            "inside the worker's cache")
                        (ok (uiop:subpathp cache own) "the root's place is inside it too"))
                      (testing "a file outside the root keeps the shared cache"
                        (ok (equal outside-before (asdf:apply-output-translations outside))
                            "unchanged"))
                      (testing "setting the same root again adds nothing"
                        (isolate-project-fasls root)
                        (ok (= count (length (asdf/output-translations:output-translations)))
                            "one entry per root"))
                      (testing "a cleared configuration gets the entry back"
                        (asdf:clear-output-translations)
                        (ok (= 1 (ensure-project-fasls-isolated)) "one entry put back")
                        (ok (uiop:subpathp (asdf:apply-output-translations inside) own)
                            "the root's files go to the worker's cache again")
                        (ok (zerop (ensure-project-fasls-isolated))
                            "and nothing more while it holds"))))
               (when *worker-cache-lock-fd*
                 (sb-posix:close *worker-cache-lock-fd*)))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(deftest sweep-dead-worker-caches-deletes-only-ended-workers
  (let* ((base (%scratch-directory "fasl-cache-sweep"))
         (ended (merge-pathnames "111-aaa/" base))
         (no-lock (merge-pathnames "222-bbb/" base))
         (live (merge-pathnames "333-ccc/" base))
         (own (merge-pathnames "444-ddd/" base))
         (not-a-cache (merge-pathnames "notes/" base))
         (orphan-lock (merge-pathnames "666-fff.lock" base))
         (outside (%scratch-directory "fasl-cache-outside"))
         (link (merge-pathnames "555-eee" base))
         (unopenable (merge-pathnames "000-unopenable/" base))
         (holder nil))
    (unwind-protect
         (progn
           (dolist (directory (list ended no-lock live own not-a-cache))
             (%write-text (merge-pathnames "a.fasl" directory) "x"))
           (dolist (lock (list (merge-pathnames "111-aaa.lock" base)
                               (merge-pathnames "333-ccc.lock" base)
                               orphan-lock))
             (%write-text lock ""))
           (%write-text (merge-pathnames "keep.fasl" outside) "x")
           ;; A link named like a cache, pointing out of the base.
           (sb-posix:symlink (uiop:native-namestring outside) (uiop:native-namestring link))
           (setf holder (%hold-lock-in-another-process (merge-pathnames "333-ccc.lock" base)))
           ;; A lock this process cannot open: judged held, and the sweep goes
           ;; on to the others instead of stopping there.
           (%write-text (merge-pathnames "a.fasl" unopenable) "x")
           (%write-text (merge-pathnames "000-unopenable.lock" base) "")
           (sb-posix:chmod (uiop:native-namestring (merge-pathnames "000-unopenable.lock" base))
                           0)
           (let ((deleted (let ((*worker-fasl-cache* own))
                            (sweep-dead-worker-caches base))))
             (testing "a cache whose lock nobody holds is deleted with its lock"
               (ok (not (uiop:directory-exists-p ended)) "the directory is gone")
               (ok (not (probe-file (merge-pathnames "111-aaa.lock" base))) "and its lock"))
             (testing "a cache with no lock file is deleted"
               (ok (not (uiop:directory-exists-p no-lock)) "gone"))
             (testing "a lock left without its directory is deleted"
               (ok (not (probe-file orphan-lock)) "gone"))
             (testing "only caches are counted"
               ;; Root opens the unopenable lock too, and deletes that cache; with
               ;; no process holding 333-ccc's lock, that one goes as well.
               (ok (= (+ 2 (if (zerop (sb-posix:geteuid)) 1 0) (if holder 0 1)) deleted)
                   (format nil "~D" deleted)))
             (testing "the rest is kept"
               (if holder
                   (ok (uiop:directory-exists-p live) "a cache whose lock a process holds")
                   (skip "python3 is needed to hold a lock in another process"))
               (ok (uiop:directory-exists-p own) "this worker's own")
               (ok (uiop:directory-exists-p not-a-cache) "a directory not named like a cache")
               (ok (probe-file (merge-pathnames "keep.fasl" outside))
                   "and nothing behind a link out of the base")
               (if (zerop (sb-posix:geteuid))
                   (skip "root opens a file whatever its mode")
                   (ok (uiop:directory-exists-p unopenable)
                       "a cache whose lock cannot be opened, the sweep going on past it")))))
      (when holder
        (ignore-errors (sb-ext:process-kill holder 9))
        (ignore-errors (sb-ext:process-wait holder))
        (ignore-errors (sb-ext:process-close holder)))
      (ignore-errors (sb-posix:unlink (uiop:native-namestring link)))
      (ignore-errors (sb-posix:chmod (uiop:native-namestring
                                      (merge-pathnames "000-unopenable.lock" base))
                                     #o600))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree outside :validate t :if-does-not-exist :ignore))))

(defmacro with-try-lock-wrapped ((wrapper) &body body)
  "Run BODY with %TRY-LOCK called through WRAPPER, a function of the original
function and its arguments.  The sb-posix calls inside %TRY-LOCK are inlined,
so the lock's outcomes are steered at %TRY-LOCK itself."
  `(progn
     (sb-int:encapsulate '%try-lock 'test-wrapper ,wrapper)
     (unwind-protect (progn ,@body)
       (sb-int:unencapsulate '%try-lock 'test-wrapper))))

(deftest claiming-a-cache-copes-with-a-lock-it-cannot-take
  (let ((base (%scratch-directory "fasl-cache-claim")))
    (unwind-protect
         (progn
           (testing "a lock held for a moment by a sweep sends it to a fresh name"
             (let ((calls 0)
                   (*worker-fasl-cache* nil)
                   (*worker-cache-lock-fd* nil))
               (with-try-lock-wrapped ((lambda (function &rest arguments)
                                         (if (= 1 (incf calls))
                                             nil
                                             (apply function arguments))))
                 (let ((own (prepare-worker-fasl-cache base)))
                   (unwind-protect
                        (progn
                          (ok (and own (uiop:directory-exists-p own)) "it gets a cache")
                          (ok (= 2 calls) "on the second name"))
                     (when *worker-cache-lock-fd*
                       (sb-posix:close *worker-cache-lock-fd*)))))))
           (testing "a lock that fails outright leaves no lock file behind"
             (let ((*worker-fasl-cache* nil)
                   (*worker-cache-lock-fd* nil)
                   (minted nil))
               ;; The lock file is made, then the lock fails, as with ENOLCK.
               (with-try-lock-wrapped ((lambda (function &rest arguments)
                                         (setf minted (first arguments))
                                         (let ((fd (apply function arguments)))
                                           (when fd (sb-posix:close fd))
                                           (error "No locks available"))))
                 (ok (null (prepare-worker-fasl-cache base)) "the worker falls back")
                 (ok (and minted (not (probe-file minted))) "and its own lock file is gone"))))
           (testing "a cache directory that cannot be made takes its lock file with it"
             (let ((*worker-fasl-cache* nil)
                   (*worker-cache-lock-fd* nil)
                   (minted nil))
               ;; A plain file where the directory should go.
               (with-try-lock-wrapped ((lambda (function &rest arguments)
                                         (setf minted (first arguments))
                                         (let ((fd (apply function arguments)))
                                           (%write-text (make-pathname :type nil
                                                                       :defaults minted)
                                                        "")
                                           fd)))
                 (ok (null (prepare-worker-fasl-cache base)) "the worker falls back")
                 (ok (and minted (not (probe-file minted))) "and the lock file is gone"))))
           (testing "a sweep's open never recreates a lock file that vanished"
             (let ((missing (merge-pathnames "999-gone.lock" base)))
               (ok (null (%try-lock missing :create nil)) "not taken")
               (ok (not (probe-file missing)) "and not made"))))
      (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore))))

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

(defun %worker-cache-of (worker)
  "Return the native namestring of WORKER's own FASL cache, as it reports it."
  (let ((text (%eval-in worker "(namestring cl-mcp/src/worker/fasl-cache:*worker-fasl-cache*)")))
    (subseq text (1+ (position #\" text)) (position #\" text :from-end t))))

(defun %swept-within (seconds directory)
  "Sweep the real cache base until DIRECTORY is gone or SECONDS have passed, and
return true when it is gone: a killed worker's lock is dropped only once the
kernel has ended the process.  The real base holds every worker's cache on this
machine, so this deletes every ended worker's, as a starting worker would."
  (loop with deadline = (+ (get-internal-real-time)
                           (* seconds internal-time-units-per-second))
        do (sweep-dead-worker-caches)
           (unless (uiop:directory-exists-p directory)
             (return t))
           (when (> (get-internal-real-time) deadline)
             (return nil))
           (sleep 0.1)))

(deftest a-fresh-worker-builds-the-project-from-source
  ;; Issue #317: a worker that had the macro in its image compiled a.lisp with
  ;; it expanded, into the cache every worker shared.  The FASL was newer than
  ;; a.lisp, so the next worker loaded it and passed, while a clean build
  ;; calls TWICE as a function and fails.
  (if (not (spawn-available-p))
      (skip "Cannot spawn workers")
      (let* ((root (%scratch-directory "fasl-cache-project"))
             (a (merge-pathnames "a.lisp" root))
             (b (merge-pathnames "b.lisp" root))
             (shared-a-fasl (asdf:apply-output-translations
                             (compile-file-pathname (merge-pathnames "a.lisp" (truename root)))))
             (first-worker nil)
             (second-worker nil))
        (unwind-protect
             (progn
               (ignore-errors (delete-file shared-a-fasl))
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
                 (ok (search "RESULT=42" (%eval-in first-worker *run-code*))
                     "42 before the edit"))
               ;; The macro moves to the later file; this worker still has it.
               (%write-text a *macro-moved-a*)
               (%write-text b *macro-moved-b*)
               (testing "the first worker still runs it after the edit"
                 (ok (search "RESULT=42" (%eval-in first-worker *run-code*))
                     "its image holds the macro"))
               (testing "and compiled nothing into the shared cache"
                 (ok (not (probe-file shared-a-fasl)) "no a.fasl there"))
               (let ((first-cache (%worker-cache-of first-worker)))
                 (kill-worker first-worker)
                 (setf first-worker nil)
                 (setf second-worker (spawn-worker))
                 (%set-worker-root second-worker root)
                 (testing "a fresh worker compiles the saved source and fails as a clean build does"
                   (ok (search "RESULT=FAILED" (%eval-in second-worker *run-code*))
                       "TWICE is called as a function"))
                 (let ((second-cache (%worker-cache-of second-worker))
                       (a-fasl (namestring (merge-pathnames "a.fasl" (truename root)))))
                   (testing "into a cache of its own"
                     (ok (search second-cache
                                 (%eval-in second-worker
                                           (format nil "(namestring ~
                                                        (asdf:apply-output-translations ~S))"
                                                   a-fasl)))
                         "its a.fasl is there")
                     (ok (search (format nil "/worker-fasls/~D-" (worker-pid second-worker))
                                 second-cache)
                         "named by its PID"))
                   (testing "a sweep deletes the killed worker's cache and keeps the live one's"
                     (ok (%swept-within 10 first-cache) "the killed worker's is gone")
                     (ok (uiop:directory-exists-p second-cache) "the live worker's stays")))))
          (when first-worker (kill-worker first-worker))
          (when second-worker (kill-worker second-worker))
          (ignore-errors (delete-file shared-a-fasl))
          (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))
