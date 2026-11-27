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
                  (inside (merge-pathnames "src/a.fasl" (truename root)))
                  (vendored (merge-pathnames "subproject/.qlot/dists/dep.fasl" (truename root)))
                  (vendored-before (asdf:apply-output-translations vendored)))
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
                      (testing "a .qlot directory within the root keeps the shared cache"
                        (ok (equal vendored-before (asdf:apply-output-translations vendored))))
                      (testing "setting the same root again adds nothing"
                        (isolate-project-fasls root)
                        (ok (= count (length (asdf/output-translations:output-translations)))
                            "one entry per root"))
                      (testing "a cleared configuration gets the entry back"
                        (asdf:clear-output-translations)
                        (let ((vendored-after-clear (asdf:apply-output-translations vendored)))
                          (ok (= 1 (ensure-project-fasls-isolated)) "one entry put back")
                          (ok (uiop:subpathp (asdf:apply-output-translations inside) own)
                              "the root's files go to the worker's cache again")
                          (ok (equal vendored-after-clear (asdf:apply-output-translations vendored))
                              ".qlot still uses the shared translation")
                          (ok (zerop (ensure-project-fasls-isolated))
                              "and nothing more while it holds")))))
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

(defun %copy-worker-install (dir)
  (let ((source (asdf:system-source-directory :cl-mcp)))
    (dolist (file '("cl-mcp.asd" "main.lisp" "scripts/worker-bootstrap.lisp"))
      (uiop:copy-file (merge-pathnames file source)
                      (ensure-directories-exist (merge-pathnames file dir))))
    (uiop:run-program (list "cp" "-R" (uiop:native-namestring (merge-pathnames "src" source))
                            (uiop:native-namestring (merge-pathnames "src" dir))))))

(defun %check-installed-worker (dir command &key clean-sbcl)
  ;; Supply the installed dependencies through ASDF even in a clean SBCL
  ;; without Quicklisp's system-definition search function.
  (let* ((directories
           (remove-duplicates
            (loop for name in (asdf:registered-systems)
                  for system = (asdf:registered-system name)
                  for file = (and system (asdf:system-source-file system))
                  when file collect (namestring (uiop:pathname-directory-pathname file)))
            :test #'equal))
         (registry (format nil "CL_SOURCE_REGISTRY=(:source-registry (:directory ~S) ~
                               ~{(:directory ~S) ~}:inherit-configuration)"
                           (namestring dir) directories))
         (cache (merge-pathnames "fasls/" (uiop:pathname-parent-directory-pathname dir)))
         ;; Add just the copied installation to the child's existing configuration,
         ;; including an ASDF_OUTPUT_TRANSLATIONS supplied by the caller.
         (inherited (asdf/output-translations:parse-output-translations-string
                     (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")))
         (translations (list* :output-translations (list (namestring dir) (namestring cache))
                              (cdr inherited)))
         (env (append (list "env" registry (format nil "ASDF_OUTPUT_TRANSLATIONS=~S" translations)
                            (format nil "CL_MCP_TEST_INSTALL=~A" (uiop:native-namestring dir)))
                      (when clean-sbcl
                        (list (format nil "CL_MCP_TEST_CLEAN_SBCL=~A" clean-sbcl))))))
    (multiple-value-bind (output stderr code)
        (uiop:run-program (append env command) :output :string :error-output :output
                                               :ignore-error-status t)
      (declare (ignore stderr))
      ;; The driver exits with the numeric result returned by the authenticated
      ;; worker. A launcher that exits zero before running it cannot pass.
      (ok (eql code 42)
          (if (eql code 42)
              "the installed worker authenticated and answered 42"
              (let ((start (or (search "Unhandled " output) (max 0 (- (length output) 2400)))))
                (subseq output start (min (length output) (+ start 2400)))))))))

(deftest workers-start-from-a-bracketed-install-with-plain-asdf-or-roswell
  (let* ((root (%scratch-directory "worker-install"))
         (dir (merge-pathnames (make-pathname :directory '(:relative "[installed] cl-mcp")) root))
         (driver (uiop:native-namestring
                  (asdf:system-relative-pathname
                   :cl-mcp "tests/fixtures/launch-installed-worker.lisp")))
         (clean (merge-pathnames "clean-sbcl" root)))
    (unwind-protect
         (progn
           (%copy-worker-install dir)
           (%write-text clean
                        (format nil "#!/bin/sh~%[ \"$1\" != --noinform ] || shift~%~
                                     exec ~A --noinform --no-sysinit --no-userinit \"$@\"~%"
                                (uiop:escape-sh-token (cl-mcp/src/worker-client::%find-sbcl-path))))
           (sb-posix:chmod clean #o700)
           (testing "stock SBCL loads the worker using ASDF alone"
             (%check-installed-worker dir (list (uiop:native-namestring clean)
                                                "--non-interactive" "--load" driver)
                                      :clean-sbcl (uiop:native-namestring clean)))
           (testing "Roswell reads the literal installation path"
             (if (cl-mcp/src/worker-client::%roswell-p)
                 (%check-installed-worker
                  dir (list (cl-mcp/src/worker-client::%find-ros-path)
                            "run" "-e" "(sb-ext:disable-debugger)"
                            "-e" (format nil "(load (uiop:parse-native-namestring ~S))" driver)
                            "-q"))
                 (skip "Roswell is not the current launcher")))
           (ok (probe-file (merge-pathnames "fasls/src/worker/main.fasl" root))
               "the installed worker compiled into this test's scratch directory"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
    (ok (not (probe-file root)) "the temporary installation and its FASLs were removed")))

(defun %bootstrap-test-launcher (dir)
  "Return a launcher for a copy of this worker system with a cold ASDF output directory."
  (let ((source (asdf:system-source-directory :cl-mcp))
        (cache (merge-pathnames "cache/" dir))
        (launcher (merge-pathnames "launch" dir))
        (runner (if (cl-mcp/src/worker-client::%roswell-p)
                    (cl-mcp/src/worker-client::%find-ros-path)
                    (cl-mcp/src/worker-client::%find-sbcl-path))))
    (%copy-worker-install dir)
    (with-open-file (out (merge-pathnames "src/worker/server.lisp" dir)
                         :direction :output :if-exists :append)
      (terpri out)
      (write-string (uiop:read-file-string
                     (merge-pathnames "tests/fixtures/worker-bootstrap-probe.lisp" source)) out))
    ;; Independent user cache directories still share the same ASDF outputs.
    ;; Reuse already compiled dependencies outside this cold checkout.
    (%write-text launcher
                 (format nil "#!/bin/sh~%exec env XDG_CACHE_HOME=~Auser-$$ ~{~A ~}\"$@\"~%"
                         (uiop:escape-sh-token (uiop:native-namestring dir))
                         (mapcar #'uiop:escape-sh-token
                                 (list (format nil "CL_SOURCE_REGISTRY=(:source-registry ~
                                                   (:directory ~S) :inherit-configuration)"
                                               (uiop:native-namestring dir))
                                       (format nil "ASDF_OUTPUT_TRANSLATIONS=(:output-translations ~
                                                   (~S ~S) (t ~S) :inherit-configuration)"
                                               (uiop:native-namestring dir)
                                               (uiop:native-namestring cache)
                                               (uiop:native-namestring
                                                (uiop:pathname-directory-pathname
                                                 (asdf:apply-output-translations #P"/probe.fasl"))))
                                       (format nil "CL_MCP_BOOT_PROBE_DIR=~A" dir)
                                       runner))))
    (sb-posix:chmod launcher #o700)
    (uiop:native-namestring launcher)))

(defun %check-concurrent-boot (launcher)
  "Release two real worker launches together and check that both authenticated."
  (let* ((gate (sb-thread:make-semaphore))
         (threads
           (loop repeat 2 collect
             (bt:make-thread
              (lambda ()
                (let ((cl-mcp/src/worker-client::*cached-ros-path* launcher)
                      (cl-mcp/src/worker-client::*cached-sbcl-path* launcher)
                      (cl-mcp/src/worker-client::*worker-startup-timeout* 90))
                  (sb-thread:wait-on-semaphore gate)
                  (handler-case (spawn-worker) (error (c) c))))))))
    (sb-thread:signal-semaphore gate 2)
    (let ((outcomes (mapcar #'bt:join-thread threads)))
      (unwind-protect
           (dolist (outcome outcomes)
             (if (typep outcome 'error)
                 (ok nil (princ-to-string outcome))
                 (ok (search "42" (%eval-in outcome "(+ 40 2)")))))
        (dolist (outcome outcomes)
          (unless (typep outcome 'error) (kill-worker outcome)))))))

(deftest workers-start-together-on-a-cold-bootstrap-cache
  (if (not (spawn-available-p))
      (skip "Cannot spawn workers")
      (let ((dir (%scratch-directory "bootstrap")))
        (unwind-protect
             (let ((launcher (%bootstrap-test-launcher dir)))
               (testing "two workers share the initially empty worker FASL cache"
                 (%check-concurrent-boot launcher)
                 (ok (probe-file (merge-pathnames "cache/src/worker/server.fasl" dir))))
               (testing "two warm workers load concurrently"
                 (%write-text (merge-pathnames "warm" dir) "")
                 (%check-concurrent-boot launcher)
                 (ok (= 2 (length (directory (merge-pathnames "reader-*" dir))))))
               (testing "a compiler that exits before its handshake releases its lock"
                 (delete-file (merge-pathnames "warm" dir))
                 (delete-file (merge-pathnames "cache/src/worker/server.fasl" dir))
                 (%write-text (merge-pathnames "abort" dir) "")
                 (let ((cl-mcp/src/worker-client::*cached-ros-path* launcher)
                       (cl-mcp/src/worker-client::*cached-sbcl-path* launcher))
                   (let ((failed (handler-case (spawn-worker) (error (c) c))))
                     (unwind-protect
                          (ok (and (typep failed 'error)
                                   (search "Aborting the bootstrap compiler"
                                           (princ-to-string failed))))
                       (unless (typep failed 'error) (kill-worker failed)))))
                 (delete-file (merge-pathnames "abort" dir))
                 (%check-concurrent-boot launcher)))
          (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

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

(deftest a-fresh-worker-reuses-qlot-fasls
  (if (not (spawn-available-p))
      (skip "Cannot spawn workers")
      (let* ((root (%scratch-directory "qlot-fasl-project"))
             (shared (merge-pathnames "shared-fasls/" root))
             (dep-dir ".qlot/dists/fixture/")
             (dep-fasl (merge-pathnames (concatenate 'string dep-dir "dep.fasl") shared))
             (marker (merge-pathnames "dependency-warmed" root))
             (mapping (list (namestring root) (namestring shared)))
             (worker nil))
        (unwind-protect
             (progn
               (%write-text
                (merge-pathnames "clmcp-qlot-app.asd" root)
                "(defsystem \"clmcp-qlot-app\" :depends-on (\"clmcp-qlot-lib\")
                   :components ((:file \"app\")))")
               (%write-text (merge-pathnames "app.lisp" root) "(in-package :cl-user)")
               (%write-text
                (merge-pathnames "clmcp-qlot-lib.asd" root)
                (format nil "(defsystem \"clmcp-qlot-lib\" :pathname ~S
                               :components ((:file \"dep\")))" dep-dir))
               (%write-text
                (merge-pathnames (concatenate 'string dep-dir "dep.lisp") root)
                (format nil "(eval-when (:compile-toplevel)
                               (when (probe-file ~S) (warn \"dependency compiled again\")))"
                        marker))
               (with-saved-translations
                 (let ((asdf:*central-registry* (cons root asdf:*central-registry*)))
                   (setf (asdf/output-translations:output-translations)
                         (list* (list (uiop:wilden shared) t)
                                (list (uiop:wilden root) (uiop:wilden shared))
                                (asdf:ensure-output-translations)))
                   (asdf:load-system "clmcp-qlot-lib")))
               (ok (probe-file dep-fasl) "The dependency was compiled into the shared cache.")
               (%write-text marker "")
               (setf worker (spawn-worker))
               (%set-worker-root worker root)
               (%eval-in worker
                         (format nil "(progn (asdf:initialize-output-translations
                                               '(:output-translations ~S :inherit-configuration))
                                            :configured)" mapping))
               (let* ((params (cl-mcp/src/tools/helpers:make-ht
                               "system" "clmcp-qlot-app" "force" yason:false))
                      (result (worker-rpc worker "worker/load-system" params)))
                 (ok (equal "loaded" (gethash "status" result))
                     (gethash "message" result))
                 (when (equal "loaded" (gethash "status" result))
                   (ok (string-equal
                        ":caches-match"
                        (%eval-in worker
                                  (format nil "(flet ((fasl (system file)
                                                (first (asdf:output-files 'asdf:compile-op
                                                         (asdf:find-component system file)))))
                                    (let ((app (fasl \"clmcp-qlot-app\" \"app\"))
                                          (dep (fasl \"clmcp-qlot-lib\" \"dep\")))
                                      (when (and (probe-file app)
                                                 (uiop:subpathp app
                                                  cl-mcp/src/worker/fasl-cache:*worker-fasl-cache*)
                                                 (equal dep ~S) (probe-file dep))
                                        :caches-match)))" dep-fasl))))
                   (setf (gethash "clear_fasls" params) t)
                   (let ((reload (worker-rpc worker "worker/load-system" params)))
                     (ok (equal "loaded" (gethash "status" reload)) (gethash "message" reload))
                     (ok (eql 1 (gethash "fasls_deleted" reload)))
                     (ok (probe-file dep-fasl)))
                   (delete-file marker)
                   (setf (gethash "system" params) "clmcp-qlot-lib")
                   (let ((reload (worker-rpc worker "worker/load-system" params)))
                     (ok (equal "loaded" (gethash "status" reload)) (gethash "message" reload))
                     (ok (eql 1 (gethash "fasls_deleted" reload)))
                     (ok (probe-file dep-fasl))))))
          (when worker (kill-worker worker))
          (asdf:clear-system "clmcp-qlot-lib")
          (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))
