;;;; src/worker/fasl-cache.lisp
;;;;
;;;; A worker compiles the files of its project root into a FASL cache of its
;;;; own (issue #317).

(defpackage #:cl-mcp/src/worker/fasl-cache
  (:use #:cl)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:documentation "Each worker's own FASL cache for the files of its project root.

Every worker, and every shell build, used to share one ASDF output cache.  A
FASL compiled in a worker whose image had drifted from the files -- a macro
moved to a later file, a symbol exported only through repl-eval -- is newer
than its source, so the next worker loaded it as current and never compiled
that file again: a fresh worker passed tests a clean build fails (issue #317).
Compiling the project root's files into a directory of the worker's own makes
a fresh worker build the project from source, and keeps any worker's
compilation away from the others and from the shared cache.  Files outside the
root -- the libraries it depends on -- keep the shared cache.")
  (:export #:*worker-fasl-cache*
           #:worker-fasl-cache-base
           #:sweep-dead-worker-caches
           #:prepare-worker-fasl-cache
           #:isolate-project-fasls))

(in-package #:cl-mcp/src/worker/fasl-cache)

(defvar *worker-fasl-cache* nil
  "This worker's own FASL cache directory, BASE/<pid>/ under
WORKER-FASL-CACHE-BASE, or NIL.  Set by PREPARE-WORKER-FASL-CACHE, which only a
worker process's entry point calls; while it is NIL, ISOLATE-PROJECT-FASLS does
nothing, so code running the worker's handlers in another process (the tests,
the inline server) keeps the shared cache.")

(defun worker-fasl-cache-base ()
  "Return the directory holding every worker's own FASL cache, one subdirectory
per worker process, named by its PID."
  (uiop:xdg-cache-home "cl-mcp" "worker-fasls/"))

(defun %process-alive-p (pid)
  "True unless no process PID exists.  A process of another user, which kill
refuses to signal, is alive."
  (handler-case (progn (sb-posix:kill pid 0) t)
    (sb-posix:syscall-error (e)
      (/= (sb-posix:syscall-errno e) sb-posix:esrch))))

(defun %pid-directory-p (directory)
  "Return the PID DIRECTORY, a subdirectory of the cache base, is named after,
or NIL when its name is not a PID."
  (let ((name (car (last (pathname-directory directory)))))
    (and (stringp name)
         (plusp (length name))
         (every #'digit-char-p name)
         (parse-integer name))))

(defun %delete-cache-directory (directory base)
  "Delete DIRECTORY, a worker's cache directly under BASE, with everything in
it.  Only a real directory is deleted -- its true name must be BASE's true name
followed by its own name -- so a symbolic link named like a PID is left alone,
and nothing outside the cache base is ever removed.  Returns true when it was
deleted."
  (let* ((base-truename (ignore-errors (truename base)))
         (name (car (last (pathname-directory directory))))
         (expected (and base-truename (stringp name)
                        (merge-pathnames (make-pathname :directory (list :relative name))
                                         base-truename)))
         (truename (ignore-errors (truename directory))))
    (and expected truename
         (equal (namestring truename) (namestring expected))
         (ignore-errors
          (uiop:delete-directory-tree
           truename :validate (lambda (path) (uiop:subpathp path base-truename)))
          t))))

(defun sweep-dead-worker-caches (&optional (base (worker-fasl-cache-base)))
  "Delete the cache directory of every worker under BASE whose process has
ended, and return how many were deleted.  A worker is usually ended by a signal
that leaves it no time to clean up, so each new worker sweeps up after the ones
before it.  A directory not named by a PID, this process's own, and that of a
process still running are kept."
  (let ((deleted 0))
    (dolist (directory (ignore-errors (uiop:subdirectories base)))
      (let ((pid (%pid-directory-p directory)))
        (when (and pid
                   (/= pid (sb-posix:getpid))
                   (not (%process-alive-p pid))
                   (%delete-cache-directory directory base))
          (incf deleted))))
    deleted))

(defun prepare-worker-fasl-cache (&optional (base (worker-fasl-cache-base)))
  "Make this worker's own, empty FASL cache directory, BASE/<pid>/, after
sweeping up those of ended workers (SWEEP-DEAD-WORKER-CACHES), and record it in
*WORKER-FASL-CACHE*.  A directory left by an earlier process with the same PID
holds FASLs this worker did not compile, so it is emptied first.  Returns the
directory, or NIL when it cannot be made: the worker then compiles into the
shared cache, as every worker did before, and the log says so."
  (handler-case
      (let ((directory (merge-pathnames (format nil "~D/" (sb-posix:getpid)) base)))
        (ensure-directories-exist base)
        (sweep-dead-worker-caches base)
        (when (uiop:directory-exists-p directory)
          (%delete-cache-directory directory base))
        (ensure-directories-exist directory)
        (log-event :info "worker.fasl-cache.ready" "path" (uiop:native-namestring directory))
        (setf *worker-fasl-cache* directory))
    (error (e)
      (log-event :warn "worker.fasl-cache.unavailable"
                 "path" (ignore-errors (uiop:native-namestring base))
                 "error" (princ-to-string e))
      nil)))

(defun %root-cache-directory (root cache)
  "Return where in CACHE the FASLs of the files under ROOT, an absolute directory,
go: ROOT's own path below CACHE, so two roots never share a FASL."
  (merge-pathnames (make-pathname :directory (cons :relative (rest (pathname-directory root))))
                   cache))

(defun isolate-project-fasls (root)
  "Compile the files under ROOT into this worker's own cache from now on: put an
output translation from ROOT's tree to its place in *WORKER-FASL-CACHE* in
front of the shared cache's.  ASDF orders translations by how deep their source
directory is, so the root's entry wins over the catch-all ones and its files
alone move; a dependency outside ROOT keeps its shared FASLs.  Called whenever
the worker's project root is set.  Every root set stays translated, so a system
loaded under an earlier root is never compiled into the shared cache either.

Does nothing and returns NIL outside a worker (*WORKER-FASL-CACHE* NIL), when
ROOT is NIL, or when it is not an existing directory; otherwise returns ROOT's
cache directory."
  (let ((root (and *worker-fasl-cache* root
                   (ignore-errors (truename (uiop:ensure-directory-pathname root))))))
    (when root
      (let ((destination (%root-cache-directory root *worker-fasl-cache*))
            (source (uiop:wilden root))
            (current (asdf/output-translations:output-translations)))
        (unless (find source current :key #'first :test #'equal)
          (setf (asdf/output-translations:output-translations)
                (cons (list source (uiop:wilden destination)) current))
          (log-event :info "worker.fasl-cache.isolated"
                     "root" (uiop:native-namestring root)
                     "cache" (uiop:native-namestring destination)))
        destination))))
