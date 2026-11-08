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
           #:isolate-project-fasls
           #:ensure-project-fasls-isolated))

(in-package #:cl-mcp/src/worker/fasl-cache)

(defvar *worker-fasl-cache* nil
  "This worker's own FASL cache directory, BASE/<pid>-<token>/ under
WORKER-FASL-CACHE-BASE, or NIL.  Set by PREPARE-WORKER-FASL-CACHE, which only a
worker process's entry point calls; while it is NIL, ISOLATE-PROJECT-FASLS does
nothing, so code running the worker's handlers in another process (the tests,
the inline server) keeps the shared cache.")

(defvar *worker-cache-lock-fd* nil
  "The file descriptor of this worker's cache lock (%LOCK-PATH), held open and
locked for as long as the process runs.  The lock is the cache's claim: the
kernel drops it when the process ends however it ends, which is how a sweep
tells a cache in use from one left behind -- across PID namespaces too, where
two live workers can share a PID.")

(defvar *isolated-roots* '()
  "The (SOURCE DESTINATION) output translations ISOLATE-PROJECT-FASLS added,
newest first, for ENSURE-PROJECT-FASLS-ISOLATED to put back when ASDF's
configuration is cleared or initialized again.")

(defun worker-fasl-cache-base ()
  "Return the directory holding every worker's own FASL cache, one subdirectory
per worker process, named <pid>-<token>."
  (uiop:xdg-cache-home "cl-mcp" "worker-fasls/"))

(defun %cache-name-p (name)
  "True when NAME is a worker cache's: the worker's PID, a hyphen, then a token of
letters and digits."
  (let ((hyphen (and (stringp name) (position #\- name))))
    (and hyphen
         (plusp hyphen)
         (< (1+ hyphen) (length name))
         (every #'digit-char-p (subseq name 0 hyphen))
         (every #'alphanumericp (subseq name (1+ hyphen))))))

(defun %lock-path (directory)
  "Return the lock file of the cache DIRECTORY: <name>.lock beside it, outside it,
so the lock exists before the directory does and outlives a half-deleted one."
  (make-pathname :name (car (last (pathname-directory directory)))
                 :type "lock"
                 :version nil
                 :defaults (uiop:pathname-parent-directory-pathname directory)))

(defun %try-lock (path &optional (attempts 3))
  "Open PATH, creating it, and take an exclusive lock on it without waiting.
Return the open file descriptor when the lock was taken -- no live process holds
it -- or NIL when another process does, or PATH cannot be opened (a sweep then
leaves it alone and goes on).  Any other failure of the lock itself, such as a
filesystem without record locks, is signalled rather than taken for a holder.

A sweep may unlink PATH between this open and the lock, leaving the lock on a
file no one else can find; the lock is kept only when PATH still names the
locked file, and taken again on a fresh file otherwise, up to ATTEMPTS times.
POSIX record locks belong to the process: one that already holds PATH's lock
would take it again, and closing any descriptor of PATH drops its lock, so a
worker never calls this on its own lock file twice."
  (let ((name (uiop:native-namestring path)))
    (let ((fd (handler-case (sb-posix:open name (logior sb-posix:o-rdwr sb-posix:o-creat) #o600)
                (sb-posix:syscall-error () (return-from %try-lock nil)))))
      (handler-case (sb-posix:lockf fd sb-posix:f-tlock 0)
        (sb-posix:syscall-error (e)
          (sb-posix:close fd)
          (if (member (sb-posix:syscall-errno e) (list sb-posix:eagain sb-posix:eacces))
              (return-from %try-lock nil)
              (error e))))
      (if (eql (ignore-errors (sb-posix:stat-ino (sb-posix:stat name)))
               (sb-posix:stat-ino (sb-posix:fstat fd)))
          fd
          (progn
            (sb-posix:close fd)
            (and (> attempts 1) (%try-lock path (1- attempts))))))))

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
  "Delete the cache of every worker under BASE that has ended, and return how
many were deleted.  A worker is usually ended by a signal that leaves it no time
to clean up, so each new worker sweeps up after the ones before it.  A cache is
left behind when no process holds its lock (%TRY-LOCK), or when its lock file is
gone; the lock, not the PID in its name, decides, since PIDs are reused and two
PID namespaces sharing BASE share PIDs.  This worker's own cache, and every
file or directory not named like a cache, are kept."
  (let ((own (and *worker-fasl-cache* (car (last (pathname-directory *worker-fasl-cache*)))))
        (deleted 0))
    (dolist (directory (ignore-errors (uiop:subdirectories base)))
      (let ((name (car (last (pathname-directory directory)))))
        (when (and (%cache-name-p name) (not (equal name own)))
          (let* ((lock (%lock-path directory))
                 ;; A lock that cannot be judged counts as held: one odd entry
                 ;; must not stop the sweep of the rest.
                 (fd (if (probe-file lock) (ignore-errors (%try-lock lock)) :no-lock)))
            (when fd
              (unwind-protect
                   (when (%delete-cache-directory directory base)
                     (ignore-errors (delete-file lock))
                     (incf deleted))
                (when (integerp fd)
                  (sb-posix:close fd))))))))
    ;; A lock whose directory is already gone: its worker ended, or a sweep
    ;; was cut short between the two deletions.
    (dolist (lock (ignore-errors (uiop:directory-files base "*.lock")))
      (let ((name (pathname-name lock)))
        (when (and (%cache-name-p name) (not (equal name own))
                   (not (uiop:directory-exists-p
                         (merge-pathnames (format nil "~A/" name) base))))
          (let ((fd (ignore-errors (%try-lock lock))))
            (when fd
              (unwind-protect (ignore-errors (delete-file lock))
                (sb-posix:close fd)))))))
    deleted))

(defun prepare-worker-fasl-cache (&optional (base (worker-fasl-cache-base)))
  "Make this worker's own, empty FASL cache directory, BASE/<pid>-<token>/, claim
it with its lock (*WORKER-CACHE-LOCK-FD*), record it in *WORKER-FASL-CACHE*, and
then sweep up the caches of ended workers (SWEEP-DEAD-WORKER-CACHES).  The token
is random, so no two workers share a directory, even with the same PID in two
PID namespaces.  Returns the directory, or NIL when it cannot be made: the
worker then compiles into the shared cache, as every worker did before, and the
log says so.  A sweep that fails is logged and changes nothing about this
worker's own cache."
  (let ((directory
          (handler-case
              (let* ((name (format nil "~D-~36R" (sb-posix:getpid)
                                   (random (expt 36 10) (make-random-state t))))
                     (directory (merge-pathnames (format nil "~A/" name) base)))
                (ensure-directories-exist base)
                (let ((fd (%try-lock (%lock-path directory))))
                  (unless fd
                    (error "another process holds the lock of ~A, or it cannot be opened"
                           name))
                  (handler-case (ensure-directories-exist directory)
                    (error (e)
                      (sb-posix:close fd)
                      (error e)))
                  (setf *worker-cache-lock-fd* fd
                        *worker-fasl-cache* directory)))
            (error (e)
              (log-event :warn "worker.fasl-cache.unavailable"
                         "path" (ignore-errors (uiop:native-namestring base))
                         "error" (princ-to-string e))
              nil))))
    (when directory
      (handler-case
          (log-event :info "worker.fasl-cache.ready"
                     "path" (uiop:native-namestring directory)
                     "swept" (sweep-dead-worker-caches base))
        (error (e)
          (log-event :warn "worker.fasl-cache.sweep-failed" "error" (princ-to-string e)))))
    directory))

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
loaded under an earlier root is never compiled into the shared cache either,
and ENSURE-PROJECT-FASLS-ISOLATED puts the entries back if ASDF's configuration
is cleared.  A root that encloses the libraries themselves ($HOME, say) moves
them too: they are then compiled again by each fresh worker.

Does nothing and returns NIL outside a worker (*WORKER-FASL-CACHE* NIL), when
ROOT is NIL, or when it is not an existing directory; otherwise returns ROOT's
cache directory."
  (let ((root (and *worker-fasl-cache* root
                   (ignore-errors (truename (uiop:ensure-directory-pathname root))))))
    (when root
      (let* ((destination (%root-cache-directory root *worker-fasl-cache*))
             (entry (list (uiop:wilden root) (uiop:wilden destination))))
        (unless (member entry *isolated-roots* :test #'equal)
          (push entry *isolated-roots*)
          (log-event :info "worker.fasl-cache.isolated"
                     "root" (uiop:native-namestring root)
                     "cache" (uiop:native-namestring destination)))
        (ensure-project-fasls-isolated)
        destination))))

(defun ensure-project-fasls-isolated ()
  "Put back, in front of ASDF's output translations, every entry
ISOLATE-PROJECT-FASLS added that they no longer hold, and return how many it
put back.  ASDF:CLEAR-CONFIGURATION, INITIALIZE-OUTPUT-TRANSLATIONS and an ASDF
upgrade rebuild the translations from configuration, which knows nothing of
these entries; without them the worker would compile its project into the
shared cache again, unannounced.  Called before every request the worker serves
(the handlers' wrapper), so a clear made by one request is undone before the
next; a load in the same request that cleared it still uses the shared cache."
  (if (null *isolated-roots*)
      0
      (let* ((current (progn (asdf:ensure-output-translations)
                             (asdf/output-translations:output-translations)))
             (missing (remove-if (lambda (entry) (member entry current :test #'equal))
                                 *isolated-roots*)))
        (when missing
          (setf (asdf/output-translations:output-translations) (append missing current))
          ;; Said, since it overrides whatever cleared them -- a project's own
          ;; INITIALIZE-OUTPUT-TRANSLATIONS included -- for the root's files.
          (log-event :info "worker.fasl-cache.restored" "entries" (length missing)))
        (length missing))))
