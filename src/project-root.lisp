;;;; src/project-root.lisp
;;;;
;;;; Project root configuration.
;;;; This minimal module defines *project-root* to avoid circular dependencies.

(defpackage #:cl-mcp/src/project-root
  (:use #:cl)
  (:import-from #:bordeaux-threads #:make-lock)
  (:import-from #:cl-mcp/src/state #:*current-session-id*)
  (:export #:*project-root*
           #:*project-root-lock*
           #:session-project-root
           #:set-project-root
           #:forget-session-project-root
           #:call-with-session-project-root
           #:with-session-project-root
           #:register-project-root-source-registry))

(in-package #:cl-mcp/src/project-root)

(defparameter *project-root*
  (let ((env-root (uiop/os:getenv "MCP_PROJECT_ROOT")))
    (when env-root
      ;; PARSE-UNIX-NAMESTRING, not ENSURE-DIRECTORY-PATHNAME: the latter reads
      ;; the string with the CL pathname reader, where [ and ] are wildcard
      ;; syntax, and signals on the wild result -- here, while loading the file,
      ;; so a root named project[old]/ would take the whole image down.
      (uiop:parse-unix-namestring env-root :ensure-directory t)))
  "Absolute pathname of the project root.
Set via MCP_PROJECT_ROOT environment variable or fs-set-project-root tool.")

(defvar *project-root-lock* (bt:make-lock "project-root-lock")
  "Lock protecting multi-step mutations of *project-root* and related globals.")

;;; Per-session roots.
;;;
;;; *PROJECT-ROOT*'s global value is the server default (MCP_PROJECT_ROOT, or a
;;; root set outside any session).  A session that sets a root -- through
;;; fs-set-project-root or its initialize rootPath/rootUri -- gets an entry in
;;; *SESSION-PROJECT-ROOTS* instead, and each of its requests runs with
;;; *PROJECT-ROOT* bound to that entry (CALL-WITH-SESSION-PROJECT-ROOT).  So a
;;; session never moves another session's root, and every reader of
;;; *PROJECT-ROOT* keeps working unchanged.

(defvar *session-project-roots* (make-hash-table :test #'equal)
  "Maps a session id to the project root (a directory pathname) that session set.
A session with no entry works under the global value of *PROJECT-ROOT*.")

(defvar *session-project-roots-lock* (bt:make-lock "session-project-roots-lock")
  "Guards *SESSION-PROJECT-ROOTS*.")

(defvar *bound-session-id* nil
  "The session whose root CALL-WITH-SESSION-PROJECT-ROOT bound *PROJECT-ROOT* to
in this thread, or NIL outside such a binding.  SET-PROJECT-ROOT assigns the
binding only when it belongs to the session being set, so a session's root never
reaches the global value.")

(defun session-project-root (session-id)
  "Return the project root SESSION-ID set, or NIL when it set none."
  (and session-id
       (bt:with-lock-held (*session-project-roots-lock*)
         (values (gethash session-id *session-project-roots*)))))

(defun forget-session-project-root (session-id)
  "Drop the project root SESSION-ID set.  Called when the session ends, so a
later session reusing the id starts from the server default."
  (when session-id
    (bt:with-lock-held (*session-project-roots-lock*)
      (remhash session-id *session-project-roots*))))

(defun set-project-root (root &key (session-id *current-session-id*))
  "Make ROOT, an absolute directory, the project root of SESSION-ID and return it
as a directory pathname.

With a SESSION-ID (the default is the current request's), only that session's
entry changes, plus the *PROJECT-ROOT* and *DEFAULT-PATHNAME-DEFAULTS* bindings
of its request when this thread is running one; the global default and every
other session keep their roots.  With a NIL SESSION-ID -- a call from outside
any transport -- the global default changes, as it always has.

Either way the process changes its working directory to ROOT.  The working
directory is shared by the whole process; no path is resolved against it."
  (let ((dir (uiop:ensure-directory-pathname root)))
    (bt:with-lock-held (*project-root-lock*)
      (cond
        (session-id
         (bt:with-lock-held (*session-project-roots-lock*)
           (setf (gethash session-id *session-project-roots*) dir))
         (when (equal *bound-session-id* session-id)
           (setf *project-root* dir
                 *default-pathname-defaults* dir)))
        (t
         (setf *project-root* dir
               *default-pathname-defaults* dir)))
      (uiop:chdir dir))
    dir))

(defun call-with-session-project-root (thunk &key (session-id *current-session-id*))
  "Call THUNK with *PROJECT-ROOT* and *DEFAULT-PATHNAME-DEFAULTS* bound to the
root SESSION-ID set, or to the global default when it set none.  With a NIL
SESSION-ID nothing is bound, so a root set inside THUNK is the global default."
  (if session-id
      (let* ((root (session-project-root session-id))
             (*bound-session-id* session-id)
             (*project-root* (or root *project-root*))
             (*default-pathname-defaults*
               (if root root *default-pathname-defaults*)))
        (funcall thunk))
      (funcall thunk)))

(defmacro with-session-project-root ((&key (session-id '*current-session-id*)) &body body)
  "Evaluate BODY under the project root of SESSION-ID (the current session by
default).  See CALL-WITH-SESSION-PROJECT-ROOT."
  `(call-with-session-project-root (lambda () ,@body) :session-id ,session-id))

(defvar *registered-project-root* nil
  "The project-root directory this worker last added to ASDF:*CENTRAL-REGISTRY*
via REGISTER-PROJECT-ROOT-SOURCE-REGISTRY. Tracked so a root change removes the
old entry instead of leaving stale project roots in the search list (which would
leak the previous project's systems into the new root).")

(defun %clear-systems-under (root)
  "Unregister every ASDF system whose pathname lives under ROOT (a canonical
directory), so a project's systems do not linger in ASDF's registry after the
worker moves to a different root. Iterates REGISTERED-SYSTEMS (every found
system, not just loaded ones — a bare FIND-SYSTEM also caches the definition)
and tests COMPONENT-PATHNAME, which — unlike SYSTEM-SOURCE-DIRECTORY — is
non-NIL for package-inferred subsystems (e.g. foo/tests, foo/src/bar) and points
at the project root. Libraries and systems outside ROOT are untouched."
  (let ((root (uiop:ensure-directory-pathname root)))
    (dolist (name (asdf:registered-systems))
      (let ((path (ignore-errors
                    (asdf:component-pathname (asdf:find-system name nil)))))
        (when (and path (uiop:subpathp path root))
          (ignore-errors (asdf:clear-system name)))))))

(defun register-project-root-source-registry (root)
  "Make systems whose .asd lives directly under ROOT (a directory) win ASDF
resolution over same-named systems reachable elsewhere (e.g. a copy under
~/.roswell/local-projects). ROOT is a directory pathname or namestring.

This is the fix for: editing a project's source under MCP_PROJECT_ROOT had no
effect when a same-named system was discoverable via the inherited registry —
ASDF kept resolving, and compiling, the inherited original.

Prepends ROOT to ASDF:*CENTRAL-REGISTRY*, which:
  - takes precedence over the inherited source registry (the project wins);
  - is ADDITIVE — the rest of the registry (CL_SOURCE_REGISTRY, user/system
    config, Roswell's local-projects that resolve rove and cl-mcp itself) is
    untouched, so nothing is dropped;
  - is re-checked on every FIND-SYSTEM rather than cached at registration
    time, so a project .asd created AFTER the root is set (e.g. via
    project-scaffold) is still found — unlike a cached (:tree ...)
    source-registry entry.

Maintains exactly ONE managed project-root entry: each call drops the
previously-registered root (tracked in *REGISTERED-PROJECT-ROOT*) and puts the
current root at the FRONT. When a reused worker changes roots A -> B, A is
removed from the search path AND the systems already loaded from under A are
unregistered (%CLEAR-SYSTEMS-UNDER), so neither the registry nor ASDF's
loaded-system cache leaks A's systems into root B (otherwise FIND-SYSTEM /
LOAD-SYSTEM would keep returning, and reloading, the cached A copy). A -> B -> A
resolves from the current root. Other CENTRAL-REGISTRY entries are left intact.
Call this whenever the project root is set or changed."
  ;; Canonicalize via TRUENAME: ASDF records component pathnames with symlinks
  ;; resolved, so the managed root must be canonical too or %CLEAR-SYSTEMS-UNDER
  ;; (and equality with a future root) would miss a symlinked project root.
  (let ((dir (and root
                  (ignore-errors
                    (uiop:ensure-directory-pathname
                     (truename (uiop:ensure-directory-pathname root)))))))
    (when dir
      (when (and *registered-project-root*
                 (not (equal *registered-project-root* dir)))
        (setf asdf:*central-registry*
              (remove *registered-project-root* asdf:*central-registry*
                      :test #'equal))
        (%clear-systems-under *registered-project-root*))
      (setf asdf:*central-registry*
            (cons dir (remove dir asdf:*central-registry* :test #'equal))
            *registered-project-root* dir))))
