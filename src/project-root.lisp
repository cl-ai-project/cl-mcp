;;;; src/project-root.lisp
;;;;
;;;; Project root configuration.
;;;; This minimal module defines *project-root* to avoid circular dependencies.

(defpackage #:cl-mcp/src/project-root
  (:use #:cl)
  (:import-from #:bordeaux-threads #:make-lock)
  (:export #:*project-root*
           #:*project-root-lock*
           #:register-project-root-source-registry))

(in-package #:cl-mcp/src/project-root)

(defparameter *project-root*
  (let ((env-root (uiop/os:getenv "MCP_PROJECT_ROOT")))
    (when env-root
      (uiop/pathname:ensure-directory-pathname env-root)))
  "Absolute pathname of the project root.
Set via MCP_PROJECT_ROOT environment variable or fs-set-project-root tool.")

(defvar *project-root-lock* (bt:make-lock "project-root-lock")
  "Lock protecting multi-step mutations of *project-root* and related globals.")

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
