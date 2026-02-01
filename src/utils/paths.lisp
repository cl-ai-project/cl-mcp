;;;; src/utils/paths.lisp
;;;;
;;;; Unified path utilities for cl-mcp.
;;;; Provides project root management, path normalization, and access control.

(defpackage #:cl-mcp/src/utils/paths
  (:use #:cl)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:export #:ensure-project-root
           #:path-inside-p
           #:canonical-path
           #:allowed-read-path
           #:ensure-write-path
           #:resolve-path-in-project
           #:normalize-path-for-display))

(in-package #:cl-mcp/src/utils/paths)

(declaim (ftype (function () null) ensure-project-root))
(defun ensure-project-root ()
  "Ensure *project-root* is set. Signal an error with instructions if not.
This guard function should be called at the beginning of all file operations."
  (unless *project-root*
    (error "Project root is not set.

SOLUTION:
call fs-set-project-root tool with your current working directory:
   Method: tools/call
   Tool: fs-set-project-root
   Arguments: {\"path\": \"/absolute/path/to/your/project\"}

CURRENT SERVER STATE:
- Current working directory: ~A
- Registered ASDF systems: ~D

For AI agents: Call fs-set-project-root at the start of your session with your
current working directory to synchronize the server's project root."
           (or (ignore-errors (namestring (uiop/os:getcwd))) "(unknown)")
           (length (asdf/system-registry:registered-systems)))))

(declaim (ftype (function ((or null string pathname) (or null string pathname))
                           boolean)
                path-inside-p))
(defun path-inside-p (child parent)
  "Return T when CHILD pathname is a subpath of directory PARENT.
Handles NIL and relative paths gracefully."
  (and child parent
       (uiop/pathname:subpathp child parent)
       t))

(declaim (ftype (function ((or string pathname)
                           &key (:relative-to (or null string pathname)))
                           pathname)
                canonical-path))
(defun canonical-path (path &key relative-to)
  "Turn PATH designator into a physical absolute pathname.
If RELATIVE-TO is provided and PATH is relative, merge it with RELATIVE-TO.
If RELATIVE-TO is NIL, uses *project-root* as the base."
  (ensure-project-root)
  (let* ((pn (uiop/pathname:ensure-pathname path
                                            :want-relative nil
                                            :ensure-directory nil
                                            :ensure-absolute nil))
         (abs (if (uiop/pathname:absolute-pathname-p pn)
                  pn
                  (uiop/pathname:merge-pathnames* pn
                                                  (or relative-to *project-root*)))))
    (uiop/pathname:ensure-pathname abs :want-relative nil)))

(declaim (ftype (function ((or string pathname)) (or null pathname))
                allowed-read-path))
(defun allowed-read-path (pn)
  "Return PN (as absolute pathname) if readable per policy, else NIL.
Allows project-root subpaths and source dirs of registered ASDF systems."
  (ensure-project-root)
  (let* ((abs (canonical-path pn))
         (normalized-abs (if (uiop/filesystem:directory-exists-p abs)
                             (uiop/pathname:ensure-directory-pathname abs)
                             abs))
         (project-ok (path-inside-p normalized-abs
                                    (uiop/pathname:ensure-directory-pathname *project-root*))))
    (when project-ok
      (return-from allowed-read-path normalized-abs))
    ;; Check ASDF system directories
    (let ((systems (asdf/system-registry:registered-systems)))
      (dolist (name systems)
        (let ((dir (ignore-errors (asdf/system:system-source-directory name))))
          (when (and dir (path-inside-p normalized-abs dir))
            (return-from allowed-read-path normalized-abs)))))
    nil))

(declaim (ftype (function ((or string pathname)) pathname) ensure-write-path))
(defun ensure-write-path (path)
  "Ensure PATH is relative to project root and return absolute pathname.
Signals an error if outside project root or absolute."
  (ensure-project-root)
  (let* ((pn (uiop/pathname:ensure-pathname path :want-relative t))
         (abs (canonical-path pn :relative-to *project-root*))
         (real (or (ignore-errors (truename abs)) abs)))
    (unless (path-inside-p real (uiop/pathname:ensure-directory-pathname *project-root*))
      (error "Write path ~A is outside project root" path))
    real))

(declaim (ftype (function ((or null string pathname) &key (:must-exist boolean))
                           pathname)
                resolve-path-in-project))
(defun resolve-path-in-project (path &key (must-exist nil))
  "Resolve PATH to an absolute pathname within project root.
If PATH is NIL or empty, returns *project-root*.
If MUST-EXIST is T, signals an error if the path does not exist.
Signals an error if PATH is outside project root."
  (ensure-project-root)
  (let* ((base *project-root*)
         (target (if (or (null path) (string= path ""))
                     base
                     (let ((pn (uiop/pathname:ensure-pathname path
                                                             :want-pathname t
                                                             :defaults base)))
                       (if (uiop/pathname:absolute-pathname-p pn)
                           pn
                           (uiop/pathname:merge-pathnames* pn base))))))
    (let ((canonical (if must-exist
                         (or (ignore-errors (truename target))
                             (error "Path does not exist: ~A" target))
                         (or (ignore-errors (truename target))
                             target))))
      (unless (path-inside-p canonical base)
        (error "Path ~A is outside project root ~A" target base))
      canonical)))

(declaim (ftype (function ((or null string pathname)) (or null string))
                normalize-path-for-display))
(defun normalize-path-for-display (pathname)
  "Return a namestring for PATHNAME, relative to *project-root* when possible.
Falls back to CWD, then cl-mcp system source directory, else absolute."
  (when pathname
    (let ((pn (uiop/pathname:ensure-pathname pathname))
          (bases (remove nil
                         (list *project-root*
                               (uiop/os:getcwd)
                               (ignore-errors
                                (asdf/system:system-source-directory :cl-mcp))))))
      (dolist (base bases (uiop/filesystem:native-namestring pn))
        (when (uiop/pathname:subpathp pn base)
          (return
           (uiop/filesystem:native-namestring
            (uiop/pathname:enough-pathname pn base))))))))
