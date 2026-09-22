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
           #:write-path-refused
           #:write-path-refused-path
           #:write-path-refused-reason
           #:resolve-path-in-project
           #:resolve-readable-path
           #:native-path-namestring
           #:normalize-path-for-display
           #:broad-root-p))

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
Allows project-root subpaths and source dirs of registered ASDF systems.
Resolves symlinks via TRUENAME before containment checks to prevent
symlink-based path traversal."
  (ensure-project-root)
  (let* ((abs (canonical-path pn))
         ;; Resolve symlinks to get real filesystem path
         (resolved (or (handler-case (truename abs) (file-error () nil)) abs))
         (normalized-abs (if (uiop/filesystem:directory-exists-p resolved)
                             (uiop/pathname:ensure-directory-pathname resolved)
                             resolved))
         (project-dir (uiop/pathname:ensure-directory-pathname *project-root*))
         ;; Also resolve project root symlinks for consistent comparison
         (resolved-project-dir (or (handler-case
                                       (uiop/pathname:ensure-directory-pathname
                                        (truename project-dir))
                                     (file-error () nil))
                                   project-dir))
         (project-ok (path-inside-p normalized-abs resolved-project-dir)))
    (when project-ok
      (return-from allowed-read-path normalized-abs))
    ;; Check ASDF system directories (also resolve symlinks)
    (let ((systems (asdf/system-registry:registered-systems)))
      (dolist (name systems)
        (let* ((dir (ignore-errors (asdf/system:system-source-directory name)))
               (resolved-dir (when dir
                               (or (handler-case (truename dir) (file-error () nil)) dir))))
          (when (and resolved-dir (path-inside-p normalized-abs resolved-dir))
            (return-from allowed-read-path normalized-abs)))))
    nil))

(declaim (ftype (function ((or string pathname)) pathname) ensure-write-path))

(define-condition write-path-refused (simple-error)
  ((path :initarg :path :reader write-path-refused-path)
   (reason :initarg :reason :reader write-path-refused-reason))
  (:documentation "ENSURE-WRITE-PATH declined PATH before anything was written.
REASON is a keyword naming why: :ABSOLUTE, :OUTSIDE-PROJECT, and for a path it
cannot check safely :NO-FILE-NAME, :UNRESOLVABLE-ROOT, :NON-DIRECTORY-ANCESTOR,
:UNRESOLVABLE-ANCESTOR, :PARENT-AFTER-LINK, :PARENT-AFTER-MISSING or
:UNRESOLVABLE-TARGET.  A SIMPLE-ERROR, so handlers written for the plain errors
this function used to signal still see it."))

(defun %refuse-write (path reason control &rest arguments)
  "Signal WRITE-PATH-REFUSED for PATH with REASON and the message CONTROL."
  (error 'write-path-refused :path path :reason reason
                             :format-control control :format-arguments arguments))

(defun ensure-write-path (path)
  "Ensure PATH is relative to project root and return absolute pathname.
Resolves symlinks via TRUENAME to prevent symlink-based path traversal.
Signals WRITE-PATH-REFUSED if outside project root or absolute."
  (ensure-project-root)
  (let ((pn (uiop/pathname:ensure-pathname path)))
    (when (uiop/pathname:absolute-pathname-p pn)
      (%refuse-write path :absolute "Write path ~A must be relative to the project root"
                     path))
    (let* ((abs (canonical-path pn :relative-to *project-root*))
           (real (or (handler-case (truename abs) (file-error () nil)) abs))
           (project-dir (uiop/pathname:ensure-directory-pathname *project-root*))
           (resolved-project-dir (or (handler-case
                                         (uiop/pathname:ensure-directory-pathname
                                          (truename project-dir))
                                       (file-error () nil))
                                     project-dir)))
      (unless (path-inside-p real resolved-project-dir)
        (%refuse-write path :outside-project "Write path ~A is outside project root" path))
      real)))

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
                         (or (handler-case (truename target) (file-error () nil))
                             (error "Path does not exist: ~A" target))
                         (or (handler-case (truename target) (file-error () nil))
                             target))))
      (unless (path-inside-p canonical base)
        (error "Path ~A is outside project root ~A" target base))
      canonical)))

(declaim (ftype (function ((or null string pathname) &key (:must-exist boolean))
                           pathname)
                resolve-readable-path))

(defun resolve-readable-path (path &key (must-exist nil))
  "Resolve PATH to an absolute pathname that is readable per the read policy.
Shares RESOLVE-PATH-IN-PROJECT's calling contract: a NIL or empty PATH resolves
to *project-root*, a relative PATH is merged against it, MUST-EXIST signals when
the target does not exist, and a disallowed PATH signals rather than returning
NIL. The containment rule is ALLOWED-READ-PATH's instead: subpaths of
*project-root* AND source directories of registered ASDF systems are both
accepted. Use this for read-only tools, which should reach the dependency
sources that the read tools already expose."
  (ensure-project-root)
  (let* ((base *project-root*)
         (target (if (or (null path) (and (stringp path) (string= path "")))
                     (uiop/pathname:ensure-directory-pathname base)
                     (canonical-path path))))
    (when (and must-exist
               (null (handler-case (truename target) (file-error () nil))))
      (error "Path does not exist: ~A" target))
    (or (allowed-read-path target)
        (error "Path ~A is outside project root ~A and outside the source ~
                directory of every registered ASDF system"
               target base))))

(declaim (ftype (function ((or null string pathname)) (or null string))
                normalize-path-for-display))

(defparameter *broad-root-deny-list*
  '("/" "/tmp/" "/home/")
  "Directory namestrings that are too broad for a project root.
Checked against both the raw path and its truename to prevent
symlink bypass (e.g. macOS /tmp -> /private/tmp/).")

(declaim (ftype (function ((or string pathname)) boolean) broad-root-p))

(defun broad-root-p (dir-path)
  "Return T when DIR-PATH is too broad to be used as a project root.
Checks both the raw namestring and the truename-resolved namestring
against *broad-root-deny-list* to prevent symlink bypass."
  (let ((raw-str (namestring (uiop/pathname:ensure-directory-pathname dir-path))))
    (when (member raw-str *broad-root-deny-list* :test #'string=)
      (return-from broad-root-p t))
    (let ((resolved (ignore-errors
                      (namestring
                       (uiop/pathname:ensure-directory-pathname
                        (truename dir-path))))))
      (when (and resolved (member resolved *broad-root-deny-list* :test #'string=))
        (return-from broad-root-p t)))
    nil))

(defun native-path-namestring (pathname)
  "Return PATHNAME as a filesystem path string, or NIL for NIL.

NAMESTRING is not one.  It escapes the characters this implementation's
pathname syntax treats as wild -- [ and ] among them on SBCL -- so what it
returns round-trips through the pathname READER, not through the filesystem:
a file under demo[old]/ comes back as demo\\[old]/, which names nothing on
disk.  Every path cl-mcp hands a caller is one the caller may hand back, and
uiop parses an incoming path natively, so the two only agree if what goes out
is native too.

Falls back to NAMESTRING for a genuinely wild pathname, which has no native
form at all -- better a path that cannot be opened than an error from a
function whose job is to describe one."
  (when pathname
    (handler-case (uiop:native-namestring pathname)
      (error () (namestring pathname)))))

(defun normalize-path-for-display (pathname)
  "Return a native path string for PATHNAME, relative to *project-root* when
possible.  Native (NATIVE-PATH-NAMESTRING) because this is the path a caller
reads and then passes back to another tool.
Falls back to CWD, then cl-mcp system source directory, else absolute.
Logical pathnames are translated to physical before processing.  Returns NIL,
without signaling, when PATHNAME is a logical pathname with no registered
translation for it -- SBCL build-internal pseudo-hosts such as SYS:OBJ;... can
appear this way in xref source locations."
  (when pathname
    (let ((pn (handler-case
                  (translate-logical-pathname (uiop/pathname:ensure-pathname pathname))
                (file-error () nil))))
      (when pn
        (let ((bases (remove nil
                             (list *project-root*
                                   (uiop/os:getcwd)
                                   (ignore-errors
                                    (asdf/system:system-source-directory :cl-mcp))))))
          (dolist (base bases (native-path-namestring pn))
            (when (uiop/pathname:subpathp pn base)
              (return
               (native-path-namestring
                (uiop/pathname:enough-pathname pn base))))))))))
