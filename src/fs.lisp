;;;; src/fs.lisp

(defpackage #:cl-mcp/src/fs
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:getenv
                #:getcwd
                #:chdir
                #:subpathp
                #:ensure-pathname
                #:merge-pathnames*
                #:read-file-string
                #:directory
                #:directory-exists-p
                #:absolute-pathname-p)
  (:import-from #:uiop/utility #:string-prefix-p)
  (:import-from #:asdf #:registered-systems #:system-source-directory)
  (:import-from #:uiop/filesystem #:ensure-directories-exist)
  (:export #:*project-root*
           #:fs-resolve-read-path
           #:fs-read-file
           #:fs-write-file
           #:fs-list-directory
           #:fs-get-project-info
           #:fs-set-project-root))

(in-package #:cl-mcp/src/fs)

(defparameter *project-root*
  (let ((env-root (uiop:getenv "MCP_PROJECT_ROOT")))
    (when env-root
      (uiop:ensure-directory-pathname env-root)))
  "Absolute pathname of the project root.
Must be set via MCP_PROJECT_ROOT environment variable or by calling fs-set-project-root.
If NIL, all file operations will fail with an error instructing to set the project root.")

(defparameter *hidden-prefixes* '("." ".git" ".hg" ".svn" ".cache" ".fasl"))
(defparameter *skip-extensions* '("fasl" "ufasl" "x86f" "cfasl"))
(defparameter *fs-read-max-bytes* 1048576
  "Maximum number of characters allowed for fs-read-file when LIMIT is provided.")

(defun %fd-count ()
  (ignore-errors (length (directory #P"/proc/self/fd/*"))))

(defun %ensure-project-root ()
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
           (or (ignore-errors (namestring (uiop:getcwd))) "(unknown)")
           (length (asdf:registered-systems)))))
(defun %path-inside-p (child parent)
  "Return T when CHILD pathname is a subpath of directory PARENT."
  (uiop:subpathp child parent))

(defun %canonical-path (path &key relative-to)
  "Turn PATH designator into a physical absolute pathname.
If RELATIVE-TO is provided and PATH is relative, merge it with RELATIVE-TO."
  (%ensure-project-root)
  (let* ((pn (uiop:ensure-pathname path :want-relative nil
                                   :ensure-directory nil :ensure-absolute nil))
         (abs (if (uiop:absolute-pathname-p pn)
                  pn
                  (uiop:merge-pathnames* pn (or relative-to *project-root*)))))
    (uiop:ensure-pathname abs :want-relative nil)))

(defun %allowed-read-path-p (pn)
  "Return PN if readable per policy, else NIL.
Allows project-root subpaths and source dirs of registered ASDF systems."
  (%ensure-project-root)
  (let* ((abs (%canonical-path pn))
         ;; Normalize as directory if it exists and is a directory
         (normalized-abs (if (uiop:directory-exists-p abs)
                             (uiop:ensure-directory-pathname abs)
                             abs))
         (project-ok (%path-inside-p normalized-abs (uiop:ensure-directory-pathname *project-root*))))
    (when project-ok (return-from %allowed-read-path-p normalized-abs))
    ;; absolute path allowed only inside system-source-directory of registered systems
    (let ((systems (asdf:registered-systems)))
      (dolist (name systems)
        (let ((dir (ignore-errors (asdf:system-source-directory name))))
          (when (and dir (%path-inside-p normalized-abs dir))
            (return-from %allowed-read-path-p normalized-abs)))))
    nil))

(defun %ensure-write-path (path)
  "Ensure PATH is relative to project root and return absolute pathname.
Signals an error if outside project root or absolute."
  (%ensure-project-root)
  (let* ((pn (uiop:ensure-pathname path :want-relative t))
         (abs (%canonical-path pn :relative-to *project-root*))
         (real (or (ignore-errors (truename abs)) abs)))
    (unless (%path-inside-p real (uiop:ensure-directory-pathname *project-root*))
      (error "Write path ~A is outside project root" path))
    real))

(defun %read-file-string (pn offset limit)
  "Read file PN honoring OFFSET and LIMIT (both may be NIL)."
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (when (and limit (> limit *fs-read-max-bytes*))
    (error "limit ~D exceeds maximum ~D" limit *fs-read-max-bytes*))
  (with-open-file (in pn :direction :input :element-type 'character)
    (when offset (file-position in offset))
    (let* ((len (or limit (ignore-errors (file-length in)) *fs-read-max-bytes*))
           (buf (make-string len))
           (count (read-sequence buf in :end len)))
      (subseq buf 0 count))))

(defun fs-resolve-read-path (path)
  "Return a canonical pathname for PATH when it is readable per policy.
Signals an error when PATH is outside the allow-list."
  (let ((pn (%allowed-read-path-p path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    pn))

(defun fs-read-file (path &key offset limit)
  "Read text file PATH with optional OFFSET and LIMIT.
Returns the content string."
  (when (and offset (not (integerp offset)))
    (error "offset must be an integer"))
  (when (and limit (not (integerp limit)))
    (error "limit must be an integer"))
  (let ((pn (%allowed-read-path-p path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (log-event :debug "fs.read.open" "path" (namestring pn) "offset" offset "limit" limit "fd" (%fd-count))
    (let ((text (%read-file-string pn offset limit)))
      (log-event :debug "fs.read.close" "path" (namestring pn) "fd" (%fd-count))
      text)))

(defun %write-string-to-file (pn content)
  (uiop/filesystem::ensure-directories-exist pn)
  (with-open-file (out pn :direction :output :if-exists :supersede :if-does-not-exist :create
                       :element-type 'character)
    (write-string content out)
    (finish-output out))
  t)

(defun fs-write-file (path content)
  "Write CONTENT to PATH relative to project root.
Returns T on success."
  (let ((pn (%ensure-write-path path)))
    (log-event :debug "fs.write.open" "path" (namestring pn) "bytes" (length content) "fd" (%fd-count))
    (unwind-protect
         (%write-string-to-file pn content)
      (log-event :debug "fs.write.close" "path" (namestring pn) "fd" (%fd-count)))))

(defun %entry-name (path)
  "Return display name for PATH, trimming trailing slash on directories."
  (let* ((namestr (file-namestring path))
         (trimmed (and namestr (string-right-trim "/" namestr))))
    (if (and trimmed (plusp (length trimmed)))
        trimmed
        (let* ((dir (pathname-directory path))
               (leaf (car (last dir))))
          (and leaf (string leaf))))))

(defun %should-skip-entry-p (path)
  (let* ((name (%entry-name path))
         (type (pathname-type path)))
    (or (null name)
        (some (lambda (pref) (uiop:string-prefix-p pref name)) *hidden-prefixes*)
        (and type (member (string-downcase type) *skip-extensions* :test #'string=)))))

(defun fs-list-directory (path)
  "List directory entries at PATH respecting read allow-list.
Returns a vector of hash-tables with keys \"name\" and \"type\" (file|directory)."
  (let* ((pn (%allowed-read-path-p path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (unless (uiop:directory-exists-p pn)
      (error "Directory ~A (resolved to ~A) does not exist or is not readable"
             path (namestring pn)))
    (let* ((patterns (list #P"*" #P"*.*"))  ; grab dirs and files (with types)
           (entries (loop for pat in patterns
                          append (directory (uiop:merge-pathnames* pat pn))))
           (seen (make-hash-table :test #'equal))
           (results '()))
      (dolist (p entries)
        (unless (%should-skip-entry-p p)
          (let ((key (namestring p)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (let* ((h (make-hash-table :test #'equal))
                     (name (%entry-name p)))
                (setf (gethash "name" h) name
                      (gethash "type" h)
                      (if (uiop:directory-pathname-p p) "directory" "file"))
                (push h results))))))
      (coerce (nreverse results) 'vector))))

(defun fs-get-project-info ()
  "Return project root and working directory information.
Returns a hash-table with keys:
  - project_root: absolute path to project root
  - cwd: current working directory
  - project_root_source: how project root was determined (env|cwd|asdf)
  - relative_cwd: cwd relative to project_root (when inside project)"
  (%ensure-project-root)
  (let* ((cwd (ignore-errors (uiop:getcwd)))
         (env-root (uiop:getenv "MCP_PROJECT_ROOT"))
         (root-source (if env-root "env" "explicit"))
         (h (make-hash-table :test #'equal)))
    (setf (gethash "project_root" h) (namestring *project-root*)
          (gethash "cwd" h) (and cwd (namestring cwd))
          (gethash "project_root_source" h) root-source)
    (let ((root (uiop:ensure-directory-pathname *project-root*)))
      (when (and cwd (uiop:subpathp cwd root))
        (setf (gethash "relative_cwd" h)
              (uiop:native-namestring (uiop:enough-pathname cwd root)))))
    h))

(defun fs-set-project-root (path)
  "Set the project root to PATH and change the current working directory.
Returns a hash-table with updated path information:
  - project_root: the new absolute project root path
  - cwd: the new current working directory
  - previous_root: the previous project root path (or \"(not set)\" if was nil)
  - status: confirmation message"
  (unless (stringp path)
    (error "path must be a string"))
  (let* ((prev-root *project-root*)
         (temp-root (uiop:ensure-directory-pathname path)))
    (unless (uiop:directory-exists-p temp-root)
      (error "Directory ~A does not exist" path))
    ;; Convert to absolute path using truename
    (let ((new-root (truename temp-root)))
      ;; Update the project root parameter
      (setf *project-root* new-root)
      ;; Change the current working directory
      (uiop:chdir new-root)
      (log-event :info "fs.set-project-root"
                 "previous" (if prev-root (namestring prev-root) "(not set)")
                 "new" (namestring new-root))
      ;; Return updated path information
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "project_root" h) (namestring new-root)
              (gethash "cwd" h) (namestring (uiop:getcwd))
              (gethash "previous_root" h) (if prev-root (namestring prev-root) "(not set)")
              (gethash "status" h) (format nil "Project root set to ~A" (namestring new-root)))
        h))))
