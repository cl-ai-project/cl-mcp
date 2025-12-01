;;;; src/fs.lisp

(defpackage #:cl-mcp/src/fs
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:getcwd
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
           #:fs-list-directory))

(in-package #:cl-mcp/src/fs)

(defparameter *project-root*
  (or (ignore-errors
        (uiop:ensure-directory-pathname
         (asdf:system-source-directory "cl-mcp")))
      (uiop:ensure-directory-pathname (uiop:getcwd)))
  "Absolute pathname of the project root. Prefer the ASDF system source
directory; fall back to the current working directory at load time.")

(defparameter *hidden-prefixes* '("." ".git" ".hg" ".svn" ".cache" ".fasl"))
(defparameter *skip-extensions* '("fasl" "ufasl" "x86f" "cfasl"))
(defparameter *fs-read-max-bytes* 1048576
  "Maximum number of characters allowed for fs-read-file when LIMIT is provided.")

(defun %fd-count ()
  (ignore-errors (length (directory #P"/proc/self/fd/*"))))

(defun %path-inside-p (child parent)
  "Return T when CHILD pathname is a subpath of directory PARENT."
  (uiop:subpathp child parent))

(defun %canonical-path (path &key relative-to)
  "Turn PATH designator into a physical absolute pathname.
If RELATIVE-TO is provided and PATH is relative, merge it with RELATIVE-TO."
  (let* ((pn (uiop:ensure-pathname path :want-relative nil
                                   :ensure-directory nil :ensure-absolute nil))
         (abs (if (uiop:absolute-pathname-p pn)
                  pn
                  (uiop:merge-pathnames* pn (or relative-to *project-root*)))))
    (uiop:ensure-pathname abs :want-relative nil)))

(defun %allowed-read-path-p (pn)
  "Return PN if readable per policy, else NIL.
Allows project-root subpaths and source dirs of registered ASDF systems."
  (let* ((abs (%canonical-path pn))
         (project-ok (%path-inside-p abs (uiop:ensure-directory-pathname *project-root*))))
    (when project-ok (return-from %allowed-read-path-p abs))
    ;; absolute path allowed only inside system-source-directory of registered systems
    (let ((systems (asdf:registered-systems)))
      (dolist (name systems)
        (let ((dir (ignore-errors (asdf:system-source-directory name))))
          (when (and dir (%path-inside-p abs dir))
            (return-from %allowed-read-path-p abs)))))
    nil))

(defun %ensure-write-path (path)
  "Ensure PATH is relative to project root and return absolute pathname.
Signals an error if outside project root or absolute."
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
      (error "Directory ~A does not exist or is not readable" path))
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
