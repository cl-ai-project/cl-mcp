;;;; src/fs.lisp

(defpackage #:cl-mcp/src/fs
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*
                #:*project-root-lock*)
  (:import-from #:bordeaux-threads #:with-lock-held)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content #:rpc-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-project-root
                #:allowed-read-path
                #:ensure-write-path)
  (:import-from #:cl-mcp/src/utils/system
                #:fd-count)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:getenv
                #:getcwd
                #:chdir
                #:subpathp
                #:merge-pathnames*
                #:directory
                #:directory-exists-p
                #:absolute-pathname-p)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/pool
                #:pool-worker-info
                #:send-root-to-session-worker)
  (:import-from #:uiop/utility #:string-prefix-p)
  (:import-from #:uiop/filesystem #:ensure-directories-exist)
  (:export #:fs-resolve-read-path
           #:fs-read-file
           #:fs-write-file
           #:fs-list-directory
           #:fs-get-project-info
           #:fs-set-project-root))

(in-package #:cl-mcp/src/fs)

;; *project-root* is imported from cl-mcp/src/project-root and re-exported

(defparameter *hidden-prefixes* '("." ".git" ".hg" ".svn" ".cache" ".fasl"))
(defparameter *skip-extensions* '("fasl" "ufasl" "x86f" "cfasl"))
(defparameter *fs-read-max-bytes* 1048576
  "Maximum number of characters allowed for fs-read-file when LIMIT is provided.")

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
  (let ((pn (allowed-read-path path)))
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
  (let ((pn (allowed-read-path path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (log-event :debug "fs.read.open"
               "path" (namestring pn)
               "offset" offset
               "limit" limit
               "fd" (fd-count))
    (let ((text (%read-file-string pn offset limit)))
      (log-event :debug "fs.read.close"
                 "path" (namestring pn)
                 "fd" (fd-count))
      text)))

(defun %write-string-to-file (pn content)
  "Write CONTENT to PN atomically via write-to-temp-then-rename.
On failure the original file is preserved."
  (uiop/filesystem::ensure-directories-exist pn)
  (let ((tmp (make-pathname :name (format nil ".~A.tmp" (pathname-name pn))
                            :type (pathname-type pn)
                            :defaults pn)))
    (unwind-protect
         (progn
           (with-open-file (out tmp
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type 'character)
             (write-string content out)
             (finish-output out))
           (rename-file tmp pn)
           t)
      ;; Clean up temp file on failure
      (when (probe-file tmp)
        (ignore-errors (delete-file tmp))))))

(defun fs-write-file (path content)
  "Write CONTENT to PATH relative to project root.
Returns T on success."
  (let ((pn (ensure-write-path path)))
    (log-event :debug "fs.write.open"
               "path" (namestring pn)
               "bytes" (length content)
               "fd" (fd-count))
    (unwind-protect
         (%write-string-to-file pn content)
      (log-event :debug "fs.write.close"
                 "path" (namestring pn)
                 "fd" (fd-count)))))

(defun %lisp-source-pathname-p (pn)
  "Return T when PN has a Common Lisp source extension."
  (let ((type (pathname-type pn)))
    (and type
         (member (string-downcase type)
                 '("lisp" "asd")
                 :test #'string=))))

(defun %existing-lisp-overwrite-error (id path)
  "Return a structured RPC error for forbidden Lisp overwrite, or NIL.
New Lisp source file creation is allowed."
  (let ((pn (ensure-write-path path)))
    (when (and (probe-file pn)
               (%lisp-source-pathname-p pn))
      (rpc-error id -32602
                 "Cannot overwrite existing .lisp/.asd with fs-write-file; use lisp-edit-form."
                 (make-ht "code" "existing_lisp_overwrite_forbidden"
                          "path" path
                          "next_tool" "lisp-edit-form"
                          "required_args"
                          (vector "file_path" "form_type" "form_name"
                                  "operation" "content")
                          "new_file_creation_allowed" t)))))

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
  (let ((name (%entry-name path))
        (type (pathname-type path)))
    (or (null name)
        (some (lambda (pref) (uiop:string-prefix-p pref name)) *hidden-prefixes*)
        (and type (member (string-downcase type) *skip-extensions* :test #'string=)))))

(defun fs-list-directory (path)
  "List directory entries at PATH respecting read allow-list.
Returns a vector of hash-tables with keys \"name\" and \"type\" (file|directory)."
  (let ((pn (allowed-read-path path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (unless (uiop:directory-exists-p pn)
      (error "Directory ~A (resolved to ~A) does not exist or is not readable"
             path (namestring pn)))
    (let* ((patterns (list #P"*" #P"*.*"))
           (entries (loop for pat in patterns
                          append (directory (uiop:merge-pathnames* pat pn))))
           (seen (make-hash-table :test #'equal))
           (results '()))
      (dolist (p entries)
        (unless (%should-skip-entry-p p)
          (let ((key (namestring p)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (let ((h (make-hash-table :test #'equal))
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
  (ensure-project-root)
  (let ((cwd (ignore-errors (uiop:getcwd)))
        (env-root (uiop:getenv "MCP_PROJECT_ROOT"))
        (h (make-hash-table :test #'equal)))
    (let ((root-source (if env-root "env" "explicit")))
      (setf (gethash "project_root" h) (namestring *project-root*)
            (gethash "cwd" h) (and cwd (namestring cwd))
            (gethash "project_root_source" h) root-source)
      (let ((root (uiop:ensure-directory-pathname *project-root*)))
        (when (and cwd (uiop:subpathp cwd root))
          (setf (gethash "relative_cwd" h)
                (uiop:native-namestring (uiop:enough-pathname cwd root)))))
      (when *use-worker-pool*
        (setf (gethash "workers" h) (pool-worker-info)))
      h)))

(defun fs-set-project-root (path)
  "Set the project root to PATH and change the current working directory.
Returns a hash-table with updated path information:
  - project_root: the new absolute project root path
  - cwd: the new current working directory
  - previous_root: the previous project root path (or (not set) if was nil)
  - status: confirmation message"
  (unless (stringp path) (error "path must be a string"))
  (when (string= (string-trim '(#\Space #\Tab) path) "")
    (error "path must not be empty"))
  (let* ((prev-root *project-root*)
         (requested (uiop/pathname:ensure-directory-pathname path))
         (base (ignore-errors (uiop/os:getcwd)))
         (temp-root
          (if (uiop/pathname:absolute-pathname-p requested)
              requested
              (uiop/pathname:merge-pathnames* requested base))))
    (unless (uiop/filesystem:directory-exists-p temp-root)
      (error "Directory ~A does not exist" path))
    (let ((new-root (truename temp-root)))
      ;; C2: Reject overly broad roots that would disable the security sandbox
      (let ((root-str (namestring new-root)))
        (when (member root-str '("/" "/tmp/" "/home/") :test #'string=)
          (error "Refusing to set project root to ~A — too broad" root-str)))
      ;; C3: Atomic multi-step mutation under lock
      (bt:with-lock-held (*project-root-lock*)
        (setf *project-root* new-root)
        (uiop/os:chdir new-root)
        (setf *default-pathname-defaults*
                (uiop/pathname:ensure-directory-pathname new-root)))
      (log-event :info "fs.set-project-root" "previous"
       (if prev-root
           (namestring prev-root)
           "(not set)")
       "new" (namestring new-root))
      (when *use-worker-pool*
        (ignore-errors
         (send-root-to-session-worker *current-session-id* new-root)))
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "project_root" h) (namestring new-root)
              (gethash "cwd" h) (namestring (uiop/os:getcwd))
              (gethash "previous_root" h)
                (if prev-root
                    (namestring prev-root)
                    "(not set)")
              (gethash "status" h)
                (format nil "Project root set to ~A" (namestring new-root)))
        h))))

(define-tool "fs-read-file"
  :description "Read a text file with optional offset and limit.
Prefer absolute paths inside the project; offset/limit are character counts
to avoid loading whole files.
It can only open files in the project or in loaded dependent libraries.

For .lisp and .asd files, prefer 'lisp-read-file' instead - it provides
collapsed signatures view that saves ~70% of context window tokens."
  :args ((path :type :string :required t
               :description "Absolute path inside the project or a registered ASDF system")
         (offset :type :integer
                 :description "0-based character offset to start reading")
         (limit :type :integer
                :description "Maximum characters to return; omit to read to end"))
  :body
  (let ((content-string (fs-read-file path :offset offset :limit limit)))
    (result id
            (make-ht "content" (text-content content-string)
                     "text" content-string
                     "path" path
                     "offset" offset
                     "limit" limit))))

(define-tool "fs-write-file"
  :description "Write text content to a file relative to project root.
Parent directories are automatically created if they do not exist.
Use this for creating NEW files or editing non-Lisp files (e.g., markdown, config files).
For editing EXISTING Lisp source code, you MUST use 'lisp-edit-form' instead
to preserve structure and comments."
  :args ((path :type :string :required t
               :description "Relative path under the project root; absolute paths are rejected")
         (content :type :string :required t
                  :description "Text content to write"))
  :body
  (or (%existing-lisp-overwrite-error id path)
      (progn
        (fs-write-file path content)
        (result id
                (make-ht "success" t
                         "content" (text-content
                                    (format nil "Wrote ~A (~D chars)" path (length content)))
                         "path" path
                         "bytes" (length content)))))
  )

(define-tool "fs-list-directory"
  :description "List entries in a directory, filtering hidden and build artifacts.
Use absolute paths inside the project or an ASDF system."
  :args ((path :type :string :required t
               :description "Absolute directory path under the project root or a registered
ASDF system"))
  :body
  (let ((entries (fs-list-directory path)))
    (result id
            (make-ht "content" (text-content (format nil "~D entries" (length entries)))
                     "entries" entries
                     "path" path))))

(define-tool "fs-get-project-info"
  :description "Get project root and current working directory information for
path resolution context."
  :args ()
  :body
  (let* ((info (fs-get-project-info))
         (summary (format nil "Project root: ~A~%CWD: ~A~%Source: ~A"
                          (gethash "project_root" info)
                          (or (gethash "cwd" info) "(none)")
                          (gethash "project_root_source" info))))
    (result id
            (make-ht "content" (text-content summary)
                     "project_root" (gethash "project_root" info)
                     "cwd" (gethash "cwd" info)
                     "project_root_source" (gethash "project_root_source" info)
                     "relative_cwd" (gethash "relative_cwd" info)
                     "workers" (gethash "workers" info)))))

(define-tool "fs-set-project-root"
  :description "Set the server's project root directory to the specified path.
Use this to synchronize the server's working directory with the client's
project location. The server will change its current working directory
to the specified path.
RESTRICTION: You MUST only provide your current working directory (e.g., obtained via pwd).
Do not use arbitrary paths."
  :args ((path :type :string :required t
               :description "Absolute path to the project root directory"))
  :body
  (let ((info (fs-set-project-root path)))
    (result id
            (make-ht "content" (text-content (gethash "status" info))
                     "info" info))))
