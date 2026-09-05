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
                #:ensure-write-path
                #:broad-root-p)
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
  (:export #:*lisp-file-unparseable-hook*
           #:fs-resolve-read-path
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
  "Read file PN honoring OFFSET and LIMIT (both may be NIL).
Returns (VALUES content-string truncated-p file-length).
TRUNCATED-P is T when the file was larger than the effective read cap.
FILE-LENGTH is the total size of the file (NIL if unknown)."
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (when (and limit (> limit *fs-read-max-bytes*))
    (error "limit ~D exceeds maximum ~D" limit *fs-read-max-bytes*))
  (with-open-file (in pn :direction :input :element-type 'character)
    (when offset (file-position in offset))
    (let* ((raw-len (ignore-errors (file-length in)))
           (remaining (and raw-len (max 0 (- raw-len (or offset 0)))))
           (effective (or limit remaining *fs-read-max-bytes*))
           (capped (min effective *fs-read-max-bytes*))
           (buf (make-string capped))
           (count (read-sequence buf in :end capped))
           (text (subseq buf 0 count))
           ;; FILE-LENGTH counts octets even on a character stream, so a
           ;; multibyte file can look bigger than the cap and still fit in
           ;; it: report truncation only when the buffer filled and input
           ;; really remains.
           ;; The peek decodes one character past the cap; if that byte is
           ;; not decodable the file simply continues, so it is truncated.
           (truncated (and raw-len (> effective capped)
                           (= count capped)
                           (handler-case
                               (not (eq (peek-char nil in nil :eof) :eof))
                             (error () t)))))
      (values text truncated raw-len))))

(defun fs-resolve-read-path (path)
  "Return a canonical pathname for PATH when it is readable per policy.
Signals an error when PATH is outside the allow-list."
  (let ((pn (allowed-read-path path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    pn))

(defun fs-read-file (path &key offset limit)
  "Read text file PATH with optional OFFSET and LIMIT.
Returns (VALUES content-string truncated-p file-length)."
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
    (multiple-value-bind (text truncated file-length)
        (%read-file-string pn offset limit)
      (log-event :debug "fs.read.close"
                 "path" (namestring pn)
                 "fd" (fd-count))
      (values text truncated file-length))))

(defun %write-string-to-file (pn content)
  "Write CONTENT to PN atomically via write-to-temp-then-rename.
On failure the original file is preserved."
  (ensure-directories-exist pn)
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
        (handler-case (delete-file tmp) (file-error () nil))))))

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

(defvar *lisp-file-unparseable-hook* nil
  "Predicate of two arguments (an absolute pathname and the file's text) that
returns T when the structural editing tools cannot parse that Lisp file in a
way no readtable can fix. The fs-write-file overwrite guard consults it so
that overwriting is permitted exactly when lisp-edit-form and
lisp-patch-form cannot locate any form in the file.
cl-mcp/src/lisp-edit-form-core installs a predicate built on its own parser
(which understands named-readtable declarations); this indirection exists
because fs cannot import that parser without a dependency cycle. When NIL
the guard always holds: there is no weaker fallback definition.")

(defun %lisp-file-unparseable-p (pn)
  "Return T when the structural editing tools cannot parse the Lisp source at
PN in a way no readtable can fix, so that overwriting it is the only repair
path. The verdict comes from *LISP-FILE-UNPARSEABLE-HOOK*, installed by
cl-mcp/src/lisp-edit-form-core at load time: the edit tools' own parser,
which handles named-readtable declarations and classifies failures by
condition type. Without a hook (a partial image that loaded fs alone) the
answer is NIL, i.e. the overwrite guard always holds -- there is no
second, weaker definition of \"unparseable\" to drift from the tools'.
A read truncated at *FS-READ-MAX-BYTES* is reported as parseable, since a
cut-off prefix proves nothing, and so is a file that cannot be decoded at
all (invalid UTF-8, say): failing closed keeps the guard in place instead of
turning the write into an internal error."
  (multiple-value-bind (text truncated)
      (handler-case (%read-file-string pn nil nil)
        (error () (values "" t)))
    (and (not truncated)
         *lisp-file-unparseable-hook*
         (funcall *lisp-file-unparseable-hook* pn text)
         t)))

(defun %existing-lisp-overwrite-error (id path allow-unparseable)
  "Return a structured RPC error for a forbidden Lisp overwrite, or NIL.
New Lisp source file creation is always allowed. An existing .lisp/.asd file
may be overwritten only when the caller passed ALLOW-UNPARSEABLE, i.e.
explicitly judged that the breakage is real rather than custom reader syntax
the structural tools could handle with a readtable, AND the file does not
parse (a missing or stray parenthesis, per %LISP-FILE-UNPARSEABLE-P). The
parse is attempted only when the caller opted in: without the flag the
answer is a refusal either way, so the common case pays nothing.
No heuristic can tell real breakage from custom syntax without knowing the
readtable, so the decision is the caller's; the guard only makes sure a
file that does parse is never rewritten wholesale."
  (let ((pn (ensure-write-path path)))
    (when (and (probe-file pn)
               (%lisp-source-pathname-p pn))
      (let ((unparseable (and allow-unparseable (%lisp-file-unparseable-p pn))))
        (unless unparseable
          (if allow-unparseable
              (rpc-error id -32602
                         (format nil "Cannot overwrite existing .lisp/.asd with fs-write-file: ~
the file parses, so allow_unparseable_overwrite does not apply; use lisp-edit-form ~
(with the readtable parameter if the file uses custom reader syntax).")
                         (make-ht "code" "existing_lisp_overwrite_forbidden"
                                  "path" path
                                  "next_tool" "lisp-edit-form"
                                  "required_args"
                                  (vector "file_path" "form_type" "form_name"
                                          "operation" "content")
                                  "new_file_creation_allowed" t))
              ;; The plain refusal keeps its historical wording; the opt-in
              ;; is advertised through the data field.
              (rpc-error id -32602
                         (format nil "Cannot overwrite existing .lisp/.asd with ~
fs-write-file; use lisp-edit-form.")
                         (make-ht "code" "existing_lisp_overwrite_forbidden"
                                  "path" path
                                  "next_tool" "lisp-edit-form"
                                  "required_args"
                                  (vector "file_path" "form_type" "form_name"
                                          "operation" "content")
                                  "new_file_creation_allowed" t
                                  "allow_unparseable_overwrite_available" t))))))))

(defun %entry-name (path)
  "Return display name for PATH, trimming trailing slash on directories."
  (let* ((namestr (file-namestring path))
         (trimmed (and namestr (string-right-trim "/" namestr))))
    (if (and trimmed (plusp (length trimmed)))
        trimmed
        (let* ((dir (pathname-directory path))
               (leaf (car (last dir))))
          (and leaf (string leaf))))))

(defun %should-skip-entry-p (path &key show-hidden)
  "Return T when PATH should be omitted from a directory listing.
Build artifacts (fasl and related extensions) are always filtered.
Dotfiles and other entries matching *HIDDEN-PREFIXES* are filtered
unless SHOW-HIDDEN is non-nil."
  (let ((name (%entry-name path)) (type (pathname-type path)))
    (or (null name)
        (and (not show-hidden)
             (some (lambda (pref) (string-prefix-p pref name))
                   *hidden-prefixes*))
        (and type
             (member (string-downcase type) *skip-extensions* :test
                     #'string=)))))

(defun fs-list-directory (path &key show-hidden)
  "List directory entries at PATH respecting read allow-list.
Returns a vector of hash-tables with keys \"name\" and \"type\" (file|directory).
When SHOW-HIDDEN is nil (default), dotfiles and entries matching
*HIDDEN-PREFIXES* are omitted. When SHOW-HIDDEN is non-nil, those are
included, but build artifacts (fasl family) remain filtered so that
listings stay useful."
  (let ((pn (allowed-read-path path)))
    (unless pn (error "Read not permitted for path ~A" path))
    (unless (directory-exists-p pn)
      (error "Directory ~A (resolved to ~A) does not exist or is not readable"
             path (namestring pn)))
    (let* ((patterns (list #P"*" #P"*.*"))
           (entries
            (loop for pat in patterns
                  append (directory (merge-pathnames* pat pn))))
           (seen (make-hash-table :test #'equal))
           (results 'nil))
      (dolist (p entries)
        (unless (%should-skip-entry-p p :show-hidden show-hidden)
          (let ((key (namestring p)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (let ((h (make-hash-table :test #'equal)) (name (%entry-name p)))
                (setf (gethash "name" h) name
                      (gethash "type" h)
                        (if (uiop/pathname:directory-pathname-p p)
                            "directory"
                            "file"))
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
    ;; C2: Reject overly broad roots that would disable the security sandbox.
    (when (broad-root-p temp-root)
      (error "Refusing to set project root to ~A — too broad"
             (namestring temp-root)))
    (let ((new-root (truename temp-root)))
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
  (multiple-value-bind (content-string truncated file-length)
      (fs-read-file path :offset offset :limit limit)
    (let ((ht (make-ht "content" (text-content
                                  (if truncated
                                      (let ((next-offset (+ (or offset 0) (length content-string))))
                                        (format nil "~A~%~%[TRUNCATED: file is ~:D chars, showing ~:D from offset ~:D. Use offset=~D to read more.]"
                                                content-string file-length (length content-string) (or offset 0) next-offset))
                                      content-string))
                       "text" content-string
                       "path" path
                       "offset" offset
                       "limit" limit)))
      (when truncated
        (setf (gethash "truncated" ht) t
              (gethash "file_length" ht) file-length
              (gethash "read_length" ht) (length content-string)))
      (result id ht))))

(define-tool "fs-write-file"
  :description "Write text content to a file relative to project root.
Parent directories are automatically created if they do not exist.
Use this for creating NEW files or editing non-Lisp files (e.g., markdown, config files).
For editing EXISTING Lisp source code, you MUST use 'lisp-edit-form' instead
to preserve structure and comments. The one exception: when an existing .lisp
file no longer parses (a missing or stray parenthesis), lisp-edit-form cannot
locate any form in it, so overwriting it here is the repair path -- but only
with allow_unparseable_overwrite=true, because a file that only looks broken
to the default reader may be valid under a custom readtable."
  :args ((path :type :string :required t
               :description "Relative path under the project root; absolute paths are rejected")
         (content :type :string :required t
                  :description "Text content to write")
         (allow-unparseable-overwrite
          :type :boolean :default nil
          :description "Permit overwriting an existing .lisp/.asd file that does not parse
(missing or stray parenthesis). Pass true only when you know the file uses no custom
reader syntax; otherwise use lisp-edit-form with the readtable parameter. Never
overrides the guard for a file that parses."))
  :body
  (or (%existing-lisp-overwrite-error id path allow-unparseable-overwrite)
      (progn
        (fs-write-file path content)
        (result id
                (make-ht "success" t
                         "content" (text-content
                                    (format nil "Wrote ~A (~D chars)" path (length content)))
                         "path" path
                         "bytes" (length content))))))

(define-tool "fs-list-directory"
  :description "List entries in a directory, filtering hidden and build artifacts.
Use absolute paths inside the project or an ASDF system.

Dotfiles (names starting with '.' such as .gitignore) are omitted by
default. Pass show_hidden=true to include them. Build artifacts (fasl
family) are always filtered so listings stay useful."
  :args ((path :type :string :required t
               :description "Absolute directory path under the project root or a registered
ASDF system")
         (show-hidden :type :boolean :default nil
                      :description "Include dotfiles and entries that would normally be hidden."))
  :body
  (let ((entries (fs-list-directory path :show-hidden show-hidden)))
    (result id
            (make-ht "content" (text-content
                                (with-output-to-string (s)
                                  (format s "~D entries in ~A~%" (length entries) path)
                                  (loop for e across entries
                                      do (if (hash-table-p e)
                                             (format s "~A ~A~%"
                                                     (if (equal (gethash "type" e) "directory")
                                                         "[dir] " "[file]")
                                                     (gethash "name" e))
                                             (format s "~A~%" e)))))
                     "entries" entries
                     "path" path
                     "show_hidden" show-hidden))))

(define-tool "fs-get-project-info"
  :description "Get project root and current working directory information for
path resolution context."
  :args ()
  :body
  (let* ((info (fs-get-project-info))
         (workers (gethash "workers" info))
         (summary (format nil "Project root: ~A~%CWD: ~A~%Source: ~A~@[~%Workers: ~A active~]"
                          (gethash "project_root" info)
                          (or (gethash "cwd" info) "(none)")
                          (gethash "project_root_source" info)
                          (when (and workers (arrayp workers) (plusp (length workers)))
                            (length workers)))))
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
