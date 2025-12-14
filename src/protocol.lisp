;;;; src/protocol.lisp

(defpackage #:cl-mcp/src/protocol
  (:use #:cl)
  (:import-from #:cl-mcp/src/core #:version)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/repl #:repl-eval)
  (:import-from #:cl-mcp/src/fs
                #:*project-root*
                #:fs-read-file #:fs-write-file #:fs-list-directory #:fs-get-project-info #:fs-set-project-root)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:shadowing-import-from #:cl-mcp/src/code
                          #:code-find-references)
  (:import-from #:cl-mcp/src/code
                #:code-find-definition #:code-describe-symbol)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens)
  (:import-from #:yason #:encode #:parse)
  (:export
   #:+protocol-version+
   #:+supported-protocol-versions+
   #:server-state
   #:initialized-p
   #:client-info
   #:make-state
   #:process-json-line))

(in-package #:cl-mcp/src/protocol)

(defparameter +protocol-version+ "2025-06-18")
(defparameter +supported-protocol-versions+
  '("2025-06-18" "2025-03-26" "2024-11-05")
  "Supported MCP protocol versions, ordered by preference.")

(defclass server-state ()
  ((initialized-p :initform nil :accessor initialized-p)
   (client-info  :initform nil :accessor client-info)))

(defun make-state () (make-instance 'server-state))

(defun %decode-json (line)
  (yason:parse line))

(defun %encode-json (obj)
  (with-output-to-string (stream)
    (yason:encode obj stream)))

(defun %make-ht (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k h) v))
    h))

(defun %result (id payload)
  (%make-ht "jsonrpc" "2.0" "id" id "result" payload))

(defun %error (id code message &optional data)
  (let* ((err (%make-ht "code" code "message" message))
         (obj (%make-ht "jsonrpc" "2.0" "id" id "error" err)))
    (when data (setf (gethash "data" err) data))
    obj))

(defun %text-content (text)
  "Return a one-element content vector with TEXT as a text part."
  (vector (%make-ht "type" "text" "text" text)))

(defun handle-initialize (state id params)
  (declare (ignore state))
  ;; Sync rootPath from client if provided
  (when params
    (let* ((root-path (gethash "rootPath" params))
           (root-uri (gethash "rootUri" params))
           (root (or root-path
                     (and root-uri
                          (if (uiop:string-prefix-p "file://" root-uri)
                              (subseq root-uri 7)
                              root-uri)))))
      (when (and root (stringp root) (plusp (length root)))
        (handler-case
            (let ((root-dir (uiop:ensure-directory-pathname root)))
              (when (uiop:directory-exists-p root-dir)
                (setf cl-mcp/src/fs:*project-root* root-dir)
                (uiop:chdir root-dir)
                (log-event :info "initialize.sync-root"
                           "rootPath" (namestring root-dir)
                           "source" (if root-path "rootPath" "rootUri"))))
          (error (e)
            (log-event :warn "initialize.sync-root-failed"
                       "path" root
                       "error" (princ-to-string e)))))))
  (let* ((client-ver (and params (gethash "protocolVersion" params)))
         (supported (and client-ver (find client-ver +supported-protocol-versions+
                                          :test #'string=)))
         (chosen (cond
                   (supported supported)
                   ((null client-ver) (first +supported-protocol-versions+))
                   (t nil)))
         (caps (%make-ht
                "tools" (%make-ht "listChanged" t))))
    (if (null chosen)
        (%error id -32602
                (format nil "Unsupported protocolVersion ~A" client-ver)
                (%make-ht "supportedVersions" +supported-protocol-versions+))
        (%result id
                 (%make-ht
                  "protocolVersion" chosen
                  "serverInfo" (%make-ht "name" "cl-mcp" "version" (version))
                  "capabilities" caps)))))

(defun handle-notification (state method params)
  (declare (ignore state params))
  (when (string= method "notifications/initialized")
    (return-from handle-notification nil))
  nil)

(defun tools-descriptor-repl ()
  (%make-ht
   "name" "repl-eval"
   "description"
   "Evaluate Common Lisp forms and return the last value as printed text.
Use this for testing, inspection, debugging, or loading systems (ql:quickload).
Provide an existing package (e.g., CL-USER) and set printLevel/printLength when needed.
WARNING: Definitions created here are TRANSIENT and lost on server restart.
To modify code permanently, you MUST use 'lisp-edit-form' or 'fs-write-file' to save changes to files."
   "inputSchema"
   (%make-ht
    "type" "object"
    "properties"
    (let ((p (make-hash-table :test #'equal)))
      (setf (gethash "code" p)
            (%make-ht "type" "string"
                      "description"
                      "Code string of one or more forms evaluated sequentially"))
      (setf (gethash "package" p)
            (%make-ht "type" "string"
                      "description"
                      "Existing package name (e.g., CL-USER); forms are read/evaluated
there"))
      (setf (gethash "printLevel" p)
            (%make-ht "type" "integer"
                      "description"
                      "Integer to limit printed nesting depth (omit to print fully)"))
      (setf (gethash "printLength" p)
            (%make-ht "type" "integer"
                      "description"
                      "Integer to limit printed list length (omit to print fully)"))
      (setf (gethash "timeoutSeconds" p)
            (%make-ht "type" "number"
                      "description"
                      "Seconds to wait before timing out evaluation"))
      (setf (gethash "maxOutputLength" p)
            (%make-ht "type" "integer"
                      "description"
                      "Maximum characters for printed result/stdout/stderr"))
      (setf (gethash "safeRead" p)
            (%make-ht "type" "boolean"
                      "description"
                      "When true, disables #. reader evaluation for safety"))
      p))))



;; Temporarily disabled: asdf-system-info
#+nil
(defun tools-descriptor-asdf-system-info ()
  (%make-ht
   "name" "asdf-system-info"
   "description"
   "Get detailed information about an ASDF system including dependencies, version, and source location."
   "inputSchema"
   (%make-ht
    "type" "object"
    "properties"
    (let ((p (make-hash-table :test #'equal)))
      (setf (gethash "system_name" p)
            (%make-ht
             "type" "string"
             "description"
             "Name of the ASDF system (e.g., \"cl-mcp\", \"alexandria\")"))
      p)
    "required" (vector "system_name"))))



;; Temporarily disabled: asdf-list-systems
#+nil
(defun tools-descriptor-asdf-list-systems ()
  (%make-ht
   "name" "asdf-list-systems"
   "description" "List all registered ASDF systems."
   "inputSchema"
   (%make-ht
    "type" "object"
    "properties" (make-hash-table :test #'equal)
    "required" (vector))))
(defun tools-descriptor-fs-read ()
  (%make-ht
   "name" "fs-read-file"
   "description"
   "Read a text file with optional offset and limit.
Prefer absolute paths inside the project; offset/limit are character counts
to avoid loading whole files.
It can only open files in the project or in loaded dependent libraries."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Absolute path inside the project or a registered
ASDF system"))
                   (setf (gethash "offset" p)
                         (%make-ht "type" "integer"
                                   "description"
                                   "0-based character offset to start reading"))
                   (setf (gethash "limit" p)
                         (%make-ht "type" "integer"
                                   "description"
                                   "Maximum characters to return; omit to read to end"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path")))))

(defun tools-descriptor-fs-write ()
  (%make-ht
   "name" "fs-write-file"
   "description" "Write text content to a file relative to project root.
Parent directories are automatically created if they do not exist.
Use this for creating NEW files or editing non-Lisp files (e.g., markdown, config files).
For editing EXISTING Lisp source code, you MUST use 'lisp-edit-form' instead to preserve structure and comments."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Relative path under the project root; absolute paths are
rejected"))
                   (setf (gethash "content" p)
                         (%make-ht "type" "string"
                                   "description" "Text content to write"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path" "content")))))

(defun tools-descriptor-fs-list ()
  (%make-ht
   "name" "fs-list-directory"
   "description"
   "List entries in a directory, filtering hidden and build artifacts.
Use absolute paths inside the project or an ASDF system."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Absolute directory path under the project root or a registered
ASDF system"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path")))))

(defun tools-descriptor-fs-project-info ()
  (%make-ht
   "name" "fs-get-project-info"
   "description"
   "Get project root and current working directory information for path resolution context."
   "inputSchema" (%make-ht "type" "object"
                           "properties" (make-hash-table :test #'equal)
                           "required" (vector))))

(defun tools-descriptor-fs-set-project-root ()
  (%make-ht
   "name" "fs-set-project-root"
   "description"
   "Set the server's project root directory to the specified path.
Use this to synchronize the server's working directory with the client's project location.
The server will change its current working directory to the specified path.
RESTRICTION: You MUST only provide your current working directory (e.g. obtained via pwd). Do not use arbitrary paths."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Absolute path to the project root directory"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path")))))
(defun tools-descriptor-lisp-read-file ()
  (%make-ht
   "name" "lisp-read-file"
   "description"
   "Read a file with Lisp-aware collapsed view to save context window tokens.
ALWAYS prefer this tool over 'fs-read-file' when reading .lisp or .asd files, unless you need exact raw bytes.
Use 'name_pattern' to locate specific definitions (e.g., functions, classes) without reading the entire file.
Use 'collapsed=true' (default) to see only signatures, or 'collapsed=false' for full source."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Path to read; absolute inside project or registered ASDF system, or relative to project root"))
                   (setf (gethash "collapsed" p)
                         (%make-ht "type" "boolean"
                                   "description"
                                   "When true (default) collapse Lisp definitions to signatures"))
                   (setf (gethash "name_pattern" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Regex to match definition names to expand (CL-PPCRE syntax)"))
                   (setf (gethash "content_pattern" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Regex to match form bodies or text lines to expand"))
                   (setf (gethash "offset" p)
                         (%make-ht "type" "integer"
                                   "description"
                                   "0-based line offset when collapsed=false (raw mode only)"))
                   (setf (gethash "limit" p)
                         (%make-ht "type" "integer"
                                   "description"
                                   "Maximum lines to return; defaults to 2000"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path")))))

(defun tools-descriptor-code-find ()
  (%make-ht
   "name" "code-find"
   "description"
   "Locate the definition of a symbol (path and line) using sb-introspect.
Quickload/load the library first; prefer package-qualified symbols or supply the package argument.
NOTE: If the symbol is not found, the system might not be loaded yet.
Try using 'lisp-read-file' with 'name_pattern' to search the file system directly as a fallback."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "symbol" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Symbol name like \"cl:mapcar\" (package-qualified preferred)"))
                   (setf (gethash "package" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Optional package used when SYMBOL is unqualified; ensure the package exists
and is loaded"))
                   (%make-ht "type" "object"
                             "properties" p
                             "required" (vector "symbol")))))

(defun tools-descriptor-code-describe ()
  (%make-ht
   "name" "code-describe"
   "description"
   "Describe a symbol: type, arglist, and documentation.
Ensure the defining library is loaded; pass a package or a package-qualified symbol to avoid resolution errors.
NOTE: If the symbol is not found, the system might not be loaded yet.
Try using 'lisp-read-file' with 'name_pattern' to search the file system directly as a fallback."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "symbol" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Symbol name like \"cl:mapcar\" (package-qualified preferred)"))
                   (setf (gethash "package" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Optional package used when SYMBOL is unqualified; ensure the package exists
and is loaded"))
                   (%make-ht "type" "object"
                             "properties" p
                             "required" (vector "symbol")))))

(defun tools-descriptor-code-references ()
  (%make-ht
   "name" "code-find-references"
   "description"
   "Find where a symbol is referenced using SBCL xref (calls, macroexpands, binds, references, sets).
Use package-qualified symbols when possible; set projectOnly=false to include external libs."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "symbol" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Symbol name like \"cl-mcp:run\" (package-qualified preferred)"))
                   (setf (gethash "package" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Optional package used when SYMBOL is unqualified"))
                   (setf (gethash "projectOnly" p)
                         (%make-ht "type" "boolean"
                                   "description"
                                   "When true (default), only include references under the project root"))
                   (%make-ht "type" "object"
                             "properties" p
                             "required" (vector "symbol")))))

(defun tools-descriptor-lisp-check-parens ()
  (%make-ht
   "name" "lisp-check-parens"
   "description"
   "Check balanced parentheses/brackets in a file slice or provided code.
Use this to DIAGNOSE syntax errors in existing files or validate code snippets before/after editing.
Returns the first mismatch position if unbalanced, or success if balanced."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Absolute path inside project or registered ASDF system (mutually exclusive with code)"))
                   (setf (gethash "code" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Raw code string to check (mutually exclusive with path)"))
                   (setf (gethash "offset" p)
                         (%make-ht "type" "integer"
                                   "description"
                                   "0-based character offset when reading from path"))
                   (setf (gethash "limit" p)
                         (%make-ht "type" "integer"
                                   "description"
                                   "Maximum characters to read from path"))
                   (%make-ht "type" "object" "properties" p))))

(defun tools-descriptor-lisp-edit-form ()
  (%make-ht
   "name" "lisp-edit-form"
   "description"
   "Structure-aware edit of a top-level Lisp form using Eclector CST parsing.
Supports replace, insert_before, and insert_after while preserving formatting and comments.
PREFERRED METHOD for editing existing Lisp source code - automatically repairs missing closing parentheses using parinfer.
ALWAYS use this tool instead of 'fs-write-file' when modifying Lisp forms to ensure safety and structure preservation."
   "inputSchema"
   (let ((p (make-hash-table :test #'equal)))
     (setf (gethash "file_path" p)
           (%make-ht "type" "string"
                     "description"
                     "Target file path (absolute recommended)"))
     (setf (gethash "form_type" p)
           (%make-ht "type" "string"
                     "description"
                     "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\""))
     (setf (gethash "form_name" p)
           (%make-ht "type" "string"
                     "description"
                     "Form name to match; for defmethod include specializers, e.g., \"print-object (my-class t)\""))
     (setf (gethash "operation" p)
           (%make-ht "type" "string"
                     "enum" (vector "replace" "insert_before" "insert_after")
                     "description"
                     "Operation to perform"))
     (setf (gethash "content" p)
           (%make-ht "type" "string"
                     "description"
                     "Full Lisp form to insert or replace with"))
     (setf (gethash "dry_run" p)
           (%make-ht "type" "boolean"
                     "description"
                     "When true, return a preview without writing to disk"))
     (%make-ht "type" "object"
               "properties" p
               "required" (vector "file_path" "form_type" "form_name" "operation" "content")))))

(defun handle-tools-list (id)
  (let* ((tools
          (vector (tools-descriptor-repl)
                  #+nil(tools-descriptor-asdf-system-info)
                  #+nil(tools-descriptor-asdf-list-systems)
                  (tools-descriptor-fs-read)
                  (tools-descriptor-fs-write)
                  (tools-descriptor-fs-list)
                  (tools-descriptor-fs-project-info)
                  (tools-descriptor-fs-set-project-root)
                  (tools-descriptor-lisp-read-file)
                  (tools-descriptor-code-find)
                  (tools-descriptor-code-describe)
                  (tools-descriptor-code-references)
                  (tools-descriptor-lisp-check-parens)
                  (tools-descriptor-lisp-edit-form))))
    (%result id (%make-ht "tools" tools))))

(defun %normalize-tool-name (name)
  "Normalize a tool NAME possibly namespaced like 'ns.tool' or 'ns/tool'.
Returns a downcased local tool name (string)."
  (let* ((s (string-downcase name))
         (dot (position #\. s :from-end t))
         (sl (position #\/ s :from-end t))
         (idx (max (or dot -1) (or sl -1))))
    (subseq s (1+ idx))))

(defun handle-asdf-tools-call (id params)
  ;; TODO: Temporarily disabled due to unresolved "Transport closed" errors.
  ;;
  ;; Issue:
  ;; Executing this tool causes the MCP client to disconnect with "Transport closed"
  ;; or timeout, even though the server logs show immediate and successful JSON response
  ;; transmission.
  ;;
  ;; Investigation Status:
  ;; 1. ASDF Output: Stdout noise is already suppressed via `with-silenced-output`.
  ;; 2. TCP Timeout: Keep-alive logic is implemented; server does not close the socket.
  ;; 3. Server Logs: Show `rpc.result` and `tcp.flushed` occurring immediately.
  "Handle ASDF-related tool calls.
Returns a JSON-RPC response hash-table when handled, or NIL to defer."
  (declare (ignore id params))
  nil)

#+nil
(defun handle-asdf-tools-call (id params)
  "Handle ASDF-related tool calls.
Returns a JSON-RPC response hash-table when handled, or NIL to defer."
  (let* ((name (and params (gethash "name" params)))
         (args (and params (gethash "arguments" params)))
         (local (and name (%normalize-tool-name name))))
    (cond
      ((member local '("asdf-system-info" "asdf_system_info" "system-info")
               :test #'string=)
       (handler-case
           (let* ((system-name (and args (gethash "system_name" args))))
             (unless system-name
               (return-from handle-asdf-tools-call
                 (%error id -32602 "Missing required parameter: system_name")))
             (%result id (asdf-system-info system-name)))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during asdf-system-info: ~A" e)))))

      ((member local '("asdf-list-systems" "asdf_list_systems" "list-systems")
               :test #'string=)
       (handler-case
           (%result id (asdf-list-systems))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during asdf-list-systems: ~A" e)))))

      (t nil))))
(defun handle-tools-call (id params)
  (let* ((name (and params (gethash "name" params)))
         (args (and params (gethash "arguments" params)))
         (local (and name (%normalize-tool-name name))))
    (when name (log-event :debug "tools.call" "name" name "local" local))
    (cond
      ((member local '("repl-eval" "repl_eval") :test #'string=)
       (let* ((code (and args (gethash "code" args)))
              (pkg  (and args (gethash "package" args)))
              (pl   (and args (gethash "printLevel" args)))
              (plen (and args (gethash "printLength" args)))
              (timeout (and args (gethash "timeoutSeconds" args)))
              (max-out (and args (gethash "maxOutputLength" args)))
              (safe-read (and args (gethash "safeRead" args))))
         (handler-case
             (multiple-value-bind (printed _ stdout stderr)
                 (repl-eval (or code "")
                            :package (or pkg *package*)
                            :print-level pl
                            :print-length plen
                            :timeout-seconds timeout
                            :max-output-length max-out
                            :safe-read safe-read)
               (declare (ignore _))
               (%result id (%make-ht
                            "content" (%text-content printed)
                            "stdout" stdout
                            "stderr" stderr)))
           (error (e)
             (cons "Internal error during REPL evaluation" e)
             (%error id -32603
                     (format nil "Internal error during REPL evaluation: ~A" e))))))

      ((member local '("lisp-read-file" "lisp_read_file") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args)))
                  (collapsed-present nil)
                  (collapsed
                    (multiple-value-bind (val presentp)
                        (and args (gethash "collapsed" args))
                      (setf collapsed-present presentp)
                      (if presentp val t)))
                  (name-pattern (and args (gethash "name_pattern" args)))
                  (content-pattern (and args (gethash "content_pattern" args)))
                  (offset (and args (gethash "offset" args)))
                  (limit (and args (gethash "limit" args))))
             (unless (stringp path)
               (return-from handle-tools-call
                 (%error id -32602 "path must be a string")))
             (when (and collapsed-present (not (member collapsed '(t nil))))
               (return-from handle-tools-call
                 (%error id -32602 "collapsed must be boolean")))
             (let ((result (lisp-read-file path
                                           :collapsed collapsed
                                           :name-pattern name-pattern
                                           :content-pattern content-pattern
                                           :offset offset
                                           :limit limit)))
               (%result id (%make-ht
                            "content" (%text-content (gethash "content" result))
                            "text" (gethash "content" result)
                            "path" (gethash "path" result)
                            "mode" (gethash "mode" result)
                           "meta" (gethash "meta" result)))))
        (error (e)
          (%error id -32603
                  (format nil "Internal error during lisp-read-file: ~A" e)))))

      ((member local '("fs-get-project-info" "fs_get_project_info") :test #'string=)
       (handler-case
           (let* ((info (fs-get-project-info))
                  (summary (format nil "Project root: ~A~%CWD: ~A~%Source: ~A"
                                   (gethash "project_root" info)
                                   (or (gethash "cwd" info) "(none)")
                                   (gethash "project_root_source" info))))
             (%result id (%make-ht
                          "content" (%text-content summary)
                          "project_root" (gethash "project_root" info)
                          "cwd" (gethash "cwd" info)
                          "project_root_source" (gethash "project_root_source" info)
                          "relative_cwd" (gethash "relative_cwd" info))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during fs-get-project-info: ~A" e)))))

      ((member local '("fs-set-project-root" "fs_set_project_root") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args))))
             (unless (stringp path)
               (return-from handle-tools-call
                 (%error id -32602 "path must be a string")))
             (let* ((info (fs-set-project-root path))
                    (summary (format nil "~A~%New project root: ~A~%New CWD: ~A"
                                     (gethash "status" info)
                                     (gethash "project_root" info)
                                     (gethash "cwd" info))))
               (%result id (%make-ht
                            "content" (%text-content summary)
                            "project_root" (gethash "project_root" info)
                            "cwd" (gethash "cwd" info)
                            "previous_root" (gethash "previous_root" info)
                            "status" (gethash "status" info)))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during fs-set-project-root: ~A" e)))))

      ((member local '("fs-read-file" "fs_read_file") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args)))
                  (offset (and args (gethash "offset" args)))
                  (limit (and args (gethash "limit" args))))
             (unless (stringp path)
               (return-from handle-tools-call
                 (%error id -32602 "path must be a string")))
             (let ((content-string (fs-read-file path :offset offset :limit limit)))
               (%result id (%make-ht
                            "content" (%text-content content-string)
                            "text" content-string
                            "path" path
                            "offset" offset
                            "limit" limit))))
         (error (e)
           (%error id -32603 (format nil "Internal error during fs-read-file: ~A" e)))))

      ((member local '("fs-write-file" "fs_write_file") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args)))
                  (content (and args (gethash "content" args))))
             (unless (and (stringp path) (stringp content))
               (return-from handle-tools-call
                 (%error id -32602 "path and content must be strings")))
             (fs-write-file path content)
             (%result id (%make-ht
                          "success" t
                          "content" (%text-content
                                     (format nil "Wrote ~A (~D chars)" path (length content)))
                          "path" path
                          "bytes" (length content))))
         (error (e)
           (%error id -32603 (format nil "Internal error during fs_write_file: ~A" e)))))

      ((member local '("fs-list-directory" "fs_list_directory") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args))))
             (unless (stringp path)
               (return-from handle-tools-call
                 (%error id -32602 "path must be a string")))
             (let* ((entries (fs-list-directory path))
                    (summary-lines (map 'list
                                        (lambda (h)
                                          (format nil "~A (~A)"
                                                  (gethash "name" h)
                                                  (gethash "type" h)))
                                        entries))
                    (summary (if summary-lines
                                 (format nil "~{~A~%~}" summary-lines)
                                 ""))
                    (content (%text-content summary)))
               (%result id (%make-ht
                            "entries" entries
                            "count" (length entries)
                            "content" content
                            "path" path))))
         (error (e)
           (%error id -32603 (format nil "Internal error during fs-list-directory: ~A" e)))))

      ((member local '("code-find" "code_find") :test #'string=)
       (handler-case
           (let* ((symbol (and args (gethash "symbol" args)))
                  (pkg (and args (gethash "package" args))))
             (unless (stringp symbol)
               (return-from handle-tools-call
                 (%error id -32602 "symbol must be a string")))
             (multiple-value-bind (path line)
                 (code-find-definition symbol :package pkg)
               (if path
                   (%result id (%make-ht
                                "path" path
                                "line" line
                                "content" (%text-content
                                           (format nil "~A defined in ~A at line ~D"
                                                   symbol path line))))
                   (%error id -32004 (format nil "Definition not found for ~A" symbol)))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during code-find: ~A" e)))))

      ((member local '("code-describe" "code_describe") :test #'string=)
       (handler-case
           (let* ((symbol (and args (gethash "symbol" args)))
                  (pkg (and args (gethash "package" args))))
             (unless (stringp symbol)
               (return-from handle-tools-call
                 (%error id -32602 "symbol must be a string")))
             (multiple-value-bind (name type arglist doc path line)
                 (code-describe-symbol symbol :package pkg)
               (%result id (%make-ht
                            "name" name
                            "type" type
                            "arglist" arglist
                            "documentation" doc
                            "path" path
                            "line" line
                            "content" (%text-content
                                       (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]~@[~%Defined at ~A:~D~]"
                                               name type arglist doc path line))))))
        (error (e)
          (%error id -32603
                  (format nil "Internal error during code-describe: ~A" e)))))

      ((member local '("code-find-references" "code_find_references") :test #'string=)
       (handler-case
           (let* ((symbol (and args (gethash "symbol" args)))
                  (pkg (and args (gethash "package" args)))
                  (project-only-present nil)
                  (project-only
                    (multiple-value-bind (val presentp)
                        (and args (gethash "projectOnly" args))
                      (setf project-only-present presentp)
                      (if presentp val t))))
             (unless (stringp symbol)
               (return-from handle-tools-call
                 (%error id -32602 "symbol must be a string")))
             (when (and project-only-present (not (member project-only '(t nil))))
               (return-from handle-tools-call
                 (%error id -32602 "projectOnly must be boolean")))
             (multiple-value-bind (refs count)
                 (code-find-references symbol :package pkg :project-only project-only)
               (let* ((summary-lines (map 'list
                                          (lambda (h)
                                            (format nil "~A:~D ~A ~A"
                                                    (gethash "path" h)
                                                    (gethash "line" h)
                                                    (gethash "type" h)
                                                    (gethash "context" h)))
                                          refs))
                      (summary (if summary-lines
                                   (format nil "~{~A~%~}" summary-lines)
                                   "")))
                 (%result id (%make-ht
                              "refs" refs
                              "count" count
                              "symbol" symbol
                              "projectOnly" project-only
                              "content" (%text-content summary))))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during code-find-references: ~A" e)))))

      ((member local '("lisp-check-parens" "lisp_check_parens") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args)))
                  (code (and args (gethash "code" args)))
                  (offset (and args (gethash "offset" args)))
                  (limit (and args (gethash "limit" args))))
             (when (and path code)
               (return-from handle-tools-call
                 (%error id -32602 "Provide only one of path or code")))
             (when (and (null path) (null code))
               (return-from handle-tools-call
                 (%error id -32602 "Either path or code is required")))
             (let* ((result (lisp-check-parens :path path :code code :offset offset :limit limit))
                    (ok (gethash "ok" result))
                    (summary (if ok
                                 "Parentheses are balanced"
                                 (let* ((kind (gethash "kind" result))
                                        (expected (gethash "expected" result))
                                        (found (gethash "found" result))
                                        (pos (gethash "position" result))
                                        (line (and pos (gethash "line" pos)))
                                        (col (and pos (gethash "column" pos))))
                                   (format nil "Unbalanced parentheses: ~A~@[ (expected ~A, found ~A)~] at line ~D, column ~D"
                                           kind expected found line col)))))
               (%result id (%make-ht
                            "content" (%text-content summary)
                            "ok" ok
                            "kind" (gethash "kind" result)
                            "expected" (gethash "expected" result)
                            "found" (gethash "found" result)
                            "position" (gethash "position" result)))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during lisp-check-parens: ~A" e)))))
 
      ((member local '("lisp-edit-form" "lisp_edit_form") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "file_path" args)))
                  (form-type (and args (gethash "form_type" args)))
                  (form-name (and args (gethash "form_name" args)))
                  (operation (and args (gethash "operation" args)))
                  (content (and args (gethash "content" args)))
                  (dry-run-present nil)
                  (dry-run (multiple-value-bind (val presentp)
                                  (and args (gethash "dry_run" args))
                                (setf dry-run-present presentp)
                                (if presentp val nil))))
             (unless (and (stringp path) (stringp form-type) (stringp form-name)
                          (stringp operation) (stringp content))
               (return-from handle-tools-call
                 (%error id -32602 "file_path, form_type, form_name, operation, and content must be strings")))
             (when (and dry-run-present (not (member dry-run '(t nil))))
               (return-from handle-tools-call
                 (%error id -32602 "dry_run must be boolean")))
             (let ((updated (lisp-edit-form :file-path path
                                            :form-type form-type
                                            :form-name form-name
                                            :operation operation
                                            :content content
                                            :dry-run dry-run)))
               (if dry-run
                   (let* ((preview (gethash "preview" updated))
                          (would-change (gethash "would_change" updated))
                          (original-form (gethash "original" updated))
                          (summary (format nil "Dry-run ~A on ~A ~A (~:[no change~;would change~])"
                                           operation form-type path would-change)))
                     (%result id (%make-ht
                                  "path" path
                                  "operation" operation
                                  "form_type" form-type
                                  "form_name" form-name
                                  "would_change" would-change
                                  "original" original-form
                                  "preview" preview
                                  "content" (%text-content summary))))
                   (%result id (%make-ht
                                "path" path
                                "operation" operation
                                "form_type" form-type
                                "form_name" form-name
                                "bytes" (length updated)
                                "content" (%text-content
                                           (format nil "Applied ~A to ~A ~A (~D chars)"
                                                   operation form-type path (length updated))))))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during lisp-edit-form: ~A" e)))))

      (t
       (%error id -32601 (format nil "Tool ~A not found" name))))))

(defun handle-request (state id method params)
  (cond
    ((string= method "initialize") (handle-initialize state id params))
    ((string= method "tools/list") (handle-tools-list id))
    ((string= method "tools/call")
     (or (handle-asdf-tools-call id params)
         (handle-tools-call id params)))
    ((string= method "ping") (%result id (%make-ht)))
    (t (%error id -32601 (format nil "Method ~A not found" method)))))

(defun process-json-line (line &optional (state (make-state)))
  "Process one JSON-RPC line and return a JSON line to send, or NIL for notifications."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (when (string= trimmed "")
      (log-event :debug "rpc.skip-empty")
      (return-from process-json-line nil))
    (let* ((msg (handler-case
                    (%decode-json trimmed)
                  (error (e)
                    (log-event :warn "rpc.parse-error" "line" trimmed "error" (princ-to-string e))
                    (return-from process-json-line
                      (%encode-json (%error nil -32700 "Parse error")))))))
      (unless (hash-table-p msg)
        (log-event :warn "rpc.invalid" "reason" "message not object")
        (return-from process-json-line
          (%encode-json (%error nil -32600 "Invalid Request"))))
      (let* ((jsonrpc (gethash "jsonrpc" msg))
             (id (gethash "id" msg))
             (method (gethash "method" msg))
             (params (gethash "params" msg)))
        (log-event :debug "rpc.dispatch" "id" id "method" method)
        (unless (and (stringp jsonrpc) (string= jsonrpc "2.0"))
          (let ((resp (%encode-json (%error id -32600 "Invalid Request"))))
            (log-event :warn "rpc.invalid" "reason" "bad jsonrpc version")
            (return-from process-json-line resp)))
        (handler-case
            (if method
                (if id
                    (let ((r (handle-request state id method params)))
                      (log-event :debug "rpc.result" "id" id "method" method)
                      (%encode-json r))
                    ;; notification
                    (progn
                      (handle-notification state method params)
                      (log-event :debug "rpc.notify" "method" method)
                      nil))
                (let ((resp (%encode-json (%error id -32600 "Invalid Request"))))
                  (log-event :warn "rpc.invalid" "reason" "missing method")
                  resp))
          (error (e)
            (log-event :error "rpc.internal" "id" id "method" method "error" (princ-to-string e))
            (%encode-json (%error id -32603 "Internal error"))))))))
