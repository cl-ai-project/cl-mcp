;;;; src/protocol.lisp

(defpackage #:cl-mcp/src/protocol
  (:use #:cl)
  (:import-from #:cl-mcp/src/core #:version)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/repl #:repl-eval)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file #:fs-write-file #:fs-list-directory)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:shadowing-import-from #:cl-mcp/src/code
                          #:code-find-references)
  (:import-from #:cl-mcp/src/code
                #:code-find-definition #:code-describe-symbol)
  (:import-from #:cl-mcp/src/validate
                #:check-parens)
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
Provide an existing package (e.g., CL-USER) and set printLevel/printLength
when you need to control truncation. Captures stdout/stderr in the response."
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
   "description" "Write text content to a file relative to project root."
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

(defun tools-descriptor-lisp-read-file ()
  (%make-ht
   "name" "lisp-read-file"
   "description"
   "Read a file with Lisp-aware collapsed view, optionally expanding forms matching patterns."
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
Quickload/load the library first; prefer package-qualified symbols or supply the
package argument."
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
Ensure the defining library is loaded; pass a package or a package-qualified
symbol to avoid resolution errors."
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

(defun tools-descriptor-check-parens ()
  (%make-ht
   "name" "check-parens"
   "description"
   "Check balanced parentheses/brackets in a file slice or provided code; returns the first mismatch position."
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

(defun handle-tools-list (id)
  (let* ((tools (vector (tools-descriptor-repl)
                        (tools-descriptor-fs-read)
                        (tools-descriptor-fs-write)
                        (tools-descriptor-fs-list)
                        (tools-descriptor-lisp-read-file)
                        (tools-descriptor-code-find)
                        (tools-descriptor-code-describe)
                        (tools-descriptor-code-references)
                        (tools-descriptor-check-parens))))
    (%result id (%make-ht "tools" tools))))

(defun %normalize-tool-name (name)
  "Normalize a tool NAME possibly namespaced like 'ns.tool' or 'ns/tool'.
Returns a downcased local tool name (string)."
  (let* ((s (string-downcase name))
         (dot (position #\. s :from-end t))
         (sl (position #\/ s :from-end t))
         (idx (max (or dot -1) (or sl -1))))
    (subseq s (1+ idx))))

(defun handle-tools-call (id params)
  (let* ((name (and params (gethash "name" params)))
         (args (and params (gethash "arguments" params)))
         (local (and name (%normalize-tool-name name))))
    (when name (log-event :debug "tools.call" "name" name "local" local))
    (cond
      ((member local '("repl-eval" "repl.eval" "repl_eval") :test #'string=)
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

      ((member local '("lisp-read-file" "lisp_read_file" "lisp.read_file" "lisp-read")
               :test #'string=)
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

      ((member local '("fs-read-file" "fs_read_file" "read_file" "read") :test #'string=)
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

      ((member local '("fs-write-file" "fs_write_file" "write_file" "write") :test #'string=)
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

      ((member local '("fs-list-directory" "fs_list_directory" "list_directory" "list" "ls")
               :test #'string=)
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

      ((member local '("code-find" "code_find" "find" "find_definition") :test #'string=)
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

      ((member local '("code-describe" "code_describe" "describe" "describe_symbol")
               :test #'string=)
       (handler-case
           (let* ((symbol (and args (gethash "symbol" args)))
                  (pkg (and args (gethash "package" args))))
             (unless (stringp symbol)
               (return-from handle-tools-call
                 (%error id -32602 "symbol must be a string")))
             (multiple-value-bind (name type arglist doc)
                 (code-describe-symbol symbol :package pkg)
               (%result id (%make-ht
                            "name" name
                            "type" type
                            "arglist" arglist
                            "documentation" doc
                            "content" (%text-content
                                       (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]"
                                               name type arglist doc))))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during code-describe: ~A" e)))))

      ((member local '("code-find-references" "code_find_references" "code-references"
                       "code_references" "references")
               :test #'string=)
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

      ((member local '("check-parens" "check_parens" "parens") :test #'string=)
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
             (let ((result (check-parens :path path :code code :offset offset :limit limit)))
               (%result id result)))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during check-parens: ~A" e)))))

      (t
       (%error id -32601 (format nil "Tool ~A not found" name))))))

(defun handle-request (state id method params)
  (cond
    ((string= method "initialize") (handle-initialize state id params))
    ((string= method "tools/list") (handle-tools-list id))
    ((string= method "tools/call") (handle-tools-call id params))
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
