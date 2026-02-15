;;;; src/protocol.lisp

(defpackage #:cl-mcp/src/protocol
  (:use #:cl)
  (:import-from #:cl-mcp/src/core
                #:version)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/state
                #:server-state
                #:initialized-p
                #:client-info
                #:protocol-version
                #:make-state)
  (:shadowing-import-from #:cl-mcp/src/prompts
                          #:prompts-directory
                          #:discover-prompts
                          #:find-prompt-by-name
                          #:read-prompt-file)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-all-tool-descriptors
                #:get-tool-handler)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:result
                #:rpc-error)
  ;; Import tools/all to trigger loading of all tool modules.
  ;; Tool modules register themselves with the registry at load time.
  (:import-from #:cl-mcp/src/tools/all)
  (:import-from #:yason
                #:encode
                #:parse)
  (:export
   #:+protocol-version+
   #:+supported-protocol-versions+
   #:server-state
   #:initialized-p
   #:client-info
   #:protocol-version
   #:make-state
   #:process-json-line))

(in-package #:cl-mcp/src/protocol)

(defparameter +protocol-version+ "2025-11-25")
(defparameter +supported-protocol-versions+
  '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05")
  "Supported MCP protocol versions, ordered by preference.")

(defun %decode-json (line)
  (yason:parse line))

(defun %encode-json (obj)
  (with-output-to-string (stream)
    (yason:encode obj stream)))

(defun handle-initialize (state id params)
  ;; Sync rootPath from client if provided
  (when params
    (let* ((root-path (gethash "rootPath" params))
           (root-uri (gethash "rootUri" params))
           (root
             (or root-path
                 (and root-uri
                      (if (uiop/utility:string-prefix-p "file://" root-uri)
                          (subseq root-uri 7)
                          root-uri)))))
      (when (and root (stringp root) (plusp (length root)))
        (handler-case
            (let ((root-dir (uiop/pathname:ensure-directory-pathname root)))
              (when (uiop/filesystem:directory-exists-p root-dir)
                (setf cl-mcp/src/project-root:*project-root* root-dir)
                (uiop/os:chdir root-dir)
                (log-event :info "initialize.sync-root"
                           "rootPath" (namestring root-dir)
                           "source" (if root-path "rootPath" "rootUri"))))
          (error (e)
            (log-event :warn "initialize.sync-root-failed"
                       "path" root
                       "error" (princ-to-string e)))))))
  (let* ((client-ver (and params (gethash "protocolVersion" params)))
         (supported
           (and client-ver
                (find client-ver +supported-protocol-versions+ :test #'string=)))
         (chosen
           (cond (supported supported)
                 ((null client-ver) (first +supported-protocol-versions+))
                 (t nil)))
         (caps (make-ht "tools" (make-ht "listChanged" t)
                        "prompts" (make-ht "listChanged" yason:false))))
    (if (null chosen)
        (rpc-error id -32602
                   (format nil "Unsupported protocolVersion ~A" client-ver)
                   (make-ht "supportedVersions" +supported-protocol-versions+))
        (progn
          (setf (protocol-version state) chosen)
          (result id
                  (make-ht "protocolVersion" chosen
                           "serverInfo" (make-ht "name" "cl-mcp"
                                                  "version" (version))
                           "capabilities" caps))))))

(defun handle-notification (state method params)
  (declare (ignore state params))
  (when (string= method "notifications/initialized")
    (return-from handle-notification nil))
  nil)

(defun handle-tools-list (id)
  "Return the list of available tools from the registry."
  (result id (make-ht "tools" (get-all-tool-descriptors))))

(defun %prompts-directory ()
  "Compatibility wrapper for tests; delegates to cl-mcp/src/prompts."
  (prompts-directory))

(defun handle-prompts-list (id)
  "Return available prompts from prompts/*.md."
  (handler-case
      (let* ((prompts (discover-prompts))
             (items
               (coerce
                (mapcar
                 (lambda (prompt)
                   (make-ht "name" (gethash "name" prompt)
                            "title" (gethash "title" prompt)
                            "description" (gethash "description" prompt)))
                 prompts)
                'vector)))
        (result id (make-ht "prompts" items)))
    (error (e)
      (rpc-error id -32603 (format nil "Failed to list prompts: ~A" e)))))

(defun handle-prompts-get (id params)
  "Return prompt content as MCP prompt messages for NAME."
  (let ((name (and params (gethash "name" params))))
    (unless (and (stringp name) (plusp (length name)))
      (return-from handle-prompts-get
        (rpc-error id -32602 "name is required")))
    (let ((prompt (find-prompt-by-name name)))
      (unless prompt
        (return-from handle-prompts-get
          (rpc-error id -32602 (format nil "Prompt ~A not found" name))))
      (handler-case
          (let* ((text (read-prompt-file (gethash "file_path" prompt)))
                 (messages (vector
                            (make-ht "role" "user"
                                     "content" (make-ht "type" "text"
                                                        "text" text)))))
            (result id (make-ht "description" (gethash "title" prompt)
                                "messages" messages)))
        (error (e)
          (rpc-error id -32603
                     (format nil "Failed to read prompt ~A: ~A" name e)))))))

(defun %normalize-tool-name (name)
  "Normalize a tool NAME possibly namespaced like 'ns.tool' or 'ns/tool'.
Returns a downcased local tool name (string)."
  (let* ((s (string-downcase name))
         (dot (position #\. s :from-end t))
         (sl (position #\/ s :from-end t))
         (idx (max (or dot -1) (or sl -1))))
    (subseq s (1+ idx))))

(defun %normalize-tool-alias (name)
  (let ((local (%normalize-tool-name name)))
    (substitute #\- #\_ local)))

(defun handle-tools-call (state id params)
  "Dispatch a tool call to the appropriate handler from the registry."
  (let* ((name (and params (gethash "name" params)))
         (args (and params (gethash "arguments" params)))
         (local (and name (%normalize-tool-alias name))))
    (when name
      (log-event :debug "tools.call" "name" name "local" local))
    (let ((handler (and local (get-tool-handler local))))
      (if handler
          (funcall handler state id args)
          (rpc-error id -32601 (format nil "Tool ~A not found" name))))))

(defun handle-request (state id method params)
  (cond
    ((string= method "initialize") (handle-initialize state id params))
    ((string= method "tools/list") (handle-tools-list id))
    ((string= method "tools/call") (handle-tools-call state id params))
    ((string= method "prompts/list") (handle-prompts-list id))
    ((string= method "prompts/get") (handle-prompts-get id params))
    ((string= method "ping") (result id (make-ht)))
    (t (rpc-error id -32601 (format nil "Method ~A not found" method)))))

(defun process-json-line (line &optional (state (make-state)))
  "Process one JSON-RPC line and return a JSON line to send, or NIL for notifications."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (when (string= trimmed "")
      (log-event :debug "rpc.skip-empty")
      (return-from process-json-line nil))
    (let ((msg (handler-case
                   (%decode-json trimmed)
                 (error (e)
                   (log-event :warn "rpc.parse-error"
                              "line" trimmed
                              "error" (princ-to-string e))
                   (return-from process-json-line
                     (%encode-json (rpc-error nil -32700 "Parse error")))))))
      (unless (hash-table-p msg)
        (log-event :warn "rpc.invalid" "reason" "message not object")
        (return-from process-json-line
          (%encode-json (rpc-error nil -32600 "Invalid Request"))))
      (let ((jsonrpc (gethash "jsonrpc" msg))
            (id (gethash "id" msg))
            (method (gethash "method" msg))
            (params (gethash "params" msg)))
        (log-event :debug "rpc.dispatch" "id" id "method" method)
        (unless (and (stringp jsonrpc) (string= jsonrpc "2.0"))
          (let ((resp (%encode-json (rpc-error id -32600 "Invalid Request"))))
            (log-event :warn "rpc.invalid" "reason" "bad jsonrpc version")
            (return-from process-json-line resp)))
        (handler-case
            (cond
              ((and method id)
               (let ((r (handle-request state id method params)))
                 (log-event :debug "rpc.result" "id" id "method" method)
                 (%encode-json r)))
              (method
               ;; notification
               (handle-notification state method params)
               (log-event :debug "rpc.notify" "method" method)
               nil)
              (t
               (let ((resp (%encode-json (rpc-error id -32600 "Invalid Request"))))
                 (log-event :warn "rpc.invalid" "reason" "missing method")
                 resp)))
          (error (e)
            (log-event :error "rpc.internal"
                       "id" id
                       "method" method
                       "error" (princ-to-string e))
            (%encode-json (rpc-error id -32603 "Internal error"))))))))
