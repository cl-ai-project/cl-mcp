;;;; src/protocol.lisp

(defpackage #:cl-mcp/src/protocol
  (:use #:cl)
  (:import-from #:quri)
  (:import-from #:cl-mcp/src/core
                #:version)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/utils/printing
                #:bounded-princ-to-string)
  (:import-from #:cl-mcp/src/state
                #:server-state
                #:initialized-p
                #:client-info
                #:protocol-version
                #:make-state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-all-tool-descriptors
                #:get-tool-handler
                #:disabled-tool-group)
  (:import-from #:cl-mcp/src/tools/helpers
                #:*wire-tool-arguments*
                #:make-ht
                #:result
                #:rpc-error)
  ;; Import tools/all to trigger loading of all tool modules.
  ;; Tool modules register themselves with the registry at load time.
  (:import-from #:cl-mcp/src/tools/all)
  (:import-from #:cl-mcp/src/tools/repl-session
                #:call-with-repl-session #:repl-tool-descriptor)
  (:import-from #:cl-mcp/src/project-root
                #:set-project-root
                #:with-session-project-root)
  (:import-from #:cl-mcp/src/pool
                #:send-root-to-session-worker)
  (:import-from #:cl-mcp/src/proxy
                #:cancel-request)
  (:import-from #:cl-mcp/src/inline-cancel
                #:cancel-inline-request)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json
                #:transport-safe-json)
  (:import-from #:cl-mcp/src/utils/nesting
                #:json-too-deep-p
                #:+max-json-nesting+)
  (:import-from #:cl-mcp/src/utils/paths
                #:broad-root-p
                #:native-path-namestring)
  (:import-from #:cl-mcp/src/server-instructions
                #:server-instructions)
  (:import-from #:yason
                #:encode
                #:parse)
  (:export
   #:+protocol-version+
   #:+supported-protocol-versions+
   #:*current-session-id*
   #:server-state
   #:initialized-p
   #:client-info
   #:protocol-version
   #:make-state
   #:handle-notification
   #:process-json-message
   #:process-json-line))

(in-package #:cl-mcp/src/protocol)

(defparameter +protocol-version+ "2025-11-25")
(defparameter +supported-protocol-versions+
  '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05")
  "Supported MCP protocol versions, ordered by preference.")

;; *current-session-id*, server-state, initialized-p, client-info,
;; protocol-version, make-state are imported from cl-mcp/src/state

(defun %decode-json (line)
  (yason:parse line))

(defparameter +sanitize-max-elements+ 10000
  "Maximum number of list/vector elements %sanitize-for-encoding will process.
Guards against cyclic or extremely long lists causing OOM.")

(defun %json-literal-p (object)
  "Return true when OBJECT is how a JSON literal is held once parsed with its
types kept: YASON:TRUE, YASON:FALSE or :NULL.

They are symbols, so without this the retry below printed them -- a false a
builder or a worker wrote went out as the string \"FALSE\"."
  (member object '(yason:true yason:false :null)))

(defun %sanitize-for-encoding (obj &optional (depth 0))
  "Recursively sanitize a JSON-compatible structure for encoding.
Walks hash-tables, vectors, lists, and strings. Converts non-serializable
leaf objects to their string representation. Depth-limited to 20 levels.
Collections are capped at +sanitize-max-elements+ to guard against
cyclic or extremely large structures."
  (when (> depth 20)
    (return-from %sanitize-for-encoding
      (typecase obj
        (string (sanitize-for-json obj))
        ((or number (member t nil)) obj)
        ((satisfies %json-literal-p) obj)
        (t (or (ignore-errors (princ-to-string obj)) "#<depth-limit>")))))
  (typecase obj
    (string (sanitize-for-json obj))
    (hash-table
     (let ((clean (make-hash-table :test (hash-table-test obj)))
           (count 0))
       (maphash (lambda (k v)
                  (when (< count +sanitize-max-elements+)
                    (setf (gethash (if (stringp k)
                                       (sanitize-for-json k)
                                       (princ-to-string k))
                                   clean)
                          (%sanitize-for-encoding v (1+ depth)))
                    (incf count)))
                obj)
       clean))
    (vector
     (let* ((limit (min (length obj) +sanitize-max-elements+))
            (result (make-array limit)))
       (loop for i below limit
             do (setf (aref result i)
                      (%sanitize-for-encoding (aref obj i) (1+ depth))))
       result))
    (cons
     ;; Manual CDR-walk: handles dotted pairs and detects circular CDR chains.
     (let ((seen (make-hash-table :test #'eq))
           (result nil)
           (tail obj)
           (count 0))
       (loop
         (when (>= count +sanitize-max-elements+) (return))
         (when (null tail) (return))
         (unless (consp tail)
           ;; Dotted tail — sanitize the atom and stop
           (push (%sanitize-for-encoding tail (1+ depth)) result)
           (return))
         (when (gethash tail seen)
           ;; Circular CDR chain detected — stop
           (return))
         (setf (gethash tail seen) t)
         (push (%sanitize-for-encoding (car tail) (1+ depth)) result)
         (setf tail (cdr tail))
         (incf count))
       (nreverse result)))
    (t (cond
         ((or (numberp obj) (eq obj t) (null obj)) obj)
         ((%json-literal-p obj) obj)
         (t (or (ignore-errors (princ-to-string obj))
                "#<unrepresentable>"))))))

(defun %encode-json-unchecked (obj)
  "Encode OBJ as a JSON string with resilient error handling; see %ENCODE-JSON.
Fast path: try normal yason:encode. On failure, sanitize all strings and retry.
On second failure, return a hardcoded valid JSON-RPC error response."
  (handler-case
      (with-output-to-string (stream) (yason:encode obj stream))
    (error (e1)
      (ignore-errors
        (log-event :warn "json-encode-retry"
                   "reason" (ignore-errors (princ-to-string e1))))
      (handler-case
          (let ((id-preserved (and (hash-table-p obj) (gethash "id" obj)))
                 (clean (%sanitize-for-encoding obj)))
            ;; Restore original "id" so yason:encode escapes it properly.
            ;; %sanitize-for-encoding strips \b/\f via sanitize-for-json,
            ;; which would break JSON-RPC ID correlation.
            (when (and id-preserved (hash-table-p clean))
              (setf (gethash "id" clean) id-preserved))
            (with-output-to-string (stream) (yason:encode clean stream)))
        (error (e2)
          (ignore-errors
            (log-event :error "json-encode-failed"
                       "reason" (ignore-errors (princ-to-string e2))))
          (let* ((id (and (hash-table-p obj) (gethash "id" obj)))
                 (id-json (cond
                            ((numberp id)
                             (or (ignore-errors
                                   (with-output-to-string (s)
                                     (yason:encode id s)))
                                 "null"))
                            ((stringp id)
                             (format nil "\"~A\""
                                     (with-output-to-string (s)
                                       ;; RFC 8259 compliant JSON string escaper.
                                       ;; Operates on raw id to preserve all chars
                                       ;; (sanitize-for-json would strip \b and \f).
                                       (loop for c across id
                                               do (let ((code (char-code c)))
                                                    (cond
                                                      ((char= c #\") (write-string "\\\"" s))
                                                      ((char= c #\\) (write-string "\\\\" s))
                                                      ((char= c #\Backspace) (write-string "\\b" s))
                                                      ((char= c #\Page) (write-string "\\f" s))
                                                      ((char= c #\Newline) (write-string "\\n" s))
                                                      ((char= c #\Tab) (write-string "\\t" s))
                                                      ((char= c #\Return) (write-string "\\r" s))
                                                      ((< code #x20)
                                                       (format s "\\u~4,'0X" code))
                                                      (t (write-char c s))))))))
                            (t "null"))))
            (format nil "{\"jsonrpc\":\"2.0\",\"id\":~A,\"error\":{\"code\":-32603,\"message\":\"Response JSON encoding failed\"}}"
                    id-json)))))))

(defun %encode-json (obj)
  "Encode OBJ as a JSON string with resilient error handling.
Fast path: try normal yason:encode. On failure, sanitize all strings and retry.
On second failure, return a hardcoded valid JSON-RPC error response.
Every path's result goes through TRANSPORT-SAFE-JSON: yason writes a lone
surrogate raw, and 0.7.x a control character too, and neither can be sent."
  (transport-safe-json (%encode-json-unchecked obj)))

;;; Response builders are imported from cl-mcp/src/tools/helpers:
;;;   result, rpc-error, text-content, tool-error

(defun %decode-file-uri-path (path)
  "Decode contiguous percent-encoded UTF-8 bytes and keep literal path characters."
  (with-output-to-string (out)
    (loop with length = (length path)
          with i = 0
          while (< i length)
          do (if (char= (char path i) #\%)
                 (let ((start i))
                   (loop while (and (< i length) (char= (char path i) #\%))
                         do (unless (and (< (+ i 2) length)
                                         (digit-char-p (char path (1+ i)) 16)
                                         (digit-char-p (char path (+ i 2)) 16))
                              (return-from %decode-file-uri-path nil))
                            (incf i 3))
                   (write-string (quri:url-decode path :start start :end i :encoding :utf-8) out))
                 (progn (write-char (char path i) out) (incf i))))))

(defun %initialize-uri-path (value)
  "Return the local path of an initialize root URI, or NIL when it cannot be used."
  (when (stringp value)
    ;; Clients also send native absolute paths in rootUri.
    (when (uiop:string-prefix-p "/" value)
      (return-from %initialize-uri-path value))
    (handler-case
        (let* ((uri (quri:uri value))
               (host (quri:uri-host uri))
               (path (quri:uri-path uri)))
          (when (and (equalp "file" (quri:uri-scheme uri))
                     (or (null host) (equalp "localhost" host))
                     (null (quri:uri-userinfo uri)) (null (quri:uri-port uri))
                     (null (quri:uri-query uri)) (null (quri:uri-fragment uri))
                     path (uiop:string-prefix-p "/" path))
            (let ((decoded (%decode-file-uri-path path)))
              (unless (find (code-char 0) decoded) decoded))))
      (error () nil))))

(defun handle-initialize (state id params)
  (when (and params
             (or (not (stringp (gethash "protocolVersion" params "")))
                 (not (hash-table-p (gethash "capabilities" params params)))
                 (not (hash-table-p (gethash "clientInfo" params params)))))
    (return-from handle-initialize (rpc-error id -32602 "Invalid initialize parameters")))
  ;; Sync rootPath from client if provided
  (when params
    (let* ((root-path (gethash "rootPath" params))
           (root-uri (gethash "rootUri" params))
           (root (or root-path (%initialize-uri-path root-uri))))
      (when (and root (stringp root) (plusp (length root)))
        (handler-case
            ;; PARSE-UNIX-NAMESTRING: ENSURE-DIRECTORY-PATHNAME hands a string
            ;; to the CL pathname reader, which reads [ and ] as wildcard
            ;; syntax and then signals on the wild result.  The HANDLER-CASE
            ;; below would swallow that into one log line and let initialize
            ;; succeed, so a client rooted at project[old]/ would silently
            ;; keep cl-mcp's own default root.
            (let ((root-dir (uiop:parse-unix-namestring root :ensure-directory t)))
              (when (uiop/filesystem:directory-exists-p root-dir)
                ;; Resolved, as fs-set-project-root resolves its path: kept as
                ;; spelled, a link such as /proc/self/cwd was re-resolved by
                ;; every later guard and followed whatever it came to name.
                (setf root-dir (uiop:ensure-directory-pathname (truename root-dir)))
                (let ((broad-p (broad-root-p root-dir)))
                  (cond
                    ;; Reject overly broad roots (same policy as fs.lisp)
                    ;; Skip root application but continue initialization normally
                    (broad-p
                     (log-event :warn "initialize.sync-root.rejected"
                                "path" (native-path-namestring root-dir)
                                "reason" "too broad"))
                    (t
                     ;; This session's root only: a client connecting must
                     ;; not re-point the sessions already connected (#129).
                     (set-project-root root-dir)
                     ;; Propagate root to this session's worker only
                     (ignore-errors
                       (send-root-to-session-worker *current-session-id* root-dir))
                     (log-event :info "initialize.sync-root" "rootPath"
                                (native-path-namestring root-dir) "source"
                                (if root-path "rootPath" "rootUri")))))))
          (error (e)
            (ignore-errors
              (log-event :warn "initialize.sync-root-failed" "path" root
                         "error" (princ-to-string e))))))))
  (let* ((client-ver (and params (gethash "protocolVersion" params)))
         (supported
          (and client-ver
               (find client-ver +supported-protocol-versions+ :test #'string=)))
         (chosen (or supported +protocol-version+)))
    (setf (protocol-version state) chosen)
    (result id (make-ht "protocolVersion" chosen
                        "serverInfo" (make-ht "name" "cl-mcp" "version" (version))
                        "capabilities" (make-ht "tools" (make-ht))
                        "instructions" (server-instructions)))))

(defun %handle-cancel-notification (params)
  "Handle a notifications/cancelled message from the MCP client.
Extracts requestId from PARAMS and calls cancel-request for that request of
*current-session-id* -- the session is part of the request's identity, so
another session's request with the same id is untouched.  A request that has
not reached its worker is stopped before it runs; one its worker is running
is stopped with the worker; one already answered is left alone.  A request a
tool runs inline is stopped only inside a region it declared cancellable
(CALL-CANCELLABLE): clgrep-search's scan."
  (let ((request-id (and (hash-table-p params)
                         (gethash "requestId" params))))
    (when request-id
      (log-event :info "protocol.cancel-notification"
                 "request_id" request-id)
      ;; Wrap in ignore-errors so any failure in the cancel path cannot
      ;; propagate to process-json-line's handler-case, which would
      ;; generate a JSON-RPC error response for a notification —
      ;; violating the spec ("Server MUST NOT reply to a Notification").
      (ignore-errors (cancel-request request-id *current-session-id*))
      ;; A tool running inline in this process has no worker to stop; if it
      ;; is in a cancellable region (a search), that region is interrupted.
      (ignore-errors (cancel-inline-request *current-session-id* request-id))))
  nil)

(defun handle-notification (state method params)
  "Handle incoming JSON-RPC notifications (no response expected).
Dispatches notifications/initialized and notifications/cancelled."
  (declare (ignore state))
  (cond
    ((string= method "notifications/initialized") nil)
    ((string= method "notifications/cancelled")
     (%handle-cancel-notification params))
    (t nil)))

(defun handle-tools-list (id)
  "Return the list of available tools from the registry."
  (result id (make-ht "tools" (map 'vector #'repl-tool-descriptor
                                  (get-all-tool-descriptors)))))

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

(defun handle-tools-call (state id params)
  "Dispatch a tool call to the appropriate handler from the registry."
  (let* ((name (and params (gethash "name" params)))
         (args (and params (gethash "arguments" params)))
         (local (and name (%normalize-tool-alias name))))
    (when name (log-event :debug "tools.call" "name" name "local" local))
    (let ((handler (and local (get-tool-handler local))))
      (cond
        (handler
          (call-with-repl-session state id local args
                                  (lambda () (funcall handler state id args))))
        ;; A tool that exists but is switched off is a setting to change, not
        ;; a name to correct, and saying "not found" sends the caller looking
        ;; for a spelling mistake that is not there.
        ((and local (disabled-tool-group local))
         (let ((group (string-downcase
                       (string (disabled-tool-group local)))))
           (rpc-error id -32601
                      (format nil "Tool ~A belongs to the optional tool group ~
'~A', which is not enabled in this server. Enable it by setting ~
MCP_ENABLE_TOOL_GROUPS=~A in the server's environment, or by passing ~
:tool-groups (list :~A) to cl-mcp:run."
                              name group group group))))
        (t (rpc-error id -32602 (format nil "Tool ~A not found" name)))))))

(defun handle-request (state id method params)
  ;; Every request runs under its own session's project root, so the parent's
  ;; file, edit and search tools resolve paths where that session set its root
  ;; rather than wherever another session last moved it (#129).
  (with-session-project-root ()
    (cond
      ((string= method "initialize") (handle-initialize state id params))
      ((string= method "tools/list") (handle-tools-list id))
      ((string= method "tools/call")
       (or (handle-asdf-tools-call id params)
           (handle-tools-call state id params)))
      ((string= method "ping") (result id (make-ht)))
      (t (rpc-error id -32601 (format nil "Method ~A not found" method))))))

(defun %dispatch-message (message state in-batch-p wire-input-p)
  "Return the reply object for one message, or NIL for a one-way message."
  (unless (hash-table-p message)
    (return-from %dispatch-message (rpc-error nil -32600 "Invalid Request")))
  (let* ((id (gethash "id" message :absent))
         (valid-id (typep id '(or integer string)))
         (method (gethash "method" message :absent))
         (params (gethash "params" message :absent))
         (result (gethash "result" message :absent))
         (error (gethash "error" message :absent))
         ;; Logged as null when absent: yason cannot encode :ABSENT, and
         ;; LOG-EVENT drops a line it cannot encode (issue #393).
         (logged-id (unless (eq id :absent) id))
         (logged-method (unless (eq method :absent) method)))
    (log-event :debug "rpc.dispatch" "id" logged-id "method" logged-method)
    (unless (and (equal (gethash "jsonrpc" message) "2.0")
                 (or (eq id :absent) valid-id))
      (return-from %dispatch-message (rpc-error (when valid-id id) -32600 "Invalid Request")))
    ;; November 2025 permits an omitted ID on an error response only.
    (when (and (eq method :absent)
               (or (and valid-id (hash-table-p result) (eq error :absent))
                   (and (or valid-id (equal (protocol-version state) "2025-11-25"))
                        (eq result :absent) (hash-table-p error)
                        (integerp (gethash "code" error))
                        (stringp (gethash "message" error)))))
      (return-from %dispatch-message nil))
    (unless (and (stringp method) (eq result :absent) (eq error :absent))
      (return-from %dispatch-message (rpc-error (when valid-id id) -32600 "Invalid Request")))
    (handler-case
        (cond
          ((eq id :absent)
           (handle-notification state method (unless (eq params :absent) params))
           nil)
          ((and (equal method "initialize") (or in-batch-p (protocol-version state)))
           (rpc-error id -32600 "Initialize must be the first, separate request of a session"))
          ((not (or (eq params :absent) (hash-table-p params)))
           (rpc-error id -32602 "Expected parameters to be an object"))
          (t
           (let ((*wire-tool-arguments*
                   (when (and wire-input-p (equal method "tools/call") (hash-table-p params))
                     (gethash "arguments" params))))
             (prog1 (handle-request state id method (unless (eq params :absent) params))
               (log-event :debug "rpc.result" "id" id "method" method)))))
      (error (e)
        ;; A condition whose report signals, or prints without end, must not
        ;; take the line (or the thread) with it.
        (ignore-errors (log-event :error "rpc.internal" "id" logged-id "method" logged-method
                                  "error" (or (ignore-errors (bounded-princ-to-string e))
                                              (format nil "unprintable ~S"
                                                      (type-of e)))))
        (when valid-id (rpc-error id -32603 "Internal error")))
      (storage-condition ()
        (ignore-errors (log-event :error "rpc.internal" "method" logged-method
                                  "error" "storage exhausted"))
        (when valid-id (rpc-error id -32603 "Internal error: the server ran out of stack or heap"))))))

(defun process-json-message (message state &key wire-input-p)
  "Dispatch a parsed MCP message and return its encoded reply, or NIL.
For a single message the second value is its reply object, if any.
Set WIRE-INPUT-P for JSON decoded from a transport so a supplied null or false
array argument can be distinguished from an omitted argument."
  (if (and (vectorp message) (not (stringp message)) (plusp (length message))
           (equal (protocol-version state) "2025-03-26"))
      ;; Encode each reply independently, preserving its ID even on the encoder's retry path.
      (let ((replies (loop for item across message
                           for reply = (%dispatch-message item state t wire-input-p)
                           when reply collect (%encode-json reply))))
        (when replies (format nil "[~{~A~^,~}]" replies)))
      (let ((reply (%dispatch-message message state nil wire-input-p)))
        (when reply (values (%encode-json reply) reply)))))

(defun process-json-line (line &optional (state (make-state)))
  "Process one JSON-RPC line and return a JSON line to send, or NIL for notifications."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (when (string= trimmed "")
      (log-event :debug "rpc.skip-empty")
      (return-from process-json-line nil))
    ;; Counted before parsing: an exhausted stack is not always catchable.
    (when (json-too-deep-p trimmed)
      (ignore-errors (log-event :warn "rpc.parse-error" "error" "nested too deeply"))
      (return-from process-json-line
        (%encode-json (rpc-error nil -32700 (format nil "Parse error: nested deeper than ~D levels"
                                                    +max-json-nesting+)))))
    (let ((msg (handler-case
                   (%decode-json trimmed)
                 (error (e)
                   (ignore-errors
                     (log-event :warn "rpc.parse-error"
                                "line" trimmed
                                "error" (princ-to-string e)))
                   (return-from process-json-line
                     (%encode-json (rpc-error nil -32700 "Parse error"))))
                 ;; Not an ERROR: without this, a line nested past the
                 ;; parser's stack would stop the connection's thread.
                 (storage-condition ()
                   (ignore-errors
                     (log-event :warn "rpc.parse-error" "error" "nested too deeply"))
                   (return-from process-json-line
                     (%encode-json
                      (rpc-error nil -32700 "Parse error: nested too deeply")))))))
      (process-json-message msg state :wire-input-p t))))
