;;;; Dedicated worker images within one MCP session.

(defpackage #:cl-mcp/src/tools/repl-session
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table)
  (:import-from #:bordeaux-threads #:with-lock-held)
  (:import-from #:cl-mcp/src/pool #:release-session #:set-dedicated-repl-idle)
  (:import-from #:cl-mcp/src/project-root
                #:remember-session-project-root #:forget-session-project-root
                #:with-session-project-root)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id* #:*transport-session-id*
                #:make-dedicated-repl-id #:protocol-version #:repl-sessions #:repl-sessions-lock)
  (:import-from #:cl-mcp/src/tools/define-tool #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht #:result #:text-content #:tool-error)
  (:import-from #:cl-mcp/src/utils/sanitize #:encode-json-line)
  (:export #:call-with-repl-session #:close-repl-sessions #:repl-tool-descriptor))

(in-package #:cl-mcp/src/tools/repl-session)

(declaim (ftype (function (string) *) %release-repl))

(defstruct (repl-session (:constructor make-repl-session (id)))
  (id "" :type string :read-only t)
  ;; The owning server state's REPL lock guards the request count and close flag.
  (active-requests 0 :type fixnum)
  (closed-p nil :type boolean))

(defun %release-repl (repl-id)
  (ignore-errors
    (unwind-protect
         (release-session repl-id)
      (forget-session-project-root repl-id))))

(defun %close-repl (state agent-id)
  (let (idle-repl closed)
    (with-lock-held ((repl-sessions-lock state))
      (let ((sessions (repl-sessions state)))
        (when sessions
          (let ((entry (gethash agent-id sessions)))
            (when (repl-session-p entry)
              (setf (repl-session-closed-p entry) t)
              (when (zerop (repl-session-active-requests entry))
                (setf idle-repl entry))))
          ;; A close request may arrive before any other call for this agent.
          (setf (gethash agent-id sessions) :closed
                closed t))))
    (when idle-repl
      (%release-repl (repl-session-id idle-repl)))
    closed))

(defun close-repl-sessions (state)
  "Stop admitting calls to STATE's REPLs and release them after admitted calls finish."
  (let (idle)
    (with-lock-held ((repl-sessions-lock state))
      (let ((sessions (repl-sessions state)))
        (when sessions
          (maphash (lambda (agent-id entry)
                     (declare (ignore agent-id))
                     (when (repl-session-p entry)
                       (setf (repl-session-closed-p entry) t)
                       (when (zerop (repl-session-active-requests entry))
                         (push (repl-session-id entry) idle))))
                   sessions)
          (setf (repl-sessions state) nil))))
    (dolist (repl-id idle)
      (%release-repl repl-id))))

(defun %acquire-repl (state agent-id)
  (with-lock-held ((repl-sessions-lock state))
    (let* ((sessions (repl-sessions state))
           (entry (and sessions (gethash agent-id sessions))))
      (when (and sessions (not (eq entry :closed)))
        (unless entry
          (let ((id (make-dedicated-repl-id)))
            (remember-session-project-root id)
            (setf entry (make-repl-session id)
                  (gethash agent-id sessions) entry)))
        (when (zerop (repl-session-active-requests entry))
          (set-dedicated-repl-idle (repl-session-id entry) nil))
        (incf (repl-session-active-requests entry))
        entry))))

(defun %finish-repl-request (state repl)
  (let ((id (repl-session-id repl)))
    (when (with-lock-held ((repl-sessions-lock state))
            (when (zerop (decf (repl-session-active-requests repl)))
              (if (repl-session-closed-p repl)
                  t
                  (progn (set-dedicated-repl-idle id t) nil))))
      (%release-repl id))))

(defun %repl-error (state id message)
  (tool-error id message :protocol-version (protocol-version state)))

(defun %repl-result (state id data)
  (let ((response (make-ht "content" (text-content (encode-json-line data))))
        (version (protocol-version state)))
    (when (and version (string>= version "2025-06-18"))
      (setf (gethash "structuredContent" response) data))
    (result id response)))

(defun call-with-repl-session (state id name args thunk)
  "Call THUNK in AGENT_ID's dedicated REPL when supplied, or in the default session."
  (multiple-value-bind (agent-id supplied-p)
      (when (hash-table-p args) (gethash "agent_id" args))
    (cond
      ((equal name "repl-close")
       (funcall thunk))
      ((not supplied-p)
       (funcall thunk))
      ((not (and (stringp agent-id) (plusp (length agent-id))))
       (%repl-error state id "agent_id must be a non-empty string."))
      ((not *use-worker-pool*)
       (%repl-error state id "agent_id requires the worker pool."))
      ((null *current-session-id*)
       (%repl-error state id "agent_id requires an MCP session."))
      (t
       (let ((repl (%acquire-repl state agent-id)))
         (unless repl
           (return-from call-with-repl-session
             (%repl-error state id "The agent's REPL or its MCP session is closed.")))
         (unwind-protect
              (let ((*transport-session-id* *current-session-id*)
                    (*current-session-id* (repl-session-id repl)))
                (with-session-project-root () (funcall thunk)))
           (%finish-repl-request state repl)))))))

(defun repl-tool-descriptor (descriptor)
  "Add the optional agent_id selector without modifying a registered descriptor."
  (if (or (not *use-worker-pool*) (equal "repl-close" (gethash "name" descriptor)))
      descriptor
      (let* ((copy (copy-hash-table descriptor))
             (schema (copy-hash-table (gethash "inputSchema" descriptor)))
             (original-properties (gethash "properties" schema))
             (properties (if original-properties
                             (copy-hash-table original-properties)
                             (make-ht))))
        (setf (gethash "inputSchema" copy) schema
              (gethash "properties" schema) properties
              (gethash "agent_id" properties)
              (make-ht "type" "string" "minLength" 1
                       "description" "Stable caller agent ID. Omit for the default REPL."))
        copy)))

(define-tool "repl-close"
  :description "End an agent's REPL in this MCP session and release its worker when admitted calls
finish. This call can be repeated and closes the agent ID even before its first call."
  :args ((agent-id :json-name "agent_id" :type :string :required t
                   :description "The stable agent ID supplied on tool calls."))
  :body
  (cond
    ((not (plusp (length agent-id)))
     (%repl-error state id "agent_id must be a non-empty string."))
    ((not *use-worker-pool*)
     (%repl-error state id "repl-close requires the worker pool."))
    ((null *current-session-id*)
     (%repl-error state id "repl-close requires an MCP session."))
    ((%close-repl state agent-id)
     (%repl-result state id (make-ht "closed" t)))
    (t
     (%repl-error state id "The MCP session is closed."))))
