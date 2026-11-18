;;;; Dedicated worker images within one MCP session.

(defpackage #:cl-mcp/src/tools/repl-session
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table)
  (:import-from #:bordeaux-threads #:with-lock-held)
  (:import-from #:cl-mcp/src/pool
                #:get-or-assign-worker #:pool-capacity-exceeded #:release-session)
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

(defstruct repl-session
  ;; Both fields are guarded by the owning server state's repl-sessions lock.
  (active-requests 0 :type fixnum)
  (closed-p nil :type boolean))

(defun %release-repl (repl-id)
  (unwind-protect
       (release-session repl-id)
    (forget-session-project-root repl-id)))

(defun %open-repl (state)
  (let ((repl-id (make-dedicated-repl-id))
        (published nil))
    (unwind-protect
         (progn
           (remember-session-project-root repl-id)
           (get-or-assign-worker repl-id)
           (with-lock-held ((repl-sessions-lock state))
             (let ((sessions (repl-sessions state)))
               (when sessions
                 (setf (gethash repl-id sessions) (make-repl-session)
                       published t)
                 repl-id))))
      (unless published
        (%release-repl repl-id)))))

(defun %close-repl (state repl-id)
  (let (closed idle)
    (with-lock-held ((repl-sessions-lock state))
      (let* ((sessions (repl-sessions state))
             (repl (and sessions (gethash repl-id sessions))))
        (when repl
          (remhash repl-id sessions)
          (setf (repl-session-closed-p repl) t
                idle (zerop (repl-session-active-requests repl))
                closed t))))
    (when idle
      (%release-repl repl-id))
    closed))

(defun close-repl-sessions (state)
  "Stop admitting calls to STATE's REPLs and release them after admitted calls finish."
  (let (idle)
    (with-lock-held ((repl-sessions-lock state))
      (let ((sessions (repl-sessions state)))
        (when sessions
          (maphash (lambda (repl-id repl)
                     (setf (repl-session-closed-p repl) t)
                     (when (zerop (repl-session-active-requests repl))
                       (push repl-id idle)))
                   sessions)
          (setf (repl-sessions state) nil))))
    (dolist (repl-id idle)
      (%release-repl repl-id))))

(defun %acquire-repl (state repl-id)
  (with-lock-held ((repl-sessions-lock state))
    (let* ((sessions (repl-sessions state))
           (repl (and sessions (gethash repl-id sessions))))
      (when repl
        (incf (repl-session-active-requests repl))
        repl))))

(defun %finish-repl-request (state repl-id repl)
  (when (with-lock-held ((repl-sessions-lock state))
          (and (zerop (decf (repl-session-active-requests repl)))
               (repl-session-closed-p repl)))
    (%release-repl repl-id)))

(defun %repl-error (state id message)
  (tool-error id message :protocol-version (protocol-version state)))

(defun %repl-result (state id data)
  (let ((response (make-ht "content" (text-content (encode-json-line data))))
        (version (protocol-version state)))
    (when (and version (string>= version "2025-06-18"))
      (setf (gethash "structuredContent" response) data))
    (result id response)))

(defun %repl-management-tool-p (name)
  (member name '("repl-open" "repl-close") :test #'equal))

(defun call-with-repl-session (state id name args thunk)
  "Call THUNK under the REPL named by args, or use the default MCP session."
  (multiple-value-bind (repl-id supplied-p)
      (when (hash-table-p args) (gethash "repl_id" args))
    (cond
      ((or (not supplied-p) (%repl-management-tool-p name))
       (funcall thunk))
      ((not (and (stringp repl-id) (plusp (length repl-id))))
       (%repl-error state id "repl_id must be a non-empty string."))
      ((not *use-worker-pool*)
       (%repl-error state id "repl_id requires the worker pool."))
      (t
       (let ((repl (%acquire-repl state repl-id)))
         (unless repl
           (return-from call-with-repl-session
             (%repl-error state id "Unknown or closed repl_id. Call repl-open first.")))
         (unwind-protect
              (let ((*transport-session-id* *current-session-id*)
                    (*current-session-id* repl-id))
                (with-session-project-root () (funcall thunk)))
           (%finish-repl-request state repl-id repl)))))))

(defun repl-tool-descriptor (descriptor)
  "Add the optional repl_id selector without modifying a registered descriptor."
  (if (or (not *use-worker-pool*) (%repl-management-tool-p (gethash "name" descriptor)))
      descriptor
      (let* ((copy (copy-hash-table descriptor))
             (schema (copy-hash-table (gethash "inputSchema" descriptor)))
             (original-properties (gethash "properties" schema))
             (properties (if original-properties
                             (copy-hash-table original-properties)
                             (make-ht))))
        (setf (gethash "inputSchema" copy) schema
              (gethash "properties" schema) properties
              (gethash "repl_id" properties)
              (make-ht "type" "string"
                       "description" "REPL handle from repl-open. Omit for the default REPL."))
        copy)))

(define-tool "repl-open"
  :description "Allocate a separate SBCL worker in this MCP session. For each subagent,
open a REPL and pass its repl_id on subsequent calls to keep its Lisp state isolated.
Requires the worker pool. Call repl-close when finished."
  :args ()
  :body
  (cond
    ((not *use-worker-pool*)
     (%repl-error state id "repl-open requires the worker pool."))
    ((null *current-session-id*)
     (%repl-error state id "repl-open requires an MCP session."))
    (t
     (handler-case
         (let ((repl-id (%open-repl state)))
           (if repl-id
               (%repl-result state id (make-ht "repl_id" repl-id))
               (%repl-error state id "The MCP session has ended.")))
       (pool-capacity-exceeded (condition)
         (%repl-error state id (princ-to-string condition)))))))

(define-tool "repl-close"
  :description "Close a REPL opened by repl-open and release its worker when admitted calls finish."
  :args ((repl-id :json-name "repl_id" :type :string :required t
                  :description "Handle returned by repl-open in this MCP session."))
  :body
  (if (%close-repl state repl-id)
      (%repl-result state id (make-ht "closed" t))
      (%repl-error state id "Unknown or closed repl_id.")))
