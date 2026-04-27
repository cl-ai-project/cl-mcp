;;;; src/attach.lisp
;;;;
;;;; Attach mode -- opt-in routing of repl-eval to a user-supplied
;;;; running Slynk image instead of a hermetic forked worker.  When
;;;; *attach-config* is NIL (the default), every export here is inert
;;;; and the worker pool path is unaffected.
;;;;
;;;; The connection cache, dispatch macro, and wrapping form are layered
;;;; in subsequent commits so each piece can be reviewed in isolation.

(defpackage #:cl-mcp/src/attach
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:result
                #:text-content)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-eval-response)
  (:import-from #:slynk-client
                #:slime-connect
                #:slime-close
                #:slime-eval
                #:slime-network-error)
  (:export #:*attach-config*
           #:attach-config
           #:make-attach-config
           #:attach-config-host
           #:attach-config-port
           #:parse-attach-spec
           #:set-attach-from-env
           #:attach-active-p
           #:attach-disconnect
           #:with-attach-dispatch))

(in-package #:cl-mcp/src/attach)

(defstruct attach-config
  "Configuration for attach mode: the host and port of a user-supplied
running Slynk listener.  When *attach-config* is bound to a non-NIL
instance, supported tools route their work to that listener instead of
the worker pool."
  (host nil :type (or null string))
  (port nil :type (or null integer)))

(defvar *attach-config* nil
  "Either NIL (attach disabled, the default) or an `attach-config'
struct describing the Slynk listener to route to.  Bound by
`set-attach-from-env' (from CL_MCP_SLYNK_ATTACH) or by a `:slynk-attach'
keyword passed to `cl-mcp:run'.")

(defun parse-attach-spec (spec)
  "Parse a \"host:port\" SPEC string into an `attach-config' struct.
SPEC must be a non-empty string of the shape \"HOST:PORT\" where HOST
contains no colon (no IPv6 literal support in v1) and PORT is a decimal
integer in 1-65535.  Signals a SIMPLE-ERROR with a human-readable
message on any other input."
  (unless (stringp spec)
    (error "CL_MCP_SLYNK_ATTACH must be a string of the form \"host:port\", got: ~S"
           spec))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) spec)))
    (when (zerop (length trimmed))
      (error "CL_MCP_SLYNK_ATTACH is empty; expected \"host:port\""))
    (let ((colon (position #\: trimmed)))
      (unless colon
        (error "CL_MCP_SLYNK_ATTACH=~S is missing a colon; expected \"host:port\""
               trimmed))
      (let ((host (subseq trimmed 0 colon))
            (port-string (subseq trimmed (1+ colon))))
        (when (zerop (length host))
          (error "CL_MCP_SLYNK_ATTACH=~S has an empty host; expected \"host:port\""
                 trimmed))
        (when (zerop (length port-string))
          (error "CL_MCP_SLYNK_ATTACH=~S has an empty port; expected \"host:port\""
                 trimmed))
        (let ((port
               (handler-case
                   (let ((*read-base* 10)
                         (*read-eval* nil))
                     (with-input-from-string (s port-string)
                       (let ((value (read s nil nil))
                             (extra (peek-char nil s nil nil)))
                         (when extra
                           (error "trailing characters after port"))
                         value)))
                 (error ()
                   (error "CL_MCP_SLYNK_ATTACH=~S has a non-integer port ~S"
                          trimmed port-string)))))
          (unless (integerp port)
            (error "CL_MCP_SLYNK_ATTACH=~S has a non-integer port ~S"
                   trimmed port-string))
          (unless (<= 1 port 65535)
            (error "CL_MCP_SLYNK_ATTACH=~S port ~A is outside 1..65535"
                   trimmed port))
          (make-attach-config :host host :port port))))))

(defun set-attach-from-env ()
  "Read CL_MCP_SLYNK_ATTACH and, when set to a non-empty value, parse it
via `parse-attach-spec' and store the result in `*attach-config*'.
When the variable is unset or empty, leave `*attach-config*' unchanged.
Returns the value of `*attach-config*' after the call."
  (let ((raw (uiop:getenv "CL_MCP_SLYNK_ATTACH")))
    (when (and raw (plusp (length raw))
               (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           raw))))
      (setf *attach-config* (parse-attach-spec raw))))
  *attach-config*)

(defvar *session-connections* (make-hash-table :test 'equal)
  "Hash table mapping a session-id string to its open
`slynk-client:slynk-connection' instance.  Each session keeps a single
connection to the attached Slynk server for the lifetime of the
session, lazily opened on first dispatch and closed on disconnect.")

(defvar *session-connections-lock* (make-lock "attach-session-connections")
  "Lock guarding *session-connections* and the per-session call locks in
*session-call-locks*.")

(defvar *session-call-locks* (make-hash-table :test 'equal)
  "Hash table mapping a session-id string to a `bordeaux-threads:lock'
that serialises in-flight `slime-eval' calls on that session's
connection.  slynk-client `slime-eval' is synchronous and the
connection is not safe to share across concurrent callers.")

(defun attach-active-p ()
  "Return T when attach mode is configured (i.e. *attach-config* is
non-NIL) and supported tools should route to the attached Slynk server
instead of executing inline."
  (not (null *attach-config*)))

(defun %get-or-open-connection (session-id)
  "Return the cached `slynk-connection' for SESSION-ID, opening one
through `slynk-client:slime-connect' if necessary.  Signals
`slime-network-error' if the connect attempt fails or returns NIL.
Logs each newly opened connection as `attach.connection.opened'."
  (check-type session-id string)
  (let ((config *attach-config*))
    (unless config
      (error "attach mode is not configured; cannot open a Slynk connection"))
    (with-lock-held (*session-connections-lock*)
      (or (gethash session-id *session-connections*)
          (let* ((host (attach-config-host config))
                 (port (attach-config-port config))
                 (conn (slime-connect host port)))
            (unless conn
              (error 'slime-network-error
                     :format-control
                     "slynk-client:slime-connect to ~A:~A returned NIL"
                     :format-arguments (list host port)))
            (setf (gethash session-id *session-connections*) conn)
            (unless (gethash session-id *session-call-locks*)
              (setf (gethash session-id *session-call-locks*)
                    (make-lock (format nil "attach-eval/~A" session-id))))
            (log-event :info "attach.connection.opened"
                       "session" session-id
                       "host" host
                       "port" port)
            conn)))))

(defun %get-call-lock (session-id)
  "Return the per-session call lock, creating it if missing.  Used by
`with-attach-dispatch' to serialise concurrent eval calls on the same
shared connection."
  (with-lock-held (*session-connections-lock*)
    (or (gethash session-id *session-call-locks*)
        (setf (gethash session-id *session-call-locks*)
              (make-lock (format nil "attach-eval/~A" session-id))))))

(defun attach-disconnect (session-id &key (reason "explicit"))
  "Close and discard the cached Slynk connection for SESSION-ID, if any.
Idempotent: a NIL or unknown session is a no-op.  REASON is logged with
the close event so the source of the disconnect (network error, session
end, manual reset) is preserved."
  (when (and session-id (stringp session-id))
    (let (conn)
      (with-lock-held (*session-connections-lock*)
        (setf conn (gethash session-id *session-connections*))
        (when conn
          (remhash session-id *session-connections*))
        (remhash session-id *session-call-locks*))
      (when conn
        (handler-case (slime-close conn)
          (error () nil))
        (log-event :info "attach.connection.closed"
                   "session" session-id
                   "reason" reason))))
  nil)

(defun %build-wrapping-form (code-string package-name)
  "Return a single s-expression to send to the attached Slynk server.
The form reads CODE-STRING under PACKAGE-NAME (or :CL-USER if NIL),
evaluates each top-level form sequentially, captures stdout/stderr, and
returns the same five-value shape `repl-core:repl-eval' produces:

  (PRINTED-STRING RAW-VALUE STDOUT-STRING STDERR-STRING ERROR-CONTEXT)

ERROR-CONTEXT is either NIL on success, or a plist with keys
:condition-type, :message, :restarts, :frames -- matching the shape
`build-eval-response' already knows how to render.  Reader errors,
unknown packages, and any error during evaluation are caught locally in
the attached image so a single slime-eval round-trip always returns the
five-tuple rather than blowing up the connection."
  `(let ((stdout-stream (make-string-output-stream))
         (stderr-stream (make-string-output-stream))
         (error-context nil)
         (raw-value nil)
         (printed nil))
     (handler-case
         (let* ((pkg-name ,(or package-name "CL-USER"))
                (pkg (or (find-package pkg-name)
                         (error "Package ~S not found in attached image"
                                pkg-name)))
                (forms
                 (let ((*package* pkg)
                       (*read-eval* nil))
                   (with-input-from-string (s ,code-string)
                     (loop with eof = '#:eof
                           for form = (read s nil eof)
                           until (eq form eof)
                           collect form)))))
           (let ((*standard-output* stdout-stream)
                 (*error-output* stderr-stream)
                 (*package* pkg))
             (dolist (form forms)
               (setf raw-value (eval form))))
           (let ((*print-readably* nil)
                 (*print-circle* t))
             (setf printed (prin1-to-string raw-value))))
       (error (e)
         (setf error-context
               (list :condition-type (princ-to-string (type-of e))
                     :message
                     (handler-case (princ-to-string e)
                       (error () (format nil "<error formatting ~A>"
                                         (type-of e))))
                     :restarts nil
                     :frames nil))
         (setf raw-value
               (handler-case (princ-to-string e)
                 (error () (format nil "<error formatting ~A>" (type-of e)))))
         (setf printed raw-value)))
     (list printed
           raw-value
           (get-output-stream-string stdout-stream)
           (get-output-stream-string stderr-stream)
           error-context)))

(defun %unsupported-tool-error (tool)
  "Return a tool-result hash-table indicating that TOOL is not supported
by attach mode in v1.  Logs `attach.unsupported-tool' for visibility."
  (log-event :warn "attach.unsupported-tool" "tool" tool)
  (make-ht "isError" t
           "content"
           (text-content
            (format nil
                    "attach mode does not support the ~A tool in v1; ~
                     unset CL_MCP_SLYNK_ATTACH or omit :slynk-attach to ~
                     use the worker pool for this call."
                    tool))))

(defun %network-error-result (session-id condition)
  "Mark the per-session connection as degraded and build an error
tool-result hash-table for a `slime-network-error'.  The next call from
the same session will lazily reconnect on demand."
  (let ((message (handler-case (princ-to-string condition)
                   (error () (format nil "<error formatting ~A>"
                                     (type-of condition))))))
    (log-event :warn "attach.network-error"
               "session" session-id
               "error" message)
    (when session-id
      (handler-case (attach-disconnect session-id :reason "network-error")
        (error () nil)))
    (make-ht "isError" t
             "content"
             (text-content
              (format nil "attach: Slynk connection error: ~A" message)))))

(defun %dispatch-attach (id tool params)
  "Route a tool call to the attached Slynk server and return a tool-result
hash-table.  ID is the JSON-RPC request id (used only for log
correlation in v1).  TOOL is the tool name as a string; the only
supported value in v1 is \"repl-eval\".  PARAMS is the hash-table built
by `with-attach-dispatch' callers (see its docstring for the expected
keys).

The call serialises on a per-session lock around `slime-eval' so
concurrent eval calls on the same session's connection don't interleave
on the wire.  On `slime-network-error' the cached connection is
discarded and an `isError'-tagged result is returned; the user's next
call will reconnect on demand."
  (declare (ignore id))
  (unless (string= tool "repl-eval")
    (return-from %dispatch-attach (%unsupported-tool-error tool)))
  (let ((session-id (or *current-session-id* "default"))
        (code (gethash "code" params))
        (package-name (gethash "package" params))
        (max-output-length (gethash "max_output_length" params))
        (preview-max-depth (gethash "preview_max_depth" params))
        (preview-max-elements (gethash "preview_max_elements" params))
        (include-preview (let ((v (gethash "include_result_preview" params)))
                           (if (null v) t v))))
    (unless (and (stringp code) (plusp (length code)))
      (return-from %dispatch-attach
        (make-ht "isError" t
                 "content"
                 (text-content
                  "attach: 'code' parameter is required and must be a non-empty string."))))
    (handler-case
        (let* ((conn (%get-or-open-connection session-id))
               (lock (%get-call-lock session-id))
               (form (%build-wrapping-form code package-name))
               (start (get-internal-real-time))
               (raw-result (with-lock-held (lock)
                             (slime-eval form conn))))
          (multiple-value-bind (printed raw stdout stderr error-context)
              (values-list (if (listp raw-result)
                               (append raw-result
                                       (loop while (< (length raw-result) 5)
                                             collect nil))
                               (list (princ-to-string raw-result)
                                     raw-result "" "" nil)))
            (log-event :debug "attach.eval.success"
                       "session" session-id
                       "ms" (round (* 1000
                                      (/ (- (get-internal-real-time) start)
                                         internal-time-units-per-second))))
            (build-eval-response printed raw stdout stderr error-context
                                 :include-result-preview include-preview
                                 :preview-max-depth (or preview-max-depth 1)
                                 :preview-max-elements
                                 (or preview-max-elements 8)
                                 :max-output-length max-output-length)))
      (slime-network-error (e)
        (%network-error-result session-id e))
      (error (e)
        ;; Catch-all: format the error so the caller never sees a raw
        ;; condition leak through and the connection cache is healed
        ;; for the next attempt.
        (let ((msg (handler-case (princ-to-string e)
                     (error () (format nil "<error formatting ~A>"
                                       (type-of e))))))
          (log-event :warn "attach.dispatch-error"
                     "session" session-id
                     "error" msg)
          (make-ht "isError" t
                   "content"
                   (text-content
                    (format nil "attach: dispatch error: ~A" msg))))))))

(defmacro with-attach-dispatch ((id tool params) &body body)
  "When attach mode is configured (i.e. `attach-active-p' is true) route
the tool call to the attached Slynk server and short-circuit BODY with
the slynk-evaluated result wrapped in a JSON-RPC response.  Otherwise
expand to BODY -- a zero-cost passthrough.

ID is the JSON-RPC request id.  TOOL is the tool name as a string
(\"repl-eval\" is the only supported value in v1).  PARAMS is a
hash-table of arguments shaped like the worker proxy's parameter map
(`code', `package', `print_level', `print_length', `max_output_length',
`safe_read'), so callers can share the same params builder with
`with-proxy-dispatch'."
  `(if (attach-active-p)
       (result ,id (%dispatch-attach ,id ,tool ,params))
       (progn ,@body)))
