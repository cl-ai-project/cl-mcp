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
  (:import-from #:slynk-client
                #:slime-connect
                #:slime-close
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

(defmacro with-attach-dispatch ((id tool params) &body body)
  "When attach mode is active, route this tool call to the attached
Slynk server and short-circuit BODY.  Otherwise expand to BODY (zero-cost
passthrough).  Stub: always falls through to BODY until the dispatch
macro is implemented in the with-attach-dispatch commit."
  (declare (ignore id tool params))
  `(progn ,@body))
