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
  (:export #:*attach-config*
           #:attach-config
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

(defun attach-active-p ()
  "Return non-NIL when attach mode is configured for the current call.
Stub: returns NIL until behaviour is layered in."
  nil)

(defun attach-disconnect (session-id)
  "Close and discard the cached Slynk connection for SESSION-ID, if any.
Idempotent.  Stub: behaviour added in the connection-cache commit."
  (declare (ignore session-id))
  nil)

(defmacro with-attach-dispatch ((id tool params) &body body)
  "When attach mode is active, route this tool call to the attached
Slynk server and short-circuit BODY.  Otherwise expand to BODY (zero-cost
passthrough).  Stub: always falls through to BODY until the dispatch
macro is implemented in the with-attach-dispatch commit."
  (declare (ignore id tool params))
  `(progn ,@body))
