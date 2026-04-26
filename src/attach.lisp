;;;; src/attach.lisp
;;;;
;;;; Attach mode -- opt-in routing of repl-eval to a user-supplied
;;;; running Slynk image instead of a hermetic forked worker.  When
;;;; *attach-config* is NIL (the default), every export here is inert
;;;; and the worker pool path is unaffected.
;;;;
;;;; This file holds only the package skeleton and configuration shape.
;;;; Behaviour (parsing, connection cache, dispatch macro, wrapping
;;;; form) is layered in subsequent commits so each piece can be
;;;; reviewed in isolation.

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
Signals a SIMPLE-ERROR with a human-readable message on bad input.
Stub: behaviour added in the next commit."
  (declare (ignore spec))
  (error "parse-attach-spec: not yet implemented"))

(defun set-attach-from-env ()
  "Read the CL_MCP_SLYNK_ATTACH environment variable, parse it via
`parse-attach-spec', and bind `*attach-config*'.  When the variable is
unset or empty, leave `*attach-config*' unchanged.  Stub: behaviour
added in the next commit."
  (error "set-attach-from-env: not yet implemented"))

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
