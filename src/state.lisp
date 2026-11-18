;;;; src/state.lisp
;;;; Server state class - extracted to avoid circular dependencies

(defpackage #:cl-mcp/src/state
  (:use #:cl)
  (:import-from #:alexandria #:starts-with-subseq)
  (:import-from #:bordeaux-threads #:make-lock)
  (:import-from #:cl-mcp/src/utils/random #:generate-random-hex-string)
  (:export
   #:make-dedicated-repl-id
   #:dedicated-repl-id-p
   #:server-state
   #:initialized-p
   #:client-info
   #:protocol-version
   #:make-state
   #:repl-sessions
   #:repl-sessions-lock
   #:*transport-session-id*
   #:*current-session-id*))

(in-package #:cl-mcp/src/state)

(defclass server-state ()
  ((initialized-p :initform nil :accessor initialized-p)
   (client-info :initform nil :accessor client-info)
   (protocol-version :initform nil :accessor protocol-version)
   (repl-sessions :initform (make-hash-table :test 'equal) :accessor repl-sessions)
   (repl-sessions-lock :initform (make-lock "repl-sessions") :reader repl-sessions-lock))
  (:documentation "Holds per-session state for an MCP connection."))

(defun make-state ()
  "Create a fresh server-state instance."
  (make-instance 'server-state))

(defvar *current-session-id* nil
  "Bound by each transport to the current session identifier string.
Used by the proxy layer to route tool calls to the correct worker
process.  Each transport binds this around request processing:
  - HTTP: per-request from Mcp-Session-Id header
  - TCP: per-connection from a unique connection ID
  - Stdio: fixed string \"stdio\" (single session)")

(defvar *transport-session-id* nil
  "The original MCP session ID while a tool call uses a dedicated REPL.
Requests remain cancellable by the transport that submitted them.")

(defparameter *dedicated-repl-id-prefix* "repl-"
  "Reserved prefix for IDs of dedicated REPL workers.")

(defconstant +dedicated-repl-id-bytes+ 16
  "The number of random bytes encoded in a dedicated REPL worker ID.")

(defun make-dedicated-repl-id ()
  "Return a fresh ID in the dedicated REPL worker namespace."
  (concatenate 'string *dedicated-repl-id-prefix*
               (generate-random-hex-string +dedicated-repl-id-bytes+)))

(defun dedicated-repl-id-p (session-id)
  "Return true when SESSION-ID identifies a dedicated REPL worker."
  (let ((prefix-length (length *dedicated-repl-id-prefix*)))
    (and (stringp session-id)
         (= (length session-id) (+ prefix-length (* 2 +dedicated-repl-id-bytes+)))
         (starts-with-subseq *dedicated-repl-id-prefix* session-id)
         (loop for index from prefix-length below (length session-id)
               always (digit-char-p (char session-id index) 16)))))
