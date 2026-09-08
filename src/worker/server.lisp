;;;; src/worker/server.lisp
;;;;
;;;; TCP server for worker processes.  Accepts a single connection and
;;;; processes JSON-RPC requests line-by-line with pluggable method
;;;; dispatch.  Designed for child worker processes that serve exactly
;;;; one parent MCP session.

(defpackage #:cl-mcp/src/worker/server
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/utils/sanitize #:sanitize-error-message)
  (:import-from #:cl-mcp/src/utils/deadline #:leaked-threads)
  (:import-from #:cl-mcp/src/worker-client
                #:%read-line-limited
                #:+max-json-line-bytes+)
  (:import-from #:usocket)
  (:import-from #:yason)
  (:export #:worker-server
           #:make-worker-server
           #:server-port
           #:start-accept-loop
           #:stop-server
           #:register-method
           #:*worker-read-timeout*))

(in-package #:cl-mcp/src/worker/server)

(defvar *worker-read-timeout* 300
  "Seconds to wait for a JSON-RPC line from the parent before
treating the connection as dead.  Prevents zombie workers from
lingering when the parent disappears without closing the socket.")

(defstruct (worker-server (:constructor %make-worker-server))
  "A single-connection TCP server for worker processes.
Listens on an ephemeral port and dispatches JSON-RPC requests
to registered method handlers.  Requires authentication with a
shared secret before processing any tool requests."
  (listen-socket nil :type t)
  (port 0 :type integer)
  (methods (make-hash-table :test 'equal) :type hash-table)
  (running-p nil :type boolean)
  (authenticated-p nil :type boolean))

(defun make-worker-server (&key (port 0) (host "127.0.0.1"))
  "Create a worker server bound to HOST:PORT.
When PORT is 0 the OS assigns an ephemeral port.  The built-in
worker/ping method is registered automatically."
  (let* ((listener (usocket:socket-listen host port
                                          :reuse-address t
                                          :element-type 'character))
         (actual-port (usocket:get-local-port listener))
         (server (%make-worker-server :listen-socket listener
                                      :port actual-port
                                      :running-p t)))
    (register-method server "worker/ping"
                     (lambda (params)
                       (declare (ignore params))
                       (let ((ht (make-hash-table :test 'equal)))
                         (setf (gethash "pong" ht) t)
                         ht)))
    (log-event :info "worker.server.created"
               "port" actual-port)
    server))

(defun server-port (server)
  "Return the TCP port the worker SERVER is listening on."
  (worker-server-port server))

(defun register-method (server method-name handler)
  "Register HANDLER for METHOD-NAME on SERVER.
HANDLER is a function of one argument (params hash-table or NIL)
that returns a hash-table to be used as the JSON-RPC result."
  (setf (gethash method-name (worker-server-methods server)) handler)
  (log-event :debug "worker.method.registered" "method" method-name))

(defun %encode-response (obj)
  "Encode OBJ as a single-line JSON string."
  (with-output-to-string (s) (yason:encode obj s)))

(defconstant +leaked-thread-exit-code+ 70
  "Exit code a worker uses when it retires for carrying a leaked thread.
Distinct from a crash so the parent's recorded exit status tells the two
apart in the logs; the parent treats both the same way, by replacing the
worker.")

(defparameter *retire-action*
  (lambda (leaked)
    (declare (ignore leaked))
    ;; :ABORT T, because unwinding would run cleanups belonging to the very
    ;; threads this is retiring for.
    (sb-ext:exit :code +leaked-thread-exit-code+ :abort t))
  "What a worker does when it finds it is carrying a leaked thread.
Called with the list of them.  Indirected so the decision to retire can be
tested without taking the test process down: exiting is the behaviour under
test, and a test that could only assert it by dying could not assert it.")

(defun %retire-if-carrying-leaked-threads (method)
  "Retire this worker when a deadline left a thread running that is still alive.
METHOD is logged so the request that found the condition is identifiable.
Returns normally, and cheaply, when there is nothing to retire for."
  (let ((leaked (leaked-threads)))
    (when leaked
      (ignore-errors
       (log-event :error "worker.retiring.leaked-threads"
                  "method" method
                  "threads" (length leaked)
                  "names" (format nil "~{~A~^,~}"
                                  (mapcar (lambda (thread)
                                            (or (ignore-errors
                                                 (bt:thread-name thread))
                                                "unnamed"))
                                          leaked))))
      (ignore-errors (finish-output *error-output*))
      (funcall *retire-action* leaked))))

(defun %note-leaked-threads (ht)
  "Add the count of threads this worker could not stop, when there are any.

Reported alongside every response so the parent can show the condition in
pool-status during the window between a deadline giving up on a thread and the
next request, which is exactly when someone is likely to be asking what went
wrong.  Absent from ordinary responses rather than reported as zero, so the
common case is unchanged on the wire."
  (let ((leaked (length (leaked-threads))))
    (when (plusp leaked)
      (setf (gethash "leaked_threads" ht) leaked)))
  ht)

(defun %make-result (id payload)
  "Build a JSON-RPC 2.0 success response hash-table."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" ht) "2.0"
          (gethash "id" ht) id
          (gethash "result" ht) payload)
    (%note-leaked-threads ht)))

(defun %make-error (id code message)
  "Build a JSON-RPC 2.0 error response hash-table."
  (let ((err (make-hash-table :test 'equal))
        (ht (make-hash-table :test 'equal)))
    (setf (gethash "code" err) code
          (gethash "message" err) message)
    (setf (gethash "jsonrpc" ht) "2.0"
          (gethash "id" ht) id
          (gethash "error" ht) err)
    ht))

(defun %dispatch-request (server id method params)
  "Dispatch a JSON-RPC request to the registered handler.
Returns a JSON string to send back.  Rejects all requests except
worker/authenticate until the connection is authenticated with
the shared secret."
  ;; Auth gate: reject non-authentication requests before handshake.
  ;; Only enforce when MCP_WORKER_SECRET is configured (pool mode).
  (unless (or (worker-server-authenticated-p server)
              (string= method "worker/authenticate")
              (null (uiop/os:getenv "MCP_WORKER_SECRET")))
    (return-from %dispatch-request
      (%encode-response
       (%make-error id -32600 "Not authenticated"))))
  ;; Handle authentication as a built-in method
  (when (string= method "worker/authenticate")
    (let ((expected (uiop/os:getenv "MCP_WORKER_SECRET"))
           (provided (and params (gethash "secret" params))))
      (cond
        ((null expected)
         ;; No secret configured, auto-authenticate
         (setf (worker-server-authenticated-p server) t)
         (return-from %dispatch-request
           (%encode-response
            (%make-result id
              (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "authenticated" ht) t)
                ht)))))
        ((and provided (string= provided expected))
         (setf (worker-server-authenticated-p server) t)
         (return-from %dispatch-request
           (%encode-response
            (%make-result id
              (let ((ht (make-hash-table :test 'equal)))
                (setf (gethash "authenticated" ht) t)
                ht)))))
        (t
         (log-event :warn "worker.auth.failed")
         (return-from %dispatch-request
           (%encode-response
            (%make-error id -32600 "Authentication failed")))))))
  ;; A deadline that could not stop its thread leaves that thread running in
  ;; this image: holding locks it took, mutating the state later work reads,
  ;; and competing for the CPU.  The caller that hit the deadline was answered
  ;; and told so; every request after it would be served by a process that is
  ;; quietly wrong.  Retire before serving one.
  ;;
  ;; Checked here rather than remembered from when it happened, because the
  ;; thread may since have finished on its own -- LEAKED-THREADS prunes -- and
  ;; a worker that recovered should keep the session state it still holds.
  ;;
  ;; Exiting rather than answering with an error: the parent's crash handling
  ;; already replaces a worker that stops answering and tells the caller its
  ;; state was reset, whereas an error would leave this image in the pool to
  ;; fail the same way on every later request.  See *RETIRE-ACTION*.
  (%retire-if-carrying-leaked-threads method)
  ;; Normal dispatch for authenticated connections
  (let ((handler (gethash method (worker-server-methods server))))
    (if (null handler)
        (%encode-response
         (%make-error id -32601
                      (format nil "Method not found: ~A" method)))
        (handler-case
            (let ((result (funcall handler params)))
              (%encode-response (%make-result id result)))
          (serious-condition (e)
            (log-event :warn "worker.handler.error"
                       "method" method
                       "error" (princ-to-string e))
            (%encode-response
             (%make-error id -32603
                          (format nil "Internal error: ~A"
                                  (sanitize-error-message e)))))))))

(defun %process-line (server line)
  "Parse a JSON-RPC line and dispatch to the appropriate handler.
Returns a JSON string response, or NIL for notifications."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (when (string= trimmed "")
      (return-from %process-line nil))
    (let ((msg (handler-case
                   (yason:parse trimmed)
                 (error (e)
                   (log-event :warn "worker.parse.error"
                              "error" (princ-to-string e))
                   (return-from %process-line
                     (%encode-response
                      (%make-error nil -32700 "Parse error")))))))
      (unless (hash-table-p msg)
        (return-from %process-line
          (%encode-response
           (%make-error nil -32600 "Invalid Request"))))
      (let ((id (gethash "id" msg))
            (method (gethash "method" msg))
            (params (gethash "params" msg)))
        (cond
          ((and method id)
           (%dispatch-request server id method params))
          (method
           ;; Notification — no response
           nil)
          (t
           (%encode-response
            (%make-error id -32600 "Invalid Request"))))))))

(defun %handle-connection (server stream)
  "Read JSON-RPC lines from STREAM and write responses until EOF.
Rejects all requests except worker/authenticate until the connection
is authenticated with the shared secret."
  (loop while (worker-server-running-p server)
        for line = (handler-case
                       (sb-ext:with-timeout *worker-read-timeout*
                         (%read-line-limited stream :eof +max-json-line-bytes+))
                     (sb-ext:timeout ()
                       (log-event :debug "worker.read.idle"
                                  "seconds" *worker-read-timeout*)
                       ;; Idle timeout — keep waiting rather than closing.
                       ;; The parent manages worker lifecycle via kill-worker
                       ;; or shutdown-pool, so we just loop.
                       :idle)
                     (error (e)
                       (log-event :warn "worker.read.error"
                                  "error" (princ-to-string e))
                       :eof))
        when (eq line :eof) do (return)
        when (stringp line)
        do (let ((resp (%process-line server line)))
             (when resp
               (handler-case
                   (progn
                     (write-line resp stream)
                     (force-output stream))
                 (stream-error (e)
                   (log-event :warn "worker.write.error"
                              "error" (princ-to-string e))
                   (return)))))))

(defun start-accept-loop (server)
  "Accept a single connection on SERVER and process requests.
Blocks until the client disconnects or STOP-SERVER is called.
Designed to be run in a dedicated thread."
  (let ((listener (worker-server-listen-socket server)))
    (log-event :info "worker.accept.waiting"
               "port" (worker-server-port server))
    (let ((client (handler-case
                      (usocket:socket-accept listener
                                            :element-type 'character)
                    (error (e)
                      (unless (not (worker-server-running-p server))
                        (log-event :warn "worker.accept.error"
                                   "error" (princ-to-string e)))
                      nil))))
      (when client
        ;; Close the listener socket — worker only serves one connection.
        ;; Prevents the port from remaining open while serving.
        (ignore-errors (usocket:socket-close listener))
        (setf (worker-server-listen-socket server) nil)
        (log-event :info "worker.accept.connected"
                   "port" (worker-server-port server))
        (unwind-protect
             (%handle-connection server (usocket:socket-stream client))
          (ignore-errors (usocket:socket-close client))
          (log-event :info "worker.connection.closed"
                     "port" (worker-server-port server)))))))

(defun stop-server (server)
  "Stop the worker SERVER.  Closes the listener socket and signals
the accept/read loop to exit."
  (setf (worker-server-running-p server) nil)
  (let ((sock (worker-server-listen-socket server)))
    (when sock
      (ignore-errors (usocket:socket-close sock))
      (setf (worker-server-listen-socket server) nil)))
  (log-event :info "worker.server.stopped"
             "port" (worker-server-port server)))
