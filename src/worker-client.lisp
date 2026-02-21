;;;; src/worker-client.lisp
;;;;
;;;; Parent-side module for spawning worker child processes,
;;;; communicating with them via JSON-RPC over TCP, and managing
;;;; their lifecycle (kill, restart).
;;;;
;;;; Workers are SBCL child processes launched via sb-ext:run-program.
;;;; Each worker outputs a JSON handshake on stdout containing
;;;; tcp_port, swank_port, and pid.  The parent connects to the
;;;; worker's TCP port and sends JSON-RPC requests line-by-line.

(defpackage #:cl-mcp/src/worker-client
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held)
  (:import-from #:cl-mcp/src/project-root #:*project-root*)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:usocket)
  (:import-from #:yason)
  (:export #:worker
           #:make-worker
           #:spawn-worker
           #:worker-rpc
           #:worker-rpc-error
           #:worker-tcp-port
           #:worker-swank-port
           #:worker-pid
           #:worker-state
           #:worker-session-id
           #:worker-id
           #:worker-needs-reset-notification
           #:worker-stream-lock
           #:clear-reset-notification
           #:check-and-clear-reset-notification
           #:worker-process-info
           #:kill-worker
           #:worker-crashed
           #:worker-spawn-failed))

(in-package #:cl-mcp/src/worker-client)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defparameter *worker-startup-timeout* 30
  "Maximum seconds to wait for a worker handshake after launch.")

(defvar *worker-id-counter* 0
  "Monotonically increasing worker ID counter.")

(defvar *worker-id-lock* (bt:make-lock "worker-id-lock")
  "Lock protecting *worker-id-counter*.")

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition worker-crashed (error)
  ((worker :initarg :worker :reader worker-crashed-worker))
  (:report (lambda (c s)
             (format s "Worker ~A (PID ~A) crashed"
                     (worker-id (worker-crashed-worker c))
                     (worker-pid (worker-crashed-worker c))))))

(define-condition worker-spawn-failed (error)
  ((message :initarg :message :reader worker-spawn-failed-message))
  (:report (lambda (c s)
             (format s "Failed to spawn worker: ~A"
                     (worker-spawn-failed-message c)))))

(define-condition worker-rpc-error (error)
  ((code :initarg :code :reader worker-rpc-error-code)
   (message :initarg :message :reader worker-rpc-error-message))
  (:report (lambda (c s)
             (format s "JSON-RPC error ~A: ~A"
                     (worker-rpc-error-code c)
                     (worker-rpc-error-message c))))
  (:documentation "Legitimate JSON-RPC error response from a worker handler.
Distinct from protocol errors (parse failure, ID mismatch) which
indicate stream corruption and require marking the worker crashed."))

;;; ---------------------------------------------------------------------------
;;; Worker struct
;;; ---------------------------------------------------------------------------

(defstruct worker
  "Represents a child worker process and its communication channel."
  (id nil)
  (state :dead :type keyword)
  (process-info nil)
  (stream nil)
  (socket nil)
  (stream-lock (bt:make-lock "worker-stream-lock"))
  (tcp-port nil)
  (swank-port nil)
  (pid nil)
  (needs-reset-notification nil :type boolean)
  (session-id nil)
  (request-counter 0 :type integer))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — ID generation
;;; ---------------------------------------------------------------------------

(defun %next-worker-id ()
  "Return the next monotonically increasing worker ID."
  (bt:with-lock-held (*worker-id-lock*)
    (incf *worker-id-counter*)))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — environment
;;; ---------------------------------------------------------------------------

(defun %build-environment ()
  "Build an environment list for the child process.
Inherits the parent environment and sets MCP_PROJECT_ROOT to the
current *project-root* value."
  (let* ((current-env (sb-ext:posix-environ))
         (filtered (remove-if
                    (lambda (s)
                      (and (>= (length s) 17)
                           (string= "MCP_PROJECT_ROOT=" s
                                    :end2 17)))
                    current-env)))
    (if *project-root*
        (cons (format nil "MCP_PROJECT_ROOT=~A"
                      (namestring *project-root*))
              filtered)
        filtered)))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — process launch
;;; ---------------------------------------------------------------------------

(defvar *cached-ros-path* nil
  "Cached result of %find-ros-path to avoid repeated subprocess forks.")

(defun %find-ros-path ()
  "Locate the ros executable.  Returns the absolute path as a string,
or \"ros\" if not found (relying on PATH).  Caches the result after
the first successful lookup."
  (or *cached-ros-path*
      (setf *cached-ros-path*
            (handler-case
                (let ((path (string-trim '(#\Newline #\Return #\Space)
                                         (uiop:run-program '("which" "ros")
                                                            :output :string))))
                  (if (and path (plusp (length path)))
                      path
                      "ros"))
              (error () "ros")))))

(defun %launch-worker-process ()
  "Launch a worker child process via sb-ext:run-program.
Returns the sb-ext:process object with stdout and stderr as streams."
  (let ((ros-path (%find-ros-path))
        (env (%build-environment)))
    (sb-ext:run-program ros-path
                        (list "run"
                              "-s" "cl-mcp/src/worker/main"
                              "-e" "(cl-mcp/src/worker/main:start)")
                        :output :stream
                        :error :stream
                        :wait nil
                        :search t
                        :environment env)))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — handshake
;;; ---------------------------------------------------------------------------

(defun %read-handshake (process timeout)
  "Read the JSON handshake line from the worker's stdout.
Returns three values: tcp-port, swank-port (or NIL), pid.
Skips non-JSON lines (e.g. compiler output on first run without
FASL cache) by trying each line as JSON and looking for the
tcp_port key.  Signals WORKER-SPAWN-FAILED on timeout or if
stdout is closed before a valid handshake is found."
  (let ((stdout (sb-ext:process-output process)))
    (handler-case
        (sb-ext:with-timeout timeout
          (loop for line = (read-line stdout nil nil)
                unless line do
                  (error 'worker-spawn-failed
                         :message "Worker closed stdout before handshake")
                do (let ((json (ignore-errors (yason:parse line))))
                     (when (and (hash-table-p json)
                                (gethash "tcp_port" json))
                       (let ((tcp-port (gethash "tcp_port" json))
                             (swank-port (gethash "swank_port" json))
                             (pid (gethash "pid" json)))
                         (unless (integerp tcp-port)
                           (error 'worker-spawn-failed
                                  :message "Handshake tcp_port is not an integer"))
                         (return (values tcp-port
                                         (if (eq swank-port :null) nil swank-port)
                                         pid)))))))
      (sb-ext:timeout ()
        (error 'worker-spawn-failed
               :message (format nil "Worker handshake timed out after ~Ds"
                                timeout))))))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — TCP connection
;;; ---------------------------------------------------------------------------

(defun %connect-to-worker (host port)
  "Open a TCP connection to the worker at HOST:PORT.
Returns the usocket object.  The stream is accessible via
USOCKET:SOCKET-STREAM.  Times out after 10 seconds to avoid
blocking on OS TCP timeout (60-120s) when the worker listener
is not yet ready."
  (usocket:socket-connect host port
                          :element-type 'character
                          :timeout 10))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — JSON-RPC
;;; ---------------------------------------------------------------------------

(defun %send-json-rpc (stream id method params)
  "Write a JSON-RPC 2.0 request to STREAM as a single line."
  (let ((req (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" req) "2.0"
          (gethash "id" req) id
          (gethash "method" req) method)
    (when params
      (setf (gethash "params" req) params))
    (let ((json-line (with-output-to-string (s)
                       (yason:encode req s))))
      (write-line json-line stream)
      (force-output stream))))

(defun %read-json-rpc-response (stream id timeout)
  "Read a JSON-RPC 2.0 response from STREAM matching ID.
When TIMEOUT is non-NIL, signals SB-EXT:TIMEOUT after that many
seconds.  Returns the parsed JSON hash-table on success.
Signals WORKER-RPC-ERROR for legitimate JSON-RPC error responses
from the worker handler.  Signals SIMPLE-ERROR for protocol-level
failures (parse errors, ID mismatches) which indicate stream
corruption."
  (flet ((do-read ()
           (let ((line (read-line stream nil nil)))
             (unless line
               (error 'end-of-file :stream stream))
             (let ((json (yason:parse line)))
               (unless (hash-table-p json)
                 (error "Invalid JSON-RPC response: not an object"))
               ;; Verify ID matches
               (let ((resp-id (gethash "id" json)))
                 (unless (eql resp-id id)
                   (error "JSON-RPC response ID mismatch: expected ~A, got ~A"
                          id resp-id)))
               ;; Check for error — signal typed condition for worker errors
               (let ((err (gethash "error" json)))
                 (when err
                   (error 'worker-rpc-error
                          :code (gethash "code" err)
                          :message (gethash "message" err))))
               ;; Return the result
               (gethash "result" json)))))
    (if timeout
        (sb-ext:with-timeout timeout
          (do-read))
        (do-read))))

;;; ---------------------------------------------------------------------------
;;; Public API — spawn
;;; ---------------------------------------------------------------------------

(defun %start-stderr-drain (worker)
  "Start a daemon thread that drains the worker's stderr pipe.
Without this, the child's log output (written to *error-output*)
accumulates in an unread OS pipe buffer.  Once that buffer fills
(typically 64 KB on Linux), the child blocks on every write to
stderr, causing worker RPC calls to hang or time out."
  (let ((process (worker-process-info worker))
        (wid (worker-id worker)))
    (when process
      (let ((err (sb-ext:process-error process)))
        (when err
          (bordeaux-threads:make-thread
           (lambda ()
             (unwind-protect
                 (ignore-errors
                  (let ((buf (make-string 4096)))
                    (loop for n = (read-sequence buf err)
                          while (plusp n))))
               (ignore-errors (close err))))
           :name (format nil "worker-stderr-~A" wid)))))))

(defun spawn-worker ()
  "Launch a worker child process and return a WORKER struct.
The worker is launched via Roswell, reads its JSON handshake to
discover tcp_port/swank_port/pid, connects to its TCP port, and
returns the worker in :standby state.

Signals WORKER-SPAWN-FAILED if the process cannot be started or
the handshake fails."
  (let ((id (%next-worker-id))
        (process nil)
        (socket nil))
    (handler-case
        (progn
          (log-event :info "worker.spawning" "id" id)
          (setf process (%launch-worker-process))
          (multiple-value-bind (tcp-port swank-port pid)
              (%read-handshake process *worker-startup-timeout*)
            (log-event :info "worker.handshake.received"
                       "id" id
                       "tcp_port" tcp-port
                       "swank_port" (or swank-port "none")
                       "pid" pid)
            (setf socket (%connect-to-worker "127.0.0.1" tcp-port))
            (let ((worker (make-worker
                           :id id
                           :state :standby
                           :process-info process
                           :stream (usocket:socket-stream socket)
                           :socket socket
                           :tcp-port tcp-port
                           :swank-port swank-port
                           :pid pid)))
              (%start-stderr-drain worker)
              (log-event :info "worker.spawned"
                         "id" id
                         "tcp_port" tcp-port
                         "pid" pid)
              worker)))
      (error (e)
        ;; Clean up on failure
        (when socket
          (ignore-errors (usocket:socket-close socket)))
        (when process
          (ignore-errors
            (when (sb-ext:process-alive-p process)
              (sb-ext:process-kill process 15)
              (sleep 0.5)
              (when (sb-ext:process-alive-p process)
                (sb-ext:process-kill process 9)))
            (sb-ext:process-close process)))
        (log-event :warn "worker.spawn.failed"
                   "id" id
                   "error" (princ-to-string e))
        (if (typep e 'worker-spawn-failed)
            (error e)
            (error 'worker-spawn-failed
                   :message (princ-to-string e)))))))

;;; ---------------------------------------------------------------------------
;;; Public API — RPC
;;; ---------------------------------------------------------------------------

(defun %mark-worker-crashed (worker reason)
  "Mark WORKER as crashed, set reset notification flag, close its
stream to prevent further use, and log the event.
Process reaping (waitpid) is deferred to a background thread to
avoid blocking the caller, which typically holds stream-lock.
Returns nothing."
  (setf (worker-state worker) :crashed)
  (setf (worker-needs-reset-notification worker) t)
  ;; Close the stream/socket to prevent stale-response corruption.
  ;; The next RPC attempt will see :crashed state before trying I/O.
  (ignore-errors
    (when (worker-socket worker)
      (usocket:socket-close (worker-socket worker))
      (setf (worker-socket worker) nil
            (worker-stream worker) nil)))
  ;; Reap the OS process in a background thread to avoid blocking
  ;; the caller.  process-close calls waitpid internally, which blocks
  ;; if the worker process is still alive (e.g. stuck in computation).
  (let ((process (worker-process-info worker))
        (wid (worker-id worker)))
    (when process
      (bt:make-thread
       (lambda ()
         (ignore-errors (sb-ext:process-close process)))
       :name (format nil "reap-worker-~A" wid))))
  (log-event :warn "worker.crashed"
             "id" (worker-id worker)
             "pid" (worker-pid worker)
             "reason" reason))

(defun worker-rpc (worker method params &key timeout)
  "Send a JSON-RPC request to WORKER and return the result hash-table.
TIMEOUT, when non-NIL, is the maximum seconds to wait for a response.

Signals WORKER-CRASHED if the worker process has died (EOF on stream),
timed out (sb-ext:timeout), encountered a stream/socket error, or if
the stream is already NIL (e.g. marked crashed by a concurrent thread).
Also signals WORKER-CRASHED for protocol errors (JSON parse failure,
response ID mismatch) which indicate the stream is desynchronized.

Signals WORKER-RPC-ERROR for legitimate JSON-RPC error responses from
the worker handler (e.g. \"symbol not found\").  These are re-signaled
without marking the worker as crashed."
  (bt:with-lock-held ((worker-stream-lock worker))
    (unless (worker-stream worker)
      (error 'worker-crashed :worker worker))
    (let ((id (incf (worker-request-counter worker))))
      (handler-case
          (progn
            (%send-json-rpc (worker-stream worker) id method params)
            (%read-json-rpc-response (worker-stream worker) id timeout))
        (end-of-file ()
          (%mark-worker-crashed worker "eof")
          (error 'worker-crashed :worker worker))
        (sb-ext:timeout ()
          (%mark-worker-crashed worker "timeout")
          (error 'worker-crashed :worker worker))
        (stream-error ()
          (%mark-worker-crashed worker "stream-error")
          (error 'worker-crashed :worker worker))
        (worker-rpc-error (e)
          ;; Legitimate worker-side error (e.g. "symbol not found").
          ;; Re-signal without marking the worker as crashed.
          (error e))
        (error (e)
          ;; Protocol error (parse failure, ID mismatch, etc.).
          ;; Mark worker as crashed since the stream is desynchronized.
          (%mark-worker-crashed worker
                                (format nil "protocol-error: ~A" e))
          (error 'worker-crashed :worker worker))))))

;;; ---------------------------------------------------------------------------
;;; Public API — kill
;;; ---------------------------------------------------------------------------

(defun kill-worker (worker)
  "Terminate the worker process and clean up resources.
Closes the TCP socket under stream-lock for mutual exclusion with
concurrent worker-rpc calls.  Sends SIGTERM first, waits up to
2 seconds, then SIGKILL if still alive.  Sets state to :dead.

Robust against already-dead processes."
  (let ((process (worker-process-info worker)))
    (log-event :info "worker.killing"
               "id" (worker-id worker)
               "pid" (worker-pid worker))
    ;; Close TCP socket under stream-lock first, so any concurrent
    ;; worker-rpc sees the closure as a stream-error rather than
    ;; racing on the file descriptor.
    (bt:with-lock-held ((worker-stream-lock worker))
      (let ((socket (worker-socket worker)))
        (when socket
          (ignore-errors (usocket:socket-close socket))
          (setf (worker-socket worker) nil
                (worker-stream worker) nil)))
      (setf (worker-state worker) :dead))
    ;; Terminate the OS process outside the lock (may block up to ~2.2s)
    (when process
      (handler-case
          (when (sb-ext:process-alive-p process)
            ;; SIGTERM
            (sb-ext:process-kill process 15)
            ;; Wait up to 2 seconds
            (loop repeat 20
                  while (sb-ext:process-alive-p process)
                  do (sleep 0.1))
            ;; SIGKILL if still alive
            (when (sb-ext:process-alive-p process)
              (log-event :warn "worker.sigkill"
                         "id" (worker-id worker)
                         "pid" (worker-pid worker))
              (sb-ext:process-kill process 9)
              (sleep 0.2)))
        (error (e)
          (log-event :warn "worker.kill.error"
                     "id" (worker-id worker)
                     "error" (princ-to-string e))))
      (ignore-errors (sb-ext:process-close process)))
    (log-event :info "worker.killed"
               "id" (worker-id worker)
               "pid" (worker-pid worker))
    worker))

;;; ---------------------------------------------------------------------------
;;; Public API — utility
;;; ---------------------------------------------------------------------------

(defun clear-reset-notification (worker)
  "Clear the needs-reset-notification flag on WORKER."
  (setf (worker-needs-reset-notification worker) nil))

(defun check-and-clear-reset-notification (worker)
  "Atomically check and clear the needs-reset-notification flag.
Returns T if the flag was set (and is now cleared), NIL otherwise.
Uses stream-lock for mutual exclusion with concurrent callers,
preventing the TOCTOU race where two threads both see the flag
as set and both return crash notifications."
  (bt:with-lock-held ((worker-stream-lock worker))
    (when (worker-needs-reset-notification worker)
      (setf (worker-needs-reset-notification worker) nil)
      t)))
