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
           #:worker-tcp-port
           #:worker-swank-port
           #:worker-pid
           #:worker-state
           #:worker-session-id
           #:worker-id
           #:worker-needs-reset-notification
           #:clear-reset-notification
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

(defun %find-ros-path ()
  "Locate the ros executable.  Returns the absolute path as a string,
or \"ros\" if not found (relying on PATH)."
  (handler-case
      (let ((path (string-trim '(#\Newline #\Return #\Space)
                               (uiop:run-program '("which" "ros")
                                                  :output :string))))
        (if (and path (plusp (length path)))
            path
            "ros"))
    (error () "ros")))

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
Signals WORKER-SPAWN-FAILED on timeout or parse error."
  (let ((stdout (sb-ext:process-output process)))
    (handler-case
        (let ((line (sb-ext:with-timeout timeout
                      (read-line stdout nil nil))))
          (unless line
            (error 'worker-spawn-failed
                   :message "Worker closed stdout before handshake"))
          (let ((json (handler-case
                          (yason:parse line)
                        (error (e)
                          (error 'worker-spawn-failed
                                 :message (format nil
                                                  "Invalid handshake JSON: ~A"
                                                  (princ-to-string e)))))))
            (unless (hash-table-p json)
              (error 'worker-spawn-failed
                     :message "Handshake is not a JSON object"))
            (let ((tcp-port (gethash "tcp_port" json))
                  (swank-port (gethash "swank_port" json))
                  (pid (gethash "pid" json)))
              (unless (and tcp-port (integerp tcp-port))
                (error 'worker-spawn-failed
                       :message "Handshake missing tcp_port"))
              (values tcp-port
                      (if (eq swank-port :null) nil swank-port)
                      pid))))
      (sb-ext:timeout ()
        (error 'worker-spawn-failed
               :message (format nil
                                "Worker handshake timed out after ~Ds"
                                timeout))))))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — TCP connection
;;; ---------------------------------------------------------------------------

(defun %connect-to-worker (host port)
  "Open a TCP connection to the worker at HOST:PORT.
Returns the usocket object.  The stream is accessible via
USOCKET:SOCKET-STREAM."
  (usocket:socket-connect host port :element-type 'character))

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
Signals an error if the response contains a JSON-RPC error."
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
               ;; Check for error
               (let ((err (gethash "error" json)))
                 (when err
                   (error "JSON-RPC error ~A: ~A"
                          (gethash "code" err)
                          (gethash "message" err))))
               ;; Return the result
               (gethash "result" json)))))
    (if timeout
        (sb-ext:with-timeout timeout
          (do-read))
        (do-read))))

;;; ---------------------------------------------------------------------------
;;; Public API — spawn
;;; ---------------------------------------------------------------------------

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

(defun worker-rpc (worker method params &key timeout)
  "Send a JSON-RPC request to WORKER and return the result hash-table.
TIMEOUT, when non-NIL, is the maximum seconds to wait for a response.

Signals WORKER-CRASHED if the worker process has died (EOF on stream).
The worker's state is set to :crashed and needs-reset-notification is
flagged."
  (bt:with-lock-held ((worker-stream-lock worker))
    (let ((id (incf (worker-request-counter worker))))
      (handler-case
          (progn
            (%send-json-rpc (worker-stream worker) id method params)
            (%read-json-rpc-response (worker-stream worker) id timeout))
        (end-of-file ()
          (setf (worker-state worker) :crashed)
          (setf (worker-needs-reset-notification worker) t)
          (log-event :warn "worker.crashed"
                     "id" (worker-id worker)
                     "pid" (worker-pid worker))
          (error 'worker-crashed :worker worker))))))

;;; ---------------------------------------------------------------------------
;;; Public API — kill
;;; ---------------------------------------------------------------------------

(defun kill-worker (worker)
  "Terminate the worker process and clean up resources.
Sends SIGTERM first, waits up to 2 seconds, then SIGKILL if still
alive.  Closes the TCP socket and sets state to :dead.

Robust against already-dead processes."
  (let ((process (worker-process-info worker))
        (socket (worker-socket worker)))
    (log-event :info "worker.killing"
               "id" (worker-id worker)
               "pid" (worker-pid worker))
    ;; Terminate the process
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
    ;; Close TCP socket
    (when socket
      (ignore-errors (usocket:socket-close socket))
      (setf (worker-socket worker) nil)
      (setf (worker-stream worker) nil))
    ;; Update state
    (setf (worker-state worker) :dead)
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
