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
  (:import-from #:cl-mcp/src/log #:log-event #:*log-stream* #:*log-lock*)
  (:import-from #:usocket)
  (:import-from #:yason)
  (:import-from #:cl-mcp/src/utils/random #:generate-random-hex-string)
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
           #:worker-crashed-reason
           #:worker-spawn-failed
           #:+max-json-line-bytes+
           #:%read-line-limited
           #:worker-crash-history-pushed-p
           #:*reaper-threads*
           #:*reaper-threads-lock*
           #:signal-worker-terminate))

(in-package #:cl-mcp/src/worker-client)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defconstant +worker-protocol-version+ 1
  "Expected worker protocol version.  Logs a warning on mismatch
but does not hard-fail for forward compatibility.")

(defparameter *worker-startup-timeout* 30
  "Maximum seconds to wait for a worker handshake after launch.")

(defvar *worker-id-counter* 0
  "Monotonically increasing worker ID counter.")

(defvar *worker-id-lock* (bt:make-lock "worker-id-lock")
  "Lock protecting *worker-id-counter*.")

(defvar *reaper-threads* nil
  "List of active reaper threads spawned by %mark-worker-crashed.
Each thread terminates and self-removes after reaping its process.")

(defvar *reaper-threads-lock* (bt:make-lock "reaper-threads-lock")
  "Lock protecting *reaper-threads*.")

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition worker-crashed (error)
  ((worker :initarg :worker :reader worker-crashed-worker)
   (reason :initarg :reason :reader worker-crashed-reason
           :initform "unknown"))
  (:report (lambda (c s)
             (format s "Worker ~A (PID ~A) ~A"
                     (worker-id (worker-crashed-worker c))
                     (worker-pid (worker-crashed-worker c))
                     (let ((r (worker-crashed-reason c)))
                       (if (string= r "timeout")
                           "timed out"
                           (format nil "crashed (~A)" r)))))))

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
;;; Message size limit
;;; ---------------------------------------------------------------------------

(defconstant +max-json-line-bytes+ (* 16 1024 1024)
  "Maximum bytes for a single JSON-RPC line (16 MB).
Guards against memory exhaustion from malformed or malicious input.")

(defun %read-line-limited (stream eof-value limit)
  "Read a line from STREAM up to LIMIT characters.
Returns the line as a string, or EOF-VALUE on end-of-file.
Signals an error if the line exceeds LIMIT characters.
Handles both LF and CRLF line endings."
  (let ((buf (make-array 256 :element-type 'character
                             :adjustable t :fill-pointer 0))
        (count 0))
    (loop (let ((ch (read-char stream nil nil)))
            (cond
              ((null ch)
               (return (if (zerop count) eof-value buf)))
              ((char= ch #\Newline)
               (return buf))
              ((char= ch #\Return)
               ;; Skip CR in CRLF
               nil)
              (t
               (incf count)
               (when (> count limit)
                 (error "JSON-RPC line exceeds ~D byte limit" limit))
               (vector-push-extend ch buf)))))))

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
  (stream-lock (make-lock "worker-stream-lock"))
  (tcp-port nil)
  (swank-port nil)
  (pid nil)
  (needs-reset-notification nil :type boolean)
  (session-id nil)
  (request-counter 0 :type integer)
  (stderr-thread nil)
  (crash-history-pushed-p nil :type boolean))

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

(defun %build-environment (secret id)
  "Build an environment list for the child process.
Inherits the parent environment, sets MCP_PROJECT_ROOT to the
current *project-root* value, passes the shared secret for
TCP authentication, and sets MCP_WORKER_ID for log context."
  (let* ((current-env (sb-ext:posix-environ))
         (filtered
          (remove-if
           (lambda (s)
             (or
              (and (>= (length s) 17) (string= "MCP_PROJECT_ROOT=" s :end2 17))
              (and (>= (length s) 18)
                   (string= "MCP_WORKER_SECRET=" s :end2 18))
              (and (>= (length s) 20)
                   (string= "MCP_NO_WORKER_POOL=" s :end2 20))
              (and (>= (length s) 14)
                   (string= "MCP_WORKER_ID=" s :end2 14))
              (and (>= (length s) 13)
                   (string= "MCP_LOG_FILE=" s :end2 13))
              (and (>= (length s) 16)
                   (string= "MCP_PARENT_PID=" s :end2 15))))
           current-env)))
    (let ((env
           (list* (format nil "MCP_WORKER_SECRET=~A" secret)
                  (format nil "MCP_WORKER_ID=~A" id)
                  (format nil "MCP_PARENT_PID=~A" (sb-posix:getpid))
                  "MCP_NO_WORKER_POOL=1" filtered)))
      (if *project-root*
          (cons (format nil "MCP_PROJECT_ROOT=~A" (namestring *project-root*))
                env)
          env))))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — process launch
;;; ---------------------------------------------------------------------------

(defun %roswell-p ()
  "Return T when running under Roswell."
  (and (member :ros.init *features*) t))

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

(defvar *cached-sbcl-path* nil
  "Cached result of %find-sbcl-path.")

(defun %find-sbcl-path ()
  "Locate the sbcl executable. Uses argv[0] of the running process first,
falls back to 'which sbcl'."
  (or *cached-sbcl-path*
      (setf *cached-sbcl-path*
            (or (let ((argv0 (first sb-ext:*posix-argv*)))
                  (when (and argv0 (search "sbcl" argv0))
                    argv0))
                (handler-case
                    (let ((path (string-trim '(#\Newline #\Return #\Space)
                                  (uiop:run-program
                                   '("which" "sbcl") :output :string))))
                      (if (and path (plusp (length path))) path "sbcl"))
                  (error () "sbcl"))))))

(defun %quicklisp-setup-path ()
  "Return the absolute path to Quicklisp's setup.lisp if Quicklisp is
loaded in the current image, or NIL otherwise."
  (let ((ql-pkg (find-package :quicklisp)))
    (when ql-pkg
      (let ((sym (find-symbol "*QUICKLISP-HOME*" ql-pkg)))
        (when (and sym (boundp sym))
          (let ((home (symbol-value sym)))
            (when home
              (namestring (merge-pathnames "setup.lisp"
                                           (namestring home))))))))))

(defun %build-sbcl-args ()
  "Build command-line arguments for spawning a worker via bare SBCL.
Configures ASDF source registry to find cl-mcp, optionally loads
Quicklisp setup.lisp if available, loads the worker system via ASDF,
and calls the entry point."
  (let ((source-dir (namestring (asdf:system-source-directory :cl-mcp)))
        (ql-setup (%quicklisp-setup-path)))
    (append
     (list "--noinform" "--non-interactive")
     (when ql-setup
       (list "--load" ql-setup))
     (list
      "--eval" (format nil "(asdf:initialize-source-registry '(:source-registry :inherit-configuration (:tree ~S)))"
                       source-dir)
      "--eval" "(asdf:load-system :cl-mcp/src/worker/main)"
      "--eval" "(cl-mcp/src/worker/main:start)"))))

(defun %launch-worker-process (secret id)
  "Launch a worker child process via sb-ext:run-program.
Returns the sb-ext:process object with stdout and stderr as streams.
Uses ros-run when running under Roswell, bare sbcl otherwise."
  (let ((env (%build-environment secret id)))
    (if (%roswell-p)
        (let ((ros-path (%find-ros-path)))
          (sb-ext:run-program ros-path
                              (list "run" "-s" "cl-mcp/src/worker/main" "-e"
                                    "(cl-mcp/src/worker/main:start)")
                              :output :stream :error :stream :wait nil
                              :search t :environment env))
        (let ((sbcl-path (%find-sbcl-path)))
          (sb-ext:run-program sbcl-path
                              (%build-sbcl-args)
                              :output :stream :error :stream :wait nil
                              :search t :environment env)))))

;;; ---------------------------------------------------------------------------
;;; Internal helpers — handshake
;;; ---------------------------------------------------------------------------

(defun %parse-handshake-from-stream (stdout)
  "Read lines from STDOUT until a valid handshake JSON line is found.
Skips non-JSON lines.  Returns (values tcp-port swank-port pid).
Handles swank_port as NIL regardless of how the worker encoded null:
YASON < 0.8 parses JSON null as NIL, YASON >= 0.8 may return :NULL
depending on *parse-json-null-as-keyword*."
  (loop for line = (%read-line-limited stdout nil +max-json-line-bytes+)
        unless line do
          (error 'worker-spawn-failed
                 :message "Worker closed stdout before handshake")
        do (let ((json (ignore-errors (yason:parse line))))
             (when (and (hash-table-p json) (gethash "tcp_port" json))
               (let ((tcp-port (gethash "tcp_port" json))
                     (swank-port (gethash "swank_port" json))
                     (pid (gethash "pid" json)))
                 (unless (integerp tcp-port)
                   (error 'worker-spawn-failed
                          :message "Handshake tcp_port is not an integer"))
                 ;; Check protocol version (warn on mismatch, don't hard-fail)
                 (let ((version (gethash "protocol_version" json)))
                   (when (and version (integerp version)
                              (/= version +worker-protocol-version+))
                     (log-event :warn "worker.handshake.version-mismatch"
                                "expected" +worker-protocol-version+
                                "got" version)))
                 (return (values tcp-port
                                 (when (integerp swank-port) swank-port)
                                 pid)))))))

(defun %read-handshake (process timeout)
  "Read the JSON handshake line from the worker's stdout.
Returns three values: tcp-port, swank-port (or NIL), pid.
Delegates to %PARSE-HANDSHAKE-FROM-STREAM with a timeout wrapper.
Signals WORKER-SPAWN-FAILED on timeout or if stdout is closed
before a valid handshake is found."
  (let ((stdout (sb-ext:process-output process)))
    (handler-case
        (sb-ext:with-timeout timeout
          (%parse-handshake-from-stream stdout))
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
USOCKET:SOCKET-STREAM.  Uses a 10-second timeout via
SB-EXT:WITH-TIMEOUT to avoid blocking on OS TCP timeout
(60-120s) when the worker listener is not yet ready.

We intentionally avoid usocket's :timeout and :connection-timeout
keyword arguments:
  - :timeout sets SO_RCVTIMEO on the socket, causing
    SB-SYS:IO-TIMEOUT on any worker operation >10s (PR #67 bug).
  - :connection-timeout was added in usocket 0.8.x and is
    unavailable in older versions (e.g. 0.7.x via Qlot).
SB-EXT:WITH-TIMEOUT provides connect-phase timeout without
either issue."
  (handler-case
      (sb-ext:with-timeout 10
        (usocket:socket-connect host port :element-type 'character))
    (sb-ext:timeout ()
      (error "Connection to worker at ~A:~A timed out after 10s"
             host port))))

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
           (let ((line (%read-line-limited stream nil +max-json-line-bytes+)))
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
  "Start a daemon thread that reads the worker's stderr line by line
and forwards each line to the parent's *log-stream*.
Without this, the child's log output accumulates in an unread OS pipe
buffer.  Once that buffer fills (typically 64 KB on Linux), the child
blocks on every write to stderr, causing worker RPC calls to hang.
Stores the thread in the worker's stderr-thread slot so kill-worker
can clean it up."
  (let ((process (worker-process-info worker)) (wid (worker-id worker)))
    (when process
      (let ((err (sb-ext:process-error process)))
        (when err
          (setf (worker-stderr-thread worker)
                  (bordeaux-threads:make-thread
                   (lambda ()
                     (unwind-protect
                         (ignore-errors
                          (loop for line = (read-line err nil nil)
                                while line
                                do (ignore-errors
                                    (bt:with-lock-held (*log-lock*)
                                      (write-string line *log-stream*)
                                      (terpri *log-stream*))
                                    (finish-output *log-stream*))))
                       (ignore-errors (close err))))
                   :name (format nil "worker-stderr-~A" wid))))))))

(defun %generate-worker-secret ()
  "Generate a random shared secret for worker TCP authentication."
  (generate-random-hex-string 32))

(defun spawn-worker ()
  "Launch a worker child process and return a WORKER struct.
The worker is launched via Roswell, reads its JSON handshake to
discover tcp_port/swank_port/pid, connects to its TCP port,
authenticates with a shared secret, and returns the worker in
:standby state.

Signals WORKER-SPAWN-FAILED if the process cannot be started,
the handshake fails, or authentication is rejected."
  (let ((id (%next-worker-id))
        (secret (%generate-worker-secret))
        (process nil)
        (socket nil))
    (handler-case
        (progn
          (log-event :info "worker.spawning" "id" id)
          (setf process (%launch-worker-process secret id))
          (multiple-value-bind (tcp-port swank-port pid)
              (%read-handshake process *worker-startup-timeout*)
            (log-event :info "worker.handshake.received"
                       "id" id
                       "tcp_port" tcp-port
                       "swank_port" (or swank-port "none")
                       "pid" pid)
            ;; Close stdout pipe - no longer needed after handshake.
            ;; Frees up an FD that would otherwise be held open for
            ;; the entire worker lifetime.
            (ignore-errors
              (close (sb-ext:process-output process)))
            (setf socket (%connect-to-worker "127.0.0.1" tcp-port))
            ;; Authenticate with shared secret
            (let ((auth-stream (usocket:socket-stream socket)))
              (%send-json-rpc auth-stream 0 "worker/authenticate"
                              (let ((ht (make-hash-table :test 'equal)))
                                (setf (gethash "secret" ht) secret)
                                ht))
              (let ((auth-resp (%read-json-rpc-response auth-stream 0 10)))
                (unless (and (hash-table-p auth-resp)
                             (gethash "authenticated" auth-resp))
                  (error 'worker-spawn-failed
                         :message "Worker authentication failed"))))
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
  ;; Wait for the stderr drain thread to finish forwarding remaining
  ;; log output (including worker.fatal crash messages).  The worker
  ;; process is dead so the pipe's write end is closed, causing
  ;; read-line to return NIL and the thread to exit naturally.
  ;; Timeout of 1 second prevents blocking if something goes wrong.
  (let ((th (worker-stderr-thread worker)))
    (when (and th (bt:thread-alive-p th))
      (ignore-errors (bt:join-thread th :timeout 1))
      (when (bt:thread-alive-p th)
        (ignore-errors (bt:destroy-thread th)))
      (setf (worker-stderr-thread worker) nil)))
  ;; Collect the exit code before reaping.  process-status is
  ;; non-blocking; if the process already exited (typical for crashes)
  ;; the exit code is immediately available.
  (let ((process (worker-process-info worker))
        (wid (worker-id worker))
        (exit-code nil)
        (exit-status nil))
    (when process
      (ignore-errors
        (let ((status (sb-ext:process-status process)))
          (setf exit-status (string-downcase (symbol-name status)))
          (when (member status '(:exited :signaled))
            (setf exit-code (sb-ext:process-exit-code process)))))
      ;; Reap the OS process in a background thread to avoid blocking
      ;; the caller.  process-close calls waitpid internally, which
      ;; blocks if the worker process is still alive.
      (let ((reaper-thread nil))
        (setf reaper-thread
              (bt:make-thread
               (lambda ()
                 (unwind-protect
                     (progn
                       ;; SIGTERM first, then wait up to 2s, then SIGKILL if needed
                       ;; (matches kill-worker's graceful shutdown pattern)
                       (ignore-errors
                         (when (sb-ext:process-alive-p process)
                           (sb-ext:process-kill process 15)
                           (loop repeat 20
                                 while (sb-ext:process-alive-p process)
                                 do (sleep 0.1))
                           (when (sb-ext:process-alive-p process)
                             (log-event :warn "worker.reaper.sigkill" "id" wid)
                             (sb-ext:process-kill process 9)
                             (sleep 0.2))))
                       (ignore-errors (sb-ext:process-wait process nil nil))
                       (ignore-errors (sb-ext:process-close process)))
                   ;; Self-remove from reaper thread list on completion
                   (bt:with-lock-held (*reaper-threads-lock*)
                     (setf *reaper-threads*
                           (remove reaper-thread *reaper-threads*)))))
               :name (format nil "reap-worker-~A" wid)))
        ;; Register the thread before it can self-remove
        (bt:with-lock-held (*reaper-threads-lock*)
          (push reaper-thread *reaper-threads*))))
    (log-event :warn "worker.crashed"
               "id" (worker-id worker)
               "pid" (worker-pid worker)
               "exit_status" (or exit-status "unknown")
               "exit_code" (or exit-code "unknown")
               "reason" reason)))

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
      (error 'worker-crashed :worker worker :reason "already-dead"))
    (let ((id (incf (worker-request-counter worker))))
      (handler-case
          (progn
            (%send-json-rpc (worker-stream worker) id method params)
            (%read-json-rpc-response (worker-stream worker) id timeout))
        (end-of-file ()
          (%mark-worker-crashed worker "eof")
          (error 'worker-crashed :worker worker :reason "eof"))
        (sb-ext:timeout ()
          (%mark-worker-crashed worker "timeout")
          (error 'worker-crashed :worker worker :reason "timeout"))
        (stream-error ()
          (%mark-worker-crashed worker "stream-error")
          (error 'worker-crashed :worker worker :reason "stream-error"))
        (worker-rpc-error (e)
          ;; Legitimate worker-side error (e.g. "symbol not found").
          ;; Re-signal without marking the worker as crashed.
          (error e))
        (error (e)
          ;; Protocol error (parse failure, ID mismatch, etc.).
          ;; Mark worker as crashed since the stream is desynchronized.
          (%mark-worker-crashed worker
                                (format nil "protocol-error: ~A" e))
          (error 'worker-crashed :worker worker
                 :reason (format nil "protocol-error: ~A" e)))))))

;;; ---------------------------------------------------------------------------
;;; Public API — kill
;;; ---------------------------------------------------------------------------

(defun signal-worker-terminate (worker)
  "Send SIGTERM to the worker's OS process to break its TCP pipe.
Used by cancel-request before kill-worker: SIGTERM causes the
blocked read in worker-rpc to fail with stream-error, releasing
stream-lock so kill-worker can acquire it without deadlocking.
Returns T if signal was sent, NIL if process was already dead."
  (let ((process (worker-process-info worker)))
    (when (and process (sb-ext:process-alive-p process))
      (ignore-errors (sb-ext:process-kill process 15))
      t)))

(defun kill-worker (worker)
  "Terminate the worker process and clean up resources.
Closes the TCP socket under stream-lock for mutual exclusion with
concurrent worker-rpc calls.  Sends SIGTERM first, waits up to
2 seconds, then SIGKILL if still alive.  Sets state to :dead.

Also destroys the stderr drain thread to prevent leaked file
descriptors from blocking subsequent subprocess launches.

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
      ;; Wait for stderr drain thread to finish forwarding remaining
      ;; log output before destroying it.  After SIGTERM/SIGKILL the
      ;; worker's pipe write-end is closed, so read-line returns NIL
      ;; and the thread exits naturally within the timeout.
      (let ((th (worker-stderr-thread worker)))
        (when (and th (bt:thread-alive-p th))
          (ignore-errors (bt:join-thread th :timeout 1))
          (when (bt:thread-alive-p th)
            (ignore-errors (bt:destroy-thread th)))
          (setf (worker-stderr-thread worker) nil)))
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
