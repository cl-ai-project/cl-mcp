;;;; src/worker/main.lisp
;;;;
;;;; Worker process entry point.  Starts the TCP server, optionally
;;;; loads Swank for human observation, reports ports to stdout as
;;;; JSON, and enters the serve loop.
;;;;
;;;; The parent process spawns a worker via uiop:launch-program and
;;;; reads the first line of stdout to discover the TCP and Swank
;;;; ports.  After the handshake line, stdout is not used again.

(defpackage #:cl-mcp/src/worker/main
  (:use #:cl)
  (:import-from #:cl-mcp/src/worker/server
                #:make-worker-server
                #:server-port
                #:start-accept-loop)
  (:import-from #:cl-mcp/src/worker/handlers
                #:register-all-handlers)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log
                #:log-event
                #:*log-context*)
  (:export #:start))

(in-package #:cl-mcp/src/worker/main)

(defconstant +worker-protocol-version+ 1
  "Worker protocol version for handshake compatibility checking.")

(defun %get-pid ()
  "Return the current process ID as an integer."
  #+sbcl (sb-posix:getpid)
  #-sbcl (or (ignore-errors
               (funcall (find-symbol "GETPID" "SB-POSIX")))
             0))

(defun %try-start-swank ()
  "Attempt to load Swank and start a Swank server on an ephemeral port.
Only starts when MCP_WORKER_SWANK environment variable is set to a
non-empty value (opt-in).  Returns the actual Swank port number on
success, or NIL if Swank is disabled, unavailable, or fails to start.
Never signals an error."
  (let ((env-val (uiop/os:getenv "MCP_WORKER_SWANK")))
    (unless (and env-val (plusp (length env-val)))
      (log-event :debug "worker.swank.skip" "reason" "MCP_WORKER_SWANK not set")
      (return-from %try-start-swank nil)))
  (handler-case
      (let ((ql-sym (find-symbol "QUICKLOAD" "QL")))
        (unless ql-sym
          (log-event :warn "worker.swank.skip" "reason" "QL package not found")
          (return-from %try-start-swank nil))
        (funcall ql-sym :swank :silent t)
        (let ((create-sym (find-symbol "CREATE-SERVER" "SWANK")))
          (unless create-sym
            (log-event :warn "worker.swank.skip"
                       "reason" "SWANK:CREATE-SERVER not found")
            (return-from %try-start-swank nil))
          (let ((port (funcall create-sym :port 0 :dont-close t)))
            (log-event :info "worker.swank.started" "port" port)
            port)))
    (error (e)
      (log-event :warn "worker.swank.failed"
                 "error" (princ-to-string e))
      nil)))

(defun %output-handshake (tcp-port swank-port
                          &optional (stream *standard-output*))
  "Write the JSON handshake line to STREAM.
The parent process reads this single line to discover the worker's
TCP port, optional Swank port, and PID.  The JSON object has keys:
  tcp_port   - integer, the worker's JSON-RPC TCP port
  swank_port - integer or null, the Swank server port
  pid        - integer, the worker's OS process ID"
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "tcp_port" ht) tcp-port
          (gethash "swank_port" ht) (or swank-port :null)
          (gethash "pid" ht) (%get-pid)
          (gethash "protocol_version" ht) +worker-protocol-version+)
    (yason:encode ht stream)
    (terpri stream)
    (force-output stream)))

(defun %setup-project-root ()
  "Read MCP_PROJECT_ROOT from the environment and configure the worker.
Sets *project-root* and changes the working directory.  Returns the
pathname on success, NIL if the environment variable is not set or
the directory does not exist."
  (let ((env-root (uiop/os:getenv "MCP_PROJECT_ROOT")))
    (when (and env-root (plusp (length env-root)))
      (let ((dir (uiop/pathname:ensure-directory-pathname env-root)))
        (if (uiop/filesystem:directory-exists-p dir)
            (progn
              (setf *project-root* dir)
              (uiop/os:chdir dir)
              (log-event :info "worker.project-root.set"
                         "path" (namestring dir))
              dir)
            (progn
              (log-event :warn "worker.project-root.invalid"
                         "path" env-root)
              nil))))))

(defun %install-signal-handlers ()
  "Install SIGTERM handler so the worker exits cleanly when the parent
shuts down the pool.  Without this, SBCL raises a condition for
SIGTERM which cascades into nested errors when stderr is a broken pipe."
  #+sbcl
  (sb-sys:enable-interrupt
   sb-posix:sigterm
   (lambda (signo context info)
     (declare (ignore signo context info))
     (ignore-errors
       (log-event :info "worker.sigterm" "pid" (%get-pid)))
     (sb-ext:exit :code 0 :abort t))))

(defun start ()
  "Entry point for worker child processes.
Creates the TCP server on an ephemeral port, registers all method
handlers, optionally starts a Swank server for human observation,
outputs a JSON handshake line to stdout, and blocks in the accept
loop waiting for the parent to connect.

After the handshake, stdout is redirected to /dev/null to prevent
accidental writes from filling the pipe buffer and causing deadlock.

When the accept loop exits (parent disconnected or server stopped),
the process exits cleanly instead of falling through to the
Roswell REPL."
  ;; Disable debugger so unhandled errors exit instead of entering
  ;; an interactive REPL that would hang or cascade on broken pipes.
  #+sbcl (sb-ext:disable-debugger)
  ;; Top-level error handler: catch ALL errors to prevent cascading
  ;; crashes when stderr is a broken pipe (parent already exited).
  (handler-case
      (progn
        (%install-signal-handlers)
        (let ((wid (uiop/os:getenv "MCP_WORKER_ID")))
          (when (and wid (plusp (length wid)))
            (setf *log-context* (list "worker_id" wid))))
        (%setup-project-root)
        (let* ((server (make-worker-server :port 0))
               (tcp-port (server-port server)))
          (register-all-handlers server)
          (let ((swank-port (%try-start-swank)))
            (log-event :info "worker.starting" "tcp_port" tcp-port
                       "swank_port" (or swank-port "none")
                       "pid" (%get-pid))
            (%output-handshake tcp-port swank-port)
            ;; Redirect stdout to /dev/null after handshake to prevent
            ;; pipe buffer deadlock — parent only reads the handshake.
            (let ((devnull (open #P"/dev/null" :direction :output
                                               :if-exists :append)))
              (setf *standard-output* devnull))
            (start-accept-loop server))))
    (serious-condition (e)
      (ignore-errors
        (log-event :error "worker.fatal" "error" (princ-to-string e)
                   "pid" (%get-pid)))))
  (sb-ext:exit :code 0 :abort t))
