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
                #:log-event)
  (:export #:start))

(in-package #:cl-mcp/src/worker/main)

(defun %get-pid ()
  "Return the current process ID as an integer."
  #+sbcl (sb-posix:getpid)
  #-sbcl (or (ignore-errors
               (funcall (find-symbol "GETPID" "SB-POSIX")))
             0))

(defun %try-start-swank ()
  "Attempt to load Swank and start a Swank server on an ephemeral port.
Returns the actual Swank port number on success, or NIL if Swank is
unavailable or fails to start.  Never signals an error."
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
          (gethash "pid" ht) (%get-pid))
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

(defun start ()
  "Entry point for worker child processes.
Creates the TCP server on an ephemeral port, registers all method
handlers, optionally starts a Swank server for human observation,
outputs a JSON handshake line to stdout, and blocks in the accept
loop waiting for the parent to connect.

This function does not return until the connection is closed or the
server is stopped."
  (%setup-project-root)
  (let* ((server (make-worker-server :port 0))
         (tcp-port (server-port server)))
    (register-all-handlers server)
    (let ((swank-port (%try-start-swank)))
      (log-event :info "worker.starting"
                 "tcp_port" tcp-port
                 "swank_port" (or swank-port "none")
                 "pid" (%get-pid))
      (%output-handshake tcp-port swank-port)
      (start-accept-loop server))))
