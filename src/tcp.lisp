;;;; src/tcp.lisp

(defpackage #:cl-mcp/src/tcp
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/protocol #:make-state #:process-json-line)
  (:import-from #:bordeaux-threads #:thread-alive-p #:make-thread #:destroy-thread #:join-thread)
  (:import-from #:usocket)
  (:export
   #:*tcp-server-thread*
   #:*tcp-server-port*
   #:*tcp-read-timeout*
   #:*tcp-accept-timeout*
   #:tcp-server-running-p
   #:start-tcp-server-thread
   #:ensure-tcp-server-thread
   #:stop-tcp-server-thread
   #:serve-tcp))

(in-package #:cl-mcp/src/tcp)

(defparameter *tcp-server-thread* nil
  "Background TCP server thread created by START-TCP-SERVER-THREAD.")

(defparameter *tcp-server-port* nil
  "Port number of the currently running background TCP server.")

(defparameter *tcp-read-timeout* 10.0
  "Seconds to wait for data on a client connection before timing out.")

(defparameter *tcp-accept-timeout* 5.0
  "Seconds to wait for an incoming connection before re-checking stop flags.")

(defparameter *tcp-listener* nil
  "Current listener socket, if any.")

(defparameter *tcp-stop-flag* nil
  "Flag set to request the TCP server loop to stop gracefully.")

(defparameter *tcp-conn-counter* 0
  "Monotonic counter used to tag connections in logs.")

(defun %fd-count ()
  (ignore-errors (length (directory #P"/proc/self/fd/*"))))

(defun tcp-server-running-p ()
  "Return T when a background TCP server thread is alive."
  (and *tcp-server-thread*
       (bordeaux-threads:thread-alive-p *tcp-server-thread*)))

(declaim (ftype (function (&key (:host string) (:port (or integer null))
                                (:accept-once t) (:on-listening (or null function)))
                          (values t (or integer null) &optional))
                start-tcp-server-thread))
(defun start-tcp-server-thread (&key (host "127.0.0.1") (port 0)
                                     (accept-once t) on-listening)
  "Start the TCP MCP server on a dedicated thread.
Returns the thread object and the bound PORT once the listener is up."
  (when (and *tcp-server-thread*
             (not (bordeaux-threads:thread-alive-p *tcp-server-thread*)))
    (setf *tcp-server-thread* nil
          *tcp-server-port* nil))
  (when (tcp-server-running-p)
    (log-event :info "tcp.thread.already-running" "port" *tcp-server-port*)
    (return-from start-tcp-server-thread (values *tcp-server-thread* *tcp-server-port*)))
  (let ((actual-port nil))
    (setf *tcp-stop-flag* nil)
    (log-event :info "tcp.thread.start" "host" host "port" port "accept-once" accept-once)
    (setf *tcp-server-thread*
          (bordeaux-threads:make-thread
           (lambda ()
             (serve-tcp :host host
                        :port port
                        :accept-once accept-once
                        :on-listening (lambda (p)
                                        (setf actual-port p)
                                        (setf *tcp-server-port* p)
                                        (when on-listening (funcall on-listening p))))
             (log-event :info "tcp.thread.exit"))
           :name "mcp-tcp-server"))
    (loop repeat 200
          until (or actual-port (not (tcp-server-running-p)))
          do (sleep 0.01))
    (values *tcp-server-thread* *tcp-server-port*)))

(declaim (ftype (function (&key (:host string)
                                (:port (or integer null))
                                (:accept-once t)
                                (:on-listening (or null function)))
                          (values (member :already-running :started nil) &optional))
                ensure-tcp-server-thread))
(defun ensure-tcp-server-thread (&key (host "127.0.0.1") (port 0)
                                      (accept-once nil) on-listening)
  "Ensure a background TCP server thread is running.
Returns :already-running when one is alive, :started when a new one was
successfully started, or NIL if the start attempt failed."
  (if (tcp-server-running-p)
      (progn
        (when (and on-listening *tcp-server-port*)
          (funcall on-listening *tcp-server-port*))
        :already-running)
      (progn
        (multiple-value-bind (thr started-port)
            (start-tcp-server-thread :host host :port port
                                     :accept-once accept-once
                                     :on-listening on-listening)
          (declare (ignore thr))
          (when started-port :started)))))

(declaim (ftype (function () (values (member nil :stopped) &optional))
                stop-tcp-server-thread))
(defun stop-tcp-server-thread ()
  "Stop the background TCP server thread if it is running."
  (when (tcp-server-running-p)
    (log-event :info "tcp.thread.stop" "port" *tcp-server-port*)
    (setf *tcp-stop-flag* t)
    ;; Nudge the accept loop by making a loopback connection so wait-for-input/accept unblocks.
    (when *tcp-server-port*
      (ignore-errors
       (let ((sock (usocket:socket-connect "127.0.0.1" *tcp-server-port*
                                           :timeout 1.0
                                           :element-type 'character)))
         (when sock
           (ignore-errors (usocket:socket-close sock))))))
    (when *tcp-listener*
      (ignore-errors (usocket:socket-close *tcp-listener*)))
    (when *tcp-server-thread*
      (bordeaux-threads:join-thread *tcp-server-thread*)
      (when (bordeaux-threads:thread-alive-p *tcp-server-thread*)
        ;; Fall back to destroy-thread if it refused to stop.
        (bordeaux-threads:destroy-thread *tcp-server-thread*)))
    (setf *tcp-server-thread* nil
          *tcp-server-port* nil
          *tcp-listener* nil)
    :stopped))

(defun %process-stream (stream socket conn-id remote)
  (let ((state (make-state))
        (log-context (list "conn" conn-id "remote" remote)))
    (let ((cl-mcp/src/log:*log-context* log-context))
      (loop
        for ready = (usocket:wait-for-input (list socket) :timeout *tcp-read-timeout*)
        do (cond
             ((null ready)
              (log-event :warn "tcp.read.timeout" "conn" conn-id "timeout" *tcp-read-timeout*)
              (return))
             (t
              (let ((line (handler-case
                              (read-line stream nil :eof)
                            (error (e)
                              (log-event :warn "tcp.read.error" "conn" conn-id "error" (princ-to-string e))
                              :eof))))
                (when (eq line :eof)
                  (log-event :info "tcp.read.eof" "conn" conn-id)
                  (return))
                (log-event :debug "tcp.read" "conn" conn-id "line" line)
                (let ((resp (process-json-line line state)))
                  (log-event :debug "tcp.response" "conn" conn-id "resp-nil" (null resp))
                  (when resp
                    (log-event :debug "tcp.write" "conn" conn-id "resp" resp)
                    (write-line resp stream)
                    (finish-output stream)
                    (log-event :debug "tcp.flushed" "conn" conn-id))))))))))

(defun serve-tcp (&key (host "127.0.0.1") (port 0) (accept-once t) on-listening)
  "Serve MCP over TCP. If PORT is 0, an ephemeral port is chosen.
Calls ON-LISTENING with the actual port when ready. If ACCEPT-ONCE is T,
accepts a single connection and returns T after the client closes."
  (let ((listener nil))
    (setf *tcp-stop-flag* nil)
    ;; Early exit if socket creation is not permitted (e.g., sandboxed CI).
    (handler-case
        (setf listener (usocket:socket-listen host port :reuse-address t
                                                        :element-type 'character))
      (error (e)
        (log-event :warn "tcp.listen.error" "error" (princ-to-string e))
        (return-from serve-tcp nil)))
    (unwind-protect
         (progn
           (setf *tcp-listener* listener)
           (let ((actual (usocket:get-local-port listener)))
             (when on-listening (funcall on-listening actual)))
           (labels
               ((handle-one (&optional conn-id client-arg)
                  (let ((client client-arg)
                        (stream nil)
                        (conn-id (or conn-id (incf *tcp-conn-counter*))))
                    (unwind-protect
                         (progn
                           ;; Proceed only if client is successfully obtained
                           (when client
                             (let ((remote (ignore-errors (usocket:get-peer-address client))))
                               (log-event :info "tcp.accept" "conn" conn-id "remote" remote)
                               (setf stream (usocket:socket-stream client))
                               (%process-stream stream client conn-id remote)))
                           t)
                      (when stream (ignore-errors (close stream)))
                      (when client (ignore-errors (usocket:socket-close client)))
                      (log-event :info "tcp.conn.closed" "conn" conn-id "fd" (%fd-count)))))
                (accept-loop ()
                  (loop while (not *tcp-stop-flag*)
                        do (let ((ready (usocket:wait-for-input (list listener)
                                                                :timeout *tcp-accept-timeout*)))
                             (cond
                               ((null ready)
                                (log-event :debug "tcp.accept.timeout" "timeout" *tcp-accept-timeout*))
                               (accept-once
                                (let ((client (handler-case
                                                   (usocket:socket-accept listener :element-type 'character)
                                                 (error (e)
                                                   (log-event :warn "tcp.accept.fail" "error" (princ-to-string e))
                                                   nil))))
                                  (when client
                                    (handle-one 0 client))
                                  (return)))
                               (t
                                (handler-case
                                    (let ((conn-id (incf *tcp-conn-counter*))
                                          (client (handler-case
                                                       (usocket:socket-accept listener :element-type 'character)
                                                     (error (e)
                                                       (log-event :warn "tcp.accept.fail" "error" (princ-to-string e))
                                                       nil))))
                                      (when client
                                        (bordeaux-threads:make-thread
                                         (lambda () (handle-one conn-id client))
                                         :name (format nil "mcp-client-~A" conn-id))))
                                  (error (e)
                                    (log-event :warn "tcp.accept.error" "error" (princ-to-string e))))))))))
             (accept-loop)))
      (when listener (ignore-errors (usocket:socket-close listener)))
      (setf *tcp-listener* nil))))
