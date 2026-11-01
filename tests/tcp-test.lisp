;;;; tests/tcp-test.lisp

(defpackage #:cl-mcp/tests/tcp-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tcp
                #:serve-tcp
                #:start-tcp-server-thread
                #:ensure-tcp-server-thread
                #:stop-tcp-server-thread
                #:tcp-server-running-p
                #:*tcp-read-timeout*)
  (:import-from #:bordeaux-threads #:make-thread #:join-thread)
  (:import-from #:usocket))

(in-package #:cl-mcp/tests/tcp-test)

(defun socket-available-p ()
  "Return T if we can bind a TCP socket on localhost."
  (handler-case
      (let ((sock (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type 'character)))
        (unwind-protect
             (progn (usocket:get-local-port sock) t)
          (ignore-errors (usocket:socket-close sock))))
    (error () nil)))

(deftest tcp-serve-initialize
  (testing "serve-tcp accepts a connection and responds to initialize"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
    (let ((port-var nil))
      (let ((thr (make-thread
                  (lambda ()
                    (serve-tcp :host "127.0.0.1" :port 0
                               :on-listening (lambda (p) (setf port-var p))
                               :accept-once t))
                  :name "tcp-test-server")))
        (loop repeat 200 until port-var do (sleep 0.01d0))
        (ok port-var)
        (let* ((sock (usocket:socket-connect "127.0.0.1" port-var
                                             :element-type 'character))
               (stream (usocket:socket-stream sock)))
          (unwind-protect
               (let ((req (concatenate
                           'string
                           "{\"jsonrpc\":\"2.0\",\"id\":1,"
                           "\"method\":\"initialize\",\"params\":{}}\n")))
                 (write-string req stream)
                 (finish-output stream)
                 ;; Half-close our write side so the server sees EOF and exits
                 ;; the read loop even if the client crashes before closing.
                 (ignore-errors (usocket:socket-shutdown sock :output))
                 (let ((line (read-line stream)))
                   (ok (search "\"result\"" line))))
            (ignore-errors (close stream))
            (ignore-errors (usocket:socket-close sock))))
        (join-thread thr))))))

(deftest tcp-idle-connection-does-not-drop
  (testing "idle tcp connections stay open for later requests"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let ((*tcp-read-timeout* 0.1d0)
              (port-var nil))
          (let ((thr (make-thread
                      (lambda ()
                        (serve-tcp :host "127.0.0.1" :port 0
                                   :accept-once t
                                   :on-listening (lambda (p) (setf port-var p))))
                      :name "tcp-idle-server")))
            (unwind-protect
                 (progn
                   (loop repeat 200 until port-var do (sleep 0.01d0))
                   (ok port-var)
                   (let* ((sock (usocket:socket-connect "127.0.0.1" port-var
                                                        :element-type 'character))
                          (stream (usocket:socket-stream sock)))
                     (unwind-protect
                          (let ((req (concatenate
                                      'string
                                      "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                      "\"method\":\"initialize\",\"params\":{}}\n")))
                            ;; Wait longer than *tcp-read-timeout* without sending.
                            (sleep 0.3d0)
                            (write-string req stream)
                            (finish-output stream)
                            (ignore-errors (usocket:socket-shutdown sock :output))
                            (let ((line (read-line stream nil nil)))
                              (ok (and line (search "\"result\"" line)))))
                       (ignore-errors (close stream))
                       (ignore-errors (usocket:socket-close sock)))))
              (join-thread thr)))))))

(deftest tcp-thread-helper-lifecycle
  (testing "start/ensure/stop manage tcp server thread"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (unwind-protect
             (multiple-value-bind (thr port)
                 (start-tcp-server-thread :host "127.0.0.1" :port 0
                                          :accept-once nil)
               (ok thr)
               (ok (tcp-server-running-p))
               (ok (integerp port))
               (let* ((sock (usocket:socket-connect "127.0.0.1" port
                                                    :element-type 'character))
                      (stream (usocket:socket-stream sock)))
                 (unwind-protect
                      (let ((req (concatenate
                                  'string
                                  "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                  "\"method\":\"initialize\",\"params\":{}}\n")))
                        (write-string req stream)
                        (finish-output stream)
                        ;; Signal EOF to server read loop after sending request.
                        (ignore-errors (usocket:socket-shutdown sock :output))
                        (let ((line (read-line stream)))
                          (ok (search "\"result\"" line))))
                   (ignore-errors (close stream))
                   (ignore-errors (usocket:socket-close sock))))
               (ok (eq :already-running
                       (ensure-tcp-server-thread :host "127.0.0.1" :port port
                                                 :accept-once nil))))
          (stop-tcp-server-thread)))
    (ok (not (tcp-server-running-p)))))

#+(or)
(deftest tcp-read-timeout-closes-idle-connection
  (testing "idle client is closed after read timeout"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let ((*tcp-read-timeout* 0.1d0)
              (port-var nil))
          (let ((thr (make-thread
                      (lambda ()
                        (serve-tcp :host "127.0.0.1" :port 0
                                   :accept-once t
                                   :on-listening (lambda (p) (setf port-var p))))
                      :name "tcp-timeout-server")))
            (unwind-protect
                 (progn
                   (loop repeat 200 until port-var do (sleep 0.01d0))
                   (ok port-var)
                   (let* ((sock (usocket:socket-connect "127.0.0.1" port-var
                                                        :element-type 'character))
                          (stream (usocket:socket-stream sock)))
                     (unwind-protect
                          (progn
                            ;; Do not send anything; wait for server to time out.
                            (sleep 0.3d0)
                            (ok (eq (read-line stream nil :eof) :eof)))
                       (ignore-errors (close stream))
                       (ignore-errors (usocket:socket-close sock)))))
              (bordeaux-threads:join-thread thr)))))))

(deftest tcp-multi-client-handling
  (testing "multiple clients receive responses without blocking each other"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let ((port nil))
          (unwind-protect
               (progn
                 (multiple-value-bind (thr p)
                     (start-tcp-server-thread :host "127.0.0.1" :port 0
                                              :accept-once nil
                                              :on-listening (lambda (v)
                                                              (setf port v)))
                   (declare (ignore thr))
                   (setf port p))
                 (loop repeat 200 until port do (sleep 0.01d0))
                 (ok port)
                 (flet ((send-init ()
                          (let* ((sock (usocket:socket-connect
                                        "127.0.0.1" port
                                        :element-type 'character))
                                 (stream (usocket:socket-stream sock)))
                            (unwind-protect
                                 (let ((req (concatenate
                                             'string
                                             "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                             "\"method\":\"initialize\","
                                             "\"params\":{}}\n")))
                                   (write-string req stream)
                                   (finish-output stream)
                                   (ignore-errors
                                     (usocket:socket-shutdown sock :output))
                                   (read-line stream nil nil))
                              (ignore-errors (close stream))
                              (ignore-errors (usocket:socket-close sock))))))
                   (let ((r1 (send-init))
                         (r2 (send-init)))
                     (ok (and r1 (search "\"result\"" r1)))
                     (ok (and r2 (search "\"result\"" r2))))))
            (stop-tcp-server-thread))))))

(deftest tcp-entry-points-apply-tool-groups
  (testing "both TCP starters reach the registry with tool-groups"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let* ((groups-symbol (find-symbol "*ENABLED-TOOL-GROUPS*"
                                           "CL-MCP/SRC/TOOLS/REGISTRY"))
               (previous (symbol-value groups-symbol)))
          (unwind-protect
               (progn
                 (setf (symbol-value groups-symbol) nil)
                 (unwind-protect
                      (progn
                        (start-tcp-server-thread :host "127.0.0.1" :port 0
                                                 :accept-once nil
                                                 :worker-pool nil
                                                 :tool-groups "cl-spec")
                        (ok (equal '("CL-SPEC") (symbol-value groups-symbol))))
                   (stop-tcp-server-thread))
                 (setf (symbol-value groups-symbol) nil)
                 (unwind-protect
                      (progn
                        (ensure-tcp-server-thread :host "127.0.0.1" :port 0
                                                  :accept-once nil
                                                  :worker-pool nil
                                                  :tool-groups (list :cl-spec))
                        (ok (equal '("CL-SPEC") (symbol-value groups-symbol))))
                   (stop-tcp-server-thread)))
            (setf (symbol-value groups-symbol) previous))))))

(defun %stop-one-shot-server (thread port)
  "Make THREAD, an accept-once SERVE-TCP, return, and destroy it if it will not.
The stop flag ends its accept loop at the next timeout; a connection that is
closed at once ends it sooner, since the server accepts it, reads EOF and
returns."
  (setf cl-mcp/src/tcp::*tcp-stop-flag* t)
  (when port
    (ignore-errors
     (usocket:socket-close
      (usocket:socket-connect "127.0.0.1" port :element-type 'character))))
  (loop repeat 1000
        while (bordeaux-threads:thread-alive-p thread)
        do (sleep 0.01d0))
  (when (bordeaux-threads:thread-alive-p thread)
    (ignore-errors (bordeaux-threads:destroy-thread thread))))

(defun %call-with-one-shot-server (function)
  "Run SERVE-TCP for one connection on a thread and call FUNCTION with its port.
Return the server thread once FUNCTION returns; the caller decides how long to
wait for it to finish.  When the server does not come up, or FUNCTION does not
return normally, the server is stopped here (see %STOP-ONE-SHOT-SERVER), so no
thread is left listening.
The server runs without a worker pool, bound on its own thread: these tests
use no worker, and starting and stopping a pool would add seconds to the
server's return on a loaded machine."
  (let* ((port nil)
         (returned nil)
         (thread (make-thread
                  (lambda ()
                    (let ((cl-mcp/src/proxy:*use-worker-pool* nil))
                      (serve-tcp :host "127.0.0.1" :port 0
                                 :on-listening (lambda (p) (setf port p))
                                 :accept-once t)))
                  :name "tcp-test-one-shot-server")))
    (loop repeat 200 until port do (sleep 0.01d0))
    (ok port "the one-shot server is listening")
    (unwind-protect
         (when port
           (funcall function port)
           (setf returned t))
      (unless returned
        (%stop-one-shot-server thread port)))
    thread))

(defun %call-capturing-log (function)
  "Call FUNCTION and return the log lines written meanwhile, by any thread.
The server's threads read the globals, so *LOG-STREAM* and *LOG-LEVEL* are set
rather than bound (a binding on this thread would not reach them) and restored
afterwards.  The level is :DEBUG so an MCP_LOG_LEVEL in the environment cannot
hide the events a test looks for; the lines still reach the previous stream."
  (let ((capture (make-string-output-stream))
        (previous-stream cl-mcp/src/log:*log-stream*)
        (previous-level cl-mcp/src/log:*log-level*))
    (unwind-protect
         (progn
           (setf cl-mcp/src/log:*log-stream*
                 (make-broadcast-stream previous-stream capture)
                 cl-mcp/src/log:*log-level* :debug)
           (funcall function))
      (setf cl-mcp/src/log:*log-stream* previous-stream
            cl-mcp/src/log:*log-level* previous-level))
    (bordeaux-threads:with-lock-held (cl-mcp/src/log:*log-lock*)
      (get-output-stream-string capture))))

(defun %thread-ends-within-p (thread seconds)
  "Return T when THREAD finishes within SECONDS, NIL when it is still running.
A thread still running then is destroyed -- as it is when the wait itself is
interrupted -- so a failing test leaves no server or client thread behind."
  (unwind-protect
       (progn
         (loop repeat (round (* seconds 100))
               while (bordeaux-threads:thread-alive-p thread)
               do (sleep 0.01d0))
         (not (bordeaux-threads:thread-alive-p thread)))
    (when (bordeaux-threads:thread-alive-p thread)
      (ignore-errors (bordeaux-threads:destroy-thread thread)))))

(defun %read-octet-line (stream)
  "Read one LF-terminated line of UTF-8 from the octet STREAM; NIL at EOF."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte)
                    (return (and (plusp (length octets))
                                 (sb-ext:octets-to-string
                                  (coerce octets '(vector (unsigned-byte 8)))
                                  :external-format :utf-8))))
                   ((= byte 10)
                    (return (sb-ext:octets-to-string
                             (coerce octets '(vector (unsigned-byte 8)))
                             :external-format :utf-8)))
                   (t (vector-push-extend byte octets))))))

(deftest tcp-undecodable-line-is-a-parse-error
  (testing "a line that is not UTF-8 is answered with -32700 and the connection goes on"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let ((thread
                (%call-with-one-shot-server
                 (lambda (port)
                   ;; :timeout puts a receive timeout on the socket, so a
                   ;; response that never comes fails the read, not the run.
                   (let* ((sock (usocket:socket-connect
                                 "127.0.0.1" port
                                 :element-type '(unsigned-byte 8) :timeout 5))
                          (stream (usocket:socket-stream sock)))
                     (unwind-protect
                          (flet ((send (octets)
                                   (write-sequence octets stream)
                                   (finish-output stream)))
                            (send (concatenate
                                   '(vector (unsigned-byte 8))
                                   (sb-ext:string-to-octets
                                    (concatenate
                                     'string
                                     "{\"jsonrpc\":\"2.0\",\"id\":77,"
                                     "\"method\":\"ping\",\"params\":{\"x\":\"")
                                    :external-format :utf-8)
                                   #(#xc3 #x28)
                                   (sb-ext:string-to-octets
                                    (format nil "\"}}~%") :external-format :utf-8)))
                            (send (sb-ext:string-to-octets
                                   (format nil "{\"jsonrpc\":\"2.0\",\"id\":78,~
                                                \"method\":\"ping\"}~%")
                                   :external-format :utf-8))
                            (ignore-errors (usocket:socket-shutdown sock :output))
                            (let ((first (handler-case (%read-octet-line stream)
                                           (error () nil)))
                                  (second (handler-case (%read-octet-line stream)
                                            (error () nil))))
                              (ok (and first (search "-32700" first))
                                  "the undecodable line gets a parse error")
                              (ok (and second (search "\"id\":78" second)
                                       (search "\"result\"" second))
                                  "the next request on the connection is answered")))
                       (ignore-errors (usocket:socket-close sock))))))))
          (ok (%thread-ends-within-p thread 5)
              "the connection ends and the server returns")))))

(deftest tcp-connection-reset-ends-the-connection
  (testing "a reset from the client ends the read loop instead of stopping the thread"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let ((text
                (%call-capturing-log
                 (lambda ()
                   (let ((thread
                           (%call-with-one-shot-server
                            (lambda (port)
                              (let* ((sock (usocket:socket-connect
                                            "127.0.0.1" port
                                            :element-type 'character))
                                     (stream (usocket:socket-stream sock)))
                                (write-line
                                 "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}"
                                 stream)
                                (finish-output stream)
                                ;; Wait for the response and leave it unread in
                                ;; the kernel: closing a socket with unread data
                                ;; makes the kernel send a reset instead of a
                                ;; FIN, which the server meets in its next read.
                                ;; Poll the descriptor itself: LISTEN, and
                                ;; USOCKET:WAIT-FOR-INPUT, which calls it, would
                                ;; read the response into the stream's buffer.
                                (ok (sb-unix:unix-simple-poll
                                     (sb-bsd-sockets:socket-file-descriptor
                                      (usocket:socket sock))
                                     :input 5000)
                                    "the response has arrived")
                                (usocket:socket-close sock))))))
                     (ok (%thread-ends-within-p thread 5)
                         "the connection ends and the server returns"))))))
          ;; The server meets the reset in WAIT-FOR-INPUT or in the read,
          ;; whichever runs first after it arrives; both log it the same way.
          (ok (search "\"tcp.read.error\"" text)
              "the server met the reset, not an orderly close")
          (ok (not (search "\"tcp.read.eof\"" text))
              "the connection did not end at EOF")))))

(deftest tcp-reset-met-while-waiting-is-a-read-error
  (testing "a reset already pending when the server waits for input is a read error"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                                :reuse-address t
                                                :element-type 'character))
               (client (usocket:socket-connect "127.0.0.1"
                                               (usocket:get-local-port listener)
                                               :element-type 'character))
               (server-side (usocket:socket-accept listener
                                                   :element-type 'character)))
          (unwind-protect
               (let ((text
                       (%call-capturing-log
                        (lambda ()
                          ;; Leave data unread on the client and close it, so
                          ;; the kernel resets the connection before the server
                          ;; side is ever read: its first WAIT-FOR-INPUT, which
                          ;; calls LISTEN, meets the reset.
                          (let ((out (usocket:socket-stream server-side)))
                            (write-line "unread" out)
                            (finish-output out))
                          (sb-unix:unix-simple-poll
                           (sb-bsd-sockets:socket-file-descriptor
                            (usocket:socket client))
                           :input 5000)
                          (usocket:socket-close client)
                          ;; The reset makes the server's descriptor readable.
                          (ok (sb-unix:unix-simple-poll
                               (sb-bsd-sockets:socket-file-descriptor
                                (usocket:socket server-side))
                               :input 5000)
                              "the reset has reached the server side")
                          (let ((thread
                                  (make-thread
                                   (lambda ()
                                     (funcall 'cl-mcp/src/tcp::%tcp-handle-client
                                              server-side 9003))
                                   :name "tcp-test-reset-while-waiting")))
                            (ok (%thread-ends-within-p thread 5)
                                "the client thread ends"))))))
                 (ok (search "\"tcp.read.error\"" text)
                     "the reset is logged as a read error")
                 (ok (not (search "\"tcp.conn.error\"" text))
                     "not as an unexpected condition"))
            (ignore-errors (usocket:socket-close client))
            (ignore-errors (usocket:socket-close listener)))))))

(deftest tcp-client-thread-survives-a-failing-socket
  (testing "a closed stream met while waiting for input ends the connection as a read error"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                                :reuse-address t
                                                :element-type 'character))
               (client (usocket:socket-connect "127.0.0.1"
                                               (usocket:get-local-port listener)
                                               :element-type 'character))
               (server-side (usocket:socket-accept listener :element-type 'character)))
          (unwind-protect
               (let ((text
                       (%call-capturing-log
                        (lambda ()
                          ;; Close the descriptor under the server's socket
                          ;; object, so its first WAIT-FOR-INPUT signals.
                          (close (usocket:socket-stream server-side))
                          (let ((thread (make-thread
                                         (lambda ()
                                           (funcall 'cl-mcp/src/tcp::%tcp-handle-client
                                                    server-side 9001))
                                         :name "tcp-test-failing-socket")))
                            (ok (%thread-ends-within-p thread 5)
                                "the client thread ends instead of stopping in the debugger"))))))
                 (ok (search "\"tcp.read.error\"" text)
                     "the closed stream is logged as a read error"))
            (ignore-errors (usocket:socket-close client))
            (ignore-errors (usocket:socket-close listener)))))))

(define-condition test-serious-condition (serious-condition) ()
  (:documentation "A SERIOUS-CONDITION that is not an ERROR, as STORAGE-CONDITION
and SB-EXT:TIMEOUT are not."))

(deftest tcp-client-thread-survives-a-serious-condition
  (testing "a serious condition that is not an error still ends the connection"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                                :reuse-address t
                                                :element-type 'character))
               (client (usocket:socket-connect "127.0.0.1"
                                               (usocket:get-local-port listener)
                                               :element-type 'character))
               (server-side (usocket:socket-accept listener
                                                   :element-type 'character))
               (name 'cl-mcp/src/tcp::%process-stream)
               (original (fdefinition name)))
          (unwind-protect
               (progn
                 (setf (fdefinition name)
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (error 'test-serious-condition)))
                 (let ((thread (make-thread
                                (lambda ()
                                  (funcall 'cl-mcp/src/tcp::%tcp-handle-client
                                           server-side 9002))
                                :name "tcp-test-serious-condition")))
                   (ok (%thread-ends-within-p thread 5)
                       "the client thread ends instead of stopping in the debugger")))
            (setf (fdefinition name) original)
            (ignore-errors (usocket:socket-close client))
            (ignore-errors (usocket:socket-close listener)))))))
