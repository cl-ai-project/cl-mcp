;;;; tests/tcp-test.lisp

(defpackage #:cl-mcp/tests/tcp-test
  (:use #:cl #:rove)
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
        (loop repeat 200 until port-var do (sleep 0.01))
        (ok port-var)
        (let* ((sock (usocket:socket-connect "127.0.0.1" port-var
                                             :element-type 'character))
               (stream (usocket:socket-stream sock)))
          (unwind-protect
               (progn
                 (write-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n" stream)
                 (finish-output stream)
                 ;; Half-close our write side so the server sees EOF and exits
                 ;; the read loop even if the client crashes before closing.
                 (ignore-errors (usocket:socket-shutdown sock :output))
                 (let ((line (read-line stream)))
                   (ok (search "\"result\"" line))))
            (ignore-errors (close stream))
            (ignore-errors (usocket:socket-close sock))))
        (join-thread thr))))))

(deftest tcp-thread-helper-lifecycle
  (testing "start/ensure/stop manage tcp server thread"
    (if (socket-available-p)
        (ok t "socket unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (thr port)
                   (start-tcp-server-thread :host "127.0.0.1" :port 0 :accept-once nil)
                 (ok thr)
                 (ok (tcp-server-running-p))
                 (ok (integerp port))
                 (let* ((sock (usocket:socket-connect "127.0.0.1" port
                                                      :element-type 'character))
                        (stream (usocket:socket-stream sock)))
                   (unwind-protect
                        (progn
                          (write-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n" stream)
                          (finish-output stream)
                          ;; Signal EOF to server read loop after sending request.
                          (ignore-errors (usocket:socket-shutdown sock :output))
                          (let ((line (read-line stream)))
                            (ok (search "\"result\"" line))))
                     (ignore-errors (close stream))
                     (ignore-errors (usocket:socket-close sock))))
                 (ok (eq :already-running
                         (ensure-tcp-server-thread :host "127.0.0.1" :port port
                                                   :accept-once nil)))))
          (stop-tcp-server-thread)))
    (ok (not (tcp-server-running-p)))))

#+(or)
(deftest tcp-read-timeout-closes-idle-connection
  (testing "idle client is closed after read timeout"
    (if (not (socket-available-p))
        (ok t "socket unavailable")
        (let ((*tcp-read-timeout* 0.1)
              (port-var nil))
          (let ((thr (make-thread
                      (lambda ()
                        (serve-tcp :host "127.0.0.1" :port 0
                                   :accept-once t
                                   :on-listening (lambda (p) (setf port-var p))))
                      :name "tcp-timeout-server")))
            (unwind-protect
                 (progn
                   (loop repeat 200 until port-var do (sleep 0.01))
                   (ok port-var)
                   (let* ((sock (usocket:socket-connect "127.0.0.1" port-var
                                                        :element-type 'character))
                          (stream (usocket:socket-stream sock)))
                     (unwind-protect
                          (progn
                            ;; Do not send anything; wait for server to time out.
                            (sleep 0.3)
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
                     (start-tcp-server-thread :host "127.0.0.1" :port 0 :accept-once nil
                                              :on-listening (lambda (v) (setf port v)))
                   (declare (ignore thr))
                   (setf port p))
                 (loop repeat 200 until port do (sleep 0.01))
                 (ok port)
                 (flet ((send-init ()
                          (let* ((sock (usocket:socket-connect "127.0.0.1" port
                                                               :element-type 'character))
                                 (stream (usocket:socket-stream sock)))
                            (unwind-protect
                                 (progn
                                   (write-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n" stream)
                                   (finish-output stream)
                                   (ignore-errors (usocket:socket-shutdown sock :output))
                                   (read-line stream nil nil))
                              (ignore-errors (close stream))
                              (ignore-errors (usocket:socket-close sock))))))
                   (let ((r1 (send-init))
                         (r2 (send-init)))
                     (ok (and r1 (search "\"result\"" r1)))
                     (ok (and r2 (search "\"result\"" r2))))))
            (stop-tcp-server-thread))))))
