;;;; tests/worker-test.lisp
;;;;
;;;; Tests for worker process TCP server infrastructure.

(defpackage #:cl-mcp/tests/worker-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/worker/server
                #:make-worker-server
                #:server-port
                #:start-accept-loop
                #:stop-server
                #:register-method))

(in-package #:cl-mcp/tests/worker-test)

(defun socket-available-p ()
  "Return T if we can bind a TCP socket on localhost."
  (handler-case
      (let ((sock (usocket:socket-listen "127.0.0.1" 0
                                         :reuse-address t
                                         :element-type 'character)))
        (unwind-protect t
          (ignore-errors (usocket:socket-close sock))))
    (error () nil)))

(deftest worker-server-accepts-connection-and-pings
  (testing "start worker server, connect, send ping, get pong"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        (let ((server (make-worker-server :port 0)))
          (unwind-protect
               (let* ((port (server-port server))
                      (thread (bordeaux-threads:make-thread
                               (lambda () (start-accept-loop server))
                               :name "test-accept")))
                 (declare (ignore thread))
                 ;; Give server time to enter accept
                 (sleep 0.1)
                 (let ((socket (usocket:socket-connect
                                "127.0.0.1" port
                                :element-type 'character)))
                   (unwind-protect
                        (let ((stream (usocket:socket-stream socket)))
                          (write-line
                           "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"worker/ping\"}"
                           stream)
                          (force-output stream)
                          (let* ((line (read-line stream))
                                 (response (yason:parse line)))
                            (ok (gethash "result" response)
                                "response has result")
                            (ok (equal (gethash "id" response) 1)
                                "response id matches")
                            (ok (gethash "pong" (gethash "result" response))
                                "result contains pong=true")))
                     (ignore-errors (usocket:socket-close socket)))))
            (stop-server server))))))

(deftest worker-server-dispatches-registered-methods
  (testing "register a custom method and verify dispatch"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        (let ((server (make-worker-server :port 0)))
          (register-method server "test/echo"
                           (lambda (params)
                             (let ((ht (make-hash-table :test 'equal)))
                               (setf (gethash "echo" ht)
                                     (gethash "msg" params))
                               ht)))
          (unwind-protect
               (let* ((port (server-port server))
                      (thread (bordeaux-threads:make-thread
                               (lambda () (start-accept-loop server))
                               :name "test-accept")))
                 (declare (ignore thread))
                 (sleep 0.1)
                 (let ((socket (usocket:socket-connect
                                "127.0.0.1" port
                                :element-type 'character)))
                   (unwind-protect
                        (let ((stream (usocket:socket-stream socket)))
                          (write-line
                           (concatenate
                            'string
                            "{\"jsonrpc\":\"2.0\",\"id\":2,"
                            "\"method\":\"test/echo\","
                            "\"params\":{\"msg\":\"hello\"}}")
                           stream)
                          (force-output stream)
                          (let* ((line (read-line stream))
                                 (response (yason:parse line)))
                            (ok (equal (gethash "id" response) 2)
                                "response id matches")
                            (ok (string= "hello"
                                         (gethash "echo"
                                                  (gethash "result" response)))
                                "echo value matches")))
                     (ignore-errors (usocket:socket-close socket)))))
            (stop-server server))))))

(deftest worker-server-returns-method-not-found-for-unknown
  (testing "unknown method returns JSON-RPC error -32601"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        (let ((server (make-worker-server :port 0)))
          (unwind-protect
               (let* ((port (server-port server))
                      (thread (bordeaux-threads:make-thread
                               (lambda () (start-accept-loop server))
                               :name "test-accept")))
                 (declare (ignore thread))
                 (sleep 0.1)
                 (let ((socket (usocket:socket-connect
                                "127.0.0.1" port
                                :element-type 'character)))
                   (unwind-protect
                        (let ((stream (usocket:socket-stream socket)))
                          (write-line
                           "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"no/such\"}"
                           stream)
                          (force-output stream)
                          (let* ((line (read-line stream))
                                 (response (yason:parse line)))
                            (ok (equal (gethash "id" response) 3)
                                "response id matches")
                            (ok (gethash "error" response)
                                "response has error field")
                            (ok (= -32601
                                   (gethash "code"
                                            (gethash "error" response)))
                                "error code is -32601")))
                     (ignore-errors (usocket:socket-close socket)))))
            (stop-server server))))))

(deftest worker-server-handles-handler-error
  (testing "handler that signals an error returns JSON-RPC -32603"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        (let ((server (make-worker-server :port 0)))
          (register-method server "test/boom"
                           (lambda (params)
                             (declare (ignore params))
                             (error "kaboom")))
          (unwind-protect
               (let* ((port (server-port server))
                      (thread (bordeaux-threads:make-thread
                               (lambda () (start-accept-loop server))
                               :name "test-accept")))
                 (declare (ignore thread))
                 (sleep 0.1)
                 (let ((socket (usocket:socket-connect
                                "127.0.0.1" port
                                :element-type 'character)))
                   (unwind-protect
                        (let ((stream (usocket:socket-stream socket)))
                          (write-line
                           "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"test/boom\"}"
                           stream)
                          (force-output stream)
                          (let* ((line (read-line stream))
                                 (response (yason:parse line)))
                            (ok (equal (gethash "id" response) 4)
                                "response id matches")
                            (ok (gethash "error" response)
                                "response has error field")
                            (ok (= -32603
                                   (gethash "code"
                                            (gethash "error" response)))
                                "error code is -32603")
                            (ok (search "kaboom"
                                        (gethash "message"
                                                 (gethash "error" response)))
                                "error message includes original")))
                     (ignore-errors (usocket:socket-close socket)))))
            (stop-server server))))))

(deftest worker-server-multiple-requests
  (testing "send multiple requests on same connection"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        (let ((server (make-worker-server :port 0)))
          (unwind-protect
               (let* ((port (server-port server))
                      (thread (bordeaux-threads:make-thread
                               (lambda () (start-accept-loop server))
                               :name "test-accept")))
                 (declare (ignore thread))
                 (sleep 0.1)
                 (let ((socket (usocket:socket-connect
                                "127.0.0.1" port
                                :element-type 'character)))
                   (unwind-protect
                        (let ((stream (usocket:socket-stream socket)))
                          ;; First request
                          (write-line
                           "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"worker/ping\"}"
                           stream)
                          (force-output stream)
                          (let* ((line1 (read-line stream))
                                 (r1 (yason:parse line1)))
                            (ok (equal (gethash "id" r1) 10)
                                "first response id"))
                          ;; Second request
                          (write-line
                           "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"worker/ping\"}"
                           stream)
                          (force-output stream)
                          (let* ((line2 (read-line stream))
                                 (r2 (yason:parse line2)))
                            (ok (equal (gethash "id" r2) 11)
                                "second response id")))
                     (ignore-errors (usocket:socket-close socket)))))
            (stop-server server))))))
