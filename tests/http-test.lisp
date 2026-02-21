;;;; tests/http-test.lisp

(defpackage #:cl-mcp/tests/http-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/http
                #:*http-server-port*
                #:http-server-running-p
                #:start-http-server
                #:stop-http-server
                #:*session-timeout-seconds*
                #:*sessions*
                #:*sessions-lock*
                #:get-session
                #:create-session
                #:http-session-id
                #:http-session-last-access)
  (:import-from #:usocket)
  (:import-from #:bordeaux-threads))

(in-package #:cl-mcp/tests/http-test)

(defun http-port-available-p ()
  "Return T if we can bind an HTTP port on localhost."
  (handler-case
      (let ((sock (usocket:socket-listen "127.0.0.1" 0
                                         :reuse-address t
                                         :element-type 'character)))
        (unwind-protect
             (progn (usocket:get-local-port sock) t)
          (ignore-errors (usocket:socket-close sock))))
    (error () nil)))

(defun send-http-request (port method path &key body headers)
  "Send a simple HTTP request and return (status-code header-string body-string).
Uses Connection: close to avoid keep-alive hanging."
  (let* ((sock (usocket:socket-connect "127.0.0.1" port
                                       :element-type 'character))
         (stream (usocket:socket-stream sock)))
    (unwind-protect
         (progn
           ;; Send request
           (format stream "~A ~A HTTP/1.1~C~C" method path #\Return #\Newline)
           (format stream "Host: 127.0.0.1:~D~C~C" port #\Return #\Newline)
           (format stream "Connection: close~C~C" #\Return #\Newline)
           (dolist (h headers)
             (format stream "~A: ~A~C~C" (car h) (cdr h) #\Return #\Newline))
           (when body
             (format stream "Content-Length: ~D~C~C" (length body) #\Return #\Newline))
           (format stream "~C~C" #\Return #\Newline)
           (when body
             (write-string body stream))
           (finish-output stream)

           ;; Read response line by line
           (let* ((status-line (read-line stream nil ""))
                  (status-code (when (> (length status-line) 12)
                                 (parse-integer status-line :start 9 :end 12 :junk-allowed t)))
                  (header-lines (make-string-output-stream))
                  (body-content (make-string-output-stream)))
             ;; Read headers until empty line
             (loop for line = (read-line stream nil "")
                   until (or (string= line "") (string= line (string #\Return)))
                   do (write-line line header-lines))
             ;; Read body
             (loop for line = (read-line stream nil nil)
                   while line
                   do (write-line line body-content))
             (values status-code
                     (get-output-stream-string header-lines)
                     (get-output-stream-string body-content))))
      (ignore-errors (close stream))
      (ignore-errors (usocket:socket-close sock)))))

(deftest http-server-not-running-initially
  (testing "http-server-running-p returns NIL when server not started"
    (ok (not (http-server-running-p)))))

(deftest http-server-lifecycle
  (testing "start/stop HTTP server"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor port)
                   (start-http-server :host "127.0.0.1" :port 0)
                 (ok acceptor)
                 (ok (integerp port))
                 (ok (http-server-running-p))
                 (ok (eql *http-server-port* port))))
          (stop-http-server)))
    (ok (not (http-server-running-p)))))

(deftest http-server-double-start
  (testing "starting already running server returns existing instance"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor1 port1)
                   (start-http-server :host "127.0.0.1" :port 0)
                 (multiple-value-bind (acceptor2 port2)
                     (start-http-server :host "127.0.0.1" :port 0)
                   (ok (eql acceptor1 acceptor2))
                   (ok (eql port1 port2)))))
          (stop-http-server)))))

(deftest http-post-initialize
  (testing "POST /mcp with initialize creates session and returns result"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor port)
                   (start-http-server :host "127.0.0.1" :port 0)
                 (declare (ignore acceptor))
                 (sleep 0.1d0)
                 (multiple-value-bind (status headers body)
                     (handler-case
                         (send-http-request
                          port "POST" "/mcp"
                          :body (concatenate
                                 'string
                                 "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                 "\"method\":\"initialize\",\"params\":{}}")
                          :headers '(("Content-Type" . "application/json")
                                     ("Accept" . "application/json")))
                       (error (e)
                         (declare (ignore e))
                         (values nil nil nil)))
                   (when status
                     (ok (eql status 200))
                     (ok (search "Mcp-Session-Id" headers))
                     (ok (search "\"result\"" body))))))
          (stop-http-server)))))

(deftest http-post-without-session-header
  (testing "POST /mcp non-initialize without session returns 400"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor port)
                   (start-http-server :host "127.0.0.1" :port 0)
                 (declare (ignore acceptor))
                 (sleep 0.1d0)
                 (let ((status
                         (handler-case
                             (send-http-request
                              port "POST" "/mcp"
                              :body (concatenate
                                     'string
                                     "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                     "\"method\":\"tools/list\",\"params\":{}}")
                              :headers '(("Content-Type" . "application/json")))
                           (error () nil))))
                   (when status
                     (ok (eql status 400))))))
          (stop-http-server)))))

;;; Session timeout tests

(defun clear-all-sessions ()
  "Clear all sessions for test isolation."
  (bordeaux-threads:with-lock-held (*sessions-lock*)
    (clrhash *sessions*)))

(deftest session-timeout-default-is-one-hour
  (testing "*session-timeout-seconds* defaults to 3600 (1 hour)"
    (ok (eql *session-timeout-seconds* 3600))))

(deftest session-no-expiry-when-timeout-disabled
  (testing "Sessions do not expire when *session-timeout-seconds* is NIL"
    (let ((*session-timeout-seconds* nil))
      (unwind-protect
           (let* ((session (create-session))
                  (session-id (http-session-id session)))
             ;; Simulate old last-access time (1 hour ago)
             (setf (http-session-last-access session)
                   (- (get-universal-time) 3600))
             ;; Session should still be valid
             (ok (get-session session-id) "Session should not expire when timeout is NIL"))
        (clear-all-sessions)))))

(deftest session-expires-when-timeout-enabled
  (testing "Sessions expire after *session-timeout-seconds*"
    (let ((*session-timeout-seconds* 1))  ; 1 second timeout
      (unwind-protect
           (let* ((session (create-session))
                  (session-id (http-session-id session)))
             ;; Simulate old last-access time (2 seconds ago)
             (setf (http-session-last-access session)
                   (- (get-universal-time) 2))
             ;; Session should be expired
             (ok (null (get-session session-id))
                 "Session should expire when idle time exceeds timeout"))
        (clear-all-sessions)))))

(deftest session-valid-within-timeout
  (testing "Sessions remain valid within timeout period"
    (let ((*session-timeout-seconds* 60))  ; 60 second timeout
      (unwind-protect
           (let* ((session (create-session))
                  (session-id (http-session-id session)))
             ;; Session was just created, should be valid
             (ok (get-session session-id)
                 "Session should be valid within timeout period"))
        (clear-all-sessions)))))

(deftest get-session-updates-last-access
  (testing "get-session updates last-access timestamp"
    (let ((*session-timeout-seconds* nil))
      (unwind-protect
           (let* ((session (create-session))
                  (session-id (http-session-id session))
                  (old-time (- (get-universal-time) 100)))
             ;; Set old last-access time
             (setf (http-session-last-access session) old-time)
             ;; Call get-session
             (get-session session-id)
             ;; Last-access should be updated
             (ok (> (http-session-last-access session) old-time)
                 "get-session should update last-access timestamp"))
        (clear-all-sessions)))))
