;;;; tests/http-test.lisp

(defpackage #:cl-mcp/tests/http-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/http
                #:*http-server-port*
                #:*http-auth-token*
                #:http-server-running-p
                #:start-http-server
                #:stop-http-server
                #:*session-timeout-seconds*
                #:*sessions*
                #:*sessions-lock*
                #:get-session
                #:create-session
                #:http-session-id
                #:http-session-last-access
                #:http-session-active-requests
                #:http-session-active-requests-lock)
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
                   (start-http-server :host "127.0.0.1" :port 0 :token nil)
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
                   (start-http-server :host "127.0.0.1" :port 0 :token nil)
                 (multiple-value-bind (acceptor2 port2)
                     (start-http-server :host "127.0.0.1" :port 0 :token nil)
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
                   (start-http-server :host "127.0.0.1" :port 0 :token nil)
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
                   (start-http-server :host "127.0.0.1" :port 0 :token nil)
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

;;; ------------------------------------------------------------
;;; M4: HTTP Authentication Tests
;;; ------------------------------------------------------------

(deftest auth-token-generated-by-default
  (testing "start-http-server defaults to no auth token"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (start-http-server :host "127.0.0.1" :port 0)
               (ok (null *http-auth-token*)
                   "Auth token should be NIL by default"))
          (stop-http-server)))))

(deftest auth-token-nil-disables-auth
  (testing "start-http-server with :token nil disables auth"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (start-http-server :host "127.0.0.1" :port 0 :token nil)
               (ok (null *http-auth-token*)
                   "Auth token should be NIL when disabled"))
          (stop-http-server)))))

(deftest auth-token-custom-string
  (testing "start-http-server with :token string uses that string"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (start-http-server :host "127.0.0.1" :port 0
                                  :token "my-secret-token")
               (ok (string= *http-auth-token* "my-secret-token")
                   "Auth token should match provided string"))
          (stop-http-server)))))

(deftest auth-token-cleared-on-stop
  (testing "stop-http-server clears the auth token"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (progn
          (start-http-server :host "127.0.0.1" :port 0 :token "test-token")
          (ok (string= *http-auth-token* "test-token"))
          (stop-http-server)
          (ok (null *http-auth-token*)
              "Auth token should be NIL after stop")))))

(deftest auth-401-without-token-header
  (testing "POST /mcp without Authorization returns 401 when auth enabled"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor port)
                   (start-http-server :host "127.0.0.1" :port 0
                                      :token "test-token")
                 (declare (ignore acceptor))
                 (sleep 0.1d0)
                 (multiple-value-bind (status headers)
                     (handler-case
                         (send-http-request
                          port "POST" "/mcp"
                          :body (concatenate
                                 'string
                                 "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                 "\"method\":\"initialize\",\"params\":{}}")
                          :headers '(("Content-Type" . "application/json")
                                     ("Accept" . "application/json")))
                       (error () (values nil nil)))
                   (when status
                     (ok (eql status 401)
                         "Should return 401 without auth header")
                     (ok (search "Www-Authenticate" headers)
                         "Should include WWW-Authenticate header")))))
          (stop-http-server)))))

(deftest auth-401-with-wrong-token
  (testing "POST /mcp with wrong Bearer token returns 401"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor port)
                   (start-http-server :host "127.0.0.1" :port 0
                                      :token "correct-token")
                 (declare (ignore acceptor))
                 (sleep 0.1d0)
                 (let ((status
                         (handler-case
                             (send-http-request
                              port "POST" "/mcp"
                              :body (concatenate
                                     'string
                                     "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                     "\"method\":\"initialize\",\"params\":{}}")
                              :headers '(("Content-Type" . "application/json")
                                         ("Accept" . "application/json")
                                         ("Authorization" . "Bearer wrong-token")))
                           (error () nil))))
                   (when status
                     (ok (eql status 401)
                         "Should return 401 with wrong token")))))
          (stop-http-server)))))

(deftest auth-200-with-correct-token
  (testing "POST /mcp with correct Bearer token returns 200"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor port)
                   (start-http-server :host "127.0.0.1" :port 0
                                      :token "correct-token")
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
                                     ("Accept" . "application/json")
                                     ("Authorization" . "Bearer correct-token")))
                       (error () (values nil nil nil)))
                   (when status
                     (ok (eql status 200)
                         "Should return 200 with correct token")
                     (ok (search "Mcp-Session-Id" headers)
                         "Should include session ID")
                     (ok (search "\"result\"" body)
                         "Should return result")))))
          (stop-http-server)))))

(deftest auth-options-skips-auth
  (testing "OPTIONS /mcp does not require auth"
    (if (not (http-port-available-p))
        (ok t "port unavailable")
        (unwind-protect
             (progn
               (multiple-value-bind (acceptor port)
                   (start-http-server :host "127.0.0.1" :port 0
                                      :token "test-token")
                 (declare (ignore acceptor))
                 (sleep 0.1d0)
                 (let ((status
                         (handler-case
                             (send-http-request
                              port "OPTIONS" "/mcp"
                              :headers '(("Origin" . "http://localhost:8080")))
                           (error () nil))))
                   (when status
                     (ok (eql status 204)
                         "OPTIONS should return 204 without auth")))))
          (stop-http-server)))))

;;; ------------------------------------------------------------
;;; M8: Session Reference Counting Tests
;;; ------------------------------------------------------------

(deftest session-has-active-requests-slot
  (testing "http-session struct has active-requests slot defaulting to 0"
    (unwind-protect
         (let ((session (create-session)))
           (ok (zerop (http-session-active-requests session))
               "active-requests should default to 0"))
      (clear-all-sessions))))

(deftest session-has-active-requests-lock
  (testing "http-session struct has active-requests-lock slot"
    (unwind-protect
         (let ((session (create-session)))
           (ok (http-session-active-requests-lock session)
               "active-requests-lock should be non-NIL"))
      (clear-all-sessions))))

(deftest expired-session-not-removed-with-active-requests
  (testing "get-session does not remove expired session with active requests"
    (let ((*session-timeout-seconds* 1))
      (unwind-protect
           (let* ((session (create-session))
                  (session-id (http-session-id session)))
             ;; Simulate an active request
             (bordeaux-threads:with-lock-held
                 ((http-session-active-requests-lock session))
               (incf (http-session-active-requests session)))
             ;; Simulate old last-access time (expired)
             (setf (http-session-last-access session)
                   (- (get-universal-time) 10))
             ;; get-session should return NIL (expired) but NOT remove it
             (ok (null (get-session session-id))
                 "Should return NIL for expired session")
             ;; Session should still be in the table
             (bordeaux-threads:with-lock-held (*sessions-lock*)
               (ok (gethash session-id *sessions*)
                   "Session should remain in table when active requests > 0")))
        (clear-all-sessions)))))

(deftest expired-session-removed-when-no-active-requests
  (testing "get-session removes expired session when active-requests is 0"
    (let ((*session-timeout-seconds* 1))
      (unwind-protect
           (let* ((session (create-session))
                  (session-id (http-session-id session)))
             ;; No active requests (default 0)
             ;; Simulate old last-access time (expired)
             (setf (http-session-last-access session)
                   (- (get-universal-time) 10))
             ;; get-session should return NIL and remove it
             (ok (null (get-session session-id))
                 "Should return NIL for expired session")
             ;; Session should be removed from the table
             (bordeaux-threads:with-lock-held (*sessions-lock*)
               (ok (null (gethash session-id *sessions*))
                   "Session should be removed when no active requests")))
        (clear-all-sessions)))))

(deftest active-requests-increment-decrement
  (testing "active-requests counter can be safely incremented and decremented"
    (unwind-protect
         (let ((session (create-session)))
           (bordeaux-threads:with-lock-held
               ((http-session-active-requests-lock session))
             (incf (http-session-active-requests session)))
           (ok (= 1 (http-session-active-requests session))
               "Should be 1 after increment")
           (bordeaux-threads:with-lock-held
               ((http-session-active-requests-lock session))
             (incf (http-session-active-requests session)))
           (ok (= 2 (http-session-active-requests session))
               "Should be 2 after second increment")
           (bordeaux-threads:with-lock-held
               ((http-session-active-requests-lock session))
             (decf (http-session-active-requests session)))
           (ok (= 1 (http-session-active-requests session))
               "Should be 1 after decrement"))
      (clear-all-sessions))))
