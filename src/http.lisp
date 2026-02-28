;;;; src/http.lisp
;;;; MCP Streamable HTTP Transport using Hunchentoot

(defpackage #:cl-mcp/src/http
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/protocol #:make-state #:process-json-line)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/pool
                #:initialize-pool #:shutdown-pool #:release-session)
  (:import-from #:bordeaux-threads #:make-lock #:with-lock-held)
  (:import-from #:hunchentoot)
  (:import-from #:yason)
  (:export
   #:*http-server*
   #:*http-server-port*
   #:*http-auth-token*
   #:http-server-running-p
   #:start-http-server
   #:stop-http-server))

(in-package #:cl-mcp/src/http)

;;; ------------------------------------------------------------
;;; Server State
;;; ------------------------------------------------------------

(defparameter *http-server* nil
  "The Hunchentoot acceptor instance.")

(defparameter *http-server-port* nil
  "Port number of the currently running HTTP server.")

(defparameter *http-auth-token* nil
  "Bearer token for HTTP authentication.
When non-NIL, all requests (except OPTIONS) must include an
Authorization: Bearer <token> header matching this value.
Set automatically by START-HTTP-SERVER.")

;;; ------------------------------------------------------------
;;; Session Management
;;; ------------------------------------------------------------

(defvar *sessions* (make-hash-table :test #'equal)
  "Hash table mapping session-id to server-state.")

(defvar *sessions-lock* (bordeaux-threads:make-lock "sessions-lock")
  "Lock for thread-safe session access.")

(defparameter *session-timeout-seconds*
  3600
  "Session expiration time in seconds.  Default is 3600 (1 hour).
When a session has been idle longer than this, the next access
evicts it and releases the associated worker process.  Set to NIL
to disable timeout (not recommended — leaked sessions consume
100-500MB each via their worker process).")

(defstruct http-session
  "HTTP session containing MCP state and metadata."
  (id "" :type string)
  (state nil)
  (created-at (get-universal-time) :type integer)
  (last-access (get-universal-time) :type integer)
  (active-requests 0 :type integer)
  (active-requests-lock (bordeaux-threads:make-lock "active-requests")))

(defun generate-session-id ()
  "Generate a cryptographically random session ID using /dev/urandom."
  (with-open-file (s #P"/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((buf (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      (format nil "~{~(~2,'0X~)~}" (coerce buf 'list)))))

(defun %generate-auth-token ()
  "Generate a cryptographically random auth token using /dev/urandom."
  (with-open-file (s #P"/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((buf (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      (format nil "~{~(~2,'0X~)~}" (coerce buf 'list)))))

(defun %check-auth ()
  "Validate the Authorization: Bearer header against *http-auth-token*.
Returns T if auth passes (token matches or auth is disabled).
When auth fails, sets 401 status with WWW-Authenticate header and
returns NIL."
  (when (null *http-auth-token*)
    (return-from %check-auth t))
  (let* ((auth-header (get-header :authorization))
         (prefix "Bearer ")
         (prefix-len (length prefix)))
    (cond
      ((null auth-header)
       (setf (hunchentoot:return-code*) 401)
       (set-header :www-authenticate "Bearer")
       nil)
      ((and (> (length auth-header) prefix-len)
            (string= auth-header prefix :end1 prefix-len)
            (string= (subseq auth-header prefix-len) *http-auth-token*))
       t)
      (t
       (setf (hunchentoot:return-code*) 401)
       (set-header :www-authenticate "Bearer")
       nil))))

(defun get-session (session-id)
  "Get session by ID, returning NIL if not found or expired.
When *session-timeout-seconds* is NIL, sessions never expire.
Expired sessions with active requests are NOT removed from the table;
they will be cleaned up once all in-flight requests complete."
  (let ((result nil)
        (expired-id nil))
    (bordeaux-threads:with-lock-held (*sessions-lock*)
      (let ((session (gethash session-id *sessions*)))
        (when session
          (let ((now (get-universal-time)))
            (cond
              ((null *session-timeout-seconds*)
               (setf (http-session-last-access session) now)
               (setf result session))
              ((> (- now (http-session-last-access session))
                  *session-timeout-seconds*)
               ;; Expired — but do not remove if requests are in flight
               (let ((active
                       (bordeaux-threads:with-lock-held
                           ((http-session-active-requests-lock session))
                         (http-session-active-requests session))))
                 (when (zerop active)
                   (remhash session-id *sessions*)
                   (setf expired-id session-id))))
              (t
               (setf (http-session-last-access session) now)
               (setf result session)))))))
    (when (and expired-id *use-worker-pool*)
      (ignore-errors (release-session expired-id)))
    result))

(defun create-session ()
  "Create a new session and return it."
  (let* ((id (generate-session-id))
         (session (make-http-session :id id :state (make-state))))
    (bordeaux-threads:with-lock-held (*sessions-lock*)
      (setf (gethash id *sessions*) session))
    session))

(defun delete-session (session-id)
  "Delete a session by ID.  Also releases the worker pool assignment."
  (bordeaux-threads:with-lock-held (*sessions-lock*)
    (remhash session-id *sessions*))
  (when *use-worker-pool*
    (release-session session-id)))

;;; ------------------------------------------------------------
;;; Session Cleanup
;;; ------------------------------------------------------------

(defvar *cleanup-thread* nil
  "Background thread for periodic session cleanup.")

(defvar *cleanup-running* nil
  "Flag controlling the session cleanup loop.")

(defparameter *cleanup-interval-seconds* 300
  "Interval in seconds between session cleanup sweeps.  Default is
300 (5 minutes).")

(defun %session-cleanup-loop ()
  "Periodically sweep expired sessions and release their workers.
Runs until *cleanup-running* becomes NIL.  Uses short sleep intervals
for responsive shutdown (at most 1 second delay).
Sessions with active in-flight requests are skipped even when expired."
  (loop while *cleanup-running*
        do (loop repeat (ceiling *cleanup-interval-seconds*)
                 while *cleanup-running*
                 do (sleep 1))
           (when *cleanup-running*
             (handler-case
                 (let ((expired nil)
                       (skipped 0)
                       (now (get-universal-time)))
                   (bordeaux-threads:with-lock-held (*sessions-lock*)
                     (when *session-timeout-seconds*
                       (maphash
                        (lambda (id session)
                          (when (> (- now (http-session-last-access session))
                                   *session-timeout-seconds*)
                            (let ((active
                                    (bordeaux-threads:with-lock-held
                                        ((http-session-active-requests-lock session))
                                      (http-session-active-requests session))))
                              (if (zerop active)
                                  (push id expired)
                                  (incf skipped)))))
                        *sessions*)
                       (dolist (id expired)
                         (remhash id *sessions*))))
                   (when expired
                     (log-event :info "http.session.cleanup"
                                "expired_count" (length expired))
                     (when *use-worker-pool*
                       (dolist (id expired)
                         (ignore-errors (release-session id)))))
                   (when (plusp skipped)
                     (log-event :debug "http.session.cleanup.skipped"
                                "skipped_count" skipped)))
               (error (e)
                 (log-event :error "http.session.cleanup.error"
                            "error" (princ-to-string e)))))))

(defun %start-session-cleanup ()
  "Start the background session cleanup thread."
  (setf *cleanup-running* t
        *cleanup-thread*
        (bordeaux-threads:make-thread #'%session-cleanup-loop
                                      :name "session-cleanup")))

(defun %stop-session-cleanup ()
  "Stop the background session cleanup thread."
  (setf *cleanup-running* nil)
  (when (and *cleanup-thread*
             (bordeaux-threads:threadp *cleanup-thread*)
             (bordeaux-threads:thread-alive-p *cleanup-thread*))
    (ignore-errors (bordeaux-threads:join-thread *cleanup-thread*)))
  (setf *cleanup-thread* nil))

;;; ------------------------------------------------------------
;;; HTTP Handlers
;;; ------------------------------------------------------------

(defun get-header (name)
  "Get HTTP request header value."
  (hunchentoot:header-in name hunchentoot:*request*))

(defun set-header (name value)
  "Set HTTP response header."
  (setf (hunchentoot:header-out name) value))

(defun json-response (content &key (status 200))
  "Send a JSON response."
  (setf (hunchentoot:return-code*) status)
  (setf (hunchentoot:content-type*) "application/json")
  content)

(defun initialize-request-p (body)
  "Check if the request body is an initialize request."
  (and (hash-table-p body)
       (string= (gethash "method" body) "initialize")))

(defun parse-json-body ()
  "Parse the request body as JSON."
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (when (and body (plusp (length body)))
      (handler-case
          (yason:parse body)
        (error (e)
          (log-event :warn "http.parse-error" "error" (princ-to-string e))
          nil)))))

(defun %http-error-json (code message)
  "Build a JSON-RPC error response string using yason for proper escaping."
  (let ((error-obj (make-hash-table :test #'equal))
        (outer (make-hash-table :test #'equal)))
    (setf (gethash "code" error-obj) code
          (gethash "message" error-obj) message)
    (setf (gethash "jsonrpc" outer) "2.0"
          (gethash "error" outer) error-obj
          (gethash "id" outer) :null)
    (with-output-to-string (s)
      (yason:encode outer s))))

(defun %handle-mcp-post-initialize (body)
  (when (initialize-request-p body)
    (let* ((session (create-session))
           (state (http-session-state session))
           (line (with-output-to-string (s)
                   (yason:encode body s)))
           (response (let ((cl-mcp/src/protocol:*current-session-id* (http-session-id session)))
                       (process-json-line line state))))
      (log-event :info "http.initialize"
                 "session-id" (http-session-id session))
      (set-header :mcp-session-id (http-session-id session))
      response)))

(defun %handle-mcp-post-session (session-id body)
  (let ((session (get-session session-id)))
    (unless session
      (return-from %handle-mcp-post-session (values nil :invalid)))
    ;; Increment active-requests to prevent cleanup during processing
    (bordeaux-threads:with-lock-held ((http-session-active-requests-lock session))
      (incf (http-session-active-requests session)))
    (unwind-protect
         (let* ((state (http-session-state session))
                (line (with-output-to-string (s)
                        (yason:encode body s)))
                (response (let ((cl-mcp/src/protocol:*current-session-id* session-id))
                            (process-json-line line state))))
           (log-event :debug "http.response"
                      "session-id" session-id
                      "has-response" (not (null response)))
           (if response
               (values response :response)
               (values nil :notification)))
      ;; Always decrement, even on error
      (bordeaux-threads:with-lock-held ((http-session-active-requests-lock session))
        (decf (http-session-active-requests session))))))

(defun handle-mcp-post ()
  "Handle POST requests to the MCP endpoint."
  ;; Auth check
  (unless (%check-auth)
    (return-from handle-mcp-post
      (json-response (%http-error-json -32600 "Unauthorized") :status 401)))

  (let ((accept (get-header :accept))
        (session-id (get-header :mcp-session-id))
        (body (parse-json-body)))
    (log-event :debug "http.post"
               "session-id" session-id
               "has-body" (not (null body)))

    ;; Validate Accept header
    (unless (or (null accept)
                (search "application/json" accept)
                (search "text/event-stream" accept)
                (search "*/*" accept))
      (return-from handle-mcp-post
        (json-response (%http-error-json -32600 "Invalid Accept header")
                       :status 406)))

    ;; Handle missing body
    (unless body
      (return-from handle-mcp-post
        (json-response (%http-error-json -32700 "Parse error")
                       :status 400)))

    ;; Handle initialization (no session required)
    (let ((init-response (%handle-mcp-post-initialize body)))
      (when init-response
        (return-from handle-mcp-post
          (json-response init-response))))

    ;; Require session for non-initialize requests
    (unless session-id
      (return-from handle-mcp-post
        (json-response (%http-error-json -32600 "Missing Mcp-Session-Id header")
                       :status 400)))

    ;; Process the request
    (multiple-value-bind (response status)
        (%handle-mcp-post-session session-id body)
      (cond
        ((eq status :invalid)
         (json-response (%http-error-json -32600 "Session expired or invalid")
                        :status 404))
        ((eq status :response)
         (json-response response))
        (t
         ;; Notification - no response body
         (setf (hunchentoot:return-code*) 202)
         "")))))

(defun handle-mcp-get ()
  "Handle GET requests to the MCP endpoint (SSE stream).
Currently returns 405 as SSE is not yet implemented."
  ;; Auth check
  (unless (%check-auth)
    (return-from handle-mcp-get
      (json-response (%http-error-json -32600 "Unauthorized") :status 401)))
  (log-event :debug "http.get.sse-not-supported")
  (json-response (%http-error-json -32601
                                   "SSE not yet supported. Use POST for request/response.")
                 :status 405))

(defun handle-mcp-delete ()
  "Handle DELETE requests to terminate a session."
  ;; Auth check
  (unless (%check-auth)
    (return-from handle-mcp-delete
      (json-response (%http-error-json -32600 "Unauthorized") :status 401)))
  (let ((session-id (get-header :mcp-session-id)))
    (cond
      (session-id
       (delete-session session-id)
       (log-event :info "http.session.deleted" "session-id" session-id)
       (setf (hunchentoot:return-code*) 204)
       "")
      (t
       (json-response (%http-error-json -32600 "Missing Mcp-Session-Id header")
                      :status 400)))))

(defun handle-mcp-options ()
  "Handle OPTIONS requests for CORS preflight."
  (set-header :access-control-allow-methods "GET, POST, DELETE, OPTIONS")
  (set-header :access-control-allow-headers "Content-Type, Accept, Authorization, Mcp-Session-Id")
  (set-header :access-control-max-age "86400")
  (setf (hunchentoot:return-code*) 204)
  "")

;;; ------------------------------------------------------------
;;; Dispatcher
;;; ------------------------------------------------------------

(defun %loopback-origin-p (origin)
  "Return T only if ORIGIN is a loopback address.
Validates the character after the hostname to prevent substring
attacks like localhost.evil.com."
  (flet ((check-prefix (prefix)
           (let ((len (length prefix)))
             (and (>= (length origin) len)
                  (string-equal origin prefix :end1 len)
                  ;; After prefix: end-of-string, colon (port), or slash (path)
                  (or (= (length origin) len)
                      (char= (char origin len) #\:)
                      (char= (char origin len) #\/))))))
    (or (check-prefix "http://localhost")
        (check-prefix "https://localhost")
        (check-prefix "http://127.0.0.1")
        (check-prefix "https://127.0.0.1")
        (check-prefix "http://[::1]")
        (check-prefix "https://[::1]"))))

(defun mcp-dispatcher (request)
  "Dispatch requests to the MCP endpoint."
  (let ((path (hunchentoot:script-name request))
        (method (hunchentoot:request-method request)))
    (when (string= path "/mcp")
      (lambda ()
        ;; Set CORS headers — restrict to loopback origins
        (let ((origin (get-header :origin)))
          (when (and origin (%loopback-origin-p origin))
            (set-header :access-control-allow-origin origin)))
        (set-header :access-control-expose-headers "Mcp-Session-Id")

        (case method
          (:post (handle-mcp-post))
          (:get (handle-mcp-get))
          (:delete (handle-mcp-delete))
          (:options (handle-mcp-options))
          (otherwise
           (setf (hunchentoot:return-code*) 405)
           (json-response "{\"error\":\"Method not allowed\"}")))))))

;;; ------------------------------------------------------------
;;; Server Control
;;; ------------------------------------------------------------

(defclass mcp-acceptor (hunchentoot:acceptor)
  ()
  (:documentation "Custom acceptor for MCP HTTP server."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor mcp-acceptor) request)
  "Dispatch incoming requests."
  (let ((handler (mcp-dispatcher request)))
    (if handler
        (funcall handler)
        (call-next-method))))

(defun http-server-running-p ()
  "Return T when the HTTP server is running."
  (and *http-server*
       (hunchentoot:started-p *http-server*)))

(defun start-http-server (&key (host "127.0.0.1") (port 3000)
                               (token :generate))
  "Start the MCP HTTP server.
TOKEN controls authentication:
  :GENERATE (default) - auto-generate a random Bearer token
  NIL                 - no authentication (suitable for local development)
  \"<string>\"        - use the given string as the Bearer token
When a token is active, all requests (except OPTIONS) must include
an Authorization: Bearer <token> header.
Returns the acceptor instance and port number."
  (when (http-server-running-p)
    (log-event :info "http.already-running" "port" *http-server-port*)
    (return-from start-http-server (values *http-server* *http-server-port*)))

  ;; Configure authentication
  (setf *http-auth-token*
        (cond
          ((eq token :generate) (%generate-auth-token))
          ((stringp token) token)
          (t nil)))

  (when *http-auth-token*
    (format *error-output* "~&MCP Auth Token: ~A~%" *http-auth-token*)
    (force-output *error-output*)
    (log-event :info "http.auth.enabled"))

  (log-event :info "http.starting" "host" host "port" port)

  (setf *http-server*
        (make-instance 'mcp-acceptor
                       :address host
                       :port port
                       :access-log-destination nil
                       :message-log-destination nil))

  ;; Initialize pool before accepting connections to avoid race window
  (when *use-worker-pool*
    (initialize-pool))

  (hunchentoot:start *http-server*)
  (setf *http-server-port* (hunchentoot:acceptor-port *http-server*))

  ;; Start periodic session cleanup
  (%start-session-cleanup)

  (log-event :info "http.started"
             "host" host
             "port" *http-server-port*
             "url" (format nil "http://~A:~D/mcp" host *http-server-port*))

  (values *http-server* *http-server-port*))

(defun stop-http-server ()
  "Stop the MCP HTTP server."
  (when (http-server-running-p)
    (log-event :info "http.stopping" "port" *http-server-port*)
    (hunchentoot:stop *http-server*)
    (setf *http-server* nil
          *http-server-port* nil
          *http-auth-token* nil)
    ;; Stop session cleanup thread
    (%stop-session-cleanup)
    ;; Shut down worker pool before clearing sessions
    (when *use-worker-pool*
      (shutdown-pool))
    ;; Clear all sessions
    (bordeaux-threads:with-lock-held (*sessions-lock*)
      (clrhash *sessions*))
    (log-event :info "http.stopped")
    t))
