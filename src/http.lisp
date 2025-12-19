;;;; src/http.lisp
;;;; MCP Streamable HTTP Transport using Hunchentoot

(defpackage #:cl-mcp/src/http
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/protocol #:make-state #:process-json-line)
  (:import-from #:bordeaux-threads #:make-lock #:with-lock-held)
  (:import-from #:hunchentoot)
  (:import-from #:yason)
  (:export
   #:*http-server*
   #:*http-server-port*
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

;;; ------------------------------------------------------------
;;; Session Management
;;; ------------------------------------------------------------

(defvar *sessions* (make-hash-table :test #'equal)
  "Hash table mapping session-id to server-state.")

(defvar *sessions-lock* (bordeaux-threads:make-lock "sessions-lock")
  "Lock for thread-safe session access.")

(defparameter *session-timeout-seconds*
  nil
  "Session expiration time in seconds. NIL disables timeout (default).
Set to a positive number to enable idle session expiration.")

(defstruct http-session
  "HTTP session containing MCP state and metadata."
  (id "" :type string)
  (state nil)
  (created-at (get-universal-time) :type integer)
  (last-access (get-universal-time) :type integer))

(defun generate-session-id ()
  "Generate a pseudo-random session ID."
  (format nil "~(~36,16,'0R~)-~(~36,16,'0R~)-~(~36,16,'0R~)-~(~36,16,'0R~)"
          (random (expt 2 64))
          (random (expt 2 64))
          (random (expt 2 64))
          (random (expt 2 64))))

(defun get-session (session-id)
  "Get session by ID, returning NIL if not found or expired.
When *session-timeout-seconds* is NIL, sessions never expire."
  (bordeaux-threads:with-lock-held (*sessions-lock*)
    (let ((session (gethash session-id *sessions*)))
      (when session
        (let ((now (get-universal-time)))
          (cond
           ;; Timeout disabled - always return session
           ((null *session-timeout-seconds*)
            (setf (http-session-last-access session) now)
            session)
           ;; Timeout enabled - check expiration
           ((> (- now (http-session-last-access session))
               *session-timeout-seconds*)
            (remhash session-id *sessions*)
            nil)
           ;; Not expired - update last access and return
           (t
            (setf (http-session-last-access session) now)
            session)))))))

(defun create-session ()
  "Create a new session and return it."
  (let* ((id (generate-session-id))
         (session (make-http-session :id id :state (make-state))))
    (bordeaux-threads:with-lock-held (*sessions-lock*)
      (setf (gethash id *sessions*) session))
    session))

(defun delete-session (session-id)
  "Delete a session by ID."
  (bordeaux-threads:with-lock-held (*sessions-lock*)
    (remhash session-id *sessions*)))

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

(defun is-initialize-request-p (body)
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
  (when (is-initialize-request-p body)
    (let* ((session (create-session))
           (state (http-session-state session))
           (line (with-output-to-string (s)
                   (yason:encode body s)))
           (response (process-json-line line state)))
      (log-event :info "http.initialize"
                 "session-id" (http-session-id session))
      (set-header :mcp-session-id (http-session-id session))
      response)))

(defun %handle-mcp-post-session (session-id body)
  (let ((session (get-session session-id)))
    (unless session
      (return-from %handle-mcp-post-session (values nil :invalid)))
    (let* ((state (http-session-state session))
           (line (with-output-to-string (s)
                   (yason:encode body s)))
           (response (process-json-line line state)))
      (log-event :debug "http.response"
                 "session-id" session-id
                 "has-response" (not (null response)))
      (if response
          (values response :response)
          (values nil :notification)))))
(defun handle-mcp-post ()
  "Handle POST requests to the MCP endpoint."
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
  (log-event :debug "http.get.sse-not-supported")
  (json-response (%http-error-json -32601
                                   "SSE not yet supported. Use POST for request/response.")
                 :status 405))

(defun handle-mcp-delete ()
  "Handle DELETE requests to terminate a session."
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
  (set-header :access-control-allow-headers "Content-Type, Accept, Mcp-Session-Id")
  (set-header :access-control-max-age "86400")
  (setf (hunchentoot:return-code*) 204)
  "")

;;; ------------------------------------------------------------
;;; Dispatcher
;;; ------------------------------------------------------------

(defun mcp-dispatcher (request)
  "Dispatch requests to the MCP endpoint."
  (let ((path (hunchentoot:script-name request))
        (method (hunchentoot:request-method request)))
    (when (string= path "/mcp")
      (lambda ()
        ;; Set CORS headers for local development
        (set-header :access-control-allow-origin "*")
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

(defun start-http-server (&key (host "127.0.0.1") (port 3000))
  "Start the MCP HTTP server.
Returns the acceptor instance and port number."
  (when (http-server-running-p)
    (log-event :info "http.already-running" "port" *http-server-port*)
    (return-from start-http-server (values *http-server* *http-server-port*)))

  (log-event :info "http.starting" "host" host "port" port)

  (setf *http-server*
        (make-instance 'mcp-acceptor
                       :address host
                       :port port
                       :access-log-destination nil
                       :message-log-destination nil))

  (hunchentoot:start *http-server*)
  (setf *http-server-port* (hunchentoot:acceptor-port *http-server*))

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
          *http-server-port* nil)
    ;; Clear all sessions
    (bordeaux-threads:with-lock-held (*sessions-lock*)
      (clrhash *sessions*))
    (log-event :info "http.stopped")
    t))
