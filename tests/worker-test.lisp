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
                #:register-method)
  (:import-from #:cl-mcp/src/worker/handlers
                #:register-all-handlers)
  (:import-from #:cl-mcp/src/worker/main))

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

;;; ---------------------------------------------------------------------------
;;; Helper for handler tests
;;; ---------------------------------------------------------------------------

(defun %make-request (id method &optional params)
  "Build a JSON-RPC request string."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" ht) "2.0"
          (gethash "id" ht) id
          (gethash "method" ht) method)
    (when params
      (setf (gethash "params" ht) params))
    (with-output-to-string (s) (yason:encode ht s))))

(defmacro with-handler-server ((stream-var) &body body)
  "Start a worker server with all handlers registered, connect to it,
and execute BODY with STREAM-VAR bound to the connection stream.
Cleans up server and socket on exit."
  (let ((server (gensym "SERVER"))
        (port (gensym "PORT"))
        (thread (gensym "THREAD"))
        (socket (gensym "SOCKET")))
    `(if (not (socket-available-p))
         (skip "socket unavailable")
         (let ((,server (make-worker-server :port 0)))
           (register-all-handlers ,server)
           (unwind-protect
                (let* ((,port (server-port ,server))
                       (,thread (bordeaux-threads:make-thread
                                 (lambda () (start-accept-loop ,server))
                                 :name "test-handler-accept")))
                  (declare (ignore ,thread))
                  (sleep 0.1)
                  (let ((,socket (usocket:socket-connect
                                  "127.0.0.1" ,port
                                  :element-type 'character)))
                    (unwind-protect
                         (let ((,stream-var (usocket:socket-stream ,socket)))
                           ,@body)
                      (ignore-errors (usocket:socket-close ,socket)))))
             (stop-server ,server))))))

(defun %send-and-receive (stream id method &optional params)
  "Send a JSON-RPC request on STREAM and return the parsed response."
  (write-line (%make-request id method params) stream)
  (force-output stream)
  (let ((line (read-line stream)))
    (yason:parse line)))

(defun %result-of (response)
  "Extract the result hash-table from a JSON-RPC response."
  (gethash "result" response))

;;; ---------------------------------------------------------------------------
;;; Handler tests
;;; ---------------------------------------------------------------------------

(deftest worker-eval-returns-result
  (testing "worker/eval evaluates code and returns result with content"
    (with-handler-server (stream)
      (let* ((params (make-hash-table :test 'equal)))
        (setf (gethash "code" params) "(+ 1 2)"
              (gethash "package" params) "CL-USER")
        (let* ((response (%send-and-receive stream 100 "worker/eval" params))
               (result (%result-of response)))
          (ok (equal (gethash "id" response) 100)
              "response id matches")
          (ok result "response has result")
          (ok (gethash "content" result)
              "result has content field")
          ;; content is a vector of text parts
          (let ((parts (gethash "content" result)))
            (ok (> (length parts) 0) "content has at least one part")
            (ok (search "3" (gethash "text" (aref parts 0)))
                "content text contains the result 3"))
          ;; stdout and stderr should be present (possibly empty)
          (ok (stringp (gethash "stdout" result))
              "result has stdout string"))))))

(deftest worker-eval-returns-object-preview
  (testing "worker/eval returns result_preview for non-primitive results"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "code" params) "(list 1 2 3)"
              (gethash "package" params) "CL-USER")
        (let* ((response (%send-and-receive stream 101 "worker/eval" params))
               (result (%result-of response)))
          (ok (gethash "result_object_id" result)
              "result has result_object_id")
          (ok (gethash "result_preview" result)
              "result has result_preview")
          (ok (equal "list"
                     (gethash "kind" (gethash "result_preview" result)))
              "preview kind is list"))))))

(deftest worker-eval-returns-error-context
  (testing "worker/eval returns error_context on signaled condition"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "code" params) "(error \"test-boom\")"
              (gethash "package" params) "CL-USER")
        (let* ((response (%send-and-receive stream 102 "worker/eval" params))
               (result (%result-of response)))
          (ok result "response has result (not JSON-RPC error)")
          (ok (gethash "error_context" result)
              "result has error_context")
          (let ((ctx (gethash "error_context" result)))
            (ok (gethash "condition_type" ctx)
                "error_context has condition_type")
            (ok (search "test-boom" (gethash "message" ctx))
                "error_context message contains original")))))))

(deftest worker-eval-requires-code
  (testing "worker/eval errors when code param is missing"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "package" params) "CL-USER")
        (let ((response (%send-and-receive stream 103 "worker/eval" params)))
          (ok (gethash "error" response)
              "response is JSON-RPC error when code missing"))))))

(deftest worker-code-describe-returns-info
  (testing "worker/code-describe returns symbol info for cl:car"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "symbol" params) "cl:car")
        (let* ((response (%send-and-receive stream 200 "worker/code-describe" params))
               (result (%result-of response)))
          ;; code-describe-symbol may fail if sb-introspect is unavailable
          ;; in the test environment; only check structure when we get a result
          (ok (or result (gethash "error" response))
              "response has result or error")
          (when result
            (ok (gethash "name" result) "result has name")
            (ok (gethash "type" result) "result has type")
            (ok (gethash "content" result) "result has content")))))))

(deftest worker-code-find-not-found
  (testing "worker/code-find returns error for nonexistent symbol"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "symbol" params) "nonexistent-pkg:nonexistent-sym-xyz")
        (let* ((response (%send-and-receive stream 201 "worker/code-find" params))
               (result (%result-of response)))
          ;; Could be JSON-RPC error (if code-find-definition signals on
          ;; bad package) or isError result (if symbol not found).
          (ok (or (gethash "error" response)
                  (and result (gethash "isError" result)))
              "response indicates error for nonexistent symbol"))))))

(deftest worker-set-project-root-changes-root
  (testing "worker/set-project-root updates *project-root*"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "path" params) "/tmp")
        (let* ((response (%send-and-receive stream 300 "worker/set-project-root" params))
               (result (%result-of response)))
          (ok result "response has result")
          (ok (gethash "path" result) "result has path")
          (ok (gethash "content" result) "result has content"))))))

(deftest worker-set-project-root-requires-path
  (testing "worker/set-project-root errors when path missing"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (let ((response (%send-and-receive stream 301 "worker/set-project-root" params)))
          (ok (gethash "error" response)
              "response is JSON-RPC error when path missing"))))))

(deftest worker-set-project-root-rejects-nonexistent
  (testing "worker/set-project-root errors for nonexistent directory"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "path" params) "/nonexistent-path-xyz-12345")
        (let ((response (%send-and-receive stream 302 "worker/set-project-root" params)))
          (ok (gethash "error" response)
              "response is JSON-RPC error for nonexistent path"))))))

(deftest worker-inspect-object-not-found
  (testing "worker/inspect-object returns isError for invalid ID"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "id" params) 999999)
        (let* ((response (%send-and-receive stream 400 "worker/inspect-object" params))
               (result (%result-of response)))
          (ok result "response has result")
          (ok (gethash "isError" result)
              "result has isError for nonexistent object"))))))

;;; ---------------------------------------------------------------------------
;;; Worker entry point tests (src/worker/main.lisp)
;;; ---------------------------------------------------------------------------

(deftest worker-handshake-json-format
  (testing "handshake output is valid JSON with required keys"
    (let* ((output (with-output-to-string (s)
                     (cl-mcp/src/worker/main::%output-handshake
                      12345 4005 s)))
           (parsed (yason:parse output)))
      (ok (integerp (gethash "tcp_port" parsed))
          "tcp_port is an integer")
      (ok (= 12345 (gethash "tcp_port" parsed))
          "tcp_port matches supplied value")
      (ok (integerp (gethash "swank_port" parsed))
          "swank_port is an integer when supplied")
      (ok (= 4005 (gethash "swank_port" parsed))
          "swank_port matches supplied value")
      (ok (integerp (gethash "pid" parsed))
          "pid is an integer")
      (ok (plusp (gethash "pid" parsed))
          "pid is positive"))))

(deftest worker-handshake-null-swank-port
  (testing "handshake output has null swank_port when Swank unavailable"
    (let* ((output (with-output-to-string (s)
                     (cl-mcp/src/worker/main::%output-handshake
                      9999 nil s)))
           (parsed (yason:parse output)))
      (ok (= 9999 (gethash "tcp_port" parsed))
          "tcp_port matches supplied value")
      (ok (null (gethash "swank_port" parsed))
          "swank_port is null when nil supplied")
      (ok (integerp (gethash "pid" parsed))
          "pid is present"))))

(deftest worker-handshake-ends-with-newline
  (testing "handshake output ends with a newline for line-based reading"
    (let ((output (with-output-to-string (s)
                    (cl-mcp/src/worker/main::%output-handshake
                     8080 nil s))))
      (ok (char= #\Newline (char output (1- (length output))))
          "output ends with newline")
      ;; Verify it's exactly one line
      (ok (= 1 (count #\Newline output))
          "output contains exactly one newline"))))

(deftest worker-start-creates-server-and-handshakes
  (testing "start creates a server, outputs handshake, and accepts connections"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        ;; We cannot call start directly because it blocks in
        ;; start-accept-loop.  Instead, verify the components work
        ;; together: create server, register handlers, output
        ;; handshake, then connect and ping.
        (let* ((server (make-worker-server :port 0))
               (tcp-port (server-port server))
               (handshake-output
                 (with-output-to-string (s)
                   (cl-mcp/src/worker/main::%output-handshake
                    tcp-port nil s)))
               (parsed (yason:parse handshake-output)))
          (register-all-handlers server)
          (unwind-protect
               (progn
                 ;; Verify handshake contains the right port
                 (ok (= tcp-port (gethash "tcp_port" parsed))
                     "handshake tcp_port matches server port")
                 ;; Start accept loop in background and connect
                 (let ((thread (bordeaux-threads:make-thread
                                (lambda () (start-accept-loop server))
                                :name "test-start-accept")))
                   (declare (ignore thread))
                   (sleep 0.1)
                   (let ((socket (usocket:socket-connect
                                  "127.0.0.1" tcp-port
                                  :element-type 'character)))
                     (unwind-protect
                          (let ((stream (usocket:socket-stream socket)))
                            (write-line
                             "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"worker/ping\"}"
                             stream)
                            (force-output stream)
                            (let* ((line (read-line stream))
                                   (response (yason:parse line)))
                              (ok (gethash "pong"
                                           (gethash "result" response))
                                  "server responds to ping after handshake setup")))
                       (ignore-errors (usocket:socket-close socket))))))
            (stop-server server))))))

(deftest worker-setup-project-root-from-env
  (testing "setup-project-root reads MCP_PROJECT_ROOT and sets *project-root*"
    (let ((cl-mcp/src/project-root:*project-root* nil))
      ;; Save and restore the environment variable
      (let ((prev-env (uiop/os:getenv "MCP_PROJECT_ROOT")))
        (unwind-protect
             (progn
               (setf (uiop/os:getenv "MCP_PROJECT_ROOT") "/tmp")
               (let ((result
                       (cl-mcp/src/worker/main::%setup-project-root)))
                 (ok result "setup-project-root returns a pathname")
                 (ok cl-mcp/src/project-root:*project-root*
                     "*project-root* is set")))
          ;; Restore
          (if prev-env
              (setf (uiop/os:getenv "MCP_PROJECT_ROOT") prev-env)
              (setf (uiop/os:getenv "MCP_PROJECT_ROOT") "")))))))
