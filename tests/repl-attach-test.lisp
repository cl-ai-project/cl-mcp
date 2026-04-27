;;;; tests/repl-attach-test.lisp
;;;;
;;;; Tests for cl-mcp/src/attach: configuration parsing, env wiring,
;;;; per-session connection cache, and the with-attach-dispatch macro
;;;; that routes repl-eval to a transient slynk:create-server fixture.

(defpackage #:cl-mcp/tests/repl-attach-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/src/attach
                #:*attach-config*
                #:make-attach-config
                #:attach-config-host
                #:attach-config-port
                #:parse-attach-spec
                #:set-attach-from-env
                #:attach-active-p
                #:attach-disconnect
                #:with-attach-dispatch))

(in-package #:cl-mcp/tests/repl-attach-test)

(defmacro %signals-error (&body body)
  "Run BODY and return T if it signalled a CL:ERROR, NIL otherwise.
Use this instead of Rove's `signals' so the assertion form survives
fasl-dumping (the default `#'identity' filter form Rove generates does
not)."
  `(handler-case (progn ,@body nil)
     (error () t)))

(defmacro %with-clean-env (&body body)
  "Bind *attach-config* to NIL and unset CL_MCP_SLYNK_ATTACH for the
duration of BODY, restoring the previous environment after."
  `(let ((*attach-config* nil)
         (saved (uiop:getenv "CL_MCP_SLYNK_ATTACH")))
     (unwind-protect
          (progn
            (setf (uiop:getenv "CL_MCP_SLYNK_ATTACH") nil)
            ,@body)
       (setf (uiop:getenv "CL_MCP_SLYNK_ATTACH") (or saved "")))))

;;; -- parse-attach-spec ------------------------------------------------------

(deftest parse-attach-spec/host-port
  (testing "valid host:port returns host and port on the struct"
    (let ((c (parse-attach-spec "127.0.0.1:4005")))
      (ok (string= (attach-config-host c) "127.0.0.1"))
      (ok (= (attach-config-port c) 4005))))
  (testing "trims surrounding whitespace before parsing"
    (let ((c (parse-attach-spec "  localhost:4005  ")))
      (ok (string= (attach-config-host c) "localhost"))
      (ok (= (attach-config-port c) 4005)))))

(deftest parse-attach-spec/rejects-empty
  (testing "empty string is rejected"
    (ok (%signals-error (parse-attach-spec ""))))
  (testing "whitespace-only string is rejected"
    (ok (%signals-error (parse-attach-spec "   "))))
  (testing "non-string input is rejected"
    (ok (%signals-error (parse-attach-spec :foo)))
    (ok (%signals-error (parse-attach-spec 4005)))))

(deftest parse-attach-spec/rejects-missing-colon
  (testing "no colon is rejected"
    (ok (%signals-error (parse-attach-spec "localhost"))))
  (testing "empty host is rejected"
    (ok (%signals-error (parse-attach-spec ":4005"))))
  (testing "empty port is rejected"
    (ok (%signals-error (parse-attach-spec "host:")))))

(deftest parse-attach-spec/rejects-bad-port
  (testing "non-integer port is rejected"
    (ok (%signals-error (parse-attach-spec "host:abc")))
    (ok (%signals-error (parse-attach-spec "host:4005x"))))
  (testing "port 0 is rejected"
    (ok (%signals-error (parse-attach-spec "host:0"))))
  (testing "port above 65535 is rejected"
    (ok (%signals-error (parse-attach-spec "host:99999")))
    (ok (%signals-error (parse-attach-spec "host:65536"))))
  (testing "negative port is rejected"
    (ok (%signals-error (parse-attach-spec "host:-1")))))

;;; -- set-attach-from-env ---------------------------------------------------

(deftest set-attach-from-env/honours-env
  (testing "non-empty CL_MCP_SLYNK_ATTACH binds *attach-config*"
    (%with-clean-env
      (setf (uiop:getenv "CL_MCP_SLYNK_ATTACH") "127.0.0.1:4005")
      (let ((cfg (set-attach-from-env)))
        (ok (not (null cfg)))
        (ok (string= (attach-config-host cfg) "127.0.0.1"))
        (ok (= (attach-config-port cfg) 4005))
        (ok (eq cfg *attach-config*))))))

(deftest set-attach-from-env/empty-env-leaves-nil
  (testing "unset CL_MCP_SLYNK_ATTACH leaves *attach-config* untouched"
    (%with-clean-env
      (ok (null (set-attach-from-env)))
      (ok (null *attach-config*))))
  (testing "empty CL_MCP_SLYNK_ATTACH leaves *attach-config* untouched"
    (%with-clean-env
      (setf (uiop:getenv "CL_MCP_SLYNK_ATTACH") "")
      (ok (null (set-attach-from-env)))
      (ok (null *attach-config*))))
  (testing "whitespace-only CL_MCP_SLYNK_ATTACH leaves *attach-config* untouched"
    (%with-clean-env
      (setf (uiop:getenv "CL_MCP_SLYNK_ATTACH") "   ")
      (ok (null (set-attach-from-env)))
      (ok (null *attach-config*))))
  (testing "malformed CL_MCP_SLYNK_ATTACH raises and *attach-config* stays nil"
    (%with-clean-env
      (setf (uiop:getenv "CL_MCP_SLYNK_ATTACH") "no-colon")
      (ok (%signals-error (set-attach-from-env)))
      (ok (null *attach-config*)))))

;;; -- attach-active-p / connection cache -----------------------------------

(defmacro %with-slynk-fixture ((port-var) &body body)
  "Spin up a transient Slynk server on an OS-assigned port, bind PORT-VAR
to that port, and tear it down on exit.  Tests that need attach to be
configured should establish *attach-config* themselves before calling
`%get-or-open-connection'."
  (let ((p (gensym "PORT-"))
        (cfg-snapshot (gensym "CFG-")))
    `(let* ((,p (slynk:create-server :port 0 :dont-close t))
            (,cfg-snapshot *attach-config*)
            (,port-var ,p))
       (unwind-protect
            (progn ,@body)
         ;; Drop any cached state we leaked into the package.
         (let ((conns
                (symbol-value
                 (find-symbol "*SESSION-CONNECTIONS*" :cl-mcp/src/attach)))
               (locks
                (symbol-value
                 (find-symbol "*SESSION-CALL-LOCKS*" :cl-mcp/src/attach))))
           (loop for sid being the hash-keys of conns
                 do (attach-disconnect sid :reason "fixture-teardown"))
           (clrhash conns)
           (clrhash locks))
         (setf *attach-config* ,cfg-snapshot)
         (handler-case (slynk:stop-server ,p)
           (error () nil))))))

(defun %open-conn (session-id)
  "Internal helper that exercises %get-or-open-connection without
exporting it from the attach package."
  (funcall (find-symbol "%GET-OR-OPEN-CONNECTION" :cl-mcp/src/attach)
           session-id))

(deftest attach-active-p/follows-config
  (testing "NIL config means attach is not active"
    (let ((*attach-config* nil))
      (ok (not (attach-active-p)))))
  (testing "non-NIL config makes attach active"
    (let ((*attach-config* (make-attach-config :host "x" :port 1)))
      (ok (attach-active-p)))))

(deftest connection-cache/same-session-returns-eq-connection
  (testing "two opens for the same session return EQ"
    (%with-slynk-fixture (port)
      (let ((*attach-config* (make-attach-config :host "127.0.0.1"
                                                 :port port)))
        (let ((c1 (%open-conn "sess-A"))
              (c2 (%open-conn "sess-A")))
          (ok (not (null c1)))
          (ok (eq c1 c2)))))))

(deftest connection-cache/different-sessions-distinct
  (testing "two sessions get distinct connections"
    (%with-slynk-fixture (port)
      (let ((*attach-config* (make-attach-config :host "127.0.0.1"
                                                 :port port)))
        (let ((c1 (%open-conn "sess-A"))
              (c2 (%open-conn "sess-B")))
          (ok (not (null c1)))
          (ok (not (null c2)))
          (ok (not (eq c1 c2))))))))

(deftest attach-disconnect/removes-and-is-idempotent
  (testing "disconnect drops the cached entry"
    (%with-slynk-fixture (port)
      (let ((*attach-config* (make-attach-config :host "127.0.0.1"
                                                 :port port))
            (conns (symbol-value
                    (find-symbol "*SESSION-CONNECTIONS*"
                                 :cl-mcp/src/attach))))
        (%open-conn "sess-X")
        (ok (gethash "sess-X" conns))
        (attach-disconnect "sess-X" :reason "test")
        (ok (null (gethash "sess-X" conns)))
        ;; Idempotent: a second call is a no-op.
        (attach-disconnect "sess-X" :reason "test-second")
        (ok (null (gethash "sess-X" conns)))
        ;; Idempotent: NIL session is a no-op.
        (attach-disconnect nil)
        (ok t)))))

(deftest attach-disconnect/unknown-session-is-noop
  (testing "disconnecting a never-opened session does not error"
    (%with-slynk-fixture (port)
      (let ((*attach-config* (make-attach-config :host "127.0.0.1"
                                                 :port port)))
        (attach-disconnect "never-opened" :reason "test")
        (ok t)))))

;;; -- with-attach-dispatch --------------------------------------------------

(defmacro %with-attach-fixture (() &body body)
  "Spin up a transient Slynk server, bind *attach-config* to its
host/port, set *current-session-id* to a fresh string, and run BODY.
Tears down the connection and the server in an unwind-protect."
  (let ((p (gensym "PORT-"))
        (s (gensym "SESSION-")))
    `(%with-slynk-fixture (,p)
       (let ((,s (format nil "test-~A" (random 1000000)))
             (*attach-config* (make-attach-config :host "127.0.0.1"
                                                  :port ,p))
             (cl-mcp/src/state:*current-session-id* nil))
         (setf cl-mcp/src/state:*current-session-id* ,s)
         (unwind-protect
              (locally ,@body)
           (attach-disconnect ,s :reason "test-teardown"))))))

(defun %dispatch (id tool params)
  "Invoke the package-internal %dispatch-attach helper without exporting it."
  (funcall (find-symbol "%DISPATCH-ATTACH" :cl-mcp/src/attach)
           id tool params))

(deftest with-attach-dispatch/passthrough-when-unset
  (testing "attach unset: macro expands to body and runs it once"
    (let ((*attach-config* nil)
          (counter 0))
      (with-attach-dispatch (1 "repl-eval" (make-ht "code" "(+ 1 2)"))
        (incf counter))
      (ok (= counter 1)))))

(deftest with-attach-dispatch/round-trips-simple-form
  (testing "(+ 1 2) round-trips through slynk and returns 3 in content"
    (%with-attach-fixture ()
(let ((res (%dispatch 11 "repl-eval"
                            (make-ht "code" "(+ 1 2)"
                                     "package" "CL-USER"))))
        (ok (hash-table-p res))
        (let ((content (gethash "content" res)))
          (ok (and (vectorp content) (= 1 (length content))))
          (let ((text (gethash "text" (aref content 0))))
            (ok (search "3" text))))))))

(deftest with-attach-dispatch/captures-stdout-and-stderr
  (testing "stdout and stderr from the attached image are captured"
    (%with-attach-fixture ()
(let* ((res (%dispatch 12 "repl-eval"
                             (make-ht "code"
                                      "(progn (format t \"hi-out\") (format *error-output* \"hi-err\") (+ 7 8))"
                                      "package" "CL-USER")))
             (stdout (gethash "stdout" res))
             (stderr (gethash "stderr" res)))
        (ok (search "hi-out" stdout))
        (ok (search "hi-err" stderr))))))

(deftest with-attach-dispatch/captures-error-context
  (testing "an error in the attached image lands in error_context"
    (%with-attach-fixture ()
(let ((res (%dispatch 13 "repl-eval"
                            (make-ht "code" "(error \"boom\")"
                                     "package" "CL-USER"))))
        (ok (hash-table-p (gethash "error_context" res)))
        (let ((ec (gethash "error_context" res)))
          (ok (search "ERROR" (gethash "condition_type" ec)))
          (ok (search "boom" (gethash "message" ec))))))))

(deftest with-attach-dispatch/unsupported-tool-is-error
  (testing "unsupported tools return isError without falling through"
    (%with-attach-fixture ()
(let ((res (%dispatch 14 "load-system"
                            (make-ht "system" "foo"))))
        (ok (gethash "isError" res))
        (let* ((content (gethash "content" res))
               (text (and (vectorp content) (plusp (length content))
                          (gethash "text" (aref content 0)))))
          (ok (search "load-system" text))
          (ok (search "v1" text)))))))

(deftest with-attach-dispatch/missing-code-is-error
  (testing "missing 'code' parameter is a clean isError"
    (%with-attach-fixture ()
(let ((res (%dispatch 15 "repl-eval"
                            (make-ht "package" "CL-USER"))))
        (ok (gethash "isError" res))))))

(deftest with-attach-dispatch/preserves-state-across-calls
  (testing "two calls on the same session see each other's defparameter"
    (%with-attach-fixture ()
;; First call defines a marker in the attached image.
      (%dispatch 21 "repl-eval"
                 (make-ht "code" "(defparameter *cl-mcp-attach-marker* :present)"
                          "package" "CL-USER"))
      ;; Second call reads the marker and the result text shows :PRESENT.
      (let* ((res (%dispatch 22 "repl-eval"
                             (make-ht "code" "*cl-mcp-attach-marker*"
                                      "package" "CL-USER")))
             (text (gethash "text" (aref (gethash "content" res) 0))))
        (ok (search ":PRESENT" text))))))
