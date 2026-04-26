;;;; tests/repl-attach-test.lisp
;;;;
;;;; Tests for cl-mcp/src/attach: configuration parsing, env wiring, and
;;;; per-session connection cache.  Dispatch macro and end-to-end
;;;; Slynk round-trip tests are added in subsequent commits as the
;;;; corresponding behaviour lands.

(defpackage #:cl-mcp/tests/repl-attach-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/attach
                #:*attach-config*
                #:make-attach-config
                #:attach-config-host
                #:attach-config-port
                #:parse-attach-spec
                #:set-attach-from-env
                #:attach-active-p
                #:attach-disconnect))

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
