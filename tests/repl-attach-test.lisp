;;;; tests/repl-attach-test.lisp
;;;;
;;;; Tests for cl-mcp/src/attach: configuration parsing and env wiring.
;;;; Connection-cache, dispatch macro, and end-to-end Slynk round-trip
;;;; tests are added in subsequent commits as the corresponding behaviour
;;;; lands.

(defpackage #:cl-mcp/tests/repl-attach-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/attach
                #:*attach-config*
                #:attach-config-host
                #:attach-config-port
                #:parse-attach-spec
                #:set-attach-from-env))

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
