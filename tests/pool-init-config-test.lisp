;;;; tests/pool-init-config-test.lisp
;;;;
;;;; Tests for parent-side worker-init-hook config parsing, env denylist,
;;;; ownership election, and crash-breaker isolation.

(defpackage #:cl-mcp/tests/pool-init-config-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/pool
                #:*worker-init-config*))

(in-package #:cl-mcp/tests/pool-init-config-test)

(defun %with-env (bindings thunk)
  "Set env BINDINGS ((name . value) ...) for the duration of THUNK, then
restore.  A NIL value unsets the variable."
  (let ((saved (loop for (name . nil) in bindings
                     collect (cons name (uiop:getenv name)))))
    (unwind-protect
         (progn
           (loop for (name . value) in bindings
                 do (if value (setf (uiop/os:getenv name) value)
                        (sb-posix:unsetenv name)))
           (funcall thunk))
      (loop for (name . value) in saved
            do (if value (setf (uiop/os:getenv name) value)
                   (sb-posix:unsetenv name))))))

(deftest parse-worker-init-config
  (testing "config is nil when SYSTEM is unset, populated when set"
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . nil))
      (lambda ()
        (ok (null (cl-mcp/src/pool::%parse-worker-init-config))
            "no SYSTEM => nil config")))
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . "recurya/dev")
                 ("MCP_WORKER_INIT_ENTRY" . "recurya/dev:start-dev-runtime!")
                 ("MCP_WORKER_INIT_MAX_FAILURES" . "1"))
      (lambda ()
        (let ((cfg (cl-mcp/src/pool::%parse-worker-init-config)))
          (ok cfg "config present")
          (ok (string= (getf cfg :system) "recurya/dev") "system parsed")
          (ok (string= (getf cfg :entry) "recurya/dev:start-dev-runtime!")
              "entry parsed")
          (ok (eql (getf cfg :max-failures) 1) "max-failures parsed"))))))
