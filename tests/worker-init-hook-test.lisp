;;;; tests/worker-init-hook-test.lisp
;;;;
;;;; Tests for the worker-side init hook: load lock, init state machine,
;;;; entry resolution, and the init RPC handlers.

(defpackage #:cl-mcp/tests/worker-init-hook-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/worker/init-hook
                #:*asdf-load-lock*
                #:with-asdf-load-lock)
  (:import-from #:cl-mcp/src/worker/handlers))

(in-package #:cl-mcp/tests/worker-init-hook-test)

(deftest with-asdf-load-lock-serializes
  (testing "two threads holding the lock never overlap in the critical section"
    (let ((inside 0) (max-inside 0) (lock (bt:make-lock "probe")))
      (flet ((body ()
               (with-asdf-load-lock
                 (bt:with-lock-held (lock)
                   (incf inside)
                   (setf max-inside (max max-inside inside)))
                 (sleep 0.02)
                 (bt:with-lock-held (lock) (decf inside)))))
        (let ((threads (loop repeat 5
                             collect (bt:make-thread #'body :name "probe"))))
          (dolist (th threads) (bt:join-thread th))))
      (ok (= max-inside 1)
          "at most one thread was inside the load-lock critical section"))))

(deftest load-system-handler-waits-on-lock
  (testing "%handle-load-system blocks while *asdf-load-lock* is held, then completes"
    (let ((started nil) (finished nil))
      (bt:acquire-lock cl-mcp/src/worker/init-hook:*asdf-load-lock*)
      (let ((th (bt:make-thread
                 (lambda ()
                   (setf started t)
                   (let ((p (make-hash-table :test 'equal)))
                     (setf (gethash "system" p) "alexandria"
                           (gethash "force" p) nil)
                     (cl-mcp/src/worker/handlers::%handle-load-system p))
                   (setf finished t))
                 :name "handler-under-lock")))
        (sleep 0.2)
        (ok started "handler thread started")
        (ok (not finished) "handler is blocked while the load lock is held")
        (bt:release-lock cl-mcp/src/worker/init-hook:*asdf-load-lock*)
        (bt:join-thread th)
        (ok finished "handler completed after the lock was released")))))

(deftest init-state-transitions
  (testing "state starts idle, moves to loading/running/failed, snapshots as a hash-table"
    (cl-mcp/src/worker/init-hook::%reset-init-state)
    (let ((s0 (cl-mcp/src/worker/init-hook::init-state-snapshot)))
      (ok (string= (gethash "init_state" s0) "idle") "starts idle"))
    (cl-mcp/src/worker/init-hook::%set-init-state :loading)
    (ok (string= (gethash "init_state"
                          (cl-mcp/src/worker/init-hook::init-state-snapshot))
                 "loading")
        "loading")
    (cl-mcp/src/worker/init-hook::%set-init-state :running :app-port 13000)
    (let ((s (cl-mcp/src/worker/init-hook::init-state-snapshot)))
      (ok (string= (gethash "init_state" s) "running") "running")
      (ok (eql (gethash "app_port" s) 13000) "app_port recorded"))
    (cl-mcp/src/worker/init-hook::%set-init-state :failed :error "boom")
    (let ((s (cl-mcp/src/worker/init-hook::init-state-snapshot)))
      (ok (string= (gethash "init_state" s) "failed") "failed")
      (ok (string= (gethash "last_init_error" s) "boom") "error recorded"))))

(defun a-test-entry-thunk () 4242)

(deftest resolve-entry
  (testing "PKG:SYM and PKG::SYM resolve to the fdefinition; bad specs error"
    (let ((fn (cl-mcp/src/worker/init-hook::%resolve-entry
               "CL-MCP/TESTS/WORKER-INIT-HOOK-TEST:A-TEST-ENTRY-THUNK")))
      (ok (functionp fn) "resolves to a function")
      (ok (eql (funcall fn) 4242) "funcalls the resolved thunk"))
    (ok (functionp
         (cl-mcp/src/worker/init-hook::%resolve-entry
          "CL-MCP/TESTS/WORKER-INIT-HOOK-TEST::A-TEST-ENTRY-THUNK"))
        "double-colon form resolves")
    (ok (handler-case
            (progn (cl-mcp/src/worker/init-hook::%resolve-entry "no-colon") nil)
          (error () t))
        "spec without a colon errors")
    (ok (handler-case
            (progn (cl-mcp/src/worker/init-hook::%resolve-entry "NOSUCHPKG:FOO") nil)
          (error () t))
        "missing package errors")
    (ok (handler-case
            (progn (cl-mcp/src/worker/init-hook::%resolve-entry "CL:NO-SUCH-SYMBOL-XYZ") nil)
          (error () t))
        "missing symbol errors")))
