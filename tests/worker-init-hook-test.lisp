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
  (:import-from #:cl-mcp/src/worker/handlers
                #:register-all-handlers)
  (:import-from #:cl-mcp/src/worker/server
                #:make-worker-server
                #:server-port
                #:start-accept-loop
                #:stop-server))

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

(deftest asdf-load-lock-times-out-with-actionable-advice
  (testing "a lock nobody releases ends in one error, not an endless block"
    ;; A run-tests deadline that cannot stop its thread can leave this lock
    ;; owned by a thread that never releases it.  SBCL has no dead-owner
    ;; detection, so waiting forever would hang every later load in this
    ;; worker with nothing to say why -- and the proxy would eventually kill
    ;; the worker with a generic crash notice instead.
    (let* ((held (bt:make-semaphore))
           (release (bt:make-semaphore))
           (holder (bt:make-thread
                    (lambda ()
                      (with-asdf-load-lock
                        (bt:signal-semaphore held)
                        (bt:wait-on-semaphore release)))
                    :name "lock-holder")))
      (unwind-protect
           (progn
             (bt:wait-on-semaphore held)
             (let ((cl-mcp/src/worker/init-hook:*asdf-load-lock-timeout* 0.3)
                   (message nil)
                   (start (get-internal-real-time)))
               (handler-case (with-asdf-load-lock :never-reached)
                 (error (e) (setf message (princ-to-string e))))
               (let ((elapsed (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second)))
                 (ok message "the wait ends in an error rather than blocking")
                 (ok (search "pool-kill-worker" (or message ""))
                     "and the message names the recovery")
                 (ok (< elapsed 5)
                     (format nil "gave up in ~,2Fs" elapsed)))))
        (bt:signal-semaphore release)
        (bt:join-thread holder))))
  (testing "the lock still works normally once released"
    (ok (eq :ran (with-asdf-load-lock :ran))
        "a thunk's own return value survives the timeout plumbing")))

(deftest asdf-load-lock-busy-does-not-advise-killing-a-healthy-worker
  (testing "a caller blocked behind a running init is not told to kill it"
    ;; The init hook holds this lock for a whole cold compile with no deadline
    ;; of its own, so a large application system legitimately outlasts the
    ;; timeout.  Telling that caller to run pool-kill-worker would abort a
    ;; load that was about to finish.  The signaller distinguishes the two
    ;; cases, but the advice is appended afterwards by the response builder,
    ;; so suppressing it there is what actually reaches the client.
    (let* ((held (bt:make-semaphore))
           (release (bt:make-semaphore))
           (holder (bt:make-thread
                    (lambda ()
                      (with-asdf-load-lock
                        (bt:signal-semaphore held)
                        (bt:wait-on-semaphore release)))
                    ;; The thread name is what tells a running init apart from
                    ;; a thread that outlived its deadline.
                    :name "mcp-worker-init")))
      (unwind-protect
           (progn
             (bt:wait-on-semaphore held)
             (let ((params (make-hash-table :test 'equal)))
               (setf (gethash "system" params) "alexandria"
                     (gethash "force" params) nil
                     (gethash "timeout_seconds" params) 2)
               (let* ((resp (cl-mcp/src/worker/handlers::%handle-load-system
                             params))
                      (text (gethash "text"
                                     (aref (gethash "content" resp) 0))))
                 (ok (search "init hook is still loading" text)
                     "the message names the real cause")
                 (ok (search "worker/init-status" text)
                     "and says what to wait for")
                 (ok (null (search "pool-kill-worker" text))
                     "and does not tell the caller to destroy a healthy worker"))))
        (bt:signal-semaphore release)
        (bt:join-thread holder)))))

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

(defun socket-available-p ()
  "Return T if we can bind a TCP socket on localhost."
  (handler-case
      (let ((sock (usocket:socket-listen "127.0.0.1" 0
                                         :reuse-address t
                                         :element-type 'character)))
        (unwind-protect t (ignore-errors (usocket:socket-close sock))))
    (error () nil)))

(defparameter *entry-ran* nil)

(defun integration-entry-thunk () (setf *entry-ran* t) 12345)

(defun %rpc (stream id method &optional params)
  "Send one JSON-RPC line and read one response line; return the parsed hash."
  (let ((req (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" req) "2.0"
          (gethash "id" req) id
          (gethash "method" req) method)
    (when params (setf (gethash "params" req) params))
    (yason:encode req stream) (terpri stream) (force-output stream)
    (yason:parse (read-line stream))))

(deftest init-start-then-status-integration
  (testing "worker/init-start acks fast; init runs the entry; status reaches running"
    (if (not (socket-available-p))
        (skip "socket unavailable")
        (let ((server (make-worker-server :port 0)))
          (register-all-handlers server)
          (setf *entry-ran* nil)
          (cl-mcp/src/worker/init-hook::%reset-init-state)
          (unwind-protect
               (let ((port (server-port server)))
                 (bt:make-thread (lambda () (start-accept-loop server))
                                 :name "test-init-accept")
                 (sleep 0.1)
                 (let ((socket (usocket:socket-connect "127.0.0.1" port
                                                       :element-type 'character)))
                   (unwind-protect
                        (let ((stream (usocket:socket-stream socket))
                              (params (make-hash-table :test 'equal)))
                          (setf (gethash "entry" params)
                                "CL-MCP/TESTS/WORKER-INIT-HOOK-TEST:INTEGRATION-ENTRY-THUNK")
                          (let ((ack (%rpc stream 1 "worker/init-start" params)))
                            (ok (gethash "accepted" (gethash "result" ack))
                                "init-start acked with accepted=t"))
                          (let ((final nil))
                            (loop repeat 50
                                  for st = (gethash "result"
                                            (%rpc stream 2 "worker/init-status"))
                                  for state = (gethash "init_state" st)
                                  do (setf final state)
                                  until (member state '("running" "failed")
                                                :test #'string=)
                                  do (sleep 0.05))
                            (ok (string= final "running") "init reached running")
                            (ok *entry-ran* "entry thunk executed")
                            (ok (eql (gethash "app_port"
                                              (gethash "result"
                                                       (%rpc stream 3
                                                             "worker/init-status")))
                                     12345)
                                "app_port recorded")))
                     (ignore-errors (usocket:socket-close socket)))))
            (stop-server server))))))

(deftest init-eval-failure-records-failed
  (testing "an erroring eval form drives init to :failed with the message recorded"
    (cl-mcp/src/worker/init-hook::%reset-init-state)
    (let ((params (make-hash-table :test 'equal)))
      (setf (gethash "eval" params) "(error \"boom\")")
      ;; %run-init runs synchronously here and never signals out; on an
      ;; eval error it records :failed via the outer handler-case.
      (cl-mcp/src/worker/init-hook::%run-init params))
    (let ((s (cl-mcp/src/worker/init-hook::init-state-snapshot)))
      (ok (string= (gethash "init_state" s) "failed") "init state is failed")
      (let ((err (gethash "last_init_error" s)))
        (ok err "last_init_error is non-nil")
        (ok (and (stringp err) (search "boom" err)) "message mentions boom")
        ;; These two discriminate the %maybe-eval fix: if it reverted to
        ;; dumping the raw error-context plist, the string would be long and
        ;; would contain the :RESTARTS key.  The clean short message must not.
        (ok (< (length err) 80)
            "message is the clean short string, not a raw plist dump")
        (ok (null (search "RESTARTS" (string-upcase err)))
            "no raw error-context plist leaked")))))

(deftest run-init-fails-on-bad-system
  (testing "%run-init records :failed (not :running) when the system fails to load"
    (cl-mcp/src/worker/init-hook::%reset-init-state)
    (let ((params (make-hash-table :test 'equal)))
      (setf (gethash "system" params) "no-such-system-xyz-98765")
      (cl-mcp/src/worker/init-hook::%run-init params)
      (let ((s (cl-mcp/src/worker/init-hook::init-state-snapshot)))
        (ok (string= (gethash "init_state" s) "failed")
            "a bad system load yields :failed, not :running")
        (ok (gethash "last_init_error" s)
            "last_init_error is set")))))
