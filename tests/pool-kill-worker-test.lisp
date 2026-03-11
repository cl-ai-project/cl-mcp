;;;; tests/pool-kill-worker-test.lisp
;;;;
;;;; Tests for pool-kill-worker MCP tool.

(defpackage #:cl-mcp/tests/pool-kill-worker-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok
                #:skip)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-tool-handler)
  (:import-from #:cl-mcp/src/state
                #:make-state
                #:*current-session-id*)
  ;; Import to trigger tool registration at load time
  (:import-from #:cl-mcp/src/tools/pool-kill-worker
                #:pool-kill-worker)
  (:import-from #:cl-mcp/src/pool
                #:get-or-assign-worker
                #:find-session-worker)
  (:import-from #:cl-mcp/src/worker-client
                #:worker-pid)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool))

(in-package #:cl-mcp/tests/pool-kill-worker-test)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun %call-kill-worker (&key (session-id "test-session") reset)
  "Invoke the pool-kill-worker handler with the given parameters."
  (let* ((handler (get-tool-handler "pool-kill-worker"))
         (state (make-state))
         (*current-session-id* session-id)
         (params (make-hash-table :test 'equal)))
    (when reset
      (setf (gethash "reset" params) t))
    (funcall handler state 1 params)))

(defun %result-of (response)
  "Extract the result hash-table from a JSON-RPC response."
  (gethash "result" response))

;;; ---------------------------------------------------------------------------
;;; Registration test
;;; ---------------------------------------------------------------------------

(deftest pool-kill-worker-tool-registered
  (testing "pool-kill-worker tool is registered in the tool registry"
    (ok (functionp (get-tool-handler "pool-kill-worker")))))

;;; ---------------------------------------------------------------------------
;;; Pool-disabled tests
;;; ---------------------------------------------------------------------------

(deftest pool-kill-worker-when-pool-disabled
  (testing "returns informative message when worker pool is disabled"
    (let* ((*use-worker-pool* nil)
           (response (%call-kill-worker)))
      (let ((result (%result-of response)))
        (ok (hash-table-p result))
        (ok (null (gethash "killed" result))
            "killed should be nil")
        (ok (search "disabled" (gethash "text"
                                        (aref (gethash "content" result) 0)))
            "message should mention disabled")))))

;;; ---------------------------------------------------------------------------
;;; No-session test
;;; ---------------------------------------------------------------------------

(deftest pool-kill-worker-with-no-session
  (testing "returns error when no session ID is available"
    (let* ((*use-worker-pool* t)
           (handler (get-tool-handler "pool-kill-worker"))
           (state (make-state))
           (*current-session-id* nil)
           (params (make-hash-table :test 'equal))
           (response (funcall handler state 1 params)))
      (let ((result (%result-of response)))
        (ok (hash-table-p result))
        (ok (null (gethash "killed" result))
            "killed should be nil")))))

;;; ---------------------------------------------------------------------------
;;; No-worker test (pool running but no worker bound)
;;; ---------------------------------------------------------------------------

(deftest pool-kill-worker-with-no-bound-worker
  (testing "returns no-worker when session has no worker"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "orphan-session"))
      (with-pool ()
        (let* ((response (%call-kill-worker :session-id "orphan-session"))
               (result (%result-of response)))
          (ok (hash-table-p result))
          (ok (null (gethash "killed" result))
              "killed should be nil when no worker bound"))))))

;;; ---------------------------------------------------------------------------
;;; Kill success tests (require spawning)
;;; ---------------------------------------------------------------------------

(deftest pool-kill-worker-kills-bound-worker
  (testing "kills the worker bound to the session (reset=false)"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "kill-test-session"))
      (with-pool ()
        ;; Assign a worker
        (let* ((worker (get-or-assign-worker "kill-test-session"))
               (pid (worker-pid worker)))
          (ok worker "worker should be assigned")
          (ok (integerp pid) "worker should have a PID")
          ;; Kill it
          (let* ((response (%call-kill-worker :session-id "kill-test-session"))
                 (result (%result-of response)))
            (ok (equal t (gethash "killed" result))
                "killed should be t")
            (ok (null (gethash "reset" result))
                "reset should be nil")
            ;; Worker should no longer be bound
            (ok (null (find-session-worker "kill-test-session"))
                "session should have no worker after kill")))))))

(deftest pool-kill-worker-with-reset
  (testing "kills and immediately respawns a worker (reset=true)"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "reset-test-session"))
      (with-pool ()
        ;; Assign a worker
        (let* ((worker (get-or-assign-worker "reset-test-session"))
               (old-pid (worker-pid worker)))
          (ok worker "worker should be assigned")
          ;; Kill with reset
          (let* ((response (%call-kill-worker :session-id "reset-test-session"
                                             :reset t))
                 (result (%result-of response)))
            (ok (equal t (gethash "killed" result))
                "killed should be t")
            (ok (equal t (gethash "reset" result))
                "reset should be t")
            ;; A new worker should be bound
            (let ((new-worker (find-session-worker "reset-test-session")))
              (ok new-worker "new worker should be bound after reset")
              (ok (not (eql old-pid (worker-pid new-worker)))
                  "new worker should have a different PID"))))))))

(deftest pool-kill-worker-double-kill
  (testing "second kill returns no-worker"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "double-kill-session"))
      (with-pool ()
        ;; Assign and kill once
        (get-or-assign-worker "double-kill-session")
        (let* ((r1 (%result-of (%call-kill-worker :session-id "double-kill-session"))))
          (ok (equal t (gethash "killed" r1))
              "first kill should succeed"))
        ;; Second kill
        (let* ((r2 (%result-of (%call-kill-worker :session-id "double-kill-session"))))
          (ok (null (gethash "killed" r2))
              "second kill should report no worker"))))))

;;; ---------------------------------------------------------------------------
;;; Empty session ID test
;;; ---------------------------------------------------------------------------

(deftest pool-kill-worker-with-empty-session-id
  (testing "returns error when session ID is empty string"
    (let* ((*use-worker-pool* t)
           (response (%call-kill-worker :session-id "")))
      (let ((result (%result-of response)))
        (ok (hash-table-p result))
        (ok (null (gethash "killed" result))
            "killed should be nil")
        (ok (search "identify"
                    (gethash "text"
                             (aref (gethash "content" result) 0)))
            "message should mention session identification")))))

;;; ---------------------------------------------------------------------------
;;; Reset spawn failure test
;;; ---------------------------------------------------------------------------

(deftest pool-kill-worker-reset-spawn-failure
  (testing "reports error when replacement spawn fails after kill (reset=true)"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "spawn-fail-session"))
      (with-pool ()
        ;; Assign a real worker so kill-session-worker returns :killed
        (get-or-assign-worker "spawn-fail-session")
        ;; Mock get-or-assign-worker to simulate spawn failure.
        ;; kill-session-worker does not call get-or-assign-worker internally,
        ;; so the real kill proceeds; only the reset spawn is affected.
        (let ((original (symbol-function 'get-or-assign-worker)))
          (unwind-protect
              (progn
                (setf (symbol-function 'get-or-assign-worker)
                      (lambda (session-id)
                        (declare (ignore session-id))
                        (error "Simulated spawn failure")))
                (let* ((response (%call-kill-worker
                                  :session-id "spawn-fail-session"
                                  :reset t))
                       (result (%result-of response)))
                  (ok (equal t (gethash "killed" result))
                      "killed should be t (worker was killed)")
                  (ok (null (gethash "reset" result))
                      "reset should be nil (spawn failed)")
                  (ok (equal t (gethash "isError" result))
                      "isError should be t")
                  (ok (search "spawn failed"
                              (gethash "text"
                                       (aref (gethash "content" result) 0)))
                      "message should mention spawn failure")))
            (setf (symbol-function 'get-or-assign-worker)
                  original)))))))
