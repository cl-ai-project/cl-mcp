;;;; tests/pool-status-test.lisp
;;;;
;;;; Tests for pool-status MCP tool.

(defpackage #:cl-mcp/tests/pool-status-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok
                #:skip)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-tool-handler)
  (:import-from #:cl-mcp/src/state
                #:make-state)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool))

(in-package #:cl-mcp/tests/pool-status-test)

(deftest pool-status-tool-registered
  (testing "pool-status tool is registered in the tool registry"
    (ok (functionp (get-tool-handler "pool-status")))))

(deftest pool-status-returns-structure-when-pool-disabled
  (testing "pool-status returns correct structure when pool is disabled"
    (let* ((*use-worker-pool* nil)
           (handler (get-tool-handler "pool-status"))
           (state (make-state))
           (response (funcall handler state 1 nil)))
      (let ((result (gethash "result" response)))
        (ok (hash-table-p result))
        (ok (equal (gethash "pool_running" result) nil))
        (ok (equal (gethash "total_workers" result) 0))
        (ok (equal (gethash "standby_count" result) 0))
        (ok (equal (gethash "bound_count" result) 0))
        (ok (arrayp (gethash "workers" result)))
        (ok (zerop (length (gethash "workers" result))))))))

(deftest pool-status-returns-structure-when-pool-running
  (testing "pool-status returns correct structure with running pool"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (with-pool ()
      (let* ((handler (get-tool-handler "pool-status"))
             (state (make-state))
             (response (funcall handler state 1 nil)))
        (let ((result (gethash "result" response)))
          (ok (hash-table-p result))
          (ok (equal (gethash "pool_running" result) t))
          (ok (numberp (gethash "total_workers" result)))
          (ok (numberp (gethash "standby_count" result)))
          (ok (numberp (gethash "bound_count" result)))
          (ok (numberp (gethash "max_pool_size" result)))
          (ok (numberp (gethash "warmup_target" result)))
          (ok (arrayp (gethash "workers" result)))
          (ok (gethash "content" result)))))))

(defun %pool-status-text (result)
  "Extract the summary text from a pool-status tool RESULT hash-table."
  (gethash "text" (aref (gethash "content" result) 0)))

(deftest pool-status-text-omits-init-hook-line-when-inactive
  (testing "pool-status text has no Init hook line when the hook is not engaged"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let* ((handler (get-tool-handler "pool-status"))
               (state (make-state))
               (response (funcall handler state 1 nil))
               (result (gethash "result" response))
               (text (%pool-status-text result)))
          (ok (not (search "Init hook:" text))
              "no Init hook line when owner is nil, not disabled, zero failures"))))))

(deftest pool-status-text-includes-init-hook-line-when-owner-set
  (testing "pool-status text shows the Init hook line with owner/worker/failures when an owner is elected"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
          (setf cl-mcp/src/pool::*runtime-owner*
                  (cons "sess-xyz" (cl-mcp/src/worker-client:make-worker :id 42))
                cl-mcp/src/pool::*runtime-init-failures* 2))
        (let* ((handler (get-tool-handler "pool-status"))
               (state (make-state))
               (response (funcall handler state 1 nil))
               (result (gethash "result" response))
               (text (%pool-status-text result)))
          (ok (search "Init hook: owner=sess-xyz (worker #42) disabled=false failures=2" text)
              "Init hook line renders owner, worker id, disabled, and failures"))))))

(deftest pool-status-text-includes-init-hook-line-when-disabled-only
  (testing "pool-status text shows the Init hook line when disabled with no owner"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
          (setf cl-mcp/src/pool::*runtime-init-disabled* t))
        (let* ((handler (get-tool-handler "pool-status"))
               (state (make-state))
               (response (funcall handler state 1 nil))
               (result (gethash "result" response))
               (text (%pool-status-text result)))
          (ok (search "Init hook: owner=none disabled=true failures=0" text)
              "Init hook line shows owner=none and no worker suffix when owner is nil")
          (ok (not (search "(worker #" text))
              "no worker suffix when init_owner_worker is nil"))))))
