;;;; tests/pool-status-test.lisp
;;;;
;;;; Tests for pool-status MCP tool.

(defpackage #:cl-mcp/tests/pool-status-test
  (:use #:cl #:rove)
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
