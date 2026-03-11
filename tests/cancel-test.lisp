;;;; tests/cancel-test.lisp
;;;;
;;;; Tests for notifications/cancelled handling.

(defpackage #:cl-mcp/tests/cancel-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok
                #:skip)
  (:import-from #:cl-mcp/src/proxy
                #:*active-requests*
                #:*active-requests-lock*
                #:cancel-request)
  (:import-from #:cl-mcp/src/protocol
                #:handle-notification)
  (:import-from #:cl-mcp/src/state
                #:make-state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/pool
                #:get-or-assign-worker)
  (:import-from #:cl-mcp/src/worker-client
                #:worker-state #:worker-pid)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool)
  (:import-from #:bordeaux-threads
                #:with-lock-held))

(in-package #:cl-mcp/tests/cancel-test)

(deftest active-requests-initially-empty
  (testing "active-requests map starts empty"
    (with-lock-held (*active-requests-lock*)
      (ok (zerop (hash-table-count *active-requests*))))))

(deftest cancel-request-unknown-id-returns-nil
  (testing "cancel-request returns NIL for unknown request ID"
    (ok (null (cancel-request "nonexistent-id-999")))))

(deftest cancel-request-known-id-returns-t
  (testing "cancel-request returns T and removes entry for known request ID"
    (with-lock-held (*active-requests-lock*)
      (setf (gethash "test-req-42" *active-requests*) "fake-session-id"))
    (unwind-protect
        (let ((result (cancel-request "test-req-42")))
          (ok (eq result t))
          (with-lock-held (*active-requests-lock*)
            (ok (null (gethash "test-req-42" *active-requests*)))))
      (with-lock-held (*active-requests-lock*)
        (remhash "test-req-42" *active-requests*)))))

;;; --- Protocol-level notification tests ---

(deftest handle-cancelled-notification-dispatches
  (testing "notifications/cancelled calls cancel-request with requestId"
    (with-lock-held (*active-requests-lock*)
      (setf (gethash "proto-req-7" *active-requests*) "fake-session"))
    (unwind-protect
        (let ((params (let ((ht (make-hash-table :test 'equal)))
                         (setf (gethash "requestId" ht) "proto-req-7")
                         ht))
               (state (make-state)))
          (handle-notification state "notifications/cancelled" params)
          (with-lock-held (*active-requests-lock*)
            (ok (null (gethash "proto-req-7" *active-requests*)))))
      (with-lock-held (*active-requests-lock*)
        (remhash "proto-req-7" *active-requests*)))))

(deftest handle-cancelled-notification-unknown-id-is-noop
  (testing "notifications/cancelled for unknown requestId is a no-op"
    (let ((params (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "requestId" ht) "unknown-req-999")
                     ht))
           (state (make-state)))
      (ok (null (handle-notification state "notifications/cancelled" params))))))

;;; --- Integration test: full cancel flow ---

(deftest cancel-kills-running-worker-e2e
  (testing "Full cancel flow: register request, cancel, verify worker killed"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (with-pool ()
      (let* ((session-id "cancel-test-session")
             (*current-session-id* session-id)
             (worker (get-or-assign-worker session-id))
             (worker-pid (worker-pid worker)))
        (declare (ignore worker-pid))
        (with-lock-held (*active-requests-lock*)
          (setf (gethash "e2e-req-1" *active-requests*) session-id))
        (cancel-request "e2e-req-1")
        (sleep 0.5)
        (ok (eq (worker-state worker) :dead)
            "Worker state should be :dead after cancel")
        (with-lock-held (*active-requests-lock*)
          (ok (null (gethash "e2e-req-1" *active-requests*))
              "Active request entry should be removed"))))))
