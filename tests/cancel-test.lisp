;;;; tests/cancel-test.lisp
;;;;
;;;; Tests for notifications/cancelled: the protocol entry, cancel-request's
;;;; answers, and one run against a real worker.  What a cancellation does at
;;;; each phase of a request is pinned in tests/request-lifecycle-test.lisp.

(defpackage #:cl-mcp/tests/cancel-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok
                #:skip)
  (:import-from #:cl-mcp/src/proxy
                #:cancel-request
                #:proxy-to-worker)
  (:import-from #:cl-mcp/src/request-lifecycle
                #:register-request
                #:unregister-request
                #:find-request
                #:request-phase
                #:cancellation-requested-p)
  (:import-from #:cl-mcp/src/protocol
                #:handle-notification)
  (:import-from #:cl-mcp/src/state
                #:make-state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/pool
                #:get-or-assign-worker)
  (:import-from #:cl-mcp/src/worker-client
                #:worker-state)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool)
  ;; Bare: threads through the BT nickname.
  (:import-from #:bordeaux-threads))

(in-package #:cl-mcp/tests/cancel-test)

(deftest cancel-request-unknown-id-returns-nil
  (testing "cancel-request returns NIL for a request the session does not have"
    (ok (null (cancel-request "nonexistent-id-999" "some-session")))
    (ok (null (cancel-request "nonexistent-id-999")) "nor for no session at all")))

(deftest cancel-request-marks-a-request-that-has-not-run
  (let ((record (register-request "cancel-session" "test-req-42")))
    (unwind-protect
         (progn
           (ok (eq :marked (cancel-request "test-req-42" "cancel-session")))
           (ok (cancellation-requested-p record)))
      (unregister-request record))))

(deftest cancel-request-from-another-session-does-nothing
  (let ((record (register-request "owner-session" "test-req-43")))
    (unwind-protect
         (progn
           (ok (null (cancel-request "test-req-43" "other-session")))
           (ok (not (cancellation-requested-p record))))
      (unregister-request record))))

;;; --- Protocol-level notification tests ---

(deftest handle-cancelled-notification-dispatches
  (testing "notifications/cancelled cancels the named request of this session"
    (let ((record (register-request "proto-session" "proto-req-7")))
      (unwind-protect
           (let ((params (make-ht "requestId" "proto-req-7"))
                 (*current-session-id* "proto-session"))
             (handle-notification (make-state) "notifications/cancelled" params)
             (ok (cancellation-requested-p record)))
        (unregister-request record)))))

(deftest handle-cancelled-notification-unknown-id-is-noop
  (testing "notifications/cancelled for unknown requestId is a no-op"
    (let ((*current-session-id* "proto-session"))
      (ok (null (handle-notification (make-state) "notifications/cancelled"
                                     (make-ht "requestId" "unknown-req-999")))))))

;;; --- Integration test: a real worker running the request ---

(deftest cancel-stops-the-worker-running-the-request-e2e
  (testing "a running evaluation is stopped with its worker, and says so"
    (unless (spawn-available-p)
      (skip "Cannot spawn workers"))
    (with-pool ()
      (let* ((session-id "cancel-test-session")
             (worker (let ((*current-session-id* session-id))
                       (get-or-assign-worker session-id)))
             (result nil)
             (thread (bt:make-thread
                      (lambda ()
                        (let ((*current-session-id* session-id))
                          (setf result
                                (proxy-to-worker "e2e-req-1" "worker/eval"
                                                 (make-ht "code" "(sleep 30)"
                                                          "timeout_seconds" 60)))))
                      :name "cancel-e2e-request")))
        ;; Running, not merely registered: the eval has been sent.
        (loop repeat 200
              until (let ((record (find-request session-id "e2e-req-1")))
                      (and record (eq :executing (request-phase record))))
              do (sleep 0.05))
        (ok (eq :stopping (cancel-request "e2e-req-1" session-id)))
        (bt:join-thread thread)
        (ok (eq t (gethash "isError" result)))
        (ok (equal "execution-unknown" (gethash "execution_status" result)))
        (ok (member (worker-state worker) '(:dead :crashed))
            "the worker that ran it was stopped")
        (ok (null (find-request session-id "e2e-req-1"))
            "and the request is no longer registered")
        (testing "the session goes on, on a fresh worker"
          ;; The first request after may be told of the reset instead of
          ;; running -- and then it must say it did not run.  Whether it is
          ;; told once or twice is 4C's; the one after runs.
          (let ((*current-session-id* session-id))
            (let ((answers (loop for id in '("e2e-req-2" "e2e-req-3" "e2e-req-4")
                                 collect (proxy-to-worker id "worker/eval"
                                                          (make-ht "code" "(+ 1 2)")))))
              (ok (some (lambda (answer) (not (eq t (gethash "isError" answer)))) answers)
                  "a request runs")
              (ok (every (lambda (answer)
                           (or (not (eq t (gethash "isError" answer)))
                               (equal "not-executed" (gethash "execution_status" answer))))
                         answers)
                  "and a reset notice in its place says it did not run")
              (ok (not (eq worker (get-or-assign-worker session-id)))))))))))
