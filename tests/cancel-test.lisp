;;;; tests/cancel-test.lisp
;;;;
;;;; Tests for notifications/cancelled handling.

(defpackage #:cl-mcp/tests/cancel-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:*active-requests*
                #:*active-requests-lock*
                #:cancel-request)
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
