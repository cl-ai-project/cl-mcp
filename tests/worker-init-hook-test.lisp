;;;; tests/worker-init-hook-test.lisp
;;;;
;;;; Tests for the worker-side init hook: load lock, init state machine,
;;;; entry resolution, and the init RPC handlers.

(defpackage #:cl-mcp/tests/worker-init-hook-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/worker/init-hook
                #:*asdf-load-lock*
                #:with-asdf-load-lock))

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
