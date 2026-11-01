;;;; tests/inline-cancel-test.lisp

(defpackage #:cl-mcp/tests/inline-cancel-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/inline-cancel
                #:call-cancellable
                #:cancel-inline-request
                #:inline-request-cancelled))

(in-package #:cl-mcp/tests/inline-cancel-test)

(deftest call-cancellable-returns-what-its-region-returns
  (testing "a region that is not cancelled runs to the end"
    (ok (equal '(1 2) (multiple-value-list
                       (call-cancellable "inline-cancel-test" 1 (lambda () (values 1 2))))))
    (ok (eq :plain (call-cancellable nil nil (lambda () :plain)))
        "without ids the thunk just runs")))

(deftest cancel-inline-request-finds-only-a-running-region
  (testing "nothing is cancelled when no region of that request runs"
    (ok (null (cancel-inline-request "inline-cancel-test" 404)) "never registered")
    (call-cancellable "inline-cancel-test" 2 (lambda () nil))
    (ok (null (cancel-inline-request "inline-cancel-test" 2)) "already ended"))
  (testing "a running region is interrupted, and only that session's"
    (let* ((outcome nil)
           (started nil)
           (thread (bordeaux-threads:make-thread
                    (lambda ()
                      (setf outcome
                            (handler-case
                                (call-cancellable "inline-cancel-test" 3
                                                  (lambda ()
                                                    (setf started t)
                                                    (loop (sleep 0.01d0))))
                              (inline-request-cancelled () :cancelled))))
                    :name "inline-cancel-test-region")))
      (unwind-protect
           (progn
             (loop repeat 300 until started do (sleep 0.01d0))
             (ok (null (cancel-inline-request "another-session" 3))
                 "another session's request with the same id is untouched")
             (ok (cancel-inline-request "inline-cancel-test" 3) "the region is found")
             (loop repeat 300
                   while (bordeaux-threads:thread-alive-p thread)
                   do (sleep 0.01d0))
             (ok (eq :cancelled outcome)
                 (format nil "the region ended with the cancellation: ~S" outcome)))
        (when (bordeaux-threads:thread-alive-p thread)
          (ignore-errors (bordeaux-threads:destroy-thread thread)))))))
