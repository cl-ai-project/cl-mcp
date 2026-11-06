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
                                                    ;; Each wait a cancellation point.
                                                    (loop (sb-sys:with-interrupts
                                                            (sleep 0.01d0)))))
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

(defun %busy (seconds)
  "Spin for SECONDS without allocating or blocking."
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) end))))

(deftest a-late-interruption-does-not-cancel-the-next-region
  (testing "an interruption aimed at an ended region leaves the thread's next region alone"
    ;; Hunchentoot runs a connection's requests on one thread: the cancel of
    ;; request 1 can be delivered after 1 has ended and 2 has begun.
    (let* ((phase nil)
           (captured nil)
           (go-on nil)
           (outcome nil)
           (thread (bordeaux-threads:make-thread
                    (lambda ()
                      (call-cancellable "inline-cancel-late" 1
                                        (lambda ()
                                          (setf phase :first)
                                          (loop until captured do (sleep 0.01d0))))
                      (setf outcome
                            (handler-case
                                (call-cancellable "inline-cancel-late" 2
                                                  (lambda ()
                                                    (setf phase :second)
                                                    (loop until go-on do (sleep 0.01d0))
                                                    :finished))
                              (inline-request-cancelled () :cancelled))))
                    :name "inline-cancel-test-late")))
      (unwind-protect
           (let ((entry nil))
             ;; Catch region 1's entry as the canceller would have read it.
             (loop repeat 300
                   until (setf entry (gethash (cons "inline-cancel-late" 1)
                                              cl-mcp/src/inline-cancel::*regions*))
                   do (sleep 0.01d0))
             (ok entry "region 1's entry was seen")
             (setf captured t)
             (loop repeat 300 until (eq phase :second) do (sleep 0.01d0))
             (when entry
               (cl-mcp/src/inline-cancel::%interrupt-region (car entry) (cdr entry)))
             ;; Region 2 has no cancellation point, so the interruption is
             ;; taken when it ends; the token check there is what is tested.
             ;; The pause lets the interruption become pending while region 2
             ;; still holds its token.
             (sleep 0.1d0)
             (setf go-on t)
             (loop repeat 300
                   while (bordeaux-threads:thread-alive-p thread)
                   do (sleep 0.01d0))
             (ok (eq :finished outcome)
                 (format nil "region 2 ran to the end: ~S" outcome)))
        (setf captured t
              go-on t)
        (when (bordeaux-threads:thread-alive-p thread)
          (ignore-errors (bordeaux-threads:destroy-thread thread)))))))

(deftest a-cancellation-lands-only-at-a-cancellation-point
  (testing "outside SB-SYS:WITH-INTERRUPTS the region finishes its step first"
    ;; The region may be in foreign code -- opendir, read -- that must not be
    ;; unwound; only a step the region marks with WITH-INTERRUPTS is.
    (flet ((timed-cancel (body)
             (let* ((started nil)
                    (thread (bordeaux-threads:make-thread
                             (lambda ()
                               (handler-case
                                   (call-cancellable "inline-cancel-point" 9
                                                     (lambda ()
                                                       (setf started t)
                                                       (funcall body)))
                                 (inline-request-cancelled () :cancelled)))
                             :name "inline-cancel-test-point"))
                    (start nil))
               (unwind-protect
                    (progn
                      (loop repeat 300 until started do (sleep 0.01d0))
                      (setf start (get-internal-real-time))
                      (cancel-inline-request "inline-cancel-point" 9)
                      (loop repeat 500
                            while (bordeaux-threads:thread-alive-p thread)
                            do (sleep 0.01d0))
                      (/ (- (get-internal-real-time) start)
                         (float internal-time-units-per-second)))
                 (when (bordeaux-threads:thread-alive-p thread)
                   (ignore-errors (bordeaux-threads:destroy-thread thread)))))))
      (let ((deferred (timed-cancel (lambda () (%busy 1))))
            (prompt (timed-cancel (lambda () (sb-sys:with-interrupts (%busy 3))))))
        (ok (>= deferred 0.8) (format nil "deferred to the end of the step: ~,2Fs" deferred))
        (ok (< prompt 0.8) (format nil "taken at the cancellation point: ~,2Fs" prompt))))))

(deftest a-repeated-cancellation-interrupts-the-region-once
  (testing "cancels of one request that pile up while interrupts are off do not nest"
    ;; Each interruption pending at the end of the region runs nested in the
    ;; one before it; past SBCL's nesting limit (8) the process dies -- 16
    ;; cancels did.  Six stay below the limit, so a regression fails the
    ;; count below instead of taking the test run down with it.
    (let* ((started nil)
           (go-on nil)
           (outcome nil)
           (thread (bordeaux-threads:make-thread
                    (lambda ()
                      (setf outcome
                            (handler-case
                                (call-cancellable "inline-cancel-repeat" 5
                                                  (lambda ()
                                                    (setf started t)
                                                    ;; No cancellation point:
                                                    ;; every cancel is pending
                                                    ;; when the region ends.
                                                    (loop until go-on do (sleep 0.01d0))
                                                    :finished))
                              (inline-request-cancelled () :cancelled))))
                    :name "inline-cancel-test-repeat")))
      (unwind-protect
           (let ((answers nil))
             (loop repeat 300 until started do (sleep 0.01d0))
             (dotimes (i 6)
               (push (cancel-inline-request "inline-cancel-repeat" 5) answers))
             (setf go-on t)
             (loop repeat 500
                   while (bordeaux-threads:thread-alive-p thread)
                   do (sleep 0.01d0))
             (ok (eq :cancelled outcome)
                 (format nil "the region ended with the cancellation: ~S" outcome))
             (ok (= 1 (count t answers))
                 (format nil "only the first cancel interrupted: ~S" (reverse answers)))
             (ok (null (gethash (cons "inline-cancel-repeat" 5)
                                cl-mcp/src/inline-cancel::*regions*))
                 "the region's entry is gone"))
        (setf go-on t)
        (when (bordeaux-threads:thread-alive-p thread)
          (ignore-errors (bordeaux-threads:destroy-thread thread)))))))
