;;;; tests/worker-leaked-thread-test.lisp
;;;;
;;;; A deadline that cannot stop its thread leaves that thread running in the
;;;; worker: holding locks it took, mutating state later work reads, competing
;;;; for the CPU.  The caller that hit the deadline is told; every request
;;;; after it would be served by a process that is quietly wrong.  These cover
;;;; the record of such threads, its pruning, and the worker retiring on it.

(defpackage #:cl-mcp/tests/worker-leaked-thread-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread
                #:leaked-threads
                #:forget-leaked-threads)
  ;; Named without importing: the worker server's side of this is internal, so
  ;; the tests reach it with ::, but the dependency still has to be declared
  ;; for the package-inferred system to load it.
  (:import-from #:cl-mcp/src/worker/server))

(in-package #:cl-mcp/tests/worker-leaked-thread-test)

(defmacro with-clean-leak-record (&body body)
  "Run BODY with the leak record empty, and leave it empty.
Deliberately leaking a thread is the only way to test any of this, and the
record is image-wide."
  `(unwind-protect (progn (forget-leaked-threads) ,@body)
     (forget-leaked-threads)))

(defun leak-one-thread ()
  "Run a deadline against a thread that cannot be stopped.
Returns two values: whether the deadline said so, and the thread itself, taken
from the record rather than from the deadline -- which reports only that a
thread was left behind, not which one.

SB-SYS:WITHOUT-INTERRUPTS defers both the cooperative unwind and
DESTROY-THREAD, which is what being unstoppable actually looks like.  The
thread ends on its own shortly after, so the image is not left carrying it."
  (let ((reported (nth-value 2 (call-with-deadline-thread
                                (lambda ()
                                  (sb-sys:without-interrupts (sleep 3))
                                  :finished)
                                0.3))))
    (values reported (first (leaked-threads)))))

(deftest leaked-threads-records-what-a-deadline-could-not-stop
  (testing "a run the deadline gave up on is recorded, not merely reported"
    ;; The caller that hit the deadline is answered and moves on; the thread
    ;; outlives every later request, so the image has to be able to say it is
    ;; still carrying it.
    (with-clean-leak-record
      (ok (leak-one-thread) "the deadline reports the leak to its caller")
      (ok (= 1 (length (leaked-threads)))
          "and the image records it")))
  (testing "a run stopped at its deadline is not recorded"
    (with-clean-leak-record
      (call-with-deadline-thread (lambda () (sleep 30) :done) 0.3)
      (ok (null (leaked-threads))
          "an interruptible run leaves nothing behind")))
  (testing "a run that finished is not recorded"
    (with-clean-leak-record
      (call-with-deadline-thread (lambda () :done) 5)
      (ok (null (leaked-threads))))))

(deftest leaked-threads-stops-reporting-a-thread-that-finished
  (testing "the record prunes, so a worker that recovered is not retired"
    ;; This is the whole reason the check is made at the moment of use rather
    ;; than remembered from when it happened.  The thread below cannot be
    ;; stopped, but it ends on its own -- and a worker that ends up carrying
    ;; nothing should keep the session state it still holds rather than be
    ;; replaced for a condition that has passed.
    (with-clean-leak-record
      (let ((thread (nth-value 1 (leak-one-thread))))
        (ok (= 1 (length (leaked-threads))) "recorded while it runs")
        (loop repeat 100
              while (bt:thread-alive-p thread)
              do (sleep 0.1))
        (ok (not (bt:thread-alive-p thread)) "the thread finished on its own")
        (ok (null (leaked-threads))
            "and stops being counted against the image")))))

(deftest worker-retires-rather-than-serve-a-request-while-carrying-one
  (testing "a request arriving while a thread is still running retires the worker"
    ;; Retiring means exiting: the parent's crash handling then replaces the
    ;; worker and tells the caller its state was reset, whereas answering with
    ;; an error would leave this image in the pool to fail the same way on
    ;; every later request.  The action is indirected so the decision can be
    ;; asserted without taking this process down with it.
    (with-clean-leak-record
      (leak-one-thread)
      (let* ((retired nil)
             (cl-mcp/src/worker/server::*retire-action*
               (lambda (leaked) (setf retired (length leaked)))))
        (cl-mcp/src/worker/server::%retire-if-carrying-leaked-threads
         "worker/eval")
        (ok (eql 1 retired)
            "the worker retires, and is told what for"))))
  (testing "a worker carrying nothing serves the request"
    (with-clean-leak-record
      (let* ((retired nil)
             (cl-mcp/src/worker/server::*retire-action*
               (lambda (leaked) (declare (ignore leaked)) (setf retired t))))
        (cl-mcp/src/worker/server::%retire-if-carrying-leaked-threads
         "worker/eval")
        (ok (null retired) "no retirement when there is nothing to retire for")))))

(deftest leaked-threads-are-reported-to-the-parent
  (testing "a response carries the count so pool-status can show it"
    ;; The worker retires before serving another request, so this is what
    ;; makes the condition visible in the window between the deadline giving
    ;; up and that next request -- which is when someone is likely to be
    ;; asking what went wrong.
    (with-clean-leak-record
      (leak-one-thread)
      (let ((ht (cl-mcp/src/worker/server::%make-result 1 "payload")))
        (ok (eql 1 (gethash "leaked_threads" ht))))))
  (testing "and ordinary responses are unchanged on the wire"
    (with-clean-leak-record
      (let ((ht (cl-mcp/src/worker/server::%make-result 1 "payload")))
        (ok (null (nth-value 1 (gethash "leaked_threads" ht)))
            "the key is absent rather than reported as zero")))))
