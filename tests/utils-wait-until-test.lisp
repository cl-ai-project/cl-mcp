;;;; tests/utils-wait-until-test.lisp
;;;;
;;;; WAIT-UNTIL, the clock-bounded poll in src/utils/deadline.lisp.

(defpackage #:cl-mcp/tests/utils-wait-until-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/deadline
                #:wait-until))

(in-package #:cl-mcp/tests/utils-wait-until-test)

(defun seconds-since (start)
  "Return the seconds elapsed since START, an internal real time."
  (/ (- (get-internal-real-time) start) (float internal-time-units-per-second)))

(deftest wait-until-returns-the-first-true-value-at-once
  (testing "a predicate already true is answered without sleeping"
    (let ((start (get-internal-real-time)))
      (ok (eq :ready (wait-until (lambda () :ready) :timeout 5)))
      (ok (< (seconds-since start) 0.5)))))

(deftest wait-until-returns-as-soon-as-the-state-is-reached
  (testing "it returns when the predicate turns true, not at the timeout"
    (let* ((flag nil)
           (thread (bt:make-thread (lambda () (sleep 0.2) (setf flag :done))))
           (start (get-internal-real-time)))
      (ok (eq :done (wait-until (lambda () flag) :timeout 10 :interval 0.01)))
      (ok (< (seconds-since start) 5) "far sooner than the 10 s timeout")
      (bt:join-thread thread))))

(deftest wait-until-gives-up-at-the-deadline
  (testing "a predicate that never holds answers NIL once the time is up"
    (let ((start (get-internal-real-time))
          (calls 0))
      (ok (null (wait-until (lambda () (incf calls) nil) :timeout 0.3 :interval 0.05)))
      (ok (>= (seconds-since start) 0.3) "it waited the whole timeout")
      (ok (< (seconds-since start) 2) "and not much longer")
      (ok (> calls 2) "polling as it went")))
  (testing "the deadline is the clock's, not a count of sleeps"
    ;; An interval longer than the timeout must not stretch the wait to it.
    (let ((start (get-internal-real-time)))
      (ok (null (wait-until (lambda () nil) :timeout 0.2 :interval 10)))
      (ok (< (seconds-since start) 2)))))
