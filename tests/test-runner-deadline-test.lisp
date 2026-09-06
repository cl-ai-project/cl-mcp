;;;; tests/test-runner-deadline-test.lisp
;;;;
;;;; Covers the deadline machinery that keeps a slow or wedged test run from
;;;; pinning its caller: numeric-string timeout coercion, and the three
;;;; outcomes of CALL-WITH-TEST-RUN-DEADLINE.

(defpackage #:cl-mcp/tests/test-runner-deadline-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:bordeaux-threads
                #:current-thread)
  (:import-from #:cl-mcp/src/test-runner-core
                #:call-with-test-run-deadline
                #:coerce-timeout-seconds
                #:make-timeout-result))

(in-package #:cl-mcp/tests/test-runner-deadline-test)

(deftest coerce-timeout-seconds-accepts-numbers-and-strings
  (testing "a number passes through"
    (ok (= 60 (coerce-timeout-seconds 60)))
    (ok (= 60 (coerce-timeout-seconds 60.0))))
  (testing "a numeric string is accepted rather than silently dropped"
    ;; A client that sends timeout_seconds as "60" used to fail the
    ;; (NUMBERP ...) guards in the worker handler and in PROXY-TO-WORKER,
    ;; and so silently fell back to the 300 s default.
    (ok (= 60 (coerce-timeout-seconds "60")))
    (ok (= 60 (coerce-timeout-seconds " 60 "))))
  (testing "unusable values yield NIL so callers keep their own default"
    (ok (null (coerce-timeout-seconds nil)))
    (ok (null (coerce-timeout-seconds "abc")))
    (ok (null (coerce-timeout-seconds "60s")))
    (ok (null (coerce-timeout-seconds "")))
    (ok (null (coerce-timeout-seconds 0)))
    (ok (null (coerce-timeout-seconds -5)))
    (ok (null (coerce-timeout-seconds :sixty)))))

(deftest deadline-returns-the-runs-value
  (testing "a run that finishes in time yields :OK and its value"
    (multiple-value-bind (result status)
        (call-with-test-run-deadline (lambda () :done) 5)
      (ok (eq :ok status))
      (ok (eq :done result)))))

(deftest deadline-runs-off-the-calling-thread
  (testing "the run executes on its own thread"
    (let ((caller (current-thread)))
      (multiple-value-bind (result status)
          (call-with-test-run-deadline
           (lambda () (not (eq caller (current-thread))))
           5)
        (ok (eq :ok status))
        (ok result "the thunk did not run on the caller's thread")))))

(deftest deadline-reports-a-slow-run-as-timeout
  (testing "a run slower than the deadline yields :TIMEOUT, bounded in time"
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (result status)
          (call-with-test-run-deadline (lambda () (sleep 30) :done) 1)
        (let ((elapsed (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))
          (ok (eq :timeout status))
          (ok (eql 1 result) "the timeout result carries the deadline")
          (ok (< elapsed 10)
              (format nil "answered in ~,2Fs, not after the 30s sleep"
                      elapsed)))))))

(deftest deadline-answers-even-when-the-run-cannot-be-interrupted
  (testing "a run that swallows the interrupt still cannot hold the caller"
    ;; Stands in for a suite blocked in something SB-EXT:WITH-TIMEOUT cannot
    ;; unwind -- a server accept loop left behind by the tests, say.  Run
    ;; inline, that wedges the worker's single connection thread and every
    ;; later tool call for the session with it; here the caller is answered
    ;; at the deadline regardless, and the blocked thread is torn down.
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (result status)
          (call-with-test-run-deadline
           (lambda ()
             (let ((stop (+ (get-internal-real-time)
                            (* 60 internal-time-units-per-second))))
               (loop while (< (get-internal-real-time) stop)
                     do (handler-case (sleep 0.05)
                          ;; Swallow the deadline unwind and keep going:
                          ;; exactly the uninterruptible case.
                          (serious-condition () nil)))
               :finished))
           1)
        (let ((elapsed (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))
          (ok (eq :timeout status))
          (ok (eql 1 result))
          (ok (< elapsed 10)
              (format nil "answered in ~,2Fs despite the run still blocking"
                      elapsed)))))))

(deftest deadline-reports-a-signalling-run-as-error
  (testing "a run that signals yields :ERROR and the condition"
    (multiple-value-bind (result status)
        (call-with-test-run-deadline (lambda () (error "boom")) 5)
      (ok (eq :error status))
      (ok (typep result 'condition))
      (ok (search "boom" (princ-to-string result))))))

(deftest make-timeout-result-is-machine-readable
  (testing "the timeout payload names itself and carries one failed entry"
    (let ((ht (make-timeout-result 42)))
      (ok (eql 0 (gethash "passed" ht)))
      (ok (eql 1 (gethash "failed" ht)))
      (ok (equal "timeout" (gethash "framework" ht)))
      (ok (eql 42000 (gethash "duration_ms" ht)))
      (ok (eql 1 (length (gethash "failed_tests" ht))))
      (ok (equal "TIMEOUT"
                 (gethash "test_name"
                          (aref (gethash "failed_tests" ht) 0)))))))
