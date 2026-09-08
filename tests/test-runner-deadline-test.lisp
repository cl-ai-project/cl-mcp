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
  (testing "a decimal string is accepted"
    (ok (= 1.5 (coerce-timeout-seconds "1.5")))
    (ok (= 90 (coerce-timeout-seconds "+90"))))
  (testing "text that only the reader would accept is rejected"
    ;; These all pass a "digits, sign, dot and exponent" character filter,
    ;; which is how the first implementation guarded READ-FROM-STRING.  The
    ;; reader turns each of them into a SYMBOL and interns it in whichever
    ;; package is current, so a client looping over such values could grow
    ;; that package without bound in a long-lived parent process.
    (dolist (bait '("e12" "1e2e" "--" "+" "..." "1.2.3" "1.5e2"))
      (ok (null (coerce-timeout-seconds bait))
          (format nil "~S is rejected" bait)))
    (ok (null (find-symbol "E12" (find-package "CL-MCP/SRC/TEST-RUNNER-CORE")))
        "and is rejected without being interned"))
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

(deftest deadline-reports-a-thread-it-could-not-stop
  (testing "a genuinely uninterruptible run is answered AND reported as leaked"
    ;; SB-SYS:WITHOUT-INTERRUPTS defers both the cooperative unwind and
    ;; DESTROY-THREAD, which is the case the polling wrapper exists for: a
    ;; suite blocked where neither can reach it.  The test above does NOT
    ;; reach this branch -- its HANDLER-CASE cannot swallow a throw, so that
    ;; thread dies on the first interrupt and the leak path stays uncovered.
    ;;
    ;; A leaked thread is still executing in the worker and may hold
    ;; *ASDF-LOAD-LOCK*, so the caller has to be told the difference between
    ;; "the run was stopped" and "the run is still going".
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (result status leaked)
          (call-with-test-run-deadline
           (lambda ()
             (sb-sys:without-interrupts (sleep 4))
             :finished)
           1)
        (let ((elapsed (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))
          (ok (eq :timeout status))
          (ok (eql 1 result))
          (ok leaked "the surviving run thread is reported to the caller")
          (ok (< elapsed 3.5)
              (format nil "answered in ~,2Fs, well before the run let go"
                      elapsed)))))))

(deftest deadline-does-not-mistake-the-runs-own-timeout-for-its-own
  (testing "an SB-EXT:TIMEOUT raised by the suite is an error, not a deadline breach"
    ;; The deadline used to be enforced with SB-EXT:WITH-TIMEOUT, whose
    ;; condition is exactly what a suite raises when it exercises timeout
    ;; behaviour -- and this repo ships such suites (tests/timeout-test,
    ;; tests/pool-test).  The two were indistinguishable, so a suite that let
    ;; one escape a test was reported as "timed out after 300 seconds",
    ;; failed:1, duration 300000 ms, with advice to kill the worker.  The
    ;; deadline is now a throw to a tag private to the call, which no
    ;; condition can impersonate.
    (multiple-value-bind (result status)
        (call-with-test-run-deadline
         (lambda () (sb-ext:with-timeout 0.05 (sleep 5)))
         30)
      (ok (eq :error status) "the suite's own timeout surfaces as an error")
      (ok (typep result 'sb-ext:timeout)))))

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

(deftest make-timeout-result-distinguishes-a-leaked-thread
  (testing "a run that was stopped and one still running get different advice"
    ;; The two cases need different actions from the user: a stopped run left
    ;; the worker healthy and can simply be retried with a longer timeout,
    ;; whereas a leaked thread is still executing and holding whatever locks
    ;; it had, so the session does not recover until the worker is replaced.
    (flet ((reason (ht)
             (gethash "reason" (aref (gethash "failed_tests" ht) 0))))
      (let ((stopped (reason (make-timeout-result 5)))
            (leaked (reason (make-timeout-result 5 :thread-leaked t))))
        (ok (search "was stopped" stopped))
        (ok (null (search "pool-kill-worker" stopped))
            "a healthy worker is not sent to pool-kill-worker")
        (ok (search "still executing" leaked))
        (ok (search "pool-kill-worker" leaked))))))
