;;;; tests/pool-startup-latency-test.lisp
;;;;
;;;; Characterization tests for the cost of bringing the worker pool
;;;; online from a cold start.
;;;;
;;;; initialize-pool spawns *worker-pool-warmup* standby workers before
;;;; returning control to its caller.  Each spawn forks an SBCL child,
;;;; loads slynk, and waits for a handshake -- several seconds per
;;;; standby on a cold FASL cache.  When cl-mcp:run is invoked over
;;;; stdio this delays the "stdio.start" log event and the first
;;;; JSON-RPC response, racing the MCP client's handshake timeout and
;;;; surfacing to the user as "Failed to connect" even though the
;;;; server starts cleanly.
;;;;
;;;; These tests pin the contract: bringing the pool online must not
;;;; block on subprocess launches.

(defpackage #:cl-mcp/tests/pool-startup-latency-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/pool
                #:*worker-pool-warmup*
                #:*health-check-interval-seconds*
                #:*shutdown-replenish-wait-seconds*
                #:initialize-pool
                #:shutdown-pool)
  (:import-from #:cl-mcp/src/run
                #:run)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p))

(in-package #:cl-mcp/tests/pool-startup-latency-test)

(defun %elapsed-seconds (thunk)
  "Run THUNK; return elapsed wall-clock seconds as a single-float."
  (let ((start (get-internal-real-time)))
    (funcall thunk)
    (coerce (/ (- (get-internal-real-time) start)
               internal-time-units-per-second)
            'single-float)))

(defmacro %with-fast-pool-defaults ((&key warmup) &body body)
  "Bind pool dynamic vars to test-friendly defaults around BODY.
HEALTH-CHECK-INTERVAL is set high so the monitor does not race the test.
SHUTDOWN-REPLENISH-WAIT is short so teardown is snappy."
  `(let ((*worker-pool-warmup* ,warmup)
         (*health-check-interval-seconds* 60.0d0)
         (*shutdown-replenish-wait-seconds* 0.01d0))
     ,@body))

(deftest initialize-pool-returns-fast-with-zero-warmup
  (testing "initialize-pool with warmup=0 returns within 1 second"
    (%with-fast-pool-defaults (:warmup 0)
      (unwind-protect
           (let ((elapsed (%elapsed-seconds #'initialize-pool)))
             (ok (< elapsed 1.0)
                 (format nil "warmup=0 init took ~,3F s (bound 1.0)"
                         elapsed)))
        (shutdown-pool)))))

(deftest initialize-pool-returns-fast-with-positive-warmup
  (testing "initialize-pool with warmup=2 returns within 2 seconds
(spawning warm standbys must not block the caller -- they belong on
a background replenish thread)"
    (unless (spawn-available-p)
      (skip "sbcl not available"))
    (%with-fast-pool-defaults (:warmup 2)
      (unwind-protect
           (let ((elapsed (%elapsed-seconds #'initialize-pool)))
             (ok (< elapsed 2.0)
                 (format nil "warmup=2 init took ~,3F s (bound 2.0)"
                         elapsed)))
        (shutdown-pool)))))

(deftest pool-eventually-reaches-warmup-target
  (testing "after initialize-pool returns, the standby count reaches
the warmup target within a generous bound"
    (unless (spawn-available-p)
      (skip "sbcl not available"))
    ;; Bound is loose because cold-cache spawn is several seconds per
    ;; worker and a prior test's in-flight replenish can still own
    ;; *replenish-running* for one extra spawn after this test starts.
    (%with-fast-pool-defaults (:warmup 2)
      (unwind-protect
           (progn
             (initialize-pool)
             (let ((deadline (+ (get-internal-real-time)
                                (* 90 internal-time-units-per-second))))
               (loop
                 while (< (get-internal-real-time) deadline)
                 when (= 2 (length cl-mcp/src/pool::*standby-workers*))
                   return nil
                 do (sleep 0.1)))
             (ok (= 2 (length cl-mcp/src/pool::*standby-workers*))
                 (format nil "expected 2 standbys within 90 s, got ~D"
                         (length cl-mcp/src/pool::*standby-workers*))))
        (shutdown-pool)))))

(defparameter +initialize-line+
  (concatenate 'string
               "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\","
               "\"params\":{\"protocolVersion\":\"2024-11-05\","
               "\"capabilities\":{},"
               "\"clientInfo\":{\"name\":\"latency-probe\",\"version\":\"0.0.1\"}}}"
               (string #\Newline)))

(deftest stdio-handshake-responds-quickly-under-warmup
  (testing "cl-mcp:run :transport :stdio responds to initialize within
3 seconds even with warmup=2.  Captures the user-visible symptom
(harness Failed to connect when warmup eats the handshake budget)."
    (unless (spawn-available-p)
      (skip "sbcl not available"))
    (%with-fast-pool-defaults (:warmup 2)
      (let ((in (make-string-input-stream +initialize-line+))
            (out (make-string-output-stream)))
        (let ((elapsed
                (%elapsed-seconds
                 (lambda ()
                   (run :transport :stdio :in in :out out
                        :worker-pool t)))))
          (let ((response (get-output-stream-string out)))
            (ok (search "\"protocolVersion\"" response)
                "stdio response includes protocolVersion")
            (ok (< elapsed 3.0)
                (format nil "stdio initialize took ~,3F s (bound 3.0)"
                        elapsed))))))))
