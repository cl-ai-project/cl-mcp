;;;; tests/test-helpers.lisp
;;;;
;;;; Shared test utilities for pool-related test suites.

(defpackage #:cl-mcp/tests/test-helpers
  (:use #:cl)
  (:import-from #:cl-mcp/src/pool
                #:*worker-pool-warmup*
                #:*health-check-interval-seconds*
                #:*shutdown-replenish-wait-seconds*
                #:initialize-pool
                #:shutdown-pool)
  (:export #:spawn-available-p
           #:with-pool))

(in-package #:cl-mcp/tests/test-helpers)

(defun spawn-available-p ()
  "Check if we can spawn worker processes.
Uses :wait t and checks exit code to avoid a TOCTOU race where
the short-lived process exits before process-alive-p is called."
  (ignore-errors
    (let ((p (sb-ext:run-program "ros" '("version")
               :search t :output :stream :wait t)))
      (prog1 (zerop (sb-ext:process-exit-code p))
        (ignore-errors (sb-ext:process-close p))))))

(defmacro with-pool ((&key (health-check-interval 60.0d0)) &body body)
  "Initialize the pool, execute BODY, and always shut down the pool.
Uses tighter timing defaults to keep integration tests fast.
HEALTH-CHECK-INTERVAL defaults to 60s so the health monitor does not
race with tests that manually crash workers.  Pass a short interval
\(e.g. 0.1d0) when testing health-monitor-driven crash detection."
  `(let ((*worker-pool-warmup* 0)
         (*health-check-interval-seconds* ,health-check-interval)
         (*shutdown-replenish-wait-seconds* 0.01d0))
     (unwind-protect
         (progn (initialize-pool) ,@body)
       (shutdown-pool))))
