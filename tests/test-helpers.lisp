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
           #:with-pool
           #:wait-for))

(in-package #:cl-mcp/tests/test-helpers)

(defmacro wait-for ((&key (timeout 5.0d0) (interval 0.01d0)) &body body)
  "Wait for BODY to return non-nil, polling at INTERVAL until TIMEOUT.
Returns the result of BODY if it became non-nil, or NIL if it timed out."
  (let ((start-var (gensym "START"))
        (result-var (gensym "RESULT")))
    `(let ((,start-var (get-internal-real-time)))
       (loop
         (let ((,result-var (progn ,@body)))
           (when ,result-var (return ,result-var))
           (when (> (/ (- (get-internal-real-time) ,start-var)
                       internal-time-units-per-second)
                    ,timeout)
             (return nil))
           (sleep ,interval))))))

(defun spawn-available-p ()
  "Check if we can spawn worker processes.
Uses :wait t and checks exit code to avoid a TOCTOU race."
  (ignore-errors
    (let* ((cmd (if (member :ros.init *features*)
                    '("ros" "version")
                    '("sbcl" "--version")))
           (p (sb-ext:run-program (first cmd) (rest cmd)
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
