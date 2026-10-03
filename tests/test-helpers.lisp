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

(defvar *spawn-available* :unknown
  "SPAWN-AVAILABLE-P's answer once it has been computed, or :UNKNOWN.")

(defun spawn-available-p ()
  "Check if we can spawn worker processes.
Uses :wait t and checks exit code to avoid a TOCTOU race.

Asked once per process and remembered: starting `ros version' takes about two
and a half seconds, eighty-odd tests ask before spawning a worker, and the
answer -- whether this machine has the launcher at all -- does not change while
the tests run.  That was about three and a half minutes of a full run."
  (when (eq *spawn-available* :unknown)
    (setf *spawn-available*
          (and (ignore-errors
                 (let* ((cmd (if (member :ros.init *features*)
                                 '("ros" "version")
                                 '("sbcl" "--version")))
                        (p (sb-ext:run-program (first cmd) (rest cmd)
                             :search t :output :stream :wait t)))
                   (prog1 (zerop (sb-ext:process-exit-code p))
                     (ignore-errors (sb-ext:process-close p)))))
               t)))
  *spawn-available*)

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
