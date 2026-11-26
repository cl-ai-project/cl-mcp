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
           #:call-with-late-warning-project))

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

(defun call-with-late-warning-project (thunk)
  "Call THUNK with the directory of a cold ASDF project with two compile warnings."
  (let ((root (merge-pathnames (format nil "clmcp-late-warning-~36R/"
                                       (random most-positive-fixnum (make-random-state t)))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (dolist (file
                    '(("clmcp-late-app.asd" .
                       "(defsystem \"clmcp-late-app\"
                          :defsystem-depends-on (\"clmcp-late-bootstrap\")
                          :depends-on (\"clmcp-late-dep\") :components ((:file \"app\")))")
                      ("app.lisp" . "(in-package :cl-user)")
                      ("clmcp-late-bootstrap.asd" .
                       "(defsystem \"clmcp-late-bootstrap\" :components ((:file \"bootstrap\")))")
                      ("bootstrap.lisp" .
                       "(eval-when (:compile-toplevel)
                          (assert (not (asdf:registered-system \"clmcp-late-dep\")))
                          (warn \"bootstrap compile warning\"))")
                      ("clmcp-late-dep.asd" .
                       "(defsystem \"clmcp-late-dep\" :pathname \".qlot/\"
                          :components ((:file \"dep\")))")
                      (".qlot/dep.lisp" .
                       "(eval-when (:compile-toplevel) (warn \"late dependency compile warning\"))")))
             (destructuring-bind (name . text) file
               (let ((path (merge-pathnames name root)))
                 (ensure-directories-exist path)
                 (with-open-file (out path :direction :output :if-exists :error)
                   (write-string text out)))))
           (let ((asdf/output-translations:*output-translations*
                   (list (cons (list (uiop:wilden root) t)
                               (asdf:ensure-output-translations)))))
             (funcall thunk root)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
