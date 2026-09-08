;;;; src/worker/init-hook.lisp
;;;;
;;;; Worker-side machinery for the init hook.  Provides the worker-global
;;;; ASDF load lock (so cl-mcp-mediated loads never overlap), the init
;;;; state machine, entry resolution, and the worker/init-start and
;;;; worker/init-status RPC handlers.  See
;;;; docs/plans/2026-07-05-worker-init-hook-design.md.

(defpackage #:cl-mcp/src/worker/init-hook
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held)
  (:import-from #:cl-mcp/src/system-loader-core #:load-system)
  (:import-from #:cl-mcp/src/repl-core #:repl-eval)
  (:import-from #:cl-mcp/src/utils/sanitize #:sanitize-error-message)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht #:transient-error)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:*asdf-load-lock*
           #:*asdf-load-lock-timeout*
           #:with-asdf-load-lock
           #:handle-init-start
           #:handle-init-status))

(in-package #:cl-mcp/src/worker/init-hook)

(defvar *asdf-load-lock* (bt:make-lock "asdf-load-lock")
  "Worker-global lock serializing every cl-mcp-mediated ASDF load site
(worker/init, worker/load-system, worker/run-tests).  Prevents two
concurrent ASDF load-ops in one worker image, which the single-threaded
dispatch loop does NOT prevent because load-system/repl-eval run their
work on spawned helper threads.")

(defparameter *asdf-load-lock-timeout* 240
  "Seconds to wait for *ASDF-LOAD-LOCK* before giving up on it.

The lock is normally held only for the duration of one load, so waiting this
long already means something is wrong -- in practice, a run-tests deadline
that could not stop its thread, leaving the load lock owned by a thread that
will never release it.  Waiting forever there would hang this worker's single
connection thread on every later load, with nothing to tell the user why.

The value sits below the proxy's own budget on purpose: the proxy waits
*PROXY-RPC-TIMEOUT* plus *PROXY-RPC-BUFFER*, and a timeout there kills the
worker with a generic crash notice.  Giving up first means the caller gets
this message, which names the cause and the fix, instead.")

(defmacro with-asdf-load-lock (&body body)
  "Evaluate BODY holding *ASDF-LOAD-LOCK*, waiting at most
*ASDF-LOAD-LOCK-TIMEOUT* seconds to acquire it."
  `(call-with-asdf-load-lock (lambda () ,@body)))

(defun %signal-asdf-load-lock-timeout ()
  "Signal that *ASDF-LOAD-LOCK* could not be acquired, naming the likely cause.

Three situations reach here and they need different advice.  The init hook
loads on its own thread, holding this lock for a whole cold compile with no
deadline of its own, so a large application system legitimately keeps it past
this timeout -- telling that caller to kill the worker would abort a healthy
load that was about to finish.  The lock may also be free again by the time we
look, which means the contention was transient and there is nothing to fix.
Only a lock still held by some other thread means a run outlived its deadline
and will never release it, and only there does the worker have to be replaced."
  (let* ((owner (sb-thread:mutex-owner *asdf-load-lock*))
         (owner-name (and owner (sb-thread:thread-name owner))))
    (ignore-errors
     (log-event :error "worker.asdf-load-lock.timeout"
                "seconds" *asdf-load-lock-timeout*
                "owner" (or owner-name "none")))
    (cond
      ;; Released between the wait expiring and this sample.  Reporting a
      ;; wedged worker for a lock that is free again would send the caller to
      ;; pool-kill-worker for nothing.
      ((null owner)
       (error 'transient-error
              :format-control
              "Timed out after ~A seconds waiting for this worker's ASDF load ~
               lock, which was released just as the wait expired. Nothing is ~
               wrong with this worker; retry."
              :format-arguments (list *asdf-load-lock-timeout*)))
      ;; TRANSIENT-ERROR, so the response builders withhold their standing
      ;; "replace the worker" advice.  Appending it here would undo the whole
      ;; point of separating these cases: the caller would be told to run
      ;; pool-kill-worker and would abort a healthy load.
      ((equal owner-name "mcp-worker-init")
       (error 'transient-error
              :format-control
              "Timed out after ~A seconds waiting for this worker's ASDF load ~
               lock: the init hook is still loading and holds it. That load ~
               has no deadline of its own, so a cold compile of a large system ~
               can legitimately take longer. Wait for worker/init-status to ~
               report ready, or retry with a larger timeout_seconds."
              :format-arguments (list *asdf-load-lock-timeout*)))
      (t
       (error "Timed out after ~A seconds waiting for this worker's ASDF load ~
               lock~@[, held by thread ~A~]. A previous run most likely left a ~
               thread behind that never released it; this worker cannot load ~
               systems again. Use pool-kill-worker to get a fresh worker."
              *asdf-load-lock-timeout*
              owner-name)))))

(defun call-with-asdf-load-lock (thunk)
  "Call THUNK holding *ASDF-LOAD-LOCK*, or signal if it cannot be acquired.
Times out rather than blocking forever, so a lock left owned by a thread that
outlived its deadline surfaces as one actionable error instead of hanging
every later load in this worker.

SB-THREAD:WITH-MUTEX rather than a GRAB-MUTEX/UNWIND-PROTECT pair: SBCL
documents GRAB-MUTEX and RELEASE-MUTEX as not interrupt-safe, and this runs on
the very thread a run-tests deadline interrupts.  An interrupt arriving
between GRAB-MUTEX returning and the UNWIND-PROTECT being established would
leak the lock outright -- the exact failure the timeout below exists to make
survivable.  WITH-MUTEX returns NIL rather than signalling when the timeout
expires, so RAN distinguishes that from a THUNK that returned NIL."
  (let* ((ran nil)
         (values (sb-thread:with-mutex (*asdf-load-lock*
                                        :timeout *asdf-load-lock-timeout*)
                   (setf ran t)
                   (multiple-value-list (funcall thunk)))))
    (if ran
        (values-list values)
        (%signal-asdf-load-lock-timeout))))

(defvar *init-lock* (bt:make-lock "worker-init-state")
  "Protects *INIT-STATE*.")

(defvar *init-state* (list :state :idle :app-port nil :error nil :started-at nil)
  "Init progress: :state is one of :idle :loading :running :failed.")

(defun %reset-init-state ()
  "Reset init state to :idle (used by tests and re-arming)."
  (bt:with-lock-held (*init-lock*)
    (setf *init-state* (list :state :idle :app-port nil :error nil
                             :started-at nil))))

(defun %set-init-state (state &key app-port error)
  "Transition init state.  STATE is a keyword; APP-PORT/ERROR update the
corresponding fields when provided."
  (bt:with-lock-held (*init-lock*)
    (setf (getf *init-state* :state) state)
    (when (eq state :loading)
      (setf (getf *init-state* :started-at) (get-universal-time)))
    (when app-port (setf (getf *init-state* :app-port) app-port))
    (when error (setf (getf *init-state* :error) error))))

(defun init-state-snapshot ()
  "Return a hash-table snapshot of init state for pool-status / RPC.
Keys: init_state, app_port, last_init_error, started_at."
  (bt:with-lock-held (*init-lock*)
    (let ((ht (make-hash-table :test 'equal)))
      (setf (gethash "init_state" ht) (string-downcase (getf *init-state* :state))
            (gethash "app_port" ht) (getf *init-state* :app-port)
            (gethash "last_init_error" ht) (getf *init-state* :error)
            (gethash "started_at" ht) (getf *init-state* :started-at))
      ht)))

(defun %resolve-entry (spec)
  "Resolve a \"PKG:SYMBOL\" or \"PKG::SYMBOL\" string to a callable.
Uses find-package / find-symbol / fdefinition only -- no read, eval, or
intern -- honoring the project's no-runtime-eval style rule.  Signals an
error if the package or symbol is missing or the symbol is not fbound."
  (let* ((dbl (search "::" spec))
         (colon (or dbl (position #\: spec))))
    (unless colon
      (error "init entry ~S must be of the form PKG:SYMBOL" spec))
    (let* ((pkg-name (string-upcase (subseq spec 0 colon)))
           (sym-name (string-upcase (subseq spec (+ colon (if dbl 2 1)))))
           (pkg (find-package pkg-name)))
      (unless pkg
        (error "init entry: package ~A not found" pkg-name))
      (let ((sym (find-symbol sym-name pkg)))
        (unless sym
          (error "init entry: symbol ~A not found in package ~A"
                 sym-name pkg-name))
        (unless (fboundp sym)
          (error "init entry: ~A is not fbound" sym))
        (fdefinition sym)))))

(defun %maybe-eval (form-string package-name)
  "Run FORM-STRING via repl-core:repl-eval in PACKAGE-NAME.  Signals an
error if the evaluation produced an error-context, so the outer
handler-case records a :failed init.  Routing through repl-eval (not raw
eval) reuses the sanctioned evaluator.  repl-eval returns its error-context
as a plist keyed by keywords (:message, :condition-type, ...), so we pull
:message for a clean failure string."
  (let ((pkg (or (find-package (string-upcase package-name)) *package*)))
    (multiple-value-bind (printed raw stdout stderr err-ctx)
        (repl-eval form-string :package pkg)
      (declare (ignore printed raw stdout stderr))
      (when err-ctx
        (error "init eval failed: ~A"
               (or (and (listp err-ctx) (getf err-ctx :message))
                   err-ctx))))))

(defun %run-init (params)
  "Background-thread init runner.  Holds *ASDF-LOAD-LOCK* for the whole
load so it cannot overlap a concurrent load-system/run-tests.  Loads with
timeout=NIL (the direct branch -- no spawned thread, no destroy-thread
mid-compile).  Never signals out of this function: on any error it records
a :failed init and leaves the worker fully usable."
  (let ((system (gethash "system" params))
        (evalform (gethash "eval" params))
        (entry (gethash "entry" params))
        (pkg (or (gethash "package" params) "CL-USER")))
    (%set-init-state :loading)
    (handler-case
        (with-asdf-load-lock
          (when system
            (let* ((result (load-system system :force nil :timeout-seconds nil))
                   (status (and (hash-table-p result) (gethash "status" result))))
              ;; load-system returns a status hash (does NOT signal) on compile
              ;; error / missing system; surface a non-"loaded" status as a
              ;; failed init instead of falsely reporting :running.
              (unless (equal status "loaded")
                (error "init system ~A failed to load (status: ~A)"
                       system (or status "unknown")))))
          (when evalform
            (%maybe-eval evalform pkg))
          (let ((port nil))
            ;; Entry contract: the thunk MUST return promptly (e.g. a
            ;; clackup with :use-thread t that starts the server on its own
            ;; thread and returns) and MUST NOT re-enter WITH-ASDF-LOAD-LOCK
            ;; or otherwise block -- the worker-global load lock is held for
            ;; this whole body, so a blocking or re-entrant entry would
            ;; deadlock every other load site.  A direct LOAD-SYSTEM call
            ;; from the entry is safe: the lock lives at the handler layer,
            ;; not inside load-system itself.
            (when entry
              (setf port (funcall (%resolve-entry entry))))
            (%set-init-state :running
                             :app-port (and (integerp port) port))
            (log-event :info "worker.init.done"
                       "app_port" (and (integerp port) port))))
      (serious-condition (e)
        (let ((msg (or (ignore-errors (sanitize-error-message e)) "init failed")))
          (%set-init-state :failed :error msg)
          (ignore-errors
            (log-event :warn "worker.init.failed" "error" msg)))))))

(defun handle-init-start (params)
  "worker/init-start handler.  Spawns the init runner on a background
thread and returns an ACK immediately, so the parent's RPC does not block
on the (heavy) load and no long stream-lock is held."
  (bt:make-thread (lambda () (%run-init params)) :name "mcp-worker-init")
  (make-ht "accepted" t))

(defun handle-init-status (params)
  "worker/init-status handler.  Returns the current init state snapshot."
  (declare (ignore params))
  (init-state-snapshot))
