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
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:*asdf-load-lock*
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

(defmacro with-asdf-load-lock (&body body)
  "Evaluate BODY holding *ASDF-LOAD-LOCK*."
  `(bt:with-lock-held (*asdf-load-lock*) ,@body))

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
            (load-system system :force nil :timeout-seconds nil))
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
