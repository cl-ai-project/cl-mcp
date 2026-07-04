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
  (:export #:*asdf-load-lock*
           #:with-asdf-load-lock))

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
