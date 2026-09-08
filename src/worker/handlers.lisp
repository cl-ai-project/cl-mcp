;;;; src/worker/handlers.lisp
;;;;
;;;; Method handlers for worker JSON-RPC server.  Each handler wraps
;;;; a core function and builds the response structure expected by
;;;; the MCP tool layer.  The worker server's %dispatch-request
;;;; wraps handler return values in a JSON-RPC result envelope, so
;;;; handlers return just the payload hash-table.

(defpackage #:cl-mcp/src/worker/handlers
  (:use #:cl)
  (:import-from #:cl-mcp/src/repl-core
                #:repl-eval)
  (:import-from #:cl-mcp/src/code-core
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references)
  (:import-from #:cl-mcp/src/system-loader-core
                #:load-system
                #:*system-load-lock-wrapper*)
  (:import-from #:cl-mcp/src/test-runner-core
                #:run-tests
                #:call-with-test-run-deadline
                #:coerce-timeout-seconds
                #:make-timeout-result
                #:*load-lock-wrapper*)
  (:import-from #:cl-mcp/src/inspect
                #:inspect-object-by-id)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*
                #:register-project-root-source-registry)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/utils/paths
                #:broad-root-p)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:text-content)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-eval-response
                #:build-load-system-response
                #:build-run-tests-response
                #:build-code-find-response
                #:build-code-describe-response
                #:build-code-find-references-response
                #:build-inspect-response
                #:expand-and-build-response)
  (:import-from #:cl-mcp/src/worker/server
                #:register-method)
  (:import-from #:cl-mcp/src/worker/init-hook
                #:with-asdf-load-lock
                #:*asdf-load-lock-timeout*
                #:handle-init-start
                #:handle-init-status)
  (:export #:register-all-handlers))

(in-package #:cl-mcp/src/worker/handlers)

;;; ---------------------------------------------------------------------------
;;; Internal helpers
;;; ---------------------------------------------------------------------------

(defun %bool-default (params key default)
  "Extract boolean KEY from PARAMS hash-table with DEFAULT.
Distinguishes between key-not-present (use DEFAULT) and
key-present-with-false (use NIL)."
  (multiple-value-bind (val present-p)
      (gethash key params)
    (if present-p val default)))

;;; ---------------------------------------------------------------------------
;;; worker/eval
;;; ---------------------------------------------------------------------------

(defun %handle-eval (params)
  "Evaluate code and return the same response structure as define-tool
\"repl-eval\": content, stdout, stderr, and optional result_object_id,
result_preview, and error_context."
  (let* ((code (gethash "code" params))
         (package (gethash "package" params))
         (print-level (gethash "print_level" params))
         (print-length (gethash "print_length" params))
         ;; Coerced for the same reason %HANDLE-LOAD-SYSTEM coerces: params
         ;; arrive unvalidated here, and %REPL-EVAL-WITH-TIMEOUT treats a
         ;; non-real value as "no deadline at all" -- so a string would run
         ;; unbounded while the proxy, which does accept it, budgets for it
         ;; and kills the worker when that budget runs out.
         (raw-timeout (gethash "timeout_seconds" params))
         (timeout-seconds (coerce-timeout-seconds raw-timeout))
         (max-output-length (gethash "max_output_length" params))
         (safe-read (gethash "safe_read" params))
         (include-result-preview (%bool-default params "include_result_preview" t))
         (preview-max-depth (gethash "preview_max_depth" params))
         (preview-max-elements (gethash "preview_max_elements" params))
         (locals-preview-frames (gethash "locals_preview_frames" params))
         (locals-preview-max-depth (gethash "locals_preview_max_depth" params))
         (locals-preview-max-elements (gethash "locals_preview_max_elements" params))
         (locals-preview-skip-internal (%bool-default params "locals_preview_skip_internal" t)))
    (unless code
      (error "code is required"))
    (when (and raw-timeout (null timeout-seconds))
      (error "timeout_seconds must be a positive number"))
    ;; Checked for the same reason timeout_seconds is: params arrive
    ;; unvalidated here, and REPL-EVAL's declaimed ftype would turn a bad one
    ;; into a raw TYPE-ERROR naming an internal type, rather than a message
    ;; naming the argument the client got wrong.
    (when (and max-output-length
               (not (and (integerp max-output-length)
                         (not (minusp max-output-length)))))
      (error "max_output_length must be a non-negative integer"))
    (multiple-value-bind (printed raw-value stdout stderr error-context)
        (repl-eval code
                   :package (or package *package*)
                   :print-level print-level
                   :print-length print-length
                   ;; Always a server-side deadline, as run-tests has: without
                   ;; one an accidental (loop) pins this worker's single
                   ;; connection thread, and the proxy -- which budgets for
                   ;; this same default -- eventually gives up and kills the
                   ;; worker, resetting the session for what should have been
                   ;; a timeout report.
                   :timeout-seconds (or timeout-seconds 300)
                   :max-output-length max-output-length
                   :safe-read safe-read
                   :locals-preview-frames locals-preview-frames
                   :locals-preview-max-depth locals-preview-max-depth
                   :locals-preview-max-elements locals-preview-max-elements
                   :locals-preview-skip-internal locals-preview-skip-internal)
      (build-eval-response printed raw-value stdout stderr error-context
                           :include-result-preview include-result-preview
                           :preview-max-depth (or preview-max-depth 1)
                           :preview-max-elements (or preview-max-elements 8)
                           :max-output-length max-output-length))))

;;; ---------------------------------------------------------------------------
;;; worker/load-system
;;; ---------------------------------------------------------------------------

(defun %handle-load-system (params)
  "Load an ASDF system.  Returns the same structure as define-tool
\"load-system\".  Serializes against every other cl-mcp-mediated ASDF load
through *ASDF-LOAD-LOCK*.

The lock is taken from inside LOAD-SYSTEM's own deadline thread rather than
around the call, by way of *SYSTEM-LOAD-LOCK-WRAPPER*, so the thread doing the
ASDF work is the thread holding the lock.  Wrapping the call instead would
release the lock the moment this handler was answered -- and a load that
outlived its deadline is still running inside ASDF, so the next load would
start a second ASDF operation alongside the first.  It also keeps the wait for
the lock inside the caller's timeout_seconds instead of ahead of it, which
matters because the proxy only allows that timeout plus a small margin before
it gives up and kills the worker."
  (let* ((system (gethash "system" params))
         (force (%bool-default params "force" t))
         (clear-fasls (gethash "clear_fasls" params))
         (raw-timeout (gethash "timeout_seconds" params))
         ;; Params arrive straight off the wire here, so the value has had no
         ;; type check: calling PLUSP on it directly turns a string into a raw
         ;; TYPE-ERROR reported as "Internal error during load-system".
         (timeout-seconds (coerce-timeout-seconds raw-timeout)))
    (unless system
      (error "system is required"))
    (when (and raw-timeout (null timeout-seconds))
      (error "timeout_seconds must be a positive number"))
    (let* ((budget (or timeout-seconds 120))
           (ht (let ((*system-load-lock-wrapper*
                       (lambda (thunk)
                         ;; Bound inside the lambda, not around it: this runs
                         ;; on the deadline thread, which does not inherit
                         ;; bindings made here.
                         ;; A margin below the load's own budget, so which of
                       ;; the two fires is settled by design rather than by
                       ;; the deadline poller's 50 ms granularity.  The lock
                       ;; message names the thread holding it and what to do;
                       ;; the load's generic timeout does not.  Proportional
                       ;; rather than "one second less": timeout_seconds
                       ;; accepts fractions, and a fixed second inverts the
                       ;; ordering for anything under two.
                       (let ((*asdf-load-lock-timeout* (* budget 9/10)))
                           (with-asdf-load-lock (funcall thunk))))))
                 (load-system system
                              :force force
                              :clear-fasls clear-fasls
                              :timeout-seconds budget))))
      (build-load-system-response system ht))))

;;; ---------------------------------------------------------------------------
;;; worker/run-tests
;;; ---------------------------------------------------------------------------

(defun %handle-run-tests (params)
  "Run tests for a system.  Returns the same structure as define-tool
\"run-tests\".  Honors timeout_seconds to limit test execution time.
Holds *ASDF-LOAD-LOCK* only for RUN-TESTS' force-reload phase, by binding
*LOAD-LOCK-WRAPPER*, so a load here cannot overlap a concurrent load.  The
test run itself must NOT hold the lock: the suite may take a lock of its
own or block on one, and a test that blocked on *ASDF-LOAD-LOCK* while the
lock sat with its holder waiting for the run would deadlock.

The run executes on its own thread under CALL-WITH-TEST-RUN-DEADLINE, so
it cannot pin this worker's single connection thread.  That matters for a
suite that leaves something blocking behind -- a server accept loop, say:
SB-EXT:WITH-TIMEOUT cannot interrupt a blocking foreign call, so run
inline such a suite would wedge the connection thread and every later tool
call for this session with it.  Polling bounds the wait regardless, and the
caller is answered at the deadline even while the suite is still blocked."
  (let ((system (gethash "system" params))
        (framework (gethash "framework" params))
        (test (gethash "test" params))
        (tests (gethash "tests" params))
        (timeout (coerce-timeout-seconds (gethash "timeout_seconds" params))))
    (unless system
      (error "system is required"))
    ;; Always enforce a server-side deadline so the worker's connection
    ;; thread is never blocked indefinitely.  Use the documented default
    ;; (300 s) when the client omits timeout_seconds.
    (let ((effective-timeout (or timeout 300)))
      (flet ((do-run ()
               ;; The lock covers RUN-TESTS' force-reload only; see the
               ;; docstring for why the test run itself must stay outside it.
               (let ((*load-lock-wrapper*
                       (lambda (thunk)
                         ;; Bound to the caller's budget, as %HANDLE-LOAD-SYSTEM
                         ;; does, so the lock's own diagnostic can actually be
                         ;; reached: left at the global default a caller asking
                         ;; for less than that always meets the run deadline
                         ;; first and never sees which thread held the lock.
                         (let ((*asdf-load-lock-timeout*
                                 (* effective-timeout 9/10)))
                           (with-asdf-load-lock (funcall thunk))))))
                 (run-tests system
                            :framework framework
                            :test test
                            :tests tests))))
        (multiple-value-bind (result status thread-leaked)
            (call-with-test-run-deadline #'do-run effective-timeout)
          (build-run-tests-response
           (ecase status
             (:ok result)
             (:timeout (make-timeout-result result
                                            :thread-leaked thread-leaked))
             ;; Re-signal so genuine failures still surface as JSON-RPC
             ;; errors instead of being reported as a bogus test result.
             (:error (error result)))))))))

;;; ---------------------------------------------------------------------------
;;; worker/code-find
;;; ---------------------------------------------------------------------------

(defun %handle-code-find (params)
  "Find symbol definition.  Returns the same structure as define-tool
\"code-find\", or an isError payload when the symbol is not found."
  (let ((symbol (gethash "symbol" params))
        (package (gethash "package" params)))
    (unless symbol
      (error "symbol is required"))
    (multiple-value-bind (path line on-disk)
        (code-find-definition symbol :package package)
      (build-code-find-response symbol path line on-disk))))

;;; ---------------------------------------------------------------------------
;;; worker/code-describe
;;; ---------------------------------------------------------------------------

(defun %handle-code-describe (params)
  "Describe a symbol.  Returns the same structure as define-tool
\"code-describe\"."
  (let ((symbol (gethash "symbol" params))
        (package (gethash "package" params)))
    (unless symbol
      (error "symbol is required"))
    (multiple-value-bind (name type arglist doc path line)
        (code-describe-symbol symbol :package package)
      (build-code-describe-response name type arglist doc path line))))

;;; ---------------------------------------------------------------------------
;;; worker/code-find-references
;;; ---------------------------------------------------------------------------

(defun %handle-code-find-references (params)
  "Find symbol references.  Returns the same structure as define-tool
\"code-find-references\"."
  (let ((symbol (gethash "symbol" params))
        (package (gethash "package" params))
        (project-only (%bool-default params "project_only" t)))
    (unless symbol
      (error "symbol is required"))
    (multiple-value-bind (refs count)
        (code-find-references symbol :package package :project-only project-only)
      (build-code-find-references-response symbol refs count project-only))))

;;; ---------------------------------------------------------------------------
;;; worker/inspect-object
;;; ---------------------------------------------------------------------------

(defun %handle-inspect-object (params)
  "Inspect a registered object by ID.  Returns the same structure as
define-tool \"inspect-object\"."
  (let ((object-id (gethash "id" params))
        (max-depth (gethash "max_depth" params))
        (max-elements (gethash "max_elements" params)))
    (unless object-id
      (error "id is required"))
    (let ((inspection-result (inspect-object-by-id object-id
                                                   :max-depth (or max-depth 1)
                                                   :max-elements (or max-elements 50))))
      (build-inspect-response inspection-result))))

;;; ---------------------------------------------------------------------------
;;; worker/macroexpand
;;; ---------------------------------------------------------------------------

(defun %handle-macroexpand (params)
  "Expand macro forms.  Returns the same structure as define-tool
\"lisp-macroexpand\".

PARAMS carries \"forms\", a JSON array (so: a vector) of objects with
\"label\" and \"source\" keys.  The parent has already located the forms
and extracted their source text; this handler only re-reads that text in
the real, loaded package and expands it.

\"sub_form\" carries the parent's sub_form argument, purely so a NOT
EXPANDED result can mention that sub-form matching is positional-blind.
Only its presence matters here; the parent already did the matching."
  (let ((forms (gethash "forms" params))
        (package (gethash "package" params))
        (level (or (gethash "level" params) "once"))
        (readtable (gethash "readtable" params))
        (note (gethash "note" params))
        (sub-form (gethash "sub_form" params)))
    (unless (and forms (plusp (length forms)))
      (error "forms is required"))
    (let ((entries (map 'list
                        (lambda (form)
                          (cons (gethash "label" form) (gethash "source" form)))
                        forms)))
      (expand-and-build-response
       entries
       :package package
       :level level
       :readtable readtable
       :print-level (gethash "print_level" params)
       :print-length (gethash "print_length" params)
       :max-output-length (gethash "max_output_length" params)
       :note note
       :sub-form-p (and sub-form t)))))

;;; ---------------------------------------------------------------------------
;;; worker/set-project-root
;;; ---------------------------------------------------------------------------

(defun %handle-set-project-root (params)
  "Set the worker's project root directory and change the working
directory.  Resolves symlinks via TRUENAME for canonical paths.
Returns a success payload."
  (let ((path (gethash "path" params)))
    (unless path
      (error "path is required"))
    (let ((dir-path (uiop/pathname:ensure-directory-pathname path)))
      (unless (uiop/filesystem:directory-exists-p dir-path)
        (error "Directory does not exist: ~A" path))
      ;; Reject overly broad roots (same policy as fs-set-project-root)
      (when (broad-root-p dir-path)
        (error "Refusing to set project root to ~A -- too broad"
               (namestring dir-path)))
      ;; Resolve symlinks for a canonical path
      (let ((resolved (truename dir-path)))
        (when resolved
          (setf dir-path resolved)))
      (setf *project-root* dir-path)
      (uiop/os:chdir dir-path)
      ;; Re-point ASDF resolution at the new project root (see the helper's
      ;; docstring). This is what fixes dynamic fs-set-project-root and reused
      ;; pool workers: without it a same-named system already reachable via the
      ;; inherited registry keeps winning after a root change.
      (register-project-root-source-registry dir-path)
      (log-event :info "worker.project-root.set" "path" (namestring dir-path))
      (make-ht "content" (text-content
                          (format nil "Project root set to ~A" (namestring dir-path)))
               "path" (namestring dir-path)))))

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(defun register-all-handlers (server)
  "Register all worker method handlers on SERVER."
  (register-method server "worker/eval" #'%handle-eval)
  (register-method server "worker/load-system" #'%handle-load-system)
  (register-method server "worker/run-tests" #'%handle-run-tests)
  (register-method server "worker/code-find" #'%handle-code-find)
  (register-method server "worker/code-describe" #'%handle-code-describe)
  (register-method server "worker/code-find-references" #'%handle-code-find-references)
  (register-method server "worker/inspect-object" #'%handle-inspect-object)
  (register-method server "worker/macroexpand" #'%handle-macroexpand)
  (register-method server "worker/set-project-root" #'%handle-set-project-root)
  (register-method server "worker/init-start" #'handle-init-start)
  (register-method server "worker/init-status" #'handle-init-status)
  (log-event :info "worker.handlers.registered" "count" 11)
  server)
