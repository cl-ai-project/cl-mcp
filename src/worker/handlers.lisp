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
                #:load-system)
  (:import-from #:cl-mcp/src/test-runner-core
                #:run-tests)
  (:import-from #:cl-mcp/src/inspect
                #:inspect-object-by-id)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:text-content)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-eval-response
                #:build-load-system-response
                #:build-run-tests-response)
  (:import-from #:cl-mcp/src/worker/server
                #:register-method)
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
         (timeout-seconds (gethash "timeout_seconds" params))
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
    (multiple-value-bind (printed raw-value stdout stderr error-context)
        (repl-eval code
                   :package (or package *package*)
                   :print-level print-level
                   :print-length print-length
                   :timeout-seconds timeout-seconds
                   :max-output-length max-output-length
                   :safe-read safe-read
                   :locals-preview-frames locals-preview-frames
                   :locals-preview-max-depth locals-preview-max-depth
                   :locals-preview-max-elements locals-preview-max-elements
                   :locals-preview-skip-internal locals-preview-skip-internal)
      (build-eval-response printed raw-value stdout stderr error-context
                           :include-result-preview include-result-preview
                           :preview-max-depth (or preview-max-depth 1)
                           :preview-max-elements (or preview-max-elements 8)))))

;;; ---------------------------------------------------------------------------
;;; worker/load-system
;;; ---------------------------------------------------------------------------

(defun %handle-load-system (params)
  "Load an ASDF system.  Returns the same structure as define-tool
\"load-system\"."
  (let* ((system (gethash "system" params))
         (force (%bool-default params "force" t))
         (clear-fasls (gethash "clear_fasls" params))
         (timeout-seconds (gethash "timeout_seconds" params)))
    (unless system
      (error "system is required"))
    (when (and timeout-seconds (not (plusp timeout-seconds)))
      (error "timeout_seconds must be a positive number"))
    (let ((ht (load-system system
                           :force force
                           :clear-fasls clear-fasls
                           :timeout-seconds (or timeout-seconds 120))))
      (build-load-system-response system ht))))

;;; ---------------------------------------------------------------------------
;;; worker/run-tests
;;; ---------------------------------------------------------------------------

(defun %handle-run-tests (params)
  "Run tests for a system.  Returns the same structure as define-tool
\"run-tests\"."
  (let* ((system (gethash "system" params))
         (framework (gethash "framework" params))
         (test (gethash "test" params))
         (tests (gethash "tests" params)))
    (unless system
      (error "system is required"))
    (let ((test-result (run-tests system
                                  :framework framework
                                  :test test
                                  :tests tests)))
      (build-run-tests-response test-result))))

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
    (multiple-value-bind (path line)
        (code-find-definition symbol :package package)
      (if path
          (make-ht "path" path
                   "line" line
                   "content" (text-content
                              (format nil "~A defined in ~A at line ~D"
                                      symbol path line)))
          (make-ht "isError" t
                   "content" (text-content
                              (format nil "Definition not found for ~A"
                                      symbol)))))))

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
      (make-ht "name" name
               "type" type
               "arglist" arglist
               "documentation" doc
               "path" path
               "line" line
               "content" (text-content
                          (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]~@[~%Defined at ~A:~D~]"
                                  name type arglist doc path line))))))

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
      (let* ((summary-lines
               (map 'list
                    (lambda (h)
                      (format nil "~A:~D ~A ~A"
                              (gethash "path" h)
                              (gethash "line" h)
                              (gethash "type" h)
                              (gethash "context" h)))
                    refs))
             (summary (if summary-lines
                          (format nil "~{~A~%~}" summary-lines)
                          "")))
        (make-ht "refs" refs
                 "count" count
                 "symbol" symbol
                 "project_only" project-only
                 "content" (text-content summary))))))

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
      (if (gethash "error" inspection-result)
          (make-ht "isError" t
                   "content" (text-content (gethash "message" inspection-result)))
          (let ((summary (format nil "[~A] ~A"
                                 (gethash "kind" inspection-result)
                                 (gethash "summary" inspection-result))))
            (setf (gethash "content" inspection-result) (text-content summary))
            inspection-result)))))

;;; ---------------------------------------------------------------------------
;;; worker/set-project-root
;;; ---------------------------------------------------------------------------

(defun %handle-set-project-root (params)
  "Set the worker's project root directory and change the working
directory.  Returns a success payload."
  (let ((path (gethash "path" params)))
    (unless path
      (error "path is required"))
    (let ((dir-path (uiop/pathname:ensure-directory-pathname path)))
      (unless (uiop/filesystem:directory-exists-p dir-path)
        (error "Directory does not exist: ~A" path))
      (setf *project-root* dir-path)
      (uiop/os:chdir dir-path)
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
  (register-method server "worker/set-project-root" #'%handle-set-project-root)
  (log-event :info "worker.handlers.registered" "count" 8)
  server)
