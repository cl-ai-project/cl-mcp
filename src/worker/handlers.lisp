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
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p
                #:register-object)
  (:import-from #:cl-mcp/src/inspect
                #:generate-result-preview
                #:inspect-object-by-id)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:text-content)
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
      (let ((ht (make-ht "content" (text-content printed)
                         "stdout" stdout
                         "stderr" stderr)))
        ;; Object registry and preview for successful non-primitive results
        (when (and (null error-context) (inspectable-p raw-value))
          (if include-result-preview
              (let ((preview
                      (generate-result-preview raw-value
                                               :max-depth (or preview-max-depth 1)
                                               :max-elements (or preview-max-elements 8))))
                (setf (gethash "result_object_id" ht) (gethash "id" preview))
                (setf (gethash "result_preview" ht) preview))
              (let ((object-id (register-object raw-value)))
                (when object-id
                  (setf (gethash "result_object_id" ht) object-id)))))
        ;; Error context
        (when error-context
          (setf (gethash "error_context" ht)
                (make-ht
                 "condition_type"
                 (sanitize-for-json (getf error-context :condition-type))
                 "message"
                 (sanitize-for-json (getf error-context :message))
                 "restarts"
                 (mapcar
                  (lambda (r)
                    (make-ht "name" (sanitize-for-json (getf r :name))
                             "description"
                             (sanitize-for-json (getf r :description))))
                  (getf error-context :restarts))
                 "frames"
                 (mapcar
                  (lambda (f)
                    (make-ht
                     "index" (getf f :index)
                     "function" (sanitize-for-json (getf f :function))
                     "source_file" (getf f :source-file)
                     "source_line" (getf f :source-line)
                     "locals"
                     (mapcar
                      (lambda (l)
                        (let ((lht (make-ht
                                    "name" (sanitize-for-json (getf l :name))
                                    "value" (sanitize-for-json (getf l :value)))))
                          (when (getf l :object-id)
                            (setf (gethash "object_id" lht) (getf l :object-id)))
                          (when (getf l :preview)
                            (setf (gethash "preview" lht) (getf l :preview)))
                          lht))
                      (getf f :locals))))
                  (getf error-context :frames)))))
        ht))))

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
    (let* ((ht (load-system system
                            :force force
                            :clear-fasls clear-fasls
                            :timeout-seconds (or timeout-seconds 120)))
           (status (gethash "status" ht))
           (summary
             (with-output-to-string (s)
               (cond
                 ((string= status "loaded")
                  (format s "System ~A loaded successfully in ~Dms"
                          system (gethash "duration_ms" ht))
                  (let ((wc (gethash "warnings" ht 0)))
                    (when (plusp wc)
                      (format s " (~D warning~:P)" wc))))
                 ((string= status "timeout")
                  (format s "~A" (gethash "message" ht)))
                 ((string= status "error")
                  (format s "Error loading ~A: ~A"
                          system (gethash "message" ht)))))))
      (setf (gethash "content" ht) (text-content summary))
      ht)))

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
    (let* ((test-result (run-tests system
                                   :framework framework
                                   :test test
                                   :tests tests))
           (passed (gethash "passed" test-result 0))
           (failed (gethash "failed" test-result 0))
           (pending (gethash "pending" test-result 0))
           (framework-name (or (gethash "framework" test-result) "unknown"))
           (duration (gethash "duration_ms" test-result 0))
           (failed-tests (gethash "failed_tests" test-result))
           (failed-tests-vector (if (vectorp failed-tests)
                                    failed-tests
                                    (coerce (or failed-tests '()) 'vector)))
           (summary
             (with-output-to-string (s)
               (format s "~A~%"
                       (if (zerop failed) "✓ PASS" "✗ FAIL"))
               (format s "Passed: ~D, Failed: ~D~@[, Pending: ~D~]~%"
                       passed failed (when (plusp pending) pending))
               (format s "Duration: ~Dms~%" duration)
               (when (plusp (length failed-tests-vector))
                 (format s "~%Failures:~%")
                 (loop for fail across failed-tests-vector
                       for i from 1
                       do (format s "  ~D. ~A~%"
                                  i (gethash "test_name" fail))
                          (when (gethash "reason" fail)
                            (format s "     Reason: ~A~%"
                                    (gethash "reason" fail))))))))
      (let ((response (make-ht "content" (text-content summary)
                               "passed" passed
                               "failed" failed
                               "pending" pending
                               "framework" framework-name
                               "duration_ms" duration
                               "failed_tests" failed-tests-vector)))
        (dolist (field '("success" "stdout" "stderr" "passed_tests"))
          (multiple-value-bind (value presentp)
              (gethash field test-result)
            (when presentp
              (setf (gethash field response) value))))
        response))))

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
