;;;; src/tools/response-builders.lisp
;;;;
;;;; Shared response builders used by both the MCP tool wrappers
;;;; (inline execution path) and the worker handlers.  Centralizes
;;;; the response structure so changes only need to happen in one place.

(defpackage #:cl-mcp/src/tools/response-builders
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content)
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p #:register-object)
  (:import-from #:cl-mcp/src/inspect
                #:generate-result-preview)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:build-eval-response
           #:build-load-system-response
           #:build-run-tests-response))

(in-package #:cl-mcp/src/tools/response-builders)

(defun %build-error-context-ht (error-context)
  "Build a hash-table from an error-context plist for JSON serialization.
ERROR-CONTEXT is a plist with keys :condition-type, :message, :restarts,
and :frames as returned by repl-eval."
  (make-ht
   "condition_type" (sanitize-for-json (getf error-context :condition-type))
   "message" (sanitize-for-json (getf error-context :message))
   "restarts"
   (mapcar (lambda (r)
             (make-ht "name" (sanitize-for-json (getf r :name))
                      "description" (sanitize-for-json (getf r :description))))
           (getf error-context :restarts))
   "frames"
   (mapcar (lambda (f)
             (make-ht
              "index" (getf f :index)
              "function" (sanitize-for-json (getf f :function))
              "source_file" (getf f :source-file)
              "source_line" (getf f :source-line)
              "locals"
              (mapcar (lambda (l)
                        (let ((lht (make-ht
                                    "name" (sanitize-for-json (getf l :name))
                                    "value" (sanitize-for-json (getf l :value)))))
                          (when (getf l :object-id)
                            (setf (gethash "object_id" lht) (getf l :object-id)))
                          (when (getf l :preview)
                            (setf (gethash "preview" lht) (getf l :preview)))
                          lht))
                      (getf f :locals))))
           (getf error-context :frames))))

(defun build-eval-response (printed raw-value stdout stderr error-context
                            &key include-result-preview
                                 (preview-max-depth 1)
                                 (preview-max-elements 8))
  "Build the standard repl-eval response hash-table.
Called by both the inline tool path and the worker handler.
Returns a hash-table with content, stdout, stderr, and optional
result_object_id, result_preview, and error_context."
  (let ((ht (make-ht "content" (text-content printed)
                     "stdout" stdout
                     "stderr" stderr)))
    (when (and (null error-context) (inspectable-p raw-value))
      (if include-result-preview
          (let ((preview (generate-result-preview
                          raw-value
                          :max-depth preview-max-depth
                          :max-elements preview-max-elements)))
            (setf (gethash "result_object_id" ht) (gethash "id" preview))
            (setf (gethash "result_preview" ht) preview))
          (let ((object-id (register-object raw-value)))
            (when object-id
              (setf (gethash "result_object_id" ht) object-id)))))
    (when error-context
      (setf (gethash "error_context" ht)
            (%build-error-context-ht error-context)))
    ht))

(defun build-load-system-response (system ht)
  "Build the standard load-system response with summary text.
HT is the hash-table returned by load-system core.  Adds a content
key with a human-readable summary and returns the same HT."
  (let* ((status (gethash "status" ht))
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
    ht))

(defun build-run-tests-response (test-result)
  "Build the standard run-tests response with summary text.
TEST-RESULT is the hash-table returned by run-tests core.
Returns a new hash-table with content, counts, and failure details."
  (let* ((passed (gethash "passed" test-result 0))
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
      response)))
