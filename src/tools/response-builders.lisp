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
                #:generate-result-preview
                #:format-inspect-elements)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/repl-core
                #:*default-max-output-length*)
  (:export #:build-eval-response
           #:build-load-system-response
           #:build-run-tests-response
           #:build-code-find-response
           #:build-code-describe-response
           #:build-code-find-references-response
           #:build-inspect-response))

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
                                 (preview-max-elements 8)
                                 max-output-length)
  "Build the standard repl-eval response hash-table.
Called by both the inline tool path and the worker handler.
Returns a hash-table with content, stdout, stderr, and optional
result_object_id, result_preview, and error_context.
The content text includes stdout/stderr/error-context/object-id
so that MCP clients rendering only content[].text still see them."
  (let ((ht (make-ht "stdout" stdout "stderr" stderr))
        (object-id nil)
        (effective-limit (or max-output-length *default-max-output-length*)))
    ;; Determine object-id (needed for text enrichment below)
    (when (and (null error-context) (inspectable-p raw-value))
      (if include-result-preview
          (handler-case
              (let ((preview (generate-result-preview
                              raw-value
                              :max-depth preview-max-depth
                              :max-elements preview-max-elements)))
                (setf object-id (gethash "id" preview))
                (setf (gethash "result_object_id" ht) object-id)
                (setf (gethash "result_preview" ht) preview))
            (serious-condition ()
              ;; Fall back to simple registration without preview
              (let ((oid (register-object raw-value)))
                (when oid
                  (setf object-id oid)
                  (setf (gethash "result_object_id" ht) oid)))))
          (let ((oid (register-object raw-value)))
            (when oid
              (setf object-id oid)
              (setf (gethash "result_object_id" ht) oid)))))
    (when error-context
      (setf (gethash "error_context" ht)
            (%build-error-context-ht error-context)))
    ;; Build enriched text for content[].text so MCP clients see everything
    (let ((enriched
            (with-output-to-string (s)
              (write-string printed s)
              (when object-id
                (format s "~&[object-id: ~A]" object-id))
              (when (and stdout (plusp (length stdout)))
                (format s "~&~%;; stdout~%~A" stdout))
              (when (and stderr (plusp (length stderr)))
                (format s "~&~%;; stderr~%~A" stderr))
              (when error-context
                (let ((ctype (sanitize-for-json
                              (getf error-context :condition-type)))
                      (msg (sanitize-for-json
                            (getf error-context :message)))
                      (restarts (getf error-context :restarts)))
                  (format s "~&~%[~A] ~A" (or ctype "ERROR") (or msg ""))
                  (when restarts
                    (format s "~&Restarts: ~{~A~^, ~}"
                            (mapcar (lambda (r)
                                      (sanitize-for-json (getf r :name)))
                                    restarts))))))))
      (setf (gethash "content" ht)
            (text-content
             (if (> (length enriched) effective-limit)
                 (concatenate 'string
                              (subseq enriched 0 effective-limit)
                              "...(truncated)")
                 enriched))))
    ht))

(defun build-load-system-response (system ht)
  "Build the standard load-system response with summary text.
HT is the hash-table returned by load-system core.  Adds a content
key with a human-readable summary and returns the same HT.
When warnings were captured, includes the warning text in the summary
(truncated at ~2KB) so MCP clients rendering only content[].text can
still see what was warned about."
  (let* ((status (gethash "status" ht))
         (summary
           (with-output-to-string (s)
             (cond
               ((string= status "loaded")
                (format s "System ~A loaded successfully in ~Dms"
                        system (gethash "duration_ms" ht))
                (let ((wc (gethash "warnings" ht 0))
                      (wd (gethash "warning_details" ht)))
                  (when (plusp wc)
                    (format s " (~D warning~:P)" wc)
                    (when (and (stringp wd) (plusp (length wd)))
                      (let* ((limit 2048)
                             (truncated-p (> (length wd) limit))
                             (body (if truncated-p
                                       (concatenate 'string
                                                    (subseq wd 0 limit)
                                                    (format nil
                                                            "~%... [~D more characters truncated]"
                                                            (- (length wd) limit)))
                                       wd)))
                        (format s "~%~A" (string-right-trim '(#\Newline) body)))))))
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
Returns a new hash-table with content, counts, and failure details.
Includes rich failure details (description, form, values) and debug output
in the summary text so MCP clients rendering only content[].text see them.
Raw stdout/stderr are kept in structured fields only (not in content text)."
  (let* ((passed (gethash "passed" test-result 0))
         (failed (gethash "failed" test-result 0))
         (pending (gethash "pending" test-result 0))
         (framework-name (or (gethash "framework" test-result) "unknown"))
         (duration (gethash "duration_ms" test-result 0))
         (failed-tests (gethash "failed_tests" test-result))
         (failed-tests-vector
          (if (vectorp failed-tests)
              failed-tests
              (coerce (or failed-tests 'nil) 'vector)))
         (debug-output-str (gethash "debug_output" test-result))
         (summary
          (with-output-to-string (s)
            (format s "~A~%"
                    (if (zerop failed)
                        "✓ PASS"
                        "✗ FAIL"))
            (format s "Passed: ~D, Failed: ~D~@[, Pending: ~D~]~%" passed
                    failed (when (plusp pending) pending))
            (format s "Duration: ~Dms~%" duration)
            (when (plusp (length failed-tests-vector))
              (format s "~%Failures:~%")
              (loop for fail across failed-tests-vector
                    for i from 1
                    do (format s "  ~D. ~A~%" i (gethash "test_name" fail))
                       (when (gethash "description" fail)
                         (format s "     ~A~%" (gethash "description" fail)))
                       (when (gethash "form" fail)
                         (format s "     Form: ~A~%" (gethash "form" fail)))
                       (when (gethash "values" fail)
                         (let ((vals (gethash "values" fail)))
                           (when (plusp (length vals))
                             (format s "     Got: ~{~A~^, ~}~%"
                                     (coerce vals 'list)))))
                       (when (gethash "reason" fail)
                         (format s "     Reason: ~A~%"
                                 (gethash "reason" fail)))))
            (when (and debug-output-str (plusp (length debug-output-str)))
              (format s "~%;; debug output~%~A" debug-output-str)))))
    (let ((response
           (make-ht "content" (text-content summary) "passed" passed "failed"
                    failed "pending" pending "framework" framework-name
                    "duration_ms" duration "failed_tests" failed-tests-vector)))
      (dolist (field '("success" "stdout" "stderr" "debug_output" "passed_tests"))
        (multiple-value-bind (value presentp)
            (gethash field test-result)
          (when presentp (setf (gethash field response) value))))
      response)))

(defun build-code-find-response (symbol path line)
  "Build the standard code-find response hash-table.
PATH is the source file path, LINE the line number.  When PATH is
NIL the symbol was not found and an isError payload is returned."
  (if path
      (make-ht "path" path
               "line" line
               "content" (text-content
                          (format nil "~A defined in ~A at line ~D"
                                  symbol path line)))
      (make-ht "isError" t
               "content" (text-content
                          (format nil "Definition not found for ~A"
                                  symbol)))))

(defun build-code-describe-response (name type arglist doc path line)
  "Build the standard code-describe response hash-table."
  (make-ht "name" name
           "type" type
           "arglist" arglist
           "documentation" doc
           "path" path
           "line" line
           "content" (text-content
                      (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]~@[~%Defined at ~A:~D~]"
                              name type arglist doc path line))))

(defun build-code-find-references-response (symbol refs count project-only)
  "Build the standard code-find-references response hash-table.
Summary text shows each reference as \"<path> (used in <caller>) [type]\"
so users see the enclosing function name instead of a potentially
misleading line snippet. The raw path, line, type, caller, and context
fields remain in the structured \"refs\" payload for programmatic use."
  (let* ((summary-lines
           (map 'list
                (lambda (h)
                  (let ((path (gethash "path" h))
                        (caller (gethash "caller" h))
                        (type (gethash "type" h))
                        (line (gethash "line" h)))
                    (cond
                      ((and caller (plusp (length caller)))
                       (format nil "~A (used in ~A) [~A]" path caller type))
                      (t
                       (format nil "~A:~A [~A]" path line type)))))
                refs))
         (summary (if summary-lines
                      (format nil "~{~A~%~}" summary-lines)
                      "")))
    (make-ht "refs" refs
             "count" count
             "symbol" symbol
             "project_only" project-only
             "content" (text-content summary))))

(defun build-inspect-response (inspection-result)
  "Build the standard inspect-object response hash-table.
INSPECTION-RESULT is the hash-table from inspect-object-by-id.
Returns a response with content added, or an isError payload."
  (if (gethash "error" inspection-result)
      (make-ht "isError" t
               "content" (text-content
                          (gethash "message" inspection-result)))
      (progn
        (setf (gethash "content" inspection-result)
              (text-content (format-inspect-elements inspection-result)))
        inspection-result)))
