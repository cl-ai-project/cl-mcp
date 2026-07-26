;;;; src/tools/response-builders.lisp
;;;;
;;;; Shared response builders used by both the MCP tool wrappers
;;;; (inline execution path) and the worker handlers.  Centralizes
;;;; the response structure so changes only need to happen in one place.

(defpackage #:cl-mcp/src/tools/response-builders
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:json-bool)
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p #:register-object)
  (:import-from #:cl-mcp/src/inspect
                #:generate-result-preview
                #:format-inspect-elements)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  ;; No cycle: macroexpand-core imports only cl-mcp/src/utils/sanitize.
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-forms
                #:macroexpand-package-error
                #:*backquote-template-marker*
                #:*shadowed-binding-marker*)
  (:import-from #:cl-mcp/src/repl-core
                #:*default-max-output-length*)
  (:import-from #:cl-mcp/src/frame-inspector
                #:%internal-frame-p)
  (:export #:build-eval-response
           #:build-load-system-response
           #:build-run-tests-response
           #:build-code-find-response
           #:build-code-describe-response
           #:build-code-find-references-response
           #:build-inspect-response
           #:build-macroexpand-response
           #:expand-and-build-response))

(in-package #:cl-mcp/src/tools/response-builders)

(defun %build-error-context-ht (error-context)
  "Build a hash-table from an error-context plist for JSON serialization.
ERROR-CONTEXT is a plist with keys :condition-type, :message, :restarts,
and :frames as returned by repl-eval.
Internal/infrastructure frames (CL-MCP, SBCL internals, ASDF, etc.) are
filtered out so that the user-visible backtrace focuses on application code.
If filtering would remove ALL frames, the filter is skipped to preserve
useful debug information."
  (let* ((all-frames (getf error-context :frames))
         (user-frames
           (remove-if (lambda (f)
                        (%internal-frame-p (or (getf f :function) "")))
                      all-frames))
         ;; Fall back to all frames when filtering removes everything
         (display-frames (if user-frames user-frames all-frames)))
    (make-ht
     "condition_type" (sanitize-for-json (getf error-context :condition-type))
     "message" (sanitize-for-json (getf error-context :message))
     "restarts"
     (mapcar (lambda (r)
               (make-ht "name" (sanitize-for-json (getf r :name))
                        "description" (sanitize-for-json (getf r :description))))
             (getf error-context :restarts))
     "frames"
     (coerce
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
              display-frames)
      'vector))))

(defun build-eval-response
    (printed raw-value stdout stderr error-context
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
                      (restarts (getf error-context :restarts))
                      (frames (getf error-context :frames)))
                  (format s "~&~%[~A] ~A" (or ctype "ERROR") (or msg ""))
                  (when restarts
                    (format s "~&Restarts: ~{~A~^, ~}"
                            (mapcar (lambda (r)
                                      (sanitize-for-json (getf r :name)))
                                    restarts)))
                  ;; Show user frames, falling back to all frames when
                  ;; filtering removes everything
                  (let* ((user-frames
                           (and frames
                                (loop for frame in frames
                                      for fn = (or (getf frame :function) "")
                                      unless (%internal-frame-p fn)
                                        collect frame)))
                         (display-frames (or user-frames
                                             (and frames (subseq frames 0
                                                          (min 5 (length frames)))))))
                    (when display-frames
                      (format s "~&Backtrace:")
                      (loop for frame in display-frames
                            for shown from 1
                            do (format s "~&  ~A: ~A~@[ (~A~@[:~A~])~]"
                                       (or (getf frame :index) "?")
                                       (sanitize-for-json
                                        (or (getf frame :function) ""))
                                       (sanitize-for-json
                                        (getf frame :source-file))
                                       (getf frame :source-line))
                            until (>= shown 5)))))))))
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
                (let ((discovered (gethash "auto_discovered_asd" ht)))
                  (when discovered
                    (format s
                            "~%Auto-registered ~A (was not on ASDF search path)"
                            discovered)))
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
                        (format s "~%⚠ ~A" (string-right-trim '(#\Newline) body))
                      (when (search "also exports" wd)
                        (format s "~%~%Hint: package-variance warnings mean the running image has stale exports. ~
Use pool-kill-worker to get a fresh worker, then re-run load-system.")))))))
               ((string= status "timeout")
                (format s "~A" (gethash "message" ht)))
               ((string= status "error")
                (format s "Error loading ~A: ~A"
                        system (gethash "message" ht))
                (let ((co (gethash "compiler_output" ht)))
                  (when (and (stringp co) (plusp (length co)))
                    (let* ((limit 2048)
                           (truncated-p (> (length co) limit))
                           (body (if truncated-p
                                     (concatenate
                                      'string (subseq co 0 limit)
                                      (format nil
                                              "~%... [~D more characters truncated]"
                                              (- (length co) limit)))
                                     co)))
                      (format s "~%~%Compiler output:~%~A"
                              (string-right-trim '(#\Newline) body)))))
                (format s "~%~%Hint: the worker process may now have a broken package state. ~
Use pool-kill-worker to get a fresh worker, then retry load-system."))))))
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
                    (cond ((string= framework-name "load-error") "✗ LOAD FAILED")
                          ((string= framework-name "unresolved") "✗ UNRESOLVED")
                          ((string= framework-name "timeout") "✗ TIMEOUT")
                          ((zerop failed) "✓ PASS")
                          (t "✗ FAIL")))
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
                          (let ((on-disk (ignore-errors (probe-file path))))
                            (cond
                              ((and line on-disk)
                               (format nil "~A defined in ~A at line ~D"
                                       symbol path line))
                              (line
                               (format nil "~A defined in ~A at line ~D (source not on disk)"
                                       symbol path line))
                              (on-disk
                               (format nil "~A defined in ~A" symbol path))
                              (t
                               (format nil "~A defined in ~A (source not on disk)"
                                       symbol path))))))
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
                      (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]~@[~%Defined at ~A~@[:~D~]~]"
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
                      ((and caller (plusp (length caller))
                            (string/= caller "(lambda)"))
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

(defun %macroexpand-result->ht (result)
  "Convert one MACROEXPAND-FORMS plist into a JSON-ready hash-table."
  (make-ht "label" (getf result :label)
           "printed" (getf result :printed)
           "expanded" (json-bool (getf result :expanded-p))
           "steps" (getf result :steps)
           "steps_capped" (json-bool (getf result :steps-capped-p))
           "truncated" (json-bool (getf result :truncated-p))
           "error" (getf result :error)))

(defun %expansion-summary (result level)
  "Return the one-line status shown above an expanded RESULT at LEVEL.

Hitting the step limit must be loud: the form below it is still a macro
call, and rendering that as a finished expansion would mislead the caller
into thinking the macro bottomed out.  Level \"all\" reports no step
count because it walks the whole tree rather than stepping the head."
  (cond
    ((getf result :steps-capped-p)
     (format nil "STOPPED at the ~D-step expansion limit: the result below is ~
STILL A MACRO CALL, not a fully expanded form"
             (getf result :steps)))
    ((string-equal level "all")
     "expanded (full code walk)")
    (t
     (format nil "expanded in ~D step~:P" (getf result :steps)))))

(defun %format-macroexpand-text (results level package note sub-form-p)
  "Render RESULTS as the agent-facing content text.
Everything a caller needs must appear here: MCP clients display only
content[].text, so anything left in a sibling JSON field is invisible.

SUB-FORM-P is true when the request addressed nested calls with sub_form.
It is threaded in from the caller rather than inferred from the label.
The two label shapes do differ -- \"name (file line N)\" for a sub-form
match against \"type name (file line N)\" for a whole form -- but telling
them apart on that basis would misfire on any form_name containing a
space, such as \"(setf foo)\" or a defmethod written with specializers.

*SHADOWED-BINDING-MARKER* is explained on both the expanded and the NOT
EXPANDED path, with different wording: on the first the expansion shown
is the global definition and is not what the compiler sees, while on the
second there is nothing to expand at all."
  (with-output-to-string (stream)
    (format stream "lisp-macroexpand (level: ~A, package: ~A)~%"
            (or level "once")
            (or package "COMMON-LISP-USER"))
    (when note
      (format stream "~A~%" note))
    (loop for result in results
          for index from 1
          for label = (getf result :label)
          for shadowed = (and label (search *shadowed-binding-marker* label))
          do (format stream "~%[~D] ~A~%" index (or label "form"))
             (cond
               ((getf result :error)
                (format stream "ERROR: ~A~%" (getf result :error))
                ;; Said only on the failing path.  A template fragment with no
                ;; commas in it reads and expands perfectly well, so attaching
                ;; this to the label instead would be wrong for those.
                (when (and label (search *backquote-template-marker* label))
                  (format stream
                          "This match is a fragment of a backquote template: ~
its commas have no enclosing backquote, so the fragment cannot be read on ~
its own. Expand the enclosing macro instead.~%")))
               ((not (getf result :expanded-p))
                ;; A shadowed match gets its own diagnosis, replacing both
                ;; stock lines rather than joining them.  "Load its system"
                ;; is wrong -- an FLET/LABELS binding is a function and has
                ;; no expansion at any load state -- and the positional-blind
                ;; guess is exactly what the shadow detection already ruled
                ;; out, so printing them here would contradict this line.
                (if shadowed
                    (format stream
                            "NOT EXPANDED: an enclosing macrolet, ~
symbol-macrolet, flet or labels binds this name and no macro by that name is ~
defined in this image, so there is nothing to expand. An flet or labels ~
binding is a function, not a macro; a macrolet binding is a macro, but it is ~
not reachable from the null lexical environment expansion runs in.~%")
                    (progn
                      (format stream
                              "NOT EXPANDED: the head of this form has no macro ~
definition in this image. If you expected a macro, load its system with ~
'load-system' first.~%")
                      (when sub-form-p
                        (format stream
                                "sub_form matching is also positional-blind: this ~
may be a binding position -- the variable of a let binding pair, say -- rather ~
than a call. Binding pairs of macrolet, symbol-macrolet, flet and labels are ~
the detected cases, and are skipped before they get here.~%"))))
                (format stream "~A~%" (getf result :printed)))
               (t
                (format stream "~A~A~%"
                        (%expansion-summary result level)
                        (if (getf result :truncated-p) " (truncated)" ""))
                ;; Said on the SUCCESSFUL path, before the expansion rather
                ;; than after it: the expansion below is real output of a real
                ;; macro, and without this line it reads as the answer.  It is
                ;; the answer for the global definition only.
                (when shadowed
                  (format stream
                          "WARNING: an enclosing macrolet, symbol-macrolet, ~
flet or labels binds this name, so the expansion below came from the GLOBAL ~
definition and is NOT what the compiler sees here. Expansion always uses a ~
null lexical environment; the local definition cannot be reached.~%"))
                (format stream "~A~%" (getf result :printed)))))))

(defun build-macroexpand-response (results &key level package note sub-form-p)
  "Build the standard lisp-macroexpand response hash-table.
RESULTS is the list of plists returned by MACROEXPAND-FORMS.  NOTE is an
optional advisory line (for example, that the sub-form match list was
capped) that must be visible to the caller.  SUB-FORM-P says whether the
request came in through sub_form, which changes how a NOT EXPANDED result
is explained."
  (let ((payload (make-ht "content" (text-content
                                     (%format-macroexpand-text results level
                                                               package note
                                                               sub-form-p))
                          "level" (or level "once")
                          "package" package
                          "note" note
                          "count" (length results)
                          "expansions" (map 'vector #'%macroexpand-result->ht
                                            results))))
    (when (some (lambda (result) (getf result :error)) results)
      (setf (gethash "isError" payload) t))
    payload))

(defun expand-and-build-response (entries &key package level readtable
                                               print-level print-length
                                               max-output-length note
                                               sub-form-p)
  "Expand ENTRIES and build the lisp-macroexpand response payload.

Shared by the worker handler and the parent's inline fallback so the two
paths cannot produce different shapes.  An absent package short-circuits
to the minimal {content, isError} payload rather than a full response,
because there is nothing to report per entry.

SUB-FORM-P is a request-level flag, not a per-entry one: sub_form yields
only sub-form entries and its absence yields exactly one whole-form or
code entry, so the two never mix within a single call."
  (handler-case
      (build-macroexpand-response
       (macroexpand-forms entries
                          :package package
                          :level level
                          :readtable readtable
                          :print-level print-level
                          :print-length print-length
                          :max-output-length max-output-length)
       :level level :package package :note note :sub-form-p sub-form-p)
    (macroexpand-package-error (condition)
      (make-ht "content" (text-content (princ-to-string condition))
               "isError" t))))
