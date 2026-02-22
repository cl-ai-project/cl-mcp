;;;; src/repl.lisp
;;;;
;;;; MCP tool definition for repl-eval. The pure evaluation logic lives
;;;; in repl-core.lisp; this file provides the tool wrapper that builds
;;;; the JSON response with object registry and preview generation.

(defpackage #:cl-mcp/src/repl
  (:use #:cl)
  (:import-from #:cl-mcp/src/repl-core
                #:repl-eval
                #:*default-eval-package*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-eval-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:repl-eval #:*default-eval-package*))

(in-package #:cl-mcp/src/repl)

(define-tool "repl-eval" :description
 "Evaluate Common Lisp forms and return the last value as printed text.
Use this for testing, inspection, debugging, or exploring runtime state.
Provide an existing package (e.g., CL-USER) and set print_level/print_length when needed.
WARNING: Definitions created here are TRANSIENT and lost on server restart.
To modify code permanently, you MUST use 'lisp-edit-form' or 'fs-write-file'
to save changes to files.

IMPORTANT: For loading ASDF systems, prefer the dedicated 'load-system' tool.
It handles staleness (force-reload), output suppression, and timeouts automatically.
Only use repl-eval with ql:quickload if you need non-standard load options.

When the result is a non-primitive object (not a number, string, symbol, or character),
the response includes:
- 'result_object_id': ID for use with 'inspect-object' for deeper drill-down
- 'result_preview': A lightweight structural preview (kind, type, elements, etc.)

The preview reduces round-trips by providing immediate insight into the result structure.
Use 'inspect-object' only when you need to drill deeper than the preview shows.

When an error occurs, 'error_context' includes stack frames with local variables.
Non-primitive locals include 'object_id' for drill-down via 'inspect-object'.
Set 'locals_preview_frames' > 0 to auto-expand local variable previews in top N frames,
providing immediate insight without extra inspect-object calls during debugging.
The 'locals_preview_skip_internal' parameter (default: true) skips internal frames
(CL-MCP, SBCL internals, ASDF, etc.) when counting frames for preview eligibility.
This ensures user code frames get previews even when buried under infrastructure.
NOTE: Local variable capture requires (declare (optimize (debug 3))) in the function.
SBCL's default optimization does not preserve locals for inspection."
 :args
 ((code :type :string :required t :description
   "Code string of one or more forms evaluated sequentially")
  (package :type :string :description
   "Existing package name (e.g., CL-USER); forms are read/evaluated there")
  (print-level :type :integer :json-name "print_level" :description
   "Integer to limit printed nesting depth (omit to print fully)")
  (print-length :type :integer :json-name "print_length" :description
   "Integer to limit printed list length (omit to print fully)")
  (timeout-seconds :type :number :json-name "timeout_seconds" :description
   "Seconds to wait before timing out evaluation")
  (max-output-length :type :integer :json-name "max_output_length" :description
   "Maximum characters for printed result/stdout/stderr")
  (safe-read :type :boolean :json-name "safe_read" :description
   "When true, disables #. reader evaluation for safety")
  (include-result-preview :type :boolean :json-name "include_result_preview"
   :default t :description
   "Include structural preview of non-primitive results (default: true)")
  (preview-max-depth :type :integer :json-name "preview_max_depth" :description
   "Max nesting depth for preview (default: 1)")
  (preview-max-elements :type :integer :json-name "preview_max_elements"
   :description "Max elements per collection in preview (default: 8)")
  (locals-preview-frames :type :integer :json-name "locals_preview_frames"
   :description
   "Number of top stack frames to include local variable previews on error (default: 0)")
  (locals-preview-max-depth :type :integer :json-name
   "locals_preview_max_depth" :description
   "Max nesting depth for local variable previews (default: 1)")
  (locals-preview-max-elements :type :integer :json-name
   "locals_preview_max_elements" :description
   "Max elements per collection in local variable previews (default: 5)")
  (locals-preview-skip-internal :type :boolean :json-name
   "locals_preview_skip_internal" :default t :description
   "Skip internal frames (CL-MCP, SBCL internals, ASDF, etc.) when counting for preview eligibility (default: true)"))
 :body
 (with-proxy-dispatch (id "worker/eval"
                         (make-ht "code" code
                                  "package" package
                                  "print_level" print-level
                                  "print_length" print-length
                                  "timeout_seconds" timeout-seconds
                                  "max_output_length" max-output-length
                                  "safe_read" safe-read
                                  "include_result_preview" include-result-preview
                                  "preview_max_depth" preview-max-depth
                                  "preview_max_elements" preview-max-elements
                                  "locals_preview_frames" locals-preview-frames
                                  "locals_preview_max_depth" locals-preview-max-depth
                                  "locals_preview_max_elements" locals-preview-max-elements
                                  "locals_preview_skip_internal" locals-preview-skip-internal))
   ;; Fallback: inline execution (default when *use-worker-pool* is nil)
   (multiple-value-bind (printed raw-value stdout stderr error-context)
       (repl-eval code :package (or package *package*) :print-level print-level
        :print-length print-length :timeout-seconds timeout-seconds
        :max-output-length max-output-length :safe-read safe-read
        :locals-preview-frames locals-preview-frames :locals-preview-max-depth
        locals-preview-max-depth :locals-preview-max-elements
        locals-preview-max-elements :locals-preview-skip-internal
        locals-preview-skip-internal)
     (result id
             (build-eval-response printed raw-value stdout stderr error-context
                                  :include-result-preview include-result-preview
                                  :preview-max-depth (or preview-max-depth 1)
                                  :preview-max-elements (or preview-max-elements 8))))))
