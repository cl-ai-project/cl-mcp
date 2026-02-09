;;;; src/frame-inspector.lisp
;;;;
;;;; Structured error context capture with SBCL frame inspection.

(defpackage #:cl-mcp/src/frame-inspector
  (:use #:cl)
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p
                #:register-object)
  (:import-from #:cl-mcp/src/utils/printing
                #:safe-prin1)
  (:import-from #:cl-mcp/src/inspect
                #:generate-result-preview)
  (:export #:capture-error-context))


(in-package #:cl-mcp/src/frame-inspector)

(defun %collect-restarts ()
  "Return list of available restarts with names and descriptions."
  (loop for restart in (compute-restarts)
        collect (list :name (string (restart-name restart))
                      :description (or (ignore-errors
                                         (princ-to-string restart))
                                       ""))))

#+sbcl
(defun %frame-locals (frame print-level print-length
                      &key include-preview preview-max-depth preview-max-elements)
  "Extract local variable names and values from FRAME.
Non-primitive values are registered in the object registry for drill-down inspection.
When INCLUDE-PREVIEW is true, generates structural preview for non-primitive locals."
  (let ((debug-fun (sb-di:frame-debug-fun frame))
        (locals '()))
    (handler-case
        (sb-di:do-debug-fun-vars (var debug-fun)
          (when (eq (sb-di:debug-var-validity var (sb-di:frame-code-location frame))
                    :valid)
            (handler-case
                (let* ((sym (sb-di:debug-var-symbol var))
                       (val (sb-di:debug-var-value var frame)))
                  (if (inspectable-p val)
                      ;; Non-primitive: generate preview or just register
                      (if include-preview
                          (let ((preview (generate-result-preview
                                          val
                                          :max-depth (or preview-max-depth 1)
                                          :max-elements (or preview-max-elements 5))))
                            (push (list :name (symbol-name sym)
                                        :value (safe-prin1 val
                                                           :level print-level
                                                           :length print-length)
                                        :object-id (gethash "id" preview)
                                        :preview preview)
                                  locals))
                          (let ((object-id (register-object val)))
                            (push (list :name (symbol-name sym)
                                        :value (safe-prin1 val
                                                           :level print-level
                                                           :length print-length)
                                        :object-id object-id)
                                  locals)))
                      ;; Primitive: just include name and value
                      (push (list :name (symbol-name sym)
                                  :value (safe-prin1 val
                                                     :level print-level
                                                     :length print-length))
                            locals)))
              (error () nil))))
      (error () nil))
    (nreverse locals)))

#+sbcl
(defun %frame-source-location (frame)
  "Extract source file and line from FRAME, or NIL if unavailable."
  (handler-case
      (let ((code-location (sb-di:frame-code-location frame)))
        (when code-location
          (let ((debug-source (sb-di:code-location-debug-source code-location)))
            (when debug-source
              (let ((namestring (sb-di:debug-source-namestring debug-source)))
                (when namestring
                  (list :file namestring
                        :line (ignore-errors
                                (sb-di:code-location-toplevel-form-offset code-location)))))))))
    (error () nil)))

#+sbcl
(defun %frame-function-name (frame)
  "Extract function name from FRAME as a string."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun frame))
             (name (sb-di:debug-fun-name debug-fun)))
        (if name
            (prin1-to-string name)
            "<anonymous>"))
    (error () "<unknown>")))

(defparameter *internal-package-prefixes*
  '(;; CL-MCP implementation (not tests - those are user code to debug)
    "CL-MCP/SRC"
    ;; SBCL internal packages (specific to avoid false positives)
    "SB-KERNEL" "SB-INT" "SB-IMPL" "SB-DEBUG" "SB-C" "SB-DI" "SB-VM"
    "SB-EXT" "SB-SYS" "SB-PCL" "SB-MOP" "SB-ALIEN" "SB-THREAD"
    "SB-INTROSPECT" "SB-PROFILE" "SB-LOOP" "SB-PRETTY" "SB-FORMAT"
    "SB-DISASSEM" "SB-BIGNUM" "SB-WALKER" "SB-CLTL2" "SB-SEQUENCE"
    "SB-GRAY" "SB-UNICODE" "SB-COVER" "SB-BSD-SOCKETS" "SB-POSIX"
    ;; Other infrastructure
    "UIOP" "ASDF" "BORDEAUX-THREADS" "HUNCHENTOOT" "USOCKET")
  "Package prefixes that indicate internal/infrastructure frames.
Uses CL-MCP/SRC (not just CL-MCP) to allow debugging of test code.")

(defun %internal-frame-p (function-name)
  "Return T if FUNCTION-NAME appears to be an internal/infrastructure frame.
Internal frames include:
- Frames from CL-MCP, SBCL internals (SB-KERNEL:, SB-INT:, etc.), ASDF, UIOP
- Anonymous functions like (FLET ...), (LAMBDA ...), (LABELS ...)
- Standard CL functions like ERROR, SIGNAL, EVAL, etc."
  (or
   ;; Anonymous/compiler-generated frames start with (
   (and (> (length function-name) 0)
        (char= (char function-name 0) #\())
   ;; Check for internal package prefixes with proper boundary
   ;; Prefix must be followed by : or / (package delimiters) or be exact match
   (some (lambda (prefix)
           (let ((prefix-len (length prefix)))
             (and (>= (length function-name) prefix-len)
                  (string-equal function-name prefix :end1 prefix-len)
                  ;; Verify boundary: next char is : or / or end of string
                  (or (= (length function-name) prefix-len)
                      (char= (char function-name prefix-len) #\:)
                      (char= (char function-name prefix-len) #\/)))))
         *internal-package-prefixes*)
   ;; Standard error signaling and evaluation functions (unqualified)
   (member function-name '("ERROR" "SIGNAL" "CERROR" "WARN"
                           "INVOKE-DEBUGGER" "BREAK" "EVAL")
           :test #'string-equal)))

#+sbcl
(defun %collect-frames (max-frames print-level print-length
                        &key locals-preview-frames preview-max-depth
                             preview-max-elements locals-preview-skip-internal)
  "Walk stack and collect frame information using SBCL backtrace API.
LOCALS-PREVIEW-FRAMES controls how many top frames get local variable previews.
When LOCALS-PREVIEW-SKIP-INTERNAL is true, only USER frames are counted and
receive previews; internal frames are skipped entirely for preview purposes.
PREVIEW-MAX-DEPTH and PREVIEW-MAX-ELEMENTS control preview generation."
  (let ((frames '())
        (index 0)
        (user-frame-index 0)
        (preview-frames (or locals-preview-frames 0))
        (skip-internal (and locals-preview-skip-internal t)))
    (handler-case
        (let ((map-fn (ignore-errors
                        (fdefinition
                         (find-symbol "MAP-BACKTRACE" "SB-DEBUG")))))
          (when map-fn
            (funcall map-fn
                     (lambda (frame)
                       (when (< index max-frames)
                         (let* ((function-name (%frame-function-name frame))
                                (source-loc (%frame-source-location frame))
                                (is-internal (%internal-frame-p function-name))
                                ;; When skip-internal: only user frames get preview
                                ;; When not skip-internal: count all frames
                                (include-preview
                                 (if skip-internal
                                     (and (not is-internal)
                                          (< user-frame-index preview-frames))
                                     (< index preview-frames))))
                           (push (list :index index
                                       :function function-name
                                       :source-file (getf source-loc :file)
                                       :source-line (getf source-loc :line)
                                       :locals
                                       (%frame-locals frame print-level
                                                      print-length
                                                      :include-preview
                                                      include-preview
                                                      :preview-max-depth
                                                      preview-max-depth
                                                      :preview-max-elements
                                                      preview-max-elements))
                                 frames)
                           (unless is-internal
                             (incf user-frame-index)))
                         (incf index))))))
      (error () nil))
    (nreverse frames)))

#-sbcl
(defun %collect-frames (max-frames print-level print-length
                        &key locals-preview-frames preview-max-depth preview-max-elements
                             locals-preview-skip-internal)
  "Fallback for non-SBCL: return empty frame list."
  (declare (ignore max-frames print-level print-length
                   locals-preview-frames preview-max-depth preview-max-elements
                   locals-preview-skip-internal))
  nil)

(defun capture-error-context (condition &key (max-frames 20)
                                             (print-level 3)
                                             (print-length 10)
                                             (locals-preview-frames 0)
                                             (preview-max-depth 1)
                                             (preview-max-elements 5)
                                             (locals-preview-skip-internal t))
  "Capture structured error context including frames and locals.

LOCALS-PREVIEW-FRAMES: Number of top frames to include local variable previews (default: 0).
  Set to a positive integer to automatically expand local variables in the top N frames.
  This helps agents immediately see the state of variables without extra inspect-object calls.
LOCALS-PREVIEW-SKIP-INTERNAL: When true (default), skip internal frames when counting for preview.
  Internal frames include CL-MCP, SBCL internals (SB-KERNEL, etc.), ASDF, UIOP, anonymous functions.
  This ensures user code frames get previews even when buried under infrastructure frames.
PREVIEW-MAX-DEPTH: Max nesting depth for local previews (default: 1).
PREVIEW-MAX-ELEMENTS: Max elements per collection in local previews (default: 5).

Returns a plist with:
  :error t
  :condition-type - string name of condition type
  :message - formatted error message
  :restarts - list of (:name STRING :description STRING)
  :frames - list of frame plists (SBCL only, NIL on other implementations)
            When LOCALS-PREVIEW-FRAMES > 0, locals in top frames include :preview field."
  (list :error t
        :condition-type (prin1-to-string (type-of condition))
        :message (handler-case
                     (princ-to-string condition)
                   (error () "<error formatting condition>"))
        :restarts (%collect-restarts)
        :frames (%collect-frames max-frames print-level print-length
                                 :locals-preview-frames locals-preview-frames
                                 :preview-max-depth preview-max-depth
                                 :preview-max-elements preview-max-elements
                                 :locals-preview-skip-internal locals-preview-skip-internal)))
