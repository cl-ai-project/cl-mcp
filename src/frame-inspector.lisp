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
  (:import-from #:cl-mcp/src/code-core
                #:%offset->line)
  (:export #:capture-error-context
           #:%internal-frame-p))


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
                (let ((sym (sb-di:debug-var-symbol var))
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
  "Extract source file and line from FRAME, or NIL if unavailable.
Uses the code-location's TLF offset and the debug-source start-positions
array to resolve the line of the enclosing top-level form.  Falls back
to the raw TLF offset when start-positions is unavailable.

NOTE: does NOT use sb-introspect FIND-DEFINITION-SOURCE — that returns
the function definition line which is misleading for backtraces (every
frame in the same function would show the defun line, not the execution
point)."
  (handler-case
      (let ((code-location (sb-di:frame-code-location frame)))
        (when code-location
          (let ((debug-source (sb-di:code-location-debug-source code-location)))
            (when debug-source
              (let ((namestring (sb-di:debug-source-namestring debug-source)))
                (when namestring
                  (let* ((tlf-offset
                           (ignore-errors
                             (sb-di:code-location-toplevel-form-offset
                              code-location)))
                         (start-positions
                           (ignore-errors
                             (sb-c::debug-source-start-positions
                              debug-source)))
                         (tlf-char
                           (when (and start-positions tlf-offset
                                      (< tlf-offset (length start-positions)))
                             (aref start-positions tlf-offset)))
                         (line
                           (if (and tlf-char
                                    (ignore-errors (probe-file namestring)))
                               (%offset->line namestring tlf-char)
                               tlf-offset)))
                    (list :file namestring :line line))))))))
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

(defparameter *standard-clos-specializers*
  '("T" "STANDARD-OBJECT" "STRUCTURE-OBJECT" "CONDITION"
    "FUNCALLABLE-STANDARD-OBJECT" "STANDARD-CLASS" "BUILT-IN-CLASS"
    "CLASS" "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD" "METHOD"
    "FUNCTION")
  "Type names used as specializers in SBCL's default CLOS methods.
When a FAST-METHOD/SLOW-METHOD wrapper has an unqualified generic function
name and all specializers are from this list, it is an SBCL default method
rather than user code.")

(defun %internal-frame-p (function-name)
  "Return T if FUNCTION-NAME appears to be an internal/infrastructure frame.
Internal frames include:
- Frames from CL-MCP, SBCL internals (SB-KERNEL:, SB-INT:, etc.), ASDF, UIOP
- Anonymous functions like (FLET ...), (LAMBDA ...), (LABELS ...)
- Standard CL functions like ERROR, SIGNAL, EVAL, etc.
- CLOS method wrappers (SB-PCL::FAST-METHOD <name> ...) are exempt only
  when the inner method name belongs to user code with non-standard specializers.
- Default CLOS methods with only standard specializers (T, STANDARD-OBJECT, etc.)
  are treated as internal even when the generic name is unqualified.
- (SETF ...) frames are checked by extracting the inner symbol name and
  applying the package-prefix filter."
  (flet ((%prefix-internal-p (name)
           "Check NAME against *internal-package-prefixes*."
           (some (lambda (prefix)
                   (let ((prefix-len (length prefix)))
                     (and (>= (length name) prefix-len)
                          (string-equal name prefix :end1 prefix-len)
                          (or (= (length name) prefix-len)
                              (char= (char name prefix-len) #\:)
                              (char= (char name prefix-len) #\/)))))
                 *internal-package-prefixes*))
         (%extract-method-name (fn-name keyword)
           "Extract method name from (SB-PCL::FAST-METHOD name ...) form."
           (let ((pos (search keyword fn-name)))
             (when pos
               (let* ((after (+ pos (length keyword)))
                      (start (position-if-not
                              (lambda (c) (char= c #\Space))
                              fn-name :start after)))
                 (when start
                   (if (char= (char fn-name start) #\()
                       ;; (SETF name) form — extract inner symbol
                       (let* ((setf-pos (search "SETF " fn-name
                                                :start2 start))
                              (ns (when setf-pos
                                    (position-if-not
                                     (lambda (c) (char= c #\Space))
                                     fn-name :start (+ setf-pos 5))))
                              (ne (when ns
                                    (or (position #\) fn-name :start ns)
                                        (length fn-name)))))
                         (when (and ns (> ne ns))
                           (subseq fn-name ns ne)))
                       ;; Plain name — delimited by space or paren
                       (let ((end (or (position #\Space fn-name :start start)
                                      (position #\( fn-name :start start)
                                      (position #\) fn-name :start start)
                                      (length fn-name))))
                         (subseq fn-name start end))))))))
         (%default-method-wrapper-p (fn-name method-name)
           "True when a FAST/SLOW-METHOD frame is an SBCL default method.
Checks that the generic function name is unqualified and all specializer
types are standard CL/MOP types (T, STANDARD-OBJECT, etc.)."
           (and (not (find #\: method-name))
                (let* ((name-pos (search method-name fn-name
                                        :test #'char-equal))
                       (search-start (when name-pos
                                       (+ name-pos (length method-name))))
                       (spec-open (when search-start
                                    (position #\( fn-name
                                              :start search-start)))
                       (spec-close (when spec-open
                                     (position #\) fn-name
                                               :start (1+ spec-open)))))
                  (when (and spec-open spec-close (< spec-open spec-close))
                    (let ((spec-text (subseq fn-name
                                            (1+ spec-open) spec-close)))
                      (loop with len = (length spec-text)
                            with start = 0
                            while (< start len)
                            for ts = (position-if-not
                                      (lambda (c) (char= c #\Space))
                                      spec-text :start start)
                            while ts
                            for te = (or (position #\Space spec-text
                                                   :start ts)
                                         len)
                            for token = (subseq spec-text ts te)
                            always (member token
                                           *standard-clos-specializers*
                                           :test #'string-equal)
                            do (setf start te))))))))
    (or
     ;; Anonymous/compiler-generated frames start with (
     (and (> (length function-name) 0)
          (char= (char function-name 0) #\()
          ;; Exempt CLOS method wrappers only if inner method is user code
          (let ((method-name
                  (or (%extract-method-name function-name "FAST-METHOD")
                      (%extract-method-name function-name "SLOW-METHOD"))))
            (if method-name
                ;; It's a method wrapper — internal if the inner method
                ;; name has an internal package prefix, OR if all
                ;; specializers are standard types (SBCL default method).
                (or (%prefix-internal-p method-name)
                    (%default-method-wrapper-p function-name method-name))
                ;; Not a method wrapper; exempt (SETF ...) with user symbols
                (not (and (>= (length function-name) 6)
                          (string-equal function-name "(SETF " :end1 6)
                          (not (%prefix-internal-p
                                (string-trim '(#\) #\Space)
                                             (subseq function-name 6)))))))))
     ;; Check for internal package prefixes with proper boundary
     (%prefix-internal-p function-name)
     ;; Standard error signaling and evaluation functions (unqualified)
     (member function-name
             '("ERROR" "SIGNAL" "CERROR" "WARN" "INVOKE-DEBUGGER" "BREAK"
               "EVAL")
             :test #'string-equal))))

#+sbcl
(defun %collect-frames (max-frames print-level print-length
                        &key locals-preview-frames preview-max-depth
                             preview-max-elements locals-preview-skip-internal
                             filter-internal)
  "Walk stack and collect frame information using SBCL backtrace API.
LOCALS-PREVIEW-FRAMES controls how many top frames get local variable previews.
When LOCALS-PREVIEW-SKIP-INTERNAL is true, only USER frames are counted and
receive previews; internal frames are skipped entirely for preview purposes.
When FILTER-INTERNAL is true, internal frames are excluded from the result
entirely, keeping backtraces focused on user code.
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
                                (is-internal (%internal-frame-p function-name)))
                           (unless (and filter-internal is-internal)
                             (let ((include-preview
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
                                     frames))
                             (unless is-internal
                               (incf user-frame-index))
                             (incf index))))))))
      (error () nil))
    (nreverse frames)))

#-sbcl
(defun %collect-frames (max-frames print-level print-length
                        &key locals-preview-frames preview-max-depth
                             preview-max-elements locals-preview-skip-internal
                             filter-internal)
  "Fallback for non-SBCL: return empty frame list."
  (declare (ignore max-frames print-level print-length
                   locals-preview-frames preview-max-depth preview-max-elements
                   locals-preview-skip-internal filter-internal))
  nil)

(defun capture-error-context
    (condition
     &key (max-frames 20) (print-level 3) (print-length 10)
     (locals-preview-frames 0) (preview-max-depth 1)
     (preview-max-elements 5) (locals-preview-skip-internal t)
     (filter-internal nil))
  "Capture structured error context including frames and locals.

LOCALS-PREVIEW-FRAMES: Number of top frames to include local variable previews (default: 0).
  Set to a positive integer to automatically expand local variables in the top N frames.
  This helps agents immediately see the state of variables without extra inspect-object calls.
LOCALS-PREVIEW-SKIP-INTERNAL: When true (default), skip internal frames when counting for preview.
  Internal frames include CL-MCP, SBCL internals (SB-KERNEL, etc.), ASDF, UIOP, anonymous functions.
  This ensures user code frames get previews even when buried under infrastructure frames.
FILTER-INTERNAL: When true (default), exclude internal frames from the result entirely.
  This keeps backtraces focused on user code by omitting CL-MCP, SBCL, and other
  infrastructure frames.
PREVIEW-MAX-DEPTH: Max nesting depth for local previews (default: 1).
PREVIEW-MAX-ELEMENTS: Max elements per collection in local previews (default: 5).

Returns a plist with:
  :error t
  :condition-type - string name of condition type
  :message - formatted error message
  :restarts - list of (:name STRING :description STRING)
  :frames - list of frame plists (SBCL only, NIL on other implementations)
            When LOCALS-PREVIEW-FRAMES > 0, locals in top frames include :preview field."
  (list :error t :condition-type (prin1-to-string (type-of condition)) :message
        (handler-case (princ-to-string condition)
                      (error nil "<error formatting condition>"))
        :restarts (%collect-restarts) :frames
        (%collect-frames max-frames print-level print-length
                         :locals-preview-frames locals-preview-frames
                         :preview-max-depth preview-max-depth
                         :preview-max-elements preview-max-elements
                         :locals-preview-skip-internal
                         locals-preview-skip-internal
                         :filter-internal filter-internal)))
