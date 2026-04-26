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
(defun %debug-source-start-positions (debug-source)
  "Return DEBUG-SOURCE start positions when this SBCL exposes the accessor.
Older SBCL versions may not provide this internal accessor; avoid a read-time
SB-C:: reference because package locks can reject interning missing symbols."
  (let ((accessor
          (or (find-symbol "DEBUG-SOURCE-START-POSITIONS" "SB-DI")
              (find-symbol "DEBUG-SOURCE-START-POSITIONS" "SB-C"))))
    (when (and accessor (fboundp accessor))
      (funcall accessor debug-source))))

#+sbcl
(defun %frame-source-location (frame)
  "Extract source file and line from FRAME, or NIL if unavailable.
Uses the code-location's TLF offset and the debug-source start-positions
array to resolve the line of the enclosing top-level form.  Returns a
NIL line (but still the file name) when start-positions is unavailable,
so callers do not render a small TLF index as if it were a line number.

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
                             (%debug-source-start-positions debug-source)))
                         (tlf-char
                           (when (and start-positions tlf-offset
                                      (< tlf-offset (length start-positions)))
                             (aref start-positions tlf-offset)))
                         (line
                           (when (and tlf-char
                                      (ignore-errors (probe-file namestring)))
                             (%offset->line namestring tlf-char))))
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

(defparameter *standard-signaling-frames*
  '("ERROR" "SIGNAL" "CERROR" "WARN" "INVOKE-DEBUGGER" "BREAK" "EVAL")
  "Unqualified standard CL function names always classified as internal frames
in backtraces, independent of the *internal-package-prefixes* list.")

(defparameter *method-wrapper-prefixes*
  '("(SB-PCL::FAST-METHOD " "(SB-PCL::SLOW-METHOD ")
  "Exact leading substrings (opening paren + package-qualified operator +
trailing space) that mark an SBCL CLOS method wrapper frame.  Matching against
the full prefix avoids collisions with arbitrary user symbols that merely
contain the substring FAST-METHOD.")

(defun %prefix-internal-p (name)
  "Return T if NAME begins with any entry in *INTERNAL-PACKAGE-PREFIXES*
terminated by a package delimiter (: or /) or end-of-string."
  (some (lambda (prefix)
          (let ((prefix-len (length prefix)))
            (and (>= (length name) prefix-len)
                 (string-equal name prefix :end1 prefix-len)
                 (or (= (length name) prefix-len)
                     (char= (char name prefix-len) #\:)
                     (char= (char name prefix-len) #\/)))))
        *internal-package-prefixes*))

(defun %parse-wrapped-symbol (s start)
  "Read a symbol token from S starting at or after START, skipping leading
spaces.  When the token is a nested (SETF NAME) form, returns just NAME.
Returns NIL when no symbol can be extracted."
  (let ((head (position-if-not (lambda (c) (char= c #\Space))
                               s :start start)))
    (when head
      (if (char= (char s head) #\()
          (let ((setf-pos (search "SETF " s :start2 head)))
            (when setf-pos
              (let* ((sym-start (position-if-not (lambda (c) (char= c #\Space))
                                                 s :start (+ setf-pos 5)))
                     (sym-end (when sym-start
                                (or (position #\) s :start sym-start)
                                    (length s)))))
                (when (and sym-start sym-end (< sym-start sym-end))
                  (subseq s sym-start sym-end)))))
          (subseq s head
                  (or (position-if (lambda (c)
                                     (or (char= c #\Space)
                                         (char= c #\()
                                         (char= c #\))))
                                   s :start head)
                      (length s)))))))

(defun %method-wrapper-name (function-name)
  "Return the user-visible method name from a (SB-PCL::FAST-METHOD NAME ...)
or (SB-PCL::SLOW-METHOD NAME ...) wrapper, or NIL when FUNCTION-NAME is not a
method wrapper.  Uses a full opening-paren-qualified prefix match so symbols
that merely contain FAST-METHOD as a substring are not misclassified."
  (dolist (prefix *method-wrapper-prefixes*)
    (let ((prefix-len (length prefix)))
      (when (and (>= (length function-name) prefix-len)
                 (string-equal function-name prefix :end1 prefix-len))
        (return-from %method-wrapper-name
          (%parse-wrapped-symbol function-name prefix-len))))))

(defun %setf-frame-target (function-name)
  "Return the target symbol name from a (SETF NAME) frame as a string, or NIL
when FUNCTION-NAME is not a SETF frame."
  (when (and (>= (length function-name) 6)
             (string-equal function-name "(SETF " :end1 6))
    (string-trim '(#\) #\Space) (subseq function-name 6))))

(defun %internal-frame-p (function-name)
  "Return T if FUNCTION-NAME appears to be an internal/infrastructure frame.
Dispatches on structural shape first, then applies the package-prefix filter
to the relevant inner name:

- (SB-PCL::FAST-METHOD NAME (...) ...) -> internal iff NAME has an internal prefix
- (SB-PCL::SLOW-METHOD NAME (...) ...) -> internal iff NAME has an internal prefix
- (SETF NAME)                          -> internal iff NAME has an internal prefix
- Any other (...) wrapper (FLET, LAMBDA, LABELS, etc.) -> always internal
- Bare symbol name matching an internal package prefix -> internal
- Unqualified name in *STANDARD-SIGNALING-FRAMES*      -> internal

Empty strings are treated as non-internal (caller's responsibility to filter)."
  (when (plusp (length function-name))
    (let ((method-name (%method-wrapper-name function-name))
          (setf-target (%setf-frame-target function-name)))
      (cond
        (method-name (%prefix-internal-p method-name))
        (setf-target (%prefix-internal-p setf-target))
        ((char= (char function-name 0) #\() t)
        ((%prefix-internal-p function-name) t)
        ((member function-name *standard-signaling-frames*
                 :test #'string-equal)
         t)))))

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
