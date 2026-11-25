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
  (:import-from #:cl-mcp/src/debugger-preview
                #:generate-debugger-preview)
  (:import-from #:cl-mcp/src/code-core
                #:%offset->line)
  (:export #:capture-error-context
           #:capture-debugger-error-context
           #:eval-at-user-code-boundary))


(in-package #:cl-mcp/src/frame-inspector)

(defun %restart-name-string (restart)
  "Return RESTART's name printed the way INVOKE-RESTART has to receive it.
Printed readably and relative to *PACKAGE*, so a keyword keeps its colon and a
symbol homed in another package keeps its qualifier.

INVOKE-RESTART compares names with EQ, and cl-mcp's own prompts tell library
authors to name restarts with keywords for that reason. Printing the name with
~A rendered :RETURN-NIL and RETURN-NIL as the same text, so the field an agent
reads to decide which of the two to type could not tell them apart. During
repl-eval *PACKAGE* is the package the caller asked for, which makes the
printed name exactly what they would have to write there."
  (let ((name (restart-name restart)))
    (if name
        (prin1-to-string name)
        "NIL")))

(defun %collect-restarts ()
  "Return list of available restarts with names and descriptions.
Names are printed readably by %RESTART-NAME-STRING so that a keyword-named
restart is distinguishable from a symbol-named one."
  (loop for restart in (compute-restarts)
        collect (list :name (%restart-name-string restart)
                      :description (or (ignore-errors
                                         (princ-to-string restart))
                                       ""))))

(defun %collect-restarts-for-debugger ()
  "Return available restarts without recovering from diagnostic failures."
  (loop for restart in (compute-restarts)
        collect (list :name (%restart-name-string restart)
                      :description (princ-to-string restart))))

#+sbcl
(defun %debug-var-name (var)
  "Return VAR's name as SBCL's debugger writes it: its symbol's name, followed by
#ID when ID is not zero.  A frame can hold several variables of one name -- an
inner binding that shadows an argument, or the variables of a LABELS helper the
compiler merged into its caller -- and SBCL tells them apart only by that id,
so without it two locals read PATTERN with nothing to say which is which.
A variable the compiler introduced or renamed -- the key CASE binds is
SB-C::X -- is written with its package, and one a macro made with a gensym
(LOOP's, DOLIST's) as #:NAME, so neither reads as a variable of the user's.
SBCL's own packages are told by their SB- prefix, the naming convention a user
package is not expected to follow."
  (let* ((symbol (sb-di:debug-var-symbol var))
         (package (symbol-package symbol))
         (name (cond ((null package)
                      (format nil "#:~A" (symbol-name symbol)))
                     ((eql 0 (search "SB-" (package-name package)))
                      (format nil "~A::~A" (package-name package) (symbol-name symbol)))
                     (t (symbol-name symbol))))
         (id (sb-di:debug-var-id var)))
    (if (zerop id)
        name
        (format nil "~A#~D" name id))))

#+sbcl
(defun %frame-locals (frame print-level print-length
                      &key include-preview preview-max-depth preview-max-elements)
  "Extract local variable names and values from FRAME.
Non-primitive values are registered in the object registry for drill-down inspection.
When INCLUDE-PREVIEW is true, generates structural preview for non-primitive locals.
A name is %DEBUG-VAR-NAME's, so two variables of one name stay apart."
  (let ((debug-fun (sb-di:frame-debug-fun frame))
        (locals '()))
    (handler-case
        (sb-di:do-debug-fun-vars (var debug-fun)
          (when (eq (sb-di:debug-var-validity var (sb-di:frame-code-location frame))
                    :valid)
            (handler-case
                (let ((name (%debug-var-name var))
                      (val (sb-di:debug-var-value var frame)))
                  (if (inspectable-p val)
                      ;; Non-primitive: generate preview or just register
                      (if include-preview
                          (let ((preview (generate-result-preview
                                          val
                                          :max-depth (or preview-max-depth 1)
                                          :max-elements (or preview-max-elements 5))))
                            (push (list :name name
                                        :value (safe-prin1 val
                                                           :level print-level
                                                           :length print-length)
                                        :object-id (gethash "id" preview)
                                        :preview preview)
                                  locals))
                          (let ((object-id (register-object val)))
                            (push (list :name name
                                        :value (safe-prin1 val
                                                           :level print-level
                                                           :length print-length)
                                        :object-id object-id)
                                  locals)))
                      ;; Primitive: just include name and value
                      (push (list :name name
                                  :value (safe-prin1 val
                                                     :level print-level
                                                     :length print-length))
                            locals)))
              (error () nil))))
      (error () nil))
    (nreverse locals)))

#+sbcl
(defun %debugger-prin1 (object print-level print-length)
  "Print OBJECT under diagnostic capture limits without absorbing conditions."
  (let ((*print-level* print-level)
        (*print-length* print-length)
        (*print-readably* nil)
        (*print-circle* nil))
    (prin1-to-string object)))

#+sbcl
(defun %frame-locals-for-debugger (frame print-level print-length
                                   &key include-preview preview-max-depth
                                        preview-max-elements)
  "Extract FRAME locals while allowing every diagnostic condition to escape.
A name is %DEBUG-VAR-NAME's, as in %FRAME-LOCALS."
  (let ((debug-fun (sb-di:frame-debug-fun frame))
        (locals '()))
    (when (and debug-fun (not (typep debug-fun 'sb-di::bogus-debug-fun)))
      (sb-di:do-debug-fun-vars (var debug-fun)
        (when (eq (sb-di:debug-var-validity var (sb-di:frame-code-location frame))
                  :valid)
          (let ((name (%debug-var-name var))
                (val (sb-di:debug-var-value var frame)))
            (if (inspectable-p val)
                (if include-preview
                    (let ((preview (generate-debugger-preview
                                    val
                                    :max-depth (or preview-max-depth 1)
                                    :max-elements (or preview-max-elements 5))))
                      (push (list :name name
                                  :value (%debugger-prin1 val print-level print-length)
                                  :object-id (gethash "id" preview)
                                  :preview preview)
                            locals))
                    (let ((object-id (register-object val)))
                      (push (list :name name
                                  :value (%debugger-prin1 val print-level print-length)
                                  :object-id object-id)
                            locals)))
                (push (list :name name
                            :value (%debugger-prin1 val print-level print-length))
                      locals))))))
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
(defun %frame-source-location-for-debugger (frame)
  "Extract FRAME source information without recovering from inspection failures."
  (let ((debug-fun (sb-di:frame-debug-fun frame)))
    (when (and debug-fun (not (typep debug-fun 'sb-di::bogus-debug-fun)))
      (let ((code-location (sb-di:frame-code-location frame)))
        (when code-location
          (let ((debug-source (sb-di:code-location-debug-source code-location)))
            (when debug-source
              (let ((namestring (sb-di:debug-source-namestring debug-source)))
                (when namestring
                  (let* ((tlf-offset
                           (sb-di:code-location-toplevel-form-offset code-location))
                         (start-positions (%debug-source-start-positions debug-source))
                         (tlf-char (when (and start-positions tlf-offset
                                              (< tlf-offset (length start-positions)))
                                     (aref start-positions tlf-offset)))
                         (line (when (and tlf-char (probe-file namestring))
                                 (%offset->line namestring tlf-char))))
                    (list :file namestring :line line)))))))))))

(defun %frame-name-string (name)
  "Return NAME, a debug-fun name, printed on one line.
The printer is otherwise the caller's -- the user's own, while their error is
being handled -- and a pretty printer breaks a long method name such as
(SB-PCL::FAST-METHOD PKG::NAME :BEFORE (T)) over several lines: the backtrace
header then wraps.  A user's *PRINT-LENGTH*, *PRINT-CASE*, *PRINT-BASE* or pprint-dispatch
entries would likewise cut, recase or re-break the name.

So the name is printed with standard printer settings, keeping only the
caller's *PACKAGE*, and with the pretty printer on but no right margin to break
at: a name then reads as SBCL's debugger writes it, (LAMBDA () :IN ...), not
the (LAMBDA NIL :IN ...) printing without the pretty printer gives."
  (let ((package *package*))
    (with-standard-io-syntax
      (let ((*package* package)
            (*print-readably* nil)
            (*print-pretty* t)
            (*print-right-margin* most-positive-fixnum))
        (prin1-to-string name)))))

(declaim (notinline eval-at-user-code-boundary))

(defun eval-at-user-code-boundary (form)
  "Evaluate FORM, code a caller was asked to run, with this function's frame
marking on the stack where that code's evaluation begins.  A backtrace captured
while FORM runs stops there: this frame, the evaluator frames just above it and
everything below are the caller's machinery, not FORM's (%STACK-WINDOW).
DEBUG 3 keeps the call to EVAL out of tail position, so the frame stays."
  (declare (optimize (debug 3) (speed 0)))
  (eval form))

(defun %symbols-named (&rest specs)
  "Return the symbols SPECS name, each a list (PACKAGE-NAME SYMBOL-NAME), leaving
out those this Lisp does not have."
  (loop for (package-name symbol-name) in specs
        for symbol = (and (find-package package-name)
                          (find-symbol symbol-name package-name))
        when symbol
          collect symbol))

(defparameter *signalling-functions*
  (%symbols-named '("COMMON-LISP" "ERROR") '("COMMON-LISP" "CERROR")
                  '("COMMON-LISP" "SIGNAL") '("COMMON-LISP" "WARN")
                  '("COMMON-LISP" "BREAK") '("COMMON-LISP" "INVOKE-DEBUGGER")
                  '("SB-INT" "%BREAK")
                  '("SB-KERNEL" "%SIGNAL") '("SB-KERNEL" "%ERROR")
                  '("SB-KERNEL" "COERCE-TO-CONDITION") '("SB-KERNEL" "%COERCE-TO-CONDITION")
                  '("SB-KERNEL" "WITH-SIMPLE-CONDITION-RESTARTS")
                  '("SB-DEBUG" "%INVOKE-DEBUGGER") '("SB-DEBUG" "RUN-HOOK"))
  "The functions a condition passes through between the call that signals it and
the handler or debugger hook that captures it.")

(defparameter *evaluator-functions*
  (%symbols-named '("COMMON-LISP" "EVAL") '("SB-INT" "SIMPLE-EVAL-IN-LEXENV")
                  '("SB-IMPL" "SIMPLE-EVAL-PROGN-BODY") '("SB-IMPL" "SIMPLE-EVAL-LOCALLY")
                  '("SB-IMPL" "%SIMPLE-EVAL") '("SB-IMPL" "EVAL-IN-LEXENV"))
  "The functions EVAL-AT-USER-CODE-BOUNDARY's EVAL runs through before the code
of the form it evaluates.")

(defparameter *capture-functions*
  '(%map-stack %collect-window capture-error-context capture-debugger-error-context)
  "This file's functions on the stack above a capture's walk; the walk starts in
the first of them unless SBCL starts it elsewhere.")

(defun %signalling-name-p (name)
  "True when NAME, a frame's debug-fun name, is one of *SIGNALLING-FUNCTIONS*."
  (and (member name *signalling-functions*) t))

(defun %stack-window (names)
  "Return (values START END) delimiting, within NAMES -- a vector of one walk's
frame names, innermost first, ending at EVAL-AT-USER-CODE-BOUNDARY's frame when
the walk met it -- the frames of the code that signalled.

Above START, when the walk begins in this file's own frames
(*CAPTURE-FUNCTIONS*): those, the handler or hook that called them, and the run
of *SIGNALLING-FUNCTIONS* that reached it -- or, with no condition signalled
(a capture after HANDLER-CASE unwound), this file's frames alone.  When the walk
begins elsewhere, as SBCL begins an internal error's at the frame that erred,
only a leading run of *SIGNALLING-FUNCTIONS*.
From END on: the boundary, and the *EVALUATOR-FUNCTIONS* frames just above it.

Every rule is about where a frame stands relative to the capture or the
boundary, never about whose code a frame looks like."
  (labels ((capture-name-p (name)
             ;; One of this file's functions, or a local function or lambda
             ;; written in one: (FLET WALK :IN %COLLECT-WINDOW).
             (or (and (member name *capture-functions*) t)
                 (and (consp name)
                      (member (first name) '(flet labels lambda))
                      (let ((outer (second (member :in (cddr name)))))
                        (and outer (capture-name-p outer)))))))
    (let* ((count (length names))
           (start
             (if (and (plusp count) (capture-name-p (aref names 0)))
                 (let ((signal (position-if #'%signalling-name-p names)))
                   (if signal
                       (or (position-if-not #'%signalling-name-p names :start signal) count)
                       (or (position-if-not #'capture-name-p names) count)))
                 (or (position-if-not #'%signalling-name-p names) count)))
           (end (or (position 'eval-at-user-code-boundary names :start start) count)))
      (loop while (and (> end start)
                       (member (aref names (1- end)) *evaluator-functions*))
            do (decf end))
      (values start end))))

(defun %sbcl-package-p (package)
  "True when PACKAGE is one of SBCL's own: its name begins with SB-."
  (let ((name (package-name package)))
    (and (> (length name) 3) (string= "SB-" name :end2 3))))

(defun %implementation-name-p (name)
  "True when NAME, a frame's debug-fun name as SBCL holds it, names SBCL's own
code rather than code it was given.
A symbol is SBCL's when its home package is (%SBCL-PACKAGE-P); a COMMON-LISP
function is the API called, not SBCL's own.  A bare string is SBCL's name for a
foreign or trampoline frame (\"foreign function: call_into_lisp\").  A lambda,
(LAMBDA ARGS :IN OUTER), is OUTER's: a symbol or name by the same rule, a
string a source file, SBCL's when it is on the SYS: logical host.  Any other
list names its second element: (SETF NAME), (LABELS NAME :IN OUTER),
(SB-PCL::FAST-METHOD NAME ...)."
  (typecase name
    (null nil)
    (symbol (let ((package (symbol-package name)))
              (and package (%sbcl-package-p package))))
    (string t)
    (cons
     (if (eq (first name) 'lambda)
         (let ((outer (second (member :in (cddr name)))))
           (typecase outer
             (null nil)
             (string (and (>= (length outer) 4) (string-equal "SYS:" outer :end2 4)))
             (t (%implementation-name-p outer))))
         (%implementation-name-p (second name))))
    (t nil)))

#+sbcl
(defun %map-stack (function)
  "Call FUNCTION on each frame of the stack, innermost first, as SBCL's debugger
walks it.  A capture's two walks both start here, so the frames they see line
up."
  (declare (optimize (debug 3) (speed 0)))
  (funcall (fdefinition (find-symbol "MAP-BACKTRACE" "SB-DEBUG")) function)
  nil)

#+sbcl
(defun %frame-name-object (frame)
  "Return FRAME's debug-fun name as SBCL holds it: a symbol, a list such as
(LABELS WALK :IN RUN), or a string."
  (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))

#+sbcl
(defun %frame-entry (frame index name implementation print-level print-length
                     &key include-preview preview-max-depth preview-max-elements
                          debugger)
  "Return the plist recording FRAME, the INDEXth of a capture's window, whose
debug-fun name is NAME.  DEBUGGER selects the helpers that let every condition
escape; otherwise a name that cannot be printed is recorded as <unknown>."
  (let ((source (if debugger
                    (%frame-source-location-for-debugger frame)
                    (%frame-source-location frame))))
    (list :index index
          :function (cond ((null name) "<anonymous>")
                          (debugger (%frame-name-string name))
                          (t (handler-case (%frame-name-string name)
                               (error () "<unknown>"))))
          :implementation implementation
          :source-file (getf source :file)
          :source-line (getf source :line)
          :locals (if debugger
                      (%frame-locals-for-debugger
                       frame print-level print-length
                       :include-preview include-preview
                       :preview-max-depth preview-max-depth
                       :preview-max-elements preview-max-elements)
                      (%frame-locals
                       frame print-level print-length
                       :include-preview include-preview
                       :preview-max-depth preview-max-depth
                       :preview-max-elements preview-max-elements)))))

#+sbcl
(defun %collect-window (max-frames print-level print-length
                        &key locals-preview-frames preview-max-depth
                             preview-max-elements locals-preview-skip-internal
                             debugger)
  "Walk the stack and return (values FRAMES TRUNCATED TOTAL) for the TOTAL frames
of %STACK-WINDOW.  FRAMES holds them all when they fit in MAX-FRAMES; otherwise
the first MAX-FRAMES less one and the outermost, so the call that started the
code is always there, and TRUNCATED is true.  Each frame's :INDEX is its place
in the window, 0 the innermost, and :IMPLEMENTATION is true for SBCL's own code
(%IMPLEMENTATION-NAME-P).

The first LOCALS-PREVIEW-FRAMES frames get a structural preview of their
locals, not counting SBCL's own when LOCALS-PREVIEW-SKIP-INTERNAL is true.
PREVIEW-MAX-DEPTH and PREVIEW-MAX-ELEMENTS bound those previews.

DEBUGGER selects the inspection helpers that let every condition escape, for
the request debugger hook; otherwise a frame that cannot be inspected is
recorded as far as it can be, and a failed walk returns what it collected."
  (let ((names (make-array 64 :adjustable t :fill-pointer 0))
        (frames '())
        (total 0)
        (preview-frames (or locals-preview-frames 0))
        (previewed 0))
    (flet ((walk ()
             (block names
               (%map-stack (lambda (frame)
                             (let ((name (if debugger
                                             (%frame-name-object frame)
                                             (ignore-errors (%frame-name-object frame)))))
                               (vector-push-extend name names)
                               (when (eq name 'eval-at-user-code-boundary)
                                 (return-from names))))))
             (multiple-value-bind (start end) (%stack-window names)
               (setf total (- end start))
               (let ((cut (and (> total max-frames) (>= max-frames 2)))
                     (position -1))
                 (block frames
                   (%map-stack
                    (lambda (frame)
                      (incf position)
                      (when (>= position end)
                        (return-from frames))
                      (let ((index (- position start)))
                        (when (and (>= index 0)
                                   (if cut
                                       (or (< index (1- max-frames)) (= index (1- total)))
                                       (< index max-frames)))
                          (let* ((name (aref names position))
                                 (implementation (%implementation-name-p name))
                                 (include-preview
                                   (and (< previewed preview-frames)
                                        (not (and locals-preview-skip-internal
                                                  implementation)))))
                            (when include-preview
                              (incf previewed))
                            (push (%frame-entry frame index name implementation
                                                print-level print-length
                                                :include-preview include-preview
                                                :preview-max-depth preview-max-depth
                                                :preview-max-elements preview-max-elements
                                                :debugger debugger)
                                  frames)))))))))))
      (if debugger
          (walk)
          (handler-case (walk)
            (error () nil))))
    (let ((frames (nreverse frames)))
      (values frames (> total (length frames)) total))))

#-sbcl
(defun %collect-window (max-frames print-level print-length
                        &key locals-preview-frames preview-max-depth
                             preview-max-elements locals-preview-skip-internal
                             debugger)
  "Return no frames on implementations without the SBCL backtrace API."
  (declare (ignore max-frames print-level print-length
                   locals-preview-frames preview-max-depth preview-max-elements
                   locals-preview-skip-internal debugger))
  (values nil nil 0))

(defun capture-error-context
    (condition
     &key (max-frames 20) (print-level 3) (print-length 10)
     (locals-preview-frames 0) (preview-max-depth 1)
     (preview-max-elements 5) (locals-preview-skip-internal t))
  "Capture structured error context including frames and locals.

The frames are those of the code that signalled (%STACK-WINDOW): from the frame
that signalled CONDITION down to the code EVAL-AT-USER-CODE-BOUNDARY was given,
or to the bottom of the stack when no boundary is on it.  The capture's own
frames, the handler's and the signalling machinery's above them, and the
evaluator and caller below the boundary, are not.

MAX-FRAMES: frames recorded (default: 20).  When the window holds more, the
  first MAX-FRAMES less one and the outermost are.
LOCALS-PREVIEW-FRAMES: Number of top frames to include local variable previews (default: 0).
  Set to a positive integer to automatically expand local variables in the top N frames.
  This helps agents immediately see the state of variables without extra inspect-object calls.
LOCALS-PREVIEW-SKIP-INTERNAL: When true (default), SBCL's own frames
  (:IMPLEMENTATION) are not counted for preview, and get none.
PREVIEW-MAX-DEPTH: Max nesting depth for local previews (default: 1).
PREVIEW-MAX-ELEMENTS: Max elements per collection in local previews (default: 5).

Returns a plist with:
  :error t
  :condition-type - string name of condition type
  :message - formatted error message
  :restarts - list of (:name STRING :description STRING)
  :frames - list of frame plists (SBCL only, NIL on other implementations): :index
            (the place in the window, 0 innermost), :function, :implementation,
            :source-file, :source-line, :locals.
            When LOCALS-PREVIEW-FRAMES > 0, locals in top frames include :preview field.
  :frames-truncated - true when the window held more frames than :FRAMES
  :frames-total - how many it held."
  (multiple-value-bind (frames truncated total)
      (%collect-window max-frames print-level print-length
                       :locals-preview-frames locals-preview-frames
                       :preview-max-depth preview-max-depth
                       :preview-max-elements preview-max-elements
                       :locals-preview-skip-internal locals-preview-skip-internal)
    (list :error t
          :condition-type (prin1-to-string (type-of condition))
          :message
          (handler-case (princ-to-string condition)
            (error nil "<error formatting condition>"))
          :restarts (%collect-restarts)
          :frames frames
          :frames-truncated truncated
          :frames-total total)))

(defun %debugger-condition-type-name (condition)
  "Return CONDITION's type text while the diagnostic handler-bind is active."
  (let ((type (type-of condition)))
    (if (symbolp type)
        (symbol-name type)
        (prin1-to-string type))))

(defun capture-debugger-error-context
    (condition on-diagnostic-condition
     &key (max-frames 20) (print-level 3) (print-length 10)
          (locals-preview-frames 0) (preview-max-depth 1)
          (preview-max-elements 5) (locals-preview-skip-internal t))
  "Capture CONDITION while every secondary condition transfers through
ON-DIAGNOSTIC-CONDITION.  Returns what CAPTURE-ERROR-CONTEXT does, for the same
window of frames.

This is for the request debugger hook only. ON-DIAGNOSTIC-CONDITION is
expected to escape; it is deliberately not an ordinary recovery callback."
  (handler-bind
      ((condition on-diagnostic-condition))
    (multiple-value-bind (frames truncated total)
        (%collect-window max-frames print-level print-length
                         :locals-preview-frames locals-preview-frames
                         :preview-max-depth preview-max-depth
                         :preview-max-elements preview-max-elements
                         :locals-preview-skip-internal locals-preview-skip-internal
                         :debugger t)
      (list :error t
            :condition-type (%debugger-condition-type-name condition)
            :message (princ-to-string condition)
            :restarts (%collect-restarts-for-debugger)
            :frames frames
            :frames-truncated truncated
            :frames-total total))))
