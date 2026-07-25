;;;; src/macroexpand-core.lisp
;;;;
;;;; Pure macro-expansion logic shared by the worker handler and the
;;;; inline (no worker pool) fallback.  Knows nothing about JSON-RPC or
;;;; MCP: callers pass (LABEL . SOURCE) conses and receive plists.

(defpackage #:cl-mcp/src/macroexpand-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:macroexpand-forms
           #:macroexpand-source
           #:macroexpand-package-error
           #:macroexpand-package-error-name
           #:*expansion-print-level*
           #:*expansion-print-length*
           #:*expansion-max-output-length*
           #:*max-expansion-steps*))

(in-package #:cl-mcp/src/macroexpand-core)

(defparameter *expansion-print-level* 50
  "Default `*print-level*` for printed macro expansions.
Must stay finite so a pathologically deep expansion cannot produce
unbounded nesting depth.  It does not bound overall output size — a wide
but shallow form can still print length^level elements — so
`*expansion-max-output-length*` is the actual size guard.  Note also that
this does NOT protect against a circular expansion — SBCL's pretty
printer ignores it for the QUOTE abbreviation.  That case is handled by
`%circular-p` in `%print-expansion`.")

(defparameter *expansion-print-length* 1000
  "Default `*print-length*` for printed macro expansions.")

(defparameter *expansion-max-output-length* 50000
  "Default maximum characters for one printed expansion.")

(defparameter *max-expansion-steps* 100
  "Upper bound on repeated MACROEXPAND-1 steps for level \"full\".
Guards against a macro that expands into itself forever.")

(define-condition macroexpand-package-error (error)
  ((name :initarg :name :reader macroexpand-package-error-name))
  (:report
   (lambda (condition stream)
     (format stream
             "Package ~A does not exist in this image, so its macros cannot be ~
expanded. Load the system that defines it with the 'load-system' tool, then retry."
             (macroexpand-package-error-name condition))))
  (:documentation "Signaled when the requested expansion package is absent.
Never synthesize a stub package instead: expanding in a stub silently
produces an unexpanded form and misleads the caller."))

(defun %resolve-package (name)
  "Return the package named NAME, or CL-USER when NAME is NIL.
Signals MACROEXPAND-PACKAGE-ERROR when NAME names no existing package."
  (if (null name)
      (find-package :cl-user)
      (or (find-package name)
          (find-package (string-upcase name))
          (error 'macroexpand-package-error :name name))))

(defun %parse-readtable-name (designator)
  "Convert the DESIGNATOR string into a symbol for FIND-READTABLE.
Accepts \"pkg:sym\", \"pkg::sym\", \":kw\" and bare \"kw\".

Uses FIND-SYMBOL rather than INTERN: the argument is caller-controlled,
and interning it would let a stream of bad values grow the package table
without bound.  A readtable registered with DEFREADTABLE always has its
name symbol interned already, so nothing valid is lost."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) designator))
         (colon (position #\: trimmed)))
    (flet ((lookup (name package)
             (or (find-symbol (string-upcase name) package)
                 (error "Readtable name ~A is not interned in ~A; ~
the system that defines it is probably not loaded."
                        name (package-name package)))))
      (cond
        ((null colon)
         (lookup trimmed (find-package :keyword)))
        ((zerop colon)
         (lookup (string-left-trim ":" trimmed) (find-package :keyword)))
        (t
         (let ((package (or (find-package
                             (string-upcase (subseq trimmed 0 colon)))
                            (error "Package ~A not found for readtable ~A"
                                   (subseq trimmed 0 colon) designator))))
           (lookup (string-left-trim ":" (subseq trimmed colon)) package)))))))

(defun %resolve-readtable (designator)
  "Return the named readtable for DESIGNATOR, or NIL when it is absent.
Looks the name up in whichever named-readtables package is loaded, so
this file does not have to depend on the parent-only tool layer.

TODO(satoshi.imai@pocket-change.jp, 2026-07-26): extract
src/utils/readtables.lisp -- src/cst.lisp and src/lisp-edit-form-core.lisp
each carry their own copy of this lookup.  Extracting one shared helper is
worth doing, but it touches two well-tested modules and is deliberately
left out of this change."
  (when (and designator (stringp designator) (string/= designator ""))
    (let ((package (or (find-package "NAMED-READTABLES")
                       (find-package "EDITOR-HINTS.NAMED-READTABLES"))))
      (unless package
        (error "A 'readtable' was requested but named-readtables is not ~
loaded in this image."))
      (let* ((finder (find-symbol "FIND-READTABLE" package))
             (name (%parse-readtable-name designator))
             (table (and finder (ignore-errors (funcall finder name)))))
        (unless table
          (error "Readtable ~A not found. Load the system that defines it first."
                 designator))
        table))))

(defun %read-source (source package readtable)
  "Read the single form in SOURCE with *PACKAGE* bound to PACKAGE.

Signals when SOURCE holds more than one form.  The caller slices this
text out of a file by form address, so a second form means the slice was
wrong, and silently expanding only the first would return a plausible but
incorrect answer.

*READ-EVAL* is deliberately left enabled: real sources contain #. and
this tool already runs the macro's expander, so reader evaluation adds no
new class of exposure.  Note that the reject path below still reads (and
therefore #.-evaluates) the offending trailing form before refusing it —
rejection happens after reading, not before."
  (let ((*package* package)
        (*readtable* (or readtable *readtable*)))
    (with-input-from-string (stream source)
      (let ((form (read stream)))
        ;; STREAM is its own eof-value here: no read datum can ever be EQ to
        ;; the stream object, so this distinguishes a real second form from
        ;; end of input.  Testing mere truthiness would fire on both.
        (unless (eq (read stream nil stream) stream)
          (error "SOURCE contains more than one form; expected exactly one."))
        form))))

(defun %expand-once (form)
  "Expand FORM one step.  Returns (values expansion steps)."
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form)
    (values expansion (if expanded-p 1 0))))

(defun %expand-full (form)
  "Repeatedly expand FORM while its head is a macro.
Returns (values expansion steps capped-p).  CAPPED-P is T when
*MAX-EXPANSION-STEPS* was reached, i.e. the result is still a macro call
rather than a fixpoint — the caller must not present it as fully expanded."
  (let ((current form)
        (steps 0))
    (loop
      (when (>= steps *max-expansion-steps*)
        (return (values current steps t)))
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 current)
        (unless expanded-p
          (return (values current steps nil)))
        (setf current expansion)
        (incf steps)))))

(defun %circular-p (form)
  "Return T when FORM contains a cycle reachable through conses.

Only conses are traversed.  A cycle through a literal vector or structure
is not something this guards against; macro expansions do not produce
those, and walking them would cost more than it saves.

The cdr spine is walked iteratively and only cars recurse, so stack depth
is bounded by nesting rather than by list length.  That matters: a long
quoted literal such as a generated lookup table is ordinary input here,
and recursing along its spine would exhaust the control stack.

Finished nodes are memoized, so a heavily shared but acyclic expansion
costs one visit per distinct cons instead of blowing up exponentially."
  (let ((on-path (make-hash-table :test #'eq))
        (finished (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (let ((spine '())
                     (found nil))
                 (loop
                   (cond
                     ((not (consp node)) (return))
                     ((gethash node on-path) (setf found t) (return))
                     ((gethash node finished) (return))
                     (t
                      (setf (gethash node on-path) t)
                      (push node spine)
                      (when (walk (car node))
                        (setf found t)
                        (return))
                      (setf node (cdr node)))))
                 (dolist (visited spine)
                   (remhash visited on-path)
                   (setf (gethash visited finished) t))
                 found)))
      (walk form))))

(defun %expand-all (form)
  "Walk FORM with SB-CLTL2:MACROEXPAND-ALL, expanding nested macros.
Returns (values expansion steps), where STEPS is 1 when the walk changed
FORM and 0 otherwise.

Refuses a circular FORM.  Source text really can be circular — #n= is
standard reader syntax, so \"#1=(list 1 . #1#)\" reads fine — and
MACROEXPAND-ALL walks the whole tree, exhausting the control stack on
such input.

The check is on the INPUT, and it is NOT a complete guarantee.  An
expander can build a circular expansion out of acyclic input, and
MACROEXPAND-ALL will descend into it and exhaust the control stack
mid-walk.  That case is caught by the STORAGE-CONDITION clause in
MACROEXPAND-FORMS, which is therefore load bearing and must not be
removed.  Level \"all\" also has no step cap of its own:
*MAX-EXPANSION-STEPS* applies to \"full\" only.

Levels \"once\" and \"full\" only look at the head, so they accept
circular input without this restriction."
  (when (%circular-p form)
    (error "Level \"all\" cannot expand a circular form; use \"once\" or \"full\"."))
  (let ((expansion (sb-cltl2:macroexpand-all form)))
    (values expansion (if (equal expansion form) 0 1))))

(defun %expand (form level)
  "Expand FORM according to LEVEL.  Returns (values expansion steps
capped-p).  CAPPED-P is only meaningful for LEVEL \"full\" (T when
*MAX-EXPANSION-STEPS* was reached before a fixpoint); it is NIL for
\"once\" and \"all\", which have no step cap of their own."
  (cond
    ((string-equal level "once") (%expand-once form))
    ((string-equal level "full") (%expand-full form))
    ((string-equal level "all") (%expand-all form))
    (t (error "Unknown level ~S: expected \"once\", \"full\" or \"all\"."
              level))))

(defun %print-expansion (form package print-level print-length)
  "Print FORM as readable source relative to PACKAGE.

*PRINT-CIRCLE* is normally NIL so that shared-structure markers such as
#1= do not pollute the output: backquote expansions share literals all
the time and those markers make the result hard to read.

It is switched on only for a genuinely circular FORM.  *PRINT-LEVEL*
cannot be relied on to bound that case, because SBCL's pretty printer
does not honour it for the QUOTE abbreviation, and the printing would
never terminate."
  (let ((*package* package)
        (*print-circle* (%circular-p form))
        (*print-level* (max 1 (or print-level *expansion-print-level*)))
        (*print-length* (max 1 (or print-length *expansion-print-length*)))
        (*print-lines* nil)
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 100)
        (*print-readably* nil)
        (*print-escape* t)
        (*print-gensym* t)
        (*print-base* 10)
        (*print-radix* nil))
    (prin1-to-string form)))

(defun %truncate (string max-output-length)
  "Truncate STRING then sanitize it for JSON.
Returns (values text truncated-p).  Truncation happens before
sanitization so the limit applies to the characters actually emitted."
  (let ((limit (or max-output-length *expansion-max-output-length*)))
    (if (and limit (> (length string) limit))
        (values (sanitize-for-json
                 (concatenate 'string (subseq string 0 limit) "...(truncated)"))
                t)
        (values (sanitize-for-json string) nil))))

(defun %expand-one-entry (source package readtable level
                          print-level print-length max-output-length)
  "Expand SOURCE in the already-resolved PACKAGE.
Returns the tail of the result plist (everything except :LABEL)."
  (let ((form (%read-source source package readtable)))
    (multiple-value-bind (expansion steps capped-p)
        (%expand form level)
      (multiple-value-bind (printed truncated-p)
          (%truncate (%print-expansion expansion package print-level print-length)
                     max-output-length)
        (list :printed printed
              :expanded-p (plusp steps)
              :steps steps
              :steps-capped-p capped-p
              :truncated-p truncated-p
              :error nil)))))

(defun macroexpand-forms (entries &key package level readtable
                                       print-level print-length
                                       max-output-length)
  "Expand every entry of ENTRIES, a list of (LABEL . SOURCE) conses.

Returns a list of plists in the same order, each with the keys
:LABEL :PRINTED :EXPANDED-P :STEPS :STEPS-CAPPED-P :TRUNCATED-P :ERROR.
A failure in one entry is recorded in that entry's :ERROR and does not
abort the batch, so a caller can render every entry uniformly.  That
includes stack exhaustion from a runaway expansion, which is a
STORAGE-CONDITION rather than an ERROR.

PACKAGE is a package-name string; NIL means CL-USER.  LEVEL is \"once\"
(default), \"full\" or \"all\".  Both the package and the level are
validated up front, before any entry is attempted, because those failures
apply to the whole request."
  (let ((resolved-package (%resolve-package package))
        (resolved-readtable (%resolve-readtable readtable))
        (effective-level (or level "once")))
    (unless (member effective-level '("once" "full" "all") :test #'string-equal)
      (error "Unknown level ~S: expected \"once\", \"full\" or \"all\"."
             effective-level))
    (flet ((failure (condition)
             (list :printed nil
                   :expanded-p nil
                   :steps 0
                   :steps-capped-p nil
                   :truncated-p nil
                   :error (sanitize-for-json (princ-to-string condition)))))
      (loop for (label . source) in entries
            collect (list* :label label
                           (handler-case
                               (%expand-one-entry source resolved-package
                                                  resolved-readtable
                                                  effective-level
                                                  print-level print-length
                                                  max-output-length)
                             ;; STORAGE-CONDITION is named explicitly rather than
                             ;; catching all of SERIOUS-CONDITION.  A runaway
                             ;; expansion raises CONTROL-STACK-EXHAUSTED, which is
                             ;; not an ERROR and must be caught here.  But
                             ;; SB-EXT:TIMEOUT is a SERIOUS-CONDITION too, and a
                             ;; deadline the caller set must abort the request --
                             ;; turning it into a per-entry note that the loop then
                             ;; ignores would silently defeat every enclosing
                             ;; WITH-TIMEOUT.
                             (storage-condition (condition) (failure condition))
                             (error (condition) (failure condition))))))))

(defun macroexpand-source (source &key package level readtable
                                       print-level print-length
                                       max-output-length)
  "Expand the single form in SOURCE.
Returns (values printed expanded-p steps truncated-p steps-capped-p).
This is a thin convenience wrapper over MACROEXPAND-FORMS so both entry
points share one implementation; unlike MACROEXPAND-FORMS it re-signals a
per-entry failure as an error instead of returning it in a plist."
  (let ((entry (first (macroexpand-forms (list (cons nil source))
                                         :package package
                                         :level level
                                         :readtable readtable
                                         :print-level print-level
                                         :print-length print-length
                                         :max-output-length max-output-length))))
    (when (getf entry :error)
      (error "~A" (getf entry :error)))
    (values (getf entry :printed)
            (getf entry :expanded-p)
            (getf entry :steps)
            (getf entry :truncated-p)
            (getf entry :steps-capped-p))))
