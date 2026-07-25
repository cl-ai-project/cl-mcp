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
           #:*expansion-max-output-length*))

(in-package #:cl-mcp/src/macroexpand-core)

(defparameter *expansion-print-level* 50
  "Default `*print-level*` for printed macro expansions.
Must stay finite so a pathologically deep expansion cannot produce
unbounded output.  Note that it does NOT protect against a circular
expansion — SBCL's pretty printer ignores it for the QUOTE abbreviation.
That case is handled by `%circular-p` in `%print-expansion`.")

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
Accepts \"pkg:sym\", \"pkg::sym\", \":kw\" and bare \"kw\"."
  (let ((colon (position #\: designator)))
    (cond
      ((null colon)
       (intern (string-upcase designator) :keyword))
      ((zerop colon)
       (intern (string-upcase (string-left-trim ":" designator)) :keyword))
      (t
       (let* ((package-name (subseq designator 0 colon))
              (symbol-name (string-left-trim ":" (subseq designator colon)))
              (package (or (find-package (string-upcase package-name))
                           (error "Package ~A not found for readtable ~A"
                                  package-name designator))))
         (intern (string-upcase symbol-name) package))))))

(defun %resolve-readtable (designator)
  "Return the named readtable for DESIGNATOR, or NIL when it is absent.
Looks the name up in whichever named-readtables package is loaded, so
this file does not have to depend on the parent-only tool layer."
  (when (and designator (stringp designator) (string/= designator ""))
    (let* ((package (or (find-package "NAMED-READTABLES")
                        (find-package "EDITOR-HINTS.NAMED-READTABLES")))
           (finder (and package (find-symbol "FIND-READTABLE" package)))
           (table (and finder
                       (ignore-errors
                        (funcall finder (%parse-readtable-name designator))))))
      (unless table
        (error "Readtable ~A not found. Load the system that defines it first."
               designator))
      table)))

(defun %read-source (source package readtable)
  "Read the first form in SOURCE with *PACKAGE* bound to PACKAGE."
  (let ((*package* package)
        (*readtable* (or readtable *readtable*)))
    (with-input-from-string (stream source)
      (read stream))))

(defun %expand-once (form)
  "Expand FORM one step.  Returns (values expansion steps)."
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form)
    (values expansion (if expanded-p 1 0))))

(defun %expand-full (form)
  "Repeatedly expand FORM while its head is a macro.
Returns (values expansion steps).  Stops at *MAX-EXPANSION-STEPS* so a
self-reproducing macro cannot loop forever."
  (let ((current form)
        (steps 0))
    (loop
      (when (>= steps *max-expansion-steps*)
        (return))
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 current)
        (unless expanded-p
          (return))
        (setf current expansion)
        (incf steps)))
    (values current steps)))

(defun %expand-all (form)
  "Walk FORM with SB-CLTL2:MACROEXPAND-ALL, expanding nested macros.
Returns (values expansion steps), where STEPS is 1 when the walk changed
FORM and 0 otherwise."
  (let ((expansion (sb-cltl2:macroexpand-all form)))
    (values expansion (if (equal expansion form) 0 1))))

(defun %expand (form level)
  "Expand FORM according to LEVEL.  Returns (values expansion steps)."
  (cond
    ((string-equal level "once") (%expand-once form))
    ((string-equal level "full") (%expand-full form))
    ((string-equal level "all") (%expand-all form))
    (t (error "Unknown level ~S: expected \"once\", \"full\" or \"all\"."
              level))))

(defun %circular-p (form)
  "Return T when FORM contains a cycle reachable through conses.

Only conses are traversed.  A cycle through a literal vector or structure
is not something this guards against; macro expansions do not produce
those, and walking them would cost more than it saves.

Finished nodes are memoized, so a heavily shared but acyclic expansion
costs one visit per distinct cons instead of blowing up exponentially."
  (let ((on-path (make-hash-table :test #'eq))
        (finished (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (cond
                 ((not (consp node)) nil)
                 ((gethash node on-path) t)
                 ((gethash node finished) nil)
                 (t
                  (setf (gethash node on-path) t)
                  (let ((found (or (walk (car node)) (walk (cdr node)))))
                    (remhash node on-path)
                    (setf (gethash node finished) t)
                    found)))))
      (walk form))))

(defun %print-expansion (form package print-level print-length)
  "Print FORM as readable source relative to PACKAGE.

*PRINT-CIRCLE* is normally NIL so that shared-structure markers such as
#1= do not pollute the output: backquote expansions share literals all
the time and those markers make the result hard to read.

It is switched on only for a genuinely circular FORM.  *PRINT-LEVEL*
cannot be relied on to bound that case, because SBCL's pretty printer
does not honour it for the QUOTE abbreviation and would exhaust the
control stack, killing the whole worker process."
  (let ((*package* package)
        (*print-circle* (%circular-p form))
        (*print-level* (max 1 (or print-level *expansion-print-level*)))
        (*print-length* (max 1 (or print-length *expansion-print-length*)))
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
    (multiple-value-bind (expansion steps)
        (%expand form level)
      (multiple-value-bind (printed truncated-p)
          (%truncate (%print-expansion expansion package print-level print-length)
                     max-output-length)
        (list :printed printed
              :expanded-p (plusp steps)
              :steps steps
              :truncated-p truncated-p
              :error nil)))))

(defun macroexpand-forms (entries &key package level readtable
                                       print-level print-length
                                       max-output-length)
  "Expand every entry of ENTRIES, a list of (LABEL . SOURCE) conses.

Returns a list of plists in the same order, each with the keys
:LABEL :PRINTED :EXPANDED-P :STEPS :TRUNCATED-P :ERROR.  A failure in one
entry is recorded in that entry's :ERROR and does not abort the batch, so
a caller can render every entry uniformly.

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
    (loop for (label . source) in entries
          collect (list* :label label
                         (handler-case
                             (%expand-one-entry source resolved-package
                                                resolved-readtable
                                                effective-level
                                                print-level print-length
                                                max-output-length)
                           (error (condition)
                             (list :printed nil
                                   :expanded-p nil
                                   :steps 0
                                   :truncated-p nil
                                   :error (sanitize-for-json
                                           (princ-to-string condition)))))))))

(defun macroexpand-source (source &key package level readtable
                                       print-level print-length
                                       max-output-length)
  "Expand the single form in SOURCE.
Returns (values printed expanded-p steps truncated-p).  This is a thin
convenience wrapper over MACROEXPAND-FORMS so both entry points share one
implementation; unlike MACROEXPAND-FORMS it re-signals a per-entry
failure as an error instead of returning it in a plist."
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
            (getf entry :truncated-p))))
