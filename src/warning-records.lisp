;;;; src/warning-records.lisp
;;;;
;;;; Warnings signalled while a system loads, recorded with their class,
;;;; severity and place: what load-system reports as warning_records, and what
;;;; run-tests names a test system's load failure by.

(defpackage #:cl-mcp/src/warning-records
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:json-bool)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/utils/paths
                #:normalize-path-for-display)
  (:import-from #:cl-mcp/src/code-core
                #:%offset->line)
  (:import-from #:cl-mcp/src/utils/fasls
                #:registered-source-fasl-pairs
                #:registered-system-files
                #:registered-fasl-sources)
  (:import-from #:cl-mcp/src/utils/project-files
                #:project-file-predicate)
  (:export #:load-failure-kind
           #:load-failure-hint
           #:%redefinition-warning-p
           #:%make-warning-record
           #:%in-project-predicate
           #:%warning-record-tables))

(in-package #:cl-mcp/src/warning-records)

(defun load-failure-kind (condition)
  "Return (values KIND DETAIL) for CONDITION, the error that stopped a load, when
it is one a fresh worker would meet again and that has a cause of its own to
name: :MISSING-DEPENDENCY, a system some :depends-on names that ASDF cannot find,
:MISSING-SYSTEM, the named system itself (each with that system's name as
DETAIL), or :DEFCONSTANT, a constant given a value not EQL to the one it has.
NIL for any other error, a dependency found in the wrong version included."
  (flet ((requires ()
           (ignore-errors
            (princ-to-string (asdf/find-component:missing-requires condition)))))
    (cond
      ((typep condition 'asdf/find-component:missing-component-of-version) nil)
      ((typep condition 'asdf/find-component:missing-dependency)
       (values :missing-dependency (requires)))
      ((typep condition 'asdf/find-component:missing-component)
       (values :missing-system (requires)))
      ((typep condition 'sb-ext:defconstant-uneql)
       (values :defconstant nil)))))

(defun load-failure-hint (kind tool &optional detail)
  "Return the hint load-system's and run-tests' failures end with, for KIND:
:VARIANCE (a package at variance with its DEFPACKAGE), :MISSING-DEPENDENCY or
:MISSING-SYSTEM (DETAIL the system's name), :DEFCONSTANT, :DID-NOT-COMPILE, or
NIL for an error
none of these explains.  TOOL is the tool to retry.  Only the first and the last
send the caller to a fresh worker, and the last only for what the worker holds:
a fresh worker meets every other cause again."
  (ecase kind
    (:variance
     (format nil "Hint: the worker still holds a package as an earlier definition ~
                  declared it -- exports, uses or shadows the file no longer declares. ~
                  Use pool-kill-worker to get a fresh worker, then retry ~A. If it comes ~
                  back on a fresh worker, two definitions of the package disagree: make ~
                  them agree."
             tool))
    (:missing-dependency
     (format nil "Hint: ASDF cannot find ~:[a system~;~:*the system ~A~] this one ~
                  depends on. Check the name in :depends-on, then register its .asd ~
                  (load-system, asdf:load-asd) or install it (ql:quickload) and retry ~A; ~
                  a fresh worker would not find it either."
             detail tool))
    (:missing-system
     (format nil "Hint: ASDF knows no system ~A, and no ~A.asd was found under the ~
                  project root. Check the name, or register its .asd with load-system ~
                  or asdf:load-asd, then retry ~A."
             detail (and detail (asdf:primary-system-name detail)) tool))
    (:defconstant
     (format nil "Hint: a DEFCONSTANT was given a value not EQL to the one it already ~
                  has. A list, string or other compound value is a new object each time ~
                  the file is compiled or loaded, so it never is: use defparameter, or ~
                  alexandria:define-constant with :test #'equal, then retry ~A. A fresh ~
                  worker meets it again whenever the file is recompiled."
             tool))
    (:did-not-compile
     (format nil "Hint: the file did not compile; the compiler output above shows why. ~
                  Fix the file and retry ~A."
             tool))
    ((nil)
     (format nil "Hint: if the error is about something this worker holds -- a ~
                  package, a structure's layout -- a fresh worker clears it: ~
                  pool-kill-worker, then retry ~A. Otherwise its cause is in the code ~
                  being loaded."
             tool))))

(defun %redefinition-warning-p (warning)
  "Return T when WARNING is an SBCL \"redefining X in DEFUN/DEFMACRO/...\"
notification, which LOAD-SYSTEM drops on a first load and a reload alike. Uses the
condition class where available and falls back to a textual prefix match
on other implementations so the filter still works in portable images."
  (or #+sbcl
      (let ((cls (find-class 'sb-kernel:redefinition-warning nil)))
        (and cls (typep warning cls)))
      (let ((text (ignore-errors (princ-to-string warning))))
        (and (stringp text)
             (uiop:string-prefix-p "redefining " text)
             ;; Require " in " so unrelated warnings happening to start
             ;; with "redefining " (e.g. method-combination chatter)
             ;; are not mistakenly muffled.
             (search " in " text)))))

(defun %warning-severity (warning)
  "Return \"style-warning\" for a STYLE-WARNING and \"warning\" for any other
warning.  It is the distinction a build turns on: only the second makes
COMPILE-FILE report a failure."
  (if (typep warning 'style-warning) "style-warning" "warning"))

(defun %warning-class-name (warning)
  "Return the name of WARNING's class with its package, as SB-INT:TYPE-WARNING
is written."
  (let ((*package* (find-package :keyword))
        (*print-case* :upcase))
    (prin1-to-string (type-of warning))))

(defun %warning-kind-key (warning)
  "Return what tells one kind of warning from another: WARNING's class and, for a
SIMPLE-CONDITION, its format control too.  SBCL signals \"defined but never
used\", \"undefined function\" and a good many more as one class, so the
control is all that separates them.  The key is compared, never shown."
  (cons (type-of warning)
        (when (typep warning 'simple-condition)
          (let ((control (simple-condition-format-control warning)))
            (if (stringp control)
                control
                ;; SBCL hands over a tokenized control; its string is what two
                ;; warnings of one kind share.
                (let ((reader (and (find-package "SB-FORMAT")
                                   (find-symbol "FMT-CONTROL-STRING" "SB-FORMAT"))))
                  (or (and reader
                           (fboundp reader)
                           (ignore-errors (funcall reader control)))
                      control)))))))

(defun %compiler-place ()
  "Return three values saying where the compiler is: the file it is reading, the
octet position at which it began to read the top-level form it is compiling,
and the definitions that enclose the code -- what SBCL prints after \"in:\".
No values when no compilation is under way.

Read from the compiler's own error context, as SLIME reads it.  Each accessor
is looked up by name, so an SBCL that lacks one yields no place rather than an
error."
  (let ((package (find-package "SB-C")))
    (flet ((call (name &rest arguments)
             (let ((function (and package (find-symbol name package))))
               (when (and function (fboundp function))
                 (ignore-errors (apply function arguments))))))
      (let ((context (call "FIND-ERROR-CONTEXT" nil)))
        (if context
            (values (call "COMPILER-ERROR-CONTEXT-FILE-NAME" context)
                    (call "COMPILER-ERROR-CONTEXT-FILE-POSITION" context)
                    (call "COMPILER-ERROR-CONTEXT-CONTEXT" context))
            (values))))))

(defun %enclosing-form-label (context)
  "Return the top-level definition CONTEXT names as source text, such as
\"(defun wrong-arity)\", or NIL.  CONTEXT is the list SBCL prints after \"in:\";
its first element names the definition the code sits in.  Printed in the
package of the name it defines, so the name stands bare.

It is the compiler's name for the definition: it says where the warning is,
and is not always what lisp-edit-form takes.  A method comes as
\"(defmethod sample (integer))\", its specializers without the parameter names
a form_name is written with."
  (let ((form (first context)))
    (when (consp form)
      (let* ((name (second form))
             (symbol (cond ((symbolp name) name)
                           ((and (consp name) (symbolp (second name)))
                            (second name))))
             (*package* (or (and symbol (symbol-package symbol)) *package*))
             (*print-case* :downcase)
             (*print-pretty* nil))
        (ignore-errors (prin1-to-string form))))))

(defun %make-warning-record (warning)
  "Return WARNING as a property list: :SEVERITY, :CLASS, :MESSAGE, the :KIND-KEY
that groups it, whether a file was :COMPILING when it was signalled and, when
the compiler has a place for it, the :FILE, the octet :POSITION and the
enclosing :FORM.  :LOADING is the file being loaded, source or FASL: the only
file a warning signalled as a file loads has, so it is what tells a dependency's
from the project's.  Runs in the handler, inside whatever signalled, so it must
not signal itself: a part that cannot be had is left out.

:COMPILING is true inside COMPILE-FILE and nowhere else.  It is not the same as
having a place: an undefined variable is reported when the compilation unit
ends, after every COMPILE-FILE has returned, with the place of the reference."
  (let ((class (or (ignore-errors (%warning-class-name warning)) "WARNING")))
    (multiple-value-bind (file position context)
        (ignore-errors (%compiler-place))
      (list :severity (%warning-severity warning)
            :class class
            ;; Printed at a wide margin: SBCL formats many compiler messages
            ;; in a logical block, which the default margin breaks mid-sentence.
            ;; A finite one: with none the pretty printer never flushes a line
            ;; and rescans what it has queued, quadratic in a large constant.
            :message (or (ignore-errors
                          (let ((*print-right-margin* 1000))
                            (princ-to-string warning)))
                         class)
            :kind-key (ignore-errors (%warning-kind-key warning))
            :compiling (and *compile-file-truename* t)
            :file (and (typep file '(or pathname string)) file)
            :position (and (integerp position) position)
            :form (ignore-errors (%enclosing-form-label context))
            :loading *load-truename*))))

(defun %in-project-predicate (system-name)
  "Return a function of a warning record that says whether the warning came from
the project's own code: T, NIL, or :UNKNOWN when nothing tells -- the record
names no file, or a FASL no registered system compiles.
The file is the record's place, or else the file loading when it was signalled;
a FASL stands for the source a registered system compiles into it
(REGISTERED-FASL-SOURCES), wherever the FASL is kept.  The source is the
project's when it is a source or .asd file of SYSTEM-NAME's project
(REGISTERED-SOURCE-FASL-PAIRS, REGISTERED-SYSTEM-FILES) -- the code being worked
on, wherever it lies, even in a directory the root's .gitignore leaves out -- or
a project file (PROJECT-FILE-PREDICATE).

The root is read when the predicate is made.  The tables are built on first
use: by then the load has registered the systems it reached."
  (let ((own-p (project-file-predicate))
        (fasl-type (uiop:compile-file-type))
        (sources nil)
        (fasl-sources nil))
    (lambda (record)
      (let* ((file (or (getf record :file) (getf record :loading)))
             (path (and file (ignore-errors (pathname file)))))
        (when (and path (equal fasl-type (pathname-type path)))
          (unless fasl-sources
            (setf fasl-sources (or (ignore-errors (registered-fasl-sources))
                                   (make-hash-table :test #'equal))))
          (setf path (gethash (namestring path) fasl-sources)))
        (cond
          ((null path) :unknown)
          (t
           (unless sources
             (setf sources (make-hash-table :test #'equal))
             (loop for (source) in (ignore-errors (registered-source-fasl-pairs system-name))
                   do (setf (gethash (namestring source) sources) t))
             (dolist (asd (ignore-errors (registered-system-files system-name)))
               (setf (gethash (namestring asd) sources) t)))
           (or (gethash (namestring path) sources)
               (funcall own-p path))))))))

(defun %warning-record-tables (records &key in-project)
  "Return RECORDS, property lists in the order signalled, as the JSON objects
load-system reports: \"severity\", \"class\", \"message\", \"kind\" -- a number
the records of one kind share, counted from 1 in order of first appearance --
and, for a warning the compiler has a place for, \"file\", \"line\" and \"form\".
The line is that of the top-level form; SBCL keeps no finer place.

\"in_project\", given IN-PROJECT, a function of a record (%IN-PROJECT-PREDICATE),
is whether the warning came from the project's own files; absent when nothing
tells, so the warning is listed whole rather than taken for a dependency's.

\"fails_compile\" is true on a full warning signalled while a file compiled, and
absent from every other record.  COMPILE-FILE reports failure for such a
warning and for no other, so it is what tells the warnings a file was refused
for from the ones that only came before."
  (let ((kinds '()))
    (mapcar
     (lambda (record)
       (let* ((key (getf record :kind-key))
              (kind (or (position key kinds :test #'equal)
                        (progn (setf kinds (append kinds (list key)))
                               (1- (length kinds)))))
              (file (getf record :file))
              (severity (getf record :severity))
              (table (make-ht "severity" severity
                              "class" (sanitize-for-json (getf record :class))
                              "message" (sanitize-for-json (getf record :message))
                              "kind" (1+ kind))))
         (when (and (getf record :compiling) (equal severity "warning"))
           (setf (gethash "fails_compile" table) t))
         (when in-project
           ;; A judgement that fails tells nothing, so it hides nothing either.
           (let ((own (handler-case (funcall in-project record)
                        (error () :unknown))))
             (unless (eq own :unknown)
               (setf (gethash "in_project" table) (json-bool own)))))
         (when file
           (let ((shown (ignore-errors (normalize-path-for-display file)))
                 (line (ignore-errors (%offset->line file (getf record :position))))
                 (form (getf record :form)))
             (when shown
               (setf (gethash "file" table) (sanitize-for-json shown)))
             (when line
               (setf (gethash "line" table) line))
             (when form
               (setf (gethash "form" table) (sanitize-for-json form)))))
         table))
     records)))
