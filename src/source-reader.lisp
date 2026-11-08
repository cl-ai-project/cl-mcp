;;;; src/source-reader.lisp
;;;;
;;;; How cl-mcp reads source text it does not evaluate.

(defpackage #:cl-mcp/src/source-reader
  (:use #:cl)
  (:import-from #:eclector.parse-result
                #:parse-result-client)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:lenient-package-p)
  (:export #:source-syntax-client
           #:source-parse-client
           #:read-time-evaluation
           #:read-time-evaluation-p
           #:read-time-evaluation-form
           #:structure-literal
           #:structure-literal-p
           #:structure-literal-name
           #:structure-literal-initargs
           #:stand-in-symbol-package-name
           #:source-featurep
           #:source-readtable
           #:standard-syntax-readtable-p
           #:source-read-from-string
           #:source-read))

(in-package #:cl-mcp/src/source-reader)

(defstruct (read-time-evaluation (:constructor make-read-time-evaluation (form)))
  "What #.FORM reads as in source cl-mcp reads but never evaluates: FORM itself,
unevaluated, standing where its value would have gone.  Printed as #.FORM."
  form)

(defmethod print-object ((object read-time-evaluation) stream)
  (write-string "#." stream)
  (write (read-time-evaluation-form object) :stream stream))

(defstruct (structure-literal (:constructor make-structure-literal (name initargs)))
  "What #S(NAME . INITARGS) reads as in source cl-mcp reads: the name and the
slot initargs as written, with no structure built (the parsing process need not
have NAME defined, and its constructor is code).  Printed as #S(NAME . INITARGS)."
  name
  initargs)

(defmethod print-object ((object structure-literal) stream)
  (write-string "#S" stream)
  (write (cons (structure-literal-name object) (structure-literal-initargs object))
         :stream stream))

(defvar *stand-in-symbols*
  (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Weak table from each stand-in symbol (%STAND-IN-SYMBOL) to the name of the
package its token named.")

(defun %stand-in-symbol (package-name symbol-name)
  "Return a fresh uninterned symbol named SYMBOL-NAME standing in for the token
PACKAGE-NAME:SYMBOL-NAME, whose package this image has but whose symbol it does
not (or, with ::, does not have yet).  Reading must not intern it there: the
package is a real one of the server, which a typo would then change for every
session, and a locked one (CL, SB-EXT) refuses the intern outright."
  (let ((symbol (make-symbol symbol-name)))
    (setf (gethash symbol *stand-in-symbols*) package-name)
    symbol))

(defun stand-in-symbol-package-name (symbol)
  "Return the package name SYMBOL's token was qualified with when SYMBOL is a
stand-in the source reader made for a symbol the package lacked, else NIL."
  (and (symbolp symbol)
       (null (symbol-package symbol))
       (values (gethash symbol *stand-in-symbols*))))

(defclass source-syntax-client ()
  ()
  (:documentation "The Eclector client cl-mcp reads source with.  It reads
everything SBCL's reader reads and evaluates nothing:

  #.FORM        a READ-TIME-EVALUATION holding FORM, never evaluated
  #S(...)       a STRUCTURE-LITERAL, no structure built
  #\\NAME        any name NAME-CHAR knows (#\\Nul, #\\Esc, #\\U+2192, #\\RIGHTWARDS_ARROW)
  PKG:SYM       SYM itself when PKG has it; a stand-in when PKG is a real package
                of this image that lacks it, so no read interns into one

A package the image lacks still gets a temporary stub from
CALL-WITH-LENIENT-PACKAGES, which the read must run inside."))

(defclass source-parse-client (source-syntax-client parse-result-client)
  ()
  (:documentation "SOURCE-SYNTAX-CLIENT building a parse result: the client
PARSE-TOP-LEVEL-FORMS reads files with."))

(defmethod eclector.reader:state-value ((client source-syntax-client)
                                        (aspect (eql '*read-eval*)))
  ;; #. reads its form and hands it to EVALUATE-EXPRESSION below, which does not
  ;; evaluate it.  CL:*READ-EVAL* itself is never bound, so a CL:READ anywhere
  ;; inside the read still refuses #.
  (declare (ignore client))
  t)

(defmethod eclector.reader:evaluate-expression ((client source-syntax-client) expression)
  (declare (ignore client))
  (make-read-time-evaluation expression))

(defmethod eclector.reader:make-structure-instance ((client source-syntax-client)
                                                    name initargs)
  (declare (ignore client))
  (make-structure-literal name initargs))

(defmethod eclector.reader:find-character ((client source-syntax-client)
                                           (designator string))
  (or (call-next-method) (name-char designator)))

(defmethod eclector.reader:interpret-symbol ((client source-syntax-client) input-stream
                                             (package-indicator string) symbol-name internp)
  (let ((package (find-package package-indicator)))
    (if (or (null package) (lenient-package-p package))
        (call-next-method)
        (multiple-value-bind (symbol status) (find-symbol symbol-name package)
          ;; PKG:SYM on an internal SYM reads as that symbol, as a stub's does:
          ;; nothing is interned, and the parse is about the text, not about
          ;; whether it would compile.
          (if status
              symbol
              ;; As written, nickname and all, for display.
              (%stand-in-symbol package-indicator symbol-name))))))

(defun source-featurep (expression)
  "UIOP:FEATUREP of EXPRESSION, a feature expression the source reader read,
except that a #. inside it (a READ-TIME-EVALUATION) holds.  Only evaluating
the form could decide it, which reading source never does; holding keeps the
form it guards read and editable, where any other answer would hide it.
Everything else -- including an operator that is not :AND, :OR or :NOT, which
UIOP refuses -- is UIOP:FEATUREP's."
  (cond
    ((read-time-evaluation-p expression) t)
    ((and (consp expression) (member (first expression) '(:and :or :not)))
     (let ((arguments (rest expression)))
       (ecase (first expression)
         (:and (every #'source-featurep arguments))
         (:or (some #'source-featurep arguments))
         (:not (if (and (consp arguments) (null (rest arguments)))
                   (not (source-featurep (first arguments)))
                   (uiop:featurep expression))))))
    (t (uiop:featurep expression))))

(defmethod eclector.reader:check-feature-expression ((client source-syntax-client)
                                                     (feature-expression
                                                      read-time-evaluation))
  (declare (ignore client feature-expression))
  nil)

(defmethod eclector.reader:evaluate-feature-expression ((client source-syntax-client)
                                                        (feature-expression
                                                         read-time-evaluation))
  ;; As SOURCE-FEATUREP decides it.
  (declare (ignore client feature-expression))
  t)

(defvar *pristine-readtable* (copy-readtable nil)
  "A standard readtable, never modified: what \"reads #. the standard way\"
and STANDARD-SYNTAX-READTABLE-P compare against.")

(defun %sharpsign-dot (stream sub-char argument)
  "The #. of SOURCE-READTABLE: read the form, return it unevaluated."
  (declare (ignore sub-char argument))
  (let ((form (read stream t nil t)))
    (unless *read-suppress*
      (make-read-time-evaluation form))))

(defun %sharpsign-s (stream sub-char argument)
  "The #S of SOURCE-READTABLE: read the list, build no structure."
  (declare (ignore sub-char argument))
  (let ((contents (read stream t nil t)))
    (cond
      (*read-suppress* nil)
      ((and (consp contents) (ignore-errors (list-length contents)))
       (make-structure-literal (first contents) (rest contents)))
      (t (error "Reader error: #S must be followed by a list naming a structure, ~
                 not ~S."
                contents)))))

(defun source-readtable (readtable)
  "Return a copy of READTABLE whose #. and #S read as the source client reads
them -- a READ-TIME-EVALUATION and a STRUCTURE-LITERAL -- where READTABLE reads
them the standard way.  A readtable that defines either itself keeps its own.
For CL:READ under a named readtable; Eclector reads standard syntax with
SOURCE-SYNTAX-CLIENT."
  (let ((copy (copy-readtable readtable)))
    (flet ((replace-standard (sub-char function)
             (when (eq (ignore-errors (get-dispatch-macro-character #\# sub-char readtable))
                       (get-dispatch-macro-character #\# sub-char *pristine-readtable*))
               (set-dispatch-macro-character #\# sub-char function copy))))
      (replace-standard #\. #'%sharpsign-dot)
      (replace-standard #\S #'%sharpsign-s))
    copy))

(defun standard-syntax-readtable-p (readtable)
  "True when READTABLE reads as the standard readtable does: the same case,
the same macro function on every ASCII character, and the same # dispatch for
every ASCII sub-character.  A named readtable that only renames the standard
one (:standard, or a copy of it) counts; one that adds or changes any of these
does not."
  (and (eq (readtable-case readtable) :upcase)
       (loop for code from 0 below 128
             for char = (code-char code)
             always (multiple-value-bind (function non-terminating)
                        (get-macro-character char readtable)
                      (multiple-value-bind (standard standard-non-terminating)
                          (get-macro-character char *pristine-readtable*)
                        (and (eq function standard)
                             (eq (not non-terminating) (not standard-non-terminating))))))
       (loop for code from 0 below 128
             for char = (code-char code)
             always (eq (ignore-errors (get-dispatch-macro-character #\# char readtable))
                        (ignore-errors
                         (get-dispatch-macro-character #\# char *pristine-readtable*))))))

(defun source-read-from-string (string &key (eof-error-p t) eof-value (start 0) end
                                            preserve-whitespace)
  "READ-FROM-STRING as the editing tools' file parser reads: with
SOURCE-SYNTAX-CLIENT when *READTABLE* has standard syntax
(STANDARD-SYNTAX-READTABLE-P), else with CL:READ under SOURCE-READTABLE of it.
So content validated with this reads exactly as the file it is written into
will be read back -- #., #S and SBCL's character names included -- and a
missing symbol of a real package is a stand-in, never an intern.  Run it inside
CALL-WITH-LENIENT-PACKAGES, as the parser runs.  Returns the object and the
index of the first character not read, as READ-FROM-STRING does."
  (if (standard-syntax-readtable-p *readtable*)
      (let ((eclector.reader:*client* (make-instance 'source-syntax-client)))
        (eclector.reader:read-from-string string eof-error-p eof-value
                                          :start start :end end
                                          :preserve-whitespace preserve-whitespace))
      (let ((*readtable* (source-readtable *readtable*)))
        (read-from-string string eof-error-p eof-value
                          :start start :end end
                          :preserve-whitespace preserve-whitespace))))

(defun source-read (stream &optional (eof-error-p t) eof-value)
  "READ from STREAM as SOURCE-READ-FROM-STRING reads a string."
  (if (standard-syntax-readtable-p *readtable*)
      (let ((eclector.reader:*client* (make-instance 'source-syntax-client)))
        (eclector.reader:read stream eof-error-p eof-value))
      (let ((*readtable* (source-readtable *readtable*)))
        (read stream eof-error-p eof-value))))
