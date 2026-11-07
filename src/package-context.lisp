;;;; src/package-context.lisp
;;;;
;;;; Parent-side package context synthesis for structural file tools.
;;;; Reconstructs enough package metadata from source files to activate
;;;; package-local nicknames while keeping the parent process isolated from
;;;; user system runtime state.

(defpackage #:cl-mcp/src/package-context
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages
                #:call-with-managed-packages
                #:record-homeless-on-teardown)
  (:import-from #:cl-mcp/src/utils/nesting
                #:lisp-too-deep-p)
  (:import-from #:cl-mcp/src/utils/printing
                #:circular-p)
  (:import-from #:uiop
                #:directory-files
                #:ensure-directory-pathname
                #:ensure-pathname
                #:subdirectories)
  (:export #:*package-spec-discovery-cache*
           #:extract-in-package-name-from-text
           #:discover-package-spec
           #:call-with-package-context
           #:call-with-file-package-context))

(in-package #:cl-mcp/src/package-context)

(defparameter *package-context-source-extensions*
  '("lisp" "lsp" "cl" "asd" "ros")
  "Source file extensions scanned for package definitions.")

(defparameter *package-context-skip-directories*
  '(".git" ".qlot" "qlot" ".cache" ".bundle-libs" "bundle-libs")
  "Directory basenames skipped during package spec discovery.

Includes qlot's vendored bundle output under BOTH spellings: current qlot
bundles to `.bundle-libs`, while `bundle-libs` was its pre-1.0 spelling
and legacy trees still carry it. Note qlot's own traversal-exclusion list
names the undotted directory (src/utils/asdf.lisp), so both are real.

Skipping them is a large win, and the cost of not doing so recurs: a cold
discovery over an 832-file bundled tree measured 620 ms without these two
entries and 1 ms with them, and that is paid on every discovery for a
file whose package is not yet loaded in the parent.

The trade-off is that a basename matches at any depth, so a user-authored
directory with one of these names would have the packages defined in it
silently hidden. That is accepted here because `bundle-libs` names
vendored output by strong convention; a more general name such as
`software` is deliberately NOT listed for the same reason.

This list is an optimization, not the correctness mechanism. Tolerating a
file we cannot READ is the catch-all clause in %PACKAGE-SPECS-IN-FILE's
job -- no name list can enumerate every vendoring convention.")

(defvar *package-spec-discovery-cache* nil
  "NIL, or an EQUAL hash-table in which DISCOVER-PACKAGE-SPEC memoizes its walk.

Without it, every lookup for a package the parent image lacks reads the header
forms of the project's source files until one defines the package -- all of
them when none does -- so a batch of lookups costs files looked up times files
in the project.  Bind this to a fresh table around such a batch over a tree
that does not change during it; CL-MCP/SRC/CODE-REFS-SCAN:SCAN-PROJECT does so
around its file loop.  Entries are keyed on the package name and the walk's
root directory, so one table never answers for a different root, and the
looked-up file's own definitions are still read on every call.

The default NIL disables memoizing, so lisp-read-file, lisp-edit-form and the
other callers see a package definition edited between two calls.")

(defstruct package-spec
  "Minimal package metadata needed to reconstruct reader context."
  (name nil :type (or null string))
  (nicknames nil :type list)
  (use nil :type list)
  (local-nicknames nil :type list)
  source-path
  source-form)

(defstruct synthesized-package-context
  "Temporary package objects created for a single reader operation."
  root-package
  (created-packages nil :type list))

(defun %designator-name (designator)
  "Return DESIGNATOR normalized to an uppercase package-name string."
  (typecase designator
    (null nil)
    (string (string-upcase designator))
    (symbol (string-upcase (symbol-name designator)))
    (t nil)))

(defun %definition-head-p (head)
  "Return T when HEAD names a package definition form we can interpret."
  (and (symbolp head)
       (member (symbol-name head) '("DEFPACKAGE" "DEFINE-PACKAGE")
               :test #'string=)))

(defun %in-package-head-p (head)
  "Return T when HEAD names an IN-PACKAGE form."
  (and (symbolp head)
       (string= (symbol-name head) "IN-PACKAGE")))

(defun %header-form-p (form)
  "Return T when FORM is part of the package header section."
  (and (consp form)
       (let ((head (car form)))
         (or (%definition-head-p head)
             (%in-package-head-p head)))))

(defun %source-file-p (pathname)
  "Return T when PATHNAME looks like a Lisp source file worth scanning."
  (let ((type (pathname-type pathname)))
    (and type
         (member (string-downcase type) *package-context-source-extensions*
                 :test #'string=))))

(defun %directory-basename (pathname)
  "Return the last directory component of PATHNAME, or NIL."
  (let ((dir (pathname-directory (ensure-directory-pathname pathname))))
    (when (consp dir)
      (let ((last (car (last dir))))
        (when (stringp last)
          last)))))

(defun %skip-directory-p (pathname)
  "Return T when PATHNAME should be skipped during recursive scans."
  (let ((base (%directory-basename pathname)))
    (and base
         (member base *package-context-skip-directories* :test #'string=))))

(defun %collect-source-files (root)
  "Return a list of source files under ROOT, recursively."
  (labels ((walk (dir)
             (let ((results nil))
               (dolist (entry (directory-files dir))
                 (when (%source-file-p entry)
                   (push entry results)))
               (dolist (subdir (subdirectories dir))
                 (unless (%skip-directory-p subdir)
                   (setf results
                         (nconc results
                                (walk (ensure-directory-pathname subdir))))))
               results)))
    (walk (ensure-directory-pathname root))))

(defun %skip-dispatch-digits (stream)
  "STREAM is just past a #: read the decimal digits of a dispatch's numeric
argument, if any, and return the sub-character after them without consuming
it, or NIL at the end of STREAM."
  (loop for char = (peek-char nil stream nil nil)
        while (and char (digit-char-p char))
        do (read-char stream)
        finally (return char)))

(defun %skip-to-next-form (stream)
  "Advance STREAM past whitespace and comments (; and #|...|#, nested, with or
without a numeric argument, which SBCL ignores) to the next form's first
character, and return that character, or NIL at the end.  STREAM must accept
FILE-POSITION, as a string stream does."
  (loop
    (let ((char (peek-char t stream nil nil)))
      (cond
        ((null char) (return nil))
        ((char= char #\;) (read-line stream nil nil))
        ((char= char #\#)
         (let ((start (file-position stream)))
           (read-char stream)
           (unless (eql (%skip-dispatch-digits stream) #\|)
             (file-position stream start)
             (return char)))
         (read-char stream)
         ;; Stop right after the closing |#: the next character may be the
         ;; form's own first one.
         (loop with depth = 1
               for c = (read-char stream nil nil)
               while c
               do (cond ((and (char= c #\|) (eql (peek-char nil stream nil nil) #\#))
                         (read-char stream)
                         (when (zerop (decf depth)) (return)))
                        ((and (char= c #\#) (eql (peek-char nil stream nil nil) #\|))
                         (read-char stream)
                         (incf depth)))))
        (t (return char))))))

(defun %refusing (standard)
  "Return a reader macro function for *HEADER-READTABLE* and
*FEATURE-READTABLE* that stands for STANDARD, the standard readtable's
function for the same syntax: a READER-ERROR, unless *READ-SUPPRESS* is on --
inside a #+/#- branch that does not hold -- where STANDARD runs, since it then
builds nothing and consumes exactly what the reader would (a whole #(...),
say)."
  (lambda (stream &rest arguments)
    (if *read-suppress*
        (apply standard stream arguments)
        (error 'reader-error :stream stream))))

(defun %make-header-readtable (kept)
  "Return the standard readtable with backquote and every # dispatch whose
sub-character is not in KEPT made a refusal (%REFUSING), and with #+/#-
replaced by %HEADER-CONDITIONAL (named, so it may be defined later)."
  (let ((standard (copy-readtable nil))
        (readtable (copy-readtable nil)))
    (set-macro-character #\` (%refusing (get-macro-character #\` standard))
                         nil readtable)
    (loop for code from 0 below 128
          for char = (code-char code)
          for function = (get-dispatch-macro-character #\# char standard)
          unless (or (digit-char-p char) (find char kept) (null function))
            do (set-dispatch-macro-character #\# char (%refusing function) readtable))
    (set-dispatch-macro-character #\# #\+ '%header-conditional readtable)
    (set-dispatch-macro-character #\# #\- '%header-conditional readtable)
    readtable))

(defvar *header-labels* nil
  "While the scan reads one header form whole, a hash table from a label's
number to the object #n= gave it, or to *LABEL-BEING-READ* until that object
is read; NIL elsewhere.")

(defvar *label-being-read* (list :label-being-read)
  "The value *HEADER-LABELS* holds for a label whose object is still being
read.  A fresh list, so no object read from the text is EQ to it.")

(defun %header-label-define (stream sub-char argument)
  "The #n= of *HEADER-READTABLE*: read the next object and record it under N.
Unlike SBCL's, it does not walk the object afterwards to replace references
to the label inside it -- that walk recurses along the object's cdrs, and a
labelled clause of 200000 symbols exhausted the stack -- so a reference made
while the object is being read is an error (%HEADER-LABEL-REFERENCE): a
header form cannot become circular.  Under *READ-SUPPRESS* it returns no
value and reads nothing, as SBCL's does."
  (declare (ignore sub-char))
  (cond
    ;; SBCL's yields nothing here and leaves the object to the enclosing
    ;; read, so a suppressed (a #1=) still closes.
    (*read-suppress* (values))
    ((or (null argument) (null *header-labels*)
         (nth-value 1 (gethash argument *header-labels*)))
     ;; No number, no form being read whole, or a label defined twice.
     (error 'reader-error :stream stream))
    (t
     (setf (gethash argument *header-labels*) *label-being-read*)
     (setf (gethash argument *header-labels*) (read stream t nil t)))))

(defun %header-label-reference (stream sub-char argument)
  "The #n# of *HEADER-READTABLE*: the object #n= recorded under N.  A label not
defined, or whose object is still being read -- a reference that would make
the form circular -- is an error.  NIL under *READ-SUPPRESS*, as SBCL's."
  (declare (ignore sub-char))
  (if *read-suppress*
      nil
      (multiple-value-bind (object found)
          (if (and argument *header-labels*)
              (gethash argument *header-labels*)
              (values nil nil))
        (when (or (not found) (eq object *label-being-read*))
          (error 'reader-error :stream stream))
        object)))

(defparameter *header-readtable*
  (let ((readtable (%make-header-readtable "|:\\+-=#BbOoXxRr")))
    (set-dispatch-macro-character #\# #\= '%header-label-define readtable)
    (set-dispatch-macro-character #\# #\# '%header-label-reference readtable)
    readtable)
  "The readtable for a header form the scan reads whole.  Of the # dispatches
it keeps #|, #:, #\\, #+, #-, the labels #n= and #n# (Eclector's defpackage
shares a clause through one) and the radix integers #B, #O, #X and #R
(DEFPACKAGE's (:size #x20)); backquote and every other # dispatch are refused
(%REFUSING).  A header form needs none of them -- IN-PACKAGE's and
DEFPACKAGE's arguments are not evaluated -- and those that walk what they read
(backquote, #(, #A, #C, #S) ran forever on valid circular text such as
`(a . #1=(b . #1#)), or asked for a huge vector (#n().  The labels are the
scan's own (%HEADER-LABEL-DEFINE): SBCL's #n= walks the object it labels,
recursing along its cdrs, so a long labelled clause exhausted the stack.  A
radix integer is a token: it builds nothing to walk.  A dispatch
sub-character is matched in either case, so both are listed.")

(defparameter *feature-readtable* (%make-header-readtable "|:\\+-")
  "*HEADER-READTABLE* without labels, for a #+/#- feature expression: FEATUREP
walks it, and through labels it could be circular, or a tree of shared nodes
with 2^n paths that FEATUREP would walk one by one.")

(defun %header-conditional (stream sub-char argument)
  "The #+ and #- of *HEADER-READTABLE* and *SKIP-READTABLE*, with the standard
semantics -- the feature expression is read and tested even under
*READ-SUPPRESS*, and a test that fails yields no value -- except that the
expression is read with *FEATURE-READTABLE*, which has no labels: FEATUREP
walks it, and a label could make it circular or a shared tree of 2^n paths.
The semantics matter in a place that takes one object: '#-sbcl a (in-package
:p) quotes the in-package on SBCL, so it is no header.  An expression that
cannot be read so, or that FEATUREP rejects, is an error, which ends the scan:
whether the conditional yields an object decides where the enclosing form
ends, so no boundary after it could be trusted."
  (declare (ignore argument))
  (let ((feature (let ((*package* (find-package "KEYWORD"))
                       (*read-suppress* nil)
                       (*readtable* *feature-readtable*))
                   (read stream t nil t))))
    (if (eq (and (sb-int:featurep feature) t) (char= sub-char #\+))
        (read stream t nil t)
        (progn (let ((*read-suppress* t)) (read stream t nil t))
               (values)))))

(defparameter *skip-readtable*
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\+ '%header-conditional readtable)
    (set-dispatch-macro-character #\# #\- '%header-conditional readtable)
    readtable)
  "The standard readtable with the #+ and #- of *HEADER-READTABLE*, for the
scan's reads under *READ-SUPPRESS* (%SKIP-OBJECT).  SBCL's own #+ reads its
feature expression with *READ-SUPPRESS* off, even inside a form being skipped,
so a backquote there was read for real; %HEADER-CONDITIONAL reads it with
*FEATURE-READTABLE*, which refuses it, and still yields an object or none as
the reader does, so a skipped form ends where the reader ends it.")

(defun %judge-feature (stream sign)
  "Read the feature expression of a #+ (SIGN #\\+) or #- (SIGN #\\-) from
STREAM with *FEATURE-READTABLE* and judge it: :HOLDS when the conditional's
object is read, :FAILS when it is not, NIL when the expression cannot be read
so (end of input, a refused dispatch, a label) or FEATUREP rejects it.  STREAM
is past the expression unless the verdict is NIL; then where it is is unknown."
  (handler-case
      (let ((feature (let ((*package* (find-package "KEYWORD"))
                           (*readtable* *feature-readtable*)
                           (*read-suppress* nil)
                           (*read-eval* nil))
                       (read-preserving-whitespace stream t))))
        (if (eq (and (sb-int:featurep feature) t) (char= sign #\+))
            :holds
            :fails))
    (error () nil)))

(defun %skip-object (stream)
  "Move STREAM past its next object without building it: a read under
*READ-SUPPRESS* with *SKIP-READTABLE*, whose #+ and #- are judged, so the
object ends where the reader ends it -- a conditional that does not hold
yields nothing and the read takes the object after it, as the reader's does.
Signal an error at the end of STREAM, or at a conditional inside that cannot
be judged (see %HEADER-CONDITIONAL)."
  (let ((*readtable* *skip-readtable*)
        (*read-suppress* t))
    (read stream t nil)))

(defun %pass-conditionals (stream)
  "Pass the #+ and #- conditionals in front of the next top-level form on
STREAM as the reader would, and return what is there:

:FORM -- STREAM is at the first character of the form the reader returns
next.  Every conditional passed held, or failed in the place of one that held:
a failing one yields nothing, so the one that held takes the object after it
(#+sbcl #-sbcl a (in-package :p) is the in-package).
:TRANSPARENT -- an outermost conditional failed; it and its object were
passed, and to the reader there is no form here at all.
:UNJUDGED -- an outermost conditional's feature expression could not be
judged; it and its object were passed.  The object takes the same text whether
the conditional holds or not, so the boundary is sound, but whether there is a
form here is not known.
NIL -- the end of STREAM.

An error is signalled where no boundary after it could be trusted: a
conditional that cannot be judged in the place of one that held (which object
the outer one takes depends on the verdict), or one inside an object or a
feature expression being passed."
  (let ((held nil))
    (loop
      (let ((first (%skip-to-next-form stream))
            (start (file-position stream)))
        (unless first
          (return nil))
        (read-char stream)
        ;; #n+ is #+ to SBCL, which ignores the argument.
        (let ((sign (and (eql first #\#) (%skip-dispatch-digits stream))))
          (unless (member sign '(#\+ #\-))
            (file-position stream start)
            (return :form))
          (read-char stream)
          ;; LET evaluates in order: the position is taken before the judging.
          (let ((after-sign (file-position stream))
                (verdict (%judge-feature stream sign)))
            (case verdict
              (:holds (setf held t))
              (:fails
               (%skip-object stream)
               (unless held
                 (return :transparent)))
              (t
               (when held
                 (error "A conditional that cannot be judged stands in the ~
                         place of one that holds."))
               (file-position stream after-sign)
               (%skip-object stream)
               (%skip-object stream)
               (return :unjudged)))))))))

(defun %read-form-whole-p (stream)
  "Judge the form at STREAM's position, which no conditional precedes
(%PASS-CONDITIONALS passed them), for the header scan.  Return T when it may
be a header form and is to be read whole: a list whose first element is
written as a symbol that names a header form (IN-PACKAGE, DEFPACKAGE and the
like, see %HEADER-FORM-P).  Return NIL for any other form.  Only the head
symbol is read, never the rest of the form, and STREAM is left somewhere
inside it: the caller rewinds."
  (when (eql (%skip-to-next-form stream) #\()
    (read-char stream)
    (let ((char (%skip-to-next-form stream)))
      ;; A symbol token, not a list, a string, a quote or a dispatch -- so
      ;; (#+sbcl in-package ...), a header to the reader, is not one here.
      (and char
           (not (find char "()'`,\"#"))
           (let ((head (handler-case (read-preserving-whitespace stream nil nil)
                         (error () nil))))
             (and (%header-form-p (list head)) t))))))

(defun %read-header-forms-from-text (text)
  "Read package-related header forms from TEXT using the CL reader.
Stops after the first non-header top-level form once package header forms
have begun, to avoid descending into the rest of the file. Extraction is
best-effort: a read error or premature end of file (a malformed later form,
custom reader syntax) ends the scan and the forms read so far are returned,
so callers such as PARSE-TOP-LEVEL-FORMS can report the breakage themselves
instead of failing here first.  Text nested too deeply to read safely
(LISP-TOO-DEEP-P) yields no forms.
The #+ and #- in front of each top-level form are passed as the reader would
(%PASS-CONDITIONALS); a top-level form that a failing conditional removes is
not there, so it neither counts as a header nor ends the header section.
Only a form whose head is written as a header symbol is then read whole (see
%READ-FORM-WHOLE-P), with backquote an error (*HEADER-READTABLE*), and kept
only if it is not circular; every other form is skipped (%SKIP-OBJECT) with
*READ-SUPPRESS*, which builds nothing, and with #+/#- whose feature
expressions are read safely.  SBCL's reader never returns from some valid
text -- a backquote template with a circular tail, `(a . #1=(b . #1#)), whose
last cons it looks for -- and this scan runs on every file the Lisp-aware
tools open.  Skipping also means a form the reader would reject (an unknown
package prefix, #. or another dispatch it does not know) no longer ends the
scan before a header that follows it.  A conditional whose verdict decides
where a form ends, and that cannot be judged, does end it: no boundary after
it could be trusted."
  (when (lisp-too-deep-p text)
    (return-from %read-header-forms-from-text nil))
  (call-with-lenient-packages
   (lambda ()
     (let ((*read-eval* nil))
       (with-input-from-string (stream text)
         (let ((forms nil)
               (saw-header nil))
           (loop for kind = (handler-case (%pass-conditionals stream)
                              (error () nil))
                 for start = (file-position stream)
                 for header-p = (and (eq kind :form) (%read-form-whole-p stream))
                 for form = (case kind
                              ((nil) :eof)
                              (:transparent :transparent)
                              (:unjudged :skipped)
                              (t
                               (file-position stream start)
                               (handler-case
                                   (if header-p
                                       ;; Labels belong to one form, as
                                       ;; to the reader's top-level READ.
                                       (let ((*readtable* *header-readtable*)
                                             (*header-labels* (make-hash-table)))
                                         (read stream nil :eof))
                                       (progn (%skip-object stream) :skipped))
                                 (error () :eof))))
                 until (eq form :eof)
                 do (cond
                      ;; A conditional that does not hold is not there.
                      ((eq form :transparent))
                      ;; A circular header form is no header: the spec
                      ;; extraction walks its clauses.  The scan's own labels
                      ;; refuse to build one (%HEADER-LABEL-REFERENCE); the
                      ;; check stays as the guarantee.  Like any non-header
                      ;; after a header, it ends the scan.
                      ((and header-p (%header-form-p form) (not (circular-p form)))
                       (push form forms)
                       (setf saw-header t))
                      (saw-header
                       (return))))
           (nreverse forms)))))))

(defun %extract-in-package-name-from-forms (forms)
  "Return the first package name mentioned in FORMS' IN-PACKAGE declaration."
  (loop for form in forms
        when (and (consp form)
                  (%in-package-head-p (car form))
                  (consp (cdr form)))
          do (return (%designator-name (second form)))))

(defun extract-in-package-name-from-text (text)
  "Return the package named by the first IN-PACKAGE form in TEXT, or NIL."
  (%extract-in-package-name-from-forms (%read-header-forms-from-text text)))

(defun %normalize-local-nickname-entries (entries)
  "Convert local nickname ENTRIES into an alist of (nickname . package-name)."
  (let ((items (if (and (= (length entries) 1)
                        (listp (first entries))
                        (every #'listp (first entries)))
                   (first entries)
                   entries))
        (result nil))
    (dolist (entry items (nreverse result))
      (when (and (consp entry)
                 (consp (cdr entry)))
        (let ((nickname (%designator-name (first entry)))
              (target (%designator-name (second entry))))
          (when (and nickname target)
            (push (cons nickname target) result)))))))

(defun %extract-package-spec (form source-path)
  "Extract a PACKAGE-SPEC from package definition FORM, or NIL."
  (when (and (consp form)
             (%definition-head-p (car form))
             (consp (cdr form)))
    (let ((name (%designator-name (second form)))
          (nicknames nil)
          (use nil)
          (local-nicknames nil))
      (dolist (clause (cddr form))
        (when (and (consp clause) (symbolp (car clause)))
          (let ((key (symbol-name (car clause))))
            (cond
              ((string= key "NICKNAMES")
               (setf nicknames
                     (remove nil (mapcar #'%designator-name (cdr clause)))))
              ((string= key "USE")
               (setf use
                     (remove nil (mapcar #'%designator-name (cdr clause)))))
              ((string= key "LOCAL-NICKNAMES")
               (setf local-nicknames
                     (%normalize-local-nickname-entries (cdr clause))))))))
      (when name
        (make-package-spec
         :name name
         :nicknames nicknames
         :use use
         :local-nicknames local-nicknames
         :source-path source-path
         :source-form form)))))

(defun %package-spec-matches-p (spec package-name)
  "Return T when SPEC defines PACKAGE-NAME or one of its nicknames."
  (let ((target (%designator-name package-name)))
    (and target
         (or (string= (package-spec-name spec) target)
             (member target (package-spec-nicknames spec) :test #'string=)))))

(defun %package-specs-in-file (pathname)
  "Return a list of PACKAGE-SPEC values extracted from PATHNAME."
  (handler-case
      (let* ((text (fs-read-file pathname))
             (forms (%read-header-forms-from-text text))
             (specs nil))
        (dolist (form forms (nreverse specs))
          (let ((spec (%extract-package-spec form pathname)))
            (when spec
              (push spec specs)))))
    (reader-error ()
      nil)
    #+sbcl
    (sb-int:simple-reader-package-error ()
      nil)
    ;; Implementation-specific reader conditionals in vendored sources (e.g.
    ;; a bare Allegro #+(version>= 9) in bordeaux-threads' impl-allegro.lisp)
    ;; signal a plain SIMPLE-ERROR from CL:READ, not a READER-ERROR.
    (error ()
      nil)))

(defun %find-package-spec-under (root target source-path)
  "Return the first PACKAGE-SPEC for TARGET among the source files under ROOT, or NIL.
SOURCE-PATH, when given, is skipped: DISCOVER-PACKAGE-SPEC reads it first."
  (dolist (file (%collect-source-files root) nil)
    (unless (and source-path
                 (equal (probe-file file)
                        (probe-file (ensure-pathname source-path :want-pathname t))))
      (let ((match
              (find-if (lambda (spec)
                         (%package-spec-matches-p spec target))
                       (%package-specs-in-file file))))
        (when match
          (return match))))))

(defun discover-package-spec (package-name &key source-path)
  "Discover a PACKAGE-SPEC for PACKAGE-NAME from project source files.

SOURCE-PATH's own package definitions are checked first.  Then the source files
under *PROJECT-ROOT* (or SOURCE-PATH's directory when it is unset) are walked
until one defines PACKAGE-NAME.  When *PACKAGE-SPEC-DISCOVERY-CACHE* holds a
hash-table, the walk's result, NIL included, is memoized there per package name
and root directory."
  (let ((target (%designator-name package-name)))
    (when target
      (when source-path
        (let ((source (ensure-pathname source-path :want-pathname t)))
          (when (probe-file source)
            (let ((match
                    (find-if (lambda (spec)
                               (%package-spec-matches-p spec target))
                             (%package-specs-in-file source))))
              (when match
                (return-from discover-package-spec match))))))
      (let ((root (or *project-root*
                      (and source-path
                           (make-pathname :name nil :type nil
                                          :defaults (ensure-pathname source-path
                                                                     :want-pathname t)))))
            (cache *package-spec-discovery-cache*))
        (cond
          ((null root) nil)
          ((hash-table-p cache)
           ;; The walk skips SOURCE-PATH, yet its result does not depend on
           ;; it: the walk only runs once SOURCE-PATH was found not to define
           ;; TARGET, so skipping that file cannot change which file matches.
           ;; Keying on TARGET and ROOT alone lets every file of one package
           ;; share a single walk.
           (let ((key (cons target (namestring (ensure-directory-pathname root)))))
             (multiple-value-bind (spec present-p) (gethash key cache)
               (if present-p
                   spec
                   (setf (gethash key cache)
                         (%find-package-spec-under root target source-path))))))
          (t (%find-package-spec-under root target source-path)))))))

(defun %make-package-with-guard (name &key nicknames use)
  "Create package NAME with optional NICKNAMES and USE list."
  (handler-case
      (make-package name :use use :nicknames nicknames)
    (error (e)
      (error "Failed to synthesize package ~A in parent process: ~A"
             name e))))

(defun %ensure-package-object (name created-packages-cell)
  "Return package NAME, creating a temporary stub if needed."
  (or (find-package name)
      (let ((pkg (%make-package-with-guard name :use nil)))
        (push pkg (car created-packages-cell))
        pkg)))

(defun %safe-use-list (package-names)
  "Return USE entries whose packages already exist in the parent."
  (loop for name in package-names
        for pkg = (find-package name)
        when pkg
          collect (package-name pkg)))

(defun %synthesize-package-context (spec)
  "Create temporary packages needed to realize SPEC."
  (let ((created-packages-cell (list nil)))
    (let ((root
            (or (find-package (package-spec-name spec))
                (progn
                  (dolist (entry (package-spec-local-nicknames spec))
                    (%ensure-package-object (cdr entry) created-packages-cell))
                  (let ((pkg (%make-package-with-guard
                              (package-spec-name spec)
                              :nicknames (package-spec-nicknames spec)
                              :use (%safe-use-list (package-spec-use spec)))))
                    (push pkg (car created-packages-cell))
                    #+sbcl
                    (dolist (entry (package-spec-local-nicknames spec))
                      (sb-ext:add-package-local-nickname
                       (car entry)
                       (%ensure-package-object (cdr entry) created-packages-cell)
                       pkg))
                    #-sbcl
                    (when (package-spec-local-nicknames spec)
                      (error "Package-local nickname synthesis requires SBCL"))
                    pkg)))))
      (make-synthesized-package-context
       :root-package root
       :created-packages (nreverse (car created-packages-cell))))))

(defun %cleanup-synthesized-package-context (context)
  "Delete temporary packages created for CONTEXT. Before deletion, the
symbols that each package currently owns are recorded as becoming
homeless-due-to-teardown so downstream display code can normalize
them without affecting genuinely uninterned symbols."
  (dolist (pkg (reverse (synthesized-package-context-created-packages context)))
    (let ((name (ignore-errors (package-name pkg))))
      (when (and name (find-package name))
        (ignore-errors (record-homeless-on-teardown pkg))
        (ignore-errors (delete-package pkg))))))

(defun call-with-package-context (package-name thunk &key source-path)
  "Call THUNK with PACKAGE-NAME available as the current package context.
If PACKAGE-NAME is absent in the parent image, attempt to reconstruct it from
project source and bind *PACKAGE* to the synthesized package while THUNK runs."
  (let ((name (%designator-name package-name)))
    (cond
      ((null name)
       (call-with-lenient-packages thunk))
      ((find-package name)
       (let ((*package* (find-package name)))
         (call-with-lenient-packages thunk)))
      (t
       (let ((spec (discover-package-spec name :source-path source-path)))
         (if (null spec)
             (call-with-lenient-packages thunk)
             (let ((context (%synthesize-package-context spec)))
               (unwind-protect
                    (let ((*package* (synthesized-package-context-root-package context)))
                      (call-with-managed-packages
                       (synthesized-package-context-created-packages context)
                       thunk))
                 (%cleanup-synthesized-package-context context)))))))))

(defun call-with-file-package-context (file-path thunk &key text)
  "Call THUNK with the package context implied by FILE-PATH's IN-PACKAGE form."
  (let ((file-text
          (or text
              (fs-read-file file-path))))
    (call-with-package-context
     (extract-in-package-name-from-text file-text)
     thunk
     :source-path file-path)))
