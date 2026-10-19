;;;; src/code-refs-core.lisp
;;;;
;;;; Worker-side half of code-find-references' impact analysis.  It turns a
;;;; symbol as written into a symbol without interning anything, decides
;;;; which of the parent's source-scan sites really name that symbol in this
;;;; image, and merges those sites with SBCL's xref entries.  It reads no
;;;; files and does not depend on eclector, so the worker image stays free of
;;;; the parent's parsing stack.

(defpackage #:cl-mcp/src/code-refs-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/helpers
                #:arg-validation-error
                #:make-ht
                #:json-bool)
  (:export #:%method-specializer-keys
           #:sequence->list
           #:parse-symbol-text
           #:parse-target-designator
           #:find-package-named
           #:resolve-target
           #:qualified-symbol-name
           #:symbol-kind
           #:resolve-site-token
           #:resolve-scan-forms
           #:macros-expanding-to
           #:add-macro-reached-references
           #:merge-references
           #:build-references-report
           #:place-references-in-source
           #:%status-string
           #:*note-stale*))

(in-package #:cl-mcp/src/code-refs-core)

(defun sequence->list (sequence)
  "Return SEQUENCE as a list.
A JSON array is a vector when the parent's scan is used in-process and a list
after it crosses the worker's JSON parser; everything that walks scan data
goes through this so the two paths cannot diverge."
  (coerce (or sequence '()) 'list))

(defun parse-symbol-text (text)
  "Split TEXT, a symbol as it would be written in source, without reading it.

Returns (values NAME PACKAGE-PART PROBLEM).  NAME and PACKAGE-PART follow the
standard reader: unescaped characters are upcased; a backslash escapes the one
character after it, inside |...| as well as outside, and is itself dropped;
other characters inside |...| are kept as written; and one or two colons
separate the package from the name, unless escaped either way.
PACKAGE-PART is NIL for an unqualified symbol and
\"KEYWORD\" for :NAME.  When TEXT is not a symbol name, NAME and PACKAGE-PART
are NIL and PROBLEM is a sentence saying why."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))
         (length (length text))
         (buffer (make-string-output-stream))
         (package-part nil)
         (first-marker nil)
         (marker-end nil)
         (in-bars nil)
         (i 0))
    (flet ((fail (control &rest args)
             (return-from parse-symbol-text
               (values nil nil (apply #'format nil control args)))))
      (when (zerop length)
        (fail "symbol must be a non-empty string"))
      (when (and (> length 1) (string= "#:" (subseq text 0 2)))
        (fail "~A is an uninterned symbol; it has no references to find" text))
      (loop while (< i length)
            do (let ((ch (char text i)))
                 ;; Clause order follows CLHS 2.4.8.1: a single escape wins over
                 ;; a multiple escape, so a backslash quotes the next character
                 ;; inside |...| as well as outside it.
                 (cond
                   ((char= ch #\\)
                    (incf i)
                    (when (>= i length)
                      (fail "~A ends with an escaping backslash" text))
                    (write-char (char text i) buffer))
                   ((char= ch #\|)
                    (setf in-bars (not in-bars)))
                   (in-bars
                    (write-char ch buffer))
                   ((char= ch #\:)
                    (cond
                      ((null package-part)
                       (setf package-part (get-output-stream-string buffer)
                             first-marker i
                             marker-end (1+ i)))
                      ((and (= i marker-end) (= i (1+ first-marker)))
                       (setf marker-end (1+ i)))
                      (t
                       (fail "~A has more than one package marker" text))))
                   (t
                    (write-char (char-upcase ch) buffer))))
               (incf i))
      (when in-bars
        (fail "~A has an unterminated |" text))
      (let ((name (get-output-stream-string buffer)))
        (when (zerop (length name))
          (fail "~A has no symbol name" text))
        (values name
                (cond ((null package-part) nil)
                      ((zerop (length package-part)) "KEYWORD")
                      (t package-part))
                nil)))))

(defun parse-target-designator (text)
  "Return (values NAME PACKAGE-PART) for TEXT, the symbol a caller asked about.
Signals ARG-VALIDATION-ERROR naming the \"symbol\" argument when TEXT is not a
symbol name, or is a keyword, which no code references in the xref sense."
  (multiple-value-bind (name package-part problem) (parse-symbol-text text)
    (cond
      (problem
       (error 'arg-validation-error :arg-name "symbol" :message problem))
      ((equal package-part "KEYWORD")
       (error 'arg-validation-error
              :arg-name "symbol"
              :message (format nil "~A is a keyword; keywords have no references to find"
                               text)))
      (t (values name package-part)))))

(defun find-package-named (name)
  "Return the package NAME designates, trying NAME as given and then upcased.
FIND-PACKAGE consults the package-local nicknames of *PACKAGE*, so bind
*PACKAGE* to the package the name was written in before calling this."
  (and (stringp name)
       (plusp (length name))
       (or (find-package name)
           (find-package (string-upcase name)))))

(defun resolve-target (text &key package)
  "Resolve TEXT to a symbol using FIND-PACKAGE and FIND-SYMBOL only.

PACKAGE is used for an unqualified TEXT and defaults to COMMON-LISP-USER; its
package-local nicknames apply to a qualified one.  It may be a name, a symbol
(its name is used, so :CL-MCP means \"CL-MCP\"), a package object or NIL.  A
single colon is accepted for an internal symbol: the question is where a
symbol is used, not whether it is exported.

Returns (values SYMBOL STATUS PACKAGE-NAME NAME), STATUS being :FOUND,
:NOT-FOUND or :PACKAGE-NOT-FOUND.  PACKAGE-NAME and NAME say where the lookup
happened, for the message shown when it fails.  Nothing is interned."
  (multiple-value-bind (name package-part) (parse-target-designator text)
    (let* ((package (typecase package
                      (null nil)
                      (package (package-name package))
                      (symbol (symbol-name package))
                      (t package)))
           (given (and (stringp package) (plusp (length package)) package))
           (context (or (and given (find-package-named given))
                        (find-package "COMMON-LISP-USER")))
           (home (cond (package-part
                        (let ((*package* context))
                          (find-package-named package-part)))
                       (given (find-package-named given))
                       (t context))))
      (if (null home)
          (values nil :package-not-found (or package-part given) name)
          (multiple-value-bind (symbol status) (find-symbol name home)
            (if status
                (values symbol :found (package-name home) name)
                (values nil :not-found (package-name home) name)))))))

(defun qualified-symbol-name (symbol)
  "Return SYMBOL's name qualified the way a reader outside its package needs:
PKG:NAME when external, PKG::NAME when internal, :NAME for a keyword and
#:NAME for an uninterned symbol.  The package's primary name is used."
  (let ((package (symbol-package symbol))
        (name (symbol-name symbol)))
    (cond
      ((null package) (format nil "#:~A" name))
      ((eq package (find-package "KEYWORD")) (format nil ":~A" name))
      (t (format nil "~A~A~A"
                 (package-name package)
                 (if (eq (nth-value 1 (find-symbol name package)) :external) ":" "::")
                 name)))))

(defun symbol-kind (symbol)
  "Return a word for what SYMBOL names in this image, most specific first:
special-operator, macro, generic-function, function, constant, variable or
unbound."
  (cond
    ((special-operator-p symbol) "special-operator")
    ((macro-function symbol) "macro")
    ((and (fboundp symbol) (typep (fdefinition symbol) 'generic-function))
     "generic-function")
    ((fboundp symbol) "function")
    ((constantp symbol) "constant")
    ((or (boundp symbol)
         (eq (sb-int:info :variable :kind symbol) :special))
     "variable")
    (t "unbound")))

(defun resolve-site-token (token in-package target)
  "Decide whether TOKEN, written where IN-PACKAGE was current, names TARGET.

IN-PACKAGE is the designator of the IN-PACKAGE in effect at the site; NIL means
COMMON-LISP-USER.  Package-local nicknames of that package apply to a
qualified TOKEN.  Returns :MATCH, :OTHER (TOKEN names another symbol, or none),
or :UNRESOLVED with the missing package's name as a second value when the site
cannot be judged because a package does not exist in this image.  Nothing is
interned."
  (multiple-value-bind (name package-part problem) (parse-symbol-text token)
    (if problem
        :other
        (let* ((home-name (or in-package "COMMON-LISP-USER"))
               (home (find-package-named home-name)))
          (if (null home)
              (values :unresolved home-name)
              (let ((package (if package-part
                                 (let ((*package* home))
                                   (find-package-named package-part))
                                 home)))
                (if (null package)
                    (values :unresolved package-part)
                    (multiple-value-bind (symbol status) (find-symbol name package)
                      (if (and status (eq symbol target)) :match :other)))))))))

(defparameter *note-stale* "file changed since load; reload for accurate results"
  "Note for a reference whose file was written after its code was compiled.")

(defparameter *note-stale-reference*
  (concatenate 'string *note-stale*
               " (a definition deleted from the file stays listed until a fresh worker)")
  "Note for a reference in a file written after its code was compiled: its form
is met by its caller's name (%STALE-ENTRY-FORM), or not at all, and a caller
deleted from the file is still in this worker's image, so a reload cannot drop
it.")

(defparameter *note-macro-expansion*
  "call not visible in source (produced by a macro expansion)"
  "Note for an xref entry whose form in a scanned file holds no matching site.")

(defparameter *note-parse-failed* "file could not be parsed; call sites unavailable"
  "Note for an xref entry in a file the source scan could not parse.")

(defparameter *note-not-scanned* "source not scanned; call sites unavailable"
  "Note for an xref entry in a file the source scan did not cover.")

(defparameter *note-not-in-xref*
  (concatenate 'string
               "not in xref (top-level form, code compiled while loading such as "
               "a FiveAM test, or not compiled since it was written)")
  "Note for a form only the source scan found, holding a site of a kind xref
records (see *SITE-KINDS-XREF-SELDOM-RECORDS*).  Code COMPILEd while its file
loads -- a FiveAM test's body -- is in xref, but with no source location, so
it cannot meet its form.")

(defparameter *xref-type-site-kinds*
  '(("call" "call" "function")
    ("macro" "macro")
    ("bind" "bind")
    ("set" "set")
    ("reference" "reference" "set"))
  "For each xref entry type, the scan site kinds that show it in the source.
A call is written (name ...) or through a designator, #'name or (funcall 'name
...), which the scan calls \"function\".  A reference is also shown by a \"set\"
site: INCF, DECF, POP, PUSH and PUSHNEW read the place they write, and SBCL
records both a reference and a set for it.  Every other type has its own kind.
A merged form holding no site of a kind listed for one of its xref types gets
*NOTE-UNMATCHED-XREF* (see %REFERENCE-NOTE); a type missing from this table is
shown by no site.")

(defparameter *site-kinds-xref-seldom-records* '("quoted" "template" "method")
  "Scan site kinds xref usually does not record: quoted data, backquote templates
and DEFMETHOD names.  (WHO-CALLS does record a function passed by name, as in
(mapcar 'name xs) or :key 'name, but a compiled form like that is then found by
xref too, and so is not source-only.)  A form the scan alone found gets
*NOTE-NOT-IN-XREF* only when it holds a site of some other kind; for these the
absence is expected, and the note's explanation (a top-level form, or code not
compiled) would be wrong.")

(defparameter *note-unmatched-xref*
  "xref records ~{~A~#[~; and ~:;, ~]~} here that no site below ~
   shows as ~:[one~;such~]~@[ (~{~A~^; ~})~]"
  "FORMAT control for the note on a form both xref and the scan found, when one or
more of its xref types has no compatible site (see *XREF-TYPE-SITE-KINDS*).  Its
arguments are the types' phrases, whether there is more than one, and their
distinct explanations, all from *UNMATCHED-XREF-WORDING*.  The sites are still
listed, each with its own kind, but none of them is that call, set, ... itself.")

(defparameter *unmatched-xref-wording*
  '(("call" "a call" "a macro expansion, or a function passed by name")
    ("macro" "a macro use" "for example inside another macro's expansion")
    ("bind" "a binding" "for example inside a macro's expansion")
    ("set" "a set" "for example inside a macro such as rotatef or multiple-value-setq")
    ("reference" "a reference" "for example inside a macro's expansion"))
  "For each xref entry type, the phrase *NOTE-UNMATCHED-XREF* names it with and
the likely reason no site shows it.  A type missing here is named as it is
spelled, with no reason.")

(defun resolve-scan-forms (forms target &key macro-p)
  "Keep the scan sites in FORMS that name TARGET in this image.

FORMS are the parent's form objects (CL-MCP/SRC/CODE-REFS-SCAN:SCAN-TEXT).
Returns (values RESOLVED UNRESOLVED).

RESOLVED lists, in input order, one plist per form that kept a site:
  (:truename :path :index :start-line :end-line :form-type :form-name
   :test-name :test-framework :context :in-package :sites)
with :SITES plists (:line :column :kind :context :shadowed-by).  When MACRO-P a
\"call\" site becomes \"macro\", the type WHO-MACROEXPANDS entries carry.

UNRESOLVED lists plists (:path :package :count :tests), one per file and
missing package, counting the sites that could not be judged."
  (let ((resolved '())
        (unresolved '()))
    (dolist (form (sequence->list forms))
      (let ((kept '()))
        (dolist (site (sequence->list (gethash "sites" form)))
          (multiple-value-bind (verdict missing)
              (resolve-site-token (gethash "token" site) (gethash "in_package" form) target)
            (case verdict
              (:match
               (push (list :line (gethash "line" site)
                           :column (gethash "column" site)
                           :kind (let ((kind (gethash "kind" site)))
                                   (if (and macro-p (equal kind "call")) "macro" kind))
                           :context (gethash "context" site)
                           :shadowed-by (gethash "shadowed_by" site))
                     kept))
              (:unresolved
               (let ((entry (find-if (lambda (entry)
                                       (and (equal (getf entry :path) (gethash "path" form))
                                            (equal (getf entry :package) missing)))
                                     unresolved))
                     (test (gethash "test_name" form)))
                 (unless entry
                   (setf entry (list :path (gethash "path" form) :package missing
                                     :count 0 :tests '()))
                   (push entry unresolved))
                 (incf (getf entry :count))
                 (when (and test (not (member test (getf entry :tests) :test #'equal)))
                   (setf (getf entry :tests) (append (getf entry :tests) (list test)))))))))
        (when kept
          (push (list :truename (gethash "abs_path" form)
                      :path (gethash "path" form)
                      :index (gethash "index" form)
                      :start-line (gethash "start_line" form)
                      :end-line (gethash "end_line" form)
                      :form-type (gethash "form_type" form)
                      :form-name (gethash "form_name" form)
                      :test-name (gethash "test_name" form)
                      :test-framework (gethash "test_framework" form)
                      :context (gethash "context" form)
                      :in-package (gethash "in_package" form)
                      :sites (nreverse kept))
                resolved))))
    (values (nreverse resolved) (nreverse unresolved))))

(defun macros-expanding-to (forms)
  "Return the qualified names of the macros whose expansion writes the target.

FORMS are RESOLVE-SCAN-FORMS' first value.  A DEFMACRO form holding a
\"template\" site names the target inside a backquote, so every use of that
macro expands into a reference to it.  Those uses never write the target's
name, and xref records them only when the code is compiled from a file it can
name: a FiveAM test, whose body is compiled while its fasl loads, keeps no
source location at all.  The macro's name is resolved in the package its
DEFMACRO was read in; one that does not resolve to a macro in this image is
left out.  Names are distinct, in the order the forms came."
  (let ((names '()))
    (dolist (form forms (nreverse names))
      (when (and (equal (getf form :form-type) "defmacro")
                 (stringp (getf form :form-name))
                 (some (lambda (site) (equal (getf site :kind) "template"))
                       (getf form :sites)))
        (multiple-value-bind (symbol status)
            (resolve-target (getf form :form-name) :package (getf form :in-package))
          (when (and (eq status :found) (macro-function symbol))
            (pushnew (qualified-symbol-name symbol) names :test #'equal)))))))

(defun %distinct (strings)
  "Return STRINGS without duplicates, keeping the first occurrence of each."
  (let ((seen '()))
    (dolist (string strings (nreverse seen))
      (unless (member string seen :test #'equal)
        (push string seen)))))

(defun %site->ht (site)
  "Return the JSON object for a resolved SITE plist."
  (make-ht "line" (getf site :line)
           "column" (getf site :column)
           "kind" (getf site :kind)
           "context" (getf site :context)
           "shadowed_by" (getf site :shadowed-by)))

(defun %compatible-site-p (type sites)
  "True when SITES, resolved site plists, hold a site of a kind that
*XREF-TYPE-SITE-KINDS* lists for the xref entry type TYPE."
  (let ((kinds (cdr (assoc type *xref-type-site-kinds* :test #'equal))))
    (and (some (lambda (site) (member (getf site :kind) kinds :test #'equal)) sites)
         t)))

(defun %unmatched-xref-note (types)
  "Return *NOTE-UNMATCHED-XREF* naming TYPES, the xref types no site shows, or NIL
when TYPES is empty."
  (when types
    (let ((wordings (mapcar (lambda (type)
                              (or (cdr (assoc type *unmatched-xref-wording* :test #'equal))
                                  (list type nil)))
                            types)))
      (format nil *note-unmatched-xref*
              (mapcar #'first wordings)
              (rest types)
              (%distinct (remove nil (mapcar #'second wordings)))))))

(defun %reference-note (form xrefs primary stale)
  "Return the note explaining a reference built from FORM and XREFS, or NIL.
PRIMARY is the xref entry the reference takes its caller from.

In order: a stale file; an xref entry with no form, explained by how the scan
covered its file; a form the scan alone found that holds a site of a kind xref
records (one whose sites are all *SITE-KINDS-XREF-SELDOM-RECORDS* is expected
to be missing from xref); and a form both found where some xref type has no
compatible site (see *XREF-TYPE-SITE-KINDS*), so that no site listed is that
call, set, ... itself."
  (cond
    (stale *note-stale-reference*)
    ((null form)
     (case (getf primary :scan-status)
       (:scanned *note-macro-expansion*)
       (:parse-failed *note-parse-failed*)
       (t *note-not-scanned*)))
    ((null xrefs)
     (let ((sites (getf form :sites)))
       (if (and sites
                (every (lambda (site)
                         (member (getf site :kind) *site-kinds-xref-seldom-records*
                                 :test #'equal))
                       sites))
           nil
           *note-not-in-xref*)))
    (t
     (let ((sites (getf form :sites)))
       (%unmatched-xref-note
        (remove-if (lambda (type) (%compatible-site-p type sites))
                   (%distinct (mapcar (lambda (entry) (getf entry :type)) xrefs))))))))

(defun %reference (form xrefs &optional stale-file)
  "Return the reference object for one top-level form.
FORM is a resolved scan form or NIL; XREFS are the xref entries in that form,
in finder order, possibly none.  A named caller is preferred over a lambda
when several xref entries share the form.  STALE-FILE says FORM's file was
written after its code was compiled: a form there that no xref entry met is
stale too, rather than missing from xref -- its entry may be one that could
not be met with it."
  (let* ((primary (or (find-if (lambda (entry) (getf entry :caller-symbol)) xrefs)
                      (first xrefs)))
         (sites (and form (getf form :sites)))
         (types (%distinct (if xrefs
                               (mapcar (lambda (entry) (getf entry :type)) xrefs)
                               (mapcar (lambda (site) (getf site :kind)) sites))))
         (stale (or (some (lambda (entry) (getf entry :stale)) xrefs)
                    (and (null xrefs) stale-file))))
    (make-ht "path" (if form (getf form :path) (getf primary :path))
             "abs_path" (if form (getf form :truename) (getf primary :truename))
             "line" (if form (getf form :start-line) (getf primary :line))
             "type" (first types)
             "types" (coerce types 'vector)
             "caller" (if primary
                          (getf primary :caller)
                          (or (getf form :form-name) ""))
             "caller_symbol" (and primary (getf primary :caller-symbol))
             "context" (if form (getf form :context) (getf primary :context))
             "form_type" (and form (getf form :form-type))
             "form_name" (and form (getf form :form-name))
             "origin" (cond ((and form xrefs) "xref+source")
                            (xrefs "xref")
                            (t "source"))
             "call_sites" (map 'vector #'%site->ht sites)
             "test" (and form
                         (getf form :test-name)
                         (make-ht "name" (getf form :test-name)
                                  "framework" (getf form :test-framework)))
             "stale" (json-bool stale)
             "note" (%reference-note form xrefs primary stale))))

(defun %span-contains-p (form line)
  "True when LINE lies within FORM's :START-LINE and :END-LINE, both included."
  (let ((start (getf form :start-line))
        (end (getf form :end-line)))
    (and (integerp line) (integerp start) (integerp end)
         (<= start line end))))

(defun %symbol-text-name (text)
  "Return the name part of TEXT, a symbol written PKG::NAME, PKG:NAME or NAME,
in lower case, or NIL for NIL."
  (when text
    (string-downcase (subseq text (let ((colon (position #\: text :from-end t)))
                                    (if colon (1+ colon) 0))))))

(defun %name-tokens (text)
  "Return TEXT split at whitespace and parentheses into lower-case tokens, each
without a package prefix, a leading colon, or a quote."
  (loop for token in (uiop:split-string (string-downcase text)
                                        :separator '(#\Space #\Tab #\Newline #\( #\)))
        for name = (string-left-trim ":'" (or (%symbol-text-name token) ""))
        when (plusp (length name))
          collect name))

(defun %list-elements (text start)
  "Return the top-level elements of the list whose ( is at START in TEXT, each
as its text, in order, or NIL when there is no ( at START or it is not closed."
  (when (and (< start (length text)) (char= (char text start) #\())
    (let ((elements '()) (depth 0) (element-start nil))
      (loop for i from start below (length text)
            for ch = (char text i)
            do (cond ((char= ch #\()
                      (when (and (= depth 1) (null element-start))
                        (setf element-start i))
                      (incf depth))
                     ((char= ch #\))
                      (decf depth)
                      (cond ((zerop depth)
                             (when element-start
                               (push (subseq text element-start i) elements))
                             (return-from %list-elements (nreverse elements)))
                            ((and (= depth 1) element-start)
                             (push (subseq text element-start (1+ i)) elements)
                             (setf element-start nil))))
                     ((member ch '(#\Space #\Tab #\Newline))
                      (when (and (= depth 1) element-start)
                        (push (subseq text element-start i) elements)
                        (setf element-start nil)))
                     ((and (= depth 1) (null element-start))
                      (setf element-start i))))
      nil)))

(defun %method-caller-signature (caller)
  "Return (values QUALIFIERS SPECIALIZERS) for CALLER, an xref caller written as
(defmethod name before (class t)): its qualifier names as %NAME-TOKENS gives
them, and its specializers in order, each as the token list of its text (T as
(\"t\"), (eql :a) as (\"eql\" \"a\")).  Both NIL, with a third value NIL, for a
caller that is no method."
  (when (and (stringp caller)
             (eql 0 (search "(defmethod " caller :test #'char-equal)))
    (let ((inner (position #\( caller :start 1)))
      (when inner
        (values (rest (%name-tokens (subseq caller (length "(defmethod ") inner)))
                (mapcar #'%name-tokens (%list-elements caller inner))
                t)))))

(defun %form-name-specializers (form-name)
  "Return the specializers of FORM-NAME, a defmethod's form name such as
\"name ((x class) y)\", in the shape %METHOD-CALLER-SIGNATURE gives them: one
token list per required parameter, (\"t\") for one not specialized."
  (let ((open (position #\( form-name)))
    (when open
      (loop for parameter in (%list-elements form-name open)
            until (char= (char parameter 0) #\&)
            collect (if (char= (char parameter 0) #\()
                        (let ((parts (%list-elements parameter 0)))
                          (if (second parts) (%name-tokens (second parts)) (list "t")))
                        (list "t"))))))

(defun %specializer-key (specializer)
  "Return the key %STALE-ENTRY-FORM compares SPECIALIZER, a method specializer as
SBCL names it -- a class name or (EQL datum) --, by: (:CLASS PACKAGE NAME) for a
symbol, so a class is never taken for its namesake in another package, and
(:EQL . tokens) for an EQL datum, its printed text as %NAME-TOKENS splits it."
  (cond ((and (consp specializer) (symbolp (first specializer))
              (string= (symbol-name (first specializer)) "EQL"))
         (cons :eql (%name-tokens (let ((*package* (find-package "KEYWORD")))
                                    (prin1-to-string (second specializer))))))
        ((and (symbolp specializer) (symbol-package specializer))
         (list :class (package-name (symbol-package specializer)) (symbol-name specializer)))
        (t (list :unknown (princ-to-string specializer)))))

(defun %method-specializer-keys (caller-name)
  "Return the specializer keys (%SPECIALIZER-KEY) of CALLER-NAME, an xref caller
as SBCL gives it -- (SB-PCL::FAST-METHOD NAME QUALIFIER... (SPECIALIZER...)) or
(:METHOD ...) --, or NIL when it is no method."
  (when (and (consp caller-name) (symbolp (first caller-name))
             (member (symbol-name (first caller-name))
                     '("FAST-METHOD" "SLOW-METHOD" "METHOD") :test #'string=))
    (let ((specializers (first (last caller-name))))
      (and (listp specializers) (mapcar #'%specializer-key specializers)))))

(defun %form-name-specializer-keys (form-name in-package)
  "Return the specializer keys of FORM-NAME, a defmethod's form name, its class
names looked up in IN-PACKAGE, the package the form is read in -- with
FIND-SYMBOL, nothing interned.  A name that is not found gets a key no
specializer has.  NIL when FORM-NAME has no lambda list."
  (let ((open (position #\( form-name)))
    (when open
      (loop for parameter in (%list-elements form-name open)
            until (char= (char parameter 0) #\&)
            collect (let* ((parts (and (char= (char parameter 0) #\()
                                       (%list-elements parameter 0)))
                           (text (if parts (second parts) "t")))
                      (cond ((null text) (list :class "COMMON-LISP" "T"))
                            ((char= (char text 0) #\()
                             (let ((inner (%list-elements text 0)))
                               (if (equalp (first inner) "eql")
                                   (cons :eql (%name-tokens (or (second inner) "")))
                                   (list :unknown text))))
                            (t
                             (multiple-value-bind (symbol status)
                                 (ignore-errors
                                  (resolve-target text :package (or in-package "CL-USER")))
                               (if (eq status :found)
                                   (%specializer-key symbol)
                                   (list :unresolved text))))))))))

(defparameter *non-function-definers*
  '("defclass" "defstruct" "define-condition" "deftype" "defvar" "defparameter"
    "defconstant" "defpackage" "in-package")
  "Form types that define no function, so no xref caller is one of them.")

(defun %stale-entry-form (entry forms)
  "Return the one form among FORMS, those of ENTRY's file, that ENTRY's caller
is, or NIL when none or several are.  A method is met only with a defmethod of
its name whose qualifiers and specializers, in order, are its own; any other
caller with a form of its name that defines a function.
An entry of a file written after its code was compiled cannot be met by index or
line, which name a different form once forms were inserted above or removed."
  (let ((name (%symbol-text-name (getf entry :caller-symbol))))
    (when name
      (multiple-value-bind (qualifiers specializers method-p)
          (%method-caller-signature (getf entry :caller))
        (let ((named
                (remove-if-not
                 (lambda (form)
                   (let* ((form-name (getf form :form-name))
                          (tokens (and (stringp form-name) (%name-tokens form-name)))
                          (form-type (string-downcase (or (getf form :form-type) ""))))
                     (and (equal name (first tokens))
                          (if method-p
                              (and (equal form-type "defmethod")
                                   ;; The qualifiers sit between the name and
                                   ;; the lambda list.
                                   (equal qualifiers
                                          (%name-tokens
                                           (subseq form-name
                                                   (or (position #\Space form-name)
                                                       (length form-name))
                                                   (or (position #\( form-name)
                                                       (length form-name)))))
                                   ;; In order, parameter by parameter: (A B)
                                   ;; is not (B A), and a parameter named like
                                   ;; a class is no specializer.
                                   ;; By symbol where both sides can say it:
                                   ;; A:NODE is not B:NODE.
                                   (if (and (getf entry :specializer-keys)
                                            (getf form :in-package))
                                       (equal (getf entry :specializer-keys)
                                              (%form-name-specializer-keys
                                               form-name (getf form :in-package)))
                                       (equal specializers
                                              (%form-name-specializers form-name))))
                              (not (member form-type *non-function-definers*
                                           :test #'string=))))))
                 forms)))
          (and named (null (rest named)) (first named)))))))

(defun merge-references (xref-entries forms)
  "Merge XREF-ENTRIES with resolved scan FORMS into reference objects.

XREF-ENTRIES are plists (:type :caller :caller-symbol :truename :path :line
:context :form-index :stale :scan-status), :SCAN-STATUS being :SCANNED,
:PARSE-FAILED or :NOT-SCANNED.  FORMS are RESOLVE-SCAN-FORMS' first value.

Entries and forms meet on (truename, top-level form index), keyed
(:FORM truename index).  SBCL's DEFINITION-SOURCE-FORM-PATH starts with the
index of the top-level form among those the reader returned, which is the
form's position among the file's :EXPR nodes -- reader conditionals, EVAL-WHEN
and PROGN included.  The character offset is no key: it is an octet position,
just past the PREVIOUS form.

The index is only trusted when the entry's :LINE lies within the span
(:START-LINE to :END-LINE) of the form with that index.  The parent counts
top-level forms with its own *FEATURES*, which need not be the compiling
image's: a feature the loaded system pushes, or one only the worker has, makes
the parent skip a #+feature form SBCL counted, so every later index in that
file is shifted and would otherwise meet the NEXT form.  When the span check
fails, the entry meets the form of the same truename whose span holds its
line; when no form does, it is unmatched.  An unmatched entry, like one without
a form path, groups on its line, keyed (:LINE truename line).

None of that holds for a :STALE entry, one of a file written after its code was
compiled: forms inserted above it or removed move both its index and its line
onto a neighbour.  It meets the form named after its caller instead
(%STALE-ENTRY-FORM), or none, and, when every entry of the file is stale -- an
edit not reloaded -- every other form of that file is stale too.  A stale entry
of a file that also has fresh ones is a definition the last reload did not
compile again, one no longer in the file: it is no reference, and is returned as
the second value, for the report to mention.

An entry meets its form whatever sites the form holds.  When none of them shows
the entry's type -- a call made by a macro expansion or through a function
passed by name beside a quoted name, say -- the reference says so in its note
(see %REFERENCE-NOTE and *XREF-TYPE-SITE-KINDS*) rather than being split, since
splitting would turn common code such as (mapcar 'name xs) into a false
macro-expansion reference.

Returns JSON-ready hash-tables, one per top-level form, in first-seen order."
  (let ((seen (make-hash-table :test #'equal))
        (form-by-key (make-hash-table :test #'equal))
        (forms-by-truename (make-hash-table :test #'equal))
        (xrefs-by-key (make-hash-table :test #'equal))
        (stale-files (make-hash-table :test #'equal))
        (fresh-files (make-hash-table :test #'equal))
        (gone-entries '())
        (order '()))
    (flet ((remember (key)
             (unless (gethash key seen)
               (setf (gethash key seen) t)
               (push key order))))
      (dolist (form forms)
        (let ((key (list :form (getf form :truename) (getf form :index))))
          (remember key)
          (setf (gethash key form-by-key) form)
          (push form (gethash (getf form :truename) forms-by-truename))))
      ;; Staleness is per definition: a file reloaded since an edit has fresh
      ;; entries, and a stale one there is a definition the reload did not
      ;; compile again -- one no longer in the file.
      (dolist (entry xref-entries)
        (unless (getf entry :stale)
          (setf (gethash (getf entry :truename) fresh-files) t)))
      (dolist (entry xref-entries)
        (let* ((truename (getf entry :truename))
               (line (getf entry :line))
               (index (getf entry :form-index))
               (indexed (and index (gethash (list :form truename index) form-by-key)))
               (stale (getf entry :stale))
               (gone (and stale (gethash truename fresh-files)))
               (form (cond
                       (gone nil)
                       ;; A caller with a name is met by it; a lambda, a (setf f)
                       ;; or a macro function has none, and is met by index and
                       ;; span as before, which an edit above it can mislead.
                       ((and stale (getf entry :caller-symbol))
                        (setf (gethash truename stale-files) t)
                        (%stale-entry-form entry (gethash truename forms-by-truename)))
                       (index
                        (when stale
                          (setf (gethash truename stale-files) t))
                        (if (and indexed (%span-contains-p indexed line))
                            indexed
                            (find-if (lambda (form) (%span-contains-p form line))
                                     (gethash truename forms-by-truename))))))
               (key (if form
                        (list :form truename (getf form :index))
                        (list :line truename line))))
          (if gone
              (push entry gone-entries)
              (progn
                (remember key)
                (push entry (gethash key xrefs-by-key)))))))
    (values
     (mapcar (lambda (key)
               (let ((form (gethash key form-by-key)))
                 (%reference form
                             (reverse (gethash key xrefs-by-key))
                             (and form (gethash (getf form :truename) stale-files)))))
             (reverse order))
     (nreverse gone-entries))))

(defun %reference< (a b)
  "Order references by path, then by line."
  (let ((path-a (gethash "path" a))
        (path-b (gethash "path" b)))
    (if (string= path-a path-b)
        (< (or (gethash "line" a) 0) (or (gethash "line" b) 0))
        (string< path-a path-b))))

(defun %tests-of (refs)
  "Return one JSON object (name, path, line) per distinct test among REFS.
A test known only from a macro use xref could not confirm (origin \"macro\")
also carries may_reach true, unless another reference confirms it."
  (let ((tests '()))
    (dolist (ref refs (nreverse tests))
      (let ((test (gethash "test" ref))
            (maybe (equal (gethash "origin" ref) "macro")))
        (when test
          (let ((seen (find-if (lambda (seen)
                                 (and (equal (gethash "name" seen) (gethash "name" test))
                                      (equal (gethash "path" seen) (gethash "path" ref))))
                               tests)))
            (cond
              ((null seen)
               (let ((entry (make-ht "name" (gethash "name" test)
                                     "path" (gethash "path" ref)
                                     "line" (gethash "line" ref))))
                 (when maybe
                   (setf (gethash "may_reach" entry) t))
                 (push entry tests)))
              ((not maybe)
               (remhash "may_reach" seen)))))))))

(defun %status-string (status)
  "Return the JSON spelling of a RESOLVE-TARGET status keyword."
  (ecase status
    (:found "found")
    (:not-found "not_found")
    (:package-not-found "package_not_found")))

(defun build-references-report (&key symbol resolved-symbol (status :found) kind
                                  lookup-package lookup-name project-only (limit 50)
                                  refs unresolved notes (xref-count 0)
                                  (files-scanned 0) (name-matches 0) scan-skipped
                                  via-macros names-class)
  "Return the code-find-references payload, everything but its content text.

REFS are MERGE-REFERENCES' objects.  They are sorted by path and line and at
most LIMIT are kept, while count, file_count and tests describe all of them.
A LIMIT of NIL keeps every one, for PLACE-REFERENCES-IN-SOURCE to cut later.
UNRESOLVED is RESOLVE-SCAN-FORMS' second value and NOTES are plain sentences.
VIA-MACROS is MACROS-EXPANDING-TO's list, for ADD-MACRO-REACHED-REFERENCES, and
NAMES-CLASS whether the symbol also names a class, which xref never records.
docs/tools.md describes every field."
  (let* ((sorted (sort (copy-list refs) #'%reference<))
         (count (length sorted))
         (limit (or limit count)))
    ;; A call no site shows names the macros that may have made it: the project
    ;; macros whose expansion writes the symbol.
    (when via-macros
      (dolist (ref sorted)
        (when (equal *note-macro-expansion* (gethash "note" ref))
          (setf (gethash "note" ref)
                (format nil "call not visible in source (produced by a macro expansion, ~
                             possibly of ~{~A~^ or ~}, whose expansion names it)"
                        via-macros)))))
    (make-ht "symbol" symbol
             "resolved_symbol" resolved-symbol
             "symbol_status" (%status-string status)
             "symbol_kind" kind
             "lookup_package" lookup-package
             "lookup_name" lookup-name
             "count" count
             "file_count" (length (%distinct (mapcar (lambda (ref) (gethash "path" ref))
                                                     sorted)))
             "limit" limit
             "truncated" (json-bool (> count limit))
             "project_only" (json-bool project-only)
             "refs" (coerce (subseq sorted 0 (min count limit)) 'vector)
             "tests" (coerce (%tests-of sorted) 'vector)
             "unresolved" (map 'vector
                               (lambda (entry)
                                 (make-ht "path" (getf entry :path)
                                          "package" (getf entry :package)
                                          "count" (getf entry :count)
                                          "tests" (coerce (getf entry :tests) 'vector)))
                               unresolved)
             "notes" (coerce notes 'vector)
             "xref_count" xref-count
             "files_scanned" files-scanned
             "name_matches" name-matches
             "scan_skipped" scan-skipped
             "via_macros" (coerce via-macros 'vector)
             "names_class" (json-bool names-class))))

(defun %true-json-p (value)
  "True when VALUE, a JSON boolean as read here, is true: T or YASON:TRUE.
A false arrives as NIL, :FALSE or YASON:FALSE depending on how it was parsed."
  (or (eq value t)
      (and (symbolp value) value (string= (symbol-name value) "TRUE"))))

(defun %reference-key (ref)
  "Return the key telling REF's form apart from every other: its file, the line
it starts on, and its form_type and form_name -- two forms can start on one
line."
  (list (gethash "abs_path" ref) (gethash "line" ref)
        (gethash "form_type" ref) (gethash "form_name" ref)))

(defparameter *xref-decided-kinds* '("function" "generic-function" "macro" "variable")
  "Symbol kinds (SYMBOL-KIND) whose uses xref records: calls, macroexpansions,
and a special variable's bindings, references and sets.  For any other -- a
class name, a constant the compiler may inline, an unbound symbol -- xref's
silence about a form says nothing.")

(defun %unconfirmed-macro-use-p (ref xref-decides-p)
  "True when REF, a reference to a macro, is a use of the global macro that
nothing else confirms or rules out as reaching the target.

When XREF-DECIDES-P (the target's kind is in *XREF-DECIDED-KINDS*), a use xref
located (origin xref or xref+source) is left to xref: that form's code was
compiled from its file, so the target's own xref already says whether the
expansion reaches the target there -- and a template naming the target shows
only that some expansion may.  Otherwise xref cannot say, and a located use
counts as well.  Either way only a site of kind macro that no flet, labels or
macrolet shadows is a use of the global macro; a quoted name or a backquote
template expands nothing."
  (and (or (not xref-decides-p)
           (equal (gethash "origin" ref) "source"))
       (some (lambda (site)
               (and (equal (gethash "kind" site) "macro")
                    (null (gethash "shadowed_by" site))))
             (sequence->list (gethash "call_sites" ref)))))

(defun add-macro-reached-references (report macro-name macro-report)
  "Add to REPORT the forms that may reach its symbol only by using MACRO-NAME.
Returns REPORT, changed in place.

REPORT and MACRO-REPORT are BUILD-REFERENCES-REPORT payloads built with a LIMIT
of NIL and already placed (PLACE-REFERENCES-IN-SOURCE), the second about
MACRO-NAME, one of REPORT's via_macros.  Each form using the macro expands
into a reference to REPORT's symbol without writing its name, so the source
scan never meets it, and xref meets it only when its code was compiled from a
file: a FiveAM test's body is compiled while its fasl loads and keeps no source
location.  Such a form is added with type \"via-macro\", origin \"macro\", a
note naming the macro, and the macro's own call sites of kind macro, which say
where the expansion happens.

Only an unshadowed use nothing else decides counts (%UNCONFIRMED-MACRO-USE-P):
a quoted name or a macrolet's local macro expands nothing of MACRO-NAME's, and
for a target whose uses xref records (*XREF-DECIDED-KINDS*) a use xref located
is decided by the target's own xref.  A template naming the symbol shows only that some expansion may reach it
-- (if flag `(target) `(other)) -- so each added form says \"may reach\", not
that it does.  A form REPORT already holds -- the same file,
line, form_type and form_name (%REFERENCE-KEY), since two forms can start on
one line -- is left as it is, and so is the macro's own DEFMACRO.

count and file_count are recomputed; tests, the order and LIMIT are left to
PLACE-REFERENCES-IN-SOURCE, which runs after this."
  (let ((refs (sequence->list (gethash "refs" report)))
        (seen (make-hash-table :test #'equal))
        (short-name (string-downcase
                     (subseq macro-name (1+ (or (position #\: macro-name :from-end t) -1)))))
        (note (format nil "may reach ~A through macro ~A, whose expansion names it ~
                           (nothing confirms this use does)"
                      (gethash "resolved_symbol" report) macro-name))
        ;; A name that is also a class (DEFCLASS POINT beside DEFUN POINT)
        ;; can be used as one in the template, which xref never records.
        (xref-decides-p (and (member (gethash "symbol_kind" report) *xref-decided-kinds*
                                     :test #'equal)
                             (not (%true-json-p (gethash "names_class" report)))))
        (added '()))
    (dolist (ref refs)
      (setf (gethash (%reference-key ref) seen) t))
    ;; In a file changed since load, a use xref located no longer meets its
    ;; source form and looks source-only, so it can come out twice, or as may
    ;; reach where xref ruled it out.  A FiveAM test there is still known only
    ;; from the source, so the forms are kept, flagged stale and told to
    ;; reload, rather than dropped without a word.
    (dolist (ref (sequence->list (gethash "refs" macro-report)))
      (when (and (%true-json-p (gethash "stale" ref))
                 (stringp (gethash "abs_path" ref)))
        (setf (gethash (list :stale (gethash "abs_path" ref)) seen) t)))
    (dolist (ref (sequence->list (gethash "refs" macro-report)))
      (let ((key (%reference-key ref))
            (stale (gethash (list :stale (gethash "abs_path" ref)) seen)))
        (unless (or (gethash key seen)
                    (not (%unconfirmed-macro-use-p ref xref-decides-p))
                    (and (equal (gethash "form_type" ref) "defmacro")
                         (string-equal (gethash "form_name" ref) short-name)))
          (setf (gethash key seen) t)
          (let ((copy (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k copy) v)) ref)
            (setf (gethash "type" copy) "via-macro"
                  (gethash "types" copy) (vector "via-macro")
                  (gethash "origin" copy) "macro"
                  (gethash "via_macro" copy) macro-name
                  (gethash "call_sites" copy)
                  (remove "macro" (coerce (or (gethash "call_sites" ref) #()) 'vector)
                          :key (lambda (site) (gethash "kind" site)) :test-not #'equal)
                  (gethash "stale" copy) (if stale t (gethash "stale" ref))
                  (gethash "note" copy) (if stale
                                            (format nil "~A; ~A" note *note-stale*)
                                            note))
            (push copy added)))))
    (when added
      (let ((all (sort (append refs (nreverse added)) #'%reference<)))
        (setf (gethash "refs" report) (coerce all 'vector)
              (gethash "count" report) (length all)
              (gethash "file_count" report)
              (length (%distinct (mapcar (lambda (ref) (gethash "path" ref)) all))))))
    report))

(defun place-references-in-source (report forms-at &key limit)
  "Name the top-level form each unplaced reference in REPORT sits in, then keep
at most LIMIT references.  Returns REPORT, changed in place.

REPORT is BUILD-REFERENCES-REPORT's payload built with a LIMIT of NIL, so it
holds every reference.  A reference is unplaced when no scanned form met its
xref entry: the call exists only in a macro expansion, inside a form that never
writes the symbol's name -- a test using a macro that calls the function, say
-- or in a file the scan did not parse.  Its line is then the line that form
starts on, which SBCL's source path records.

FORMS-AT is called once per file as (FORMS-AT abs-path lines) and returns a
table from each line to the forms starting on it, plists carrying :FORM-TYPE,
:FORM-NAME, :TEST-NAME and :TEST-FRAMEWORK, as
CL-MCP/SRC/CODE-REFS-SCAN:TOP-LEVEL-FORMS-AT does.  It runs where the source can
be parsed, which this image may not be.  A reference is placed only when
exactly one form starts on its line; otherwise it is left as it was, since a
guessed form_name would be passed straight to lisp-edit-form.  A stale
reference -- its file changed after it was compiled -- is never placed: its
line was worked out from the compiled form's position among the file's forms
as they are NOW, so after an edit it can name a different form, a test the
symbol has nothing to do with, say.  It keeps its reload note instead.

A placed reference gains form_type, form_name and, for a test form, test; its
note still says the call is not visible in source.  tests is recomputed over
every reference before LIMIT cuts the list, as BUILD-REFERENCES-REPORT counts
it, and limit and truncated are set for the list that remains."
  (let ((refs (sequence->list (gethash "refs" report)))
        (by-file (make-hash-table :test #'equal)))
    (dolist (ref refs)
      (when (and (null (gethash "form_type" ref))
                 (not (member (gethash "stale" ref) '(t yason:true)))
                 (stringp (gethash "abs_path" ref))
                 (integerp (gethash "line" ref)))
        (push ref (gethash (gethash "abs_path" ref) by-file))))
    (maphash
     (lambda (abs-path file-refs)
       (let ((table (funcall forms-at abs-path
                             (mapcar (lambda (ref) (gethash "line" ref)) file-refs))))
         (dolist (ref file-refs)
           (let ((forms (gethash (gethash "line" ref) table)))
             (when (and (= 1 (length forms)) (getf (first forms) :form-type))
               (let ((form (first forms)))
                 (setf (gethash "form_type" ref) (getf form :form-type)
                       (gethash "form_name" ref) (getf form :form-name))
                 (when (getf form :test-name)
                   (setf (gethash "test" ref)
                         (make-ht "name" (getf form :test-name)
                                  "framework" (getf form :test-framework))))))))))
     by-file)
    (let* ((count (length refs))
           (limit (or limit count)))
      (setf (gethash "tests" report) (coerce (%tests-of refs) 'vector)
            (gethash "refs" report) (coerce (subseq refs 0 (min count limit)) 'vector)
            (gethash "limit" report) limit
            (gethash "truncated" report) (json-bool (> count limit))))
    report))
