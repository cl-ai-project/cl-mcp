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
  (:export #:sequence->list
           #:parse-symbol-text
           #:parse-target-designator
           #:find-package-named
           #:resolve-target
           #:qualified-symbol-name
           #:symbol-kind
           #:resolve-site-token
           #:resolve-scan-forms
           #:merge-references
           #:build-references-report
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
standard reader: unescaped characters are upcased, characters inside |...| or
after a backslash are kept as written, and one or two colons separate the
package from the name.  PACKAGE-PART is NIL for an unqualified symbol and
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
                 (cond
                   ((char= ch #\|)
                    (setf in-bars (not in-bars)))
                   (in-bars
                    (write-char ch buffer))
                   ((char= ch #\\)
                    (incf i)
                    (when (>= i length)
                      (fail "~A ends with an escaping backslash" text))
                    (write-char (char text i) buffer))
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

(defparameter *note-macro-expansion*
  "call not visible in source (produced by a macro expansion)"
  "Note for an xref entry whose form in a scanned file holds no matching site.")

(defparameter *note-parse-failed* "file could not be parsed; call sites unavailable"
  "Note for an xref entry in a file the source scan could not parse.")

(defparameter *note-not-scanned* "source not scanned; call sites unavailable"
  "Note for an xref entry in a file the source scan did not cover.")

(defparameter *note-not-in-xref*
  "not in xref (top-level form, or not compiled since it was written)"
  "Note for a form only the source scan found, holding a site of a kind xref
records (see *SITE-KINDS-XREF-SELDOM-RECORDS*).")

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
   :test-name :test-framework :context :sites)
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
                      :sites (nreverse kept))
                resolved))))
    (values (nreverse resolved) (nreverse unresolved))))

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
    (stale *note-stale*)
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

(defun %reference (form xrefs)
  "Return the reference object for one top-level form.
FORM is a resolved scan form or NIL; XREFS are the xref entries in that form,
in finder order, possibly none.  A named caller is preferred over a lambda
when several xref entries share the form."
  (let* ((primary (or (find-if (lambda (entry) (getf entry :caller-symbol)) xrefs)
                      (first xrefs)))
         (sites (and form (getf form :sites)))
         (types (%distinct (if xrefs
                               (mapcar (lambda (entry) (getf entry :type)) xrefs)
                               (mapcar (lambda (site) (getf site :kind)) sites))))
         (stale (some (lambda (entry) (getf entry :stale)) xrefs)))
    (make-ht "path" (if form (getf form :path) (getf primary :path))
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
      (dolist (entry xref-entries)
        (let* ((truename (getf entry :truename))
               (line (getf entry :line))
               (index (getf entry :form-index))
               (indexed (and index (gethash (list :form truename index) form-by-key)))
               (form (and index
                          (if (and indexed (%span-contains-p indexed line))
                              indexed
                              (find-if (lambda (form) (%span-contains-p form line))
                                       (gethash truename forms-by-truename)))))
               (key (if form
                        (list :form truename (getf form :index))
                        (list :line truename line))))
          (remember key)
          (push entry (gethash key xrefs-by-key)))))
    (mapcar (lambda (key)
              (%reference (gethash key form-by-key)
                          (reverse (gethash key xrefs-by-key))))
            (reverse order))))

(defun %reference< (a b)
  "Order references by path, then by line."
  (let ((path-a (gethash "path" a))
        (path-b (gethash "path" b)))
    (if (string= path-a path-b)
        (< (or (gethash "line" a) 0) (or (gethash "line" b) 0))
        (string< path-a path-b))))

(defun %tests-of (refs)
  "Return one JSON object (name, path, line) per distinct test among REFS."
  (let ((tests '()))
    (dolist (ref refs (nreverse tests))
      (let ((test (gethash "test" ref)))
        (when (and test
                   (not (find-if (lambda (seen)
                                   (and (equal (gethash "name" seen) (gethash "name" test))
                                        (equal (gethash "path" seen) (gethash "path" ref))))
                                 tests)))
          (push (make-ht "name" (gethash "name" test)
                         "path" (gethash "path" ref)
                         "line" (gethash "line" ref))
                tests))))))

(defun %status-string (status)
  "Return the JSON spelling of a RESOLVE-TARGET status keyword."
  (ecase status
    (:found "found")
    (:not-found "not_found")
    (:package-not-found "package_not_found")))

(defun build-references-report (&key symbol resolved-symbol (status :found) kind
                                  lookup-package lookup-name project-only (limit 50)
                                  refs unresolved notes (xref-count 0)
                                  (files-scanned 0) (name-matches 0) scan-skipped)
  "Return the code-find-references payload, everything but its content text.

REFS are MERGE-REFERENCES' objects.  They are sorted by path and line and at
most LIMIT are kept, while count, file_count and tests describe all of them.
UNRESOLVED is RESOLVE-SCAN-FORMS' second value and NOTES are plain sentences.
docs/tools.md describes every field."
  (let* ((sorted (sort (copy-list refs) #'%reference<))
         (count (length sorted)))
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
             "scan_skipped" scan-skipped)))
