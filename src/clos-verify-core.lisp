;;;; src/clos-verify-core.lisp
;;;;
;;;; Worker-side resolution and three-valued judgment for clos-describe's
;;;; source matching (design spec 3.1-3.4): given each report entry's
;;;; structured identity (task 1, src/clos-core.lisp) and the parent's
;;;; candidate source signatures (task 2, src/code-refs-scan.lisp, as JSON --
;;;; see VERIFY-ENTRIES's docstring for the exact shape), resolves every name
;;;; with FIND-PACKAGE and FIND-SYMBOL only and judges each candidate
;;;; MATCHED, MISMATCHED or UNVERIFIED.  Never interns, evaluates or
;;;; macroexpands; loads no eclector, so the worker image stays free of the
;;;; parent's parsing stack.

(defpackage #:cl-mcp/src/clos-verify-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-refs-core
                #:parse-symbol-text
                #:sequence->list)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:export #:verify-entries))

(in-package #:cl-mcp/src/clos-verify-core)

(defun %get (ht key)
  "Return the value of KEY in HT, or NIL when HT is not a hash-table.
Guards every optional nested JSON object this file reads, so a missing or
malformed field never signals an error: it flows through as NIL toward
UNVERIFIED instead of a crash."
  (and (hash-table-p ht) (gethash key ht)))

(defun %tag= (value tag)
  "True when VALUE, an untrusted JSON field, is the string TAG.  Never
signals for a VALUE that is not a string, unlike STRING= on a non-designator."
  (and (stringp value) (string= value tag)))

(defun %true-p (value)
  "True when VALUE is a genuine JSON true, not merely non-NIL.  JSON-BOOL's
false value is the symbol YASON:FALSE, a non-NIL object that (AND value T)
would misread as true.  A boolean only crosses an actual JSON wire when the
worker pool is enabled; with MCP_NO_WORKER_POOL=1 (src/run.lisp)
VERIFY-ENTRIES is called in-process on a live CLOS-DESCRIBE-REPORT identity
whose \"setf\" field can still be YASON:FALSE, never having been encoded and
parsed back into plain NIL."
  (and value (not (eq value 'yason:false))))

(defun %as-list (value)
  "Return VALUE, an untrusted JSON field expected to be an array, as a list
via SEQUENCE->LIST, or NIL when VALUE is not list- or vector-shaped (a
string included, since a JSON string is never this field's array shape) --
so a malformed field never aborts the whole batch with a type error; it
just carries no elements to judge."
  (and (or (listp value) (and (vectorp value) (not (stringp value))))
       (sequence->list value)))

(defun %find-package-named (name)
  "Return the package NAME designates, trying NAME as given and then
upcased -- CL-MCP/SRC/CODE-REFS-CORE:FIND-PACKAGE-NAMED's own logic, kept
local so this file's imports stop at PARSE-SYMBOL-TEXT and SEQUENCE->LIST."
  (and (stringp name)
       (plusp (length name))
       (or (find-package name)
           (find-package (string-upcase name)))))

(defun %resolve-token (token in-package)
  "Resolve TOKEN, source text for a symbol written where IN-PACKAGE was the
current package (or COMMON-LISP-USER when NIL), to the symbol it names,
using FIND-PACKAGE and FIND-SYMBOL only.  Returns (values SYMBOL T) when
TOKEN parses as a symbol name and FIND-SYMBOL knows it in the resulting
package, or (values NIL NIL) for a malformed token, a package this image
does not have, or a name FIND-SYMBOL does not know -- never a new symbol."
  (multiple-value-bind (name package-part problem) (parse-symbol-text token)
    (if problem
        (values nil nil)
        (let* ((home-name (or in-package "COMMON-LISP-USER"))
               (home (%find-package-named home-name)))
          (if (null home)
              (values nil nil)
              (let ((package (if package-part
                                  (let ((*package* home))
                                    (%find-package-named package-part))
                                  home)))
                (if (null package)
                    (values nil nil)
                    (multiple-value-bind (symbol status) (find-symbol name package)
                      (if status (values symbol t) (values nil nil))))))))))

(defun %resolve-identity-symbol (identity)
  "Return (values SYMBOL T) for IDENTITY, a {package, name} structured
identity (spec 3.2), resolved via FIND-PACKAGE and FIND-SYMBOL only, or
(values NIL NIL) when IDENTITY is absent, its package is null, does not
exist in this image, or FIND-SYMBOL does not know the name."
  (let ((package-name (%get identity "package"))
        (name (%get identity "name")))
    (if (or (null package-name) (null name))
        (values nil nil)
        (let ((package (%find-package-named package-name)))
          (if (null package)
              (values nil nil)
              (multiple-value-bind (symbol status) (find-symbol name package)
                (if status (values symbol t) (values nil nil))))))))

(defun %compare-symbol-token (token identity)
  "Compare TOKEN, a {token, in_package} source token (spec 3.2), against
IDENTITY, a {package, name} structured identity, resolving both with
FIND-PACKAGE/FIND-SYMBOL only and comparing with EQ.  Returns (values
:MATCHED NIL), (values :MISMATCHED reason) or (values :UNVERIFIED reason)."
  (multiple-value-bind (source-symbol source-ok)
      (%resolve-token (%get token "token") (%get token "in_package"))
    (multiple-value-bind (target-symbol target-ok) (%resolve-identity-symbol identity)
      (cond
        ((or (not source-ok) (not target-ok))
         (values :unverified "a name could not be resolved to a symbol in this image"))
        ((eq source-symbol target-symbol) (values :matched nil))
        (t (values :mismatched "the resolved symbols are not the same"))))))

(defun %compare-name (source-name identity)
  "Compare SOURCE-NAME, a {token, setf, in_package} source name (spec 3.2),
against IDENTITY, a {package, name, setf} function-name identity, or a
{package, name} class identity when SETF cannot apply -- a missing \"setf\"
key on either side reads as false, and so does YASON:FALSE (%TRUE-P), since
an in-process caller's boolean may not have crossed a JSON wire at all.  A
SETF mismatch is conclusive on its own and is reported before the base name
is even resolved."
  (cond
    ((null source-name) (values :unverified "this form's name could not be located"))
    ((null identity) (values :unverified "this identity has no name to match"))
    ((not (eq (%true-p (%get source-name "setf")) (%true-p (%get identity "setf"))))
     (values :mismatched "(setf ...) status differs"))
    (t (%compare-symbol-token source-name identity))))

(defmacro %mv-cons (form)
  "Evaluate FORM, which returns (values STATUS REASON), as (STATUS . REASON)
-- shorthand for building %COMBINE's input list."
  (let ((status (gensym "STATUS")) (reason (gensym "REASON")))
    `(multiple-value-bind (,status ,reason) ,form (cons ,status ,reason))))

(defun %combine (results)
  "Combine RESULTS, a list of (STATUS . REASON) pairs judging one
candidate's or one inline method's components (spec 3.1's tail rule): any
:MISMATCHED wins, else any :UNVERIFIED, else :MATCHED with a NIL reason."
  (let ((mismatched (find :mismatched results :key #'car))
        (unverified (find :unverified results :key #'car)))
    (cond
      (mismatched (values :mismatched (cdr mismatched)))
      (unverified (values :unverified (cdr unverified)))
      (t (values :matched nil)))))

(defun %compare-qualifiers (source identity)
  "Compare SOURCE, a candidate's list of {token, in_package} qualifier
tokens, against IDENTITY, a method identity's list of {package, name}
qualifier identities, pairwise in order (spec 3.2)."
  (let ((source (%as-list source))
        (identity (%as-list identity)))
    (if (/= (length source) (length identity))
        (values :mismatched "qualifier count differs")
        (%combine (mapcar (lambda (s i) (%mv-cons (%compare-symbol-token s i)))
                           source identity)))))

(defun %resolve-eql-symbol (source)
  "Resolve SOURCE, a {kind: symbol, ...} source EQL datum (spec 3.3), to the
symbol its quoted token names.  Returns (values SYMBOL T NIL) once the quote
is confirmed and the token resolves, else (values NIL NIL REASON): a
reader-quoted datum (\"quoted\": \"reader\") is confirmed by the source text
alone, since the ' macro character cannot be shadowed, while an
operator-quoted one (\"quoted\": \"operator\") is confirmed only when its own
QUOTE_TOKEN resolves and is EQ to CL:QUOTE.  NIL is itself a symbol this
resolves to, so the second value, never the first, says whether it did."
  (flet ((resolve-datum-token ()
           (multiple-value-bind (symbol resolved-p)
               (%resolve-token (%get source "token") (%get source "in_package"))
             (if resolved-p
                 (values symbol t nil)
                 (values nil nil "a name could not be resolved to a symbol in this image")))))
    (let ((quoted (%get source "quoted")))
      (cond
        ((%tag= quoted "reader") (resolve-datum-token))
        ((%tag= quoted "operator")
         (let ((quote-token (%get source "quote_token")))
           (multiple-value-bind (quote-symbol resolved-p)
               (%resolve-token (%get quote-token "token") (%get quote-token "in_package"))
             (if (and resolved-p (eq quote-symbol 'cl:quote))
                 (resolve-datum-token)
                 (values nil nil "the quoting of this EQL symbol could not be confirmed")))))
        (t (values nil nil "the quoting of this EQL symbol could not be confirmed"))))))

(defun %eql-target-symbol (target)
  "Return (values SYMBOL STATUS) for TARGET, the runtime side's tagged EQL
datum (spec 3.3), read as the symbol a quoted source datum would have to
name: a keyword by its NAME, T or NIL by its boolean VALUE, or any interned
symbol by its PACKAGE and NAME, all resolved with FIND-SYMBOL only.  STATUS
is :SYMBOL when SYMBOL is that symbol, :OTHER when TARGET is a well-formed
datum of a kind no symbol is ever EQL to (an integer, ratio or character),
and :UNRESOLVED when the tag is one this cannot read or names a symbol this
image does not have."
  (let ((kind (%get target "kind")))
    (cond
      ((%tag= kind "keyword")
       (let ((name (%get target "name")))
         (if (stringp name)
             (multiple-value-bind (symbol status) (find-symbol name "KEYWORD")
               (if status (values symbol :symbol) (values nil :unresolved)))
             (values nil :unresolved))))
      ((%tag= kind "boolean")
       (let ((value (%get target "value")))
         (cond
           ((%tag= value "T") (values t :symbol))
           ((%tag= value "NIL") (values nil :symbol))
           (t (values nil :unresolved)))))
      ((%tag= kind "symbol")
       (multiple-value-bind (symbol resolved-p) (%resolve-identity-symbol target)
         (if resolved-p (values symbol :symbol) (values nil :unresolved))))
      ((or (%tag= kind "integer") (%tag= kind "ratio") (%tag= kind "character"))
       (values nil :other))
      (t (values nil :unresolved)))))

(defun %compare-eql-symbol (source target)
  "Compare SOURCE, a {kind: symbol, ...} source EQL datum, against TARGET,
the runtime datum of whatever kind (spec 3.3).  SOURCE's quote is resolved
before anything is decided, so a quoted keyword, T or NIL normalizes to the
same identity as its unquoted spelling and matches the very method it
names; the symbol it resolves to is then compared by EQ against the symbol
TARGET denotes.  A SOURCE whose quote or token cannot be resolved is
UNVERIFIED, never MISMATCHED -- that would claim a certainty this has none
of -- while one that does resolve and names something else, a non-symbol
datum such as an integer included, is MISMATCHED."
  (multiple-value-bind (source-symbol resolved-p reason) (%resolve-eql-symbol source)
    (if (not resolved-p)
        (values :unverified reason)
        (multiple-value-bind (target-symbol status) (%eql-target-symbol target)
          (case status
            (:symbol (if (eq source-symbol target-symbol)
                         (values :matched nil)
                         (values :mismatched "the resolved symbols are not the same")))
            (:other (values :mismatched "EQL datum kinds differ"))
            (t (values :unverified
                       "an EQL datum could not be resolved to a symbol in this image")))))))

(defun %string-verdict (a b mismatch-reason)
  "Return (values :MATCHED NIL) when A and B are equal strings (STRING=),
else (values :MISMATCHED MISMATCH-REASON).  NIL on either side is never a
match."
  (if (and (stringp a) (stringp b) (string= a b))
      (values :matched nil)
      (values :mismatched mismatch-reason)))

(defun %compare-eql-datum (source target)
  "Compare SOURCE and TARGET, tagged EQL datums (spec 3.3), by kind-specific
rules -- never by evaluating SOURCE or comparing printed representations.
An \"unverifiable\" tag on either side (an off-allow-list form or value)
always yields UNVERIFIED, per spec 3.3.  A SOURCE tagged \"symbol\" is
judged by %COMPARE-EQL-SYMBOL whatever TARGET's kind is, before the kinds
are compared at all: the source text of a quoted keyword, T or NIL is a
symbol only until its quote is resolved, and the runtime object it names is
tagged \"keyword\" or \"boolean\", so gating on kind equality first would
call the two spellings of one datum a contradiction."
  (let ((source-kind (%get source "kind"))
        (target-kind (%get target "kind")))
    (cond
      ((or (%tag= source-kind "unverifiable") (%tag= target-kind "unverifiable"))
       (values :unverified (or (%get source "reason") (%get target "reason")
                                "an EQL datum could not be verified")))
      ((%tag= source-kind "symbol") (%compare-eql-symbol source target))
      ((not (equal source-kind target-kind))
       (values :mismatched "EQL datum kinds differ"))
      ((%tag= source-kind "keyword")
       (%string-verdict (%get source "name") (%get target "name") "keyword names differ"))
      ((%tag= source-kind "integer")
       (%string-verdict (%get source "value") (%get target "value") "integer values differ"))
      ((%tag= source-kind "ratio")
       (multiple-value-bind (status reason)
           (%string-verdict (%get source "numerator") (%get target "numerator")
                             "ratio numerator differs")
         (if (eq status :matched)
             (%string-verdict (%get source "denominator") (%get target "denominator")
                               "ratio denominator differs")
             (values status reason))))
      ((%tag= source-kind "character")
       (%string-verdict (%get source "value") (%get target "value") "character values differ"))
      ((%tag= source-kind "boolean")
       (%string-verdict (%get source "value") (%get target "value") "boolean values differ"))
      (t (values :unverified "unrecognized EQL datum kind")))))

(defun %compare-specializer (source target)
  "Compare SOURCE, a candidate's specializer (spec 3.2: kind class, eql or
unverifiable), against TARGET, the identity's specializer (spec 3.2/3.3)."
  (let ((source-kind (%get source "kind"))
        (target-kind (%get target "kind")))
    (cond
      ((or (%tag= source-kind "unverifiable") (%tag= target-kind "unverifiable"))
       (values :unverified (or (%get target "reason") (%get source "reason")
                                "a specializer could not be verified")))
      ((and (%tag= source-kind "class") (%tag= target-kind "class"))
       (%compare-symbol-token source target))
      ((and (%tag= source-kind "eql") (%tag= target-kind "eql"))
       (%compare-eql-datum (%get source "datum") (%get target "datum")))
      (t (values :mismatched "specializer kinds differ")))))

(defun %compare-specializer-lists (source identity)
  "Compare SOURCE and IDENTITY, parallel lists of specializers (spec 3.2),
pairwise in order; a length mismatch is a conclusive contradiction."
  (let ((source (%as-list source))
        (identity (%as-list identity)))
    (if (/= (length source) (length identity))
        (values :mismatched "specializer count differs")
        (%combine (mapcar (lambda (s i) (%mv-cons (%compare-specializer s i)))
                           source identity)))))

(defparameter *container-symbols*
  (list (list "defmethod" 'cl:defmethod :defmethod)
        (list "defgeneric" 'cl:defgeneric :defgeneric)
        (list "defclass" 'cl:defclass :defclass)
        (list "define-condition" 'cl:define-condition :define-condition)
        (list "defstruct" 'cl:defstruct :defstruct))
  "The definition forms VERIFY-ENTRIES supports (spec 3.4): each entry is
(TAG STANDARD-SYMBOL KEYWORD), TAG being a candidate's JSON \"kind\" string,
STANDARD-SYMBOL the CL operator its head token must resolve to and be EQ to,
and KEYWORD the internal container-kind %VERIFIED-CONTAINER-KIND returns.")

(defun %container-entry (tag)
  "Return TAG's entry in *CONTAINER-SYMBOLS*, or NIL when TAG names none of
the five supported definition forms."
  (find-if (lambda (entry) (%tag= tag (first entry))) *container-symbols*))

(defun %verified-container-kind (candidate)
  "Return CANDIDATE's confirmed container kind -- one of :DEFMETHOD,
:DEFGENERIC, :DEFCLASS, :DEFINE-CONDITION or :DEFSTRUCT -- when its head
token resolves and is EQ to the standard operator of that name, else NIL.
Never trusts CANDIDATE's own \"kind\" tag (built by name only, spec 3.4)
without this check, so a same-named operator shadowed from another package
is never treated as a standard definition form."
  (let ((entry (%container-entry (%get candidate "kind")))
        (head (%get candidate "head")))
    (when (and entry head)
      (multiple-value-bind (symbol resolved-p)
          (%resolve-token (%get head "token") (%get head "in_package"))
        (when (and resolved-p (eq symbol (second entry)))
          (third entry))))))

(defun %verify-class-candidate (identity candidate)
  "Judge CANDIDATE against a class IDENTITY (spec 3.4): CANDIDATE must be a
verified DEFCLASS, DEFINE-CONDITION or DEFSTRUCT whose name matches
IDENTITY's class."
  (let ((class (%get identity "class")))
    (cond
      ((null class) (values :unverified "this class has no name to match"))
      ((member (%verified-container-kind candidate)
               '(:defclass :define-condition :defstruct))
       (%compare-name (%get candidate "name") class))
      (t (values :unverified "unsupported or shadowed definition form")))))

(defun %verify-generic-function-candidate (identity candidate)
  "Judge CANDIDATE against a generic-function IDENTITY (spec 3.4): CANDIDATE
must be a verified DEFGENERIC whose name matches IDENTITY's generic
function."
  (if (eq (%verified-container-kind candidate) :defgeneric)
      (%compare-name (%get candidate "name") (%get identity "generic_function"))
      (values :unverified "unsupported or shadowed definition form")))

(defun %verify-defmethod-candidate (identity candidate)
  "Judge a verified DEFMETHOD CANDIDATE's name, qualifiers and specializers
against a plain method IDENTITY, combined by spec 3.1's rule."
  (%combine (list (%mv-cons (%compare-name (%get candidate "name")
                                            (%get identity "generic_function")))
                  (%mv-cons (%compare-qualifiers (%get candidate "qualifiers")
                                                  (%get identity "qualifiers")))
                  (%mv-cons (%compare-specializer-lists (%get candidate "specializers")
                                                         (%get identity "specializers"))))))

(defun %verify-defgeneric-inline-method (identity candidate)
  "Judge a verified DEFGENERIC CANDIDATE against a plain method IDENTITY
(spec 3.4): its own name must match IDENTITY's generic function, and
exactly one of its inline (:method ...) descriptions must match IDENTITY's
qualifiers and specializers.  Zero or more than one is not a match."
  (multiple-value-bind (name-status name-reason)
      (%compare-name (%get candidate "name") (%get identity "generic_function"))
    (if (not (eq name-status :matched))
        (values name-status name-reason)
        (let ((matched 0) (mismatched-reason nil) (unverified-reason nil))
          (dolist (method (%as-list (%get candidate "methods")))
            (multiple-value-bind (status reason)
                (%combine (list (%mv-cons (%compare-qualifiers
                                            (%get method "qualifiers")
                                            (%get identity "qualifiers")))
                                (%mv-cons (%compare-specializer-lists
                                           (%get method "specializers")
                                           (%get identity "specializers")))))
              (case status
                (:matched (incf matched))
                (:mismatched (setf mismatched-reason (or mismatched-reason reason)))
                (:unverified (setf unverified-reason (or unverified-reason reason))))))
          (cond
            ((> matched 1) (values :unverified "ambiguous: more than one inline method matches"))
            ((= matched 1) (values :matched nil))
            (mismatched-reason (values :mismatched mismatched-reason))
            (t (values :unverified (or unverified-reason "no inline method matches"))))))))

(defun %verify-method-candidate (identity candidate)
  "Judge CANDIDATE against a plain (non-accessor) method IDENTITY (spec
3.4): either a verified DEFMETHOD matching name, qualifiers and
specializers, or a verified DEFGENERIC whose inline methods are searched
the same way."
  (case (%verified-container-kind candidate)
    (:defmethod (%verify-defmethod-candidate identity candidate))
    (:defgeneric (%verify-defgeneric-inline-method identity candidate))
    (t (values :unverified "unsupported or shadowed definition form"))))

(defun %any-name-matches (names identity)
  "Judge whether any of NAMES -- the {token, setf, in_package} source names
one slot's options of a single access kind define (spec 3.2) -- names
IDENTITY, a {package, name, setf} function-name identity.  %COMPARE-NAME
judges each one, so a candidate differing only in its SETF flag -- a
`:writer x' option against a live (SETF X) writer, say -- is a different
function, not a match: the option's shape settles that whatever symbol x
turns out to name, which is why the SETF shortcut counts as conclusive here.

The three-valued rule, in one place: :MATCHED as soon as one name matches;
otherwise :MISMATCHED only when every candidate reached a conclusive verdict
and none matched; otherwise :UNVERIFIED.  So a list mixing a name this image
cannot resolve -- an unknown package, an unreadable token, or a NIL entry the
parent could not read as a function name at all -- with a conclusively
different one is :UNVERIFIED: the unresolvable one might have been the
definition, so there is no certainty to report.  An empty list is
:UNVERIFIED for the same reason it always was: nothing was judged."
  (let ((conclusive nil) (inconclusive nil))
    (dolist (name (%as-list names))
      (multiple-value-bind (status reason) (%compare-name name identity)
        (declare (ignore reason))
        (case status
          (:matched (return-from %any-name-matches (values :matched nil)))
          (:mismatched (setf conclusive t))
          (:unverified (setf inconclusive t)))))
    (cond
      (inconclusive
       (values :unverified "an accessor of this kind could not be resolved"))
      (conclusive
       (values :mismatched "no accessor of this kind names the expected generic function"))
      (t (values :unverified "no accessor name could be resolved")))))

(defun %compare-accessor-slot (candidate identity)
  "Find the slot in CANDIDATE's :slots whose name matches IDENTITY's slot,
and confirm that its reader or writer names (per IDENTITY's access) include
IDENTITY's generic function, SETF flag and all (spec 3.4)."
  (let ((accessor-key (cond ((%tag= (%get identity "access") "reader") "readers")
                             ((%tag= (%get identity "access") "writer") "writers")
                             (t nil))))
    (if (null accessor-key)
        (values :unverified "accessor kind is neither reader nor writer")
        (let ((named '()) (any-unresolved nil))
          (dolist (slot (%as-list (%get candidate "slots")))
            (multiple-value-bind (status reason)
                (%compare-symbol-token (%get slot "name") (%get identity "slot"))
              (declare (ignore reason))
              (case status
                (:matched (push slot named))
                (:unverified (setf any-unresolved t)))))
          (cond
            ((> (length named) 1) (values :unverified "ambiguous slot name in this definition"))
            ((= (length named) 1)
             (%any-name-matches (%get (first named) accessor-key)
                                (%get identity "generic_function")))
            (any-unresolved (values :unverified "a slot name could not be resolved"))
            (t (values :mismatched "no slot in this definition matches")))))))

(defun %verify-accessor-candidate (identity candidate)
  "Judge CANDIDATE against an accessor method IDENTITY (spec 3.4): CANDIDATE
must be a verified DEFCLASS or DEFINE-CONDITION whose name matches
IDENTITY's class and whose slots include one matching name, access and
generic function."
  (if (member (%verified-container-kind candidate) '(:defclass :define-condition))
      (%combine (list (%mv-cons (%compare-name (%get candidate "name") (%get identity "class")))
                      (%mv-cons (%compare-accessor-slot candidate identity))))
      (values :unverified "unsupported or shadowed definition form")))

(defun %accessor-identity-p (identity)
  "True when IDENTITY, a method identity, describes a standard accessor:
its class, slot and access fields are all present (spec 3.2) and it carries
no qualifiers -- an accessor is never :BEFORE/:AFTER/:AROUND-qualified.
Belt-and-braces against CLOS-CORE:%METHOD-ENTRY ever handing this a
qualified method's identity with class/slot/access filled in by mistake (it
should not, since it now checks this itself): such an identity is judged
here as a plain method instead, so a qualified method is never verified
against a DEFCLASS or DEFINE-CONDITION form it merely shares a generic
function and specializer with."
  (and (%get identity "class") (%get identity "slot") (%get identity "access")
       (null (%as-list (%get identity "qualifiers")))
       t))

(defun %verify-candidate (identity candidate)
  "Judge one CANDIDATE, a source_signature (spec 3.2) as JSON, against
IDENTITY, a structured identity (spec 3.2), by spec 3.1-3.4.  Returns
(values STATUS REASON), STATUS one of :MATCHED, :MISMATCHED, :UNVERIFIED,
REASON an English sentence or NIL for :MATCHED."
  (let ((identity-kind (%get identity "kind")))
    (cond
      ((%tag= identity-kind "class") (%verify-class-candidate identity candidate))
      ((%tag= identity-kind "generic-function")
       (%verify-generic-function-candidate identity candidate))
      ((%tag= identity-kind "method")
       (if (%accessor-identity-p identity)
           (%verify-accessor-candidate identity candidate)
           (%verify-method-candidate identity candidate)))
      (t (values :unverified "unrecognized identity kind")))))

(defun %result-status-string (status)
  "Return STATUS's JSON spelling: matched, mismatched or unverified."
  (ecase status
    (:matched "matched")
    (:mismatched "mismatched")
    (:unverified "unverified")))

(defun %verify-entry (entry)
  "Return the JSON result object for ENTRY: {id, status, reason,
candidate_index} (task 3 brief).  Judges each of ENTRY's candidates against
its identity (%VERIFY-CANDIDATE), then combines them by spec 3.4's tail
rule: exactly one MATCHED candidate is MATCHED (with its index); two or
more is UNVERIFIED (ambiguous); zero MATCHED with at least one MISMATCHED
is MISMATCHED; anything else -- no candidates, or all UNVERIFIED -- is
UNVERIFIED."
  (if (not (hash-table-p entry))
      (make-ht "id" nil "status" "unverified" "reason" "malformed entry"
               "candidate_index" nil)
      (let ((id (gethash "id" entry))
            (identity (gethash "identity" entry))
            (matched-indices '())
            (mismatched-reason nil)
            (unverified-reason nil))
        (loop for candidate in (%as-list (gethash "candidates" entry))
              for index from 0
              do (multiple-value-bind (status reason) (%verify-candidate identity candidate)
                   (case status
                     (:matched (push index matched-indices))
                     (:mismatched (unless mismatched-reason (setf mismatched-reason reason)))
                     (:unverified (unless unverified-reason (setf unverified-reason reason))))))
        (setf matched-indices (nreverse matched-indices))
        (multiple-value-bind (status reason index)
            (cond
              ((> (length matched-indices) 1)
               (values :unverified "ambiguous: more than one candidate matches" nil))
              ((= (length matched-indices) 1)
               (values :matched nil (first matched-indices)))
              (mismatched-reason (values :mismatched mismatched-reason nil))
              (t (values :unverified
                         (or unverified-reason "no candidates to verify against") nil)))
          (make-ht "id" id
                   "status" (%result-status-string status)
                   "reason" reason
                   "candidate_index" index)))))

(defun verify-entries (entries)
  "Resolve and judge ENTRIES against the source signatures the parent
extracted from its CST (spec 3.1-3.4), using FIND-PACKAGE and FIND-SYMBOL
only -- nothing is interned, evaluated or macroexpanded, and no EQL
specializer's source expression is ever evaluated.

ENTRIES is a JSON array (list or vector) of objects:
  {\"id\": <string>, \"identity\": <task 1's identity, spec 3.2>,
   \"candidates\": [<candidate>...]}
Each <candidate> is task 2's %DEFINITION-SOURCE-SIGNATURE plist (spec 3.2)
converted to JSON this way -- the shape a later task's parent-side
conversion must produce:
  {\"kind\": \"defmethod\"|\"defgeneric\"|\"defclass\"|\"define-condition\"
             |\"defstruct\"|\"other\",
   \"head\": <token>,
   ;; :defmethod
   \"name\": <name>, \"qualifiers\": [<token>...], \"specializers\": [<specializer>...],
   ;; :defgeneric
   \"name\": <name>, \"methods\": [{\"qualifiers\": [<token>...],
                                   \"specializers\": [<specializer>...] | null}...],
   ;; :defclass / :define-condition
   \"name\": <name>, \"slots\": [{\"name\": <token>, \"readers\": [<name>...],
                                 \"writers\": [<name>...]}...],
   ;; :defstruct
   \"name\": <name>}
<token> is {\"token\": <string>, \"in_package\": <string or null>} -- the
literal source text at a node's span, never resolved.  <name> adds
\"setf\": <boolean> to <token>; a slot's readers and writers carry it too,
since :READER X, :WRITER X, :WRITER (SETF X) and :ACCESSOR X do not all
define the same function, and a null there is an option this image must not
try to resolve.  <specializer> is {\"kind\": \"class\",
\"token\":.., \"in_package\":..} | {\"kind\": \"eql\", \"datum\": <datum>} |
{\"kind\": \"unverifiable\", \"reason\": <string>}.  <datum> (spec 3.3) is
{\"kind\": \"keyword\", \"name\":..} | {\"kind\": \"integer\"|\"character\"
|\"boolean\", \"value\":..} | {\"kind\": \"ratio\", \"numerator\":..,
\"denominator\":..} | {\"kind\": \"symbol\", \"token\":.., \"in_package\":..,
\"quoted\": \"reader\"|\"operator\", \"quote_token\": <token> (\"operator\"
only)} | {\"kind\": \"unverifiable\", \"reason\":..}.

Returns {\"results\": [{\"id\", \"status\", \"reason\", \"candidate_index\"}...]}:
STATUS is \"matched\", \"mismatched\" or \"unverified\"; REASON is an
English sentence, or null for \"matched\"; CANDIDATE_INDEX is CANDIDATES'
0-based position of the matching entry, or null unless STATUS is
\"matched\"."
  (make-ht "results" (map 'vector #'%verify-entry (%as-list entries))))
