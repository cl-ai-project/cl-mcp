;;;; src/lisp-edit-form-core.lisp
;;;;
;;;; Shared helpers for lisp-edit-form and lisp-patch-form tools.
;;;; Contains form matching, path normalization, and the common
;;;; prologue (locate-target-form) used by both tools.

(defpackage #:cl-mcp/src/lisp-edit-form-core
  (:use #:cl)
  (:shadowing-import-from #:cl-mcp/src/cst
                          #:cst-node
                          #:cst-node-kind
                          #:cst-node-value
                          #:cst-node-start
                          #:cst-node-end)
  (:import-from #:cl-ppcre
                #:scan-to-strings)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:stray-right-parenthesis
                #:*standard-readtable*)
  (:import-from #:cl-mcp/src/package-context
                #:extract-in-package-name-from-text)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis
                #:format-overwrite-recovery)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/fs
                #:*lisp-file-unparseable-hook*
                #:*fs-read-max-bytes*
                #:fs-read-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/source-snapshot
                #:read-source-snapshot
                #:snapshot-decode-lossy-p
                #:snapshot-range-digest)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-condition-text
                #:sanitize-for-json)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:enough-pathname
                #:native-namestring
                #:subpathp)
  (:export #:%normalize-string
           #:%defmethod-candidates
           #:%definition-candidates
           #:%normalize-paths
           #:%strip-name-prefix
           #:%find-target
           #:locate-form-in-nodes
           #:%resolve-named-readtable
           #:%nonstandard-readtable-p
           #:%parse-readtable-designator
           #:%detect-readtable-before-node
           #:%whitespace-char-p
           #:%locate-target-form
           #:%file-unparseable-by-edit-tools-p
           #:%delimiter-failure-p
           #:%reader-level-failure-p
           #:file-unparseable-error
           #:file-unparseable-path
           #:file-unparseable-diagnosis
           #:file-unparseable-cause
           #:file-unparseable-readtable
           #:file-unparseable-recoverable-p
           #:file-unparseable-editable-prefix-p
           #:file-unparseable-message
           #:make-file-unparseable-condition
           #:signal-file-unparseable
           #:+edit-guard-version+
           #:+edit-guard-token-separator+
           #:format-edit-guard-token
           #:parse-edit-guard-token
           #:normalize-edit-guard
           #:check-edit-guard
           #:edit-guard-conflict-error
           #:edit-guard-conflict))

(in-package #:cl-mcp/src/lisp-edit-form-core)

(defun %normalize-string (thing)
  "Normalize THING to a lowercase string for form matching.
Uses SYMBOL-NAME for symbols to avoid package prefix in the output."
  (string-downcase
   (if (symbolp thing)
       (symbol-name thing)
       (princ-to-string thing))))

(defun %names-only (tree)
  "Return TREE with each symbol outside COMMON-LISP and KEYWORD replaced by an
uninterned symbol of the same name, so %SIGNATURE-TEXT prints it bare.
COMMON-LISP symbols are kept so the pretty printer still writes (QUOTE X) as
'X; they print without a prefix from COMMON-LISP-USER anyway."
  (let ((cl (find-package "COMMON-LISP"))
        (keyword (find-package "KEYWORD")))
    (labels ((walk (node)
               (cond
                 ((consp node) (cons (walk (car node)) (walk (cdr node))))
                 ((and (symbolp node)
                       (not (member (symbol-package node) (list cl keyword))))
                  (make-symbol (symbol-name node)))
                 (t node))))
      (walk tree))))

(defun %signature-text (object)
  "Return OBJECT, part of a definition's signature, printed as form names are
compared: lower case, on one line, and with no package prefix on any symbol.

The package a form was read in decides how PRIN1 qualifies its symbols, so
printing them as read made a method's lambda list come out as
\"((stream cl-mcp/src/utils/bounded-stream:bounded-output-stream) character)\"
in one process and unqualified in another, and a long lambda list gained line
breaks.  Neither matched what a caller writes."
  (let ((*package* (find-package "COMMON-LISP-USER"))
        (*print-gensym* nil)
        (*print-pretty* t)
        (*print-right-margin* most-positive-fixnum)
        (*print-readably* nil))
    (string-downcase (prin1-to-string (%names-only object)))))

(defun %defmethod-candidates (form)
  "Return candidate signature strings for a DEFMETHOD FORM.
Candidates are generated in order of specificity:
1. name only: \"resize\"
2. name + qualifier: \"resize :after\"
3. name + lambda-list: \"resize ((s shape) factor)\"
4. name + qualifier + lambda-list: \"resize :after ((s shape) factor)\"

Qualifiers and the lambda list are printed by %SIGNATURE-TEXT, so symbols
carry no package prefix and '#:' never appears, whichever package the form
was read in."
  (destructuring-bind
      (_ name &rest rest)
      form
    (declare (ignore _))
    (let ((qualifiers 'nil) (lambda-list nil))
      (dolist (part rest)
        (when (listp part) (setf lambda-list part) (return))
        (push part qualifiers))
      (let ((name-str (%normalize-string name))
            (lambda-str (and lambda-list (%signature-text lambda-list)))
            (qual-str
             (and qualifiers
                  (format nil "~{~A~^ ~}"
                          (mapcar #'%signature-text (nreverse qualifiers))))))
        (remove nil
                (list name-str
                      (and qual-str (format nil "~A ~A" name-str qual-str))
                      (and lambda-str (format nil "~A ~A" name-str lambda-str))
                      (and (and qual-str lambda-str)
                           (format nil "~A ~A ~A" name-str qual-str
                                   lambda-str))))))))

(defun %definition-candidates (form form-type)
  "Return candidate strings that identify FORM with FORM-TYPE."
  (let ((name (second form)))
    (cond
      ((string= form-type "defmethod")
       (%defmethod-candidates form))
      ((symbolp name)
       (list (%normalize-string name)))
      ;; defstruct: (defstruct (name &rest options) ...) — first element is the name
      ((string= form-type "defstruct")
       (if (and (listp name) (symbolp (car name)))
           (list (%normalize-string (car name)))
           (list (%normalize-string name))))
      (t (list (%normalize-string name))))))

(defun %whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return)))

(defun %normalize-paths (file-path)
  "Return two values: absolute path (pathname) and relative namestring for FS tools.
The project root is resolved with TRUENAME before the containment test, as
FS-RESOLVE-READ-PATH resolves the file: a root that is itself a symlink
(macOS /tmp, a git worktree) must still contain its own files."
  (let* ((resolved (fs-resolve-read-path file-path))
         (declared (ensure-directory-pathname *project-root*))
         (root (or (ignore-errors (ensure-directory-pathname (truename declared)))
                   declared)))
    (unless (subpathp resolved root)
      (error "Write path ~A is outside project root ~A" file-path root))
    (let* ((relative (enough-pathname resolved root))
           (rel-namestring (native-namestring relative)))
      (values resolved rel-namestring))))

(defun %resolve-named-readtable (designator)
  "Resolve DESIGNATOR to a readtable via named-readtables, or return NIL.
Looks up the readtable using FIND-READTABLE from the named-readtables or
editor-hints.named-readtables package."
  (when designator
    (let ((pkg (or (find-package :named-readtables)
                   (find-package :editor-hints.named-readtables))))
      (when pkg
        (let ((find-fn (find-symbol "FIND-READTABLE" pkg)))
          (when (and find-fn (fboundp find-fn))
            (funcall find-fn designator)))))))

(defun %standard-syntax-readtable-p (rt)
  "Return T when readtable RT reads exactly like the standard one: the same
readtable case, the same macro function and terminating status for every
character below 128, and the same dispatch function for every # sub-character
below 128. A readtable that merely copies :standard (or the :standard
designator itself) reads standard syntax and must not switch off the
standard-syntax delimiter diagnostics.
A dispatching macro character's own function is a fresh closure in every
readtable copy (SBCL), so for a character that dispatches in both readtables
the comparison is made on its sub-characters instead.
Known limit: only code points below 128 are compared, so a reader macro
defined on a non-ASCII character is not noticed and the standard-syntax
diagnostics stay on for that readtable."
  (let ((std *standard-readtable*))
    (flet ((dispatching-p (table ch)
             (and (ignore-errors (progn (get-dispatch-macro-character ch #\a table) t))
                  t)))
      (and (eq (readtable-case rt) (readtable-case std))
           (loop for code from 0 below 128
                 for ch = (code-char code)
                 always (or (and (dispatching-p rt ch) (dispatching-p std ch))
                            (multiple-value-bind (fn non-terminating)
                                (get-macro-character ch rt)
                              (multiple-value-bind (std-fn std-nt)
                                  (get-macro-character ch std)
                                (and (eq fn std-fn)
                                     (eq (not non-terminating) (not std-nt)))))))
           (loop for code from 0 below 128
                 for ch = (code-char code)
                 always (eq (ignore-errors (get-dispatch-macro-character #\# ch rt))
                            (ignore-errors (get-dispatch-macro-character #\# ch std))))))))

(defun %nonstandard-readtable-p (designator)
  "Return T when DESIGNATOR resolves (via named-readtables) to a readtable
whose syntax differs from the standard one, so the standard-syntax delimiter
diagnostics (the ] refusal, bracket advice, lisp-patch-form's depth message)
must not be used. NIL for no designator, for one that does not resolve (the
tools then read with the standard readtable anyway), and for a readtable that
is standard in all but name."
  (let ((rt (and designator (%resolve-named-readtable designator))))
    (and rt (not (%standard-syntax-readtable-p rt)) t)))

(defun %parse-readtable-designator (readtable-string)
  "Parse a readtable string from MCP tool args into a symbol designator.
Handles three forms:
  \"pkg:sym\"  or \"pkg::sym\" → interned symbol in PKG
  \":keyword\" or \"keyword\"  → keyword symbol
Returns NIL if READTABLE-STRING is NIL or blank."
  (let ((trimmed (and (stringp readtable-string)
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   readtable-string))))
    (when (and trimmed (plusp (length trimmed)))
      (let ((colon-pos (position #\: trimmed)))
        (if (and colon-pos (plusp colon-pos))
            (let* ((pkg-name (subseq trimmed 0 colon-pos))
                   (sym-start (if (and (< (1+ colon-pos) (length trimmed))
                                       (char= (char trimmed (1+ colon-pos)) #\:))
                                  (+ colon-pos 2)
                                  (1+ colon-pos)))
                   (sym-name (subseq trimmed sym-start))
                   (pkg (find-package (string-upcase pkg-name))))
              (if pkg
                  (intern (string-upcase sym-name) pkg)
                  (error "Package ~A not found for readtable ~A"
                         pkg-name readtable-string)))
            (intern (string-upcase (string-left-trim ":" trimmed))
                    :keyword))))))

(defun %strip-name-prefix (name)
  "Strip reader macro prefixes (#: : \"...\") from NAME for form-name matching.
Handles uninterned symbols (#:pkg), keywords (:pkg), and string literals (\"pkg\")."
  (cond
    ((and (>= (length name) 2) (string= (subseq name 0 2) "#:"))
     (subseq name 2))
    ((and (plusp (length name)) (char= (char name 0) #\:))
     (subseq name 1))
    ((and (>= (length name) 2)
          (char= (char name 0) #\")
          (char= (char name (1- (length name))) #\"))
     (subseq name 1 (1- (length name))))
    (t name)))

(defun %strip-hash-colon (s)
  "Return S with every '#:' reader-macro prefix removed.
Normalizes uninterned symbol prints (produced by PRIN1 on symbols from
package-inferred-system sources) so candidate strings and user-supplied
form-name strings compare equal regardless of whether the original
source used interned or uninterned symbols. Keyword prefixes ':foo' are
preserved, so defmethod qualifiers like ':after' still match.

Scans S as a simple state machine that tracks whether the cursor is
inside a string literal. Only '#:' occurrences OUTSIDE string literals
are removed; '#:' embedded in an EQL specializer like \"#:tag\" is
preserved so two defmethods differing only by a string literal prefix
remain distinguishable."
  (with-output-to-string (out)
    (let ((len (length s))
          (in-string nil)
          (i 0))
      (loop while (< i len) do
        (let ((c (char s i)))
          (cond
            ;; Escaped character inside a string literal: emit both as-is.
            ((and in-string (char= c #\\) (< (1+ i) len))
             (write-char c out)
             (write-char (char s (1+ i)) out)
             (incf i 2))
            ;; String delimiter: toggle state and pass through.
            ((char= c #\")
             (write-char c out)
             (setf in-string (not in-string))
             (incf i))
            ;; '#:' outside a string literal: drop both characters.
            ((and (not in-string)
                  (char= c #\#)
                  (< (1+ i) len)
                  (char= (char s (1+ i)) #\:))
             (incf i 2))
            ;; Everything else: pass through unchanged.
            (t
             (write-char c out)
             (incf i))))))))

(defun %normalize-form-name-text (s)
  "Return S, a form_name a caller wrote, as the candidates are written.
Outside string literals, each run of whitespace becomes one space and a
package prefix -- 'pkg:' or 'pkg::' at the start of a token -- is dropped, so
\"sb-gray:stream-write-char ((stream\\n  bounded-output-stream) character)\"
reads as the candidate does.  A token starting with a colon is a keyword and
is kept."
  (with-output-to-string (out)
    (let ((len (length s))
          (in-string nil)
          (pending-space nil)
          (i 0))
      (flet ((token-start-p ()
               ;; I begins a token when nothing, whitespace or an opening
               ;; delimiter precedes it.
               (or (zerop i)
                   (find (char s (1- i)) '(#\( #\' #\` #\, #\Space #\Tab
                                           #\Newline #\Return #\Page)))))
        (loop while (< i len) do
          (let ((c (char s i)))
            (cond
              ((and in-string (char= c #\\) (< (1+ i) len))
               (write-char c out)
               (write-char (char s (1+ i)) out)
               (incf i 2))
              (in-string
               (when (char= c #\") (setf in-string nil))
               (write-char c out)
               (incf i))
              ((%whitespace-char-p c)
               (setf pending-space t)
               (incf i))
              (t
               (when pending-space
                 (write-char #\Space out)
                 (setf pending-space nil))
               (if (and (token-start-p) (not (find c "():\"'`,#")))
                   ;; Copy the token from just past its last colon.
                   (let* ((end (or (position-if (lambda (ch)
                                                  (or (%whitespace-char-p ch)
                                                      (find ch "()\"'`,")))
                                                s :start i)
                                   len))
                          (colon (position #\: s :start i :end end :from-end t)))
                     (write-string s out :start (if colon (1+ colon) i) :end end)
                     (setf i end))
                   (progn
                     (when (char= c #\") (setf in-string t))
                     (write-char c out)
                     (incf i)))))))))))

(defun locate-form-in-nodes (nodes form-type form-name)
  "Find the CST node among NODES -- top-level nodes as PARSE-TOP-LEVEL-FORMS
returns them -- matching FORM-TYPE and FORM-NAME, the same rules %FIND-TARGET
documents (the [N] index suffix, defmethod's normalized signature matching,
reader-prefix stripping).  Returns (VALUES NODE ERROR-STRING).

NODE is the sole matching node, or NIL when it cannot be resolved to exactly
one: zero matches, an [N] index out of range, or more than one match without
a disambiguating index.  ERROR-STRING is NIL for zero matches -- a plain,
non-exceptional absence -- and a descriptive message for the other two: an
out-of-range index, or an ambiguous set of matches (naming each candidate's
own signature and its [N] index).  A FORM-NAME that strips down to the empty
string is also reported this way, before any node is searched.

%FIND-TARGET re-signals ERROR-STRING as a Lisp error, preserving its own
contract for lisp-edit-form/lisp-patch-form.  The clos-describe observer
calls this directly instead, to decide a round trip failed (spec 3.5)
without installing a condition handler around every candidate it checks."
  (multiple-value-bind (base-name index)
      (let ((match (nth-value 1 (scan-to-strings "^(.+?)\\[(\\d+)\\]$" form-name))))
        (if match
            (values (aref match 0) (parse-integer (aref match 1)))
            (values form-name nil)))
    (let* ((stripped (%strip-hash-colon (string-downcase (%strip-name-prefix base-name))))
           (target (if (string= form-type "defmethod")
                       (%normalize-form-name-text stripped)
                       stripped))
           (matches nil))
      (if (zerop (length target))
          (values nil (format nil "form_name resolved to empty string after prefix stripping; ~
provide a non-empty name (e.g. \"my-pkg\" instead of \"#:\" alone)"))
          (progn
            (loop for node in nodes
                  when (and (typep node 'cst-node)
                            (eq (cst-node-kind node) :expr))
                    do (let ((value (cst-node-value node)))
                         (when (and (consp value)
                                    (string= (string-downcase (symbol-name (car value))) form-type)
                                    (some (lambda (cand) (string= cand target))
                                          (%definition-candidates value form-type)))
                           (push (cons node value) matches))))
            (setf matches (nreverse matches))
            ;; A method's candidates include its lambda list without its
            ;; qualifiers, so "area ((s circle))" names both the primary
            ;; method and the :around one.  When no index was given, a form
            ;; whose full signature is exactly FORM-NAME wins over forms it
            ;; only abbreviates.
            (unless index
              (let ((exact (remove-if-not
                            (lambda (match)
                              (string= target
                                       (car (last (%definition-candidates (cdr match) form-type)))))
                            matches)))
                (when exact
                  (setf matches exact))))
            (cond
              ((null matches) (values nil nil))
              ((and index (< index (length matches))) (values (car (nth index matches)) nil))
              (index
               (values nil (format nil "Index [~D] out of range, only ~D match~:P found for ~A"
                                    index (length matches) form-name)))
              ((= (length matches) 1) (values (car (first matches)) nil))
              (t
               (let ((descriptions
                       (loop for (node . form) in matches
                             for i from 0
                             collect (let ((candidates (%definition-candidates form form-type)))
                                       (format nil "[~D] ~A" i
                                               (or (car (last candidates)) (first candidates)))))))
                 (values nil (format nil "Multiple matches for ~A ~A. Specify an index:~%~{  ~A~%~}"
                                     form-type form-name descriptions))))))))))

(defun %find-target (nodes form-type form-name)
  "Find a target node matching FORM-TYPE and FORM-NAME (LOCATE-FORM-IN-NODES
documents the matching rules in full).  Returns the node, or NIL when
nothing matches; signals a Lisp error when LOCATE-FORM-IN-NODES reports one
instead (an out-of-range [N] index, ambiguous matches, or an empty
FORM-NAME) -- the contract lisp-edit-form and lisp-patch-form already rely
on."
  (multiple-value-bind (node reason) (locate-form-in-nodes nodes form-type form-name)
    (if reason (error "~A" reason) node)))

(defun %detect-readtable-before-node (nodes target)
  "Return the readtable designator active before TARGET, or NIL.
Scans NODES in order and only considers IN-READTABLE forms that appear
before TARGET's start position."
  (let ((target-start (cst-node-start target))
        (result nil))
    (dolist (node nodes result)
      (when (>= (cst-node-start node) target-start)
        (return result))
      (when (and (typep node 'cst-node)
                 (eq (cst-node-kind node) :expr))
        (let ((value (cst-node-value node)))
          (when (and (consp value)
                     (symbolp (car value))
                     (string= (symbol-name (car value)) "IN-READTABLE")
                     (consp (cdr value)))
            (setf result (second value))))))))

(defun %delimiter-failure-p (condition)
  "Return T when CONDITION says the default reader ran out of input
(END-OF-FILE, including cst's UNTERMINATED-SOURCE: a missing \")\", but also
an unterminated string or #| comment, since \" and #| are macro characters
too) or met a stray \")\" (cst's STRAY-RIGHT-PARENTHESIS, which cst raises
itself on both its parsing paths). Every other reader failure -- an unknown
dispatch macro such as #?, a disabled #. -- is not counted. Even a delimiter
failure is not proof that the file is broken (a reader macro supplied through
the readtable parameter could consume the offending characters), which is why
fs-write-file additionally requires the caller to opt in before overwriting."
  (or (typep condition 'end-of-file)
      (typep condition 'stray-right-parenthesis)))

(defun %reader-level-failure-p (condition)
  "Return T when the reader itself rejected the text for a reason other than
a delimiter: a disabled #., an unknown dispatch macro such as #?. Only such a
failure makes a bracket verdict of the standard-syntax scan a finding rather
than an instruction. END-OF-FILE is not a READER-ERROR and is excluded anyway
as a delimiter failure; the edit tools' own synthesized errors (\"content is
empty\", \"multiple top-level forms\") are plain ERRORs and do not count, so
a genuinely stray ) keeps its instruction."
  (and (typep condition 'reader-error)
       (not (%delimiter-failure-p condition))))

(defun file-unparseable-message (condition)
  "Return the guidance text for CONDITION, a FILE-UNPARSEABLE-ERROR.
When the failure is recoverable (a delimiter problem no readtable can fix),
the text opens with the shared delimiter diagnosis, or the reader error when
the scan has nothing to add, and ends with an executable recovery path
(FORMAT-OVERWRITE-RECOVERY: read, hand-apply the fix, fs-write-file, which
permits overwriting such a file). A recoverable file outside the project root
-- a dependency's source, which lisp-read-file can read but fs-write-file
cannot write -- gets no recovery path, since fs-write-file would refuse the
absolute path; the text says to fix it outside cl-mcp. When the caller
supplied a readtable, no standard-syntax verdict exists: the text names the
readtable and says how the overwrite guard will decide. Otherwise the failure
is reader-level (custom reader syntax, a disabled #. form); the file keeps its
overwrite protection, so the text points at the readtable parameter instead."
  (let* ((path (file-unparseable-path condition))
         (diagnosis (file-unparseable-diagnosis condition))
         (readtable (file-unparseable-readtable condition))
         (scan-ok (getf diagnosis :ok))
         (line (getf diagnosis :unclosed-form-line))
         (fixes (getf diagnosis :likely-fixes))
         (head (if scan-ok
                   (format nil "Cannot parse ~A~@[ under readtable ~(~S~)~]: ~A"
                           path readtable (file-unparseable-cause condition))
                   (format-delimiter-diagnosis diagnosis :target path))))
    (cond
      ((file-unparseable-recoverable-p condition)
       ;; fs-write-file takes only a project-relative path, so that is the
       ;; form the instruction gives; the absolute one stays in the head.
       (let* ((root (ignore-errors
                     (ensure-directory-pathname
                      (truename (ensure-directory-pathname *project-root*)))))
              (relative (and root
                             (subpathp (pathname path) root)
                             (ignore-errors
                              (namestring (enough-pathname (pathname path) root))))))
         (if relative
             (format nil "~A~%The file itself does not parse~:[, so lisp-edit-form and ~
                          lisp-patch-form cannot locate any form in it~; past its ~
                          broken form: the forms before it can still be edited with ~
                          lisp-edit-form, but this one is in the broken tail~].~%~
                          Run lisp-check-parens with path=~S to see the full diagnosis, ~
                          then ~A"
                     head (file-unparseable-editable-prefix-p condition) path
                     (format-overwrite-recovery relative
                                                :have-fix (not (null fixes))
                                                :where "above"
                                                :form-line line
                                                :fix-line (or (and fixes
                                                                   (getf (first fixes) :line))
                                                              line
                                                              (getf diagnosis :line))))
             ;; Outside the project root: neither the structural tools nor
             ;; fs-write-file can touch it, so no recovery path is promised.
             (format nil "~A~%The file does not parse, and it is outside the project ~
                          root, so fs-write-file cannot rewrite it and lisp-edit-form ~
                          cannot locate any form in it; fix it outside cl-mcp."
                     head))))
      (readtable
       (format nil "~A~%No standard-syntax diagnosis is offered under a custom ~
                    readtable (a reader macro may consume raw parentheses). Run ~
                    lisp-check-parens with path=~S: if it reports a missing or stray ~
                    parenthesis and the file uses no custom syntax at that point, ~
                    fs-write-file with allow_unparseable_overwrite=true can rewrite ~
                    it (that guard judges the file with the default reader); ~
                    otherwise fix the custom syntax the reader complained about."
               head path))
      (t
       ;; The reader stopped on something other than a delimiter. When the
       ;; scan also found a delimiter problem, both are shown: the reader's
       ;; complaint is what blocks parsing, and the diagnosis above may be a
       ;; second, real problem or an artifact of custom syntax.
       (format nil "~A~@[~%The reader itself reported: ~A.~]~%~
                    ~:[This~;That reader-level failure~] is not a missing or ~
                    stray parenthesis, so the overwrite path does not apply and ~
                    fs-write-file keeps refusing to overwrite the file. It may ~
                    depend on a readtable: if the file uses custom reader macros, ~
                    pass the readtable parameter (a named-readtable designator) to ~
                    lisp-edit-form / lisp-patch-form~:[.~; -- the delimiter ~
                    diagnosis above comes from the standard-syntax scan and may ~
                    then turn out to be right, or to be that syntax.~]"
               head
               (and (not scan-ok) (file-unparseable-cause condition))
               (not scan-ok)
               (not scan-ok))))))

(define-condition file-unparseable-error (error)
  ((path :initarg :path :reader file-unparseable-path)
   (diagnosis :initarg :diagnosis :reader file-unparseable-diagnosis)
   (cause :initarg :cause :reader file-unparseable-cause)
   (readtable :initarg :readtable :initform nil
              :reader file-unparseable-readtable)
   (recoverable :initarg :recoverable :initform nil
                :reader file-unparseable-recoverable-p)
   (editable-prefix :initarg :editable-prefix :initform nil
                    :reader file-unparseable-editable-prefix-p))
  (:report (lambda (c s) (write-string (file-unparseable-message c) s)))
  (:documentation "Signaled when the target file cannot be parsed into top-level forms.
RECOVERABLE is T when the failure is a delimiter problem (missing or stray
parenthesis) that no readtable can fix; only then does fs-write-file permit
overwriting the file, and only then does the message advertise that path.
READTABLE is the designator the caller supplied, if any: under a custom
readtable no standard-syntax verdict is attached (DIAGNOSIS is then a plain
balanced plist and RECOVERABLE is NIL), and the message says so.
EDITABLE-PREFIX is T when the parse still returned the forms before the
breakage (the lenient CL-reader pass after an IN-READTABLE switch does), so
those forms remain editable and the message must not claim that no form can
be located."))

(defun make-file-unparseable-condition (abs text cause &key readtable editable-prefix)
  "Return a FILE-UNPARSEABLE-ERROR for the file at ABS whose TEXT failed to
parse with CAUSE, the condition PARSE-TOP-LEVEL-FORMS signalled or returned as
its second value. Under a caller-supplied READTABLE the standard delimiter scan
is not evidence (a reader macro may consume raw parentheses), so no scan-based
diagnosis or recoverable verdict is attached; the message explains the
situation instead. EDITABLE-PREFIX says the lenient pass returned the forms
before the breakage, which lisp-edit-form can still address. This is the one
place the classification is made: %LOCATE-TARGET-FORM signals the condition
through SIGNAL-FILE-UNPARSEABLE, and lisp-read-file renders its message under
the forms it could still show."
  (make-condition 'file-unparseable-error
                  :path (namestring abs)
                  :readtable readtable
                  :editable-prefix editable-prefix
                  :diagnosis (if readtable
                                 (list :ok t)
                                 (diagnose-delimiters text))
                  :recoverable (and (null readtable)
                                    (%delimiter-failure-p cause))
                  :cause (sanitize-condition-text cause)))

(defun signal-file-unparseable (abs text cause &key readtable editable-prefix)
  "Signal the FILE-UNPARSEABLE-ERROR MAKE-FILE-UNPARSEABLE-CONDITION builds for
ABS, TEXT and CAUSE. Never returns."
  (error (make-file-unparseable-condition abs text cause
                                         :readtable readtable
                                         :editable-prefix editable-prefix)))

(defconstant +edit-guard-version+ 1
  "The only value GUARD's version field may carry for CHECK-EDIT-GUARD to
accept it (design doc 2026-09-16-clos-describe-fail-closed, section 4.1).")

(defun %guard-conflict (reason expected actual)
  "Build one of CHECK-EDIT-GUARD's CONFLICT values: a plist (:REASON REASON
:EXPECTED EXPECTED :ACTUAL ACTUAL). EXPECTED and ACTUAL are run through
SANITIZE-FOR-JSON, since ACTUAL -- and sometimes EXPECTED -- echoes a value
read from GUARD, a caller-supplied argument; SANITIZE-FOR-JSON also coerces
a non-string value to one."
  (list :reason reason
        :expected (sanitize-for-json expected)
        :actual (sanitize-for-json actual)))

(define-condition edit-guard-conflict-error (error)
  ((conflict :initarg :conflict :reader edit-guard-conflict))
  (:report
   (lambda (condition stream)
     (let ((conflict (edit-guard-conflict condition)))
       (format stream
               "Edit guard conflict: ~A (expected: ~A; actual: ~A). Call ~
                clos-describe again for a fresh edit_guard and retry with ~
                it; do not retry without a guard or through another tool."
               (getf conflict :reason) (getf conflict :expected)
               (getf conflict :actual)))))
  (:documentation
   "Signaled by %LOCATE-TARGET-FORM when a caller's GUARD argument no longer
matches the file or form it was observed on (design doc section 4.2), on the
verdict of %CHECK-EDIT-GUARD-PRE-PARSE before the parse (checks 1-4) or of
CHECK-EDIT-GUARD once the target is matched (all six). CONFLICT (reader
EDIT-GUARD-CONFLICT) is a plist (:REASON string :EXPECTED string :ACTUAL
string) naming the first of the six checks that failed. Always signaled
before %LOCATE-TARGET-FORM returns a value, so its caller -- LISP-EDIT-FORM
in src/lisp-edit-form.lisp -- never sees, and so never writes, content that
disagrees with GUARD: no name-only fallback, no adopting the new digest and
continuing."))

(defun %guard-field (guard name)
  "Return GUARD's NAME field, or NIL when GUARD is not a hash-table or carries
no such field. GUARD is an edit_guard JSON object (design doc section 4.1)
that reached this file straight from a caller, so every field is read through
here rather than assuming the object has the shape it should."
  (and (hash-table-p guard) (gethash name guard)))

(defconstant +edit-guard-token-separator+ #\|
  "The character separating the fields of an edit guard token.

Chosen because none of the five fields written before abs_path can contain
it: a decimal version, two decimal offsets and two digests, each of which is
an algorithm name, a colon and hex digits.  Only abs_path can, which is why
the token carries it last and unsplit.")

(defun %encode-guard-path (path)
  "Return PATH with every character that would break a one-line token
percent-encoded: the separator, the percent sign that introduces an escape,
and every control character.

A path is the one field a caller does not choose, and on this platform it may
hold anything but a null byte.  A newline in it would put a line break inside
the printed [guard: ...] token, so a client copying the line it can see would
get half a guard and no way to tell -- the token is offered as something to
copy off one line, and this is what makes that true of every path."
  (with-output-to-string (out)
    (loop for ch across path
          for code = (char-code ch)
          do (if (or (char= ch #\%)
                     (char= ch +edit-guard-token-separator+)
                     (< code 32)
                     (= code 127))
                 (format out "%~2,'0X" code)
                 (write-char ch out)))))

(defun %decode-guard-path (text)
  "Return TEXT with the %XX escapes %ENCODE-GUARD-PATH wrote read back.

Returns NIL when an escape is truncated or is not two hexadecimal digits, so
a token damaged in transit is refused outright rather than decoded into some
other path that might still name a real file."
  (let ((out (make-string-output-stream))
        (i 0)
        (n (length text)))
    (loop while (< i n)
          do (let ((ch (char text i)))
               (if (char= ch #\%)
                   (let ((hi (and (< (+ i 1) n) (digit-char-p (char text (+ i 1)) 16)))
                         (lo (and (< (+ i 2) n) (digit-char-p (char text (+ i 2)) 16))))
                     (unless (and hi lo)
                       (return-from %decode-guard-path nil))
                     (write-char (code-char (+ (* 16 hi) lo)) out)
                     (incf i 3))
                   (progn (write-char ch out)
                          (incf i)))))
    (get-output-stream-string out)))

(defun format-edit-guard-token (guard)
  "Return GUARD, an edit_guard JSON object, as the one-line token clos-describe
prints in its content text and PARSE-EDIT-GUARD-TOKEN reads back:

  version|file_digest|form_start|form_end|form_digest|abs_path

separated by +EDIT-GUARD-TOKEN-SEPARATOR+, with abs_path percent-encoded by
%ENCODE-GUARD-PATH so that neither the separator nor a line break can occur
inside it, and last so that even an unencoded separator would not shear the
token.  These are exactly the six fields CHECK-EDIT-GUARD reads; GUARD's path
field is left out because no check reads it, and it would repeat abs_path's
bulk on every line.

Returns NIL when GUARD is not a hash-table or lacks any of the six, so a
partial token -- one CHECK-EDIT-GUARD would refuse over a field its holder
never saw -- is never printed.  The reason this exists at all: the edit_guard
object is a sibling JSON field of the tool result, and a client that renders
only content[].text never sees it, which left the guarded-edit workflow
documented in docs/tools.md unreachable from such a client."
  (let ((version (%guard-field guard "version"))
        (file-digest (%guard-field guard "file_digest"))
        (form-start (%guard-field guard "form_start"))
        (form-end (%guard-field guard "form_end"))
        (form-digest (%guard-field guard "form_digest"))
        (abs-path (%guard-field guard "abs_path")))
    (when (and (integerp version)
               (integerp form-start) (integerp form-end)
               (stringp file-digest) (plusp (length file-digest))
               (stringp form-digest) (plusp (length form-digest))
               (stringp abs-path) (plusp (length abs-path)))
      (with-output-to-string (out)
        (flet ((sep () (write-char +edit-guard-token-separator+ out)))
          ;; ~D rather than PRINC: it binds *PRINT-BASE* to 10 and
          ;; *PRINT-RADIX* to false, so an offset comes out as the decimal
          ;; PARSE-EDIT-GUARD-TOKEN reads back whatever printer control
          ;; variables happen to be bound around this call.
          (format out "~D" version) (sep)
          (write-string file-digest out) (sep)
          (format out "~D" form-start) (sep)
          (format out "~D" form-end) (sep)
          (write-string form-digest out) (sep)
          (write-string (%encode-guard-path abs-path) out))))))

(defun %split-edit-guard-token (token)
  "Return TOKEN's six fields as a list of strings, or NIL when it holds fewer.
Only the first five separators split; the sixth field is whatever is left, so
an abs_path containing +EDIT-GUARD-TOKEN-SEPARATOR+ comes back whole."
  (let ((fields '())
        (start 0))
    (loop repeat 5
          do (let ((sep (position +edit-guard-token-separator+ token :start start)))
               (unless sep
                 (return-from %split-edit-guard-token nil))
               (push (subseq token start sep) fields)
               (setf start (1+ sep))))
    (nreverse (cons (subseq token start) fields))))

(defun %parse-guard-offset (text)
  "Return TEXT as a non-negative integer, or NIL unless TEXT is written as one
in full: PARSE-INTEGER with :junk-allowed stops at the first non-digit and
would read \"12abc\" as 12, which must not pass for an offset into a file."
  (multiple-value-bind (value end)
      (parse-integer text :junk-allowed t)
    (and value (= end (length text)) (<= 0 value) value)))

(defun parse-edit-guard-token (token)
  "Return the edit_guard object TOKEN encodes: a hash-table carrying the six
fields CHECK-EDIT-GUARD reads, built from the string FORMAT-EDIT-GUARD-TOKEN
wrote.  No check is run here; the object goes on to the same six.

Signals EDIT-GUARD-CONFLICT-ERROR when TOKEN is not one, rather than
returning NIL.  A token a caller mistyped, truncated or copied from the wrong
line has to refuse the edit exactly as a stale guard does: returning NIL would
let the call continue as if no guard had been asked for, which is the one
outcome a caller that passed a guard must never get."
  (let ((fields (and (stringp token) (%split-edit-guard-token token))))
    (destructuring-bind (&optional version file-digest form-start form-end
                         form-digest abs-path)
        (or fields '())
      (let ((version-value (and version (%parse-guard-offset version)))
            (start-value (and form-start (%parse-guard-offset form-start)))
            (end-value (and form-end (%parse-guard-offset form-end)))
            (path-value (and abs-path (%decode-guard-path abs-path))))
        (unless (and version-value start-value end-value
                     (plusp (length file-digest))
                     (plusp (length form-digest))
                     (plusp (length path-value)))
          (error 'edit-guard-conflict-error
                 :conflict
                 (%guard-conflict
                  (concatenate 'string
                               "guard token is malformed; copy the [guard: ...] token "
                               "clos-describe printed, verbatim and whole")
                  "version|file_digest|form_start|form_end|form_digest|abs_path"
                  token)))
        (let ((guard (make-hash-table :test #'equal)))
          (setf (gethash "version" guard) version-value
                (gethash "file_digest" guard) file-digest
                (gethash "form_start" guard) start-value
                (gethash "form_end" guard) end-value
                (gethash "form_digest" guard) form-digest
                (gethash "abs_path" guard) path-value)
          guard)))))

(defun normalize-edit-guard (guard)
  "Return GUARD as the hash-table the six checks read.

A hash-table is the edit_guard JSON object itself and is returned unchanged.
A string is the compact token clos-describe prints, and is read by
PARSE-EDIT-GUARD-TOKEN -- which signals rather than returning NIL when it is
malformed, so a guard that cannot be understood refuses the edit instead of
silently becoming no guard at all.  NIL means no guard was asked for."
  (if (stringp guard)
      (parse-edit-guard-token guard)
      guard))

(defun %check-edit-guard-pre-parse (guard abs-path snapshot)
  "Run checks 1-4 of CHECK-EDIT-GUARD against GUARD, ABS-PATH and SNAPSHOT --
the checks that need no matched form, and so can run before SNAPSHOT's text is
parsed. Returns (VALUES OK-P CONFLICT) in the same shape CHECK-EDIT-GUARD
returns, CONFLICT naming the first of the four, in order, that failed:

 1. GUARD's version is the one this function supports (+EDIT-GUARD-VERSION+).
 2. GUARD's abs_path names the same file as ABS-PATH.
 3. GUARD's file_digest matches SNAPSHOT's own digest of the whole file.
 4. GUARD's form_start/form_end lie within SNAPSHOT's text, with form_end
    greater than form_start.

%LOCATE-TARGET-FORM runs these as soon as it has SNAPSHOT, so a file that
changed after GUARD observed it is reported as a guard conflict even when the
lookup that follows would fail -- the observed form renamed or deleted, its
name now ambiguous, or the file no longer parsing at all. Without this early
pass those cases end in a plain \"not found\", \"Multiple matches\" or
unparseable-file error that says nothing about the guard, even though the
change the guard exists to catch is exactly what caused them."
  (let ((version (%guard-field guard "version"))
        (guard-abs-path (%guard-field guard "abs_path"))
        (guard-file-digest (%guard-field guard "file_digest"))
        (form-start (%guard-field guard "form_start"))
        (form-end (%guard-field guard "form_end"))
        (text (getf snapshot :text))
        (file-digest (getf snapshot :digest)))
    (cond
      ((not (eql version +edit-guard-version+))
       (values nil (%guard-conflict "unsupported guard version"
                                     +edit-guard-version+ version)))
      ((not (and (stringp guard-abs-path) (string= guard-abs-path abs-path)))
       (values nil (%guard-conflict "abs_path does not match the file being edited"
                                     abs-path guard-abs-path)))
      ((not (and (stringp guard-file-digest) (stringp file-digest)
                 (string= guard-file-digest file-digest)))
       (values nil (%guard-conflict
                    "file changed since the guard observed it (file_digest mismatch)"
                    guard-file-digest (or file-digest "unavailable"))))
      ((not (and (integerp form-start) (integerp form-end)
                 (<= 0 form-start) (<= form-end (length text))
                 (> form-end form-start)))
       (values nil (%guard-conflict
                    "form_start/form_end are not a valid range in the file"
                    (format nil "0 <= form_start < form_end <= ~D" (length text))
                    (format nil "form_start=~A form_end=~A" form-start form-end))))
      (t (values t nil)))))

(defun %check-edit-guard-post-match (guard snapshot node)
  "Run checks 5-6 of CHECK-EDIT-GUARD against GUARD, SNAPSHOT and NODE -- the
checks that need NODE, the CST node LOCATE-FORM-IN-NODES matched by form_type
and form_name in that same SNAPSHOT, and so can only run once the lookup has
succeeded. Returns (VALUES OK-P CONFLICT) in the same shape
CHECK-EDIT-GUARD returns, CONFLICT naming the first of the two that failed:

 5. NODE's own CST span is exactly form_start/form_end -- the form a plain
    form_type/form_name search resolves to today is the same span GUARD
    observed, not a different definition that merely shares the name.
 6. When GUARD carries a form_digest, it matches SNAPSHOT-RANGE-DIGEST of
    that range. Absent entirely, this check is skipped; a non-NIL value
    that is not a matching digest string still fails it.

Assumes %CHECK-EDIT-GUARD-PRE-PARSE has already passed, which is what makes
form_start/form_end safe to compare here: CHECK-EDIT-GUARD runs both parts in
order, and %LOCATE-TARGET-FORM reaches the lookup only after the pre-parse
part passed."
  (let ((form-start (%guard-field guard "form_start"))
        (form-end (%guard-field guard "form_end"))
        (guard-form-digest (%guard-field guard "form_digest")))
    (cond
      ((not (and (= (cst-node-start node) form-start)
                 (= (cst-node-end node) form-end)))
       (values nil (%guard-conflict
                    "the form moved, was replaced, or was deleted since the guard observed it"
                    (format nil "start=~D end=~D" form-start form-end)
                    (format nil "start=~D end=~D"
                            (cst-node-start node) (cst-node-end node)))))
      ((and guard-form-digest
            (not (and (stringp guard-form-digest)
                      (equal guard-form-digest
                             (snapshot-range-digest snapshot form-start form-end)))))
       (values nil (%guard-conflict
                    "form content changed since the guard observed it (form_digest mismatch)"
                    guard-form-digest
                    (or (snapshot-range-digest snapshot form-start form-end)
                        "unavailable"))))
      (t (values t nil)))))

(defun check-edit-guard (guard abs-path snapshot node)
  "Verify GUARD, an edit_guard JSON object (design doc section 4.1), against
ABS-PATH (a namestring for the file about to be edited), SNAPSHOT (a plist
from CL-MCP/SRC/SOURCE-SNAPSHOT:READ-SOURCE-SNAPSHOT read for this same
edit), and NODE (the CST node LOCATE-FORM-IN-NODES matched by form_type and
form_name in that same SNAPSHOT). Returns (VALUES OK-P CONFLICT): OK-P is T
when every check below passes, with CONFLICT then NIL; otherwise OK-P is NIL
and CONFLICT is a plist (:REASON string :EXPECTED string :ACTUAL string)
naming the first check, in order, that failed.

Never reads or parses anything itself: SNAPSHOT and NODE are taken as given,
so this always judges the exact bytes %LOCATE-TARGET-FORM is about to splice
an edit into, never a second, possibly different, read.

The checks, in order (design doc section 4.2), split over two functions by
what each needs:
 1. GUARD's version is the one this function supports (+EDIT-GUARD-VERSION+).
 2. GUARD's abs_path names the same file as ABS-PATH.
 3. GUARD's file_digest matches SNAPSHOT's own digest of the whole file.
 4. GUARD's form_start/form_end lie within SNAPSHOT's text, with form_end
    greater than form_start.
      -- 1-4 are %CHECK-EDIT-GUARD-PRE-PARSE: no matched form needed.
 5. NODE's own CST span is exactly form_start/form_end -- the form a plain
    form_type/form_name search resolves to today is the same span GUARD
    observed, not a different definition that merely shares the name.
 6. When GUARD carries a form_digest, it matches SNAPSHOT-RANGE-DIGEST of
    that range. Absent entirely, this check is skipped; a non-NIL value
    that is not a matching digest string still fails it.
      -- 5-6 are %CHECK-EDIT-GUARD-POST-MATCH: NODE needed.

%LOCATE-TARGET-FORM calls %CHECK-EDIT-GUARD-PRE-PARSE by itself, before the
parse, so a file that changed after GUARD observed it conflicts even when the
lookup that would produce NODE fails; it then calls this function at the point
where NODE exists. Re-running 1-4 here judges the same GUARD against the same
SNAPSHOT and so cannot reach a different verdict, and costs four comparisons
against values already in hand -- worth it to keep all six checks and their
order visible at the call site that decides whether the edit proceeds."
  (multiple-value-bind (ok-p conflict)
      (%check-edit-guard-pre-parse guard abs-path snapshot)
    (if ok-p
        (%check-edit-guard-post-match guard snapshot node)
        (values nil conflict))))

(defun %locate-target-form (file-path form-type form-name readtable &optional guard)
  "Shared prologue: resolve paths, read file, parse, find target, extract snippet.
Signals FILE-UNPARSEABLE-ERROR (through SIGNAL-FILE-UNPARSEABLE, which owns the
classification), carrying a delimiter diagnosis, when the file cannot be parsed
at all, or when the target form is not found and the lenient CL-reader pass
(after an IN-READTABLE switch, or under a READTABLE argument) stopped early on
a read error; on that lenient pass the forms before the breakage remain
editable, whereas the Eclector pass yields no forms at all from a file that
does not parse. A file larger than the fs read cap is reported as such
instead, because its truncated prefix would only yield a misleading delimiter
diagnosis.

GUARD, when non-NIL, is an edit_guard JSON object (design doc section 4.1), or
the compact token clos-describe prints for one, which NORMALIZE-EDIT-GUARD
reads into the same object before anything else happens -- a token that cannot
be read signals EDIT-GUARD-CONFLICT-ERROR here rather than degrading into an
unguarded edit.  It changes how the file is read: instead of FS-READ-FILE, the file is read
once via CL-MCP/SRC/SOURCE-SNAPSHOT:READ-SOURCE-SNAPSHOT, so ORIGINAL (below)
and the digests the guard is verified against come from the exact same bytes
-- this function never reads the file twice for one call. The six checks of
design doc section 4.2 run in two parts, each as early as it can:
%CHECK-EDIT-GUARD-PRE-PARSE (checks 1-4) as soon as the snapshot is in hand,
before the parse, and CHECK-EDIT-GUARD (all six, its first four a free re-run)
once TARGET is located. EDIT-GUARD-CONFLICT-ERROR is signaled on the first
failing check, before any value is returned, so a stale or mismatched GUARD
never reaches a write. Splitting it this way is what makes a file that changed
since GUARD observed it conflict even when the lookup cannot finish: the
observed form renamed or deleted, its name now matching several forms, or the
file no longer parsing all reach the early check first and report the
file_digest mismatch. When the file is unchanged, none of those is a guard
problem, so a form_type/form_name the caller got wrong still gets the ordinary
\"not found\" or \"Multiple matches\" error.
READ-SOURCE-SNAPSHOT never truncates, so this path re-applies
CL-MCP/SRC/FS:*FS-READ-MAX-BYTES* by hand against the whole text it read,
refusing (not truncating) a file over the same limit FS-READ-FILE enforces
below -- a GUARD never lets this tool read more than an unguarded call
could. A file that is not valid UTF-8 is refused there too
(SNAPSHOT-DECODE-LOSSY-P), with a plain error rather than an
EDIT-GUARD-CONFLICT-ERROR: the snapshot's text has already replaced that
file's invalid bytes with #\\?, so writing it back would destroy them, and
the unguarded path's FS-READ-FILE refuses the same file with a decoding
error. Without GUARD, behavior is unchanged.

Returns eight values:
  ABS — absolute pathname
  REL — relative namestring for FS write
  ORIGINAL — full file text
  NODES — parsed CST nodes
  TARGET — matched CST node
  TARGET-SNIPPET — text of the matched form
  FORM-TYPE-STR — downcased form-type string
  FILE-PACKAGE-NAME — package named by the file's first IN-PACKAGE form"
  (let ((form-type-str (string-downcase form-type))
        (guard (normalize-edit-guard guard)))
    (multiple-value-bind (abs rel)
        (%normalize-paths file-path)
      (let (original snapshot)
        (if guard
            (multiple-value-bind (snap failure) (read-source-snapshot abs)
              (when (null snap)
                (error "Cannot read ~A to verify guard: ~A" (namestring abs)
                       (if (eq failure :denied)
                           "read not permitted for this path"
                           failure)))
              (let ((text (getf snap :text)))
                ;; READ-SOURCE-SNAPSHOT never truncates, so the read cap
                ;; FS-READ-FILE enforces below must be re-applied here by
                ;; hand: a guarded call must refuse a file the unguarded
                ;; path would refuse too, not read it in full instead.
                (when (> (length text) *fs-read-max-bytes*)
                  (error "~A exceeds the read limit (~D characters); ~
                          lisp-edit-form and lisp-patch-form cannot edit files ~
                          this large, and fs-write-file will not overwrite it ~
                          either. Split the file or edit it outside cl-mcp."
                         (namestring abs) (length text)))
                ;; The snapshot decodes an invalid byte to #\? (its :TEXT is
                ;; what would be written back), so a file that is not valid
                ;; UTF-8 must be refused outright: the unguarded path's
                ;; FS-READ-FILE signals a decoding error on it, and a guarded
                ;; call must not quietly rewrite bytes it could not read.
                ;; This is a plain refusal, not a guard conflict -- nothing
                ;; about GUARD is wrong -- and it comes before the parse and
                ;; before CHECK-EDIT-GUARD.
                (when (snapshot-decode-lossy-p snap)
                  (error "~A is not valid UTF-8: reading it replaced at least one byte ~
                          with #\\?, and writing the file back would destroy that byte. ~
                          lisp-edit-form and lisp-patch-form cannot edit this file; ~
                          fix its encoding first."
                         (namestring abs)))
                (setf snapshot snap
                      original text)
                ;; Guard checks 1-4 (design doc section 4.2) need only the
                ;; guard, the path and this snapshot, so they run here rather
                ;; than only after the lookup: a file that changed since the
                ;; guard observed it must be reported as a conflict even when
                ;; the parse or the form lookup below fails first -- that
                ;; change is precisely what the guard exists to catch.
                (multiple-value-bind (ok-p conflict)
                    (%check-edit-guard-pre-parse guard (namestring abs) snapshot)
                  (unless ok-p
                    (error 'edit-guard-conflict-error :conflict conflict)))))
            (multiple-value-bind (text truncated file-length) (fs-read-file abs)
              (when truncated
                (error "~A exceeds the read limit (~@[~D bytes, ~]only ~D characters read); ~
                        lisp-edit-form and lisp-patch-form cannot edit files this large, ~
                        and fs-write-file will not overwrite it either (a truncated read ~
                        cannot prove the file is broken). Split the file or edit it ~
                        outside cl-mcp."
                       (namestring abs) file-length (length text)))
              (setf original text)))
        (multiple-value-bind (nodes swallowed)
            (handler-case
                (parse-top-level-forms original
                                       :readtable readtable
                                       :source-path abs)
              (error (e)
                (signal-file-unparseable abs original e :readtable readtable)))
          (let ((target (%find-target nodes form-type-str form-name)))
            (unless target
              (when swallowed
                ;; The lenient pass returned the forms before the breakage,
                ;; which lisp-edit-form can still address.
                (signal-file-unparseable abs original swallowed
                                        :readtable readtable
                                        :editable-prefix (and nodes t)))
              (error "Form ~A ~A not found in ~A" form-type form-name
                     (namestring abs)))
            (when guard
              (multiple-value-bind (ok-p conflict)
                  (check-edit-guard guard (namestring abs) snapshot target)
                (unless ok-p
                  (error 'edit-guard-conflict-error :conflict conflict))))
            (let ((target-snippet (subseq original
                                          (cst-node-start target)
                                          (cst-node-end target))))
              (values abs rel original nodes target target-snippet form-type-str
                      (extract-in-package-name-from-text original)))))))))

(defun %file-unparseable-by-edit-tools-p (pn &optional text)
  "Return T when the file at PN is broken in a way no readtable can fix:
PARSE-TOP-LEVEL-FORMS fails (it signals, or its lenient CL-reader pass after
an IN-READTABLE switch stopped early, reported as a second value) with a
delimiter failure per %DELIMITER-FAILURE-P. Any other reader failure -- an
unknown dispatch macro such as #? that may even consume delimiter-looking
characters as data -- is not evidence, since the tools' readtable parameter
may make the file editable, so the overwrite guard must stay in place.
The second value says why: :DELIMITER (the primary value is T), :PARSED (the
file parses cleanly, so a scan verdict against it is a false positive),
:READER-LEVEL (it fails, but not on a delimiter), or :TRUNCATED. The third
value is T when the failing parse still returned forms (the lenient
CL-reader pass after an IN-READTABLE switch keeps the forms before the
breakage), so those forms remain editable with lisp-edit-form and the
guidance must not claim that no form can be located; callers consult it
only with a :DELIMITER verdict.
TEXT, when supplied by the caller (fs's %LISP-FILE-UNPARSEABLE-P has already
read the file), avoids a second read; otherwise the file is read here, and a read truncated
at the fs read cap returns NIL because a cut-off prefix of a valid file
would look unparseable.
Installed into cl-mcp/src/fs:*lisp-file-unparseable-hook* so fs-write-file
permits overwriting only files that are broken this way, and called directly
by lisp-check-parens so its next-step hint rests on the same verdict."
  (multiple-value-bind (source truncated)
      (if text (values text nil) (fs-read-file pn))
    (if truncated
        (values nil :truncated nil)
        (handler-case
            (multiple-value-bind (nodes swallowed)
                (parse-top-level-forms source :source-path pn)
              (cond ((null swallowed) (values nil :parsed nil))
                    ((%delimiter-failure-p swallowed)
                     (values t :delimiter (and nodes t)))
                    (t (values nil :reader-level (and nodes t)))))
          (error (e)
            (if (%delimiter-failure-p e)
                (values t :delimiter nil)
                (values nil :reader-level nil)))))))

;; Register at load time so fs-write-file's overwrite guard agrees with the
;; edit tools about which files are unparseable.
(setf *lisp-file-unparseable-hook* #'%file-unparseable-by-edit-tools-p)
