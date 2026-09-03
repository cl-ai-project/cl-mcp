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
                #:stray-right-parenthesis)
  (:import-from #:cl-mcp/src/package-context
                #:extract-in-package-name-from-text)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/fs
                #:*lisp-file-unparseable-hook*
                #:fs-read-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message)
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
           #:%resolve-named-readtable
           #:%parse-readtable-designator
           #:%detect-readtable-before-node
           #:%whitespace-char-p
           #:%locate-target-form
           #:file-unparseable-error
           #:file-unparseable-path
           #:file-unparseable-diagnosis
           #:file-unparseable-cause
           #:file-unparseable-message))

(in-package #:cl-mcp/src/lisp-edit-form-core)

(defun %normalize-string (thing)
  "Normalize THING to a lowercase string for form matching.
Uses SYMBOL-NAME for symbols to avoid package prefix in the output."
  (string-downcase
   (if (symbolp thing)
       (symbol-name thing)
       (princ-to-string thing))))

(defun %defmethod-candidates (form)
  "Return candidate signature strings for a DEFMETHOD FORM.
Candidates are generated in order of specificity:
1. name only: \"resize\"
2. name + qualifier: \"resize :after\"
3. name + lambda-list: \"resize ((s shape) factor)\"
4. name + qualifier + lambda-list: \"resize :after ((s shape) factor)\"

Every candidate is passed through %STRIP-HASH-COLON so that lambda-list
prints from package-inferred-system sources (which surface uninterned
symbols as '#:foo') compare equal to user inputs written without the
'#:' reader-macro prefix."
  (destructuring-bind
      (_ name &rest rest)
      form
    (declare (ignore _))
    (let ((qualifiers 'nil) (lambda-list nil))
      (dolist (part rest)
        (when (listp part) (setf lambda-list part) (return))
        (push part qualifiers))
      (let ((name-str (%normalize-string name))
            (lambda-str
             (and lambda-list
                  (%strip-hash-colon
                   (%normalize-string
                    (with-output-to-string (s) (prin1 lambda-list s))))))
            (qual-str
             (and qualifiers
                  (%strip-hash-colon
                   (%normalize-string
                    (format nil "~{~S~^ ~}" (nreverse qualifiers)))))))
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
  "Return two values: absolute path (pathname) and relative namestring for FS tools."
  (let ((resolved (fs-resolve-read-path file-path))
        (root (ensure-directory-pathname *project-root*)))
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

(defun %find-target (nodes form-type form-name)
  "Find a target node matching FORM-TYPE and FORM-NAME.
If FORM-NAME ends with [N] (e.g., 'resize[1]'), select the Nth match (0-indexed).
If multiple matches exist without an index, signals an error with candidate info."
  (multiple-value-bind (base-name index)
      (let ((match (nth-value 1 (scan-to-strings "^(.+?)\\[(\\d+)\\]$" form-name))))
        (if match
            (values (aref match 0) (parse-integer (aref match 1)))
            (values form-name nil)))
    (let ((target (%strip-hash-colon
                   (string-downcase (%strip-name-prefix base-name))))
          (matches nil))
      (when (zerop (length target))
        (error "form_name resolved to empty string after prefix stripping; ~
provide a non-empty name (e.g. \"my-pkg\" instead of \"#:\" alone)"))
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
      (cond
        ((null matches)
         nil)
        ((and index (< index (length matches)))
         (car (nth index matches)))
        (index
         (error "Index [~D] out of range, only ~D match~:P found for ~A"
                index (length matches) form-name))
        ((= (length matches) 1)
         (car (first matches)))
        (t
         ;; Multiple matches without index - provide helpful error
         (let ((descriptions
                 (loop for (node . form) in matches
                       for i from 0
                       collect (format nil "[~D] ~A"
                                       i
                                       (let ((candidates (%definition-candidates form form-type)))
                                         (or (car (last candidates)) (first candidates)))))))
           (error "Multiple matches for ~A ~A. Specify an index:~%~{  ~A~%~}"
                  form-type form-name descriptions)))))))

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
  "Return T when CONDITION is a parse failure the default reader attributes to
delimiters: a missing \")\" (END-OF-FILE, including cst's UNTERMINATED-SOURCE)
or a stray \")\" (cst's STRAY-RIGHT-PARENTHESIS, which cst raises itself on
both its parsing paths). Every other reader failure -- an unknown dispatch
macro such as #?, a disabled #. -- is not counted. Even a delimiter failure
is not proof that the file is broken (a reader macro supplied through the
readtable parameter could consume the offending characters), which is why
fs-write-file additionally requires the caller to opt in before overwriting."
  (or (typep condition 'end-of-file)
      (typep condition 'stray-right-parenthesis)))

(defun file-unparseable-message (condition)
  "Return the guidance text for CONDITION, a FILE-UNPARSEABLE-ERROR.
When the failure is recoverable (a delimiter problem no readtable can fix),
the text opens with the shared delimiter diagnosis, or the reader error when
the scan has nothing to add, and ends with an executable recovery path:
fs-read-file, hand-apply the fix, fs-write-file (which permits overwriting
such a file). Otherwise the failure is reader-level (custom reader syntax, a
disabled #. form); the file keeps its overwrite protection, so the text
points at the readtable parameter instead."
  (let* ((path (file-unparseable-path condition))
         (diagnosis (file-unparseable-diagnosis condition))
         (scan-ok (getf diagnosis :ok))
         (line (getf diagnosis :unclosed-form-line))
         (head (if scan-ok
                   (format nil "Cannot parse ~A: ~A" path (file-unparseable-cause condition))
                   (format-delimiter-diagnosis diagnosis :target path))))
    (if (file-unparseable-recoverable-p condition)
        (format nil "~A~%The file itself does not parse, so lisp-edit-form and ~
                     lisp-patch-form cannot locate any form in it.~%~
                     Run lisp-check-parens with path=~S to see the full diagnosis, ~
                     then read the file with fs-read-file, apply the change~
                     ~:[ described above~; shown under \"Likely fix\"~]~
                     ~@[ to the form starting at line ~D~], and ~
                     write the whole file back with fs-write-file, passing ~
                     allow_unparseable_overwrite=true (it refuses to overwrite an ~
                     existing Lisp file otherwise). If the file uses custom reader ~
                     syntax that the default reader cannot parse, pass the readtable ~
                     parameter to lisp-edit-form instead of overwriting."
                head path (not scan-ok) line)
        (format nil "~A~%This is a reader-level failure, not a missing or stray ~
                     parenthesis, so it may depend on a readtable: if the file uses ~
                     custom reader macros, pass the readtable parameter (a ~
                     named-readtable designator) to lisp-edit-form / lisp-patch-form. ~
                     fs-write-file keeps refusing to overwrite it."
                head))))

(define-condition file-unparseable-error (error)
  ((path :initarg :path :reader file-unparseable-path)
   (diagnosis :initarg :diagnosis :reader file-unparseable-diagnosis)
   (cause :initarg :cause :reader file-unparseable-cause)
   (recoverable :initarg :recoverable :initform nil
                :reader file-unparseable-recoverable-p))
  (:report (lambda (c s) (write-string (file-unparseable-message c) s)))
  (:documentation "Signaled when the target file cannot be parsed into top-level forms.
RECOVERABLE is T when the failure is a delimiter problem (missing or stray
parenthesis) that no readtable can fix; only then does fs-write-file permit
overwriting the file, and only then does the message advertise that path."))

(defun %locate-target-form (file-path form-type form-name readtable)
  "Shared prologue: resolve paths, read file, parse, find target, extract snippet.
Signals FILE-UNPARSEABLE-ERROR, carrying a delimiter diagnosis, when the file
cannot be parsed at all, or when the target form is not found and the lenient
CL-reader pass (after an IN-READTABLE switch) stopped early on a read error;
forms before such a breakage remain editable. A file larger than the fs read
cap is reported as such instead, because its truncated prefix would only
yield a misleading delimiter diagnosis.
Returns eight values:
  ABS — absolute pathname
  REL — relative namestring for FS write
  ORIGINAL — full file text
  NODES — parsed CST nodes
  TARGET — matched CST node
  TARGET-SNIPPET — text of the matched form
  FORM-TYPE-STR — downcased form-type string
  FILE-PACKAGE-NAME — package named by the file's first IN-PACKAGE form"
  (let ((form-type-str (string-downcase form-type)))
    (multiple-value-bind (abs rel)
        (%normalize-paths file-path)
      (multiple-value-bind (original truncated file-length)
          (fs-read-file abs)
        (when truncated
          (error "~A exceeds the read limit (~@[~D characters, ~]only ~D read); ~
                  lisp-edit-form and lisp-patch-form cannot edit files this large"
                 (namestring abs) file-length (length original)))
        (flet ((unparseable (cause)
                 (error 'file-unparseable-error
                        :path (namestring abs)
                        :diagnosis (diagnose-delimiters original)
                        :recoverable (%delimiter-failure-p cause)
                        :cause (sanitize-error-message (princ-to-string cause)))))
          (multiple-value-bind (nodes swallowed)
              (handler-case
                  (parse-top-level-forms original
                                         :readtable readtable
                                         :source-path abs)
                (error (e) (unparseable e)))
            (let ((target (%find-target nodes form-type-str form-name)))
              (unless target
                (when swallowed
                  (unparseable swallowed))
                (error "Form ~A ~A not found in ~A" form-type form-name
                       (namestring abs)))
              (let ((target-snippet (subseq original
                                           (cst-node-start target)
                                           (cst-node-end target))))
                (values abs rel original nodes target target-snippet form-type-str
                        (extract-in-package-name-from-text original))))))))))

(defun %file-unparseable-by-edit-tools-p (pn)
  "Return T when the file at PN is broken in a way no readtable can fix:
PARSE-TOP-LEVEL-FORMS fails (it signals, or its lenient CL-reader pass after
an IN-READTABLE switch stopped early, reported as a second value) with a
delimiter failure per %DELIMITER-FAILURE-P. Any other reader failure -- an
unknown dispatch macro such as #? that may even consume delimiter-looking
characters as data -- is not evidence, since the tools' readtable parameter
may make the file editable, so the overwrite guard must stay in place.
Installed into cl-mcp/src/fs:*lisp-file-unparseable-hook* so fs-write-file
permits overwriting exactly the broken files. A read truncated at the fs
read cap returns NIL: a cut-off prefix of a valid file would look unparseable."
  (multiple-value-bind (text truncated) (fs-read-file pn)
    (and (not truncated)
         (handler-case
             (multiple-value-bind (nodes swallowed)
                 (parse-top-level-forms text :source-path pn)
               (declare (ignore nodes))
               (and swallowed (%delimiter-failure-p swallowed) t))
           (error (e) (%delimiter-failure-p e))))))

;; Register at load time so fs-write-file's overwrite guard agrees with the
;; edit tools about which files are unparseable.
(setf *lisp-file-unparseable-hook* #'%file-unparseable-by-edit-tools-p)
