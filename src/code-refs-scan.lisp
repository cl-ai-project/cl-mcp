;;;; src/code-refs-scan.lisp
;;;;
;;;; Parent-side half of code-find-references' impact analysis.  It finds
;;;; every place a symbol name is written in the project's Lisp files,
;;;; classifies the position (call, quoted, bound, ...), and records the
;;;; enclosing top-level form.  It does not decide which symbol a site names:
;;;; the parent image does not have the user's packages, so the worker does
;;;; that (CL-MCP/SRC/CODE-REFS-CORE:RESOLVE-SCAN-FORMS).

(defpackage #:cl-mcp/src/code-refs-scan
  (:use #:cl)
  (:import-from #:cl-mcp/src/cst
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line
                #:cst-node-end-line
                #:parse-top-level-forms
                #:%in-package-form-p)
  (:import-from #:cl-mcp/src/package-context
                #:*package-spec-discovery-cache*)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%definition-candidates)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:collect-target-files)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-source-text)
  (:import-from #:cl-mcp/src/utils/paths
                #:allowed-read-path
                #:normalize-path-for-display)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/src/code-refs-core
                #:parse-target-designator
                #:find-package-named)
  (:export #:*max-scan-sites*
           #:target-name-from-designator
           #:scan-text
           #:scan-project
           #:top-level-forms-at))

(in-package #:cl-mcp/src/code-refs-scan)

(defparameter *max-scan-sites* 5000
  "Most sites one scan collects.  The rest are dropped and the scan says so,
which keeps the request the parent sends the worker bounded.")

(defparameter *context-width* 160
  "Characters of a source line kept as a site's or a form's context.")

(defparameter *test-frameworks*
  '(("DEFTEST" . "rove") ("TEST" . "fiveam") ("DEF-TEST" . "fiveam")
    ("DEFINE-TEST" . "parachute"))
  "Head names of test-defining forms, with the framework each usually means.")

(defparameter *lambda-list-positions*
  '(("DEFUN" . 1) ("DEFMACRO" . 1) ("DEFGENERIC" . 1)
    ("DEFINE-COMPILER-MACRO" . 1) ("LAMBDA" . 0)
    ("DESTRUCTURING-BIND" . 0) ("MULTIPLE-VALUE-BIND" . 0))
  "Operators whose argument at the given position is a lambda list, counting
the operator's arguments from 0.  For the DEF forms argument 0 is the name.")

(defparameter *shadowing-operators* '("FLET" "LABELS" "MACROLET")
  "Operators whose bindings shadow a global function or macro of the same name.")

(defparameter *function-designator-operators* '("FUNCALL" "APPLY" "MULTIPLE-VALUE-CALL")
  "Operators whose first argument designates the function they call.  A quoted
name there -- (funcall 'name ...) -- names that function the way #'name does,
and SBCL's WHO-CALLS records it as a call, so its site is \"function\", not
\"quoted\".")

(defparameter *place-modifying-operators*
  '(("INCF" . 0) ("DECF" . 0) ("POP" . 0) ("PUSH" . 1) ("PUSHNEW" . 1))
  "Macros that read and write the place at the given argument position, counting
from 0.  A bare symbol there is a \"set\" site, as a SETF place is; SBCL records
both a set and a reference for it.  The other arguments are ordinary code.")

(defparameter *definers-with-name-and-options* '("DEFSTRUCT")
  "DEF... operators, beyond the specially handled ones, whose argument 0 --
when it is not a bare name or a (SETF name) list -- is (NAME . OPTIONS) and
must still be skipped as a whole, rather than walked as ordinary code.")

(defun target-name-from-designator (designator)
  "Return the symbol name DESIGNATOR spells, as the reader would read it.
Signals ARG-VALIDATION-ERROR for text that is not a symbol name or is a
keyword, before any file is read."
  (values (parse-target-designator designator)))

(defun %expr-children (node)
  "Return NODE's children that are expressions, skipping comments."
  (remove-if-not (lambda (child) (eq (cst-node-kind child) :expr))
                 (cst-node-children node)))

(defun %unwrap (node)
  "Return the innermost node carrying the same value as NODE.  A #+feature or
#-feature reader conditional's node value is the wrapped form's value, but its
start/end span the #+/#- prefix too, and its children are the feature
expression plus the wrapped form -- not the wrapped form's own children.  This
steps into the child that carries the real token or sub-form, repeating for
nested conditionals, so positions, tokens and further structural checks (EMIT,
and anything that reads NODE's own children) see only the wrapped form."
  (loop for child = (find (cst-node-value node) (cst-node-children node)
                          :key #'cst-node-value :test #'eq)
        while child
        do (setf node child))
  node)

(defun %definer-name-p (name)
  "True when NAME, a head's symbol name, names a DEF... form."
  (and (> (length name) 3) (string= "DEF" (subseq name 0 3))))

(defun %definer-name-position-p (node)
  "True when NODE's value looks like a DEF...'s name argument: a bare symbol,
or a (SETF symbol) list naming a setf function.  A user macro merely named
DEF... (or a CASE clause whose key happens to read as a DEF...-prefixed
symbol) is not guaranteed to have any such shape in its first argument."
  (let ((value (cst-node-value node)))
    (or (symbolp value)
        (and (consp value)
             (symbolp (car value))
             (string= (symbol-name (car value)) "SETF")
             (consp (cdr value))
             (symbolp (cadr value))
             (null (cddr value))))))

(defun %eclector-marker-p (symbol name)
  "True when SYMBOL is eclector's own backquote marker NAME.
Eclector reads `x as (ECLECTOR.READER:QUASIQUOTE x) and ,x as
(ECLECTOR.READER:UNQUOTE x); comparing the package keeps a user's own UNQUOTE
function from being taken for one."
  (and (symbolp symbol)
       (string= (symbol-name symbol) name)
       (let ((package (symbol-package symbol)))
         (and package (string= (package-name package) "ECLECTOR.READER")))))

(defun %quoted-symbol-node (node)
  "Return the node of the symbol NODE quotes, or NIL.
NODE, once unwrapped (see %UNWRAP), must read as (QUOTE symbol): written 'name or
(quote name).  Anything else -- a quoted list, a bare symbol, a call -- is NIL."
  (let* ((node (%unwrap node))
         (value (cst-node-value node)))
    (when (and (eq (cst-node-kind node) :expr)
               (consp value)
               (symbolp (car value))
               (string= (symbol-name (car value)) "QUOTE")
               (consp (cdr value))
               (null (cddr value))
               (symbolp (cadr value)))
      ;; 'name has the quoted symbol as its only child; (quote name) has the
      ;; QUOTE token first.  Either way the quoted symbol is the last child.
      (let ((quoted (car (last (%expr-children node)))))
        (and quoted
             (eq (cst-node-value quoted) (cadr value))
             quoted)))))

(defun %line-context (text start)
  "Return the source line containing START, trimmed and cut to *CONTEXT-WIDTH*."
  (let* ((newline (position #\Newline text :end start :from-end t))
         (line-start (if newline (1+ newline) 0))
         (line-end (or (position #\Newline text :start start) (length text)))
         (line (string-trim '(#\Space #\Tab #\Return)
                            (subseq text line-start line-end))))
    (if (> (length line) *context-width*)
        (subseq line 0 *context-width*)
        line)))

(defun %column (text start)
  "Return the 1-based column of character offset START in TEXT."
  (- start (or (position #\Newline text :end start :from-end t) -1)))

(defun %first-line (text)
  "Return TEXT's first line, for a one-line error summary."
  (subseq text 0 (or (position #\Newline text) (length text))))

(defun %collect-sites (top text target-name)
  "Return plists (:node :kind :token :shadowed-by) for TARGET-NAME inside the
top-level node TOP, in source order.

Classification is positional, not a code walker.  The head of a list in an
evaluated position is \"call\" and any other evaluated position \"reference\";
QUOTE data is \"quoted\"; #'name is \"function\", and so is 'name written as the
function argument of FUNCALL, APPLY or MULTIPLE-VALUE-CALL (see
*FUNCTION-DESIGNATOR-OPERATORS*); lambda lists and LET bindings
are \"bind\"; SETF and SETQ places, and the bare-symbol place of INCF, DECF, POP,
PUSH and PUSHNEW (see *PLACE-MODIFYING-OPERATORS*), are \"set\"; a DEFMETHOD's name is
\"method\"; and anything inside a backquote template, outside its unquotes, is
\"template\".  The name position of a DEF... form is the definition itself and
is skipped only when it looks like a name -- a bare symbol, a (SETF sym) list,
or (for DEFSTRUCT) a (name . options) list; anything else in that position is
walked as ordinary code, so a user macro merely named DEF... (or a CASE clause
whose key happens to be a DEF...-prefixed symbol) does not lose real call
sites.  DEFPACKAGE forms, keywords and #:symbols are excluded outright.  Below
a FLET, LABELS or MACROLET that binds the name every site carries that operator
in :SHADOWED-BY, however far down it is.  A #+feature/#-feature reader
conditional around any of the above is transparent: it is unwrapped (see
%UNWRAP) before classification, so its own start/end/children never leak into
a site's position, token or structure."
  (let ((sites '()))
    (labels
        ((emit (node kind template shadowed-by)
           (let* ((node (%unwrap node))
                  (value (cst-node-value node)))
             (when (and (eq (cst-node-kind node) :expr)
                        (symbolp value)
                        (not (keywordp value))
                        (string= (symbol-name value) target-name))
               (let ((token (subseq text (cst-node-start node) (cst-node-end node))))
                 (unless (or (zerop (length token))
                             (char= (char token 0) #\:)
                             (and (> (length token) 1) (string= "#:" (subseq token 0 2))))
                   (push (list :node node
                               :kind (if template "template" kind)
                               :token token
                               :shadowed-by shadowed-by)
                         sites))))))
         (walk (node template shadowed-by)
           (let ((node (%unwrap node)))
             (when (eq (cst-node-kind node) :expr)
               (if (consp (cst-node-value node))
                   (walk-list node template shadowed-by)
                   (emit node "reference" template shadowed-by)))))
         (walk-quoted (node shadowed-by)
           (let ((node (%unwrap node)))
             (when (eq (cst-node-kind node) :expr)
               (if (consp (cst-node-value node))
                   (dolist (child (%expr-children node))
                     (walk-quoted child shadowed-by))
                   (emit node "quoted" nil shadowed-by)))))
         (walk-lambda-list (node template shadowed-by)
           ;; A variable is a binding.  A (var init) or ((key var) init) entry
           ;; also holds evaluated forms, and a DEFMETHOD's (var class) holds a
           ;; class name, which then reads as a reference.  Each entry (and a
           ;; key/var pair within it) may itself be a #+feature-wrapped form,
           ;; so it is unwrapped before its own children are read.
           (let ((node (%unwrap node)))
             (when (eq (cst-node-kind node) :expr)
               (if (consp (cst-node-value node))
                   (dolist (raw-entry (%expr-children node))
                     (let ((entry (%unwrap raw-entry)))
                       (if (consp (cst-node-value entry))
                           (let ((parts (%expr-children entry)))
                             (when parts
                               (let ((first-part (%unwrap (first parts))))
                                 (if (consp (cst-node-value first-part))
                                     (dolist (key-or-var (%expr-children first-part))
                                       (emit key-or-var "bind" template shadowed-by))
                                     (emit first-part "bind" template shadowed-by)))
                               (dolist (part (rest parts))
                                 (walk part template shadowed-by))))
                           (emit entry "bind" template shadowed-by))))
                   (emit node "bind" template shadowed-by)))))
         (walk-bindings (node shadowed-by)
           (let ((node (%unwrap node)))
             (when (and (eq (cst-node-kind node) :expr) (consp (cst-node-value node)))
               (dolist (raw-binding (%expr-children node))
                 (let ((binding (%unwrap raw-binding)))
                   (if (consp (cst-node-value binding))
                       (let ((parts (%expr-children binding)))
                         (when parts
                           (emit (first parts) "bind" nil shadowed-by)
                           (dolist (part (rest parts))
                             (walk part nil shadowed-by))))
                       (emit binding "bind" nil shadowed-by)))))))
         (binds-target-p (bindings)
           (let ((bindings (and bindings (%unwrap bindings))))
             (and bindings
                  (some (lambda (raw-binding)
                          (let* ((binding (%unwrap raw-binding))
                                 (parts (%expr-children binding))
                                 (name (and parts (cst-node-value (first parts)))))
                            (and name
                                 (symbolp name)
                                 (string= (symbol-name name) target-name))))
                        (%expr-children bindings)))))
         (walk-list (node template shadowed-by)
           ;; NODE arrives already unwrapped: WALK, the only caller, unwraps
           ;; before dispatching here, so a #+feature/#-feature wrapper around
           ;; the whole form never reaches this point.
           (let* ((value (cst-node-value node))
                  (children (%expr-children node))
                  (head (car value))
                  (explicit (and children
                                 (symbolp head)
                                 (eq (cst-node-value (first children)) head)))
                  (head-node (and explicit (first children)))
                  (args (if explicit (rest children) children))
                  (name (and (symbolp head) (symbol-name head))))
             (flet ((emit-head (kind)
                      (when head-node
                        (emit head-node kind template shadowed-by)))
                    (walk-args (&optional (from args) (in-template template)
                                 (shadow shadowed-by))
                      (dolist (arg from)
                        (walk arg in-template shadow))))
               (cond
                 ((null name)
                  (dolist (child children)
                    (walk child template shadowed-by)))
                 ((%eclector-marker-p head "QUASIQUOTE")
                  (walk-args args t))
                 ((or (%eclector-marker-p head "UNQUOTE")
                      (%eclector-marker-p head "UNQUOTE-SPLICING"))
                  (walk-args args nil))
                 (template
                  (emit-head "template")
                  (walk-args))
                 ((string= name "QUOTE")
                  (emit-head "call")
                  (dolist (arg args)
                    (walk-quoted arg shadowed-by)))
                 ((string= name "FUNCTION")
                  (emit-head "call")
                  (dolist (arg args)
                    (if (consp (cst-node-value arg))
                        (walk arg nil shadowed-by)
                        (emit arg "function" nil shadowed-by))))
                 ((member name *function-designator-operators* :test #'string=)
                  ;; (funcall 'name ...) designates NAME as #'name does; the
                  ;; remaining arguments are ordinary code.
                  (emit-head "call")
                  (when args
                    (let ((designator (%quoted-symbol-node (first args))))
                      (if designator
                          (emit designator "function" nil shadowed-by)
                          (walk (first args) nil shadowed-by))))
                  (walk-args (rest args)))
                 ((member name '("DEFPACKAGE" "DEFINE-PACKAGE") :test #'string=)
                  nil)
                 ((string= name "DEFMETHOD")
                  (emit-head "call")
                  (when args
                    (emit (first args) "method" nil shadowed-by)
                    (let ((lambda-list-seen nil))
                      (dolist (arg (rest args))
                        (cond
                          (lambda-list-seen
                           (walk arg nil shadowed-by))
                          ((listp (cst-node-value arg))
                           (setf lambda-list-seen t)
                           (walk-lambda-list arg nil shadowed-by)))))))
                 ((assoc name *lambda-list-positions* :test #'string=)
                  (emit-head "call")
                  (let ((position (cdr (assoc name *lambda-list-positions*
                                              :test #'string=))))
                    (loop for arg in args
                          for i from 0
                          do (cond
                               ((and (= i 0) (= position 1)) nil)
                               ((= i position) (walk-lambda-list arg nil shadowed-by))
                               (t (walk arg nil shadowed-by))))))
                 ((member name '("LET" "LET*") :test #'string=)
                  (emit-head "call")
                  (when args
                    (walk-bindings (first args) shadowed-by)
                    (walk-args (rest args))))
                 ((member name '("SETF" "SETQ" "PSETF" "PSETQ") :test #'string=)
                  (emit-head "call")
                  (loop for arg in args
                        for i from 0
                        do (if (and (evenp i) (not (consp (cst-node-value arg))))
                               (emit arg "set" nil shadowed-by)
                               (walk arg nil shadowed-by))))
                 ((assoc name *place-modifying-operators* :test #'string=)
                  (emit-head "call")
                  (let ((position (cdr (assoc name *place-modifying-operators*
                                              :test #'string=))))
                    (loop for arg in args
                          for i from 0
                          do (if (and (= i position) (not (consp (cst-node-value arg))))
                                 (emit arg "set" nil shadowed-by)
                                 (walk arg nil shadowed-by)))))
                 ((member name *shadowing-operators* :test #'string=)
                  (emit-head "call")
                  (let* ((bindings (and args (%unwrap (first args))))
                         (inner (if (binds-target-p bindings)
                                    (string-downcase name)
                                    shadowed-by)))
                    (when bindings
                      (dolist (raw-binding (%expr-children bindings))
                        (let* ((binding (%unwrap raw-binding))
                               (parts (%expr-children binding)))
                          (when (second parts)
                            (walk-lambda-list (second parts) nil inner))
                          (dolist (part (cddr parts))
                            (walk part nil inner)))))
                    (walk-args (rest args) nil inner)))
                 ((member name '("DEFCLASS" "DEFINE-CONDITION") :test #'string=)
                  (emit-head "call")
                  (when (second args)
                    (dolist (super (%expr-children (%unwrap (second args))))
                      (emit super "reference" nil shadowed-by)))
                  (walk-args (cddr args)))
                 ((%definer-name-p name)
                  ;; Argument 0 is normally the name being defined, but only
                  ;; skip it when it looks like one: some DEF...-prefixed
                  ;; heads are not definers at all (a CASE clause whose key
                  ;; reads as DEFAULT, a function like DEFAULT-VALUE or
                  ;; DEFLATE), and their first argument is ordinary code.
                  (emit-head "call")
                  (cond
                    ((null args) nil)
                    ((member name *definers-with-name-and-options* :test #'string=)
                     (walk-args (rest args)))
                    ((%definer-name-position-p (first args))
                     (walk-args (rest args)))
                    (t
                     (walk-args args))))
                 (t
                  (emit-head "call")
                  (walk-args)))))))
      (walk top nil nil))
    (nreverse sites)))

(defun %form-metadata (value in-package)
  "Return (values FORM-TYPE FORM-NAME TEST-NAME TEST-FRAMEWORK) for VALUE, a
top-level form read where IN-PACKAGE (a designator or NIL) was current.

FORM-NAME is what lisp-edit-form's form_name matches, a method's specializers
included, and is given only for DEF... and test forms.  FORM-TYPE is the head's
lower-case name whenever the head is a symbol."
  (let ((head (and (consp value) (symbolp (car value)) (car value))))
    (if (null head)
        (values nil nil nil nil)
        (let* ((name (symbol-name head))
               (form-type (string-downcase name))
               (framework (cdr (assoc name *test-frameworks* :test #'string=)))
               (form-name
                 (and (or framework (%definer-name-p name))
                      (consp (cdr value))
                      ;; Print as the parser read it, so the name's symbols
                      ;; come out without package prefixes.
                      (let ((*package* (or (find-package-named in-package)
                                           (find-package "COMMON-LISP-USER"))))
                        (car (last (ignore-errors
                                    (%definition-candidates value form-type))))))))
          (values form-type form-name (and framework form-name) framework)))))

(defun %site->ht (site text)
  "Return the JSON object for SITE, a %COLLECT-SITES plist, in TEXT."
  (let ((node (getf site :node)))
    (make-ht "line" (cst-node-start-line node)
             "column" (%column text (cst-node-start node))
             "kind" (getf site :kind)
             "token" (getf site :token)
             "context" (%line-context text (cst-node-start node))
             "shadowed_by" (getf site :shadowed-by))))

(defun scan-text (text target-name &key path abs-path (max-sites *max-scan-sites*))
  "Scan TEXT, one file's contents, for sites of TARGET-NAME.

TARGET-NAME is a symbol name as read (see TARGET-NAME-FROM-DESIGNATOR).  PATH
is the display path and ABS-PATH the truename namestring the worker matches
xref entries on; ABS-PATH also gives the parser the file's package context.

Returns (values FORMS SITE-COUNT TRUNCATED-P READTABLE-SWITCH-REASON).  FORMS
lists one JSON-ready hash-table per top-level form holding a site, with path,
abs_path, index, start_line, end_line, form_type, form_name, test_name,
test_framework, in_package, context and sites.  INDEX counts the file's
top-level expressions from 0, the number SBCL's DEFINITION-SOURCE-FORM-PATH
starts with.  Each site has line, column, kind, token, context and
shadowed_by.  Collection stops after MAX-SITES sites.  Signals the parser's
error when TEXT does not parse.

READTABLE-SWITCH-REASON is a one-line string when TEXT contains an
IN-READTABLE form that switches the parser off Eclector partway through
(this only happens when named-readtables is loaded in the parent): the forms
and sites found before the switch are still returned in FORMS, but nothing
from the switch onward is scanned, because the fallback CL reader's nodes
carry no children to walk.  NIL when there was no such switch."
  (multiple-value-bind (nodes read-error)
      (if abs-path
          (parse-top-level-forms text :source-path (pathname abs-path))
          (let ((*package* (find-package "COMMON-LISP-USER")))
            (parse-top-level-forms text)))
    (let ((forms '())
          (count 0)
          (truncated nil)
          (index -1)
          (in-package nil)
          (switch-reason nil))
      (dolist (node nodes)
        (when (eq (cst-node-kind node) :expr)
          (if (and (consp (cst-node-value node)) (null (cst-node-children node)))
              ;; A node with a cons value but no children was read by the CL
              ;; reader after an in-file IN-READTABLE switch (see
              ;; %READ-REMAINING-WITH-CL-READER): there is no structure left to
              ;; classify sites in, so scanning stops here, keeping whatever
              ;; was already collected.
              (progn
                (setf switch-reason
                      (format nil "switches to a custom readtable at line ~D; ~
later forms were not scanned"
                              (cst-node-start-line node)))
                (return))
              (progn
                (incf index)
                (let ((sites (and (not truncated)
                                  (%collect-sites node text target-name))))
                  (when (> (+ count (length sites)) max-sites)
                    (setf sites (subseq sites 0 (- max-sites count))
                          truncated t))
                  (when sites
                    (incf count (length sites))
                    (multiple-value-bind (form-type form-name test-name framework)
                        (%form-metadata (cst-node-value node) in-package)
                      (push (make-ht "path" path
                                     "abs_path" abs-path
                                     "index" index
                                     "start_line" (cst-node-start-line node)
                                     "end_line" (cst-node-end-line node)
                                     "form_type" form-type
                                     "form_name" form-name
                                     "test_name" test-name
                                     "test_framework" framework
                                     "in_package" in-package
                                     "context" (%line-context text (cst-node-start node))
                                     "sites" (map 'vector
                                                  (lambda (site) (%site->ht site text))
                                                  sites))
                            forms))))
                (let ((designator (%in-package-form-p (cst-node-value node))))
                  (when designator
                    (setf in-package designator)))))))
      (when (and (not switch-reason) read-error)
        (setf switch-reason
              (format nil "switches to a custom readtable; later forms were not scanned (~A)"
                      (%first-line (princ-to-string read-error)))))
      (values (nreverse forms) count truncated switch-reason))))

(defun %readable-path (path)
  "Return PATH's resolved pathname when the read policy allows reading it, else NIL.

The policy is FS-READ-FILE's: CL-MCP/SRC/UTILS/PATHS:ALLOWED-READ-PATH, which
resolves symlinks and accepts only paths inside *PROJECT-ROOT* or the source
directory of a registered ASDF system.  NIL also when *PROJECT-ROOT* is unset
or the check itself signals, so a path the policy cannot vouch for is never
read."
  (and *project-root*
       (handler-case (allowed-read-path path)
         (error () nil))))

(defun scan-project (designator &key (root *project-root*) (max-sites *max-scan-sites*))
  "Scan every Lisp file under ROOT for sites of the symbol DESIGNATOR spells.

Validates DESIGNATOR first (see TARGET-NAME-FROM-DESIGNATOR).  The files are
those clgrep-search reads (.lisp, .asd and .ros, honouring .gitignore); only
those whose text contains the name, ignoring case, are parsed.

Nothing is read that FS-READ-FILE would refuse (see %READABLE-PATH): a ROOT
outside the project root and every registered ASDF system's source directory
is skipped, as is any scan while *PROJECT-ROOT* is unset, and a file under ROOT
the policy denies -- one reached through a symlink leaving the allowed tree,
say -- is not read, not counted in FILES_SCANNED and not listed in
SCANNED_FILES, only counted in FILES_DENIED.

An allowed file is read whole by CL-MCP/SRC/FS:FS-READ-SOURCE-TEXT, as UTF-8
with invalid bytes replaced by #\\?, so one bad byte (in a comment, say) does
not drop the whole file; a file that still cannot be read (missing,
unreadable, ...) is reported in parse_failures instead of being silently
skipped.  Every file's abs_path -- in a form or a parse_failures
entry -- is its truename namestring (falling back to its plain namestring when
TRUENAME fails), so a file reached through a symlinked directory is keyed the
same way SCANNED_FILES and CL-MCP/SRC/CODE-CORE's xref matching are.  The
search the parser makes for the definition of a package the parent lacks is
done once per package for the whole scan (see
CL-MCP/SRC/PACKAGE-CONTEXT:*PACKAGE-SPEC-DISCOVERY-CACHE*).  Returns a
JSON-ready hash-table:
  target_name     the name matched
  root            ROOT's truename namestring, or null
  files_scanned   files the read policy allowed, whether read or not
  files_matched   files whose text contains the name
  files_denied    files under ROOT the read policy denied, which were not read
  forms           SCAN-TEXT's forms for every file, concatenated
  parse_failures  path, abs_path and error of each file that did not parse, or
                  that could not be read, or whose scan stopped partway at an
                  IN-READTABLE switch (see SCAN-TEXT) -- that file's forms up
                  to the switch are still in FORMS
  scanned_files   the truename namestring of every file FILES_SCANNED counted,
                  matched or not -- what CL-MCP/SRC/CODE-CORE:%SCAN-STATUS
                  checks an xref entry's file against, so a file under ROOT
                  this scan never considered (gitignored, not .lisp/.asd/.ros,
                  or denied) is never claimed as scanned
  truncated_at    MAX-SITES when collection stopped there, else null
  skipped_reason  why nothing was scanned, else null"
  (let ((name (target-name-from-designator designator))
        (root-truename (and root (ignore-errors (namestring (truename root)))))
        (forms '())
        (failures '())
        (scanned-files '())
        (scanned 0)
        (matched 0)
        (denied 0)
        (count 0)
        (truncated nil))
    (flet ((report (&optional skipped)
             (make-ht "target_name" name
                      "root" root-truename
                      "files_scanned" scanned
                      "files_matched" matched
                      "files_denied" denied
                      "forms" (coerce forms 'vector)
                      "parse_failures" (coerce (reverse failures) 'vector)
                      "scanned_files" (coerce (nreverse scanned-files) 'vector)
                      "truncated_at" (and truncated max-sites)
                      "skipped_reason" skipped))
           (fail (file abs-path reason)
             (push (make-ht "path" (normalize-path-for-display file)
                            "abs_path" abs-path
                            "error" reason)
                   failures)))
      (let ((skipped (cond
                       ((or (null root) (null *project-root*))
                        "project root is not set")
                       ((null root-truename)
                        "project root is not readable")
                       ((null (%readable-path root-truename))
                        (concatenate 'string
                                     "root is outside the readable paths "
                                     "(project root and registered ASDF system sources)")))))
        (when skipped
          (return-from scan-project (report skipped))))
      ;; Each matched file whose package the parent lacks sends the parser
      ;; looking for that package's definition across the project; one table
      ;; for the whole loop makes that one walk per package, not per file.
      (let ((*package-spec-discovery-cache* (make-hash-table :test #'equal)))
        (dolist (file (collect-target-files root-truename))
          (let ((readable (%readable-path file)))
            (if (null readable)
                (incf denied)
                (let ((abs-path (or (ignore-errors (namestring (truename file)))
                                    (namestring file))))
                  (incf scanned)
                  (push abs-path scanned-files)
                  (unless truncated
                    ;; FS-READ-SOURCE-TEXT, not FS-READ-FILE: the latter caps a
                    ;; read at *FS-READ-MAX-BYTES*, silently cutting a large
                    ;; file short, and decodes without replacement, so one
                    ;; invalid byte would drop the file.  The wrapper applies
                    ;; the same read policy as %READABLE-PATH above again
                    ;; before it opens the file; that check is what feeds
                    ;; files_denied.  Anything it signals is a parse failure.
                    (multiple-value-bind (text read-condition)
                        (ignore-errors (fs-read-source-text readable))
                      (cond
                        ((null text)
                         (fail file abs-path (%first-line (princ-to-string read-condition))))
                        ((search name text :test #'char-equal)
                         (incf matched)
                         (handler-case
                             (multiple-value-bind (file-forms file-count file-truncated
                                                   file-reason)
                                 (scan-text text name
                                            :path (normalize-path-for-display file)
                                            :abs-path abs-path
                                            :max-sites (- max-sites count))
                               (setf forms (append forms file-forms))
                               (incf count file-count)
                               (when file-truncated
                                 (setf truncated t))
                               (when file-reason
                                 (fail file abs-path file-reason)))
                           (error (e)
                             (fail file abs-path
                                   (%first-line (princ-to-string e))))))))))))))
      (report))))

(defparameter *method-lambda-list-keywords*
  '("&OPTIONAL" "&REST" "&KEY" "&AUX" "&ALLOW-OTHER-KEYS")
  "Names of the lambda-list keywords that end a method's required parameters.")

(defun %decimal-text (integer)
  "Return INTEGER printed in base 10 without a radix marker, matching
CLOS-CORE's %DECIMAL-STRING so the worker compares an EQL datum's integer or
ratio parts (spec 3.3) as plain decimal text, never a float."
  (let ((*print-base* 10) (*print-radix* nil))
    (princ-to-string integer)))

(defun %source-eql-unverifiable-reason (value)
  "Return one sentence explaining why VALUE, an (EQL ...) specializer's
unevaluated source form, is not on the tagged allow-list of spec 3.3 (a
literal integer, ratio, character, keyword, boolean, or a quoted interned
symbol)."
  (cond
    ((stringp value) "a string is not EQL-comparable by content")
    ((floatp value) "a float compares unsoundly by printed value")
    ((typep value 'complex) "a complex number compares unsoundly by printed value")
    ((arrayp value) "an array is not EQL-comparable by content")
    ((consp value) "an arbitrary form is not evaluated for source matching")
    ((and (symbolp value) (null (symbol-package value)))
     "an uninterned symbol has no package to resolve it by")
    ((symbolp value) "a variable reference is not a literal datum")
    (t (format nil "a value of type ~(~A~) is not on the EQL datum allow-list" (type-of value)))))

(defun %source-eql-datum (node text in-package)
  "Return NODE, an (EQL datum)'s unevaluated source form, tagged per spec
3.3.  KEYWORD, INTEGER, RATIO, CHARACTER and BOOLEAN come straight from the
read value: always safe, since none of those types depend on a package the
parent might lack.  SYMBOL instead carries (:TOKEN :IN-PACKAGE :QUOTED) for
the worker to resolve, since an arbitrary quoted symbol's home package may
not exist in the parent's image.  Anything else is UNVERIFIABLE."
  (let* ((node (%unwrap node))
         (value (cst-node-value node)))
    (cond
      ((keywordp value) (list :kind :keyword :name (symbol-name value)))
      ((integerp value) (list :kind :integer :value (%decimal-text value)))
      ((typep value 'ratio)
       (list :kind :ratio :numerator (%decimal-text (numerator value))
             :denominator (%decimal-text (denominator value))))
      ((characterp value) (list :kind :character :value (string value)))
      ((eq value t) (list :kind :boolean :value "T"))
      ((null value) (list :kind :boolean :value "NIL"))
      (t
       (let ((quoted (%quoted-symbol-node node)))
         (if quoted
             (list :kind :symbol
                   :token (subseq text (cst-node-start quoted) (cst-node-end quoted))
                   :in-package in-package :quoted t)
             (list :kind :unverifiable :reason (%source-eql-unverifiable-reason value))))))))

(defun %source-specializer (param text in-package)
  "Return PARAM, one required parameter of a method's lambda list, as its
specializer identity: {:KIND :CLASS :TOKEN .. :IN-PACKAGE ..} for a class
name or an unspecialized parameter -- synthesized as the literal token \"T\",
safe because COMMON-LISP:T is inherited into every package -- {:KIND :EQL
:DATUM ..} for (EQL datum), or {:KIND :UNVERIFIABLE :REASON ..} for any
other shape this cannot describe."
  (let* ((param (%unwrap param))
         (value (cst-node-value param)))
    (cond
      ((symbolp value) (list :kind :class :token "T" :in-package in-package))
      ((and (consp value) (consp (rest value)) (null (cddr value)))
       (let* ((children (%expr-children param))
              (specializer (%unwrap (second children)))
              (specializer-value (cst-node-value specializer)))
         (cond
           ((symbolp specializer-value)
            (list :kind :class
                  :token (subseq text (cst-node-start specializer) (cst-node-end specializer))
                  :in-package in-package))
           ((and (consp specializer-value) (symbolp (first specializer-value))
                 (string= "EQL" (symbol-name (first specializer-value)))
                 (consp (rest specializer-value)) (null (cddr specializer-value)))
            (list :kind :eql
                  :datum (%source-eql-datum (second (%expr-children specializer)) text
                                            in-package)))
           (t (list :kind :unverifiable
                    :reason "specializer is neither a class name nor an EQL form")))))
      (t (list :kind :unverifiable
               :reason "parameter is not a name or a (name specializer) list")))))

(defun %source-specializers (lambda-list text in-package)
  "Return one %SOURCE-SPECIALIZER plist per required parameter of LAMBDA-LIST,
a method's specialized lambda list node: every child up to the first lambda
list keyword (&OPTIONAL, &REST, ...), which ends the required parameters."
  (loop for param in (%expr-children lambda-list)
        until (and (symbolp (cst-node-value param))
                  (member (symbol-name (cst-node-value param))
                          *method-lambda-list-keywords* :test #'string=))
        collect (%source-specializer param text in-package)))

(defun %split-qualifiers (nodes)
  "Return (values QUALIFIER-NODES LAMBDA-LIST-NODE) splitting NODES, the
children after a DEFMETHOD's or (:METHOD ...)'s name, at the first whose
value is a list (the lambda list; an empty lambda list () counts, since NIL
is a list).  (values NIL NIL) when NODES never reaches one."
  (let ((qualifiers '()))
    (loop for tail on nodes
          do (if (listp (cst-node-value (car tail)))
                 (return (values (nreverse qualifiers) (car tail)))
                 (push (car tail) qualifiers))
          finally (return (values (nreverse qualifiers) nil)))))

(defun %source-token (node text in-package)
  "Return NODE's source text and IN-PACKAGE as (:TOKEN .. :IN-PACKAGE ..):
the literal characters at NODE's span, never a resolved or normalized name."
  (let ((node (%unwrap node)))
    (list :token (subseq text (cst-node-start node) (cst-node-end node))
          :in-package in-package)))

(defun %source-name (name-node text in-package struct-p)
  "Return NAME-NODE's identity as (:TOKEN .. :SETF .. :IN-PACKAGE ..), or NIL
when NAME-NODE is absent or not a name shape this recognizes: a bare symbol,
a (SETF symbol) list (STRUCT-P NIL), or -- for DEFSTRUCT (STRUCT-P T), whose
name may carry options -- a (name . options) list, taking its first element."
  (when name-node
    (let* ((node (%unwrap name-node))
           (value (cst-node-value node)))
      (cond
        ((symbolp value)
         (list :token (subseq text (cst-node-start node) (cst-node-end node))
               :setf nil :in-package in-package))
        ((and (not struct-p) (consp value) (symbolp (first value))
              (string= "SETF" (symbol-name (first value)))
              (consp (rest value)) (symbolp (second value)) (null (cddr value)))
         (let ((inner (second (%expr-children node))))
           (list :token (subseq text (cst-node-start inner) (cst-node-end inner))
                 :setf t :in-package in-package)))
        ((and struct-p (consp value) (symbolp (first value)))
         (let ((inner (first (%expr-children node))))
           (list :token (subseq text (cst-node-start inner) (cst-node-end inner))
                 :setf nil :in-package in-package)))
        (t nil)))))

(defun %source-method-option (option-node text in-package)
  "Return OPTION-NODE's (:QUALIFIERS .. :SPECIALIZERS ..) when it is a
DEFGENERIC (:METHOD ...) method-description, else NIL for an ordinary
DEFGENERIC option such as (:documentation ...)."
  (let ((value (cst-node-value option-node)))
    (when (and (consp value) (keywordp (first value))
              (string= "METHOD" (symbol-name (first value))))
      (multiple-value-bind (qualifiers lambda-list)
          (%split-qualifiers (rest (%expr-children option-node)))
        (list :qualifiers (mapcar (lambda (q) (%source-token q text in-package)) qualifiers)
              :specializers (and lambda-list
                                 (%source-specializers lambda-list text in-package)))))))

(defun %source-slot (slot-node text in-package)
  "Return SLOT-NODE, a DEFCLASS or DEFINE-CONDITION slot specifier, as
(:NAME name-token :READERS (token...) :WRITERS (token...)).  A bare slot
name has no readers or writers.  :ACCESSOR NAME contributes NAME to both
lists, since it defines both a reader and a (SETF NAME) writer."
  (let* ((node (%unwrap slot-node))
         (value (cst-node-value node)))
    (if (symbolp value)
        (list :name (%source-token node text in-package) :readers nil :writers nil)
        (let* ((children (%expr-children node))
               (name-node (first children))
               (readers '())
               (writers '()))
          (loop for (key-node value-node) on (rest children) by #'cddr
                while (and key-node value-node)
                do (let ((key (cst-node-value key-node)))
                     (when (keywordp key)
                       (let ((token (%source-token value-node text in-package)))
                         (cond
                           ((string= (symbol-name key) "READER") (push token readers))
                           ((string= (symbol-name key) "WRITER") (push token writers))
                           ((string= (symbol-name key) "ACCESSOR")
                            (push token readers) (push token writers)))))))
          (list :name (%source-token name-node text in-package)
                :readers (nreverse readers) :writers (nreverse writers))))))

(defun %source-slots (slots-node text in-package)
  "Return one %SOURCE-SLOT plist per slot specifier in SLOTS-NODE, a
DEFCLASS or DEFINE-CONDITION's slot list; NIL when SLOTS-NODE is absent or
not a list."
  (and slots-node (listp (cst-node-value slots-node))
       (mapcar (lambda (slot) (%source-slot slot text in-package))
               (%expr-children slots-node))))

(defparameter *source-signature-kinds*
  '(("DEFMETHOD" . :defmethod) ("DEFGENERIC" . :defgeneric) ("DEFCLASS" . :defclass)
    ("DEFINE-CONDITION" . :define-condition) ("DEFSTRUCT" . :defstruct))
  "Head names %DEFINITION-SOURCE-SIGNATURE builds kind-specific fields for;
every other head becomes :OTHER.")

(defun %source-signature-kind (head-name)
  "Return HEAD-NAME's source_signature :KIND (spec 3.2): one of
*SOURCE-SIGNATURE-KINDS*, matched by name only, or :OTHER.  A shadowing
operator of the same name is not ruled out here; only the worker, which can
resolve HEAD-NAME, may confirm it (spec 3.4)."
  (or (cdr (assoc head-name *source-signature-kinds* :test #'string=)) :other))

(defun %source-signature-body (kind children text in-package)
  "Return the KIND-specific fields of %DEFINITION-SOURCE-SIGNATURE's plist,
CHILDREN being the node's expression children including the head."
  (case kind
    (:defmethod
      (multiple-value-bind (qualifiers lambda-list) (%split-qualifiers (nthcdr 2 children))
        (list :name (%source-name (nth 1 children) text in-package nil)
              :qualifiers (mapcar (lambda (q) (%source-token q text in-package)) qualifiers)
              :specializers (and lambda-list (%source-specializers lambda-list text in-package)))))
    (:defgeneric
      (list :name (%source-name (nth 1 children) text in-package nil)
            :methods (loop for option in (nthcdr 2 children)
                          for parsed = (%source-method-option option text in-package)
                          when parsed collect parsed)))
    ((:defclass :define-condition)
      (list :name (%source-name (nth 1 children) text in-package nil)
            :slots (%source-slots (nth 3 children) text in-package)))
    (:defstruct
      (list :name (%source-name (nth 1 children) text in-package t)))))

(defun %definition-source-signature (node text in-package)
  "Return NODE's source_signature (spec 3.2): a plist (:KIND kind :HEAD head
...), with kind-specific fields from %SOURCE-SIGNATURE-BODY.  Every :TOKEN in
it is the literal source text at that node's span -- (subseq text start end)
-- never a resolved or normalized name; :IN-PACKAGE is IN-PACKAGE, the
package in effect for every token in NODE (an in-package form cannot occur
inside another top-level form, so one value covers the whole of NODE).  KIND
is :DEFMETHOD, :DEFGENERIC, :DEFCLASS, :DEFINE-CONDITION or :DEFSTRUCT when
NODE's head is spelled that way (%SOURCE-SIGNATURE-KIND, by name only -- a
shadowing operator is the worker's problem, spec 3.4), and :OTHER for
anything else, including a malformed definer too broken for the rest of this
to describe."
  (let* ((value (cst-node-value node))
         (children (%expr-children node))
         (head (first children)))
    (if (or (not (consp value)) (null head) (not (symbolp (cst-node-value head))))
        (list :kind :other)
        (let ((kind (%source-signature-kind (symbol-name (cst-node-value head)))))
          (if (eq kind :other)
              (list :kind :other :head (%source-token head text in-package))
              (list* :kind kind :head (%source-token head text in-package)
                     (%source-signature-body kind children text in-package)))))))

(defun top-level-forms-at (abs-path lines &key text)
  "Describe every top-level form of the file at ABS-PATH that starts on LINES.

Returns (values TABLE FAILURE).  TABLE maps each line in LINES on which one
or more top-level forms start to the list of those forms -- more than one
when two top-level forms begin on the same line -- each a plist (:FORM-TYPE
:FORM-NAME :SIGNATURE :START :END).  FORM-TYPE and FORM-NAME are what
%FORM-METADATA gives code-find-references, with the package from the file's
IN-PACKAGE forms.  SIGNATURE is %DEFINITION-SOURCE-SIGNATURE's token-based
source_signature (spec 3.2) for a DEFMETHOD, DEFGENERIC, DEFCLASS,
DEFINE-CONDITION or DEFSTRUCT, and (:KIND :OTHER) for any other form.  START
and END are character offsets into the file's text, END exclusive, spanning
that one form -- (subseq text start end) reproduces it.  A line no form
starts on is absent from TABLE.  A form wrapped in #+feature or #-feature is
found both on its own line and on the line of the form it wraps (%UNWRAP),
the same entry under both keys.

TEXT, when given, is used instead of reading ABS-PATH from disk (for a
caller that already holds the file's snapshot text); the read policy is not
consulted in that case, since no file is opened.

FAILURE is NIL when the file was read and parsed.  It is :DENIED when the
read policy refuses ABS-PATH (%READABLE-PATH) and TEXT was not given -- the
file is then not opened -- and a one-line string when the file cannot be
read or does not parse; TABLE is empty in both cases."
  (let ((table (make-hash-table))
        (wanted (remove-duplicates (remove-if-not #'integerp lines))))
    (labels ((add (line entry)
               (push entry (gethash line table)))
             (scan (file-text)
               (handler-case
                   (let ((in-package nil)
                         (nodes (parse-top-level-forms file-text :source-path (pathname abs-path))))
                     (dolist (node nodes)
                       (when (eq (cst-node-kind node) :expr)
                         (let* ((value (cst-node-value node))
                                (unwrapped (%unwrap node))
                                (line1 (cst-node-start-line node))
                                (line2 (cst-node-start-line unwrapped))
                                (wanted1 (member line1 wanted))
                                (wanted2 (member line2 wanted)))
                           (when (or wanted1 wanted2)
                             (multiple-value-bind (form-type form-name)
                                 (%form-metadata value in-package)
                               (let ((entry (list :form-type form-type :form-name form-name
                                                  :signature (%definition-source-signature
                                                              unwrapped file-text in-package)
                                                  :start (cst-node-start unwrapped)
                                                  :end (cst-node-end unwrapped))))
                                 (when wanted1 (add line1 entry))
                                 (when (and wanted2 (/= line1 line2)) (add line2 entry)))))
                           (let ((designator (%in-package-form-p value)))
                             (when designator
                               (setf in-package designator))))))
                     (dolist (line (loop for key being the hash-keys of table collect key))
                       (setf (gethash line table) (nreverse (gethash line table))))
                     (values table nil))
                 (error (e) (values table (%first-line (princ-to-string e)))))))
      (cond
        ((null wanted) (values table nil))
        (text (scan text))
        (t (let ((readable (%readable-path abs-path)))
             (if (null readable)
                 (values table :denied)
                 (multiple-value-bind (file-text read-condition)
                     (ignore-errors (fs-read-source-text readable))
                   (if (null file-text)
                       (values table (%first-line (princ-to-string read-condition)))
                       (scan file-text))))))))))
