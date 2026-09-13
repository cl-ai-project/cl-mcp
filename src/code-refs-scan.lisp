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
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%definition-candidates)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:collect-target-files)
  (:import-from #:cl-mcp/src/utils/paths
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
           #:scan-project))

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

(defun target-name-from-designator (designator)
  "Return the symbol name DESIGNATOR spells, as the reader would read it.
Signals ARG-VALIDATION-ERROR for text that is not a symbol name or is a
keyword, before any file is read."
  (values (parse-target-designator designator)))

(defun %expr-children (node)
  "Return NODE's children that are expressions, skipping comments."
  (remove-if-not (lambda (child) (eq (cst-node-kind child) :expr))
                 (cst-node-children node)))

(defun %definer-name-p (name)
  "True when NAME, a head's symbol name, names a DEF... form."
  (and (> (length name) 3) (string= "DEF" (subseq name 0 3))))

(defun %eclector-marker-p (symbol name)
  "True when SYMBOL is eclector's own backquote marker NAME.
Eclector reads `x as (ECLECTOR.READER:QUASIQUOTE x) and ,x as
(ECLECTOR.READER:UNQUOTE x); comparing the package keeps a user's own UNQUOTE
function from being taken for one."
  (and (symbolp symbol)
       (string= (symbol-name symbol) name)
       (let ((package (symbol-package symbol)))
         (and package (string= (package-name package) "ECLECTOR.READER")))))

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

(defun %collect-sites (top text target-name)
  "Return plists (:node :kind :token :shadowed-by) for TARGET-NAME inside the
top-level node TOP, in source order.

Classification is positional, not a code walker.  The head of a list in an
evaluated position is \"call\" and any other evaluated position \"reference\";
QUOTE data is \"quoted\"; #'name is \"function\"; lambda lists and LET bindings
are \"bind\"; SETF and SETQ places are \"set\"; a DEFMETHOD's name is
\"method\"; and anything inside a backquote template, outside its unquotes, is
\"template\".  The name position of any other DEF... form is the definition
itself and is skipped, as are DEFPACKAGE forms, keywords and #:symbols.  Below
a FLET, LABELS or MACROLET that binds the name every site carries that operator
in :SHADOWED-BY, however far down it is."
  (let ((sites '()))
    (labels
        ((emit (node kind template shadowed-by)
           (let ((value (cst-node-value node)))
             (when (and (eq (cst-node-kind node) :expr)
                        (symbolp value)
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
           (when (eq (cst-node-kind node) :expr)
             (if (consp (cst-node-value node))
                 (walk-list node template shadowed-by)
                 (emit node "reference" template shadowed-by))))
         (walk-quoted (node shadowed-by)
           (when (eq (cst-node-kind node) :expr)
             (if (consp (cst-node-value node))
                 (dolist (child (%expr-children node))
                   (walk-quoted child shadowed-by))
                 (emit node "quoted" nil shadowed-by))))
         (walk-lambda-list (node template shadowed-by)
           ;; A variable is a binding.  A (var init) or ((key var) init) entry
           ;; also holds evaluated forms, and a DEFMETHOD's (var class) holds a
           ;; class name, which then reads as a reference.
           (when (eq (cst-node-kind node) :expr)
             (if (consp (cst-node-value node))
                 (dolist (entry (%expr-children node))
                   (if (consp (cst-node-value entry))
                       (let ((parts (%expr-children entry)))
                         (when parts
                           (if (consp (cst-node-value (first parts)))
                               (dolist (key-or-var (%expr-children (first parts)))
                                 (emit key-or-var "bind" template shadowed-by))
                               (emit (first parts) "bind" template shadowed-by))
                           (dolist (part (rest parts))
                             (walk part template shadowed-by))))
                       (emit entry "bind" template shadowed-by)))
                 (emit node "bind" template shadowed-by))))
         (walk-bindings (node shadowed-by)
           (when (and (eq (cst-node-kind node) :expr) (consp (cst-node-value node)))
             (dolist (binding (%expr-children node))
               (if (consp (cst-node-value binding))
                   (let ((parts (%expr-children binding)))
                     (when parts
                       (emit (first parts) "bind" nil shadowed-by)
                       (dolist (part (rest parts))
                         (walk part nil shadowed-by))))
                   (emit binding "bind" nil shadowed-by)))))
         (binds-target-p (bindings)
           (and bindings
                (some (lambda (binding)
                        (let* ((parts (%expr-children binding))
                               (name (and parts (cst-node-value (first parts)))))
                          (and name
                               (symbolp name)
                               (string= (symbol-name name) target-name))))
                      (%expr-children bindings))))
         (walk-list (node template shadowed-by)
           (let* ((value (cst-node-value node))
                  (children (%expr-children node))
                  (wrapped (find value children :key #'cst-node-value :test #'eq)))
             ;; #+feature form reads as a node whose value is the form and whose
             ;; children are the feature expression and the form itself.
             (when wrapped
               (return-from walk-list (walk wrapped template shadowed-by)))
             (let* ((head (car value))
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
                   ((member name *shadowing-operators* :test #'string=)
                    (emit-head "call")
                    (let* ((bindings (first args))
                           (inner (if (binds-target-p bindings)
                                      (string-downcase name)
                                      shadowed-by)))
                      (when bindings
                        (dolist (binding (%expr-children bindings))
                          (let ((parts (%expr-children binding)))
                            (when (second parts)
                              (walk-lambda-list (second parts) nil inner))
                            (dolist (part (cddr parts))
                              (walk part nil inner)))))
                      (walk-args (rest args) nil inner)))
                   ((member name '("DEFCLASS" "DEFINE-CONDITION") :test #'string=)
                    (emit-head "call")
                    (when (second args)
                      (dolist (super (%expr-children (second args)))
                        (emit super "reference" nil shadowed-by)))
                    (walk-args (cddr args)))
                   ((%definer-name-p name)
                    ;; Any other definer: argument 0 is the name being defined.
                    (emit-head "call")
                    (walk-args (rest args)))
                   (t
                    (emit-head "call")
                    (walk-args))))))))
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

Returns (values FORMS SITE-COUNT TRUNCATED-P).  FORMS lists one JSON-ready
hash-table per top-level form holding a site, with path, abs_path, index,
start_line, end_line, form_type, form_name, test_name, test_framework,
in_package, context and sites.  INDEX counts the file's top-level expressions
from 0, the number SBCL's DEFINITION-SOURCE-FORM-PATH starts with.  Each site
has line, column, kind, token, context and shadowed_by.  Collection stops after
MAX-SITES sites.  Signals the parser's error when TEXT does not parse."
  (let ((nodes (if abs-path
                   (parse-top-level-forms text :source-path (pathname abs-path))
                   (let ((*package* (find-package "COMMON-LISP-USER")))
                     (parse-top-level-forms text))))
        (forms '())
        (count 0)
        (truncated nil)
        (index -1)
        (in-package nil))
    (dolist (node nodes)
      (when (eq (cst-node-kind node) :expr)
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
            (setf in-package designator)))))
    (values (nreverse forms) count truncated)))

(defun %first-line (text)
  "Return TEXT's first line, for a one-line error summary."
  (subseq text 0 (or (position #\Newline text) (length text))))

(defun scan-project (designator &key (root *project-root*) (max-sites *max-scan-sites*))
  "Scan every Lisp file under ROOT for sites of the symbol DESIGNATOR spells.

Validates DESIGNATOR first (see TARGET-NAME-FROM-DESIGNATOR).  The files are
those clgrep-search reads (.lisp, .asd and .ros, honouring .gitignore); only
those whose text contains the name, ignoring case, are parsed.  Returns a
JSON-ready hash-table:
  target_name     the name matched
  root            ROOT's truename namestring, or null
  files_scanned   files considered
  files_matched   files whose text contains the name
  forms           SCAN-TEXT's forms for every file, concatenated
  parse_failures  path, abs_path and error of each file that did not parse
  truncated_at    MAX-SITES when collection stopped there, else null
  skipped_reason  why nothing was scanned, else null"
  (let ((name (target-name-from-designator designator))
        (root-truename (and root (ignore-errors (namestring (truename root)))))
        (forms '())
        (failures '())
        (scanned 0)
        (matched 0)
        (count 0)
        (truncated nil))
    (flet ((report (&optional skipped)
             (make-ht "target_name" name
                      "root" root-truename
                      "files_scanned" scanned
                      "files_matched" matched
                      "forms" (coerce forms 'vector)
                      "parse_failures" (coerce (reverse failures) 'vector)
                      "truncated_at" (and truncated max-sites)
                      "skipped_reason" skipped)))
      (unless root-truename
        (return-from scan-project
          (report (if root "project root is not readable" "project root is not set"))))
      (dolist (file (collect-target-files root-truename))
        (incf scanned)
        (let ((text (and (not truncated) (ignore-errors (uiop:read-file-string file)))))
          (when (and text (search name text :test #'char-equal))
            (incf matched)
            (let ((abs-path (namestring file)))
              (handler-case
                  (multiple-value-bind (file-forms file-count file-truncated)
                      (scan-text text name
                                 :path (normalize-path-for-display file)
                                 :abs-path abs-path
                                 :max-sites (- max-sites count))
                    (setf forms (append forms file-forms))
                    (incf count file-count)
                    (when file-truncated
                      (setf truncated t)))
                (error (e)
                  (push (make-ht "path" (normalize-path-for-display file)
                                 "abs_path" abs-path
                                 "error" (%first-line (princ-to-string e)))
                        failures)))))))
      (report))))
