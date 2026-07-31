;;;; src/lisp-macroexpand.lisp
;;;;
;;;; MCP tool definition for lisp-macroexpand.  This file owns the
;;;; parent-side half of the job: resolving the path, locating the target
;;;; form in the CST, and slicing out its source text.  The expansion
;;;; itself happens in the worker (src/worker/handlers.lisp), because only
;;;; the worker image has the macro definitions loaded.
;;;;
;;;; The parent deliberately ships raw source TEXT rather than a parsed
;;;; form: parse-top-level-forms deletes any package it synthesized before
;;;; returning, so symbols recovered from the CST can be homeless and
;;;; would print with the wrong package qualification.

(defpackage #:cl-mcp/src/lisp-macroexpand
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content
                #:arg-validation-error #:validation-message #:tool-error)
  (:import-from #:cl-mcp/src/state
                #:protocol-version)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json
                #:sanitize-error-message)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:expand-and-build-response)
  ;; Shared with response-builders, which reads these markers back out of the
  ;; label.  No cycle: macroexpand-core imports only cl-mcp/src/utils/sanitize.
  (:import-from #:cl-mcp/src/macroexpand-core
                #:*backquote-template-marker*
                #:*shadowed-binding-marker*)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%locate-target-form
                #:%parse-readtable-designator)
  (:import-from #:cl-mcp/src/cst
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line
                #:%in-package-form-p)
  (:documentation "Parent-side form addressing and tool definition for
lisp-macroexpand.")
  (:export #:lisp-macroexpand))

(in-package #:cl-mcp/src/lisp-macroexpand)

(defparameter *max-sub-form-matches* 10
  "Maximum number of SUB_FORM matches expanded in one call.
When more match, the extras are dropped and the response carries a note
saying so — a silently truncated list would read as full coverage.")

(defparameter *shadowing-operators*
  '("MACROLET" "SYMBOL-MACROLET" "FLET" "LABELS")
  "Operator names whose binding list can shadow a SUB_FORM name.
Compared case-insensitively against SYMBOL-NAME, never against a symbol:
symbols recovered from the CST can be homeless once package-context
teardown has run, so only their name is reliable.")

(defun %bare-symbol-name (name)
  "Return NAME without any package qualifier: \"pkg:sym\" becomes \"sym\"."
  (let ((colon (position #\: name :from-end t)))
    (if colon
        (subseq name (1+ colon))
        name)))

(defun %validate-level (level)
  "Signal ARG-VALIDATION-ERROR unless LEVEL is a recognized expansion level.
The :ENUM in the tool's arg spec only decorates the JSON schema; nothing
validates it at runtime, so an unknown level would otherwise surface as a
generic internal error.  Returns no useful value; call it for effect."
  (when (and level
             (not (member level '("once" "full" "all") :test #'string-equal)))
    (error 'arg-validation-error :arg-name "level"
           :message "level must be one of \"once\", \"full\" or \"all\"."))
  (values))

(defun %find-sub-forms (target sub-form)
  "Return the matches for SUB-FORM strictly inside TARGET.

Each match is a list (NODE IN-TEMPLATE BINDING-OPERATOR SHADOWED-BY):

  IN-TEMPLATE      true when the node sits inside a backquote template.
  BINDING-OPERATOR the lower-case operator name when this node IS a
                   binding pair rather than a call, else NIL.
  SHADOWED-BY      the lower-case operator name of the nearest enclosing
                   form that binds SUB-FORM, else NIL.

Comparison is case-insensitive and ignores any package qualifier written
in SUB-FORM, because symbols recovered from the CST may be homeless after
package-context teardown and only their SYMBOL-NAME is reliable.

A QUOTE subtree is not searched, and neither is #'name: nothing inside
them is evaluated in that position, so a match there would always be
fabricated.  The body of #'(lambda ...) IS searched.  Eclector reads it
as (FUNCTION (LAMBDA ...)), so skipping every FUNCTION subtree used to
hide the whole lambda -- yet its body is ordinary evaluated code, and a
call there is as real as any other.
A backquote template IS searched, because an unquoted island inside one --
`(foo ,(double-it x)) -- is a real call; skipping the template outright
would turn a false positive into a false negative.  Template matches
carry the flag instead, so the caller can label them.

Scope awareness is deliberately NOT an implementation of CL scoping
rules.  It is exactly this one rule: when a node's head names MACROLET,
SYMBOL-MACROLET, FLET or LABELS, its second :EXPR child is the binding
list, and a direct child of that binding list whose own head names
SUB-FORM is a binding position rather than a call.  When such a binding
exists, everything below that node is treated as shadowed for that name,
with no attempt to model which sub-forms the binding actually covers --
FLET's own binding bodies, for one, are outside it.  Over-reporting a
shadow is a hint the caller can check; under-reporting one produces a
confidently wrong expansion.

Matching is otherwise POSITIONAL-BLIND: any other list whose head names
SUB-FORM matches, so the variable of a LET binding pair is still reported
exactly like a call.  Recognizing every such position would require a
full code walker.

Nested self-similar calls overlap: (double-it (double-it n)) yields two
matches, the first of which contains the second."
  (let ((wanted (%bare-symbol-name sub-form))
        (binding-positions (make-hash-table :test #'eq))
        (found '()))
    (labels ((head-name (node)
               ;; The node's own read value, not its first child: eclector
               ;; synthesizes no child node for a reader-macro operator, so
               ;; '(f x), #'f and `(f x) each have the inner form as their
               ;; only child and would otherwise read as calls to whatever
               ;; they contain.  Reading the value also means comments,
               ;; which appear as :SKIPPED children, need no special case.
               (let ((value (cst-node-value node)))
                 (and (consp value)
                      (symbolp (car value))
                      (car value)
                      (symbol-name (car value)))))
             (operator-p (name operator)
               (and name (string-equal name operator)))
             (function-lambda-p (node)
               ;; #'(lambda ...) reads as (FUNCTION (LAMBDA ...)); the
               ;; lambda body is ordinary evaluated code, unlike #'name
               ;; which only references a function.  Compared by
               ;; SYMBOL-NAME because CST symbols can be homeless after
               ;; package-context teardown.
               ;; CONSP on the cdr before SECOND: source text really can
               ;; hold a dotted (function . x), and SECOND would raise a raw
               ;; TYPE-ERROR on it where the pre-branch walk simply skipped
               ;; the node.
               (let ((value (cst-node-value node)))
                 (and (consp value)
                      (consp (cdr value))
                      (consp (second value))
                      (symbolp (car (second value)))
                      (car (second value))
                      (member (symbol-name (car (second value)))
                              '("LAMBDA" "NAMED-LAMBDA")
                              :test #'string-equal))))
             (expr-children (node)
               ;; :SKIPPED children -- comments -- must not shift the
               ;; position of the binding list.
               (remove-if-not (lambda (child)
                                (eq (cst-node-kind child) :expr))
                              (cst-node-children node)))
             (register-bindings (node name)
               ;; Records every binding pair for WANTED under NODE and
               ;; returns the operator name when at least one exists, i.e.
               ;; when NODE shadows WANTED for its whole subtree.  Called
               ;; before descending, so the walk finds the registration
               ;; already in place when it reaches the pair itself.
               (when (and name
                          (member name *shadowing-operators* :test #'string-equal))
                 (let ((bindings (second (expr-children node)))
                       (operator nil))
                   (when bindings
                     (dolist (pair (expr-children bindings))
                       (let ((pair-name (head-name pair)))
                         (when (and pair-name (string-equal pair-name wanted))
                           (setf operator (string-downcase name))
                           (setf (gethash pair binding-positions) operator)))))
                   operator)))
             (walk (node in-template shadowed-by)
               (when (eq (cst-node-kind node) :expr)
                 (let ((name (head-name node)))
                   (when (and name (string-equal name wanted))
                     (push (list node in-template
                                 (gethash node binding-positions)
                                 shadowed-by)
                           found))
                   (unless (or (operator-p name "quote")
                               (and (operator-p name "function")
                                    (not (function-lambda-p node))))
                     (let ((nested (or in-template
                                       (operator-p name "quasiquote")))
                           ;; Innermost binder wins: it is the one whose
                           ;; definition the compiler would use here.
                           (deeper (or (register-bindings node name)
                                       shadowed-by)))
                       (dolist (child (cst-node-children node))
                         (walk child nested deeper))))))))
      (dolist (child (cst-node-children target))
        (walk child nil nil)))
    (nreverse found)))

(defun %binding-position-note (skipped sub-form file)
  "Return the advisory line describing the SKIPPED binding-position matches.
SKIPPED holds matches from %FIND-SUB-FORMS whose BINDING-OPERATOR is set.
They are never expanded -- the parent knows they are not calls -- so the
note is the only place they are reported, and it must say both what they
were and that nothing was done with them."
  (let ((count (length skipped))
        (sites (format nil "~{~A~^, ~}"
                       (loop for (node nil operator) in skipped
                             collect (format nil "~A (~A line ~D, ~A binding)"
                                             sub-form
                                             (file-namestring file)
                                             (cst-node-start-line node)
                                             operator)))))
    (if (= count 1)
        (format nil "NOTE: 1 match is a binding position, not a call, and was ~
skipped: ~A."
                sites)
        (format nil "NOTE: ~D matches are binding positions, not calls, and ~
were skipped: ~A."
                count sites))))

(defun %sub-form-entries (original target sub-form file)
  "Return (values ENTRIES NOTE) for every SUB-FORM match inside TARGET.
ENTRIES is a list of (LABEL . SOURCE) conses; NOTE carries the advisory
lines -- binding positions skipped, match list capped -- or NIL.

A match inside a backquote template is labelled as such: at that site it
is template data, not necessarily a call, and an unlabelled entry would
read exactly like a real call site.

A match in a binding position is not expanded at all.  It is not a call,
so the expander would be handed the binding pair as its argument list and
would answer with its own arity complaint, which reads like a defect in
the macro under inspection.  It is reported through NOTE instead.

A match below a binder for the same name IS expanded -- the null-lexical
expansion is the only one this tool can produce -- but its label carries
*SHADOWED-BINDING-MARKER* so the caller is told the result comes from the
global definition and is not what the compiler sees there."
  (let* ((matches (%find-sub-forms target sub-form))
         (skipped (remove-if-not #'third matches))
         (callable (remove-if #'third matches))
         (total (length callable))
         (skip-note (when skipped
                      (%binding-position-note skipped sub-form file))))
    (when (null matches)
      (error "No call to ~A found inside the addressed form in ~A."
             sub-form file))
    (when (null callable)
      ;; Every match was a binding position.  Saying only "no call found"
      ;; would hide the matches that were found and deliberately dropped.
      (error "No call to ~A found inside the addressed form in ~A. ~A"
             sub-form file skip-note))
    (let* ((kept (if (> total *max-sub-form-matches*)
                     (subseq callable 0 *max-sub-form-matches*)
                     callable))
           (cap-note (when (> total *max-sub-form-matches*)
                       (format nil "NOTE: ~D calls to ~A matched; showing the first ~D."
                               total sub-form *max-sub-form-matches*)))
           (notes (remove nil (list skip-note cap-note))))
      (values
       (loop for (node in-template nil shadowed-by) in kept
             for index from 1
             collect (cons (format nil "~A (~A line ~D)~A~A~A"
                                   sub-form
                                   (file-namestring file)
                                   (cst-node-start-line node)
                                   (if (> total 1)
                                       (format nil " [~D/~D]" index total)
                                       "")
                                   ;; The markers are shared parameters, not
                                   ;; literals: %FORMAT-MACROEXPAND-TEXT
                                   ;; searches the label for them.
                                   (if in-template
                                       *backquote-template-marker*
                                       "")
                                   (if shadowed-by
                                       *shadowed-binding-marker*
                                       ""))
                           (subseq original
                                   (cst-node-start node)
                                   (cst-node-end node))))
       (when notes
         (format nil "~{~A~^~%~}" notes))))))

(defun %package-in-effect-at (nodes offset)
  "Return the package named by the last IN-PACKAGE form before OFFSET, or NIL.

NODES are the file's top-level CST nodes in source order.  The file's
FIRST in-package is the wrong answer for the classic prologue --
(in-package :cl-user), (defpackage ...), (in-package :real-pkg) -- where
everything after it belongs to :real-pkg.  Reading the target in
COMMON-LISP-USER instead makes a macro that IS loaded look undefined, and
builds a whole-form expansion out of CL-USER symbols while presenting it
as correct.

:SKIPPED nodes -- comments -- are ignored, and an in-package form that
starts at or after OFFSET has not taken effect yet.  The designator is
extracted with cl-mcp/src/cst's own %IN-PACKAGE-FORM-P, so this answer
cannot drift from the one the parser used while reading the file."
  (let ((designator nil))
    (dolist (node nodes designator)
      (when (and (eq (cst-node-kind node) :expr)
                 (< (cst-node-start node) offset))
        (let ((name (%in-package-form-p (cst-node-value node))))
          (when name
            (setf designator name)))))))

(defun %collect-file-sources (path form-type form-name sub-form package readtable)
  "Locate the addressed form in PATH.
Returns (values ENTRIES PACKAGE-NAME NOTE)."
  (unless form-type
    (error 'arg-validation-error :arg-name "form_type"
           :message "form_type is required when 'path' is given."))
  (unless form-name
    (error 'arg-validation-error :arg-name "form_name"
           :message "form_name is required when 'path' is given."))
  (when (and sub-form readtable)
    (error 'arg-validation-error :arg-name "sub_form"
           ;; FORMAT NIL, not a bare literal: ARG-VALIDATION-ERROR's report
           ;; prints :MESSAGE verbatim with ~A, so a "~" line continuation in
           ;; a plain string would reach the user as a literal tilde.
           :message (format nil "sub_form cannot be combined with 'readtable': ~
a custom readtable forces the standard CL reader, which does not record ~
sub-form positions. Drop one of the two arguments.")))
  (let ((designator (%parse-readtable-designator readtable)))
    (multiple-value-bind (absolute relative original nodes target snippet
                          form-type-string file-package-name)
        (%locate-target-form path form-type form-name designator)
      (declare (ignore relative))
      ;; FILE-PACKAGE-NAME is the file's FIRST in-package, which is the
      ;; right package only for a file without a prologue.  It stays as the
      ;; fallback for a target that precedes every in-package form -- the
      ;; defpackage itself, say -- where the file's declared package is a
      ;; better guess than CL-USER.
      (let ((package-name (or package
                              (%package-in-effect-at nodes
                                                     (cst-node-start target))
                              file-package-name
                              "COMMON-LISP-USER")))
        (if sub-form
            (progn
              ;; The argument guard above covers a readtable the CALLER
              ;; passed.  A file that declares its own (in-readtable ...)
              ;; switches cst.lisp to the standard CL reader for everything
              ;; after it, and that path records no children at all -- so
              ;; sub_form would report "no call found" for a call plainly
              ;; present.  Keying on the empty children rather than on the
              ;; declaration keeps this correct however the switch happened.
              (when (null (cst-node-children target))
                (error 'arg-validation-error :arg-name "sub_form"
                       :message (format nil "sub_form cannot be used with ~A: ~
the file declares its own readtable with an (in-readtable ...) form, which ~
forces the standard CL reader, and that reader records no sub-form ~
positions. Address the whole form without sub_form, or pass the nested ~
call itself as 'code'."
                                        (file-namestring absolute))))
              (multiple-value-bind (entries note)
                  (%sub-form-entries original target sub-form
                                     (namestring absolute))
                (values entries package-name note)))
            (values (list (cons (format nil "~A ~A (~A line ~D)"
                                        form-type-string form-name
                                        (file-namestring absolute)
                                        (cst-node-start-line target))
                                snippet))
                    package-name
                    nil))))))

(defun %collect-expansion-sources (&key path form-type form-name sub-form code
                                        package readtable)
  "Resolve the request into expandable source text.

Returns (values ENTRIES PACKAGE-NAME NOTE), where ENTRIES is a list of
(LABEL . SOURCE) conses.  Exactly one of PATH and CODE must be supplied."
  (when (and path code)
    (error 'arg-validation-error :arg-name "code"
           ;; FORMAT NIL, not a bare literal: see %COLLECT-FILE-SOURCES.
           :message (format nil "Provide either 'path' (with form_type and ~
form_name) or 'code', not both.")))
  (when (and (null path) (null code))
    (error 'arg-validation-error :arg-name "path"
           :message (format nil "Provide either 'path' (with form_type and ~
form_name) or 'code'.")))
  (when (and code sub-form)
    (error 'arg-validation-error :arg-name "sub_form"
           :message (format nil "sub_form applies to 'path' mode only: with ~
'code' there is no enclosing form to search in. Drop one of the two.")))
  (if code
      (values (list (cons nil code))
              (or package "COMMON-LISP-USER")
              nil)
      (%collect-file-sources path form-type form-name sub-form package
                             readtable)))

(defun %entries->json (entries)
  "Convert (LABEL . SOURCE) conses into a JSON-ready vector of hash-tables."
  (map 'vector
       (lambda (entry)
         (make-ht "label" (car entry) "source" (cdr entry)))
       entries))

(defun lisp-macroexpand (&key path form-type form-name sub-form code package
                              level readtable print-level print-length
                              max-output-length)
  "Locate the target form(s) and expand them in THIS image.

Returns the tool response payload hash-table.  This is the inline path
used when the worker pool is disabled; normally the tool proxies to the
worker instead, because only the worker has the target system loaded.

Expansion and response building are delegated to
EXPAND-AND-BUILD-RESPONSE, the same function the worker handler calls, so
the two paths cannot drift."
  (%validate-level level)
  (multiple-value-bind (entries package-name note)
      (%collect-expansion-sources :path path :form-type form-type
                                  :form-name form-name :sub-form sub-form
                                  :code code :package package
                                  :readtable readtable)
    (expand-and-build-response entries
                               :package package-name
                               :level (or level "once")
                               :readtable readtable
                               :print-level print-level
                               :print-length print-length
                               :max-output-length max-output-length
                               :note note
                               :sub-form-p (and sub-form t))))

(define-tool "lisp-macroexpand" :description
 "Expand a macro call and show the resulting source.

Two ways to name what to expand:
1. FILE MODE — 'path' plus 'form_type' and 'form_name', the same addressing
   'lisp-edit-form' uses. Add 'sub_form' to expand a macro call nested INSIDE
   that top-level form (e.g. a 'with-...' block inside a defun). This is the
   reason to prefer this tool over calling macroexpand-1 through 'repl-eval':
   you never have to copy the form into a string and escape it.
2. CODE MODE — 'code' plus 'package', for a call site you compose yourself.

'level' controls how far to go:
  once (default) — one macroexpand-1 step, best for checking a macro you just wrote
  full           — repeat until the head is no longer a macro, or until 100 steps
  all            — walk the whole form and expand nested macros too. Output can
                   get very large; nested macro calls in the body are expanded
                   (loop, for one, expands down to special forms), but defun
                   itself is left in place, so a whole defun still comes back
                   headed by defun.

PREREQUISITE: the macro must be DEFINED in the worker image. If the package
does not exist you get an error telling you to run 'load-system' first; after
editing a defmacro on disk, reload before expanding.

Output is printed lower-case, pretty-printed, and relative to the target
package, so it can be read as source. Shared-structure markers (#1=) are
suppressed unless the expansion is genuinely circular, in which case they are
switched on so printing terminates; depth and length are bounded regardless.

LIMITATIONS: expansion always uses a null lexical environment. 'sub_form'
detects an enclosing macrolet, symbol-macrolet, flet or labels that binds the
same name: a match below one is still expanded — the global definition is the
only one available — but is labelled as shadowed, so a result that differs from
what the compiler sees is never presented as correct. A binding pair of those
four operators is recognized as a binding position, skipped instead of expanded,
and reported in a note. Matching is otherwise positional-blind: it matches any
list whose head is the name, so the variable of a let binding pair is still
reported just like a call. Quoted subtrees and #'name are skipped, but the body
of #'(lambda ...) is searched, and a match inside a backquote template is
labelled as such.
'sub_form' cannot be combined with 'readtable', nor used on a file that
declares its own (in-readtable ...). Expanding runs the macro's expander
function, i.e. arbitrary code, in the isolated worker process — or in the
server process itself when the worker pool is disabled."
 :args
 ((path :type :string :description
   "File containing the form; use with form_type and form_name")
  (form_type :type :string :description
   "Defining form type, e.g. defun, defmacro, define-tool")
  (form_name :type :string :description
   "Name of the form; supports 'name[N]' to pick the Nth match")
  (sub_form :type :string :description
   ;; Built from the variable so the prose and the cap cannot disagree.
   (format nil "Macro name to expand INSIDE the addressed form; expands ~
every call, up to ~D" *max-sub-form-matches*))
  (code :type :string :description
   "Source text of a form to expand; alternative to path")
  (package :type :string :description
   "Package to read the form in; defaults to the in-package in effect at the form, else CL-USER")
  (level :type :string :enum ("once" "full" "all") :description
   "Expansion depth: once (default), full, or all")
  (readtable :type :string :description
   "Named-readtable designator for files using custom reader macros")
  (print_level :type :integer :description
   "Maximum printed nesting depth (default 50; must stay finite)")
  (print_length :type :integer :description
   "Maximum printed list length (default 1000)")
  (max_output_length :type :integer :description
   "Maximum characters per expansion (default 50000)"))
 :body
 ;; Mirrors lisp-edit-form's tool body: an expected operational failure --
 ;; above all "Form <type> <name> not found", the most likely way to address
 ;; this tool wrongly -- is presented as its own message instead of behind
 ;; define-tool's generic "Internal error during ..." wrapper.
 ;;
 ;; ARG-VALIDATION-ERROR is a subtype of ERROR and MUST stay listed first:
 ;; HANDLER-CASE takes the first matching clause, so a lone ERROR clause would
 ;; catch validation failures too and strip them of the protocol-aware
 ;; treatment define-tool's own handler gives them.  lisp-edit-form avoids
 ;; that by raising its one validation error outside the HANDLER-CASE, and
 ;; that was available here too -- these are pure argument checks, touching
 ;; neither disk nor CST, so they could have been hoisted ahead of it.  It
 ;; was rejected because %COLLECT-EXPANSION-SOURCES has to keep them
 ;; regardless: the exported function and the tests call it directly.  One
 ;; set of rules would then live in two places, free to drift apart.
 ;; Spelling the clause out keeps them in one place -- same treatment,
 ;; reached explicitly rather than by ordering.
 (handler-case
     (with-proxy-dispatch (id "worker/macroexpand"
                              (progn
                                (%validate-level level)
                                (multiple-value-bind (entries package-name note)
                                    (%collect-expansion-sources
                                     :path path :form-type form_type
                                     :form-name form_name :sub-form sub_form
                                     :code code :package package
                                     :readtable readtable)
                                  (make-ht "forms" (%entries->json entries)
                                           "package" package-name
                                           "note" note
                                           ;; Sent so the worker can tell a
                                           ;; sub_form match from a whole-form
                                           ;; one when wording NOT EXPANDED.
                                           "sub_form" sub_form
                                           "level" (or level "once")
                                           "readtable" readtable
                                           "print_level" print_level
                                           "print_length" print_length
                                           "max_output_length" max_output_length))))
       (result id
               (lisp-macroexpand :path path :form-type form_type
                                 :form-name form_name :sub-form sub_form
                                 :code code :package package :level level
                                 :readtable readtable :print-level print_level
                                 :print-length print_length
                                 :max-output-length max_output_length)))
   (arg-validation-error (e)
     (tool-error id (validation-message e)
                 :protocol-version (protocol-version state)))
   (error (e)
     (let ((msg (sanitize-for-json
                 (sanitize-error-message (format nil "~A" e)))))
       (if (and (protocol-version state)
                (string>= (protocol-version state) "2025-11-25"))
           (result id (make-ht "content" (text-content msg) "isError" t))
           (rpc-error id -32603 msg))))))
