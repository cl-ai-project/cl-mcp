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
  ;; Shared with response-builders, which reads this marker back out of the
  ;; label.  No cycle: macroexpand-core imports only cl-mcp/src/utils/sanitize.
  (:import-from #:cl-mcp/src/macroexpand-core
                #:*backquote-template-marker*)
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
                #:cst-node-start-line)
  (:documentation "Parent-side form addressing and tool definition for
lisp-macroexpand.")
  (:export #:lisp-macroexpand))

(in-package #:cl-mcp/src/lisp-macroexpand)

(defparameter *max-sub-form-matches* 10
  "Maximum number of SUB_FORM matches expanded in one call.
When more match, the extras are dropped and the response carries a note
saying so — a silently truncated list would read as full coverage.")

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

Each match is a cons of the CST node and a flag that is true when the
node sits inside a backquote template.

Comparison is case-insensitive and ignores any package qualifier written
in SUB-FORM, because symbols recovered from the CST may be homeless after
package-context teardown and only their SYMBOL-NAME is reliable.

QUOTE and FUNCTION subtrees are not searched: nothing inside them is
evaluated in that position, so a match there would always be fabricated.
A backquote template IS searched, because an unquoted island inside one --
`(foo ,(double-it x)) -- is a real call; skipping the template outright
would turn a false positive into a false negative.  Template matches
carry the flag instead, so the caller can label them.

Matching is otherwise POSITIONAL-BLIND: any list whose head names
SUB-FORM matches, so a binding position -- the variable of a LET binding
pair, or an FLET/LABELS name -- is reported exactly like a call.
Recognizing those would require a full code walker.

Nested self-similar calls overlap: (double-it (double-it n)) yields two
matches, the first of which contains the second."
  (let ((wanted (%bare-symbol-name sub-form))
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
             (walk (node in-template)
               (when (eq (cst-node-kind node) :expr)
                 (let ((name (head-name node)))
                   (when (and name (string-equal name wanted))
                     (push (cons node in-template) found))
                   (unless (or (operator-p name "quote")
                               (operator-p name "function"))
                     (let ((nested (or in-template
                                       (operator-p name "quasiquote"))))
                       (dolist (child (cst-node-children node))
                         (walk child nested))))))))
      (dolist (child (cst-node-children target))
        (walk child nil)))
    (nreverse found)))

(defun %sub-form-entries (original target sub-form file)
  "Return (values ENTRIES NOTE) for every SUB-FORM match inside TARGET.
ENTRIES is a list of (LABEL . SOURCE) conses; NOTE is non-NIL when the
match list was capped at *MAX-SUB-FORM-MATCHES*.

A match inside a backquote template is labelled as such: at that site it
is template data, not necessarily a call, and an unlabelled entry would
read exactly like a real call site."
  (let* ((matches (%find-sub-forms target sub-form))
         (total (length matches)))
    (when (null matches)
      (error "No call to ~A found inside the addressed form in ~A."
             sub-form file))
    (let ((kept (if (> total *max-sub-form-matches*)
                    (subseq matches 0 *max-sub-form-matches*)
                    matches))
          (note (when (> total *max-sub-form-matches*)
                  (format nil "NOTE: ~D calls to ~A matched; showing the first ~D."
                          total sub-form *max-sub-form-matches*))))
      (values
       (loop for (node . in-template) in kept
             for index from 1
             collect (cons (format nil "~A (~A line ~D)~A~A"
                                   sub-form
                                   (file-namestring file)
                                   (cst-node-start-line node)
                                   (if (> total 1)
                                       (format nil " [~D/~D]" index total)
                                       "")
                                   ;; The marker is a shared parameter, not a
                                   ;; literal: %FORMAT-MACROEXPAND-TEXT
                                   ;; searches the label for it.
                                   (if in-template
                                       *backquote-template-marker*
                                       ""))
                           (subseq original
                                   (cst-node-start node)
                                   (cst-node-end node))))
       note))))

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
      (declare (ignore relative nodes))
      (let ((package-name (or package file-package-name "COMMON-LISP-USER")))
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

LIMITATIONS: expansion uses a null lexical environment, so a form enclosed in
macrolet or symbol-macrolet may expand differently than the compiler sees it.
'sub_form' matching is positional-blind: it matches any list whose head is the
name, so a binding position — the variable of a let binding pair, or an
flet/labels name — is reported just like a call. Quoted and #' subtrees are
skipped, and a match inside a backquote template is labelled as such.
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
   "Package to read the form in; defaults to the file's in-package, else CL-USER")
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
