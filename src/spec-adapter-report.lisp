;;;; src/spec-adapter-report.lisp
;;;;
;;;; The three operations the cl-spec tools expose, as plists.
;;;;
;;;; Everything here answers with a plist headed by :STATUS rather than by
;;;; signalling.  cl-spec signals UNKNOWN-PROPERTY for a name it does not
;;;; know, and an agent asking "is there a contract for this symbol?" must be
;;;; told "no" rather than handed a condition -- "not registered", "cl-spec is
;;;; not loaded" and "this cl-spec cannot do that" are three different answers
;;;; and none of them is an error in the caller.

(defpackage #:cl-mcp/src/spec-adapter-report
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:cl-spec-api-version
                #:cl-spec-api-system-directory
                #:cl-spec-api-missing
                #:api-fn
                #:api-has-p
                #:api-class
                #:api-special
                #:api-backend-available-p
                #:resolve-symbol-designator
                #:find-package-named
                #:find-keyword
                #:symbol-data
                #:externalize-value
                #:definition-digest
                #:printed-for-display
                #:print-form-bounded)
  (:import-from #:cl-mcp/src/object-registry
                #:register-object)
  (:import-from #:cl-mcp/src/utils/bounded-stream
                #:make-bounded-output-stream
                #:bounded-output-string)
  (:import-from #:cl-mcp/src/code-core
                #:code-describe-symbol)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread)
  (:export #:list-report
           #:+result-statuses+
           #:+call-statuses+
           #:+verification-gap-values+
           #:+listing-kinds+
           #:listing-kind-wanted-p
           #:environment-data
           #:unavailable-report
           #:symbol-report
           #:describe-report
           #:check-report
           #:*default-check-timeout-seconds*))

(in-package #:cl-mcp/src/spec-adapter-report)

;;; ---------------------------------------------------------------------------
;;; Environment
;;; ---------------------------------------------------------------------------

(defun %backend-name (api)
  "Return the installed generator backend's class name, or NIL."
  (handler-case
      (let ((backend (and (api-has-p api :generator-backend)
                          (funcall (api-fn api :generator-backend)))))
        (when backend
          (if (symbolp backend)
              (princ-to-string backend)
              (let ((name (class-name (class-of backend))))
                (format nil "~A:~A"
                        (package-name (symbol-package name))
                        (symbol-name name))))))
    (error () nil)))

(defun %registry-description (api)
  "Return a short printed description of the registry in effect, or NIL."
  (handler-case
      (let ((registry (and (api-has-p api :registry)
                           (funcall (api-fn api :registry)))))
        (when registry
          (let ((*print-level* 1) (*print-length* 2) (*print-readably* nil))
            (princ-to-string registry))))
    (error () nil)))

(defun environment-data (api api-status)
  "Return what this image can say about the cl-spec it is talking to.

Attached to every response.  An agent that cannot tell a missing system from a
missing registration will read an empty answer as a clean bill of health, and
that is the single most expensive mistake available here."
  (list :cl-spec-loaded (eq api-status :ok)
        :cl-spec-status api-status
        :cl-spec-version (and api (cl-spec-api-version api))
        :cl-spec-system-directory (and api (cl-spec-api-system-directory api))
        :generator-backend (and api (%backend-name api))
        :backend-available (and api (api-backend-available-p api))
        :registry (and api (%registry-description api))
        :missing (and api (cl-spec-api-missing api))
        :lisp (format nil "~A ~A"
                      (lisp-implementation-type)
                      (lisp-implementation-version))))

(defparameter +not-loaded-message+
  (concatenate 'string
               "cl-spec is not loaded in this session's worker. Run "
               "load-system with system \"cl-spec/check-it\" to get "
               "introspection and property execution, or \"cl-spec\" for "
               "introspection only. Until then this tool cannot tell whether "
               "anything is registered: an empty answer here is NOT evidence "
               "that the symbol has no contract.")
  "Said when the CL-SPEC package is absent.

CONCATENATE rather than a string literal broken across lines: in Common Lisp
a backslash before a newline escapes the newline INTO the string rather than
continuing the line, so the C-looking spelling would put hard newlines and
trailing spaces into every message and every JSON field carrying one.")

(defparameter +incomplete-message+
  (concatenate 'string
               "cl-spec is loaded but does not provide every name this "
               "adapter needs; see the missing list in environment. This is a "
               "version mismatch between cl-mcp and cl-spec, not a statement "
               "about the symbol.")
  "Said when cl-spec is present but a required name is missing.")

(defparameter +runtime-escaping-message+
  (concatenate 'string
               "symbol or package name would need escaping; the runtime "
               "lookup reads its argument back and could resolve a different "
               "symbol")
  "Said when the runtime join declines a name it cannot print unambiguously.")

(defparameter +symbol-is-a-property-note+
  (concatenate 'string
               "this symbol is itself a registered property; run it with "
               "property=")
  "Noted when a symbol is both a property name and a target of others.")

(defparameter +symbol-is-a-property-not-selected-note+
  (concatenate 'string
               "this symbol is itself a registered property; it was NOT "
               "selected -- run it with property=")
  "Noted on a selection that deliberately left the symbol's own property out.")

(defparameter +contract-coverage-note+
  (concatenate 'string
               "Only the contract named: its :pre, :returns and :post over "
               "arguments generated from its :args. Properties registered "
               "about the same symbol are NOT included and were not run.")
  "The limit of what an explicit contract selection covers.")

(defparameter +about-source+
  (concatenate 'string
               "cl-spec:semantic-data -> :properties-about "
               "(registry :about reverse index)")
  "Where an :ABOUT selection came from, reported so a caller can weigh it.")

(defparameter +budget-derivation-note+
  (concatenate 'string
               "derived by cl-mcp from cl-spec:property-trials and "
               "cl-spec:backend-default-trials; cl-spec does not expose the "
               "resolved budget")
  "How the trial budget was arrived at, said so a reader can question it.")

(defun unavailable-report (api-status environment)
  "Return the plist answering a call cl-spec cannot serve at all."
  (list :status (if (eq api-status :not-loaded)
                    :cl-spec-not-loaded
                    :cl-spec-incomplete)
        :verified nil
        :message (if (eq api-status :not-loaded)
                     +not-loaded-message+
                     +incomplete-message+)
        :environment environment))

;;; ---------------------------------------------------------------------------
;;; Shared helpers
;;; ---------------------------------------------------------------------------

(defun %take (list limit)
  "Return at most LIMIT elements of LIST."
  (if (and limit (> (length list) limit))
      (subseq list 0 limit)
      list))

(defun %print-bounded-form (form max-chars)
  "Return (values TEXT COMPLETE-P OMITTED-CHARS) for FORM at MAX-CHARS.

Delegates to PRINT-FORM-BOUNDED, which stops at the budget instead of
rendering the whole form and cutting.  The previous spelling measured the text
PRINTED-FOR-DIGEST had already truncated at its own million-character limit,
so a larger form reported the wrong remainder -- and reported itself complete
whenever MAX-CHARS reached that limit.  It also fed MAX-CHARS straight to
SUBSEQ as an end index, where a negative value signalled a type error that the
caller above then mislabelled."
  (print-form-bounded form (max 1 max-chars)))

(defun %plainly-printable-p (name)
  "Return true when NAME needs no escaping to be read back as itself.

CODE-DESCRIBE-SYMBOL takes a string and reads it.  Handing it the unescaped
name of a symbol containing a colon, a space or a bar would have it read a
different symbol -- or a symbol in a different package -- and report that
one's signature under this one's name.  Refusing is the only safe answer:
this join is decoration on the registry's own facts, never the identity."
  (and (plusp (length name))
       (every (lambda (character)
                (and (graphic-char-p character)
                     (not (member character '(#\: #\Space #\| #\\ #\( #\) #\"
                                              #\; #\' #\` #\, #\#)))))
              name)
       (string= name (string-upcase name))))

(defun %runtime-data (symbol)
  "Return (values RUNTIME REASON) for SYMBOL from cl-mcp's own introspection.

This is the half of specification 28's describe_symbol that cl-spec's registry
does not hold: the signature, the docstring and where the definition lives.
A symbol with no binding at all is not an error here -- a property may be
registered about a symbol that is not yet defined -- so the reason is
reported and the caller carries on."
  (let ((package (symbol-package symbol))
        (name (symbol-name symbol)))
    (cond
      ((null package)
       (values nil "symbol is uninterned and has no source to look up"))
      ((not (and (%plainly-printable-p name)
                 (%plainly-printable-p (package-name package))))
       (values nil +runtime-escaping-message+))
      (t
       (handler-case
           (multiple-value-bind (found type arglist documentation path line)
               (code-describe-symbol (format nil "~A::~A"
                                             (package-name package) name))
             (declare (ignore found))
             (values (list :type type
                           :arglist arglist
                           :documentation documentation
                           :source-file path
                           :source-line line)
                     nil))
         (error (condition)
           (values nil (princ-to-string condition))))))))

(defun %spec-summary (spec-plist)
  "Return the one-line summary of an argument's spec, or NIL when there is none.

Guarded rather than assumed to be a plist: an argument whose :SPEC is NIL --
or anything that is not a list -- must answer \"no spec\" rather than signal
into the caller's handler and take the whole listing entry down with it."
  (unless (and spec-plist (listp spec-plist))
    (return-from %spec-summary nil))
  (list :kind (getf spec-plist :kind)
        :name (let ((name (getf spec-plist :name)))
                (when name (symbol-data name)))
        :target (let ((target (getf spec-plist :target)))
                  (when target (symbol-data target)))))

(defun %property-summary (api name registry)
  "Return the listing entry for property NAME, with its body omitted.

The body and the source form are deliberately left out: PROPERTY-DATA carries
the author's whole DEFPROPERTY form, and inlining that for every property a
symbol has would make one response's size depend on how much its author
wrote.  BODY-OMITTED says so rather than leaving a reader to infer it."
  (handler-case
      (let ((data (funcall (api-fn api :property-data) name :registry registry)))
        (list :name (symbol-data name)
              :kind (getf data :kind)
              :tags (getf data :tags)
              :targets (mapcar #'symbol-data (getf data :targets))
              :documentation (getf data :documentation)
              :arguments (loop for argument in (getf data :arguments)
                               collect (list :variable
                                             (symbol-data (getf argument :variable))
                                             :spec (%spec-summary
                                                    (getf argument :spec))))
              :trials-table (let ((table (getf data :trials)))
                              (when table (printed-for-display table)))
              :shrink-enabled (getf (getf data :metadata) :shrink)
              :source-location (getf data :source-location)
              ;; The digest is derived from the data already in hand.  Left
              ;; to fetch its own, a listing of N properties made 2N calls.
              :definition-digest (definition-digest api name registry
                                                    :property data)
              :body-forms (length (getf data :body))
              :body-omitted t
              :detail-via "spec-describe kind=property"))
    (error (condition)
      (list :name (symbol-data name)
            :unavailable-reason (princ-to-string condition)))))

;;; ---------------------------------------------------------------------------
;;; spec-symbol
;;; ---------------------------------------------------------------------------

(defun symbol-report (api api-status designator &key package (include-runtime t))
  "Return the plist behind the spec-symbol tool.

Joins what cl-spec's registry knows about DESIGNATOR with what this image
knows about it (specification 27-28: describe_symbol is a join, and the half
that is a signature and a source location belongs to cl-mcp)."
  (let ((environment (environment-data api api-status)))
    (unless (eq api-status :ok)
      (return-from symbol-report (unavailable-report api-status environment)))
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator designator :package package)
      (unless symbol
        (return-from symbol-report
          (list :status :unresolved-symbol
                :reason reason
                :input designator
                :environment environment)))
      (multiple-value-bind (registry routing failure)
          (handler-case
              (let ((registry (funcall (api-fn api :registry))))
                (values registry
                        (funcall (api-fn api :semantic-data) symbol
                                 :registry registry)
                        nil))
            (error (condition) (values nil nil condition)))
        (when failure
          ;; The module answers with a status rather than signalling, and
          ;; this call was the one place that did not.  RESOLVE-CL-SPEC-API
          ;; only checks FBOUNDP, so a cl-spec whose signatures drifted
          ;; resolves to :OK and then signals a PROGRAM-ERROR out of the tool.
          (return-from symbol-report
            (list :status :internal-error
                  :symbol (symbol-data symbol)
                  :message (format nil "cl-spec signalled while reading what ~
is registered about this symbol: ~A" failure)
                  :environment environment)))
        (let ((about (getf routing :properties-about)))
        (multiple-value-bind (runtime runtime-reason)
            (if include-runtime (%runtime-data symbol) (values nil nil))
          (list :status :ok
                :symbol (symbol-data symbol)
                :runtime runtime
                :runtime-unavailable-reason
                (cond (runtime nil)
                      ((not include-runtime) "include_runtime was false")
                      (t runtime-reason))
                :registry
                (list :spec (let ((name (getf routing :spec)))
                              (when name (symbol-data name)))
                      :function-spec (let ((name (getf routing :function-spec)))
                                       (when name (symbol-data name)))
                      :property (let ((name (getf routing :property)))
                                  (when name (symbol-data name)))
                      :properties-about (mapcar #'symbol-data about))
                :properties (loop for name in about
                                  collect (%property-summary api name registry))
                :nothing-registered (not (or about
                                             (getf routing :spec)
                                             (getf routing :function-spec)
                                             (getf routing :property)))
                :notes (append
                        (list "properties_about lists direct (:about ...) registrations only")
                        (when (getf routing :property)
                          (list +symbol-is-a-property-note+))
                        (when (getf routing :function-spec)
                          (list (%contract-note api))))
                :environment environment)))))))

;;; ---------------------------------------------------------------------------
;;; spec-describe
;;; ---------------------------------------------------------------------------

(defun %unknown-registration-p (api condition)
  "Return true when CONDITION is cl-spec saying a name is not registered.

Asked of the API's condition classes rather than of the message, and false
when the classes are unavailable: mistaking an internal failure for an absent
registration is the expensive direction, so an image whose cl-spec predates
UNKNOWN-SPEC reports the failure as a failure."
  (flet ((is-a (key)
           (let ((class (api-class api key)))
             (and class (typep condition class)))))
    (or (is-a :unknown-spec)
        (is-a :unknown-property)
        (is-a :unknown-function-spec))))

(defparameter +listing-kinds+
  '(("specs" "specs" :specs-listable (:list-specs))
    ("properties" "properties" :properties-listable (:list-properties))
    ("function-specs" "function specs" :function-specs-listable
     (:list-function-specs :function-spec-data)))
  "One row per listing half: KIND name, label, listable key, required API keys.

The kind-to-handle mapping was written four times -- the refusal message, the
reachability gate, the per-half bindings here and the renderer's notes -- and
two of those four have already disagreed in this branch: a gate that refused a
kind reading neither handle, and a tag header claiming a filter over a half
this cl-spec cannot enumerate.  One table, read by all four.

\"both\" asks for every row; any other KIND asks for the row it names.")

(defun listing-kind-wanted-p (row kind)
  "Return true when KIND asks for the listing half ROW describes."
  (or (string= kind "both") (string= kind (first row))))

(defun %listing-unsupported-message (missing)
  "Return the message for a listing kind this cl-spec cannot enumerate.

Names the handles the requested kind actually needs.  One sentence covering
both list functions and claiming \"nothing can be enumerated\" was two false
statements on a revision that exports one of them: the other kinds still
enumerate, and contracts enumerate through readers neither of these names."
  (format nil "this cl-spec revision does not export ~{~A~^ or ~}, so the ~
requested kind cannot be enumerated. Another kind may still list, and every ~
other spec tool still works on a name you already have."
          missing))

(defparameter +listing-coverage-note+
  (concatenate 'string
               "Everything registered in this worker's cl-spec registry. A "
               "definition whose system has not been loaded is not here, and "
               "an empty listing is not evidence that a project has no "
               "contracts.")
  "The limit of what a listing covers.")

(defun %property-listing (api name registry)
  "Return the one-line listing entry for property NAME.

Deliberately cheaper than %PROPERTY-SUMMARY: no digest, no arguments, no body
count.  A listing is the entry point for someone who does not yet know what is
here, and computing a transitive spec closure per property to answer \"what
exists\" is the shape of question that should stay cheap."
  (handler-case
      (let ((data (funcall (api-fn api :property-data) name :registry registry)))
        (list :name (symbol-data name)
              :kind (getf data :kind)
              :tags (getf data :tags)
              :targets (mapcar #'symbol-data (getf data :targets))
              :documentation (getf data :documentation)))
    (error () (list :name (symbol-data name)))))

(defun %function-spec-listing (api name registry)
  "Return the one-line listing entry for the contract registered for NAME.

Reports the parameter names and whether the contract carries a :RETURNS, which
is what tells a reader whether the entry answers \"which output must this
return\" at all.  The specs themselves stay in spec-describe: a listing is for
finding a name, not for reading a contract."
  (handler-case
      (let ((data (funcall (api-fn api :function-spec-data) name :registry registry)))
        (list :name (symbol-data name)
              :parameters (mapcar (lambda (argument)
                                    (symbol-data (getf argument :variable)))
                                  (getf data :arguments))
              :returns-specified (and (getf data :returns) t)
              :precondition-count (length (getf data :preconditions))
              :postcondition-count (length (getf data :postconditions))
              :documentation (getf data :documentation)))
    ;; Marked, not merely emptied.  An entry with no parameters and no
    ;; :RETURNS renders exactly like a genuine zero-argument contract with no
    ;; return spec, so a read that failed came out as a positive claim about
    ;; the contract -- and telling a reader whether an entry answers "which
    ;; output must this return" is the listing's whole job.
    (error () (list :name (symbol-data name) :read-failed t))))

(defun %listing-package-filter (package)
  "Return (values PACKAGE-OBJECT ERROR) for the listing's package filter."
  (if (null package)
      (values nil nil)
      (let ((found (find-package-named package)))
        (if found
            (values found nil)
            (values nil (list :status :unresolved-package
                              :message
                              (format nil "No package named ~A exists in this ~
image, so nothing can be listed from it. It was looked up, not created."
                                      package)))))))

(defun %in-package-p (name package)
  "Return true when NAME's home package is PACKAGE, or PACKAGE is NIL."
  (or (null package) (eq (symbol-package name) package)))

(defun list-report (api api-status &key kind package tag (limit 200))
  "Return the plist behind the spec-list tool.

KIND is \"specs\", \"properties\", \"function-specs\" or \"both\", and each is
gated on the cl-spec handles it actually reads.  \"specs\" needs LIST-SPECS,
\"properties\" needs LIST-PROPERTIES, \"both\" needs the two of them, and
\"function-specs\" needs neither -- it reads LIST-FUNCTION-SPECS and
FUNCTION-SPEC-DATA, whose absence is fatal to no kind: contracts are left out
and FUNCTION-SPECS-LISTABLE says so, because \"cannot look\" and \"none here\"
are different answers.

PACKAGE and TAG narrow the result; TAG applies to properties only, and a tag
no loaded code mentions is reported as unresolved rather than as an empty
result, for the same reason."
  (let ((environment (environment-data api api-status)))
    (unless (eq api-status :ok)
      (return-from list-report (unavailable-report api-status environment)))
    (unless (and (stringp kind)
                 (member kind '("specs" "properties" "function-specs" "both")
                         :test #'string=))
      (return-from list-report
        (list :status :invalid-arguments
              :message (format nil "kind must be one of specs, properties, ~
function-specs or both; got ~S" kind)
              :environment environment)))
    ;; Gated on what THIS kind needs.  The blanket check predates
    ;; kind="function-specs", which reads neither of these two, and refused a
    ;; listing it could have produced -- while reporting
    ;; function_specs_listable true three keys later.
    ;; Refused only when NOTHING the kind asks for can be listed.  "both" is
    ;; the default kind, and failing it outright on a revision that can still
    ;; enumerate contracts reproduced the "nothing can be enumerated" answer
    ;; this gate was narrowed to stop giving.  A half it cannot list is
    ;; reported as its own null count, the way FUNCTION-SPECS-LISTABLE already
    ;; reports the third half.
    (let* ((available (remove-if-not
                       (lambda (row)
                         (every (lambda (key) (api-has-p api key)) (fourth row)))
                       +listing-kinds+))
           (asked (remove-if-not (lambda (row) (listing-kind-wanted-p row kind))
                                 +listing-kinds+))
           (reachable (intersection asked available :test #'eq)))
      (unless reachable
        (return-from list-report
          (list :status :unsupported
                ;; Built here rather than above: it is used only on this
                ;; branch, and formatting a message for every successful
                ;; listing is work inside the caller's introspection deadline.
                :message (%listing-unsupported-message
                          (mapcar
                           (lambda (row)
                             (format nil "~{~A~^ with ~}"
                                     (mapcar (lambda (key)
                                               (string-downcase (symbol-name key)))
                                             (fourth row))))
                           (set-difference asked reachable :test #'eq)))
                :environment environment)))
      (multiple-value-bind (package-object package-error)
          (%listing-package-filter package)
        (when package-error
          (return-from list-report
            (append package-error (list :environment environment))))
        (let* ((registry (funcall (api-fn api :registry)))
               (tag-keyword (and tag (find-keyword tag)))
               (want-specs (assoc "specs" asked :test #'string=))
               (want-properties (assoc "properties" asked :test #'string=))
               (want-function-specs (assoc "function-specs" asked :test #'string=))
               ;; Listed only when cl-spec can enumerate them.  Absence is
               ;; reported as its own answer below rather than as an empty
               ;; list: "this revision cannot enumerate contracts" and "there
               ;; are none" are different, and only one of them is a fact
               ;; about the project.
               ;; Off AVAILABLE, not REACHABLE.  REACHABLE is filtered to the
               ;; kind this call asked for, and these three flags answer a
               ;; question about the loaded revision: "can it enumerate this
               ;; half at all".  Read off the filtered list, kind=function-specs
               ;; reported that properties cannot be listed -- a client that
               ;; asks contracts first would conclude the revision has no
               ;; property listing and never ask again.
               (function-specs-listable (assoc "function-specs" available
                                               :test #'string=))
               (function-spec-names
                 (when (and want-function-specs function-specs-listable)
                   (remove-if-not
                    (lambda (name) (%in-package-p name package-object))
                    (funcall (api-fn api :list-function-specs) registry))))
               ;; Each half asks only for the reader it uses.  A revision
               ;; missing one of them still lists the others, and the half it
               ;; could not look at comes back as a null count beside a false
               ;; listable flag -- never as an empty list, which would say the
               ;; registry holds none.
               (specs-listable (assoc "specs" available :test #'string=))
               (properties-listable (assoc "properties" available :test #'string=))
               (spec-names
                 (when (and want-specs specs-listable)
                   (remove-if-not (lambda (name) (%in-package-p name package-object))
                                  (funcall (api-fn api :list-specs) registry))))
               ;; A tag needs a reader of its own, and a revision without one
               ;; cannot answer the question at all.  Falling through to an
               ;; empty list published "no property carries this tag" on the
               ;; evidence of a missing function -- the conflation the three
               ;; listable flags exist to prevent, for the one filter that had
               ;; no flag.
               (tag-filterable (or (null tag)
                                   (api-has-p api :properties-with-tag)))
               (property-names
                 (when (and want-properties properties-listable tag-filterable)
                   (remove-if-not
                    (lambda (name) (%in-package-p name package-object))
                    (cond
                      ((null tag) (funcall (api-fn api :list-properties) registry))
                      ((null tag-keyword) '())
                      (t (funcall (api-fn api :properties-with-tag)
                                  tag-keyword registry)))))))
          (list :status :ok
                :kind kind
                :specs (mapcar #'symbol-data (%take spec-names limit))
                :properties (loop for name in (%take property-names limit)
                                  collect (%property-listing api name registry))
                :function-specs (loop for name in (%take function-spec-names limit)
                                      collect (%function-spec-listing api name registry))
                :function-specs-listable (and function-specs-listable t)
                :specs-listable (and specs-listable t)
                :properties-listable (and properties-listable t)
                :tag-filterable (and tag-filterable t)
                ;; NIL, not 0, for a kind that was not asked for.  The count
                ;; is a fact about the registry and the list is what this
                ;; response carries; not looking leaves the first unknown, and
                ;; reporting it as zero says the registry holds none -- the
                ;; same conflation the tag and no-properties answers go out of
                ;; their way to avoid.
                :counts (list :specs (when (and want-specs specs-listable)
                                       (length spec-names))
                              :properties (when (and want-properties
                                                     properties-listable
                                                     tag-filterable)
                                            (length property-names))
                              :function-specs (when (and want-function-specs
                                                         function-specs-listable)
                                                (length function-spec-names)))
                :truncated (or (> (length spec-names) limit)
                               (> (length property-names) limit)
                               (> (length function-spec-names) limit))
                :limit limit
                :filters (list :package (when package-object
                                          (package-name package-object))
                               :tag tag
                               ;; A tag that names no keyword in this image
                               ;; cannot be carried by any registered property,
                               ;; so an empty result is correct -- but saying
                               ;; only "empty" would read as "no property has
                               ;; it" rather than "no such tag exists here".
                               :tag-resolved (cond ((null tag) :not-requested)
                                                   (tag-keyword t)
                                                   (t nil))
                               ;; Whether the tag narrowed anything at all.  The
                               ;; text said so and the payload did not, so a
                               ;; JSON consumer reading filters.tag beside
                               ;; tag_filterable concluded the listing had been
                               ;; filtered -- the reading SELECTION.CONTRACT_
                               ;; NOT_RUN was made data to avoid.
                               :tag-applied (and tag
                                                 want-properties
                                                 properties-listable
                                                 tag-filterable
                                                 t))
                :coverage +listing-coverage-note+
                :environment environment))))))

(defparameter +function-spec-unsupported-message+
  (concatenate 'string
               "the cl-spec loaded here does not export function-spec-data, "
               "so there is nothing to project. cl-mcp will not assemble one "
               "out of the individual readers: that would duplicate cl-spec's "
               "introspection responsibility on this side of the boundary. "
               "This is a statement about the loaded revision, not about "
               "whether a contract is registered for the symbol.")
  "Said when spec-describe is asked for a function spec cl-spec cannot project.")

(defun %contract-unsupported-message (api)
  "Return the message for a contract the loaded cl-spec cannot run.

Names the handle that is actually absent.  A single sentence blaming
CHECK-FUNCTION was wrong for the revision that has it and lacks
FUNCTION-SPEC-DATA -- and its fallback, \"read it with spec-describe\", is the
one operation that revision cannot do either.  The whole point of the wording
is that it is a statement about the loaded revision, so it has to name the
right fact about it."
  (let ((missing (remove nil
                         (list (unless (api-has-p api :check-function)
                                 "check-function")
                               (unless (api-has-p api :function-spec-data)
                                 "function-spec-data")))))
    (format nil "the cl-spec loaded here does not export ~{~A~^ or ~}, so a ~
contract cannot be executed.~@[ ~A~]"
            missing
            (when (api-has-p api :function-spec-data)
              (concatenate 'string
                           "Its text can still be read with spec-describe "
                           "kind=function-spec.")))))

(defun %contract-note (api &key not-run)
  "Return the note about a contract registered for a symbol, matched to the API.

NOT-RUN adds what a spec-check :ABOUT selection has to say for itself: the
contract exists and this run did not cover it.

One builder rather than two near-copies.  Every qualifier here comes off the
same pair of API answers, and every message this module prints is meant to be
a true statement about the loaded revision -- an instruction is a statement
too, so sending a caller to spec-describe kind=function-spec on a cl-spec with
no FUNCTION-SPEC-DATA is a dead end issued by the one place that can see it is
one.  Written twice, a wording or capability fix reached one copy and not the
other."
  (let ((readable (api-has-p api :function-spec-data))
        (runnable (api-has-p api :check-function)))
    (concatenate
     'string
     (if not-run
         (concatenate 'string
                      "a function spec is registered for this symbol and was "
                      "NOT run: an :about selection covers properties only. ")
         "a function spec is registered for this symbol. ")
     (cond
       ((and readable runnable)
        (concatenate 'string
                     "Read it with spec-describe kind=function-spec, run it "
                     "with spec-check function=<this symbol>. It says which "
                     "inputs the function accepts and which output it must "
                     "return, which the properties about it do not."))
       (readable
        (concatenate 'string
                     "Read it with spec-describe kind=function-spec. The "
                     "loaded cl-spec cannot run it -- it exports no "
                     "check-function."))
       (t
        (concatenate 'string
                     "The loaded cl-spec exports no function-spec-data, so "
                     "this adapter can neither project nor run it."))))))

(defun %spec-tree (spec-plist)
  "Return SPEC-PLIST with its symbols externalized, or NIL when there is none.

Kept as a projection of cl-spec's own SPEC-DATA rather than a re-derivation:
every key here comes straight across, and nothing is computed on this side.

The guard tests for a non-empty list, not merely for a list.  LISTP is true of
NIL, so an argument with no spec used to render as a full node of nulls whose
source_form was the literal string \"NIL\" -- a spec nobody wrote, described
to the caller as one that exists."
  (when (and spec-plist (listp spec-plist))
    (list :kind (getf spec-plist :kind)
          :name (let ((name (getf spec-plist :name)))
                  (when name (symbol-data name)))
          :target (let ((target (getf spec-plist :target)))
                    (when target (symbol-data target)))
          :type (let ((type (getf spec-plist :type)))
                  (when type (printed-for-display type)))
          :predicate (let ((predicate (getf spec-plist :predicate)))
                       (when predicate (printed-for-display predicate)))
          :values (let ((values (getf spec-plist :values)))
                    (when values (printed-for-display values)))
          :base-type (let ((base (getf spec-plist :base-type)))
                       (when base (printed-for-display base)))
          :min (%range-bound (getf spec-plist :min) (getf spec-plist :kind))
          :max (%range-bound (getf spec-plist :max) (getf spec-plist :kind))
          :class-name (let ((name (getf spec-plist :class-name)))
                        (when name (symbol-data name)))
          :source-form (printed-for-display (getf spec-plist :source-form))
          :source-location (getf spec-plist :source-location)
          :children (mapcar #'%spec-tree (getf spec-plist :children)))))

(defun %range-bound (value kind)
  "Return a range end as text, or NIL when the node has no such end.

cl-spec spells an open end :UNBOUNDED.  Mapped back to the * the author wrote,
here rather than in a renderer, so the text and the JSON agree and neither has
to know the IR's word for it.

On a RANGE node an absent end is an open end, so NIL maps to * as well.  A
revision that spells one that way -- and this adapter exists to tolerate
revisions -- would otherwise give (range integer 0 *) one printable bound, and
a renderer showing only the pair it could complete would drop the 0 with it:
a wider input domain than the author wrote, silently.  On any other node there
is no end to report and NIL stays NIL."
  (cond ((eq :unbounded value) "*")
        (value (printed-for-display value))
        ((eq :range kind) "*")))

(defun %describe-function-spec (api name registry max-chars)
  "Return the detail plist for the contract registered for NAME.

Built from cl-spec's own FUNCTION-SPEC-DATA, for the same reason the spec and
property projections are: the two halves a caller asks about before editing a
function -- which inputs are accepted, which output is required -- are
cl-spec's answer to give, not this adapter's to assemble.

:PRE and :POST are the author's forms.  Their compiled counterparts are not
projected: a function cannot be read, and whether they hold is what spec-check
answers."
  (let ((data (funcall (api-fn api :function-spec-data) name :registry registry)))
    ;; NIL is not a contract with no arguments and no :returns.  It renders
    ;; byte-identically to one, which is the reading %FUNCTION-SPEC-LISTING
    ;; carries :READ-FAILED to prevent -- and this is the path where a caller
    ;; actually reads the contract before editing the function.
    (unless data
      (return-from %describe-function-spec
        (list :status :unsupported
              :kind "function-spec"
              :name (symbol-data name)
              :message
              (concatenate 'string
                           "cl-spec returned no projection for this contract. "
                           "It is registered; what it says could not be read, "
                           "and an empty description would read as a contract "
                           "with no arguments and no :returns."))))
    (flet ((clause (forms)
             ;; Bounded like the body a property describe carries.  A :PRE or
             ;; :POST form is short in practice, but "in practice" is not a
             ;; budget, and every other form this module prints is cut at one.
             ;; NIL rather than the string "NIL" for an absent clause, so a
             ;; renderer can tell a contract with no :PRE from one whose :PRE
             ;; is the literal NIL.
             ;; What cl-spec evaluates, not the list it stores.
             ;; :PRECONDITIONS is a list of forms -- (:pre (<= low high))
             ;; arrives as ((<= low high)) -- and DEFSPEC-FUNCTION compiles
             ;; them as (and ,@pre), so one clause prints as the clause and
             ;; several print as that AND.  Printing the bare list gave the
             ;; reader a form they cannot paste back: a call to the list.
             (when forms
               (multiple-value-bind (text complete omitted)
                   (%print-bounded-form (if (null (rest forms))
                                            (first forms)
                                            (cons 'and forms))
                                        max-chars)
                 (list text complete omitted)))))
      (let ((pre (clause (getf data :preconditions)))
            (post (clause (getf data :postconditions))))
        (multiple-value-bind (source source-complete source-omitted)
            (%print-bounded-form (getf data :source-form) max-chars)
          (list :status :ok
                :kind "function-spec"
                :name (symbol-data name)
                :documentation (getf data :documentation)
                :arguments (loop for argument in (getf data :arguments)
                                 collect (list :variable
                                               (symbol-data (getf argument :variable))
                                               :spec (%spec-tree (getf argument :spec))))
                :returns (%spec-tree (getf data :returns))
                :preconditions (first pre)
                ;; Only when there is a clause.  "complete: true" about a
                ;; :PRE the contract does not have is a claim, and the
                ;; response has %OPTIONAL-BOOL to carry an absent flag as
                ;; null -- which is what this PR added it for.
                :preconditions-complete (if pre (second pre) :not-applicable)
                :preconditions-omitted-chars (third pre)
                :postconditions (first post)
                :postconditions-complete (if post (second post) :not-applicable)
                :postconditions-omitted-chars (third post)
                :source-form source
                :source-form-complete source-complete
                :source-form-omitted-chars source-omitted
                :source-location (getf data :source-location)
                :definition-digest (definition-digest api name registry
                                                      :property data)))))))

(defun %describe-property (api name registry max-chars)
  "Return the detail plist for property NAME."
  (let ((data (funcall (api-fn api :property-data) name :registry registry)))
    (multiple-value-bind (body complete omitted)
        (%print-bounded-form (getf data :body) max-chars)
      (multiple-value-bind (source source-complete source-omitted)
          (%print-bounded-form (getf data :source-form) max-chars)
        (list :status :ok
              :kind "property"
              :name (symbol-data name)
              :property-kind (getf data :kind)
              :tags (getf data :tags)
              :targets (mapcar #'symbol-data (getf data :targets))
              :documentation (getf data :documentation)
              :trials-table (let ((table (getf data :trials)))
                              (when table (printed-for-display table)))
              :shrink-enabled (getf (getf data :metadata) :shrink)
              :arguments (loop for argument in (getf data :arguments)
                               collect (list :variable
                                             (symbol-data (getf argument :variable))
                                             :spec (%spec-tree (getf argument :spec))))
              :body body
              :body-complete complete
              :body-omitted-chars omitted
              :source-form source
              :source-form-complete source-complete
              :source-form-omitted-chars source-omitted
              :source-location (getf data :source-location)
              ;; The plist this function already read.  Without it the digest
              ;; reads PROPERTY-DATA a second time and re-normalizes every
              ;; argument spec -- the 2N that :PROPERTY exists to avoid, in
              ;; the one describe path that was not passing it.
              :definition-digest (definition-digest api name registry
                                                    :property data))))))

(defun %describe-spec (api name registry max-chars)
  "Return the detail plist for spec NAME."
  (let ((data (funcall (api-fn api :spec-data) name :registry registry)))
    (multiple-value-bind (source complete omitted)
        (%print-bounded-form (getf data :source-form) max-chars)
      (list :status :ok
            :kind "spec"
            :name (symbol-data name)
            :spec (%spec-tree data)
            :source-form source
            :source-form-complete complete
            :source-form-omitted-chars omitted
            :source-location (getf data :source-location)))))

(defun describe-report (api api-status kind name &key package (max-chars 8000))
  "Return the plist behind the spec-describe tool.

KIND is \"property\", \"spec\" or \"function-spec\"."
  (let ((environment (environment-data api api-status)))
    (unless (eq api-status :ok)
      (return-from describe-report (unavailable-report api-status environment)))
    (unless (and (stringp kind)
                 (member kind '("property" "spec" "function-spec") :test #'string=))
      (return-from describe-report
        (list :status :invalid-arguments
              :message (format nil "kind must be one of property, spec or ~
function-spec; got ~S" kind)
              :environment environment)))
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator name :package package)
      (unless symbol
        (return-from describe-report
          (list :status :unresolved-symbol :reason reason :input name
                :environment environment)))
      (when (and (string= kind "function-spec")
                 (not (api-has-p api :function-spec-data)))
        (return-from describe-report
          (list :status :unsupported
                :name (symbol-data symbol)
                :message +function-spec-unsupported-message+
                :environment environment)))
      (handler-case
          (let ((registry (funcall (api-fn api :registry))))
            (append (cond
                      ((string= kind "property")
                       (%describe-property api symbol registry max-chars))
                      ((string= kind "function-spec")
                       (%describe-function-spec api symbol registry max-chars))
                      (t (%describe-spec api symbol registry max-chars)))
                    (list :environment environment)))
        (error (condition)
          ;; Only cl-spec's own "no such name" conditions become
          ;; :NOT-REGISTERED.  A blanket mapping reported every internal
          ;; failure -- a bad argument, a printer error, a malformed
          ;; :ARGUMENTS entry -- as the absence of a registration, which is
          ;; exactly the false negative this file exists to prevent.
          (list :status (if (%unknown-registration-p api condition)
                            :not-registered
                            :internal-error)
                :kind kind
                :name (symbol-data symbol)
                :message (princ-to-string condition)
                :environment environment))))))

;;; ---------------------------------------------------------------------------
;;; spec-check
;;; ---------------------------------------------------------------------------

(defvar *default-check-timeout-seconds* 60
  "Whole-call budget a spec-check uses when the caller names none.

Whole-call rather than per-property because the proxy reads the same
timeout_seconds to decide how long to wait for the worker's answer
(SRC/PROXY.LISP, %EFFECTIVE-RPC-TIMEOUT).  A per-property figure would let a
selection of five properties outlive the proxy's patience, and a proxy timeout
is not a timeout report: it kills the worker and resets the session.")

(defparameter *minimum-run-budget-seconds* 0.05
  "Budget below which a property is reported as not run rather than started.

Starting a run with a few milliseconds left produces a timeout that says
nothing about the property, and costs a thread to say it.")

(defparameter +backend-missing-message+
  (concatenate 'string
               "No cl-spec generator backend is installed, so no property can "
               "be executed. Run load-system with system "
               "\"cl-spec/check-it\". Nothing was executed: this is NOT a "
               "successful verification.")
  "Said when execution is requested with *GENERATOR-BACKEND* unset.")

(defparameter +about-coverage-note+
  (concatenate 'string
               "Direct (:about ...) registrations only. Callers, "
               "generic-function methods, macro users and shared mutable "
               "state are NOT analysed. This is not a change impact analysis "
               "(cl-spec specification 31 and 72.5).")
  "The limit of what the :about reverse index can be said to cover.")

(defparameter +explicit-coverage-note+
  (concatenate 'string
               "Only the property named. Nothing else was selected, and "
               "nothing else was checked.")
  "The limit of what an explicit single-property run covers.")

(defparameter +shrink-note+
  (concatenate 'string
               "Backend-searched reduction. NOT a guaranteed global minimum, "
               "and the backend does not report whether shrinking completed, "
               "exhausted its budget or was interrupted (cl-spec "
               "specification 16 and 72.4).")
  "What a shrunk counterexample does and does not mean.")

(defparameter +reproduce-scope-note+
  (concatenate 'string
               "Regenerates the trial sequence from this seed under the same "
               "definitions, backend, profile and image. It does NOT "
               "reproduce the code revision, external I/O, the clock, or "
               "shared mutable state. This is regeneration, not replay of a "
               "saved counterexample against a fixed implementation (cl-spec "
               "specification 15 and 72.3).")
  "What a seed does and does not fix.")

(defparameter +zero-properties-message+
  (concatenate 'string
               "0 properties selected -- this is NOT a successful "
               "verification. Nothing was executed, and a registry with no "
               "property registered about this symbol says nothing about "
               "whether it is correct.")
  "Said when a selection comes back empty.")

(defparameter +options-note+
  (concatenate 'string
               "cl-mcp passes no backend options, so a re-run under the same "
               "seed and profile cannot silently differ in them.")
  "Why the options field is always null.")

(defparameter +budget-exhausted-message+
  (concatenate 'string
               "the whole-call timeout_seconds budget was spent before this "
               "property started; nothing about it was checked")
  "Said for a property the budget never reached.")

(defparameter +timeout-leaked-message+
  (concatenate 'string
               "the property run exceeded its deadline and could not be "
               "stopped: it is still executing in this worker and may hold "
               "locks. Use pool-kill-worker to get a fresh worker before "
               "retrying.")
  "Said for a timeout whose run thread outlived every attempt to stop it.")

(defparameter +timeout-stopped-message+
  (concatenate 'string
               "the property run exceeded its deadline and its run thread was "
               "stopped. Nothing was proved or disproved; retry with a larger "
               "timeout_seconds if the property legitimately needs longer.")
  "Said for a timeout whose run thread was stopped cleanly.")

(defparameter +worker-reuse-unknown-message+
  (concatenate 'string
               "A property run was stopped at its deadline. Its thread is "
               "gone, but nothing here can show that the state it was "
               "changing was restored: cl-spec runs no cleanup this adapter "
               "can observe, and an unwound property may have left shared "
               "state part-way. Treat this image as unknown for later "
               "verification (cl-spec specification 72.4). With a worker "
               "pool, use pool-kill-worker before trusting a later result in "
               "this session; running inline (MCP_NO_WORKER_POOL), restart "
               "the process.")
  "Said after any timeout whose thread was stopped.")

(defparameter +worker-reuse-unsafe-message+
  (concatenate 'string
               "A property run could not be stopped and is still executing "
               "in this image, holding whatever locks it had. Do not reuse "
               "it: with a worker pool, use pool-kill-worker; running inline "
               "(MCP_NO_WORKER_POOL), restart the process.")
  "Said when a run thread outlived every attempt to stop it.")

(defparameter +seed-needs-one-property-message+
  (concatenate 'string
               "seed reproduces a single property run; the selection holds "
               "more than one property. Name one with property= instead.")
  "Said when a seed arrives alongside a multi-property selection.")

(defun %resolve-profile (profile)
  "Return (values KEYWORD NIL) for the profile named by PROFILE.

PROFILE arrives as text from outside the image, so the keyword is looked up
rather than interned.  A profile no loaded code mentions cannot be interned
either, which means no property's :TRIALS table can name it -- running under
it would silently fall back to the backend default while the response claimed
the requested profile.  Refusing is the honest answer."
  (let* ((name (string-upcase (or profile "normal")))
         (keyword (find-symbol name "KEYWORD")))
    (if keyword
        (values keyword nil)
        (values nil
                (format nil
                        "profile ~S names no keyword present in this image, so ~
no registered property can select a trial count for it. Use a profile that ~
appears in the property's trials table (see spec-describe)."
                        profile)))))

(defun %registered-p (api data-key name registry)
  "Return (values FOUND-P MESSAGE DATA STATUS) for NAME under DATA-KEY.

DATA-KEY is :PROPERTY-DATA or :FUNCTION-SPEC-DATA.  Every error is read as
\"not registered\", which is true of cl-spec's unknown-name conditions and an
approximation for anything else it might signal; the message is carried so the
approximation is at least visible.

DATA is the projection the probe already paid for.  Handed back rather than
thrown away: the caller reads the same definition again a few frames later,
and FUNCTION-SPEC-DATA normalizes every argument spec and the return spec on
each call.  Same reason DEFINITION-DIGEST takes a :PROPERTY."
  (handler-case
      (values t nil (funcall (api-fn api data-key) name :registry registry))
    ;; CL:UNDEFINED-FUNCTION gets its own status rather than being read as
    ;; "not registered".  cl-spec raises it on purpose for a contract whose
    ;; target has not been written yet -- the contract IS registered, and the
    ;; run path already answers :UNDEFINED-FUNCTION for the same condition.
    ;; Answering not-registered here made the two paths disagree about one
    ;; fact, and told the caller to register something they already had.
    (undefined-function (condition)
      (values nil (princ-to-string condition) nil :undefined-function))
    (error (condition)
      (values nil (princ-to-string condition) nil :not-registered))))

(defun %select-named (api designator package registry
                      &key data-key mode requested-key source coverage)
  "Return (values NAMES SELECTION ERROR DATA) for one explicitly named definition.

Serves both explicit selections -- a property by name and a contract by name.
They resolve, check and describe identically, and differ only in the reader
they ask and the five literals they put in the selection plist; kept apart,
each fix to one had to be remembered for the other.

DATA is the definition the registration check already read, passed on so the
facts derived from it next need not read it again.  It belongs to the single
name in NAMES and to no other."
  (multiple-value-bind (name reason)
      (resolve-symbol-designator designator :package package)
    (if (null name)
        (values nil nil (list :status :unresolved-symbol :reason reason
                              :input designator))
        (multiple-value-bind (found-p message data status)
            (%registered-p api data-key name registry)
          (if (not found-p)
              (values nil nil (list :status (or status :not-registered)
                                    :name (symbol-data name)
                                    :message message))
              (values (list name)
                      (list :mode mode
                            ;; The keyword, beside the display string.  A
                            ;; renderer that decides "contract or property"
                            ;; by matching MODE's text is one typo from
                            ;; calling a contract a property while every
                            ;; other line still treats it as a contract --
                            ;; which is how a fixture written with mode
                            ;; "function" hid that branch from the suite.
                            :kind (if (eq requested-key :function)
                                      :contract
                                      :property)
                            :requested (list requested-key (symbol-data name))
                            :selected (list (symbol-data name))
                            :count 1
                            :source source
                            :coverage coverage)
                      nil
                      data))))))

(defun %select-about (api symbol package registry)
  "Return (values NAMES SELECTION ERROR) for an :ABOUT reverse-index request."
  (multiple-value-bind (name reason)
      (resolve-symbol-designator symbol :package package)
    (if (null name)
        (values nil nil (list :status :unresolved-symbol :reason reason
                              :input symbol))
        (let* ((routing (handler-case
                            (funcall (api-fn api :semantic-data) name
                                     :registry registry)
                          (error (condition)
                            (return-from %select-about
                              (values nil nil
                                      (list :status :internal-error
                                            :name (symbol-data name)
                                            :message
                                            (format nil "cl-spec signalled ~
while listing the properties about this symbol: ~A" condition)))))))
               (about (getf routing :properties-about)))
          (values about
                  (list :mode "about"
                        ;; The keyword every consumer of this plist reads --
                        ;; %SELECTION-NOUN among them, which would otherwise
                        ;; reach the property branch by falling through rather
                        ;; than by being told.
                        :kind :property
                        :requested (list :symbol (symbol-data name))
                        :selected (mapcar #'symbol-data about)
                        :count (length about)
                        :source +about-source+
                        :coverage +about-coverage-note+
                        ;; Carried as a name, not only as prose in a note, so
                        ;; the headline can say it and a JSON consumer can act
                        ;; on it.  A note several lines under a bare "VERIFIED"
                        ;; is read after the reader has already stopped.
                        :contract-not-run (let ((contract (getf routing :function-spec)))
                                            (when contract (symbol-data contract)))
                        :notes (append
                                (when (getf routing :property)
                                  (list +symbol-is-a-property-not-selected-note+))
                                (when (getf routing :function-spec)
                                  (list (%contract-note api :not-run t)))))
                  nil)))))

(defparameter +trials-needs-a-contract-message+
  (concatenate 'string
               "trials applies to a contract run (function=...) only. A "
               "property's trial count comes from its own :trials table, "
               "selected by profile; cl-spec's run-property takes no override, "
               "so honouring trials here would report a budget the run did not "
               "use.")
  "Said when trials is given for a property selection.")

(defparameter +profile-needs-a-property-message+
  (concatenate 'string
               "profile applies to a property run only. A contract has no "
               ":trials table for a profile to select from, and cl-spec's "
               "check-function takes no profile, so honouring it here would "
               "report a profile the run did not use -- the mirror of why "
               "trials is refused with property=. Size a contract run with "
               "trials= instead.")
  "Said when profile is given for a contract selection.")

(defun %target-argument-error (property symbol function trials profile)
  "Return the plist for a bad target selection, or NIL when it is fine.

Checked before cl-spec is consulted.  Naming more than one or none of them is
a mistake in the call itself, and answering \"cl-spec is not loaded\" would
send the caller to fix the wrong thing -- the same reason SPEC-ENTRY validates
the seed before it resolves the API.

TRIALS and PROFILE sit here for that same reason, and as a pair: each sizes
one kind of run and neither reaches the other, so accepting either against the
wrong target would publish a budget or a profile the run never used.  PROFILE
is the caller's own word, NIL when none was given, so a contract run is
refused only for a profile that was actually asked for."
  (let ((given (count-if-not #'null (list property symbol function))))
    (cond
      ((> given 1)
       (list :status :invalid-arguments
             :message "give exactly one of property, symbol or function"))
      ((zerop given)
       (list :status :invalid-arguments
             :message "give one of property, symbol or function"))
      ((and trials (not function))
       (list :status :invalid-arguments
             :message +trials-needs-a-contract-message+))
      ((and profile function)
       (list :status :invalid-arguments
             :message +profile-needs-a-property-message+)))))

(defun %properties-about (api name registry)
  "Return the properties registered (:about NAME), as symbol plists.

Read for a contract selection so the response can say which properties it left
alone.  The mirror of what an :ABOUT selection reports about the contract it
did not run: both are coverage a caller would otherwise have to infer from
prose, and the argument for naming one is the argument for naming the other."
  (handler-case
      (let ((routing (funcall (api-fn api :semantic-data) name
                              :registry registry)))
        ;; :PROPERTY as well as :PROPERTIES-ABOUT.  A symbol can be both a
        ;; contract's subject and a property in its own name, and the :ABOUT
        ;; path reports that direction explicitly -- dropping it here let a
        ;; contract run answer "nothing else was left unrun" about a property
        ;; of the same name, off a routing table already in hand.
        (values (append (let ((own (getf routing :property)))
                          (when own (list (symbol-data own))))
                        (mapcar #'symbol-data (getf routing :properties-about)))
                t))
    ;; (values NIL NIL) rather than NIL: an empty list here says the symbol has
    ;; no properties registered about it, and a read that failed has no
    ;; evidence for that.  Reported as its own fact, the way every sibling in
    ;; this module is -- FUNCTION-SPECS-LISTABLE, REJECTED-MEASURED,
    ;; FAILURE-REASON-READABLE, HAS-PRECONDITION.
    (error () (values nil nil))))

(defun %select-properties (api property symbol function package registry)
  "Return (values NAMES SELECTION ERROR KIND DATA) for the requested selection.

DATA is the definition an explicit selection already read, or NIL: an :ABOUT
selection names many and reads none of them here.

Exactly one of PROPERTY, SYMBOL and FUNCTION is supplied; %TARGET-ARGUMENT-ERROR
has already rejected the other shapes.  KIND is :PROPERTY or :CONTRACT and says
which runner the names go to.

A symbol that is itself a registered property is deliberately not added to an
:ABOUT selection, and neither is a contract registered for it: those are
different relationships, and quietly widening what gets executed would make the
reported coverage wrong.  The contract is named in a note instead, so a caller
learns it exists rather than being left to assume the run covered it."
  (cond
    (function
     (multiple-value-bind (names selection error data)
         (%select-named api function package registry
                        :data-key :function-spec-data
                        :mode "contract"
                        :requested-key :function
                        :source "explicit function argument"
                        :coverage +contract-coverage-note+)
       (values names
               (if names
                   (multiple-value-bind (about read)
                       (%properties-about api (first names) registry)
                     (append selection
                             (list :properties-not-run about
                                   :properties-not-run-read (and read t))
                             (cond
                               (about
                                (list :notes
                                      (list (format nil "~D propert~:@P ~
registered (:about this symbol) ~:*~[~;is~:;are~] NOT covered by a contract ~
run: run them with spec-check symbol=<this symbol>."
                                                    (length about)))))
                               ((not read)
                                (list :notes
                                      (list (concatenate 'string
                                                         "whether any property "
                                                         "is registered about "
                                                         "this symbol could "
                                                         "not be read, so "
                                                         "this run's coverage "
                                                         "is unknown beyond "
                                                         "the contract")))))))
                   selection)
               error :contract data)))
    (property
     (multiple-value-bind (names selection error data)
         (%select-named api property package registry
                        :data-key :property-data
                        :mode "explicit"
                        :requested-key :property
                        :source "explicit property argument"
                        :coverage +explicit-coverage-note+)
       (values names selection error :property data)))
    (t
     (multiple-value-bind (names selection error)
         (%select-about api symbol package registry)
       (values names selection error :property)))))

(defun %property-facts (api name registry &optional pre-read)
  "Return the facts about property NAME a result needs to describe itself.

PRE-READ is the definition a caller has already projected, used instead of
reading it a second time.

  (:argument-count <integer-or-nil> :shrink-enabled <boolean>
   :trials-table <plist-or-nil> :known <boolean>)

Fetched once per property and threaded through, rather than read again from
PROPERTY-DATA at each point that needs one of them.  ARGUMENT-COUNT is what
separates \"this property generates no arguments, so its counterexample is
legitimately empty\" from \"no counterexample was obtained\" -- a distinction
an empty list on its own cannot carry.  :KNOWN is false when PROPERTY-DATA
could not be read at all, so a consumer is not told zero arguments when the
truth is that nothing was read."
  (handler-case
      (let ((data (or pre-read
                      (funcall (api-fn api :property-data) name
                               :registry registry))))
        (list :argument-count (length (getf data :arguments))
              :kind :property
              :shrink-enabled (and (getf (getf data :metadata) :shrink) t)
              :trials-table (getf data :trials)
              ;; Carried so the digest beside it does not fetch the same
              ;; plist again.
              :data data
              :known t))
    (error ()
      (list :argument-count nil :kind :property :shrink-enabled nil
            :trials-table nil :data nil :known nil))))

(define-condition unreadable-projection (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "cl-spec returned no projection for this name.")))
  (:documentation "Signalled when a cl-spec reader answers NIL for a name.

Caught by the facts readers' own handler-case, which is what turns it into
:KNOWN NIL -- the state that says nothing about this definition was read,
rather than a set of answers derived from an empty plist."))

(defun %contract-facts (api name registry &optional pre-read)
  "Return the facts about the contract for NAME a result needs.

PRE-READ is the definition a caller has already projected, used instead of
reading it a second time.

The same shape %PROPERTY-FACTS returns, so everything downstream reads one
plist.  A contract has no :TRIALS table -- cl-spec's CHECK-FUNCTION takes a
count, not a profile -- and shrinking is always on, so those two are constants
here rather than things read off a definition."
  (handler-case
      (let ((data (or pre-read
                      (funcall (api-fn api :function-spec-data) name
                               :registry registry))))
        ;; A projection that came back NIL is not a contract with no arguments
        ;; and no :pre.  %DESCRIBE-FUNCTION-SPEC refuses that inference and
        ;; %FUNCTION-SPEC-LISTING carries :READ-FAILED for it; read here as a
        ;; real answer it produced four positive claims -- no :pre, every
        ;; input passed, a usable refusal count, an effective trial count --
        ;; and a verified verdict resting on them.
        (unless data (error 'unreadable-projection))
        (list :argument-count (length (getf data :arguments))
              :kind :contract
              :shrink-enabled t
              :trials-table nil
              ;; :UNKNOWN in the error branch, not NIL.  A contract with no
              ;; :PRE refuses nothing, and saying so is different from having
              ;; failed to read whether it has one.
              :precondition-p (and (getf data :preconditions) t)
              :data data
              :known t))
    (error ()
      (list :argument-count nil :kind :contract :shrink-enabled t
            :trials-table nil :precondition-p :unknown :data nil :known nil))))

(defun %digest-facts (api name registry facts)
  "Return (:value <string-or-nil> :complete <boolean>) for NAME's digest.

Carried as one plist rather than two arguments because the pair travels
together everywhere: a digest whose input was truncated is not a digest a
caller may compare, and separating them invites reporting the value without
the caveat."
  (multiple-value-bind (value complete)
      ;; The fallback reads the definition itself, so it has to ask the reader
      ;; for the kind being run.  A symbol carrying both a property and a
      ;; contract would otherwise have the property's digest stamped on the
      ;; contract's result -- the one field whose job is to say the definition
      ;; behind this run did not move.
      (let ((data-key (if (eq :contract (getf facts :kind))
                          :function-spec-data
                          :property-data)))
        (if (getf facts :known)
            (definition-digest api name registry :property (getf facts :data)
                                                 :data-key data-key)
            (definition-digest api name registry :data-key data-key)))
    (list :value value :complete (and value complete t))))

(defun %trials-budget (api facts profile backend &optional requested)
  "Return the trial budget plist for a property described by FACTS under PROFILE.

REQUESTED, when given, is the caller's explicit trial count for a contract run.
A contract has no :TRIALS table for a profile to select from, so without this
the only reachable budget is the backend's default -- and a contract whose
failing region is a boundary needs more trials than that to reach it.

cl-spec resolves this internally in RESOLVE-TRIALS and neither exports that
function nor records the figure on a PROPERTY-RESULT, so it is derived here
from the two exported readers.  BUDGET-DERIVATION says so: a number a consumer
cannot trace back is worse than one it can question.

BACKEND is passed in rather than read here, and is the same object the run
will be given.  Reading *GENERATOR-BACKEND* separately would let the budget be
computed from one backend and the run executed under another."
  (let* ((table (getf facts :trials-table))
         (from-profile (and table (getf table profile)))
         (default (and backend
                       (handler-case
                           (funcall (api-fn api :backend-default-trials) backend)
                         (error () nil)))))
    (list :budget (or requested from-profile default)
          :budget-source (cond (requested "requested")
                               (from-profile "property-profile")
                               (default "backend-default")
                               (t "unknown"))
          :property-trials (when table (printed-for-display table))
          :backend-default default
          :budget-derivation +budget-derivation-note+)))

(defun %classify-condition (api condition)
  "Return the adapter status keyword for CONDITION.

The condition classes are looked up on the API rather than named here, so an
image whose cl-spec predates one of them degrades to a coarser status instead
of failing to load.

:INTERNAL-ERROR is the last resort and is documented as \"this adapter
failed\", so anything cl-spec raises on purpose has to be recognized before it.
A contract for a function nobody has written yet is the case that matters:
cl-spec's FUNCTION-SPEC-TARGET signals CL:UNDEFINED-FUNCTION deliberately, so
that a project can adopt cl-spec one function at a time, and reading that as
an adapter fault tells the caller cl-mcp is broken when the fact is that they
have not written the function."
  (flet ((is-a (key)
           (let ((class (api-class api key)))
             (and class (typep condition class)))))
    (cond
      ((or (is-a :no-generator-backend) (is-a :generator-unavailable))
       :generator-error)
      ((is-a :unknown-property) :not-registered)
      ;; Both ahead of :CL-SPEC-ERROR, which they specialize.
      ((is-a :unknown-function-spec) :not-registered)
      ((is-a :not-implemented) :unsupported)
      ;; Not a cl-spec condition, so it cannot be asked for by class: the
      ;; named function is absent from this image, which is a fact about the
      ;; image and not about either side of the adapter boundary.
      ((typep condition 'undefined-function) :undefined-function)
      ((is-a :cl-spec-error) :backend-error)
      ;; A cl-spec that predates the condition hierarchy, or a condition from
      ;; somewhere else entirely.  The message is the only evidence available,
      ;; and it is reported rather than interpreted -- except for the one
      ;; phrase the backend-missing condition is guaranteed to carry.
      ((search "generator backend" (princ-to-string condition)) :generator-error)
      (t :internal-error))))

(defun %condition-report (condition max-chars)
  "Return CONDITION's report text, retaining at most MAX-CHARS of it.

Bounded on the way out rather than printed in full and cut afterwards: a
condition signalled by a property body can carry a generated value as its
datum, and its report is then as large as that value."
  (handler-case
      (let ((stream (make-bounded-output-stream (max 1 max-chars))))
        (princ condition stream)
        (bounded-output-string stream))
    (serious-condition () "#<unprintable condition>")))

(defun %condition-data (condition &key (max-chars 2000))
  "Return CONDITION as the plist a response carries for it.

The object id lets the existing inspect-object tool reach the condition's
slots, which is where a signalled datum a caller needs is actually kept: the
report text is a rendering, not the value."
  (list :type (princ-to-string (type-of condition))
        :message (%condition-report condition max-chars)
        :object-id (ignore-errors (register-object condition))))

(defun %named-values (plist max-value-chars)
  "Return cl-spec's {variable value} counterexample PLIST as a list of plists."
  (loop for (variable value) on plist by #'cddr
        collect (list :variable (symbol-data variable)
                      :value (externalize-value value
                                                :max-chars max-value-chars))))

(defun %contract-plist (api result executed max-value-chars
                        &optional (precondition-p :unknown))
  "Return the contract-specific half of a CHECK-FUNCTION result.

Always a plist: %RESULT-PLIST decides whether a run had a contract half and
calls this only then, and %EVALUATED-P reads a non-NIL :CONTRACT as \"this was
a contract run\" -- a NIL return here would put it back on the raw trial count
this function exists to withhold.

REJECTED is what separates a run that checked the function from one that only
generated arguments for it: cl-spec's checker refuses inputs its :PRE does not
admit, and a trial count that includes them overstates the work.  It is
reported with REJECTED-MEASURED beside it, because a cl-spec whose readers this
adapter could not resolve gives NIL, which must not read as zero rejections.

REJECTED can exceed EXECUTED, in which case the difference is withheld and the
overshoot reported rather than published as a negative count.  cl-spec stops
counting refusals at the first failure it recognizes, but a target that
SIGNALS unwinds past that point with the counter still running, and shrinking
then re-runs the predicate over candidates its :PRE refuses.  Measured at 3
runs in 8 against a contract whose function signals inside its :PRE region,
one of them reporting 1 trial and 2 rejections.

FAILURE-REASON names which half of the contract broke; EXPLANATION carries
cl-spec's structured account of a return value that missed its spec."
  (flet ((read-slot (key)
           ;; (values VALUE OK-P).  Resolving the symbol is not the same as
           ;; calling it: a reader that signals -- a result type that drifted,
           ;; an argument count that moved -- also gives NIL, and a flag
           ;; recording only that the name existed then told the caller their
           ;; function was non-deterministic on the strength of an adapter
           ;; read that failed.
           (if (api-has-p api key)
               (handler-case (values (funcall (api-fn api key) result) t)
                 (error () (values nil nil)))
               (values nil nil))))
    (multiple-value-bind (reason reason-read) (read-slot :check-failure-reason)
      (multiple-value-bind (explanation explanation-read)
          (read-slot :check-explanation)
        (let* ((rejected (read-slot :check-rejected))
               (countable (and (integerp executed) (integerp rejected)))
               (overcounted (and countable (> rejected executed)))
               ;; A refusal reported against a contract whose projection says it
               ;; has nothing to refuse with.  Two readers disagreeing, like the
               ;; overcount above, and handled the same way rather than only in
               ;; the text: a subtraction over figures that contradict each
               ;; other is not a call count, and publishing it while the text
               ;; says it cannot be derived leaves one response saying both.
               (contradicted (and countable
                                  (null precondition-p)
                                  (plusp rejected)))
               ;; One keyword for one three-valued question, decided in one
               ;; place.  Five booleans meant the renderer re-derived which
               ;; reason applied by testing them in an order that could not
               ;; change -- :UNKNOWN is not NULL, so its clause had to precede
               ;; the no-:pre one, silently -- while the gap list and the
               ;; verdict read a sixth.  The booleans below are published from
               ;; this, not computed beside it.
               (rejection-status
                 ;; COUNTABLE first, and before every clause below it: each of
                 ;; the others ends in a subtraction, and cl-spec can report a
                 ;; result whose trial count is NIL -- its own check-function
                 ;; writes (- (or (property-result-trials result) 0) rejected)
                 ;; for that reason.  Ordered after :NO-PRECONDITION, a contract
                 ;; without a :pre reached (- NIL 0) and the TYPE-ERROR was
                 ;; reported to the caller as "this adapter failed".
                 (cond ((not countable) :unmeasured)
                       (overcounted :overcounted)
                       ((eq :unknown precondition-p) :precondition-unknown)
                       (contradicted :contradicted)
                       ((null precondition-p) :no-precondition)
                       (t :usable)))
               (usable (member rejection-status '(:usable :no-precondition))))
          (multiple-value-bind (explanation-text explanation-complete
                                explanation-omitted)
              (if explanation
                  ;; %PRINT-BOUNDED-FORM, not PRINT-FORM-BOUNDED: the clamp on a
                  ;; non-positive budget lives in the wrapper, and this was the
                  ;; one bounded print in the file reaching the stream without
                  ;; it.
                  (%print-bounded-form explanation max-value-chars)
                  (values nil :not-applicable nil))
            (list :rejected rejected
                  :rejection-status rejection-status
                  :rejected-measured (and (integerp rejected) t)
                  ;; Carried so a renderer does not describe a refusal that
                  ;; cannot happen: "0 of them refused by :pre" told the reader
                  ;; a precondition exists, on a contract written without one.
                  :precondition-p precondition-p
                  :rejected-overcounted (and overcounted t)
                  :rejected-contradicted (and contradicted t)
                  ;; The single question every consumer of this plist asks: is
                  ;; the refusal count one this response may subtract with.
                  :rejected-usable (and usable t)
                  ;; Absent, not floored, when the two cannot be subtracted: 0
                  ;; is itself a claim -- "the function was never called" --
                  ;; about a run that did call it, and the text says the number
                  ;; cannot be derived while the JSON would have said zero.
                  :effective-trials (when usable (- executed rejected))
                  :failure-reason reason
                  ;; Whether the reader resolved, not whether it returned it.
                  ;; NIL is a legitimate answer from cl-spec -- a passing run,
                  ;; or a failing one whose counterexample did not reproduce --
                  ;; so it cannot double as "this adapter could not ask".
                  ;; Reported for the same reason REJECTED-MEASURED is: without
                  ;; it a renderer tells the caller their function is
                  ;; non-deterministic on the evidence of a name this image
                  ;; could not find.
                  :failure-reason-readable (and reason-read t)
                  :explanation explanation-text
                  :explanation-readable (and explanation-read t)
                  :explanation-complete explanation-complete
                  :explanation-omitted-chars explanation-omitted)))))))

(defun %result-plist (api result name kind trials digest expected-digest
                      max-value-chars facts)
  "Return the per-property plist for a cl-spec RESULT.

COUNTEREXAMPLE-STATUS and SHRINK-STATUS carry what an empty list cannot.  A
property that generates no arguments and fails has a counterexample that is
legitimately empty, and cl-spec reports it as NIL -- exactly what a run that
never reached a verdict also reports.  Reading one as the other is how a
consumer ends up believing a timeout produced a counterexample with no
arguments, or that a failure was somehow argument-free."
  (let* ((status (funcall (api-fn api :result-status) result))
         (counterexample (funcall (api-fn api :result-counterexample) result))
         (shrunk (funcall (api-fn api :result-shrunk-counterexample) result))
         (condition (funcall (api-fn api :result-condition) result))
         (seed (funcall (api-fn api :result-seed) result))
         (argument-count (getf facts :argument-count))
         (zero-argument-property (eql 0 argument-count))
         (executed (funcall (api-fn api :result-trials) result))
         (verdict (member status '(:failed :error))))
    (list :property (symbol-data name)
          :kind kind
          :contract (when (eq kind :contract)
                      (%contract-plist api result executed max-value-chars
                                       (getf facts :precondition-p)))
          :status status
          :trials (list* :executed executed trials)
          ;; Text, not a number: a cl-spec seed reaches 2^62 and a JSON
          ;; consumer holding it as a number would round it, which turns a
          ;; reproducible failure into one that cannot be reproduced.
          :seed (when seed (format nil "~D" seed))
          ;; NIL for a contract, which has no profile.  CHECK-FUNCTION takes
          ;; none; :NORMAL appears on the result only because RUN-PROPERTY
          ;; defaults it on the synthetic property cl-spec builds underneath.
          ;; Publishing that is the same mis-report profile= is refused for.
          :profile (unless (eq kind :contract)
                     (funcall (api-fn api :result-profile) result))
          :counterexample (%named-values counterexample max-value-chars)
          :counterexample-status
          (cond ((not verdict) :not-applicable)
                (counterexample :present)
                (zero-argument-property :present)
                ((null argument-count) :unknown)
                (t :none))
          :counterexample-unavailable-reason
          (when (and verdict (null counterexample) (null argument-count))
            "the property's argument list could not be read, so an empty
counterexample cannot be told from a missing one")
          :shrunk-counterexample (%named-values shrunk max-value-chars)
          :shrink-status
          (cond ((not verdict) :not-applicable)
                ((not (getf facts :shrink-enabled)) :disabled)
                (shrunk :present)
                (zero-argument-property :present)
                (t :none))
          :shrink-note (when (and verdict (getf facts :shrink-enabled))
                         +shrink-note+)
          :condition (when condition (%condition-data condition))
          :elapsed (funcall (api-fn api :result-elapsed) result)
          :definition-digest (getf digest :value)
          :definition-digest-complete (getf digest :complete)
          ;; What the digest is a digest OF.  It covers the definition
          ;; cl-spec holds and the specs reachable from it -- which for a
          ;; property is the thing that ran, and for a contract is not: the
          ;; code under test is the function, and nothing here reads a
          ;; function body.  Editing TRANSFER and replaying its contract from
          ;; the same seed is faithful by this digest and is not a
          ;; reproduction, so the field has to say which it measured.
          :definition-digest-covers (getf digest :covers)
          :definition-match (%definition-match digest expected-digest))))

(defun %definition-match (digest expected)
  "Return how DIGEST compares to EXPECTED: one of four answers, not two.

:UNKNOWN is the one that was missing.  A digest the adapter could not compute,
or one whose input hit the print limit, is not a digest that disagrees -- and
reporting it as :FALSE told a caller re-running against an unchanged image
that its definitions had moved, which is grounds for throwing away a
reproduction that was in fact faithful."
  (let ((value (getf digest :value)))
    (cond ((null expected) :not-checked)
          ((or (null value) (not (getf digest :complete))) :unknown)
          ((string-equal value expected) :true)
          (t :false))))

(defun %elapsed-since (start)
  "Return the seconds elapsed since internal real time START."
  (/ (float (- (get-internal-real-time) start))
     internal-time-units-per-second))

(defun %run-one (api name registry kind profile seed trials digest expected-digest
                 remaining max-value-chars backend facts)
  "Run property or contract NAME within REMAINING seconds and return its plist.

KIND is :PROPERTY or :CONTRACT.  A contract goes to CHECK-FUNCTION, which takes
a trial count rather than a profile: cl-spec resolves a property's count from
its own :TRIALS table and a contract has none, so the budget derived here is
handed over directly.

The deadline covers rendering as well as running.  Generation, evaluation,
shrinking and the printing of the counterexample all happen inside it, because
a value's own PRINT-OBJECT is user code as much as the property body is and a
budget that stopped at the property's last trial would not bound it.

cl-spec's specials are PROGV-bound inside the thread.  RUN-PROPERTY reads
*GENERATOR-BACKEND* where it runs, and a thread does not inherit dynamic
bindings, so without this a caller who rebound the backend would have the
budget computed from one backend and the trials run under another."
  (if (< remaining *minimum-run-budget-seconds*)
      (list :property (symbol-data name)
            :kind kind
            :status :not-run
            :reason :budget-exhausted
            :trials trials
            :definition-digest (getf digest :value)
            :definition-digest-complete (getf digest :complete)
            :definition-match :not-checked
            :counterexample-status :not-run
            :shrink-status :not-run
            :message +budget-exhausted-message+)
      (let* ((bindings
               (remove nil
                       (list (let ((symbol (api-special api :generator-backend)))
                               (when symbol (cons symbol backend)))
                             (let ((symbol (api-special api :registry)))
                               (when symbol (cons symbol registry))))))
             (names (mapcar #'car bindings))
             (settings (mapcar #'cdr bindings)))
        (multiple-value-bind (value status leaked)
            (call-with-deadline-thread
             (lambda ()
               (progv names settings
                 (%result-plist api
                                (if (eq kind :contract)
                                    ;; The keyword only when there is a
                                    ;; number.  :TRIALS NIL is not "use your
                                    ;; default", it is an override that
                                    ;; suppresses it -- and the budget is NIL
                                    ;; whenever BACKEND-DEFAULT-TRIALS could
                                    ;; not be read.
                                    (apply (api-fn api :check-function) name
                                           :seed seed :registry registry
                                           (let ((budget (getf trials :budget)))
                                             (when budget (list :trials budget))))
                                    (funcall (api-fn api :run-property) name
                                             :profile profile :seed seed
                                             :registry registry))
                                name kind trials digest expected-digest
                                max-value-chars facts)))
             remaining
             :name "mcp-spec-check")
          (ecase status
            (:ok (first value))
            (:timeout
             (list :property (symbol-data name)
                   :kind kind
                   :status :timeout
                   :timeout-seconds value
                   :thread-leaked leaked
                   :trials trials
                   :definition-digest (getf digest :value)
                   :definition-digest-complete (getf digest :complete)
                   :definition-match :not-checked
                   :counterexample-status :unavailable
                   :counterexample-unavailable-reason
                   "the run did not reach a verdict within its deadline"
                   :shrink-status :unavailable
                   :message
                   (if leaked +timeout-leaked-message+ +timeout-stopped-message+)))
            (:error
             (list :property (symbol-data name)
                   :kind kind
                   :status (%classify-condition api value)
                   :trials trials
                   :definition-digest (getf digest :value)
                   :definition-digest-complete (getf digest :complete)
                   :definition-match :not-checked
                   :counterexample-status :unavailable
                   :counterexample-unavailable-reason
                   "the run signalled before producing a result"
                   :shrink-status :unavailable
                   :condition (%condition-data value))))))))

(defun %terminal-status-p (status)
  "Return true when STATUS is a verdict about the property rather than about
the run's own machinery."
  (member status '(:passed :failed :error :skipped :pending)))

(defparameter +result-statuses+
  '(:passed :failed :error :skipped :pending
    :timeout :not-run :generator-error :backend-error :not-registered
    :undefined-function :unsupported :internal-error)
  "Every status one property's result can carry.")

(defparameter +call-statuses+
  '(:no-properties :completed :incomplete :unsupported
    :cl-spec-not-loaded :cl-spec-incomplete :backend-not-loaded
    :unresolved-symbol :not-registered :undefined-function
    :invalid-arguments :internal-error)
  "Every status a whole spec-check call can carry.")

(defparameter +verification-gap-values+
  '(:zero-trials :effective-trials-unknown :rejection-counts-unmeasured
    :input-coverage-unmeasured :contract-not-run :properties-not-run
    :related-properties-unknown :no-properties-selected)
  "Every verification_gaps value that is not a per-result status.

A result status that is not a verdict is pushed into the list as itself, and
those are documented through +RESULT-STATUSES+.  These are the rest, kept here
for the same reason the status lists are: the tool description is the only
documentation a model ever sees, this set has grown four times in one branch,
and a value the code can emit that the description does not name is a value the
caller has to guess at.")

(defparameter +named-count-statuses+
  '((:passed . :passed) (:failed . :failed) (:error . :errored)
    (:timeout . :timed-out) (:not-run . :not-run))
  "Statuses with a field of their own in the counts plist.")

(defun %counts (results)
  "Return the per-status tally of RESULTS.

BY-STATUS covers every status that occurred, not only the five with fields of
their own.  The named fields alone lost :GENERATOR-ERROR, :BACKEND-ERROR,
:INTERNAL-ERROR and :NOT-REGISTERED entirely, so a selection of one property
whose run blew up reported one selected and zero of everything -- a run that
failed outright, reading as a run in which nothing went wrong.  OTHER is the
same total in one number, and SELECTED always equals the sum."
  (flet ((tally (status)
           (count status results :key (lambda (result) (getf result :status)))))
    (let ((by-status '()))
      (dolist (result results)
        (let* ((status (getf result :status))
               (entry (assoc status by-status)))
          (if entry
              (incf (cdr entry))
              (push (cons status 1) by-status))))
      (list :selected (length results)
            :passed (tally :passed)
            :failed (tally :failed)
            :errored (tally :error)
            :timed-out (tally :timeout)
            :not-run (tally :not-run)
            :other (count-if-not (lambda (result)
                                   (assoc (getf result :status)
                                          +named-count-statuses+))
                                 results)
            :by-status (sort by-status #'string<
                             :key (lambda (entry)
                                    (princ-to-string (car entry))))))))

(defun %evaluated-p (result)
  "Return true when RESULT records at least one trial actually evaluated.

cl-spec reports :PASSED for a property whose profile resolves to a budget of
zero -- the trial loop simply never runs -- and a passing status with nothing
behind it is precisely the shape of a verification that did not happen.

For a contract the effective count is the only count that answers this, and
there is no falling back to the raw one when it is missing.  The raw count is
what the effective count exists to correct: it includes every argument list
the :PRE refused, so a contract whose precondition admitted nothing at all
reads as a hundred evaluated trials.  Missing means unknown, and unknown is
not evidence -- whether it went missing because this cl-spec exports no
refused-input reader, because that reader signalled, or because cl-spec
counted more refusals than trials."
  (let* ((contract (getf result :contract))
         (effective (getf contract :effective-trials))
         (executed (getf (getf result :trials) :executed)))
    (if contract
        (and (integerp effective) (plusp effective))
        (and (integerp executed) (plusp executed)))))

(defun %verification-gaps (results &optional selection)
  "Return the reasons RESULTS fall short of a complete verification.

Input coverage holds on every run this adapter can make and is listed anyway:
nothing reports which parts of the input domain were reached, so a caller must
not read a trial count as that (cl-spec specification 72.1).

Rejection counts are listed whenever they were not measured, which is every
property run and no contract run.  The asymmetry is not an oversight: a
property's inputs are refused inside the generator, where check-it retries a
guard without reporting how often, so how much of the generated domain the
body actually saw is unknown.  A contract's are refused by the checker, which
counts them -- so the gap is listed where the number is missing and left off
where it is present, including on a contract with no :PRE, where the honest
count is zero.  Claiming a gap whose answer is right there trains a reader to
ignore the list.

SELECTION is read for what did not run at all.  A contract an :ABOUT selection
left alone is a coverage shortfall like any other, and until it was listed
here the only place that said so was the headline: a consumer branching on
VERIFIED and this list -- the documented pair for what a run could not
establish -- read full coverage for a function whose contract never ran."
  (let ((gaps '())
        (rejections-measured t))
    (dolist (result results)
      ;; Keyed on REJECTED-USABLE, the one flag that answers "may this
      ;; response subtract with the refusal count".  Keyed on the reader
      ;; alone, every case %CONTRACT-PLIST withholds the figure for -- an
      ;; overcount, a refusal against a contract with no :pre -- reported no
      ;; shortfall whatever.  A property has no contract half at all, and its
      ;; rejections happen inside the generator where nothing counts them.
      (let ((contract (getf result :contract)))
        ;; A contract run that timed out or never started carries no
        ;; contract half at all, so it is neither "measured" nor a property
        ;; run: nothing was counted, and the gap says so.
        (when (or (null contract)
                  (not (member (getf contract :rejection-status)
                               '(:usable :no-precondition))))
          (setf rejections-measured nil)))
      (let ((status (getf result :status)))
        (case status
          (:passed
           (unless (%evaluated-p result)
             ;; Two different shortfalls.  ZERO-TRIALS is documented as a
             ;; budget that resolved to nothing; a contract whose effective
             ;; count could not be derived did run trials, and saying its
             ;; budget was zero sends the reader to raise a number that was
             ;; never the problem.
             (pushnew (if (and (getf result :contract)
                               (not (integerp
                                     (getf (getf result :contract)
                                           :effective-trials))))
                          :effective-trials-unknown
                          :zero-trials)
                      gaps)))
          ((:failed :error) nil)
          (t (pushnew status gaps)))))
    (append (nreverse gaps)
            (when (getf selection :contract-not-run) (list :contract-not-run))
            (when (getf selection :properties-not-run)
              (list :properties-not-run))
            (when (and (getf selection :kind)
                       (eq :contract (getf selection :kind))
                       (not (getf selection :properties-not-run-read)))
              (list :related-properties-unknown))
            (unless rejections-measured (list :rejection-counts-unmeasured))
            (list :input-coverage-unmeasured))))

(defun %verified-p (results)
  "Return true only when RESULTS are evidence that every property held.

Three conditions, not one: something was selected, every result is :PASSED,
and every one of them evaluated at least one trial.  Dropping the third would
let a property budgeted zero trials report itself verified."
  (and results
       (every (lambda (result)
                (and (eq :passed (getf result :status))
                     (%evaluated-p result)))
              results)
       t))

(defun check-report (api api-status &key property symbol function package profile
                                         seed trials expect-definition-digest
                                         timeout-seconds
                                         (max-value-chars 2000))
  "Return the plist behind the spec-check tool.

Runs the property named by PROPERTY, every property registered :ABOUT the
symbol named by SYMBOL, or the function spec named by FUNCTION.
TIMEOUT-SECONDS is the budget for the whole call, spent across the selection
in order.

:VERIFIED is true only when at least one property was selected and every one
of them passed.  A selection of zero, a timeout, a generator failure and a
skipped run are each reported as themselves: none of them is evidence that
anything holds."
  (let ((environment (environment-data api api-status))
        (argument-error (%target-argument-error property symbol function
                                                trials profile)))
    ;; Before the availability check, deliberately: an argument that is wrong
    ;; is wrong whatever cl-spec is doing, and reporting the load state first
    ;; would send the caller to fix the wrong thing.
    (when argument-error
      (return-from check-report
        (append argument-error (list :verified nil :environment environment))))
    (unless (eq api-status :ok)
      (return-from check-report (unavailable-report api-status environment)))
    ;; Above the backend check, for the reason the argument check is above
    ;; both: whether this revision exports the contract API is a fact about
    ;; the image and does not change when a generator backend is installed.
    ;; Ordered the other way, a caller was sent to load cl-spec/check-it and
    ;; only then told that the cl-spec they have cannot run a contract at all.
    (when (and function (not (and (api-has-p api :check-function)
                                  (api-has-p api :function-spec-data))))
      (return-from check-report
        (list :status :unsupported
              :verified nil
              :message (%contract-unsupported-message api)
              :environment environment)))
    (unless (api-backend-available-p api)
      (return-from check-report
        (list :status :backend-not-loaded
              :verified nil
              :message +backend-missing-message+
              :environment environment)))
    (multiple-value-bind (profile-keyword profile-error) (%resolve-profile profile)
      (when profile-error
        (return-from check-report
          (list :status :invalid-arguments :verified nil
                :message profile-error :environment environment)))
      (let ((registry (funcall (api-fn api :registry))))
        (multiple-value-bind (names selection selection-error kind selected-data)
            (%select-properties api property symbol function package registry)
          (when selection-error
            (return-from check-report
              (append selection-error
                      (list :verified nil :environment environment))))
          (when (null names)
            (return-from check-report
              (list :status :no-properties
                    :verified nil
                    ;; Through %VERIFICATION-GAPS like every other branch,
                    ;; rather than assembled here: built by hand this list
                    ;; omitted input-coverage-unmeasured, which the tool
                    ;; description says is always in it.
                    :verification-gaps
                    (cons :no-properties-selected
                          (%verification-gaps nil selection))
                    :selection selection
                    :results nil
                    :counts (%counts nil)
                    ;; Carried explicitly, even though nothing ran: the
                    ;; builder reads these keys, and an absent
                    ;; :REPRODUCTION-FAITHFUL published as "unfaithful" --
                    ;; an unfaithful reproduction of a run that never
                    ;; happened, for a call that requested no comparison.
                    :profile profile-keyword
                    :timeout-seconds (or timeout-seconds
                                         *default-check-timeout-seconds*)
                    :thread-leaked nil
                    :worker-reuse :safe
                    :elapsed 0
                    :options nil
                    :options-note +options-note+
                    :reproduction-faithful :not-checked
                    :message +zero-properties-message+
                    :environment environment)))
          (when (and seed (rest names))
            (return-from check-report
              (list :status :invalid-arguments
                    :verified nil
                    :selection selection
                    :reproduction-faithful :not-checked
                    :message +seed-needs-one-property-message+
                    :environment environment)))
          (let ((budget (or timeout-seconds *default-check-timeout-seconds*))
                (start (get-internal-real-time))
                (results '())
                (thread-leaked nil)
                ;; Captured once, on this thread, and handed to both the
                ;; budget derivation and the run.  Reading the special in
                ;; each place would let the two disagree.
                (backend (handler-case
                             (funcall (api-fn api :generator-backend))
                           (error () nil))))
            (dolist (name names)
              ;; SELECTED-DATA is set only by an explicit selection, which
              ;; names exactly one definition -- this one.  An :ABOUT
              ;; selection leaves it NIL and each name is read here.
              (let* ((facts (if (eq kind :contract)
                                (%contract-facts api name registry selected-data)
                                (%property-facts api name registry selected-data)))
                     (budget-plist (%trials-budget api facts profile-keyword
                                                   backend trials))
                     (digest (append (%digest-facts api name registry facts)
                                     (list :covers (if (eq kind :contract)
                                                       :contract
                                                       :property))))
                     (remaining (- budget (%elapsed-since start)))
                     (result (%run-one api name registry kind profile-keyword seed
                                       budget-plist digest
                                       expect-definition-digest remaining
                                       max-value-chars backend facts)))
                (when (getf result :thread-leaked) (setf thread-leaked t))
                (push result results)))
            (setf results (nreverse results))
            (list :status (if (every (lambda (result)
                                       (%terminal-status-p (getf result :status)))
                                     results)
                              :completed
                              :incomplete)
                  :verified (%verified-p results)
                  :verification-gaps (%verification-gaps results selection)
                  :selection selection
                  :results results
                  :counts (%counts results)
                  ;; Absent on a contract run for the same reason it is
                  ;; refused as an argument there: nothing selected it and
                  ;; nothing used it.
                  :profile (unless (eq kind :contract) profile-keyword)
                  :timeout-seconds budget
                  :thread-leaked thread-leaked
                  ;; Reported for any timeout, not only a leaked one.  A
                  ;; thread that stopped is not evidence that what it was
                  ;; doing was undone.
                  :worker-reuse (cond (thread-leaked :unsafe)
                                      ((find :timeout results
                                             :key (lambda (result)
                                                    (getf result :status)))
                                       :unknown)
                                      (t :safe))
                  :worker-reuse-message
                  (cond (thread-leaked +worker-reuse-unsafe-message+)
                        ((find :timeout results
                               :key (lambda (result) (getf result :status)))
                         +worker-reuse-unknown-message+)
                        (t nil))
                  :elapsed (%elapsed-since start)
                  :options nil
                  :options-note +options-note+
                  :reproduce-scope +reproduce-scope-note+
                  ;; Kept apart from :VERIFIED on purpose.  A run whose
                  ;; definitions moved can still pass; what it cannot claim
                  ;; is to have reproduced the earlier run.
                  :reproduction-faithful
                  (cond ((null expect-definition-digest) :not-checked)
                        ((find :unknown results
                               :key (lambda (result)
                                      (getf result :definition-match)))
                         :unknown)
                        ((every (lambda (result)
                                  (eq :true (getf result :definition-match)))
                                results)
                         :true)
                        ;; :FALSE rather than NIL: absence of a verdict and a
                        ;; verdict of "not faithful" are different answers,
                        ;; and NIL was carrying both.
                        (t :false))
                  :environment environment)))))))
