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

(defparameter +symbol-has-a-contract-note+
  (concatenate 'string
               "a function spec is registered for this symbol: read it with "
               "spec-describe kind=function-spec, run it with spec-check "
               "function=<this symbol>. It says which inputs the function "
               "accepts and which output it must return, which the properties "
               "about it do not.")
  "Said by spec-symbol when a contract is registered for the symbol.")

(defparameter +contract-not-selected-note+
  (concatenate 'string
               "a function spec is registered for this symbol and was NOT "
               "run: an :about selection covers properties only. Run it with "
               "spec-check function=<this symbol>.")
  "Said by spec-check when an :about selection leaves a contract unchecked.")

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
                          (list +symbol-has-a-contract-note+)))
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

(defparameter +listing-unsupported-message+
  (concatenate 'string
               "this cl-spec revision does not export the listing functions "
               "(list-specs, list-properties); nothing can be enumerated. "
               "Every other spec tool still works on a name you already have.")
  "Said when the loaded cl-spec cannot enumerate its registry.")

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
    (error () (list :name (symbol-data name)))))

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

KIND is \"specs\", \"properties\" or \"both\".  PACKAGE and TAG narrow the
result; TAG applies to properties only, and a tag no loaded code mentions is
reported as unresolved rather than as an empty result, because the two are
different answers."
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
    (unless (and (api-has-p api :list-specs) (api-has-p api :list-properties))
      (return-from list-report
        (list :status :unsupported
              :message +listing-unsupported-message+
              :environment environment)))
    (multiple-value-bind (package-object package-error)
        (%listing-package-filter package)
      (when package-error
        (return-from list-report
          (append package-error (list :environment environment))))
      (let* ((registry (funcall (api-fn api :registry)))
             (tag-keyword (and tag (find-keyword tag)))
             (want-specs (member kind '("specs" "both") :test #'string=))
             (want-properties (member kind '("properties" "both") :test #'string=))
             (want-function-specs (member kind '("function-specs" "both")
                                          :test #'string=))
             ;; Listed only when cl-spec can enumerate them.  Absence is
             ;; reported as its own answer below rather than as an empty
             ;; list: "this revision cannot enumerate contracts" and "there
             ;; are none" are different, and only one of them is a fact
             ;; about the project.
             (function-specs-listable (and (api-has-p api :list-function-specs)
                                           (api-has-p api :function-spec-data)))
             (function-spec-names
               (when (and want-function-specs function-specs-listable)
                 (remove-if-not
                  (lambda (name) (%in-package-p name package-object))
                  (funcall (api-fn api :list-function-specs) registry))))
             (spec-names
               (when want-specs
                 (remove-if-not (lambda (name) (%in-package-p name package-object))
                                (funcall (api-fn api :list-specs) registry))))
             (property-names
               (when want-properties
                 (remove-if-not
                  (lambda (name) (%in-package-p name package-object))
                  (cond
                    ((null tag) (funcall (api-fn api :list-properties) registry))
                    ((null tag-keyword) '())
                    ((api-has-p api :properties-with-tag)
                     (funcall (api-fn api :properties-with-tag)
                              tag-keyword registry))
                    (t '()))))))
        (list :status :ok
              :kind kind
              :specs (mapcar #'symbol-data (%take spec-names limit))
              :properties (loop for name in (%take property-names limit)
                                collect (%property-listing api name registry))
              :function-specs (loop for name in (%take function-spec-names limit)
                                    collect (%function-spec-listing api name registry))
              :function-specs-listable (and function-specs-listable t)
              ;; NIL, not 0, for a kind that was not asked for.  The count
              ;; is a fact about the registry and the list is what this
              ;; response carries; not looking leaves the first unknown, and
              ;; reporting it as zero says the registry holds none -- the
              ;; same conflation the tag and no-properties answers go out of
              ;; their way to avoid.
              :counts (list :specs (when want-specs (length spec-names))
                            :properties (when want-properties
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
                                                 (t nil)))
              :coverage +listing-coverage-note+
              :environment environment)))))

(defparameter +function-spec-unsupported-message+
  (concatenate 'string
               "the cl-spec loaded here does not export function-spec-data, "
               "so there is nothing to project. cl-mcp will not assemble one "
               "out of the individual readers: that would duplicate cl-spec's "
               "introspection responsibility on this side of the boundary. "
               "This is a statement about the loaded revision, not about "
               "whether a contract is registered for the symbol.")
  "Said when spec-describe is asked for a function spec cl-spec cannot project.")

(defparameter +check-function-unsupported-message+
  (concatenate 'string
               "the cl-spec loaded here does not export check-function, so a "
               "contract cannot be executed. Its text can still be read with "
               "spec-describe kind=function-spec when function-spec-data is "
               "available.")
  "Said when spec-check is asked to run a contract cl-spec cannot run.")

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
          :min (%range-bound (getf spec-plist :min))
          :max (%range-bound (getf spec-plist :max))
          :class-name (let ((name (getf spec-plist :class-name)))
                        (when name (symbol-data name)))
          :source-form (printed-for-display (getf spec-plist :source-form))
          :source-location (getf spec-plist :source-location)
          :children (mapcar #'%spec-tree (getf spec-plist :children)))))

(defun %range-bound (value)
  "Return a range end as text, or NIL when the node has none.

cl-spec spells an open end :UNBOUNDED.  Mapped back to the * the author wrote,
here rather than in a renderer, so the text and the JSON agree and neither has
to know the IR's word for it."
  (when value
    (if (eq :unbounded value) "*" (printed-for-display value))))

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
    (flet ((clause (forms)
             ;; Bounded like the body a property describe carries.  A :PRE or
             ;; :POST form is short in practice, but "in practice" is not a
             ;; budget, and every other form this module prints is cut at one.
             ;; NIL rather than the string "NIL" for an absent clause, so a
             ;; renderer can tell a contract with no :PRE from one whose :PRE
             ;; is the literal NIL.
             (when forms
               (multiple-value-bind (text complete) (%print-bounded-form forms max-chars)
                 (list text complete)))))
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
                :preconditions-complete (if pre (second pre) t)
                :postconditions (first post)
                :postconditions-complete (if post (second post) t)
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
              :definition-digest (definition-digest api name registry))))))

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

(defun %registered-property-p (api name registry)
  "Return (values FOUND-P MESSAGE) for property NAME in REGISTRY."
  (handler-case
      (progn (funcall (api-fn api :property-data) name :registry registry)
             (values t nil))
    (error (condition) (values nil (princ-to-string condition)))))

(defun %select-explicit (api property package registry)
  "Return (values NAMES SELECTION ERROR) for an explicit property request."
  (multiple-value-bind (name reason)
      (resolve-symbol-designator property :package package)
    (if (null name)
        (values nil nil (list :status :unresolved-symbol :reason reason
                              :input property))
        (multiple-value-bind (found-p message)
            (%registered-property-p api name registry)
          (if (not found-p)
              (values nil nil (list :status :not-registered
                                    :name (symbol-data name)
                                    :message message))
              (values (list name)
                      (list :mode "explicit"
                            :requested (list :property (symbol-data name))
                            :selected (list (symbol-data name))
                            :count 1
                            :source "explicit property argument"
                            :coverage +explicit-coverage-note+)
                      nil))))))

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
                        :requested (list :symbol (symbol-data name))
                        :selected (mapcar #'symbol-data about)
                        :count (length about)
                        :source +about-source+
                        :coverage +about-coverage-note+
                        :notes (append
                                (when (getf routing :property)
                                  (list +symbol-is-a-property-not-selected-note+))
                                (when (getf routing :function-spec)
                                  (list +contract-not-selected-note+))))
                  nil)))))

(defparameter +trials-needs-a-contract-message+
  (concatenate 'string
               "trials applies to a contract run (function=...) only. A "
               "property's trial count comes from its own :trials table, "
               "selected by profile; cl-spec's run-property takes no override, "
               "so honouring trials here would report a budget the run did not "
               "use.")
  "Said when trials is given for a property selection.")

(defun %target-argument-error (property symbol function)
  "Return the plist for a bad target selection, or NIL when it is fine.

Checked before cl-spec is consulted.  Naming more than one or none of them is
a mistake in the call itself, and answering \"cl-spec is not loaded\" would
send the caller to fix the wrong thing -- the same reason SPEC-ENTRY validates
the seed before it resolves the API."
  (let ((given (count-if-not #'null (list property symbol function))))
    (cond
      ((> given 1)
       (list :status :invalid-arguments
             :message "give exactly one of property, symbol or function"))
      ((zerop given)
       (list :status :invalid-arguments
             :message "give one of property, symbol or function")))))

(defun %registered-contract-p (api name registry)
  "Return (values FOUND-P MESSAGE) for the contract registered for NAME."
  (handler-case
      (progn (funcall (api-fn api :function-spec-data) name :registry registry)
             (values t nil))
    (error (condition) (values nil (princ-to-string condition)))))

(defun %select-contract (api function package registry)
  "Return (values NAMES SELECTION ERROR) for an explicit contract request."
  (multiple-value-bind (name reason)
      (resolve-symbol-designator function :package package)
    (if (null name)
        (values nil nil (list :status :unresolved-symbol :reason reason
                              :input function))
        (multiple-value-bind (found-p message)
            (%registered-contract-p api name registry)
          (if (not found-p)
              (values nil nil (list :status :not-registered
                                    :name (symbol-data name)
                                    :message message))
              (values (list name)
                      (list :mode "contract"
                            :requested (list :function (symbol-data name))
                            :selected (list (symbol-data name))
                            :count 1
                            :source "explicit function argument"
                            :coverage +contract-coverage-note+)
                      nil))))))

(defun %select-properties (api property symbol function package registry)
  "Return (values NAMES SELECTION ERROR KIND) for the requested selection.

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
     (multiple-value-bind (names selection error)
         (%select-contract api function package registry)
       (values names selection error :contract)))
    (property
     (multiple-value-bind (names selection error)
         (%select-explicit api property package registry)
       (values names selection error :property)))
    (t
     (multiple-value-bind (names selection error)
         (%select-about api symbol package registry)
       (values names selection error :property)))))

(defun %property-facts (api name registry)
  "Return the facts about property NAME a result needs to describe itself.

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
      (let ((data (funcall (api-fn api :property-data) name :registry registry)))
        (list :argument-count (length (getf data :arguments))
              :shrink-enabled (and (getf (getf data :metadata) :shrink) t)
              :trials-table (getf data :trials)
              ;; Carried so the digest beside it does not fetch the same
              ;; plist again.
              :data data
              :known t))
    (error ()
      (list :argument-count nil :shrink-enabled nil :trials-table nil
            :data nil :known nil))))

(defun %contract-facts (api name registry)
  "Return the facts about the contract for NAME a result needs.

The same shape %PROPERTY-FACTS returns, so everything downstream reads one
plist.  A contract has no :TRIALS table -- cl-spec's CHECK-FUNCTION takes a
count, not a profile -- and shrinking is always on, so those two are constants
here rather than things read off a definition."
  (handler-case
      (let ((data (funcall (api-fn api :function-spec-data) name :registry registry)))
        (list :argument-count (length (getf data :arguments))
              :shrink-enabled t
              :trials-table nil
              :data data
              :known t))
    (error ()
      (list :argument-count nil :shrink-enabled t :trials-table nil
            :data nil :known nil))))

(defun %digest-facts (api name registry facts)
  "Return (:value <string-or-nil> :complete <boolean>) for NAME's digest.

Carried as one plist rather than two arguments because the pair travels
together everywhere: a digest whose input was truncated is not a digest a
caller may compare, and separating them invites reporting the value without
the caveat."
  (multiple-value-bind (value complete)
      (if (getf facts :known)
          (definition-digest api name registry :property (getf facts :data))
          (definition-digest api name registry))
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
of failing to load."
  (flet ((is-a (key)
           (let ((class (api-class api key)))
             (and class (typep condition class)))))
    (cond
      ((or (is-a :no-generator-backend) (is-a :generator-unavailable))
       :generator-error)
      ((is-a :unknown-property) :not-registered)
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

(defun %contract-plist (api result executed max-value-chars)
  "Return the contract-specific half of a CHECK-FUNCTION result, or NIL.

REJECTED is what separates a run that checked the function from one that only
generated arguments for it: cl-spec's checker refuses inputs its :PRE does not
admit, and a trial count that includes them overstates the work.  It is
reported with REJECTED-MEASURED beside it, because a cl-spec whose readers this
adapter could not resolve gives NIL, which must not read as zero rejections.

FAILURE-REASON names which half of the contract broke; EXPLANATION carries
cl-spec's structured account of a return value that missed its spec."
  (flet ((read-slot (key)
           (when (api-has-p api key)
             (handler-case (funcall (api-fn api key) result)
               (error () nil)))))
    (let* ((rejected (read-slot :check-rejected))
           (reason (read-slot :check-failure-reason))
           (explanation (read-slot :check-explanation)))
      (list :rejected rejected
            :rejected-measured (and (integerp rejected) t)
            :effective-trials (when (and (integerp executed) (integerp rejected))
                                (- executed rejected))
            :failure-reason reason
            :explanation (when explanation
                           (print-form-bounded explanation max-value-chars))))))

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
                      (%contract-plist api result executed max-value-chars))
          :status status
          :trials (list* :executed executed trials)
          ;; Text, not a number: a cl-spec seed reaches 2^62 and a JSON
          ;; consumer holding it as a number would round it, which turns a
          ;; reproducible failure into one that cannot be reproduced.
          :seed (when seed (format nil "~D" seed))
          :profile (funcall (api-fn api :result-profile) result)
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
                                    (funcall (api-fn api :check-function) name
                                             :trials (getf trials :budget)
                                             :seed seed :registry registry)
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
    :internal-error)
  "Every status one property's result can carry.

Listed in one place so the tool description can be checked against it.  The
set grew three times while the adapter was being reviewed and the description
did not follow, which left an agent reading about seven statuses that a run
could answer with eleven -- and the description is the only documentation a
model ever sees.")

(defparameter +call-statuses+
  '(:no-properties :completed :incomplete
    :cl-spec-not-loaded :cl-spec-incomplete :backend-not-loaded
    :unresolved-symbol :not-registered :invalid-arguments :internal-error)
  "Every status a whole spec-check call can carry.")

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

For a contract the number that counts is the one with the rejected inputs taken
out.  A generated argument list its :PRE refused was never passed to the
function, so counting it here would let a contract nothing exercised report
itself evaluated."
  (let ((effective (getf (getf result :contract) :effective-trials))
        (executed (getf (getf result :trials) :executed)))
    (if (integerp effective)
        (plusp effective)
        (and (integerp executed) (plusp executed)))))

(defun %verification-gaps (results)
  "Return the reasons RESULTS fall short of a complete verification.

Input coverage holds on every run this adapter can make and is listed anyway:
nothing reports which parts of the input domain were reached, so a caller must
not read a trial count as that (cl-spec specification 72.1).

Rejection counts are listed only when they were in fact not measured.  A
property has no precondition and no rejection to count, and a contract check
reports one; claiming the gap where the number is right there would train a
reader to ignore the list."
  (let ((gaps '())
        (rejections-measured t))
    (dolist (result results)
      (unless (getf (getf result :contract) :rejected-measured)
        (setf rejections-measured nil))
      (let ((status (getf result :status)))
        (case status
          (:passed (unless (%evaluated-p result) (pushnew :zero-trials gaps)))
          ((:failed :error) nil)
          (t (pushnew status gaps)))))
    (append (nreverse gaps)
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
        (argument-error (%target-argument-error property symbol function)))
    ;; Before the availability check, deliberately: an argument that is wrong
    ;; is wrong whatever cl-spec is doing, and reporting the load state first
    ;; would send the caller to fix the wrong thing.
    (when argument-error
      (return-from check-report
        (append argument-error (list :verified nil :environment environment))))
    (unless (eq api-status :ok)
      (return-from check-report (unavailable-report api-status environment)))
    (unless (api-backend-available-p api)
      (return-from check-report
        (list :status :backend-not-loaded
              :verified nil
              :message +backend-missing-message+
              :environment environment)))
    (when (and trials (not function))
      (return-from check-report
        (list :status :invalid-arguments
              :verified nil
              :message +trials-needs-a-contract-message+
              :environment environment)))
    (when (and function (not (and (api-has-p api :check-function)
                                  (api-has-p api :function-spec-data))))
      (return-from check-report
        (list :status :unsupported
              :verified nil
              :message +check-function-unsupported-message+
              :environment environment)))
    (multiple-value-bind (profile-keyword profile-error) (%resolve-profile profile)
      (when profile-error
        (return-from check-report
          (list :status :invalid-arguments :verified nil
                :message profile-error :environment environment)))
      (let ((registry (funcall (api-fn api :registry))))
        (multiple-value-bind (names selection selection-error kind)
            (%select-properties api property symbol function package registry)
          (when selection-error
            (return-from check-report
              (append selection-error
                      (list :verified nil :environment environment))))
          (when (null names)
            (return-from check-report
              (list :status :no-properties
                    :verified nil
                    :verification-gaps (list :no-properties-selected)
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
              (let* ((facts (if (eq kind :contract)
                                (%contract-facts api name registry)
                                (%property-facts api name registry)))
                     (budget-plist (%trials-budget api facts profile-keyword
                                                   backend trials))
                     (digest (%digest-facts api name registry facts))
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
                  :verification-gaps (%verification-gaps results)
                  :selection selection
                  :results results
                  :counts (%counts results)
                  :profile profile-keyword
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
