;;;; specs/spec-inspection.lisp
;;;;
;;;; Properties of what cl-mcp reports about cl-spec before anything runs:
;;;; which operations this cl-spec can serve, what is registered, what a
;;;; declaration says, and which digest stands for it.  The functions checked,
;;;; each called directly:
;;;;
;;;;   API-BACKEND-AVAILABLE-P, CONTRACT-OPERATION-MISSING   what can be served
;;;;   LIST-REPORT                                           what is registered
;;;;   SYMBOL-REPORT, DESCRIBE-REPORT       registration against read failure
;;;;   %DESCRIBE-FUNCTION-SPEC                     what a contract declares
;;;;   DEFINITION-DIGEST                        which digest stands for it
;;;;
;;;; The danger throughout is the false negative: an empty answer given for
;;;; something that could not be read.  Each property therefore keeps three
;;;; answers apart -- present, absent, and unreadable -- and checks what was
;;;; read as well as what came back, from the calls the stub records.
;;;;
;;;; Nothing here runs a property or a contract: the stub's runners signal if
;;;; a read reaches them.  Nothing goes through a deadline thread, a real
;;;; cl-spec or the bundle.  Expectations come from
;;;; specs/spec-inspection-fixtures.lisp: the capability manifest, a registry
;;;; descriptor's relations, and a contract descriptor's own order and
;;;; content -- never from cl-mcp's tables or record shapes.
;;;;
;;;; Verified domain: handle subsets over the six operations; registries built
;;;; from nine fixed definitions across two packages, with tags and one
;;;; (:about ...) relation; listings of each kind under a package, a tag and a
;;;; limit; declarations of one to three arguments of each kind, every clause
;;;; shape, zero to three cases, and character budgets around a clause's
;;;; length; versioned and old-shape records.  Not covered: the renderer,
;;;; JSON-RPC, real cl-spec's own projections (the opt-in suite), and
;;;; resolving the API from a live package (its own suite, in its own
;;;; process).

(defpackage #:cl-mcp/specs/spec-inspection
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:api-backend-available-p
                #:definition-digest)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:contract-operation-missing
                #:list-report
                #:symbol-report
                #:describe-report
                #:%describe-function-spec)
  (:import-from #:cl-mcp/specs/spec-inspection-fixtures
                #:+operation-handles+
                #:+required-handles+
                #:+listing-handles+
                #:+home-package-name+
                #:definition-symbol
                #:definition-name
                #:expected-listing
                #:inspection-api
                #:api-calls
                #:calls-of
                #:contract-record
                #:contract-descriptor
                #:clause-forms
                #:clause-reads-back-p
                #:long-form
                #:spec-node
                #:draw-availability-case
                #:draw-listing-case
                #:draw-registration-case
                #:draw-contract-case
                #:draw-digest-case)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/spec-inspection)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(spec-inspection-operations-need-their-own-handles
    spec-inspection-listing-separates-capability-from-count
    spec-inspection-registration-is-not-read-failure
    spec-inspection-contract-declaration-survives-describe
    spec-inspection-digest-comes-from-the-record-or-the-readers))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(availability-case listing-case registration-case contract-case digest-case))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(availability-case-generator listing-case-generator registration-case-generator
    contract-case-generator digest-case-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none, as it has no
contract.  Its fixed cases are the Rove tests of tests/spec-inspection-test.lisp."
  '())

;;; ------------------------------------------------------------------------
;;; A. Availability

(defun %operation-missing-holds-p (operation present noise)
  "Return true when CONTRACT-OPERATION-MISSING names exactly the handles
OPERATION needs and PRESENT does not hold.  NOISE adds handles the operation
does not need, which must not change the answer."
  (let* ((needed (rest (assoc operation +operation-handles+)))
         (handles (append +required-handles+ present noise))
         (api (inspection-api :handles handles))
         (missing (contract-operation-missing api (ecase operation
                                                    (:describe-contract :describe)
                                                    (:run-contract :run)))))
    (null (set-exclusive-or (set-difference needed handles) missing))))

(defun %backend-availability-holds-p (state)
  "Return true when API-BACKEND-AVAILABLE-P answers for the backend STATE:
an object is available; a special bound to NIL, a reader that signals and a
missing reader are not -- and none of them is an error."
  (let ((api (inspection-api :backend state
                             :handles (if (eq state :missing)
                                          (remove :generator-backend +required-handles+)
                                          +required-handles+))))
    (eq (eq state :object) (and (api-backend-available-p api) t))))

;;; ------------------------------------------------------------------------
;;; B. Listings

(defun %listing-holds-p (registry handles kind package tag limit)
  "Return true when LIST-REPORT reports the capability, the scope and the
counts the descriptor says, reads only what the kind asks for, and hands every
reader the registry it was given."
  (multiple-value-bind (api calls registry-object)
      (inspection-api :handles (append +required-handles+ handles) :registry registry)
    (let* ((expected (expected-listing registry handles :kind kind :package package
                                                        :tag tag))
           (report (list-report api :ok :kind kind :package package :tag tag
                                       :limit limit))
           (counts (getf report :counts)))
      (and
       ;; Every reader was handed the registry, and no runner was called.
       (every (lambda (call) (or (null (third call)) (eq registry-object (third call))))
              (api-calls calls))
       (null (calls-of calls :run-property))
       (null (calls-of calls :check-function))
       (if (getf expected :reachable)
           (and
            (eq :ok (getf report :status))
            ;; Counts: the number registered after the filters, or nothing at
            ;; all for a kind this call did not ask for or could not list.
            (eql (getf expected :specs) (getf counts :specs))
            (eql (getf expected :properties) (getf counts :properties))
            (eql (getf expected :function-specs) (getf counts :function-specs))
            ;; Capability is about the revision, not about this request.
            (eq (getf expected :specs-listable) (getf report :specs-listable))
            (eq (getf expected :properties-listable) (getf report :properties-listable))
            (eq (getf expected :function-specs-listable)
                (getf report :function-specs-listable))
            (eq (getf expected :tag-filterable) (getf report :tag-filterable))
            (eq (getf expected :tag-applied) (getf (getf report :filters) :tag-applied))
            ;; A limit cuts the lists and says so; it never changes a count.
            (eql (min (or (getf expected :specs) 0) limit) (length (getf report :specs)))
            (eql (min (or (getf expected :properties) 0) limit)
                 (length (getf report :properties)))
            (eql (min (or (getf expected :function-specs) 0) limit)
                 (length (getf report :function-specs)))
            (eq (or (> (or (getf expected :specs) 0) limit)
                    (> (or (getf expected :properties) 0) limit)
                    (> (or (getf expected :function-specs) 0) limit))
                (and (getf report :truncated) t))
            ;; Only the kinds asked for were enumerated.
            (or (member kind '("specs" "both") :test #'string=)
                (null (calls-of calls :list-specs)))
            (or (member kind '("function-specs" "both") :test #'string=)
                (null (calls-of calls :list-function-specs)))
            (or (member kind '("properties" "both") :test #'string=)
                (and (null (calls-of calls :list-properties))
                     (null (calls-of calls :properties-with-tag)))))
           ;; Nothing the request asked for can be listed: refused as a
           ;; statement about the revision, not as an empty registry.
           (and (eq :unsupported (getf report :status))
                (null (getf report :counts))
                (stringp (getf report :message))))))))

(defun %tag-states-hold-p (registry)
  "Return true when a tag's three answers stay apart: no tag asked for, a tag
this image knows, and a tag it does not -- which is resolved false rather than
an empty result, and is never interned by asking."
  (let ((handles (append +required-handles+ +listing-handles+))
        (unknown (format nil "cl-mcp-inspection-no-such-tag-~36R" (random (expt 36 10)))))
    (flet ((filters (tag)
             (multiple-value-bind (api) (inspection-api :handles handles :registry registry)
               (getf (list-report api :ok :kind "properties" :tag tag) :filters))))
      (and (eq :not-requested (getf (filters nil) :tag-resolved))
           (eq t (getf (filters "fast") :tag-resolved))
           (null (find-symbol (string-upcase unknown) "KEYWORD"))
           (null (getf (filters unknown) :tag-resolved))
           (null (find-symbol (string-upcase unknown) "KEYWORD"))))))

;;; ------------------------------------------------------------------------
;;; C. Registration against read failure

(defun %registration-holds-p (registry subject failure kind)
  "Return true when DESCRIBE-REPORT tells what is registered from what could
not be read, and SYMBOL-REPORT keeps each relation on its own.

A reader that says the name is unknown gives not-registered; one that breaks
for its own reasons gives an internal error, never an absent registration; and
CL:UNDEFINED-FUNCTION means the target is undefined only for a contract."
  (let ((registered (and (member subject (getf registry :definitions)) t))
        (reader (cond ((string= kind "property") :property-data)
                      ((string= kind "spec") :spec-data)
                      (t :function-spec-data))))
    (multiple-value-bind (api calls)
        (inspection-api :handles (append +required-handles+ +listing-handles+)
                        :registry registry
                        :reader-failures (and failure (list reader failure)))
      (let ((described (describe-report api :ok kind (%designator subject))))
        (and (null (calls-of calls :run-property))
             (null (calls-of calls :check-function))
             (eq (%expected-describe-status subject registered failure kind)
                 (getf described :status))
             (%symbol-report-holds-p api subject registry))))))

(defun kind-of (subject)
  "Return SUBJECT's kind keyword, for the registration case."
  (ecase subject
    ((:spec-a :spec-b :spec-elsewhere) :spec)
    ((:property-a :property-b :property-about :property-elsewhere) :property)
    ((:contract-a :contract-elsewhere) :function-spec)))

(defun %describe-status (report)
  "Return REPORT's status."
  (getf report :status))

(defun %expected-describe-status (subject registered failure kind)
  "Return the status a describe of SUBJECT as KIND must answer.

Read failures come first: whatever is registered, a reader that could not
answer is not evidence of absence.  A name of another kind, or one not
registered at all, is not-registered."
  (let ((contract-p (string= kind "function-spec"))
        (asked (cond ((string= kind "property") :property)
                     ((string= kind "spec") :spec)
                     (t :function-spec))))
    (cond ((eq failure :fails) :internal-error)
          ((eq failure :undefined-function)
           (if contract-p :undefined-function :internal-error))
          ((eq failure :unknown) :not-registered)
          ((and registered (eq (kind-of subject) asked)) :ok)
          (t :not-registered))))

(defun %symbol-report-holds-p (api subject registry)
  "Return true when SYMBOL-REPORT reports each relation of SUBJECT on its own,
says nothing-registered only when the registry holds none of them, and with
include_runtime false reads no runtime and says why."
  (let* ((report (symbol-report api :ok (%designator subject) :include-runtime nil))
         (registered (getf report :registry))
         (held (and (member subject (getf registry :definitions)) t))
         (kind (kind-of subject)))
    (and (eq :ok (getf report :status))
         (null (getf report :runtime))
         (stringp (getf report :runtime-unavailable-reason))
         (eq (and held (eq :spec kind)) (and (getf registered :spec) t))
         (eq (and held (eq :property kind)) (and (getf registered :property) t))
         (eq (and held (eq :function-spec kind))
             (and (getf registered :function-spec) t))
         ;; The (:about ...) properties are the registered ones that name this
         ;; symbol, and no more.
         (eql (length (%about-keys subject registry))
              (length (getf registered :properties-about)))
         (eq (and (null (getf registered :spec))
                  (null (getf registered :property))
                  (null (getf registered :function-spec))
                  (null (getf registered :properties-about))
                  t)
             (and (getf report :nothing-registered) t)))))

(defun %designator (subject)
  "Return SUBJECT's package-qualified designator."
  (destructuring-bind (package name) (definition-name subject)
    (format nil "~A::~A" package name)))

(defun %about-keys (subject registry)
  "Return the registered properties REGISTRY holds that are (:about SUBJECT).
Only :PROPERTY-ABOUT is declared that way, and only about :CONTRACT-A."
  (when (eq subject :contract-a)
    (intersection '(:property-about) (getf registry :definitions))))

;;; ------------------------------------------------------------------------
;;; D. Contract declarations

(defun %contract-describe (descriptor max-chars)
  "Return the description of the contract DESCRIBER describes, under MAX-CHARS."
  (multiple-value-bind (api calls)
      (inspection-api :handles (append +required-handles+ +listing-handles+)
                      :registry (list :definitions (list :contract-a))
                      :contract descriptor)
    (values (%describe-function-spec api (definition-symbol :contract-a)
                                     :inspection-registry max-chars)
            calls)))

(defun %arguments-hold-p (descriptor description)
  "Return true when the arguments arrive in the order declared, each with its
own variable, kind, supplied-p and keyword.  A required argument's kind is
reported as :REQUIRED, which version 1 omits."
  (let ((declared (getf descriptor :arguments))
        (given (getf description :arguments)))
    (and (= (length declared) (length given))
         (loop for (kind . options) in declared
               for argument in given
               for index from 0
               always (and (equal (format nil "ARG-~D" index)
                                  (getf (getf argument :variable) :name))
                           (eq kind (getf argument :kind))
                           (eq (and (getf options :supplied-p) t)
                               (and (getf argument :supplied-p) t))
                           (equal (getf options :keyword) (getf argument :keyword))
                           ;; The spec node came across, with its range ends:
                           ;; an open end as *, a zero as "0".
                           (let ((spec (getf argument :spec)))
                             (and (eq :range (getf spec :kind))
                                  (equal (if (evenp index) "0" "*") (getf spec :min))
                                  (equal (princ-to-string index) (getf spec :max)))))))))

(defun %clauses-hold-p (descriptor description)
  "Return true when each clause reads back as what it means -- one form as
itself, several joined by AND -- and a clause that is not there is reported as
not applicable rather than as an empty one."
  (flet ((clause (shape text-key complete-key)
           (let ((text (getf description text-key))
                 (complete (getf description complete-key)))
             (if (eq shape :none)
                 (and (null text) (eq :not-applicable complete))
                 (and (clause-reads-back-p (clause-forms shape) text)
                      (eq t complete))))))
    (and (clause (getf descriptor :pre) :preconditions :preconditions-complete)
         (clause (getf descriptor :post) :postconditions :postconditions-complete)
         (clause (getf descriptor :state-post) :state-post :state-post-complete))))

(defun %cases-hold-p (descriptor description)
  "Return true when the cases arrive in the order declared, each with its
guard, outcome and clauses, and a contract with no cases declares none."
  (let ((declared (getf descriptor :cases)))
    (if (eq declared :none)
        (and (null (getf description :case-selection))
             (null (getf description :cases)))
        (and (eq :exclusive (getf description :case-selection))
             (equal declared (mapcar (lambda (case) (getf case :name))
                                     (getf description :cases)))
             (loop for case in (getf description :cases)
                   for index from 0
                   always (and (eq (if (evenp index) :returns :signals)
                                   (getf case :outcome))
                               (clause-reads-back-p
                                (list (list '= (intern "ARG-0" (find-package
                                                               +home-package-name+))
                                            index))
                                (getf case :guard))
                               (eq t (getf case :guard-complete))
                               (clause-reads-back-p
                                (clause-forms (if (evenp index) :one :two))
                                (getf case :postconditions))
                               (if (evenp index)
                                   (and (null (getf case :state-post))
                                        (eq :not-applicable
                                            (getf case :state-post-complete)))
                                   (clause-reads-back-p (clause-forms :one)
                                                        (getf case :state-post)))))))))

(defun %cut-holds-p (length max-chars)
  "Return true when a precondition whose text is exactly LENGTH characters is
whole under a budget of MAX-CHARS or more, cut under less, and says which.

The form is built here rather than drawn: this case needs a printed length it
states, and the text it must print as is written beside it, never taken from
the printer under test."
  (multiple-value-bind (form text) (long-form length)
    (let* ((record (contract-record (contract-descriptor :pre :none)))
           (record (append (list :preconditions (list form))
                           (%without record :preconditions)))
           (description (%describe-record record max-chars)))
      (if (<= length max-chars)
          (and (equal text (getf description :preconditions))
               (eq t (getf description :preconditions-complete)))
          (and (null (getf description :preconditions-complete))
               (integerp (getf description :preconditions-omitted-chars))
               (plusp (getf description :preconditions-omitted-chars))
               ;; Cut, not absent: a clause that is not there answers
               ;; :NOT-APPLICABLE, which this one must not.
               (stringp (getf description :preconditions)))))))

(defun %without (plist key)
  "Return PLIST without KEY."
  (loop for (indicator value) on plist by #'cddr
        unless (eq indicator key) append (list indicator value)))

(defun %api-answering (record)
  "Return (values API CALLS) for a cl-spec whose FUNCTION-SPEC-DATA answers
RECORD for the fixture contract."
  (inspection-api :handles (append +required-handles+ +listing-handles+)
                  :registry (list :definitions (list :contract-a :spec-a))
                  :contract-record record))

(defun %describe-record (record max-chars)
  "Return the description of the contract RECORD, under MAX-CHARS."
  (%describe-function-spec (%api-answering record) (definition-symbol :contract-a)
                           :inspection-registry max-chars))

;;; ------------------------------------------------------------------------
;;; E. Digests

(defun %digest-of (record &key (property nil property-p) spec-record)
  "Return (values DIGEST COMPLETE CALLS) for DEFINITION-DIGEST over the
contract whose FUNCTION-SPEC-DATA is RECORD.  SPEC-RECORD is what the named
spec it references answers.  PROPERTY, when passed, is handed to
DEFINITION-DIGEST as the definition already read."
  (multiple-value-bind (api calls)
      (inspection-api :handles (append +required-handles+ +listing-handles+)
                      :registry (list :definitions (list :contract-a :spec-a))
                      :contract-record record
                      :spec-record spec-record)
    (multiple-value-bind (digest complete)
        (if property-p
            (definition-digest api (definition-symbol :contract-a) :inspection-registry
                               :property property :data-key :function-spec-data)
            (definition-digest api (definition-symbol :contract-a) :inspection-registry
                               :data-key :function-spec-data))
      (values digest complete calls))))

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator availability-case-generator ()
    "Draw a handle subset per operation and a backend state (DRAW-AVAILABILITY-CASE)."
    (draw-availability-case))
  (defspec availability-case list (:generator availability-case-generator))
  (defgenerator listing-case-generator ()
    "Draw a registry, handles, kind, package, tag and limit (DRAW-LISTING-CASE)."
    (draw-listing-case))
  (defspec listing-case list (:generator listing-case-generator))
  (defgenerator registration-case-generator ()
    "Draw a registry, a subject and a reader failure (DRAW-REGISTRATION-CASE)."
    (draw-registration-case))
  (defspec registration-case list (:generator registration-case-generator))
  (defgenerator contract-case-generator ()
    "Draw a declaration and a character budget (DRAW-CONTRACT-CASE)."
    (draw-contract-case))
  (defspec contract-case list (:generator contract-case-generator))
  (defgenerator digest-case-generator ()
    "Draw a record's metadata state and a schema version (DRAW-DIGEST-CASE)."
    (draw-digest-case))
  (defspec digest-case list (:generator digest-case-generator))

  (defproperty spec-inspection-operations-need-their-own-handles
      ((case availability-case))
    "An operation is available when the handles it needs are there, and not
otherwise.  Every trial runs every combination of the handles each of the six
operations needs, with unrelated handles drawn in beside them:
CONTRACT-OPERATION-MISSING names exactly the ones that are absent, so a
cl-spec that cannot run a contract can still describe one.  A backend is
available only as an object: a special bound to NIL, a reader that signals and
a missing reader are all unavailable, and none of them is an error -- reading
a registry needs no backend."
    (:about contract-operation-missing api-backend-available-p)
    (:kind :resolution)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key subsets backend noise) case
      (and (%backend-availability-holds-p backend)
           (every (lambda (row)
                    (destructuring-bind (operation present) row
                      (or (not (member operation '(:describe-contract :run-contract)))
                          (%operation-missing-holds-p operation present noise))))
                  subsets)
           (every #'%backend-availability-holds-p '(:object :none :signals :missing)))))

  (defproperty spec-inspection-listing-separates-capability-from-count
      ((case listing-case))
    "A listing keeps three things apart: what this cl-spec can enumerate, what
this request asked for, and how many are registered.  A kind that was not
asked for, or that cannot be listed, has no count -- never 0, which would say
the registry holds none.  The three listable flags and tag-filterable describe
the revision, not the request.  A package narrows by the home package of the
registered name; a tag narrows properties only, and its three states -- not
requested, known, and no such keyword in this image -- stay apart without
interning the unknown one.  A limit cuts the lists and sets truncated, and
changes no count.  Only the kinds asked for are enumerated, every reader is
handed the registry it was given, and nothing is run."
    (:about list-report)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key registry handles kind package tag limit) case
      (and (%listing-holds-p registry handles kind package tag limit)
           ;; Every kind, under the full set of handles, every trial.
           (every (lambda (one)
                    (%listing-holds-p registry +listing-handles+ one package tag limit))
                  '("specs" "properties" "function-specs" "both"))
           (%tag-states-hold-p registry))))

  (defproperty spec-inspection-registration-is-not-read-failure
      ((case registration-case))
    "What is registered and what could not be read are different answers.  A
reader that says the name is unknown gives not-registered; one that breaks for
its own reasons gives an internal error, never an absent registration; and
CL:UNDEFINED-FUNCTION means the contract's target is not defined only for a
contract -- out of a property or spec reader it is an adapter fault.  A symbol
report keeps the spec, the property, the contract and the (:about ...)
relations apart, says nothing-registered only about a lookup that worked, and
with include_runtime false reads no runtime at all and says why.  Nothing is
run.  Every trial runs the drawn subject under all four reader behaviours."
    (:about describe-report symbol-report)
    (:kind :resolution)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key registry subject failure kind) case
      (declare (ignore failure))
      (every (lambda (one)
               (%registration-holds-p registry subject one kind))
             '(nil :unknown :fails :undefined-function))))

  (defproperty spec-inspection-contract-declaration-survives-describe
      ((case contract-case))
    "A contract's declaration reaches the reader as it was written: the
arguments in order, each with its variable, kind -- :REQUIRED where version 1
omits the key -- supplied-p and keyword, and its spec node with an open range
end as * and a zero as \"0\"; the clauses as forms that can be pasted back,
one as itself and several joined by AND, with a clause that is not there
reported as not applicable; the cases in the order declared, each with its
guard, outcome, returns or signals, and its own clauses.  A clause of a stated
length is whole under a budget of that length and cut under one character
less, and says which.  Every trial checks the drawn declaration and both sides
of one budget."
    (:about %describe-function-spec)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key arguments pre post state-post cases returns signals
                           generator schema max-chars)
        case
      (let* ((descriptor (contract-descriptor :arguments arguments :pre pre :post post
                                              :state-post state-post :cases cases
                                              :returns returns :signals signals
                                              :generator generator :schema schema))
             (description (%contract-describe descriptor 8000)))
        (and (eq :ok (getf description :status))
             (%arguments-hold-p descriptor description)
             (%clauses-hold-p descriptor description)
             (%cases-hold-p descriptor description)
             (eq (and generator t) (and (getf description :argument-generator) t))
             (eq (and schema t) (and (getf description :argument-schema) t))
             (eq (and returns t) (and (getf description :returns) t))
             (eq (and signals t) (and (getf description :signals) t))
             (%cut-holds-p max-chars max-chars)
             (%cut-holds-p (1+ max-chars) max-chars)))))

  (defproperty spec-inspection-digest-comes-from-the-record-or-the-readers
      ((case digest-case))
    "A declaration's digest comes from the record when the record carries one,
and from the readers only when it does not.  A version 1 record whose digest
is complete is used as it stands, and no dependency reader is called for it.
A record whose digest is missing, incomplete or not a string, and one of a
version this cl-mcp does not know, give no digest at all -- never a digest
computed from the readers instead, which would report a definition as
unchanged on the strength of a reading the record refused.  A record with no
version key is the old shape, where the readers are the only source: it
digests, and a named spec it references changes that digest.  An explicitly
passed NIL definition is unread, and no reader is called to replace it."
    (:about definition-digest)
    (:kind :resolution)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key metadata version spec-change) case
      (let ((versioned (contract-record (contract-descriptor :metadata metadata
                                                             :reference t)))
            (unsupported (contract-record (contract-descriptor :version version
                                                               :reference t)))
            (old (contract-record (contract-descriptor :metadata :no-version
                                                       :reference t))))
        (and
         ;; A complete version 1 digest is the record's own, and nothing else
         ;; is read to produce it.
         (multiple-value-bind (digest complete calls)
             (%digest-of (contract-record (contract-descriptor :reference t)))
           (and (equal "fnv1a64-v1:00000000000000dd" digest)
                (eq t complete)
                (null (calls-of calls :spec-data))))
         ;; A record that cannot stand for itself gives no digest, and does
         ;; not fall back to the readers.
         (multiple-value-bind (digest complete calls) (%digest-of versioned)
           (if (eq metadata :complete)
               (and (stringp digest) (eq t complete))
               (and (null digest) (null complete) (null (calls-of calls :spec-data)))))
         ;; A version this cl-mcp does not know: the same answer.
         (multiple-value-bind (digest complete calls) (%digest-of unsupported)
           (and (null digest) (null complete) (null (calls-of calls :spec-data))))
         ;; The old shape digests from the readers, which it does read, and
         ;; the digest follows the spec it references.
         (multiple-value-bind (digest complete calls) (%digest-of old)
           (and (stringp digest) (eq t complete) (calls-of calls :spec-data)
                (not (equal digest
                            (%digest-of old :spec-record
                                        (spec-node :kind :range :type 'integer
                                                   :min 0 :max spec-change))))))
         ;; An explicit NIL definition is unread, and nothing is fetched to
         ;; replace it.
         (multiple-value-bind (digest complete calls) (%digest-of old :property nil)
           (and (null digest) (null complete)
                (null (calls-of calls :function-spec-data))
                (null (calls-of calls :spec-data)))))))))
