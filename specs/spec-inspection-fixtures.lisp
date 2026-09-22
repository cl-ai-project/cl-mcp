;;;; specs/spec-inspection-fixtures.lisp
;;;;
;;;; Descriptors and a recording cl-spec stub for the inspection properties
;;;; (specs/spec-inspection.lisp) and their fixed cases
;;;; (tests/spec-inspection-test.lisp).  Needs no cl-spec, so the fixed cases
;;;; run in the default suite.
;;;;
;;;; Inspection is what an agent reads before editing: which operations this
;;;; cl-spec can serve at all, what is registered, what a declaration says,
;;;; and which digest stands for it.  The danger is the false negative --
;;;; reporting "there is none" for something that could not be read -- so
;;;; every descriptor here keeps three answers apart: present, absent, and
;;;; unreadable.
;;;;
;;;; Expectations come from this file, stated once:
;;;;   +OPERATION-HANDLES+   which handles each operation needs
;;;;   +REQUIRED-HANDLES+    what a usable adapter needs at all
;;;;   a REGISTRY descriptor its definitions, their packages, tags and
;;;;                         :about relations
;;;;   a CONTRACT descriptor its arguments, clauses and cases, in order
;;;; None of them is read from cl-mcp's own +LISTING-KINDS+,
;;;; +CONTRACT-OPERATIONS+, +REQUIRED-FUNCTIONS+ or record shapes: a mistake
;;;; there must not update the expectation with it.
;;;;
;;;; INSPECTION-API builds a CL-SPEC-API holding exactly the handles it is
;;;; given, and records every reader call -- its key, the name asked for and
;;;; the registry it was handed -- so a check can say not only what came back
;;;; but what was read, and what was not.

(defpackage #:cl-mcp/specs/spec-inspection-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:make-cl-spec-api)
  (:export #:+operation-handles+
           #:+required-handles+
           #:+listing-handles+
           #:+home-package-name+
           #:+other-package-name+
           #:inspection-unknown-name
           #:inspection-failure
           #:definition-symbol
           #:definition-name
           #:draw-registry
           #:registry-names
           #:expected-listing
           #:inspection-api
           #:api-calls
           #:calls-of
           #:contract-record
           #:contract-descriptor
           #:draw-contract
           #:clause-forms
           #:clause-reads-back-p
           #:long-form
           #:spec-node
           #:draw-availability-case
           #:draw-listing-case
           #:draw-registration-case
           #:draw-contract-case
           #:draw-digest-case))

(in-package #:cl-mcp/specs/spec-inspection-fixtures)

(defparameter +home-package-name+ "CL-MCP/SPECS/SPEC-INSPECTION-FIXTURES"
  "The package the fixture definitions live in.")

(defparameter +other-package-name+ "CL-MCP-SPEC-INSPECTION-ELSEWHERE"
  "A second package, for names a package filter must leave out.")

(defpackage #:cl-mcp-spec-inspection-elsewhere
  (:use)
  (:export #:elsewhere-spec #:elsewhere-property #:elsewhere-contract))

(define-condition inspection-unknown-name (error)
  ()
  (:report "No such name is registered in the described registry.")
  (:documentation "Stands in for cl-spec's UNKNOWN-SPEC / UNKNOWN-PROPERTY /
UNKNOWN-FUNCTION-SPEC: the conditions that mean a name is not registered."))

(define-condition inspection-failure (error)
  ()
  (:report "The reader broke while reading a registered definition.")
  (:documentation "A reader failing for its own reasons.  Not a statement
about whether anything is registered, and must never be read as one."))

;;; ------------------------------------------------------------------------
;;; What each operation needs

(defparameter +operation-handles+
  '((:list-specs :list-specs)
    (:list-properties :list-properties)
    (:list-function-specs :list-function-specs :function-spec-data)
    (:describe-contract :function-spec-data)
    (:run-contract :function-spec-data :check-function)
    (:filter-by-tag :properties-with-tag))
  "The handles each operation needs beyond a usable adapter, restated from
spec-check's and spec-list's descriptions: listing a kind needs its own
enumerator, a contract's listing also needs the projection its entries read,
describing a contract needs that projection, running one needs the checker as
well, and narrowing properties by tag needs the tag reader.")

(defparameter +required-handles+
  '(:registry :generator-backend :semantic-data :spec-data :property-data
    :properties-for :run-property :backend-default-trials :result-status
    :result-trials :result-seed :result-profile :result-counterexample
    :result-shrunk-counterexample :result-condition :result-elapsed)
  "What an adapter needs before it can answer anything: the two specials and
the readers every path uses.  A stub carries these unless a case takes one
away.")

(defparameter +listing-handles+ '(:list-specs :list-properties :list-function-specs
                                  :function-spec-data :properties-with-tag)
  "The handles a listing may or may not have, drawn over in the listing case.")

(defun operation-handles (operation)
  "Return the handles OPERATION needs."
  (rest (or (assoc operation +operation-handles+)
            (error "No such operation ~S." operation))))

;;; ------------------------------------------------------------------------
;;; Definitions, as descriptors

(defparameter +definitions+
  '((:spec-a :kind :spec :package :home)
    (:spec-b :kind :spec :package :home)
    (:spec-elsewhere :kind :spec :package :other)
    (:property-a :kind :property :package :home :tags (:fast))
    (:property-b :kind :property :package :home :tags (:slow :fast))
    (:property-about :kind :property :package :home :tags () :about :contract-a)
    (:property-elsewhere :kind :property :package :other :tags (:fast))
    (:contract-a :kind :function-spec :package :home)
    (:contract-elsewhere :kind :function-spec :package :other))
  "Every definition a described registry may hold: its kind, the package its
name lives in, the tags a property carries and the symbol it is (:about ...).")

(defun definition-symbol (key)
  "Return the symbol a definition KEY names."
  (ecase key
    (:spec-a 'inspection-spec-a)
    (:spec-b 'inspection-spec-b)
    (:spec-elsewhere 'cl-mcp-spec-inspection-elsewhere:elsewhere-spec)
    (:property-a 'inspection-property-a)
    (:property-b 'inspection-property-b)
    (:property-about 'inspection-property-about)
    (:property-elsewhere 'cl-mcp-spec-inspection-elsewhere:elsewhere-property)
    (:contract-a 'inspection-contract-a)
    (:contract-elsewhere 'cl-mcp-spec-inspection-elsewhere:elsewhere-contract)))

(defun definition-name (key)
  "Return (PACKAGE-NAME SYMBOL-NAME) for a definition KEY."
  (let ((symbol (definition-symbol key)))
    (list (package-name (symbol-package symbol)) (symbol-name symbol))))

(defun definition-row (key)
  "Return the +DEFINITIONS+ row for KEY."
  (or (assoc key +definitions+) (error "No such definition ~S." key)))

(defun definition-kind (key)
  "Return the kind of definition KEY: :SPEC, :PROPERTY or :FUNCTION-SPEC."
  (getf (rest (definition-row key)) :kind))

(defun definition-package (key)
  "Return the package name definition KEY's symbol lives in."
  (if (eq :home (getf (rest (definition-row key)) :package))
      +home-package-name+
      +other-package-name+))

(defun definition-tags (key)
  "Return the tags definition KEY carries."
  (getf (rest (definition-row key)) :tags))

(defun draw-registry ()
  "Return a registry descriptor: which definitions are registered, drawn with
CL:RANDOM, always holding at least one of each kind in the home package."
  (let ((keys (loop for (key) in +definitions+
                    when (or (member key '(:spec-a :property-a :contract-a))
                             (zerop (random 2)))
                      collect key)))
    (list :definitions keys)))

(defun registry-names (registry kind &key package tag)
  "Return the definition keys REGISTRY holds of KIND, narrowed by the home
PACKAGE and, for properties, by TAG."
  (loop for key in (getf registry :definitions)
        when (and (eq kind (definition-kind key))
                  (or (null package) (string= package (definition-package key)))
                  (or (null tag) (member tag (definition-tags key))))
          collect key))

;;; ------------------------------------------------------------------------
;;; Contract declarations, as descriptors

(defun spec-node (&key (kind :type) name type min max generator target children)
  "Return one spec node as cl-spec's SPEC-DATA projects it.  A :REFERENCE node
names the spec it stands for under :TARGET, which is what the digest follows."
  (list :kind kind :name name :type type :min min :max max
        :generator generator :target target :children children
        :source-form (or type name target :spec) :source-location nil))

(defun long-form (length)
  "Return (values FORM TEXT) for a form whose printed text is exactly LENGTH
characters.

The form is a string of a's, so its printed length does not depend on which
package the printer prints it from -- a symbol would be written with whatever
package qualification the printer's own *PACKAGE* calls for -- and the text is
written here rather than measured by the printer under test."
  (let ((body (make-string (max 1 (- length 2)) :initial-element #\a)))
    (values body (format nil "\"~A\"" body))))

(defun clause-forms (shape)
  "Return the forms of a clause SHAPE: none, one, two, or one that is NIL."
  (ecase shape
    (:none '())
    (:one '((> x 0)))
    (:two '((> x 0) (< x 100)))
    (:present-nil '(nil))))

(defun clause-reads-back-p (forms text)
  "Return true when TEXT reads back as the clause FORMS mean: one form as
itself, several joined by AND.

A clause is printed package-qualified, from whichever package the projector
prints in, so its text is compared by reading it rather than by matching
characters.  Reading is safe here because every form compared this way was
written in this file: *READ-EVAL* is off, and nothing from outside reaches
the reader."
  (and (stringp text)
       (let ((*read-eval* nil)
             (*package* (find-package +home-package-name+)))
         (multiple-value-bind (form position)
             (ignore-errors (read-from-string text))
           (and position
                (equal form (if (rest forms) (cons 'and forms) (first forms))))))))

(defun contract-descriptor (&key (arguments '((:required) (:optional :supplied-p t)
                                              (:key :keyword :size)))
                              (pre :one) (post :two) (state-post :none)
                              (capture t) (cases :none) (returns t) (signals nil)
                              (generator t) (schema nil) (documentation t)
                              (source :short) (version 1) (metadata :complete)
                              (record-kind :definition) (entity-kind :function-spec)
                              reference)
  "Return a descriptor for one contract declaration.  ARGUMENTS is a list of
(KIND &key SUPPLIED-P KEYWORD) in the order they are declared; the clause keys
take a CLAUSE-FORMS shape; CASES is :NONE or a list of case names."
  (list :arguments arguments :pre pre :post post :state-post state-post
        :capture capture :cases cases :returns returns :signals signals
        :generator generator :schema schema :documentation documentation
        :source source :version version :metadata metadata
        :record-kind record-kind :entity-kind entity-kind :reference reference))

(defun %argument-records (arguments &key reference)
  "Return ARGUMENTS as FUNCTION-SPEC-DATA records them: a required argument
omits :KIND, and each carries its own spec node.  REFERENCE makes the first
argument's spec a reference to the named spec instead, which the digest's
legacy path follows."
  (loop for (kind . options) in arguments
        for index from 0
        for variable = (intern (format nil "ARG-~D" index)
                               (find-package +home-package-name+))
        collect (append (list :variable variable
                              :spec (if (and reference (zerop index))
                                        (spec-node :kind :reference
                                                   :target (definition-symbol :spec-a))
                                        (spec-node :kind :range :type 'integer
                                                   :min (if (evenp index) 0 :unbounded)
                                                   :max index)))
                        (unless (eq kind :required) (list :kind kind))
                        (when (getf options :supplied-p)
                          (list :supplied-p (intern (format nil "ARG-~D-P" index)
                                                    (find-package +home-package-name+))))
                        (when (getf options :keyword)
                          (list :keyword (getf options :keyword))))))

(defun %case-records (names)
  "Return one record per case name, in order, each with its own guard,
outcome and clauses."
  (loop for name in names
        for index from 0
        collect (list :name name
                      :documentation (format nil "Case ~A." name)
                      :when (list '= 'arg-0 index)
                      :outcome (if (evenp index) :returns :signals)
                      :returns (when (evenp index)
                                 (spec-node :kind :type :type 'integer))
                      :signals (unless (evenp index)
                                 (spec-node :kind :type :type 'error))
                      :postconditions (clause-forms (if (evenp index) :one :two))
                      :post-value-variables (list (intern "RESULT"
                                                          (find-package
                                                           +home-package-name+)))
                      :state-post (clause-forms (if (evenp index) :none :one)))))

(defun contract-record (descriptor &key (name (definition-symbol :contract-a)))
  "Return the v1 FUNCTION-SPEC-DATA record DESCRIPTOR describes.

METADATA :COMPLETE carries the seven keys a version 1 record requires;
:MISSING-DIGEST leaves one out.  A record with no :SCHEMA-VERSION key at all
is the old shape, which the digest's legacy path reads."
  (destructuring-bind (&key arguments pre post state-post capture cases returns
                         signals generator schema documentation source version
                         metadata record-kind entity-kind reference)
      descriptor
    (append
     (unless (eq metadata :no-version)
       (append (list :schema-version version
                     :record-kind record-kind :entity-kind entity-kind)
               (unless (eq metadata :missing-digest)
                 (list :definition-digest "fnv1a64-v1:00000000000000dd"))
               (list :definition-digest-complete (not (eq metadata :incomplete-digest))
                     :definition-digest-covers :declaration-and-registered-dependencies
                     :capabilities (list :generation :available :shrinking :available
                                         :instrumentation :none))))
     (list :name name
           :arguments (%argument-records arguments :reference reference)
           :argument-generator (when generator
                                 (intern "SCRIPTED-ARGUMENTS"
                                         (find-package +home-package-name+)))
           :argument-schema (when schema
                              (spec-node :kind :tuple
                                         :generator (intern "TUPLE-GENERATOR"
                                                            (find-package
                                                             +home-package-name+))))
           :returns (when returns (spec-node :kind :type :type 'integer))
           :signals (when signals (spec-node :kind :type :type 'error))
           :post-value-variables (list (intern "RESULT" (find-package
                                                         +home-package-name+)))
           :preconditions (clause-forms pre)
           :postconditions (clause-forms post)
           :state-post (clause-forms state-post)
           :capture (when capture
                      (list (list :name (intern "BEFORE" (find-package
                                                          +home-package-name+))
                                  :form '(balance account))))
           :case-selection (unless (eq cases :none) :exclusive)
           :cases (unless (eq cases :none) (%case-records cases))
           :documentation (when documentation "A contract of this file's own.")
           :source-form (ecase source
                          (:short '(defspec-function inspection-contract-a))
                          (:long (long-form 400)))
           :source-location (list :file "spec-inspection.lisp" :package
                                  +home-package-name+)))))

;;; ------------------------------------------------------------------------
;;; The recording stub

(defun inspection-api (&key (handles (append +required-handles+ +listing-handles+))
                         registry (registry-object (list :inspection-registry))
                         (contract (contract-descriptor))
                         contract-record property-record spec-record
                         reader-failures backend)
  "Return (values API CALLS REGISTRY-OBJECT) for a cl-spec holding HANDLES.

REGISTRY is a registry descriptor.  CONTRACT-RECORD, when given, is the raw
record FUNCTION-SPEC-DATA answers, instead of the one CONTRACT describes;
:ANSWERS-NIL makes that reader answer NIL, which is a projection that failed
and not a contract with nothing in it.
READER-FAILURES maps a handle key to
:UNKNOWN (the name is not registered), :FAILS (the reader breaks for its own
reasons) or :UNDEFINED-FUNCTION.  BACKEND is :OBJECT, :NONE (the special is
bound to NIL) or :SIGNALS.  CALLS' CAR collects (KEY NAME REGISTRY), most
recent first."
  (let ((calls (list nil)))
    (labels ((note (key name given) (push (list key name given) (car calls)))
             (registered-p (name kind)
               (loop for key in (getf registry :definitions)
                     thereis (and (eq kind (definition-kind key))
                                  (eq name (definition-symbol key)))))
             (fail (key)
               (ecase (getf reader-failures key)
                 ((nil) nil)
                 (:unknown (error 'inspection-unknown-name))
                 (:fails (error 'inspection-failure))
                 (:undefined-function (error 'undefined-function :name 'no-such-target))))
             (names-of (kind)
               (mapcar #'definition-symbol (registry-names registry kind)))
             (handle (key function) (when (member key handles) (list key function))))
      (values
       (make-cl-spec-api
        :version "0.1.0"
        :system-directory "/tmp/cl-spec/"
        :classes (list :unknown-spec 'inspection-unknown-name
                       :unknown-property 'inspection-unknown-name
                       :unknown-function-spec 'inspection-unknown-name)
        :functions
        (append
         (handle :registry (lambda () registry-object))
         (handle :generator-backend
                 (lambda ()
                   (ecase (or backend :object)
                     (:object :inspection-backend)
                     (:none nil)
                     (:signals (error 'inspection-failure)))))
         (handle :semantic-data
                 (lambda (symbol &key ((:registry given)))
                   (note :semantic-data symbol given)
                   (fail :semantic-data)
                   (list :symbol symbol
                         :package (package-name (symbol-package symbol))
                         :spec (when (registered-p symbol :spec) symbol)
                         :function-spec (when (registered-p symbol :function-spec) symbol)
                         :property (when (registered-p symbol :property) symbol)
                         :properties-about
                         (loop for key in (getf registry :definitions)
                               for about = (and (eq :property (definition-kind key))
                                                (getf (rest (definition-row key)) :about))
                               when (and about (eq symbol (definition-symbol about)))
                                 collect (definition-symbol key)))))
         (handle :property-data
                 (lambda (name &key ((:registry given)))
                   (note :property-data name given)
                   (fail :property-data)
                   (unless (registered-p name :property) (error 'inspection-unknown-name))
                   (or property-record
                       (list :name name :kind :invariant :targets nil
                             :tags (definition-tags
                                    (find name (getf registry :definitions)
                                          :key #'definition-symbol))
                             :documentation "A property of this file's own."
                             :trials (list :smoke 5 :normal 25)
                             :arguments nil :body (list t)
                             :source-form (list 'defproperty name)
                             :source-location nil :metadata (list :shrink t)))))
         (handle :function-spec-data
                 (lambda (name &key ((:registry given)))
                   (note :function-spec-data name given)
                   (fail :function-spec-data)
                   (unless (registered-p name :function-spec)
                     (error 'inspection-unknown-name))
                   (cond ((eq :answers-nil contract-record) nil)
                         (contract-record contract-record)
                         (t (contract-record contract :name name)))))
         (handle :spec-data
                 (lambda (name &key ((:registry given)))
                   (note :spec-data name given)
                   (fail :spec-data)
                   (unless (registered-p name :spec) (error 'inspection-unknown-name))
                   (or spec-record
                       (spec-node :kind :range :name name :type 'integer :min 0 :max 10))))
         (handle :properties-for
                 (lambda (symbol &optional given)
                   (note :properties-for symbol given)
                   nil))
         (handle :list-specs
                 (lambda (given)
                   (note :list-specs nil given)
                   (fail :list-specs)
                   (names-of :spec)))
         (handle :list-properties
                 (lambda (given)
                   (note :list-properties nil given)
                   (fail :list-properties)
                   (names-of :property)))
         (handle :list-function-specs
                 (lambda (given)
                   (note :list-function-specs nil given)
                   (fail :list-function-specs)
                   (names-of :function-spec)))
         (handle :properties-with-tag
                 (lambda (tag given)
                   (note :properties-with-tag tag given)
                   (fail :properties-with-tag)
                   (mapcar #'definition-symbol
                           (registry-names registry :property :tag tag))))
         (handle :check-function (lambda (name &rest arguments)
                                   (note :check-function name arguments)
                                   (error "A read ran a contract.")))
         (handle :run-property (lambda (name &rest arguments)
                                 (note :run-property name arguments)
                                 (error "A read ran a property.")))
         (handle :backend-default-trials (lambda (backend)
                                           (declare (ignore backend))
                                           25))
         (loop for key in '(:result-status :result-trials :result-seed :result-profile
                            :result-counterexample :result-shrunk-counterexample
                            :result-condition :result-elapsed)
               append (handle key (constantly nil)))))
       calls
       registry-object))))

(defun api-calls (calls)
  "Return the calls CALLS recorded, oldest first."
  (reverse (car calls)))

(defun calls-of (calls key)
  "Return the calls CALLS recorded for the handle KEY, oldest first."
  (remove-if-not (lambda (call) (eq key (first call))) (api-calls calls)))

;;; ------------------------------------------------------------------------
;;; Expected listings

(defun expected-listing (registry handles &key kind package tag)
  "Return what a listing of KIND should say, as a plist: :SPECS, :PROPERTIES
and :FUNCTION-SPECS counts (NIL for a kind not asked for or not listable),
the three listable flags, whether a tag could be applied, and whether any
asked-for kind can be listed at all."
  (flet ((has (key) (and (member key handles) t))
         (wanted (this) (or (string= kind "both") (string= kind this))))
    (let (;; Resolved, never interned: a tag this image does not know cannot
          ;; be on any property, and asking must not make it exist.
          (tag-keyword (and tag (find-symbol (string-upcase tag) "KEYWORD")))
          (specs-listable (has :list-specs))
          (properties-listable (has :list-properties))
          (function-specs-listable (and (has :list-function-specs)
                                        (has :function-spec-data)))
          (tag-filterable (or (null tag) (has :properties-with-tag)))
          (want-specs (wanted "specs"))
          (want-properties (wanted "properties"))
          (want-function-specs (wanted "function-specs")))
      (list :specs (when (and want-specs specs-listable)
                     (length (registry-names registry :spec :package package)))
            :properties (when (and want-properties properties-listable tag-filterable)
                          (if (and tag (null tag-keyword))
                              0
                              (length (registry-names registry :property :package package
                                                               :tag tag-keyword))))
            :function-specs (when (and want-function-specs function-specs-listable)
                              (length (registry-names registry :function-spec
                                                               :package package)))
            :specs-listable specs-listable
            :properties-listable properties-listable
            :function-specs-listable function-specs-listable
            :tag-filterable tag-filterable
            :tag-applied (and tag want-properties properties-listable tag-filterable t)
            :reachable (or (and want-specs specs-listable)
                           (and want-properties properties-listable)
                           (and want-function-specs function-specs-listable))))))

;;; ------------------------------------------------------------------------
;;; Draws

(defun %pick (list) (nth (random (length list)) list))

(defun draw-availability-case ()
  "Return an availability case: a handle subset for every operation, and a
backend state."
  (list :subsets (loop for (operation) in +operation-handles+
                       collect (list operation
                                     (loop for handle in (operation-handles operation)
                                           when (zerop (random 2)) collect handle)))
        :backend (%pick '(:object :none :signals))
        :noise (loop for handle in +listing-handles+
                     when (zerop (random 2)) collect handle)))

(defun draw-listing-case ()
  "Return a listing case: a registry, a handle subset, a kind, and the
package, tag and limit to narrow with."
  (list :registry (draw-registry)
        :handles (loop for handle in +listing-handles+
                       when (zerop (random 3)) collect handle)
        :kind (%pick '("specs" "properties" "function-specs" "both"))
        :package (%pick (list nil +home-package-name+ +other-package-name+))
        :tag (%pick '(nil "fast" "slow"))
        :limit (%pick '(1 2 200))))

(defun draw-registration-case ()
  "Return a registration case: a registry, the definition to ask about, and
how the reader behaves."
  (list :registry (draw-registry)
        :subject (%pick (mapcar #'first +definitions+))
        :failure (%pick '(nil :unknown :fails :undefined-function))
        :kind (%pick '("property" "spec" "function-spec"))))

(defun draw-contract-case ()
  "Return a contract case: a declaration descriptor with drawn arguments,
clauses and cases, and the character budget to describe it under."
  (list :arguments (loop repeat (1+ (random 3))
                         collect (%pick '((:required) (:optional :supplied-p t)
                                          (:key :keyword :size) (:rest))))
        :pre (%pick '(:none :one :two :present-nil))
        :post (%pick '(:none :one :two))
        :state-post (%pick '(:none :one))
        :cases (%pick '(:none (:small) (:small :large) (:small :large :huge)))
        :returns (zerop (random 2))
        :signals (zerop (random 2))
        :generator (zerop (random 2))
        :schema (zerop (random 2))
        :max-chars (+ 40 (random 200))))

(defun draw-digest-case ()
  "Return a digest case: the metadata state of a versioned record, and a
schema version for the unsupported rows."
  (list :metadata (%pick '(:complete :missing-digest :incomplete-digest))
        :version (%pick '(2 3 17))
        :spec-change (random 1000)))
