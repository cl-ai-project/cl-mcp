;;;; specs/check-routing-fixtures.lisp
;;;;
;;;; Descriptors, stub APIs and a recording spy for the routing properties
;;;; (specs/check-routing.lisp) and their fixed cases
;;;; (tests/check-routing-test.lisp).  Needs no cl-spec, so the fixed cases run
;;;; in the default suite.
;;;;
;;;; "Routing" is what spec-check does between a request and a runner: which
;;;; target arguments it accepts, how a seed is read, which definitions a
;;;; selection names, which trial budget a run is given, and how a definition
;;;; digest is compared.  Each expectation here comes from a policy stated
;;;; once, as data, from spec-check's published description -- never from the
;;;; functions under test.
;;;;
;;;; A registry is described, not built: a REGISTRY descriptor says which
;;;; definitions exist and how they relate (a Function Spec for ROUTING-F, a
;;;; property named ROUTING-F, properties (:about ROUTING-F), an unrelated
;;;; property), and REGISTRY-API answers cl-spec's readers from it.  The
;;;; symbols are a fixed set defined with this file, plus one same-named
;;;; symbol in a second package made at load time; nothing is interned per
;;;; trial, and a name meant to be unknown is only ever a string.
;;;;
;;;; SPY-API is the recording stand-in for a whole cl-spec used by the fixed
;;;; CHECK-REPORT tests: its runners take only the keywords the real ones take
;;;; and signal on any other, record their raw argument lists, and record the
;;;; registry and backend they see in the thread they run on.  That thread
;;;; does not see the caller's dynamic bindings, so the record lives in a
;;;; closure, and the specials the adapter is meant to bind there are named in
;;;; the API's :SPECIALS.

(defpackage #:cl-mcp/specs/check-routing-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:make-cl-spec-api)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:make-result-record
                #:record-with
                #:decimal-string)
  (:export #:+fixture-package-name+
           #:+elsewhere-package-name+
           #:routing-f
           #:routing-p1
           #:routing-p2
           #:routing-p3
           #:routing-u
           #:routing-unknown-name
           #:*spy-backend*
           #:*spy-registry*
           #:target-symbol
           #:designator
           #:target-error-expected-p
           #:draw-target-case
           #:unknown-profile-name
           #:+required-seeds+
           #:draw-seed-text-case
           #:registry-api
           #:registered-properties
           #:expected-selection
           #:+selection-requests+
           #:draw-registry
           #:draw-selection-case
           #:budget-inputs
           #:expected-budget
           #:draw-budget-case
           #:digest-plist
           #:expected-match
           #:draw-digest-case
           #:spy-api
           #:spy-calls
           #:spy-run-calls
           #:contract-definition))

(in-package #:cl-mcp/specs/check-routing-fixtures)

(defparameter +fixture-package-name+ "CL-MCP/SPECS/CHECK-ROUTING-FIXTURES"
  "The package the fixture symbols live in, as a designator names it.")

(defparameter +elsewhere-package-name+ "CL-MCP-CHECK-ROUTING-ELSEWHERE"
  "A second package holding a symbol named ROUTING-F with nothing registered
under it: the same name, a different symbol.")

(defpackage #:cl-mcp-check-routing-elsewhere
  (:use)
  (:export #:routing-f))

(define-condition routing-unknown-name (error)
  ()
  (:report "No such name is registered in the described registry.")
  (:documentation "Stands in for cl-spec's UNKNOWN-SPEC / UNKNOWN-PROPERTY."))

(defvar *spy-backend* :global-backend
  "The special SPY-API names as cl-spec's generator backend.  Its global value
is never the backend a check-report call captured, so a run thread that reads
it unbound by the adapter shows as a mismatch.")

(defvar *spy-registry* :global-registry
  "The special SPY-API names as cl-spec's registry, for the same reason.")

;;; ------------------------------------------------------------------------
;;; Targets and designators

(defun target-symbol (target)
  "Return the symbol a TARGET keyword stands for, or NIL for a name that must
stay unknown (:MISSING-SYMBOL, :MISSING-PACKAGE)."
  (ecase target
    (:f 'routing-f)
    (:p1 'routing-p1)
    (:p2 'routing-p2)
    (:p3 'routing-p3)
    (:u 'routing-u)
    (:elsewhere-f 'cl-mcp-check-routing-elsewhere:routing-f)
    ((:missing-symbol :missing-package) nil)))

(defun designator (target form)
  "Return (values DESIGNATOR PACKAGE) naming TARGET: package-qualified when
FORM is :QUALIFIED, or a bare name with its package beside it when
:UNQUALIFIED.  An unknown target's name is built as text and never interned."
  (multiple-value-bind (package name)
      (case target
        (:missing-symbol (values +fixture-package-name+ "ROUTING-NOWHERE"))
        (:missing-package (values "CL-MCP-CHECK-ROUTING-NO-SUCH-PACKAGE" "ROUTING-F"))
        (t (let ((symbol (target-symbol target)))
             (values (package-name (symbol-package symbol)) (symbol-name symbol)))))
    (ecase form
      (:qualified (values (format nil "~A::~A" package name) nil))
      (:unqualified (values name package)))))

;;; ------------------------------------------------------------------------
;;; A. Target arguments

(defun target-error-expected-p (property symbol function trials profile)
  "Return true when spec-check's target arguments should be refused.

The rule, as spec-check's description states it: exactly one of property,
symbol and function; trials only with function, because a property takes its
count from its own :TRIALS table; profile only with property or symbol,
because a contract has no :TRIALS table to select from.  Every other
combination is valid."
  (let ((given (count-if-not #'null (list property symbol function))))
    (or (/= 1 given)
        (and trials (not function))
        (and profile function))))

(defparameter +profile-spellings+ '("normal" "NORMAL" "Normal" "smoke" "SMOKE" "Smoke")
  "Spellings of the two profiles every bundle property declares.")

(defun unknown-profile-name ()
  "Return a profile name no loaded code interns, drawn with CL:RANDOM.  It is
built as a string, so drawing it interns nothing."
  (format nil "cl-mcp-routing-no-such-profile-~36R" (random (expt 36 12))))

(defun draw-target-case ()
  "Return a target case: a designator for each target argument, a trial count
and a profile to put beside them, known profile spellings, and unknown ones."
  (list :names (loop repeat 3
                     collect (designator (nth (random 6) '(:f :p1 :p2 :p3 :u :elsewhere-f))
                                         :qualified))
        :trials (1+ (random 1000000))
        :profile (nth (random (length +profile-spellings+)) +profile-spellings+)
        :known (loop repeat 3
                     collect (nth (random (length +profile-spellings+)) +profile-spellings+))
        :unknown (loop repeat 3 collect (unknown-profile-name))))

;;; ------------------------------------------------------------------------
;;; B. Seed text

(defparameter +required-seeds+
  (list 0 1 9 10
        (1- (expt 2 53)) (expt 2 53) (1+ (expt 2 53))
        (1- (expt 2 62)) (expt 2 62) (1+ (expt 2 62))
        (1- (expt 2 64)) (+ (expt 10 40) 7))
  "Seeds every trial reads: the smallest, the binary64 edge, both sides of
cl-spec's 2^62 draw bound, the 64-bit edge, and one far past all of them.")

(defun %corruptions (text)
  "Return spellings of the seed TEXT that are not decimal digits alone."
  (list (concatenate 'string "+" text)
        (concatenate 'string "-" text)
        (concatenate 'string " " text)
        (concatenate 'string text " ")
        (concatenate 'string text (string #\Tab))
        (concatenate 'string text "e3")
        (concatenate 'string text ".0")
        (concatenate 'string "#x" text)
        (concatenate 'string text "/1")
        (concatenate 'string "#.(+ " text " 1)")
        (concatenate 'string "1," text)))

(defun draw-seed-text-case ()
  "Return a seed case: drawn seeds below 2^128, each with zero to three
leading zeros, and the spellings of one of them that must be refused."
  (let ((seeds (loop repeat 4 collect (random (expt 2 128)))))
    (list :seeds (loop for seed in seeds
                       collect (list :seed seed :zeros (random 4)))
          :refused (%corruptions (decimal-string (first seeds))))))

;;; ------------------------------------------------------------------------
;;; C. Registries and selection

(defparameter +about-targets+ '(:p1 :p2 :p3)
  "The properties a registry may register (:about ROUTING-F).")

(defun registered-properties (registry)
  "Return the property targets REGISTRY registers: its :ABOUT ones, ROUTING-F
itself when a property of that name exists, and the unrelated one."
  (append (getf registry :about)
          (and (getf registry :own-property) (list :f))
          (and (getf registry :unrelated) (list :u))))

(defun %routing-for (registry symbol)
  "Return what cl-spec's SEMANTIC-DATA says about SYMBOL under REGISTRY."
  (let ((f (eq symbol (target-symbol :f)))
        (registered (mapcar #'target-symbol (registered-properties registry))))
    (list :symbol symbol
          :package (package-name (symbol-package symbol))
          :spec nil
          :function-spec (and f (getf registry :contract) symbol)
          :property (and (member symbol registered) symbol)
          :properties-about (and f (mapcar #'target-symbol (getf registry :about))))))

(defun contract-definition (name &key (preconditions '((> x 0))) case-selection
                                   (digest "fnv1a64-v1:00000000000000cc"))
  "Return a v1 definition record for the contract of NAME."
  (list :schema-version 1 :record-kind :definition :entity-kind :function-spec
        :definition-digest digest :definition-digest-complete t
        :definition-digest-covers :declaration-and-registered-dependencies
        :capabilities (list :generation :available :shrinking :available
                            :instrumentation :none)
        :name name :arguments nil :preconditions preconditions
        :case-selection case-selection :cases nil))

(defun %property-definition (name &key (trials (list :smoke 5 :normal 25)))
  "Return a property's definition as cl-spec's PROPERTY-DATA projects it."
  (list :name name :kind :invariant :targets (list (target-symbol :f)) :tags nil
        :documentation "A fixture property." :trials (copy-list trials)
        :arguments nil :body (list t) :source-form (list 'defproperty name)
        :source-location nil :metadata (list :shrink t)))

(defun registry-api (registry)
  "Return (values API CALLS REGISTRY-OBJECT) answering cl-spec's readers from
the REGISTRY descriptor.

REGISTRY-OBJECT is the registry a caller should hand the readers.  A reader
given any other object answers from an empty registry, so a registry that was
not passed through shows as missing definitions.  CALLS is a cons whose CAR
collects (KEY NAME REGISTRY-ARGUMENT), most recent first, for every handle
called -- the runners included, which a selection must never call."
  (let ((calls (list nil))
        (registry-object (list :described-registry))
        (empty (list :about nil)))
    (labels ((note (key name given)
               (push (list key name given) (car calls)))
             (described (given)
               (if (eq given registry-object) registry empty)))
      (values
       (make-cl-spec-api
        :version "0.1.0"
        :system-directory "/tmp/cl-spec/"
        :classes (list :unknown-spec 'routing-unknown-name
                       :unknown-property 'routing-unknown-name)
        :functions
        (list :registry (lambda () registry-object)
              :generator-backend (lambda () :described-backend)
              :backend-default-trials (lambda (backend) (declare (ignore backend)) 25)
              :semantic-data
              (lambda (symbol &key ((:registry given)))
                (note :semantic-data symbol given)
                (let ((descriptor (described given)))
                  (when (eq :fails (getf descriptor :lookup))
                    (error "The reverse index could not be read."))
                  (%routing-for descriptor symbol)))
              :property-data
              (lambda (name &key ((:registry given)))
                (note :property-data name given)
                (unless (member name (mapcar #'target-symbol
                                             (registered-properties (described given))))
                  (error 'routing-unknown-name))
                (%property-definition name))
              :function-spec-data
              (lambda (name &key ((:registry given)))
                (note :function-spec-data name given)
                (unless (and (eq name (target-symbol :f))
                             (getf (described given) :contract))
                  (error 'routing-unknown-name))
                (contract-definition name))
              :run-property
              (lambda (name &rest arguments)
                (note :run-property name (getf arguments :registry))
                (error "A selection ran a property."))
              :check-function
              (lambda (name &rest arguments)
                (note :check-function name (getf arguments :registry))
                (error "A selection ran a contract."))
              :check-rejected (constantly 0)))
       calls
       registry-object))))

(defparameter +selection-requests+
  '((:property :f) (:property :p1) (:property :p2) (:property :p3) (:property :u)
    (:property :elsewhere-f) (:property :missing-symbol) (:property :missing-package)
    (:symbol :f) (:symbol :elsewhere-f) (:symbol :missing-symbol)
    (:symbol :missing-package)
    (:function :f) (:function :elsewhere-f) (:function :missing-symbol)
    (:function :missing-package))
  "Every request a selection case tries: each selection kind against the
subject, its relations, the same name elsewhere, and two names that resolve to
nothing.")

(defun expected-selection (registry kind target)
  "Return what selecting TARGET by KIND should give under the REGISTRY
descriptor, as a plist: :ERROR, a status, or :NAMES, :KIND, :MODE and the
coverage keys.  From the registration relations alone: property= names one
registered property; symbol= names the properties (:about) the subject and
nothing else, leaving a contract and a same-named property as not run;
function= names the contract only, leaving the related properties as not run."
  (let ((symbol (target-symbol target))
        (f (eq target :f))
        (lookup-ok (not (eq :fails (getf registry :lookup))))
        (about (mapcar #'target-symbol (getf registry :about))))
    (cond
      ((null symbol) (list :error :unresolved-symbol))
      ((eq kind :property)
       (if (member target (registered-properties registry))
           (list :names (list symbol) :kind :property :mode "explicit"
                 :requested-key :property)
           (list :error :not-registered)))
      ((eq kind :symbol)
       (cond ((not lookup-ok) (list :error :internal-error))
             (f (list :names about :kind :property :mode "about" :requested-key :symbol
                      :contract-not-run (and (getf registry :contract) symbol)
                      :own-property-not-run (and (getf registry :own-property) symbol)))
             (t (list :names nil :kind :property :mode "about" :requested-key :symbol
                      :contract-not-run nil :own-property-not-run nil))))
      (t
       (if (and f (getf registry :contract))
           (list :names (list symbol) :kind :contract :mode "contract"
                 :requested-key :function
                 :properties-not-run (and lookup-ok about)
                 :own-property-not-run (and lookup-ok (getf registry :own-property) symbol)
                 :properties-not-run-read lookup-ok)
           (list :error :not-registered))))))

(defun draw-registry ()
  "Return a registry descriptor: whether ROUTING-F has a contract and a
same-named property, which of P1-P3 are (:about ROUTING-F) and in which order
they were registered, whether the unrelated property exists, and whether the
reverse index can be read."
  (let ((about (loop for target in +about-targets+
                     when (zerop (random 2)) collect target)))
    (list :contract (zerop (random 2))
          :own-property (zerop (random 2))
          :about (let ((items (coerce about 'vector)))
                   (loop for i from (1- (length items)) downto 1
                         do (rotatef (aref items i) (aref items (random (1+ i)))))
                   (coerce items 'list))
          :unrelated (zerop (random 2))
          :lookup (if (zerop (random 5)) :fails :ok))))

(defun draw-selection-case ()
  "Return a selection case: a drawn registry, the same registry with a
contract, a same-named property and a readable index forced on, and a
designator form for each request."
  (let ((registry (draw-registry)))
    (list :registry registry
          :full (list* :contract t :own-property t :lookup :ok
                       (loop for (key value) on registry by #'cddr
                             unless (member key '(:contract :own-property :lookup))
                               append (list key value)))
          :forms (loop repeat (length +selection-requests+)
                       collect (if (zerop (random 2)) :qualified :unqualified)))))

;;; ------------------------------------------------------------------------
;;; D. Trial budgets

(defun budget-inputs (&key kind table (backend :present) (reader :value) (default 150))
  "Return (values FACTS BACKEND API CALLS) for one budget derivation.

KIND is :PROPERTY or :CONTRACT; TABLE a property's :TRIALS plist, which a
contract's facts never carry.  BACKEND :PRESENT hands over a fresh backend
object, :ABSENT none.  READER says what the backend-default reader does:
answers DEFAULT, answers NIL, signals, or is missing.  CALLS is a cons whose
CAR collects the backend object each call of the reader received."
  (let* ((calls (list nil))
         (backend-object (and (eq backend :present) (list :budget-backend)))
         (facts (if (eq kind :property)
                    (list :argument-count 1 :kind :property :shrink-enabled t
                          :trials-table (copy-list table) :data nil :known t)
                    (list :argument-count 1 :kind :contract :shrink-enabled t
                          :trials-table nil :precondition-p t :declares-cases nil
                          :data nil :known t)))
         (functions (unless (eq reader :missing)
                      (list :backend-default-trials
                            (lambda (given)
                              (push given (car calls))
                              (ecase reader
                                (:value default)
                                (:nil nil)
                                (:signals (error "The backend default could not be read."))))))))
    (values facts backend-object (make-cl-spec-api :functions functions) calls)))

(defun expected-budget (&key kind table profile requested backend reader default)
  "Return (values BUDGET SOURCE) the policy gives: a property's own entry for
the profile, 0 included; else the backend default when it can be read.  A
contract's explicit trials; else the backend default when it can be read.
Nothing readable is NIL with source \"unknown\" -- never a made-up number."
  (let ((readable-default (and (eq backend :present) (eq reader :value) default)))
    (multiple-value-bind (entry present) (%plist-value table profile)
      (cond ((and (eq kind :property) present) (values entry "property-profile"))
            ((and (eq kind :contract) requested) (values requested "requested"))
            (readable-default (values readable-default "backend-default"))
            (t (values nil "unknown"))))))

(defun %plist-value (plist key)
  "Return (values VALUE PRESENT-P) for KEY in PLIST, stepping by pairs."
  (loop for (indicator value) on plist by #'cddr
        when (eq indicator key) do (return (values value t))
        finally (return (values nil nil))))

(defun draw-budget-case ()
  "Return a budget case: distinct ranges for a profile entry (0 to 40), the
backend default (100 to 199) and explicit trials (1000 to 1999), so taking one
for another shows."
  (list :entry (random 41)
        :other-entry (random 41)
        :default (+ 100 (random 100))
        :requested (+ 1000 (random 1000))))

;;; ------------------------------------------------------------------------
;;; E. Digests

(defun digest-plist (value complete)
  "Return a digest as the adapter carries one: VALUE or NIL, and whether its
input was complete."
  (list :value (and value (copy-seq value)) :complete complete :covers :property))

(defun expected-match (value complete expected)
  "Return the answer the four-way comparison should give: not checked when
nothing was expected, unknown when there is no complete digest to compare,
true or false otherwise."
  (cond ((null expected) :not-checked)
        ((or (null value) (not complete)) :unknown)
        ((string= value expected) :true)
        (t :false)))

(defun %hex-digest ()
  "Return a digest string of cl-spec's form with sixteen drawn hex digits."
  (format nil "fnv1a64-v1:~(~16,'0X~)" (random (expt 16 16))))

(defun draw-digest-case ()
  "Return a digest case: a drawn digest, another differing from it in one
hex digit (never only in case), and the position changed."
  (let* ((digest (%hex-digest))
         (position (+ 11 (random 16)))
         (other (copy-seq digest)))
    (setf (char other position)
          (let ((digits "0123456789abcdef"))
            (char digits (mod (1+ (+ (position (char digest position) digits)
                                     (random 15)))
                              16))))
    (list :digest digest :other other)))

;;; ------------------------------------------------------------------------
;;; The recording spy for CHECK-REPORT

(defun %check-keywords (arguments allowed who)
  "Signal unless ARGUMENTS is a keyword plist whose keys are all in ALLOWED."
  (unless (and (evenp (length arguments))
               (loop for (key) on arguments by #'cddr always (member key allowed)))
    (error "~A was given ~S; it takes only ~S." who arguments allowed)))

(defun spy-api (entries &key (backend-default 25) (backend (list :spy-backend))
                          (registry (list :spy-registry)))
  "Return (values API CALLS) for a cl-spec whose definitions are ENTRIES.

Each entry is a plist: :NAME, :KIND (:PROPERTY or :CONTRACT), :ABOUT (true for
a property (:about ROUTING-F)), :TRIALS (a property's table), :STATUS,
:EXECUTED, :BUDGET (the budget the result records, or NIL for the one the run
was given), :DIGEST and :COMPLETE (the digest the result records), :SEED (the
seed the result reports when the run was given none), :DEFINITION-DIGEST (a
contract definition's own digest) and :SIGNAL (a condition the run signals
instead of answering).

BACKEND-DEFAULT is what the backend-default reader answers, or :UNREADABLE for
a reader that signals.  CALLS is a cons whose CAR collects, most recent first,
one plist per handle called: :KEY, :NAME, :ARGUMENTS (as given) and, for a
runner, the registry and backend it saw on its own thread."
  (let ((calls (list nil)))
    (labels ((entry (name kind)
               (find-if (lambda (entry) (and (eq name (getf entry :name))
                                             (eq kind (getf entry :kind))))
                        entries))
             (note (&rest plist) (push plist (car calls)))
             (run (key kind allowed)
               (lambda (name &rest arguments)
                 (note :key key :name name :arguments (copy-list arguments)
                       :thread-backend *spy-backend* :thread-registry *spy-registry*)
                 (%check-keywords arguments allowed key)
                 (let ((entry (entry name kind)))
                   (when (getf entry :signal) (error (getf entry :signal)))
                   (list :spy-result name kind arguments))))
             (record (result)
               (destructuring-bind (name kind arguments) (rest result)
                 (let* ((entry (entry name kind))
                        (seed (or (getf arguments :seed) (getf entry :seed 7)))
                        (record (make-result-record
                                 :entity-kind (if (eq kind :contract) :function-spec :property)
                                 :status (getf entry :status :passed)
                                 :trials (getf entry :executed 3)
                                 :seed seed)))
                   (setf record (record-with record :name name))
                   (setf record (record-with record :budget
                                             (or (getf entry :budget)
                                                 (getf arguments :trials)
                                                 (getf entry :executed 3))))
                   (setf record (record-with record :definition-digest
                                             (getf entry :digest
                                                   "fnv1a64-v1:00000000000000aa")))
                   (setf record (record-with record :definition-digest-complete
                                             (getf entry :complete t)))
                   record))))
      (values
       (make-cl-spec-api
        :version "0.1.0"
        :system-directory "/tmp/cl-spec/"
        :classes (list :unknown-spec 'routing-unknown-name
                       :unknown-property 'routing-unknown-name)
        :specials (list :generator-backend '*spy-backend* :registry '*spy-registry*)
        :functions
        (list :registry (lambda () registry)
              :generator-backend (lambda () backend)
              :backend-default-trials
              (lambda (given)
                (note :key :backend-default-trials :name nil :arguments (list given))
                (if (eq backend-default :unreadable)
                    (error "The backend default could not be read.")
                    backend-default))
              :semantic-data
              (lambda (symbol &key registry)
                (note :key :semantic-data :name symbol :arguments (list :registry registry))
                (list :symbol symbol :package (package-name (symbol-package symbol))
                      :spec nil
                      :function-spec (and (entry symbol :contract) symbol)
                      :property (and (entry symbol :property) symbol)
                      :properties-about
                      (and (eq symbol (target-symbol :f))
                           (loop for entry in entries
                                 when (and (eq :property (getf entry :kind))
                                           (getf entry :about))
                                   collect (getf entry :name)))))
              :property-data
              (lambda (name &key registry)
                (note :key :property-data :name name :arguments (list :registry registry))
                (let ((entry (entry name :property)))
                  (unless entry (error 'routing-unknown-name))
                  (%property-definition name :trials (getf entry :trials
                                                           (list :smoke 5 :normal 25)))))
              :function-spec-data
              (lambda (name &key registry)
                (note :key :function-spec-data :name name :arguments (list :registry registry))
                (let ((entry (entry name :contract)))
                  (unless entry (error 'routing-unknown-name))
                  (contract-definition name :digest (getf entry :definition-digest
                                                          "fnv1a64-v1:00000000000000cc"))))
              :run-property (run :run-property :property '(:profile :seed :registry))
              :check-function (run :check-function :contract '(:seed :registry :trials))
              :result-data #'record
              :result-status (lambda (result) (getf (record result) :status))
              :result-trials (lambda (result) (getf (record result) :trials))
              :result-seed (lambda (result) (getf (record result) :seed))
              :result-profile (constantly :normal)
              :result-counterexample (constantly nil)
              :result-shrunk-counterexample (constantly nil)
              :result-elapsed (constantly 0.01)
              :result-condition (constantly nil)
              :check-rejected (constantly 0)
              :check-failure-reason (constantly nil)))
       calls))))

(defun spy-calls (calls &optional key)
  "Return the calls CALLS recorded, oldest first, only those of KEY if given."
  (remove-if-not (lambda (call) (or (null key) (eq key (getf call :key))))
                 (reverse (car calls))))

(defun spy-run-calls (calls)
  "Return the runner calls CALLS recorded, oldest first."
  (remove-if-not (lambda (call) (member (getf call :key) '(:run-property :check-function)))
                 (reverse (car calls))))
