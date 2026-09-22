;;;; specs/check-routing.lisp
;;;;
;;;; Properties of spec-check's routing: what it accepts, what it selects and
;;;; with which budget it runs, and how it compares a digest.  The functions
;;;; checked, each called directly:
;;;;
;;;;   %TARGET-ARGUMENT-ERROR, %RESOLVE-PROFILE  which target arguments are valid
;;;;   PARSE-SEED-STRING                         seed text to an integer
;;;;   %SELECT-PROPERTIES                        which definitions a request names
;;;;   %TRIALS-BUDGET                            the budget a run is given
;;;;   %DEFINITION-MATCH                         the four-way digest comparison
;;;;
;;;; All but PARSE-SEED-STRING are internal to cl-mcp/src/spec-adapter-report.
;;;; None of these properties goes through CHECK-REPORT, a deadline thread or
;;;; a real cl-spec: whether what they decide reaches the runner is the fixed
;;;; spy tests of tests/check-routing-test.lisp, and whether cl-spec itself
;;;; agrees with the registration model is the opt-in
;;;; tests/check-routing-specs-test.lisp.  Every expectation comes from
;;;; specs/check-routing-fixtures.lisp: a rule stated as data, a registry
;;;; descriptor's relations, or independently written decimal text.
;;;;
;;;; Verified domain: every combination of the three target arguments with
;;;; and without trials and profile; seeds up to 2^128 and the required edges;
;;;; registries built from a fixed set of symbols (a contract, a same-named
;;;; property, three related properties, an unrelated one, a same name in
;;;; another package) in any registration order, with the reverse index
;;;; readable or not; budgets from a profile entry, explicit trials or the
;;;; backend default, each readable or not; digests complete or not.  Not
;;;; covered: symbol resolution beyond these names, the listing and describe
;;;; paths, and anything after the runner is called.

(defpackage #:cl-mcp/specs/check-routing
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:%target-argument-error
                #:%resolve-profile
                #:%select-properties
                #:%trials-budget
                #:%definition-match)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:parse-seed-string)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:decimal-string)
  (:import-from #:cl-mcp/specs/check-routing-fixtures
                #:target-symbol
                #:designator
                #:target-error-expected-p
                #:draw-target-case
                #:+required-seeds+
                #:draw-seed-text-case
                #:registry-api
                #:expected-selection
                #:+selection-requests+
                #:draw-selection-case
                #:budget-inputs
                #:expected-budget
                #:draw-budget-case
                #:digest-plist
                #:expected-match
                #:draw-digest-case)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/check-routing)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(check-routing-target-arguments-are-exclusive
    check-routing-seed-text-keeps-every-digit
    check-routing-selection-names-only-what-was-asked
    check-routing-budget-comes-from-its-stated-source
    check-routing-digest-comparison-has-four-answers))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(target-case seed-text-case selection-case budget-case digest-case))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(target-case-generator seed-text-case-generator selection-case-generator
    budget-case-generator digest-case-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none, as it has no
contract.  Its fixed cases are the Rove tests of tests/check-routing-test.lisp."
  '())

;;; ------------------------------------------------------------------------
;;; A. Target arguments

(defun %target-answers-hold-p (names trials profile)
  "Return true when %TARGET-ARGUMENT-ERROR refuses exactly the combinations
the rule refuses, over all eight presences of the three targets, each with and
without TRIALS and PROFILE.  A refusal is :INVALID-ARGUMENTS with a message."
  (loop for mask below 8
        always
        (loop for with-trials in '(nil t)
              always
              (loop for with-profile in '(nil t)
                    always
                    (let* ((property (and (logbitp 0 mask) (first names)))
                           (symbol (and (logbitp 1 mask) (second names)))
                           (function (and (logbitp 2 mask) (third names)))
                           (trials (and with-trials trials))
                           (profile (and with-profile profile))
                           (answer (%target-argument-error property symbol function
                                                           trials profile)))
                      (if (target-error-expected-p property symbol function trials profile)
                          (and (eq :invalid-arguments (getf answer :status))
                               (stringp (getf answer :message)))
                          (null answer)))))))

(defun %profiles-hold-p (known unknown)
  "Return true when no profile means :NORMAL, a known profile name in any case
means its keyword, and a name no loaded code interns is refused with a message
-- and trying it interns nothing."
  (and (eq :normal (%resolve-profile nil))
       (every (lambda (spelling)
                (multiple-value-bind (keyword message) (%resolve-profile spelling)
                  (and (keywordp keyword)
                       (string-equal spelling (symbol-name keyword))
                       (null message))))
              known)
       (every (lambda (name)
                (let ((key (string-upcase name)))
                  (and (null (find-symbol key "KEYWORD"))
                       (multiple-value-bind (keyword message) (%resolve-profile name)
                         (and (null keyword) (stringp message)))
                       (null (find-symbol key "KEYWORD")))))
              unknown)))

;;; ------------------------------------------------------------------------
;;; B. Seed text

(defun %reads-as-p (text integer)
  "Return true when PARSE-SEED-STRING reads TEXT as exactly INTEGER."
  (multiple-value-bind (seed message) (parse-seed-string text)
    (and (eql integer seed) (null message))))

(defun %refused-p (input)
  "Return true when PARSE-SEED-STRING refuses INPUT with a message: no seed,
and not the silence that means none was given."
  (multiple-value-bind (seed message) (parse-seed-string input)
    (and (null seed) (stringp message))))

;;; ------------------------------------------------------------------------
;;; C. Selection

(defun %symbol-key (symbol)
  "Return SYMBOL as (PACKAGE-NAME NAME)."
  (list (package-name (symbol-package symbol)) (symbol-name symbol)))

(defun %plist-key (symbol-plist)
  "Return a response's symbol plist as (PACKAGE-NAME NAME), or NIL."
  (and symbol-plist (list (getf symbol-plist :package) (getf symbol-plist :name))))

(defun %same-keys-p (symbols symbol-plists)
  "Return true when SYMBOL-PLISTS name exactly SYMBOLS, each once."
  (let ((wanted (mapcar #'%symbol-key symbols))
        (given (mapcar #'%plist-key symbol-plists)))
    (and (= (length wanted) (length given))
         (null (set-exclusive-or wanted given :test #'equal)))))

(defun %select (registry kind target form)
  "Select TARGET by KIND under the REGISTRY descriptor, with TARGET written in
FORM.  Return (values NAMES SELECTION ERROR SELECTED-KIND CALLS REGISTRY-OBJECT)."
  (multiple-value-bind (api calls registry-object) (registry-api registry)
    (multiple-value-bind (text package) (designator target form)
      (multiple-value-bind (names selection error selected-kind)
          (%select-properties api
                              (and (eq kind :property) text)
                              (and (eq kind :symbol) text)
                              (and (eq kind :function) text)
                              package registry-object)
        (values names selection error selected-kind calls registry-object)))))

(defun %selection-matches-p (registry kind target form)
  "Return true when selecting TARGET by KIND under REGISTRY gives what
EXPECTED-SELECTION says, runs nothing, and hands every reader the registry it
was given."
  (let ((expected (expected-selection registry kind target)))
    (multiple-value-bind (names selection error selected-kind calls registry-object)
        (%select registry kind target form)
      (and
       (notany (lambda (call) (member (first call) '(:run-property :check-function)))
               (car calls))
       (every (lambda (call) (eq registry-object (third call))) (car calls))
       (if (getf expected :error)
           (and (null names) (eq (getf expected :error) (getf error :status)))
           (let ((wanted (getf expected :names)))
             (and (null error)
                  (= (length wanted) (length names))
                  (null (set-exclusive-or wanted names))
                  (eq (getf expected :kind) selected-kind)
                  (eq (getf expected :kind) (getf selection :kind))
                  (equal (getf expected :mode) (getf selection :mode))
                  (eql (length wanted) (getf selection :count))
                  (%same-keys-p wanted (getf selection :selected))
                  (getf (getf selection :requested) (getf expected :requested-key))
                  (equal (and (getf expected :contract-not-run)
                              (%symbol-key (getf expected :contract-not-run)))
                         (%plist-key (getf selection :contract-not-run)))
                  (equal (and (getf expected :own-property-not-run)
                              (%symbol-key (getf expected :own-property-not-run)))
                         (%plist-key (getf selection :own-property-not-run)))
                  (or (not (eq kind :function))
                      (and (%same-keys-p (getf expected :properties-not-run)
                                         (getf selection :properties-not-run))
                           (eq (getf expected :properties-not-run-read)
                               (and (getf selection :properties-not-run-read) t)))))))))))

(defun %every-request-matches-p (registry forms)
  "Return true when every request in +SELECTION-REQUESTS+, each written in
its form from FORMS, selects what REGISTRY's relations say."
  (loop for (kind target) in +selection-requests+
        for form in forms
        always (%selection-matches-p registry kind target form)))

(defun %selected-keys (registry kind)
  "Return, as a sorted list of name keys, what selecting ROUTING-F by KIND
names: the properties an :about selection runs, or those a contract selection
leaves unrun."
  (multiple-value-bind (names selection) (%select registry kind :f :qualified)
    (sort (mapcar #'princ-to-string
                  (if (eq kind :symbol)
                      (mapcar #'%symbol-key names)
                      (mapcar #'%plist-key (getf selection :properties-not-run))))
          #'string<)))

(defun %relations-hold-p (registry)
  "Return true when, for ROUTING-F's :about and contract selections, adding
the unrelated property changes nothing, reversing the registration order
changes nothing, and removing one related property removes exactly it."
  (every (lambda (kind)
           (let ((base (%selected-keys registry kind))
                 (about (getf registry :about)))
             (and (equal base (%selected-keys (list* :unrelated t registry) kind))
                  (equal base (%selected-keys (list* :about (reverse about) registry) kind))
                  (or (null about)
                      (let ((removed (princ-to-string
                                      (%symbol-key (target-symbol (first about))))))
                        (equal (remove removed base :test #'string=)
                               (%selected-keys (list* :about (rest about) registry)
                                               kind)))))))
         '(:symbol :function)))

;;; ------------------------------------------------------------------------
;;; D. Budgets

(defun %budget-holds-p (&key kind table profile requested (backend :present)
                          (reader :value) default)
  "Return true when %TRIALS-BUDGET gives the budget and source the policy
names, reports the backend default only as read, and asks the default reader
about exactly the backend it was handed, or not at all."
  (multiple-value-bind (facts backend-object api calls)
      (budget-inputs :kind kind :table table :backend backend :reader reader
                     :default default)
    (multiple-value-bind (budget source)
        (expected-budget :kind kind :table table :profile profile :requested requested
                         :backend backend :reader reader :default default)
      (let ((plist (%trials-budget api facts profile backend-object requested)))
        (and (eql budget (getf plist :budget))
             (equal source (getf plist :budget-source))
             (eql (and (eq backend :present) (eq reader :value) default)
                  (getf plist :backend-default))
             (every (lambda (given) (eq given backend-object)) (car calls))
             (or (eq backend :present) (null (car calls))))))))

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator target-case-generator ()
    "Draw target designators, a trial count and profile names (DRAW-TARGET-CASE)."
    (draw-target-case))
  (defspec target-case list (:generator target-case-generator))
  (defgenerator seed-text-case-generator ()
    "Draw seeds below 2^128 with leading zeros, and refused spellings (DRAW-SEED-TEXT-CASE)."
    (draw-seed-text-case))
  (defspec seed-text-case list (:generator seed-text-case-generator))
  (defgenerator selection-case-generator ()
    "Draw a registry, its fully related variant and designator forms (DRAW-SELECTION-CASE)."
    (draw-selection-case))
  (defspec selection-case list (:generator selection-case-generator))
  (defgenerator budget-case-generator ()
    "Draw a profile entry, a backend default and explicit trials (DRAW-BUDGET-CASE)."
    (draw-budget-case))
  (defspec budget-case list (:generator budget-case-generator))
  (defgenerator digest-case-generator ()
    "Draw a digest and another differing in one hex digit (DRAW-DIGEST-CASE)."
    (draw-digest-case))
  (defspec digest-case list (:generator digest-case-generator))

  (defproperty check-routing-target-arguments-are-exclusive
      ((case target-case))
    "%TARGET-ARGUMENT-ERROR accepts exactly one of property, symbol and
function, trials only with function and profile only with property or symbol,
and refuses every other combination as :INVALID-ARGUMENTS with a message.
Every trial runs all eight presences of the three targets, each with and
without trials and profile, under drawn names, trial count and profile.
%RESOLVE-PROFILE reads no profile as :NORMAL and a known profile in any case
as its keyword, and refuses a name no loaded code interns -- without interning
it.  Thirty-nine calls a trial: thirty-two and seven."
    (:about %target-argument-error %resolve-profile)
    (:kind :invariant)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key names trials profile known unknown) case
      (and (%target-answers-hold-p names trials profile)
           (%profiles-hold-p known unknown))))

  (defproperty check-routing-seed-text-keeps-every-digit
      ((case seed-text-case))
    "PARSE-SEED-STRING reads decimal text as exactly its integer, and nothing
else as a seed.  Every trial reads NIL as no seed at all and \"0\" as seed 0;
reads the required seeds -- 0, 1, 9, 10, both sides of 2^53, 2^62 and 2^64,
and 10^40+7 -- and four drawn ones below 2^128 with zero to three leading
zeros, each written by integer division, not by the reader; and refuses, with
a message rather than the silence that means no seed was given, a sign,
surrounding whitespace, an exponent, a fraction, a radix prefix, a
read-time evaluation, a separator, the empty string and non-strings.
Thirty-four calls a trial."
    (:about parse-seed-string)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key seeds refused) case
      (and (multiple-value-bind (seed message) (parse-seed-string nil)
             (and (null seed) (null message)))
           (%reads-as-p "0" 0)
           (every (lambda (seed) (%reads-as-p (decimal-string seed) seed)) +required-seeds+)
           (every (lambda (entry)
                    (destructuring-bind (&key seed zeros) entry
                      (%reads-as-p (concatenate 'string
                                                (make-string zeros :initial-element #\0)
                                                (decimal-string seed))
                                   seed)))
                  seeds)
           (every #'%refused-p refused)
           (every #'%refused-p (list "" (getf (first seeds) :seed) 4.2 :seed (list "1"))))))

  (defproperty check-routing-selection-names-only-what-was-asked
      ((case selection-case))
    "%SELECT-PROPERTIES names what a request asks for and nothing else, and
runs nothing.  property= names that one registered property; symbol= names
the properties (:about) the symbol, never its contract or a property of its
own name, which it reports as not run; function= names the contract only and
reports the related properties as not run, or as unknown when the reverse
index cannot be read.  An unregistered name is not-registered, a name that
resolves to nothing is unresolved-symbol, an unreadable index on symbol= is
internal-error -- never an empty selection -- and the same name in another
package is another symbol.  Every trial runs sixteen requests, in drawn
qualified or unqualified form, against a drawn registry and against the same
one with a contract, a same-named property and a readable index, checking the
names, their count, kind, mode, what is reported as not run, and that every
reader was handed the registry given.  Then adding the unrelated property or
reversing the registration order changes nothing, and removing one related
property removes exactly it.  Thirty-eight to forty selections a trial,
forty when the drawn registry has a related property to remove."
    (:about %select-properties)
    (:kind :invariant)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key registry full forms) case
      (and (%every-request-matches-p registry forms)
           (%every-request-matches-p full forms)
           (%relations-hold-p full))))

  (defproperty check-routing-budget-comes-from-its-stated-source
      ((case budget-case))
    "%TRIALS-BUDGET gives a property its own entry for the profile, 0
included, and a contract its explicit trials; otherwise the backend default
when it can be read; otherwise no budget, with source unknown -- never a
made-up number.  The entry, the default and the explicit trials are drawn
from ranges that do not overlap, so taking one for another shows.  Every
trial runs eleven derivations: a property with an entry for the profile, with
an entry of 0, with an entry only for another profile, and without an entry
while the default has no backend, a reader that signals, answers NIL or is
missing; a contract with explicit trials, without them, and without them or a
backend.  Each also reports the default only as read, and asks the reader
about exactly the backend it was handed."
    (:about %trials-budget)
    (:kind :resolution)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key entry other-entry default requested) case
      (and (%budget-holds-p :kind :property :table (list :smoke other-entry :normal entry)
                            :profile :normal :default default)
           (%budget-holds-p :kind :property :table (list :smoke other-entry :normal 0)
                            :profile :normal :default default)
           (%budget-holds-p :kind :property :table (list :smoke other-entry)
                            :profile :smoke :default default)
           (%budget-holds-p :kind :property :table (list :smoke other-entry)
                            :profile :normal :default default)
           (%budget-holds-p :kind :property :table (list :smoke other-entry)
                            :profile :normal :backend :absent :default default)
           (%budget-holds-p :kind :property :table (list :smoke other-entry)
                            :profile :normal :reader :signals :default default)
           (%budget-holds-p :kind :property :table (list :smoke other-entry)
                            :profile :normal :reader :nil :default default)
           (%budget-holds-p :kind :property :table (list :smoke other-entry)
                            :profile :normal :reader :missing :default default)
           (%budget-holds-p :kind :contract :profile :normal :requested requested
                            :default default)
           (%budget-holds-p :kind :contract :profile :normal :default default)
           (%budget-holds-p :kind :contract :profile :normal :backend :absent
                            :default default))))

  (defproperty check-routing-digest-comparison-has-four-answers
      ((case digest-case))
    "%DEFINITION-MATCH answers not-checked when no digest was expected,
unknown when there is no complete digest to compare -- an incomplete one that
happens to be equal included -- and true or false only for two complete
digests.  Every trial runs all nine rows: three with nothing expected, and
six against a drawn digest and one differing from it in a single hex digit."
    (:about %definition-match)
    (:kind :equivalence)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key digest other) case
      (every (lambda (row)
               (destructuring-bind (value complete expected) row
                 (eq (expected-match value complete expected)
                     (%definition-match (digest-plist value complete) expected))))
             (list (list digest t nil) (list digest nil nil) (list nil nil nil)
                   (list nil nil digest) (list nil t digest)
                   (list digest nil digest) (list digest nil other)
                   (list digest t digest) (list digest t other))))))
