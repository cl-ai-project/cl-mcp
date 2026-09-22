;;;; specs/spec-responses.lisp
;;;;
;;;; Properties of the last step before a caller: the four response builders,
;;;; checked through the JSON a client actually receives and the text an MCP
;;;; client actually renders.  The functions checked, each called directly:
;;;;
;;;;   BUILD-SPEC-LIST-RESPONSE       what is registered
;;;;   BUILD-SPEC-SYMBOL-RESPONSE     what is registered about one symbol
;;;;   BUILD-SPEC-DESCRIBE-RESPONSE   what a declaration says
;;;;   BUILD-SPEC-CHECK-RESPONSE      what a run established, and what it did not
;;;;
;;;; Three kinds of correctness, kept apart.  The structured fields must keep
;;;; the meaning of the report they were given; the text must not claim
;;;; anything about that report that is not so; and both must survive being
;;;; encoded and read back.  The first two are checked against the scenario
;;;; descriptors of specs/spec-response-fixtures.lisp rather than against each
;;;; other -- a field and a sentence rendered from the same mistake agree
;;;; perfectly.
;;;;
;;;; Everything here goes through JSON.  Reading the builder's hash-table
;;;; cannot tell a false from an absent key, because YASON:FALSE and NIL are
;;;; both objects in Lisp and only the document distinguishes them.
;;;;
;;;; Verified domain: the six listing answers under three limits; the six
;;;; spec-symbol answers; five kinds of declaration with four clause states
;;;; and four documentation strings (ASCII, Japanese, quotes and a backslash,
;;;; empty); the seventeen spec-check answers.  Not covered: the tool entry
;;;; points, the worker's own JSON round trip, JSON-RPC and the transports
;;;; (all of them 3E-2), and cl-spec itself.

(defpackage #:cl-mcp/specs/spec-responses
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/tools/spec-response-builders
                #:build-spec-list-response
                #:build-spec-symbol-response
                #:build-spec-describe-response
                #:build-spec-check-response)
  (:import-from #:cl-mcp/specs/spec-response-fixtures
                #:response-json
                #:parse-response
                #:json-at
                #:json-kind
                #:json-array-p
                #:json-object-p
                #:json-true-p
                #:json-false-p
                #:json-null-p
                #:json-text
                #:first-line
                #:line-starting-with
                #:replay-arguments
                #:replay-argument
                #:claims-p
                #:+verdict-tokens+
                #:verdict-token
                #:response-shape
                #:qualified
                #:+listing-states+
                #:+symbol-states+
                #:+describe-entities+
                #:+clause-states+
                #:+check-cases+
                #:list-scenario
                #:symbol-scenario
                #:describe-scenario
                #:check-scenario
                #:draw-list-case
                #:draw-symbol-case
                #:draw-describe-case
                #:draw-check-case)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/spec-responses)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(spec-response-json-keeps-false-null-and-absent-apart
    spec-list-response-says-why-a-kind-has-no-names
    spec-symbol-response-separates-registration-from-failure
    spec-describe-response-carries-the-declaration-it-was-given
    spec-check-response-carries-the-verdict-and-its-reservations
    spec-check-replay-line-asks-for-the-run-it-reports))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(list-case symbol-case describe-case check-case))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(list-case-generator symbol-case-generator describe-case-generator
    check-case-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none, as it has no
contract.  Its fixed cases are the Rove tests of tests/spec-responses-test.lisp."
  '())

;;; ------------------------------------------------------------------------
;;; A. The document

(defparameter +status-only-shape+
  '(("schema_version" :string) ("status" :string) ("message" :string)
    ("environment" :object) ("content" :array))
  "The keys every answer carries that reports a status and no run.")

(defun %document (response)
  "Return RESPONSE as a client reads it: encoded, then parsed with false,
null, [] and a missing key kept apart."
  (parse-response (response-json response)))

(defun %shape-holds-p (rows document)
  "Return true when DOCUMENT carries each of ROWS' keys, as its stated kind."
  (and (json-object-p document)
       (loop for (key kind) in rows
             always (multiple-value-bind (value present) (json-at document key)
                      (eq kind (json-kind value present))))))

(defun %envelope-holds-p (document)
  "Return true when the envelope every response shares is intact: this
adapter's own schema version, and one text part a client can render."
  (let ((content (json-at document "content")))
    (and (equal "1" (json-at document "schema_version"))
         (json-array-p content)
         (eql 1 (length content))
         (equal "text" (json-at (aref content 0) "type"))
         (stringp (json-at (aref content 0) "text"))
         (plusp (length (json-at (aref content 0) "text"))))))

(defun %symbol-object-holds-p (object package name)
  "Return true when OBJECT is the JSON a symbol crosses as: its package and
its name apart, and the qualified spelling of both.

Two symbols of one name in different packages are different registrations, so
a response that carried only the printed name would make them one."
  (and (json-object-p object)
       (equal package (json-at object "package"))
       (equal name (json-at object "name"))
       (equal (format nil "~A::~A" package name) (json-at object "qualified"))))

(defun %decimal-string-p (value)
  "Return true when VALUE is a string of decimal digits.

A seed is carried as text because it does not fit a double: 2^62 read as a
JSON number comes back rounded, and a rounded seed replays a different run."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character) (find character "0123456789")) value)))

;;; ------------------------------------------------------------------------
;;; B. The text

(defun %text-holds-p (facts text)
  "Return true when TEXT makes every claim FACTS requires and none it forbids."
  (and (stringp text)
       (every (lambda (phrase) (claims-p text phrase)) (getf facts :text-must))
       (notany (lambda (phrase) (claims-p text phrase))
               (getf facts :text-must-not))))

(defun %verdict-holds-p (verdict text)
  "Return true when TEXT opens with VERDICT's word and carries no other.

Compared as whole tokens, mark and all.  \"VERIFIED\" is a substring of \"NOT
VERIFIED\", so a check that searches for the good news finds it in the bad."
  (let ((headline (first-line text)))
    (and (stringp headline)
         (if (eq :status-only verdict)
             ;; A status with no run reports the status.  A verdict word here
             ;; would be a claim about a run that did not happen.
             (loop for (nil . token) in +verdict-tokens+
                   never (claims-p text token))
             (let ((token (verdict-token verdict)))
               (and (<= (length token) (length headline))
                    (string= token headline :end2 (length token))
                    (loop for (key . other) in +verdict-tokens+
                          always (or (eq key verdict)
                                     ;; "⚠ NOT VERIFIED" contains neither of
                                     ;; the other two marks, so a token found
                                     ;; anywhere is a second verdict.
                                     (not (claims-p text other))))))))))

;;; ------------------------------------------------------------------------
;;; C. spec-list

(defun %names-of (array)
  "Return the qualified names of an array of listing entries."
  (loop for entry across array
        collect (let ((name (json-at entry "name")))
                  (if (json-object-p name)
                      (json-at name "qualified")
                      name))))

(defun %listing-holds-p (state limit)
  "Return true when a listing in STATE reports the count, the capability and
the names the descriptor says, and says nothing about kinds nobody looked at."
  (multiple-value-bind (report facts) (list-scenario state :limit limit)
    (let* ((response (build-spec-list-response report))
           (document (%document response))
           (text (json-text response))
           (count (getf facts :count))
           (names (getf facts :names))
           (shown (%names-of (json-at document "properties"))))
      (and (%shape-holds-p (response-shape :list) document)
           (%envelope-holds-p document)
           ;; A kind nobody could look at has no count.  Null, never 0: 0 is
           ;; the answer "this registry holds none of them".
           (multiple-value-bind (value present) (json-at document "counts" "properties")
             (if count
                 (eql count value)
                 (and present (json-null-p value))))
           ;; A count that is not a number is not a number in the text either.
           (or count (not (claims-p text "0 propert")))
           ;; Capability is a boolean whatever the request was.
           (eq (getf facts :listable)
               (json-true-p (json-at document "properties_listable")))
           (eq (getf facts :tag-filterable)
               (json-true-p (json-at document "tag_filterable")))
           (eq (getf facts :truncated) (json-true-p (json-at document "truncated")))
           ;; The names behind the count, in the order given, each still two
           ;; fields -- and a limit cuts this list without touching the count.
           (equal names shown)
           (or (not (getf facts :truncated))
               (and (< (length shown) count) (claims-p text "raise limit")))
           (loop for entry across (json-at document "properties")
                 always (json-object-p (json-at entry "name")))
           ;; A tag that could not be applied says so rather than reporting an
           ;; empty result as though the filter had run.
           (eq (getf facts :tag-applied)
               (json-true-p (json-at document "filters" "tag_applied")))
           (or (getf facts :tag-filterable)
               (claims-p text "was NOT applied"))))))

;;; ------------------------------------------------------------------------
;;; D. spec-symbol

(defun %symbol-holds-p (state)
  "Return true when a spec-symbol answer in STATE keeps the registrations, the
runtime and the failures apart."
  (multiple-value-bind (report facts) (symbol-scenario state)
    (let* ((response (build-spec-symbol-response report))
           (document (%document response))
           (text (json-text response)))
      (if (getf facts :ok)
          (and (%shape-holds-p (response-shape :symbol) document)
               (%envelope-holds-p document)
               (equal "ok" (json-at document "status"))
               ;; Nothing registered is a boolean about a lookup that worked.
               (eq (getf facts :nothing-registered)
                   (json-true-p (json-at document "nothing_registered")))
               ;; The three registrations are separate fields, and a name
               ;; that is not registered is null rather than an empty object.
               (loop for key in '("spec" "function_spec" "property")
                     always (let ((value (json-at document "registry" key)))
                              (or (json-null-p value) (json-object-p value))))
               (equal (getf facts :property-names)
                      (%names-of (json-at document "properties")))
               ;; A runtime that was not read says why, and does not read as a
               ;; symbol with no definition.
               (if (getf facts :runtime-read)
                   (json-object-p (json-at document "runtime"))
                   (and (json-null-p (json-at document "runtime"))
                        (stringp (json-at document "runtime_unavailable_reason"))
                        (claims-p text "runtime information unavailable")))
               ;; A property whose body was deliberately left out says so, and
               ;; says where to fetch it.
               (or (not (getf facts :body-omitted))
                   (and (claims-p text "body omitted")
                        (claims-p text "spec-describe kind=property"))))
          ;; Not a registration answer at all.  An image without cl-spec and a
          ;; reader that broke must not read as "nothing is registered".
          (and (%shape-holds-p +status-only-shape+ document)
               (%envelope-holds-p document)
               (not (equal "ok" (json-at document "status")))
               (not (claims-p text "Nothing is registered"))
               (multiple-value-bind (value present)
                   (json-at document "nothing_registered")
                 (declare (ignore value))
                 (not present)))))))

;;; ------------------------------------------------------------------------
;;; E. spec-describe

(defun %clause-holds-p (facts document)
  "Return true when the precondition clause arrives as the state it is in.

Three states, and the flag is what tells them apart: a clause that is there
and whole is true, one that was cut is false, and a definition with no such
clause is null.  False there would say the declaration has a precondition
that did not fit."
  (let ((text (json-at document "preconditions"))
        (complete (json-at document "preconditions_complete"))
        (omitted (json-at document "preconditions_omitted_chars")))
    (ecase (getf facts :pre)
      (:absent (and (json-null-p text) (json-null-p complete)))
      (:present-nil (and (equal "NIL" text) (json-true-p complete) (eql 0 omitted)))
      (:whole (and (equal (getf facts :pre-text) text)
                   (json-true-p complete)
                   (eql 0 omitted)))
      (:cut (and (equal (getf facts :pre-text) text)
                 (json-false-p complete)
                 (eql (getf facts :pre-omitted) omitted))))))

(defun %describe-holds-p (entity pre documentation)
  "Return true when a declaration reaches the caller as it was given."
  (multiple-value-bind (report facts) (describe-scenario entity :pre pre
                                                                :documentation documentation)
    (let* ((response (build-spec-describe-response report))
           (document (%document response))
           (text (json-text response))
           (arguments (json-at document "arguments"))
           (cases (json-at document "cases")))
      (and (%shape-holds-p (response-shape :describe) document)
           (%envelope-holds-p document)
           (equal (getf facts :kind) (json-at document "kind"))
           (json-object-p (json-at document "name"))
           (equal (getf facts :name) (json-at document "name" "qualified"))
           ;; Text the caller wrote crosses as itself, whatever is in it.
           (equal documentation (json-at document "documentation"))
           (%clause-holds-p facts document)
           ;; Each argument keeps its own kind, in the order declared.
           (equal (getf facts :argument-kinds)
                  (loop for argument across arguments
                        for kind = (json-at argument "kind")
                        unless (json-null-p kind) collect kind))
           (loop for argument across arguments
                 always (json-object-p (json-at argument "variable")))
           ;; A contract's cases keep their order and their outcome.
           (equal (getf facts :case-names)
                  (loop for case across cases collect (json-at case "name")))
           (ecase (or (getf facts :outcome) :none)
             (:returns (and (json-object-p (json-at document "returns"))
                            (json-null-p (json-at document "signals"))))
             (:signals (and (json-object-p (json-at document "signals"))
                            (json-null-p (json-at document "returns"))))
             (:none t))
           ;; A key that is not part of this kind of definition is null: false
           ;; would say the definition turned it off.
           (ecase (getf facts :shrink-enabled)
             (:true (json-true-p (json-at document "shrink_enabled")))
             (:null (json-null-p (json-at document "shrink_enabled"))))
           ;; A cut clause says so in the text too, and says how much is
           ;; missing: a reader who sees only the text would otherwise take
           ;; the part for the whole condition.
           (or (not (eq :cut (getf facts :pre)))
               (and (claims-p text "truncated")
                    (claims-p text (princ-to-string (getf facts :pre-omitted)))))))))

;;; ------------------------------------------------------------------------
;;; F. spec-check

(defun %counterexample-holds-p (facts document)
  "Return true when the evidence for a failure is the state it is in.

Four answers, and they are not the same one.  An empty counterexample is a
property of no arguments that failed; an absent one is a run with nothing to
show; an unavailable one is evidence that could not be read; and a run that
never got as far as generating has none to have."
  (let ((results (json-at document "results")))
    (or (zerop (length results))
        (let* ((result (aref results (1- (length results))))
               (value (json-at result "counterexample"))
               (status (json-at result "counterexample_status")))
          (and (json-array-p value)
               (ecase (getf facts :counterexample)
                 (:present (and (equal "present" status) (plusp (length value))))
                 (:present-empty (and (equal "present" status) (zerop (length value))))
                 (:absent (and (equal "absent" status) (zerop (length value))))
                 (:unavailable (and (equal "unavailable" status)
                                    (zerop (length value))))
                 (:not-applicable (and (equal "not-applicable" status)
                                       (zerop (length value))))))))))

(defun %gaps-hold-p (facts document)
  "Return true when the gaps reach both the payload and the text, each once."
  (let ((gaps (json-at document "verification_gaps")))
    (and (json-array-p gaps)
         (equal (getf facts :gaps) (coerce gaps 'list)))))

(defun %check-holds-p (case)
  "Return true when a spec-check answer carries its verdict, its evidence and
its reservations -- in the payload and in the text alike."
  (multiple-value-bind (report facts) (check-scenario case)
    (let* ((response (build-spec-check-response report))
           (document (%document response))
           (text (json-text response))
           (status-only (eq :status-only (getf facts :verdict))))
      (and (%envelope-holds-p document)
           (%shape-holds-p (if status-only
                               +status-only-shape+
                               (response-shape :check))
                           document)
           (equal (getf facts :status) (json-at document "status"))
           ;; The verdict is a boolean on every answer, including the ones
           ;; that report a status and no run.
           (ecase (getf facts :verified)
             (:true (json-true-p (json-at document "verified")))
             (:false (json-false-p (json-at document "verified"))))
           (%verdict-holds-p (getf facts :verdict) text)
           (%text-holds-p facts text)
           (or status-only
               (and (%gaps-hold-p facts document)
                    (%counterexample-holds-p facts document)
                    ;; Every result keeps its seed as a decimal string, its
                    ;; symbol as package and name, and the raw record cl-spec
                    ;; handed over stays out of the payload.
                    (loop for result across (json-at document "results")
                          always (and (%decimal-string-p (json-at result "seed"))
                                      (json-object-p (json-at result "property"))
                                      (multiple-value-bind (value present)
                                          (json-at result "core_result" "source")
                                        (declare (ignore value))
                                        (not present))))))
           ;; The footer's own line agrees with the field beside it.
           (or (eq :true (getf facts :verified))
               (not (claims-p text "verified: true")))))))

;;; ------------------------------------------------------------------------
;;; G. The replay line

(defun %replay-holds-p (case)
  "Return true when the Replay line asks for the run the response reports.

Read out of the text the builder produced, not composed here: a line nobody
can act on is the failure this asks about."
  (multiple-value-bind (report facts) (check-scenario case)
    (let* ((response (build-spec-check-response report))
           (text (json-text response))
           (line (line-starting-with text "Replay:"))
           (arguments (replay-arguments line))
           (expected (getf facts :replay)))
      (if (null expected)
          ;; Nothing ran, or nothing that ran has a seed: no instruction is
          ;; better than one whose arguments are NIL.
          (null line)
          (and arguments
               (equal (getf expected :seed) (replay-argument arguments "seed"))
               (equal (getf expected :digest)
                      (replay-argument arguments "expect_definition_digest"))
               (ecase (getf expected :target)
                 ;; A contract is replayed by function= and a budget.  Asked
                 ;; for as property= it names a property that does not exist,
                 ;; and without the budget a failure found at a raised trial
                 ;; count need not reappear.
                 (:function
                  (and (equal (getf expected :name)
                              (replay-argument arguments "function"))
                       (null (replay-argument arguments "property"))
                       (null (replay-argument arguments "profile"))
                       (equal (princ-to-string (getf expected :trials))
                              (replay-argument arguments "trials"))))
                 (:property
                  (and (equal (getf expected :name)
                              (replay-argument arguments "property"))
                       (null (replay-argument arguments "function"))
                       (equal (getf expected :profile)
                              (replay-argument arguments "profile"))))))))))

;;; ------------------------------------------------------------------------
;;; The declarations

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator list-case-generator ()
    "Draw a listing answer and a limit (DRAW-LIST-CASE)."
    (draw-list-case))
  (defspec list-case list (:generator list-case-generator))

  (defgenerator symbol-case-generator ()
    "Draw a spec-symbol answer (DRAW-SYMBOL-CASE)."
    (draw-symbol-case))
  (defspec symbol-case list (:generator symbol-case-generator))

  (defgenerator describe-case-generator ()
    "Draw a declaration, a clause state and a docstring (DRAW-DESCRIBE-CASE)."
    (draw-describe-case))
  (defspec describe-case list (:generator describe-case-generator))

  (defgenerator check-case-generator ()
    "Draw a spec-check answer (DRAW-CHECK-CASE)."
    (draw-check-case))
  (defspec check-case list (:generator check-case-generator))

  (defproperty spec-response-json-keeps-false-null-and-absent-apart
      ((case check-case))
    "Every response is a document, and the document is where false, null and
an absent key are three different answers.  The keys each builder always
writes are there and are what they are -- a flag is a boolean, a list is an
array, the text envelope is an array of one text part -- and the adapter's own
schema version is a string beside the record's own.  A seed crosses as a
decimal string, so a value past 2^53 is the value that was used; a symbol
crosses as its package and its name, so two of one name in different packages
stay apart; and the raw record cl-spec handed over does not cross at all.
Every trial checks all four builders, and the drawn spec-check answer besides."
    (:about build-spec-list-response build-spec-symbol-response
            build-spec-describe-response build-spec-check-response)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key case) case
      (and (%check-holds-p case)
           (every (lambda (state) (%listing-holds-p state 50)) +listing-states+)
           (every #'%symbol-holds-p +symbol-states+)
           (every (lambda (entity) (%describe-holds-p entity :whole "A contract."))
                  +describe-entities+))))

  (defproperty spec-list-response-says-why-a-kind-has-no-names
      ((case list-case))
    "A listing answers about a kind in one of six ways, and no names is four
of them.  A count is a number only where someone looked: not requested, not
listable and not filterable each give null and a text that does not print a
zero for them.  The capability flags stay booleans whatever the request was.
The names behind a count come back in order, each as a package and a name, and
a limit cuts that list without touching the count.  A tag that could not be
applied says so."
    (:about build-spec-list-response)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key state limit) case
      (and (%listing-holds-p state limit)
           ;; Every answer, under the drawn limit, every trial.
           (every (lambda (one) (%listing-holds-p one limit)) +listing-states+))))

  (defproperty spec-symbol-response-separates-registration-from-failure
      ((case symbol-case))
    "What is registered about a symbol, what could not be read, and what was
not asked for are three answers.  A lookup that worked and found nothing says
nothing-registered; an image without cl-spec and a reader that broke carry a
status and no such claim at all.  The spec, the contract and the property are
separate fields, a name that is not registered is null rather than an empty
object, and the properties about the symbol keep their order and their
packages.  A runtime nobody read says why instead of reading as a symbol with
no definition, and a body left out of a listing says where to fetch it."
    (:about build-spec-symbol-response)
    (:kind :resolution)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key state) case
      (and (%symbol-holds-p state)
           (every #'%symbol-holds-p +symbol-states+))))

  (defproperty spec-describe-response-carries-the-declaration-it-was-given
      ((case describe-case))
    "A declaration reaches the caller as it was written: its kind and name,
its arguments in order with their own kinds, its cases in order, and its
returns or its signals -- never both.  Its documentation crosses as itself,
through JSON escaping, for text with quotes, a backslash, a newline and
characters outside ASCII.  A clause that is whole, one that was cut and a
definition with no such clause are three answers: true, false and null, with
the count of what was dropped beside the second.  A key that is not part of
this kind of definition is null, never a false that says the definition turned
it off."
    (:about build-spec-describe-response)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key entity pre documentation) case
      (and (%describe-holds-p entity pre documentation)
           ;; Every clause state, on the drawn declaration, every trial.
           (every (lambda (one) (%describe-holds-p entity one documentation))
                  +clause-states+))))

  (defproperty spec-check-response-carries-the-verdict-and-its-reservations
      ((case check-case))
    "A run's answer is its verdict and everything the verdict does not cover.
The three verdicts are three words, and the word a reader stops at carries the
coverage it stands on: a case nobody reached, a contract that was not run, the
properties that were not.  A status with no run reports the status and claims
no verdict at all.  The gaps reach the text as well as the payload; a
counterexample that is empty, absent, unavailable or never generated stays
four answers; a captured NIL is application data and an unavailable capture is
not a value.  VERIFIED is checked as a word, not as a substring of NOT
VERIFIED.  Every trial checks the drawn answer and every other one."
    (:about build-spec-check-response)
    (:kind :resolution)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key case) case
      (and (%check-holds-p case)
           (every #'%check-holds-p +check-cases+))))

  (defproperty spec-check-replay-line-asks-for-the-run-it-reports
      ((case check-case))
    "The replay line is read out of the text the response produced and checked
as the request it asks for.  It names the first result that did not pass,
falling back to the first, and carries that result's own seed and digest -- not
another result's.  A property is asked for by property= and a profile; a
contract by function= and a trial budget, and never by property= or a profile
it did not use.  A run with no seed to reproduce from prints no line at all,
rather than one whose arguments are NIL."
    (:about build-spec-check-response)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key case) case
      (and (%replay-holds-p case)
           (every #'%replay-holds-p +check-cases+)))))
