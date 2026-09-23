;;;; specs/spec-response-fixtures.lisp
;;;;
;;;; Scenario descriptors and a JSON layer for the response properties
;;;; (specs/spec-responses.lisp) and their fixed cases
;;;; (tests/spec-responses-test.lisp).  Needs no cl-spec, so the fixed cases
;;;; run in the default suite.
;;;;
;;;; What the response layer decides is not what a report says but what
;;;; reaches a caller: the structured fields after they have been through
;;;; JSON, and the text an MCP client renders.  Both are checked against the
;;;; descriptors here rather than against each other -- two renderings of one
;;;; mistake agree with each other perfectly.
;;;;
;;;; The JSON layer below encodes exactly as the server does and parses with
;;;; false, true, null, [] and a missing key kept apart.  The server's own
;;;; reader does not keep them apart (see PARSE-RESPONSE), which is why a
;;;; check that reads the builder's hash-table, or that parses with the
;;;; server's settings, cannot see the difference between "this was measured
;;;; false" and "this is absent".

(defpackage #:cl-mcp/specs/spec-response-fixtures
  (:use #:cl)
  ;; A bare :import-from declares the dependency without importing a symbol:
  ;; this file names yason's functions in full, and a package-inferred system
  ;; reads its dependencies from here, not from the body.
  (:import-from #:yason)
  ;; The evidence a failure carries is built as cl-spec hands it over and put
  ;; through the record layer's own projection (checked in
  ;; specs/core-records.lisp), so the payload these fixtures feed the builder
  ;; has the shape a real report has -- not one written here to match the
  ;; checks that read it.
  (:import-from #:cl-mcp/src/spec-core-record
                #:project-core-record)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:make-result-record
                #:record-with)
  (:export #:+listing-states+
           #:+symbol-states+
           #:+describe-entities+
           #:+clause-states+
           #:+clause-text+
           #:+cut-clause-text+
           #:+cut-clause-omitted+
           #:response-json
           #:parse-response
           #:json-at
           #:json-array-p
           #:json-object-p
           #:json-true-p
           #:json-false-p
           #:json-null-p
           #:json-kind
           #:+response-shape+
           #:response-shape
           #:json-text
           #:text-lines
           #:first-line
           #:line-starting-with
           #:replay-arguments
           #:replay-argument
           #:claims-p
           #:+verdict-tokens+
           #:verdict-token
           #:symbol-data
           #:qualified
           #:fixture-name
           #:+fixture-names+
           #:environment
           #:list-scenario
           #:symbol-scenario
           #:describe-scenario
           #:draw-list-case
           #:draw-symbol-case
           #:draw-describe-case
           #:+check-cases+
           #:+check-robustness-cases+
           #:check-scenario
           #:draw-check-case))

(in-package #:cl-mcp/specs/spec-response-fixtures)

;;; ------------------------------------------------------------------------
;;; A. The JSON layer

(defun response-json (response)
  "Return RESPONSE encoded as the JSON text the server writes.

The same call the server makes (SRC/PROTOCOL::%ENCODE-JSON's fast path), so
what is parsed back below is the document a client receives, not a
re-rendering of the hash-table by some other route."
  (with-output-to-string (stream) (yason:encode response stream)))

(defun parse-response (text)
  "Parse TEXT with false, true, null, [] and a missing key kept apart.

Deliberately not the server's own settings.  The server parses with
YASON:PARSE and no arguments, where false and null both arrive as NIL -- fine
for reading a request, and useless for asking whether a response said false or
said nothing.  Under the settings here a true is YASON:TRUE, a false is
YASON:FALSE, a null is :NULL, an array is a vector and an absent key is absent,
so the five can be told apart by what they are rather than by what they are
not."
  (yason:parse text :object-as :hash-table
                    :json-arrays-as-vectors t
                    :json-booleans-as-symbols t
                    :json-nulls-as-keyword t))

(defun json-at (table &rest keys)
  "Return (values VALUE PRESENT-P) for KEYS followed from the parsed TABLE.

PRESENT-P is false as soon as one key is missing, which is the state a value
comparison cannot express: a key that is not there is not a key whose value is
null."
  (let ((value table) (present t))
    (dolist (key keys (values value present))
      (unless (hash-table-p value) (return (values nil nil)))
      (multiple-value-setq (value present) (gethash key value))
      (unless present (return (values nil nil))))))

(defun json-array-p (value)
  "True when VALUE is what a JSON array parses to here: a vector that is not a
string.  VECTORP alone is true of every string, so an empty array and an empty
string would be the same answer."
  (and (vectorp value) (not (stringp value))))

(defun json-object-p (value)
  "True when VALUE is what a JSON object parses to here."
  (hash-table-p value))

(defun json-true-p (value)
  "True when VALUE is JSON true, and not merely non-NIL.

YASON:FALSE is a symbol, so a Lisp truth test says a JSON false is true."
  (eq 'yason:true value))

(defun json-false-p (value)
  "True when VALUE is JSON false, as opposed to null or absent."
  (eq 'yason:false value))

(defun json-null-p (value)
  "True when VALUE is JSON null, as opposed to false or absent."
  (eq :null value))

(defun json-kind (value present)
  "Return what VALUE is, as JSON: :ABSENT, :NULL, :BOOLEAN, :STRING, :NUMBER,
:ARRAY, :OBJECT or :OTHER.

Seven answers where a Lisp truth test gives two.  Absent, null and false are
each a different statement about a field, and an empty array and an empty
string are each a value."
  (cond ((not present) :absent)
        ((json-null-p value) :null)
        ((or (json-true-p value) (json-false-p value)) :boolean)
        ((stringp value) :string)
        ((numberp value) :number)
        ((json-array-p value) :array)
        ((json-object-p value) :object)
        (t :other)))

(defparameter +response-shape+
  '((:list ("schema_version" :string)
           ("status" :string)
           ("specs" :array)
           ("properties" :array)
           ("function_specs" :array)
           ("specs_listable" :boolean)
           ("properties_listable" :boolean)
           ("function_specs_listable" :boolean)
           ("tag_filterable" :boolean)
           ("truncated" :boolean)
           ("counts" :object)
           ("filters" :object)
           ("environment" :object)
           ("content" :array))
    (:symbol ("schema_version" :string)
             ("status" :string)
             ("symbol" :object)
             ("registry" :object)
             ("properties" :array)
             ("nothing_registered" :boolean)
             ("notes" :array)
             ("environment" :object)
             ("content" :array))
    (:describe ("schema_version" :string)
               ("status" :string)
               ("kind" :string)
               ("name" :object)
               ("tags" :array)
               ("targets" :array)
               ("arguments" :array)
               ("capture" :array)
               ("cases" :array)
               ("environment" :object)
               ("content" :array))
    (:check ("schema_version" :string)
            ("status" :string)
            ("verified" :boolean)
            ("selection" :object)
            ("results" :array)
            ("counts" :object)
            ("verification_gaps" :array)
            ("environment" :object)
            ("content" :array)))
  "What each response's own keys are, and what each one is as JSON.

Every row here is a key the builder writes unconditionally, so :ABSENT and
:NULL are both wrong answers for it whatever the report said.  A flag is a
boolean and never a null; a list is an array and never a null standing in for
an empty one; the text envelope is an array whatever happened.  Rows that are
allowed to vary -- a count that is null when nobody looked, a name that is
null when there is none -- are not here: they are stated per scenario, where
the reason they vary is stated with them.")

(defun response-shape (builder)
  "Return the shape rows for BUILDER: :LIST, :SYMBOL, :DESCRIBE or :CHECK."
  (or (rest (assoc builder +response-shape+))
      (error "No such builder ~S." builder)))

;;; ------------------------------------------------------------------------
;;; B. The text an MCP client renders

(defun json-text (response)
  "Return the text of RESPONSE's first content part, out of the parsed JSON.

Read from the document rather than from the hash-table: content[].text is the
only part of a tool result an MCP client is required to show, so it is the
part whose wording has to survive encoding."
  (let ((content (json-at (parse-response (response-json response)) "content")))
    (when (and (json-array-p content) (plusp (length content)))
      (gethash "text" (aref content 0)))))

(defun text-lines (text)
  "Return TEXT's lines."
  (let ((lines '()) (start 0))
    (loop for index = (position #\Newline text :start start)
          do (push (subseq text start index) lines)
             (if index (setf start (1+ index)) (return)))
    (nreverse lines)))

(defun first-line (text)
  "Return TEXT's first line."
  (first (text-lines text)))

(defun line-starting-with (text prefix)
  "Return the first line of TEXT that starts with PREFIX, or NIL."
  (find-if (lambda (line)
             (and (<= (length prefix) (length line))
                  (string= prefix line :end2 (length prefix))))
           (text-lines text)))

(defun replay-arguments (line)
  "Return the arguments the Replay LINE asks for, as a plist of strings.

Read from the line the response printed, by the grammar the line is written
in -- \"Replay: spec-check key=value ...\" -- rather than by evaluating it.  A
replay instruction is only worth checking if it is the one the caller sees,
and nothing here hands the caller's text to the reader or to a shell."
  (let ((prefix "Replay: spec-check "))
    (when (and line (<= (length prefix) (length line))
               (string= prefix line :end2 (length prefix)))
      (let ((arguments '())
            (start (length prefix)))
        (loop for space = (position #\Space line :start start)
              for token = (subseq line start space)
              do (let ((equals (position #\= token)))
                   (when equals
                     (setf arguments
                           (append arguments
                                   (list (subseq token 0 equals)
                                         (subseq token (1+ equals)))))))
                 (if space (setf start (1+ space)) (return)))
        arguments))))

(defun replay-argument (arguments key)
  "Return the value ARGUMENTS gives for KEY, or NIL."
  (loop for (name value) on arguments by #'cddr
        when (equal key name) return value))

(defun claims-p (text phrase)
  "True when TEXT contains PHRASE."
  (and (search phrase text) t))

(defparameter +verdict-tokens+
  '((:verified . "✓ VERIFIED")
    (:failed . "✗ FAILED")
    (:not-verified . "⚠ NOT VERIFIED")
    (:no-properties . "⚠ NO PROPERTIES"))
  "The four words a spec-check headline can open with.

Each carries its mark, so they can be compared as whole tokens.  Searching for
\"VERIFIED\" alone finds it inside \"NOT VERIFIED\" as well, which is how a
check for the good news passes on the bad.")

(defun verdict-token (verdict)
  "Return the headline token VERDICT is written as."
  (or (rest (assoc verdict +verdict-tokens+))
      (error "No such verdict ~S." verdict)))

;;; ------------------------------------------------------------------------
;;; C. Names and the environment

(defparameter +fixture-names+
  '((:subject "PROBE" "WITHDRAW!")
    (:property "PROBE" "WITHDRAW-KEEPS-THE-TOTAL")
    (:other-property "PROBE" "WITHDRAW-REFUSES-OVERDRAFT")
    (:elsewhere "PROBE-ELSEWHERE" "WITHDRAW-KEEPS-THE-TOTAL")
    (:spec "PROBE" "MONEY")
    (:argument "PROBE" "AMOUNT")
    (:generator "PROBE" "WITHDRAW-ARGUMENTS"))
  "The names these fixtures use.

:ELSEWHERE shares :PROPERTY's symbol name in another package: a response that
carries a name as one printed string, or compares two by name alone, makes
those two the same registration.")

(defun fixture-name (key)
  "Return (PACKAGE NAME) for the fixture name KEY."
  (or (rest (assoc key +fixture-names+))
      (error "No such fixture name ~S." key)))

(defun qualified (key)
  "Return the package-qualified designator of the fixture name KEY."
  (destructuring-bind (package name) (fixture-name key)
    (format nil "~A::~A" package name)))

(defun symbol-data (key)
  "Return the symbol plist the report layer carries for the fixture name KEY."
  (destructuring-bind (package name) (fixture-name key)
    (list :package package :name name :qualified (format nil "~A::~A" package name))))

(defun environment (&key (loaded t))
  "Return an environment plist, healthy unless LOADED is false."
  (if loaded
      (list :cl-spec-loaded t :cl-spec-status :ok :cl-spec-version "0.1.0"
            :cl-spec-system-directory "/tmp/cl-spec/"
            :generator-backend "CL-SPEC/SRC/BACKENDS/CHECK-IT:CHECK-IT-BACKEND"
            :backend-available t :registry "#<HASH-TABLE-REGISTRY>"
            :missing nil :lisp "SBCL 2.5.8")
      (list :cl-spec-loaded nil :cl-spec-status :not-loaded
            :backend-available nil :missing '("CL-SPEC") :lisp "SBCL 2.5.8")))

;;; ------------------------------------------------------------------------
;;; D. spec-list scenarios

(defparameter +listing-states+
  '(:present :none-registered :not-requested :unlistable :tag-unfilterable)
  "The five answers a listing gives about one kind.

:NONE-REGISTERED and the three after it all come back with no names.  What
separates them is why, and a response that renders all four the same way tells
a caller the registry is empty when nobody looked.")

(defun %listing-entries (state)
  "Return the property entries a listing in STATE reports.

An entry is not a bare name: it carries the kind, the tags and the
documentation a caller scans the listing for, and the name itself is a symbol
in two fields.  The two entries here share a symbol name across packages."
  (case state
    (:present (list (list :name (symbol-data :property)
                          :kind :invariant
                          :tags '(:fast)
                          :targets (list (symbol-data :subject))
                          :documentation "A withdrawal moves money.")
                    (list :name (symbol-data :elsewhere)
                          :kind :invariant
                          :tags '()
                          :targets '()
                          :documentation nil)))
    (t '())))

(defun list-scenario (state &key (limit 50))
  "Return (values REPORT FACTS) for a spec-list report in STATE under LIMIT.

The limit is applied here, where the report layer applies it: it cuts the list
the report carries and sets the truncation flag, and it never touches the
count.  FACTS states what the response must say, in the terms this file chose
-- the count, whether the kind could be listed at all, which names come back
-- and is not computed from the report by a second copy of the report layer."
  (let* ((all (%listing-entries state))
         (shown (subseq all 0 (min (length all) limit)))
         (truncated (> (length all) limit))
         (listable (not (eq state :unlistable)))
         (requested (not (eq state :not-requested)))
         (tag (when (member state '(:tag-unfilterable :present)) "fast"))
         (tag-filterable (not (eq state :tag-unfilterable)))
         (counted (and requested listable tag-filterable))
         (count (when counted (length all)))
         (report
           (list :status :ok
                 :kind (if requested "properties" "specs")
                 :specs '()
                 :properties shown
                 :function-specs '()
                 :specs-listable t
                 :properties-listable listable
                 :function-specs-listable t
                 :tag-filterable tag-filterable
                 :counts (list :specs nil :properties count :function-specs nil)
                 :truncated truncated
                 :limit limit
                 :filters (list :package nil :tag tag
                                :tag-resolved (when tag :resolved)
                                :tag-applied (and tag tag-filterable t))
                 :coverage "What this registry holds."
                 :environment (environment))))
    (values report
            (list :state state
                  :count count
                  :listable listable
                  :tag-filterable tag-filterable
                  :tag-applied (and tag tag-filterable t)
                  :truncated truncated
                  :names (mapcar (lambda (entry)
                                   (getf (getf entry :name) :qualified))
                                 shown)
                  ;; A count of none is the one state that may print a zero.
                  :text-may-say-zero (eq state :none-registered)))))

(defun draw-list-case ()
  "Return a listing case: which of the six answers, and a limit."
  (list :state (nth (random (length +listing-states+)) +listing-states+)
        :limit (nth (random 3) '(1 2 50))))

;;; ------------------------------------------------------------------------
;;; E. spec-symbol scenarios

(defparameter +symbol-states+
  '(:all-three :spec-only :nothing-registered :runtime-not-read :not-loaded
    :internal-error)
  "What a spec-symbol answer can be.

The last two are not registrations at all: one is an image without cl-spec,
the other a lookup that broke.  Rendered as \"nothing is registered\" either
would answer a question nobody asked.")

(defun symbol-scenario (state)
  "Return (values REPORT FACTS) for a spec-symbol report in STATE."
  (let ((registered (member state '(:all-three :spec-only :runtime-not-read)))
         (report
           (case state
             (:not-loaded (list :status :cl-spec-not-loaded
                                :message "cl-spec is not loaded in this image."
                                :environment (environment :loaded nil)))
             (:internal-error (list :status :internal-error
                                    :name (symbol-data :subject)
                                    :message "The registry reader signalled."
                                    :environment (environment)))
             (t
              (list :status :ok
                    :symbol (symbol-data :subject)
                    :runtime (unless (eq state :runtime-not-read)
                               (list :type "function"
                                     :arglist "(ACCOUNT AMOUNT)"
                                     :documentation "Withdraw AMOUNT."
                                     :source-file "/tmp/probe/account.lisp"
                                     :source-line 12))
                    :runtime-unavailable-reason
                    (when (eq state :runtime-not-read)
                      "include_runtime was false, so no runtime was read.")
                    :registry
                    (list :spec (when (member state '(:all-three :spec-only))
                                  (symbol-data :subject))
                          :function-spec (when (eq state :all-three)
                                           (symbol-data :subject))
                          :property (when (eq state :all-three)
                                      (symbol-data :subject))
                          :properties-about
                          (when (eq state :all-three)
                            (list (symbol-data :property) (symbol-data :elsewhere))))
                    :properties
                    (when (eq state :all-three)
                      (list (list :name (symbol-data :property)
                                  :kind :invariant
                                  :tags '(:fast)
                                  :documentation "A withdrawal moves money."
                                  :definition-digest "fnv1a64-v1:00000000000000dd"
                                  :trials-table "(:SMOKE 5 :NORMAL 25)"
                                  :body-omitted t
                                  :body-forms 3
                                  :detail-via "spec-describe kind=property")
                            (list :name (symbol-data :elsewhere)
                                  :kind :invariant
                                  :tags '()
                                  :definition-digest nil
                                  :body-omitted t
                                  :body-forms 1
                                  :detail-via "spec-describe kind=property")))
                    :nothing-registered (eq state :nothing-registered)
                    :notes '()
                    :environment (environment))))))
    (values report
            (list :state state
                  :ok (not (member state '(:not-loaded :internal-error)))
                  :nothing-registered (eq state :nothing-registered)
                  :registered (and registered t)
                  :runtime-read (not (member state '(:runtime-not-read :not-loaded
                                                     :internal-error)))
                  :property-names (if (eq state :all-three)
                                      (list (qualified :property) (qualified :elsewhere))
                                      '())
                  :body-omitted (eq state :all-three)))))

(defun draw-symbol-case ()
  "Return a spec-symbol case: which of the six answers."
  (list :state (nth (random (length +symbol-states+)) +symbol-states+)))

;;; ------------------------------------------------------------------------
;;; F. spec-describe scenarios

(defparameter +describe-entities+ '(:contract :contract-signals :contract-cases
                                    :property :spec)
  "The five things spec-describe describes.")

(defparameter +clause-states+ '(:absent :present-nil :whole :cut)
  "What one clause of a declaration can be.

:ABSENT is \"this definition has no such clause\", :PRESENT-NIL is a clause
whose text is NIL, and they are different answers.  A boolean cannot carry
both, which is why the complete flag is null for one and false for neither.")

(defparameter +clause-text+ "(> AMOUNT 0)"
  "The text of a whole clause, written here rather than printed by the code
under test.")

(defparameter +cut-clause-text+ "(AND (> AMOUNT 0) (<= AMOUNT BALA"
  "The first 33 characters of a longer clause, as a budget of 33 leaves it.")

(defparameter +cut-clause-omitted+ 17
  "How many characters +CUT-CLAUSE-TEXT+ is missing.")

(defun %clause-plist (state)
  "Return the report keys for the precondition clause in STATE.

An absent clause contributes no text and no count, and :NOT-APPLICABLE rather
than NIL for its complete flag: that is what the response layer has to turn
into null rather than into false.  The keys are written out because this file
names the clause it describes; building them from a string would need INTERN,
which this project does not do at run time."
  (ecase state
    (:absent (list :preconditions nil
                   :preconditions-complete :not-applicable
                   :preconditions-omitted-chars nil))
    (:present-nil (list :preconditions "NIL"
                        :preconditions-complete t
                        :preconditions-omitted-chars 0))
    (:whole (list :preconditions +clause-text+
                  :preconditions-complete t
                  :preconditions-omitted-chars 0))
    (:cut (list :preconditions +cut-clause-text+
                :preconditions-complete nil
                :preconditions-omitted-chars +cut-clause-omitted+))))

(defun describe-scenario (entity &key (pre :whole) (documentation "A contract of this fixture's own."))
  "Return (values REPORT FACTS) for a spec-describe report of ENTITY.

PRE is the state of the precondition clause.  DOCUMENTATION is carried as
given: it is where the text this file controls meets the escaping JSON does."
  (let* ((cases (when (eq entity :contract-cases)
                  (list (list :name :sufficient
                              :guard "(<= AMOUNT BALANCE)"
                              :guard-complete t
                              :outcome :returns
                              :documentation "Enough money.")
                        (list :name :insufficient
                              :guard "(> AMOUNT BALANCE)"
                              :guard-complete t
                              :outcome :signals))))
         (report
           (append
            (list :status :ok
                  :kind (case entity
                          (:property "property")
                          (:spec "spec")
                          (t "function-spec"))
                  :name (symbol-data (case entity
                                       (:spec :spec)
                                       (:property :property)
                                       (t :subject)))
                  :documentation documentation
                  :environment (environment))
            (case entity
              (:spec (list :spec (list :kind :range :type "INTEGER" :min "1" :max "1000"
                                       :source-form "(RANGE INTEGER 1 1000)")))
              (:property
               (list :property-kind :invariant
                     :tags '(:fast)
                     :targets (list (symbol-data :subject))
                     :trials-table "(:SMOKE 5 :NORMAL 25)"
                     :shrink-enabled t
                     :body "(= (+ A B) (+ B A))"
                     :body-complete t
                     :body-omitted-chars 0
                     :arguments (list (list :variable (symbol-data :argument)
                                            :spec (list :kind :reference
                                                        :target (symbol-data :spec))))))
              (t
               (append
                (list :arguments (list (list :variable (symbol-data :argument)
                                             :kind :required
                                             :spec (list :kind :reference
                                                         :target (symbol-data :spec)))
                                       (list :variable (symbol-data :subject)
                                             :kind :key
                                             :keyword :force
                                             :supplied-p (symbol-data :argument)
                                             :spec (list :kind :type :type "BOOLEAN")))
                      :argument-generator (symbol-data :generator)
                      :capture (list (list :name (symbol-data :argument)
                                           :form "(BALANCE ACCOUNT)"
                                           :form-complete t
                                           :form-omitted-chars 0)))
                (if (eq entity :contract-signals)
                    (list :signals (list :kind :type :type "INSUFFICIENT-FUNDS"))
                    (list :returns (list :kind :type :type "INTEGER")))
                (when cases (list :case-selection :exclusive :cases cases))
                (%clause-plist pre))))))
         (facts
           (list :entity entity
                 :kind (getf report :kind)
                 :name (getf (getf report :name) :qualified)
                 :documentation documentation
                 ;; A property and a named spec have no precondition clause to
                 ;; be in a state: the keys are not part of that kind of
                 ;; definition, and PRE describes a contract's clause only.
                 :pre (if (member entity '(:contract :contract-signals :contract-cases))
                          pre
                          :absent)
                 :pre-text (case (if (member entity '(:contract :contract-signals
                                                      :contract-cases))
                                     pre
                                     :absent)
                             (:absent nil)
                             (:present-nil "NIL")
                             (:whole +clause-text+)
                             (:cut +cut-clause-text+))
                 :pre-omitted (case pre (:cut +cut-clause-omitted+) (:absent nil) (t 0))
                 :argument-kinds (when (member entity '(:contract :contract-signals
                                                        :contract-cases))
                                   '("required" "key"))
                 :case-names (when cases '("sufficient" "insufficient"))
                 :outcome (cond ((eq entity :contract-signals) :signals)
                                ((member entity '(:contract :contract-cases)) :returns)
                                (t nil))
                 ;; A property describe carries no :SHRINK-ENABLED for a
                 ;; contract and no :PRECONDITIONS for a property: the keys
                 ;; that are simply not part of that kind must arrive as null,
                 ;; never as a false that says the definition declared it off.
                 :shrink-enabled (case entity
                                   (:property :true)
                                   (t :null)))))
    (values report facts)))

(defun draw-describe-case ()
  "Return a describe case: which entity, and the state of its precondition."
  (list :entity (nth (random (length +describe-entities+)) +describe-entities+)
        :pre (nth (random (length +clause-states+)) +clause-states+)
        :documentation (nth (random 4)
                            (list "A contract of this fixture's own."
                                  "引き落としは残高を超えない。"
                                  "Quotes \"like this\", a backslash \\ and a
second line."
                                  ""))))

(defparameter +check-cases+
  '(:passed-with-gaps :has-failure :no-properties :no-effective-trials
    :case-never-reached :properties-only :contract-only :generation-failed
    :timed-out-result :digest-moved :empty-counterexample
    :no-counterexample
    :counterexample-not-collected :capture-collected-nil :capture-unavailable
    :shrink-limited)
  "The sixteen spec-check answers these fixtures describe.

Each is a state the report layer actually produces, not a combination of keys
assembled because each key exists somewhere.  Between them they cover the
three verdicts, a run that selected nothing, a run whose trials never
happened, evidence that is absent against evidence that could not be read, and
a digest that disagrees with the caller while the run itself holds.")

(defparameter +check-robustness-cases+
  '(:timeout)
  "Answers BUILD-SPEC-CHECK-RESPONSE handles that spec-check does not produce.

A whole-call timeout is one: %WITHIN-DEADLINE wraps spec-list, spec-symbol and
spec-describe, and SPEC-CHECK-RESPONSE calls CHECK-REPORT without it, so no
spec-check call carries that status today -- which is why it is not in
+CALL-STATUSES+ either.  The builder is written to answer it anyway, and what
it answers is checked here, apart from the positive examples so that a
robustness case cannot be mistaken for a state the report layer builds.")

(defun %value (printed &key (type "integer") (complete t) (omitted 0) object-id)
  "Return one externalized value, as the report layer carries it."
  (list :printed printed :printed-complete complete :omitted-chars omitted
        :type type :object-id object-id :restorable complete))

(defun %counterexample (&rest printed)
  "Return a counterexample binding each of PRINTED to a fixture argument."
  (loop for text in printed
        for key in '(:argument :subject :spec)
        collect (list :variable (symbol-data key) :value (%value text))))

(defun %result (&key (name :property) (kind :property) (status :passed)
                  (seed "3963993791726803706") (seed-p t) (profile :normal)
                  (executed 25) (budget 25) contract counterexample
                  (counterexample-status :not-applicable) counterexample-unavailable-reason
                  shrunk (shrink-status :not-applicable) shrink-note
                  (digest "fnv1a64-v1:00000000000000dd") (match :not-checked)
                  core-record message condition)
  "Return one per-property result, as %RESULT-PLIST builds it for a run that
returned.

The defaults are a passing run.  Only :FAILED and :ERROR are verdicts a
counterexample belongs to, so a pass answers :NOT-APPLICABLE for the
counterexample and for the shrink search alike; a failure states its own
pair.  SEED-P false leaves the key out.  A run that never returned -- a
timeout, a condition before any result -- is not built here but by
%UNFINISHED-RESULT, whose shape is %RUN-ONE's and not this one."
  (append
   (list :property (symbol-data name)
         :kind kind
         :status status
         :profile (unless (eq kind :contract) profile))
   (when seed-p (list :seed seed))
   (list :trials (list :executed executed :budget budget
                       :budget-source "backend-default")
         :contract contract
         :counterexample counterexample
         :counterexample-status counterexample-status
         :counterexample-unavailable-reason counterexample-unavailable-reason
         :shrunk-counterexample shrunk
         :shrink-status shrink-status
         :shrink-note shrink-note
         :condition condition
         :definition-digest digest
         :definition-digest-complete t
         :definition-match match
         :core-record core-record
         :elapsed 0.02
         :message message)))

(defun %unfinished-result (status &key condition (timeout-seconds 60) thread-leaked
                                       message)
  "Return one per-property result for a run that returned nothing, as
%RUN-ONE builds it.

STATUS is :TIMEOUT, or the status a condition signalled before any result was
classified as.  Neither reached a verdict, so neither has a seed, a profile
or an executed count to report, and the counterexample and the shrink search
answer :UNAVAILABLE with the reason %RUN-ONE gives.  A timeout says whether
its thread was left running; a condition is carried as the condition."
  (append
   (list :property (symbol-data :property)
         :kind :property
         :status status
         :trials (list :budget 25 :budget-source "backend-default")
         :definition-digest "fnv1a64-v1:00000000000000dd"
         :definition-digest-covers :property
         :definition-digest-complete t
         :definition-match :not-checked
         :counterexample-status :unavailable
         :counterexample-unavailable-reason
         (if (eq :timeout status)
             "the run did not reach a verdict within its deadline"
             "the run signalled before producing a result")
         :shrink-status :unavailable)
   (if (eq :timeout status)
       (list :timeout-seconds timeout-seconds
             :thread-leaked thread-leaked
             :message message)
       (list :condition condition))))

(defun %projected-record (status &rest fields)
  "Return the core record the report layer publishes for a cl-spec v1 result
record of STATUS whose FIELDS (a plist) replace the defaults.

The record is a valid one from specs/core-record-fixtures.lisp, and the core
record is what PROJECT-CORE-RECORD makes of it -- the call %RESULT-PLIST
makes.  Its :SOURCE is what the text is written from and its :DATA is what the
JSON carries, so both have the shape a real report has.  The values under
FIELDS are the ones the checks expect to find; nothing here recomputes them
from the projection."
  (let ((record (make-result-record :status status)))
    (loop for (key value) on fields by #'cddr
          do (setf record (record-with record key value)))
    (multiple-value-bind (core state reason)
        (project-core-record record :result-data :expected-record-kind :result)
      (unless (eq :ok state)
        (error "The fixture record did not project: ~S ~S." state reason))
      core)))

(defun %capture-record (record)
  "Return one capture RECORD as cl-spec's raw source carries it."
  (if (eq :collected (getf record :availability))
      (list :name (getf record :name)
            :availability :collected
            ;; The application's own value, as the code under test produced
            ;; it.  NIL here is a value, not an absence.
            :value (getf record :value))
      (list :name (getf record :name)
            :availability :unavailable
            :reason (getf record :reason)
            :type (getf record :type))))

(defun %capture-evidence (records)
  "Return the core record of a failure that captured RECORDS.

The capture sits where cl-spec puts it, under the failure's state, so the
payload carries it at core_result.data.failure.state.capture.values."
  (%projected-record :failed
                     :failure
                     (list :state
                           (list :capture
                                 (list :status :collected
                                       :values (mapcar #'%capture-record records))))))

(defun %case-report (never-called)
  "Return the core record of a passing run that declared cases and missed some.

The names under :NEVER-CALLED are the declared cases no trial reached, which
is the gap a bare verdict hides.  The payload carries them at
core_result.data.case_report.never_called."
  (%projected-record :passed :case-report (list :never-called never-called)))

(defun check-scenario (case)
  "Return (values REPORT FACTS) for the spec-check answer CASE.

Each report is a state CHECK-REPORT builds, in its own vocabulary: the
statuses of +RESULT-STATUSES+ and +CALL-STATUSES+, the gaps of
+VERIFICATION-GAP-VALUES+ in the order %VERIFICATION-GAPS appends them, and
the counterexample and shrink answers a run that reached a verdict gives.  A
combination the report layer does not build is not a positive example of
anything, however plausible it reads.

Two consequences worth naming, because they are where a hand-written report
tends to drift.  Every run carries at least :INPUT-COVERAGE-UNMEASURED, and a
property run carries :REJECTION-COUNTS-UNMEASURED beside it -- there is no
such thing as a check with no gaps.  And :VERIFIED is decided by the results
alone: a digest that disagrees with the caller, or a contract nobody ran,
leaves it true and is reported beside it.

FACTS says what the response must carry and what its text must and must not
claim.  It is written here; nothing recomputes it from the report."
  (let* ((property (symbol-data :property))
         (other (symbol-data :other-property))
         (subject (symbol-data :subject))
         (other-digest "fnv1a64-v1:00000000000000ee")
         (property-gaps '("rejection-counts-unmeasured" "input-coverage-unmeasured"))
         (selection
           (list :mode "explicit" :kind :property
                 :requested (list :property property)
                 :selected (list property)
                 :count 1
                 :source "explicit property argument"
                 :coverage "Only the property named."
                 :notes '()))
         (contract-selection
           (list :mode "function" :kind :contract
                 :requested (list :function subject)
                 :selected (list subject)
                 :count 1
                 :source "explicit function argument"
                 :coverage "Only the contract named."
                 :notes '()
                 :properties-not-run '()
                 :properties-not-run-read t))
         (usable-contract (list :rejection-status :measured
                                :precondition-p t
                                :rejected 0
                                :rejected-measured t
                                :rejected-readable t
                                :rejected-usable t
                                :effective-trials 25))
         (base (list :status :completed
                     :verified t
                     :selection selection
                     :results (list (%result))
                     :counts (list :selected 1 :passed 1 :failed 0 :errored 0
                                   :timed-out 0 :not-run 0 :other 0
                                   :by-status '((:passed . 1)))
                     :profile :normal
                     :timeout-seconds 60
                     :thread-leaked nil
                     :worker-reuse :safe
                     :verification-gaps '(:rejection-counts-unmeasured
                                          :input-coverage-unmeasured)
                     :elapsed 0.5
                     :reproduction-faithful :not-checked
                     :environment (environment)))
         (facts (list :case case
                      :verdict :verified
                      :verified :true
                      :status "completed"
                      :gaps property-gaps
                      ;; Every answer but the zero-selection one prints its
                      ;; gaps on a line of their own.
                      :gap-line t
                      :text-must '()
                      :text-must-not '()
                      ;; A pass is not a verdict a counterexample belongs to.
                      :counterexample :not-applicable
                      :replay (list :target :property
                                    :name (qualified :property)
                                    :seed "3963993791726803706"
                                    :profile "normal"
                                    :digest "fnv1a64-v1:00000000000000dd"))))
    (flet ((report (&rest overrides)
             (loop for (key value) on overrides by #'cddr
                   do (setf (getf base key) value))
             base)
           (expect (&rest overrides)
             (loop for (key value) on overrides by #'cddr
                   do (setf (getf facts key) value))
             facts)
           (failed-counts ()
             (list :selected 1 :passed 0 :failed 1 :errored 0
                   :timed-out 0 :not-run 0 :other 0
                   :by-status '((:failed . 1)))))
      (ecase case
        (:passed-with-gaps
         ;; The plain passing run.  It still carries two gaps, because two
         ;; things are never measured, and a verdict is not a summary of them.
         (values (report)
                 (expect :text-must '("verification gaps:"
                                      "input-coverage-unmeasured"
                                      "rejection-counts-unmeasured"
                                      "verified: true"))))
        (:has-failure
         (values (report :verified nil
                         :selection (append (list :selected (list property other)
                                                  :count 2)
                                            selection)
                         :results (list (%result)
                                        (%result :name :other-property
                                                 :status :failed
                                                 :seed "11"
                                                 :digest other-digest
                                                 :counterexample
                                                 (%counterexample "68" "0")
                                                 :counterexample-status :present
                                                 ;; Shrinking ran and found
                                                 ;; nothing smaller.
                                                 :shrink-status :none))
                         :counts (list :selected 2 :passed 1 :failed 1 :errored 0
                                       :timed-out 0 :not-run 0 :other 0
                                       :by-status '((:passed . 1) (:failed . 1))))
                 (expect :verdict :failed
                         :verified :false
                         :counterexample :present
                         :text-must (list "AMOUNT = 68" (qualified :other-property))
                         ;; The replay line points at the failure, with that
                         ;; result's own seed and its own digest -- the two
                         ;; results carry different ones so a line that mixed
                         ;; them could be seen.
                         :replay (list :target :property
                                       :name (qualified :other-property)
                                       :seed "11"
                                       :profile "normal"
                                       :digest other-digest))))
        (:no-properties
         (values (report :status :no-properties
                         :verified nil
                         :results '()
                         :selection (list :mode "about" :kind :about
                                          :requested (list :symbol subject)
                                          :selected '()
                                          :count 0
                                          :source "properties about the symbol"
                                          :coverage "Nothing was selected."
                                          :notes (list "A contract is registered for this symbol."))
                         :counts (list :selected 0 :passed 0 :failed 0 :errored 0
                                       :timed-out 0 :not-run 0 :other 0
                                       :by-status '())
                         :verification-gaps '(:no-properties-selected
                                              :rejection-counts-unmeasured
                                              :input-coverage-unmeasured)
                         :message "No property is registered about this symbol.")
                 (expect :verdict :no-properties
                         :verified :false
                         :status "no-properties"
                         :gaps (cons "no-properties-selected" property-gaps)
                         ;; This answer's text is a short form with no gap
                         ;; line.  What it says instead is that nothing was
                         ;; selected and that nothing is verified, which is
                         ;; what the gaps here amount to.
                         :gap-line nil
                         :text-must (list "Selected 0 properties"
                                          "A contract is registered for this symbol."
                                          "verified: false")
                         :replay nil)))
        (:no-effective-trials
         ;; A contract whose every generated argument was refused: the trials
         ;; ran, and none of them reached the function.  cl-spec says passed;
         ;; what it passed is nothing, so the verdict is not verified and the
         ;; gap names the budget that resolved to no evaluation.
         (values (report :verified nil
                         :selection contract-selection
                         :results (list (%result :name :subject :kind :contract
                                                 :contract
                                                 (append (list :rejected 25
                                                               :effective-trials 0)
                                                         usable-contract)))
                         :verification-gaps '(:zero-trials :input-coverage-unmeasured))
                 (expect :verdict :not-verified
                         :verified :false
                         ;; A contract's refusals are counted, so that gap is
                         ;; not listed for it.
                         :gaps '("zero-trials" "input-coverage-unmeasured")
                         :effective-trials 0
                         :text-must '("zero-trials")
                         :replay (list :target :function
                                       :name (qualified :subject)
                                       :trials 25
                                       :seed "3963993791726803706"
                                       :digest "fnv1a64-v1:00000000000000dd"))))
        (:case-never-reached
         (values (report :verified nil
                         :results (list (%result :core-record
                                                 (%case-report '(:insufficient))))
                         :verification-gaps (list* :cases-never-called
                                                   '(:rejection-counts-unmeasured
                                                     :input-coverage-unmeasured)))
                 (expect :verdict :not-verified
                         :verified :false
                         :gaps (cons "cases-never-called" property-gaps)
                         :case-never-called "insufficient"
                         :text-must '("insufficient" "never reached"))))
        (:properties-only
         ;; Everything selected passed, so the run is verified.  What it does
         ;; not cover is the contract nobody ran, and that is said in the same
         ;; line as the verdict rather than instead of it.
         (values (report :selection (append (list :contract-not-run subject) selection)
                         :verification-gaps (list* :contract-not-run
                                                   '(:rejection-counts-unmeasured
                                                     :input-coverage-unmeasured)))
                 (expect :gaps (cons "contract-not-run" property-gaps)
                         :text-must (list "properties only" (qualified :subject)
                                          "verified: true"))))
        (:contract-only
         (values (report :selection (append (list :properties-not-run
                                                  (list property other))
                                            contract-selection)
                         :results (list (%result :name :subject :kind :contract
                                                 :contract usable-contract))
                         :verification-gaps '(:properties-not-run
                                              :input-coverage-unmeasured))
                 (expect :gaps '("properties-not-run" "input-coverage-unmeasured")
                         :text-must '("contract only" "2 properties")
                         :replay (list :target :function
                                       :name (qualified :subject)
                                       :trials 25
                                       :seed "3963993791726803706"
                                       :digest "fnv1a64-v1:00000000000000dd"))))
        (:generation-failed
         ;; Nothing was falsified, because nothing ran: the backend could not
         ;; generate, and the run signalled before it returned a result.  That
         ;; is %RUN-ONE's error path: the condition is classified, it is not a
         ;; verdict, so the call is incomplete, and the evidence the run never
         ;; produced is unavailable rather than empty.
         (values (report :status :incomplete
                         :verified nil
                         :results (list (%unfinished-result
                                         :generator-error
                                         :condition (list :type "GENERATOR-UNAVAILABLE"
                                                          :message "no generator backend")))
                         :counts (list :selected 1 :passed 0 :failed 0 :errored 0
                                       :timed-out 0 :not-run 0 :other 1
                                       :by-status '((:generator-error . 1)))
                         :verification-gaps (list* :generator-error
                                                   '(:rejection-counts-unmeasured
                                                     :input-coverage-unmeasured)))
                 (expect :verdict :not-verified
                         :verified :false
                         :status "incomplete"
                         :gaps (cons "generator-error" property-gaps)
                         :counterexample :unavailable
                         :text-must '("generator-error" "no generator backend")
                         :text-must-not '("✗ FAILED")
                         :replay nil)))
        (:timeout
         ;; The call itself ran out of time, so there is no run to report on.
         ;; This answer carries a message and no selection, and its text is
         ;; the status and that message -- not a verdict about anything.
         (values (list :status :timeout
                       :verified nil
                       :message "reading the registry exceeded its 60 second deadline."
                       :environment (environment))
                 (expect :verdict :status-only
                         :verified :false
                         :status "timeout"
                         :gaps '()
                         :text-must '("exceeded its 60 second deadline")
                         :replay nil)))
        (:timed-out-result
         ;; The call finished; one selected property did not.  A timeout is
         ;; not a verdict, so the call is incomplete, the result carries no
         ;; seed to replay from, and the evidence it has none of says
         ;; "unavailable" rather than "none".  Its thread was stopped, not
         ;; left running, so reuse is :UNKNOWN -- a leaked thread would make
         ;; it :UNSAFE.
         (values (report :status :incomplete
                         :verified nil
                         :results (list (%unfinished-result
                                         :timeout
                                         :thread-leaked nil
                                         :message "The run thread was stopped."))
                         :counts (list :selected 1 :passed 0 :failed 0 :errored 0
                                       :timed-out 1 :not-run 0 :other 0
                                       :by-status '((:timeout . 1)))
                         :worker-reuse :unknown
                         :worker-reuse-message
                         "Nothing here can show that the state it was changing was restored."
                         :thread-leaked nil
                         :verification-gaps (list* :timeout
                                                   '(:rejection-counts-unmeasured
                                                     :input-coverage-unmeasured)))
                 (expect :verdict :not-verified
                         :verified :false
                         :status "incomplete"
                         :gaps (cons "timeout" property-gaps)
                         :counterexample :unavailable
                         :text-must '("worker_reuse: unknown" "timeout"
                                      "state it was changing was restored")
                         ;; A timeout is not a counterexample, and a result
                         ;; with no seed has nothing to replay.
                         :text-must-not '("✗ FAILED")
                         :replay nil)))
        (:digest-moved
         ;; The run holds, and it is reported as holding.  What it does not do
         ;; is reproduce the run the caller named, and that is a separate
         ;; sentence beside the verdict -- not a verdict of its own.
         (values (report :reproduction-faithful :false
                         :results (list (%result :match :false)))
                 (expect :match "mismatch"
                         :text-must '("did NOT reproduce" "reproduction:"
                                      "verified: true"))))
        (:empty-counterexample
         ;; A property of no arguments can fail, and its counterexample is
         ;; present with nothing in it.
         (values (report :verified nil
                         :results (list (%result :status :failed
                                                 :counterexample '()
                                                 :counterexample-status :present
                                                 :shrink-status :present))
                         :counts (failed-counts))
                 (expect :verdict :failed
                         :verified :false
                         :counterexample :present-empty)))
        (:no-counterexample
         ;; A failure the backend reported no arguments for, on a property
         ;; whose argument list was read.  "None reported" is what that is.
         (values (report :verified nil
                         :results (list (%result :status :failed
                                                 :counterexample-status :none
                                                 :shrink-status :none))
                         :counts (failed-counts))
                 (expect :verdict :failed
                         :verified :false
                         :counterexample :none
                         :text-must '("none reported by the backend"))))
        (:counterexample-not-collected
         (values (report :verified nil
                         :results (list (%result :status :failed
                                                 :counterexample-status :unavailable
                                                 :counterexample-unavailable-reason
                                                 "the backend recorded no arguments"
                                                 :shrink-status :unavailable))
                         :counts (failed-counts))
                 (expect :verdict :failed
                         :verified :false
                         :counterexample :unavailable
                         :text-must '("UNAVAILABLE"))))
        (:capture-collected-nil
         ;; The captured value is NIL, and NIL is a value the code under test
         ;; produced.  Rendered as "unavailable" it would become a fact about
         ;; cl-mcp instead of one about the run.
         (values (report :verified nil
                         :results (list (%result :status :failed
                                                 :counterexample (%counterexample "3")
                                                 :counterexample-status :present
                                                 :shrink-status :present
                                                 :core-record
                                                 (%capture-evidence
                                                  (list (list :name "BALANCE"
                                                              :availability :collected
                                                              :value nil
                                                              :printed "NIL"
                                                              :type "null")))))
                         :counts (failed-counts))
                 (expect :verdict :failed
                         :verified :false
                         :counterexample :present
                         :capture (list :availability "collected" :printed "NIL")
                         :text-must '("captured:" "BALANCE = NIL")
                         :text-must-not '("BALANCE = UNAVAILABLE"))))
        (:capture-unavailable
         (values (report :verified nil
                         :results (list (%result :status :failed
                                                 :counterexample (%counterexample "3")
                                                 :counterexample-status :present
                                                 :shrink-status :present
                                                 :core-record
                                                 (%capture-evidence
                                                  (list (list :name "BALANCE"
                                                              :availability :unavailable
                                                              :reason :not-restorable
                                                              :type 'integer)))))
                         :counts (failed-counts))
                 (expect :verdict :failed
                         :verified :false
                         :counterexample :present
                         :capture (list :availability "unavailable"
                                        :reason "not-restorable")
                         :text-must '("BALANCE = UNAVAILABLE" "not-restorable")
                         ;; The value is not there to print, and a line that
                         ;; printed one would be cl-mcp's invention.
                         :text-must-not '("BALANCE = NIL"))))
        (:shrink-limited
         ;; The finding stands; only the reduction is unfinished.  The search
         ;; came back with nothing smaller, and the record says why -- which
         ;; is what tells "found nothing" from "never really looked".
         (values (report :verified nil
                         :results (list (%result :status :failed
                                                 :counterexample (%counterexample "68" "0")
                                                 :counterexample-status :present
                                                 :shrink-status :none
                                                 :core-record
                                                 (%projected-record
                                                  :failed
                                                  :shrink-report
                                                  (list :termination :budget-exhausted))))
                         :counts (failed-counts))
                 (expect :verdict :failed
                         :verified :false
                         :counterexample :present
                         :shrink-termination "budget-exhausted"
                         :text-must '("AMOUNT = 68" "shrinking: budget-exhausted"
                                      "the shrink budget ran out")
                         ;; The reduction is unfinished; the finding is not.
                         :text-must-not '("did not reach a verdict"))))))))

(defun draw-check-case ()
  "Return a spec-check case: which of the sixteen answers."
  (list :case (nth (random (length +check-cases+)) +check-cases+)))
