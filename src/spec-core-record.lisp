;;;; src/spec-core-record.lisp
;;;;
;;;; One cl-spec versioned record, turned into data an MCP client can read.
;;;;
;;;; Nothing here calls cl-spec.  The input is a plist cl-spec already
;;;; produced -- RESULT-DATA or FUNCTION-SPEC-DATA -- so every rule in this
;;;; file can be exercised against the revisions this project cannot install,
;;;; including ones that do not exist yet.
;;;;
;;;; Three rules shape it.  A record is validated before it is read, because a
;;;; versioned API that answers something malformed is a fault to report
;;;; rather than a reason to fall back to an older reader.  Availability is
;;;; decided with GET-PROPERTIES and never with GETF, because a key that is
;;;; absent and a key whose value is NIL are different answers and GETF gives
;;;; the same one for both.  And projection is driven by a schema descriptor
;;;; rather than by the shape of the value, because the shape does not say
;;;; what the value means: (:AT-LEAST :AT-MOST) is two case names and
;;;; (:KIND :RANGE) is a plist, and nothing in the conses tells them apart.

(defpackage #:cl-mcp/src/spec-core-record
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:symbol-data
                #:externalize-value)
  (:export #:safe-json-integer-p
           #:project-value
           #:project-record
           #:project-core-record
           #:validate-versioned-record
           #:field-availability
           #:+v1-required-metadata+
           #:+sentinel-fields+
           #:*projection-max-depth*
           #:*projection-max-length*
           #:*record-shapes*))

(in-package #:cl-mcp/src/spec-core-record)

(defconstant +max-safe-json-integer+ (1- (expt 2 53))
  "The largest integer a JSON consumer holds exactly.

A double's mantissa is 53 bits, so a consumer reading JSON into doubles --
which is every JavaScript one -- rounds anything wider.  An SBCL fixnum reaches
2^62, and a cl-spec seed is a fixnum, which is why this adapter has always sent
seeds as text.  The rule is stated once here rather than at each site that
publishes a number.")

(defun safe-json-integer-p (value)
  "Return true when VALUE is an integer every JSON consumer holds exactly."
  (and (integerp value)
       (<= (- +max-safe-json-integer+) value +max-safe-json-integer+)))

(defun %opaque-marker-node (value)
  "Return cl-spec's unfreezable-value marker as a node, or NIL when VALUE is not one.

cl-spec publishes (:UNAVAILABLE :REASON :OPAQUE-VALUE :TYPE <type>) in place of a
captured value it could not freeze as evidence.  Externalizing that list registers
an object id for the marker itself, so the response would offer an inspection of
the very object cl-spec had just declined to keep -- and would report its type as
CONS, the marker's own type, rather than the type cl-spec named.

Called from both PROJECT-VALUE's own fall-through and PROJECT-RECORD's WALK for
an :OPAQUE field: the marker can appear anywhere a captured value can, and
:OPAQUE is exactly WALK's route for a value cl-spec did not describe -- the two
hooks share this one recognizer so they cannot drift apart."
  (when (and (consp value)
             (eq :unavailable (first value))
             (eq :opaque-value (getf (rest value) :reason)))
    (list :object
          (list (cons "unavailable" (list :scalar t))
                (cons "reason" (list :scalar "opaque-value"))
                (cons "type" (project-value (getf (rest value) :type)))))))

(defun project-value (value &key (max-chars 2000))
  "Return VALUE as a tagged projection node, decided by its type.

The node is (:SCALAR x), (:SYMBOL plist) or (:VALUE plist).  The tag is for the
renderer: a SYMBOL-DATA plist and an EXTERNALIZE-VALUE plist are both plists,
and a renderer that had to tell them apart by looking for :QUALIFIED or
:PRINTED would break the first time either grew a key.

A keyword becomes its lower-case name, because cl-spec uses keywords as the
vocabulary of its statuses and kinds and a client renders them as words.  NIL
is a scalar null rather than the symbol COMMON-LISP::NIL, or every absent value
in every record would arrive as a symbol reference.  Any other symbol keeps its
package: two same-named symbols from different packages are different
definitions.

An integer inside the JSON-safe range stays a number; a wider one becomes a
decimal string rather than a number a consumer would round.  Everything else --
a CLOS instance, a structure, a hash table, a function, a value from the code
under test -- goes through EXTERNALIZE-VALUE, which prints it bounded and
offers an object id instead of pretending the text is the object.

One shape of cons is the exception: %OPAQUE-MARKER-NODE recognizes cl-spec's
own (:UNAVAILABLE :REASON :OPAQUE-VALUE :TYPE type) marker, its statement that
a value could not be frozen as evidence, and this keeps that statement rather
than handing it to EXTERNALIZE-VALUE, which would register an object id for
the marker list itself instead of for the value cl-spec declined to keep."
  (cond ((keywordp value) (list :scalar (string-downcase (symbol-name value))))
        ((null value) (list :scalar nil))
        ((symbolp value) (list :symbol (symbol-data value)))
        ((stringp value) (list :scalar value))
        ((safe-json-integer-p value) (list :scalar value))
        ((integerp value) (list :scalar (format nil "~D" value)))
        (t (or (%opaque-marker-node value)
               (list :value (externalize-value value :max-chars max-chars))))))

(defparameter *projection-max-depth* 12
  "How deep a record projection descends before it externalizes the rest.

Matched to *VALUE-PRINT-LEVEL*, which bounds the printer this module hands its
leaves to: a record whose depth ran past what the printer would show anyway
costs traversal for output nobody sees.")

(defparameter *projection-max-length* 200
  "How many entries of one list or plist a record projection keeps.")

(defvar *record-shapes* '()
  "Plist of NAME to descriptor, for the descriptors that refer to themselves.

An EXPLAIN-DATA error datum holds :ERRORS, a list of error datums, so its
descriptor cannot be written as a literal without (:REF :ERROR-DATUM).")

(defun %json-key (keyword)
  "Return KEYWORD as the snake_case JSON key this project publishes under.

Keys are snake_case and values keep their hyphens -- \"budget_source\" holding
\"not-collected\" -- which is the convention every existing response follows."
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun %dotted-path (path)
  "Return PATH, a list of key names and indices, as one dotted string."
  (format nil "~{~A~^.~}" path))

(defun %resolve-descriptor (descriptor)
  "Return DESCRIPTOR with a (:REF NAME) indirection followed."
  (if (and (consp descriptor) (eq :ref (first descriptor)))
      (getf *record-shapes* (second descriptor))
      descriptor))

(defun %tail-unit-count (tail unit cap)
  "Return (values COUNT EXACT-P) for TAIL's length in units of UNIT conses,
walked at most CAP units so this always terminates.

TAIL may be circular or far longer than any real record, so this never calls
LENGTH on it.  NTHCDR always takes exactly the steps it is asked for,
regardless of what it is walking, so stepping through TAIL that way is safe
where LENGTH is not.  COUNT is exact -- TAIL's true length in units -- when
TAIL runs out within CAP units.  Otherwise COUNT is CAP, the most this walked
and not a total it never reached, and EXACT-P is NIL."
  (let ((rest tail))
    (dotimes (count cap)
      (when (null rest)
        (return-from %tail-unit-count (values count t)))
      (setf rest (nthcdr unit rest)))
    (if (null rest)
        (values cap t)
        (values cap nil))))

(defparameter +sentinel-fields+
  '(:shrink-report :generation-report :case-report
    :digest-omissions :digest-exclusions)
  "The fields whose :NOT-COLLECTED value means availability rather than data.

:NOT-COLLECTED is not a sentinel wherever it appears.  On an observation's
:OUTCOME it means the Function Spec target was never called, and inside
provenance's :COLLECTION-STATES it names an item nobody collected -- both are
the answer, not the absence of one.  Converting either would delete a fact.

Declared ahead of PROJECT-RECORD, which now reads this list too: WALK-OBJECT
projects a sentinel field's bare :NOT-COLLECTED value as JSON null rather than
walking it under the field's own container descriptor.")

(defun project-record (value descriptor &key path (max-chars 2000))
  "Project VALUE under DESCRIPTOR and return (values NODE ISSUES UNKNOWN-KEYS).

DESCRIPTOR says what VALUE means; VALUE's own shape never decides.  That is the
point: (:AT-LEAST :AT-MOST) is two case names and (:KIND :RANGE) is a plist,
and no test on the conses tells them apart, so a projector that guessed would
publish a key/value relation cl-spec never declared.

NODE is one of (:SCALAR x), (:SYMBOL plist), (:VALUE plist),
(:OBJECT ((key . NODE) ...)) or (:ARRAY (NODE ...)).  ISSUES records every
place the projection was cut, as (:PATH path :REASON reason [:OMITTED-ITEMS n
:OMITTED-ITEMS-EXACT-P boolean]).  OMITTED-ITEMS is exact when EXACT-P is
true; otherwise it is only how far %TAIL-UNIT-COUNT got before giving up, not
the value's true excess, because counting that exactly could mean walking
however long an untrusted value turns out to be.  UNKNOWN-KEYS names the keys
no descriptor covers: their existence is reported and their meaning is
deliberately not guessed.  A field descriptor of :ELSEWHERE means the record
declares this key and cl-mcp already publishes it outside :DATA, so
WALK-OBJECT names it in neither NODE nor UNKNOWN-KEYS -- a declared omission,
not an unrecognized one.

A container descriptor is chosen by what cl-spec's own accessor is documented
to build, never guessed from the cons cells in front of it.  Two of them
produce the same {name, value} JSON from different Lisp shapes, which is
exactly the trap that rule exists to prevent: (:ALIST D) reads
((NAME . VALUE) ...) -- dotted pairs, CAR and CDR apart -- the shape
CAPTURE-EVIDENCE's :VALUES is measured as, e.g. ((BALANCE-BEFORE . 30)).
(:PAIRS D) reads a flat (NAME VALUE NAME VALUE ...) plist two at a time --
cl-spec's own NAME-ARGUMENTS shape -- the shape a counterexample is measured
as, e.g. (BALANCE 5 AMOUNT 5).  Pointing a plist field at (:ALIST D) calls CAR
and CDR on a bare argument-value symbol and signals a TYPE-ERROR; pointing an
alist field at (:PAIRS D) reads a dotted pair's CDR as the next NAME.  Neither
mistake is caught by a record that happens to carry NIL there, which is why
this pairing is named here rather than left to be rediscovered from a
production failure.

PATH is the position reached so far, for the entries of ISSUES and
UNKNOWN-KEYS.  MAX-CHARS bounds every value this projects, leaf or opaque, the
same way EXTERNALIZE-VALUE's own :MAX-CHARS does."
  (let ((issues '())
        (unknown '()))
    (labels
        ((walk (value descriptor path depth)
           (let ((descriptor (%resolve-descriptor descriptor)))
             (cond
               ((eq :leaf descriptor)
                (project-value value :max-chars max-chars))
               ((eq :opaque descriptor)
                ;; Marker-first: :OPAQUE is WALK's route for a value from the
                ;; code under test, exactly where cl-spec's own
                ;; could-not-freeze-this marker appears, and this branch used
                ;; to hand it straight to EXTERNALIZE-VALUE -- which reached
                ;; every :OPAQUE field in *RECORD-SHAPES* (capture and
                ;; counterexample values, an error datum's :ACTUAL/:KEY, an
                ;; observation's :ARGUMENTS/:VALUE, and more), each one a
                ;; place an object id could be registered for the marker
                ;; itself instead of the value cl-spec declined to keep.
                (or (%opaque-marker-node value)
                    (list :value (externalize-value value :max-chars max-chars))))
               ;; >= rather than >: DEPTH counts containers already opened on
               ;; the way here, so the container that would be the (n+1)th is
               ;; the one cut, not one further past it.
               ((>= depth *projection-max-depth*)
                (push (list :path (reverse path) :reason :depth-limit) issues)
                (list :value (externalize-value value :max-chars max-chars)))
               ((eq :word-list descriptor)
                (list :array (walk-list value :leaf path depth)))
               ((eq :signature descriptor)
                ;; FAILURE-SIGNATURE builds (:RETURN-VALUE :RETURN-SPEC shapes)
                ;; and (:CONDITION-SPEC type shapes); every other form is a flat
                ;; run of tags.  Only the trailing shapes of those two are
                ;; structured, and the leading keyword stays a tag, never a key.
                (let ((shaped (and (consp value)
                                   (member (first value)
                                           '(:return-value :condition-spec))
                                   (= 3 (length value)))))
                  (list :array
                        (if shaped
                            (list (project-value (first value))
                                  (project-value (second value))
                                  (walk (third value)
                                        '(:array (:ref :error-datum))
                                        (cons 2 path) (1+ depth)))
                            (walk-list value :leaf path depth)))))
               ((not (consp descriptor))
                (project-value value :max-chars max-chars))
               ;; A container descriptor paired with a non-NIL atom cannot be
               ;; decomposed.  cl-spec's own :NOT-COLLECTED sentinel reaching
               ;; an :OBSERVATION's :OUTCOME this way is exactly this case --
               ;; kept out of +SENTINEL-FIELDS+ on purpose (see its
               ;; docstring): the target really was never called, which is an
               ;; answer, not an absence to blank to JSON null.  Projecting
               ;; the atom itself preserves that fact instead of crashing on
               ;; CAR, CDR or NTHCDR of something that was never a list --
               ;; the same guard every container shape needs, not only
               ;; :OBJECT's.
               ((and value (not (consp value))
                     (member (first descriptor) '(:array :alist :pairs :object)))
                (project-value value :max-chars max-chars))
               ((eq :array (first descriptor))
                (list :array (walk-list value (second descriptor) path depth)))
               ((eq :alist (first descriptor))
                (list :array (walk-alist value (second descriptor) path depth)))
               ((eq :pairs (first descriptor))
                (list :array (walk-pairs value (second descriptor) path depth)))
               ((eq :object (first descriptor))
                (walk-object value (rest descriptor) path depth))
               (t (project-value value :max-chars max-chars)))))
         (bounded (items path &optional (unit 1))
           ;; Cut here rather than in each caller, so the entry that records
           ;; the cut cannot be forgotten in one of them.  Never call LENGTH:
           ;; ITEMS came from outside this module and may be circular, and
           ;; LENGTH does not return on one.  UNIT is 2 for WALK-OBJECT's flat
           ;; plist, so the cut always falls on a pair boundary instead of
           ;; splitting one and fabricating a value for the key it orphans.
           (let* ((limit *projection-max-length*)
                  (cut-at (* limit unit))
                  (tail (nthcdr cut-at items)))
             (if (null tail)
                 items
                 (multiple-value-bind (dropped exactp)
                     (%tail-unit-count tail unit (1+ limit))
                   (push (list :path (reverse path) :reason :length-limit
                               :omitted-items dropped
                               :omitted-items-exact-p exactp)
                         issues)
                   (subseq items 0 cut-at)))))
         (walk-list (items descriptor path depth)
           (loop for item in (bounded items path)
                 for index from 0
                 collect (walk item descriptor (cons index path) (1+ depth))))
         (walk-alist (entries descriptor path depth)
           (loop for entry in (bounded entries path)
                 for index from 0
                 collect
                 (list :object
                       (list (cons "name" (project-value (car entry)))
                             (cons "value"
                                   (walk (cdr entry) descriptor
                                         (cons index path) (1+ depth)))))))
         (walk-pairs (plist descriptor path depth)
           ;; PLIST is a flat (NAME VALUE NAME VALUE ...) run -- cl-spec's
           ;; own NAME-ARGUMENTS shape for a counterexample, not an alist of
           ;; dotted pairs.  UNIT 2 for the same reason WALK-OBJECT's BOUNDED
           ;; call is: the cut must fall on a pair boundary, not split one
           ;; and orphan its value.  The {name, value} object built here is
           ;; identical to WALK-ALIST's, so a client cannot tell which Lisp
           ;; shape supplied it.
           (loop for (name value) on (bounded plist path 2) by #'cddr
                 for index from 0
                 collect
                 (list :object
                       (list (cons "name" (project-value name))
                             (cons "value"
                                   (walk value descriptor
                                         (cons index path) (1+ depth)))))))
         (walk-object (plist fields path depth)
           (let ((entries '()))
             (loop for (key raw) on (bounded plist path 2) by #'cddr
                   for field = (assoc key fields)
                   do (cond
                        ;; A +SENTINEL-FIELDS+ key whose raw value is the bare
                        ;; :NOT-COLLECTED keyword carries no substructure to
                        ;; walk -- FIELD-AVAILABILITY already reports this as
                        ;; availability, not data, and walking the literal
                        ;; keyword under this key's container descriptor would
                        ;; either crash (BOUNDED expects a list) or publish a
                        ;; scalar word where every other record publishes an
                        ;; object.  Project it as JSON null instead.
                        ((and (member key +sentinel-fields+)
                              (eq :not-collected raw))
                         (push (cons (%json-key key) (list :scalar nil))
                               entries))
                        ;; :ELSEWHERE is declared, not unknown, and not
                        ;; projected here: cl-mcp names it deliberately
                        ;; because it already publishes this key outside
                        ;; :DATA (see PROJECT-RECORD's docstring), so it must
                        ;; not fall through to the UNKNOWN-KEYS branch below.
                        ((and field (eq :elsewhere (cdr field)))
                         nil)
                        (field
                         (push (cons (%json-key key)
                                     (walk raw (cdr field)
                                           (cons (%json-key key) path)
                                           (1+ depth)))
                               entries))
                        (t
                         ;; Named, not interpreted.  A future key's meaning is
                         ;; cl-spec's to define, and publishing a guess at it
                         ;; is the one thing this module must not do.
                         (push (%dotted-path
                                (reverse (cons (%json-key key) path)))
                               unknown))))
             (list :object (nreverse entries)))))
      (let ((node (walk value descriptor (reverse path) 0)))
        (values node (nreverse issues) (nreverse unknown))))))

(defparameter +v1-required-metadata+
  '(:schema-version :record-kind :entity-kind :definition-digest
    :definition-digest-complete :definition-digest-covers :capabilities)
  "The metadata keys cl-spec's SCHEMA-INFO declares required for version 1.

A record claiming version 1 without one of them is broken, not old.  Reporting
it as a field that happens to be absent would let a malformed answer from the
versioned API read as an older revision -- which is the one confusion this
module's availability states exist to prevent.")

(defparameter *plist-scan-limit* 4096
  "How far a plist is walked before it is refused as malformed.

A bound rather than a proper-list test, so a circular or improper answer from a
future revision is refused instead of hanging the worker.")

(defun %proper-plist-p (value)
  "Return true when VALUE is a bounded plist with keyword indicators."
  (loop with tail = value
        for count from 0 below *plist-scan-limit*
        do (cond ((null tail) (return t))
                 ((not (consp tail)) (return nil))
                 ((not (keywordp (car tail))) (return nil))
                 ((not (consp (cdr tail))) (return nil))
                 (t (setf tail (cddr tail))))
        finally (return nil)))

(defun field-availability (record key)
  "Return :COLLECTED, :NOT-COLLECTED or :ABSENT for KEY in RECORD.

GET-PROPERTIES rather than GETF, and that is the whole point: GETF answers NIL
both for a key that is not there and for a key whose value is NIL, and
:FAILURE-PHASE NIL is a measurement -- an ordinary target observation with no
special phase -- not an absence.

:NOT-COLLECTED counts as availability only for +SENTINEL-FIELDS+; anywhere else
it is the value cl-spec meant to give."
  (multiple-value-bind (indicator value tail) (get-properties record (list key))
    (declare (ignore indicator))
    (cond ((null tail) :absent)
          ((and (eq :not-collected value) (member key +sentinel-fields+))
           :not-collected)
          (t :collected))))

(defun validate-versioned-record (record &key expected-record-kind
                                              expected-entity-kind)
  "Return (values STATUS REASON) for one versioned record.

STATUS is :OK, :UNSUPPORTED-SCHEMA when the record declares a version this
adapter does not know, or :MALFORMED.  REASON is the declared version for
:UNSUPPORTED-SCHEMA and a sentence for :MALFORMED.

A malformed answer from a versioned API is reported rather than quietly
replaced by an older reader: falling back would hide a signature mismatch
behind a response that looked fine.  The same judgement %DESCRIBE-FUNCTION-SPEC
already makes when FUNCTION-SPEC-DATA answers NIL.

EXPECTED-RECORD-KIND and EXPECTED-ENTITY-KIND are checked when supplied --
RESULT-DATA answers :RESULT and FUNCTION-SPEC-DATA answers :DEFINITION with
:FUNCTION-SPEC -- so a record projected under the wrong reader is caught here
rather than by whatever reads it next."
  (flet ((bad (reason) (return-from validate-versioned-record
                         (values :malformed reason))))
    (unless record (bad "the versioned reader returned NIL"))
    (unless (%proper-plist-p record)
      (bad "the versioned reader returned something that is not a plist"))
    (when (eq :absent (field-availability record :schema-version))
      (bad "the record carries no :schema-version"))
    (let ((version (getf record :schema-version)))
      (unless (eql 1 version)
        (return-from validate-versioned-record
          (values :unsupported-schema version)))
      (dolist (key +v1-required-metadata+)
        (when (eq :absent (field-availability record key))
          (bad (format nil "a version 1 record is missing required metadata ~A"
                       (%json-key key)))))
      (when (and expected-record-kind
                 (not (eq expected-record-kind (getf record :record-kind))))
        (bad (format nil "expected a ~A record and got ~A"
                     (%json-key expected-record-kind)
                     (%json-key (getf record :record-kind)))))
      (when (and expected-entity-kind
                 (not (eq expected-entity-kind (getf record :entity-kind))))
        (bad (format nil "expected a ~A record and got ~A"
                     (%json-key expected-entity-kind)
                     (%json-key (getf record :entity-kind)))))
      :ok)))

(setf *record-shapes*
      (list
       ;; An EXPECTED descriptor is what cl-spec says the spec required; it is
       ;; spec-derived and stays structured.  :TYPE holds a type specifier,
       ;; which is a form and reaches EXTERNALIZE-VALUE through :LEAF.
       :expected
       '(:object (:kind . :leaf) (:type . :leaf) (:satisfies . :leaf)
                 (:closed . :leaf) (:test . :leaf) (:class . :leaf)
                 (:tag-reader . :leaf)
                 (:fields . (:array (:ref :field-expectation)))
                 (:branches . (:array (:object (:name . :leaf)
                                               (:expected . (:ref :expected))))))
       :field-expectation
       ;; :KEY is a key name out of the value under test, so it is value-derived
       ;; -- cl-spec's own *FAILURE-SHAPE-KEYS* docstring classifies it that way.
       '(:object (:key . :opaque) (:required . :leaf)
                 (:expected . (:ref :expected)))
       ;; One EXPLAIN-DATA error datum.  :ACTUAL and :ACTUAL-LENGTH come off the
       ;; value, :EXPECTED and the bounds off the spec, and the three container
       ;; keys hold more error datums -- which is cl-spec's own classification
       ;; in *FAILURE-SHAPE-KEYS* and *FAILURE-SHAPE-CONTAINERS*.
       :error-datum
       '(:object (:kind . :leaf) (:path . (:array :leaf))
                 (:tuple-path . (:array :leaf)) (:field-path . (:array :leaf))
                 (:actual . :opaque) (:actual-test . :opaque)
                 (:key . :opaque) (:expected . (:ref :expected))
                 (:violated-bound . :leaf) (:predicate . :leaf)
                 (:condition-type . :leaf) (:condition-report . :leaf)
                 (:expected-length . :leaf) (:minimum-length . :leaf)
                 (:maximum-length . :leaf) (:actual-length . :leaf)
                 (:status . :leaf) (:branch . :leaf)
                 (:branch-path . (:array :leaf)) (:known-tags . :word-list)
                 (:errors . (:array (:ref :error-datum)))
                 (:branches . (:array (:ref :error-datum)))
                 (:conjuncts . (:array (:ref :error-datum))))
       ;; The union of every explanation cl-spec builds: an EXPLAIN-DATA root
       ;; for a spec violation, and the :KIND plists a case-selection error, a
       ;; capture error and a state-post violation record.
       :explanation
       '(:object (:valid . :leaf) (:spec . :leaf) (:value . :opaque)
                 (:path . (:array :leaf))
                 (:errors . (:array (:ref :error-datum)))
                 (:kind . :leaf) (:case-error . :leaf) (:function . :leaf)
                 (:cases . :word-list) (:case . :leaf) (:index . :leaf)
                 (:form . :leaf) (:binding . :leaf) (:captured . (:alist :opaque))
                 (:condition-type . :leaf) (:condition-report . :leaf))
       :target-outcome
       '(:object (:kind . :leaf) (:values . (:array :opaque))
                 (:condition-type . :leaf) (:condition-report . :leaf))
       :capture-evidence
       '(:object (:status . :leaf) (:declared . (:array :leaf))
                 ;; Measured as ((NAME . VALUE) ...); dotted pairs are not
                 ;; proper lists, so they get their own descriptor rather than
                 ;; an array rule that has nothing to say about them.
                 (:values . (:alist :opaque))
                 (:error . (:object (:binding . :leaf) (:index . :leaf)
                                    (:condition-type . :leaf))))
       :state
       '(:object (:capture . (:ref :capture-evidence))
                 (:state-post . (:object (:status . :leaf) (:reason . :leaf)
                                         (:case . :leaf) (:index . :leaf)
                                         (:form . :leaf)
                                         (:condition-type . :leaf))))
       :observation
       '(:object (:arguments . (:array :opaque)) (:status . :leaf)
                 (:reason . :leaf) (:signature . :signature)
                 (:explanation . (:ref :explanation))
                 (:outcome . (:ref :target-outcome)) (:value . :opaque)
                 (:case . :leaf) (:condition-report . :leaf)
                 (:state . (:ref :state)))
       :case-report
       '(:object (:selection . :leaf) (:unit . :leaf)
                 (:declared-cases . :word-list)
                 (:cases . (:array (:object (:name . :leaf)
                                            (:documentation . :leaf)
                                            (:called . :leaf) (:passed . :leaf)
                                            (:failed . :leaf) (:error . :leaf))))
                 (:case-selection-errors . :leaf) (:capture-errors . :leaf)
                 (:never-called . :word-list))
       :generation-report
       '(:object (:scope . :leaf) (:unit . :leaf) (:policy . :leaf)
                 (:budget . :leaf) (:budget-source . :leaf)
                 (:default-coefficient . :leaf) (:requested-values . :leaf)
                 (:generated-values . :leaf) (:attempts . :leaf)
                 (:rejections . :leaf)
                 (:phases . (:object
                             (:generation . (:object (:attempts . :leaf)
                                                     (:rejections . :leaf)))
                             (:shrinking . (:object (:attempts . :leaf)
                                                    (:rejections . :leaf)))))
                 (:termination . :leaf) (:exhaustion-phase . :leaf)
                 (:exhausted-at . :leaf))
       :shrink-report
       '(:object (:candidates . :leaf) (:budget . :leaf) (:termination . :leaf))
       :provenance
       '(:object (:backend . :leaf) (:lisp-implementation-type . :leaf)
                 (:lisp-implementation-version . :leaf)
                 (:cl-spec-version . :leaf) (:target-revision . :leaf)
                 (:collection-states
                  . (:object (:backend . :leaf)
                             (:lisp-implementation-type . :leaf)
                             (:lisp-implementation-version . :leaf)
                             (:cl-spec-version . :leaf)
                             (:target-revision . :leaf))))
       :capabilities
       '(:object (:generation . :leaf) (:shrinking . :leaf)
                 (:instrumentation . :leaf))
       :digest-omission
       '(:object (:kind . :leaf) (:path . (:array :leaf)) (:target . :leaf)
                 (:reason . :leaf))
       :counterexample
       ;; cl-spec's NAME-ARGUMENTS (PROPERTY-NAMED-ARGUMENTS) zips each
       ;; argument variable with its value as a flat {variable value} PLIST
       ;; -- (BALANCE 5 AMOUNT 5), confirmed against
       ;; cl-spec/tests/rest-function-test.lisp's own
       ;; (equal '(head 1 tail (2 3)) (property-named-arguments ...)) -- not
       ;; an alist of dotted pairs.  :PAIRS reads it two at a time; :ALIST
       ;; would call CAR/CDR on a bare argument-value symbol and signal a
       ;; TYPE-ERROR on the first one, which a passing run's NIL
       ;; counterexample never exercises.
       '(:pairs :opaque)
       :result-data
       '(:object (:schema-version . :leaf) (:record-kind . :leaf)
                 (:entity-kind . :leaf) (:definition-digest . :leaf)
                 (:definition-digest-complete . :leaf)
                 (:definition-digest-covers . :leaf)
                 (:digest-omissions . (:array (:ref :digest-omission)))
                 (:digest-exclusions . :word-list)
                 (:capabilities . (:ref :capabilities))
                 (:state-constraints . :leaf)
                 (:name . :leaf) (:status . :leaf) (:trials . :leaf)
                 (:budget . :leaf) (:rejected . :leaf) (:seed . :leaf)
                 (:profile . :leaf)
                 ;; cl-spec v1 does not publish the shape of caller options.
                 (:options . :opaque)
                 (:provenance . (:ref :provenance))
                 (:counterexample . (:ref :counterexample))
                 (:shrunk-counterexample . (:ref :counterexample))
                 (:shrunk-outcome . :leaf)
                 (:shrink-report . (:ref :shrink-report))
                 (:generation-report . (:ref :generation-report))
                 (:failure-phase . :leaf) (:failure-reason . :leaf)
                 (:case-report . (:ref :case-report))
                 (:failure . (:ref :observation))
                 (:shrunk-failure . (:ref :observation))
                 (:elapsed . :leaf))
       :function-spec-data
       '(:object (:schema-version . :leaf) (:record-kind . :leaf)
                 (:entity-kind . :leaf) (:definition-digest . :leaf)
                 (:definition-digest-complete . :leaf)
                 (:definition-digest-covers . :leaf)
                 (:digest-omissions . (:array (:ref :digest-omission)))
                 (:digest-exclusions . :word-list)
                 (:capabilities . (:ref :capabilities))
                 (:state-constraints . :leaf)
                 (:name . :leaf) (:kind . :leaf) (:documentation . :leaf)
                 (:argument-generator . :leaf)
                 (:preconditions . (:array :leaf))
                 (:postconditions . (:array :leaf))
                 (:post-value-variables . (:array :leaf))
                 (:capture . (:array (:object (:name . :leaf) (:form . :leaf))))
                 (:state-post . (:array :leaf))
                 (:case-selection . :leaf)
                 (:source-form . :leaf)
                 (:metadata . :opaque)
                 ;; Declared by the record and published at the top level of the
                 ;; describe response through %SPEC-TREE, which projects an IR
                 ;; node.  Named here so they are not reported as keys this
                 ;; adapter does not understand -- it understands all six and
                 ;; renders them -- and so a reader of this shape can see that
                 ;; the omission is deliberate rather than an oversight.
                 (:arguments . :elsewhere)
                 (:argument-schema . :elsewhere)
                 (:returns . :elsewhere)
                 (:signals . :elsewhere)
                 (:cases . :elsewhere)
                 (:source-location . :elsewhere))))

(defun project-core-record (record shape-name &key expected-record-kind
                                                   expected-entity-kind)
  "Return (values REPORT STATUS REASON) for one versioned cl-spec RECORD.

STATUS is :OK, :UNSUPPORTED-SCHEMA or :MALFORMED.  A malformed record yields no
REPORT: the caller reports it as an adapter-visible fault rather than falling
back to a legacy reader, which would hide it.

REPORT separates what cl-mcp knows about the transport from what cl-spec said.
:DATA is the record and carries no key cl-mcp added; :AVAILABILITY,
:SCHEMA-SUPPORTED, :FIELD-AVAILABILITY, :UNKNOWN-KEYS and :PROJECTION are the
transport metadata, and they live outside it.  A truncated projection is
visible there rather than silently shorter inside :DATA."
  (multiple-value-bind (status reason)
      (validate-versioned-record record
                                 :expected-record-kind expected-record-kind
                                 :expected-entity-kind expected-entity-kind)
    (case status
      (:malformed (values nil :malformed reason))
      (:unsupported-schema
       (values (list :availability :collected
                     :schema-supported nil
                     :schema-version reason
                     :field-availability nil
                     :unknown-keys nil
                     :source nil
                     :projection (list :complete t :issues nil)
                     :data nil)
               :unsupported-schema
               reason))
      (t
       (multiple-value-bind (node issues unknown)
           (project-record record (list :ref shape-name))
         (values (list :availability :collected
                       :schema-supported t
                       :schema-version 1
                       ;; The record as cl-spec gave it, kept for the adapter's
                       ;; own reading.  The verdict logic asks questions like
                       ;; "was any declared case never reached", and answering
                       ;; them off :DATA would mean re-parsing projected JSON
                       ;; nodes to recover keywords this already has.  Never
                       ;; rendered: :DATA is what reaches the client.
                       :source record
                       :field-availability
                       (loop for (key . nil) in (rest (%resolve-descriptor
                                                       (list :ref shape-name)))
                             append (list key (field-availability record key)))
                       :unknown-keys unknown
                       :projection (list :complete (null issues) :issues issues)
                       :data node)
                 :ok
                 nil))))))
