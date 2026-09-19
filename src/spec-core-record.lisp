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
offers an object id instead of pretending the text is the object."
  (cond ((keywordp value) (list :scalar (string-downcase (symbol-name value))))
        ((null value) (list :scalar nil))
        ((symbolp value) (list :symbol (symbol-data value)))
        ((stringp value) (list :scalar value))
        ((safe-json-integer-p value) (list :scalar value))
        ((integerp value) (list :scalar (format nil "~D" value)))
        (t (list :value (externalize-value value :max-chars max-chars)))))

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
deliberately not guessed.

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
                (list :value (externalize-value value :max-chars max-chars)))
               ;; >= rather than >: DEPTH counts containers already opened on
               ;; the way here, so the container that would be the (n+1)th is
               ;; the one cut, not one further past it.
               ((>= depth *projection-max-depth*)
                (push (list :path (reverse path) :reason :depth-limit) issues)
                (list :value (externalize-value value :max-chars max-chars)))
               ((eq :word-list descriptor)
                (list :array (walk-list value :leaf path depth)))
               ((not (consp descriptor))
                (project-value value :max-chars max-chars))
               ((eq :array (first descriptor))
                (list :array (walk-list value (second descriptor) path depth)))
               ((eq :alist (first descriptor))
                (list :array (walk-alist value (second descriptor) path depth)))
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
         (walk-object (plist fields path depth)
           (let ((entries '()))
             (loop for (key raw) on (bounded plist path 2) by #'cddr
                   for field = (assoc key fields)
                   do (if field
                          (push (cons (%json-key key)
                                      (walk raw (cdr field)
                                            (cons (%json-key key) path)
                                            (1+ depth)))
                                entries)
                          ;; Named, not interpreted.  A future key's meaning is
                          ;; cl-spec's to define, and publishing a guess at it
                          ;; is the one thing this module must not do.
                          (push (%dotted-path
                                 (reverse (cons (%json-key key) path)))
                                unknown)))
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

(defparameter +sentinel-fields+
  '(:shrink-report :generation-report :case-report
    :digest-omissions :digest-exclusions)
  "The fields whose :NOT-COLLECTED value means availability rather than data.

:NOT-COLLECTED is not a sentinel wherever it appears.  On an observation's
:OUTCOME it means the Function Spec target was never called, and inside
provenance's :COLLECTION-STATES it names an item nobody collected -- both are
the answer, not the absence of one.  Converting either would delete a fact.")

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
