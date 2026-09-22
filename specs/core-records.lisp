;;;; specs/core-records.lisp
;;;;
;;;; Properties of the record layer: how cl-mcp carries one cl-spec versioned
;;;; result record without changing what it says.  The functions checked are
;;;; the public ones of cl-mcp/src/spec-core-record:
;;;;
;;;;   FIELD-AVAILABILITY        a missing key, a present NIL and a sentinel
;;;;   VALIDATE-VERSIONED-RECORD ok, unsupported schema and malformed apart
;;;;   PROJECT-RECORD            each field projected by its role
;;;;   PROJECT-CORE-RECORD       the report: data beside transport metadata
;;;;
;;;; Every input is a small record built fresh from a descriptor
;;;; (specs/core-record-fixtures.lisp), and every expectation comes from
;;;; cl-spec's public record contract restated there -- never from this
;;;; module's *RECORD-SHAPES*, +SENTINEL-FIELDS+ or +V1-REQUIRED-METADATA+.
;;;; Each property calls the functions directly; none runs cl-spec, the bundle
;;;; or spec-check.  Every projection happens inside
;;;; WITH-ISOLATED-OBJECT-REGISTRY, so opaque values are registered in a
;;;; registry of the check's own.
;;;;
;;;; Verified domain: proper, finite v1 result records of the 31 keys
;;;; RESULT-DATA documents, varied in the fields each property names; seeds
;;;; from 0 past cl-spec's own 2^62 draw bound; one limit at a time pushed just
;;;; below, to and just past its bound; single-cause malformations.  Not
;;;; covered: selection, the verified tally, verification gaps, legacy
;;;; fallback, other cl-spec versions, Function Spec definition records,
;;;; circular metadata (kept to the fixed termination tests), and JSON-RPC.
;;;; Records from a real cl-spec, taken through the renderer to JSON, are the
;;;; opt-in fixed tests of tests/core-record-specs-test.lisp.

(defpackage #:cl-mcp/specs/core-records
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/spec-core-record
                #:field-availability
                #:validate-versioned-record
                #:project-record
                #:project-core-record
                #:*projection-max-depth*
                #:*projection-max-length*)
  (:import-from #:cl-mcp/src/object-registry
                #:lookup-object)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:with-isolated-object-registry
                #:decimal-string
                #:make-result-record
                #:record-without
                #:record-with
                #:object-field
                #:node-at
                #:object-keys
                #:draw-availability-case
                #:draw-role-case
                #:draw-seed-case
                #:draw-key-relation-case
                #:draw-cut-case
                #:draw-validation-case
                #:role-case-record
                #:record-capture-value
                #:numbered-items
                #:numbered-text
                #:permute-record
                #:error-chain-record
                #:chain-container-path
                #:malformed-variant
                #:unsupported-variant)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/core-records)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(core-record-availability-separates-absence-from-nil
    core-record-projects-each-field-by-its-role
    core-record-seeds-stay-decimal-text
    core-record-ignores-order-duplicates-and-unknown-keys
    core-record-reports-every-cut
    core-record-validation-separates-ok-unsupported-malformed))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(availability-case role-case seed-case key-relation-case cut-case validation-case))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(availability-case-generator role-case-generator seed-case-generator
    key-relation-case-generator cut-case-generator validation-case-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none, as it has no
contract.  Its fixed cases are Rove tests (tests/spec-core-record-test.lisp and,
with a real cl-spec, tests/core-record-specs-test.lisp)."
  '())

(defun %json-key (key)
  "Return KEY as the snake_case JSON key cl-mcp publishes it under -- the
convention every response follows, restated rather than borrowed."
  (substitute #\_ #\- (string-downcase (symbol-name key))))

(defun %core (record &rest options)
  "Project RECORD as a result record in a registry of its own; return
\(values REPORT STATUS REASON)."
  (with-isolated-object-registry
    (apply #'project-core-record record :result-data :expected-record-kind :result
           options)))

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator availability-case-generator ()
    "Draw a sentinel field, a value field and a position (DRAW-AVAILABILITY-CASE)."
    (draw-availability-case))
  (defspec availability-case list (:generator availability-case-generator))
  (defgenerator role-case-generator ()
    "Draw how each role-bearing field is set (DRAW-ROLE-CASE)."
    (draw-role-case))
  (defspec role-case list (:generator role-case-generator))
  (defgenerator seed-case-generator ()
    "Draw five seeds across the JSON and generator boundaries (DRAW-SEED-CASE)."
    (draw-seed-case))
  (defspec seed-case list (:generator seed-case-generator))
  (defgenerator key-relation-case-generator ()
    "Draw a base record, a field to duplicate and unknown keys (DRAW-KEY-RELATION-CASE)."
    (draw-key-relation-case))
  (defspec key-relation-case list (:generator key-relation-case-generator))
  (defgenerator cut-case-generator ()
    "Draw the one limit to push and its bound (DRAW-CUT-CASE)."
    (draw-cut-case))
  (defspec cut-case list (:generator cut-case-generator))
  (defgenerator validation-case-generator ()
    "Draw one malformation and one unsupported version (DRAW-VALIDATION-CASE)."
    (draw-validation-case))
  (defspec validation-case list (:generator validation-case-generator))

  (defproperty core-record-availability-separates-absence-from-nil
      ((case availability-case))
    "FIELD-AVAILABILITY keeps three answers apart, in every trial and at the
front or the back of a record: a missing key is :ABSENT; a key present with
NIL is :COLLECTED; the bare :NOT-COLLECTED is :NOT-COLLECTED in a field cl-spec
documents as a sentinel and :COLLECTED in an ordinary value field, where it is
the value.  A present, non-NIL value is :COLLECTED.  Every trial checks all of
these, so no answer can be missed by the draw."
    (:about field-availability)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key sentinel-field value-field position seed) case
      (let ((base (make-result-record :seed seed)))
        (and
         ;; A missing key.
         (eq :absent (field-availability (record-without base sentinel-field) sentinel-field))
         (eq :absent (field-availability (record-without base value-field) value-field))
         ;; A present NIL is a value, not an absence.
         (eq :collected (field-availability (record-with base sentinel-field nil
                                                         :position position)
                                            sentinel-field))
         (eq :collected (field-availability (record-with base value-field nil
                                                         :position position)
                                            value-field))
         ;; :NOT-COLLECTED is availability in a sentinel field only.
         (eq :not-collected (field-availability (record-with base sentinel-field
                                                             :not-collected
                                                             :position position)
                                                sentinel-field))
         (eq :collected (field-availability (record-with base value-field :not-collected
                                                         :position position)
                                            value-field))
         ;; An ordinary value.
         (eq :collected (field-availability (record-with base sentinel-field (list :x 1)
                                                         :position position)
                                            sentinel-field))))))

  (defproperty core-record-projects-each-field-by-its-role
      ((case role-case))
    "PROJECT-CORE-RECORD projects each field by what cl-spec's record says it
is, not by its Lisp value: a boolean NIL is (:BOOL NIL), JSON false; an absent
observation is (:SCALAR NIL), JSON null; an empty collection is (:ARRAY ()),
JSON []; a key missing from the record is missing from :DATA while a present
NIL phase is there as null.  A collected capture value is application data --
an externalized (:VALUE ...) node -- whatever it looks like, even a list shaped
like cl-spec's own unavailable marker, and the capture record invents no
:REASON or :TYPE for it.  The value itself survives, not only its tag: its
printed text is what the standard printer writes for it, and a list carries an
object id that, in the same registry, names the record's own list, while an
atom carries none."
    (:about project-core-record project-record)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (multiple-value-bind (record capture-value) (role-case-record case)
      ;; One registry scope for the projection and the lookup below, so an
      ;; object id can be followed to the object it names.
      (with-isolated-object-registry
        (let* ((data (getf (project-core-record record :result-data
                                                :expected-record-kind :result)
                           :data))
               (capture (node-at data '("failure" "state" "capture" "values" 0)))
               (value (second (object-field capture "value")))
               (stored (record-capture-value record))
               (id (getf value :object-id)))
          (and
           ;; A boolean.
           (equal (list :bool (getf case :definition-digest-complete))
                  (object-field data "definition_digest_complete"))
           ;; An optional object: null when absent, an object when present.
           (ecase (getf case :shrunk-failure)
             (:none (equal '(:scalar nil) (object-field data "shrunk_failure")))
             (:present (eq :object (first (object-field data "shrunk_failure")))))
           ;; Collections: [] when empty, never null.
           (eq :array (first (object-field data "counterexample")))
           (eq (eq :empty (getf case :counterexample))
               (null (second (object-field data "counterexample"))))
           (eq :array (first (object-field data "digest_exclusions")))
           (eq (eq :empty (getf case :digest-exclusions))
               (null (second (object-field data "digest_exclusions"))))
           ;; A missing key against a present NIL.
           (ecase (getf case :failure-phase)
             (:absent (not (nth-value 1 (object-field data "failure_phase"))))
             (:present-nil (multiple-value-bind (child present-p)
                               (object-field data "failure_phase")
                             (and present-p (equal '(:scalar nil) child)))))
           ;; A collected capture value is application data.
           (equal '(:scalar "collected") (object-field capture "availability"))
           (eq :value (first (object-field capture "value")))
           (not (nth-value 1 (object-field capture "reason")))
           (not (nth-value 1 (object-field capture "type")))
           ;; And it is the value: the standard printer's text for it, and for
           ;; a list, an id naming the record's own list.
           (equal (prin1-to-string capture-value) (getf value :printed))
           (if (consp stored)
               (and id (eq stored (lookup-object id)))
               (null id)))))))

  (defproperty core-record-seeds-stay-decimal-text
      ((case seed-case))
    "A result record's seed reaches :DATA as its decimal text, never as a JSON
number, for every seed cl-spec can report: small ones, the binary64 edge
around 2^53, just under cl-spec's own 2^62 draw bound, anywhere below it, and
past it, since cl-spec accepts any non-negative integer.  JSON's grammar
allows these as numbers; a consumer reading them into doubles would round
them, and a rounded seed reproduces a different run.  The expected text is
computed by integer division, not by the projector.  Beside it, a small trial
count stays a number: the rule is the seed's, not every integer's."
    (:about project-core-record project-record)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key seeds trials) case
      (every (lambda (seed)
               (let ((expected (decimal-string seed))
                     (data (getf (%core (make-result-record :seed seed :trials trials))
                                 :data)))
                 (and (equal (list :scalar expected) (object-field data "seed"))
                      (equal (list :scalar trials) (object-field data "trials")))))
             seeds)))

  (defproperty core-record-ignores-order-duplicates-and-unknown-keys
      ((case key-relation-case))
    "Three changes to a valid record leave every known field's projection and
availability as they were.  Putting the pairs in another order changes no key
and no value.  A later duplicate of a known key with a different value is
ignored: the first occurrence is the record's answer.  Keys no version-1 record
declares are named in :UNKNOWN-KEYS, are not guessed into :DATA, and neither
make the projection incomplete nor unsettle the schema.  Keys are compared as
sets; the values keep their own order."
    (:about project-core-record project-record field-availability)
    (:kind :invariance)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key seed status duplicate-index unknown-count) case
      (let* ((base (make-result-record :seed seed :status status))
             (expected (%core base))
             (expected-data (getf expected :data))
             (duplicate (nth duplicate-index
                             '((:status . :failed) (:trials . 99) (:profile . :smoke)
                               (:seed . 123) (:shrink-report . :not-collected)
                               (:failure-reason . :unexpected))))
             (unknown (subseq '(:x-future-alpha :x-future-beta :x-future-gamma)
                              0 unknown-count)))
        (flet ((same-known-fields-p (report)
                 (let ((data (getf report :data)))
                   (and (null (set-exclusive-or (object-keys expected-data)
                                                (object-keys data) :test #'equal))
                        (every (lambda (key)
                                 (equal (object-field expected-data key)
                                        (object-field data key)))
                               (object-keys expected-data))
                        (equal (getf expected :field-availability)
                               (getf report :field-availability))))))
          (let ((permuted (%core (permute-record base)))
                (duplicated (%core (append (copy-list base)
                                           (list (car duplicate) (cdr duplicate)))))
                (extended (%core (append (copy-list base)
                                         (loop for key in unknown append (list key 1))))))
            (and (same-known-fields-p permuted)
                 (null (getf permuted :unknown-keys))
                 (same-known-fields-p duplicated)
                 (null (getf duplicated :unknown-keys))
                 (same-known-fields-p extended)
                 (null (set-exclusive-or (getf extended :unknown-keys)
                                         (mapcar #'%json-key unknown) :test #'equal))
                 (notany (lambda (key) (nth-value 1 (object-field (getf extended :data)
                                                                  (%json-key key))))
                         unknown)
                 (getf (getf extended :projection) :complete)
                 (getf extended :schema-supported)))))))

  (defproperty core-record-reports-every-cut
      ((case cut-case))
    "Every trial pushes all three limits, one record at a time -- list length,
string length and nesting depth -- to just under, at, just past and far past
its bound.  Just under and at it, the record is projected whole: no issue,
PROJECTION.COMPLETE true, and every item or character kept, in order.  Past
it, the field is cut and the report says so: exactly one issue, at that
field's path, with the limit's reason, and PROJECTION.COMPLETE false.  What is
kept is the head, in order -- compared item by item and character by
character, and every item and character here differs from its neighbours, so
keeping the tail or reordering shows -- and a cut container is left as an
externalized value.  An omitted count is the true excess when it says it is
exact, and less than it when it says it is not.  Fields away from the cut stay
whole, and no issue is written into :DATA."
    (:about project-core-record project-record)
    (:kind :boundary)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key length-limit chars-limit depth-limit) case
      (flet ((issues (report) (getf (getf report :projection) :issues))
             (complete-p (report) (getf (getf report :projection) :complete))
             (clean-p (report)
               ;; The transport metadata never lands inside the record.
               (not (or (nth-value 1 (object-field (getf report :data) "issues"))
                        (nth-value 1 (object-field (getf report :data) "projection")))))
             (whole-status-p (report)
               (member (object-field (getf report :data) "status")
                       '((:scalar "passed") (:scalar "failed")) :test #'equal))
             (prefix-nodes (count)
               ;; A small integer projects as itself: (:SCALAR n).
               (loop for i below count collect (list :scalar i))))
        (and
         ;; Length: a list of distinct integers.
         (let ((*projection-max-length* length-limit))
           (every (lambda (count)
                    (let* ((report (%core (make-result-record
                                           :digest-exclusions (numbered-items count))))
                           (kept (second (object-field (getf report :data)
                                                       "digest_exclusions")))
                           (issue (first (issues report)))
                           (excess (- count length-limit)))
                      (and (clean-p report) (whole-status-p report)
                           (equal (prefix-nodes (min count length-limit)) kept)
                           (if (<= count length-limit)
                               (and (complete-p report) (null (issues report)))
                               (and (not (complete-p report))
                                    (= 1 (length (issues report)))
                                    (equal '("digest_exclusions") (getf issue :path))
                                    (eq :length-limit (getf issue :reason))
                                    (if (getf issue :omitted-items-exact-p)
                                        (= excess (getf issue :omitted-items))
                                        (< (getf issue :omitted-items) excess)))))))
                  (list (1- length-limit) length-limit (1+ length-limit)
                        (+ (* 2 length-limit) 5))))
         ;; Characters: a string that never repeats a stretch.
         (every (lambda (count)
                  (let* ((text (numbered-text count))
                         (report (%core (make-result-record
                                         :status :failed
                                         :failure (list :status :failed
                                                        :condition-report text))
                                        :max-chars chars-limit))
                         (node (node-at (getf report :data) '("failure" "condition_report")))
                         (issue (first (issues report))))
                    (and (clean-p report) (whole-status-p report)
                         (equal (list :scalar (subseq text 0 (min count chars-limit))) node)
                         (if (<= count chars-limit)
                             (and (complete-p report) (null (issues report)))
                             (and (not (complete-p report))
                                  (= 1 (length (issues report)))
                                  (equal '("failure" "condition_report") (getf issue :path))
                                  (eq :char-limit (getf issue :reason))
                                  (getf issue :omitted-items-exact-p)
                                  (= (- count chars-limit) (getf issue :omitted-items)))))))
                (list (1- chars-limit) chars-limit (1+ chars-limit) (* 3 chars-limit)))
         ;; Depth: a chain of error datums.
         (let ((*projection-max-depth* depth-limit))
           (every (lambda (deepest)
                    (let* ((report (%core (error-chain-record deepest)))
                           (issue (first (issues report)))
                           (cut-path (chain-container-path depth-limit)))
                      (and (clean-p report) (whole-status-p report)
                           (if (< deepest depth-limit)
                               (and (complete-p report) (null (issues report))
                                    (nth-value 1 (node-at (getf report :data)
                                                          (chain-container-path deepest))))
                               (and (not (complete-p report))
                                    (= 1 (length (issues report)))
                                    (eq :depth-limit (getf issue :reason))
                                    (equal cut-path (getf issue :path))
                                    (eq :value (first (node-at (getf report :data)
                                                               cut-path))))))))
                  (list (1- depth-limit) depth-limit (1+ depth-limit) (+ depth-limit 3))))))))

  (defproperty core-record-validation-separates-ok-unsupported-malformed
      ((case validation-case))
    "Every trial holds three records that differ from one valid v1 result
record in one cause each.  The valid one is :OK and projected, with
SCHEMA-SUPPORTED true.  The malformed one -- a required metadata key dropped,
NIL, an improper or odd-length plist, a non-keyword indicator, a record or
entity kind other than the expected one, or no schema version -- is :MALFORMED,
and PROJECT-CORE-RECORD returns no report for it.  The one declaring an
unsupported integer version is :UNSUPPORTED-SCHEMA with that version: read, but
not understood, so its report says collected, SCHEMA-SUPPORTED false and the
version, and carries no :DATA and no field availability rather than reading it
as version 1."
    (:about validate-versioned-record project-core-record)
    (:kind :boundary)
    (:trials (:smoke 5 :normal 25))
    (let ((valid (make-result-record))
          (version (getf case :version)))
      (multiple-value-bind (malformed record-kind entity-kind) (malformed-variant case)
        (and
         ;; The valid record.
         (eq :ok (validate-versioned-record valid :expected-record-kind :result
                                                  :expected-entity-kind :property))
         (multiple-value-bind (report status) (%core valid)
           (and (eq :ok status) (getf report :schema-supported)
                (eq :object (first (getf report :data)))))
         ;; The malformed one.
         (eq :malformed (validate-versioned-record malformed
                                                   :expected-record-kind record-kind
                                                   :expected-entity-kind entity-kind))
         (multiple-value-bind (report status)
             (with-isolated-object-registry
               (project-core-record malformed :result-data
                                    :expected-record-kind record-kind
                                    :expected-entity-kind entity-kind))
           (and (null report) (eq :malformed status)))
         ;; The unsupported one.
         (let ((unsupported (unsupported-variant case)))
           (and (multiple-value-bind (status reason) (validate-versioned-record unsupported)
                  (and (eq :unsupported-schema status) (eql version reason)))
                (multiple-value-bind (report status) (%core unsupported)
                  (and (eq :unsupported-schema status)
                       (eq :collected (getf report :availability))
                       (null (getf report :schema-supported))
                       (eql version (getf report :schema-version))
                       (null (getf report :data))
                       (null (getf report :field-availability))))))))))
  (values))
