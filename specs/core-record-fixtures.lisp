;;;; specs/core-record-fixtures.lisp
;;;;
;;;; Small cl-spec v1 result records, built fresh from descriptors, for the
;;;; record-fidelity properties (specs/core-records.lisp) and their fixed cases
;;;; (tests/spec-core-record-test.lisp).  Needs no cl-spec, so the fixed cases
;;;; run in the default suite.
;;;;
;;;; What a record means is restated here from cl-spec's public record contract
;;;; -- its SCHEMA-INFO for version 1 and the documented shape of RESULT-DATA --
;;;; and never read off the adapter under test: nothing here consults
;;;; cl-mcp/src/spec-core-record's *RECORD-SHAPES*, +SENTINEL-FIELDS+ or
;;;; +V1-REQUIRED-METADATA+.  Repeating the requirement is the point: a check
;;;; whose expected value came from the code it checks would agree with any
;;;; mistake in it.
;;;;
;;;; Every builder returns a fresh list, so nothing a projection or a later
;;;; call does can change an expectation taken earlier.  Descriptors are plain
;;;; data drawn with CL:RANDOM, which cl-spec binds from the run's seed, and
;;;; use only keywords that already exist.
;;;;
;;;; Projection can register opaque values in cl-mcp's object registry (see
;;;; EXTERNALIZE-VALUE).  WITH-ISOLATED-OBJECT-REGISTRY binds a registry of the
;;;; check's own around its body, so a check neither evicts a user's object
;;;; ids nor sees them.  It is a dynamic binding: it covers the body's own
;;;; thread only, which is where every call it wraps runs.

(defpackage #:cl-mcp/specs/core-record-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/object-registry
                #:*object-registry*
                #:make-object-registry)
  (:export #:+required-metadata+
           #:+sentinel-fields+
           #:+value-fields+
           #:+seed-generator-limit+
           #:with-isolated-object-registry
           #:call-with-isolated-object-registry
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
           #:permute-record
           #:error-chain-record
           #:chain-container-path
           #:malformed-variant
           #:unsupported-variant))

(in-package #:cl-mcp/specs/core-record-fixtures)

;;; ------------------------------------------------------------------------
;;; The record contract, restated

(defparameter +required-metadata+
  '(:schema-version :record-kind :entity-kind :definition-digest
    :definition-digest-complete :definition-digest-covers :capabilities)
  "The metadata cl-spec's SCHEMA-INFO lists as required for version 1.  A v1
record without one of them is malformed, not old.")

(defparameter +sentinel-fields+
  '(:shrink-report :generation-report :case-report
    :digest-omissions :digest-exclusions)
  "The result fields cl-spec documents as holding the bare keyword
:NOT-COLLECTED when the thing was not collected.  There, :NOT-COLLECTED says
the value is missing; anywhere else it is a value like any other.")

(defparameter +value-fields+
  '(:status :failure-phase :failure-reason :shrunk-outcome :profile
    :state-constraints)
  "Ordinary result fields whose value is a word or NIL, where :NOT-COLLECTED,
if it appeared, would be the value itself.")

(defparameter +seed-generator-limit+ (expt 2 62)
  "The exclusive bound of the seeds cl-spec draws for itself.  cl-spec accepts
any non-negative integer a caller passes, so seeds at and above this bound are
in the domain too.")

;;; ------------------------------------------------------------------------
;;; Isolation

(defun call-with-isolated-object-registry (thunk)
  "Call THUNK with *OBJECT-REGISTRY* bound to a fresh registry of its own."
  (let ((*object-registry* (make-object-registry)))
    (funcall thunk)))

(defmacro with-isolated-object-registry (&body body)
  "Run BODY with *OBJECT-REGISTRY* bound to a fresh registry of its own."
  `(call-with-isolated-object-registry (lambda () ,@body)))

;;; ------------------------------------------------------------------------
;;; Independent expectations

(defun decimal-string (integer)
  "Return the decimal digits of the non-negative INTEGER, by repeated division
rather than by the printer or anything the adapter uses."
  (check-type integer (integer 0))
  (if (zerop integer)
      "0"
      (let ((digits '()))
        (loop until (zerop integer)
              do (multiple-value-bind (quotient remainder) (floor integer 10)
                   (push (code-char (+ (char-code #\0) remainder)) digits)
                   (setf integer quotient)))
        (coerce digits 'string))))

;;; ------------------------------------------------------------------------
;;; Records

(defun make-result-record (&key (seed 7) (status :passed) (entity-kind :property)
                             (trials 3) (definition-digest-complete t)
                             (counterexample '()) (failure '()) (shrunk-failure '())
                             (digest-exclusions '()))
  "Return a fresh, valid cl-spec v1 result record: the seven required metadata
keys, both optional ones, and the fields RESULT-DATA documents, 31 keys in
all.  Sentinel fields say :NOT-COLLECTED unless given.  Strings stay under 30
characters, so a :MAX-CHARS of 30 or more cuts nothing but a field a check
lengthens on purpose."
  (list :schema-version 1
        :record-kind :result
        :entity-kind entity-kind
        :definition-digest (copy-seq "fnv1a64-v1:0123456789abcdef")
        :definition-digest-complete definition-digest-complete
        :definition-digest-covers :declaration-and-registered-dependencies
        :digest-omissions (list)
        :digest-exclusions (copy-list digest-exclusions)
        :capabilities (list :generation :available :shrinking :available
                            :instrumentation :none)
        :state-constraints nil
        :name 'fixture-property
        :status status
        :trials trials
        :budget trials
        :rejected 0
        :seed seed
        :profile :normal
        :options nil
        :provenance (list :backend (copy-seq "check-it")
                          :cl-spec-version (copy-seq "0.1.0"))
        :counterexample (copy-tree counterexample)
        :shrunk-counterexample (list)
        :shrunk-outcome nil
        :shrink-report :not-collected
        :generation-report :not-collected
        :failure-phase nil
        :failure-reason nil
        :case-report :not-collected
        :failure (copy-tree failure)
        :shrunk-failure (copy-tree shrunk-failure)
        :elapsed 0.01))

(defun record-without (record key)
  "Return a fresh copy of RECORD with every occurrence of KEY removed."
  (loop for (indicator value) on record by #'cddr
        unless (eq indicator key)
          append (list indicator value)))

(defun record-with (record key value &key (position :back))
  "Return a fresh copy of RECORD without KEY, then with KEY holding VALUE at
the front or the back."
  (let ((rest (record-without record key)))
    (ecase position
      (:front (list* key value rest))
      (:back (append rest (list key value))))))

(defun permute-record (record)
  "Return a fresh copy of RECORD with its key/value pairs in a random order,
drawn with CL:RANDOM; each pair stays together."
  (let ((pairs (coerce (loop for (key value) on record by #'cddr
                             collect (list key value))
                       'vector)))
    (loop for i from (1- (length pairs)) downto 1
          do (rotatef (aref pairs i) (aref pairs (random (1+ i)))))
    (loop for pair across pairs append (copy-list pair))))

;;; ------------------------------------------------------------------------
;;; Reading projection nodes

(defun object-field (node key)
  "Return (values CHILD PRESENT-P) for the JSON key KEY of the (:OBJECT ...)
NODE.  PRESENT-P tells a missing key from one whose child is (:SCALAR NIL)."
  (if (and (consp node) (eq :object (first node)))
      (let ((entry (assoc key (second node) :test #'equal)))
        (values (cdr entry) (and entry t)))
      (values nil nil)))

(defun node-at (node path)
  "Return (values CHILD FOUND-P) for PATH, a list of JSON keys and array
indices, starting at NODE."
  (if (null path)
      (values node t)
      (let ((step (first path)))
        (cond ((and (stringp step) (consp node) (eq :object (first node)))
               (multiple-value-bind (child present-p) (object-field node step)
                 (if present-p
                     (node-at child (rest path))
                     (values nil nil))))
              ((and (integerp step) (consp node) (eq :array (first node))
                    (< step (length (second node))))
               (node-at (nth step (second node)) (rest path)))
              (t (values nil nil))))))

(defun object-keys (node)
  "Return the JSON keys of the (:OBJECT ...) NODE, in any order."
  (mapcar #'car (second node)))

;;; ------------------------------------------------------------------------
;;; A. Availability

(defun %pick (sequence)
  "Return a random element of SEQUENCE."
  (elt sequence (random (length sequence))))

(defun draw-availability-case ()
  "Return a case for the availability property: one sentinel field, one
ordinary value field, the position the checked key is put at, and a seed."
  ;; LET evaluates its forms in order, so a seed replays the same case.
  (let ((sentinel (%pick +sentinel-fields+))
        (value-field (%pick +value-fields+))
        (position (%pick '(:front :back)))
        (seed (random 1000)))
    (list :sentinel-field sentinel :value-field value-field
          :position position :seed seed)))

;;; ------------------------------------------------------------------------
;;; B. Field roles

(defparameter +capture-values+
  (list nil 0 "text" :unavailable
        '(:unavailable :reason :opaque-value :type :hash-table)
        '(:availability :unavailable :reason :opaque-value)
        '(1 2 3))
  "Collected capture values: NIL, atoms, and lists shaped like cl-spec's own
metadata.  Each is application data whatever it looks like.")

(defun draw-role-case ()
  "Return a case for the field-role property: which way each role-bearing
field is set, and the collected capture value."
  (let ((complete (%pick '(t nil)))
        (shrunk-failure (%pick '(:none :present)))
        (counterexample (%pick '(:empty :present)))
        (exclusions (%pick '(:empty :present)))
        (phase (%pick '(:absent :present-nil)))
        (capture (random (length +capture-values+))))
    (list :definition-digest-complete complete
          :shrunk-failure shrunk-failure
          :counterexample counterexample
          :digest-exclusions exclusions
          :failure-phase phase
          :capture-index capture)))

(defun role-case-record (case)
  "Return a fresh record for the field-role CASE and the capture value it uses,
as (values RECORD CAPTURE-VALUE)."
  (let* ((capture-value (copy-tree (nth (getf case :capture-index) +capture-values+)))
         (failure (list :status :failed :reason :target-returned
                        :state (list :capture
                                     (list :status :collected :declared (list 'before)
                                           :values (list (list :name 'before
                                                               :availability :collected
                                                               :value capture-value))))))
         (record (make-result-record
                  :status :failed
                  :definition-digest-complete (getf case :definition-digest-complete)
                  :failure failure
                  :shrunk-failure (ecase (getf case :shrunk-failure)
                                    (:none '())
                                    (:present (list :status :failed)))
                  :counterexample (ecase (getf case :counterexample)
                                    (:empty '())
                                    (:present (list 'x 1)))
                  :digest-exclusions (ecase (getf case :digest-exclusions)
                                       (:empty '())
                                       (:present (list :target-implementation))))))
    (values (ecase (getf case :failure-phase)
              (:absent (record-without record :failure-phase))
              (:present-nil record))
            capture-value)))

;;; ------------------------------------------------------------------------
;;; C. Seeds

(defun draw-seed-case ()
  "Return a case for the seed property: five seeds, always one small, one
around 2^53, one just under cl-spec's own draw limit, one anywhere below it,
and one at or past it -- cl-spec accepts any non-negative integer -- plus a
small trial count."
  (let ((small (random 1000))
        (around-2^53 (+ (expt 2 53) (- (random 5) 2)))
        (under-limit (- +seed-generator-limit+ 1 (random 4)))
        (anywhere (random +seed-generator-limit+))
        (past-limit (+ +seed-generator-limit+ (random (expt 2 64))))
        (trials (1+ (random 200))))
    (list :seeds (list small around-2^53 under-limit anywhere past-limit)
          :trials trials)))

;;; ------------------------------------------------------------------------
;;; D. Order, duplicates, unknown keys

(defparameter +unknown-keys+ '(:x-future-alpha :x-future-beta :x-future-gamma)
  "Keys no version-1 record declares.  A fixed set, so no key is interned.")

(defparameter +duplicable-fields+
  '((:status . :failed) (:trials . 99) (:profile . :smoke) (:seed . 123)
    (:shrink-report . :not-collected) (:failure-reason . :unexpected))
  "Known fields and a different value to put in a later, duplicate occurrence.")

(defun draw-key-relation-case ()
  "Return a case for the key-relation property: a base record's seed and
status, the known field to duplicate, and how many unknown keys to add."
  (let ((seed (random 1000))
        (status (%pick '(:passed :failed)))
        (duplicate (random (length +duplicable-fields+)))
        (unknown-count (1+ (random (length +unknown-keys+)))))
    (list :seed seed :status status :duplicate-index duplicate
          :unknown-count unknown-count)))

;;; ------------------------------------------------------------------------
;;; E. Cuts

(defun draw-cut-case ()
  "Return a case for the cut property: the one limit to push, its bound, and a
filler length for a string.  :LENGTH bounds are at least 32, above the 31 keys
of a record, and :CHARS bounds at least 30, above every other string in it, so
only the field the check lengthens can be cut."
  (let ((kind (%pick '(:length :chars :depth))))
    (list :kind kind
          :limit (ecase kind
                   (:length (+ 32 (random 9)))
                   (:chars (+ 30 (random 11)))
                   (:depth (+ 3 (random 5)))))))

(defun error-chain-record (deepest)
  "Return a fresh failing record whose failure explanation nests error datums
so that the deepest container in the chain sits at depth DEEPEST, counting the
record itself as depth 0: failure 1, explanation 2, then alternately an
:ERRORS array and an error datum.  DEEPEST is at least 2."
  (check-type deepest (integer 2))
  (labels ((errors-at (depth)
             ;; The :ERRORS array at DEPTH, holding datums from DEPTH + 1 on.
             (if (>= depth deepest)
                 (list)
                 (list (datum-at (1+ depth)))))
           (datum-at (depth)
             (if (>= depth deepest)
                 (list :kind :leaf-datum)
                 (list :kind :nested-datum :errors (errors-at (1+ depth))))))
    (make-result-record
     :status :failed
     :failure (list :status :failed
                    :explanation (if (>= 2 deepest)
                                     (list :valid nil)
                                     (list :valid nil :errors (errors-at 3)))))))

(defun chain-container-path (depth)
  "Return the JSON path of the container at DEPTH in ERROR-CHAIN-RECORD's
chain: failure, explanation, then errors, 0, errors, 0 ..."
  (check-type depth (integer 1))
  (let ((path (list "failure")))
    (when (>= depth 2) (setf path (append path (list "explanation"))))
    (loop for d from 3 to depth
          do (setf path (append path (list (if (oddp d) "errors" 0)))))
    path))

;;; ------------------------------------------------------------------------
;;; F. Validation

(defun draw-validation-case ()
  "Return a case for the validation property: which malformation to apply, the
required key it may drop, and the unsupported version to declare."
  (let ((malformation (%pick '(:drop-required :nil :improper :odd-length
                               :non-keyword-indicator :record-kind :entity-kind
                               :no-schema-version)))
        (required (%pick +required-metadata+))
        (version (%pick (list 0 2 3 99 (expt 2 62)))))
    (list :malformation malformation :required required :version version)))

(defun malformed-variant (case)
  "Return a fresh record, built from a valid one, that CASE's malformation
breaks in exactly one way, and the expected kinds to check it against, as
\(values RECORD EXPECTED-RECORD-KIND EXPECTED-ENTITY-KIND)."
  (let ((valid (make-result-record)))
    (ecase (getf case :malformation)
      (:drop-required
       (values (record-without valid (getf case :required)) :result :property))
      (:nil (values nil :result :property))
      (:improper (values (append (copy-list valid) :tail) :result :property))
      (:odd-length (values (append (copy-list valid) (list :dangling)) :result :property))
      (:non-keyword-indicator (values (list* "status" :passed valid) :result :property))
      (:record-kind (values valid :definition :property))
      (:entity-kind (values valid :result :function-spec))
      (:no-schema-version (values (record-without valid :schema-version) :result :property)))))

(defun unsupported-variant (case)
  "Return a fresh record that differs from a valid one only in declaring CASE's
schema version, which is not 1."
  (record-with (make-result-record) :schema-version (getf case :version)
               :position :front))
