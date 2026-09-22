;;;; specs/check-verdicts.lisp
;;;;
;;;; Properties of the verdict layer: what spec-check may say it established
;;;; from the results a selection produced.  The functions checked are the
;;;; internal ones of cl-mcp/src/spec-adapter-report that CHECK-REPORT builds
;;;; its answer from:
;;;;
;;;;   %COUNTS             the per-status tally
;;;;   %CONTRACT-PLIST     a contract's refusal count and effective trials
;;;;   %VERIFIED-P         the verified verdict
;;;;   %VERIFICATION-GAPS  what the run could not establish
;;;;
;;;; Each property calls its function directly, on this thread, with inputs in
;;;; the shape it really receives (specs/check-verdict-fixtures.lisp).  None
;;;; goes through CHECK-REPORT, whose selection, deadline thread and rendering
;;;; are not part of this domain; its connection to these four functions is
;;;; the fixed integration tests of tests/check-verdict-test.lisp.  No
;;;; expectation is computed by the functions under test or their helpers:
;;;; each comes from the fixtures' policy tables, from a count of the drawn
;;;; descriptors, or from the drawn numbers.
;;;;
;;;; Verified domain: finite lists of normalized results of every kind in
;;;; +RESULT-KINDS+ -- every status, property and contract runs, every
;;;; refusal-count state, case report and schema state the adapter produces
;;;; -- under every selection kind or none; refusal counts read from a v1
;;;; record or through the legacy readers.  Robustness rows (a property with
;;;; no trial count, an unreadable declaration beside a usable count) are
;;;; checked for the verdict only.  Not covered: selecting from a real
;;;; registry, routing, the deadline, rendering, the legacy fallback beyond
;;;; the two refusal readers, and a record availability of :UNAVAILABLE.

(defpackage #:cl-mcp/specs/check-verdicts
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:%counts
                #:%contract-plist
                #:%verified-p
                #:%verification-gaps)
  (:import-from #:cl-mcp/specs/check-verdict-fixtures
                #:+all-statuses+
                #:+count-fields+
                #:selection-kind
                #:rejection-row
                #:build-result
                #:build-selection
                #:expected-gaps
                #:shuffle
                #:status-count
                #:rejection-inputs
                #:draw-count-case
                #:draw-rejection-case
                #:draw-verdict-case
                #:draw-gap-case)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/check-verdicts)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(check-verdict-counts-keep-every-status
    check-verdict-effective-trials-only-from-a-usable-count
    check-verdict-verified-needs-evidence-from-every-result
    check-verdict-gaps-name-each-shortfall-and-nothing-else))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(count-case rejection-case verdict-case gap-case))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(count-case-generator rejection-case-generator verdict-case-generator
    gap-case-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none, as it has no
contract.  Its fixed cases are the Rove tests of tests/check-verdict-test.lisp."
  '())

;;; ------------------------------------------------------------------------
;;; Counts

(defun %by-status-count (counts status)
  "Return the BY-STATUS count COUNTS gives STATUS, 0 when it has no entry."
  (or (cdr (assoc status (getf counts :by-status))) 0))

(defun %counts-match-p (counts descriptors)
  "Return true when COUNTS is the exact tally of the results DESCRIPTORS
describe: each named field the number of results with its status, OTHER the
number with a status of no field of its own, BY-STATUS one positive entry per
status that occurred, and SELECTED their length and each sum."
  (let ((by-status (getf counts :by-status))
        (named (mapcar #'car +count-fields+)))
    (and (eql (length descriptors) (getf counts :selected))
         (every (lambda (entry)
                  (eql (status-count descriptors (car entry))
                       (getf counts (cdr entry))))
                +count-fields+)
         (eql (loop for status in +all-statuses+
                    unless (member status named)
                      sum (status-count descriptors status))
              (getf counts :other))
         ;; One entry per status, each a positive count, none invented.
         (= (length by-status) (length (remove-duplicates by-status :key #'car)))
         (every (lambda (entry)
                  (and (member (car entry) +all-statuses+)
                       (integerp (cdr entry))
                       (plusp (cdr entry))))
                by-status)
         (every (lambda (status)
                  (eql (status-count descriptors status)
                       (%by-status-count counts status)))
                +all-statuses+)
         ;; And the sums a reader relies on.
         (eql (getf counts :selected)
              (+ (loop for (nil . field) in +count-fields+ sum (getf counts field))
                 (getf counts :other)))
         (eql (getf counts :selected) (reduce #'+ by-status :key #'cdr)))))

(defun %counts-sum-p (whole part-a part-b)
  "Return true when every field of WHOLE is the sum of PART-A's and PART-B's."
  (and (every (lambda (field)
                (eql (getf whole field) (+ (getf part-a field) (getf part-b field))))
              (list* :selected :other (mapcar #'cdr +count-fields+)))
       (every (lambda (status)
                (eql (%by-status-count whole status)
                     (+ (%by-status-count part-a status)
                        (%by-status-count part-b status))))
              +all-statuses+)))

;;; ------------------------------------------------------------------------
;;; Refusal counts

(defun %contract-half-holds-p (instance)
  "Return true when %CONTRACT-PLIST reads the refusal count INSTANCE
describes as its +REJECTION-ROWS+ row says, and reads it from the record when
there is one."
  (multiple-value-bind (api source calls) (rejection-inputs instance)
    (destructuring-bind (&key row path executed rejected rejected-form precondition
                           failure-reason failure-reason-form)
        instance
      (let* ((row (rejection-row row))
             (usable (getf row :usable))
             (status (getf row :status))
             (source-before (copy-tree source))
             ;; Readable: the record declares the key, even as NIL; or the
             ;; legacy reader exists and answered without signalling.
             (readable (ecase path
                         (:record (not (eq rejected-form :absent)))
                         (:legacy (and (member rejected-form '(:value :nil :non-integer))
                                       t))))
             (reason-readable (eq failure-reason-form :value))
             (half (%contract-plist api (list :fixture-result) executed 2000
                                    source precondition)))
        (and
         ;; The one question every consumer asks, and the number it guards.
         (eq (and usable t) (getf half :rejected-usable))
         (if usable
             (and (eql (- executed rejected) (getf half :effective-trials))
                  (ecase (getf row :name)
                    (:usable t)
                    (:all-refused (eql 0 (getf half :effective-trials)))
                    (:no-precondition (eql executed (getf half :effective-trials)))))
             ;; Missing, not zero and not the raw trial count.
             (null (getf half :effective-trials)))
         (if (listp status)
             (member (getf half :rejection-status) status)
             (eq status (getf half :rejection-status)))
         ;; A count is published only as a count.
         (eql (and (integerp rejected) rejected) (getf half :rejected))
         (eq readable (getf half :rejected-readable))
         (eq (and readable (integerp rejected)) (getf half :rejected-measured))
         (eq precondition (getf half :precondition-p))
         (eq (and (getf row :overcounted) t) (getf half :rejected-overcounted))
         (eq (and (getf row :contradicted) t) (getf half :rejected-contradicted))
         (eql (and reason-readable failure-reason) (getf half :failure-reason))
         (eq reason-readable (getf half :failure-reason-readable))
         ;; The record is the only thing read when there is one, and it is
         ;; left as it was.
         (or (eq path :legacy) (null (car calls)))
         (equal source-before source))))))

;;; ------------------------------------------------------------------------
;;; Verdicts and gaps

(defun %insert (item list position)
  "Return a fresh copy of LIST with ITEM inserted before index POSITION."
  (append (subseq list 0 position) (list item) (nthcdr position list)))

(defun %without (index list)
  "Return a fresh copy of LIST without its element at INDEX."
  (append (subseq list 0 index) (nthcdr (1+ index) list)))

(defun %gaps-as-expected-p (gaps descriptors selection-name)
  "Return true when GAPS are, as a set and without duplicates, the gaps the
results DESCRIPTORS describe under a selection of kind SELECTION-NAME justify.
An empty list may also carry no-properties-selected: the empty-selection path
of CHECK-REPORT adds it, and the helper may or may not."
  (let ((expected (expected-gaps descriptors selection-name)))
    (and (= (length gaps) (length (remove-duplicates gaps)))
         (subsetp expected gaps)
         (subsetp gaps (if descriptors
                           expected
                           (cons :no-properties-selected expected))))))

(defun %gaps-hold-p (descriptors selection-name)
  "Return true when %VERIFICATION-GAPS names exactly the justified gaps for
the results DESCRIPTORS describe under SELECTION-NAME, and still does with
each result taken away and with the selection's own shortfall taken away."
  (let* ((results (mapcar #'build-result descriptors))
         (results-before (copy-tree results))
         (selection (and selection-name
                         (build-selection selection-name
                                          :count (length descriptors))))
         (gaps (%verification-gaps results selection)))
    (and (%gaps-as-expected-p gaps descriptors selection-name)
         (equal results-before results)
         ;; Take each result's ground away: its gaps go, the rest stay.
         (loop for index below (length descriptors)
               always (%gaps-as-expected-p
                       (%verification-gaps (%without index results) selection)
                       (%without index descriptors)
                       selection-name))
         ;; Take the selection's ground away the same way.
         (or (null selection-name)
             (let ((plain (getf (selection-kind selection-name) :plain)))
               (%gaps-as-expected-p
                (%verification-gaps results
                                    (build-selection plain
                                                     :count (length descriptors)))
                descriptors
                plain))))))

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering runs nothing."
  (defgenerator count-case-generator ()
    "Draw two lists of results, one holding every status (DRAW-COUNT-CASE)."
    (draw-count-case))
  (defspec count-case list (:generator count-case-generator))
  (defgenerator rejection-case-generator ()
    "Draw every refusal-count row on both paths (DRAW-REJECTION-CASE)."
    (draw-rejection-case))
  (defspec rejection-case list (:generator rejection-case-generator))
  (defgenerator verdict-case-generator ()
    "Draw good results and one result of every other kind (DRAW-VERDICT-CASE)."
    (draw-verdict-case))
  (defspec verdict-case list (:generator verdict-case-generator))
  (defgenerator gap-case-generator ()
    "Draw a property, a contract and a mixed list of results (DRAW-GAP-CASE)."
    (draw-gap-case))
  (defspec gap-case list (:generator gap-case-generator))

  (defproperty check-verdict-counts-keep-every-status
      ((case count-case))
    "%COUNTS drops no status and invents none.  For the empty list and for two
drawn lists -- one holding each of the thirteen statuses zero to three times,
one holding every status at least once -- each named field is the number of
results with its status, OTHER the number with a status of no field of its
own, and BY-STATUS one positive entry per status that occurred, all counted
from the drawn descriptors.  SELECTED equals the list's length, the sum of the
named fields and OTHER, and the sum of BY-STATUS.  A reordered list gives the
same tally, and the tally of two lists joined is the sum of theirs.  Five
calls a trial."
    (:about %counts)
    (:kind :preservation)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key first second) case
      (let ((first-counts (%counts (mapcar #'build-result first)))
            (second-counts (%counts (mapcar #'build-result second))))
        (and (%counts-match-p (%counts '()) '())
             (%counts-match-p first-counts first)
             (%counts-match-p second-counts second)
             (%counts-match-p (%counts (mapcar #'build-result (shuffle second))) second)
             (%counts-sum-p (%counts (mapcar #'build-result (append first second)))
                            first-counts second-counts)))))

  (defproperty check-verdict-effective-trials-only-from-a-usable-count
      ((case rejection-case))
    "%CONTRACT-PLIST publishes effective trials only from a refusal count it
may subtract with.  Every trial reads one count of each +REJECTION-ROWS+ row
from a v1 record and through the legacy readers, and the unreadable row once
for each way a count can be missing: absent from the record, present as NIL
or as a non-count, no reader, a reader that signals, or one that answers NIL
or a non-count.  A known :PRE with 0 <= R <= E, or no :PRE with R = 0, is
usable with E - R effective trials -- 0 when every input was refused.  A
count or trial count that cannot be read, R < 0, R > E, a refusal without a
:PRE and an unknown :PRE are unusable, with no effective trials: not zero, not
the raw count.  The flags keep their own meanings: readable (the record
declares the key, or the reader answered), measured (readable and a count),
overcounted, contradicted, and the failure reason beside its own readable
flag.  On the record path the legacy readers, which answer differently, are
never called.  25 calls a trial."
    (:about %contract-plist)
    (:kind :invariant)
    (:trials (:smoke 5 :normal 25))
    (every #'%contract-half-holds-p case))

  (defproperty check-verdict-verified-needs-evidence-from-every-result
      ((case verdict-case))
    "%VERIFIED-P is true for one to four results that are each evidence --
passed, at least one trial reaching the function (for a contract, counted
after refusals), every declared case reached, record and declaration read --
in any order and under any names, seeds and timings.  Those results still
carry gaps: verified is not an empty gap list.  It is false for the empty
list, and false as soon as one result of any other kind is there, alone or
inserted anywhere among good ones: every status but passed (completed is not
verified), a property with zero or no trial count, a contract with zero or no
effective trials however many raw trials it ran, a declared case never
reached, a case report missing where cases are declared, a declaration whose
cases could not be read, and a record of a schema this adapter cannot read.
A cut projection is not one of them.  Sixty-five calls a trial: sixty-four to
%VERIFIED-P -- four for the good results and the empty list, two for each of
the thirty kinds of result that are not evidence -- and one to
%VERIFICATION-GAPS."
    (:about %verified-p %verification-gaps)
    (:kind :invariant)
    (:trials (:smoke 5 :normal 25))
    (destructuring-bind (&key good relabelled defects selection) case
      (let* ((results (mapcar #'build-result good))
             (results-before (copy-tree results)))
        (and (%verified-p results)
             (equal results-before results)
             (%verified-p (shuffle results))
             (%verified-p (mapcar #'build-result relabelled))
             (let ((gaps (%verification-gaps
                          results
                          (build-selection selection :count (length results)))))
               (and (member :input-coverage-unmeasured gaps)
                    (subsetp (getf (selection-kind selection) :gaps) gaps)))
             (not (%verified-p '()))
             (every (lambda (defect)
                      (destructuring-bind (&key descriptor position) defect
                        (let ((bad (build-result descriptor)))
                          (and (not (%verified-p (list bad)))
                               (not (%verified-p (%insert bad results position)))))))
                    defects)))))

  (defproperty check-verdict-gaps-name-each-shortfall-and-nothing-else
      ((case gap-case))
    "%VERIFICATION-GAPS names, as a set and without duplicates, exactly the
gaps the results and the selection justify: each result's own (its status when
that is not a verdict, zero or unknown effective trials, a case never reached
or a case report missing, generation that did not finish, an unreadable
record or declaration), the selection's (a contract, the properties or the
same-named property left unrun, related properties that could not be read),
rejection-counts-unmeasured unless every result is a contract with a usable
count, and input-coverage-unmeasured always.  Failed and error add nothing, a
shrink that ran out adds nothing, and cases whose existence is unknown are not
reported uncovered.  Every trial checks a property= or symbol= run, a
function= run and a mixed list that always pairs a usable count with a result
without one, then takes away each result and the selection's shortfall in
turn: the gaps it grounded go and the others stay.  Eight to fifteen calls a
trial."
    (:about %verification-gaps)
    (:kind :invariant)
    (:trials (:smoke 5 :normal 25))
    (every (lambda (family)
             (destructuring-bind (&key selection results) (getf case family)
               (%gaps-hold-p results selection)))
           '(:property :contract :mixed))))
