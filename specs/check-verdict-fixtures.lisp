;;;; specs/check-verdict-fixtures.lisp
;;;;
;;;; Results, selections and cl-spec reader stubs for the verdict properties
;;;; (specs/check-verdicts.lisp) and their fixed cases
;;;; (tests/check-verdict-test.lisp).  Needs no cl-spec, so the fixed cases run
;;;; in the default suite.
;;;;
;;;; Two layers are kept apart, because the functions under test sit on either
;;;; side of one normalization:
;;;;
;;;;   %CONTRACT-PLIST reads cl-spec's own answers: a v1 result record
;;;;   (MAKE-RESULT-RECORD, from specs/core-record-fixtures.lisp) or, for a
;;;;   cl-spec without one, the individual readers of a CL-SPEC-API.
;;;;   REJECTION-INPUTS builds both.
;;;;
;;;;   %COUNTS, %VERIFIED-P and %VERIFICATION-GAPS read what the adapter made
;;;;   of those answers: the result plists %RESULT-PLIST and %RUN-ONE return,
;;;;   and the selection plist the selection step returns.  BUILD-RESULT and
;;;;   BUILD-SELECTION make those shapes.
;;;;
;;;; What a result is worth is stated once, in +RESULT-KINDS+: whether it can
;;;; support a verified verdict, the verification gaps it justifies, and
;;;; whether it carries a refusal count a response may subtract with.
;;;; +SELECTION-KINDS+ does the same for a selection, and +REJECTION-ROWS+ for
;;;; one refusal count.  Each row restates the published meaning of
;;;; spec-check's fields -- verified, verification_gaps, counts and
;;;; results[].contract, as src/tools/spec-tools.lisp describes them to a
;;;; client.  No row is derived from the functions under test, and no builder
;;;; calls them.  The gaps a list of results should carry are its rows' own
;;;; labels put together (EXPECTED-GAPS): each shortfall named by the gap its
;;;; row declares, not a second pass over the result fields.
;;;;
;;;; Every builder returns fresh structure, so nothing a function under test
;;;; does to its argument can change an expectation taken before the call.
;;;; Descriptors are plain data drawn with CL:RANDOM, which cl-spec binds from
;;;; the run's seed, and use only keywords that already exist.

(defpackage #:cl-mcp/specs/check-verdict-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:make-cl-spec-api)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:+sentinel-fields+
                #:make-result-record
                #:record-with
                #:record-without)
  (:export #:+verdict-statuses+
           #:+machinery-statuses+
           #:+all-statuses+
           #:+count-fields+
           #:+result-kinds+
           #:+selection-kinds+
           #:+rejection-rows+
           #:result-kind
           #:result-kind-names
           #:selection-kind
           #:selection-kind-names
           #:rejection-row
           #:draw-result
           #:descriptor-row
           #:build-result
           #:build-selection
           #:expected-gaps
           #:expected-verified-p
           #:same-set-p
           #:shuffle
           #:status-count
           #:rejection-inputs
           #:draw-rejection-instance
           #:draw-count-case
           #:draw-rejection-case
           #:draw-verdict-case
           #:draw-gap-case))

(in-package #:cl-mcp/specs/check-verdict-fixtures)

;;; ------------------------------------------------------------------------
;;; The vocabulary, restated

(defparameter +verdict-statuses+ '(:passed :failed :error :skipped :pending)
  "The statuses cl-spec reports for a run it carried out.  spec-check's
description calls only passed, failed and error verdicts; skipped and pending
are cl-spec's own answers that are not.")

(defparameter +machinery-statuses+
  '(:timeout :not-run :generator-error :backend-error :not-registered
    :undefined-function :unsupported :internal-error)
  "The statuses the adapter gives a run that never produced a cl-spec result:
the deadline, the budget, or a condition classified on the way.")

(defparameter +all-statuses+ (append +verdict-statuses+ +machinery-statuses+)
  "Every status one result can carry, as spec-check's description lists them.")

(defparameter +count-fields+
  '((:passed . :passed) (:failed . :failed) (:error . :errored)
    (:timeout . :timed-out) (:not-run . :not-run))
  "The five statuses with a counts field of their own, and that field's name.
Every other status is counted under OTHER, and every status under BY-STATUS.")

;;; ------------------------------------------------------------------------
;;; The policy tables

(defparameter +result-kinds+
  '(;; Property runs.  A property's inputs are refused inside the generator,
    ;; which does not say how often, so no property result is COUNTED.
    (:name :property-passed :runs :property :status :passed
     :verifiable t :gaps () :trials :positive)
    (:name :property-passed-cut :runs :property :status :passed
     :verifiable t :gaps () :trials :positive :projection :cut)
    (:name :property-zero-trials :runs :property :status :passed
     :verifiable nil :gaps (:zero-trials) :trials :zero)
    (:name :property-failed :runs :property :status :failed
     :verifiable nil :gaps () :trials :positive)
    (:name :property-failed-shrink-limit :runs :property :status :failed
     :verifiable nil :gaps () :trials :positive :generation :shrinking-exhausted)
    (:name :property-error :runs :property :status :error
     :verifiable nil :gaps () :trials :positive)
    (:name :property-error-in-generation :runs :property :status :error
     :verifiable nil :gaps (:generation-incomplete) :trials :zero
     :phase :generation :generation :generation-exhausted)
    (:name :property-skipped :runs :property :status :skipped
     :verifiable nil :gaps (:skipped) :trials :zero)
    (:name :property-pending :runs :property :status :pending
     :verifiable nil :gaps (:pending) :trials :zero)
    (:name :property-schema-unsupported :runs :property :status :passed
     :verifiable nil :gaps (:core-schema-unsupported) :trials :positive
     :schema :unsupported)
    ;; Contract runs.  COUNTED when the refusal count may be subtracted with.
    (:name :contract-passed :runs :contract :status :passed
     :verifiable t :gaps () :counted t :count :usable)
    (:name :contract-passed-without-pre :runs :contract :status :passed
     :verifiable t :gaps () :counted t :count :no-precondition)
    (:name :contract-passed-every-case :runs :contract :status :passed
     :verifiable t :gaps () :counted t :count :usable :declares t
     :cases :all-reached)
    (:name :contract-passed-cut :runs :contract :status :passed
     :verifiable t :gaps () :counted t :count :usable :projection :cut)
    (:name :contract-passed-all-refused :runs :contract :status :passed
     :verifiable nil :gaps (:zero-trials) :counted t :count :all-refused)
    (:name :contract-skipped-all-refused :runs :contract :status :skipped
     :verifiable nil :gaps (:skipped) :counted t :count :all-refused)
    (:name :contract-passed-count-unusable :runs :contract :status :passed
     :verifiable nil :gaps (:effective-trials-unknown) :count :unusable)
    (:name :contract-failed :runs :contract :status :failed
     :verifiable nil :gaps () :counted t :count :usable)
    (:name :contract-failed-count-unusable :runs :contract :status :failed
     :verifiable nil :gaps (:effective-trials-unknown) :count :unusable)
    (:name :contract-failed-shrink-limit :runs :contract :status :failed
     :verifiable nil :gaps () :counted t :count :usable
     :generation :shrinking-exhausted)
    (:name :contract-error-in-generation :runs :contract :status :error
     :verifiable nil :gaps (:generation-incomplete) :counted t :count :zero-budget
     :phase :generation :generation :generation-exhausted)
    (:name :contract-error-after-generation :runs :contract :status :error
     :verifiable nil :gaps () :counted t :count :usable :phase :after-generation)
    (:name :contract-case-never-called :runs :contract :status :passed
     :verifiable nil :gaps (:cases-never-called) :counted t :count :usable
     :declares t :cases :never-called)
    (:name :contract-case-report-missing :runs :contract :status :passed
     :verifiable nil :gaps (:case-coverage-unknown) :counted t :count :usable
     :declares t :cases :report-missing)
    (:name :contract-declaration-unreadable :runs :contract :status :passed
     :verifiable nil :gaps (:contract-schema-unsupported :effective-trials-unknown)
     :count :precondition-unknown :declares :unknown)
    (:name :contract-schema-unsupported :runs :contract :status :passed
     :verifiable nil :gaps (:core-schema-unsupported) :counted t :count :usable
     :schema :unsupported)
    ;; Runs that never produced a cl-spec result, of either kind.  The status
    ;; is itself the gap, and nothing was counted.
    (:name :timeout :runs :either :status :timeout
     :verifiable nil :gaps (:timeout) :stopped t)
    (:name :not-run :runs :either :status :not-run
     :verifiable nil :gaps (:not-run) :stopped t)
    (:name :generator-error :runs :either :status :generator-error
     :verifiable nil :gaps (:generator-error) :stopped t)
    (:name :backend-error :runs :either :status :backend-error
     :verifiable nil :gaps (:backend-error) :stopped t)
    (:name :not-registered :runs :either :status :not-registered
     :verifiable nil :gaps (:not-registered) :stopped t)
    (:name :undefined-function :runs :either :status :undefined-function
     :verifiable nil :gaps (:undefined-function) :stopped t)
    (:name :unsupported :runs :either :status :unsupported
     :verifiable nil :gaps (:unsupported) :stopped t)
    (:name :internal-error :runs :either :status :internal-error
     :verifiable nil :gaps (:internal-error) :stopped t)
    ;; Robustness.  States no cl-spec reaches through the adapter, kept to show
    ;; that each verdict condition refuses on its own.  A cl-spec run always
    ;; counts a property's trials, and an unreadable declaration always makes
    ;; the precondition unknown too.  Their gaps are not part of the gap
    ;; property's domain.
    (:name :property-trials-uncounted :runs :property :status :passed
     :domain :robustness :verifiable nil :gaps :unspecified :trials :unknown)
    (:name :contract-declaration-unreadable-but-counted :runs :contract
     :status :passed :domain :robustness :verifiable nil
     :gaps (:contract-schema-unsupported) :counted t :count :usable
     :declares :unknown))
  "One row per kind of result, with what it is worth.

VERIFIABLE says whether the result can support a verified verdict: it passed,
at least one trial reached the function (for a contract, by the count left
after refusals), every declared case was reached, and both its record and its
contract's declaration could be read.  GAPS are the verification gaps the
result justifies on its own; COUNTED says the result is a contract run whose
refusal count may be subtracted with.  The remaining keys say how to build it.")

(defparameter +selection-kinds+
  '((:name :explicit-property :runs :property :plain :explicit-property :gaps ())
    (:name :about :runs :property :plain :about :gaps ())
    (:name :about-contract-left :runs :property :plain :about
     :gaps (:contract-not-run))
    (:name :about-own-property-left :runs :property :plain :about
     :gaps (:properties-not-run))
    (:name :about-both-left :runs :property :plain :about
     :gaps (:contract-not-run :properties-not-run))
    (:name :contract :runs :contract :plain :contract :gaps ())
    (:name :contract-properties-left :runs :contract :plain :contract
     :gaps (:properties-not-run))
    (:name :contract-own-property-left :runs :contract :plain :contract
     :gaps (:properties-not-run))
    (:name :contract-related-unknown :runs :contract :plain :contract
     :gaps (:related-properties-unknown)))
  "One row per kind of selection, with the gaps it justifies.

An :ABOUT selection leaves out a contract registered for the symbol and a
property registered under the symbol's own name; a contract selection leaves
out the properties about the symbol, and may have been unable to read whether
there are any.  PLAIN names the same selection with nothing left out.")

(defparameter +rejection-rows+
  '((:name :usable :precondition t :status :usable :usable t)
    (:name :all-refused :precondition t :status :usable :usable t)
    (:name :no-precondition :precondition nil :status :no-precondition :usable t)
    (:name :executed-uncounted :precondition t :status :trials-uncounted)
    (:name :negative :precondition :any :status :negative)
    (:name :overcounted :precondition :known :status :overcounted :overcounted t)
    (:name :contradicted :precondition nil :status :contradicted :contradicted t)
    (:name :precondition-unknown :precondition :unknown
     :status :precondition-unknown)
    (:name :overcounted-without-pre :precondition nil
     :status (:overcounted :contradicted) :overcounted t :contradicted t)
    (:name :rejected-unreadable :precondition :any :status :unmeasured))
  "One row per condition on a contract's refusal count, restated from
spec-check's description of results[].contract.

USABLE rows publish EFFECTIVE-TRIALS as executed minus rejected; every other
row publishes none -- not zero, and not the raw trial count.  STATUS is the
REJECTION-STATUS keyword, or the keywords either of which names a row with two
causes.  OVERCOUNTED and CONTRADICTED are the two flags of that name.
PRECONDITION is what the contract's declaration says about :PRE: T, NIL,
:UNKNOWN, :KNOWN (T or :UNKNOWN) or :ANY.")

(defun %row (table name)
  "Return the row of TABLE named NAME, signalling when there is none."
  (or (find name table :key (lambda (row) (getf row :name)))
      (error "No row ~S." name)))

(defun result-kind (name)
  "Return the +RESULT-KINDS+ row named NAME."
  (%row +result-kinds+ name))

(defun selection-kind (name)
  "Return the +SELECTION-KINDS+ row named NAME."
  (%row +selection-kinds+ name))

(defun rejection-row (name)
  "Return the +REJECTION-ROWS+ row named NAME."
  (%row +rejection-rows+ name))

(defun result-kind-names (&key (domain :normal) runs (verifiable :any) status)
  "Return the names of the result kinds in DOMAIN (:NORMAL, :ROBUSTNESS or
:ANY) that can run as RUNS (:PROPERTY or :CONTRACT; NIL for any), are
VERIFIABLE (T, NIL or :ANY) and have STATUS when one is given."
  (loop for row in +result-kinds+
        when (and (or (eq domain :any) (eq domain (or (getf row :domain) :normal)))
                  (or (null runs) (member (getf row :runs) (list runs :either)))
                  (or (eq verifiable :any)
                      (eq (and verifiable t) (and (getf row :verifiable) t)))
                  (or (null status) (eq status (getf row :status))))
          collect (getf row :name)))

(defun selection-kind-names (&optional runs)
  "Return the names of the selection kinds whose results run as RUNS."
  (loop for row in +selection-kinds+
        when (or (null runs) (eq runs (getf row :runs)))
          collect (getf row :name)))

;;; ------------------------------------------------------------------------
;;; Drawing

(defun %pick (list)
  "Return an element of LIST drawn with CL:RANDOM."
  (nth (random (length list)) list))

(defun %between (low high)
  "Return an integer from LOW to HIGH inclusive, drawn with CL:RANDOM."
  (+ low (random (1+ (- high low)))))

(defun shuffle (list)
  "Return a fresh copy of LIST in an order drawn with CL:RANDOM."
  (let ((items (coerce list 'vector)))
    (loop for i from (1- (length items)) downto 1
          do (rotatef (aref items i) (aref items (random (1+ i)))))
    (coerce items 'list)))

(defun %count-numbers (count)
  "Return (values EXECUTED REJECTED PRECONDITION REJECTION-STATUS) for a
contract whose refusal count is in state COUNT.  REJECTION-STATUS is the
keyword spec-check publishes for it."
  (let ((executed (%between 1 60)))
    (ecase count
      (:usable (values executed (%between 0 (1- executed)) t :usable))
      (:no-precondition (values executed 0 nil :no-precondition))
      (:all-refused (values executed executed t :usable))
      (:zero-budget (let ((pre (%pick '(t nil))))
                      (values 0 0 pre (if pre :usable :no-precondition))))
      (:precondition-unknown (values executed (%between 0 executed) :unknown
                                     :precondition-unknown))
      (:unusable
       (ecase (%pick '(:unmeasured :trials-uncounted :negative :overcounted
                       :contradicted))
         (:unmeasured (values executed nil (%pick '(t nil)) :unmeasured))
         (:trials-uncounted (values nil (%between 0 5) t :trials-uncounted))
         (:negative (values executed (- (%between 1 9)) t :negative))
         (:overcounted (values executed (+ executed (%between 1 9)) t :overcounted))
         (:contradicted (values executed (%between 1 executed) nil :contradicted)))))))

(defun draw-result (name &optional runs)
  "Return a descriptor for one result of kind NAME, with its numbers drawn.
RUNS picks :PROPERTY or :CONTRACT for a kind that can be either; it is drawn
when NIL."
  (let* ((row (result-kind name))
         (runs (case (getf row :runs)
                 (:either (or runs (%pick '(:property :contract))))
                 (t (getf row :runs))))
         (descriptor (list :kind-name name
                           :runs runs
                           :label (format nil "fixture-target-~D" (random 1000))
                           :seed (random (expt 2 62))
                           :elapsed (/ (%between 1 999) 1000.0))))
    (unless (getf row :stopped)
      (if (eq runs :contract)
          (multiple-value-bind (executed rejected precondition rejection-status)
              (%count-numbers (getf row :count))
            (setf descriptor (append descriptor
                                     (list :executed executed :rejected rejected
                                           :precondition precondition
                                           :rejection-status rejection-status))))
          (setf descriptor
                (append descriptor
                        (list :executed (ecase (getf row :trials)
                                          (:positive (%between 1 60))
                                          (:zero 0)
                                          (:unknown nil))))))
      (setf descriptor
            (append descriptor
                    (list :phase (case (getf row :phase)
                                   (:after-generation
                                    (%pick '(:capture :state-post :case-selection)))
                                   (t (getf row :phase)))
                          :case-report-form (%pick '(:not-collected :absent))))))
    descriptor))

(defun descriptor-row (descriptor)
  "Return the +RESULT-KINDS+ row DESCRIPTOR was drawn for."
  (result-kind (getf descriptor :kind-name)))

;;; ------------------------------------------------------------------------
;;; Building normalized results

(defparameter +record-keys+
  (loop for (key) on (make-result-record) by #'cddr collect key)
  "The keys of a v1 result record, in the order MAKE-RESULT-RECORD writes them.")

(defun %plist-entry (plist key)
  "Return (values VALUE PRESENT-P) for KEY in PLIST, stepping by pairs."
  (loop for (indicator value) on plist by #'cddr
        when (eq indicator key)
          do (return (values value t))
        finally (return (values nil nil))))

(defun %field-availability (record)
  "Return the field availability plist a report carries for RECORD: :ABSENT
for a missing key, :NOT-COLLECTED for the bare marker in a sentinel field, and
:COLLECTED for anything else -- a present NIL included."
  (loop for key in +record-keys+
        append (list key
                     (multiple-value-bind (value present-p) (%plist-entry record key)
                       (cond ((not present-p) :absent)
                             ((and (eq value :not-collected)
                                   (member key +sentinel-fields+))
                              :not-collected)
                             (t :collected))))))

(defun %case-report (cases executed)
  "Return the case report a record carries for CASES, or :NOT-COLLECTED."
  (flet ((case-entry (name called)
           (list :name name :documentation nil :called called :passed called
                 :failed 0 :error 0)))
    (ecase cases
      ((:none :report-missing) :not-collected)
      (:all-reached
       (let ((small (max 1 (floor (or executed 2) 2))))
         (list :selection :exclusive :unit :normal-trials
               :declared-cases (list :small :large)
               :cases (list (case-entry :small small) (case-entry :large 1))
               :case-selection-errors 0 :capture-errors 0
               :never-called (list))))
      (:never-called
       (list :selection :exclusive :unit :normal-trials
             :declared-cases (list :small :large)
             :cases (list (case-entry :small (or executed 1)) (case-entry :large 0))
             :case-selection-errors 0 :capture-errors 0
             :never-called (list :large))))))

(defun %generation-report (generation executed)
  "Return the generation report a record carries, or :NOT-COLLECTED."
  (ecase generation
    ((nil) :not-collected)
    (:shrinking-exhausted
     (list :scope :request :termination :budget-exhausted
           :exhaustion-phase :shrinking :attempts (+ 8 (or executed 0))
           :rejections 0 :budget 8))
    (:generation-exhausted
     (list :scope :request :termination :budget-exhausted
           :exhaustion-phase :generation :attempts 8 :rejections 8 :budget 8))))

(defun %result-source (row descriptor)
  "Return the v1 result record behind DESCRIPTOR's result."
  (let* ((contract (eq :contract (getf descriptor :runs)))
         (executed (getf descriptor :executed))
         (record (make-result-record :entity-kind (if contract :function-spec :property)
                                     :status (getf row :status)
                                     :trials executed
                                     :seed (getf descriptor :seed))))
    (setf record (record-with record :failure-phase (getf descriptor :phase)))
    (setf record (record-with record :generation-report
                              (%generation-report (getf row :generation) executed)))
    (when contract
      (setf record (record-with record :rejected (getf descriptor :rejected))))
    (let ((cases (or (getf row :cases) :none)))
      (if (and (eq cases :report-missing)
               (eq :absent (getf descriptor :case-report-form)))
          (record-without record :case-report)
          (record-with record :case-report (%case-report cases executed))))))

(defun %core-record (row descriptor)
  "Return the core record report %RESULT-PLIST carries for DESCRIPTOR."
  (if (eq :unsupported (getf row :schema))
      ;; A version this adapter cannot read: nothing of it is kept as a source.
      (list :availability :collected :schema-supported nil :schema-version 2
            :field-availability nil :unknown-keys nil :source nil
            :projection (list :complete t :issues nil) :data nil)
      (let ((record (%result-source row descriptor)))
        (list :availability :collected :schema-supported t :schema-version 1
              :source record
              :field-availability (%field-availability record)
              :unknown-keys nil
              :projection
              (if (eq :cut (getf row :projection))
                  (list :complete nil
                        :issues (list (list :path (list "counterexample")
                                            :limit :length :omitted 3 :exact t)))
                  (list :complete t :issues nil))
              :data (list :object (list))))))

(defun %contract-half (descriptor)
  "Return the contract half spec-check publishes for DESCRIPTOR's refusal count."
  (let* ((executed (getf descriptor :executed))
         (rejected (getf descriptor :rejected))
         (status (getf descriptor :rejection-status))
         (usable (and (member status '(:usable :no-precondition)) t)))
    (list :rejected (and (integerp rejected) rejected)
          :rejection-status status
          :rejected-measured (and (integerp rejected) t)
          :rejected-readable (not (eq status :unmeasured))
          :precondition-p (getf descriptor :precondition)
          :rejected-overcounted (eq status :overcounted)
          :rejected-contradicted (eq status :contradicted)
          :rejected-usable usable
          :effective-trials (and usable (- executed rejected))
          :failure-reason nil
          :failure-reason-readable t
          :explanation nil
          :explanation-readable nil
          :explanation-complete :not-applicable
          :explanation-omitted-chars nil)))

(defun %symbol-plist (label)
  "Return the symbol plist a response carries for a target named LABEL."
  (let ((name (string-upcase label)))
    (list :package "CL-MCP/SPECS/CHECK-VERDICT-FIXTURES" :name name
          :qualified (format nil "CL-MCP/SPECS/CHECK-VERDICT-FIXTURES::~A" name))))

(defun %budget-plist (budget)
  "Return the trial budget plist the adapter derives for a run."
  (list :budget budget :budget-source "backend-default" :property-trials nil
        :backend-default budget
        :budget-derivation "derived by the fixture, as the adapter derives it"))

(defun %digest-fields (contract)
  "Return the digest fields every result carries."
  (list :definition-digest (copy-seq "fnv1a64-v1:0123456789abcdef")
        :definition-digest-complete t
        :definition-digest-covers (if contract :contract :property)
        :definition-match :not-checked))

(defun %stopped-result (row descriptor)
  "Return the result %RUN-ONE gives a run that produced no cl-spec result."
  (let* ((status (getf row :status))
         (contract (eq :contract (getf descriptor :runs)))
         (text (format nil "~(~A~) for ~A" status (getf descriptor :label))))
    (append (list :property (%symbol-plist (getf descriptor :label))
                  :kind (if contract :contract :property)
                  :status status)
            (case status
              (:not-run (list :reason :budget-exhausted))
              (:timeout (list :timeout-seconds 0.3 :thread-leaked nil)))
            (list :trials (%budget-plist 25))
            (%digest-fields contract)
            (list :counterexample-status (if (eq status :not-run) :not-run :unavailable)
                  :counterexample-unavailable-reason text
                  :shrink-status (if (eq status :not-run) :not-run :unavailable))
            (unless (member status '(:not-run :timeout))
              (list :condition (list :type "FIXTURE-CONDITION" :message text)))
            (list :message text))))

(defun build-result (descriptor &key (contract nil contract-p))
  "Return the normalized result plist for DESCRIPTOR, in the shape
%RESULT-PLIST (or, for a run that stopped, %RUN-ONE) returns.  CONTRACT, when
given, is the contract half to carry instead of the one DESCRIPTOR describes."
  (let* ((row (descriptor-row descriptor))
         (status (getf row :status))
         (runs (getf descriptor :runs))
         (verdict (member status '(:failed :error))))
    (if (getf row :stopped)
        (%stopped-result row descriptor)
        (append
         (list :core-schema (unless (eq :unsupported (getf row :schema))
                              (list :schema-version 1 :record-kind :result
                                    :entity-kind (if (eq runs :contract)
                                                     :function-spec
                                                     :property)))
               :core-record (%core-record row descriptor)
               :property (%symbol-plist (getf descriptor :label))
               :kind runs
               :declares-cases (and (eq runs :contract) (getf row :declares))
               :contract (and (eq runs :contract)
                              (if contract-p contract (%contract-half descriptor)))
               :status status
               :trials (list* :executed (getf descriptor :executed)
                              (%budget-plist (or (getf descriptor :executed) 25)))
               :seed (format nil "~D" (getf descriptor :seed))
               :profile (unless (eq runs :contract) :normal)
               :counterexample (list)
               :counterexample-status (if verdict :present :not-applicable)
               :counterexample-unavailable-reason nil
               :shrunk-counterexample (list)
               :shrink-status (if verdict :none :not-applicable)
               :shrink-note nil
               :condition nil
               :elapsed (getf descriptor :elapsed))
         (%digest-fields (eq runs :contract))))))

;;; ------------------------------------------------------------------------
;;; Building selections

(defun build-selection (name &key (count 1) (label "fixture-subject"))
  "Return the selection plist of kind NAME for a selection of COUNT names, in
the shape the selection step returns for property=, symbol= or function=.
LABEL names the subject; it changes only text."
  (let* ((row (selection-kind name))
         (gaps (getf row :gaps))
         (subject (%symbol-plist label))
         (selected (loop for i below count
                         collect (%symbol-plist (format nil "~A-~D" label i))))
         (note (format nil "notes about ~A" label)))
    (ecase name
      (:explicit-property
       (list :mode "explicit" :kind :property :requested (list :property subject)
             :selected (list subject) :count 1 :source "explicit property argument"
             :coverage (format nil "Only ~A." label)))
      ((:about :about-contract-left :about-own-property-left :about-both-left)
       (list :mode "about" :kind :property :requested (list :symbol subject)
             :selected selected :count count
             :source "cl-spec:semantic-data -> :properties-about"
             :coverage (format nil "Direct registrations about ~A." label)
             :own-property-not-run (and (member :properties-not-run gaps) subject)
             :contract-not-run (and (member :contract-not-run gaps) subject)
             :notes (and gaps (list note))))
      ((:contract :contract-properties-left :contract-own-property-left
        :contract-related-unknown)
       (list :mode "contract" :kind :contract :requested (list :function subject)
             :selected (list subject) :count 1 :source "explicit function argument"
             :coverage (format nil "Only the contract of ~A." label)
             :properties-not-run (and (eq name :contract-properties-left)
                                      (list (%symbol-plist (format nil "~A-about" label))))
             :own-property-not-run (and (eq name :contract-own-property-left) subject)
             :properties-not-run-read (not (eq name :contract-related-unknown))
             :notes (and gaps (list note)))))))

;;; ------------------------------------------------------------------------
;;; Expectations

(defun expected-verified-p (descriptors)
  "Return true when DESCRIPTORS should be verified: something ran, and every
result is of a kind that can support a verified verdict."
  (and descriptors
       (every (lambda (descriptor) (getf (descriptor-row descriptor) :verifiable))
              descriptors)
       t))

(defun expected-gaps (descriptors &optional selection-name)
  "Return the gaps a list of results of DESCRIPTORS under a selection of kind
SELECTION-NAME (NIL for none) should carry, without duplicates.

Each result's row names its own gaps, and so does the selection's.  Beside
them: refusal counts are unmeasured unless every result is a contract run whose
count may be subtracted with -- nothing ran, a property ran, or a count is
unusable, and the gap is there -- and input coverage is never measured."
  (let ((rows (mapcar #'descriptor-row descriptors)))
    (dolist (row rows)
      (unless (listp (getf row :gaps))
        (error "Result kind ~S states no gaps to expect." (getf row :name))))
    (remove-duplicates
     (append (loop for row in rows append (copy-list (getf row :gaps)))
             (and selection-name (copy-list (getf (selection-kind selection-name) :gaps)))
             (unless (and rows (every (lambda (row) (getf row :counted)) rows))
               (list :rejection-counts-unmeasured))
             (list :input-coverage-unmeasured)))))

(defun same-set-p (a b)
  "Return true when lists A and B hold the same elements, ignoring order."
  (and (subsetp a b) (subsetp b a)))

(defun status-count (descriptors status)
  "Return how many of DESCRIPTORS are results with STATUS."
  (count status descriptors
         :key (lambda (descriptor) (getf (descriptor-row descriptor) :status))))

;;; ------------------------------------------------------------------------
;;; %CONTRACT-PLIST's inputs

(defparameter +non-integer-counts+ '(2.5 :many "3" (1))
  "Values a reader or record may hold where a count belongs, none a count.")

(defun draw-rejection-instance (row-name path &optional unreadable-form)
  "Return a descriptor for one refusal count of row ROW-NAME read along PATH
(:RECORD or :LEGACY), with its numbers drawn.  UNREADABLE-FORM fixes how the
:REJECTED-UNREADABLE row's count is missing; it is drawn when NIL."
  (let* ((row (rejection-row row-name))
         (precondition (ecase (getf row :precondition)
                         ((t) t)
                         ((nil) nil)
                         (:unknown :unknown)
                         (:known (%pick '(t :unknown)))
                         (:any (%pick '(t nil :unknown)))))
         (executed (%between 0 60))
         (rejected nil)
         (form :value))
    (ecase row-name
      (:usable (setf rejected (%pick (list 0 executed (%between 0 executed)))))
      (:all-refused (setf executed (max executed 1) rejected executed))
      (:no-precondition (setf rejected 0))
      (:executed-uncounted (setf executed (%pick (list nil 3.0 :many))
                                 rejected (%between 0 10)))
      (:negative (setf rejected (- (%between 1 9))))
      ((:overcounted :overcounted-without-pre)
       (setf rejected (+ executed (%between 1 9))))
      (:contradicted (setf executed (max executed 1)
                           rejected (%between 1 executed)))
      (:precondition-unknown (setf rejected (%between 0 executed)))
      (:rejected-unreadable
       (setf executed (%pick (list executed nil)))
       (setf form (or unreadable-form
                      (%pick (if (eq path :record)
                                 '(:absent :nil :non-integer)
                                 '(:no-reader :signals :nil :non-integer)))))
       (setf rejected (case form
                        (:non-integer (copy-tree (%pick +non-integer-counts+)))
                        (t nil)))))
    (list :row row-name :path path :executed executed :rejected rejected
          :rejected-form form :precondition precondition
          :failure-reason (%pick '(nil :postcondition :return-spec :precondition
                                   :condition))
          :failure-reason-form (%pick (if (eq path :record)
                                          '(:value :absent)
                                          '(:value :no-reader :signals))))))

(defun rejection-inputs (instance)
  "Return (values API SOURCE CALLS) for one refusal count INSTANCE.

SOURCE is the record %CONTRACT-PLIST is given on the record path and NIL on
the legacy path.  On the record path API's readers answer something other than
the record, so an answer taken from them shows; on the legacy path they answer
INSTANCE's values, are missing, or signal, as INSTANCE says.  CALLS is a cons
whose CAR collects the key of every reader called, most recent first."
  (destructuring-bind (&key path executed rejected rejected-form failure-reason
                         failure-reason-form &allow-other-keys)
      instance
    (let ((calls (list nil))
          (functions '()))
      (flet ((reader (key answer)
               (setf functions
                     (list* key (lambda (result)
                                  (declare (ignore result))
                                  (push key (car calls))
                                  (funcall answer))
                            functions))))
        (ecase path
          (:record
           (reader :check-rejected
                   (lambda () (if (integerp rejected) (+ rejected 7) 3)))
           (reader :check-failure-reason (lambda () :answer-from-the-reader))
           (let ((record (make-result-record :entity-kind :function-spec
                                             :trials executed)))
             (setf record (if (eq rejected-form :absent)
                              (record-without record :rejected)
                              (record-with record :rejected (copy-tree rejected))))
             (setf record (if (eq failure-reason-form :absent)
                              (record-without record :failure-reason)
                              (record-with record :failure-reason failure-reason)))
             (values (make-cl-spec-api :functions functions) record calls)))
          (:legacy
           (ecase rejected-form
             ((:value :nil :non-integer)
              (reader :check-rejected (lambda () (copy-tree rejected))))
             (:signals
              (reader :check-rejected (lambda () (error "The refusal reader broke."))))
             (:no-reader nil))
           (ecase failure-reason-form
             (:value (reader :check-failure-reason (lambda () failure-reason)))
             (:signals (reader :check-failure-reason
                               (lambda () (error "The reason reader broke."))))
             (:no-reader nil))
           (values (make-cl-spec-api :functions functions) nil calls)))))))

;;; ------------------------------------------------------------------------
;;; Generators' draws

(defun draw-count-case ()
  "Return a count case: two lists of result descriptors.  FIRST holds each
status zero to three times; SECOND holds every status one to three times, so
every status is present in every trial."
  (flet ((draw-list (low)
           (shuffle
            (loop for status in +all-statuses+
                  append (loop repeat (%between low 3)
                               collect (draw-result
                                        (%pick (result-kind-names :status status))))))))
    (list :first (draw-list 0) :second (draw-list 1))))

(defun draw-rejection-case ()
  "Return a rejection case: one instance of every +REJECTION-ROWS+ row on
both paths, and the unreadable row once for each way a count can be missing."
  (append
   (loop for row in +rejection-rows+
         for name = (getf row :name)
         unless (eq name :rejected-unreadable)
           append (list (draw-rejection-instance name :record)
                        (draw-rejection-instance name :legacy)))
   (loop for (path form) in '((:record :absent) (:record :nil) (:record :non-integer)
                              (:legacy :no-reader) (:legacy :signals) (:legacy :nil)
                              (:legacy :non-integer))
         collect (draw-rejection-instance :rejected-unreadable path form))))

(defun draw-verdict-case ()
  "Return a verdict case: GOOD, one to four results that can support a
verified verdict; RELABELLED, the same kinds drawn again, so every number and
text differs; DEFECTS, one result of every kind that cannot, each with the
position it goes in; and a SELECTION kind."
  (let* ((good-names (loop repeat (%between 1 4)
                           collect (%pick (result-kind-names :verifiable t))))
         (good (mapcar #'draw-result good-names)))
    (list :good good
          :relabelled (loop for descriptor in good
                            collect (draw-result (getf descriptor :kind-name)
                                                 (getf descriptor :runs)))
          :defects (loop for name in (result-kind-names :domain :any :verifiable nil)
                         collect (list :descriptor (draw-result name)
                                       :position (random (1+ (length good)))))
          :selection (%pick (selection-kind-names)))))

(defun draw-gap-case ()
  "Return a gap case: three lists of results, each with the selection it came
from.  PROPERTY is a property= or symbol= run; CONTRACT is a function= run;
MIXED is any results under no selection, and always holds a contract run with
a usable count beside a result without one."
  (let* ((property-selection (%pick (selection-kind-names :property)))
         (property-count (if (eq property-selection :explicit-property)
                             1
                             (%between 0 4))))
    (list :property
          (list :selection property-selection
                :results (loop repeat property-count
                               collect (draw-result
                                        (%pick (result-kind-names :runs :property))
                                        :property)))
          :contract
          (list :selection (%pick (selection-kind-names :contract))
                :results (list (draw-result (%pick (result-kind-names :runs :contract))
                                            :contract)))
          :mixed
          (list :selection nil
                :results
                (shuffle
                 (append
                  (list (draw-result
                         (%pick (loop for name in (result-kind-names :runs :contract)
                                      when (getf (result-kind name) :counted)
                                        collect name))
                         :contract)
                        (draw-result
                         (%pick (loop for name in (result-kind-names)
                                      unless (getf (result-kind name) :counted)
                                        collect name))))
                  (loop repeat (%between 0 3)
                        collect (draw-result (%pick (result-kind-names))))))))))
