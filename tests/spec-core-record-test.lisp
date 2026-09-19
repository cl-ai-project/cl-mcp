;;;; tests/spec-core-record-test.lisp
;;;;
;;;; The versioned-record layer, exercised with plain plists and no cl-spec in
;;;; the image.  Every function here is pure over data cl-spec would have
;;;; returned, which is what lets these cases cover the revisions this project
;;;; cannot install.

(defpackage #:cl-mcp/tests/spec-core-record-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-core-record
                #:safe-json-integer-p
                #:project-value
                #:project-record
                #:validate-versioned-record
                #:field-availability
                #:project-core-record
                #:*projection-max-depth*
                #:*projection-max-length*))

(in-package #:cl-mcp/tests/spec-core-record-test)

(deftest safe-json-integer-covers-the-double-mantissa
  (testing "the boundary itself is safe"
    (ok (safe-json-integer-p (1- (expt 2 53))))
    (ok (safe-json-integer-p (- (1- (expt 2 53))))))
  (testing "one past it is not, although it is still a fixnum"
    (ok (not (safe-json-integer-p (expt 2 53))))
    (ok (not (safe-json-integer-p (- (expt 2 53)))))
    ;; The case this rule exists for: a cl-spec seed is a fixnum and is far
    ;; wider than a JSON consumer holds exactly.
    (ok (not (safe-json-integer-p 4611686018427387903)))))

(deftest project-value-tags-each-type-once
  (testing "a keyword is a scalar word, not a symbol node"
    (ok (equal '(:scalar "state-post") (project-value :state-post))))
  (testing "NIL is a scalar null, not the symbol COMMON-LISP::NIL"
    (ok (equal '(:scalar nil) (project-value nil))))
  (testing "any other symbol is a symbol node and keeps its package"
    (let ((node (project-value 'cl-user::widen)))
      (ok (eq :symbol (first node)))
      (ok (equal "WIDEN" (getf (second node) :name)))
      (ok (equal "COMMON-LISP-USER" (getf (second node) :package)))))
  (testing "a string is a scalar"
    (ok (equal '(:scalar "boom") (project-value "boom"))))
  (testing "a safe integer stays a number and a wider one becomes text"
    (ok (equal '(:scalar 42) (project-value 42)))
    (ok (equal '(:scalar "4611686018427387903")
               (project-value 4611686018427387903))))
  (testing "anything else is a value node from externalize-value"
    (let ((node (project-value (make-hash-table))))
      (ok (eq :value (first node)))
      (ok (stringp (getf (second node) :printed)))
      (ok (equal "hash-table" (getf (second node) :type)))))
  (testing "cl-spec's own can-not-freeze-this marker is kept as data, not
externalized -- externalizing it would assign an object id to the marker
list itself rather than to the value cl-spec said it could not freeze"
    (let ((node (project-value (list :unavailable :reason :opaque-value
                                     :type :hash-table))))
      (ok (eq :object (first node)))
      (let ((fields (second node)))
        (ok (equal '(:scalar t)
                   (cdr (assoc "unavailable" fields :test #'equal))))
        (ok (equal '(:scalar "opaque-value")
                   (cdr (assoc "reason" fields :test #'equal))))
        (ok (equal '(:scalar "hash-table")
                   (cdr (assoc "type" fields :test #'equal))))))))

(deftest object-descriptor-projects-only-declared-keys
  (let ((shape '(:object (:kind . :leaf) (:index . :leaf))))
    (multiple-value-bind (node issues unknown)
        (project-record '(:kind :state-postcondition :index 0 :surprise 7) shape)
      (ok (null issues))
      (testing "declared keys are projected under snake_case names"
        (ok (equal '(:scalar "state-postcondition")
                   (cdr (assoc "kind" (second node) :test #'equal))))
        (ok (equal '(:scalar 0)
                   (cdr (assoc "index" (second node) :test #'equal)))))
      (testing "an undeclared key is named and its value is not interpreted"
        (ok (equal '("surprise") unknown))
        (ok (null (assoc "surprise" (second node) :test #'equal)))))))

(deftest object-descriptor-elsewhere-field-is-declared-not-unknown
  ;; :ELSEWHERE says the record declares this key and cl-mcp already
  ;; publishes it outside :DATA (spec-adapter-report's %SPEC-TREE, for the
  ;; six Function Spec keys this exists for) -- it must not land in either
  ;; half of the ordinary two-way split project-record otherwise makes.
  (let ((shape '(:object (:kind . :leaf) (:arguments . :elsewhere))))
    (multiple-value-bind (node issues unknown)
        (project-record '(:kind :function-spec :arguments (1 2) :surprise 7)
                         shape)
      (ok (null issues))
      (testing "a declared :elsewhere key is projected into neither half"
        (ok (null (assoc "arguments" (second node) :test #'equal)))
        (ok (not (member "arguments" unknown :test #'equal))))
      (testing "an ordinary declared key is still projected"
        (ok (equal '(:scalar "function-spec")
                   (cdr (assoc "kind" (second node) :test #'equal)))))
      (testing "an undeclared key beside it still reaches unknown-keys"
        (ok (equal '("surprise") unknown))))))

(deftest a-keyword-list-is-an-array-not-an-object
  ;; §6.2.1's own counterexample.  (:AT-LEAST :AT-MOST) is two case names;
  ;; read as a plist it becomes {"at-least": "at-most"}, a relation cl-spec
  ;; never declared.
  (let ((node (project-record '(:at-least :at-most) :word-list)))
    (ok (eq :array (first node)))
    (ok (equal '((:scalar "at-least") (:scalar "at-most")) (second node)))))

(deftest actual-and-expected-are-projected-differently
  ;; The same cons under two keys of one error datum: :ACTUAL is a value from
  ;; the code under test, :EXPECTED is a descriptor cl-spec built.
  (let* ((shape '(:object (:actual . :opaque) (:expected . (:object (:kind . :leaf)))))
         (node (project-record '(:actual (1 2 3) :expected (:kind :range)) shape))
         (fields (second node)))
    (testing ":actual is externalized, never structured"
      (let ((actual (cdr (assoc "actual" fields :test #'equal))))
        (ok (eq :value (first actual)))
        (ok (search "1 2 3" (getf (second actual) :printed)))))
    (testing ":expected keeps its structure"
      (let ((expected (cdr (assoc "expected" fields :test #'equal))))
        (ok (eq :object (first expected)))
        (ok (equal '(:scalar "range")
                   (cdr (assoc "kind" (second expected) :test #'equal))))))))

(deftest an-alist-of-capture-values-becomes-name-value-pairs
  ;; Measured shape: ((BALANCE-BEFORE . 30) (ID-BEFORE . 7)).  Dotted pairs are
  ;; not proper lists, so an array rule has nothing to say about them.
  (let* ((node (project-record (list (cons 'cl-user::balance-before 30))
                               '(:alist :opaque)))
         (entry (first (second node)))
         (fields (second entry)))
    (ok (eq :array (first node)))
    (ok (equal "BALANCE-BEFORE"
               (getf (second (cdr (assoc "name" fields :test #'equal))) :name)))
    (ok (eq :value (first (cdr (assoc "value" fields :test #'equal)))))))

(deftest an-opaque-value-marker-survives-the-walk-path-not-only-project-value
  ;; The bug this closes: WALK's :OPAQUE branch used to hand a captured value
  ;; straight to EXTERNALIZE-VALUE, bypassing PROJECT-VALUE's own marker case
  ;; entirely -- so a capture value cl-spec could not freeze still got an
  ;; object id, and its type read "cons" (the marker list's own type) rather
  ;; than the type cl-spec named.  A test that only calls PROJECT-VALUE
  ;; directly cannot see this: it never goes through WALK/:OPAQUE at all.
  (let* ((node (project-record
                (list :status :ok :declared '(:balance-before)
                      :values (list (cons :balance-before
                                          (list :unavailable :reason :opaque-value
                                                :type :hash-table))))
                '(:ref :capture-evidence)))
         (values-node (cdr (assoc "values" (second node) :test #'equal)))
         (entry (first (second values-node)))
         (value (cdr (assoc "value" (second entry) :test #'equal))))
    (testing "the marker is kept as an object, not externalized"
      (ok (eq :object (first value)))
      (let ((fields (second value)))
        (ok (equal '(:scalar t)
                   (cdr (assoc "unavailable" fields :test #'equal))))
        (ok (equal '(:scalar "opaque-value")
                   (cdr (assoc "reason" fields :test #'equal))))
        (ok (equal '(:scalar "hash-table")
                   (cdr (assoc "type" fields :test #'equal))))))
    (testing "no field of the marker is an externalized-value node"
      ;; Only a (:VALUE plist) node ever carries :OBJECT-ID.  Every field
      ;; here is a plain :SCALAR, so none of them could hold one.
      (dolist (pair (second value))
        (ok (not (eq :value (first (cdr pair)))))))))

(deftest a-second-opaque-field-keeps-the-marker-too
  ;; Generality check: :COUNTEREXAMPLE is another (:PAIRS :OPAQUE) field, a
  ;; sibling to capture values rather than a special case wired in on its own.
  ;; The fixture is a flat plist, not an alist -- see
  ;; A-COUNTEREXAMPLE-PLIST-PROJECTS-AS-NAME-VALUE-PAIRS for why.
  (let* ((node (project-record
                (list 'cl-user::a (list :unavailable :reason :opaque-value
                                        :type :hash-table))
                '(:ref :counterexample)))
         (entry (first (second node)))
         (value (cdr (assoc "value" (second entry) :test #'equal))))
    (ok (eq :object (first value)))
    (ok (equal '(:scalar "hash-table")
               (cdr (assoc "type" (second value) :test #'equal))))))

(deftest a-length-cut-is-reported-not-hidden
  (let ((*projection-max-length* 2))
    (multiple-value-bind (node issues)
        (project-record '(:a :b :c :d :e) :word-list :path '("failure" "cases"))
      (ok (= 2 (length (second node))))
      (ok (= 1 (length issues)))
      (let ((issue (first issues)))
        (ok (equal '("failure" "cases") (getf issue :path)))
        (ok (eq :length-limit (getf issue :reason)))
        ;; Five errors must not arrive as two that look complete.
        (ok (eql 3 (getf issue :omitted-items)))))))

(deftest a-depth-cut-leaves-the-standard-value-node
  (let ((*projection-max-depth* 1)
        (shape '(:object (:inner . (:object (:deeper . :leaf))))))
    (multiple-value-bind (node issues)
        (project-record '(:inner (:deeper 1)) shape)
      (let ((inner (cdr (assoc "inner" (second node) :test #'equal))))
        ;; Not a marker invented for this: the externalized-value plist is what
        ;; already represents any Lisp value everywhere else in the record.
        (ok (eq :value (first inner))))
      (ok (eq :depth-limit (getf (first issues) :reason)))
      (ok (equal '("inner") (getf (first issues) :path))))))

(deftest a-length-cut-on-an-object-never-fabricates-a-value
  ;; An odd number of surviving pairs used to let a raw-element cut fall mid
  ;; pair, leaving a key whose real value was dropped and silently reporting
  ;; NIL for it instead -- a fabricated measurement, the one thing this
  ;; module must not produce.
  (let ((*projection-max-length* 3)
        (shape '(:object (:a . :leaf) (:b . :leaf) (:c . :leaf) (:d . :leaf))))
    (multiple-value-bind (node issues)
        (project-record '(:a 1 :b 2 :c 3 :d 4) shape)
      (let ((fields (second node)))
        (testing "only whole pairs are kept, never a key with a fabricated value"
          (ok (equal '(:scalar 1) (cdr (assoc "a" fields :test #'equal))))
          (ok (equal '(:scalar 2) (cdr (assoc "b" fields :test #'equal))))
          (ok (equal '(:scalar 3) (cdr (assoc "c" fields :test #'equal))))
          (ok (null (assoc "d" fields :test #'equal))))
        (testing "the cut is reported by entries, not raw plist elements"
          (ok (= 1 (length issues)))
          (ok (eq :length-limit (getf (first issues) :reason)))
          (ok (eql 1 (getf (first issues) :omitted-items)))
          (ok (getf (first issues) :omitted-items-exact-p)))))))

(deftest a-length-cut-on-an-alist-drops-whole-entries
  (let ((*projection-max-length* 2))
    (multiple-value-bind (node issues)
        (project-record (list (cons 'cl-user::a 1) (cons 'cl-user::b 2)
                               (cons 'cl-user::c 3))
                         '(:alist :opaque))
      (ok (= 2 (length (second node))))
      (ok (= 1 (length issues)))
      (ok (eq :length-limit (getf (first issues) :reason)))
      (ok (eql 1 (getf (first issues) :omitted-items)))
      (ok (getf (first issues) :omitted-items-exact-p)))))

(deftest a-circular-list-is-projected-without-hanging
  ;; LENGTH loops forever on a circular list; this must never call it on
  ;; untrusted input.  A regression here should fail loudly, not wedge the
  ;; suite, hence the timeout.
  (let ((circular (list 1 2 3)))
    (setf (cdddr circular) circular)
    (multiple-value-bind (node issues)
        (sb-ext:with-timeout 5 (project-record circular :word-list))
      (ok (eq :array (first node)))
      (ok (= *projection-max-length* (length (second node))))
      (testing "the drop count is honest about only what it counted"
        (ok (eql (1+ *projection-max-length*) (getf (first issues) :omitted-items)))
        (ok (not (getf (first issues) :omitted-items-exact-p)))))))

(deftest max-chars-reaches-a-nested-leaf
  ;; The bound must reach every PROJECT-VALUE/EXTERNALIZE-VALUE call inside
  ;; WALK's own recursion, not just a value project-record is handed
  ;; directly -- this leaf is two OBJECT layers down.
  (let* ((shape '(:object (:outer . (:object (:inner . :leaf)))))
         (many-numbers (loop for i below 500 collect i))
         (node (project-record (list :outer (list :inner many-numbers)) shape
                                :max-chars 5))
         (outer (cdr (assoc "outer" (second node) :test #'equal)))
         (inner (cdr (assoc "inner" (second outer) :test #'equal))))
    (ok (eq :value (first inner)))
    (ok (= 5 (length (getf (second inner) :printed))))
    (ok (null (getf (second inner) :printed-complete)))))

(defun remove-from-plist-once (plist key)
  "Return PLIST without KEY and its value."
  (loop for (indicator value) on plist by #'cddr
        unless (eq indicator key)
          append (list indicator value)))

(defparameter *v1-metadata*
  '(:schema-version 1 :record-kind :result :entity-kind :function-spec
    :definition-digest "abc" :definition-digest-complete t
    :definition-digest-covers :declaration-and-registered-dependencies
    :capabilities (:generation :available :shrinking :none))
  "A minimal well-formed v1 result envelope, shared by the validation cases.")

(deftest a-well-formed-v1-record-validates
  (ok (eq :ok (validate-versioned-record *v1-metadata*
                                         :expected-record-kind :result))))

(deftest a-future-schema-is-unsupported-not-malformed
  (let ((record (list* :schema-version 2 (cddr *v1-metadata*))))
    (multiple-value-bind (status reason) (validate-versioned-record record)
      (ok (eq :unsupported-schema status))
      ;; The version travels so the response can name it rather than saying
      ;; only that something was wrong.
      (ok (eql 2 reason)))))

(deftest a-v1-record-missing-required-metadata-is-malformed
  ;; schema-info declares these seven required.  Continuing with
  ;; field_availability :absent would treat a broken record as an old one.
  (let ((record (remove-from-plist-once *v1-metadata* :record-kind)))
    (ok (eq :malformed (validate-versioned-record record)))))

(deftest a-record-kind-mismatch-is-malformed
  (ok (eq :malformed
          (validate-versioned-record *v1-metadata*
                                     :expected-record-kind :definition))))

(deftest nil-and-non-plists-are-malformed
  (ok (eq :malformed (validate-versioned-record nil)))
  (ok (eq :malformed (validate-versioned-record '(:schema-version))))
  (ok (eq :malformed (validate-versioned-record '("not" "a" "plist" 1)))))

(deftest present-nil-is-not-absent
  ;; The most important regression in this file.  GETF answers NIL for both.
  (let ((present (append *v1-metadata* '(:failure-phase nil)))
        (missing *v1-metadata*))
    (ok (eq :collected (field-availability present :failure-phase)))
    (ok (eq :absent (field-availability missing :failure-phase)))))

(deftest not-collected-is-a-sentinel-only-where-the-schema-says-so
  (testing "a top-level report field uses it as an availability sentinel"
    (ok (eq :not-collected
            (field-availability '(:shrink-report :not-collected) :shrink-report))))
  (testing "a field whose :NOT-COLLECTED is data keeps it"
    ;; failure.outcome :NOT-COLLECTED means the Function Spec target was never
    ;; called, and provenance records it for an item nobody collected.  Neither
    ;; is an availability marker.
    (ok (eq :collected (field-availability '(:outcome :not-collected) :outcome)))
    (ok (eq :collected
            (field-availability '(:target-revision :not-collected)
                                :target-revision)))))

(defparameter *passing-result*
  (append *v1-metadata*
          '(:name cl-user::widen :status :passed :trials 2 :budget 2
            :rejected 0 :seed 4611686018427387903 :profile :normal
            :options nil :counterexample nil :shrunk-counterexample nil
            :shrunk-outcome nil :shrink-report :not-collected
            :generation-report (:scope :request :termination :completed
                                :attempts 0 :rejections 0)
            :failure-phase nil :failure-reason nil
            :case-report (:selection :exclusive :unit :normal-trials
                          :declared-cases (:success :insufficient)
                          :cases ((:name :success :documentation nil
                                   :called 2 :passed 2 :failed 0 :error 0)
                                  (:name :insufficient :documentation nil
                                   :called 0 :passed 0 :failed 0 :error 0))
                          :case-selection-errors 0 :capture-errors 0
                          :never-called (:insufficient))
            :failure nil :shrunk-failure nil :elapsed 0.005))
  "A measured v1 result whose second case was never reached.")

(defun field-of (node key)
  "Return the child NODE holds under the JSON key KEY."
  (cdr (assoc key (second node) :test #'equal)))

(deftest a-result-record-projects-its-whole-envelope
  (multiple-value-bind (report status) (project-core-record *passing-result*
                                                            :result-data)
    (ok (eq :ok status))
    (ok (eq :collected (getf report :availability)))
    (ok (getf report :schema-supported))
    (testing "the envelope keys are in data, not only in the core_schema alias"
      (let ((data (getf report :data)))
        (ok (equal '(:scalar 1) (field-of data "schema_version")))
        (ok (equal '(:scalar "result") (field-of data "record_kind")))
        (ok (equal '(:scalar "abc") (field-of data "definition_digest")))))
    (testing "a seed is text even here"
      (ok (equal '(:scalar "4611686018427387903")
                 (field-of (getf report :data) "seed"))))
    (testing "the never-called case survives as a word"
      (let* ((report-node (field-of (getf report :data) "case_report"))
             (never (field-of report-node "never_called")))
        (ok (equal '((:scalar "insufficient")) (second never)))))
    (testing "an uncollected report is availability, not a projected value"
      (ok (eq :not-collected
              (getf (getf report :field-availability) :shrink-report)))
      (ok (eq :collected
              (getf (getf report :field-availability) :generation-report))))
    (testing "a present NIL is collected"
      (ok (eq :collected
              (getf (getf report :field-availability) :failure-phase))))))

(deftest an-unsupported-schema-projects-nothing
  (multiple-value-bind (report status)
      (project-core-record (list* :schema-version 2 (cddr *passing-result*))
                           :result-data)
    (ok (eq :unsupported-schema status))
    (ok (not (getf report :schema-supported)))
    (ok (eql 2 (getf report :schema-version)))
    ;; No v1 field rules may run over a record this adapter cannot read.
    (ok (null (getf report :data)))))

(deftest a-malformed-record-is-not-projected-at-all
  (multiple-value-bind (report status reason)
      (project-core-record '(:schema-version 1 :record-kind :result) :result-data)
    (ok (null report))
    (ok (eq :malformed status))
    (ok (search "required metadata" reason))))

(deftest an-outcome-keeps-its-kind-and-values
  ;; core_result.data is a pure mirror of cl-spec's own record: the field
  ;; stays "outcome", cl-spec's own name, not a cl-mcp word for it.  The
  ;; shape's internal name in *RECORD-SHAPES* is still :TARGET-OUTCOME --
  ;; that is this project's name for the shape, never a published key.
  (let* ((observation '(:arguments (2 10) :status :failed :reason :missing-condition
                        :signature (:missing-condition) :explanation nil
                        :outcome (:kind :returned :values (0)) :value 0
                        :case :insufficient :condition-report nil))
         (node (project-record observation '(:ref :observation)))
         (outcome (field-of node "outcome")))
    (ok (equal '(:scalar "returned") (field-of outcome "kind")))
    (ok (eq :array (first (field-of outcome "values"))))))

(deftest a-counterexample-plist-projects-as-name-value-pairs
  ;; The regression this guards: cl-spec's NAME-ARGUMENTS (measured via
  ;; PROPERTY-NAMED-ARGUMENTS in cl-spec/tests/rest-function-test.lisp) builds
  ;; a flat {variable value} PLIST for a counterexample -- (BALANCE 5
  ;; AMOUNT 5) -- not an alist of dotted pairs.  A descriptor of
  ;; (:ALIST :OPAQUE) calls CAR/CDR on the bare argument-value 5 here and
  ;; signals a TYPE-ERROR; this must fail against that descriptor and pass
  ;; only against (:PAIRS :OPAQUE), which :COUNTEREXAMPLE now uses.
  (let* ((record (append (list :counterexample '(balance 5 amount 5))
                         (remove-from-plist-once *passing-result*
                                                 :counterexample)))
         (node (project-record record '(:ref :result-data)))
         (counterexample (field-of node "counterexample")))
    (ok (eq :array (first counterexample)))
    (let ((entries (second counterexample)))
      (ok (= 2 (length entries)))
      (testing "each entry pairs the variable's name with its value"
        (ok (equal "BALANCE"
                   (getf (second (field-of (first entries) "name")) :name)))
        (ok (equal "AMOUNT"
                   (getf (second (field-of (second entries) "name")) :name)))
        ;; The value side is :OPAQUE, the same descriptor CAPTURE-EVIDENCE's
        ;; :VALUES uses -- EXTERNALIZE-VALUE's own {printed, type, object_id}
        ;; shape, matching what %NAMED-VALUES already renders for the alias
        ;; beside this field, not PROJECT-VALUE's bare (:SCALAR n).
        (let ((value-node (field-of (first entries) "value")))
          (ok (eq :value (first value-node)))
          (ok (equal "5" (getf (second value-node) :printed)))
          (ok (equal "integer" (getf (second value-node) :type))))))))

(deftest a-not-collected-outcome-does-not-crash-the-walk
  ;; A second regression, found while auditing every :OPAQUE-adjacent shape
  ;; entry per the review that caught the counterexample bug above, and
  ;; confirmed against a real cl-spec run
  ;; (CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE::CLAMP-IS-WRONG-ON-PURPOSE): a plain
  ;; property's failing trial-observation carries :OUTCOME :NOT-COLLECTED --
  ;; OBSERVED-OUTCOME-DATA's own answer for "the target was never classified"
  ;; -- not only a function-spec's capture/case-selection error.  :OUTCOME is
  ;; deliberately excluded from +SENTINEL-FIELDS+ (see its docstring: this is
  ;; real data, not an absence to blank out), so the guard has to live in
  ;; WALK itself: a container descriptor (:OBJECT here, via :TARGET-OUTCOME)
  ;; paired with a bare atom must project the atom, not call CAR/CDR/NTHCDR
  ;; on it.
  (let ((observation '(:arguments (4) :status :failed :reason :predicate-false
                       :signature (:property-false) :explanation nil
                       :outcome :not-collected :value nil
                       :case nil :condition-report nil)))
    (multiple-value-bind (node issues) (project-record observation '(:ref :observation))
      (let ((outcome (field-of node "outcome")))
        (ok (equal '(:scalar "not-collected") outcome)))
      (testing ":not-collected is a documented cl-spec value, not a shape miss"
        ;; The other half of this guard, in AN-UNPREDICTED-ATOM-..., pushes an
        ;; :atom-for-container issue for an atom the descriptor did not
        ;; predict.  :not-collected is predicted -- recording an issue for it
        ;; would make projection.complete read false on every run whose
        ;; target was never called, which is ordinary, not a loss.
        (ok (null issues))))))

(deftest an-unpredicted-atom-under-a-container-is-recorded-as-an-issue
  ;; The other half of the guard above.  :NOT-COLLECTED is the one atom
  ;; cl-spec documents landing under a container descriptor; any other atom
  ;; there is a shape this descriptor did not predict.  WALK still projects
  ;; it as a scalar rather than crashing on CAR/CDR/NTHCDR of something that
  ;; was never a list, but a silent substitution here is exactly the
  ;; mechanism that hid the :EXPECTED and :COUNTEREXAMPLE shape bugs, so this
  ;; one must be recorded in ISSUES rather than degrading quietly.
  (let ((observation '(:arguments (4) :status :failed :reason :predicate-false
                       :signature (:property-false) :explanation nil
                       :outcome :some-unpredicted-atom :value nil
                       :case nil :condition-report nil)))
    (multiple-value-bind (node issues) (project-record observation '(:ref :observation))
      (testing "the atom is still projected, never crashed on"
        (ok (equal '(:scalar "some-unpredicted-atom") (field-of node "outcome"))))
      (testing "but this time it is recorded as an issue, not silently absorbed"
        (ok (= 1 (length issues)))
        (let ((issue (first issues)))
          (ok (eq :atom-for-container (getf issue :reason)))
          (ok (equal '("outcome") (getf issue :path))))))))

(deftest a-signature-keeps-its-shapes-inside-an-array
  (let ((node (project-record '(:return-value :return-spec ((:kind :range-failed)))
                              :signature)))
    (ok (eq :array (first node)))
    (ok (equal '(:scalar "return-value") (first (second node))))
    (testing "the trailing failure shapes are objects, not flattened words"
      (let ((shapes (third (second node))))
        (ok (eq :array (first shapes)))
        (ok (equal '(:scalar "range-failed")
                   (field-of (first (second shapes)) "kind")))))))

(deftest expected-descriptor-is-a-recursive-array-not-a-plist
  ;; EXPECTED-DESCRIPTOR builds a flat, positionally tagged list for most
  ;; spec kinds -- the leading keyword is a tag, not a key.  Read as
  ;; (:OBJECT ...), (:RANGE :MIN 0 :MAX 100) desyncs at :RANGE -> :MIN, then
  ;; hands the integer 0 to %JSON-KEY as a key and signals a TYPE-ERROR --
  ;; the crash every WIDEN test whose :RETURNS is a range spec hit under the
  ;; old declaration.  Each case here must fail against that old (:OBJECT
  ;; (:kind . :leaf) ...) descriptor -- the range one by crashing, so this
  ;; asserts on the projected value rather than on the absence of a crash.
  (testing "a range spec stays a flat array, not a desynced plist"
    (let* ((node (project-record '(:kind :type-failed :expected (:range :min 0 :max 100))
                                 '(:ref :error-datum)))
           (expected (field-of node "expected")))
      (ok (eq :array (first expected)))
      (ok (equal '((:scalar "range") (:scalar "min") (:scalar 0)
                   (:scalar "max") (:scalar 100))
                 (second expected)))))
  (testing "a nested descriptor inside :and recurses, staying structured"
    (let* ((node (project-record '(:kind :type-failed
                                    :expected (:and (:type integer)
                                                     (:range :min 0 :max 100)))
                                 '(:ref :error-datum)))
           (expected (field-of node "expected"))
           (elements (second expected)))
      (ok (eq :array (first expected)))
      (ok (equal '(:scalar "and") (first elements)))
      (testing "the :type child is its own array, not externalized text"
        (let ((type-child (second elements)))
          (ok (eq :array (first type-child)))
          (ok (equal '(:scalar "type") (first (second type-child))))
          (ok (eq :symbol (first (second (second type-child)))))))
      (testing "the :range child recurses the same way, sibling to :type"
        (let ((range-child (third elements)))
          (ok (eq :array (first range-child)))
          (ok (equal '((:scalar "range") (:scalar "min") (:scalar 0)
                       (:scalar "max") (:scalar 100))
                     (second range-child)))))))
  (testing "a :kind-keyed form becomes an array too, by design"
    (let* ((node (project-record '(:kind :type-failed
                                    :expected (:kind :plist :closed nil :fields nil))
                                 '(:ref :error-datum)))
           (expected (field-of node "expected")))
      (ok (eq :array (first expected)))
      (ok (equal '((:scalar "kind") (:scalar "plist") (:scalar "closed")
                   (:scalar nil) (:scalar "fields") (:scalar nil))
                 (second expected))))))
