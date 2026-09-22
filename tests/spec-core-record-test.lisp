;;;; tests/spec-core-record-test.lisp
;;;;
;;;; The versioned-record layer, exercised with plain plists and no cl-spec in
;;;; the image.  Every function here is pure over data cl-spec would have
;;;; returned, which is what lets these cases cover the revisions this project
;;;; cannot install.

(defpackage #:cl-mcp/tests/spec-core-record-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/spec-core-record
                #:safe-json-integer-p
                #:project-value
                #:project-record
                #:validate-versioned-record
                #:field-availability
                #:project-core-record
                #:*projection-max-depth*
                #:*projection-max-length*)
  (:import-from #:cl-mcp/src/object-registry
                #:*object-registry*
                #:registry-count
                #:lookup-object)
  ;; A bare :import-from makes the renderer a dependency: the JSON cases reach
  ;; its internal %CORE-RECORD-HT, which is what turns a report into JSON.
  (:import-from #:cl-mcp/src/tools/spec-response-builders)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:+required-metadata+
                #:+sentinel-fields+
                #:+value-fields+
                #:with-isolated-object-registry
                #:decimal-string
                #:make-result-record
                #:record-without
                #:record-with
                #:object-field
                #:node-at
                #:object-keys
                #:json-array-p
                #:numbered-items
                #:numbered-text
                #:error-chain-record
                #:chain-container-path))

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
  (testing "the pre-release opaque marker shape is application data, not cl-spec
metadata -- cl-mcp v1 decides availability from the record, never from the
value's shape, so this is externalized like any other value"
    (let ((node (project-value (list :unavailable :reason :opaque-value
                                     :type :hash-table))))
      (ok (eq :value (first node)))
      (ok (stringp (getf (second node) :printed))))))

(deftest a-collected-capture-value-shaped-like-the-old-marker-is-application-data
  ;; The P1 regression this branch was written to close.  cl-spec v1 marks a
  ;; capture's availability in the record around the value, so a domain value
  ;; that happens to be (:UNAVAILABLE :REASON :OPAQUE-VALUE :TYPE ...) is
  ;; :COLLECTED application data.  The projector must not reclassify it from
  ;; its shape and must route it through the ordinary externalization path.
  (let* ((node (project-record
                (list :status :completed :declared '(:diagnostic-before)
                      :values
                      (list (list :name :diagnostic-before
                                  :availability :collected
                                  :value (list :unavailable :reason :opaque-value
                                               :type :hash-table)))
                      :error nil)
                '(:ref :capture-evidence)))
         (values-node (field-of node "values"))
         (entry (first (second values-node))))
    (testing "the record's availability is collected"
      (ok (equal '(:scalar "collected") (field-of entry "availability"))))
    (testing "and its value is externalized application data"
      (let ((value (field-of entry "value")))
        (ok (eq :value (first value)))
        (ok (stringp (getf (second value) :printed)))
        (ok (getf (second value) :object-id)))))
  ;; The same plist under a bare :OPAQUE field (a counterexample value, an
  ;; observation's :VALUE) is application data too.
  (let* ((node (project-record
                (list 'cl-user::a (list :unavailable :reason :opaque-value
                                        :type :hash-table))
                '(:ref :counterexample)))
         (entry (first (second node)))
         (value (field-of entry "value")))
    (ok (eq :value (first value)))
    (ok (getf (second value) :object-id))))

(deftest a-collected-nil-capture-value-is-a-value-not-an-absence
  ;; A captured NIL is a :COLLECTED record with a NIL :VALUE, not an absent
  ;; key.  Its application value is externalized like any other, so the JSON
  ;; carries a value node (printed "NIL") rather than dropping the key.
  (let* ((node (project-record
                (list :values (list (list :name :nothing-before
                                          :availability :collected
                                          :value nil)))
                '(:ref :capture-evidence)))
         (entry (first (second (field-of node "values")))))
    (ok (equal '(:scalar "collected") (field-of entry "availability")))
    (let ((value (field-of entry "value")))
      (ok (eq :value (first value)))
      (ok (equal "NIL" (getf (second value) :printed))))))

(deftest an-unavailable-capture-value-claims-no-value-and-no-object-id
  (let* ((node (project-record
                (list :values (list (list :name :account-before
                                          :availability :unavailable
                                          :reason :opaque-value
                                          :type 'cl-user::account)))
                '(:ref :capture-evidence)))
         (entry (first (second (field-of node "values")))))
    (testing "availability, reason and type are all preserved"
      (ok (equal '(:scalar "unavailable") (field-of entry "availability")))
      (ok (equal '(:scalar "opaque-value") (field-of entry "reason")))
      (ok (equal "ACCOUNT"
                 (getf (second (field-of entry "type")) :name))))
    (testing "there is no application value, so there is no value key at all"
      (ok (null (assoc "value" (second entry) :test #'equal))))))

(deftest a-capture-diagnostic-type-carries-all-three-v1-forms
  (flet ((type-node (type)
           (let* ((node (project-record
                         (list :values
                               (list (list :name :x
                                           :availability :unavailable
                                           :reason :opaque-value
                                           :type type)))
                         '(:ref :capture-evidence)))
                  (entry (first (second (field-of node "values")))))
             (field-of entry "type"))))
    (testing "a named type is ordinary symbol metadata"
      (let ((node (type-node 'cl-user::account)))
        (ok (eq :symbol (first node)))
        (ok (equal "ACCOUNT" (getf (second node) :name)))))
    (testing "an anonymous class is a schema-known object, never externalized"
      (let ((node (type-node '(:kind :anonymous-class
                               :metaclass standard-class))))
        (ok (eq :object (first node)))
        (ok (equal '(:scalar "anonymous-class") (field-of node "kind")))
        (ok (equal "STANDARD-CLASS"
                   (getf (second (field-of node "metaclass")) :name)))
        (dolist (pair (second node))
          (ok (not (eq :value (first (cdr pair))))))))
    (testing "the :unknown fallback survives as a word"
      (ok (equal '(:scalar "unknown") (type-node :unknown))))))

(deftest a-duplicate-plist-key-keeps-its-first-occurrence
  ;; cl-spec's records are open plists read with ordinary plist semantics,
  ;; where GETF answers the first occurrence.  A projection that emitted both
  ;; let a JSON renderer keep the last, so a compatibility alias built with
  ;; GETF and core_result.data could disagree about one record.  One record,
  ;; one interpretation: first occurrence wins, later ones are ignored.
  (let ((node (project-record '(:status :passed :trials 3 :status :failed)
                              '(:object (:status . :leaf) (:trials . :leaf)))))
    (ok (equal '(:scalar "passed") (field-of node "status")))
    (ok (equal '(:scalar 3) (field-of node "trials")))
    (testing "the key appears once in the projected object"
      (ok (= 1 (count "status" (second node)
                      :key #'car :test #'equal)))))
  (testing "an unknown key's duplicate is reported once too"
    (multiple-value-bind (node issues unknown)
        (project-record '(:mystery 1 :mystery 2)
                        '(:object (:status . :leaf)))
      (declare (ignore node issues))
      (ok (equal '("mystery") unknown))))
  (testing "a known server key in a nested object follows the same rule"
    (let* ((node (project-record
                  (list :values (list (list :name :x
                                            :availability :unavailable
                                            :availability :collected
                                            :reason :opaque-value)))
                  '(:ref :capture-evidence)))
           (entry (first (second (field-of node "values")))))
      (ok (equal '(:scalar "unavailable") (field-of entry "availability"))))))

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

(deftest project-core-record-threads-max-chars-into-data
  ;; A caller that asked for a smaller bound must get it inside :DATA too.
  ;; Before this, project-core-record used the projector's default 2000 no
  ;; matter what the caller passed, so one response could publish a 10-char
  ;; compatibility alias beside 2000 chars of core_result.data.
  (let* ((record (append (remove-from-plist-once *passing-result* :counterexample)
                         (list :counterexample
                               (list 'value (make-list 500
                                                       :initial-element
                                                       'padding)))))
         (report (project-core-record record :result-data :max-chars 10))
         (node (field-of (getf report :data) "counterexample"))
         (entry (first (second node)))
         (value (field-of entry "value")))
    (ok (eq :value (first value)))
    (ok (= 10 (length (getf (second value) :printed))))
    (ok (null (getf (second value) :printed-complete)))))

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
  ;; AMOUNT 5) -- not an alist of dotted pairs.  A dotted-pair reader would
  ;; call CAR/CDR on the bare argument-value 5 here and signal a TYPE-ERROR;
  ;; this must pass only against (:PAIRS :OPAQUE), which :COUNTEREXAMPLE now
  ;; uses.
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

(deftest a-signature-reads-the-grammar-cl-spec-actually-builds
  ;; FAILURE-SIGNATURE (cl-spec/src/function-spec.lisp) builds more than the
  ;; two forms this walk used to know, and cl-spec's own validator
  ;; VALID-SIGNATURE-SHAPE-P (cl-spec/src/counterexample.lisp:88-108) is the
  ;; list of them.  Only two carry failure-shape data in their third element.
  (testing "a postcondition failure is not failure-shape data, although it
matches on head and length"
    ;; (:RETURN-VALUE :POSTCONDITION (:POST-FORM 0)) --
    ;; cl-spec/src/function-spec.lisp:1335, asserted literally by
    ;; cl-spec/tests/multiple-values-function-test.lisp:149.  Walked as
    ;; failure shapes, :POST-FORM and 0 each land as a bare atom under an
    ;; (:OBJECT ...) descriptor and each push an :ATOM-FOR-CONTAINER issue --
    ;; so PROJECTION.COMPLETE read false on the commonest contract failure
    ;; there is, which is exactly the signal that instrument exists to give.
    (multiple-value-bind (node issues)
        (project-record '(:return-value :postcondition (:post-form 0))
                        :signature)
      (ok (null issues))
      (ok (eq :array (first node)))
      (ok (equal '(:scalar "postcondition") (second (second node))))))
  (testing "a (values ...) return spec rewrites the head and keeps the shapes"
    ;; cl-spec/src/function-spec.lisp:1400-1406, asserted by
    ;; cl-spec/tests/multiple-values-function-test.lisp:123,160.
    (let* ((node (project-record
                  '(:return-values :return-spec ((:kind :range-failed)))
                  :signature))
           (shapes (third (second node))))
      (ok (equal '(:scalar "return-values") (first (second node))))
      (ok (eq :array (first shapes)))
      (ok (equal '(:scalar "range-failed")
                 (field-of (first (second shapes)) "kind")))))
  (testing "a case wrapper is peeled, and the inner grammar still recognized"
    ;; (:CASE NAME . INNER), cl-spec/src/function-spec.lisp:1572-1574, with
    ;; CASE-SIGNATURE-PARTS (counterexample.lisp:76-84) as cl-spec's own
    ;; unwrapper.  Unpeeled, the nested failure shapes flatten to one
    ;; externalized string.
    (let* ((node (project-record
                  '(:case :a :return-value :return-spec ((:kind :range-failed)))
                  :signature))
           (elements (second node)))
      (ok (equal '(:scalar "case") (first elements)))
      (ok (equal '(:scalar "a") (second elements)))
      (ok (equal '(:scalar "return-value") (third elements)))
      (let ((shapes (fifth elements)))
        (ok (eq :array (first shapes)))
        (ok (equal '(:scalar "range-failed")
                   (field-of (first (second shapes)) "kind"))))))
  (testing "and a case-wrapped postcondition is still not failure-shape data"
    ;; cl-spec/tests/function-cases-test.lisp:829 asserts this literal.
    (multiple-value-bind (node issues)
        (project-record '(:case :a :return-value :postcondition (:post-form 0))
                        :signature)
      (ok (null issues))
      (ok (eq :array (first node)))))
  (testing "every other form cl-spec builds stays a flat array of leaves"
    (dolist (signature '((:missing-condition)
                         (:target-signal cl-user::boom)
                         (:contract-error cl-user::boom)
                         (:state-postcondition 0)
                         (:state-post 0 :contract-error cl-user::boom)
                         (:case-selection :case-guard-error :a)
                         (:property-false)
                         (:property-condition cl-user::boom)))
      (multiple-value-bind (node issues) (project-record signature :signature)
        (ok (eq :array (first node)))
        (ok (null issues))))))

(deftest an-absent-record-is-null-and-an-empty-collection-is-not
  ;; RESULT-DATA (cl-spec/src/property-runner.lisp:255-276) is one
  ;; unconditional APPEND, so every key is emitted on every run, and
  ;; OBSERVATION-DATA answers NIL (property-runner.lisp:216,225) whenever
  ;; there is no failure evidence -- which is every passing run.  Projected as
  ;; {}, that reads as a failure observation whose every field happens to be
  ;; missing, which is not what cl-spec said.
  (multiple-value-bind (report status)
      (project-core-record *passing-result* :result-data)
    (ok (eq :ok status))
    (let ((data (getf report :data)))
      (testing "an absent observation is null, not an empty record"
        (ok (equal '(:scalar nil) (field-of data "failure")))
        (ok (equal '(:scalar nil) (field-of data "shrunk_failure"))))
      (testing "and an empty collection keeps its brackets, because it was
measured empty"
        (ok (equal '(:array nil) (field-of data "counterexample"))))))
  (testing "a NIL explanation inside an observation is null too"
    (let ((node (project-record '(:status :failed :explanation nil)
                                '(:ref :observation))))
      (ok (equal '(:scalar nil) (field-of node "explanation"))))))

(deftest a-seed-is-decimal-text-even-inside-the-safe-range
  ;; Design 6.2.3: a seed is ALWAYS a decimal string, data.seed included.  A
  ;; cl-spec seed is a fixnum reaching 2^62, a rounded seed cannot reproduce a
  ;; run, and data.seed reading 1 beside the top-level alias reading "1" is
  ;; two representations of one fact.  Only the wide seed was pinned, and a
  ;; wide one is text under the plain integer rule anyway -- so the rule this
  ;; field actually needs was never tested.
  (let ((report (project-core-record
                 (append (remove-from-plist-once *passing-result* :seed)
                         '(:seed 1))
                 :result-data)))
    (ok (equal '(:scalar "1") (field-of (getf report :data) "seed"))))
  (testing "and a wide one is still text"
    (let ((report (project-core-record *passing-result* :result-data)))
      (ok (equal '(:scalar "4611686018427387903")
                 (field-of (getf report :data) "seed")))))
  (testing "while an ordinary counter stays a number"
    (let ((report (project-core-record *passing-result* :result-data)))
      (ok (equal '(:scalar 2) (field-of (getf report :data) "trials"))))))

(deftest a-leaf-string-is-bounded-and-the-cut-is-reported
  ;; :CONDITION-REPORT is (PRINC-TO-STRING condition) at four cl-spec sites
  ;; (explain.lisp:212,565,614 and execution.lisp:288) and is declared :LEAF
  ;; in six places, and PROJECT-VALUE's string branch returned it whole --
  ;; MAX-CHARS reached EXTERNALIZE-VALUE and nothing else.
  ;;
  ;; Reported as a PROJECTION.ISSUES entry rather than a sibling flag: :DATA
  ;; is a mirror of cl-spec's record and carries no key cl-mcp added.
  (multiple-value-bind (node issues)
      (project-record (list :kind :type-failed
                            :condition-report (make-string 3000
                                                           :initial-element #\x))
                      '(:ref :error-datum)
                      :max-chars 40)
    (ok (= 40 (length (second (field-of node "condition_report")))))
    (ok (= 1 (length issues)))
    (let ((issue (first issues)))
      (ok (eq :char-limit (getf issue :reason)))
      (ok (equal '("condition_report") (getf issue :path)))
      (ok (= 2960 (getf issue :omitted-items)))
      (ok (eq t (getf issue :omitted-items-exact-p))))))

(deftest the-two-explanation-keys-cl-spec-emits-are-declared
  ;; Both were undeclared, so both landed in UNKNOWN-KEYS with their values
  ;; dropped -- and for a :MISSING-CONDITION failure that key IS the whole
  ;; explanation.
  (testing ":expected carries a real EXPECTED-DESCRIPTOR, not a dropped key"
    ;; (list :expected (expected-descriptor signal-spec)),
    ;; cl-spec/src/function-spec.lisp:1419-1420.
    (multiple-value-bind (node issues unknown)
        (project-record '(:expected (:type cl-user::my-error))
                        '(:ref :explanation))
      (ok (null issues))
      (ok (null unknown))
      (let ((expected (field-of node "expected")))
        (ok (eq :array (first expected)))
        (ok (equal '(:scalar "type") (first (second expected)))))))
  (testing ":post-form names which :post form did not hold"
    ;; (list :post-form index), cl-spec/src/function-spec.lisp:1437-1441.
    (multiple-value-bind (node issues unknown)
        (project-record '(:post-form 2) '(:ref :explanation))
      (ok (null issues))
      (ok (null unknown))
      (ok (equal '(:scalar 2) (field-of node "post_form"))))))

(deftest the-two-error-datum-keys-cl-spec-emits-are-declared
  (testing ":observed-tag is value-derived, so it is externalized not worded"
    ;; cl-spec/src/explain.lisp:618, asserted by
    ;; cl-spec/tests/tagged-union-test.lisp:77.  The tag reader may answer any
    ;; object, so this is :OPAQUE rather than a leaf.
    (multiple-value-bind (node issues unknown)
        (project-record '(:kind :no-branch :observed-tag :circle
                          :known-tags (:square :triangle))
                        '(:ref :error-datum))
      (ok (null issues))
      (ok (null unknown))
      (ok (eq :value (first (field-of node "observed_tag"))))))
  (testing ":first-index is where a duplicate element was first seen"
    ;; cl-spec/src/explain.lisp:327-328, asserted by
    ;; cl-spec/tests/collection-constraints-test.lisp:73.
    (multiple-value-bind (node issues unknown)
        (project-record '(:kind :duplicate-element :first-index 1)
                        '(:ref :error-datum))
      (ok (null issues))
      (ok (null unknown))
      (ok (equal '(:scalar 1) (field-of node "first_index"))))))

(deftest a-declared-boolean-has-two-values-and-neither-is-a-symbol
  ;; T is not a keyword, so PROJECT-VALUE renders it the way it renders every
  ;; other symbol: a measured true arrived as {"package": "COMMON-LISP",
  ;; "name": "T"} beside a measured false that arrived as null -- two shapes
  ;; for one two-valued fact, and the false one indistinguishable from an
  ;; absence.  Declared per field, because NIL is also the empty list and also
  ;; the absence of a phase, and both of those stay JSON null.
  (testing "a digest that covered everything, and one that did not"
    (let ((complete (project-core-record *passing-result* :result-data))
          (partial (project-core-record
                    (append (remove-from-plist-once
                             *passing-result* :definition-digest-complete)
                            '(:definition-digest-complete nil))
                    :result-data)))
      (ok (equal '(:bool t)
                 (field-of (getf complete :data) "definition_digest_complete")))
      (ok (equal '(:bool nil)
                 (field-of (getf partial :data)
                           "definition_digest_complete")))))
  (testing "while a NIL that is an absence stays null"
    ;; :FAILURE-PHASE NIL is an ordinary target observation with no special
    ;; phase, and it is a :LEAF, not a boolean.
    (let ((report (project-core-record *passing-result* :result-data)))
      (ok (equal '(:scalar nil)
                 (field-of (getf report :data) "failure_phase")))))
  (testing "and an explanation's :valid is the other declared boolean"
    (let ((node (project-record '(:valid nil :errors nil)
                                '(:ref :explanation))))
      (ok (equal '(:bool nil) (field-of node "valid"))))))

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

(defun %core (record &rest options)
  "Project RECORD as a result record, in a registry of its own; return
\(values REPORT STATUS REASON)."
  (with-isolated-object-registry
    (apply #'project-core-record record :result-data :expected-record-kind :result
           options)))

(defun %json (report)
  "Render REPORT with the adapter's own renderer, encode it as JSON text, and
parse the text back so that false, null, [] and a missing key stay apart."
  (yason:parse (with-output-to-string (stream)
                 (yason:encode (cl-mcp/src/tools/spec-response-builders::%core-record-ht report)
                               stream))
               :object-as :hash-table :json-arrays-as-vectors t
               :json-booleans-as-symbols t :json-nulls-as-keyword t))

(defun %json-at (table &rest keys)
  "Return (values VALUE PRESENT-P) for KEYS followed from the parsed TABLE."
  (let ((value table) (present t))
    (dolist (key keys (values value present))
      (if (hash-table-p value)
          (multiple-value-setq (value present) (gethash key value))
          (return (values nil nil)))
      (unless present (return (values nil nil))))))

(deftest the-json-reader-keeps-false-null-empty-and-missing-apart
  ;; Every JSON case below leans on this decoder setting, so check it first.
  (let ((table (yason:parse "{\"f\":false,\"n\":null,\"a\":[],\"t\":true}"
                            :object-as :hash-table :json-arrays-as-vectors t
                            :json-booleans-as-symbols t :json-nulls-as-keyword t)))
    (ok (eq 'yason:false (%json-at table "f")) "false is YASON:FALSE")
    (ok (eq :null (%json-at table "n")) "null is :NULL")
    (let ((empty (%json-at table "a")))
      (ok (and (json-array-p empty) (zerop (length empty))) "[] is an empty array"))
    (ok (eq 'yason:true (%json-at table "t")) "true is YASON:TRUE")
    (ok (not (nth-value 1 (%json-at table "missing"))) "a missing key is not present")))

(deftest a-json-string-is-never-taken-for-an-array
  ;; EQUALP compares a string and a vector element by element, so an array
  ;; check built on it would take "" for [] -- and a length check would take
  ;; any non-empty string for a non-empty array.
  (ok (equalp #() "") "EQUALP alone cannot tell [] from \"\"")
  (ng (json-array-p "") "\"\" is not an array")
  (ng (json-array-p "lost") "a non-empty string is not an array")
  (ng (json-array-p nil) "null is not an array")
  (ok (json-array-p (vector)) "[] is an array")
  (ok (json-array-p (vector (make-hash-table))) "[{}] is an array")
  (let ((decoded (yason:parse "{\"s\":\"\",\"a\":[]}"
                              :object-as :hash-table :json-arrays-as-vectors t
                              :json-booleans-as-symbols t :json-nulls-as-keyword t)))
    (ng (json-array-p (%json-at decoded "s")) "a decoded \"\" is not an array")
    (ok (json-array-p (%json-at decoded "a")) "a decoded [] is an array")))

(deftest a-core-record-reaches-json-with-each-role-intact
  (let* ((record (record-without (make-result-record :definition-digest-complete nil)
                                 :failure-phase))
         (json (%json (%core record))))
    (ok (eq 'yason:false (%json-at json "data" "definition_digest_complete"))
        "a boolean NIL is false")
    (ok (eq :null (%json-at json "data" "failure")) "an absent observation is null")
    (let ((counterexample (%json-at json "data" "counterexample")))
      (ok (and (json-array-p counterexample) (zerop (length counterexample)))
          "an empty collection is [], an array and not a string"))
    (let ((unknown (%json-at json "unknown_keys")))
      (ok (and (json-array-p unknown) (zerop (length unknown)))
          "unknown_keys is an empty array"))
    (ok (not (nth-value 1 (%json-at json "data" "failure_phase")))
        "a key missing from the record is missing from data")
    (ok (eq :null (%json-at json "data" "shrink_report"))
        "a sentinel's :NOT-COLLECTED is null in data")
    (ok (equal "not-collected" (%json-at json "field_availability" "shrink_report"))
        "and not-collected in field_availability")
    (ok (equal "absent" (%json-at json "field_availability" "failure_phase"))
        "the missing key is absent in field_availability")
    (ok (equal "7" (%json-at json "data" "seed")) "the seed is text")
    (ok (eql 3 (%json-at json "data" "trials")) "a small count stays a number")
    (ok (eq 'yason:true (%json-at json "schema_supported")) "schema_supported is true")
    (ok (not (nth-value 1 (%json-at json "source")))
        "the raw record kept for the adapter never reaches JSON"))
  (let ((json (%json (%core (make-result-record)))))
    (ok (eq 'yason:true (%json-at json "data" "definition_digest_complete"))
        "a boolean T is true")
    (ok (eq :null (%json-at json "data" "failure_phase"))
        "a present NIL phase is null, and present")))

(deftest seeds-at-the-json-and-generator-boundaries-stay-decimal-text
  ;; 2^53 +/- 1 is where a binary64 consumer starts to round; 2^62 is the
  ;; bound of the seeds cl-spec draws, and cl-spec accepts any non-negative
  ;; integer past it.  JSON's grammar allows every one of these as a number;
  ;; text is for the consumers that would read one into a double.
  (dolist (seed (list 0 1 42 (1- (expt 2 53)) (expt 2 53) (1+ (expt 2 53))
                      (1- (expt 2 62)) (expt 2 62) (1+ (expt 2 64))))
    (let* ((expected (decimal-string seed))
           (report (%core (make-result-record :seed seed :trials 5)))
           (data (getf report :data)))
      (ok (equal (list :scalar expected) (object-field data "seed"))
          (format nil "seed ~A projects as the text ~A" seed expected))
      (ok (equal '(:scalar 5) (object-field data "trials"))
          (format nil "beside seed ~A, trials stays the number 5" seed))
      (ok (equal expected (%json-at (%json report) "data" "seed"))
          (format nil "seed ~A is still the text ~A after JSON" seed expected)))))

(deftest every-sentinel-and-value-field-keeps-its-three-states-apart
  (let ((base (make-result-record)))
    (dolist (position '(:front :back))
      (dolist (key +sentinel-fields+)
        (let ((label (format nil "~(~A~) at the ~(~A~)" key position)))
          (ok (eq :absent (field-availability (record-without base key) key))
              (format nil "~A: a missing key is absent" label))
          (ok (eq :collected (field-availability (record-with base key nil :position position)
                                                 key))
              (format nil "~A: a present NIL is collected" label))
          (ok (eq :not-collected
                  (field-availability (record-with base key :not-collected :position position)
                                      key))
              (format nil "~A: :NOT-COLLECTED is not collected" label))
          (ok (eq :collected (field-availability (record-with base key (list :x 1)
                                                               :position position)
                                                 key))
              (format nil "~A: a value is collected" label))))
      (dolist (key +value-fields+)
        (let ((label (format nil "~(~A~) at the ~(~A~)" key position)))
          (ok (eq :absent (field-availability (record-without base key) key))
              (format nil "~A: a missing key is absent" label))
          (ok (eq :collected (field-availability (record-with base key nil :position position)
                                                 key))
              (format nil "~A: a present NIL is collected" label))
          (ok (eq :collected
                  (field-availability (record-with base key :not-collected :position position)
                                      key))
              (format nil "~A: :NOT-COLLECTED is an ordinary value here" label)))))
    (let* ((not-collected (let ((record base))
                            (dolist (key +sentinel-fields+ record)
                              (setf record (record-with record key :not-collected)))))
           (report (%core not-collected)))
      (dolist (key +sentinel-fields+)
        (let ((json-key (substitute #\_ #\- (string-downcase (symbol-name key)))))
          (multiple-value-bind (child present-p) (object-field (getf report :data) json-key)
            (ok (and present-p (equal '(:scalar nil) child))
                (format nil "~A: :NOT-COLLECTED is a present null in data" json-key)))
          (ok (eq :not-collected (getf (getf report :field-availability) key))
              (format nil "~A: and not-collected in field availability" json-key)))))))

(deftest order-duplicates-and-unknown-keys-leave-known-meanings-alone
  (let* ((base (make-result-record :status :passed :seed 11))
         (reversed (loop with pairs = (loop for (key value) on base by #'cddr
                                            collect (list key value))
                         for pair in (reverse pairs) append (copy-list pair)))
         (duplicated (append (copy-list base) (list :status :failed :trials 99)))
         (extended (append (copy-list base) (list :x-future-alpha 1 :x-future-beta "b")))
         (base-report (%core base)))
    (flet ((same-known-fields-p (report)
             (let ((expected (getf base-report :data))
                   (actual (getf report :data)))
               (and (null (set-exclusive-or (object-keys expected) (object-keys actual)
                                            :test #'equal))
                    (every (lambda (key)
                             (equal (object-field expected key) (object-field actual key)))
                           (object-keys expected))
                    (equal (getf base-report :field-availability)
                           (getf report :field-availability))))))
      (let ((report (%core reversed)))
        (ok (same-known-fields-p report) "reversed pairs: every known field is the same")
        (ok (null (getf report :unknown-keys)) "reversed pairs: nothing unknown"))
      (let ((report (%core duplicated)))
        (ok (same-known-fields-p report) "a later duplicate: the first occurrence wins")
        (ok (null (getf report :unknown-keys)) "a later duplicate is not unknown"))
      (let ((report (%core extended)))
        (ok (same-known-fields-p report) "unknown keys: every known field is the same")
        (ok (equal '("x_future_alpha" "x_future_beta") (getf report :unknown-keys))
            "unknown keys: both are named")
        (ok (getf (getf report :projection) :complete)
            "unknown keys do not make the projection incomplete")
        (ok (getf report :schema-supported) "unknown keys do not unsettle the schema")
        (ok (not (nth-value 1 (object-field (getf report :data) "x_future_alpha")))
            "an unknown key is not guessed into data")))))

(deftest each-limit-cuts-just-past-it-and-says-so
  ;; Every item and every character differs from its neighbours, so a cut that
  ;; kept the wrong part -- the tail, or a reordering -- shows in the content,
  ;; not only in the counts.
  (flet ((issues (report) (getf (getf report :projection) :issues))
         (complete-p (report) (getf (getf report :projection) :complete))
         (prefix-nodes (count)
           ;; A small integer projects as itself: (:SCALAR n).
           (loop for i below count collect (list :scalar i))))
    (testing "length: 39, 40 and 41 items against 40"
      (let ((*projection-max-length* 40))
        (dolist (count '(39 40))
          (let ((report (%core (make-result-record :digest-exclusions (numbered-items count)))))
            (ok (and (complete-p report) (null (issues report))
                     (equal (prefix-nodes count)
                            (second (object-field (getf report :data) "digest_exclusions"))))
                (format nil "~D items: all kept, in order, nothing reported" count))))
        (let* ((report (%core (make-result-record :digest-exclusions (numbered-items 41))))
               (issue (first (issues report))))
          (ok (and (not (complete-p report)) (= 1 (length (issues report)))
                   (equal '("digest_exclusions") (getf issue :path))
                   (eq :length-limit (getf issue :reason))
                   (eql 1 (getf issue :omitted-items)) (getf issue :omitted-items-exact-p))
              "41 items: one exact length cut of 1, reported")
          (ok (equal (prefix-nodes 40)
                     (second (object-field (getf report :data) "digest_exclusions")))
              "41 items: exactly the first 40 are kept, in order"))
        (let* ((report (%core (make-result-record :digest-exclusions (numbered-items 82))))
               (issue (first (issues report))))
          (ok (and (not (getf issue :omitted-items-exact-p))
                   (< (getf issue :omitted-items) 42))
              "82 items: the count of 42 is not claimed, and says it is not exact")
          (ok (equal (prefix-nodes 40)
                     (second (object-field (getf report :data) "digest_exclusions")))
              "82 items: exactly the first 40 are kept, in order"))))
    (testing "chars: 29, 30 and 31 characters against 30"
      (flet ((report-for (text)
               (%core (make-result-record
                       :status :failed
                       :failure (list :status :failed :condition-report text))
                      :max-chars 30)))
        (dolist (count '(29 30))
          (let* ((text (numbered-text count))
                 (report (report-for text)))
            (ok (and (complete-p report) (null (issues report))
                     (equal (list :scalar text)
                            (node-at (getf report :data) '("failure" "condition_report"))))
                (format nil "~D characters: kept whole, nothing reported" count))))
        (let* ((text (numbered-text 31))
               (report (report-for text))
               (issue (first (issues report))))
          (ok (and (not (complete-p report)) (= 1 (length (issues report)))
                   (equal '("failure" "condition_report") (getf issue :path))
                   (eq :char-limit (getf issue :reason))
                   (eql 1 (getf issue :omitted-items)) (getf issue :omitted-items-exact-p))
              "31 characters: one exact character cut of 1, reported")
          (ok (equal (list :scalar (subseq text 0 30))
                     (node-at (getf report :data) '("failure" "condition_report")))
              "31 characters: exactly the first 30 are kept"))))
    (testing "depth: the deepest container at 3, 4 and 5 against 4"
      (let ((*projection-max-depth* 4))
        (let ((report (%core (error-chain-record 3))))
          (ok (and (complete-p report) (null (issues report))
                   (eq :array (first (node-at (getf report :data)
                                              (chain-container-path 3)))))
              "deepest at 3: every container kept, nothing reported"))
        (dolist (deepest '(4 5))
          (let* ((report (%core (error-chain-record deepest)))
                 (issue (first (issues report)))
                 (path (chain-container-path 4)))
            (ok (and (not (complete-p report)) (= 1 (length (issues report)))
                     (eq :depth-limit (getf issue :reason))
                     (equal path (getf issue :path))
                     (eq :value (first (node-at (getf report :data) path))))
                (format nil "deepest at ~D: the container at 4 is cut and reported" deepest))
            (ok (equal '(:scalar "failed") (object-field (getf report :data) "status"))
                (format nil "deepest at ~D: fields outside the chain are whole" deepest))))))))

(deftest validation-names-each-single-cause
  (let ((valid (make-result-record)))
    (ok (eq :ok (validate-versioned-record valid :expected-record-kind :result
                                                 :expected-entity-kind :property))
        "the base record is valid")
    (dolist (key +required-metadata+)
      (ok (eq :malformed (validate-versioned-record (record-without valid key)))
          (format nil "without ~(~A~): malformed" key)))
    (loop for (label record) in (list (list "NIL" nil)
                                      (list "an improper list" (append (copy-list valid) :tail))
                                      (list "an odd-length plist"
                                            (append (copy-list valid) (list :dangling)))
                                      (list "a string indicator" (list* "status" :passed valid)))
          do (ok (eq :malformed (validate-versioned-record record))
                 (format nil "~A: malformed" label)))
    (ok (eq :malformed (validate-versioned-record valid :expected-record-kind :definition))
        "a record kind other than the expected one: malformed")
    (ok (eq :malformed (validate-versioned-record valid :expected-entity-kind :function-spec))
        "an entity kind other than the expected one: malformed")
    (multiple-value-bind (report status) (%core (record-without valid :schema-version))
      (ok (and (null report) (eq :malformed status))
          "a record with no schema version is malformed and not projected"))
    (dolist (version (list 0 2 99 (expt 2 62)))
      (let ((unsupported (record-with valid :schema-version version :position :front)))
        (multiple-value-bind (status reason) (validate-versioned-record unsupported)
          (ok (and (eq :unsupported-schema status) (eql version reason))
              (format nil "version ~D: unsupported, and named" version)))
        (multiple-value-bind (report status) (%core unsupported)
          (ok (and (eq :unsupported-schema status)
                   (eq :collected (getf report :availability))
                   (null (getf report :schema-supported))
                   (eql version (getf report :schema-version))
                   (null (getf report :data))
                   (null (getf report :field-availability)))
              (format nil "version ~D: collected, not understood, and not projected"
                      version)))))))

(deftest projection-registers-opaque-values-in-its-own-registry
  ;; A collected capture value that is a list is externalized with an object
  ;; id.  The id must live in the registry the caller bound, and the global
  ;; one -- where a user's inspect-object ids live -- must be left alone.
  (let ((global-count (registry-count *object-registry*))
        (value (list 1 2 3)))
    (with-isolated-object-registry
      (let* ((record (make-result-record
                      :status :failed
                      :failure (list :status :failed
                                     :state (list :capture
                                                  (list :status :collected
                                                        :declared (list 'before)
                                                        :values (list (list :name 'before
                                                                            :availability
                                                                            :collected
                                                                            :value value)))))))
             (report (project-core-record record :result-data :expected-record-kind :result))
             (node (node-at (getf report :data)
                            '("failure" "state" "capture" "values" 0 "value")))
             (id (getf (second node) :object-id)))
        (ok (eq :value (first node)) "the list is externalized as a value")
        ;; MAKE-RESULT-RECORD copies the failure, so the object the id names
        ;; is the record's own copy of VALUE, not VALUE itself.
        (ok (and id (eq (getf (first (getf (getf (getf (getf record :failure) :state)
                                                 :capture)
                                           :values))
                              :value)
                        (lookup-object id)))
            "its id resolves, in the bound registry, to the record's own object")))
    (ok (= global-count (registry-count *object-registry*))
        "the global registry holds as many objects as before")))
