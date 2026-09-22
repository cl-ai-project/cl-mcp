;;;; tests/core-record-specs-test.lisp
;;;;
;;;; Records from a real cl-spec, carried to JSON.  Opt-in: it needs cl-spec
;;;; with the check-it backend, and it is not in tests.lisp.
;;;; scripts/check-specs.lisp's self-test runs it, and there a missing
;;;; dependency, a missing declaration or an empty run is a failure, never a
;;;; skip.
;;;;
;;;; Four declarations of this file's own -- a property that holds and one that
;;;; does not, a Function Spec that holds and one that does not -- are
;;;; registered in a cl-spec registry made for them, never in the bundle's.
;;;; Each is run once with a fixed seed.  Its RESULT-DATA record then goes
;;;; through PROJECT-CORE-RECORD, the adapter's own core-record renderer, JSON
;;;; encoding and a decoder that keeps false, null, [] and a missing key apart.
;;;; A few fields are compared with what the record itself says and with
;;;; cl-spec's contract, never with a second projection of the record.
;;;; :ELAPSED and the digest's value are left alone: they differ from run to
;;;; run and say nothing about fidelity.

(defpackage #:cl-mcp/tests/core-record-specs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-spec/main
                #:*registry*
                #:make-hash-table-registry
                #:defproperty
                #:defspec-function
                #:find-property
                #:find-function-spec
                #:run-property
                #:check-function
                #:result-data
                #:schema-info)
  ;; A bare :import-from declares the check-it backend as a dependency.
  (:import-from #:cl-spec/src/backends/check-it)
  (:import-from #:cl-mcp/src/spec-core-record
                #:project-core-record)
  ;; And the renderer, whose internal %CORE-RECORD-HT turns a report into the
  ;; data an MCP response encodes.
  (:import-from #:cl-mcp/src/tools/spec-response-builders)
  (:import-from #:cl-mcp/specs/core-record-fixtures
                #:+required-metadata+
                #:+sentinel-fields+
                #:with-isolated-object-registry
                #:decimal-string
                #:json-array-p))

(in-package #:cl-mcp/tests/core-record-specs-test)

;;; ------------------------------------------------------------------------
;;; The fixtures

(defun %twice (x)
  "Return twice X: the function the holding declarations are about."
  (* 2 x))

(defun %twice-plus-one (x)
  "Return twice X plus one: wrong on purpose, for the declarations that fail."
  (1+ (* 2 x)))

(defparameter +fixture-seed+ (1+ (expt 2 53))
  "The seed every fixture run uses: the first integer a binary64 consumer
cannot hold, so the record carries a seed that would round as a JSON number.")

(defun %fixture-registry ()
  "Return a fresh cl-spec registry holding this file's four declarations and
nothing else."
  (let ((registry (make-hash-table-registry)))
    (let ((*registry* registry))
      (defproperty twice-is-even ((x (range integer 0 1000)))
        "Twice anything is even."
        (evenp (%twice x)))
      (defproperty twice-plus-one-is-even ((x (range integer 0 1000)))
        "Wrong on purpose: twice anything plus one is odd."
        (evenp (%twice-plus-one x)))
      (defspec-function %twice
        "Returns twice its argument."
        (:args (x (range integer 0 1000)))
        (:returns integer)
        (:post (= (%twice x) (* 2 x))))
      (defspec-function %twice-plus-one
        "Wrong on purpose: claims to return twice its argument."
        (:args (x (range integer 0 1000)))
        (:returns integer)
        (:post (= (%twice-plus-one x) (* 2 x)))))
    registry))

(defparameter +runs+
  '((:property twice-is-even :passed)
    (:property twice-plus-one-is-even :failed)
    (:function-spec %twice :passed)
    (:function-spec %twice-plus-one :failed))
  "Each fixture run: the kind of declaration, its name, and the status it must
come back with.")

(defun %run (registry kind name)
  "Run the declaration NAME of KIND from REGISTRY once, with the fixed seed,
and return its versioned result record."
  (result-data
   (ecase kind
     (:property (run-property name :seed +fixture-seed+ :registry registry))
     (:function-spec (check-function name :seed +fixture-seed+ :trials 20
                                          :registry registry)))))

;;; ------------------------------------------------------------------------
;;; JSON, kept apart

(defun %json (report)
  "Render REPORT with the adapter's own renderer, encode it as JSON text, and
parse it back with false, null, [] and a missing key kept apart."
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

(defun %record-value (record key)
  "Return (values VALUE PRESENT-P) for KEY in RECORD, walking its pairs."
  (loop for (indicator value) on record by #'cddr
        when (eq indicator key) return (values value t)
        finally (return (values nil nil))))

(defun %json-key (key)
  "Return KEY as the snake_case JSON key cl-mcp publishes."
  (substitute #\_ #\- (string-downcase (symbol-name key))))

;;; ------------------------------------------------------------------------
;;; Tests

(deftest the-decoder-keeps-false-null-empty-and-missing-apart
  (let ((table (yason:parse "{\"f\":false,\"n\":null,\"a\":[]}"
                            :object-as :hash-table :json-arrays-as-vectors t
                            :json-booleans-as-symbols t :json-nulls-as-keyword t)))
    (ok (eq 'yason:false (%json-at table "f")) "false")
    (ok (eq :null (%json-at table "n")) "null")
    (let ((empty (%json-at table "a")))
      (ok (and (json-array-p empty) (zerop (length empty))) "[]"))
    (ok (not (nth-value 1 (%json-at table "missing"))) "missing")))

(deftest the-restated-contract-matches-this-cl-spec
  (ok (null (set-exclusive-or +required-metadata+
                              (getf (schema-info) :required-metadata)))
      "the required metadata restated for the properties is cl-spec's own"))

(deftest real-records-reach-json-with-their-meaning
  (let ((registry (%fixture-registry)))
    (testing "the fixture registry holds exactly the four declarations"
      (ok (find-property 'twice-is-even registry) "twice-is-even")
      (ok (find-property 'twice-plus-one-is-even registry) "twice-plus-one-is-even")
      (ok (find-function-spec '%twice registry) "%twice")
      (ok (find-function-spec '%twice-plus-one registry) "%twice-plus-one"))
    (loop for (kind name expected-status) in +runs+
          do (let* ((label (format nil "~(~A ~A~)" kind name))
                    (record (%run registry kind name))
                    (report (with-isolated-object-registry
                              (project-core-record record :result-data
                                                   :expected-record-kind :result
                                                   :expected-entity-kind kind)))
                    (json (%json report)))
               (testing label
                 (ok (eq expected-status (%record-value record :status))
                     (format nil "~A: cl-spec says ~(~A~)" label expected-status))
                 (ok (eql +fixture-seed+ (%record-value record :seed))
                     (format nil "~A: the record carries the seed it ran with" label))
                 (ok (equal "collected" (%json-at json "availability"))
                     (format nil "~A: collected" label))
                 (ok (eq 'yason:true (%json-at json "schema_supported"))
                     (format nil "~A: schema supported" label))
                 (ok (eq 'yason:true (%json-at json "projection" "complete"))
                     (format nil "~A: projected whole" label))
                 (let ((unknown (%json-at json "unknown_keys")))
                   (ok (and (json-array-p unknown) (zerop (length unknown)))
                       (format nil "~A: no key this adapter does not know" label)))
                 (ok (not (nth-value 1 (%json-at json "source")))
                     (format nil "~A: the raw record stays inside the adapter" label))
                 (ok (equal "result" (%json-at json "data" "record_kind"))
                     (format nil "~A: a result record" label))
                 ;; A keyword value keeps its hyphens; only keys are snake_case.
                 (ok (equal (string-downcase (symbol-name kind))
                            (%json-at json "data" "entity_kind"))
                     (format nil "~A: of its own entity kind" label))
                 (ok (equal (string-downcase (symbol-name expected-status))
                            (%json-at json "data" "status"))
                     (format nil "~A: with its status" label))
                 (ok (equal (decimal-string +fixture-seed+) (%json-at json "data" "seed"))
                     (format nil "~A: the seed is its decimal text" label))
                 (ok (eq (if (%record-value record :definition-digest-complete)
                             'yason:true
                             'yason:false)
                         (%json-at json "data" "definition_digest_complete"))
                     (format nil "~A: digest completeness is a boolean" label))
                 (dolist (key +sentinel-fields+)
                   (multiple-value-bind (value present-p) (%record-value record key)
                     (ok (equal (cond ((not present-p) "absent")
                                      ((eq :not-collected value) "not-collected")
                                      (t "collected"))
                                (%json-at json "field_availability" (%json-key key)))
                         (format nil "~A: ~A's availability" label (%json-key key)))))
                 (if (eq :passed expected-status)
                     (progn
                       (ok (eq :null (%json-at json "data" "failure"))
                           (format nil "~A: no failure, as null" label))
                       (let ((counterexample (%json-at json "data" "counterexample")))
                         (ok (and (json-array-p counterexample)
                                  (zerop (length counterexample)))
                             (format nil "~A: no counterexample, as the array []" label))))
                     (progn
                       (ok (hash-table-p (%json-at json "data" "failure"))
                           (format nil "~A: the failure is an object" label))
                       (let ((counterexample (%json-at json "data" "counterexample")))
                         (ok (and (json-array-p counterexample)
                                  (plusp (length counterexample))
                                  (every #'hash-table-p counterexample))
                             (format nil "~A: the counterexample is a non-empty array of objects"
                                     label))))))))))
