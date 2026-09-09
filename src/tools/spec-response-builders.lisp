;;;; src/tools/spec-response-builders.lisp
;;;;
;;;; Report plists into MCP tool responses.
;;;;
;;;; Two rules shape every builder here.  An MCP client renders only
;;;; content[].text, so anything a caller must not miss goes into the text as
;;;; well as into the payload -- a zero-property selection reported only in a
;;;; sibling field is a zero-property selection nobody sees.  And no Lisp
;;;; value becomes a JSON number: cl-spec seeds reach 2^62, and trial counts
;;;; are the only numbers here small enough to be safe.

(defpackage #:cl-mcp/src/tools/spec-response-builders
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:json-bool)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:build-spec-symbol-response
           #:build-spec-describe-response
           #:build-spec-check-response))

(in-package #:cl-mcp/src/tools/spec-response-builders)

;;; ---------------------------------------------------------------------------
;;; Shared conversions
;;; ---------------------------------------------------------------------------

(defparameter +schema-version+ "1"
  "Version of the JSON these tools emit.

Owned by cl-mcp, not by cl-spec: this shape is the adapter's own external
representation, so it carries a version from the first release rather than
waiting on a capability API in the framework underneath (cl-spec
specification 72.6).  Bumped when a field changes meaning or disappears;
adding a field does not bump it.")

(defun %keyword-string (value)
  "Return VALUE as a lower-case string, or NIL.

Status keywords cross the boundary as text rather than as JSON identifiers:
:NOT-RUN reads as \"not-run\", which is what the tool documentation names."
  (when value
    (string-downcase (princ-to-string value))))

(defun %symbol-ht (data)
  "Return a symbol plist as a hash-table, or NIL when there is no symbol."
  (when data
    (make-ht "package" (getf data :package)
             "name" (getf data :name)
             "qualified" (getf data :qualified))))

(defun %symbol-hts (list)
  "Return a list of symbol plists as a vector of hash-tables."
  (coerce (mapcar #'%symbol-ht list) 'vector))

(defun %value-ht (data)
  "Return an externalized value plist as a hash-table."
  (make-ht "printed" (sanitize-for-json (getf data :printed))
           "printed_complete" (json-bool (getf data :printed-complete))
           "omitted_chars" (getf data :omitted-chars)
           ;; Kept apart from printed_complete on purpose: complete text is
           ;; still not a value that can be read back for anything but a
           ;; number, character, string, keyword or NIL/T.
           "restorable" (json-bool (getf data :restorable))
           "print_level" (getf data :print-level)
           "print_length" (getf data :print-length)
           "type" (getf data :type)
           "object_id" (getf data :object-id)))

(defun %named-value-hts (list)
  "Return a counterexample as a vector of {variable, value} hash-tables."
  (coerce (mapcar (lambda (entry)
                    (make-ht "variable" (%symbol-ht (getf entry :variable))
                             "value" (%value-ht (getf entry :value))))
                  list)
          'vector))

(defun %environment-ht (data)
  "Return the environment plist as a hash-table."
  (make-ht "cl_spec_loaded" (json-bool (getf data :cl-spec-loaded))
           "cl_spec_status" (%keyword-string (getf data :cl-spec-status))
           "cl_spec_version" (getf data :cl-spec-version)
           "cl_spec_system_directory" (getf data :cl-spec-system-directory)
           "generator_backend" (getf data :generator-backend)
           "backend_available" (json-bool (getf data :backend-available))
           "registry" (sanitize-for-json (getf data :registry))
           "missing" (coerce (getf data :missing) 'vector)
           "lisp" (getf data :lisp)))

(defun %strings (list)
  "Return LIST as a vector of printed strings."
  (coerce (mapcar (lambda (item)
                    (if (stringp item) item (princ-to-string item)))
                  list)
          'vector))

(defun %source-location-ht (location)
  "Return a source location plist as a hash-table, or NIL."
  (when location
    (make-ht "file" (getf location :file)
             "package" (getf location :package))))

(defun %unavailable-response (report)
  "Return the response for a status that carries only a message.

Not an isError: the tool answered correctly.  \"cl-spec is not loaded\" is a
fact about the image, and flagging it as a tool failure would push a caller
towards retrying rather than towards loading the system."
  (make-ht "schema_version" +schema-version+
           "status" (%keyword-string (getf report :status))
           "verified" (json-bool nil)
           "message" (sanitize-for-json (getf report :message))
           "environment" (%environment-ht (getf report :environment))
           "content" (text-content
                      (format nil "~A~%~%~A"
                              (string-upcase (%keyword-string (getf report :status)))
                              (getf report :message)))))

(defun %reason-line (reason)
  "Return a one-line rendering of a symbol resolution failure."
  (case (getf reason :reason)
    (:package-not-found
     (format nil "No package named ~A exists in this image."
             (getf reason :package)))
    (:symbol-not-found
     (format nil "Package ~A has no symbol named ~A. It was looked up, not ~
created: this tool never interns a name it was given."
             (getf reason :package) (getf reason :name)))
    (:not-external
     (format nil "~A is internal to package ~A. Write ~A::~A to reach it."
             (getf reason :name) (getf reason :package)
             (getf reason :package) (getf reason :name)))
    (t (format nil "~A is not a symbol name this tool accepts~@[: ~A~]"
               (getf reason :input) (getf reason :detail)))))

(defun %unresolved-response (report)
  "Return the response for a symbol designator that resolves to nothing."
  (let ((line (%reason-line (getf report :reason))))
    (make-ht "schema_version" +schema-version+
             "status" "unresolved-symbol"
             "verified" (json-bool nil)
             "input" (getf report :input)
             "reason" (%keyword-string (getf (getf report :reason) :reason))
             "message" line
             "environment" (%environment-ht (getf report :environment))
             "content" (text-content
                        (format nil "UNRESOLVED SYMBOL~%~A" line)))))

(defun %simple-status-response (report)
  "Return the response for a status carrying a name and a message."
  (make-ht "schema_version" +schema-version+
           "status" (%keyword-string (getf report :status))
           "name" (%symbol-ht (getf report :name))
           "message" (sanitize-for-json (getf report :message))
           "environment" (%environment-ht (getf report :environment))
           "content" (text-content
                      (format nil "~A~@[ ~A~]~%~%~A"
                              (string-upcase (%keyword-string (getf report :status)))
                              (getf (getf report :name) :qualified)
                              (or (getf report :message) "")))))

;;; ---------------------------------------------------------------------------
;;; spec-symbol
;;; ---------------------------------------------------------------------------

(defun %property-summary-ht (data)
  "Return one property listing entry as a hash-table."
  (make-ht "name" (%symbol-ht (getf data :name))
           "kind" (%keyword-string (getf data :kind))
           "tags" (%strings (getf data :tags))
           "targets" (%symbol-hts (getf data :targets))
           "documentation" (sanitize-for-json (getf data :documentation))
           "arguments"
           (coerce (mapcar (lambda (argument)
                             (let ((spec (getf argument :spec)))
                               (make-ht "variable"
                                        (%symbol-ht (getf argument :variable))
                                        "spec"
                                        (make-ht "kind" (%keyword-string
                                                         (getf spec :kind))
                                                 "name" (%symbol-ht (getf spec :name))
                                                 "target" (%symbol-ht
                                                           (getf spec :target))))))
                           (getf data :arguments))
                   'vector)
           "trials_table" (sanitize-for-json (getf data :trials-table))
           "shrink_enabled" (json-bool (getf data :shrink-enabled))
           "source_location" (%source-location-ht (getf data :source-location))
           "definition_digest" (getf data :definition-digest)
           "body_forms" (getf data :body-forms)
           "body_omitted" (json-bool (getf data :body-omitted))
           "detail_via" (getf data :detail-via)
           ;; A raw PRINC of a condition, so caller-controlled text like any
           ;; docstring: the sanitizer applies for the same reason it does
           ;; four lines up.
           "unavailable_reason" (sanitize-for-json (getf data :unavailable-reason))))

(defun %format-symbol-runtime (stream report)
  "Write the runtime half of the spec-symbol text to STREAM."
  (let ((runtime (getf report :runtime)))
    (if runtime
        (progn
          (format stream "  ~A~@[ ~A~]" (getf runtime :type) (getf runtime :arglist))
          (when (getf runtime :source-file)
            (format stream "~&  defined at ~A~@[:~D~]"
                    (getf runtime :source-file) (getf runtime :source-line)))
          (when (getf runtime :documentation)
            (format stream "~&  ~A" (getf runtime :documentation))))
        (format stream "~&  runtime information unavailable~@[: ~A~]"
                (getf report :runtime-unavailable-reason)))))

(defun %format-symbol-properties (stream report)
  "Write the registry half of the spec-symbol text to STREAM."
  (if (getf report :nothing-registered)
      (format stream "~&~%Nothing is registered about this symbol in the ~
cl-spec registry: no spec, no function spec, and no property. An empty ~
registry answer is not evidence that the symbol needs no contract -- the ~
system defining them may simply not be loaded.")
      (let ((registry (getf report :registry))
            (properties (getf report :properties)))
        (format stream "~&~%Registered:")
        (format stream "~&  spec:          ~A"
                (or (getf (getf registry :spec) :qualified) "none"))
        (format stream "~&  function spec: ~A"
                (or (getf (getf registry :function-spec) :qualified) "none"))
        (format stream "~&  property:      ~A"
                (or (getf (getf registry :property) :qualified) "none"))
        (format stream "~&~%Properties about this symbol (~D):" (length properties))
        (dolist (property properties)
          (format stream "~&  ~A~@[  [~A]~]"
                  (getf (getf property :name) :qualified)
                  (%keyword-string (getf property :kind)))
          (when (getf property :documentation)
            (format stream "~&      ~A" (getf property :documentation)))
          (when (getf property :tags)
            (format stream "~&      tags: ~{~A~^, ~}"
                    (mapcar #'%keyword-string (getf property :tags))))
          (format stream "~&      digest: ~A~@[  trials: ~A~]"
                  (or (getf property :definition-digest) "unavailable")
                  (getf property :trials-table))
          (when (getf property :body-omitted)
            ;; Defaulted rather than printed straight through: the report
            ;; layer always sets this, but a missing value would render as
            ;; "fetch it with NIL", which reads as an instruction and is not
            ;; one.
            (format stream "~&      body omitted (~D form~:P) -- fetch it with ~A"
                    (getf property :body-forms)
                    (or (getf property :detail-via)
                        "spec-describe kind=property")))))))

(defun %format-symbol-text (report)
  "Render the spec-symbol report as the text an MCP client will show."
  (with-output-to-string (stream)
    (format stream "~A" (getf (getf report :symbol) :qualified))
    (%format-symbol-runtime stream report)
    (%format-symbol-properties stream report)
    (dolist (note (getf report :notes))
      (format stream "~&~%note: ~A" note))))

(defun build-spec-symbol-response (report)
  "Return the MCP response for a SYMBOL-REPORT plist."
  (case (getf report :status)
    ((:cl-spec-not-loaded :cl-spec-incomplete) (%unavailable-response report))
    (:unresolved-symbol (%unresolved-response report))
    ((:internal-error :timeout :invalid-arguments)
     (%simple-status-response report))
    (t
     (let ((registry (getf report :registry))
           (runtime (getf report :runtime)))
       (make-ht "schema_version" +schema-version+
                "status" "ok"
                "symbol" (%symbol-ht (getf report :symbol))
                "runtime" (when runtime
                            (make-ht "type" (getf runtime :type)
                                     "arglist" (sanitize-for-json (getf runtime :arglist))
                                     "documentation" (sanitize-for-json
                                                      (getf runtime :documentation))
                                     "source_file" (getf runtime :source-file)
                                     "source_line" (getf runtime :source-line)))
                "runtime_unavailable_reason" (sanitize-for-json
                                              (getf report :runtime-unavailable-reason))
                "registry"
                (make-ht "spec" (%symbol-ht (getf registry :spec))
                         "function_spec" (%symbol-ht (getf registry :function-spec))
                         "property" (%symbol-ht (getf registry :property))
                         "properties_about" (%symbol-hts
                                             (getf registry :properties-about)))
                "properties" (coerce (mapcar #'%property-summary-ht
                                             (getf report :properties))
                                     'vector)
                "nothing_registered" (json-bool (getf report :nothing-registered))
                "notes" (%strings (getf report :notes))
                "environment" (%environment-ht (getf report :environment))
                "content" (text-content (%format-symbol-text report)))))))

;;; ---------------------------------------------------------------------------
;;; spec-describe
;;; ---------------------------------------------------------------------------

(defun %spec-tree-ht (data)
  "Return a spec-data tree as nested hash-tables."
  (when data
    (make-ht "kind" (%keyword-string (getf data :kind))
             "name" (%symbol-ht (getf data :name))
             "target" (%symbol-ht (getf data :target))
             "type" (getf data :type)
             "predicate" (getf data :predicate)
             "values" (getf data :values)
             "base_type" (getf data :base-type)
             "min" (getf data :min)
             "max" (getf data :max)
             "class_name" (%symbol-ht (getf data :class-name))
             "source_form" (getf data :source-form)
             "children" (coerce (mapcar #'%spec-tree-ht (getf data :children))
                                'vector))))

(defun %format-describe-text (report)
  "Render the spec-describe report as text."
  (with-output-to-string (stream)
    (format stream "~A ~A" (string-upcase (getf report :kind))
            (getf (getf report :name) :qualified))
    (when (getf report :property-kind)
      (format stream "  [~A]" (%keyword-string (getf report :property-kind))))
    (when (getf report :documentation)
      (format stream "~&~A" (getf report :documentation)))
    (when (getf report :targets)
      (format stream "~&about: ~{~A~^, ~}"
              (mapcar (lambda (target) (getf target :qualified))
                      (getf report :targets))))
    (when (getf report :arguments)
      (format stream "~&~%arguments:")
      (dolist (argument (getf report :arguments))
        (format stream "~&  ~A : ~A~@[ -> ~A~]"
                (getf (getf argument :variable) :name)
                (%keyword-string (getf (getf argument :spec) :kind))
                (getf (getf (getf argument :spec) :target) :qualified))))
    (when (getf report :definition-digest)
      (format stream "~&~%definition_digest: ~A" (getf report :definition-digest)))
    (when (getf report :body)
      (format stream "~&~%body:~%~A" (getf report :body))
      (unless (getf report :body-complete)
        (format stream "~&... truncated, ~D more character~:P. Raise max_chars ~
to see the rest; the text above is a preview, not a form that can be read back."
                (getf report :body-omitted-chars))))
    (when (getf report :source-form)
      (format stream "~&~%source form:~%~A" (getf report :source-form))
      (unless (getf report :source-form-complete)
        (format stream "~&... truncated, ~D more character~:P."
                (getf report :source-form-omitted-chars))))
    (let ((location (getf report :source-location)))
      (when location
        (format stream "~&~%defined in ~A~@[ (package ~A)~]"
                (getf location :file) (getf location :package))))))

(defun build-spec-describe-response (report)
  "Return the MCP response for a DESCRIBE-REPORT plist."
  (case (getf report :status)
    ((:cl-spec-not-loaded :cl-spec-incomplete) (%unavailable-response report))
    (:unresolved-symbol (%unresolved-response report))
    ((:not-registered :unsupported :invalid-arguments :internal-error :timeout)
     (%simple-status-response report))
    (t
     (make-ht "schema_version" +schema-version+
              "status" "ok"
              "kind" (getf report :kind)
              "name" (%symbol-ht (getf report :name))
              "property_kind" (%keyword-string (getf report :property-kind))
              "tags" (%strings (getf report :tags))
              "targets" (%symbol-hts (getf report :targets))
              "documentation" (sanitize-for-json (getf report :documentation))
              "trials_table" (sanitize-for-json (getf report :trials-table))
              "shrink_enabled" (json-bool (getf report :shrink-enabled))
              "arguments"
              (coerce (mapcar (lambda (argument)
                                (make-ht "variable"
                                         (%symbol-ht (getf argument :variable))
                                         "spec"
                                         (%spec-tree-ht (getf argument :spec))))
                              (getf report :arguments))
                      'vector)
              "spec" (%spec-tree-ht (getf report :spec))
              "body" (sanitize-for-json (getf report :body))
              "body_complete" (json-bool (getf report :body-complete))
              "body_omitted_chars" (getf report :body-omitted-chars)
              "source_form" (sanitize-for-json (getf report :source-form))
              "source_form_complete" (json-bool (getf report :source-form-complete))
              "source_form_omitted_chars" (getf report :source-form-omitted-chars)
              "source_location" (%source-location-ht (getf report :source-location))
              "definition_digest" (getf report :definition-digest)
              "environment" (%environment-ht (getf report :environment))
              "content" (text-content (%format-describe-text report))))))

;;; ---------------------------------------------------------------------------
;;; spec-check
;;; ---------------------------------------------------------------------------

(defun %by-status-ht (by-status)
  "Return the status-to-count alist as a hash-table keyed by status name."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (status . count) in by-status
          do (setf (gethash (%keyword-string status) table) count))
    table))

(defun %match-string (value)
  "Return a definition-match keyword as the word the tool documents.

Four answers.  \"unknown\" is the digest that could not be computed or whose
input was truncated: it disagrees with nothing, and calling it a mismatch told
a caller its definitions had moved when all that happened was that they could
not be read."
  (case value
    (:true "match")
    (:false "mismatch")
    (:unknown "unknown")
    (t "not-checked")))

(defun %faithful-string (value)
  "Return the reproduction-faithful value as a documented word.

Four answers, and NIL is not one of them.  A report that carries no verdict at
all -- a selection of zero, where nothing ran and no digest was requested --
was being published as an unfaithful reproduction of a run that never
happened, because \"unfaithful\" was the fallback for both an absent key and a
real disagreement.  CHECK-REPORT now says :FALSE for the disagreement."
  (case value
    (:true "faithful")
    (:false "unfaithful")
    (:unknown "unknown")
    ;; NIL included: a report with no verdict at all has not been checked.
    (t "not-checked")))

(defun %trials-ht (trials)
  "Return the trial budget plist as a hash-table."
  (make-ht "executed" (getf trials :executed)
           "budget" (getf trials :budget)
           "budget_source" (getf trials :budget-source)
           "property_trials" (getf trials :property-trials)
           "backend_default" (getf trials :backend-default)
           "budget_derivation" (getf trials :budget-derivation)))

(defun %result-ht (result)
  "Return one per-property result as a hash-table."
  (make-ht "property" (%symbol-ht (getf result :property))
           "status" (%keyword-string (getf result :status))
           "reason" (%keyword-string (getf result :reason))
           "trials" (%trials-ht (getf result :trials))
           "seed" (getf result :seed)
           "profile" (%keyword-string (getf result :profile))
           "counterexample" (%named-value-hts (getf result :counterexample))
           "counterexample_status" (%keyword-string
                                    (getf result :counterexample-status))
           "counterexample_unavailable_reason"
           (getf result :counterexample-unavailable-reason)
           "shrunk_counterexample" (%named-value-hts
                                    (getf result :shrunk-counterexample))
           "shrink_status" (%keyword-string (getf result :shrink-status))
           "shrink_note" (getf result :shrink-note)
           "condition" (let ((condition (getf result :condition)))
                         (when condition
                           (make-ht "type" (getf condition :type)
                                    "message" (sanitize-for-json
                                               (getf condition :message))
                                    "object_id" (getf condition :object-id))))
           "elapsed" (getf result :elapsed)
           "timeout_seconds" (getf result :timeout-seconds)
           "thread_leaked" (json-bool (getf result :thread-leaked))
           "definition_digest" (getf result :definition-digest)
           "definition_digest_complete" (json-bool
                                         (getf result :definition-digest-complete))
           "definition_match" (%match-string (getf result :definition-match))
           "message" (sanitize-for-json (getf result :message))))

(defun %format-values (entries)
  "Return \"A = 68, B = 85\" for a counterexample, or NIL when there is none."
  (when entries
    (format nil "~{~A~^, ~}"
            (mapcar (lambda (entry)
                      (format nil "~A = ~A"
                              (getf (getf entry :variable) :name)
                              (getf (getf entry :value) :printed)))
                    entries))))

(defun %format-counterexample (stream result)
  "Write RESULT's counterexample and shrinking lines to STREAM.

The status words are printed, not inferred from the list being empty.  A
property that generates no arguments has an empty counterexample and so does a
run that never produced one, and the difference is the whole question of
whether anything was learned."
  (let* ((original (%format-values (getf result :counterexample)))
         (shrunk (%format-values (getf result :shrunk-counterexample)))
         ;; A result that carries no status is one this builder did not
         ;; produce.  Values are still shown -- swallowing a counterexample
         ;; because a field was absent would be the worse failure -- but the
         ;; absence is reported as :UNKNOWN rather than guessed at.
         (status (or (getf result :counterexample-status)
                     (if original :present :unknown)))
         (shrink-status (or (getf result :shrink-status)
                            (if shrunk :present :unknown))))
    (case status
      (:present
       (format stream "~&    counterexample:        ~A"
               (or original "(this property generates no arguments)")))
      (:unavailable
       (format stream "~&    counterexample:        UNAVAILABLE~@[ -- ~A~]"
               (getf result :counterexample-unavailable-reason)))
      (:unknown
       (format stream "~&    counterexample:        UNKNOWN~@[ -- ~A~]"
               (getf result :counterexample-unavailable-reason)))
      (:none
       (format stream "~&    counterexample:        none reported by the backend"))
      (t nil))
    (case shrink-status
      (:present
       (format stream "~&    shrunk counterexample: ~A"
               (or shrunk "(this property generates no arguments)"))
       (when (getf result :shrink-note)
         (format stream "~&      ~A" (getf result :shrink-note))))
      (:disabled
       (format stream "~&    shrunk counterexample: not attempted -- this ~
property is defined with (:shrink nil)"))
      (:none
       (format stream "~&    shrunk counterexample: shrinking was enabled but ~
returned no smaller input"))
      (:unavailable
       (format stream "~&    shrunk counterexample: UNAVAILABLE -- the run did ~
not reach a verdict"))
      (t nil))))

(defun %format-one-result (stream result index)
  "Write one per-property result to STREAM."
  (format stream "~&~%[~D] ~A  ~A"
          index
          (getf (getf result :property) :qualified)
          (%keyword-string (getf result :status)))
  (let ((trials (getf result :trials)))
    (format stream "~&    trials: ~A executed of ~A budget (~A)"
            (or (getf trials :executed) "none")
            (or (getf trials :budget) "unknown")
            (or (getf trials :budget-source) "unknown")))
  (%format-counterexample stream result)
  (let ((condition (getf result :condition)))
    (when condition
      (format stream "~&    condition: [~A] ~A"
              (getf condition :type) (getf condition :message))))
  (when (getf result :seed)
    (format stream "~&    seed: ~A   profile: ~A"
            (getf result :seed)
            (%keyword-string (getf result :profile))))
  (when (getf result :definition-digest)
    (format stream "~&    definition_digest: ~A~@[  (~A)~]"
            (getf result :definition-digest)
            (unless (eq :not-checked (getf result :definition-match))
              (%match-string (getf result :definition-match)))))
  (when (getf result :message)
    (format stream "~&    ~A" (getf result :message))))

(defun %format-check-footer (stream report)
  "Write the tally, the warnings and the replay line to STREAM."
  ;; The replay line points at the first result that did NOT pass, falling
  ;; back to the first.  A selection of three where the third failed would
  ;; otherwise hand the caller the seed of a property that already holds --
  ;; the one run they have no reason to reproduce.
  (let* ((counts (getf report :counts))
         (results (getf report :results))
         (first-result (or (find-if-not (lambda (result)
                                          (eq :passed (getf result :status)))
                                        results)
                           (first results))))
    (format stream "~&~%verified: ~A   ~D selected: ~{~A~^, ~}"
            (if (getf report :verified) "true" "false")
            (getf counts :selected)
            (or (loop for (status . count) in (getf counts :by-status)
                      collect (format nil "~D ~(~A~)" count status))
                (list "nothing ran")))
    ;; Printed for any timeout, not only a leaked thread.  A stopped thread
    ;; is not evidence that what it was doing was undone.
    (when (getf report :worker-reuse-message)
      (format stream "~&~%worker_reuse: ~A~&~A"
              (string-downcase (princ-to-string (getf report :worker-reuse)))
              (getf report :worker-reuse-message)))
    (let ((gaps (getf report :verification-gaps)))
      (when gaps
        (format stream "~&verification gaps: ~{~(~A~)~^, ~}" gaps)))
    (unless (eq :not-checked (getf report :reproduction-faithful))
      (format stream "~&reproduction: ~A"
              (%faithful-string (getf report :reproduction-faithful))))
    (when (and first-result (getf first-result :seed))
      (format stream "~&~%Replay: spec-check property=~A seed=~A profile=~A~
~@[ expect_definition_digest=~A~]"
              (getf (getf first-result :property) :qualified)
              (getf first-result :seed)
              (%keyword-string (getf first-result :profile))
              (getf first-result :definition-digest)))
    ;; Gated on there being a seed to reproduce from.  A run where nothing
    ;; executed has nothing to say about reproduction, and printing the
    ;; caveat there is noise the caller has to read past every time.
    (when (and (getf report :reproduce-scope)
               first-result
               (getf first-result :seed))
      (format stream "~&~A" (getf report :reproduce-scope)))))

(defun %check-headline (report)
  "Return the first line of a spec-check text, in this project's house style.

Three outcomes rather than two.  A property that was falsified and a run that
could not finish are different news: collapsing them under one word would let
a timeout read as a counterexample, and it is the timeout that means nothing
was learned either way."
  (cond ((getf report :verified) "✓ VERIFIED")
        ((plusp (or (getf (getf report :counts) :failed) 0)) "✗ FAILED")
        (t "⚠ NOT VERIFIED")))

(defun %format-check-text (report)
  "Render the spec-check report as the text an MCP client will show."
  (with-output-to-string (stream)
    (let ((selection (getf report :selection)))
      (if (eq :no-properties (getf report :status))
          (format stream "⚠ NO PROPERTIES  ~A~&Selected 0 properties via ~A.~&~%~A~
~&verified: false"
                  (or (getf (getf (getf selection :requested) :symbol) :qualified)
                      "")
                  (getf selection :source)
                  (getf report :message))
          (progn
            (format stream "~A" (%check-headline report))
            (format stream "~&Selected ~D propert~:@P via ~A."
                    (getf selection :count) (getf selection :source))
            (format stream "~&  ~A" (getf selection :coverage))
            (dolist (note (getf selection :notes))
              (format stream "~&  note: ~A" note))
            (loop for result in (getf report :results)
                  for index from 1
                  do (%format-one-result stream result index))
            (%format-check-footer stream report))))))

(defun build-spec-check-response (report)
  "Return the MCP response for a CHECK-REPORT plist."
  (case (getf report :status)
    ((:cl-spec-not-loaded :cl-spec-incomplete) (%unavailable-response report))
    (:unresolved-symbol (%unresolved-response report))
    ((:not-registered :invalid-arguments :backend-not-loaded :internal-error
      :timeout)
     (let ((response (%simple-status-response report)))
       (setf (gethash "verified" response) (json-bool nil))
       response))
    (t
     (let ((selection (getf report :selection))
           (counts (getf report :counts)))
       (make-ht "schema_version" +schema-version+
                "status" (%keyword-string (getf report :status))
                "verified" (json-bool (getf report :verified))
                "selection"
                (make-ht "mode" (getf selection :mode)
                         "requested"
                         (let ((requested (getf selection :requested)))
                           (make-ht "property" (%symbol-ht (getf requested :property))
                                    "symbol" (%symbol-ht (getf requested :symbol))))
                         "selected" (%symbol-hts (getf selection :selected))
                         "count" (getf selection :count)
                         "source" (getf selection :source)
                         "coverage" (getf selection :coverage)
                         "notes" (%strings (getf selection :notes)))
                "results" (coerce (mapcar #'%result-ht (getf report :results))
                                  'vector)
                "counts" (make-ht "selected" (getf counts :selected)
                                  "passed" (getf counts :passed)
                                  "failed" (getf counts :failed)
                                  "errored" (getf counts :errored)
                                  "timed_out" (getf counts :timed-out)
                                  "not_run" (getf counts :not-run)
                                  "other" (getf counts :other)
                                  ;; Every status that occurred, so a tally
                                  ;; that does not sum to selected cannot
                                  ;; hide a status without a field of its own.
                                  "by_status" (%by-status-ht (getf counts :by-status)))
                "profile" (%keyword-string (getf report :profile))
                "timeout_seconds" (getf report :timeout-seconds)
                "thread_leaked" (json-bool (getf report :thread-leaked))
                "worker_reuse" (or (%keyword-string (getf report :worker-reuse))
                                   "safe")
                "worker_reuse_message" (getf report :worker-reuse-message)
                "verification_gaps"
                (coerce (mapcar #'%keyword-string
                                (getf report :verification-gaps))
                        'vector)
                "elapsed" (getf report :elapsed)
                "options" nil
                "options_note" (getf report :options-note)
                "reproduce_scope" (getf report :reproduce-scope)
                "reproduction_faithful" (%faithful-string
                                         (getf report :reproduction-faithful))
                "message" (getf report :message)
                "environment" (%environment-ht (getf report :environment))
                "content" (text-content (%format-check-text report)))))))
