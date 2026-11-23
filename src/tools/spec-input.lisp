;;;; src/tools/spec-input.lisp

(defpackage #:cl-mcp/src/tools/spec-input
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:api-class #:api-fn #:api-has-p #:api-special
                #:resolve-cl-spec-api #:resolve-symbol-designator)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:environment-data #:unavailable-report #:*default-check-timeout-seconds*
                #:exact-case-selector #:%classify-condition #:%condition-report)
  (:import-from #:cl-mcp/src/spec-core-record #:project-core-record)
  (:import-from #:cl-mcp/src/tools/spec-response-builders #:%core-record-ht #:%reason-line)
  (:import-from #:cl-mcp/src/tools/spec-entry #:%positive-integer-arg)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht #:text-content #:json-bool)
  (:import-from #:cl-mcp/src/utils/sanitize #:sanitize-for-json)
  (:import-from #:cl-mcp/src/utils/deadline #:call-with-deadline-thread)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:request-debugger-escape-error-p #:request-debugger-escape-error-display-text)
  (:import-from #:cl-mcp/src/object-registry #:*object-registry* #:lookup-object)
  (:import-from #:cl-ppcre #:scan)
  (:import-from #:yason)
  (:export #:spec-input-response))

(in-package #:cl-mcp/src/tools/spec-input)

(declaim (optimize (debug 3) (safety 3)))

(define-condition input-problem (simple-error)
  ((status :initarg :status :reader input-problem-status)
   (code :initarg :code :initform nil :reader input-problem-code))
  (:documentation "An input token could not be resolved before calling the target."))

(defun %invalid (message)
  (error 'input-problem :status :invalid-arguments :format-control "~A"
                        :format-arguments (list message)))

(defun %message (status message &key input-kind code)
  (let* ((word (string-downcase status))
         (clean (sanitize-for-json message))
         (result (make-ht "schema_version" "1" "scope" "single-input" "status" word
                          "message" clean "content"
                          (text-content (format nil "~A -- ~A" word clean)))))
    (when input-kind (setf (gethash "input_kind" result) input-kind))
    (when code (setf (gethash "code" result) code))
    result))

(defun %symbol-input (name package &optional (status :invalid-arguments))
  (multiple-value-bind (symbol reason) (resolve-symbol-designator name :package package)
    (when reason
      (error 'input-problem :status status :format-control "~A"
                            :format-arguments (list (%reason-line reason))))
    symbol))

(defun %object-input (id)
  ;; LOOKUP-OBJECT identifies the generation and eviction. Check the wire spelling
  ;; too, so a permissively parsed integer suffix cannot alias a differently spelled id.
  (unless (and (stringp id) (scan "^o-[0-9a-fA-F]{32}-[1-9][0-9]*\\z" id))
    (error 'input-problem :status :invalid-arguments :code "INVALID_OBJECT_ID"
                          :format-control "object_id requires a handle from this worker."))
  (multiple-value-bind (object found-p why) (lookup-object id)
    (if found-p object
        (ecase why
          (:stale
           (error 'input-problem :status :stale-object-id :code "OBJECT_STALE"
                                 :format-control "The object id belongs to another worker or a ~
previous registry generation. Obtain a current handle in this session."))
          (:evicted
           (error 'input-problem :status :evicted-object-id :code "OBJECT_NOT_FOUND"
                                 :format-control "This worker no longer retains the object."))
          (:invalid (%invalid "Malformed object_id."))))))

(defun %decode-input (value package)
  (cond
    ((hash-table-p value)
     (unless (= 1 (hash-table-count value))
       (%invalid "A value token must contain exactly one of symbol, keyword, or object_id."))
     (cond
       ((nth-value 1 (gethash "symbol" value))
        (%symbol-input (gethash "symbol" value) package))
       ((nth-value 1 (gethash "keyword" value))
        (let ((name (gethash "keyword" value)))
          (unless (and (stringp name) (plusp (length name)) (not (find #\: name)))
            (%invalid "keyword requires a non-empty keyword name without a package prefix."))
          (%symbol-input (concatenate 'string ":" name) nil)))
       ((nth-value 1 (gethash "object_id" value)) (%object-input (gethash "object_id" value)))
       (t (%invalid "Unknown value token. Use symbol, keyword, or object_id."))))
    ((or (stringp value) (realp value)) value)
    ((or (null value) (eq value 'yason:false) (eq value :null)) nil)
    ((or (eq value t) (eq value 'yason:true)) t)
    ((vectorp value) (map 'list (lambda (item) (%decode-input item package)) value))
    (t (%invalid "Expected a JSON scalar, array, or typed value token."))))

(defun %input-outcome (observation fixture-p)
  (let ((outcome (and observation (gethash "outcome" observation))))
    (cond ((and fixture-p (hash-table-p outcome)) outcome)
          ((hash-table-p outcome) (make-ht "availability" "collected" "value" outcome))
          (t (make-ht "availability" "not-collected")))))

(defun %summary-text (function status selected selector outcome phase reason lifecycle)
  (let* ((availability (gethash "availability" outcome))
         (value (gethash "value" outcome))
         (kind (and (hash-table-p value) (gethash "kind" value))))
    (with-output-to-string (s)
      (format s "~A on one concrete input to ~A~%  selected case: ~A~@[ (selector ~S)~]~%  target outcome: ~A"
              status function
              (if (hash-table-p selected) (gethash "qualified" selected) (or selected "none"))
              (and selector (not (equal selected selector)) selector)
              (or kind availability))
      (when (gethash "reason" outcome) (format s " (~A)" (gethash "reason" outcome)))
      (format s "~%  failure phase: ~A~@[ (~A)~]" (or phase "none") reason)
      (when lifecycle
        (format s "~%  fixture: setup ~A, evaluation ~A, cleanup ~A, state ~A, completion ~A"
                (gethash "setup" lifecycle) (gethash "evaluation" lifecycle)
                (gethash "cleanup" lifecycle) (gethash "state" lifecycle)
                (gethash "completion" lifecycle))
        (when (nth-value 1 (gethash "target_called" lifecycle))
          (format s ", target called ~A" (if (eq t (gethash "target_called" lifecycle))
                                             "yes" "no")))
        (loop for issue across (gethash "errors" lifecycle #())
              do (format s "~%    ~A: ~A~@[ -- ~A~]"
                         (gethash "phase" issue) (gethash "reason" issue)
                         (gethash "condition_report" issue)))))))

(defun %record-response (record function fixture-p max-chars)
  (let ((input-kind (if fixture-p "recipe" "arguments")))
    (multiple-value-bind (report status reason)
        (project-core-record record (if fixture-p :fixture-check-data :call-check-data)
                             :expected-record-kind (if fixture-p :fixture-check :result)
                             :expected-entity-kind :function-spec :max-chars max-chars)
      (unless (eq status :ok)
        (return-from %record-response
          (%message (if (eq status :malformed) :internal-error status)
                    (format nil "cl-spec returned ~A: ~A." status reason)
                    :input-kind input-kind)))
      (let* ((core (%core-record-ht report))
             (data (gethash "data" core))
             (observation (gethash "observation" data))
             (status (gethash "status" data))
             (selected (and observation (gethash "case" observation)))
             (case-name (and selected (getf (getf record :observation) :case)))
             (selector (and (keywordp case-name) (exact-case-selector case-name)))
             (outcome (%input-outcome observation fixture-p))
             (phase (gethash "failure_phase" data))
             (reason (or (gethash "reason" data) (and observation (gethash "reason" observation))))
             (lifecycle (gethash "lifecycle" data))
             (summary (%summary-text function status selected selector outcome phase reason lifecycle))
             (result (make-ht "schema_version" "1" "scope" "single-input"
                              "function" function "input_kind" input-kind
                              "status" status "selected_case" selected "selected_case_selector" selector
                              "outcome" outcome "failure_phase" phase "reason" reason
                              "core_result" core
                              "content" (text-content (sanitize-for-json summary)))))
        (when lifecycle (setf (gethash "lifecycle" result) lifecycle))
        result))))

(defun %input-error (api condition input-kind max-chars)
  (if (typep condition 'input-problem)
      (%message (input-problem-status condition) (princ-to-string condition)
                :input-kind input-kind :code (input-problem-code condition))
      (flet ((is-a (key)
               (let ((class (api-class api key))) (and class (typep condition class)))))
        (%message (cond ((or (is-a :invalid-call-arguments) (is-a :invalid-argument))
                         :invalid-arguments)
                        ((is-a :unbound-target) :undefined-function)
                        (t (%classify-condition api condition)))
                  (%condition-report condition max-chars)
                  :input-kind input-kind))))

(defun %run-input (api function package input fixture-p seconds max-chars)
  (let* ((input-kind (if fixture-p "recipe" "arguments"))
         (runner (if fixture-p :check-fixture :check-call))
         (reader (if fixture-p :fixture-check-data :call-check-data))
         (objects *object-registry*)
         (specials (list (api-special api :registry) (api-special api :generator-backend)))
         (bindings (mapcar #'symbol-value specials)))
    (unless (and (api-has-p api runner) (api-has-p api reader))
      (return-from %run-input
        (%message :unsupported (format nil "The loaded cl-spec must provide ~A and ~A."
                                       runner reader) :input-kind input-kind)))
    (multiple-value-bind (value status leaked)
        (call-with-deadline-thread
         (lambda ()
           (progv specials bindings
             (let ((*object-registry* objects))
               ;; Classifying and reporting a lookup error can run a user-defined printer.
               (handler-case
                   (let* ((name (%symbol-input function package :unresolved-symbol))
                          (arguments-or-recipe (%decode-input input package))
                          (result (funcall (api-fn api runner) name arguments-or-recipe
                                           :registry (first bindings))))
                     (%record-response (funcall (api-fn api reader) result)
                                       function fixture-p max-chars))
                 (serious-condition (condition)
                   (%input-error api condition input-kind max-chars))))))
         seconds :name "mcp-spec-input")
      (ecase status
        (:ok (first value))
        (:error
         ;; The deadline has ended. Only a debugger snapshot has safe, saved text.
         (%message :internal-error
                   (if (request-debugger-escape-error-p value)
                       (%condition-report (request-debugger-escape-error-display-text value)
                                          max-chars)
                       "The concrete check ended before it could report its error.")
                   :input-kind input-kind))
        (:timeout
         (let ((response (%message :timeout
                                   (format nil "The concrete check exceeded ~D second~:P. ~
Its side effects and fixture cleanup are unknown.~:[~; The check thread is still running. ~
Replace this worker before further checks.~]" seconds leaked) :input-kind input-kind)))
           (setf (gethash "thread_leaked" response) (json-bool leaked)
                 (gethash "worker_reuse" response) (if leaked "unsafe" "unknown"))
           response))))))

(defun spec-input-response (params)
  "Check one caller-supplied argument array or one fixture recipe and return its MCP response."
  (flet ((positive-parameter (key default)
           (multiple-value-bind (number error) (%positive-integer-arg params key default)
             (if error (%invalid error) number))))
    (handler-case
        (let* ((function (gethash "function" params))
               (package (gethash "package" params))
               (arguments (gethash "arguments" params))
               (args-p (nth-value 1 (gethash "arguments" params)))
               (fixture-p (nth-value 1 (gethash "recipe" params)))
               (seconds (positive-parameter "timeout_seconds" *default-check-timeout-seconds*))
               (max-chars (positive-parameter "max_value_chars" 2000)))
          (unless (and (stringp function) (plusp (length function)))
            (%invalid "function must be a non-empty symbol designator."))
          (when (and package (not (and (stringp package) (plusp (length package)))))
            (%invalid "package must be a non-empty package name."))
          (when (eq args-p fixture-p)
            (%invalid "Supply exactly one of arguments or recipe."))
          (when (and args-p (not (and (vectorp arguments) (not (stringp arguments)))))
            (%invalid "arguments must be a JSON array."))
          (multiple-value-bind (api status) (resolve-cl-spec-api)
            (if (eq status :ok)
                (%run-input api function package (if fixture-p (gethash "recipe" params) arguments)
                            fixture-p seconds max-chars)
                (let ((report (unavailable-report status (environment-data api status))))
                  (%message (getf report :status) (getf report :message))))))
      (input-problem (condition)
        (%message (input-problem-status condition) (princ-to-string condition))))))
