;;;; src/tools/spec-batch.lisp

(defpackage #:cl-mcp/src/tools/spec-batch
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-cl-spec-api #:api-fn #:api-special #:resolve-symbol-designator
                #:symbol-data)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:check-report #:unavailable-report #:*default-check-timeout-seconds*
                #:%resolve-profile)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:parse-seed-string #:%positive-integer-arg #:%bounded-integer-arg
                #:*maximum-trials*)
  (:import-from #:cl-mcp/src/tools/spec-response-builders #:build-spec-check-response)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:json-bool #:text-content #:extract-arg #:extract-array-arg
                #:arg-validation-error #:validation-message)
  (:import-from #:cl-mcp/src/utils/deadline #:leaked-threads)
  (:import-from #:cl-mcp/src/spec-execution
                #:resolve-check-wrapper #:call-with-check-deadline #:check-wrapper-message)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:request-debugger-escape-error-p #:request-debugger-escape-error-display-text)
  (:export #:spec-batch-response))

(in-package #:cl-mcp/src/tools/spec-batch)

(declaim (optimize (debug 3) (safety 3)))

(defun %options (params)
  "Return normalized budgets and an argument error, if any, before running any item."
  (handler-case
      (let ((symbol (extract-arg params "symbol" :type :string))
            (functions (extract-array-arg params "functions" :string))
            (properties (extract-array-arg params "properties" :string))
            (profile (extract-arg params "profile" :type :string))
            (package (extract-arg params "package" :type :string))
            (timeout (or (extract-arg params "timeout_seconds" :type :number)
                         *default-check-timeout-seconds*)))
        (multiple-value-bind (seed seed-error)
            (parse-seed-string (and params (gethash "seed" params)))
          (multiple-value-bind (trials trials-error)
              (%bounded-integer-arg params "trials" *maximum-trials*)
            (multiple-value-bind (chars chars-error)
                (%positive-integer-arg params "max_value_chars" 2000)
              (values (list :symbol symbol :functions functions :properties properties
                            :package package :profile profile :trials trials :seed seed
                            :timeout timeout :max-value-chars chars)
                      (or (when (eq (not symbol) (not (or functions properties)))
                            "Supply either symbol or an explicit functions/properties array.")
                          (when (and symbol (zerop (length symbol)))
                            "symbol must be a non-empty string.")
                          (unless (realp timeout) "timeout_seconds must be a real number.")
                          seed-error trials-error chars-error
                          (when profile (nth-value 1 (%resolve-profile profile)))))))))
    (arg-validation-error (condition) (values nil (validation-message condition)))))

(defun %targets (kind names package)
  "Keep the first spelling of each symbol identity within KIND, in selection order."
  (let ((seen (make-hash-table :test #'equal))
        (items '()))
    (map nil (lambda (name)
               (let* ((symbol (if (symbolp name) name
                                  (resolve-symbol-designator name :package package)))
                      ;; Keep unresolved names so each gets its own diagnostic in CHECK-REPORT.
                      (key (or symbol name)))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)
                   (push (cons kind (if symbol (getf (symbol-data symbol) :qualified) name))
                         items))))
         names)
    (nreverse items)))

(defun %combined-targets (api designator package)
  (multiple-value-bind (symbol reason) (resolve-symbol-designator designator :package package)
    (if reason
        (values nil "unresolved-symbol"
                (gethash "message"
                         (build-spec-check-response
                          (list :status :unresolved-symbol :input designator :reason reason))))
        (let* ((registry (funcall (api-fn api :registry)))
               (routing (funcall (api-fn api :semantic-data) symbol :registry registry))
               (contract (getf routing :function-spec)))
          (append (%targets :contract (when contract (list contract)) nil)
                  (%targets :property (getf routing :properties-about) nil))))))

(defun %remaining (deadline)
  (float (/ (- deadline (get-internal-real-time)) internal-time-units-per-second) 1d0))

(defun %before-deadline (deadline thunk &optional specials values)
  ;; Selection, digests and rendering may run registry methods or printers, too.
  ;; The inner check's timeout alone bounds none of these.
  (let ((remaining (%remaining deadline)))
    (if (plusp remaining)
        (call-with-check-deadline
         (lambda () (progv specials values (funcall thunk))) remaining :name "mcp-spec-batch")
        (values nil :not-run nil))))

(defun %condition-message (condition)
  ;; A condition's own printer can block just as a registry reader can.
  (if (request-debugger-escape-error-p condition)
      (request-debugger-escape-error-display-text condition)
      (format nil "Processing the selection signalled ~S." (type-of condition))))

(defun %unexecuted-item (target status &optional message)
  (make-ht "kind" (string-downcase (car target)) "name" (cdr target) "status" status
           "verified" (json-bool nil) "verification_gaps" (vector status)
           "message" message "seed" nil "definition_digest" nil "replay" nil))

(defun %replay (row kind name profile wrapper-name)
  "Return spec-check arguments for the actual seed and budget reported in ROW."
  (when (gethash "seed" row)
    (let* ((contract (eq kind :contract))
           (args (make-ht (if contract "function" "property") name "seed" (gethash "seed" row)))
           (budget (gethash "budget" (gethash "trials" row)))
           (digest (gethash "definition_digest" row)))
      (cond ((and contract (typep budget '(integer 1)) (<= budget *maximum-trials*))
             (setf (gethash "trials" args) budget))
            ((and (not contract) profile) (setf (gethash "profile" args) profile)))
      (when digest (setf (gethash "expect_definition_digest" args) digest))
      (when wrapper-name (setf (gethash "around_function" args) wrapper-name))
      args)))

(defun %check-item (api target options deadline)
  (let* ((kind (car target))
         (name (cdr target))
         (report (check-report api :ok
                               :function (when (eq kind :contract) name)
                               :property (when (eq kind :property) name)
                               :package (getf options :package)
                               :trials (when (eq kind :contract) (getf options :trials))
                               :profile (when (eq kind :property) (getf options :profile))
                               :seed (getf options :seed)
                               :max-value-chars (getf options :max-value-chars)
                               :timeout-seconds (%remaining deadline)))
         (response (build-spec-check-response report))
         (results (gethash "results" response))
         (item (if (plusp (length results)) (elt results 0)
                   (%unexecuted-item target (gethash "status" response)
                                     (gethash "message" response)))))
    ;; Preserve the single-check evidence, including structured counterexamples,
    ;; case counts and selection-specific gaps. Its call status is not a verdict.
    (setf (gethash "name" item) name
          (gethash "verified" item) (gethash "verified" response)
          (gethash "verification_gaps" item) (or (gethash "verification_gaps" response)
                                                (gethash "verification_gaps" item))
          (gethash "selection" item) (gethash "selection" response)
          (gethash "replay" item) (%replay item kind name (gethash "profile" response)
                                        (getf options :around-function)))
    (values item (gethash "worker_reuse" response "safe"))))

(defun %run-targets (api targets options deadline specials settings progress)
  (let ((reuse "safe")
        (results '()))
    (loop for pending on targets
          for target = (first pending)
          do (when progress (funcall progress results pending t))
             (multiple-value-bind (value status leaked)
                 (if (equal reuse "safe")
                     (%before-deadline deadline (lambda () (%check-item api target options deadline))
                                       specials settings)
                     (values nil :not-run nil))
               (push (case status
                       (:ok (setf reuse (second value)) (first value))
                       (:not-run (%unexecuted-item target "not-run" "The item did not start."))
                       (:timeout
                        (setf reuse "unknown")
                        (%unexecuted-item target "timeout" "The deadline expired during this item."))
                       (:error (%unexecuted-item target "internal-error" (%condition-message value))))
                     results)
               (when (or leaked (and (eq status :timeout) (leaked-threads)))
                 (setf reuse "unsafe")))
             (when progress (funcall progress results (rest pending) nil)))
    (values (reverse results) reuse)))

(defun %batch-response (results &key status message (reuse "safe") (selection-complete t))
  (let ((counts (make-ht))
        (contracts (count "contract" results :test #'equal
                          :key (lambda (row) (gethash "kind" row))))
        (verified (and (null status) selection-complete results
                       (every (lambda (row) (eq t (gethash "verified" row))) results))))
    (dolist (row results) (incf (gethash (gethash "status" row) counts 0)))
    (let ((status (or status
                      (cond ((null results) "no-selection")
                            ((loop for row in results
                                   always (and (member (gethash "status" row)
                                                       '("passed" "failed" "error" "skipped"
                                                         "pending") :test #'equal)
                                               (not (find "generation-incomplete"
                                                          (gethash "verification_gaps" row)
                                                          :test #'equal))))
                             "completed")
                            (t "incomplete")))))
      (make-ht
       "schema_version" "1" "status" status "verified" (json-bool verified)
       "selection_complete" (json-bool selection-complete)
       "counts" (make-ht "selected" (length results) "contracts" contracts
                         "properties" (- (length results) contracts)
                         "not_run" (gethash "not-run" counts 0) "by_status" counts)
       "worker_reuse" reuse "thread_leaked" (json-bool (equal "unsafe" reuse))
       "results" (coerce results 'vector) "message" message
       "content"
       (text-content
        (with-output-to-string (out)
          (format out "Batch ~A. ~D contract~:P, ~D propert~:@P. verified: ~A"
                  status contracts (- (length results) contracts) (if verified "true" "false"))
          (when message (format out "~&~A" message))
          (dolist (row results)
            (format out "~&~A ~A  ~A" (gethash "kind" row) (gethash "name" row)
                    (gethash "status" row))
            (let ((replay (gethash "replay" row)))
              (when replay
                (format out "~&  Replay: spec-check ~A=~A seed=~A~@[ trials=~A~]~
~@[ profile=~A~]~@[ expect_definition_digest=~A~]~@[ around_function=~A~]"
                        (if (gethash "function" replay) "function" "property")
                        (gethash "name" row) (gethash "seed" replay)
                        (gethash "trials" replay) (gethash "profile" replay)
                        (gethash "expect_definition_digest" replay)
                         (gethash "around_function" replay)))))
          (unless (equal "safe" reuse) (format out "~&worker_reuse: ~A" reuse))))))))

(defun %discovery-failure (targets combined status value leaked)
  (let* ((timed-out (member status '(:timeout :not-run)))
         (reuse (cond ((or leaked (and (eq status :timeout) (leaked-threads))) "unsafe")
                      ((eq status :timeout) "unknown")
                      (t "safe")))
         (message (if timed-out "The deadline expired before registry selection completed."
                      (%condition-message value))))
    (%batch-response (mapcar (lambda (target) (%unexecuted-item target "not-run")) targets)
                     :status (cond ((not timed-out) "internal-error")
                                   (combined "timeout")
                                   (t "incomplete"))
                     :selection-complete (not combined) :reuse reuse :message message)))

(defun %check-batch (options deadline targets progress)
  (let ((symbol (getf options :symbol))
        (package (getf options :package)))
    (multiple-value-bind (resolved status leaked)
        (%before-deadline deadline #'resolve-cl-spec-api)
      (unless (eq :ok status)
        (return-from %check-batch
          (%discovery-failure targets symbol status resolved leaked)))
      (destructuring-bind (api api-status) resolved
        (unless (eq api-status :ok)
          (let* ((report (unavailable-report api-status nil))
                 (status (string-downcase (getf report :status))))
            (return-from %check-batch
              (%batch-response (mapcar (lambda (target) (%unexecuted-item target status)) targets)
                               :status status :message (getf report :message)
                               :selection-complete (not symbol)))))
        (let* ((specials (remove nil (list (api-special api :registry)
                                           (api-special api :generator-backend))))
               (settings (mapcar #'symbol-value specials)))
          (when symbol
            (multiple-value-bind (selection status leaked)
                (%before-deadline deadline (lambda () (%combined-targets api symbol package))
                                  specials settings)
              (unless (eq status :ok)
                (return-from %check-batch
                  (%discovery-failure nil symbol status selection leaked)))
              (destructuring-bind (items &optional error-status message) selection
                (when error-status
                  (return-from %check-batch
                    (%batch-response nil :status error-status :selection-complete nil
                                     :message message)))
                (setf targets items))))
          (when progress (funcall progress nil targets nil))
          (multiple-value-bind (results reuse)
              (%run-targets api targets options deadline specials settings progress)
            (%batch-response results :reuse reuse)))))))

(defun %wrapped-batch (wrapper options deadline targets)
  (let ((snapshot (list nil targets nil (not (getf options :symbol)))))
    (multiple-value-bind (value status leaked)
        (call-with-check-deadline
         (lambda ()
           (%check-batch options deadline targets
                         (lambda (results pending active)
                           (setf snapshot (list results pending active t)))))
         (%remaining deadline) :wrapper wrapper :name "mcp-spec-batch-around")
      (if (eq status :ok)
          (first value)
          (destructuring-bind (results pending active selected) snapshot
            (let* ((timeoutp (eq status :timeout))
                   (rows (append (reverse results)
                                 (when active
                                   (list (%unexecuted-item
                                          (pop pending) (if timeoutp "timeout" "internal-error"))))
                                 (mapcar (lambda (target) (%unexecuted-item target "not-run"))
                                         pending))))
              (%batch-response rows
                               :status (cond ((not timeoutp) "internal-error")
                                             (selected "incomplete")
                                             (t "timeout"))
                               :selection-complete selected
                               :reuse (if leaked "unsafe" "unknown")
                               :message (if timeoutp
                                            "The deadline expired during around_function or its checks."
                                            (check-wrapper-message value)))))))))

(defun spec-batch-response (params)
  "Return the MCP response for a selection of checks, with optional wrapper setup."
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (options error) (%options params)
      (when error
        (return-from spec-batch-response
          (%batch-response nil :status "invalid-arguments" :selection-complete nil :message error)))
      (multiple-value-bind (wrapper wrapper-error)
          (resolve-check-wrapper (gethash "around_function" params))
        (when wrapper-error
          (return-from spec-batch-response
            (%batch-response nil :status "invalid-arguments" :selection-complete nil
                             :message wrapper-error)))
        (setf (getf options :around-function) (gethash "around_function" params))
        (let* ((deadline (+ start (* (getf options :timeout) internal-time-units-per-second)))
               (package (getf options :package))
               (targets (append (%targets :contract (getf options :functions) package)
                                (%targets :property (getf options :properties) package))))
          (if (and wrapper (plusp (%remaining deadline)))
              (%wrapped-batch wrapper options deadline targets)
              (%check-batch options deadline targets nil)))))))
