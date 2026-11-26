;;;; tests/spec-input-test.lisp

(defpackage #:cl-mcp/tests/spec-input-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok #:skip)
  (:import-from #:cl-mcp/src/object-registry #:register-object #:lookup-object)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:cl-mcp/tests/spec-integration-test
                #:%contracts-available-p #:%fixture-name #:with-fixture-registry)
  (:import-from #:cl-mcp/tests/spec-tools-test #:%call))

(in-package #:cl-mcp/tests/spec-input-test)

(declaim (optimize (debug 3) (safety 3)))

(defun input-check (function &rest fields)
  (let* ((params (apply #'make-ht "function" function fields))
         (wire (with-output-to-string (stream) (yason:encode params stream))))
    (gethash "result" (%call "spec-check-input" wire))))

(defun field (object &rest keys)
  (reduce (lambda (node key) (and (hash-table-p node) (gethash key node)))
          keys :initial-value object))

(defun input-text (result)
  (let ((content (field result "content")))
    (if (and (vectorp content) (plusp (length content)))
        (gethash "text" (aref content 0)) "")))

(defmacro with-self-specs (&body body)
  ;; Loading and (re)registering a bundle affects the dynamically current registry.
  `(if (not (asdf:find-system "cl-spec/specs" nil))
       (skip "The optional cl-spec/specs system is unavailable")
       (progn
         (asdf:load-system "cl-spec")
         (if (not (fboundp (find-symbol "FIXTURE-CHECK-DATA" "CL-SPEC")))
             (skip "This cl-spec predates the fixture-check data API")
             (progv (list (find-symbol "*REGISTRY*" "CL-SPEC")
                          (find-symbol "*GENERATOR-BACKEND*" "CL-SPEC"))
                    (list (uiop:symbol-call :cl-spec :make-hash-table-registry) nil)
               (asdf:load-system "cl-spec/specs")
               (let ((pkg (find-package "CL-SPEC/SELF-SPEC-FIXTURES")))
                 (if (and pkg (fboundp (find-symbol "REGISTRATION-SCENARIO-ARGUMENTS" pkg)))
                     (progn
                       (uiop:symbol-call :cl-spec/specs :register-specifications)
                       ,@body)
                     (skip "This cl-spec/specs predates the registry registration fixture"))))))))

(defun wire-argument (value)
  (typecase value
    (keyword (make-ht "keyword" (symbol-name value)))
    (symbol (make-ht "symbol" (format nil "~A::~A" (package-name (symbol-package value))
                                     (symbol-name value))))
    (cons (map 'vector #'wire-argument value))
    (t (make-ht "object_id" (register-object value)))))

(deftest concrete-input-identifies-the-exact-selected-case
  (if (%contracts-available-p)
      (with-fixture-registry
        (loop for argument in '(0 1 -1)
              for selector in '("ready" "READY" ":||")
              for result = (input-check (%fixture-name "CASE-SENSITIVE-IDENTITY")
                                        "arguments" (vector argument))
              do (ok (equal "passed" (field result "status")))
                 (ok (equal selector (field result "selected_case_selector")))
                 (ok (search selector (input-text result)))))
      (skip "The optional cl-spec function contracts are unavailable.")))

(deftest caller-arguments-check-and-mutate-the-supplied-registry
  (with-self-specs
    (let* ((args (uiop:symbol-call :cl-spec/self-spec-fixtures
                                   :registration-scenario-arguments :new))
           (result (input-check "cl-spec:registry-register-property"
                                "arguments" (map 'vector #'wire-argument args)))
           (record (field result "core_result" "data")))
      (ok (equal "passed" (field result "status")))
      (ok (equal "single-input" (field result "scope")))
      (ok (equal "arguments" (field result "input_kind")))
      (ok (equal "new-registration" (field result "selected_case")))
      (ok (equal "returned" (field result "outcome" "value" "kind")))
      (ok (hash-table-p result))
      (when result (ok (nth-value 1 (gethash "failure_phase" result))))
      (ok (null (field result "failure_phase")))
      (ok (null (field result "lifecycle")))
      (ok (eql 1 (field record "schema_version")))
      (ok (equal "result" (field record "record_kind")))
      (ok (equal "direct-call" (field record "execution_mode")))
      (ok (eq (third args)
              (uiop:symbol-call :cl-spec :registry-find-property (first args) (second args))))
      (let ((values (field record "observation" "outcome" "values")))
        (ok (and (vectorp values) (= 1 (length values))
                 (eq (third args) (lookup-object (field (aref values 0) "object_id"))))))
      (ok (search "new-registration" (input-text result)))
      (ok (search "returned" (input-text result))))))

(deftest fixture-recipe-reports-owned-lifecycle-and-snapshot-availability
  (with-self-specs
    (let* ((result (input-check "registry-register-property" "package" "cl-spec"
                                "recipe" (make-ht "keyword" "new")))
           (record (field result "core_result" "data")))
      (ok (equal "passed" (field result "status")))
      (ok (equal "single-input" (field result "scope")))
      (ok (equal "recipe" (field result "input_kind")))
      (ok (equal "new-registration" (field result "selected_case")))
      (ok (equal "NEW-REGISTRATION" (field result "selected_case_selector")))
      (ok (null (field result "failure_phase")))
      (ok (equal "unavailable" (field result "outcome" "availability")))
      (ok (equal "opaque-value" (field result "outcome" "reason")))
      (ok (eql 2 (field record "schema_version")))
      (ok (equal "fixture-check" (field record "record_kind")))
      (ok (equal "fixture-recipe" (field record "input_kind")))
      (ok (equal "completed" (field result "lifecycle" "setup")))
      (ok (equal "completed" (field result "lifecycle" "evaluation")))
      (ok (equal "completed" (field result "lifecycle" "cleanup")))
      (ok (equal "released" (field result "lifecycle" "state")))
      (ok (field result "lifecycle" "target_called"))
      (ok (search "cleanup" (input-text result)))
      (ok (search "unavailable" (input-text result))))))

(deftest concrete-input-names-an-unsupported-fixture-isolation
  (let* ((record '(:schema-version 2 :record-kind :fixture-check :entity-kind :function-spec
                   :input-kind :fixture-recipe :definition-digest "abc"
                   :definition-digest-complete t :definition-digest-covers :declaration
                   :capabilities nil :fixture (:isolation :sandbox :version 1 :recipe-name recipe
                                               :setup ((list recipe)) :cleanup (nil))))
         (result (cl-mcp/src/tools/spec-input::%record-response record "example" t 200)))
    (ok (equal "unsupported" (field result "status")))
    (ok (equal "UNSUPPORTED_ISOLATION" (field result "code")))
    (ok (search "SANDBOX" (field result "message")))))

(deftest concrete-input-distinguishes-empty-arguments-scalars-and-null-recipe
  (with-self-specs
    (ok (equal "passed"
               (field (input-check "cl-spec:make-hash-table-registry" "arguments" #())
                      "status")))
    (dolist (test (list (list 'integer 42) (list 'string "abc") (list 'boolean t)
                        (list 'boolean yason:false) (list 'null nil) (list 'list #(5 "x"))))
      (let* ((spec (uiop:symbol-call :cl-spec :normalize-spec-form (first test)))
             (result (input-check "cl-spec:validate" "arguments"
                                  (vector (wire-argument spec) (second test)))))
        (ok (equal "passed" (field result "status")))
        (ok (equal "conforming" (field result "selected_case")))))
    (let ((result (input-check "cl-spec:registry-register-property" "recipe" nil)))
      (ok (equal "error" (field result "status")))
      (ok (equal "fixture" (field result "failure_phase")))
      (ok (equal "not-started" (field result "lifecycle" "setup")))
      (ok (equal "aborted" (field result "lifecycle" "completion")))
      (ok (search "recipe" (input-text result)))
      (ok (search "fixture-recipe-invalid" (input-text result))))))

(deftest invalid-concrete-input-selection-precedes-checking
  (dolist (fields (list nil (list "arguments" #() "recipe" nil)
                        (list "arguments" #() "timeout_seconds" 0)
                        (list "arguments" #() "max_value_chars" -1)))
    (ok (equal "invalid-arguments"
               (field (apply #'input-check "cl:car" fields) "status")))))

(deftest invalid-concrete-tokens-never-start-a-call
  (with-self-specs
    (dolist (fields (list (list "arguments" (vector (make-ht "keyword" 9)))
                          (list "arguments" (vector (make-ht "object_id" 12)))
                          (list "arguments" (vector (make-ht "symbol" "cl:no-such-390-symbol")))
                          (list "arguments" (vector (make-ht "keyword" "no-such-390-keyword")))
                          (list "arguments" (vector (make-ht "symbol" "cl:t" "other" 2)))
                          (list "arguments" (vector (make-ht "text" "unsupported")))
                          (list "arguments" (vector (make-ht "object_id" "malformed")))))
      (ok (equal "invalid-arguments"
                 (field (apply #'input-check "cl-spec:validate" fields) "status"))))
    (ok (not (find-symbol "NO-SUCH-390-KEYWORD" :keyword)))
    (let ((other (cl-mcp/src/object-registry::make-object-registry)))
      (let* ((handle (register-object (list :foreign) other))
             (result (input-check "cl-spec:validate" "arguments"
                                  (vector (make-ht "object_id" handle) 3))))
        (ok (equal "stale-object-id" (field result "status")))
        (ok (search "object" (input-text result)))))
    (let ((cl-mcp/src/object-registry:*object-registry*
            (cl-mcp/src/object-registry::make-object-registry)))
      (ok (equal "invalid-arguments"
                 (field (input-check "cl-spec:validate" "arguments" #(1 2)) "status")))
      (ok (zerop (cl-mcp/src/object-registry:registry-count))))))

(defun slow-predicate (value)
  (declare (ignore value))
  (sleep 2)
  t)

(deftest concrete-input-deadline-reports-uncertain-worker-state
  (with-self-specs
    (let* ((spec (uiop:symbol-call :cl-spec :normalize-spec-form '(satisfies slow-predicate)))
           (result (input-check "cl-spec:validate" "arguments" (vector (wire-argument spec) 1)
                                "timeout_seconds" 1)))
      (ok (equal "timeout" (field result "status")))
      (ok (equal "unknown" (field result "worker_reuse")))
      (ok (hash-table-p result))
      (when result (ok (nth-value 1 (gethash "thread_leaked" result))))
      (ok (not (field result "thread_leaked"))))))

(defun run-input-probe (probe)
  (funcall probe))

(deftest concrete-input-reports-unsafe-when-the-thread-survives
  (with-self-specs
    (let* ((release (sb-thread:make-semaphore))
           (thread nil)
           (probe (lambda ()
                    (setf thread sb-thread:*current-thread*)
                    (sb-sys:without-interrupts
                      (loop repeat 500 until (plusp (sb-thread:semaphore-count release))
                            do (sleep 0.02)))
                    t))
           (spec (uiop:symbol-call :cl-spec :normalize-spec-form '(satisfies run-input-probe))))
      (unwind-protect
           (let ((report (input-check "cl-spec:validate" "arguments"
                                      (vector (wire-argument spec) (wire-argument probe))
                                      "timeout_seconds" 1)))
             (ok (and thread (sb-thread:thread-alive-p thread)))
             (ok (equal "timeout" (field report "status")))
             (ok (eq t (field report "thread_leaked")))
             (ok (equal "unsafe" (field report "worker_reuse")))
             (ok (search "still running" (input-text report))))
        (sb-thread:signal-semaphore release)
        (when thread
          (sb-thread:join-thread thread :timeout 5 :default nil)
          (ok (not (sb-thread:thread-alive-p thread))))))))

(deftest concrete-input-deadline-covers-condition-reporting
  (with-self-specs
    (let* ((registry (symbol-value (find-symbol "*REGISTRY*" "CL-SPEC")))
           (reader (fdefinition (find-symbol "REGISTRY-FIND-FUNCTION-SPEC" "CL-SPEC")))
           (started nil)
           (finished nil)
           (unwound nil)
           (failure (make-condition
                     'simple-error :format-arguments nil
                     :format-control (lambda (stream)
                                       (setf started t)
                                       (unwind-protect
                                            (progn
                                              (sleep 2)
                                              (setf finished t)
                                              (write-string "registry lookup failed" stream))
                                         (setf unwound t)))))
           (method (make-instance (sb-mop:generic-function-method-class reader)
                                  :qualifiers '(:before) :lambda-list '(registry name)
                                  :specializers (list (sb-mop:intern-eql-specializer registry)
                                                      (find-class 't))
                                  :function (lambda (args next)
                                              (declare (ignore args next))
                                              (error failure)))))
      (add-method reader method)
      (unwind-protect
           (let ((result (input-check "cl-spec:validate" "arguments" #() "timeout_seconds" 1)))
             (ok started)
             (ok unwound)
             (ok (not finished))
             (ok (equal "timeout" (field result "status")))
             (ok (equal "unknown" (field result "worker_reuse")))
             (ok (nth-value 1 (gethash "thread_leaked" result)))
             (ok (not (field result "thread_leaked"))))
        (remove-method reader method)))))

(deftest unavailable-handles-and-contracts-are-distinct
  (with-self-specs
    (ok (equal "unresolved-symbol"
               (field (input-check "cl:unknown-symbol-for-390" "arguments" #()) "status")))
    (ok (equal "not-registered"
               (field (input-check "cl:car" "arguments" #()) "status")))
    (let ((cl-mcp/src/object-registry:*object-registry*
            (cl-mcp/src/object-registry::make-object-registry)))
      (let ((handle (register-object (list :old))))
        (dotimes (index cl-mcp/src/object-registry::+max-registry-size+)
          (register-object (list index)))
        (let ((result (input-check "cl-spec:validate" "arguments"
                                   (vector (make-ht "object_id" handle) 1))))
          (ok (equal "evicted-object-id" (field result "status")))
          (ok (equal "OBJECT_NOT_FOUND" (field result "code"))))))))
