;;;; tests/spec-batch-test.lisp

(defpackage #:cl-mcp/tests/spec-batch-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok #:skip)
  (:import-from #:bordeaux-threads #:current-thread #:thread-alive-p)
  (:import-from #:cl-mcp/src/utils/deadline #:wait-until)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:cl-mcp/src/tools/spec-entry #:spec-check-response)
  (:import-from #:cl-mcp/src/tools/spec-batch #:spec-batch-response))

(in-package #:cl-mcp/tests/spec-batch-test)

(declaim (optimize (debug 3) (safety 3)))

(defun batch (&rest keys-and-values)
  (spec-batch-response (apply #'make-ht keys-and-values)))

(defun qualified (symbol)
  (format nil "~A::~A" (package-name (symbol-package symbol)) (symbol-name symbol)))

(defun values-of (response key)
  (map 'list (lambda (item) (gethash key item)) (gethash "results" response)))

(defun call-with-registry (thunk &key self-specs)
  ;; cl-mcp does not require cl-spec. Bind a private registry before loading
  ;; the optional self bundle, which installs declarations at load time.
  (when (or (not (asdf:find-system "cl-spec/check-it" nil))
            (and self-specs (not (asdf:find-system "cl-spec/specs" nil))))
    (return-from call-with-registry (skip "The optional cl-spec test systems are unavailable.")))
  (asdf:load-system "cl-spec/check-it")
  (progv (list (find-symbol "*REGISTRY*" "CL-SPEC"))
         (list (uiop:symbol-call :cl-spec :make-hash-table-registry))
    (when self-specs
      (asdf:load-system "cl-spec/specs")
      (unless (every (lambda (name)
                       (let ((symbol (find-symbol name "CL-SPEC/SPECS")))
                         (and symbol (fboundp symbol))))
                     '("REGISTER-SPECIFICATIONS" "CONTRACT-NAMES" "PROPERTY-NAMES"))
        (return-from call-with-registry (skip "This cl-spec predates the public self-spec set.")))
      (when (and (eq self-specs :validate-cases)
                 (not (find-symbol "VALIDATE-CASES-CLASSIFY-ADMITTED-AND-REFUSED" "CL-SPEC/SPECS")))
        (return-from call-with-registry (skip "This cl-spec predates the validate case property.")))
      (uiop:symbol-call :cl-spec/specs :register-specifications))
    (funcall thunk)))

(defun register-property (name thunk)
  ;; Construct the same public PROPERTY instance as DEFPROPERTY, keeping
  ;; cl-spec an optional runtime dependency of this test system.
  (uiop:symbol-call :cl-spec :register-property
                    (make-instance (find-symbol "PROPERTY" "CL-SPEC")
                                   :name name :arguments nil :targets nil
                                   :trials '(:smoke 1 :normal 1)
                                   :body '(t) :source-form (list 'batch-fixture name)
                                   :function thunk)))

(deftest batch-validates-the-selection-and-budgets
  (ok (equal "invalid-arguments" (gethash "status" (spec-batch-response nil))))
  (dolist (params (list nil
                        (list "symbol" "cl:car" "functions" #())
                        (list "functions" "cl:car")
                        (list "properties" #("cl:car" 12))
                        (list "functions" #("cl:car") "trials" 0)
                        (list "properties" #("cl:car") "seed" 42)
                        (list "properties" #("cl:car") "max_value_chars" -1)
                        (list "properties" #("cl:car") "timeout_seconds" "60")))
    (let ((response (apply #'batch params)))
      (ok (equal "invalid-arguments" (gethash "status" response)))
      (ok (eq yason:false (gethash "verified" response))))))

(defvar *test-environment* nil
  "The connection-like binding that wrapped checks must see.")

(defun call-with-test-environment (check)
  "Bind a fresh environment for the entire check."
  (let ((*test-environment* (list (current-thread))))
    (funcall check)))

(defun call-check-twice (check)
  "Violate the wrapper protocol after one call."
  (funcall check)
  (ignore-errors (funcall check)))

(deftest spec-check-and-batch-run-in-one-test-environment
  (call-with-registry
   (lambda ()
     (let ((environments '()))
       (dolist (name '(first-wrapped-check second-wrapped-check))
         (register-property name
                            (lambda ()
                              (push *test-environment* environments)
                              (and *test-environment*
                                   (eq (current-thread) (first *test-environment*))))))
       (let* ((name (qualified 'call-with-test-environment))
              (single (spec-check-response
                       (make-ht "property" (qualified 'first-wrapped-check)
                                "around_function" name "seed" "42")))
              (report (batch "properties" (vector (qualified 'first-wrapped-check)
                                                   (qualified 'second-wrapped-check))
                             "around_function" name "seed" "42"))
              (replay (gethash "replay" (elt (gethash "results" report) 0))))
         (ok (equal "passed" (gethash "status" (elt (gethash "results" single) 0))))
         (ok (equal '("passed" "passed") (values-of report "status")))
         (ok (and (= 3 (length environments)) (first environments)
                  (eq (first environments) (second environments))
                  (not (eq (second environments) (third environments)))))
         (ok (equal name (gethash "around_function" replay)))
         (ok (search (format nil "around_function=~A" name)
                     (gethash "text" (elt (gethash "content" single) 0))))
         (ok (null *test-environment*)))))))

(deftest an-around-function-may-run-the-check-only-once
  (call-with-registry
   (lambda ()
     (let ((count 0))
       (register-property 'counted-check (lambda () (incf count) t))
       (let ((report (batch "properties" (vector (qualified 'counted-check))
                            "around_function" (qualified 'call-check-twice))))
         (ok (= 1 count))
         (ok (equal "internal-error" (gethash "status" report)))
         (ok (search "around_function" (gethash "message" report)))
         (ok (eq yason:false (gethash "verified" report))))))))

(defvar *finished-wrapper-cleanup* nil
  "Whether the deadline unwound through the wrapper.")

(defun call-with-test-cleanup (check)
  "Record wrapper cleanup even when a check times out."
  (unwind-protect (funcall check)
    (setf *finished-wrapper-cleanup* t)))

(deftest wrapper-timeouts-retain-check-progress
  (call-with-registry
   (lambda ()
     (let ((visits '())
           (wrapper (qualified 'call-with-test-cleanup)))
       (register-property 'wrapped-fast (lambda () (push :first visits) t))
       (register-property 'wrapped-slow (lambda () (push :slow visits) (sleep 5) t))
       (register-property 'wrapped-later (lambda () (push :later visits) t))
       (setf *finished-wrapper-cleanup* nil)
       (let ((report (batch "properties" (map 'vector #'qualified
                                              '(wrapped-fast wrapped-slow wrapped-later))
                            "around_function" wrapper "timeout_seconds" 1)))
         (ok (equal '(:slow :first) visits))
         (ok *finished-wrapper-cleanup*)
         (ok (equal '("passed" "timeout" "not-run") (values-of report "status")))
         (ok (equal "incomplete" (gethash "status" report)))
         (ok (equal "unknown" (gethash "worker_reuse" report)))
         (ok (eq yason:false (gethash "verified" report))))
       (setf *finished-wrapper-cleanup* nil)
       (let ((report (spec-check-response
                      (make-ht "property" (qualified 'wrapped-slow)
                               "around_function" wrapper "timeout_seconds" 1))))
         (ok *finished-wrapper-cleanup*)
         (ok (equal "incomplete" (gethash "status" report)))
         (ok (equal '("timeout") (values-of report "status")))
         (ok (equal "unknown" (gethash "worker_reuse" report)))
         (ok (eq yason:false (gethash "verified" report))))))))

(deftest combined-run-selects-the-contract-and-direct-properties
  (call-with-registry
   (lambda ()
     (let ((validate (find-symbol "VALIDATE" "CL-SPEC")))
       ;; The registry can contain a property named VALIDATE as well. It has
       ;; no :about relation and must be excluded from a combined selection.
       (register-property validate (lambda () (error "unselected own property")))
       (let* ((report (batch "symbol" "validate" "package" "cl-spec"
                             "trials" 1 "profile" "smoke" "seed" "00042"))
              (rows (gethash "results" report))
              (names (values-of report "name")))
         (ok (equal "completed" (gethash "status" report)))
         (ok (eq yason:false (gethash "verified" report)))
         (ok (= 3 (length rows)))
         (ok (equal (qualified validate) (first names)))
         (ok (equal '("CL-SPEC/SPECS::VALIDATE-CASES-CLASSIFY-ADMITTED-AND-REFUSED"
                      "CL-SPEC/SPECS::VALIDATION-PRESERVES-VALUES-OR-EXPLAINS-REFUSAL")
                    (sort (copy-list (rest names)) #'string<)))
         (ok (equal '("contract" "property" "property") (values-of report "kind")))
         (loop for row across rows
               for replay = (gethash "replay" row)
               for contract = (equal "contract" (gethash "kind" row))
               do (ok (equal "passed" (gethash "status" row)))
                  (ok (equal "42" (gethash "seed" row)))
                  (ok (stringp (gethash "definition_digest" row)))
                  (ok (equal (gethash "name" row)
                             (gethash (if contract "function" "property") replay)))
                  (ok (equal (gethash "seed" row) (gethash "seed" replay)))
                  (ok (equal (gethash "definition_digest" row)
                             (gethash "expect_definition_digest" replay)))
                  (ok (if contract
                          (and (eql 1 (gethash "trials" replay)) (not (gethash "profile" replay)))
                          (and (equal "smoke" (gethash "profile" replay))
                               (not (gethash "trials" replay)))))
                  (ok (equal "match" (gethash "definition_match"
                                              (elt (gethash "results"
                                                            (spec-check-response replay)) 0))))))))
   :self-specs :validate-cases))

(deftest explicit-bundle-excludes-unrelated-registrations-and-deduplicates-aliases
  (call-with-registry
   (lambda ()
     (let ((unrelated-calls 0)
           (functions (mapcar #'qualified (uiop:symbol-call :cl-spec/specs :contract-names)))
           (properties (mapcar #'qualified (uiop:symbol-call :cl-spec/specs :property-names))))
       (register-property 'outsider (lambda () (incf unrelated-calls) t))
       (let* ((report (batch "functions" (coerce (cons "cl-spec:validate" functions) 'vector)
                             "properties" (coerce (append properties (list (first properties)))
                                                  'vector)
                             "seed" "42" "trials" 1 "profile" "smoke" "timeout_seconds" 120))
              (counts (gethash "counts" report))
              (names (values-of report "name")))
         (ok (equal "completed" (gethash "status" report)))
         (ok (plusp (length properties)))
         (ok (plusp (length functions)))
         (ok (= (length functions) (gethash "contracts" counts)))
         (ok (= (length properties) (gethash "properties" counts)))
         (ok (= (+ (length functions) (length properties)) (length names)))
         (ok (null (set-exclusive-or (append functions properties) (copy-list names)
                                     :test #'equal)))
         (ok (every (lambda (item)
                      (and (equal "passed" (gethash "status" item))
                           (equal "42" (gethash "seed" item))
                           (hash-table-p (gethash "replay" item))))
                    (gethash "results" report)))
         (ok (zerop unrelated-calls)))))
   :self-specs t))

(deftest explicit-items-keep-distinct-kinds-and-actual-failures
  (call-with-registry
   (lambda ()
     (let ((calls 0))
       (register-property (find-symbol "VALIDATE" "CL-SPEC") (lambda () (incf calls) nil))
       (let* ((report (batch "functions" #("cl:car" "cl-spec:validate")
                             "properties" #("validate" "cl-spec:validate") "package" "cl-spec"
                             "profile" "smoke" "trials" 1))
              (failed (elt (gethash "results" report) 2)))
         (ok (equal '("not-registered" "passed" "failed") (values-of report "status")))
         (ok (equal '("contract" "contract" "property") (values-of report "kind")))
         (ok (eq yason:false (gethash "verified" report)))
         (ok (= 1 calls))
         (ok (stringp (gethash "seed" failed)))
         (ok (equal (gethash "seed" failed) (gethash "seed" (gethash "replay" failed)))))))
   :self-specs t))

(deftest batch-deadline-retains-finished-items-and-stops-before-the-next
  (call-with-registry
   (lambda ()
     (let ((calls nil) (unwound nil))
       (register-property 'first-check (lambda () (push :first calls) t))
       (register-property 'slow-check
                          (lambda ()
                            (push :slow calls)
                            (unwind-protect (sleep 5) (setf unwound t))
                            t))
       (register-property 'last-check (lambda () (push :last calls) t))
       (let* ((report (batch "properties" #("first-check" "slow-check" "last-check")
                             "package" (package-name (symbol-package 'first-check))
                             "profile" "smoke" "seed" "42" "timeout_seconds" 1))
              (rows (gethash "results" report)))
         (ok (equal '(:slow :first) calls))
         (ok unwound)
         (ok (equal '("passed" "timeout" "not-run") (values-of report "status")))
         (ok (equal "incomplete" (gethash "status" report)))
         (ok (equal "unknown" (gethash "worker_reuse" report)))
         (ok (eq yason:false (gethash "thread_leaked" report)))
         (ok (equal "42" (gethash "seed" (elt rows 0))))
         (ok (null (gethash "replay" (elt rows 2))))
         (ok (= 1 (gethash "not_run" (gethash "counts" report)))))))))

(deftest slow-registry-discovery-leaves-membership-unknown
  (call-with-registry
   (lambda ()
     (let* ((registry (symbol-value (find-symbol "*REGISTRY*" "CL-SPEC")))
            (reader (fdefinition (find-symbol "REGISTRY-PROPERTIES-FOR" "CL-SPEC")))
            (started nil)
            (unwound nil)
            ;; Specialize only on this test's registry. Other sessions and
            ;; other registries in this image can continue reading normally.
            (method (make-instance (sb-mop:generic-function-method-class reader)
                                   :qualifiers '(:before) :lambda-list '(registry name)
                                   :specializers (list (sb-mop:intern-eql-specializer registry)
                                                       (find-class 't))
                                   :function (lambda (args next)
                                               (declare (ignore args next))
                                               (setf started t)
                                               (unwind-protect (sleep 5) (setf unwound t))))))
       (add-method reader method)
       (unwind-protect
            (let ((report (batch "symbol" "cl:car" "timeout_seconds" 0.5)))
              (ok started)
              (ok unwound)
              (ok (equal "timeout" (gethash "status" report)))
              (ok (eq yason:false (gethash "selection_complete" report)))
              (ok (equalp #() (gethash "results" report)))
              (ok (equal "unknown" (gethash "worker_reuse" report))))
         (remove-method reader method))))))

(deftest batch-reports-an-unstoppable-nested-check
  (call-with-registry
   (lambda ()
     (let ((release (sb-thread:make-semaphore))
           (thread nil)
           (later-calls 0))
       (register-property
        'held-check
        (lambda ()
          (setf thread (current-thread))
          (sb-sys:without-interrupts
            (loop until (plusp (sb-thread:semaphore-count release)) do (sleep 0.02)))
          t))
       (register-property 'later-check (lambda () (incf later-calls) t))
       (unwind-protect
            (let ((report (batch "properties" #("held-check" "later-check")
                                 "package" (package-name (symbol-package 'held-check))
                                 "timeout_seconds" 0.5)))
              (ok (equal '("timeout" "not-run") (values-of report "status")))
              (ok (equal "unsafe" (gethash "worker_reuse" report)))
              (ok (eq t (gethash "thread_leaked" report)))
              (ok (zerop later-calls)))
         (sb-thread:signal-semaphore release)
         (when thread
           (ok (wait-until (lambda () (not (thread-alive-p thread))) :timeout 5))))))))

(deftest zero-budget-identifies-explicit-items-without-running
  (let ((report (batch "functions" #("cl:car" "common-lisp::car")
                       "properties" #("cl:car") "timeout_seconds" 0)))
    (ok (equal '("not-run" "not-run") (values-of report "status")))
    (ok (equal '("contract" "property") (values-of report "kind")))
    (ok (equal "safe" (gethash "worker_reuse" report)))
    (ok (eq yason:false (gethash "verified" report)))))
