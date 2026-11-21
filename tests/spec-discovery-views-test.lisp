;;;; Discovery and focused views through the same responders the MCP worker uses.

(defpackage #:cl-mcp/tests/spec-discovery-views-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok #:skip)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-list-response #:spec-describe-response #:spec-check-response)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:cl-mcp/src/spec-adapter-report #:list-report #:describe-report)
  (:import-from #:cl-mcp/src/tools/spec-response-builders #:build-spec-describe-response)
  (:import-from #:cl-mcp/specs/spec-inspection-fixtures
                #:+required-handles+ #:+listing-handles+ #:inspection-api #:calls-of
                #:definition-name #:contract-record #:contract-descriptor)
  (:import-from #:cl-mcp/tests/spec-integration-test
                #:%contracts-available-p #:%cl-spec-available-p #:+no-contracts-reason+
                #:+skip-reason+ #:with-fixture-registry #:%fixture-name #:%text))

(in-package #:cl-mcp/tests/spec-discovery-views-test)

(defmacro with-contracts (&body body)
  `(if (%contracts-available-p)
       (with-fixture-registry ,@body)
       (skip (if (%cl-spec-available-p) +no-contracts-reason+ +skip-reason+))))

(defun describe-contract (name &rest arguments)
  (spec-describe-response
   (apply #'make-ht "kind" "function-spec" "name" (%fixture-name name) arguments)))

(deftest public-exports-use-exact-symbol-identity-for-every-kind
  (with-contracts
    (let* ((home (find-package "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
           (public (make-package (gensym "MCP-PUBLIC-") :use (list home)))
           (package (package-name public)))
      (unwind-protect
           (progn
             ;; CLAMP is an external homonym, WIDEN is inherited and GROW-BY-NOTHING
             ;; is imported but internal. Only the three explicit re-exports qualify.
             (shadow "CLAMP" public)
             (export (find-symbol "CLAMP" public) public)
             (import (find-symbol "GROW-BY-NOTHING" home) public)
             (dolist (name '("MAGNITUDE" "SMALL-INT" "CLAMP-IS-IDEMPOTENT"))
               (export (find-symbol name home) public))
             (let* ((home-view (spec-list-response (make-ht "package" package)))
                    (external (spec-list-response
                               (make-ht "package" package "package_mode" "external"
                                        "detail" "compact")))
                    (text (%text external)))
               (ok (= 0 (gethash "function_specs" (gethash "counts" home-view))))
               (ok (equal "external" (gethash "package_mode" (gethash "filters" external))))
               (ok (equal "compact" (gethash "detail" external)))
               (loop for key in '("specs" "properties" "function_specs")
                     for name in '("SMALL-INT" "CLAMP-IS-IDEMPOTENT" "MAGNITUDE")
                     for qualified = (%fixture-name name)
                     do (ok (equal (list qualified) (coerce (gethash key external) 'list)))
                        (ok (eql 1 (gethash key (gethash "counts" external))))
                        (ok (search qualified text)))
               (ok (search "export" text :test #'char-equal))
               (ok (null (search "The magnitude is never negative" text)))
               (ok (null (search (%fixture-name "WIDEN") text)))))
        (delete-package public)))))

(deftest compact-list-retains-counts-and-limit
  (with-contracts
    (let* ((params (make-ht "kind" "function-specs" "limit" 1))
           (full (spec-list-response params)))
      (setf (gethash "detail" params) "compact")
      (let* ((compact (spec-list-response params))
             (name (gethash "qualified" (gethash "name" (elt (gethash "function_specs" full) 0))))
             (counts (gethash "counts" compact)))
        (ok (= (gethash "function_specs" counts)
               (gethash "function_specs" (gethash "counts" full))))
        (ok (> (gethash "function_specs" counts) 1))
        (ok (equal (list name) (coerce (gethash "function_specs" compact) 'list)))
        (ok (eq t (gethash "truncated" compact)))
        (ok (search "Showing at most 1" (%text compact)))))))

(deftest summary-lists-the-contract-interface-and-case-outcomes
  (with-contracts
    (let* ((view (describe-contract "REMAINING-BALANCE" "detail" "summary"))
           (text (%text view))
           (cases (gethash "cases" view)))
      (ok (equal "summary" (gethash "detail" view)))
      (ok (equal "cases" (gethash "outcome" view)))
      (ok (eq 'yason:false (gethash "fixture_present" view)))
      (ok (equal '("BALANCE" "AMOUNT")
                 (map 'list (lambda (a) (gethash "name" (gethash "variable" a)))
                      (gethash "arguments" view))))
      (ok (equal '("sufficient-funds" "insufficient-funds")
                 (map 'list (lambda (c) (gethash "name" c)) cases)))
      (ok (equal '("returns" "signals") (map 'list (lambda (c) (gethash "outcome" c)) cases)))
      (ok (not (nth-value 1 (gethash "guard" (elt cases 0)))))
      (ok (not (nth-value 1 (gethash "core_record" view))))
      (ok (not (nth-value 1 (gethash "source_form" view))))
      (dolist (word '("BALANCE" "AMOUNT" "sufficient-funds" "insufficient-funds"
                      "returns" "signals" "fixture" "absent"))
        (ok (search word text :test #'char-equal)))
      (ok (null (search "when:" text)))
      (ok (null (search "source form:" text))))
    (let ((flat (describe-contract "MAGNITUDE" "detail" "summary")))
      (ok (equal "returns" (gethash "outcome" flat)))
      (ok (gethash "returns" flat))
      (ok (search "no named cases" (%text flat) :test #'char-equal))
      (ok (zerop (length (gethash "cases" flat)))))))

(deftest selecting-a-case-expands-only-its-own-clauses
  (with-contracts
    (let* ((view (describe-contract "REMAINING-BALANCE" "case" "sufficient-funds"))
           (cases (gethash "cases" view))
           (selected (elt cases 0))
           (text (%text view)))
      (ok (= 1 (length cases)))
      (ok (equal "sufficient-funds" (gethash "selected_case" view)))
      (ok (equal (gethash "selected_case" view) (gethash "name" selected)))
      (ok (equal "returns" (gethash "outcome" selected)))
      (ok (search "AMOUNT" (gethash "guard" selected)))
      (ok (search "RESULT" (gethash "postconditions" selected)))
      (ok (search "selected case" text :test #'char-equal))
      (ok (search (gethash "guard" selected) text))
      (ok (search (gethash "postconditions" selected) text))
      (ok (not (nth-value 1 (gethash "core_record" view))))
      (ok (null (search "insufficient-funds" text :test #'char-equal))))
    (let* ((view (describe-contract "WITHDRAW-WITHOUT-RECORDING!"
                                    "detail" "summary" "case" "sufficient-funds" "max_chars" 8))
           (selected (elt (gethash "cases" view) 0)))
      (ok (equal "sufficient-funds" (gethash "name" selected)))
      (ok (eq 'yason:false (gethash "guard_complete" selected)))
      (ok (eq 'yason:false (gethash "state_post_complete" selected)))
      (ok (gethash "state_post" selected))
      (ok (search "truncated" (%text view) :test #'char-equal)))))

(deftest case-selectors-distinguish-case-sensitive-keywords
  (with-contracts
    (let* ((summary (describe-contract "CASE-SENSITIVE-IDENTITY" "detail" "summary"))
           (full (describe-contract "CASE-SENSITIVE-IDENTITY"))
           (names (map 'list (lambda (row) (gethash "name" row)) (gethash "cases" summary))))
      (ok (equal '("ready" "READY" ":||") names))
      (ok (equal names (map 'list (lambda (row) (gethash "name" row)) (gethash "cases" full))))
      (loop for selector in names
            for guard in '("ZEROP" "PLUSP" "MINUSP")
            for answer = (describe-contract "CASE-SENSITIVE-IDENTITY" "case" selector)
            for row = (elt (gethash "cases" answer) 0)
            do (ok (search selector (%text summary)))
               (ok (equal selector (gethash "selected_case" answer)))
               (ok (equal selector (gethash "name" row)))
               (ok (search guard (gethash "guard" row)))
               (ok (search (gethash "guard" row) (%text answer)))))
    (let ((ambiguous (describe-contract "CASE-SENSITIVE-IDENTITY" "case" "rEaDy")))
      (ok (equal "invalid-arguments" (gethash "status" ambiguous)))
      (ok (search "ambiguous" (%text ambiguous) :test #'char-equal))
      (ok (search "ready" (%text ambiguous)))
      (ok (search "READY" (%text ambiguous))))))

(deftest case-coverage-summary-publishes-the-same-selectors
  (with-contracts
    (let* ((script (find-symbol "*SCRIPTED-ARGUMENTS*" "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
           (before (symbol-value script)))
      (unwind-protect
           (progn
             (setf (symbol-value script) (list '(0) '(1) '(1) '(-1)))
             (let* ((answer (spec-check-response
                             (make-ht "function" (%fixture-name "CASE-SENSITIVE-IDENTITY")
                                      "detail" "summary" "seed" "42" "trials" 4)))
                    (coverage (gethash "case_coverage" (elt (gethash "results" answer) 0))))
               (ok (equal '(("ready" 1) ("READY" 2) (":||" 1))
                          (map 'list (lambda (row)
                                       (list (gethash "name" row) (gethash "called" row)))
                               (gethash "cases" coverage))))
               (dolist (line '("ready: 1" "READY: 2" ":||: 1"))
                 (ok (search line (%text answer))))))
        (setf (symbol-value script) before)))))

(deftest compact-names-only-need-enumeration
  (multiple-value-bind (api calls)
      (inspection-api :handles (append +required-handles+
                                       (remove :function-spec-data +listing-handles+))
                      :registry '(:definitions (:contract-a :property-a)))
    (let* ((compact (list-report api :ok :kind "both" :detail "compact"))
           (counts (getf compact :counts)))
      (ok (eq :ok (getf compact :status)))
      (ok (getf compact :function-specs-listable))
      (ok (eql 1 (getf counts :function-specs)))
      (ok (eql 1 (getf counts :properties)))
      (ok (null (calls-of calls :property-data)))
      (ok (null (calls-of calls :function-spec-data))))
    (ok (eq :unsupported (getf (list-report api :ok :kind "function-specs") :status)))))

(deftest fixture-presence-and-unsupported-schemas-survive-focused-describe
  (let ((name (format nil "~{~A~^::~}" (definition-name :contract-a)))
         (record (contract-record (contract-descriptor :version 1))))
    (setf (getf record :input-kind) :fixture-recipe
          (getf record :state-constraints) :present
          (getf record :fixture)
          '(:isolation :fresh :version 1 :recipe-name scenario
            :setup ((error "unexpected setup")) :cleanup ((error "unexpected cleanup"))))
    (let* ((api (inspection-api :registry '(:definitions (:contract-a)) :contract-record record))
           (view (build-spec-describe-response
                  (describe-report api :ok "function-spec" name :detail "summary"))))
      (ok (equal "ok" (gethash "status" view)))
      (ok (eq t (gethash "fixture_present" view)))
      (ok (search "fixture present" (%text view) :test #'char-equal))
      (ok (null (search "unexpected" (%text view))))
      (setf (getf record :schema-version) 99)
      (dolist (options '(("summary" nil) ("full" "some-case")))
        (let ((answer (build-spec-describe-response
                       (describe-report api :ok "function-spec" name
                                        :detail (first options) :case (second options)))))
          (ok (equal "unsupported" (gethash "status" answer)))
          (ok (not (nth-value 1 (gethash "cases" answer)))))))))

(deftest invalid-views-name-the-option-or-available-cases
  (with-contracts
    (dolist (request
             (list (list (spec-list-response
                          (make-ht "package_mode" "accessible")) "package_mode" "external")
                   (list (spec-list-response
                          (make-ht "package_mode" "external")) "package" "external")
                   (list (spec-list-response (make-ht "detail" "summary")) "detail" "compact")
                   (list (describe-contract "CLAMP" "detail" "compact") "detail" "summary")
                   (list (describe-contract "REMAINING-BALANCE" "case" "missing")
                         "missing" "sufficient-funds")
                   (list (describe-contract "CLAMP" "case" "") "case" "non-empty")
                   (list (spec-describe-response
                          (make-ht "kind" "spec" "name" (%fixture-name "SMALL-INT")
                                   "case" "sufficient-funds")) "case" "function-spec")))
      (destructuring-bind (response &rest words) request
        (ok (equal "invalid-arguments" (gethash "status" response)))
        (dolist (word words)
          (ok (search word (%text response) :test #'char-equal)))))))
