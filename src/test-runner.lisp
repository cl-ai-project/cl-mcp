;;;; src/test-runner.lisp --- Unified test runner with structured results

(defpackage #:cl-mcp/src/test-runner
  (:use #:cl)
  (:import-from #:cl-mcp/src/test-runner-core
                #:run-tests
                #:detect-test-framework)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-run-tests-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:run-tests
           #:detect-test-framework))

(in-package #:cl-mcp/src/test-runner)

;;; ---------------------------------------------------------------------------
;;; Tool Definition
;;; ---------------------------------------------------------------------------

(define-tool "run-tests"
  :description "Run tests for a system and return structured results.

Supports multiple test frameworks with automatic detection:
- Rove: Full structured results with failure details
- FiveAM: (planned) Structured results
- ASDF fallback: Text output capture

Returns:
- content (summary text, backward compatible)
- passed (integer)
- failed (integer)
- pending (integer)
- framework (string)
- duration_ms (integer)
- failed_tests (array of objects)

Examples:
  Run all tests: system='cl-mcp/tests/clhs-test'
  Run single test: system='cl-mcp/tests/clhs-test' test='cl-mcp/tests/clhs-test::clhs-lookup-symbol-returns-hash-table'
  Run selected tests: system='cl-mcp/tests/clhs-test' tests=['cl-mcp/tests/clhs-test::clhs-lookup-symbol-returns-hash-table']"
  :args ((system :type :string :required t
                 :description "System name to test (e.g., 'my-project/tests')")
         (framework :type :string :required nil
                    :description "Force framework: 'rove', 'fiveam', 'prove', or 'auto' (default: auto-detect)")
         (test :type :string :required nil
               :description "Run only this specific test (fully qualified: 'package::test-name')")
         (tests :type :array :required nil
                :description "Run only these specific tests (array of 'package::test-name')")
         (timeout_seconds :type :number :required nil
                          :description "Maximum seconds to wait for the test run to complete (default: 300). Increase for large test suites."))
  :body
  (with-proxy-dispatch (id "worker/run-tests"
                          (make-ht "system" system
                                   "framework" framework
                                   "test" test
                                   "tests" tests
                                   "timeout_seconds" timeout_seconds))
    (let ((effective-timeout (or timeout_seconds 300)))
      (let ((test-result
              (sb-ext:with-timeout effective-timeout
                (run-tests system
                           :framework framework
                           :test test
                           :tests tests))))
        (result id (build-run-tests-response test-result))))))
