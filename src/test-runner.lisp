;;;; src/test-runner.lisp --- Unified test runner with structured results

(defpackage #:cl-mcp/src/test-runner
  (:use #:cl)
  (:import-from #:cl-mcp/src/test-runner-core
                #:run-tests
                #:detect-test-framework
                #:call-with-test-run-deadline
                #:coerce-timeout-seconds
                #:make-timeout-result)
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

The system is force-reloaded from disk first, so no load-system is needed after
an edit, and test/tests names resolve against the freshly loaded packages.

Supports multiple test frameworks with automatic detection:
- Rove: Full structured results with failure details
- FiveAM: Full structured results with failure details
- Prove (prove-asdf test files): one count per assertion, with failure details
- ASDF fallback: Text output capture only.  It cannot count, so a run that
  returned without signalling is reported as RAN, RESULT UNKNOWN, never as a
  pass: read stdout

Returns:
- content (summary text, backward compatible)
- passed (integer) -- tests, not assertions (Rove and FiveAM, whether the whole
  system or a test/tests selection ran); prove counts assertions
- failed (integer) -- same unit as passed
- pending (integer) -- tests that only skipped, so checked nothing (Rove: a
  test whose only results are (skip ...), however deep in testing blocks)
- skipped_tests (array, Rove, present when any test skipped) -- test_name and
  reasons of every test that skipped anything, including one that passed on
  what it did check; also listed under 'Skipped' in the summary text.  When
  every test only skipped, the summary says ALL SKIPPED, not PASS
- framework (string)
- duration_ms (integer)
- stdout (string, present when non-empty) — captured test standard output
- stderr (string, present when non-empty) — captured test error output
- debug_output (string, present when non-empty) — output written to *test-debug-output* stream
NOTE: stdout/stderr are in structured fields only, NOT shown in the summary text.
To include debug prints in the visible summary, write to *test-debug-output*:
  (format cl-mcp/src/test-runner-core:*test-debug-output* \"debug: ~A~%\" value)
- failed_tests (array of objects with fields:)
  - test_name (string) — name of the failing test
  - description (string) — assertion description (Rove) or the test's docstring (FiveAM)
  - form (string) — the assertion form expression, printed as the test's package
    reads it (Rove, FiveAM)
  - values (array of strings) — evaluated argument values (Rove, prove)
  - reason (string) — error reason or condition message
  - source (object) — source location with file and line (Rove only; FiveAM
    records no source location for a test, so use test_name and description)

Examples:
  Run all tests: system='cl-mcp/tests/clhs-test'
  Run single test: system='cl-mcp/tests/clhs-test' test='cl-mcp/tests/clhs-test::clhs-lookup-symbol-returns-hash-table'
  Run selected tests: system='cl-mcp/tests/clhs-test' tests=['cl-mcp/tests/clhs-test::clhs-lookup-symbol-returns-hash-table']"
  :args ((system :type :string :required t
                 :description "System name to test (e.g., 'my-project/tests')")
         (framework :type :string :required nil
                    :description "Force framework: 'rove', 'fiveam', 'prove', 'asdf', or 'auto' (default: auto-detect from the system's :depends-on). Any other value runs the ASDF fallback")
         (test :type :string :required nil
               :description "Run only this specific test, written 'package::test-name' (double colon). Rove and FiveAM only; exclusive with tests")
         (tests :type :array :required nil
                :description "Run only these specific tests (array of 'package::test-name'). Rove and FiveAM only; exclusive with test")
         (timeout-seconds :type :number :json-name "timeout_seconds" :required nil
                          :description "Maximum seconds to wait for the test run to complete (default: 300; a value of 0 or below means the default). Increase for large test suites."))
  :body
  (with-proxy-dispatch (id "worker/run-tests"
                          (make-ht "system" system
                                   "framework" framework
                                   "test" test
                                   "tests" tests
                                   "timeout_seconds" timeout-seconds))
    (let ((effective-timeout (or (coerce-timeout-seconds timeout-seconds) 300)))
      (multiple-value-bind (test-result status thread-leaked)
          (call-with-test-run-deadline
           (lambda ()
             (run-tests system
                        :framework framework
                        :test test
                        :tests tests))
           effective-timeout)
        (result id
                (build-run-tests-response
                 (ecase status
                   (:ok test-result)
                   (:timeout (make-timeout-result test-result
                                                  :thread-leaked thread-leaked))
                   ;; Re-signal so real failures stay visible instead of
                   ;; being reported as a bogus test result.
                   (:error (error test-result)))))))))
