;;;; src/test-runner.lisp --- Unified test runner with structured results

(defpackage #:cl-mcp/src/test-runner
  (:use #:cl)
  (:import-from #:cl-mcp/src/test-runner-core
                #:run-tests
                #:detect-test-framework)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker
                #:*use-worker-pool*)
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
                :description "Run only these specific tests (array of 'package::test-name')"))
  :body
  (if *use-worker-pool*
      (result id
              (proxy-to-worker "worker/run-tests"
                               (make-ht "system" system
                                        "framework" framework
                                        "test" test
                                        "tests" tests)))
      ;; Fallback: inline execution
      (let* ((test-result (run-tests system
                                     :framework framework
                                     :test test
                                     :tests tests))
             (passed (gethash "passed" test-result 0))
             (failed (gethash "failed" test-result 0))
             (pending (gethash "pending" test-result 0))
             (framework-name (or (gethash "framework" test-result) "unknown"))
             (duration (gethash "duration_ms" test-result 0))
             (failed-tests (gethash "failed_tests" test-result))
             (failed-tests-vector (if (vectorp failed-tests)
                                      failed-tests
                                      (coerce (or failed-tests '()) 'vector)))
             (summary (with-output-to-string (s)
                        (format s "~A~%"
                                (if (zerop failed) "✓ PASS" "✗ FAIL"))
                        (format s "Passed: ~D, Failed: ~D~@[, Pending: ~D~]~%"
                                passed failed (when (plusp pending) pending))
                        (format s "Duration: ~Dms~%" duration)
                        (when (plusp (length failed-tests-vector))
                          (format s "~%Failures:~%")
                          (loop for fail across failed-tests-vector
                                for i from 1
                                do (format s "  ~D. ~A~%"
                                           i (gethash "test_name" fail))
                                   (when (gethash "reason" fail)
                                     (format s "     Reason: ~A~%"
                                             (gethash "reason" fail))))))))
        (let ((response (make-ht "content" (text-content summary)
                                 "passed" passed
                                 "failed" failed
                                 "pending" pending
                                 "framework" framework-name
                                 "duration_ms" duration
                                 "failed_tests" failed-tests-vector)))
          (dolist (field '("success" "stdout" "stderr" "passed_tests"))
            (multiple-value-bind (value presentp)
                (gethash field test-result)
              (when presentp
                (setf (gethash field response) value))))
          (result id response)))))
