;;;; tests/test-runner-test.lisp

(defpackage #:cl-mcp/tests/test-runner-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:signals)
  (:import-from #:cl-mcp/src/test-runner
                #:run-tests
                #:detect-test-framework)
  ;; Load clhs-test system so we can use it as a test subject
  ;; NOTE: Do NOT import from helper test packages (test-runner-test-failures, etc.)
  ;; as that would register their intentionally-failing tests with Rove
  (:import-from #:cl-mcp/tests/clhs-test)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-run-tests-response))

(in-package #:cl-mcp/tests/test-runner-test)

;;; ---------------------------------------------------------------------------
;;; Framework Detection Tests
;;; ---------------------------------------------------------------------------

(deftest detect-test-framework-finds-rove
  (testing "detect-test-framework returns :rove when rove is loaded"
    ;; Rove is loaded since we're using it for tests
    (ok (eq :rove (detect-test-framework "any-system")))))

;;; ---------------------------------------------------------------------------
;;; Result Structure Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-returns-hash-table
  (testing "run-tests returns a hash table"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      (ok (hash-table-p result)))))

(deftest run-tests-contains-required-fields
  (testing "run-tests result contains required structured fields"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      (ok (integerp (gethash "passed" result)))
      (ok (integerp (gethash "failed" result)))
      (ok (integerp (gethash "duration_ms" result)))
      (ok (string= "rove" (gethash "framework" result)))
      (let ((failures (gethash "failed_tests" result)))
        (ok (vectorp failures) "failed_tests should be an array")
        (ok (= 0 (length failures))
            "successful suite should return empty failed_tests")))))

(deftest run-tests-contains-duration
  (testing "run-tests result contains duration_ms"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      (ok (gethash "duration_ms" result))
      (ok (numberp (gethash "duration_ms" result))))))

;;; ---------------------------------------------------------------------------
;;; Passing Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-reports-passing-tests
  (testing "run-tests correctly reports passing tests"
    (let ((result (run-tests "cl-mcp/tests/clhs-test")))
      ;; clhs-test should pass
      (ok (>= (gethash "passed" result) 0))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-captures-stdout
  (testing "run-tests includes stdout from test execution"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-stdout")))
      (ok (= 0 (gethash "failed" result)) "Helper test should pass")
      (let ((stdout (gethash "stdout" result)))
        (ok (stringp stdout) "stdout should be present as a string")
        (ok (search "DEBUG-MARKER-12345" stdout)
            "stdout should contain the debug output from the test")))))

(deftest run-tests-selected-captures-stdout
  (testing "run-tests with :test captures stdout"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-stdout"
                             :test "cl-mcp/tests/test-runner-test-stdout::stdout-capture-test")))
      (ok (= 0 (gethash "failed" result)))
      (let ((stdout (gethash "stdout" result)))
        (ok (stringp stdout) "stdout should be present")
        (ok (search "DEBUG-MARKER-12345" stdout)
            "stdout should contain the debug output")))))

(deftest run-tests-captures-debug-output
  (testing "run-tests includes debug_output from *test-debug-output* stream"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-debug-output")))
      (ok (= 0 (gethash "failed" result)) "Helper test should pass")
      (let ((debug-out (gethash "debug_output" result)))
        (ok (stringp debug-out) "debug_output should be present as a string")
        (ok (search "DEBUG-STREAM-MARKER-98765" debug-out)
            "debug_output should contain the debug stream output")))))

(deftest run-tests-selected-captures-debug-output
  (testing "run-tests with :test captures debug_output"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-debug-output"
                             :test "cl-mcp/tests/test-runner-test-debug-output::debug-output-capture-test")))
      (ok (= 0 (gethash "failed" result)))
      (let ((debug-out (gethash "debug_output" result)))
        (ok (stringp debug-out) "debug_output should be present")
        (ok (search "DEBUG-STREAM-MARKER-98765" debug-out)
            "debug_output should contain the debug stream output")))))

(deftest run-tests-content-text-excludes-stdout
  (testing "content text does not contain raw stdout (kept in structured field only)"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-stdout"))
           (resp (build-run-tests-response result))
           (text (gethash "text" (aref (gethash "content" resp) 0))))
      (ok (search "DEBUG-MARKER-12345" (gethash "stdout" resp))
          "stdout structured field should contain the marker")
      (ok (not (search "DEBUG-MARKER-12345" text))
          "content text should not contain raw stdout"))))

(deftest run-tests-content-text-includes-debug-output
  (testing "content text includes debug_output from *test-debug-output*"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-debug-output"))
           (resp (build-run-tests-response result))
           (text (gethash "text" (aref (gethash "content" resp) 0))))
      (ok (search "DEBUG-STREAM-MARKER-98765" text)
          "content text should include debug output"))))

;;; ---------------------------------------------------------------------------
;;; Failure Details Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-captures-failure-details
  (testing "run-tests captures failure details for failed tests"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-failures")))
      (ok (> (gethash "failed" result) 0) "Should have failures")
      (let ((failures (gethash "failed_tests" result)))
        (ok (vectorp failures) "failed_tests should be an array")
        (ok (> (length failures) 0) "Should have at least one failure")
        (let ((first-failure (aref failures 0)))
          (ok (gethash "test_name" first-failure)
              "Failure should include test_name")
          (multiple-value-bind (reason presentp)
              (gethash "reason" first-failure)
            (ok (or (not presentp) (stringp reason))
                "Failure reason should be absent or a string")))))))

(deftest run-tests-failure-reason-is-string
  (testing "run-tests converts error conditions to strings in failure reason"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-failures")))
      (let* ((failures (gethash "failed_tests" result))
             (failure (aref failures 0))
             (reason (gethash "reason" failure)))
        ;; reason may be nil for assertion failures, but if present must be string
        (ok (or (null reason) (stringp reason))
            "Reason should be nil or a string, not a condition object")))))

;;; ---------------------------------------------------------------------------
;;; Error Handling During Test Execution
;;; ---------------------------------------------------------------------------

(deftest run-tests-handles-error-during-execution
  (testing "run-tests captures errors signaled during test execution"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-error")))
      (ok (= 0 (gethash "passed" result)) "Should have no passed tests")
      (ok (= 1 (gethash "failed" result)) "Should have one failed test")
      (let* ((failures (gethash "failed_tests" result))
             (failure (aref failures 0))
             (reason (gethash "reason" failure)))
        (ok (stringp reason) "Reason should be a string, not a condition object")))))

(deftest run-tests-handles-undefined-function
  (testing "run-tests captures undefined function errors"
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-undefined")))
      (ok (= 0 (gethash "passed" result)) "Should have no passed tests")
      (ok (= 1 (gethash "failed" result)) "Should have one failed test")
      (let* ((failures (gethash "failed_tests" result))
             (failure (aref failures 0))
             (reason (gethash "reason" failure)))
        (ok (stringp reason) "Reason should be a string")))))

;;; ---------------------------------------------------------------------------
;;; Error Handling Tests - Missing Suite
;;; ---------------------------------------------------------------------------

(deftest run-tests-errors-on-missing-suite
 (testing "run-tests reports load-error framework for non-existent suite"
  (let ((result (run-tests "non-existent-test-suite-xyz")))
    (ok (hash-table-p result))
    (ok (string= "load-error" (gethash "framework" result))
     "framework should be load-error when suite cannot be loaded")
    (ok (>= (gethash "failed" result) 1)
     "load failure is reported as at least one failed test")
    (ok (zerop (gethash "passed" result))
     "no passes when the suite cannot be loaded"))))

;;; ---------------------------------------------------------------------------
;;; Framework Parameter Tests
;;; ---------------------------------------------------------------------------

(deftest run-tests-accepts-framework-parameter
  (testing "run-tests accepts framework parameter"
    ;; Force rove framework
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :framework "rove")))
      (ok (string= "rove" (gethash "framework" result))))))

(deftest run-tests-asdf-fallback
  (testing "run-tests falls back to asdf and keeps structured response fields"
    ;; Force unknown framework - should fall back to asdf
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :framework "unknown")))
      (ok (string= "asdf" (gethash "framework" result)))
      (ok (integerp (gethash "passed" result)))
      (ok (integerp (gethash "failed" result)))
      (ok (integerp (gethash "duration_ms" result)))
      (ok (vectorp (gethash "failed_tests" result)))
      (ok (member (gethash "success" result) '(t nil))))))

(deftest run-tests-single-test-runs-only-target
  (testing "run-tests runs only the specified single test"
    (let ((result (run-tests "cl-mcp/tests/clhs-test"
                             :test "cl-mcp/tests/clhs-test::clhs-lookup-symbol-with-hyphen")))
      (ok (= 1 (gethash "passed" result)))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-single-test-loads-target-system-package
  (testing "run-tests loads the target test system before selective execution"
    (let ((result (run-tests
                   "cl-mcp/tests/utils-strings-test"
                   :framework "rove"
                   :test
                   "cl-mcp/tests/utils-strings-test::ensure-trailing-newline-adds-newline")))
      (ok (= 1 (gethash "passed" result)))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-tests-array-runs-selected-tests
  (testing "run-tests runs only tests listed in :tests"
    (let ((result (run-tests "cl-mcp/tests/clhs-test"
                             :tests '("cl-mcp/tests/clhs-test::clhs-lookup-symbol-with-hyphen"
                                      "cl-mcp/tests/clhs-test::clhs-lookup-format-as-symbol"))))
      (ok (= 2 (gethash "passed" result)))
      (ok (= 0 (gethash "failed" result))))))

(deftest run-tests-framework-auto-detects
  (testing "run-tests treats framework=auto as automatic detection"
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :framework "auto")))
      (ok (string= "rove" (gethash "framework" result))))))

(deftest run-tests-rejects-test-and-tests-together
  (testing "run-tests signals error when test and tests are both provided"
    (ok (signals (run-tests "cl-mcp/tests/clhs-test"
                            :test "cl-mcp/tests/clhs-test::clhs-lookup-symbol-with-hyphen"
                            :tests '("cl-mcp/tests/clhs-test::clhs-lookup-format-as-symbol"))))))

(deftest run-tests-tests-array-rejects-nil-element
  (testing "run-tests rejects NIL entries in :tests"
    (ok (signals (run-tests "cl-mcp/tests/clhs-test"
                            :tests '(nil))))))

(deftest run-tests-failure-includes-assertion-details
  (testing "run-tests includes description, form, and values in failure details"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-failures"))
           (failures (gethash "failed_tests" result))
           (failure (aref failures 0)))
      (ok (> (length failures) 0) "Should have failures")
      (ok (gethash "test_name" failure) "Should have test_name")
      ;; These come from (ok (= 1 2) "1 should equal 2") in the helper
      (let ((desc (gethash "description" failure)))
        (ok (stringp desc) "Should include assertion description")
        (ok (search "1 should equal 2" desc)
            "Description should contain the ok message"))
      (let ((form (gethash "form" failure)))
        (ok (stringp form) "Should include assertion form")
        (ok (search "= 1 2" form)
            "Form should contain the assertion expression"))
      ;; reason may be NIL for simple (ok ...) assertions; just check it doesn't error
      (ok (or (null (gethash "reason" failure))
              (stringp (gethash "reason" failure)))
          "reason should be nil or a string"))))

(deftest run-tests-handles-direct-assertion-failures
  (testing "run-tests handles failures from direct assertions without (testing ...) wrapper"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-direct-assertion"))
           (failures (gethash "failed_tests" result)))
      (ok (> (gethash "failed" result) 0) "Should have failures")
      (ok (> (length failures) 0) "Should have failure details")
      (let ((failure (aref failures 0)))
        (ok (gethash "test_name" failure) "Should have test_name")
        (let ((desc (gethash "description" failure)))
          (ok (stringp desc) "Should include assertion description")
          (ok (search "3 should equal 4" desc)
              "Description should contain the ok message"))
        (let ((form (gethash "form" failure)))
          (ok (stringp form) "Should include assertion form"))))))

(deftest ensure-system-loaded-reloads-system
  (testing "%%ensure-system-loaded clears and reloads so ASDF re-checks timestamps"
    (let ((system-name "cl-mcp/tests/clhs-test"))
      ;; Ensure the system is loaded first
      (asdf:load-system system-name)
      ;; Call the function under test — it should clear+load without error
      (cl-mcp/src/test-runner-core::%ensure-system-loaded system-name)
      ;; System should still be findable after the clear+load cycle
      (ok (asdf:find-system system-name nil)
          "System is loaded after %%ensure-system-loaded"))))

(deftest rove-purge-ghost-suites-removes-stale-tests
  (testing "%rove-purge-ghost-suites removes deftest entries for test packages"
    ;; Setup: write a tiny throwaway test system with two deftests,
    ;; load it, verify both are registered, then edit the file to
    ;; remove the second deftest and load again through
    ;; %ensure-system-loaded. Without the purge fix, Rove would still
    ;; remember the deleted deftest and continue running it. With the
    ;; fix, the registry is cleared before reload and only the current
    ;; source is honored.
    ;;
    ;; NOTE: A 1.1s sleep between the two source writes is required
    ;; because POSIX file-modification timestamps on most Linux
    ;; filesystems have 1-second resolution. Without it, ASDF's
    ;; FASL-freshness check sees the rewritten source as not-newer-
    ;; than the cached FASL and skips recompilation — which would
    ;; mask the purge+reload behavior we are trying to verify.
    ;; In real usage the edit-to-rerun interval always exceeds 1s.
    (let* ((tmp-dir
             (uiop:ensure-directory-pathname
              (uiop:merge-pathnames*
               (format nil "cl-mcp-ghost-test-~A/" (get-universal-time))
               (uiop:temporary-directory))))
           (asd-path (uiop:merge-pathnames* "ghost-test-sys.asd" tmp-dir))
           (src-path (uiop:merge-pathnames* "ghost-test-body.lisp" tmp-dir))
           (test-pkg-name "GHOST-TEST-SYS/SUITE")
           (system-name "ghost-test-sys"))
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output :if-exists :supersede)
               (format s "(asdf:defsystem ~S~%  :depends-on (:rove)~%  :components ((:file \"ghost-test-body\")))~%"
                       system-name))
             (with-open-file (s src-path :direction :output :if-exists :supersede)
               (format s "(defpackage #:~A~%  (:use #:cl #:rove))~%"
                       test-pkg-name)
               (format s "(in-package #:~A)~%" test-pkg-name)
               (format s "(deftest alive-test (ok t))~%")
               (format s "(deftest ghost-test (ok (= 1 2)))~%"))
             (asdf:load-asd asd-path)
             (asdf:load-system system-name)
             (let* ((suite-fn (find-symbol "PACKAGE-SUITE"
                                           :rove/core/suite/package))
                    (tests-fn (find-symbol "SUITE-TESTS"
                                           :rove/core/suite/package))
                    (suite-before (funcall suite-fn test-pkg-name))
                    (tests-before (funcall tests-fn suite-before)))
               (ok (= 2 (length tests-before))
                   "both alive-test and ghost-test should be registered initially")
               ;; Ensure mtime bump is visible to ASDF's second-resolution check.
               (sleep 1.1)
               ;; Rewrite the source with ghost-test REMOVED.
               (with-open-file (s src-path :direction :output :if-exists :supersede)
                 (format s "(defpackage #:~A~%  (:use #:cl #:rove))~%"
                         test-pkg-name)
                 (format s "(in-package #:~A)~%" test-pkg-name)
                 (format s "(deftest alive-test (ok t))~%"))
               ;; Reload via the function we patched. This should PURGE
               ;; the suite first, then load, leaving only alive-test.
               (cl-mcp/src/test-runner-core::%ensure-system-loaded system-name)
               (let* ((suite-after (funcall suite-fn test-pkg-name))
                      (tests-after (funcall tests-fn suite-after)))
                 (ok (= 1 (length tests-after))
                     "only alive-test should remain after purge+reload")
                 (ok (find (find-symbol "ALIVE-TEST" test-pkg-name)
                           tests-after)
                     "alive-test should still be present")
                 (ok (not (find (find-symbol "GHOST-TEST" test-pkg-name)
                                tests-after))
                     "ghost-test must not linger after source removal"))))
        ;; Cleanup
        (ignore-errors (asdf:clear-system system-name))
        (ignore-errors (let ((p (find-package test-pkg-name)))
                         (when p (delete-package p))))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

(deftest format-load-error-includes-compiler-output
  (testing "no compiler output: message is just the base error"
    (let ((msg (cl-mcp/src/test-runner-core::%format-load-error
                "my-system"
                (make-condition 'simple-error
                                :format-control "base error"
                                :format-arguments nil)
                "")))
      (ok (search "my-system" msg))
      (ok (search "base error" msg))
      (ok (null (search "Compiler output" msg)))))
  (testing "with compiler output: tail is appended under a clear header"
    (let* ((stderr (with-output-to-string (s)
                     (dotimes (i 60)
                       (format s "line ~D of compiler output~%" i))))
           (msg (cl-mcp/src/test-runner-core::%format-load-error
                 "my-system"
                 (make-condition 'simple-error
                                 :format-control "compile-file-error"
                                 :format-arguments nil)
                 stderr)))
      (ok (search "my-system" msg))
      (ok (search "compile-file-error" msg))
      (ok (search "Compiler output" msg))
      ;; Keeps the most recent lines, not the earliest ones
      (ok (search "line 59" msg))
      ;; Truncated: line 0 should be gone (*load-error-tail-max-lines* = 40)
      (ok (null (search "line 0 of" msg))))))

(deftest run-tests-load-failure-returns-structured-result
  (testing "compile error during system load surfaces as load-error result"
    (let* ((tmp-dir
            (uiop:ensure-directory-pathname
             (uiop:merge-pathnames*
              (format nil "cl-mcp-load-fail-~A-~A/"
                      (get-universal-time) (random 100000))
              (uiop:temporary-directory))))
           (asd-path (uiop:merge-pathnames* "broken-loadfail-sys.asd" tmp-dir))
           (src-path (uiop:merge-pathnames* "broken-loadfail.lisp" tmp-dir))
           (system-name "broken-loadfail-sys"))
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output :if-exists :supersede)
               (format s "(asdf:defsystem ~S~%  :components ((:file \"broken-loadfail\")))~%"
                       system-name))
             (with-open-file (s src-path :direction :output :if-exists :supersede)
               (format s "(defpackage #:broken-loadfail (:use #:cl))~%")
               (format s "(in-package #:broken-loadfail)~%")
               (format s "(defun oops ("))
             (asdf:load-asd asd-path)
             (let ((result (run-tests system-name)))
               (ok (= 0 (gethash "passed" result)))
               (ok (= 1 (gethash "failed" result)))
               (ok (string= "load-error" (gethash "framework" result))
                   "framework field marks the failure category")
               (let* ((fails (gethash "failed_tests" result))
                      (first (and (vectorp fails)
                                  (plusp (length fails))
                                  (aref fails 0))))
                 (ok first "failed_tests has at least one entry")
                 (when first
                   (ok (string= "SYSTEM-LOAD" (gethash "test_name" first))
                       "synthetic test_name is SYSTEM-LOAD")
                   (ok (search "pool-kill-worker" (gethash "reason" first))
                       "reason carries the recovery hint")
                   (ok (search system-name (gethash "description" first))
                       "description names the offending system")))))
        (ignore-errors (asdf:clear-system system-name))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

(deftest build-run-tests-response-uses-load-failed-banner
  (testing "load-error framework renders as ✗ LOAD FAILED in summary"
    (let* ((result
            (cl-mcp/src/test-runner-core::make-load-failure-result
             "some-system"
             (make-condition 'simple-error
                             :format-control "boom"
                             :format-arguments nil)))
           (response (build-run-tests-response result))
           (content (gethash "content" response))
           (text (when (and (vectorp content) (plusp (length content)))
                   (gethash "text" (aref content 0)))))
      (ok text "response has content text")
      (when text
        (ok (search "LOAD FAILED" text)
            "summary uses LOAD FAILED banner instead of generic FAIL")
        (ok (search "SYSTEM-LOAD" text)
            "summary lists the synthetic SYSTEM-LOAD failure")
        (ok (search "pool-kill-worker" text)
            "recovery hint surfaces in the rendered text")))))
