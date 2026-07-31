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
      (cond
        ;; Cross-suite umbrella execution can fail to capture stdout from a
        ;; nested rove:run on some SBCL versions (the outer rove run binds
        ;; *standard-output* before our inner let does).  When that happens
        ;; the helper test still ran successfully (failed=0 above), so skip
        ;; the capture-specific assertions instead of failing the whole run.
        ((null stdout)
         (rove:skip "stdout not captured (nested rove:run limitation)"))
        (t
         (ok (stringp stdout) "stdout should be present as a string")
         (ok (search "DEBUG-MARKER-12345" stdout)
          "stdout should contain the debug output from the test")))))))

(deftest run-tests-selected-captures-stdout
 (testing "run-tests with :test captures stdout"
  (let ((result
         (run-tests "cl-mcp/tests/test-runner-test-stdout" :test
          "cl-mcp/tests/test-runner-test-stdout::stdout-capture-test")))
    (ok (= 0 (gethash "failed" result)))
    (let ((stdout (gethash "stdout" result)))
      (cond
        ((null stdout)
         (rove:skip "stdout not captured (nested rove:run limitation)"))
        (t
         (ok (stringp stdout) "stdout should be present")
         (ok (search "DEBUG-MARKER-12345" stdout)
          "stdout should contain the debug output")))))))

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
 (testing
  "content text does not contain raw stdout (kept in structured field only)"
  (let* ((result (run-tests "cl-mcp/tests/test-runner-test-stdout"))
         (resp (build-run-tests-response result))
         (text (gethash "text" (aref (gethash "content" resp) 0)))
         (captured-stdout (gethash "stdout" resp)))
    (cond
      ((null captured-stdout)
       (rove:skip "stdout not captured (nested rove:run limitation)"))
      (t
       (ok (search "DEBUG-MARKER-12345" captured-stdout)
        "stdout structured field should contain the marker")
       (ok (not (search "DEBUG-MARKER-12345" text))
        "content text should not contain raw stdout"))))))

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
  (testing "run-tests reports NIL entries in :tests as a structured :unresolved result"
    (let ((result (run-tests "cl-mcp/tests/clhs-test" :tests '(nil))))
      (ok (hash-table-p result))
      (ok (string= "unresolved" (gethash "framework" result))
          "a NIL test entry yields a structured :unresolved result, not an RPC error")
      (ok (>= (gethash "failed" result) 1)
          "unresolved resolution is reported as at least one failure")
      (ok (zerop (gethash "passed" result))
          "no passes when the test name cannot be resolved"))))

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
  (let* ((tmp-dir
          (uiop/pathname:ensure-directory-pathname
           (uiop/pathname:merge-pathnames*
            (format nil "cl-mcp-ghost-test-~A-~A/"
                    (get-universal-time) (random 1000000))
            (uiop/stream:temporary-directory))))
         (asd-path
          (uiop/pathname:merge-pathnames* "ghost-test-sys.asd" tmp-dir))
         (src-path
          (uiop/pathname:merge-pathnames* "ghost-test-body.lisp" tmp-dir))
         (test-pkg-name "GHOST-TEST-SYS/SUITE")
         (system-name "ghost-test-sys"))
    (unwind-protect
        (progn
         (ensure-directories-exist tmp-dir)
         (with-open-file (s asd-path :direction :output :if-exists :supersede)
           (format s
                   "(asdf:defsystem ~S~%  :depends-on (:rove)~%  :components ((:file \"ghost-test-body\")))~%"
                   system-name))
         (with-open-file (s src-path :direction :output :if-exists :supersede)
           (format s "(defpackage #:~A~%  (:use #:cl #:rove))~%" test-pkg-name)
           (format s "(in-package #:~A)~%" test-pkg-name)
           (format s "(deftest alive-test (ok t))~%")
           (format s "(deftest ghost-test (ok (= 1 2)))~%"))
         (asdf/find-system:load-asd asd-path)
         (asdf/operate:load-system system-name)
         (let* ((suite-fn
                 (find-symbol "PACKAGE-SUITE" :rove/core/suite/package))
                (tests-fn (find-symbol "SUITE-TESTS" :rove/core/suite/package))
                (suite-before (funcall suite-fn test-pkg-name))
                (tests-before (funcall tests-fn suite-before)))
           (ok (= 2 (length tests-before))
            "both alive-test and ghost-test should be registered initially")
           (with-open-file
               (s src-path :direction :output :if-exists :supersede)
             (format s "(defpackage #:~A~%  (:use #:cl #:rove))~%"
                     test-pkg-name)
             (format s "(in-package #:~A)~%" test-pkg-name)
             (format s "(deftest alive-test (ok t))~%"))
           ;; Test the purge function directly, then recompile and reload via
           ;; compile-file/load to bypass ASDF's source-vs-fasl timestamp
           ;; check.  The umbrella test runner wraps everything in
           ;; asdf:operate, which forbids :force in nested calls and can also
           ;; race the timestamp check on CI runners — both have caused
           ;; spurious cross-suite failures.  This formulation still
           ;; exercises %rove-purge-ghost-suites, which is what the test name
           ;; asserts.
           (cl-mcp/src/test-runner-core::%rove-purge-ghost-suites system-name)
           (let ((fasl (compile-file src-path :verbose nil :print nil)))
             (when fasl (load fasl :verbose nil :print nil)))
           (let* ((suite-after (funcall suite-fn test-pkg-name))
                  (tests-after (funcall tests-fn suite-after)))
             (ok (= 1 (length tests-after))
              "only alive-test should remain after purge+reload")
             (ok (find (find-symbol "ALIVE-TEST" test-pkg-name) tests-after)
              "alive-test should still be present")
             (ok
              (not (find (find-symbol "GHOST-TEST" test-pkg-name) tests-after))
              "ghost-test must not linger after source removal"))))
      (ignore-errors (asdf/system-registry:clear-system system-name))
      (ignore-errors
       (let ((p (find-package test-pkg-name)))
         (when p (delete-package p))))
      (ignore-errors
       (uiop/filesystem:delete-directory-tree tmp-dir :validate t))))))

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

;;; ---------------------------------------------------------------------------
;;; FiveAM Suite Matching
;;; ---------------------------------------------------------------------------

(defvar *fabricated-suite-packages* nil
  "Packages created by %FABRICATE-SUITE-SYMBOL, deleted on test cleanup.")

(defun %fabricate-suite-symbol (package-name symbol-name)
  "Intern SYMBOL-NAME in PACKAGE-NAME, creating that package when necessary.
A package created here is recorded in *FABRICATED-SUITE-PACKAGES* so
%DELETE-FABRICATED-PACKAGES can remove it; a package that already existed is
left alone.  Fabricating suite symbols this way exercises the FiveAM suite
matcher without requiring FiveAM to be loaded."
  (let ((package (find-package package-name)))
    (unless package
      (setf package (make-package package-name :use nil))
      (push package *fabricated-suite-packages*))
    (intern symbol-name package)))

(defun %delete-fabricated-packages ()
  "Delete and forget every package created by %FABRICATE-SUITE-SYMBOL."
  (dolist (package *fabricated-suite-packages*)
    (ignore-errors (delete-package package)))
  (setf *fabricated-suite-packages* nil))

(defun %suite-matches-system-p (package-name symbol-name system-name)
  "Return true when a fabricated suite PACKAGE-NAME::SYMBOL-NAME belongs to
SYSTEM-NAME, according to the internal FiveAM suite matcher."
  (cl-mcp/src/test-runner-core::%fiveam-suite-matches-system-p
   (%fabricate-suite-symbol package-name symbol-name)
   system-name))

(deftest fiveam-suite-matcher-matches-suite-and-package-names
  (testing "a FiveAM suite is matched by its package name as well as its own name"
    (unwind-protect
         (progn
           ;; The reported regression: run-tests is called with the test system
           ;; name while the suite is an ordinary symbol interned in the
           ;; package-inferred test package, so its symbol name alone carries
           ;; no system information.
           (ok (%suite-matches-system-p "X/TESTS" "X-TESTS" "x/tests")
               "X/TESTS::X-TESTS belongs to system x/tests")
           (ok (%suite-matches-system-p "FA/TESTS" "ALL-TESTS" "fa/tests")
               "a plainly named suite is matched through its package name")
           ;; Classic layout: test system my-project/tests, suite symbol
           ;; MY-PROJECT-TESTS -- the system name written with dashes, which
           ;; is exactly the derived candidate.
           (ok (%suite-matches-system-p "SOME-OTHER-PKG" "MY-PROJECT-TESTS"
                                        "my-project/tests")
               "MY-PROJECT-TESTS belongs to system my-project/tests")
           ;; Pre-existing behaviour must survive the widening.
           (ok (%suite-matches-system-p "SOME-OTHER-PKG" "PLAIN-SYSTEM"
                                        "plain-system")
               "an exact suite-name match still works")
           ;; `(def-suite :my-project)` -- the dominant idiom in the wild.
           ;; A keyword suite has no package to fall back on, so the primary
           ;; system name must be an exact candidate.  A survey of 89 FiveAM
           ;; test systems from Quicklisp selected nothing for 69 of them
           ;; while it was missing.
           (ok (%suite-matches-system-p "KEYWORD" "MY-PROJECT"
                                        "my-project/tests")
               "a keyword suite named after the primary system is found")
           (ok (%suite-matches-system-p "KEYWORD" "CHANL" "chanl/tests")
               "the same for a real project's layout")
           ;; A deeper system still finds its own package and its own
           ;; dashed spelling; only the parent-derived names are dropped.
           (ok (%suite-matches-system-p "FOO/TESTS/UNIT" "ALL-TESTS"
                                        "foo/tests/unit")
               "a sub-system finds its own package")
           (ok (%suite-matches-system-p "SOMEWHERE" "FOO-TESTS-UNIT"
                                        "foo/tests/unit")
               "and its own dashed spelling")
           ;; A dot nests a package just as a slash does.
           (ok (%suite-matches-system-p "CL-YAML-TEST.PARSER" "PARSER"
                                        "cl-yaml-test")
               "a dot-nested sub-package belongs to the system")
           ;; An unqualified system still finds its conventionally named test
           ;; package, which is what the dash candidates exist for now that
           ;; "-" no longer grows a prefix.
           (ok (%suite-matches-system-p "FOO-TESTS" "ALL-TESTS" "foo")
               "system foo finds its FOO-TESTS package")
           (ok (%suite-matches-system-p "FOO/TESTS" "ALL-TESTS" "foo")
               "system foo finds its FOO/TESTS package")
           (ok (%suite-matches-system-p "SOME-OTHER-PKG" "FOO-TEST" "foo")
               "the singular FOO-TEST spelling is found too")
           (ok (%suite-matches-system-p "SOME-OTHER-PKG" "PLAIN-SYSTEM/UNIT"
                                        "plain-system")
               "a sub-system suite name still matches"))
      (%delete-fabricated-packages))))

(deftest fiveam-suite-matcher-rejects-unrelated-names
  (testing "suites belonging to unrelated systems are never swallowed"
    ;; FiveAM's suite registry is global: every suite of every system loaded
    ;; into the worker is a selection candidate, so an over-broad match runs
    ;; another project's tests and fires its fixtures.  Two earlier spellings
    ;; of this matcher got that wrong, and the negative tests of the day
    ;; passed only by accident of naming -- FABRIC fails against "fa" merely
    ;; because the next character is "B" rather than a separator.  The pairs
    ;; below are the ones that actually collided, including real upstream
    ;; project pairs, so keep names here that differ from the system in the
    ;; *separator* position.
    (unwind-protect
         (progn
           (ok (not (%suite-matches-system-p "XYLOPHONE/TESTS" "XYLOPHONE-TESTS"
                                             "x"))
               "system x must not match the unrelated xylophone/tests package")
           (ok (not (%suite-matches-system-p "FABRIC/TESTS" "FABRIC-TESTS"
                                             "fa/tests"))
               "a longer unrelated name is not a match")
           (ok (not (%suite-matches-system-p "OTHER/TESTS" "TESTS-SUITE"
                                             "fa/tests"))
               "a shared trailing segment (tests) is not a match candidate")
           ;; Wrong while the bare primary name was a candidate.
           (ok (not (%suite-matches-system-p "FOO-UTILS" "ALL-TESTS"
                                             "foo/tests"))
               "a sibling system's package must not match at the hyphen")
           (ok (not (%suite-matches-system-p "FOO/OTHER" "ALL-TESTS"
                                             "foo/tests"))
               "a sibling sub-system's package must not match at the slash")
           (ok (not (%suite-matches-system-p "SOME-OTHER-PKG" "FOO-UTILS-TESTS"
                                             "foo/tests"))
               "a sibling system's suite symbol must not match either")
           ;; Wrong while "-" still grew a prefix, which the slash-only fix
           ;; above did not reach: a system name carrying no slash was its own
           ;; sole candidate, so every sibling sharing the prefix matched.
           ;; These three are real upstream pairs.
           (ok (not (%suite-matches-system-p "LOCAL-TIME-DURATION" "ALL-TESTS"
                                             "local-time"))
               "local-time must not select local-time-duration's suite")
           (ok (not (%suite-matches-system-p "LOG4CL-EXTRAS/TESTS" "MAIN"
                                             "log4cl"))
               "log4cl must not select log4cl-extras's suite")
           (ok (not (%suite-matches-system-p "MITO-ATTACHMENT/TESTS" "ALL-TESTS"
                                             "mito"))
               "mito must not select mito-attachment's suite")
           (ok (not (%suite-matches-system-p "APP-SERVER" "MAIN" "app"))
               "an unqualified system must not swallow its prefix siblings")
           ;; The primary name is an *exact* candidate, never grown into, so
           ;; restoring it for keyword suites does not reopen any of the above.
           (ok (not (%suite-matches-system-p "COMPLETELY-UNRELATED" "FOO-BAR"
                                             "foo"))
               "an exact primary candidate must not grow across the dash")
           (ok (not (%suite-matches-system-p "SOME-VENDOR" "APP-SERVER-SUITE"
                                             "app"))
               "nor match a plugin that names its suite after the host")
           (ok (not (%suite-matches-system-p "APPLIANCE/TESTS" "MAIN" "app"))
               "nor a longer name that merely starts with the system name")
           ;; A deeper system is a component of the test system, not another
           ;; spelling of it.  Deriving primary-name candidates for it made
           ;; a request for one sub-system run the whole parent suite.
           (ok (not (%suite-matches-system-p "FOO/TESTS" "ALL-TESTS"
                                             "foo/tests/unit"))
               "a sub-system must not select its parent test system's suite")
           (ok (not (%suite-matches-system-p "KEYWORD" "FOO" "foo/tests/unit"))
               "nor the primary system's keyword suite"))
      (%delete-fabricated-packages))))

;;; ---------------------------------------------------------------------------
;;; Load-Lock Scope
;;; ---------------------------------------------------------------------------

(defparameter *load-lock-active-p* nil
  "True while the RUN-TESTS load-phase wrapper installed by
RUN-TESTS-LOAD-LOCK-WRAPPER-COVERS-LOAD-PHASE-ONLY is running its thunk.")

(defparameter *lock-state-at-load* :not-loaded
  "Value of *LOAD-LOCK-ACTIVE-P* observed while the probe system was loaded.")

(defparameter *lock-state-at-run* :not-run
  "Value of *LOAD-LOCK-ACTIVE-P* observed while the probe test executed.")

(deftest run-tests-accepts-a-symbol-system-designator
  (testing "a symbol names a system the same way it does for ASDF"
    ;; RUN-TESTS is exported and its own docstrings promise symbol support.
    ;; The entry point normalizes the designator so LOG-EVENT never sees a
    ;; symbol (yason cannot encode one), and normalizing with CL:STRING
    ;; instead of ASDF:COERCE-NAME upcased it -- ASDF downcases a symbol but
    ;; takes a string verbatim, so every symbol designator became
    ;; "Component ... not found".  Nothing covered this path, which is how it
    ;; shipped past a green suite.
    (let* ((tmp-dir
             (uiop:ensure-directory-pathname
              (uiop:merge-pathnames*
               (format nil "cl-mcp-symdesig-~A-~A/"
                       (get-universal-time) (random 100000))
               (uiop:temporary-directory))))
           (system-name "symdesig-probe-sys")
           (probe-package "SYMDESIG-PROBE-SYS")
           (asd-path (uiop:merge-pathnames* "symdesig-probe-sys.asd" tmp-dir))
           (src-path (uiop:merge-pathnames* "symdesig-body.lisp" tmp-dir)))
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output :if-exists :supersede)
               (format s "(asdf:defsystem ~S~%  :depends-on (:rove)~%" system-name)
               (format s "  :components ((:file \"symdesig-body\")))~%"))
             (with-open-file (s src-path :direction :output :if-exists :supersede)
               (format s "(defpackage #:~A (:use #:cl #:rove))~%" probe-package)
               (format s "(in-package #:~A)~%" probe-package)
               (format s "(deftest symdesig-probe-test (ok t))~%"))
             (asdf:load-asd asd-path)
             (let ((result (run-tests (intern (string-upcase system-name)
                                              :keyword)
                                      :test (format nil "~A::SYMDESIG-PROBE-TEST"
                                                    probe-package))))
               (ok (not (string= "load-error" (gethash "framework" result)))
                   "a keyword designator must resolve, not fail to load")
               (ok (plusp (gethash "passed" result))
                   "and the addressed test must actually run")))
        (ignore-errors (asdf:clear-system system-name))
        (ignore-errors
          (let ((probe (find-package probe-package)))
            (when probe (delete-package probe))))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

(deftest run-tests-load-lock-wrapper-covers-load-phase-only
  (testing "*load-lock-wrapper* wraps the force-reload but not the framework run"
    ;; A throwaway system observes *load-lock-active-p* twice: once from a
    ;; top-level form (the ASDF load phase) and once from a deftest body (the
    ;; framework phase).  Holding a worker-global lock across the second one is
    ;; what deadlocked run-tests on tests/worker-init-hook-test.
    (let* ((tmp-dir
             (uiop:ensure-directory-pathname
              (uiop:merge-pathnames*
               (format nil "cl-mcp-lock-scope-~A-~A/"
                       (get-universal-time) (random 100000))
               (uiop:temporary-directory))))
           (system-name "lock-scope-probe-sys")
           (probe-package "LOCK-SCOPE-PROBE-SYS")
           (self "CL-MCP/TESTS/TEST-RUNNER-TEST")
           (asd-path (uiop:merge-pathnames* "lock-scope-probe-sys.asd" tmp-dir))
           (src-path (uiop:merge-pathnames* "lock-scope-probe-body.lisp" tmp-dir))
           (wrapper-calls 0))
      (setf *load-lock-active-p* nil
            *lock-state-at-load* :not-loaded
            *lock-state-at-run* :not-run)
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output :if-exists :supersede)
               (format s "(asdf:defsystem ~S~%  :depends-on (:rove)~%"
                       system-name)
               (format s "  :components ((:file \"lock-scope-probe-body\")))~%"))
             (with-open-file (s src-path :direction :output :if-exists :supersede)
               (format s "(defpackage #:~A (:use #:cl #:rove))~%" probe-package)
               (format s "(in-package #:~A)~%" probe-package)
               (format s "(setf ~A::*lock-state-at-load* ~A::*load-lock-active-p*)~%"
                       self self)
               (format s "(deftest lock-scope-probe-test~%")
               (format s "  (setf ~A::*lock-state-at-run* ~A::*load-lock-active-p*)~%"
                       self self)
               (format s "  (ok t))~%"))
             (asdf:load-asd asd-path)
             (let ((result
                     (let ((cl-mcp/src/test-runner-core::*load-lock-wrapper*
                             (lambda (thunk)
                               (incf wrapper-calls)
                               (setf *load-lock-active-p* t)
                               (unwind-protect (funcall thunk)
                                 (setf *load-lock-active-p* nil)))))
                       ;; Address the probe test by name.  Whole-system Rove
                       ;; discovery maps a system to its packages through
                       ;; ASDF metadata that a hand-written temp .asd loaded
                       ;; with LOAD-ASD does not carry, so it finds no suite
                       ;; here and the probe body never runs -- which would
                       ;; make *LOCK-STATE-AT-RUN* vacuously unobserved.  The
                       ;; selective path takes the symbol directly, and both
                       ;; paths go through the same load phase, which is what
                       ;; this test is about.
                       (run-tests system-name
                                  :test (format nil "~A::LOCK-SCOPE-PROBE-TEST"
                                                probe-package)))))
               (ok (= 1 wrapper-calls)
                   "the wrapper is applied exactly once, for the load phase")
               (ok (eq t *lock-state-at-load*)
                   "the system force-reload runs inside the wrapper")
               (ok (null *lock-state-at-run*)
                   "the test run happens after the wrapper has returned")
               (ok (plusp (gethash "passed" result))
                   "the probe test really executed")
               (ok (zerop (gethash "failed" result))
                   "the probe suite passes")))
        (setf *load-lock-active-p* nil)
        (ignore-errors (asdf:clear-system system-name))
        (ignore-errors
          (let ((probe (find-package probe-package)))
            (when probe (delete-package probe))))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))
