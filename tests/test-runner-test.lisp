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

(deftest detect-test-framework-prefers-the-systems-own-dependency
  (testing "a FiveAM system is detected as FiveAM even with Rove in the image"
    ;; This suite runs under Rove, so the image always has Rove loaded --
    ;; exactly the state a long-lived cl-mcp worker reaches the moment any
    ;; session touches a Rove project.  Detection used to read the image and
    ;; hand every system to the Rove backend, which found no FiveAM suite,
    ;; ran nothing, and returned passed=0 failed=0 -- rendered as a pass.
    ;; The system's own :depends-on is the only system-specific evidence.
    (let* ((tmp-dir
             (uiop:ensure-directory-pathname
              (uiop:merge-pathnames*
               (format nil "cl-mcp-detect-~A-~A/"
                       (get-universal-time) (random 100000))
               (uiop:temporary-directory))))
           (system-name "detect-probe-fiveam")
           (test-system (format nil "~A/tests" system-name))
           (asd-path (uiop:merge-pathnames*
                      (format nil "~A.asd" system-name) tmp-dir)))
      (ok (find-package :rove) "precondition: Rove is loaded in this image")
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output
                                         :if-exists :supersede)
               (format s "(asdf:defsystem ~S)~%" system-name)
               (format s "(asdf:defsystem ~S :depends-on (\"fiveam\"))~%"
                       test-system))
             (asdf:load-asd asd-path)
             (ok (eq :fiveam (detect-test-framework test-system))
                 "the declared dependency decides, not the loaded packages")
             (ok (eq :rove (detect-test-framework "no-such-system-xyzzy"))
                 "an unresolvable system still falls back to the image"))
        (ignore-errors (asdf:clear-system test-system))
        (ignore-errors (asdf:clear-system system-name))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

(deftest detect-test-framework-observes-without-building
  (testing "detection loads nothing and prints nothing"
    ;; Detection walks the dependency graph.  Resolving a name with
    ;; ASDF:FIND-SYSTEM would load the .asd, and a .asd may carry
    ;; :DEFSYSTEM-DEPENDS-ON, which compiles and loads whole systems.
    ;; Measured before the registry-only rewrite: one call for "cl-mcp/tests"
    ;; reached usocket's (:feature :usocket-iolib :iolib) branch and pulled in
    ;; six systems, 13 MB of fasls and 32k characters of compiler output --
    ;; on the run-tests path, after the ASDF load lock had been released.
    (let ((before (length (asdf:already-loaded-systems)))
          (out (make-string-output-stream))
          (err (make-string-output-stream)))
      (let ((*standard-output* out) (*error-output* err))
        (detect-test-framework "cl-mcp/tests"))
      (ok (= before (length (asdf:already-loaded-systems)))
          "detection must not load a system")
      (ok (zerop (length (get-output-stream-string out)))
          "nor write to stdout")
      (ok (zerop (length (get-output-stream-string err)))
          "nor to stderr"))))

(deftest detect-test-framework-prefers-a-direct-declaration
  (testing "a framework inherited through a library does not win"
    ;; A test system names the framework it is written against.  The
    ;; transitive set also carries whatever its libraries test with, so a
    ;; FiveAM project depending on a Rove-tested library used to tie, and the
    ;; tie went to Rove -- the wrong backend, which then ran nothing.
    (let* ((tmp-dir
             (uiop:ensure-directory-pathname
              (uiop:merge-pathnames*
               (format nil "cl-mcp-inherit-~A-~A/"
                       (get-universal-time) (random 100000))
               (uiop:temporary-directory))))
           (asd-path (uiop:merge-pathnames* "twi.asd" tmp-dir)))
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output
                                         :if-exists :supersede)
               (format s "(asdf:defsystem \"twi-lib\" :depends-on (\"rove\"))~%")
               (format s "(asdf:defsystem \"twi\" :depends-on (\"twi-lib\"))~%")
               (format s "(asdf:defsystem \"twi/tests\"~%")
               (format s "  :depends-on (\"twi\" \"fiveam\"))~%")
               (format s "(asdf:defsystem \"twi-umbrella/tests\"~%")
               (format s "  :depends-on (\"twi/tests\"))~%"))
             (handler-bind ((warning #'muffle-warning))
               (asdf:load-asd asd-path))
             (ok (eq :fiveam (detect-test-framework "twi/tests"))
                 "the direct fiveam declaration beats the inherited rove")
             (ok (eq :rove (detect-test-framework "twi-umbrella/tests"))
                 "a system declaring none falls back to the transitive set"))
        (dolist (s '("twi-umbrella/tests" "twi/tests" "twi" "twi-lib"))
          (ignore-errors (asdf:clear-system s)))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

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

(deftest run-tests-bounds-what-a-chatty-suite-costs
  (testing "a suite printing far past the limit is capped, and says so"
    ;; The limit governs what is *held*, not only what is reported.  Captured
    ;; into a STRING-OUTPUT-STREAM and truncated afterwards, a suite emitting
    ;; 40 million characters cost 367 MB of heap to report 50 KB of it, and
    ;; under a 256 MB dynamic space the run died with HEAP-EXHAUSTED-ERROR
    ;; while materialising the string -- in a fifth of a second, so the run
    ;; deadline was no protection either.
    (let ((result (run-tests "cl-mcp/tests/test-runner-test-chatty")))
      (ok (= 0 (gethash "failed" result)) "the helper test itself passes")
      (let ((stdout (gethash "stdout" result)))
        (cond
          ;; Same nested-rove caveat as RUN-TESTS-CAPTURES-STDOUT above.
          ((null stdout)
           (rove:skip "stdout not captured (nested rove:run limitation)"))
          (t
           (ok (<= (length stdout)
                   (+ cl-mcp/src/test-runner-core:*max-test-output-length*
                      100))
               (format nil "reported ~D chars for a limit of ~D"
                       (length stdout)
                       cl-mcp/src/test-runner-core:*max-test-output-length*))
           ;; The note has to carry the true total, not the retained length:
           ;; that number is the only evidence the caller gets about how much
           ;; was dropped, and reporting the kept size would read as "nothing
           ;; was lost".  The fixture emits 400 000 characters of its own.
           (let ((marker (search "(truncated, " stdout)))
             (ok marker "the note is present")
             (let ((total (and marker
                               (parse-integer stdout
                                              :start (+ marker 12)
                                              :junk-allowed t))))
               (ok (and total (>= total 400000))
                   (format nil "the note reports the real total (~A)"
                           total))))))))))

(deftest run-tests-selected-counts-tests-not-assertions
  (let ((system "cl-mcp/tests/test-runner-test-counts")
        (pkg "cl-mcp/tests/test-runner-test-counts::")
        (names '("three-passing-assertions" "two-passing-assertions"
                 "one-of-three-assertions-fails" "nested-testing-blocks" "only-skipped"
                 "skip-inside-testing" "passes-and-skips")))
    (flet ((selected (&rest names)
             (run-tests system :tests (mapcar (lambda (name) (concatenate 'string pkg name))
                                              names)))
           (counts (result)
             (list (gethash "passed" result) (gethash "failed" result)
                   (or (gethash "pending" result) 0)))
           (skipped (result)
             (map 'list (lambda (entry) (string-downcase (gethash "test_name" entry)))
                  (or (gethash "skipped_tests" result) #()))))
      (let ((all (apply #'selected names))
            (whole (run-tests system)))
        (testing "a selected Rove run counts tests, as a whole-system run does"
          (ok (equal '(4 1 2) (counts all))
              "seven tests: four checked something, one failed, two only skipped")
          (ok (equal (counts whole) (counts all))
              "selecting every test reports what the whole-system run reports")
          (ok (equal (sort (skipped whole) #'string<) (sort (skipped all) #'string<))
              "and the same skipped tests")
          (ok (= 1 (length (gethash "failed_tests" all)))
              "failure details still name the failing assertion"))
        (testing "one test is one, however it is built"
          (ok (equal '(1 0 0) (counts (selected "three-passing-assertions")))
              "several assertions")
          (ok (equal '(1 0 0) (counts (selected "nested-testing-blocks")))
              "nested testing blocks"))
        (testing "a skip is never reported as a check that passed"
          (let ((only (selected "only-skipped")))
            (ok (equal '(0 0 1) (counts only)) "a test that only skips is pending")
            (ok (equal '("only-skipped") (skipped only)))
            (ok (equalp #("nothing to check yet")
                        (gethash "reasons" (aref (gethash "skipped_tests" only) 0)))
                "with the reason it gave"))
          (ok (equal '(0 0 1) (counts (selected "skip-inside-testing")))
              "however deep inside testing blocks the skip is")
          (let ((mixed (selected "passes-and-skips")))
            (ok (equal '(1 0 0) (counts mixed))
                "a test that checked something and skipped the rest passed")
            (ok (equal '("passes-and-skips") (skipped mixed))
                "but its skip is still reported")))))))

(deftest run-tests-response-says-when-everything-was-skipped
  (let ((only (build-run-tests-response
               (run-tests "cl-mcp/tests/test-runner-test-counts"
                          :test "cl-mcp/tests/test-runner-test-counts::only-skipped")))
        (mixed (build-run-tests-response
                (run-tests "cl-mcp/tests/test-runner-test-counts"
                           :test "cl-mcp/tests/test-runner-test-counts::passes-and-skips"))))
    (flet ((text (response) (gethash "text" (aref (gethash "content" response) 0))))
      (testing "nothing checked is not a pass"
        (ok (null (search "✓ PASS" (text only))))
        (ok (search "SKIPPED" (text only)))
        (ok (search "nothing to check yet" (text only)) "the reason is in the text"))
      (testing "a pass that skipped something says so"
        (ok (search "✓ PASS" (text mixed)))
        (ok (search "Skipped" (text mixed)))
        (ok (search "the rest needs a network" (text mixed)))
        (ok (= 1 (length (gethash "skipped_tests" mixed))))))))

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
      ;; The target skips where the :clhs library is missing (CI), and a test
      ;; that only skips counts as pending, not passed: count what ran.
      (ok (= 1 (+ (gethash "passed" result) (gethash "pending" result 0))))
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
      ;; Both targets skip where the :clhs library is missing (CI): count
      ;; what ran, passed or only skipped.
      (ok (= 2 (+ (gethash "passed" result) (gethash "pending" result 0))))
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

(deftest failure-detail-prints-form-and-values-as-source
  (testing "a string value stays distinguishable from a number printing the same"
    ;; PRINC-TO-STRING stood here once and reported the failing (equal "6" 6)
    ;; as the visibly true (EQUAL 6 6), with "6" and 6 as the same Got: entry.
    (let ((detail (cl-mcp/src/test-runner-core::make-failure-detail
                   :test-name "t"
                   :form '(equal "6" 6)
                   :values (list "6" 6))))
      (ok (search "\"6\"" (gethash "form" detail))
          "the string literal keeps its quotes in the form")
      (ok (equal '("\"6\"" "6") (coerce (gethash "values" detail) 'list))
          "and the two values no longer print as one and the same")))
  (testing "a keyword keeps its colon"
    (let ((detail (cl-mcp/src/test-runner-core::make-failure-detail
                   :test-name "t"
                   :form '(make-instance 'rect :width 2))))
      (ok (search ":WIDTH" (gethash "form" detail))
          "an initarg printed without its colon is not readable-back code")))
  (testing "a form that is itself a string still keeps its quotes"
    ;; Rove records the quoted form a user wrote, so (ng "truthy") records the
    ;; string itself.  Passing a string through as already-rendered text would
    ;; print it as the bare, symbol-looking truthy -- the very confusion this
    ;; function exists to avoid.
    (let ((detail (cl-mcp/src/test-runner-core::make-failure-detail
                   :test-name "t" :form "truthy")))
      (ok (equal "\"truthy\"" (gethash "form" detail)))))
  ;; The next three run from CL-USER, as the runner does, so that a pass
  ;; cannot come from *PACKAGE* already being this test package.
  (testing "symbols print as the test's own package reads them"
    ;; Printed from CL-USER, a test's local came out as PKG::NAME.
    (let ((*package* (find-package '#:cl-user)))
      (let ((detail (cl-mcp/src/test-runner-core::make-failure-detail
                     :test-name "t" :form '(= 1 probe-local) :values '(probe-local)
                     :package (find-package '#:cl-mcp/tests/test-runner-test))))
        (ok (equal "(= 1 PROBE-LOCAL)" (gethash "form" detail)))
        (ok (equal '("PROBE-LOCAL") (coerce (gethash "values" detail) 'list))))))
  (testing "without a package the form keeps qualifying what CL-USER cannot read"
    (let ((*package* (find-package '#:cl-user)))
      (let ((detail (cl-mcp/src/test-runner-core::make-failure-detail
                     :test-name "t" :form '(= 1 probe-local))))
        (ok (equal "(= 1 CL-MCP/TESTS/TEST-RUNNER-TEST::PROBE-LOCAL)"
                   (gethash "form" detail))))))
  (testing "a reason that is a condition is printed relative to the same package"
    (let ((*package* (find-package '#:cl-user)))
      (let ((detail (cl-mcp/src/test-runner-core::make-failure-detail
                     :test-name "t"
                     :reason (make-condition 'simple-error
                                             :format-control "~S is unbound"
                                             :format-arguments '(probe-local))
                     :package (find-package '#:cl-mcp/tests/test-runner-test))))
        (ok (equal "PROBE-LOCAL is unbound" (gethash "reason" detail))))))
  (testing "a keyword test name does not make every symbol print qualified"
    (let ((*package* (find-package '#:cl-user)))
      (ok (null (cl-mcp/src/test-runner-core::%test-name-package :some-test)))
      (ok (eq (find-package '#:cl-mcp/tests/test-runner-test)
              (cl-mcp/src/test-runner-core::%test-name-package 'some-test)))
      (ok (null (cl-mcp/src/test-runner-core::%test-name-package "some-test")))
      (ok (null (cl-mcp/src/test-runner-core::%test-name-package
                 (make-symbol "SOME-TEST")))))))

(deftest run-tests-keeps-the-quotes-on-a-string-assertion-form
  (testing "a real Rove failure whose form is a bare string reports it quoted"
    ;; The whole path, not just MAKE-FAILURE-DETAIL: Rove's ASSERTION-FORM
    ;; hands back the quoted form a user wrote, which for (ng "truthy") is the
    ;; string itself.
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-string-form"))
           (failures (gethash "failed_tests" result))
           (failure (and (plusp (length failures)) (aref failures 0))))
      (ok (plusp (gethash "failed" result)) "the helper test does fail")
      (ok failure "and the failure is reported with details")
      (when failure
        (ok (equal "\"truthy\"" (gethash "form" failure))
            "the form is the quoted string, not a bare truthy")))))

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
          (ok (stringp form) "Should include assertion form")
          (ok (equal "(= 3 FOUR)" form)
              "the test's own local prints unqualified, as it is written")))))
  (testing "a selected run prints the form relative to the test's package too"
    (let* ((result (run-tests "cl-mcp/tests/test-runner-test-direct-assertion"
                              :test (concatenate
                                     'string
                                     "cl-mcp/tests/test-runner-test-direct-assertion"
                                     "::direct-assertion-failure")))
           (failures (gethash "failed_tests" result)))
      (ok (plusp (length failures)) "the selected test fails")
      (when (plusp (length failures))
        (ok (equal "(= 3 FOUR)" (gethash "form" (aref failures 0))))))))

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

(deftest rove-purge-ghost-suites-resolves-names-from-the-registry-only
  (testing "%rove-purge-ghost-suites must not load a dependency's .asd"
    ;; Resolving dependency names with ASDF:FIND-SYSTEM loads the .asd of any
    ;; name that is merely discoverable, which both builds systems as a side
    ;; effect (:defsystem-depends-on) and, outside an ASDF session, re-runs the
    ;; source-registry search on every call -- 103 seconds on a project whose
    ;; graph reaches 905 systems.  Pin the registry-only lookup by leaving a
    ;; discoverable-but-unregistered dependency in the graph and asserting the
    ;; purge never registers it.
    (let* ((tmp-dir
             (uiop:ensure-directory-pathname
              (uiop:merge-pathnames*
               (format nil "cl-mcp-purge-registry-~A-~A/"
                       (get-universal-time) (random 100000))
               (uiop:temporary-directory))))
           (root-name "cl-mcp-purge-registry-root")
           (leaf-name "cl-mcp-purge-registry-leaf")
           (root-asd (uiop:merge-pathnames*
                      (format nil "~A.asd" root-name) tmp-dir))
           (leaf-asd (uiop:merge-pathnames*
                      (format nil "~A.asd" leaf-name) tmp-dir))
           (asdf:*central-registry* (cons tmp-dir asdf:*central-registry*)))
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s root-asd :direction :output :if-exists :supersede)
               (format s "(asdf:defsystem ~S~%  :depends-on (~S))~%"
                       root-name leaf-name))
             (with-open-file (s leaf-asd :direction :output :if-exists :supersede)
               (format s "(asdf:defsystem ~S)~%" leaf-name))
             (asdf/find-system:load-asd root-asd)
             (ok (asdf:registered-system root-name)
                 "root system is registered before the purge")
             (ok (null (asdf:registered-system leaf-name))
                 "leaf system is discoverable but unregistered before the purge")
             ;; Guard against a false pass: the purge returns immediately when
             ;; Rove's registry is empty, which would satisfy the assertion
             ;; below without the walk ever running.
             (ok (plusp (hash-table-count
                         (symbol-value
                          (find-symbol "*PACKAGE-SUITES*"
                                       :rove/core/suite/package))))
                 "Rove holds at least one suite, so the purge really walks")
             (cl-mcp/src/test-runner-core::%rove-purge-ghost-suites root-name)
             (ok (null (asdf:registered-system leaf-name))
                 "purge must not load a dependency's .asd to resolve its name"))
        (ignore-errors (asdf/system-registry:clear-system root-name))
        (ignore-errors (asdf/system-registry:clear-system leaf-name))
        (ignore-errors
         (uiop:delete-directory-tree tmp-dir :validate t))))))

(deftest extract-defpackage-names-does-not-intern-into-cl-user
  (testing "%extract-defpackage-names-from-file leaves CL-USER untouched"
    ;; READ interns every unqualified symbol it reads into *PACKAGE*.  A purge
    ;; traversal scans over a thousand files, so scanning in CL-USER dumps all
    ;; of their symbols there.
    (let* ((tmp-dir
             (uiop:ensure-directory-pathname
              (uiop:merge-pathnames*
               (format nil "cl-mcp-scan-package-~A-~A/"
                       (get-universal-time) (random 100000))
               (uiop:temporary-directory))))
           (src-path (uiop:merge-pathnames* "scan-target.lisp" tmp-dir))
           (pkg-name "CL-MCP-SCAN-TARGET-PACKAGE")
           (marker "CL-MCP-SCAN-MARKER-SYMBOL"))
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s src-path :direction :output :if-exists :supersede)
               (format s "(defpackage #:~A~%  (:use #:cl))~%" pkg-name)
               (format s "(in-package #:~A)~%" pkg-name)
               (format s "(defun ~A (x) x)~%" marker))
             (ok (null (find-symbol marker :cl-user))
                 "marker symbol is absent from CL-USER before the scan")
             (let ((names
                     (cl-mcp/src/test-runner-core::%extract-defpackage-names-from-file
                      src-path)))
               (ok (member pkg-name names :test #'string-equal)
                   "the defpackage name is still extracted"))
             (ok (null (find-symbol marker :cl-user))
                 "scanning must not intern the file's symbols into CL-USER"))
        (ignore-errors
         (uiop:delete-directory-tree tmp-dir :validate t))))))

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

(deftest build-run-tests-response-does-not-call-zero-tests-a-pass
  (testing "a run that executed nothing is not reported as green"
    ;; Zero failures out of zero tests is what let a mis-detected framework
    ;; report success while running nothing: the agent reads content[].text,
    ;; sees the banner, and believes its suite is green.  It is not a failure
    ;; either -- a system may genuinely have no tests -- so the banner says so.
    (flet ((banner (passed failed pending &key (framework :rove) failed-tests)
             (gethash "text"
                      (aref (gethash
                             "content"
                             (build-run-tests-response
                              (cl-mcp/src/test-runner-core::make-test-result
                               :passed passed :failed failed :pending pending
                               :failed-tests failed-tests
                               :framework framework :duration 1)))
                            0))))
      (let ((text (banner 0 0 0)))
        (ok (search "NO TESTS RAN" text) "the banner must say nothing ran")
        (ok (not (search "✓ PASS" text)) "and must not claim a pass"))
      (ok (search "✓ PASS" (banner 3 0 0))
          "a run that did execute tests still passes")
      (ok (search "✗ FAIL" (banner 0 1 0))
          "and a failing run still fails")
      ;; The ASDF fallback reports no counts at all, so its SUCCESS is also
      ;; zero/zero.  Calling that "no tests ran" would be false -- and so is
      ;; calling it a pass: a runner that reports failures by its return value
      ;; (prove, rove:run) returns normally from a red suite too (#131).
      (let ((text (banner 0 0 0 :framework :asdf)))
        (ok (search "RAN, RESULT UNKNOWN" text)
            "a successful asdf:test-system run says its result is unknown")
        (ok (not (search "✓ PASS" text)) "not that it passed")
        (ok (not (search "NO TESTS RAN" text)) "nor that nothing ran"))
      ;; Likewise, entries in failed_tests prove something ran.
      (ok (not (search "NO TESTS RAN"
                       (banner 0 0 0 :failed-tests
                               (list (cl-mcp/src/test-runner-core::make-failure-detail
                                      :test-name "x" :reason "y")))))
          "a non-empty failure list contradicts an empty run"))))

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

(deftest fiveam-reason-text-collapses-blank-line-runs
  ;; FiveAM builds its failure message by printing each fragment on its own
  ;; line with a blank line between, so "1200 /= 1201" arrives as ten lines.
  ;; The summary text is the only part of the response most clients render,
  ;; and every failure in a run pays that cost.
  (let ((collapse #'cl-mcp/src/test-runner-core::%collapse-blank-lines))
    (testing "a run of blank lines becomes a single line break"
      (ok (equal (format nil "a~%b") (funcall collapse (format nil "a~%~%~%b")))))
    (testing "an ordinary line break is left alone"
      (ok (equal (format nil "a~%b") (funcall collapse (format nil "a~%b")))))
    (testing "leading and trailing blank lines are trimmed"
      (ok (equal "a" (funcall collapse (format nil "~%~%a~%~%")))))
    (testing "text without blank lines is returned unchanged"
      (ok (equal "plain" (funcall collapse "plain"))))
    (testing "a non-string passes through, so callers need no type check"
      (ok (null (funcall collapse nil))))))

(deftest fiveam-failure-detail-carries-the-tests-docstring
  ;; run-tests documents failed_tests[].description and the Rove backend fills
  ;; it in. The FiveAM backend left it empty even though FiveAM keeps the
  ;; test's docstring on the test-case object, so a FiveAM failure arrived
  ;; with no statement of what was being asserted -- and, having no source
  ;; location either, nothing to locate it by but the bare test name.
  ;;
  ;; FiveAM is resolved at run time rather than declared: cl-mcp's own suite
  ;; is a Rove suite, and its FiveAM backend is written for an image where
  ;; FiveAM may be absent.
  (if (null (asdf:find-system "fiveam" nil))
      (rove:skip "FiveAM is not installed; the FiveAM backend cannot run here")
      (let* ((tmp-dir (uiop:ensure-directory-pathname
                       (uiop:merge-pathnames*
                        (format nil "cl-mcp-fiveam-detail-~A-~A/"
                                (get-universal-time) (random 100000))
                        (uiop:temporary-directory))))
             (system "fiveam-detail-probe")
             (asd-path (uiop:merge-pathnames*
                        (format nil "~A.asd" system) tmp-dir)))
        (asdf:load-system "fiveam")
        (unwind-protect
             (progn
               (ensure-directories-exist tmp-dir)
               (with-open-file (s asd-path :direction :output
                                           :if-exists :supersede)
                 (format s "(asdf:defsystem ~S~%  :depends-on (\"fiveam\")~%~
                            ~2@T:components ((:file \"suite\")))~%"
                         system))
               (with-open-file (s (uiop:merge-pathnames* "suite.lisp" tmp-dir)
                                  :direction :output :if-exists :supersede)
                 (format s "(defpackage #:fiveam-detail-probe-suite~%~
                            ~2@T(:use #:cl #:fiveam))~%~
                            (in-package #:fiveam-detail-probe-suite)~%~
                            (def-suite :fiveam-detail-probe)~%~
                            (in-suite :fiveam-detail-probe)~%~
                            (test deliberate-failure~%~
                            ~2@T\"documented on purpose\"~%~
                            ~2@T(let ((probe-local 2))~%~
                            ~4@T(is (= 1 probe-local))))~%"))
               ;; On the central registry rather than only ASDF:LOAD-ASD'd:
               ;; RUN-TESTS force-reloads, and the CLEAR-SYSTEM that precedes
               ;; the reload drops a system ASDF cannot re-find from any
               ;; search path, which surfaces as MISSING-COMPONENT instead of
               ;; the failure this test is about.
               (let ((asdf:*central-registry*
                       (cons tmp-dir asdf:*central-registry*)))
                 (asdf:load-asd asd-path)
                 (let* ((result (run-tests system))
                        (failures (gethash "failed_tests" result))
                        (failure (and (plusp (length failures))
                                      (aref failures 0))))
                   (testing "the run is reported as a FiveAM failure"
                     ;; Quote the reason: when the probe system fails to load,
                     ;; every later assertion fails for that reason rather than
                     ;; for the one under test, and a bare framework mismatch
                     ;; says nothing about why.
                     (ok (equal "fiveam" (gethash "framework" result))
                         (format nil "framework=~A reason=~A"
                                 (gethash "framework" result)
                                 (and failure (gethash "reason" failure))))
                     (ok (= 1 (gethash "failed" result))))
                   (testing "the failure quotes the test's docstring"
                     (ok failure)
                     (ok (equal "documented on purpose"
                                (and failure (gethash "description" failure)))))
                   (testing "the form is printed as the test's own package reads it"
                     ;; It was printed from CL-USER, so the test's own local
                     ;; came out as FIVEAM-DETAIL-PROBE-SUITE::PROBE-LOCAL.
                     (ok (equal "(= 1 PROBE-LOCAL)"
                                (and failure (gethash "form" failure)))))
                   (testing "the reason carries no blank-line runs"
                     (let ((reason (and failure (gethash "reason" failure))))
                       (ok (stringp reason))
                       (ok (null (search (format nil "~%~%") reason))))))))
          (let ((var (uiop:find-symbol* '#:*toplevel-suites* :fiveam)))
            (setf (symbol-value var)
                  (remove :fiveam-detail-probe (symbol-value var))))
          (ignore-errors (asdf:clear-system system))
          (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

(defun %write-fixture-file (directory name text)
  "Write TEXT to the file NAME under DIRECTORY, replacing it."
  (with-open-file (s (uiop:merge-pathnames* name directory)
                     :direction :output :if-exists :supersede)
    (write-string text s)))

(defun %call-with-fiveam-fixture (system files thunk)
  "Write SYSTEM's .asd -- a FiveAM system of FILES' components, in order -- and
FILES, a list of (NAME . TEXT), to a fresh directory on ASDF's central registry,
then call THUNK with that directory.  Clean up the system, its suites and the
directory afterwards.  NIL when FiveAM is not installed, after a skip."
  (if (null (asdf:find-system "fiveam" nil))
      (rove:skip "FiveAM is not installed; the FiveAM backend cannot run here")
      (let ((dir (uiop:ensure-directory-pathname
                  (uiop:merge-pathnames*
                   (format nil "cl-mcp-~A-~A-~A/" system (get-universal-time)
                           (random 100000))
                   (uiop:temporary-directory)))))
        (asdf:load-system "fiveam")
        (unwind-protect
             (progn
               (ensure-directories-exist dir)
               (%write-fiveam-fixture-asd dir system (mapcar #'car files))
               (dolist (file files)
                 (%write-fixture-file dir (car file) (cdr file)))
               (let ((asdf:*central-registry* (cons dir asdf:*central-registry*)))
                 (asdf:load-asd (uiop:merge-pathnames* (format nil "~A.asd" system) dir))
                 (funcall thunk dir)))
          (let ((var (uiop:find-symbol* '#:*toplevel-suites* :fiveam)))
            (setf (symbol-value var)
                  (remove-if (lambda (suite) (search (string-upcase system) (string suite)))
                             (symbol-value var))))
          (ignore-errors (asdf:clear-system system))
          (ignore-errors (uiop:delete-directory-tree dir :validate t))))))

(defun %write-fiveam-fixture-asd (directory system file-names)
  "Write SYSTEM's .asd under DIRECTORY: FiveAM, then FILE-NAMES' components in
order, each with a :type when its extension is not lisp."
  (%write-fixture-file
   directory (format nil "~A.asd" system)
   (format nil "(asdf:defsystem ~S~%  :depends-on (\"fiveam\")~%  :components (~{~A~^ ~}))~%"
           system
           (mapcar (lambda (file)
                     (let ((type (pathname-type file)))
                       (format nil "(:file ~S~@[ :type ~S~])" (pathname-name file)
                               (and type (string/= type "lisp") type))))
                   file-names))))

(defun %fiveam-fixture-file (package suite tests &optional parent)
  "Return the text of a FiveAM test file: PACKAGE using FiveAM, the suite SUITE
(written as given, `:root' or `sub-suite') nested :in PARENT when that is
given, an IN-SUITE of it, and TESTS, each (NAME FORM)."
  (format nil "(defpackage #:~A (:use #:cl #:fiveam))~%(in-package #:~A)~%~
               (def-suite ~A~@[ :in ~A~])~%(in-suite ~A)~%~{~A~%~}"
          package package suite parent suite
          (mapcar (lambda (test) (format nil "(test ~A (is ~A))" (first test) (second test)))
                  tests)))

(defun %summary-text (result)
  "Return the content text run-tests' response would show for RESULT."
  (gethash "text" (aref (gethash "content" (build-run-tests-response result)) 0)))

(defun %headline (result)
  "Return the first line of RESULT's summary text: its verdict."
  (let ((text (%summary-text result)))
    (subseq text 0 (position #\Newline text))))

(deftest run-tests-recompiles-a-file-edited-in-the-second-of-its-compile
  ;; ASDF compares FILE-WRITE-DATEs, whole seconds: an edit landing in the
  ;; second its fasl was written leaves the two equal, ASDF keeps the fasl, and
  ;; run-tests reported the old, passing test as a pass.  Review of #218: a
  ;; source whose extension is not .lisp, (:file "main" :type "cl"), was missed.
  (dolist (file '("main.lisp" "main.cl"))
    (testing (format nil "a source named ~A" file)
      (flet ((main (form)
               (%fiveam-fixture-file "fiveam-stale-probe/main" ":fiveam-stale-probe"
                                     (list (list "stays-true" form)))))
        (%call-with-fiveam-fixture
         "fiveam-stale-probe"
         (list (cons file (main "(= 1 1)")))
         (lambda (dir)
           (ok (equal "✓ PASS" (%headline (run-tests "fiveam-stale-probe")))
               "the first run passes")
           (let* ((source (uiop:merge-pathnames* file dir))
                  (fasl (asdf:apply-output-translations (compile-file-pathname source))))
             (%write-fixture-file dir file (main "(= 1 2)"))
             ;; The edit's second is the compile's: give the source the fasl's date.
             (let ((unix (- (file-write-date fasl) (encode-universal-time 0 0 0 1 1 1970 0))))
               (sb-posix:utimes (namestring source) unix unix))
             (ok (= 1 (gethash "failed" (run-tests "fiveam-stale-probe")))
                 "the edited, failing test is what runs"))))))))

(deftest run-tests-recompiles-a-same-second-edit-in-a-package-inferred-project
  ;; The run clears the test system's subsystems from ASDF before reloading,
  ;; so a stale-fasl check that reads only registered components, run after
  ;; that clearing, missed this layout -- the scaffold's -- entirely.
  (if (null (asdf:find-system "fiveam" nil))
      (rove:skip "FiveAM is not installed; the FiveAM backend cannot run here")
      (let ((dir (uiop:ensure-directory-pathname
                  (uiop:merge-pathnames*
                   (format nil "cl-mcp-pi-stale-probe-~A-~A/" (get-universal-time)
                           (random 100000))
                   (uiop:temporary-directory)))))
        (asdf:load-system "fiveam")
        (flet ((test-file (form)
                 (%fiveam-fixture-file "pi-stale-probe/tests/main-test" ":pi-stale-probe"
                                       (list (list "stays-true" form)))))
          (unwind-protect
               (progn
                 (ensure-directories-exist (uiop:merge-pathnames* "tests/" dir))
                 (%write-fixture-file
                  dir "pi-stale-probe.asd"
                  ;; One system per .asd, named after it: run inside an ASDF
                  ;; session -- rove cl-mcp.asd's test-op -- a second system of
                  ;; the file is not defined again after run-tests clears it.
                  (format nil "(asdf:defsystem \"pi-stale-probe\" ~
                                 :class :package-inferred-system ~
                                 :depends-on (\"fiveam\" \"pi-stale-probe/tests/main-test\"))~%"))
                 (%write-fixture-file dir "tests/main-test.lisp" (test-file "(= 1 1)"))
                 (let ((asdf:*central-registry* (cons dir asdf:*central-registry*)))
                   (asdf:load-asd (uiop:merge-pathnames* "pi-stale-probe.asd" dir))
                   (let ((first (run-tests "pi-stale-probe")))
                     ;; Quote the reason: a fixture that fails to load fails
                     ;; every later step for that reason, not the one tested.
                     (ok (equal "✓ PASS" (%headline first))
                         (format nil "the first run passes (~A)"
                                 (let ((failures (gethash "failed_tests" first)))
                                   (and (plusp (length failures))
                                        (gethash "reason" (aref failures 0)))))))
                   (let* ((source (uiop:merge-pathnames* "tests/main-test.lisp" dir))
                          (fasl (asdf:apply-output-translations
                                 (compile-file-pathname source))))
                     (%write-fixture-file dir "tests/main-test.lisp" (test-file "(= 1 2)"))
                     (let ((unix (- (file-write-date fasl)
                                    (encode-universal-time 0 0 0 1 1 1970 0))))
                       (sb-posix:utimes (namestring source) unix unix))
                     (ok (= 1 (gethash "failed" (run-tests "pi-stale-probe")))
                         "the edited, failing test is what runs"))))
            (let ((var (uiop:find-symbol* '#:*toplevel-suites* :fiveam)))
              (setf (symbol-value var) (remove :pi-stale-probe (symbol-value var))))
            (dolist (system '("pi-stale-probe/tests/main-test" "pi-stale-probe"))
              (ignore-errors (asdf:clear-system system)))
            (ignore-errors (uiop:delete-directory-tree dir :validate t)))))))

(deftest run-tests-names-fiveam-suites-outside-the-root-suite
  ;; A suite declared without :in is run by run-tests, which runs every
  ;; top-level suite of the system, but not by a test-op that runs the root
  ;; suite -- the scaffold's runs (fiveam:run! :<name>).  run-tests reported
  ;; them as an ordinary pass, so nothing showed the two runs differed.
  (%call-with-fiveam-fixture
   "fiveam-orphan-probe"
   (list (cons "main.lisp" (%fiveam-fixture-file "fiveam-orphan-probe/main"
                                                 ":fiveam-orphan-probe"
                                                 '(("in-root" "(= 1 1)"))))
         (cons "orphan.lisp" (%fiveam-fixture-file "fiveam-orphan-probe/orphan" "orphan-suite"
                                                   '(("outside-root" "(= 1 1)")))))
   (lambda (dir)
     (declare (ignore dir))
     (let* ((result (run-tests "fiveam-orphan-probe"))
            (outside (gethash "suites_outside_root" result))
            (text (%summary-text result)))
       (ok (= 2 (gethash "passed" result)) "both suites still run")
       (ok (equal '("FIVEAM-ORPHAN-PROBE/ORPHAN::ORPHAN-SUITE") (coerce outside 'list))
           "the suite outside the root is named")
       (ok (search "⚠ PASS" text) "the headline is not a plain pass")
       (ok (search ":in" text) "and the text says how to nest it")))))

(deftest run-tests-names-fiveam-tests-that-did-not-run
  ;; A file declaring its suite :in the root suite, loaded before the file that
  ;; defines the root on a worker that had loaded both: its suite joins the
  ;; previous root object, the new root replaces it, and its tests are reached
  ;; from no suite.  run-tests ran the rest and reported a pass, the tests just
  ;; gone.
  (%call-with-fiveam-fixture
   "fiveam-lost-probe"
   (list (cons "main.lisp" (%fiveam-fixture-file "fiveam-lost-probe/main" ":fiveam-lost-probe"
                                                 '(("in-root" "(= 1 1)"))))
         (cons "sub.lisp" (%fiveam-fixture-file "fiveam-lost-probe/sub" "sub-suite"
                                                '(("in-sub" "(= 1 1)"))
                                                ":fiveam-lost-probe")))
   (lambda (dir)
     (ok (= 2 (gethash "passed" (run-tests "fiveam-lost-probe"))) "in order, both run")
     (%write-fiveam-fixture-asd dir "fiveam-lost-probe" '("sub.lisp" "main.lisp"))
     (let* ((result (run-tests "fiveam-lost-probe"))
            (lost (coerce (gethash "unreached_tests" result) 'list))
            (text (%summary-text result)))
       (ok (equal '("FIVEAM-LOST-PROBE/SUB::IN-SUB") lost) "the lost test is named")
       (ok (search "⚠ PASS, BUT 1 TEST DID NOT RUN" text) "and the headline says so")
       (ok (search ":import-from" text) "with the way to fix the load order")))))

(deftest run-tests-counts-a-fiveam-dependency-as-run
  ;; Review of #218: a test the root suite reaches only as another test's
  ;; :depends-on -- FiveAM runs it on demand -- was listed as unreached
  ;; although it ran and passed.
  (%call-with-fiveam-fixture
   "fiveam-dependency-probe"
   (list (cons "main.lisp"
               (format nil "(defpackage #:fiveam-dependency-probe/main (:use #:cl #:fiveam))~%~
                            (in-package #:fiveam-dependency-probe/main)~%~
                            (def-suite :fiveam-dependency-probe)~%~
                            (in-suite :fiveam-dependency-probe)~%~
                            (test (prerequisite :suite nil) (is (= 1 1)))~%~
                            (test (root-test :depends-on (and prerequisite)) (is (= 2 2)))~%")))
   (lambda (dir)
     (declare (ignore dir))
     (let ((result (run-tests "fiveam-dependency-probe")))
       (ok (= 2 (gethash "passed" result)) "both tests ran")
       (ok (null (gethash "unreached_tests" result)) "and neither is called unreached")
       (ok (equal "✓ PASS" (%headline result)))))))

(deftest run-tests-names-a-fiveam-dependency-a-short-circuit-skipped
  ;; Second review of #218: walking :depends-on counted every name in the
  ;; expression as run, but (or prerequisite never-run) stops at the first
  ;; dependency that passes, so FiveAM never runs the second -- which was then
  ;; hidden behind a plain pass.  What ran is what produced results.
  (%call-with-fiveam-fixture
   "fiveam-short-probe"
   (list (cons "main.lisp"
               (format nil "(defpackage #:fiveam-short-probe/main (:use #:cl #:fiveam))~%~
                            (in-package #:fiveam-short-probe/main)~%~
                            (def-suite :fiveam-short-probe)~%~
                            (in-suite :fiveam-short-probe)~%~
                            (test (prerequisite :suite nil) (is (= 1 1)))~%~
                            (test (never-run :suite nil) (is (= 1 1)))~%~
                            (test (root-test :depends-on (or prerequisite never-run)) ~
                            (is (= 2 2)))~%")))
   (lambda (dir)
     (declare (ignore dir))
     (let ((result (run-tests "fiveam-short-probe")))
       (ok (= 2 (gethash "passed" result)) "root-test and prerequisite ran")
       (ok (equal '("FIVEAM-SHORT-PROBE/MAIN::NEVER-RUN")
                  (coerce (gethash "unreached_tests" result) 'list))
           "the dependency the OR never reached is named")))))

(deftest load-failure-hint-for-an-unknown-fiveam-suite-names-the-load-order
  ;; On a fresh worker the same mistake is loud -- `Unknown suite X' -- but the
  ;; hint blamed the worker's package state and sent the caller to replace it.
  (let* ((condition (make-condition 'simple-error :format-control "Unknown suite ~A."
                                                  :format-arguments '("PROBE")))
         (result (cl-mcp/src/test-runner-core:make-load-failure-result "probe/tests"
                                                                         condition))
         (reason (gethash "reason" (aref (gethash "failed_tests" result) 0))))
    (ok (search ":import-from" reason))
    (ok (not (search "pool-kill-worker" reason)))))

(defun %call-with-prove-fixture (thunk &key failing)
  "Write a prove-asdf test system with two test files -- one green, one with a
SUBTEST and, when FAILING, one wrong assertion -- then call THUNK with its name."
  (let* ((tmp-dir (uiop:ensure-directory-pathname
                   (uiop:merge-pathnames*
                    (format nil "cl-mcp-prove-~A-~A/" (get-universal-time) (random 100000))
                    (uiop:temporary-directory))))
         (system (format nil "prove-probe-~A" (random 1000000)))
         (asd-path (uiop:merge-pathnames* (format nil "~A.asd" system) tmp-dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (with-open-file (s asd-path :direction :output :if-exists :supersede)
             (format s "(asdf:defsystem ~S~%  :defsystem-depends-on (\"prove-asdf\")~%~
                        ~2@T:depends-on (\"prove\")~%~
                        ~2@T:components ((:test-file \"first\") (:test-file \"second\"))~%~
                        ~2@T:perform (asdf:test-op (o c) ~
                        (uiop:symbol-call :prove-asdf :run-test-system c)))~%"
                     system))
           (with-open-file (s (uiop:merge-pathnames* "first.lisp" tmp-dir)
                              :direction :output :if-exists :supersede)
             (format s "(defpackage #:~A-first (:use #:cl #:prove))~%~
                        (in-package #:~A-first)~%~
                        (plan 2)~%(sleep 0.3)~%(ok t \"truth\")~%(is (+ 1 1) 2 \"adds\")~%~
                        (finalize)~%"
                     system system))
           (with-open-file (s (uiop:merge-pathnames* "second.lisp" tmp-dir)
                              :direction :output :if-exists :supersede)
             (format s "(defpackage #:~A-second (:use #:cl #:prove))~%~
                        (in-package #:~A-second)~%~
                        (plan 2)~%~
                        (subtest \"nested\" (ok t \"inner one\") (ok t \"inner two\"))~%~
                        (is (* 2 3) ~D \"multiplies\")~%(finalize)~%"
                     system system (if failing 7 6)))
           ;; On the central registry: RUN-TESTS force-reloads, and the
           ;; CLEAR-SYSTEM before the reload drops a system ASDF cannot re-find.
           (let ((asdf:*central-registry* (cons tmp-dir asdf:*central-registry*)))
             (asdf:load-asd asd-path)
             (funcall thunk system)))
      (ignore-errors (asdf:clear-system system))
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(deftest prove-suites-are-counted-per-assertion
  ;; #131: the :prove branch fell through to the ASDF fallback, which counts
  ;; nothing and took "asdf:test-system did not signal" for success -- and
  ;; prove reports failure by return value -- so a red prove suite read as
  ;; ✓ PASS, 0/0.  Prove is resolved at run time, as FiveAM is.
  (if (null (asdf:find-system "prove-asdf" nil))
      (rove:skip "prove is not installed; the prove backend cannot run here")
      (progn
        (asdf:load-system "prove-asdf")
        (asdf:load-system "prove")
        (testing "a green suite reports its real counts"
          (%call-with-prove-fixture
           (lambda (system)
             (let ((result (run-tests system)))
               (ok (equal "prove" (gethash "framework" result))
                   (format nil "framework=~A" (gethash "framework" result)))
               ;; truth, adds, the subtest's two, multiplies.
               (ok (= 5 (gethash "passed" result)) (format nil "passed=~A" (gethash "passed" result)))
               (ok (= 0 (gethash "failed" result)))
               ;; The first file sleeps 0.3s: the duration is the run's, not
               ;; the ~100ms the fallback reported for a 25s suite.
               (ok (>= (gethash "duration_ms" result) 250)
                   (format nil "duration_ms=~A" (gethash "duration_ms" result)))
               (ok (search "✓ PASS" (gethash "text" (aref (gethash "content"
                                                                    (build-run-tests-response result))
                                                          0))))))))
        (testing "a failing assertion is a failure, with its detail"
          (%call-with-prove-fixture
           (lambda (system)
             (let* ((result (run-tests system))
                    (failures (gethash "failed_tests" result))
                    (failure (and (plusp (length failures)) (aref failures 0)))
                    (text (gethash "text" (aref (gethash "content"
                                                         (build-run-tests-response result))
                                                0))))
               (ok (= 4 (gethash "passed" result)))
               (ok (= 1 (gethash "failed" result)))
               (ok (search "✗ FAIL" text) "the banner is a failure, not a pass")
               (ok (and failure (search "multiplies" (gethash "test_name" failure)))
                   "the failure names the assertion")
               (ok (and failure (equal "multiplies" (gethash "description" failure))))
               (ok (and failure (search "expected 7" (gethash "reason" failure)))
                   (and failure (gethash "reason" failure)))))
           :failing t)))))

(deftest asdf-fallback-does-not-call-a-silent-run-a-pass
  (testing "a test-op that reports failure by return value is not reported green"
    ;; The hazard #131 names beyond prove: the fallback took "no condition
    ;; signalled" for success, so any runner that reports failure by its
    ;; return value rendered as ✓ PASS.
    (let* ((tmp-dir (uiop:ensure-directory-pathname
                     (uiop:merge-pathnames*
                      (format nil "cl-mcp-silent-~A-~A/" (get-universal-time) (random 100000))
                      (uiop:temporary-directory))))
           (system (format nil "silent-probe-~A" (random 1000000)))
           (asd-path (uiop:merge-pathnames* (format nil "~A.asd" system) tmp-dir)))
      (unwind-protect
           (progn
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output :if-exists :supersede)
               ;; Chatty on purpose: more than *MAX-TEST-OUTPUT-LENGTH* before
               ;; the summary, so the bounded head of stdout does not reach it.
               (format s "(asdf:defsystem ~S~%  :perform (asdf:test-op (o c) ~
                          (format t \"~~A~~%1 of 1 tests failed~~%\" ~
                          (make-string 60000 :initial-element #\\x)) nil))~%"
                       system))
             (let ((asdf:*central-registry* (cons tmp-dir asdf:*central-registry*)))
               (asdf:load-asd asd-path)
               (let* ((result (run-tests system :framework "asdf"))
                      (response (build-run-tests-response result))
                      (text (gethash "text" (aref (gethash "content" response) 0))))
                 (ok (not (search "✓ PASS" text)) text)
                 (ok (search "RESULT UNKNOWN" text))
                 ;; The runner's own words are the fallback's only verdict,
                 ;; and content[].text is all a client renders.
                 (ok (search "1 of 1 tests failed" text)
                     "the text carries the tail of what the runner printed")
                 (ok (eq 'yason:false (gethash "counts_available" response))
                     "counts_available says no counts were taken")
                 ;; The structured field must not say what the banner denies.
                 (multiple-value-bind (success presentp) (gethash "success" response)
                   (ok (and presentp (null success))
                       (format nil "success is null (unknown), not true: ~S" success))))))
        (ignore-errors (asdf:clear-system system))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

(deftest run-tests-finds-an-unregistered-asd-under-the-project-root
  (testing "a test system whose .asd nobody registered yet is found, as load-system finds it"
    ;; Found dogfooding v3.0.1: a freshly written .asd made run-tests stop at
    ;; MISSING-COMPONENT while load-system found the same system unprompted.
    (let* ((root (uiop:ensure-directory-pathname (asdf:system-source-directory :cl-mcp)))
           (system (format nil "discover-probe-~A" (random 1000000)))
           (dir (uiop:merge-pathnames* (format nil "tests/tmp/~A/" system) root)))
      (unwind-protect
           (progn
             (ensure-directories-exist dir)
             (with-open-file (s (uiop:merge-pathnames* (format nil "~A.asd" system) dir)
                                :direction :output :if-exists :supersede)
               (format s "(asdf:defsystem ~S :perform (asdf:test-op (o c) ~
                          (format t \"probe ran~~%\")))~%" system))
             (ok (null (asdf:find-system system nil)) "precondition: ASDF does not know it")
             ;; Bound first: the discovery searches *PROJECT-ROOT*.
             (let ((cl-mcp/src/project-root:*project-root* root))
               (let ((result (run-tests system :framework "asdf")))
                 (ok (equal "asdf" (gethash "framework" result))
                     (format nil "it ran instead of failing to load: framework=~A"
                             (gethash "framework" result))))))
        (ignore-errors (asdf:clear-system system))
        (ignore-errors (uiop:delete-directory-tree dir :validate t))))))

(deftest run-tests-on-an-unknown-system-does-not-blame-the-worker
  (testing "a name ASDF cannot find says so, rather than advising pool-kill-worker"
    (let* ((system (format nil "no-such-system-~A" (random 1000000)))
           (result (let ((cl-mcp/src/project-root:*project-root*
                           (asdf:system-source-directory :cl-mcp)))
                     (run-tests system)))
           (reason (gethash "reason" (aref (gethash "failed_tests" result) 0))))
      (ok (equal "load-error" (gethash "framework" result)))
      (ok (search "Check the name" reason) reason)
      (ok (not (search "pool-kill-worker" reason)) "the worker is not to blame"))))

(deftest output-tail-keeps-the-last-lines
  (let ((tail #'cl-mcp/src/tools/response-builders::%output-tail))
    (ok (null (funcall tail "")) "empty is NIL")
    (ok (null (funcall tail nil)) "absent is NIL")
    (ok (equal "a" (funcall tail (format nil "a~%~%"))) "trailing blank lines dropped")
    (ok (equal (format nil "c~%d") (funcall tail (format nil "a~%b~%c~%d~%") :max-lines 2))
        "the last N lines")
    (ok (equal "xyz" (funcall tail "uvwxyz" :max-chars 3)) "and at most N characters")))

(deftest fiveam-captures-output-from-a-suite-with-threads-and-sockets
  ;; The FiveAM backend's stdout/stderr capture was removed on the grounds
  ;; that binding the standard streams -- "even to a broadcast stream" --
  ;; hangs suites that spawn real threads and sockets.  That mechanism cannot
  ;; hold: in SBCL a new thread starts from the GLOBAL value of a special, so
  ;; a binding made on the run thread is invisible to any thread the suite
  ;; spawns and cannot be what blocked them.  This suite does both of the
  ;; things named -- printing threads and a real accept loop -- so a
  ;; regression to the reported behaviour fails here rather than silently
  ;; costing every FiveAM user their stdout and stderr again.
  (if (null (asdf:find-system "fiveam" nil))
      (rove:skip "FiveAM is not installed; the FiveAM backend cannot run here")
      (let* ((tmp-dir (uiop:ensure-directory-pathname
                       (uiop:merge-pathnames*
                        (format nil "cl-mcp-fiveam-threads-~A-~A/"
                                (get-universal-time) (random 100000))
                        (uiop:temporary-directory))))
             (system "fiveam-thread-probe")
             (asd-path (uiop:merge-pathnames*
                        (format nil "~A.asd" system) tmp-dir)))
        (asdf:load-system "fiveam")
        (unwind-protect
             (progn
               (ensure-directories-exist tmp-dir)
               (with-open-file (s asd-path :direction :output
                                           :if-exists :supersede)
                 (write-string
                  "(asdf:defsystem \"fiveam-thread-probe\"
  :depends-on (\"fiveam\" \"usocket\" \"bordeaux-threads\")
  :components ((:file \"suite\")))
"
                  s))
               (with-open-file (s (uiop:merge-pathnames* "suite.lisp" tmp-dir)
                                  :direction :output :if-exists :supersede)
                 (write-string
                  "(defpackage #:fiveam-thread-probe-suite (:use #:cl #:fiveam))
(in-package #:fiveam-thread-probe-suite)
(def-suite :fiveam-thread-probe)
(in-suite :fiveam-thread-probe)

(test spawns-threads-that-print
  (format t \"parent-line~%\")
  (let ((threads (loop repeat 8
                       collect (bordeaux-threads:make-thread
                                (lambda ()
                                  (dotimes (i 20)
                                    (format t \"child-line ~A~%\" i)))))))
    (dolist (th threads) (bordeaux-threads:join-thread th)))
  (is (= 1 1)))

(test opens-a-real-socket
  (let* ((server (usocket:socket-listen \"127.0.0.1\" 0 :reuse-address t))
         (port (usocket:get-local-port server)))
    (unwind-protect
         (let ((acceptor (bordeaux-threads:make-thread
                          (lambda ()
                            (let ((c (usocket:socket-accept server)))
                              (format t \"accepted~%\")
                              (usocket:socket-close c))))))
           (usocket:socket-close (usocket:socket-connect \"127.0.0.1\" port))
           (bordeaux-threads:join-thread acceptor))
      (usocket:socket-close server))
    (is (= 2 2))))
"
                  s))
               (let ((asdf:*central-registry*
                       (cons tmp-dir asdf:*central-registry*)))
                 (asdf:load-asd asd-path)
                 ;; Bound so a regression shows up as a failure here instead
                 ;; of hanging the whole suite.
                 (let ((result (sb-ext:with-timeout 120
                                 (run-tests system))))
                   (testing "the run completes rather than blocking"
                     (ok (equal "fiveam" (gethash "framework" result))
                         (format nil "framework=~A" (gethash "framework" result)))
                     (ok (= 2 (gethash "passed" result)))
                     (ok (= 0 (gethash "failed" result))))
                   (testing "and its standard output is captured"
                     (let ((stdout (gethash "stdout" result)))
                       (ok (stringp stdout) "stdout is reported")
                       (ok (search "parent-line" (or stdout ""))
                           "output written by the test itself is captured"))))))
          (let ((var (uiop:find-symbol* '#:*toplevel-suites* :fiveam)))
            (setf (symbol-value var)
                  (remove :fiveam-thread-probe (symbol-value var))))
          (ignore-errors (asdf:clear-system system))
          (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))
