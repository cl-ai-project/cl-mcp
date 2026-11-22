;;;; src/test-runner.lisp --- Unified test runner with structured results

(defpackage #:cl-mcp/src/test-runner
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table)
  (:import-from #:cl-mcp/src/state #:protocol-version)
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

(defparameter *structured-output-max-chars* 4000
  "Characters of a run's stdout or stderr kept in run-tests' structuredContent:
the last ones of the capture, where a runner prints its summary and an error its
report.  The capture is itself bounded (*MAX-TEST-OUTPUT-LENGTH*, its head), so
for a longer run these end at its truncation note.")

(defun %clip-output-for-structured (text)
  "Return TEXT, or its last *STRUCTURED-OUTPUT-MAX-CHARS* characters headed by a
line saying how many were left out."
  (if (or (not (stringp text)) (<= (length text) *structured-output-max-chars*))
      text
      (let ((omitted (- (length text) *structured-output-max-chars*)))
        (format nil "[... ~D earlier character~:P omitted]~%~A"
                omitted (subseq text omitted)))))

(defun %publish-test-report (reply version)
  "Attach the test report as structured content for clients that negotiated it.
Some clients -- Claude Code -- show structuredContent in place of content[].text,
so it has to stand on its own without growing far past the text: the summary
text goes with it as summary (the verdict headline, the advice under it, the ASDF
fallback's stdout tail), debug_output, which the summary holds verbatim, is left
out, and stdout and stderr, which the text leaves out, are cut to their ends
(%CLIP-OUTPUT-FOR-STRUCTURED).  failed_tests, one entry per failed assertion
with every field whole, is left out, whatever the verdict, so a reader never
mistakes its absence for no failures: failures_index stands for it, one bounded
entry per failing test whose object_id holds its failures for inspect-object,
and a load, resolution or timeout failure, which has no index, is written whole
in summary.  passed_tests, a name per passing test, is left to the count in
passed.
The result's own fields keep everything."
  (let ((report (gethash "result" reply)))
    (when (and version (string>= version "2025-06-18") (hash-table-p report)
               (nth-value 1 (gethash "passed" report)))
      (let* ((data (copy-hash-table report))
             (content (gethash "content" report))
             (part (and (typep content 'sequence) (plusp (length content))
                        (elt content 0)))
             (text (and (hash-table-p part) (gethash "text" part))))
        (remhash "content" data)
        (when (stringp text)
          (setf (gethash "summary" data) text)
          (remhash "debug_output" data))
        (remhash "failed_tests" data)
        (remhash "passed_tests" data)
        (dolist (field '("stdout" "stderr"))
          (multiple-value-bind (value presentp) (gethash field data)
            (when presentp
              (setf (gethash field data) (%clip-output-for-structured value)))))
        (setf (gethash "structuredContent" report) data))))
  reply)

(define-tool "run-tests"
  :description "Run tests for a system and return structured results.

The system is reloaded from disk first, so no load-system is needed after an
edit, and test/tests names resolve against the freshly loaded packages.  The
test system and its direct <system>/... dependencies are cleared and their .asd
read again, so every system defined there that the test system reaches is loaded
again.  Any other file of a package-inferred system, or of a system defined in
another .asd, is loaded again only when its FASL is stale, so a definition made in repl-eval there
stays in effect for the run (load-system on its file's system or with
clear_fasls=true, or pool-kill-worker, drops it).  A
file of the system's tree written in the same second as its fasl is recompiled
too: ASDF's one-second timestamps would otherwise keep the code from before
the edit.  Rove's tests and hooks made by the files this load loads again are
dropped first and made again by the load, so a deftest deleted from one of
those files stops running, and a file the load leaves alone keeps its tests --
one another system shares, or one sharing a package with a reloaded file.
Rove and FiveAM resolve every selected name before running any test. An unknown
name returns UNRESOLVED with the available names in its package.

Supports multiple test frameworks with automatic detection:
- Rove: Full structured results with failure details
- FiveAM: Full structured results with failure details
- Prove (prove-asdf test files): one count per assertion, with failure details
- ASDF fallback: Text output capture only.  It cannot count, so a run that
  returned without signalling is reported as RAN, RESULT UNKNOWN, never as a
  pass: read stdout

Returns:
- content (summary text): opens with the verdict headline (PASS, FAIL, NO TESTS
  RAN, RAN, RESULT UNKNOWN, ...).  Under Failures (one per failed
  assertion) it details the first 10, a field over 1000 characters cut at its
  middle, then lists the tests the rest come from and counts those past the
  index.  Each listed test, the counted rest and any failure with a field cut
  carry [object-id: ...]: inspect-object it to read those failures whole (ids
  are the worker's; re-run to get fresh ones).  A LOAD FAILED, UNRESOLVED or
  TIMEOUT reason is never cut
- structuredContent (MCP 2025-06-18 and later): the report fields listed below,
  plus summary, the content text itself -- some clients (Claude Code) show
  structuredContent in its place.  It leaves out debug_output, which summary
  holds, and its stdout and stderr keep only their last 4000 characters, headed
  by a line counting the rest.  It leaves out failed_tests, whatever
  the verdict, and passed_tests: failures_index stands for the first (a load,
  resolution or timeout report, which has none, is whole in summary)
- verdict (string) -- the headline as a value: pass, fail, load-failed,
  unresolved, timeout, no-tests-ran, result-unknown (the ASDF fallback, which
  counts nothing: read the stdout tail at the end of the summary), all-skipped,
  or pass-incomplete (tests that did not run, or suites outside the root
  suite).  Every verdict but pass means the run is not a clean pass; a pass
  may still list skipped_tests
- passed (integer) -- tests, not assertions (Rove and FiveAM, whether the whole
  system or a test/tests selection ran); prove counts assertions
- failed (integer) -- same unit as passed
- pending (integer) -- tests that only skipped, so checked nothing (Rove: a
  test whose only results are (skip ...), however deep in testing blocks)
- skipped_tests (array, Rove, present when any test skipped) -- test_name and
  reasons of every test that skipped anything, including one that passed on
  what it did check; also listed under 'Skipped' in the summary text.  When
  every test only skipped, the summary says ALL SKIPPED, not PASS
- unreached_tests (array, FiveAM whole-system run, present when any) -- tests
  defined in the system's packages that the run did not reach: in no suite
  that ran, and neither started nor skipped by FiveAM during the run (a test
  run as another's :depends-on, or by a run nested in a test, counts):
  a file declaring its suite :in the root suite that loaded before the root's
  file, on a warm worker, or a test in no suite that no test that ran depends
  on.  The summary then says PASS, BUT N TESTS DID NOT RUN.  A test deleted
  from its file is dropped from FiveAM before the reload, not reported here
- suites_outside_root (array, FiveAM, present when any) -- run suites other than
  the root suite (the one named after the primary system): run here, but a
  test-op that runs the root suite skips them.  The summary then says PASS, BUT
  N SUITES ARE OUTSIDE THE ROOT SUITE
- warning_records (array, LOAD FAILED only, present when any) -- the full
  warnings ASDF refused a file the load compiled for (a dependency's too), as
  load-system's warning_records: each with fails_compile true and, where the
  compiler has a place, file, line and form.  The summary lists them with their places under
  Warnings, and its hint says to fix the file, as it does for a file that did
  not compile for an error alone -- or, for package variance alone, to get a
  fresh worker
- framework (string)
- duration_ms (integer)
- stdout (string, present when non-empty) — captured test standard output
- stderr (string, present when non-empty) — captured test error output
- debug_output (string, present when non-empty) — output written to *test-debug-output* stream
NOTE: stdout/stderr are in structured fields only, NOT shown in the summary text.
To include debug prints in the visible summary, write to *test-debug-output*:
  (format cl-mcp/src/test-runner-core:*test-debug-output* \"debug: ~A~%\" value)
- failures_index (array) -- one entry per failing test, the first 30: test_name,
  failures (its failed assertions), object_id, source (Rove)
- failures_index_rest (object, when more tests failed) -- failures, tests,
  object_id
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
                    :description "Force framework: 'rove', 'fiveam', 'prove', 'asdf', or 'auto'
(default: auto-detect from the system's :depends-on). Any other value runs the ASDF fallback")
         (test :type :string :required nil
               :description "Run only this specific test, written 'package::test-name' (double
colon). Rove and FiveAM only; exclusive with tests")
         (tests :type :array :required nil
                :description "Run only these specific tests (array of 'package::test-name'). Rove
and FiveAM only; exclusive with test")
         (timeout-seconds :type :number :json-name "timeout_seconds" :required nil
                          :description "Maximum seconds to wait for the test run to complete
(default: 300; a value of 0 or below means the default). Increase for large test suites."))
  :body
  (%publish-test-report
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
                    (:error (error test-result))))))))
   (protocol-version state)))
