;;;; scripts/check-specs.lisp
;;;;
;;;; Command-line entry for the cl-mcp/specs bundle.  CI runs it in a fresh
;;;; process; so can you:
;;;;
;;;;   sbcl --non-interactive --load scripts/check-specs.lisp
;;;;   CL_MCP_SPECS_MODE=negative-control sbcl --non-interactive --load scripts/check-specs.lisp
;;;;   CL_MCP_SPECS_MODE=self-test sbcl --non-interactive --load scripts/check-specs.lisp
;;;;
;;;; CL_MCP_SPECS_MODE    check (default): run the bundle with the fixed seeds;
;;;;                      negative-control: swap in wrong implementations and
;;;;                        demand that the bundle catches them (this process
;;;;                        only -- it replaces production functions);
;;;;                      self-test: run cl-mcp/tests/specs-runner-test,
;;;;                        cl-mcp/tests/path-specs-test,
;;;;                        cl-mcp/tests/write-path-specs-test and
;;;;                        cl-mcp/tests/core-record-specs-test;
;;;;                      integration: run the real-cl-spec suite
;;;;                        CL_MCP_SPECS_SUITE names, and fail unless every
;;;;                        test it must run ran and passed, and none skipped.
;;;; CL_MCP_SPECS_SUITE   for integration: cl-mcp/tests/spec-integration-test
;;;;                      or cl-mcp/tests/check-routing-specs-test.
;;;; CL_MCP_SPECS_REPORT  file to write the check or negative-control report
;;;;                      to, as one Lisp form.
;;;;
;;;; The script puts THIS checkout first in ASDF's search, and the runner then
;;;; fails the run unless cl-mcp and every contracted function were loaded from
;;;; it: a green result from another copy of cl-mcp is not evidence about this
;;;; one.  cl-spec and the other dependencies must already be findable, through
;;;; Quicklisp, Roswell's local-projects or CL_SOURCE_REGISTRY.
;;;;
;;;; Exit status: 0 passed; 1 the checks ran and something failed; 2 the script
;;;; could not run them (a load error, an unknown mode).

;;; First, before anything can signal: an error outside MAIN's handler would
;;; otherwise enter the debugger, and under `ros run` with no terminal the
;;; debugger reads end of file and the process exits 0 -- a green CI step for a
;;; script that never ran its checks.
#+sbcl (sb-ext:disable-debugger)

(require "asdf")

(defpackage #:cl-mcp/scripts/check-specs
  (:use #:cl))

(in-package #:cl-mcp/scripts/check-specs)

(defparameter *checkout*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname
    (or *load-truename* (error "Load scripts/check-specs.lisp as a file."))))
  "The cl-mcp checkout this script belongs to, and the one it checks.")

(defun ensure-quicklisp ()
  "Load Quicklisp from its usual places unless it is loaded already."
  (unless (find-package "QL")
    (let ((setup (find-if #'probe-file
                          (list (merge-pathnames "quicklisp/setup.lisp"
                                                 (user-homedir-pathname))
                                (merge-pathnames ".roswell/lisp/quicklisp/setup.lisp"
                                                 (user-homedir-pathname))))))
      (when setup
        (load setup)))))

(defun load-system* (name)
  "Load the ASDF system NAME, through Quicklisp when it is present so that
missing dependencies are fetched."
  (if (find-package "QL")
      (uiop:symbol-call :ql :quickload name :silent t)
      (asdf:load-system name)))

(defun run-runner (mode)
  "Run the bundle runner in MODE and return its exit code."
  (load-system* "cl-mcp/specs/runner")
  (let ((report (uiop:getenv "CL_MCP_SPECS_REPORT")))
    (uiop:symbol-call :cl-mcp/specs/runner :main
                      :mode mode
                      :expected-root *checkout*
                      :report-pathname (and report (plusp (length report))
                                            (uiop:parse-native-namestring report)))))

(defparameter *self-test-systems*
  '("cl-mcp/tests/specs-runner-test" "cl-mcp/tests/path-specs-test"
    "cl-mcp/tests/write-path-specs-test" "cl-mcp/tests/core-record-specs-test")
  "The test systems self-test runs: the runner's own verdicts, the read
fixtures with the read policy's fixed cases, the write fixtures with the write
policy's, and real cl-spec records carried to JSON.  The last one needs
cl-spec, so it is here rather than in the default suite.")

(defun run-self-test ()
  "Run *SELF-TEST-SYSTEMS* and return an exit code.  A system that loaded no
test counts as a failure, not a pass."
  (load-system* "rove")
  (let ((code 0))
    (dolist (system *self-test-systems* code)
      (load-system* system)
      (let* ((suite (uiop:symbol-call :rove/core/suite/package :find-suite
                                      (find-package (string-upcase system))))
             (count (length (and suite
                                 (uiop:symbol-call :rove/core/suite/package :suite-tests
                                                   suite)))))
        (format t "~&~A: ~D test~:P loaded~%" system count)
        (unless (and (plusp count)
                     (uiop:symbol-call :rove :run system))
          (setf code 1))))))

(defparameter *integration-suites*
  '(("cl-mcp/tests/spec-integration-test"
     "real-function-core-schema-survives-check"
     "real-core-schema-survives-describe-and-check"
     "cl-spec-adapter-discovers-and-describes"
     "cl-spec-adapter-runs-and-replays"
     "cl-spec-adapter-sees-a-redefinition"
     "cl-spec-adapter-reads-a-contract"
     "cl-spec-adapter-runs-a-contract"
     "real-named-cases-report-the-one-never-reached"
     "real-state-post-failure-keeps-the-target-outcome"
     "real-case-selection-error-does-not-blame-the-target"
     "real-generation-exhaustion-is-not-a-target-failure"
     "real-state-contract-says-why-it-was-not-shrunk"
     "real-postcondition-failure-is-read-whole"
     "real-capture-availability-records-survive-both-branches")
    ("cl-mcp/tests/check-routing-specs-test"
     "real-selections-run-only-what-they-name"
     "real-registries-answer-for-themselves"
     "real-budgets-and-seeds-reach-cl-spec"
     "real-replay-and-a-changed-declaration"
     "real-entry-reads-seed-text-and-reports-what-ran"))
  "The real-cl-spec suites integration mode runs, each with the tests it must
see run.  The names are listed here, apart from the suites, so that a test
that is deleted or renamed fails the step instead of quietly leaving it.
spec-integration-test skips each test when cl-spec cannot be found, which is
right for the default suite and wrong here, where cl-spec is pinned.")

(defun run-integration ()
  "Run the suite CL_MCP_SPECS_SUITE names and return an exit code: 0 when every
test it must run ran and passed and nothing skipped, 1 when the suite ran and
something did not, 2 when the suite is not one of *INTEGRATION-SUITES*.  The
verdict is read from Rove's per-test results, not from ROVE:RUN's answer,
which counts a skipped test as passed."
  (let* ((system (uiop:getenv "CL_MCP_SPECS_SUITE"))
         (entry (assoc system *integration-suites* :test #'equal)))
    (if (null entry)
        (progn
          (format *error-output* "~&check-specs: CL_MCP_SPECS_SUITE must be one of ~
~{~A~^, ~}; got ~S~%" (mapcar #'first *integration-suites*) system)
          2)
        (progn
          (load-system* "rove")
          (load-system* "cl-mcp/specs/suite-judge")
          (load-system* system)
          (uiop:symbol-call :rove :run system)
          (let ((verdict (uiop:symbol-call
                          :cl-mcp/specs/suite-judge :judge-suite-results
                          (symbol-value (uiop:find-symbol* "*LAST-SUITE-REPORT*" :rove))
                          (rest entry))))
            (uiop:symbol-call :cl-mcp/specs/suite-judge :print-suite-verdict system verdict)
            (if (getf verdict :ok) 0 1))))))

(defun main ()
  "Run the mode CL_MCP_SPECS_MODE names and exit with its code."
  (handler-bind ((error (lambda (condition)
                          (format *error-output* "~&check-specs: ~A~%" condition)
                          (uiop:print-backtrace :stream *error-output* :count 40)
                          (uiop:quit 2))))
    (push *checkout* asdf:*central-registry*)
    (ensure-quicklisp)
    (let ((mode (or (uiop:getenv "CL_MCP_SPECS_MODE") "check")))
      (format t "~&check-specs: mode ~A, checkout ~A~%" mode
              (uiop:native-namestring *checkout*))
      (uiop:quit
       (cond ((string= mode "check") (run-runner :check))
             ((string= mode "negative-control") (run-runner :negative-control))
             ((string= mode "self-test") (run-self-test))
             ((string= mode "integration") (run-integration))
             (t (format *error-output* "~&check-specs: unknown CL_MCP_SPECS_MODE ~S~%" mode)
                2))))))

(main)
