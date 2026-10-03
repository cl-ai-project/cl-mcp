(defpackage #:cl-mcp/tests
  (:use #:cl)
  (:import-from #:rove)
  (:import-from #:cl-mcp/tests/bridge-test)
  (:import-from #:cl-mcp/tests/request-lifecycle-test)
  (:import-from #:cl-mcp/tests/reset-events-test)
  (:import-from #:cl-mcp/tests/asdf-tools-test)
  (:import-from #:cl-mcp/tests/clhs-test)
  (:import-from #:cl-mcp/tests/code-test)
  (:import-from #:cl-mcp/tests/code-refs-core-test)
  (:import-from #:cl-mcp/tests/code-refs-scan-test)
  (:import-from #:cl-mcp/tests/clos-core-test)
  (:import-from #:cl-mcp/tests/clos-verify-core-test)
  (:import-from #:cl-mcp/tests/clos-response-builders-test)
  (:import-from #:cl-mcp/tests/core-test)
  (:import-from #:cl-mcp/tests/fs-test)
  (:import-from #:cl-mcp/tests/frame-inspector-test)
  (:import-from #:cl-mcp/tests/inspect-test)
  (:import-from #:cl-mcp/tests/logging-test)
  (:import-from #:cl-mcp/tests/protocol-test)
  (:import-from #:cl-mcp/tests/nesting-test)
  (:import-from #:cl-mcp/tests/session-project-root-test)
  (:import-from #:cl-mcp/tests/server-instructions-test)
  (:import-from #:cl-mcp/tests/proxy-test)
  (:import-from #:cl-mcp/tests/repl-test)
  (:import-from #:cl-mcp/tests/repl-error-context-test)
  (:import-from #:cl-mcp/tests/repl-inspect-integration-test)
  (:import-from #:cl-mcp/tests/cst-test)
  (:import-from #:cl-mcp/tests/lisp-read-file-test)
  (:import-from #:cl-mcp/tests/lisp-edit-form-test)
  (:import-from #:cl-mcp/tests/lisp-patch-form-test)
  (:import-from #:cl-mcp/tests/lenient-read-test)
  (:import-from #:cl-mcp/tests/object-registry-test)
  (:import-from #:cl-mcp/tests/package-context-test)
  (:import-from #:cl-mcp/tests/response-builders-test)
  (:import-from #:cl-mcp/tests/test-runner-test)
  (:import-from #:cl-mcp/tests/tools-helpers-test)
  (:import-from #:cl-mcp/tests/utils-paths-test)
  (:import-from #:cl-mcp/tests/path-specs-test)
  (:import-from #:cl-mcp/tests/write-path-specs-test)
  (:import-from #:cl-mcp/tests/utils-bounded-stream-test)
  (:import-from #:cl-mcp/tests/utils-request-debugger-boundary-test)
  (:import-from #:cl-mcp/tests/utils-wait-until-test)
  (:import-from #:cl-mcp/tests/validate-test)
  (:import-from #:cl-mcp/tests/tools-test)
  (:import-from #:cl-mcp/tests/define-tool-test)
  (:import-from #:cl-mcp/tests/integration-test)
  (:import-from #:cl-mcp/tests/parinfer-test)
  (:import-from #:cl-mcp/tests/paren-diagnostics-test)
  (:import-from #:cl-mcp/tests/clgrep-utils-test)
  (:import-from #:cl-mcp/tests/clgrep-test)
  (:import-from #:cl-mcp/tests/utils-strings-test)
  (:import-from #:cl-mcp/tests/utils-hash-test)
  (:import-from #:cl-mcp/tests/utils-printing-test)
  (:import-from #:cl-mcp/tests/utils-sanitize-test)
  (:import-from #:cl-mcp/tests/utils-system-test)
  (:import-from #:cl-mcp/tests/utils-random-test)
  (:import-from #:cl-mcp/tests/yason-compat-test)
  (:import-from #:cl-mcp/tests/system-loader-test)
  (:import-from #:cl-mcp/tests/worker-init-hook-test)
  (:import-from #:cl-mcp/tests/pool-env-config-test)
  (:import-from #:cl-mcp/tests/pool-status-test)
  (:import-from #:cl-mcp/tests/project-scaffold-test)
  (:import-from #:cl-mcp/tests/spec-adapter-core-test)
  (:import-from #:cl-mcp/tests/spec-core-record-test)
  (:import-from #:cl-mcp/tests/spec-adapter-report-test)
  (:import-from #:cl-mcp/tests/check-verdict-test)
  (:import-from #:cl-mcp/tests/check-routing-test)
  (:import-from #:cl-mcp/tests/suite-judge-test)
  (:import-from #:cl-mcp/tests/spec-inspection-test)
  (:import-from #:cl-mcp/tests/spec-response-builders-test)
  (:import-from #:cl-mcp/tests/spec-responses-test)
  (:import-from #:cl-mcp/tests/spec-tools-test)
  (:import-from #:cl-mcp/tests/spec-integration-test)
  (:import-from #:cl-mcp/tests/lisp-macroexpand-test)
  (:import-from #:cl-mcp/tests/source-snapshot-test))

(in-package #:cl-mcp/tests)

(defparameter *process-tier-suites*
  '("cl-mcp/tests/pool-test"
    "cl-mcp/tests/worker-leaked-thread-test"
    "cl-mcp/tests/pool-ownership-test"
    "cl-mcp/tests/debugger-boundary-worker-test"
    "cl-mcp/tests/spec-worker-test"
    "cl-mcp/tests/pool-kill-worker-test"
    "cl-mcp/tests/pool-startup-latency-test"
    "cl-mcp/tests/concurrency-test"
    "cl-mcp/tests/http-test"
    "cl-mcp/tests/pool-init-config-test"
    "cl-mcp/tests/worker-test"
    "cl-mcp/tests/clos-describe-integration-test"
    "cl-mcp/tests/tcp-test"
    "cl-mcp/tests/cancel-test"
    "cl-mcp/tests/test-runner-deadline-test")
  "Suites that start worker processes or network servers, or wait out real
deadlines.  Together they take most of a full run (about twelve of fourteen
minutes, measured 2026-10-03), so the quick tier leaves them out.

They are deliberately NOT in this system's :import-from list: run-tests on
cl-mcp/tests runs the suites that list names, so keeping these out of it is
what makes that run the quick tier too.  The test-op below still loads and
compiles them in every tier, so a compile error in one is caught either way,
and runs them in the full tier.  Running one by name (run-tests, or
(rove:run :cl-mcp/tests/pool-test)) runs all of it.

A new suite that spawns workers or servers goes here instead of into the
:import-from list.")

(defun test-tier ()
  "Return :FULL when CL_MCP_TEST_TIER is \"full\" (any case), else :QUICK.

Quick is the default because it is what a developer runs between edits; CI
sets the variable to run everything."
  (let ((value (uiop:getenv "CL_MCP_TEST_TIER")))
    (if (and value (string-equal value "full")) :full :quick)))

(defmethod asdf:perform :after ((op asdf:test-op) (system (eql (asdf:find-system :cl-mcp/tests))))
  (let ((quick (remove-if-not
                (lambda (dep)
                  (and (stringp dep)
                       (uiop:string-prefix-p "cl-mcp/tests/" dep)))
                (asdf:system-depends-on system)))
        (missing (remove-if (lambda (suite) (asdf:find-system suite nil))
                            *process-tier-suites*)))
    ;; A renamed or removed suite would otherwise drop out of the full tier
    ;; without a word.
    (when missing
      (error "*PROCESS-TIER-SUITES* names suites ASDF cannot find: ~{~A~^, ~}" missing))
    ;; Loaded, and so compiled, in both tiers: a compile error in one of them
    ;; is caught by a quick run too.
    (dolist (suite *process-tier-suites*)
      (asdf:load-system suite))
    (if (eq (test-tier) :full)
        (rove:run (append quick *process-tier-suites*))
        ;; Said before the run and again after it, so neither the top nor the
        ;; tail of the output reads as a full pass.  On *ERROR-OUTPUT*: the
        ;; rove command discards *STANDARD-OUTPUT* while tests run.
        (flet ((notice ()
                 (format *error-output* "~&;; Quick tier: ~D of ~D suites; the ~D that ~
                            start worker processes or servers were skipped.~%;; Run ~
                            them all with CL_MCP_TEST_TIER=full (as CI does).~%"
                         (length quick)
                         (+ (length quick) (length *process-tier-suites*))
                         (length *process-tier-suites*))))
          (notice)
          (prog1 (rove:run quick)
            (notice))))))
