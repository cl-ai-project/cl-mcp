;;;; tests/timeout-test.lisp
;;;;
;;;; A fixture, not a suite to run on its own: one test that sleeps ten
;;;; seconds, for run-tests-tool-times-out-a-real-slow-suite
;;;; (tests/test-runner-deadline-test.lisp) to run under a short
;;;; timeout_seconds.  Run directly, it checks nothing and only costs ten
;;;; seconds, which is why neither tests.lisp's :import-from list nor
;;;; *process-tier-suites* names it.

(defpackage #:cl-mcp/tests/timeout-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:ok))

(in-package #:cl-mcp/tests/timeout-test)

(deftest slow-test
  (sleep 10)
  (ok t "this should not be reached if timeout fires"))
