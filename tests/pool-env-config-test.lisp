;;;; tests/pool-env-config-test.lisp
;;;;
;;;; Coverage for the env-var-driven defaults that seed
;;;; *worker-pool-warmup* and *max-pool-size* at load time.  Tests
;;;; the helper directly rather than reloading the system, so the
;;;; defvars in the running image are not disturbed.

(defpackage #:cl-mcp/tests/pool-env-config-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:signals)
  (:import-from #:cl-mcp/src/pool
                #:*worker-pool-warmup*
                #:*max-pool-size*
                #:initialize-pool))

(in-package #:cl-mcp/tests/pool-env-config-test)

(defparameter +probe-var+ "CL_MCP_TEST_ENV_INT_PROBE"
  "Env var name reserved for these tests; kept distinct from the
real CL_MCP_* knobs so a developer running tests with the live env
configured does not see interference.")

(defmacro %with-env ((name value) &body body)
  "Set env var NAME to VALUE for the duration of BODY, restoring its
prior value on exit.  Treats an empty string as the unset state, which
is what %env-int observes."
  (let ((saved (gensym "SAVED-")))
    `(let ((,saved (uiop:getenv ,name)))
       (unwind-protect
            (progn
              (setf (uiop:getenv ,name) (or ,value ""))
              ,@body)
         (setf (uiop:getenv ,name) (or ,saved ""))))))

(deftest env-int-returns-default-when-empty
  (testing "%env-int returns DEFAULT when the env value is empty"
    (%with-env (+probe-var+ "")
      (ok (= 7 (cl-mcp/src/pool::%env-int +probe-var+ 7))
          "empty value returns default"))))

(deftest env-int-parses-valid-integer
  (testing "%env-int parses a numeric env value"
    (%with-env (+probe-var+ "42")
      (ok (= 42 (cl-mcp/src/pool::%env-int +probe-var+ 7))
          "valid integer is honoured"))))

(deftest env-int-falls-back-on-junk
  (testing "%env-int falls back to DEFAULT when the env value is junk"
    (%with-env (+probe-var+ "not-a-number")
      ;; Suppress the warning; we are asserting the fallback, not the
      ;; warning text.
      (handler-bind ((warning #'muffle-warning))
        (ok (= 9 (cl-mcp/src/pool::%env-int +probe-var+ 9))
            "unparseable value returns default")))))

(deftest env-int-rejects-below-min
  (testing "%env-int returns DEFAULT when the value is below :min"
    (%with-env (+probe-var+ "-1")
      (handler-bind ((warning #'muffle-warning))
        (ok (= 5 (cl-mcp/src/pool::%env-int +probe-var+ 5 :min 0))
            "negative value rejected when min=0")))))

(deftest env-int-accepts-zero-when-min-zero
  (testing "%env-int accepts 0 when :min is 0"
    (%with-env (+probe-var+ "0")
      (ok (zerop (cl-mcp/src/pool::%env-int +probe-var+ 5 :min 0))
          "zero is accepted when allowed"))))

(deftest env-int-accepts-positive-when-min-one
  (testing "%env-int accepts a positive value when :min is 1"
    (%with-env (+probe-var+ "32")
      (ok (= 32 (cl-mcp/src/pool::%env-int +probe-var+ 16 :min 1))
          "positive integer accepted"))))

(deftest initialize-pool-rejects-warmup-above-max
  (testing "initialize-pool errors when *worker-pool-warmup* exceeds *max-pool-size*"
    ;; Plain dynamic binding works because the validation error is signaled
    ;; on the calling thread before %schedule-replenish (which would spawn
    ;; a background thread) runs.  No cross-thread binding propagation is
    ;; needed for this path.
    (let ((cl-mcp/src/pool::*worker-pool-warmup* 5)
          (cl-mcp/src/pool::*max-pool-size* 2))
      (ok (signals (cl-mcp/src/pool:initialize-pool))
          "init signals when warmup > max"))))
