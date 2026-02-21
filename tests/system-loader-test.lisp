;;;; tests/system-loader-test.lisp

(defpackage #:cl-mcp/tests/system-loader-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/system-loader
                #:load-system)
  (:import-from #:cl-mcp/src/system-loader-core
                #:%load-with-timeout))

(in-package #:cl-mcp/tests/system-loader-test)

(deftest load-system-basic
  (testing "loads an already-available system and returns structured result"
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (string= (gethash "system" ht) "cl-mcp"))
      (ok (integerp (gethash "duration_ms" ht)))
      (ok (integerp (gethash "warnings" ht))))))

(deftest load-system-force-reload
  (testing "force=true clears and reloads the system"
    (let ((ht (load-system "cl-mcp" :force t)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (eq t (gethash "forced" ht))))))

(deftest load-system-nonexistent
  (testing "returns error status for nonexistent system"
    (let ((ht (load-system "nonexistent-system-that-does-not-exist-12345"
                           :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "error"))
      (ok (stringp (gethash "message" ht))))))

(deftest load-system-timeout
  (testing "returns timeout status when worker is still running at deadline"
    ;; Use %load-with-timeout directly with a thunk that sleeps well beyond
    ;; the polling window to guarantee the worker is still alive at deadline.
    (multiple-value-bind (result-list timed-out-p errored-p)
        (%load-with-timeout
         (lambda () (sleep 10) :never-reached)
         0.05)
      (ok timed-out-p "should report timeout")
      (ok (not errored-p))
      (ok (null result-list)))))

(deftest load-system-clear-fasls-flag
  (testing "clear_fasls flag is reflected in response"
    (let ((ht (load-system "cl-mcp" :force t :clear-fasls nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (null (gethash "clear_fasls" ht))))))

(deftest load-system-warning-fields
  (testing "warning fields are properly typed in response"
    ;; Verify the warning-related fields exist and have correct types.
    ;; An already-loaded system should produce zero warnings.
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (integerp (gethash "warnings" ht)))
      (ok (zerop (gethash "warnings" ht)))
      ;; No warning_details key when warnings are zero
      (ok (null (gethash "warning_details" ht))))))

(deftest load-system-force-false-no-clear
  (testing "force=false skips clearing and uses quickload"
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (null (gethash "forced" ht))))))

(deftest timeout-returns-completed-work
  (testing "worker completing within polling granularity returns success, not timeout"
    ;; Keep this test stable on slower CI runners (especially macOS),
    ;; where thread startup and timer scheduling jitter can be higher.
    ;; timeout-seconds=0.2001 rounds up to five 50ms polling intervals
    ;; (about 250ms effective). The worker sleeps 220ms, so it exceeds the
    ;; nominal timeout but still completes before the rounded polling deadline
    ;; with enough slack to avoid flaky false timeouts.
    (multiple-value-bind (result-list timed-out-p errored-p)
        (%load-with-timeout
         (lambda () (sleep 0.22) :done)
         0.2001)
      (ok (not timed-out-p) "completed work should not be reported as timeout")
      (ok (not errored-p))
      (ok (equal result-list '(:done))))))
