;;;; tests/system-loader-test.lisp

(defpackage #:cl-mcp/tests/system-loader-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
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

(deftest load-system-response-includes-warning-text
  (testing "build-load-system-response inlines warning_details in content text"
    ;; Build a fake load-system core result with warnings captured.
    ;; build-load-system-response should render them into content[].text
    ;; (the only field MCP clients display), not just the structured
    ;; warning_details field.
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "status" ht) "loaded"
            (gethash "system" ht) "fake-system"
            (gethash "duration_ms" ht) 42
            (gethash "warnings" ht) 1
            (gethash "warning_details" ht)
            "redefining FOO in DEFUN")
      (let* ((built (cl-mcp/src/tools/response-builders:build-load-system-response
                     "fake-system" ht))
             (content (gethash "content" built))
             (text (and (vectorp content)
                        (plusp (length content))
                        (gethash "text" (aref content 0)))))
        (ok (stringp text))
        (ok (search "System fake-system loaded successfully" text))
        (ok (search "(1 warning)" text))
        ;; The actual warning message must now be visible in content text.
        (ok (search "redefining FOO" text))))))

(deftest load-system-response-truncates-long-warnings
  (testing "warning text longer than the cap is truncated with a marker"
    (let ((ht (make-hash-table :test #'equal))
          (long-text (make-string 3000 :initial-element #\W)))
      (setf (gethash "status" ht) "loaded"
            (gethash "system" ht) "fake-system"
            (gethash "duration_ms" ht) 1
            (gethash "warnings" ht) 1
            (gethash "warning_details" ht) long-text)
      (let* ((built (cl-mcp/src/tools/response-builders:build-load-system-response
                     "fake-system" ht))
             (content (gethash "content" built))
             (text (gethash "text" (aref content 0))))
        (ok (stringp text))
        (ok (search "more characters truncated" text))))))

(deftest load-system-force-false-no-clear
  (testing "force=false skips clearing and uses quickload"
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (null (gethash "forced" ht))))))

(deftest timeout-returns-completed-work
  (testing "worker completing within polling granularity returns success, not timeout"
    ;; timeout=0.001s → ceiling(0.001/0.05)=1 → effective deadline ≈ 50ms.
    ;; Worker sleeps 10ms, exceeding the nominal 1ms timeout but finishing
    ;; well before the 50ms polling deadline (~40ms slack).  This avoids
    ;; flaky failures on slow CI runners (macOS) where the previous 30ms
    ;; slack was insufficient.
    (multiple-value-bind (result-list timed-out-p errored-p)
        (%load-with-timeout
         (lambda () (sleep 0.01) :done)
         0.001)
      (ok (not timed-out-p) "completed work should not be reported as timeout")
      (ok (not errored-p))
      (ok (equal result-list '(:done))))))

(deftest load-system-force-preserves-local-asd-registration
  (testing "force=true re-registers locally-loaded systems after clear-system"
    ;; Simulate a system registered only via asdf:load-asd (not on any
    ;; standard search path).  Without the fix, clear-system drops it from
    ;; the in-memory registry and the subsequent load-system errors with
    ;; "System definition for ... not found".
    (let* ((tmp-dir (uiop:ensure-directory-pathname
                     (uiop:merge-pathnames*
                      (format nil "cl-mcp-test-local-~A/" (get-universal-time))
                      (uiop:temporary-directory))))
           (asd-path (uiop:merge-pathnames* "cl-mcp-test-local.asd" tmp-dir))
           (system-name "cl-mcp-test-local"))
      (unwind-protect
           (progn
             ;; Create a minimal .asd file in a fresh temp directory.
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output
                                         :if-exists :supersede)
               (format s "(asdf:defsystem ~S :description \"test\" :components ())~%"
                       system-name))
             ;; Register it locally (not on ASDF search path).
             (asdf:load-asd asd-path)
             (ok (asdf:find-system system-name nil)
                 "system should be registered after load-asd")
             ;; Reload with force=t — this is the scenario that used to fail.
             (let ((ht (load-system system-name :force t)))
               (ok (hash-table-p ht))
               (ok (string= "loaded" (gethash "status" ht))
                   "force=true should not lose local .asd registration")))
        ;; Cleanup: remove from registry and delete temp files.
        (ignore-errors (asdf:clear-system system-name))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))
