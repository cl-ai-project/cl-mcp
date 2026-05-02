;;;; tests/proxy-test.lisp
;;;;
;;;; Direct unit tests for cl-mcp/src/proxy.
;;;; The proxy verifies its late-bound symbol table and formats crash
;;;; notifications when a worker dies.  Both are critical to fail loudly
;;;; rather than silently dispatch into the void, so this suite covers
;;;; the success and failure shapes of each.

(defpackage #:cl-mcp/tests/proxy-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/proxy
                #:verify-proxy-bindings))

(in-package #:cl-mcp/tests/proxy-test)

(defun first-text (response)
  "Pull the text of the first content part out of RESPONSE, or NIL."
  (let ((content (gethash "content" response)))
    (when (and (vectorp content) (plusp (length content)))
      (gethash "text" (aref content 0)))))

(deftest verify-proxy-bindings-passes-when-symbols-exist
 (testing "verify-proxy-bindings returns T when worker pool packages are loaded"
  ;; The umbrella test system pulls in the worker-client and pool packages,
  ;; so the bindings table should resolve cleanly.
  (ok (eq t (verify-proxy-bindings)))))

(deftest verify-proxy-bindings-detects-missing-symbol
 (testing "verify-proxy-bindings signals an error listing unresolvable symbols"
  ;; Rebinding the proxy's internal bindings table forces it to look up a
  ;; nonexistent symbol while leaving the real table intact for later tests.
  (let ((cl-mcp/src/proxy::%proxy-bindings%
         '(("CL-MCP/SRC/PROXY" . "DEFINITELY-NOT-A-REAL-SYMBOL-XYZQ"))))
    (declare (ignorable cl-mcp/src/proxy::%proxy-bindings%))
    (let ((msg (handler-case
                   (progn (verify-proxy-bindings) nil)
                 (error (c) (princ-to-string c)))))
      (ok msg "an error was raised")
      (ok (search "DEFINITELY-NOT-A-REAL-SYMBOL-XYZQ" msg)
       "error message lists the missing symbol")))))

(deftest verify-proxy-bindings-detects-missing-package
 (testing "verify-proxy-bindings signals when a referenced package is absent"
  (let ((cl-mcp/src/proxy::%proxy-bindings%
         '(("NO-SUCH-PROXY-PKG-XYZQ" . "ANY"))))
    (declare (ignorable cl-mcp/src/proxy::%proxy-bindings%))
    (let ((msg (handler-case
                   (progn (verify-proxy-bindings) nil)
                 (error (c) (princ-to-string c)))))
      (ok msg)
      (ok (search "NO-SUCH-PROXY-PKG-XYZQ" msg))))))

(deftest crash-notification-includes-reason-when-given
 (testing "%crash-notification-result includes the reason in content text"
  (let* ((r (cl-mcp/src/proxy::%crash-notification-result
             :reason "OOM killed"
             :exit-status :exited
             :exit-code 137))
         (text (first-text r)))
    (ok (eq t (gethash "isError" r)) "isError flag is set")
    (ok (search "crashed" text) "text mentions crash")
    (ok (search "OOM killed" text) "reason is included")
    (ok (search "exit_status=EXITED" text) "exit-status is included")
    (ok (search "exit_code=137" text) "exit-code is included"))))

(deftest crash-notification-omits-unknown-fields
 (testing "fields equal to \"unknown\" or empty are excluded from the message"
  (let* ((r (cl-mcp/src/proxy::%crash-notification-result
             :reason "unknown"
             :exit-status ""
             :exit-code nil))
         (text (first-text r)))
    (ok (search "crashed" text))
    (ok (not (search "unknown" text)))
    (ok (not (search "exit_status=" text)))
    (ok (not (search "exit_code=" text))))))

(deftest crash-notification-no-details-when-all-empty
 (testing "no parenthetical detail when no fields are provided"
  (let* ((r (cl-mcp/src/proxy::%crash-notification-result))
         (text (first-text r)))
    (ok (search "crashed" text))
    (ok (search "load-system again" text)
     "guidance about reloading is always present"))))
