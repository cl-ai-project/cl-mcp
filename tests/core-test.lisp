;;;; tests/core-test.lisp

(defpackage #:lisp-mcp-server/tests/core-test
  (:use #:cl #:rove)
  (:import-from #:lisp-mcp-server/src/core #:version)
  (:import-from #:lisp-mcp-server/src/run))

(in-package #:lisp-mcp-server/tests/core-test)

(deftest version-available
  (testing "version returns a non-empty string"
    (ok (stringp (version)))
    (ok (> (length (version)) 0))))

(deftest run-skeleton
  (testing "run returns T for skeleton"
    ;; Provide empty in/out streams so the loop hits EOF immediately and doesn't
    ;; block on *standard-input* during automated runs.
    (let* ((in (make-string-input-stream ""))
           (out (make-string-output-stream)))
      (ok (eq t (lisp-mcp-server/src/run:run :transport :stdio :in in :out out))))))

(deftest stdio-one-message
  (testing "run processes one line from :in and writes to :out"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n")
           (in (make-string-input-stream req))
           (out (make-string-output-stream)))
      (ok (eq t (lisp-mcp-server/src/run:run :transport :stdio :in in :out out)))
      (let* ((s (get-output-stream-string out)))
        (ok (> (length s) 0))
        (ok (search "\"result\"" s))))))
