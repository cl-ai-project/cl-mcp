;;;; tests/logging-test.lisp

(defpackage #:cl-mcp/tests/logging-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:cl-mcp/src/log
                #:*log-level* #:*log-stream* #:*log-context*
                #:*log-file-stream* #:should-log-p #:log-event
                #:setup-log-file))

(in-package #:cl-mcp/tests/logging-test)

(deftest logging-rpc-dispatch
  (testing "process-json-line emits debug logs when level=debug"
    (let ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")
          (sink (make-string-output-stream)))
      (let ((*log-level* :debug)
            (*log-stream* sink))
        (ok (stringp (process-json-line req)))
        (let ((s (get-output-stream-string sink)))
          (ok (> (length s) 0))
          (ok (search "\"event\":\"rpc.dispatch\"" s))
          (ok (search "\"event\":\"rpc.result\"" s)))))))

(deftest should-log-p-level-filtering
  (testing "warn and error are visible at default debug level"
    (let ((*log-level* :debug))
      (ok (should-log-p :debug) "debug at debug")
      (ok (should-log-p :info) "info at debug")
      (ok (should-log-p :warn) "warn at debug")
      (ok (should-log-p :error) "error at debug")))
  (testing "debug is suppressed at warn level"
    (let ((*log-level* :warn))
      (ok (not (should-log-p :debug)) "debug hidden at warn")
      (ok (not (should-log-p :info)) "info hidden at warn")
      (ok (should-log-p :warn) "warn at warn")
      (ok (should-log-p :error) "error at warn"))))

(deftest log-context-appended-to-output
  (testing "log-event includes *log-context* fields in output"
    (let ((sink (make-string-output-stream)))
      (let ((*log-level* :debug)
            (*log-stream* sink)
            (*log-context* (list "worker_id" "42" "role" "test")))
        (log-event :info "ctx.test")
        (let ((s (get-output-stream-string sink)))
          (ok (search "\"worker_id\":\"42\"" s) "worker_id in output")
          (ok (search "\"role\":\"test\"" s) "role in output"))))))

(deftest log-file-creates-timestamped-file
  (testing "setup-log-file creates a broadcast stream to stderr and file"
    (let ((tmp-dir (namestring (uiop:ensure-pathname
                                (uiop:temporary-directory)
                                :truenamize t)))
          (old-stream *log-stream*)
          (old-file-stream *log-file-stream*))
      (unwind-protect
           (let ((template (format nil "~Acl-mcp-test.log" tmp-dir)))
             (setf (uiop/os:getenv "MCP_LOG_FILE") template)
             (setf *log-stream* *error-output*)
             (setf *log-file-stream* nil)
             (setup-log-file)
             (ok (not (null *log-file-stream*)) "file stream opened")
             (ok (typep *log-stream* 'broadcast-stream) "broadcast stream created")
             ;; Write a log line and verify it appears in the file
             (let ((*log-level* :debug)
                   (*log-context* nil))
               (log-event :info "file.test" "key" "val"))
             (finish-output *log-file-stream*)
             (let ((path (pathname *log-file-stream*)))
               (ok (probe-file path) "log file exists on disk")
               (let ((content (uiop:read-file-string path)))
                 (ok (search "\"event\":\"file.test\"" content)
                     "log line written to file")
                 (ok (search "\"key\":\"val\"" content)
                     "key-value in file output"))
               ;; Cleanup
               (close *log-file-stream*)
               (ignore-errors (delete-file path))))
        (setf *log-stream* old-stream)
        (setf *log-file-stream* old-file-stream)
        (setf (uiop/os:getenv "MCP_LOG_FILE") "")))))

