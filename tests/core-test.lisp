;;;; tests/core-test.lisp

(defpackage #:cl-mcp/tests/core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%parse-readtable-designator)
  (:import-from #:cl-mcp/src/core #:version)
  (:import-from #:cl-mcp/src/run))

(in-package #:cl-mcp/tests/core-test)

(deftest version-available
  (testing "version returns a non-empty string"
    (ok (stringp (version)))
    (ok (> (length (version)) 0))))

(deftest run-skeleton
  (testing "run returns T for skeleton"
    ;; Provide empty in/out streams so the loop hits EOF immediately and doesn't
    ;; block on *standard-input* during automated runs.
    (let ((in (make-string-input-stream ""))
          (out (make-string-output-stream)))
      (ok (eq t (cl-mcp/src/run:run :transport :stdio :in in :out out))))))

(deftest parse-readtable-designator-blank
  (testing "blank readtable arguments are ignored"
    (ok (null (%parse-readtable-designator nil)))
    (ok (null (%parse-readtable-designator "")))
    (ok (null (%parse-readtable-designator "   ")))))

(deftest stdio-one-message
  (testing "run processes one line from :in and writes to :out"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n")
           (in (make-string-input-stream req))
           (out (make-string-output-stream)))
      (ok (eq t (cl-mcp/src/run:run :transport :stdio :in in :out out)))
      (let ((s (get-output-stream-string out)))
        (ok (> (length s) 0))
        (ok (search "\"result\"" s))))))

(deftest stdio-keeps-the-image-off-the-protocol-channel
  ;; With the worker pool disabled every tool runs in the server process, and
  ;; a thread a tool or test suite spawns sees only the GLOBAL value of a
  ;; special -- so a stray (FORMAT T ...) there lands between JSON-RPC lines.
  ;; RUN moves the global output streams off that descriptor for the duration
  ;; of a stdio session.
  (testing "a stream on the process's own stdout is recognized"
    (ok (cl-mcp/src/run::%process-stdout-p sb-sys:*stdout*))
    (ok (cl-mcp/src/run::%process-stdout-p
         (make-synonym-stream 'sb-sys:*stdout*))
        "through a synonym stream, which is what *standard-output* normally is")
    (ok (not (cl-mcp/src/run::%process-stdout-p (make-string-output-stream)))
        "a caller's own stream is not the process channel")
    ;; Every composite the standard defines, not just the two SBCL happens to
    ;; use for *STANDARD-OUTPUT*: a broadcast stream reaches the descriptor if
    ;; any component does, and missing that leaves the image writing to the
    ;; protocol channel.
    (ok (cl-mcp/src/run::%process-stdout-p
         (make-broadcast-stream (make-string-output-stream) sb-sys:*stdout*))
        "through a broadcast stream that includes it")
    (ok (cl-mcp/src/run::%process-stdout-p
         (make-echo-stream (make-concatenated-stream) sb-sys:*stdout*))
        "through an echo stream")
    (ok (not (cl-mcp/src/run::%process-stdout-p
              (make-broadcast-stream (make-string-output-stream))))
        "and a broadcast stream with no component on it is left alone"))
  (testing "every stream on that descriptor moves while the session runs"
    ;; *TRACE-OUTPUT* matters as much as *STANDARD-OUTPUT* here: it is a
    ;; synonym for the same descriptor by default, and it is where (TIME ...)
    ;; and TRACE write.
    (let (inside)
      (cl-mcp/src/run::%call-with-stdout-isolated
       sb-sys:*stdout*
       (lambda ()
         (setf inside
               (list (cl-mcp/src/run::%process-stdout-p *standard-output*)
                     (cl-mcp/src/run::%process-stdout-p *trace-output*)
                     (cl-mcp/src/run::%process-stdout-p *debug-io*)
                     (cl-mcp/src/run::%process-stdout-p *terminal-io*)
                     (cl-mcp/src/run::%process-stdout-p *query-io*)))))
      (ok (equal '(nil nil nil nil nil) inside)
          "nothing reachable from a spawned thread still points at fd 1")))
  (testing "and standard input stops competing for the client's requests"
    ;; A stray READ in evaluated code would otherwise consume the next
    ;; JSON-RPC request line off the same pipe the server is reading.
    (let (eof)
      (cl-mcp/src/run::%call-with-stdout-isolated
       sb-sys:*stdout*
       (lambda () (setf eof (read-char *standard-input* nil :eof))))
      (ok (eq :eof eof))))
  (testing "the replacements stay bidirectional, as ANSI requires"
    (let (readable)
      (cl-mcp/src/run::%call-with-stdout-isolated
       sb-sys:*stdout*
       (lambda () (setf readable (input-stream-p *query-io*))))
      (ok readable)))
  (testing "and are restored, so a later run does not inherit the sink as OUT"
    ;; Left in place, the sink becomes the next RUN's default OUT and every
    ;; JSON-RPC response is silently discarded.
    (let ((before (list *standard-output* *trace-output* *standard-input*
                        *debug-io* *terminal-io* *query-io*)))
      (cl-mcp/src/run::%call-with-stdout-isolated sb-sys:*stdout*
                                                  (lambda () nil))
      (ok (equal before (list *standard-output* *trace-output* *standard-input*
                              *debug-io* *terminal-io* *query-io*)))))
  (testing "a caller's own OUT leaves the image alone"
    (let ((before *standard-output*))
      (cl-mcp/src/run::%call-with-stdout-isolated (make-string-output-stream)
                                                  (lambda () nil))
      (ok (eq before *standard-output*)))))
