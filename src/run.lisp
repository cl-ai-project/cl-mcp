;;;; src/run.lisp

(defpackage #:cl-mcp/src/run
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/protocol #:process-json-line #:make-state)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/pool #:initialize-pool #:shutdown-pool)
  (:import-from #:cl-mcp/src/tcp #:serve-tcp)
  (:import-from #:cl-mcp/src/worker-client
                #:%read-line-limited #:+max-json-line-bytes+
                #:line-too-long)
  (:export #:run))

(in-package #:cl-mcp/src/run)

;; MVP placeholder: provide a minimal RUN entry point signature only.
;; Real transport/protocol handling will be implemented TDD-first later.

(declaim (ftype (function (&key (:transport (member :stdio :tcp))
                                (:in stream) (:out stream)
                                (:host string) (:port (or integer null))
                                (:accept-once t) (:on-listening function)
                                (:worker-pool t))
                          (values boolean &optional))
                run))

(defun run (&key (transport :stdio) (in *standard-input*) (out *standard-output*)
                 (host "127.0.0.1") (port 0) (accept-once t) on-listening
                 (worker-pool nil worker-pool-supplied-p))
  "Start the MCP server loop. For :stdio, reads newline-delimited JSON from IN
and writes responses to OUT. Returns T when input is exhausted (EOF).

WORKER-POOL controls process isolation: T enables the worker pool (default),
NIL runs all tools in-process.  When not supplied, the current value of
*use-worker-pool* is used (which defaults to T unless MCP_NO_WORKER_POOL=1)."
  (when worker-pool-supplied-p
    (setf *use-worker-pool* worker-pool))
  (ecase transport
    (:stdio
     (when *use-worker-pool* (initialize-pool))
     (unwind-protect
         (let ((state (make-state))
               (cl-mcp/src/protocol:*current-session-id* "stdio"))
           (log-event :info "stdio.start")
           (loop for line = (handler-case
                                 (%read-line-limited in :eof +max-json-line-bytes+)
                               (line-too-long (e)
                                 (log-event :warn "stdio.read.line-too-long"
                                            "error" (princ-to-string e))
                                 ;; Drain remaining bytes on the current line
                                 ;; so the next read-line starts fresh.
                                 (loop for ch = (read-char in nil nil)
                                       while (and ch (not (char= ch #\Newline))))
                                 :read-error))
                 until (eq line :eof)
                 do (cond
                      ((eq line :read-error)
                       ;; Return JSON-RPC error for the oversized line
                       (handler-case
                           (progn
                             (write-line
                              "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32600,\"message\":\"Request too large\"}}"
                              out)
                             (force-output out))
                         (stream-error (e)
                           (log-event :warn "stdio.write.error"
                                      "error" (princ-to-string e))
                           (return))))
                      (t
                       (let ((resp (process-json-line line state)))
                         (when resp
                           (handler-case
                               (progn
                                 (write-line resp out)
                                 (force-output out))
                             (stream-error (e)
                               (log-event :warn "stdio.write.error"
                                          "error" (princ-to-string e))
                               (return))))))))
           (log-event :info "stdio.stop")
           t)
       (when *use-worker-pool* (ignore-errors (shutdown-pool)))))
    (:tcp
     (log-event :info "tcp.start" "host" host "port" port)
     (serve-tcp :host host :port port :accept-once accept-once :on-listening on-listening))))
