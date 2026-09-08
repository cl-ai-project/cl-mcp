;;;; src/run.lisp

(defpackage #:cl-mcp/src/run
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/protocol #:process-json-line #:make-state)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/pool
                #:initialize-pool #:shutdown-pool #:%warn-if-init-without-pool)
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

(defun %resolve-output-stream (stream)
  "Follow synonym and two-way streams down to the stream that really writes."
  (typecase stream
    (synonym-stream (%resolve-output-stream
                     (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream (%resolve-output-stream
                     (two-way-stream-output-stream stream)))
    (t stream)))

(defun %process-stdout-p (stream)
  "True when STREAM ultimately writes to this process's own stdout (fd 1).
Asked of the stream itself rather than of how RUN was called: a caller that
passes :OUT *STANDARD-OUTPUT* explicitly is using the same descriptor as one
that passes nothing, and both need the same protection."
  (let ((base (%resolve-output-stream stream)))
    (and (typep base 'sb-sys:fd-stream)
         (eql 1 (sb-sys:fd-stream-fd base)))))

(defun %call-with-stdout-isolated (out thunk)
  "Call THUNK with this image's global standard streams kept off OUT.

Only when OUT really is the process's own stdout.  That descriptor is then the
JSON-RPC channel, and nothing else in the image may write to it: with the
worker pool disabled every tool runs in this process, and a thread a tool or a
test suite spawns sees only the GLOBAL value of a special -- so a stray
(FORMAT T ...) there lands between JSON-RPC lines and desynchronizes the
client.  Being invisible to spawned threads is exactly why these are SETF and
not bound.

Every stream SBCL points at that descriptor by default moves, not just
*STANDARD-OUTPUT*: *TRACE-OUTPUT* is where (TIME ...) and TRACE write and is a
synonym for the same fd, and *DEBUG-IO*, *TERMINAL-IO* and *QUERY-IO* default
to the terminal.  The three interactive ones get a two-way stream so they keep
the bidirectional contract ANSI requires of them, the same shape the worker
process uses.  *STANDARD-INPUT* moves too: a stray READ in evaluated code
would otherwise eat the client's next request line.  Logging is untouched --
*LOG-STREAM* follows *ERROR-OUTPUT* on fd 2.

The globals are restored on the way out, so a second RUN in this image does
not pick the sink up as its own OUT and silently discard every response.

One caveat for embedders: SETF assigns to the innermost binding, so a caller
that wraps RUN in its own (LET ((*STANDARD-OUTPUT* ...)) ...) gets its binding
rewritten and leaves the global -- the value spawned threads actually see --
untouched.  Both documented entry points call RUN at toplevel, where there is
no such binding."
  (if (not (%process-stdout-p out))
      (funcall thunk)
      (let ((saved-output *standard-output*)
            (saved-trace *trace-output*)
            (saved-input *standard-input*)
            (saved-debug *debug-io*)
            (saved-terminal *terminal-io*)
            (saved-query *query-io*)
            (sink (make-broadcast-stream)))
        (flet ((interactive ()
                 (make-two-way-stream (make-concatenated-stream) sink)))
          (unwind-protect
               (progn
                 (setf *standard-output* sink
                       *trace-output* sink
                       *standard-input* (make-concatenated-stream)
                       *debug-io* (interactive)
                       *terminal-io* (interactive)
                       *query-io* (interactive))
                 (funcall thunk))
            (setf *standard-output* saved-output
                  *trace-output* saved-trace
                  *standard-input* saved-input
                  *debug-io* saved-debug
                  *terminal-io* saved-terminal
                  *query-io* saved-query))))))

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
  (%warn-if-init-without-pool *use-worker-pool*)
  (ecase transport
    (:stdio
     (%call-with-stdout-isolated
      out
      (lambda ()
        (when *use-worker-pool* (initialize-pool))
        (unwind-protect
             (let ((state (make-state))
                   (cl-mcp/src/protocol:*current-session-id* "stdio"))
               (log-event :info "stdio.start")
               (loop for line = (handler-case
                                    (%read-line-limited in :eof
                                                        +max-json-line-bytes+)
                                  (line-too-long (e)
                                    (log-event :warn "stdio.read.line-too-long"
                                               "error" (princ-to-string e))
                                    ;; Drain remaining bytes on the current line
                                    ;; so the next read-line starts fresh.
                                    (loop for ch = (read-char in nil nil)
                                          while (and ch
                                                     (not (char= ch #\Newline))))
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
          (when *use-worker-pool* (ignore-errors (shutdown-pool)))))))
    (:tcp
     (log-event :info "tcp.start" "host" host "port" port)
     (serve-tcp :host host :port port :accept-once accept-once
                :on-listening on-listening))))
