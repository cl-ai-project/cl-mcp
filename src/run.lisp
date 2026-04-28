;;;; src/run.lisp

(defpackage #:cl-mcp/src/run
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/protocol #:process-json-line #:make-state)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/attach
                #:*attach-config*
                #:parse-attach-spec
                #:set-attach-from-env
                #:attach-config-host
                #:attach-config-port)
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
                                (:worker-pool t)
                                (:slynk-attach (or null string)))
                          (values boolean &optional))
                run))

(defun %dispatch-with-stdout-guard (thunk)
  "Run THUNK with `*standard-output*' bound to a capture stream so any
in-process code -- a tool, a library it calls into, a slynk-client
event handler -- cannot corrupt the stdio JSON-RPC pipe by printing.
Returns two values: THUNK's primary return, and the captured stdout
string (empty when nothing was printed).  `*error-output*' is left
alone: it maps to the process's stderr fd, which is separate from the
JSON-RPC pipe and is the intended channel for log output."
  (let* ((capture (make-string-output-stream))
         (result (let ((*standard-output* capture))
                   (funcall thunk))))
    (values result (get-output-stream-string capture))))

(defun %apply-slynk-attach (slynk-attach supplied-p)
  "Apply attach-mode configuration on `cl-mcp:run' entry.
When SUPPLIED-P is true, treat SLYNK-ATTACH as authoritative: NIL
clears `*attach-config*'; a string is parsed via `parse-attach-spec'
and bound.  When SUPPLIED-P is false, fall back to
`set-attach-from-env' so CL_MCP_SLYNK_ATTACH still applies.  Logs
`attach.configured' or `attach.cleared' so the operator can confirm
the configuration in the server log."
  (cond
    (supplied-p
     (cond
       ((null slynk-attach)
        (setf *attach-config* nil)
        (log-event :info "attach.cleared"))
       (t
        (setf *attach-config* (parse-attach-spec slynk-attach))
        (log-event :info "attach.configured"
                   "host" (attach-config-host *attach-config*)
                   "port" (attach-config-port *attach-config*)))))
    (t
     (set-attach-from-env)
     (when *attach-config*
       (log-event :info "attach.configured"
                  "host" (attach-config-host *attach-config*)
                  "port" (attach-config-port *attach-config*))))))

(defun run (&key (transport :stdio) (in *standard-input*) (out *standard-output*)
                 (host "127.0.0.1") (port 0) (accept-once t) on-listening
                 (worker-pool nil worker-pool-supplied-p)
                 (slynk-attach nil slynk-attach-supplied-p))
  "Start the MCP server loop. For :stdio, reads newline-delimited JSON from IN
and writes responses to OUT. Returns T when input is exhausted (EOF).

WORKER-POOL controls process isolation: T enables the worker pool (default),
NIL runs all tools in-process.  When not supplied, the current value of
*use-worker-pool* is used (which defaults to T unless MCP_NO_WORKER_POOL=1).

SLYNK-ATTACH, when supplied, opts the server into attach mode for the
duration of this run.  The argument is a \"host:port\" string naming a
running Slynk listener; supported tools (currently only repl-eval) will
route their evaluation to that listener instead of the hermetic worker
pool.  Passing NIL explicitly clears any previously bound configuration.
When not supplied, the CL_MCP_SLYNK_ATTACH environment variable is
consulted via `set-attach-from-env'."
  (when worker-pool-supplied-p
    (setf *use-worker-pool* worker-pool))
  (%apply-slynk-attach slynk-attach slynk-attach-supplied-p)
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
                       (multiple-value-bind (resp captured)
                           (%dispatch-with-stdout-guard
                            (lambda () (process-json-line line state)))
                         (when (plusp (length captured))
                           (log-event :warn "stdio.transport.stdout-pollution"
                                      "bytes" (length captured)
                                      "preview"
                                      (subseq captured
                                              0
                                              (min 200 (length captured)))))
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
