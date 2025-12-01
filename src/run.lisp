;;;; src/run.lisp

(defpackage #:lisp-mcp-server/src/run
  (:use #:cl)
  (:import-from #:lisp-mcp-server/src/log #:log-event)
  (:import-from #:lisp-mcp-server/src/protocol #:process-json-line #:make-state)
  (:import-from #:lisp-mcp-server/src/tcp #:serve-tcp)
  (:export #:run))

(in-package #:lisp-mcp-server/src/run)

;; MVP placeholder: provide a minimal RUN entry point signature only.
;; Real transport/protocol handling will be implemented TDD-first later.

(declaim (ftype (function (&key (:transport (member :stdio :tcp))
                                (:in stream) (:out stream)
                                (:host string) (:port (or integer null))
                                (:accept-once t) (:on-listening function))
                          (values boolean &optional))
                run))
(defun run (&key (transport :stdio) (in *standard-input*) (out *standard-output*)
                 (host "127.0.0.1") (port 0) (accept-once t) on-listening)
  "Start the MCP server loop. For :stdio, reads newline-delimited JSON from IN
and writes responses to OUT. Returns T when input is exhausted (EOF).

This is a minimal loop for E2E bring-up; full transport features come later."
  (ecase transport
    (:stdio
     (let ((state (make-state)))
       (log-event :info "stdio.start")
       (loop for line = (read-line in nil :eof)
             until (eq line :eof)
             do (let ((resp (process-json-line line state)))
                  (when resp
                    (write-line resp out)
                    (force-output out))))
       (log-event :info "stdio.stop")
       t))
    (:tcp
     (log-event :info "tcp.start" "host" host "port" port)
     (serve-tcp :host host :port port :accept-once accept-once :on-listening on-listening))))
