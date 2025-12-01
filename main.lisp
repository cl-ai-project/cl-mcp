;;;; main.lisp

(defpackage #:lisp-mcp-server/main
  (:nicknames #:lisp-mcp-server/main #:lisp-mcp-server #:mcp)
  (:use #:cl)
  (:import-from #:lisp-mcp-server/src/core
                #:version)
  (:import-from #:lisp-mcp-server/src/log
                #:log-event
                #:set-log-level-from-env)
  (:import-from #:lisp-mcp-server/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory
                #:*project-root*)
  (:import-from #:lisp-mcp-server/src/lisp-read-file
                #:lisp-read-file)
  (:import-from #:lisp-mcp-server/src/code
                #:code-find-definition
                #:code-describe-symbol)
  (:import-from #:lisp-mcp-server/src/repl
                #:repl-eval)
  (:import-from #:lisp-mcp-server/src/validate
                #:check-parens)
  (:import-from #:lisp-mcp-server/src/protocol
                #:process-json-line)
  (:import-from #:lisp-mcp-server/src/tcp
                #:serve-tcp
                #:*tcp-server-thread*
                #:*tcp-server-port*
                #:tcp-server-running-p
                #:start-tcp-server-thread
                #:ensure-tcp-server-thread
                #:stop-tcp-server-thread)
  (:import-from #:lisp-mcp-server/src/run
                #:run)
  (:export #:run
           #:version
           ;; File system tools
           #:fs-read-file
           #:fs-write-file
           #:fs-list-directory
           #:lisp-read-file
           #:*project-root*
           #:check-parens
           ;; Code intelligence
           #:code-find-definition
           #:code-describe-symbol
           ;; Logging controls
           #:set-log-level-from-env
           ;; REPL interfaces
           #:repl-eval
           ;; Protocol helpers (for tests and custom transports)
           #:process-json-line
           ;; TCP server
           #:serve-tcp
           #:*tcp-server-thread*
           #:*tcp-server-port*
           #:tcp-server-running-p
           #:start-tcp-server-thread
           #:ensure-tcp-server-thread
           #:stop-tcp-server-thread))

(in-package #:lisp-mcp-server/main)
