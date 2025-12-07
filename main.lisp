;;;; main.lisp

(defpackage #:cl-mcp/main
  (:nicknames #:cl-mcp/main #:cl-mcp #:mcp)
  (:use #:cl)
  (:import-from #:cl-mcp/src/core
                #:version)
  (:import-from #:cl-mcp/src/log
                #:log-event
                #:set-log-level-from-env)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory
                #:*project-root*)
  (:import-from #:cl-mcp/src/edit-lisp-form
                #:edit-lisp-form)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:import-from #:cl-mcp/src/code
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references)
  (:import-from #:cl-mcp/src/repl
                #:repl-eval)
  (:import-from #:cl-mcp/src/validate
                #:check-parens)
  (:import-from #:cl-mcp/src/protocol
                #:process-json-line)
  (:import-from #:cl-mcp/src/tcp
                #:serve-tcp
                #:*tcp-server-thread*
                #:*tcp-server-port*
                #:tcp-server-running-p
                #:start-tcp-server-thread
                #:ensure-tcp-server-thread
                #:stop-tcp-server-thread)
  (:import-from #:cl-mcp/src/run
                #:run)
  (:export #:run
           #:version
           ;; File system tools
           #:fs-read-file
           #:fs-write-file
           #:fs-list-directory
           #:lisp-read-file
           #:*project-root*
           #:edit-lisp-form
           #:check-parens
           ;; Code intelligence
           #:code-find-definition
           #:code-describe-symbol
           #:code-find-references
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

(in-package #:cl-mcp/main)
