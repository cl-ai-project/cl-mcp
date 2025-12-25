;;;; main.lisp

(defpackage #:cl-mcp/main
  (:nicknames #:cl-mcp/main #:cl-mcp #:mcp)
  (:use #:cl)
  (:import-from #:cl-mcp/src/core
                #:version)
  (:import-from #:cl-mcp/src/log
                #:log-event
                #:set-log-level-from-env)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory
                #:fs-get-project-info)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:import-from #:cl-mcp/src/code
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:cl-mcp/src/repl
                #:repl-eval)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens)
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
  (:import-from #:cl-mcp/src/http
                #:*http-server*
                #:*http-server-port*
                #:http-server-running-p
                #:start-http-server
                #:stop-http-server)
  (:import-from #:cl-mcp/src/run
                #:run)
  (:import-from #:cl-mcp/src/clgrep
                #:clgrep-search)
  (:export #:run
           #:version
           ;; File system tools
           #:fs-read-file
           #:fs-write-file
           #:fs-list-directory
           #:fs-get-project-info
           #:lisp-read-file
           #:*project-root*
           #:lisp-edit-form
           #:lisp-check-parens
           ;; Code intelligence
           #:code-find-definition
           #:code-describe-symbol
           #:code-find-references
           ;; Semantic grep (clgrep)
           #:clgrep-search
           ;; Parinfer
           #:apply-indent-mode
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
           #:stop-tcp-server-thread
           ;; HTTP server (Streamable HTTP transport)
           #:*http-server*
           #:*http-server-port*
           #:http-server-running-p
           #:start-http-server
           #:stop-http-server))


(in-package #:cl-mcp/main)
