;;;; cl-mcp.asd

;; Tell ASDF that eclector.parse-result package is provided by eclector
(asdf:register-system-packages "eclector"
                               '(:eclector.parse-result
                                 :eclector.reader
                                 :eclector.base))

(asdf:defsystem "cl-mcp"
  :class :package-inferred-system
  :description "Model Context Protocol server for Common Lisp"
  :author "cxxxr, Satoshi Imai"
  :license "MIT"
  :version "2.2.0"
  :depends-on ("alexandria"
               "cl-ppcre"
               "yason"
               "usocket"
               "bordeaux-threads"
               "eclector"
               "hunchentoot"
               "cl-mcp/main")
  ;; Default test-op runs the fast tier (excludes pool-test, http-test,
  ;; pool-kill-worker-test).  Run those via `(asdf:test-system :cl-mcp/tests-heavy)`,
  ;; or the everything-aggregate via `(asdf:test-system :cl-mcp/tests)`.
  :in-order-to ((test-op (test-op "cl-mcp/tests-fast"))))
