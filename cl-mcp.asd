;;;; cl-mcp.asd

(asdf:defsystem "cl-mcp"
  :class :package-inferred-system
  :description "Model Context Protocol server for Common Lisp"
  :author "cxxxr, Satoshi Imai"
  :license "MIT"
  :version "1.0.0"
  :depends-on ("alexandria"
               "cl-ppcre"
               "yason"
               "usocket"
               "bordeaux-threads"
               "eclector"
               "hunchentoot"
               "clgrep"
               "cl-mcp/main")
  :in-order-to ((test-op (test-op "cl-mcp/tests"))))
