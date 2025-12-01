;;;; lisp-mcp-server.asd

(asdf:defsystem "lisp-mcp-server"
  :class :package-inferred-system
  :description "Model Context Protocol server for Common Lisp"
  :author ""
  :license "MIT"
  :version "0.2.0"
  :depends-on (:alexandria
               :cl-ppcre
               :yason
               :usocket
               :bordeaux-threads
               "lisp-mcp-server/main")
  :in-order-to ((test-op (test-op "lisp-mcp-server/tests"))))
