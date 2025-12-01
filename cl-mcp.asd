;;;; cl-mcp.asd

(asdf:defsystem "cl-mcp"
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
               "cl-mcp/main")
  :in-order-to ((test-op (test-op "cl-mcp/tests"))))
