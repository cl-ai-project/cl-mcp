;;;; cl-mcp.asd

(asdf:defsystem "cl-mcp"
  :class :package-inferred-system
  :description "Model Context Protocol server for Common Lisp"
  :author "cxxxr, Satoshi Imai"
  :license "MIT"
  :version "1.0.0"
  :depends-on (:alexandria
               :cl-ppcre
               :yason
               :usocket
               :bordeaux-threads
               :eclector
               :hunchentoot
               "cl-mcp/main")
  :in-order-to ((test-op (test-op "cl-mcp/tests"))))

;; Alias system so package-inferred dependencies on the ECLECTOR.PARSE-RESULT
;; package resolve via the main Eclector distribution available in Quicklisp.
(asdf:defsystem "eclector.parse-result"
  :description "Compatibility shim: route eclector.parse-result to eclector"
  :depends-on ("eclector"))
