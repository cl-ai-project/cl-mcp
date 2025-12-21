;;;; cl-mcp.asd

;;; Register vendor/clgrep in ASDF by loading its .asd file
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((here (make-pathname :defaults *load-truename* :name nil :type nil))
         (clgrep-asd (merge-pathnames "vendor/clgrep/clgrep.asd" here)))
    (when (probe-file clgrep-asd)
      (asdf:load-asd clgrep-asd))))

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
