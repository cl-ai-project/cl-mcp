;;;; cl-mcp.asd

;; Pre-load SBCL contribs that source files reference via package-qualified
;; symbols (sb-posix:getpid, sb-posix:sigterm, ...).  SBCL resolves package
;; names embedded in FASLs at load time, before any top-level form in the
;; FASL runs, so adding (require :sb-posix) inside a source file is too late.
;; Putting it here guarantees the contrib is available before ASDF loads any
;; cl-mcp FASL — including from the bare-SBCL worker, whose dep graph would
;; not otherwise pull in sb-posix transitively (see %build-sbcl-args).
#+sbcl (require :sb-posix)
#+sbcl (require :sb-cltl2)

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
  :in-order-to ((test-op (test-op "cl-mcp/tests"))))
