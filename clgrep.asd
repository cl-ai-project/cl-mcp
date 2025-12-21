(defsystem "clgrep"
  :version "0.1.0"
  :author "clgrep contributors"
  :license "MIT"
  :depends-on (#:cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A semantic grep tool for Common Lisp codebases that understands Lisp structure"
  :in-order-to ((test-op (test-op "clgrep/tests"))))

(defsystem "clgrep/tests"
  :author "clgrep contributors"
  :license "MIT"
  :depends-on ("clgrep"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clgrep"
  :perform (test-op (op c) (symbol-call :rove :run c)))
