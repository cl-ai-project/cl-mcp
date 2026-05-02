;;;; tests-heavy.lisp — heavy-tier test aggregate.
;;;;
;;;; Holds the three test families that dominate suite wall time
;;;; because each deftest spawns one or more child SBCLs (worker pool
;;;; tests) or stands up a full Hunchentoot server (http-test):
;;;;
;;;;   - cl-mcp/tests/pool-test
;;;;   - cl-mcp/tests/http-test
;;;;   - cl-mcp/tests/pool-kill-worker-test
;;;;
;;;; Run on demand: `(asdf:test-system :cl-mcp/tests-heavy)`.  Not
;;;; wired into upstream CI's fast lane (cl-mcp.asd's test-op points
;;;; at cl-mcp/tests-fast).  See `.planning/phases/999-worker-image-cache/`
;;;; for the parking-lot plan to make these cheap enough to unify.

(defpackage #:cl-mcp/tests-heavy
  (:use #:cl)
  (:import-from #:rove)
  (:import-from #:cl-mcp/tests/pool-test)
  (:import-from #:cl-mcp/tests/http-test)
  (:import-from #:cl-mcp/tests/pool-kill-worker-test))

(in-package #:cl-mcp/tests-heavy)

(defmethod asdf:perform :after ((op asdf:test-op)
                                (system (eql (asdf:find-system :cl-mcp/tests-heavy))))
  (let ((test-packages (remove-if-not
                        (lambda (dep)
                          (and (stringp dep)
                               (uiop:string-prefix-p "cl-mcp/tests/" dep)))
                        (asdf:system-depends-on system))))
    (rove:run test-packages)))
