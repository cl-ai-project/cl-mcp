;;;; src/worker/init-hook.lisp
;;;;
;;;; Worker-side machinery for the init hook.  Provides the worker-global
;;;; ASDF load lock (so cl-mcp-mediated loads never overlap), the init
;;;; state machine, entry resolution, and the worker/init-start and
;;;; worker/init-status RPC handlers.  See
;;;; docs/plans/2026-07-05-worker-init-hook-design.md.

(defpackage #:cl-mcp/src/worker/init-hook
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held)
  (:export #:*asdf-load-lock*
           #:with-asdf-load-lock))

(in-package #:cl-mcp/src/worker/init-hook)

(defvar *asdf-load-lock* (bt:make-lock "asdf-load-lock")
  "Worker-global lock serializing every cl-mcp-mediated ASDF load site
(worker/init, worker/load-system, worker/run-tests).  Prevents two
concurrent ASDF load-ops in one worker image, which the single-threaded
dispatch loop does NOT prevent because load-system/repl-eval run their
work on spawned helper threads.")

(defmacro with-asdf-load-lock (&body body)
  "Evaluate BODY holding *ASDF-LOAD-LOCK*."
  `(bt:with-lock-held (*asdf-load-lock*) ,@body))
