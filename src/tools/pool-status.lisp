;;;; src/tools/pool-status.lisp
;;;;
;;;; MCP tool for worker pool diagnostics.

(defpackage #:cl-mcp/src/tools/pool-status
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:text-content #:result)
  (:import-from #:cl-mcp/src/pool
                #:pool-status-info)
  (:export #:pool-status))

(in-package #:cl-mcp/src/tools/pool-status)

(define-tool "pool-status"
  :description "Return worker pool diagnostic information including per-worker
details and pool-level summary.  No arguments required.
Returns pool_running, total_workers, standby_count, bound_count,
max_pool_size, warmup_target, and a workers array."
  :args ()
  :body
  (let ((info (pool-status-info)))
    (setf (gethash "content" info)
          (text-content
           (format nil "Pool: ~D workers (~D bound, ~D standby)~@[ [stopped]~]"
                   (gethash "total_workers" info)
                   (gethash "bound_count" info)
                   (gethash "standby_count" info)
                   (not (gethash "pool_running" info)))))
    (result id info)))
