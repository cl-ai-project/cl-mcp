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
max_pool_size, warmup_target, and a workers array.

Each entry in the workers array is an object with keys:
id (integer), session (string or null, truncated to 8 chars),
tcp_port (integer), pid (integer), state (\"bound\" or \"standby\")."
  :args ()
  :body
  (let ((info (pool-status-info)))
    (setf (gethash "content" info)
          (text-content
           (with-output-to-string (s)
             (format s "Pool: ~D workers (~D bound, ~D standby)~@[ [stopped]~]"
                     (gethash "total_workers" info)
                     (gethash "bound_count" info)
                     (gethash "standby_count" info)
                     (not (gethash "pool_running" info)))
             (format s "~&Max pool size: ~A, Warmup target: ~A"
                     (gethash "max_pool_size" info)
                     (gethash "warmup_target" info))
             (let ((workers (gethash "workers" info)))
               (when (and workers (plusp (length workers)))
                 (format s "~&~%Workers:")
                 (loop for w across workers
                       do (format s "~&  #~A [~A] pid=~A port=~A~@[ session=~A~]"
                                  (gethash "id" w)
                                  (gethash "state" w)
                                  (gethash "pid" w)
                                  (gethash "tcp_port" w)
                                  (gethash "session" w))))))))
    (result id info)))
