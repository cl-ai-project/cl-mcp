;;;; src/tools/pool-kill-worker.lisp
;;;;
;;;; MCP tool to kill (and optionally reset) the session's worker process.

(defpackage #:cl-mcp/src/tools/pool-kill-worker
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:result)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:termination-phrase)
  (:import-from #:cl-mcp/src/reset-events
                #:reset-event-worker-id
                #:reset-event-cause)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/pool
                #:kill-session-worker
                #:get-or-assign-worker)
  (:export #:pool-kill-worker))

(in-package #:cl-mcp/src/tools/pool-kill-worker)

(defun %with-earlier-resets (text events)
  "Return TEXT followed by the resets among EVENTS other than this kill's own:
workers the session lost earlier and had not been told about (see
KILL-SESSION-WORKER).  This response is the one that tells them.  Each is
named \"Worker <id>\", as every other reset notice names it."
  (let ((earlier (remove :killed events :key #'reset-event-cause)))
    (if (null earlier)
        text
        (format nil "~A Before this, ~{Worker ~A ~A.~^ ~} This session's Lisp ~
                     state was lost with ~:[it~;them~] too."
                text
                (loop for event in earlier
                      collect (reset-event-worker-id event)
                      collect (termination-phrase event))
                (cdr earlier)))))

(define-tool "pool-kill-worker"
  :description "Kill the worker process bound to the current session.
All Lisp state (loaded systems, REPL definitions, packages) in the worker
is lost.  Use this when a worker is stuck, has corrupted state, or you
want a clean environment.

With reset=false (default): the worker is killed and the next tool call
that requires a worker (repl-eval, load-system, etc.) will automatically
spawn a fresh one.  The next call will have a small latency penalty.

With reset=true: the worker is killed AND a new one is immediately spawned
and bound to your session.  The next tool call is ready without delay.
Note that reset=true makes this call slower (includes spawn time).

In both cases, you must call load-system again to restore previously
loaded systems."
  :args ((reset :type :boolean :default nil
                :description "When true, immediately spawn a replacement worker
after killing the current one.  When false (default), defer spawning until
the next tool call that needs a worker."))
  :body
  (let ((session-id *current-session-id*))
    (cond
      ((not *use-worker-pool*)
       (result id
               (make-ht "content"
                        (text-content
                         "Worker pool is disabled. All tools run inline in the parent process.")
                        "killed" nil)))
      ((not (and (stringp session-id) (plusp (length session-id))))
       (result id
               (make-ht "content"
                        (text-content
                         "Cannot identify session. No worker to kill.")
                        "killed" nil)))
      (t
       (multiple-value-bind (kill-result events)
           (kill-session-worker session-id)
         (case kill-result
           (:no-worker
            (result id
                    (make-ht "content"
                             (text-content
                              (%with-earlier-resets
                               "No worker is bound to this session."
                               events))
                             "killed" nil)))
           (:placeholder
            (result id
                    (make-ht "content"
                             (text-content
                              (%with-earlier-resets
                               "Worker spawn was in progress and has been cancelled."
                               events))
                             "killed" nil
                             "cancelled_spawn" t)))
           (:killed
            (cond
              (reset
               (handler-case
                   (progn
                     (get-or-assign-worker session-id)
                     (result id
                             (make-ht
                              "content"
                              (text-content
                               (%with-earlier-resets
                                "Worker killed and replaced with a fresh one. Run load-system to restore your environment."
                                events))
                              "killed" t
                              "reset" t)))
                 (error (e)
                   (log-event :warn "pool.kill-worker.reset-failed"
                              "session" session-id
                              "error" (princ-to-string e))
                   (result id
                           (make-ht
                            "content"
                            (text-content
                             (%with-earlier-resets
                              (format nil
                                      "Worker killed but replacement spawn failed: ~A. The next tool call will retry automatically."
                                      (princ-to-string e))
                              events))
                            "killed" t
                            "reset" nil
                            "isError" t)))))
              (t
               (result id
                       (make-ht
                        "content"
                        (text-content
                         (%with-earlier-resets
                          "Worker killed. A fresh one will spawn on the next tool call. Run load-system to restore your environment."
                          events))
                        "killed" t
                        "reset" nil)))))))))))
