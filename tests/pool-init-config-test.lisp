;;;; tests/pool-init-config-test.lisp
;;;;
;;;; Tests for parent-side worker-init-hook config parsing, env denylist,
;;;; ownership election, and crash-breaker isolation.

(defpackage #:cl-mcp/tests/pool-init-config-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/pool
                #:*worker-init-config*)
  (:import-from #:cl-mcp/src/worker-client
                #:make-worker #:spawn-worker #:kill-worker
                #:worker-session-id #:worker-state #:worker-rpc))

(in-package #:cl-mcp/tests/pool-init-config-test)

(defun %with-env (bindings thunk)
  "Set env BINDINGS ((name . value) ...) for the duration of THUNK, then
restore.  A NIL value unsets the variable."
  (let ((saved (loop for (name . nil) in bindings
                     collect (cons name (uiop:getenv name)))))
    (unwind-protect
         (progn
           (loop for (name . value) in bindings
                 do (if value (setf (uiop/os:getenv name) value)
                        (sb-posix:unsetenv name)))
           (funcall thunk))
      (loop for (name . value) in saved
            do (if value (setf (uiop/os:getenv name) value)
                   (sb-posix:unsetenv name))))))

(deftest parse-worker-init-config
  (testing "config is nil when SYSTEM is unset, populated when set"
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . nil))
      (lambda ()
        (ok (null (cl-mcp/src/pool::%parse-worker-init-config))
            "no SYSTEM => nil config")))
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . "recurya/dev")
                 ("MCP_WORKER_INIT_ENTRY" . "recurya/dev:start-dev-runtime!")
                 ("MCP_WORKER_INIT_MAX_FAILURES" . "1"))
      (lambda ()
        (let ((cfg (cl-mcp/src/pool::%parse-worker-init-config)))
          (ok cfg "config present")
          (ok (string= (getf cfg :system) "recurya/dev") "system parsed")
          (ok (string= (getf cfg :entry) "recurya/dev:start-dev-runtime!")
              "entry parsed")
          (ok (eql (getf cfg :max-failures) 1) "max-failures parsed"))))))

(deftest init-vars-are-denylisted
  (testing "MCP_WORKER_INIT_* are excluded from inherited worker env"
    (let ((denylist cl-mcp/src/worker-client::*worker-env-denylist*))
      (ok (member "MCP_WORKER_INIT_SYSTEM" denylist :test #'string=)
          "SYSTEM denylisted")
      (ok (member "MCP_WORKER_INIT_ENTRY" denylist :test #'string=)
          "ENTRY denylisted")
      (ok (member "MCP_WORKER_INIT_EVAL" denylist :test #'string=)
          "EVAL denylisted"))))

(deftest pool-off-guard-warns
  (testing "%warn-if-init-without-pool warns only when INIT set AND pool disabled"
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . "recurya/dev"))
      (lambda ()
        (ok (handler-case
                (progn (cl-mcp/src/pool::%warn-if-init-without-pool nil) nil)
              (warning () t))
            "warns when INIT set and pool disabled")
        (ok (handler-case
                (progn (cl-mcp/src/pool::%warn-if-init-without-pool t) t)
              (warning () nil))
            "no warning when pool enabled")))
    (%with-env '(("MCP_WORKER_INIT_SYSTEM" . nil))
      (lambda ()
        (ok (handler-case
                (progn (cl-mcp/src/pool::%warn-if-init-without-pool nil) t)
              (warning () nil))
            "no warning when INIT unset (even with pool disabled)")))))

(defun %socket-available-p ()
  "Return T if we can bind a TCP socket on localhost."
  (handler-case
      (let ((s (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                     :element-type 'character)))
        (unwind-protect t (ignore-errors (usocket:socket-close s))))
    (error () nil)))

(deftest elect-runtime-owner-rules
  (testing "grant when owner nil; re-grant same session; reclaim when owner dead"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        ;; Fake workers with NIL process-info are treated as NOT alive by
        ;; %worker-process-alive-p, so they model a dead owner.
        (let ((w-a (make-worker :id 1 :state :bound :session-id "sess-a"))
              (w-b (make-worker :id 2 :state :bound :session-id "sess-b")))
          (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
            (ok (cl-mcp/src/pool::%elect-runtime-owner w-a "sess-a")
                "grants when owner is nil")
            (ok (cl-mcp/src/pool::%elect-runtime-owner w-a "sess-a")
                "re-grants to the same session")
            (ok (cl-mcp/src/pool::%elect-runtime-owner w-b "sess-b")
                "reclaims when the current owner process is dead")))))))

(deftest elect-refuses-live-other-session
  (testing "election refuses to migrate to a different session while the owner process is alive"
    (if (not (%socket-available-p))
        (rove:skip "socket unavailable")
        (cl-mcp/src/pool::%with-owner-reset
          (lambda ()
            (let ((owner (spawn-worker))
                  (other (make-worker :id 999 :state :bound :session-id "s-other")))
              (unwind-protect
                   (progn
                     (setf (worker-session-id owner) "s-owner"
                           (worker-state owner) :bound)
                     (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                       (ok (cl-mcp/src/pool::%elect-runtime-owner owner "s-owner")
                           "owner elected")
                       (ok (not (cl-mcp/src/pool::%elect-runtime-owner other "s-other"))
                           "refuses a different session while the owner is alive")
                       (ok (eq (cdr cl-mcp/src/pool::*runtime-owner*) owner)
                           "owner unchanged after a refused election")))
                (ignore-errors (kill-worker owner)))))))))

(deftest elect-refuses-live-session-with-dead-worker
  (testing "no migration to a different session while the owner session is alive (L4)"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((w-owner (make-worker :id 10 :state :crashed :session-id "own"))
              (w-other (make-worker :id 11 :state :bound :session-id "oth")))
          (unwind-protect
               (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                 ;; owner session "own" elected and still present in the affinity map;
                 ;; its worker is dead (nil process-info => not alive)
                 (setf (gethash "own" cl-mcp/src/pool::*affinity-map*) w-owner)
                 (ok (cl-mcp/src/pool::%elect-runtime-owner w-owner "own") "owner elected")
                 (ok (not (cl-mcp/src/pool::%elect-runtime-owner w-other "oth"))
                     "refuses a different session while the owner session is still alive")
                 (ok (string= (car cl-mcp/src/pool::*runtime-owner*) "own")
                     "owner unchanged after refused migration")
                 ;; once the owner session leaves the map, reclaim is allowed
                 (remhash "own" cl-mcp/src/pool::*affinity-map*)
                 (ok (cl-mcp/src/pool::%elect-runtime-owner w-other "oth")
                     "reclaims once the owner session is gone"))
            (remhash "own" cl-mcp/src/pool::*affinity-map*)
            (remhash "oth" cl-mcp/src/pool::*affinity-map*)))))))

(deftest release-runtime-owner
  (testing "release clears ownership only for the owning worker"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((w1 (make-worker :id 1 :state :bound :session-id "s1"))
              (w2 (make-worker :id 2 :state :bound :session-id "s2")))
          (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
            (cl-mcp/src/pool::%elect-runtime-owner w1 "s1"))
          ;; releasing a non-owner does nothing
          (cl-mcp/src/pool::%release-runtime-owner-if w2)
          (ok cl-mcp/src/pool::*runtime-owner* "non-owner release is a no-op")
          ;; releasing the owner clears it
          (cl-mcp/src/pool::%release-runtime-owner-if w1)
          (ok (null cl-mcp/src/pool::*runtime-owner*)
              "owner release clears *runtime-owner*"))))))

(deftest ensure-runtime-init-drives-a-real-worker
  (testing "%ensure-runtime-init elects and drives init to a terminal state"
    (if (not (%socket-available-p))
        (rove:skip "socket unavailable")
        (cl-mcp/src/pool::%with-owner-reset
          (lambda ()
            (let ((worker (spawn-worker)))
              (unwind-protect
                   (let ((cl-mcp/src/pool::*worker-init-config*
                           (list :system nil :entry nil
                                 :eval "(+ 1 2)" :package "CL-USER"
                                 :max-failures 1 :mode "singleton")))
                     (setf (worker-session-id worker) "sx"
                           (worker-state worker) :bound)
                     (cl-mcp/src/pool::%ensure-runtime-init worker "sx")
                     ;; Poll worker/init-status directly until terminal.
                     (let ((state nil))
                       (loop repeat 60
                             for r = (ignore-errors
                                       (worker-rpc worker "worker/init-status"
                                                   nil :timeout 5))
                             when r do (setf state (gethash "init_state" r))
                             until (member state '("running" "failed")
                                            :test #'equal)
                             do (sleep 0.05))
                       (ok (equal state "running")
                           "init reached running for a trivial eval")))
                (ignore-errors (kill-worker worker)))))))))

(deftest ensure-runtime-init-failed-accounting
  (testing "a failing init counts toward failures, disables at max, releases ownership"
    (if (not (%socket-available-p))
        (rove:skip "socket unavailable")
        (cl-mcp/src/pool::%with-owner-reset
          (lambda ()
            (let ((worker (spawn-worker)))
              (unwind-protect
                   (let ((cl-mcp/src/pool::*worker-init-config*
                           (list :system nil :entry nil
                                 :eval "(error \"boom\")" :package "CL-USER"
                                 :max-failures 1 :mode "singleton")))
                     (setf (worker-session-id worker) "sf"
                           (worker-state worker) :bound)
                     (cl-mcp/src/pool::%ensure-runtime-init worker "sf")
                     (loop repeat 100
                           until (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                                   cl-mcp/src/pool::*runtime-init-disabled*)
                           do (sleep 0.05))
                     (ok (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                           cl-mcp/src/pool::*runtime-init-disabled*)
                         "init disabled after max soft failures")
                     (ok (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                           (null cl-mcp/src/pool::*runtime-owner*))
                         "ownership released after failure"))
                (ignore-errors (kill-worker worker)))))))))

(deftest kill-session-worker-rearms-quarantined-runtime
  (testing "pool-kill-worker clears the disable latch (reachable quarantine: owner nil, disabled t)"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((w (make-worker :id 30 :state :bound :session-id "kso"))
              (cl-mcp/src/pool::*worker-init-config*
                (list :system "x" :entry nil :eval nil :package "CL-USER"
                      :max-failures 1 :mode "singleton")))
          (unwind-protect
               (progn
                 ;; Reachable quarantine: disabled=t => owner=nil; a session still has a worker.
                 (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                   (setf (gethash "kso" cl-mcp/src/pool::*affinity-map*) w
                         cl-mcp/src/pool::*runtime-owner* nil
                         cl-mcp/src/pool::*runtime-init-disabled* t
                         cl-mcp/src/pool::*runtime-init-failures* 5))
                 (cl-mcp/src/pool:kill-session-worker "kso")
                 (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                   (ok (null cl-mcp/src/pool::*runtime-init-disabled*)
                       "init re-armed (disabled cleared) by pool-kill-worker")
                   (ok (zerop cl-mcp/src/pool::*runtime-init-failures*)
                       "failure count reset")))
            (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
              (remhash "kso" cl-mcp/src/pool::*affinity-map*))))))))

(deftest kill-session-worker-releases-owner
  (testing "killing the owner session releases runtime ownership (healthy owner)"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((w (make-worker :id 31 :state :bound :session-id "kow"))
              (cl-mcp/src/pool::*worker-init-config*
                (list :system "x" :max-failures 1 :mode "singleton")))
          (unwind-protect
               (progn
                 (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                   (setf (gethash "kow" cl-mcp/src/pool::*affinity-map*) w
                         cl-mcp/src/pool::*runtime-owner* (cons "kow" w)))
                 (cl-mcp/src/pool:kill-session-worker "kow")
                 (ok (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                       (null cl-mcp/src/pool::*runtime-owner*))
                     "ownership released on owner kill"))
            (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
              (remhash "kow" cl-mcp/src/pool::*affinity-map*))))))))

(deftest get-or-assign-fires-runtime-init
  (testing "get-or-assign-worker elects a runtime owner when init config is set"
    (if (not (%socket-available-p))
        (rove:skip "socket unavailable")
        (%with-env '(("MCP_WORKER_INIT_SYSTEM" . "alexandria")
                     ("MCP_WORKER_INIT_EVAL" . "(+ 1 2)")
                     ("CL_MCP_WORKER_POOL_WARMUP" . "0"))
          (lambda ()
            (unwind-protect
                 (progn
                   (cl-mcp/src/pool:initialize-pool)
                   (let ((w (cl-mcp/src/pool:get-or-assign-worker "sess-init")))
                     (ok w "worker assigned on demand")
                     (ok (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                           (and cl-mcp/src/pool::*runtime-owner*
                                (string= (car cl-mcp/src/pool::*runtime-owner*)
                                         "sess-init")))
                         "runtime owner elected for the session")))
              (ignore-errors (cl-mcp/src/pool:shutdown-pool))))))))

(deftest init-crash-excluded-from-breaker
  (testing "%init-attributable-crash-p reflects the marked set"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((w (make-worker :id 77 :state :crashed)))
          (ok (not (cl-mcp/src/pool::%init-attributable-crash-p w))
              "unmarked worker is not init-attributable")
          (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
            (setf (gethash 77 cl-mcp/src/pool::*init-attributable-crashes*) t))
          (ok (cl-mcp/src/pool::%init-attributable-crash-p w)
              "marked worker is init-attributable"))))))

(deftest pool-status-includes-init-fields
  (testing "pool-status-info exposes runtime init fields"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((info (cl-mcp/src/pool:pool-status-info)))
          (ok (nth-value 1 (gethash "init_owner_session" info))
              "init_owner_session key present")
          (ok (nth-value 1 (gethash "init_disabled" info))
              "init_disabled key present")
          (ok (nth-value 1 (gethash "init_failures" info))
              "init_failures key present"))))))

(deftest kill-session-worker-rearms-when-no-worker
  (testing "pool-kill-worker re-arms even when the session's worker was already reaped"
    (cl-mcp/src/pool::%with-owner-reset
      (lambda ()
        (let ((cl-mcp/src/pool::*worker-init-config*
                (list :system "x" :max-failures 1 :mode "singleton")))
          ;; Reachable quarantine after an init hard-crash whose worker the
          ;; health monitor already reaped: disabled=t, owner=nil, NO affinity entry.
          (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
            (setf cl-mcp/src/pool::*runtime-init-disabled* t
                  cl-mcp/src/pool::*runtime-init-failures* 3))
          (let ((result (cl-mcp/src/pool:kill-session-worker "gone-session")))
            (ok (eq result :no-worker) "no worker was bound to the session")
            (ok (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                  (null cl-mcp/src/pool::*runtime-init-disabled*))
                "init re-armed even though kill found no worker")
            (ok (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                  (zerop cl-mcp/src/pool::*runtime-init-failures*))
                "failure count reset")))))))
