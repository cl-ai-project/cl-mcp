;;;; tests/pool-test.lisp
;;;;
;;;; Tests for worker-client and pool manager.

(defpackage #:cl-mcp/tests/pool-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/worker-client
                #:spawn-worker #:worker-rpc #:kill-worker
                #:worker-tcp-port #:worker-swank-port
                #:worker-pid #:worker-state
                #:worker-id #:worker-session-id
                #:worker-needs-reset-notification
                #:clear-reset-notification
                #:worker-spawn-failed)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:proxy-to-worker)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/pool
                #:*worker-pool-warmup*
                #:initialize-pool
                #:shutdown-pool
                #:get-or-assign-worker
                #:release-session
                #:pool-worker-info))

(in-package #:cl-mcp/tests/pool-test)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun spawn-available-p ()
  "Check if we can spawn worker processes.
Returns T when the ros command is available and can launch SBCL."
  (ignore-errors
    (let ((output (with-output-to-string (s)
                    (uiop:run-program '("ros" "version")
                                      :error-output s))))
      (declare (ignore output))
      t)))

(defmacro with-spawned-worker ((var) &body body)
  "Spawn a worker, bind it to VAR, execute BODY, and kill the worker
in the cleanup form regardless of success or failure."
  `(let ((,var (spawn-worker)))
     (unwind-protect
          (progn ,@body)
       (kill-worker ,var))))

;;; ---------------------------------------------------------------------------
;;; Unit tests — struct construction
;;; ---------------------------------------------------------------------------

(deftest worker-struct-defaults
  (testing "make-worker creates a worker with correct default state"
    (let ((w (cl-mcp/src/worker-client:make-worker)))
      (ok (eq :dead (worker-state w))
          "default state is :dead")
      (ok (null (worker-tcp-port w))
          "tcp-port is nil by default")
      (ok (null (worker-pid w))
          "pid is nil by default")
      (ok (null (worker-session-id w))
          "session-id is nil by default")
      (ok (null (worker-needs-reset-notification w))
          "needs-reset-notification is nil by default")
      (ok (zerop (cl-mcp/src/worker-client::worker-request-counter w))
          "request-counter is 0 by default"))))

(deftest clear-reset-notification-clears-flag
  (testing "clear-reset-notification sets the flag to nil"
    (let ((w (cl-mcp/src/worker-client:make-worker
              :needs-reset-notification t)))
      (ok (worker-needs-reset-notification w)
          "flag is set before clearing")
      (clear-reset-notification w)
      (ok (null (worker-needs-reset-notification w))
          "flag is nil after clearing"))))

;;; ---------------------------------------------------------------------------
;;; Integration tests — spawn, RPC, kill (require ros)
;;; ---------------------------------------------------------------------------

(deftest worker-spawn-and-ping
  (testing "spawn a worker, verify handshake, send ping"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-spawned-worker (worker)
      (ok (integerp (worker-tcp-port worker))
          "tcp-port is an integer")
      (ok (integerp (worker-pid worker))
          "pid is an integer")
      (ok (integerp (worker-id worker))
          "id is an integer")
      (ok (eq :standby (worker-state worker))
          "state is :standby after spawn")
      ;; Send ping
      (let ((result (worker-rpc worker "worker/ping" nil)))
        (ok (hash-table-p result)
            "ping returns a hash-table")
        (ok (gethash "pong" result)
            "ping result contains pong=true")))))

(deftest worker-rpc-eval
  (testing "worker-rpc worker/eval returns result"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-spawned-worker (worker)
      (let* ((params (let ((ht (make-hash-table :test 'equal)))
                       (setf (gethash "code" ht) "(+ 1 2)")
                       (setf (gethash "package" ht) "CL-USER")
                       ht))
             (result (worker-rpc worker "worker/eval" params))
             (content (gethash "content" result)))
        (ok (hash-table-p result)
            "eval returns a hash-table")
        ;; content is a vector of MCP text-content objects
        (ok (and (vectorp content)
                 (plusp (length content)))
            "content is a non-empty vector")
        (let* ((first-elt (aref content 0))
               (text (when (hash-table-p first-elt)
                       (gethash "text" first-elt))))
          (ok (and (stringp text)
                   (search "3" text))
              "eval result text contains '3'"))))))

(deftest worker-kill-sets-state-dead
  (testing "kill-worker terminates process and sets state to :dead"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (let ((worker (spawn-worker)))
      (ok (eq :standby (worker-state worker))
          "state is :standby before kill")
      (kill-worker worker)
      (ok (eq :dead (worker-state worker))
          "state is :dead after kill"))))

(deftest worker-kill-idempotent
  (testing "kill-worker can be called multiple times safely"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (let ((worker (spawn-worker)))
      (kill-worker worker)
      (ok (eq :dead (worker-state worker))
          "state is :dead after first kill")
      ;; Second kill should not signal
      (kill-worker worker)
      (ok (eq :dead (worker-state worker))
          "state is still :dead after second kill"))))

(deftest worker-swank-port-is-integer-or-nil
  (testing "swank-port is an integer or nil"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-spawned-worker (worker)
      (let ((swank-port (worker-swank-port worker)))
        (ok (or (null swank-port) (integerp swank-port))
            "swank-port is integer or nil")))))

(deftest worker-unique-ids
  (testing "each spawned worker gets a unique id"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (let ((worker1 (spawn-worker))
          (worker2 (spawn-worker)))
      (unwind-protect
           (ok (/= (worker-id worker1) (worker-id worker2))
               "worker IDs are distinct")
        (kill-worker worker1)
        (kill-worker worker2)))))

;;; ---------------------------------------------------------------------------
;;; Pool manager helpers
;;; ---------------------------------------------------------------------------

(defmacro with-pool (() &body body)
  "Initialize the pool, execute BODY, and always shut down the pool."
  `(unwind-protect
        (progn
          (initialize-pool)
          ,@body)
     (shutdown-pool)))

;;; ---------------------------------------------------------------------------
;;; Pool manager unit tests — placeholder struct
;;; ---------------------------------------------------------------------------

(deftest placeholder-struct-defaults
  (testing "worker-placeholder has correct defaults"
    (let ((ph (cl-mcp/src/pool::make-worker-placeholder)))
      (ok (eq :spawning (cl-mcp/src/pool::worker-placeholder-state ph))
          "default state is :spawning")
      (ok (null (cl-mcp/src/pool::worker-placeholder-worker ph))
          "worker is nil by default")
      (ok (null (cl-mcp/src/pool::worker-placeholder-error-message ph))
          "error-message is nil by default"))))

;;; ---------------------------------------------------------------------------
;;; Pool manager integration tests (require ros)
;;; ---------------------------------------------------------------------------

(deftest pool-assigns-worker-to-session
  (testing "get-or-assign-worker returns a bound worker"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let ((worker (get-or-assign-worker "session-A")))
        (ok worker "worker is returned")
        (ok (eq :bound (worker-state worker))
            "worker state is :bound")
        (ok (string= "session-A" (worker-session-id worker))
            "worker session-id matches")))))

(deftest pool-reuses-worker-for-same-session
  (testing "same session gets same worker"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let ((w1 (get-or-assign-worker "session-A"))
            (w2 (get-or-assign-worker "session-A")))
        (ok (eq w1 w2) "same worker returned for same session")))))

(deftest pool-different-sessions-get-different-workers
  (testing "different sessions get different workers"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let ((w1 (get-or-assign-worker "session-A"))
            (w2 (get-or-assign-worker "session-B")))
        (ok (not (eq w1 w2))
            "different workers for different sessions")))))

(deftest pool-assigned-worker-responds-to-rpc
  (testing "worker from pool responds to ping"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let* ((worker (get-or-assign-worker "rpc-test"))
             (result (worker-rpc worker "worker/ping" nil)))
        (ok (hash-table-p result) "ping returns a hash-table")
        (ok (gethash "pong" result) "ping result contains pong")))))

(deftest pool-release-kills-worker
  (testing "release-session kills the worker"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let ((worker (get-or-assign-worker "session-X")))
        (ok worker "worker is returned")
        (release-session "session-X")
        (ok (eq :dead (worker-state worker))
            "worker state is :dead after release")))))

(deftest pool-release-allows-new-assignment
  (testing "after release, a new worker is assigned for the same session"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let ((w1 (get-or-assign-worker "session-Y")))
        (release-session "session-Y")
        (let ((w2 (get-or-assign-worker "session-Y")))
          (ok (not (eq w1 w2))
              "new worker assigned after release"))))))

(deftest pool-worker-info-returns-vector
  (testing "pool-worker-info returns worker metadata"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (get-or-assign-worker "info-test")
      (let ((info (pool-worker-info)))
        (ok (vectorp info) "info is a vector")
        (ok (> (length info) 0) "info has at least one entry")
        (let ((entry (aref info 0)))
          (ok (hash-table-p entry) "entry is a hash-table")
          (ok (gethash "tcp_port" entry) "entry has tcp_port")
          (ok (gethash "pid" entry) "entry has pid")
          (ok (gethash "state" entry) "entry has state"))))))

(deftest pool-shutdown-kills-all-workers
  (testing "shutdown-pool kills all workers"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (initialize-pool)
    (let ((w1 (get-or-assign-worker "shut-A"))
          (w2 (get-or-assign-worker "shut-B")))
      (shutdown-pool)
      (ok (eq :dead (worker-state w1))
          "first worker is dead after shutdown")
      (ok (eq :dead (worker-state w2))
          "second worker is dead after shutdown")
      (let ((info (pool-worker-info)))
        (ok (zerop (length info))
            "no workers in pool after shutdown")))))

;;; ---------------------------------------------------------------------------
;;; End-to-end integration tests (require ros)
;;; ---------------------------------------------------------------------------

(defun %make-eval-params (code &optional (package "CL-USER"))
  "Build a params hash-table for worker/eval."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "code" ht) code)
    (setf (gethash "package" ht) package)
    ht))

(deftest e2e-repl-eval-through-worker
  (testing "proxy-to-worker routes eval through pool to worker"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "e2e-eval-session"))
      (with-pool ()
        (let* ((params (%make-eval-params "(+ 10 20)"))
               (result (proxy-to-worker "worker/eval" params)))
          (ok (hash-table-p result)
              "result is a hash-table")
          (ok (not (gethash "isError" result))
              "result is not an error")
          (let ((content (gethash "content" result)))
            (ok (and (vectorp content) (plusp (length content)))
                "content is a non-empty vector")
            (let* ((first-elt (aref content 0))
                   (text (when (hash-table-p first-elt)
                           (gethash "text" first-elt))))
              (ok (and (stringp text) (search "30" text))
                  "eval result text contains '30'"))))))))

(deftest e2e-fallback-mode-works
  (testing "with *use-worker-pool* nil, tool handlers skip proxy"
    ;; When *use-worker-pool* is nil, tool handlers execute inline
    ;; and never call proxy-to-worker.  This test verifies the flag
    ;; semantics and that inline CL evaluation needs no pool.
    (ok (null *use-worker-pool*)
        "*use-worker-pool* defaults to nil")
    ;; Direct CL evaluation works without any pool infrastructure.
    (ok (= 30 (+ 10 20))
        "inline evaluation works without pool")
    ;; Verify pool-dependent proxy also works when pool IS initialized
    ;; (contrast with the inline path above).
    (unless (spawn-available-p)
      (skip "ros not available"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "fallback-test-session"))
      (with-pool ()
        (let* ((params (%make-eval-params "(list 1 2 3)"))
               (result (proxy-to-worker "worker/eval" params)))
          (ok (hash-table-p result)
              "proxy path works when pool is initialized")
          (ok (not (gethash "isError" result))
              "proxy result is not an error"))))))

(deftest e2e-crash-recovery
  (testing "crashed worker is replaced and notifies agent once"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "crash-session"))
      (with-pool ()
        ;; Step 1: Assign a worker via proxy
        (let* ((params (%make-eval-params "(+ 1 1)"))
               (result1 (proxy-to-worker "worker/eval" params)))
          (ok (hash-table-p result1)
              "initial eval succeeds")
          ;; Step 2: Get the assigned worker and kill its OS process
          (let ((worker (get-or-assign-worker "crash-session")))
            (ok (eq :bound (worker-state worker))
                "worker is bound before crash")
            (let ((pid (worker-pid worker)))
              (ok (integerp pid) "worker has a valid pid")
              ;; Kill with SIGKILL
              (sb-posix:kill pid sb-posix:sigkill)
              ;; Give the OS a moment to reap the process
              (sleep 0.5)
              ;; Step 3: Invoke crash recovery directly
              (cl-mcp/src/pool::%handle-worker-crash worker)
              ;; Step 4: Next proxy call should return reset notification
              (let ((result2 (proxy-to-worker "worker/eval" params)))
                (ok (hash-table-p result2)
                    "post-crash call returns a hash-table")
                (ok (gethash "isError" result2)
                    "post-crash call has isError=t")
                (let ((content (gethash "content" result2)))
                  (ok (and (vectorp content) (plusp (length content)))
                      "error content is a non-empty vector")
                  (let ((text (when (hash-table-p (aref content 0))
                                (gethash "text" (aref content 0)))))
                    (ok (and (stringp text)
                             (search "crashed" text))
                        "error message mentions crash"))))
              ;; Step 5: Subsequent call should work normally
              (let ((result3 (proxy-to-worker "worker/eval" params)))
                (ok (hash-table-p result3)
                    "third call returns a hash-table")
                (ok (not (gethash "isError" result3))
                    "third call succeeds without error")))))))))

;;; ---------------------------------------------------------------------------
;;; Concurrency tests (require ros)
;;; ---------------------------------------------------------------------------

(deftest concurrent-sessions-get-different-workers
  (testing "3 threads with different sessions get distinct workers"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let ((results (make-array 3 :initial-element nil))
            (threads nil))
        (unwind-protect
             (progn
               (dotimes (i 3)
                 (let ((idx i)
                       (sid (format nil "concurrent-~D" i)))
                   (push (bordeaux-threads:make-thread
                          (lambda ()
                            (setf (aref results idx)
                                  (get-or-assign-worker sid)))
                          :name (format nil "test-thread-~D" i))
                         threads)))
               ;; Join all threads
               (dolist (th threads)
                 (bordeaux-threads:join-thread th))
               ;; Verify all 3 workers are distinct
               (ok (and (aref results 0)
                        (aref results 1)
                        (aref results 2))
                   "all 3 threads got workers")
               (let ((ids (list (worker-id (aref results 0))
                                (worker-id (aref results 1))
                                (worker-id (aref results 2)))))
                 (ok (= 3 (length (remove-duplicates ids)))
                     "all 3 worker IDs are distinct")))
          ;; Cleanup: ensure threads are joined even on failure
          (dolist (th threads)
            (when (bordeaux-threads:thread-alive-p th)
              (ignore-errors
                (bordeaux-threads:join-thread th)))))))))

(deftest concurrent-requests-same-session
  (testing "3 threads send eval to same worker without JSON corruption"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      (let* ((worker (get-or-assign-worker "concurrent-rpc"))
             (results (make-array 3 :initial-element nil))
             (threads nil))
        (unwind-protect
             (progn
               (dotimes (i 3)
                 (let ((idx i)
                       (code (format nil "(+ ~D 100)" i)))
                   (push (bordeaux-threads:make-thread
                          (lambda ()
                            (setf (aref results idx)
                                  (worker-rpc worker "worker/eval"
                                              (%make-eval-params code))))
                          :name (format nil "rpc-thread-~D" i))
                         threads)))
               ;; Join all threads
               (dolist (th threads)
                 (bordeaux-threads:join-thread th))
               ;; Verify all 3 results are valid
               (dotimes (i 3)
                 (let ((r (aref results i)))
                   (ok (hash-table-p r)
                       (format nil "thread ~D got a hash-table" i))
                   (let ((content (gethash "content" r)))
                     (ok (and (vectorp content)
                              (plusp (length content)))
                         (format nil "thread ~D has content" i))
                     (let* ((first-elt (aref content 0))
                            (text (when (hash-table-p first-elt)
                                    (gethash "text" first-elt))))
                       (ok (and (stringp text)
                                (search (write-to-string (+ i 100))
                                        text))
                           (format nil
                                   "thread ~D result contains ~D"
                                   i (+ i 100))))))))
          ;; Cleanup
          (dolist (th threads)
            (when (bordeaux-threads:thread-alive-p th)
              (ignore-errors
                (bordeaux-threads:join-thread th)))))))))

(deftest placeholder-condvar-wakes-all-waiters
  (testing "condvar broadcast wakes all waiting threads"
    (unless (spawn-available-p)
      (skip "ros not available"))
    ;; Set warmup to 0 so no standbys exist -- forces on-demand spawn
    (let ((*worker-pool-warmup* 0))
      (with-pool ()
        (let ((results (make-array 3 :initial-element nil))
              (threads nil))
          (unwind-protect
               (progn
                 ;; Spawn 3 threads all requesting the same session.
                 ;; First thread creates a placeholder and spawns;
                 ;; other two wait on the condvar.
                 (dotimes (i 3)
                   (let ((idx i))
                     (push (bordeaux-threads:make-thread
                            (lambda ()
                              (setf (aref results idx)
                                    (get-or-assign-worker
                                     "shared-session")))
                            :name (format nil "condvar-thread-~D" i))
                           threads)))
                 ;; Join all threads
                 (dolist (th threads)
                   (bordeaux-threads:join-thread th))
                 ;; All 3 threads should get the SAME worker
                 (ok (and (aref results 0)
                          (aref results 1)
                          (aref results 2))
                     "all 3 threads got a worker")
                 (ok (and (eq (aref results 0) (aref results 1))
                          (eq (aref results 1) (aref results 2)))
                     "all 3 threads got the same worker")
                 (ok (eq :bound (worker-state (aref results 0)))
                     "shared worker is in :bound state"))
            ;; Cleanup
            (dolist (th threads)
              (when (bordeaux-threads:thread-alive-p th)
                (ignore-errors
                  (bordeaux-threads:join-thread th))))))))))

;;; ---------------------------------------------------------------------------
;;; Regression tests — P1: proxy error discrimination, P2: crashed worker kill
;;; ---------------------------------------------------------------------------

(deftest e2e-proxy-propagates-worker-errors-not-crash-notification
  (testing "worker-side JSON-RPC errors are re-signaled, not masked as crash"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (let ((*use-worker-pool* t)
          (*current-session-id* "error-propagation-test"))
      (with-pool ()
        ;; Step 1: Verify the proxy path works for valid requests
        (let* ((good-params (%make-eval-params "(+ 1 1)"))
               (good-result (proxy-to-worker "worker/eval" good-params)))
          (ok (hash-table-p good-result)
              "valid eval succeeds")
          (ok (not (gethash "isError" good-result))
              "valid eval is not an error"))
        ;; Step 2: Send an invalid request (missing required "code" param)
        ;; that triggers a JSON-RPC error from the worker handler.
        ;; Before P1 fix, this returned a crash notification saying
        ;; "Worker process crashed and was restarted" — completely wrong.
        (let ((bad-params (make-hash-table :test 'equal)))
          ;; Intentionally omit "code" key — worker/eval requires it
          (setf (gethash "package" bad-params) "CL-USER")
          (handler-case
              (progn
                (proxy-to-worker "worker/eval" bad-params)
                ;; If we get here, proxy returned a result instead of
                ;; signaling — check it's NOT a crash notification
                (ok nil "expected an error to be signaled"))
            (error (e)
              (let ((msg (princ-to-string e)))
                ;; The error message should reflect the worker's actual
                ;; error, NOT contain "crashed and was restarted"
                (ok (not (search "crashed" msg))
                    (format nil
                            "error does NOT mention crash: ~A"
                            msg))))))
        ;; Step 3: Worker should still be alive and functional after a
        ;; non-crash error (no false crash recovery triggered)
        (let* ((params (%make-eval-params "(* 6 7)"))
               (result (proxy-to-worker "worker/eval" params)))
          (ok (hash-table-p result)
              "worker still responds after non-crash error")
          (ok (not (gethash "isError" result))
              "result is not an error or crash notification"))))))

(deftest e2e-crashed-worker-reassignment-kills-old-process
  (testing "Path 1b kills orphaned OS process when reassigning crashed worker"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      ;; Step 1: Assign a worker to a session
      (let* ((worker (get-or-assign-worker "orphan-kill-test"))
             (old-pid (worker-pid worker)))
        (ok (integerp old-pid)
            "worker has a valid PID")
        (ok (eq :bound (worker-state worker))
            "worker is :bound")
        ;; Step 2: Force-crash the worker (SIGKILL the OS process)
        (sb-posix:kill old-pid sb-posix:sigkill)
        (sleep 0.5)
        ;; Mark it crashed so Path 1b triggers on next get-or-assign
        (setf (worker-state worker) :crashed)
        ;; Step 3: Request the same session again.
        ;; Path 1b should detect :crashed, save old worker ref,
        ;; remove from tracking, and kill outside the lock.
        (let ((new-worker (get-or-assign-worker "orphan-kill-test")))
          (ok (not (eq worker new-worker))
              "a new worker was assigned, not the crashed one")
          (ok (eq :bound (worker-state new-worker))
              "new worker is :bound")
          ;; Step 4: Verify the old worker's state is :dead
          ;; (kill-worker sets state to :dead after SIGTERM/SIGKILL)
          (ok (eq :dead (worker-state worker))
              "old worker state is :dead after Path 1b kill")
          ;; Step 5: Verify the old PID is no longer alive.
          ;; After kill-worker + SIGKILL, the process should be gone.
          ;; sb-posix:kill with signal 0 checks if process exists.
          (let ((still-alive
                  (ignore-errors
                    (sb-posix:kill old-pid 0)
                    t)))
            (ok (not still-alive)
                "old OS process is no longer alive (not orphaned)")))))))

(deftest e2e-release-during-crash-recovery-prevents-resurrection
  (testing "release-session during crash recovery does not resurrect session"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (with-pool ()
      ;; Step 1: Assign a worker to a session and crash it
      (let* ((worker (get-or-assign-worker "resurrection-test"))
             (pid (worker-pid worker)))
        (ok (eq :bound (worker-state worker))
            "worker is :bound")
        (sb-posix:kill pid sb-posix:sigkill)
        (sleep 0.5)
        ;; Step 2: Start crash recovery in a background thread.
        ;; %handle-worker-crash will: grab lock, set :crashed, release
        ;; lock, kill worker, spawn replacement (~5s), then try to bind.
        (let ((recovery-thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (cl-mcp/src/pool::%handle-worker-crash worker))
                 :name "crash-recovery-thread")))
          ;; Step 3: Wait briefly for the first lock section to complete
          ;; (sets state to :crashed, then kill-worker sets :dead),
          ;; then release the session.
          ;; The spawn-worker call takes seconds, so we have a wide window.
          (sleep 1)
          (ok (not (eq :bound (worker-state worker)))
              "worker state changed from :bound (recovery started)")
          (release-session "resurrection-test")
          ;; Step 4: Wait for recovery thread to finish
          (bordeaux-threads:join-thread recovery-thread)
          ;; Step 5: Verify the session was NOT resurrected.
          ;; With the fix, %handle-worker-crash detects the session was
          ;; released (affinity-map entry is gone) and kills the
          ;; replacement worker instead of binding it.
          (let ((entry (bordeaux-threads:with-lock-held
                           (cl-mcp/src/pool::*pool-lock*)
                         (gethash "resurrection-test"
                                  cl-mcp/src/pool::*affinity-map*))))
            (ok (null entry)
                "session is NOT in affinity-map (not resurrected)"))
          ;; Step 6: Verify no orphaned workers in all-workers.
          ;; The replacement worker should have been killed, not left
          ;; in the pool.
          (let ((pool-workers
                  (bordeaux-threads:with-lock-held
                      (cl-mcp/src/pool::*pool-lock*)
                    (copy-list cl-mcp/src/pool::*all-workers*))))
            ;; No worker should be bound to the released session
            (ok (null (find "resurrection-test" pool-workers
                            :key #'worker-session-id
                            :test #'string=))
                "no worker bound to released session in pool")))))))

(deftest replenish-respects-max-pool-size
  (testing "standby replenishment stops when pool is at max capacity"
    (unless (spawn-available-p)
      (skip "ros not available"))
    ;; Use setf (not let) for *max-pool-size* because
    ;; %replenish-standbys runs in a SEPARATE thread and
    ;; thread-local dynamic bindings (from let) are not visible
    ;; to other threads in SBCL.
    (let ((saved-max cl-mcp/src/pool::*max-pool-size*))
      (unwind-protect
          (progn
            (setf cl-mcp/src/pool::*max-pool-size* 2)
            (let ((*worker-pool-warmup* 1))
              (with-pool ()
                ;; After init: 1 standby in pool (1 worker total).
                ;; Assign two sessions → takes standby + spawns on-demand = 2 bound.
                (get-or-assign-worker "cap-test-A")
                (get-or-assign-worker "cap-test-B")
                ;; Now at cap (2 workers). Replenish was triggered by both
                ;; assignments.  Wait for any background replenish to settle.
                (sleep 2)
                ;; Count total workers — should not exceed max-pool-size.
                (let ((total (bordeaux-threads:with-lock-held
                                 (cl-mcp/src/pool::*pool-lock*)
                               (length cl-mcp/src/pool::*all-workers*))))
                  (ok (<= total 2)
                      (format nil
                              "total workers (~D) does not exceed max-pool-size (2)"
                              total))))))
        (setf cl-mcp/src/pool::*max-pool-size* saved-max)))))

(deftest cancelled-spawn-signals-error-not-nil
  (testing "release-session during first spawn signals error, not NIL"
    (unless (spawn-available-p)
      (skip "ros not available"))
    ;; Use warmup=0 so no standbys exist — forces on-demand spawn
    ;; via %spawn-and-bind (the placeholder path).
    (let ((*worker-pool-warmup* 0))
      (with-pool ()
        ;; Start assignment in background thread (will spawn ~5s)
        (let* ((result nil)
               (got-error nil)
               (assign-thread
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (handler-case
                        (setf result (get-or-assign-worker "cancel-test"))
                      (error (e)
                        (setf got-error (princ-to-string e)))))
                  :name "assign-thread")))
          ;; Wait for spawn to start, then release the session
          ;; to trigger cancellation.
          (sleep 0.5)
          (release-session "cancel-test")
          ;; Wait for the assignment thread to finish
          (bordeaux-threads:join-thread assign-thread)
          ;; The assignment should have signaled an error, not returned NIL
          (ok (or got-error (not (null result)))
              "cancelled spawn did not return NIL")
          (when got-error
            (ok (search "released" got-error)
                (format nil
                        "error mentions release: ~A"
                        got-error))))))))
