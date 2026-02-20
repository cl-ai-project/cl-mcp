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
  (:import-from #:cl-mcp/src/pool
                #:*use-worker-pool*
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
