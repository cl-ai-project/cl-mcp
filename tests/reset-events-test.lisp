;;;; tests/reset-events-test.lisp
;;;;
;;;; Fixed cases for state-loss events (Phase 4C): the ledger's own rules,
;;;; the sequences behind each defect 4C fixed, orderings fixed with a held
;;;; fake worker, and the checks of specs/reset-fixtures.lisp run against
;;;; deliberately wrong implementations -- a check that cannot fail proves
;;;; nothing.  The generated properties are specs/reset-events.lisp.

(defpackage #:cl-mcp/tests/reset-events-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/reset-events
                #:record-termination
                #:amend-termination-exit
                #:termination-cause
                #:pending-session-resets
                #:claim-session-resets
                #:discard-session-resets
                #:reset-event-cause
                #:reset-event-status
                #:reset-event-exit-code
                #:reset-event-exit-status)
  (:import-from #:cl-mcp/src/worker-client
                #:record-worker-termination
                #:worker-state)
  (:import-from #:cl-mcp/specs/reset-fixtures
                #:run-reset-sequence
                #:reset-violation-kinds
                #:told-in
                #:run-handle-sequence)
  (:import-from #:cl-mcp/specs/request-fixtures
                #:with-fake-worker
                #:start-request
                #:request-result
                #:hold
                #:release
                #:await-received
                #:result-text
                #:execution-status
                #:run-scenario
                #:scenario-violations
                #:await-phase
                #:received-p
                #:fake-server-dropped-p)
  (:import-from #:cl-mcp/src/proxy
                #:cancel-request)
  (:import-from #:cl-mcp/src/object-registry))

(in-package #:cl-mcp/tests/reset-events-test)

;;; ------------------------------------------------------------------------
;;; The ledger

(defun %worker (&optional (session "ledger-test"))
  "A key standing for a worker: any object does."
  (declare (ignore session))
  (list :worker))

(deftest the-first-cause-recorded-is-the-one-kept
  (let ((worker (%worker)))
    (unwind-protect
         (progn
           (record-termination worker :cancelled :worker-id 1
                                                 :session-id "ledger-test" :owed t)
           (multiple-value-bind (event newp)
               (record-termination worker :crashed :worker-id 1
                                                   :session-id "ledger-test" :owed t
                                                   :reason "eof")
             (ok (not newp) "a second record makes nothing")
             (ok (eq :cancelled (reset-event-cause event))
                 "and the EOF a cancellation produced is not taken for a crash"))
           (ok (= 1 (length (pending-session-resets "ledger-test")))
               "one end, one reset owed"))
      (discard-session-resets "ledger-test"))))

(deftest exit-details-are-filled-in-and-the-cause-kept
  (let ((worker (%worker)))
    (unwind-protect
         (progn
           (record-termination worker :timeout :worker-id 2 :session-id "ledger-test"
                                                :owed t :exit-status "running"
                                                :exit-code "unknown")
           (amend-termination-exit worker "signaled" 15)
           (let ((event (first (pending-session-resets "ledger-test"))))
             (ok (eq :timeout (reset-event-cause event)))
             (ok (equal "signaled" (reset-event-exit-status event)))
             (ok (eql 15 (reset-event-exit-code event))))
           (amend-termination-exit worker "exited" 0)
           (ok (eql 15 (reset-event-exit-code
                        (first (pending-session-resets "ledger-test"))))
               "an exit already observed is not replaced"))
      (discard-session-resets "ledger-test"))))

(deftest a-reset-is-claimed-once
  (let ((a (%worker)) (b (%worker)))
    (record-termination a :crashed :worker-id 3 :session-id "ledger-test" :owed t)
    (record-termination b :killed :worker-id 4 :session-id "ledger-test" :owed t)
    (let ((claimed (claim-session-resets "ledger-test")))
      (ok (equal '(:crashed :killed) (mapcar #'reset-event-cause claimed))
          "several pending resets are claimed together, oldest first")
      (ok (every (lambda (event) (eq :delivered (reset-event-status event))) claimed)))
    (ok (null (claim-session-resets "ledger-test")) "and never again")))

(deftest a-reset-nobody-is-owed-is-never-pending
  (let ((standby (%worker)) (released (%worker)))
    (record-termination standby :crashed :worker-id 5 :session-id nil :owed t)
    (record-termination released :released :worker-id 6 :session-id "ledger-test"
                                            :owed nil)
    (ok (eq :unowed (reset-event-status
                     (cl-mcp/src/reset-events:worker-termination standby))))
    (ok (null (pending-session-resets "ledger-test")))))

(deftest a-bound-worker-owes-its-session-and-others-do-not
  (let ((bound (cl-mcp/src/worker-client::make-worker
                :id -21 :state :bound :session-id "ledger-test"))
        (standby (cl-mcp/src/worker-client::make-worker
                  :id -22 :state :standby)))
    (unwind-protect
         (progn
           (record-worker-termination bound :crashed)
           (record-worker-termination standby :crashed)
           (ok (= 1 (length (pending-session-resets "ledger-test"))))
           (ok (eq :crashed (termination-cause standby)))
           (ok (eq :unowed (reset-event-status
                            (cl-mcp/src/reset-events:worker-termination standby)))))
      (discard-session-resets "ledger-test"))))

(deftest a-worker-the-pool-let-go-is-not-marked-crashed
  ;; KILL-SESSION-WORKER and RELEASE-SESSION mark their worker :RELEASED and
  ;; signal it; the request it was running then meets an EOF.  Marking the
  ;; worker :CRASHED there made it read as a death still to be handled, and
  ;; the request as one that met a crash.
  (let ((worker (cl-mcp/src/worker-client::make-worker
                 :id -23 :state :released :session-id "ledger-test")))
    (record-worker-termination worker :killed :owed nil)
    (cl-mcp/src/worker-client::%mark-worker-crashed worker "eof")
    (ok (eq :released (worker-state worker)))
    (ok (eq :killed (termination-cause worker)))))

;;; ------------------------------------------------------------------------
;;; Reading what was told

(deftest told-in-reads-each-named-worker-and-its-cause
  (ok (equal '((12 . :crashed) (13 . :killed))
             (told-in "Worker 12 stopped unexpectedly (eof). Worker 13 was stopped by pool-kill-worker. This session's...")))
  (ok (equal '((-4 . :timeout))
             (told-in "Worker -4 did not answer within its deadline and was abandoned.")))
  (ok (null (told-in "Worker error: no. Worker RPC timed out. Worker killed."))
      "no id, nothing told")
  (ok (equal '((7 . :unreadable)) (told-in "Worker 7 vanished."))
      "an unknown phrase is not guessed"))

;;; ------------------------------------------------------------------------
;;; The sequences behind each defect

(defparameter +regressions+
  '(;; A1: a cancellation's worker was told twice -- by the request and, as a
    ;; crash, by the next one.  Here the kill is the stop, told by its own
    ;; response; the next request must say nothing.
    ("a kill is told by the kill alone"
     ((:run-work) (:acquire "s0") (:kill-session "s0") (:request "s0")))
    ;; A2: pool-kill-worker left an older, untold reset for the next request.
    ("a kill tells what the session was already owed"
     ((:run-work) (:acquire "s0") (:session-rpc-crash "s0") (:acquire "s0")
      (:kill-session "s0") (:request "s0")))
    ;; A3: recovery by the health monitor and the request that met the death
    ;; each told it, with different reasons.
    ("a death met by a request and found by the monitor is told once"
     ((:run-work) (:acquire "s0") (:request-crash "s0") (:health-check) (:run-work)
      (:request "s0")))
    ("a death the monitor recovers is told once, by the next request"
     ((:run-work) (:acquire "s0") (:die 0) (:health-check) (:run-work)
      (:request "s0") (:request "s0")))
    ;; A4: of two deaths before either was told, only the first was kept.
    ("two deaths before either is told are both told"
     ((:run-work) (:acquire "s0") (:session-rpc-crash "s0") (:acquire "s0")
      (:session-rpc-crash "s0") (:request "s0")))
    ;; A released session is owed nothing, and a session reusing its id is
    ;; told nothing of it.
    ("a release discards what its session was owed"
     ((:run-work) (:acquire "s0") (:session-rpc-crash "s0") (:release "s0")
      (:request "s0")))
    ("a shutdown discards everything"
     ((:run-work) (:acquire "s0") (:session-rpc-crash "s0") (:shutdown) (:restart)
      (:request "s0"))))
  "Fixed sequences, each one a defect 4C fixed.")

(deftest each-defect-sequence-now-passes
  (dolist (case +regressions+)
    (destructuring-bind (label operations) case
      (let ((violations (run-reset-sequence operations)))
        (ok (null violations) (format nil "~A~@[: ~S~]" label violations))))))

;;; ------------------------------------------------------------------------
;;; Orderings fixed with a held fake worker

(defun %held-request (session thunk)
  "Start SESSION's request R on a fake worker that holds it, call THUNK with
the worker once R is running there, then drop the connection under R -- a
death, as the stop that THUNK decided produces -- and return R's result."
  (with-fake-worker (server worker)
    (hold server "worker/r")
    (let ((r (start-request session 7 "worker/r")))
      (await-received server "worker/r")
      (funcall thunk worker)
      (release server "worker/r" :drop)
      (request-result r))))

(deftest a-cause-found-first-is-not-replaced-by-the-eof
  ;; The health monitor classifies the dead process while the request is
  ;; still blocked on its read; the EOF that follows says less.
  (let ((result (%held-request
                 "owner"
                 (lambda (worker)
                   (setf (cl-mcp/src/worker-client:worker-last-crash-reason worker)
                         "process-died")
                   (record-worker-termination worker :crashed
                                              :reason "process-died")))))
    (unwind-protect
         (progn
           (ok (equal '((990001 . :crashed)) (told-in (result-text result)))
               "the death is told once, in the request that met it")
           (ok (search "(process-died)" (result-text result)))
           (ok (not (search "(eof)" (result-text result))))
           (ok (equal "execution-unknown" (execution-status result))))
      (discard-session-resets "owner"))))

(deftest a-kill-during-a-request-is-told-as-a-kill
  ;; B5: the request running when pool-kill-worker ends its worker was told
  ;; the worker crashed and was restarted.  The kill is recorded first and
  ;; told by the kill's own response, as KILL-SESSION-WORKER does.
  (let* ((told-by-kill '())
         (result (%held-request
                  "owner"
                  (lambda (worker)
                    (record-worker-termination worker :killed)
                    (setf (worker-state worker) :released)
                    (setf told-by-kill (claim-session-resets "owner"))))))
    (ok (equal '(:killed) (mapcar #'reset-event-cause told-by-kill))
        "the kill's response tells it")
    (ok (search "was stopped by pool-kill-worker" (result-text result))
        "the request says what stopped its worker")
    (ok (null (told-in (result-text result))) "and does not tell it again")
    (ok (not (search "crashed" (result-text result))))
    (ok (equal "execution-unknown" (execution-status result)))
    (ok (null (pending-session-resets "owner")))))

(deftest a-cancelled-worker-is-told-once-as-a-cancellation
  (let ((observed (run-scenario '(:behavior :hold :cancel :executing :queued t))))
    (ok (null (scenario-violations observed)))
    (let ((told (append (told-in (result-text (getf observed :r)))
                        (told-in (result-text (getf observed :q))))))
      (ok (equal '((990001 . :cancelled)) told)
          (format nil "one notice, naming the cancellation: ~S" told)))))

(defun %owe-owner-a-crash (id)
  "Record that a worker with ID, bound to session \"owner\", crashed, and
return it: a reset the session is owed and has not been told."
  (let ((dead (cl-mcp/src/worker-client::make-worker
               :id id :state :bound :session-id "owner")))
    (record-worker-termination dead :crashed :reason "eof")
    dead))

(deftest a-cancellation-during-acquire-tells-only-what-it-knows
  ;; A reset is pending; a request is cancelled while its worker is being
  ;; found.  The acquire succeeds, the worker stays the session's, and the
  ;; notice may say so -- and nothing about a worker still to come.
  (%owe-owner-a-crash -31)
  (unwind-protect
       (with-fake-worker (server worker)
         (let* ((gate (sb-thread:make-semaphore))
                (r (start-request "owner" 7 "worker/r" :gate gate)))
           (await-phase "owner" 7 :acquiring)
           (cancel-request 7 "owner")
           (sb-thread:signal-semaphore gate)
           (let* ((result (request-result r))
                  (text (result-text result)))
             (ok (equal "not-executed" (execution-status result)))
             (ok (equal '((-31 . :crashed)) (told-in text)) "the reset is told once")
             (ok (search "The session is now using another worker." text)
                 "the worker the acquire found is said to be in place")
             (ok (not (search "next request" text)) "no future worker is promised")
             (ok (not (received-p server "worker/r")) "the request did not run")
             (ok (not (fake-server-dropped-p server)) "and its worker was kept")
             (ok (eq :bound (worker-state worker))))))
    (discard-session-resets "owner")))

(deftest a-pool-error-tells-a-reset-without-promising-a-worker
  (%owe-owner-a-crash -32)
  (unwind-protect
       (let* ((cl-mcp/src/proxy::*current-session-id* "owner")
              (cl-mcp/src/proxy::%cached-get-or-assign%
                (progn (cl-mcp/src/proxy::%ensure-cached-bindings)
                       (lambda (session)
                         (declare (ignore session))
                         (error "Pool size limit reached."))))
              (result (cl-mcp/src/proxy:proxy-to-worker 9 "worker/eval"
                                                        (make-hash-table :test 'equal)))
              (text (result-text result)))
         (declare (ignorable cl-mcp/src/proxy::%cached-get-or-assign%))
         (ok (search "Pool error" text))
         (ok (equal '((-32 . :crashed)) (told-in text)))
         (ok (not (search "now using" text)) "no worker was found, so none is claimed")
         (ok (not (search "new worker" text)))
         (ok (equal "not-executed" (execution-status result))))
    (discard-session-resets "owner")))

;;; ------------------------------------------------------------------------
;;; The checks catch wrong implementations

(defun %with-replaced (symbol replacement thunk)
  "Call THUNK with SYMBOL's function replaced by REPLACEMENT."
  (let ((original (fdefinition symbol)))
    (unwind-protect
         (progn (setf (fdefinition symbol) replacement)
                (funcall thunk))
      (setf (fdefinition symbol) original))))

(defun %kinds-under (symbol replacement operations)
  "Return the violation kinds OPERATIONS finds with SYMBOL replaced."
  (%with-replaced symbol replacement
                  (lambda () (reset-violation-kinds (run-reset-sequence operations)))))

(defmacro %with-ledger-lock (&body body)
  `(bt:with-lock-held (cl-mcp/src/reset-events::*reset-events-lock*) ,@body))

(deftest the-reset-checks-catch-a-wrong-ledger
  (testing "a claim that leaves the reset pending is told twice"
    (ok (member :told-twice
                (%kinds-under 'claim-session-resets
                              (lambda (session)
                                (%with-ledger-lock
                                  (copy-list (gethash session
                                                      cl-mcp/src/reset-events::*pending*))))
                              '((:run-work) (:acquire "s0") (:session-rpc-crash "s0")
                                (:request "s0") (:request "s0"))))))
  (testing "a claim that takes nothing leaves the reset untold"
    (ok (member :never-told
                (%kinds-under 'claim-session-resets
                              (lambda (session) (declare (ignore session)) nil)
                              '((:run-work) (:acquire "s0") (:session-rpc-crash "s0")
                                (:request "s0"))))))
  (testing "a release that keeps its session's resets tells them to the next"
    (ok (member :told-after-excused
                (%kinds-under 'discard-session-resets
                              (lambda (session) (declare (ignore session)) nil)
                              '((:run-work) (:acquire "s0") (:session-rpc-crash "s0")
                                (:release "s0") (:request "s0"))))))
  (testing "a later record replacing the first tells a cancellation twice"
    ;; The cancellation is recorded first; the EOF its stop produces, recorded
    ;; over it, is a second reset for the same end.
    (let ((violations
            (%with-replaced 'record-termination
                            (let ((real (fdefinition 'record-termination)))
                              (lambda (worker &rest args)
                                (%with-ledger-lock
                                  (remhash worker cl-mcp/src/reset-events::*terminations*))
                                (apply real worker args)))
                            (lambda ()
                              (scenario-violations
                               (run-scenario '(:behavior :hold :cancel :executing
                                               :queued nil)))))))
      (ok (find :worker-end-not-told-once violations :key #'first)
          (format nil "~S" violations)))
    (ok (null (scenario-violations
               (run-scenario '(:behavior :hold :cancel :executing :queued nil))))
        "and the real ledger tells it once"))
  (testing "and every one of those sequences passes on the real ledger"
    (dolist (operations '(((:run-work) (:acquire "s0") (:session-rpc-crash "s0")
                           (:request "s0") (:request "s0"))
                          ((:run-work) (:acquire "s0") (:session-rpc-crash "s0")
                           (:release "s0") (:request "s0"))
                          ((:run-work) (:acquire "s0") (:kill-session "s0")
                           (:request "s0"))))
      (let ((violations (run-reset-sequence operations)))
        (ok (null violations) (format nil "~S" violations))))))

(deftest the-reset-checks-catch-a-kill-that-tells-nothing
  ;; The kill's own response is where it is told: a pool-kill-worker that
  ;; claims the resets and then drops them from its text tells nobody.
  (let ((operations '((:run-work) (:acquire "s0") (:kill-session "s0")
                      (:request "s0"))))
    (ok (null (run-reset-sequence operations)) "the real tool tells the kill")
    (ok (member :never-told
                (%kinds-under 'cl-mcp/src/tools/pool-kill-worker::%with-resets
                              (lambda (text events &key worker-in-place)
                                (declare (ignore events worker-in-place))
                                text)
                              operations)))))

(deftest the-reset-checks-catch-a-notice-claiming-a-worker
  ;; A notice that says the session is using another worker when the pool
  ;; holds none for it.
  (let ((operations '((:run-work) (:acquire "s0") (:session-rpc-crash "s0")
                      (:release "s0") (:acquire "s1") (:session-rpc-crash "s1")
                      (:kill-session "s1"))))
    (ok (null (run-reset-sequence operations)))
    (ok (member :claims-a-worker-it-lacks
                (%kinds-under 'cl-mcp/src/proxy:reset-notice
                              (let ((real (fdefinition 'cl-mcp/src/proxy:reset-notice)))
                                (lambda (events &key worker-in-place)
                                  (declare (ignore worker-in-place))
                                  (funcall real events :worker-in-place t)))
                              operations)))))

(deftest the-handle-check-catches-a-registry-that-ignores-generations
  (let ((operations '((:register 0) (:replace 0) (:register 0) (:lookup 0 0)
                      (:register 1) (:clear 1) (:register 1) (:lookup 2 1))))
    (ok (null (run-handle-sequence operations)) "the real registry passes")
    (let ((kinds (%with-replaced
                  'cl-mcp/src/object-registry:lookup-object
                  (lambda (handle &optional
                                    (registry cl-mcp/src/object-registry:*object-registry*))
                    ;; Integer ids, in effect: the number alone decides.
                    (multiple-value-bind (generation number)
                        (cl-mcp/src/object-registry::%parse-handle handle)
                      (if generation
                          (multiple-value-bind (object found-p)
                              (gethash number
                                       (cl-mcp/src/object-registry::object-registry-storage
                                        registry))
                            (values object found-p (unless found-p :evicted)))
                          (values nil nil :invalid))))
                  (lambda () (reset-violation-kinds (run-handle-sequence operations))))))
      (ok (member :another-object kinds)
          (format nil "a stale id answered with another image's object: ~S" kinds)))))
