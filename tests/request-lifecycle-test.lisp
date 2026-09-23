;;;; tests/request-lifecycle-test.lisp
;;;;
;;;; Fixed cases for a proxied request's lifecycle (Phase 4B): which request a
;;;; cancellation acts on, what a request is reported to have done, and that
;;;; nothing runs twice.  The generated scenarios are in
;;;; specs/request-lifecycle.lisp; these pin the faults fixed in 4B and the
;;;; checks themselves, run in the default suite and need no cl-spec.
;;;;
;;;; Most drive the real PROXY-TO-WORKER, WORKER-RPC and CANCEL-REQUEST over a
;;;; real socket to a fake worker (specs/request-fixtures.lisp), whose ledger
;;;; of received requests is the independent account of what ran.  Their
;;;; orderings are fixed -- a request held at the worker, another waiting
;;;; behind it -- not raced; racing them is 4D.

(defpackage #:cl-mcp/tests/request-lifecycle-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/request-lifecycle
                #:register-request
                #:unregister-request
                #:find-request
                #:request-key
                #:note-request-phase
                #:note-request-worker
                #:begin-send
                #:note-response
                #:cancel-request-record
                #:request-outcome
                #:cancellation-requested-p)
  (:import-from #:cl-mcp/specs/request-fixtures
                #:run-scenario
                #:scenario-violations
                #:execution-status
                #:error-result-p
                #:result-text
                #:+behaviors+
                #:+cancel-points+)
  (:import-from #:cl-mcp/src/repl-core
                #:repl-eval)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-eval-response)
  ;; Bare: threads through the BT nickname.
  (:import-from #:bordeaux-threads))

(in-package #:cl-mcp/tests/request-lifecycle-test)

(defun %scenario (&rest scenario)
  "Run SCENARIO and return its observations."
  (run-scenario scenario))

(defun %received (observed method)
  (and (member method (getf observed :received) :test #'equal) t))

;;; ------------------------------------------------------------------------
;;; The registry

(deftest a-request-is-identified-by-its-session-and-id
  (testing "two sessions' requests with the same id are two requests"
    (let ((a (register-request "session-a" 1))
          (b (register-request "session-b" 1)))
      (unwind-protect
           (progn
             (ok (eq a (find-request "session-a" 1)))
             (ok (eq b (find-request "session-b" 1)))
             (cancel-request-record b (lambda (w) (declare (ignore w))))
             (ok (not (cancellation-requested-p a)) "cancelling one leaves the other"))
        (unregister-request a)
        (unregister-request b))))
  (testing "the integer 1 and the string \"1\" are two ids"
    (ok (not (equal (request-key "s" 1) (request-key "s" "1")))))
  (testing "a request unregisters only itself"
    (let ((first (register-request "session-a" 2))
          (second (register-request "session-a" 2)))
      (unregister-request first)
      (ok (eq second (find-request "session-a" 2)))
      (unregister-request second)
      (ok (null (find-request "session-a" 2))))))

(deftest a-cancellation-acts-by-how-far-the-request-got
  ;; Fresh objects stand in for workers: a worker stopped for a cancellation
  ;; is remembered, and a keyword would be remembered across runs.
  (let ((stopped '())
        (worker-1 (list :worker-1))
        (worker-2 (list :worker-2))
        (worker-3 (list :worker-3)))
    (flet ((cancel (record)
             (cancel-request-record record (lambda (w) (push w stopped)))))
      (testing "before it reaches its worker it is only marked"
        (let ((record (register-request "phases" 1)))
          (note-request-phase record :acquiring)
          (ok (eq :marked (cancel record)))
          (ok (null stopped) "and no worker is stopped")
          (note-request-worker record worker-1)
          (ok (eq :cancelled (begin-send record)) "and it is never sent")
          (ok (eq :not-executed (request-outcome record)))
          (unregister-request record)))
      (testing "while its worker runs it, that worker is stopped"
        (let ((record (register-request "phases" 2)))
          (note-request-worker record worker-2)
          (ok (eq :send (begin-send record)))
          (ok (eq :stopping (cancel record)))
          (ok (equal (list worker-2) stopped))
          (ok (eq :execution-unknown (request-outcome record)))
          (testing "and a request waiting on that worker is not sent to it"
            (let ((behind (register-request "phases" 3)))
              (note-request-worker behind worker-2)
              (ok (eq :worker-stopped (begin-send behind)))
              (ok (eq :not-executed (request-outcome behind)))
              (unregister-request behind)))
          (unregister-request record)))
      (testing "once answered, nothing is done"
        (setf stopped '())
        (let ((record (register-request "phases" 4)))
          (note-request-worker record worker-3)
          (begin-send record)
          (note-response record)
          (ok (eq :too-late (cancel record)))
          (ok (null stopped))
          (ok (eq :completed (request-outcome record)))
          (unregister-request record))))))

;;; ------------------------------------------------------------------------
;;; The faults fixed in 4B, over a real socket

(deftest a-cancellation-while-the-worker-is-found-stops-the-request
  ;; a: the request was registered only once it had a worker, and a
  ;; cancellation that came while the worker was being found or started was
  ;; lost -- the request then ran.
  (let ((observed (%scenario :behavior :answer :cancel :acquiring :queued nil)))
    (ok (null (scenario-violations observed)))
    (ok (not (%received observed "worker/r")) "it never reached the worker")
    (ok (equal "not-executed" (execution-status (getf observed :r))))
    (ok (not (getf observed :dropped)) "and the worker was kept")))

(deftest a-cancellation-stops-only-the-request-it-names
  ;; b: a cancellation stopped the session's worker, whatever it was running.
  (testing "R waiting behind P: R is withdrawn, P runs on"
    (let ((observed (%scenario :behavior :answer :cancel :waiting :queued nil)))
      (ok (null (scenario-violations observed)))
      (ok (not (%received observed "worker/r")))
      (ok (not (error-result-p (getf observed :p))) "P, which the worker was running, finished")
      (ok (not (getf observed :dropped)))))
  (testing "R running with Q waiting behind it: R is stopped, Q is not run"
    (let ((observed (%scenario :behavior :hold :cancel :executing :queued t)))
      (ok (null (scenario-violations observed)))
      (ok (eq :stopping (getf observed :verdict)))
      (ok (equal "execution-unknown" (execution-status (getf observed :r))))
      (ok (search "cancelled while it was running" (result-text (getf observed :r))))
      (ok (not (%received observed "worker/q")))
      (ok (equal "not-executed" (execution-status (getf observed :q)))
          "Q is told it did not run, not that it crashed while running")))
  (testing "a cancellation after the answer does nothing"
    (let ((observed (%scenario :behavior :answer :cancel :answered :queued t)))
      (ok (null (scenario-violations observed)))
      ;; NIL once the request is gone from the registry; :TOO-LATE in the
      ;; moment between its answer and that.  Either way nothing is done.
      (ok (member (getf observed :verdict) '(nil :too-late)))
      (ok (not (getf observed :dropped)))
      (ok (not (error-result-p (getf observed :q))) "the next request still runs"))))

(deftest another-session-cannot-cancel-a-request
  ;; c: requests were keyed by id alone, and every session numbers its
  ;; requests from the same small integers.
  (let ((observed (%scenario :behavior :answer :cancel :other-session :queued nil)))
    (ok (null (scenario-violations observed)))
    (ok (null (getf observed :verdict)))
    (ok (not (error-result-p (getf observed :r))))))

(deftest a-request-that-never-ran-is-not-reported-as-running
  ;; e: a request waiting behind one that failed was reported as having
  ;; timed out, or crashed, itself.
  (let ((observed (%scenario :behavior :drop :cancel :none :queued t)))
    (ok (null (scenario-violations observed)))
    (ok (equal "execution-unknown" (execution-status (getf observed :r)))
        "R reached the worker, and no answer came")
    (ok (not (%received observed "worker/q")))
    (ok (equal "not-executed" (execution-status (getf observed :q))))
    (ok (search "was not run" (result-text (getf observed :q))))))

(deftest a-worker-error-is-an-answer
  (let ((observed (%scenario :behavior :error :cancel :none :queued nil)))
    (ok (null (scenario-violations observed)))
    (ok (error-result-p (getf observed :r)))
    (ok (equal "completed" (execution-status (getf observed :r))))))

(deftest a-repl-eval-timeout-is-not-a-result
  ;; f: a timed-out evaluation went out as a successful tool result.
  (testing "repl-eval says it timed out, in a value of its own"
    (multiple-value-bind (printed raw stdout stderr context timed-out)
        (repl-eval "(sleep 5)" :timeout-seconds 0.2)
      (declare (ignore printed raw stdout stderr context))
      (ok (eq t timed-out))))
  (testing "which an expression returning :TIMEOUT does not"
    (multiple-value-bind (printed raw stdout stderr context timed-out)
        (repl-eval ":timeout" :timeout-seconds 5)
      (declare (ignore printed stdout stderr context))
      (ok (eq :timeout raw))
      (ok (null timed-out))))
  (testing "and the response is an error whose outcome is unknown"
    (let ((timed-out (build-eval-response "Evaluation timed out" :timeout "" "" nil
                                          :timed-out t))
          (returned (build-eval-response ":TIMEOUT" :timeout "" "" nil)))
      (ok (eq t (gethash "isError" timed-out)))
      (ok (equal "execution-unknown" (gethash "execution_status" timed-out)))
      (ok (null (gethash "isError" returned)))
      (ok (null (gethash "execution_status" returned))))))

;;; ------------------------------------------------------------------------
;;; The checks find what they claim to

(deftest the-scenario-checks-catch-a-wrong-lifecycle
  (flet ((swapped-violations (symbol replacement scenario)
           (let ((original (fdefinition symbol)))
             (unwind-protect
                  (progn (setf (fdefinition symbol) replacement)
                         (mapcar #'first (scenario-violations (run-scenario scenario))))
               (setf (fdefinition symbol) original)))))
    (testing "a cancellation that stops the worker whatever the request's phase"
      (ok (member :worker-stopped-for-unsent-request
                  (swapped-violations
                   'cancel-request-record
                   (lambda (record stop)
                     (funcall stop (cl-mcp/src/request-lifecycle:request-worker record))
                     :stopping)
                   '(:behavior :answer :cancel :waiting :queued nil)))))
    (testing "an account that calls every request completed"
      (ok (member :unsent-not-reported-as-not-run
                  (swapped-violations
                   'request-outcome
                   (lambda (record) (declare (ignore record)) :completed)
                   '(:behavior :drop :cancel :none :queued t)))))
    (testing "a lookup that ignores the session"
      (ok (member :other-session-cancel-acted
                  (swapped-violations
                   'find-request
                   (lambda (session id)
                     (declare (ignore session))
                     (bt:with-lock-held (cl-mcp/src/request-lifecycle:*requests-lock*)
                       (loop for key being the hash-keys
                               of cl-mcp/src/request-lifecycle:*requests*
                                 using (hash-value record)
                             when (equal (cdr key) (prin1-to-string id))
                               return record)))
                   '(:behavior :answer :cancel :other-session :queued nil)))))
    (testing "and every scenario of the real lifecycle passes"
      (ok (loop for behavior in +behaviors+
                always (loop for cancel in +cancel-points+
                             always (loop for queued in '(nil t)
                                          always (null (scenario-violations
                                                        (run-scenario
                                                         (list :behavior behavior
                                                               :cancel cancel
                                                               :queued queued)))))))))))
