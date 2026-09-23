;;;; specs/request-fixtures.lisp
;;;;
;;;; A proxied request's lifecycle, run for real: PROXY-TO-WORKER, WORKER-RPC
;;;; and CANCEL-REQUEST over a real socket, to a fake worker that is a TCP
;;;; server in this image rather than a process.  For the request properties
;;;; (specs/request-lifecycle.lisp) and their fixed cases
;;;; (tests/request-lifecycle-test.lisp); needs no cl-spec.
;;;;
;;;; The fake worker keeps its own LEDGER of what reached it -- every request
;;;; line, by method -- and does what its script says with each: answer it,
;;;; answer with a JSON-RPC error, hold it until told, or drop the
;;;; connection.  The ledger is how a check knows, independently of the
;;;; proxy's own account, whether a request ran: a request the fake worker
;;;; never received did not.
;;;;
;;;; Only the pool is stood in for: GET-OR-ASSIGN-WORKER hands out the one
;;;; fake worker (and can be made to wait, to hold a request in its
;;;; acquiring phase), and stopping a worker drops its connection from the
;;;; server side, as SIGTERM would.

(defpackage #:cl-mcp/specs/request-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker
                #:cancel-request)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/worker-client
                #:make-worker)
  (:import-from #:cl-mcp/src/request-lifecycle
                #:find-request
                #:request-phase
                #:request-external-id
                #:cancellation-requested-p
                #:*requests*
                #:*requests-lock*)
  (:import-from #:cl-mcp/src/log
                #:*log-level*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  ;; Bare, as a dependency: the proxy resolves the pool's functions at run
  ;; time, so a process that loaded only these fixtures would have none.
  (:import-from #:cl-mcp/src/pool)
  ;; Bare: named in full, or through the BT nickname.
  (:import-from #:bordeaux-threads)
  (:import-from #:usocket)
  (:import-from #:yason)
  (:export #:fake-server
           #:fake-server-received
           #:fake-server-dropped-p
           #:received-p
           #:hold
           #:release
           #:with-fake-worker
           #:start-request
           #:request-result
           #:await-phase
           #:await-received
           #:registry-empty-p
           #:execution-status
           #:error-result-p
           #:result-text
           #:run-scenario
           #:scenario-violations
           #:+behaviors+
           #:+cancel-points+
           #:draw-scenario))

(in-package #:cl-mcp/specs/request-fixtures)

;;; ------------------------------------------------------------------------
;;; A. The fake worker

(defstruct (fake-server (:constructor %make-fake-server))
  "A worker that is a TCP server in this image.  SCRIPT maps a method to what
to do with a request for it: :ANSWER, :ERROR, :DROP or :HOLD."
  listener
  port
  connection
  (lock (bt:make-lock "fake-worker"))
  (condvar (bt:make-condition-variable))
  ;; Methods received, oldest first: the ledger.
  (received '())
  ;; Method -> :ANSWER / :ERROR / :DROP / :HOLD.
  (script (make-hash-table :test 'equal))
  ;; Method -> what a held request becomes when released.
  (released (make-hash-table :test 'equal))
  (dropped nil)
  thread)

(defun received-p (server method)
  "True when SERVER received a request for METHOD."
  (bt:with-lock-held ((fake-server-lock server))
    (and (member method (fake-server-received server) :test #'equal) t)))

(defun fake-server-dropped-p (server)
  "True when SERVER's connection was dropped -- by a stop, a :DROP, or the
parent closing it."
  (bt:with-lock-held ((fake-server-lock server))
    (fake-server-dropped server)))

(defun hold (server method)
  "Make SERVER hold requests for METHOD until RELEASE."
  (bt:with-lock-held ((fake-server-lock server))
    (setf (gethash method (fake-server-script server)) :hold)))

(defun release (server method &optional (action :answer))
  "Let a held request for METHOD go, as ACTION."
  (bt:with-lock-held ((fake-server-lock server))
    (setf (gethash method (fake-server-released server)) action)
    (bt:condition-notify (fake-server-condvar server))))

(defun %drop (server)
  "Drop SERVER's connection from its side, as a stopped worker's would be.

Shut down rather than closed: a close from this thread leaves the server's
own thread blocked in its read on that socket for good, while a shutdown
wakes it with end of file and sends the parent end of file too.  The thread
closes the socket itself on its way out."
  (bt:with-lock-held ((fake-server-lock server))
    (setf (fake-server-dropped server) t)
    (bt:condition-notify (fake-server-condvar server)))
  (ignore-errors
   (sb-bsd-sockets:socket-shutdown (usocket:socket (fake-server-connection server))
                                   :direction :io)))

(defun %write-json (stream object)
  "Write OBJECT to STREAM as one JSON line."
  (yason:encode object stream)
  (terpri stream)
  (finish-output stream))

(defun %serve (server)
  "Serve SERVER's one connection until it is dropped or closed."
  (let* ((connection (fake-server-connection server))
         (stream (usocket:socket-stream connection)))
    (handler-case
        (loop
          (let ((line (read-line stream nil nil)))
            (unless line (return))
            (let* ((request (yason:parse line))
                   (id (gethash "id" request))
                   (method (gethash "method" request))
                   (action nil))
              (bt:with-lock-held ((fake-server-lock server))
                (setf (fake-server-received server)
                      (append (fake-server-received server) (list method)))
                (setf action (gethash method (fake-server-script server) :answer))
                (when (eq action :hold)
                  (loop until (or (gethash method (fake-server-released server))
                                  (fake-server-dropped server))
                        do (bt:condition-wait (fake-server-condvar server)
                                              (fake-server-lock server)))
                  (setf action (if (fake-server-dropped server)
                                   :dropped
                                   (gethash method (fake-server-released server))))))
              (ecase action
                (:dropped (return))
                (:drop (%drop server) (return))
                (:answer
                 (%write-json stream
                              (make-ht "jsonrpc" "2.0" "id" id
                                       "result" (make-ht "content"
                                                         (vector (make-ht "type" "text"
                                                                          "text" method))))))
                (:error
                 (%write-json stream
                              (make-ht "jsonrpc" "2.0" "id" id
                                       "error" (make-ht "code" -32000
                                                        "message" "worker said no"))))))))
      (error () nil))
    (bt:with-lock-held ((fake-server-lock server))
      (setf (fake-server-dropped server) t)
      (bt:condition-notify (fake-server-condvar server)))
    (ignore-errors (usocket:socket-close connection))))

(defun %start-fake-server ()
  "Return a started fake server and the worker connected to it."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type 'character))
         (server (%make-fake-server :listener listener
                                    :port (usocket:get-local-port listener)))
         (accepted (bt:make-thread
                    (lambda () (usocket:socket-accept listener :element-type 'character))
                    :name "fake-worker-accept"))
         (client (usocket:socket-connect "127.0.0.1" (fake-server-port server)
                                         :element-type 'character
                                         :connection-timeout 5)))
    (setf (fake-server-connection server) (bt:join-thread accepted))
    (setf (fake-server-thread server)
          (bt:make-thread (lambda () (%serve server)) :name "fake-worker"))
    (values server
            (make-worker :id 990001 :state :bound :session-id "owner"
                         :socket client :stream (usocket:socket-stream client)))))

(defun %stop-fake-server (server worker)
  "Close WORKER's end, drop SERVER's -- a shutdown, which wakes its thread's
read (%DROP) -- and wait a bounded time for that thread, which closes its
socket on the way out; then close the listener."
  (ignore-errors (usocket:socket-close (cl-mcp/src/worker-client::worker-socket worker)))
  (%drop server)
  (sb-thread:join-thread (fake-server-thread server) :timeout 5 :default nil)
  (ignore-errors (usocket:socket-close (fake-server-listener server))))

;;; ------------------------------------------------------------------------
;;; B. The harness

(defvar *acquire-gate* nil
  "When a semaphore, GET-OR-ASSIGN-WORKER waits on it: a request held in its
acquiring phase.")

(defmacro with-fake-worker ((server worker) &body body)
  "Run BODY with SERVER and WORKER bound to a fake worker the proxy uses for
every session, and put the proxy's bindings back afterwards.

Stopping a worker drops the fake worker's connection, as SIGTERM would break
the real one's; ending it is the real KILL-WORKER, which on a worker with no
process closes its socket and marks it :DEAD."
  (let ((saved (gensym "SAVED")) (level (gensym "LEVEL")))
    `(multiple-value-bind (,server ,worker) (%start-fake-server)
       (declare (ignorable ,worker))
       (let ((,saved (list cl-mcp/src/proxy::%cached-get-or-assign%
                           cl-mcp/src/proxy::%cached-signal-worker-terminate%))
             (,level *log-level*))
         (cl-mcp/src/proxy::%ensure-cached-bindings)
         (unwind-protect
              (progn
                (setf *log-level* :error
                      cl-mcp/src/proxy::%cached-get-or-assign%
                      (lambda (session)
                        (declare (ignore session))
                        (let ((gate *acquire-gate*))
                          (when gate (sb-thread:wait-on-semaphore gate :timeout 30)))
                        ,worker)
                      cl-mcp/src/proxy::%cached-signal-worker-terminate%
                      (lambda (w) (declare (ignore w)) (%drop ,server)))
                ,@body)
           (setf cl-mcp/src/proxy::%cached-get-or-assign% (first ,saved)
                 cl-mcp/src/proxy::%cached-signal-worker-terminate% (second ,saved)
                 *log-level* ,level)
           (%stop-fake-server ,server ,worker))))))

(defstruct (started (:constructor %make-started (thread)))
  "A request running on a thread of its own."
  thread
  (result nil))

(defun start-request (session id method &key gate)
  "Start PROXY-TO-WORKER for SESSION's request ID naming METHOD, on a thread
of its own, and return a handle for REQUEST-RESULT.  GATE, a semaphore, holds
it in its acquiring phase until signalled."
  (let ((handle (%make-started nil)))
    (setf (started-thread handle)
          (bt:make-thread
           (lambda ()
             (let ((*current-session-id* session)
                   (*acquire-gate* gate))
               (setf (started-result handle)
                     (list :returned
                           (proxy-to-worker id method (make-ht "timeout_seconds" 5))))))
           :name (format nil "request-~A" id)))
    handle))

(defun request-result (handle)
  "Wait for HANDLE's request and return the result it got, or :NO-RESULT."
  (bt:join-thread (started-thread handle))
  (let ((outcome (started-result handle)))
    (if (and (consp outcome) (eq :returned (first outcome)))
        (second outcome)
        :no-result)))

(defun await-phase (session id phase &key (within 10))
  "Wait until SESSION's request ID is in PHASE; true when it got there."
  (loop repeat (* 100 within)
        do (let ((record (find-request session id)))
             (when (and record (eq phase (request-phase record)))
               (return t)))
           (sleep 0.01)
        finally (return nil)))

(defun await-received (server method &key (within 10))
  "Wait until SERVER received METHOD; true when it did."
  (loop repeat (* 100 within)
        when (received-p server method) return t
        do (sleep 0.01)
        finally (return nil)))

(defun registry-empty-p ()
  "True when no request is registered."
  (bt:with-lock-held (*requests-lock*)
    (zerop (hash-table-count *requests*))))

(defun error-result-p (result)
  "True when RESULT is a tool error."
  (and (hash-table-p result) (gethash "isError" result) t))

(defun execution-status (result)
  "Return RESULT's execution_status, or NIL."
  (and (hash-table-p result) (gethash "execution_status" result)))

(defun result-text (result)
  "Return RESULT's first text."
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (if (and (vectorp content) (plusp (length content)))
        (gethash "text" (aref content 0))
        "")))

;;; ------------------------------------------------------------------------
;;; C. Scenarios

(defparameter +behaviors+ '(:answer :error :drop :hold)
  "What the fake worker does with the request under test, R: answer it,
answer it with an error, drop the connection, or hold it until the scenario
is done with it.")

(defparameter +cancel-points+
  '(:none :acquiring :waiting :executing :answer-read :answered :other-session)
  "Where R's cancellation arrives: never; while its worker is being found;
while it waits behind another request on the worker; while the worker runs
it; after the worker's answer was read and before it was delivered; after it
was answered; or from another session using the same id.")

(defun draw-scenario ()
  "Draw a scenario with CL:RANDOM."
  (list :behavior (nth (random (length +behaviors+)) +behaviors+)
        :cancel (nth (random (length +cancel-points+)) +cancel-points+)
        :queued (zerop (random 2))))

(defun %call-with-answer-paused (function)
  "Call FUNCTION with two semaphores, READ and GO: while it runs, the answer to
the request with id 7 is paused once it has been read and before it is
delivered -- READ is signalled there, and delivery waits for GO.

The pause is in NOTE-RESPONSE, the point where an answer and a cancellation
are ordered, so a cancellation sent while it holds is one that reached the
registry after the answer was read and before it was published."
  (let* ((symbol 'cl-mcp/src/request-lifecycle:note-response)
         (original (fdefinition symbol))
         (read (sb-thread:make-semaphore))
         (go (sb-thread:make-semaphore)))
    (unwind-protect
         (progn
           (setf (fdefinition symbol)
                 (lambda (record)
                   (when (eql 7 (request-external-id record))
                     (sb-thread:signal-semaphore read)
                     (sb-thread:wait-on-semaphore go :timeout 30))
                   (funcall original record)))
           (funcall function read go))
      (setf (fdefinition symbol) original))))

(defun run-scenario (scenario)
  "Run SCENARIO and return what happened, as a plist of observations.

Up to three requests on one worker, in session \"owner\": P (id 1), which
holds the worker while R waits, only for a :WAITING cancellation; R (id 7),
the request under test; and Q (id 8) when :QUEUED.

The order is fixed, not raced.  When R reaches the worker in this scenario it
is held there until the scenario lets it go, and Q, when queued, is started
only once R is held -- so Q waits behind R on the worker's stream.  R's
cancellation is sent at the scenario's point; afterwards R is let go as its
BEHAVIOR says.  A request the scenario cancelled before it reached the worker
has nothing ahead of Q, which is then started after R returns.

For an :ANSWER-READ cancellation the worker answers at once -- with an error
for an :ERROR behavior, a result otherwise -- and R is paused after reading
the answer and before delivering it (%CALL-WITH-ANSWER-PAUSED).  The
cancellation runs on a thread of its own, since ending the worker waits for
the stream R holds; R is let go once the cancellation is registered.  The
observed scenario records the behavior that ran."
  (destructuring-bind (&key behavior cancel queued) scenario
    (when (eq cancel :answer-read)
      (setf behavior (if (eq behavior :error) :error :answer)))
    (flet ((run (read go)
             (with-fake-worker (server worker)
               (let ((gate (and (eq cancel :acquiring) (sb-thread:make-semaphore)))
                     (held (not (member cancel '(:acquiring :waiting :answer-read))))
                     (blocker nil) (r nil) (q nil) (verdict :not-sent))
                 (if held
                     (hold server "worker/r")
                     (setf (gethash "worker/r" (fake-server-script server)) behavior))
                 (when (eq cancel :waiting)
                   (hold server "worker/p")
                   (setf blocker (start-request "owner" 1 "worker/p"))
                   (await-received server "worker/p"))
                 (setf r (start-request "owner" 7 "worker/r" :gate gate))
                 ;; R at the worker -- held there, or its answer read and
                 ;; paused -- and Q waiting behind it.
                 (when (or held read)
                   (if read
                       (sb-thread:wait-on-semaphore read :timeout 30)
                       (await-received server "worker/r"))
                   (when queued
                     (setf q (start-request "owner" 8 "worker/q"))
                     (await-phase "owner" 8 :waiting-to-send)
                     ;; Registered as waiting; the moment to block on the stream.
                     (sleep 0.05)))
                 (ecase cancel
                   ((:none :answered) nil)
                   (:acquiring
                    (await-phase "owner" 7 :acquiring)
                    (setf verdict (cancel-request 7 "owner"))
                    (sb-thread:signal-semaphore gate))
                   (:waiting
                    (await-phase "owner" 7 :waiting-to-send)
                    (sleep 0.05)
                    (setf verdict (cancel-request 7 "owner"))
                    (release server "worker/p"))
                   (:executing
                    (setf verdict (cancel-request 7 "owner")))
                   (:answer-read
                    (let ((canceller (bt:make-thread
                                      (lambda () (cancel-request 7 "owner"))
                                      :name "answer-read-cancel"))
                          (record (find-request "owner" 7)))
                      (loop repeat 1000
                            until (and record (cancellation-requested-p record))
                            do (sleep 0.01))
                      (sb-thread:signal-semaphore go)
                      (setf verdict (bt:join-thread canceller))))
                   (:other-session
                    (setf verdict (cancel-request 7 "intruder"))))
                 ;; Let a held R go as its behavior says -- unless its
                 ;; cancellation already dropped the connection under it.
                 (when (and held (not (eq cancel :executing)))
                   (release server "worker/r" (if (eq behavior :hold) :answer behavior)))
                 (let ((r-result (request-result r)))
                   (when (eq cancel :answered)
                     (setf verdict (cancel-request 7 "owner")))
                   (when (and queued (null q))
                     (setf q (start-request "owner" 8 "worker/q")))
                   (list :scenario (list :behavior behavior :cancel cancel :queued queued)
                         :verdict verdict
                         :r r-result
                         :q (and q (request-result q))
                         :p (and blocker (request-result blocker))
                         :received (bt:with-lock-held ((fake-server-lock server))
                                     (copy-list (fake-server-received server)))
                         :dropped (fake-server-dropped-p server)
                         :registry-empty (registry-empty-p)))))))
      (if (eq cancel :answer-read)
          (%call-with-answer-paused #'run)
          (run nil nil)))))

(defun %received (observed method)
  (and (member method (getf observed :received) :test #'equal) t))

(defun scenario-violations (observed)
  "Return the request-lifecycle promises OBSERVED breaks, judged against what
the fake worker received rather than what the proxy says.

- Every request got exactly one result, and nothing was sent twice.
- A request the worker never received reports \"not-executed\", and is an
  error; one it received never reports \"not-executed\".
- A request the worker answered, uncancelled, gets the answer: a success, or
  for a worker error \"completed\"; one whose worker dropped it mid-run
  reports \"execution-unknown\".
- A cancellation acts on the request named, and only while it runs does it
  stop the worker: cancelled before it ran, R is not sent and the worker is
  kept; after it was answered, or from another session, nothing changes.
  Cancelled after R's answer was read and before it was delivered, the
  cancellation stands and the answer is not delivered -- never both a
  stopped worker and a success.
- Q, queued behind R, is never reported as having run when it was not sent,
  and a cancellation of R that did not stop the worker leaves Q to run.
- Nothing is left registered."
  (destructuring-bind (&key behavior cancel queued) (getf observed :scenario)
    (let ((violations '())
          (r (getf observed :r))
          (q (getf observed :q))
          (verdict (getf observed :verdict))
          (r-sent (%received observed "worker/r"))
          (q-sent (%received observed "worker/q")))
      (flet ((add (kind &rest detail) (push (list* kind detail) violations)))
        (dolist (method '("worker/r" "worker/q" "worker/p"))
          (when (> (count method (getf observed :received) :test #'equal) 1)
            (add :sent-twice :method method)))
        (unless (hash-table-p r) (add :no-result :request "R"))
        (when (and queued (not (hash-table-p q))) (add :no-result :request "Q"))
        (flet ((check-truth (label result sent)
                 (when (hash-table-p result)
                   (let ((status (execution-status result)))
                     (cond
                       ((not sent)
                        (unless (and (error-result-p result)
                                     (equal status "not-executed"))
                          (add :unsent-not-reported-as-not-run :request label
                               :status status)))
                       ((equal status "not-executed")
                        (add :sent-reported-as-not-run :request label)))))))
          (check-truth "R" r r-sent)
          (when queued (check-truth "Q" q q-sent)))
        ;; What R must have been told.
        (when (hash-table-p r)
          (case cancel
            ((:acquiring :waiting)
             (when r-sent (add :cancelled-request-was-sent))
             (unless (equal "not-executed" (execution-status r))
               (add :cancelled-before-run-not-reported)))
            ((:executing :answer-read)
             ;; Stopped while it ran -- or after its answer was read and
             ;; before it was delivered, when the cancellation, not the
             ;; answer, is what stands.
             (unless (and (error-result-p r)
                          (equal "execution-unknown" (execution-status r)))
               (add :cancelled-while-running-not-unknown
                    :status (execution-status r))))
            ((:none :answered :other-session)
             (ecase behavior
               ((:answer :hold)
                (when (error-result-p r) (add :answer-not-delivered :text (result-text r))))
               (:error
                (unless (equal "completed" (execution-status r))
                  (add :worker-error-not-completed :status (execution-status r))))
               (:drop
                (unless (equal "execution-unknown" (execution-status r))
                  (add :dropped-not-unknown :status (execution-status r))))))))
        ;; What the cancellation did.
        (case cancel
          ((:acquiring :waiting)
           (unless (eq :marked verdict) (add :wrong-verdict :verdict verdict))
           (when (getf observed :dropped) (add :worker-stopped-for-unsent-request)))
          ((:executing :answer-read)
           (unless (eq :stopping verdict) (add :wrong-verdict :verdict verdict))
           (unless (getf observed :dropped) (add :running-request-not-stopped)))
          (:answered
           (when (and verdict (not (eq :too-late verdict)))
             (add :wrong-verdict :verdict verdict)))
          (:other-session
           (when verdict (add :other-session-cancel-acted :verdict verdict))))
        ;; Q: runs unless R's cancellation stopped the worker under it.
        (when (and queued (hash-table-p q)
                   (not (member cancel '(:executing :answer-read)))
                   (not (eq behavior :drop)))
          (when (error-result-p q) (add :queued-request-harmed :text (result-text q))))
        (unless (getf observed :registry-empty) (add :left-registered)))
      (nreverse violations))))
