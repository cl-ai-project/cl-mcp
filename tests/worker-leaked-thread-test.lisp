;;;; tests/worker-leaked-thread-test.lisp
;;;;
;;;; A deadline that cannot stop its thread leaves that thread running in the
;;;; worker: holding locks it took, mutating state later work reads, competing
;;;; for the CPU.  The caller that hit the deadline is told; every request
;;;; after it would be served by a process that is quietly wrong.  These cover
;;;; the record of such threads, its pruning, and the worker retiring on it.

(defpackage #:cl-mcp/tests/worker-leaked-thread-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread
                #:leaked-threads
                #:forget-leaked-threads)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool)
  (:import-from #:cl-mcp/src/tools/registry
                #:get-tool-handler)
  ;; Named for the dependency alone: the tool registers itself when its file
  ;; is loaded, and the registry is empty without it.
  (:import-from #:cl-mcp/src/tools/pool-status)
  (:import-from #:cl-mcp/src/state
                #:make-state)
  ;; Named without importing: the worker server's side of this is internal, so
  ;; the tests reach it with ::, but the dependency still has to be declared
  ;; for the package-inferred system to load it.
  (:import-from #:cl-mcp/src/worker/server)
  (:import-from #:cl-mcp/src/worker-client)
  (:import-from #:cl-mcp/src/pool)
  (:import-from #:cl-mcp/src/proxy)
  (:import-from #:yason))

(in-package #:cl-mcp/tests/worker-leaked-thread-test)

(defvar *probe-release* nil
  "Set to end the deliberately unstoppable probe threads.
They cannot be interrupted -- that is the point of them -- so ending them has
to be cooperative.")

(defun release-probe-threads ()
  "Let the probe threads finish and wait for them, so the image is left clean."
  (setf *probe-release* t)
  (dolist (thread (leaked-threads))
    (loop repeat 200
          while (bt:thread-alive-p thread)
          do (sleep 0.02))))

(defmacro with-clean-leak-record (&body body)
  "Run BODY with the leak record empty, and leave the image as it was found.
Deliberately leaking a thread is the only way to test any of this, and both
the record and the thread are image-wide: forgetting the record alone would
leave a live thread running into whatever runs next.

The release flag is cleared on the way in as well as set on the way out: the
previous block's cleanup left it set, and a probe that starts released is not
a probe -- it finishes immediately and leaks nothing, which would leave the
assertions passing for the wrong reason."
  `(unwind-protect (progn (setf *probe-release* nil)
                          (forget-leaked-threads)
                          ,@body)
     (release-probe-threads)
     (forget-leaked-threads)))

(defun unstoppable-probe ()
  "Run until released, in a way neither half of the deadline's stop can end.

SB-SYS:WITHOUT-INTERRUPTS defers both the cooperative unwind and
DESTROY-THREAD, which is what being unstoppable actually looks like.  The
thread polls *PROBE-RELEASE* rather than sleeping a fixed time so the test can
end it deliberately: a fixed sleep leaves it running into whatever runs next,
and makes the assertions depend on the runner being fast enough to observe
it."
  (sb-sys:without-interrupts
    (let ((stop (+ (get-internal-real-time)
                   (* 30 internal-time-units-per-second))))
      (loop until (or *probe-release*
                      (> (get-internal-real-time) stop))
            do (sleep 0.02))))
  :finished)

(defun leak-one-thread (&key (name "leak-probe"))
  "Run a deadline against a thread that cannot be stopped.
Returns two values: whether the deadline said so, and the thread itself, taken
from the record rather than from the deadline -- which reports only that a
thread was left behind, not which one."
  (let ((reported (nth-value 2 (call-with-deadline-thread
                                #'unstoppable-probe 0.3 :name name))))
    (values reported (first (leaked-threads)))))

(defun eval-params (code &optional timeout)
  "Params for a worker/eval request against a real worker process."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "code" ht) code
          (gethash "package" ht) "CL-USER")
    (when timeout
      (setf (gethash "timeout_seconds" ht) timeout))
    ht))

(defun leaking-params ()
  "A worker/eval request whose run the worker's own deadline cannot stop.
SB-SYS:WITHOUT-INTERRUPTS defers both the cooperative unwind and
DESTROY-THREAD, so the deadline answers its caller and the thread keeps
running -- the condition the worker retires for."
  (eval-params "(sb-sys:without-interrupts (sleep 10))" 1))

(deftest leaked-threads-records-what-a-deadline-could-not-stop
  (testing "a run the deadline gave up on is recorded, not merely reported"
    ;; The caller that hit the deadline is answered and moves on; the thread
    ;; outlives every later request, so the image has to be able to say it is
    ;; still carrying it.
    (with-clean-leak-record
      (ok (leak-one-thread) "the deadline reports the leak to its caller")
      (ok (= 1 (length (leaked-threads)))
          "and the image records it")))
  (testing "a run stopped at its deadline is not recorded"
    (with-clean-leak-record
      (call-with-deadline-thread (lambda () (sleep 30) :done) 0.3)
      (ok (null (leaked-threads))
          "an interruptible run leaves nothing behind")))
  (testing "a run that finished is not recorded"
    (with-clean-leak-record
      (call-with-deadline-thread (lambda () :done) 5)
      (ok (null (leaked-threads))))))

(deftest leaked-threads-stops-reporting-a-thread-that-finished
  (testing "the record prunes, so a worker that recovered is not retired"
    ;; This is the whole reason the check is made at the moment of use rather
    ;; than remembered from when it happened.  A leaked thread is running, not
    ;; dead, and it may finish -- and a worker that ends up carrying nothing
    ;; should keep the session state it still holds rather than be replaced
    ;; for a condition that has passed.
    (with-clean-leak-record
      (let ((thread (nth-value 1 (leak-one-thread))))
        (ok (= 1 (length (leaked-threads))) "recorded while it runs")
        ;; Let it finish.  It cannot be interrupted -- that is what made it
        ;; leak -- so ending it has to be cooperative.
        (release-probe-threads)
        (ok (not (bt:thread-alive-p thread)) "the thread has finished")
        (ok (null (leaked-threads))
            "and stops being counted against the image")))))

(deftest the-record-is-written-from-cleanups-and-from-several-threads
  (testing "a deadline unwound by an outer one still records what it left"
    ;; A run thread can enforce a deadline of its own -- RUN-TESTS inside a
    ;; REPL-EVAL, say -- and the outer one unwinds the inner before it can
    ;; return.  Recording on the way out would miss exactly this: the inner
    ;; call never reaches its own return, so the thread it could not stop
    ;; would run on with nothing recording it, leaving the image looking
    ;; clean while still carrying it.  Only the cleanup runs, so that is
    ;; where the record has to be written.
    (with-clean-leak-record
      (call-with-deadline-thread
       (lambda ()
         (call-with-deadline-thread #'unstoppable-probe 10
                                    :name "leak-probe-inner"))
       0.3 :name "leak-probe-outer")
      ;; Polled rather than read once: the inner call's stop and the outer's
      ;; both end around the same moment, and which finishes first is a race
      ;; the test should not be deciding.  A record that is never written
      ;; fails here just as surely, only slower.
      (let ((inner (loop repeat 100
                         thereis (find "leak-probe-inner" (leaked-threads)
                                       :key #'bt:thread-name :test #'equal)
                         do (sleep 0.05))))
        (ok inner "the unwound call's thread is on the record"))))
  (testing "deadlines expiring on several threads at once all record"
    ;; The record is image-wide and written from whichever thread hit its
    ;; deadline: one per session in a worker, and again from a nested run.
    ;; An unlocked write keeps the last one and loses the rest, which reads
    ;; as an image carrying one leaked thread when it is carrying four.
    (with-clean-leak-record
      (let ((drivers (loop for i below 4
                           collect (bt:make-thread
                                    (lambda () (leak-one-thread))
                                    :name (format nil "leak-driver-~D" i)))))
        (mapc (lambda (th) (bt:join-thread th)) drivers)
        (ok (= 4 (length (leaked-threads)))
            "every thread that could not be stopped is on the record")))))

(deftest worker-retires-rather-than-serve-a-request-while-carrying-one
  ;; Driven through %DISPATCH-REQUEST, the function a real request goes
  ;; through, rather than the helper it calls: a test that invokes the helper
  ;; directly still passes when the call is deleted from the dispatch path, or
  ;; moved after the handler, which is exactly the wiring under test.
  (labels ((server-with-probe (fired)
             ;; The raw constructor: MAKE-WORKER-SERVER would bind a real
             ;; listening socket, which this has no use for.
             (let ((server (cl-mcp/src/worker/server::%make-worker-server)))
               (setf (cl-mcp/src/worker/server::worker-server-authenticated-p
                      server)
                     t)
               (cl-mcp/src/worker/server:register-method
                server "worker/probe"
                (lambda (params) (declare (ignore params))
                  (setf (car fired) t)
                  "served"))
               server))
           (dispatch (server)
             (let* ((retired nil)
                    (cl-mcp/src/worker/server::*retire-action*
                      (lambda (leaked)
                        (setf retired (length leaked))
                        ;; Stands in for the exit: the request must not be
                        ;; served, so leave the dispatch the way exiting does.
                        (throw :retired nil))))
               (catch :retired
                 (cl-mcp/src/worker/server::%dispatch-request
                  server 1 "worker/probe" (make-hash-table :test 'equal)))
               retired)))
    (testing "a request arriving while a thread is still running is not served"
      ;; Retiring means exiting: the parent's crash handling already replaces a
      ;; worker that stops answering and tells the caller its state was reset,
      ;; whereas answering with an error would leave this image in the pool to
      ;; fail the same way on every later request.
      (with-clean-leak-record
        (leak-one-thread)
        (let* ((fired (list nil))
               (retired (dispatch (server-with-probe fired))))
          (ok (eql 1 retired) "the worker retires, and is told what for")
          (ok (null (car fired))
              "and the handler never ran on the compromised image"))))
    (testing "a worker carrying nothing serves the request"
      (with-clean-leak-record
        (let* ((fired (list nil))
               (retired (dispatch (server-with-probe fired))))
          (ok (null retired) "no retirement when there is nothing to retire for")
          (ok (car fired) "and the request is served normally"))))))

(deftest leaked-threads-are-reported-to-the-parent
  ;; Driven through WORKER-RPC against a canned response, so the whole chain
  ;; is covered: the worker putting the count on the envelope, the reader
  ;; returning it, and the parent storing it where pool-status reads it.
  ;; Asserting only on %MAKE-RESULT would pass with any link of that broken.
  (labels ((canned-worker (line)
             (cl-mcp/src/worker-client::make-worker
              :state :bound
              :stream (make-two-way-stream
                       (make-string-input-stream line)
                       (make-broadcast-stream))))
           (count-on (worker)
             (cl-mcp/src/worker-client::worker-leaked-threads worker))
           (envelope (&key error leaked)
             (with-output-to-string (s)
               (yason:encode
                (let ((ht (make-hash-table :test 'equal)))
                  (setf (gethash "jsonrpc" ht) "2.0"
                        (gethash "id" ht) 1)
                  (if error
                      (setf (gethash "error" ht)
                            (let ((e (make-hash-table :test 'equal)))
                              (setf (gethash "code" e) -32603
                                    (gethash "message" e) "boom")
                              e))
                      (setf (gethash "result" ht) "ok"))
                  (when leaked
                    (setf (gethash "leaked_threads" ht) leaked))
                  ht)
                s)
               (terpri s))))
    (testing "a successful response carries the count to the parent"
      (let ((worker (canned-worker (envelope :leaked 2))))
        (cl-mcp/src/worker-client:worker-rpc worker "worker/probe" nil)
        (ok (eql 2 (count-on worker)))))
    (testing "and an error response carries it too"
      ;; A handler can leak its deadline's thread and then return an error.
      ;; Updating only on success leaves the parent reporting what it last
      ;; saw, stale in both directions.
      (let ((worker (canned-worker (envelope :error t :leaked 3))))
        ;; The error has to reach the caller, not merely be observed on the
        ;; way past: recording the count means catching the condition, and a
        ;; catch that forgets to re-signal turns every worker-side error into
        ;; a successful NIL result -- "symbol not found" would read as a
        ;; symbol that was found and evaluated to nothing.
        (ok (handler-case
                (progn (cl-mcp/src/worker-client:worker-rpc
                        worker "worker/probe" nil)
                       nil)
              (cl-mcp/src/worker-client:worker-rpc-error () t))
            "the worker's error is re-signalled to the caller")
        (ok (eql 3 (count-on worker)))))
    (testing "and with a timeout set, which is what production always sends"
      ;; The proxy computes a timeout for every call, so the no-timeout path
      ;; above is the one production never takes.  With one, the response is
      ;; read inside SB-EXT:WITH-TIMEOUT and the count comes back as a second
      ;; value through it.
      (let ((worker (canned-worker (envelope :leaked 4))))
        (cl-mcp/src/worker-client:worker-rpc worker "worker/probe" nil
                                             :timeout 5)
        (ok (eql 4 (count-on worker)))))
    (testing "a response without the field clears a count that has passed"
      (let ((worker (canned-worker (envelope))))
        (setf (cl-mcp/src/worker-client::worker-leaked-threads worker) 5)
        (cl-mcp/src/worker-client:worker-rpc worker "worker/probe" nil)
        (ok (eql 0 (count-on worker))
            "so pool-status stops reporting a worker that recovered")))
    (testing "the worker puts the count on both kinds of envelope"
      ;; The cases above hand WORKER-RPC an envelope built by the test, so
      ;; they cover the reader and the parent's slot but not the worker
      ;; actually writing the field -- and a handler that leaks and then
      ;; returns an error uses the error envelope.
      (with-clean-leak-record
        (leak-one-thread)
        (ok (eql 1 (gethash "leaked_threads"
                            (cl-mcp/src/worker/server::%make-result 1 "ok")))
            "on a result")
        (ok (eql 1 (gethash "leaked_threads"
                            (cl-mcp/src/worker/server::%make-error
                             1 -32603 "boom")))
            "and on an error")))
    (testing "the worker omits the field entirely when carrying nothing"
      (with-clean-leak-record
        (dolist (ht (list (cl-mcp/src/worker/server::%make-result 1 "payload")
                          (cl-mcp/src/worker/server::%make-error 1 -1 "e")))
          (ok (null (nth-value 1 (gethash "leaked_threads" ht)))
              "the key is absent rather than reported as zero"))))))

(deftest a-retirement-is-classified-apart-from-a-crash
  ;; The chain nothing covered, and the one with the widest blast radius: a
  ;; worker that retires reaches the parent as EOF like any other death, and
  ;; %MONITOR-INIT reads a crash by the runtime-init owner as init's fault and
  ;; disables initialization for every later worker in the pool.  The first
  ;; version of this guard was inert -- the reader it called was not imported
  ;; and IGNORE-ERRORS swallowed the undefined-function error -- and shipped
  ;; because no test asked the question.
  (labels ((dead-worker (&key leaked exit-code)
             ;; A real process when the exit code matters: the classifier
             ;; asks SB-EXT for it, and with no process to ask there is
             ;; nothing to go on and the answer is "crash".
             (cl-mcp/src/worker-client::make-worker
              :state :bound
              :leaked-threads (or leaked 0)
              :process-info
              (when exit-code
                (sb-ext:run-program "/bin/sh"
                                    (list "-c" (format nil "exit ~D" exit-code))
                                    :wait nil :search nil))
              :stream (make-two-way-stream (make-string-input-stream "")
                                           (make-broadcast-stream))))
           (reason-of (worker)
             (handler-case
                 (progn (cl-mcp/src/worker-client:worker-rpc
                         worker "worker/probe" nil)
                        nil)
               (cl-mcp/src/worker-client:worker-crashed (c)
                 (cl-mcp/src/worker-client:worker-crashed-reason c)))))
    (testing "a worker that died carrying one is reported as retired"
      (let ((reason (reason-of (dead-worker :leaked 1 :exit-code 70))))
        (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                   reason)
            (format nil "reason was ~S" reason))))
    (testing "a worker that died carrying nothing is reported as a crash"
      (ok (equal "eof" (reason-of (dead-worker :exit-code 70))))
      (ok (equal "eof" (reason-of (dead-worker :leaked 1)))
          "and so is one with nothing to read an exit code from"))
    (testing "and the pool tells the two apart"
      ;; The predicate the init monitor consults.  Asserted through a real
      ;; WORKER-CRASHED condition, because what broke before was the reader
      ;; used to get the reason out of one.
      (let ((retired (make-condition
                      'cl-mcp/src/worker-client:worker-crashed
                      :worker nil
                      :reason cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*))
            (crashed (make-condition
                      'cl-mcp/src/worker-client:worker-crashed
                      :worker nil :reason "eof")))
        (ok (cl-mcp/src/pool::%retirement-crash-p retired)
            "a retirement is recognized, so init is not blamed for it")
        (ok (not (cl-mcp/src/pool::%retirement-crash-p crashed))
            "and a real crash still counts against init")))
    (testing "a retirement is kept out of the crash breaker too"
      ;; Three uninterruptible timeouts in five minutes would otherwise trip
      ;; the per-session breaker and halt the session.
      ;; Classified through the function that does the recording, not by
      ;; setting the slot the pool reads: what a classified death leaves
      ;; behind is the thing the pool depends on.
      (let ((retired (dead-worker))
            (crashed (dead-worker)))
        (cl-mcp/src/worker-client::%mark-worker-crashed
         retired cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)
        (cl-mcp/src/worker-client::%mark-worker-crashed crashed "eof")
        (ok (cl-mcp/src/worker-client::worker-retired-p retired))
        (ok (not (cl-mcp/src/worker-client::worker-retired-p crashed)))
        ;; The decision both push sites actually make.  Asserting only
        ;; WORKER-RETIRED-P leaves the exclusion deletable from either site
        ;; without a test noticing.
        (ok (not (cl-mcp/src/pool::%breaker-countable-crash-p retired))
            "a retirement is not counted against the session")
        (ok (cl-mcp/src/pool::%breaker-countable-crash-p crashed)
            "a real crash still is"))
      ;; The health monitor can reach a dead worker before any RPC has seen
      ;; the EOF, and then nothing has recorded anything.  The exit code is
      ;; the only witness left on that path -- and on its own it is one a
      ;; REPL can produce, so it is not taken on its own.  Which of the two
      ;; questions the pool asks, and when, is covered against a real pool in
      ;; A-RETIREMENT-IS-NOT-COUNTED-AGAINST-THE-SESSION-BREAKER.
      (let ((unclassified (dead-worker :leaked 1))
            (forged (dead-worker :leaked 0)))
        (setf (cl-mcp/src/worker-client:worker-last-exit-code unclassified) 70
              (cl-mcp/src/worker-client:worker-last-exit-code forged) 70)
        (ok (cl-mcp/src/worker-client::exit-code-says-retired-p unclassified)
            "an unclassified death can still be read from the code it left")
        (ok (not (cl-mcp/src/worker-client::worker-retired-p unclassified))
            "though nothing has recorded that answer yet")
        (ok (not (cl-mcp/src/worker-client::exit-code-says-retired-p forged))
            "and a worker that never reported a leak did not retire for one")))))

(deftest the-classification-is-published-for-whoever-asks-next
  ;; One caller works out what killed a worker; everyone else reads what it
  ;; recorded.  Those others are not stragglers: the init monitor polls the
  ;; same worker every fraction of a second while a load runs, and what it
  ;; reads decides whether initialization is disabled for every later worker
  ;; in the pool.
  (labels ((dead-worker (&key leaked)
             (cl-mcp/src/worker-client::make-worker
              :state :bound
              :leaked-threads (or leaked 0)
              :stream (make-two-way-stream (make-string-input-stream "")
                                           (make-broadcast-stream))))
           (reason-of (worker)
             (handler-case
                 (progn (cl-mcp/src/worker-client:worker-rpc
                         worker "worker/probe" nil)
                        nil)
               (cl-mcp/src/worker-client:worker-crashed (c)
                 (cl-mcp/src/worker-client:worker-crashed-reason c)))))
    (testing "the reason is recorded even when there is no process to ask"
      ;; It used to be recorded beside the exit code, inside the branch that
      ;; needs a process object, so a worker without one recorded nothing at
      ;; all -- and every reader after the first saw an unexplained crash.
      (let ((worker (dead-worker)))
        (cl-mcp/src/worker-client::%mark-worker-crashed worker "eof")
        (ok (equal "eof" (cl-mcp/src/worker-client:worker-last-crash-reason
                          worker)))))
    (testing "a caller that arrives after the stream is closed reads it"
      (let ((worker (dead-worker :leaked 1)))
        (cl-mcp/src/worker-client::%mark-worker-crashed
         worker cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)
        (setf (cl-mcp/src/worker-client::worker-stream worker) nil)
        (let ((reason (reason-of worker)))
          (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                     reason)
              "it is told what killed the worker, not that it was already
dead -- which the init monitor would read as a crash and disable
initialization pool-wide for")
          (ok (cl-mcp/src/pool::%retirement-crash-p
               (make-condition 'cl-mcp/src/worker-client:worker-crashed
                               :worker worker :reason reason))
              "and the exclusion that reads it recognizes what it gets"))))
    (testing "and still reads it after the worker has been killed and reaped"
      ;; The pool kills a crashed worker as part of replacing it, which
      ;; leaves it :DEAD.  The callers that arrive after that are the ones
      ;; that most need the answer -- the init monitor is still polling the
      ;; worker it was watching, and reads a crash there as init's fault for
      ;; every later worker in the pool.
      (let ((worker (dead-worker :leaked 1)))
        (cl-mcp/src/worker-client::%mark-worker-crashed
         worker cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)
        (setf (cl-mcp/src/worker-client::worker-stream worker) nil
              (cl-mcp/src/worker-client:worker-state worker) :dead)
        (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                   (reason-of worker))
            "a retirement is still reported as one once the worker is :dead")))
    (testing "but a worker that did not crash reports no reason of its own"
      ;; A replacement carries the details of the death that produced it,
      ;; and one killed deliberately -- pool-kill-worker, a cancellation,
      ;; shutdown -- has not crashed at all.  Reporting the carried string
      ;; would describe someone else's death as this worker's, and for
      ;; "timeout" the user is then told an operation they never ran took
      ;; too long.
      (let ((worker (dead-worker)))
        (setf (cl-mcp/src/worker-client::worker-stream worker) nil
              (cl-mcp/src/worker-client:worker-last-crash-reason worker)
              "timeout")
        (ok (equal "already-dead" (reason-of worker)))))))

(deftest marking-a-crash-does-not-wait-on-a-worker-that-is-still-running
  ;; The stderr drain thread ends when the worker's pipe closes, so waiting
  ;; for it is only meaningful once the process is gone -- and every caller
  ;; here holds WORKER-STREAM-LOCK, which KILL-WORKER and a cancellation have
  ;; to take.  "timeout" and "stream-error" abandon a worker that is still
  ;; running: waiting there would block a cancellation for the full second on
  ;; the very path a user reaches by asking to cancel something slow.
  (testing "a still-running worker's drain thread is not waited on"
    (let* ((process (sb-ext:run-program "/bin/sh" (list "-c" "sleep 30")
                                        :wait nil :search nil))
           (drain (bt:make-thread (lambda () (sleep 30)) :name "probe-drain"))
           (worker (cl-mcp/src/worker-client::make-worker
                    :state :bound
                    :process-info process
                    :stderr-thread drain))
           (start (get-internal-real-time)))
      (unwind-protect
           (progn
             (cl-mcp/src/worker-client::%mark-worker-crashed worker "timeout")
             (ok (< (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)
                    0.5)
                 "it returns without serving out the join timeout")
             ;; Not waiting is not the same as not cleaning up: the thread
             ;; still has to be stopped and the slot cleared, or the worker
             ;; leaves a thread of its own behind on every timeout.
             ;; Polled: DESTROY-THREAD asks the thread to die rather than
             ;; killing it where it stands, so reading the flag immediately
             ;; is a race with the scheduler, not a test of anything.
             (ok (loop repeat 100
                       thereis (not (bt:thread-alive-p drain))
                       do (sleep 0.02))
                 "the drain thread is stopped rather than left running")
             (ok (null (cl-mcp/src/worker-client::worker-stderr-thread worker))
                 "and the slot no longer points at it"))
        (ignore-errors (bt:destroy-thread drain))
        (ignore-errors (sb-ext:process-kill process 9))
        (ignore-errors (sb-ext:process-wait process))))))

(deftest killing-a-worker-lets-its-last-words-through
  ;; KILL-WORKER closes the worker's pipe by killing it, so the drain thread
  ;; ends on its own with whatever the worker said last -- after a SIGKILL,
  ;; the part that says why.  The wait for it was written as
  ;; (BT:JOIN-THREAD th :timeout 1), which that function does not accept, so
  ;; IGNORE-ERRORS swallowed a program-error and the thread was destroyed
  ;; mid-line instead.
  (testing "the drain thread is joined rather than destroyed mid-line"
    ;; The process is already gone, so KILL-WORKER goes straight to the join
    ;; rather than spending an unpredictable amount of time on SIGTERM: the
    ;; wait is what is under test, and it has to be the thing the drain
    ;; thread outlives.
    (let* ((finished nil)
           ;; The process first, and the thread after it: spawning is the
           ;; slow part here, and a drain thread that finishes during it
           ;; would look joined whether or not anything waited.
           (process (sb-ext:run-program "/bin/sh" (list "-c" "exit 0")
                                        :wait t :search nil))
           (drain (bt:make-thread (lambda () (sleep 0.5) (setf finished t))
                                  :name "probe-drain"))
           (worker (cl-mcp/src/worker-client::make-worker
                    :state :bound
                    :process-info process
                    :stderr-thread drain)))
      (unwind-protect
           (progn
             (cl-mcp/src/worker-client:kill-worker worker)
             (ok finished "it ran to the end instead of being cut off")
             (ok (null (cl-mcp/src/worker-client::worker-stderr-thread worker))
                 "and the slot no longer points at it")
             ;; A kill resets the session's state as surely as a crash does,
             ;; and the flag is how the replacement knows to say so.
             (ok (cl-mcp/src/worker-client:worker-needs-reset-notification
                  worker)
                 "the reset it owes the user is recorded"))
        (ignore-errors (bt:destroy-thread drain))))))

(deftest pool-status-shows-a-worker-that-is-carrying-one
  ;; Between a deadline giving up on a thread and the next request arriving,
  ;; the worker is still serving and nothing else says so.  pool-status is
  ;; where someone asking what just went wrong looks, which is the whole
  ;; reason the count is reported on every response rather than only when
  ;; the worker finally retires.
  (testing "the count reaches the tool's data and its summary"
    (let* ((worker (cl-mcp/src/worker-client::make-worker
                    :id 99 :state :bound :leaked-threads 2))
           (cl-mcp/src/pool::*pool-running* t)
           (cl-mcp/src/pool::*all-workers* (list worker))
           (cl-mcp/src/pool::*standby-workers* ())
           (info (cl-mcp/src/pool::pool-worker-info)))
      (ok (eql 2 (gethash "leaked_threads" (aref info 0)))
          "the per-worker data carries it")
      (let* ((response (funcall (get-tool-handler "pool-status")
                                (make-state) 1 nil))
             (text (gethash "text"
                            (aref (gethash "content"
                                           (gethash "result" response))
                                  0))))
        (ok (search "leaked_threads=2" text)
            "and the summary the user actually reads says so")))))

(deftest a-real-exit-code-decides-over-the-reported-count
  ;; The count is a proxy taken from the worker's *previous* answer.  A worker
  ;; can report a leak, have the thread finish, and then genuinely crash --
  ;; and reading the count alone files that as a retirement, which suppresses
  ;; both the init-failure attribution and the circuit breaker for a real
  ;; crash.  The exit code describes the death itself, so it decides, in both
  ;; directions.
  (labels ((exited-with (code)
             ;; A real process, because the classification asks SB-EXT for its
             ;; status and code -- and at the moment the parent sees EOF that
             ;; status has not settled yet, which is what made the first
             ;; version of this fall back to the count every time.
             (sb-ext:run-program "/bin/sh" (list "-c" (format nil "exit ~D" code))
                                 :wait nil :search nil))
           (classify-signaled (&key leaked)
             (let ((process (sb-ext:run-program "/bin/sh" (list "-c" "sleep 30")
                                                :wait nil :search nil)))
               (sb-ext:process-kill process 9)
               (cl-mcp/src/worker-client::%retired-for-leaked-thread-p
                (cl-mcp/src/worker-client::make-worker
                 :state :bound
                 :leaked-threads leaked
                 :process-info process))))
           (classify (&key code leaked)
             (cl-mcp/src/worker-client::%retired-for-leaked-thread-p
              (cl-mcp/src/worker-client::make-worker
               :state :bound
               :leaked-threads leaked
               :process-info (exited-with code)))))
    (testing "the exit code and the count have to agree"
      (ok (classify :code 70 :leaked 1)
          "a worker that said it was carrying one, and then exited saying so")
      (ok (not (classify :code 70 :leaked 0))
          "an exit code on its own is not enough: worker/eval runs whatever
the user asks, so (sb-ext:exit :code 70) in a healthy worker is one line of
REPL away -- and it would otherwise be reported to that user as an earlier
timeout leaving a thread behind, and excused from the circuit breaker"))
    (testing "any other exit code says crash, whatever the count"
      (ok (not (classify :code 1 :leaked 1))
          "a genuine crash is not hidden by a count left over from earlier"))
    (testing "a worker killed by a signal did not retire"
      ;; A retiring worker exits under its own power.  Left to the count, a
      ;; SIGKILL -- the OOM killer, an operator, a segfault -- on a worker
      ;; that had reported a leak is filed as a deliberate retirement:
      ;; excused from the circuit breaker, and explained to the user as a
      ;; timeout of their own from earlier.
      (ok (not (classify-signaled :leaked 1))))
    (testing "with no exit code to read at all the answer is crash"
      ;; The count is the witness that can be stale: a worker that reported a
      ;; leak, had the thread finish, and then genuinely crashed reads as a
      ;; retirement on the count alone -- excusing a real crash from the
      ;; breaker and from init attribution.  A retirement misfiled the other
      ;; way costs one breaker tick, which is what it cost before any of this
      ;; existed.
      (ok (not (cl-mcp/src/worker-client::%retired-for-leaked-thread-p
                (cl-mcp/src/worker-client::make-worker :state :bound
                                                       :leaked-threads 1)))
          "a reported leak is not on its own a retirement"))))

(deftest a-real-worker-retires-and-the-parent-reads-it-as-a-retirement
  ;; End to end against a real worker process.  Three joints are covered
  ;; nowhere else: the production *RETIRE-ACTION* -- every other test replaces
  ;; it, so what it actually does, exit code included, is never run -- the
  ;; count travelling on a real response, and the parent classifying a real
  ;; exit through the wait for SBCL's status to settle.
  (testing "it leaks, says so, retires on the next request, and is read as one"
    (unless (spawn-available-p)
      (skip "worker processes cannot be spawned here"))
    (let ((worker (cl-mcp/src/worker-client:spawn-worker)))
      (unwind-protect
           (progn
             (handler-case
                 (cl-mcp/src/worker-client:worker-rpc
                  worker "worker/eval" (leaking-params))
               (cl-mcp/src/worker-client:worker-rpc-error () nil))
             (ok (eql 1 (cl-mcp/src/worker-client::worker-leaked-threads
                         worker))
                 "the worker tells the parent it is carrying one")
             (let ((reason
                     (handler-case
                         (progn (cl-mcp/src/worker-client:worker-rpc
                                 worker "worker/eval" (eval-params "(+ 1 2)"))
                                nil)
                       (cl-mcp/src/worker-client:worker-crashed (c)
                         (cl-mcp/src/worker-client:worker-crashed-reason c)))))
               (ok (equal
                    cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                    reason)
                   (format nil "the next request retires it (reason ~S)"
                           reason)))
             (ok (eql cl-mcp/src/utils/deadline::+leaked-thread-exit-code+
                      (cl-mcp/src/worker-client:worker-last-exit-code worker))
                 "by exiting with the code the parent classifies on, and the
parent waited long enough for the status to settle to read it"))
        (cl-mcp/src/worker-client:kill-worker worker)))))

(deftest a-recorded-retirement-survives-a-second-look
  ;; Two things classify a death, and the second one sees less.  The RPC that
  ;; met the EOF reads the exit code while the process is still there; by the
  ;; time the pool runs over the same worker the reaper may have closed it,
  ;; leaving the code unreadable -- and killing what it replaces makes the
  ;; process look signalled rather than exited.  Neither may turn a recorded
  ;; retirement back into a crash: that would put it in front of the circuit
  ;; breaker and blame initialization for it.
  (testing "the pool's own look does not overwrite what the RPC established"
    (let ((worker (cl-mcp/src/worker-client::make-worker
                   :state :bound :leaked-threads 1)))
      (cl-mcp/src/worker-client::%mark-worker-crashed
       worker cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)
      (ok (cl-mcp/src/pool::%record-worker-death worker "signaled" 9)
          "it is still a retirement")
      (ok (cl-mcp/src/worker-client::worker-retired-p worker)
          "and stays recorded as one")
      (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                 (cl-mcp/src/worker-client:worker-last-crash-reason worker))
          "with the reason the user is shown intact")))
  (testing "but a death it has no record of is read from what it can see"
    (let ((worker (cl-mcp/src/worker-client::make-worker
                   :state :bound :leaked-threads 1)))
      (ok (not (cl-mcp/src/pool::%record-worker-death worker "signaled" 9))
          "a signalled death is not a retirement")
      (ok (equal "process-died"
                 (cl-mcp/src/worker-client:worker-last-crash-reason worker))))))

(deftest a-retirement-is-not-counted-against-the-session-breaker
  ;; Against a real pool, because what is under test is the two push sites
  ;; rather than the predicate they call: asking the predicate directly still
  ;; passes with the call deleted from either site, and a session whose
  ;; breaker trips is halted -- the failure this exclusion exists to prevent.
  ;;
  ;; The threshold is set to one so a single death decides, and the
  ;; classification is recorded by hand: what the death was is settled
  ;; elsewhere, and killing a real worker in a way that produces exit 70
  ;; would be testing the worker again rather than the pool.
  (testing "neither push site counts a worker that retired"
    (unless (spawn-available-p)
      (skip "worker processes cannot be spawned here"))
    (let ((cl-mcp/src/pool::*crash-breaker-threshold* 1))
      (with-pool ()
        (flet ((kill-and-record-retirement (worker)
                 (sb-posix:kill (cl-mcp/src/worker-client:worker-pid worker)
                                sb-posix:sigkill)
                 (sleep 0.5)
                 ;; Recorded the way the RPC that sees the EOF records it,
                 ;; rather than by setting the slots the pool reads: what a
                 ;; classified death leaves behind is part of what is under
                 ;; test here.
                 (cl-mcp/src/worker-client::%mark-worker-crashed
                  worker
                  cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)))
          ;; The health monitor's path: it reaches the dead worker itself and
          ;; hands it to %HANDLE-WORKER-CRASH.  Retired for real here, and the
          ;; request that retires it is written straight to the socket so
          ;; nothing reads the EOF -- which is the state the health monitor
          ;; finds such a worker in, with no reason recorded by anyone and the
          ;; exit code the only thing left to go on.
          (let* ((session "retire-breaker-health-monitor")
                 (worker (cl-mcp/src/pool:get-or-assign-worker session)))
            (cl-mcp/src/worker-client:worker-rpc
             worker "worker/eval" (leaking-params))
            (cl-mcp/src/worker-client::%send-json-rpc
             (cl-mcp/src/worker-client::worker-stream worker)
             99 "worker/eval" (eval-params "(+ 1 2)"))
            (loop repeat 200
                  while (ignore-errors
                         (sb-ext:process-alive-p
                          (cl-mcp/src/worker-client:worker-process-info worker)))
                  do (sleep 0.05))
            (ok (null (cl-mcp/src/worker-client:worker-last-crash-reason
                       worker))
                "nothing has classified the death yet")
            (cl-mcp/src/pool::%handle-worker-crash worker)
            (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                       (cl-mcp/src/worker-client:worker-last-crash-reason
                        worker))
                "the pool classifies it from the exit code it left, and keeps
that answer rather than flattening it to a generic crash")
            (ok (cl-mcp/src/worker-client::worker-retired-p worker)
                "and records it, so a later reader gets the same answer
without re-deriving it from a string that will be copied elsewhere")
            (let ((replacement
                    (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                      (gethash session cl-mcp/src/pool::*affinity-map*))))
              (ok replacement
                  "the session is recovered rather than halted on its first
retirement, against a threshold of one")
              ;; The replacement is handed its predecessor's crash details so
              ;; the user can be told why the session was reset.  It has not
              ;; itself retired, and answering otherwise -- by reading those
              ;; inherited details as its own classification -- would leave
              ;; this session's breaker off for as long as the session lived,
              ;; since every later replacement inherits them again.  A crash
              ;; loop is the thing the breaker exists for.
              (ok (equal
                   cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                   (cl-mcp/src/worker-client:worker-last-crash-reason
                    replacement))
                  "it carries the reason forward, which is how the user's
next call learns why the session was reset")
              (ok (not (cl-mcp/src/worker-client::worker-retired-p replacement))
                  "the fresh worker has not retired")
              (sb-posix:kill (cl-mcp/src/worker-client:worker-pid replacement)
                             sb-posix:sigkill)
              (sleep 0.5)
              (cl-mcp/src/pool::%handle-worker-crash replacement)
              (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
                (ok (null (gethash session cl-mcp/src/pool::*affinity-map*))
                    "and its own crash is counted, tripping the breaker"))))
          ;; The other path: the next request finds the crashed worker still
          ;; in the affinity map and pushes there instead.
          (let* ((session "retire-breaker-next-request")
                 (worker (cl-mcp/src/pool:get-or-assign-worker session)))
            (kill-and-record-retirement worker)
            (let ((replacement
                    (handler-case (cl-mcp/src/pool:get-or-assign-worker session)
                      (error () nil))))
              (ok replacement "the session is still served after a retirement")
              (ok (not (eq worker replacement))
                  "and served by a new worker"))))))))

(deftest a-retirement-is-not-an-init-failure
  ;; The pool runs one worker's initialization as a singleton, and treats a
  ;; crash by that worker as initialization's fault: it disables init for
  ;; every later worker in the pool until an operator re-arms it.  A worker
  ;; that retired did not fail to initialize.
  ;;
  ;; This exclusion has the widest blast radius on this branch and had no
  ;; test of its wiring: deleting it from both handlers left the suite green.
  ;; Driven through the two functions that hold those handlers rather than
  ;; through %RETIREMENT-CRASH-P, which is what passed while they were inert.
  (labels ((dead-worker (reason)
             (let ((worker (cl-mcp/src/worker-client::make-worker
                            :state :crashed)))
               (setf (cl-mcp/src/worker-client:worker-last-crash-reason worker)
                     reason)
               worker))
           (init-disabled-after-monitor (reason)
             (let* ((worker (dead-worker reason))
                    (cl-mcp/src/pool::*runtime-owner* (cons "init-probe" worker))
                    (cl-mcp/src/pool::*runtime-init-disabled* nil)
                    (cl-mcp/src/pool::*init-attributable-crashes*
                      (make-hash-table :test 'eql)))
               (cl-mcp/src/pool::%monitor-init worker "init-probe" 1)
               cl-mcp/src/pool::*runtime-init-disabled*))
           (init-disabled-after-start (reason)
             (let ((worker (dead-worker reason))
                   (cl-mcp/src/pool::*runtime-owner* nil)
                    (cl-mcp/src/pool::*runtime-init-failures* 0)
                   (cl-mcp/src/pool::*runtime-init-disabled* nil)
                   (cl-mcp/src/pool::*worker-init-config*
                     (list :system "init-probe-system" :max-failures 1))
                   (cl-mcp/src/pool::*init-attributable-crashes*
                     (make-hash-table :test 'eql)))
               (cl-mcp/src/pool::%ensure-runtime-init worker "init-probe")
               cl-mcp/src/pool::*runtime-init-disabled*)))
    (testing "the init monitor does not blame a retirement"
      (ok (not (init-disabled-after-monitor
                cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*))
          "initialization stays available to every later worker")
      (ok (init-disabled-after-monitor "eof")
          "while a real crash of the init owner still disables it"))
    (testing "nor does the init-start path"
      (ok (not (init-disabled-after-start
                cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*))
          "a worker that retired before answering did not fail to initialize")
      (ok (init-disabled-after-start "eof")
          "while a real crash there still disables it"))))

(deftest a-retirement-on-the-pools-own-rpc-still-reaches-the-user
  ;; The pool sends RPCs of its own: the project-root sync after
  ;; fs-set-project-root, the init monitor's polling.  Their errors are
  ;; swallowed by design -- they are housekeeping, not the user's request --
  ;; so when the worker dies on one, nobody has told the user anything.
  ;;
  ;; Retirement makes that reachable on purpose rather than by chance: the
  ;; root sync is a request like any other, so a worker carrying a leaked
  ;; thread retires on it.  Their next call would otherwise land in a fresh
  ;; image with their systems unloaded and nothing said about it.
  (testing "the reset it owes is handed to the replacement"
    (unless (spawn-available-p)
      (skip "worker processes cannot be spawned here"))
    (with-pool ()
      (let* ((session "retire-on-internal-rpc")
             (worker (cl-mcp/src/pool:get-or-assign-worker session)))
        (cl-mcp/src/worker-client:worker-rpc
         worker "worker/eval" (leaking-params))
        ;; The pool's own RPC, called the way fs-set-project-root calls it.
        (cl-mcp/src/pool::send-root-to-session-worker
         session (namestring (uiop:temporary-directory)))
        (ok (not (eq :bound (cl-mcp/src/worker-client:worker-state worker)))
            "the worker retired on it")
        (let ((replacement (cl-mcp/src/pool:get-or-assign-worker session)))
          (ok (not (eq worker replacement)) "the session gets a new worker")
          (ok (cl-mcp/src/worker-client:worker-needs-reset-notification
               replacement)
              "which owes the user the notification nobody delivered")
          ;; And can say what it was.  The notification is built from the
          ;; crash details the replacement carries, so handing over the debt
          ;; without them turns the one message this branch exists to
          ;; produce back into "your worker crashed".
          (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                     (cl-mcp/src/worker-client:worker-last-crash-reason
                      replacement))
              "explained by what actually happened"))))))

(deftest an-owed-reset-outlives-the-worker-that-owed-it
  ;; The debt belongs to the session, because the paths that throw a dead
  ;; worker away do not all have a replacement in hand to give it to.  Each
  ;; of these lost it, and losing it means the user's next call lands in a
  ;; fresh image -- systems unloaded, definitions gone -- with nothing said.
  (testing "recovery that drops the worker without replacing it keeps it"
    ;; The health monitor reaching a worker another thread has already marked
    ;; crashed: this arm removes it from the pool and only schedules
    ;; replenishment, so there is nothing to hand the debt to.
    (unless (spawn-available-p)
      (skip "worker processes cannot be spawned here"))
    (with-pool ()
      (let* ((session "owed-reset-dropped")
             (worker (cl-mcp/src/pool:get-or-assign-worker session)))
        (cl-mcp/src/worker-client::%mark-worker-crashed
         worker cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)
        (cl-mcp/src/pool::%handle-worker-crash worker)
        (let ((replacement (cl-mcp/src/pool:get-or-assign-worker session)))
          (ok (not (eq worker replacement)))
          (ok (cl-mcp/src/worker-client:worker-needs-reset-notification
               replacement)
              "the reset survives the worker it was owed by")
          (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                     (cl-mcp/src/worker-client:worker-last-crash-reason
                      replacement))
              "with what to say about it")))))
  (testing "a replacement that cannot be spawned does not consume it"
    ;; The debt used to live in one call's local variable, so a spawn that
    ;; failed took it with it and the retry came back silently fresh.
    (unless (spawn-available-p)
      (skip "worker processes cannot be spawned here"))
    (with-pool ()
      (let* ((session "owed-reset-failed-spawn")
             (worker (cl-mcp/src/pool:get-or-assign-worker session)))
        (cl-mcp/src/worker-client::%mark-worker-crashed
         worker cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)
        ;; Two ways to fail, because they fail in different places: no room
        ;; in the pool gives up before the spawn is attempted, while a
        ;; handshake that times out fails inside it, after the debt has been
        ;; taken out of the session's hands and put on a worker that is then
        ;; thrown away.
        (ok (handler-case
                (let ((cl-mcp/src/pool:*max-pool-size* 0))
                  (cl-mcp/src/pool:get-or-assign-worker session)
                  nil)
              (error () t))
            "there is no room to spawn the replacement")
        (ok (handler-case
                (let ((cl-mcp/src/worker-client::*worker-startup-timeout*
                        0.01))
                  (cl-mcp/src/pool:get-or-assign-worker session)
                  nil)
              (error () t))
            "and then the spawn itself fails")
        (let ((replacement (cl-mcp/src/pool:get-or-assign-worker session)))
          (ok (cl-mcp/src/worker-client:worker-needs-reset-notification
               replacement)
              "the retry still owes the user the reset")
          (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                     (cl-mcp/src/worker-client:worker-last-crash-reason
                      replacement))
              "and still knows why"))))))

(deftest retirement-is-visible-where-it-has-to-be
  (testing "real work retires; only observation is exempt"
    ;; The exemption exists so an init-status poll -- sent every fraction of a
    ;; second while a load runs -- cannot throw away that load on a request
    ;; that asks for nothing.  Putting a working method on that list would
    ;; produce a worker that carries a leaked thread forever, which is the
    ;; worst regression this feature can suffer and the cheapest to make.
    (dolist (method '("worker/eval" "worker/run-tests" "worker/load-system"
                      "worker/set-project-root"))
      (ok (not (member method
                       cl-mcp/src/worker/server::*methods-exempt-from-retirement*
                       :test #'string=))
          (format nil "~A retires" method)))
    (dolist (method '("worker/init-status"))
      (ok (member method
                  cl-mcp/src/worker/server::*methods-exempt-from-retirement*
                  :test #'string=)
          (format nil "~A does not" method)))
    ;; And the list is exactly those two.  Naming four methods that are not
    ;; on it leaves every method not named free to be added to it --
    ;; worker/code-find, worker/macroexpand and worker/inspect-object among
    ;; them -- and each addition is a way for a worker to carry a leaked
    ;; thread indefinitely while answering requests with it.
    (ok (null (set-exclusive-or
               '("worker/ping" "worker/init-status")
               cl-mcp/src/worker/server::*methods-exempt-from-retirement*
               :test #'equal))
        "the exemption list is exactly the methods that ask for nothing"))
  (testing "an exempt method is served rather than retiring the worker"
    ;; Both of them: naming the list is not the same as the dispatcher
    ;; honouring it, and the entry that is never exercised is the one that
    ;; quietly stops working.
    (dolist (method '("worker/init-status" "worker/ping"))
      (with-clean-leak-record
        (leak-one-thread)
        (let* ((served nil)
               (retired nil)
               (server (cl-mcp/src/worker/server::%make-worker-server))
               (cl-mcp/src/worker/server::*retire-action*
                 (lambda (leaked) (declare (ignore leaked)) (setf retired t))))
          (setf (cl-mcp/src/worker/server::worker-server-authenticated-p server)
                t)
          (cl-mcp/src/worker/server:register-method
           server method
           (lambda (params) (declare (ignore params)) (setf served t) "ok"))
          (cl-mcp/src/worker/server::%dispatch-request
           server 1 method (make-hash-table :test 'equal))
          (ok (null retired)
              (format nil "~A does not retire the worker" method))
          (ok served (format nil "and ~A is answered" method))))))
  (testing "a non-string method cannot take the worker down"
    ;; The exemption check runs on every authenticated request; reaching
    ;; STRING= with a non-string would unwind to the worker's toplevel and
    ;; kill the session on one malformed line.
    (with-clean-leak-record
      (let ((server (cl-mcp/src/worker/server::%make-worker-server)))
        (setf (cl-mcp/src/worker/server::worker-server-authenticated-p server) t)
        (ok (search "Method not found"
                    (cl-mcp/src/worker/server::%dispatch-request
                     server 1 5 (make-hash-table :test 'equal)))
            "it is answered, not fatal"))))
  (testing "an unauthenticated peer is told nothing about the worker's state"
    ;; The retirement gate sits after authentication on purpose; reporting the
    ;; count before it gives away exactly what that placement withholds.
    (with-clean-leak-record
      (leak-one-thread)
      ;; Driven through %DISPATCH-REQUEST with the gate actually armed, not by
      ;; calling the builder with NIL by hand: the latter passes even when the
      ;; call site stops passing it.
      (let ((had (sb-posix:getenv "MCP_WORKER_SECRET")))
        (unwind-protect
             (let ((server (cl-mcp/src/worker/server::%make-worker-server)))
               (sb-posix:setenv "MCP_WORKER_SECRET" "probe-secret" 1)
               (flet ((withholds-p (label json-string)
                        (let ((json (yason:parse json-string)))
                          (ok (gethash "error" json)
                              (format nil "~A is refused" label))
                          (ok (null (nth-value 1
                                               (gethash "leaked_threads" json)))
                              (format nil "~A says nothing about the worker"
                                      label)))))
                 ;; Every way a peer can be answered before it has
                 ;; authenticated, not just the one.  Singling out normal
                 ;; dispatch protects nothing: a peer that wants the count
                 ;; can send a malformed line just as easily as a method
                 ;; name, and each of these is a separate call site that has
                 ;; to pass the authentication state through.
                 (withholds-p
                  "an unauthenticated request"
                  (cl-mcp/src/worker/server::%dispatch-request
                   server 1 "worker/eval" (make-hash-table :test 'equal)))
                 (withholds-p
                  "a parse error"
                  (cl-mcp/src/worker/server::%process-line server "{"))
                 (withholds-p
                  "a request that is not an object"
                  (cl-mcp/src/worker/server::%process-line server "[]"))
                 (withholds-p
                  "a request with no method"
                  (cl-mcp/src/worker/server::%process-line
                   server "{\"id\": 1}"))
                 (withholds-p
                  "a failed authentication"
                  (cl-mcp/src/worker/server::%dispatch-request
                   server 1 "worker/authenticate"
                   (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash "secret" ht) "wrong")
                     ht)))))
          (if had
              (sb-posix:setenv "MCP_WORKER_SECRET" had 1)
              (sb-posix:unsetenv "MCP_WORKER_SECRET"))))))
  (testing "the user is told a worker was replaced, not that it crashed"
    ;; Driven through PROXY-TO-WORKER, the function a tool call actually
    ;; reaches, rather than the builder it calls: the builder can be right
    ;; about every reason while the proxy hands it the wrong one, and then
    ;; what every user sees is the generic crash text.
    (flet ((text-for (reason)
             (let* ((worker (cl-mcp/src/worker-client::make-worker
                             :state :bound))
                    (cl-mcp/src/proxy::*current-session-id* "leak-probe-session")
                    (cl-mcp/src/proxy::%cached-get-or-assign%
                      (lambda (session) (declare (ignore session)) worker))
                    (cl-mcp/src/proxy::%cached-check-and-clear%
                      (lambda (w) (declare (ignore w)) nil))
                    (cl-mcp/src/proxy::%cached-worker-rpc%
                      (lambda (w method params &key timeout)
                        (declare (ignore method params timeout))
                        (error 'cl-mcp/src/worker-client:worker-crashed
                               :worker w :reason reason)))
                    (cl-mcp/src/proxy::%cached-worker-crashed-sym%
                      'cl-mcp/src/worker-client:worker-crashed)
                    (cl-mcp/src/proxy::%cached-worker-crashed-reason%
                      #'cl-mcp/src/worker-client:worker-crashed-reason)
                    (cl-mcp/src/proxy::%cached-worker-last-crash-reason%
                      #'cl-mcp/src/worker-client:worker-last-crash-reason)
                    (cl-mcp/src/proxy::%cached-worker-last-exit-status%
                      #'cl-mcp/src/worker-client:worker-last-exit-status)
                    (cl-mcp/src/proxy::%cached-worker-last-exit-code%
                      #'cl-mcp/src/worker-client:worker-last-exit-code))
               (gethash "text"
                        (aref (gethash "content"
                                       (cl-mcp/src/proxy:proxy-to-worker
                                        "leak-probe-request" "repl-eval" nil))
                              0)))))
      (ok (search "left a thread that could not be stopped"
                  (text-for
                   cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*))
          "a retirement says what actually happened")
      (ok (search "crashed" (text-for "eof"))
          "and a crash still reads as one"))
    ;; The other way the message is delivered, and the one production
    ;; usually takes: the worker died between requests, the pool replaced it,
    ;; and the next call gets the one-time notification from the replacement
    ;; -- which is why the pool copies the dead worker's reason onto it.
    (flet ((text-after-reset (reason)
             (let* ((worker (cl-mcp/src/worker-client::make-worker
                             :state :bound))
                    (cl-mcp/src/proxy::*current-session-id* "leak-probe-reset")
                    (cl-mcp/src/proxy::%cached-get-or-assign%
                      (lambda (session) (declare (ignore session)) worker))
                    (cl-mcp/src/proxy::%cached-check-and-clear%
                      (lambda (w) (declare (ignore w)) t))
                    (cl-mcp/src/proxy::%cached-worker-last-crash-reason%
                      #'cl-mcp/src/worker-client:worker-last-crash-reason)
                    (cl-mcp/src/proxy::%cached-worker-last-exit-status%
                      #'cl-mcp/src/worker-client:worker-last-exit-status)
                    (cl-mcp/src/proxy::%cached-worker-last-exit-code%
                      #'cl-mcp/src/worker-client:worker-last-exit-code))
               (setf (cl-mcp/src/worker-client:worker-last-crash-reason worker)
                     reason)
               (gethash "text"
                        (aref (gethash "content"
                                       (cl-mcp/src/proxy:proxy-to-worker
                                        "leak-probe-reset-request"
                                        "repl-eval" nil))
                              0)))))
      (ok (search "left a thread that could not be stopped"
                  (text-after-reset
                   cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*))
          "the replacement's one-time notification says it too")
      (ok (search "crashed" (text-after-reset "eof"))
          "and still reads as a crash when it was one"))
    ;; Delivering the message settles what the death owed the user.  The
    ;; pool hands an *undelivered* one to the replacement, so a proxy that
    ;; reports the death without consuming it produces the message twice --
    ;; and, before the flag meant this, a pool that read it the other way
    ;; round produced it neither time for a death nobody reported.
    (let* ((worker (cl-mcp/src/worker-client::make-worker :state :bound))
           (cl-mcp/src/proxy::*current-session-id* "leak-probe-consume")
           (cl-mcp/src/proxy::%cached-get-or-assign%
             (lambda (session) (declare (ignore session)) worker))
           (cl-mcp/src/proxy::%cached-check-and-clear%
             #'cl-mcp/src/worker-client:check-and-clear-reset-notification)
           (cl-mcp/src/proxy::%cached-worker-rpc%
             (lambda (w method params &key timeout)
               (declare (ignore method params timeout))
               ;; What %MARK-WORKER-CRASHED does on its way out.
               (setf (cl-mcp/src/worker-client:worker-needs-reset-notification
                      w)
                     t)
               (error 'cl-mcp/src/worker-client:worker-crashed
                      :worker w :reason "eof")))
           (cl-mcp/src/proxy::%cached-worker-crashed-sym%
             'cl-mcp/src/worker-client:worker-crashed)
           (cl-mcp/src/proxy::%cached-worker-crashed-reason%
             #'cl-mcp/src/worker-client:worker-crashed-reason)
           (cl-mcp/src/proxy::%cached-worker-last-crash-reason%
             #'cl-mcp/src/worker-client:worker-last-crash-reason)
           (cl-mcp/src/proxy::%cached-worker-last-exit-status%
             #'cl-mcp/src/worker-client:worker-last-exit-status)
           (cl-mcp/src/proxy::%cached-worker-last-exit-code%
             #'cl-mcp/src/worker-client:worker-last-exit-code))
      ;; Bound for PROXY-TO-WORKER to find, not for this body to read: they
      ;; are the proxy's own cached bindings, and it resolves them itself.
      (declare (ignorable cl-mcp/src/proxy::%cached-get-or-assign%
                          cl-mcp/src/proxy::%cached-check-and-clear%
                          cl-mcp/src/proxy::%cached-worker-rpc%
                          cl-mcp/src/proxy::%cached-worker-crashed-sym%
                          cl-mcp/src/proxy::%cached-worker-crashed-reason%
                          cl-mcp/src/proxy::%cached-worker-last-crash-reason%
                          cl-mcp/src/proxy::%cached-worker-last-exit-status%
                          cl-mcp/src/proxy::%cached-worker-last-exit-code%))
      (cl-mcp/src/proxy:proxy-to-worker "leak-probe-consume-request"
                                        "repl-eval" nil)
      (ok (not (cl-mcp/src/worker-client:worker-needs-reset-notification
                worker))
          "reporting the death here settles the reset it owed"))))
