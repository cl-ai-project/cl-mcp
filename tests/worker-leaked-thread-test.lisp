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
  (labels ((dead-worker (&key leaked)
             ;; No process, so classification falls back to the count the
             ;; worker last reported, which is the path that runs when the
             ;; process is gone or not yet reaped.
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
    (testing "a worker that died carrying one is reported as retired"
      (let ((reason (reason-of (dead-worker :leaked 1))))
        (ok (equal cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
                   reason)
            (format nil "reason was ~S" reason))))
    (testing "a worker that died carrying nothing is reported as a crash"
      (ok (equal "eof" (reason-of (dead-worker)))))
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
      (let ((retired (dead-worker))
            (crashed (dead-worker)))
        (setf (cl-mcp/src/worker-client::worker-last-crash-reason retired)
              cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*
              (cl-mcp/src/worker-client::worker-last-crash-reason crashed)
              "eof")
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
      ;; the EOF, and then nothing has recorded a reason at all.  The exit
      ;; code is the only witness left on that path -- and on its own it is
      ;; one a REPL can produce, so it is not taken on its own.
      (let ((unclassified (dead-worker :leaked 1))
            (forged (dead-worker :leaked 0)))
        (setf (cl-mcp/src/worker-client:worker-last-exit-code unclassified) 70
              (cl-mcp/src/worker-client:worker-last-exit-code forged) 70)
        (ok (cl-mcp/src/worker-client::worker-retired-p unclassified)
            "an unclassified death is read from the exit code it left")
        (ok (not (cl-mcp/src/pool::%breaker-countable-crash-p unclassified))
            "and is excluded from the breaker on that basis alone")
        (ok (not (cl-mcp/src/worker-client::worker-retired-p forged))
            "but a worker that never reported a leak did not retire for one")))))

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
        (setf (cl-mcp/src/worker-client::worker-stream worker) nil
              (cl-mcp/src/worker-client:worker-last-crash-reason worker)
              cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*)
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
    (testing "and with nothing recorded it still says something"
      (let ((worker (dead-worker)))
        (setf (cl-mcp/src/worker-client::worker-stream worker) nil)
        (ok (equal "already-dead" (reason-of worker)))))))

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
    (testing "with no process at all the count is what is left to go on"
      (ok (cl-mcp/src/worker-client::%retired-for-leaked-thread-p
           (cl-mcp/src/worker-client::make-worker :state :bound
                                                  :leaked-threads 1))
          "reported a leak")
      (ok (not (cl-mcp/src/worker-client::%retired-for-leaked-thread-p
                (cl-mcp/src/worker-client::make-worker :state :bound
                                                       :leaked-threads 0)))
          "reported none"))))

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
        (flet ((kill-and-mark-retired (worker)
                 (sb-posix:kill (cl-mcp/src/worker-client:worker-pid worker)
                                sb-posix:sigkill)
                 (sleep 0.5)
                 (setf (cl-mcp/src/worker-client:worker-last-crash-reason
                        worker)
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
            (bt:with-lock-held (cl-mcp/src/pool::*pool-lock*)
              (ok (gethash session cl-mcp/src/pool::*affinity-map*)
                  "the session is recovered rather than halted on its first
retirement, against a threshold of one")))
          ;; The other path: the next request finds the crashed worker still
          ;; in the affinity map and pushes there instead.
          (let* ((session "retire-breaker-next-request")
                 (worker (cl-mcp/src/pool:get-or-assign-worker session)))
            (kill-and-mark-retired worker)
            (setf (cl-mcp/src/worker-client:worker-state worker) :crashed)
            (let ((replacement
                    (handler-case (cl-mcp/src/pool:get-or-assign-worker session)
                      (error () nil))))
              (ok replacement "the session is still served after a retirement")
              (ok (not (eq worker replacement))
                  "and served by a new worker"))))))))

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
    (with-clean-leak-record
      (leak-one-thread)
      (let* ((served nil)
             (retired nil)
             (server (cl-mcp/src/worker/server::%make-worker-server))
             (cl-mcp/src/worker/server::*retire-action*
               (lambda (leaked) (declare (ignore leaked)) (setf retired t))))
        (setf (cl-mcp/src/worker/server::worker-server-authenticated-p server) t)
        (cl-mcp/src/worker/server:register-method
         server "worker/init-status"
         (lambda (params) (declare (ignore params)) (setf served t) "status"))
        (cl-mcp/src/worker/server::%dispatch-request
         server 1 "worker/init-status" (make-hash-table :test 'equal))
        (ok (null retired) "a poll does not retire the worker")
        (ok served "and is answered"))))
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
          "and a crash still reads as one"))))
