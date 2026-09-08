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
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread
                #:leaked-threads
                #:forget-leaked-threads)
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
leave a live thread running into whatever runs next."
  `(unwind-protect (progn (forget-leaked-threads) ,@body)
     (release-probe-threads)
     (forget-leaked-threads)))

(defun leak-one-thread ()
  "Run a deadline against a thread that cannot be stopped.
Returns two values: whether the deadline said so, and the thread itself, taken
from the record rather than from the deadline -- which reports only that a
thread was left behind, not which one.

SB-SYS:WITHOUT-INTERRUPTS defers both the cooperative unwind and
DESTROY-THREAD, which is what being unstoppable actually looks like.  The
thread polls *PROBE-RELEASE* rather than sleeping a fixed time so the test can
end it deliberately: a fixed sleep leaves it running into whatever runs next,
and makes the assertions depend on the runner being fast enough to observe it."
  (setf *probe-release* nil)
  (let ((reported (nth-value 2 (call-with-deadline-thread
                                (lambda ()
                                  (sb-sys:without-interrupts
                                    (let ((stop (+ (get-internal-real-time)
                                                   (* 30 internal-time-units-per-second))))
                                      (loop until (or *probe-release*
                                                      (> (get-internal-real-time) stop))
                                            do (sleep 0.02))))
                                  :finished)
                                0.3))))
    (values reported (first (leaked-threads)))))

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
        (ignore-errors
         (cl-mcp/src/worker-client:worker-rpc worker "worker/probe" nil))
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
        (ok (cl-mcp/src/pool::%retired-worker-p retired))
        (ok (not (cl-mcp/src/pool::%retired-worker-p crashed)))
        ;; The decision both push sites actually make.  Asserting only
        ;; %RETIRED-WORKER-P leaves the exclusion deletable from either site
        ;; without a test noticing.
        (ok (not (cl-mcp/src/pool::%breaker-countable-crash-p retired))
            "a retirement is not counted against the session")
        (ok (cl-mcp/src/pool::%breaker-countable-crash-p crashed)
            "a real crash still is")
        ;; One caller has to sample the reason before the surrounding function
        ;; overwrites it, so the predicate takes it as an argument -- and that
        ;; path needs its own assertion.
        (ok (not (cl-mcp/src/pool::%breaker-countable-crash-p
                  crashed :retired t))
            "including when the caller had to capture it early")))))

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
    (testing "the retirement exit code says retirement, whatever the count"
      (ok (classify :code 70 :leaked 0)
          "even with no count, an exit 70 is a retirement"))
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
          (format nil "~A does not" method))))
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
               (let ((json (yason:parse
                            (cl-mcp/src/worker/server::%dispatch-request
                             server 1 "worker/eval"
                             (make-hash-table :test 'equal)))))
                 (ok (gethash "error" json) "the request is refused")
                 (ok (null (nth-value 1 (gethash "leaked_threads" json)))
                     "and says nothing about the worker's internal state")))
          (if had
              (sb-posix:setenv "MCP_WORKER_SECRET" had 1)
              (sb-posix:unsetenv "MCP_WORKER_SECRET"))))))
  (testing "the user is told a worker was replaced, not that it crashed"
    (let ((retired (cl-mcp/src/proxy::%crash-notification-result
                    :reason cl-mcp/src/utils/deadline:*retired-leaked-thread-reason*))
          (crashed (cl-mcp/src/proxy::%crash-notification-result :reason "eof")))
      (flet ((text-of (ht)
               (gethash "text" (aref (gethash "content" ht) 0))))
        (ok (search "left a thread that could not be stopped" (text-of retired))
            "a retirement says what actually happened")
        (ok (search "crashed" (text-of crashed))
            "and a crash still reads as one")))))
