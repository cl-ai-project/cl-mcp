;;;; src/utils/deadline.lisp
;;;;
;;;; One implementation of "run this on its own thread and answer by a
;;;; deadline", shared by repl-eval, load-system and run-tests.  Each of the
;;;; three grew its own copy of the spawn/poll/destroy sequence and the copies
;;;; drifted: only one logged a leaked thread, only one re-read the result
;;;; after the deadline, and each named the outcome differently.

(defpackage #:cl-mcp/src/utils/deadline
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:thread-alive-p
                #:destroy-thread
                #:interrupt-thread
                #:make-lock
                #:with-lock-held)
  (:export #:call-with-deadline-thread
           #:leaked-threads
           #:*retired-leaked-thread-reason*
           #:+leaked-thread-exit-code+
           #:forget-leaked-threads
           #:*poll-interval*
           #:*unwind-grace-seconds*
           #:*destroy-grace-seconds*))

(in-package #:cl-mcp/src/utils/deadline)

(defparameter *poll-interval* 0.05d0
  "Seconds between liveness checks while waiting on a deadline thread.")

(defparameter *unwind-grace-seconds* 0.5d0
  "Seconds allowed for a cooperative unwind before the thread is destroyed.
An unwind releases the locks the run holds and runs its UNWIND-PROTECT
cleanups, so it is always preferable to DESTROY-THREAD.")

(defparameter *destroy-grace-seconds* 1.0d0
  "Seconds allowed for DESTROY-THREAD to take effect before the thread is
reported as leaked.")

(defparameter *retired-leaked-thread-reason* "retired-leaked-thread"
  "Crash reason for a worker that exited rather than serve a request while
carrying a thread a deadline could not stop.

It reaches the parent as EOF like any other death, so this is what the reason
is set to once the parent has established that is what happened.  Callers that
treat a crash as evidence about something else -- the pool's init monitor,
which disables runtime initialization when the init owner crashes, and its
per-session circuit breaker -- check for it rather than blaming an unrelated
subsystem for a deliberate retirement.")

(defconstant +leaked-thread-exit-code+ 70
  "Exit code a worker uses when it retires for carrying a leaked thread.
The parent reads it to tell a deliberate retirement from a crash.

Both of these live here, with the deadline machinery that creates the
condition, rather than with either half of the worker protocol: the parent and
the worker are the two ends of this contract and neither can own it without
the other depending on it.")

(defvar %leaked-threads% ()
  "Threads a deadline could not stop, still running in this image.

Recorded centrally so every deadline call site contributes without plumbing
one through, and so the process can ask whether it is still carrying any.  A
thread stays on this list only while it is alive: LEAKED-THREADS prunes, which
is what lets a run that was given up on but finished later stop counting
against the image.")

(defvar %leaked-lock% (make-lock "deadline-leaked-threads")
  "Protects %LEAKED-THREADS%.  Deadlines can be enforced from several threads
at once -- one per session in a worker, and nested within a run.")

(defun leaked-threads ()
  "Return the threads a deadline could not stop that are still running.

Pruned on every call, so a run that was given up on and then finished on its
own stops being reported.  That is the difference between \"this image was
compromised at some point\" and \"this image is compromised now\": the first
would retire a worker that had recovered, and the state a session loses to
that is the very thing the deadline machinery exists to protect."
  (with-lock-held (%leaked-lock%)
    (setf %leaked-threads% (remove-if-not #'thread-alive-p %leaked-threads%))))

(defun forget-leaked-threads ()
  "Drop the record of leaked threads without stopping them.
For tests, which need to leak a thread deliberately and then leave the image
as they found it."
  (with-lock-held (%leaked-lock%)
    (setf %leaked-threads% ())))

(defun %wait-until-dead (thread seconds)
  "Poll until THREAD is gone or SECONDS elapse.  Returns true when it is gone."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop while (and (thread-alive-p thread)
                     (< (get-internal-real-time) deadline))
          do (sleep *poll-interval*))
    (not (thread-alive-p thread))))

(defun call-with-deadline-thread (thunk timeout-seconds &key (name "mcp-deadline"))
  "Run THUNK on a dedicated thread and answer within TIMEOUT-SECONDS.

Returns three values:
  RESULT  the list of THUNK's values on :OK, TIMEOUT-SECONDS on :TIMEOUT,
          the condition on :ERROR.
  STATUS  :OK, :TIMEOUT or :ERROR.
  LEAKED  true when the run thread was still alive on return, i.e. neither the
          cooperative unwind nor DESTROY-THREAD could stop it.  The caller is
          answered either way, but a leaked thread is still running: it may
          hold locks and it will contend with later work in this image.
          Threads THUNK spawns are not tracked -- only the run thread is.

Without a usable TIMEOUT-SECONDS the thunk runs inline on the caller's thread
and any condition propagates -- a thread buys nothing when there is no
deadline to enforce, and running inline keeps the caller's own handlers and
backtrace intact.

At the deadline the thread is first asked to unwind cooperatively, by
INTERRUPT-THREAD throwing to a tag private to this call.  The private tag,
rather than SB-EXT:WITH-TIMEOUT, is what makes the outcome unambiguous:
WITH-TIMEOUT signals SB-EXT:TIMEOUT, which the thunk may also signal on its
own -- a test suite exercising timeout behaviour, say -- leaving the two
indistinguishable and reporting the suite's own timeout as a deadline breach.
A throw also cannot be swallowed by a HANDLER-CASE inside the thunk, which a
condition can.  Only when the interrupt goes unobserved -- a blocking foreign
call cannot run it -- is the thread destroyed.

Completed work survives a deadline that expires alongside it, which takes two
guards rather than one.  The thunk runs with interrupts enabled inside a
WITHOUT-INTERRUPTS, so once it returns, delivery of the deadline throw is
deferred until its values have been published -- a naked assignment would let
the throw land after the thunk returned but before its result was stored.  And
the interrupt function re-reads OUTCOME in the interrupted thread before
throwing, where publication cannot be half-done, so a run that finished just
as the deadline expired is left to return normally instead of being unwound
out of its own result.

A thread that dies before the deadline without publishing anything -- THUNK
called SB-THREAD:ABORT-THREAD, say -- is reported as :ERROR rather than
:TIMEOUT, so a deadline that never elapsed is not blamed for it."
  (if (not (and timeout-seconds (realp timeout-seconds) (plusp timeout-seconds)))
      (values (multiple-value-list (funcall thunk)) :ok nil)
      (let ((tag (list :deadline))
            (outcome nil)
            (thread nil)
            (answered nil))
        (flet ((stop ()
                 ;; Cooperative unwind first: it runs the thread's
                 ;; UNWIND-PROTECT cleanups and releases the locks it holds.
                 (when (and thread (thread-alive-p thread))
                   (ignore-errors
                    (interrupt-thread
                     thread
                     ;; OUTCOME is checked in the interrupted thread, where
                     ;; publication cannot be in progress: it runs under
                     ;; WITHOUT-INTERRUPTS, so an OUTCOME seen here is
                     ;; complete.  A run that finished just as the deadline
                     ;; expired is therefore left to return normally instead
                     ;; of being unwound out of its own result.
                     ;;
                     ;; IGNORE-ERRORS around the throw, and not only around
                     ;; INTERRUPT-THREAD: this closure runs later, on the run
                     ;; thread, outside any handler of ours.  A thread can be
                     ;; alive with the CATCH already gone -- an earlier throw
                     ;; consumed it and the thread is still winding down --
                     ;; and a throw to a tag that no longer exists is an
                     ;; unhandled CONTROL-ERROR there.  The worker runs under
                     ;; SB-EXT:DISABLE-DEBUGGER, where that kills the process
                     ;; outright: the session would lose all its state to a
                     ;; deadline whose whole purpose is to answer gracefully.
                     ;; The window is narrow -- STOP has to run twice, which
                     ;; takes a non-local exit between FINISH and ANSWERED --
                     ;; but the guard costs nothing and the failure it
                     ;; prevents is total.
                     (lambda ()
                       (unless outcome
                         (ignore-errors (throw tag :deadline))))))
                   (%wait-until-dead thread *unwind-grace-seconds*))
                 (when (and thread (thread-alive-p thread))
                   (ignore-errors (destroy-thread thread))
                   (%wait-until-dead thread *destroy-grace-seconds*))
                 ;; Recorded here rather than on the way out, because STOP is
                 ;; also what the UNWIND-PROTECT cleanup runs.  A nested
                 ;; deadline unwound by an outer one never reaches its normal
                 ;; return, and registering only there would let the thread it
                 ;; could not stop go unrecorded -- leaving the image looking
                 ;; clean while still carrying it.
                 (when (and thread (thread-alive-p thread))
                   (with-lock-held (%leaked-lock%)
                     (pushnew thread %leaked-threads%))))
               (finish (timed-out leaked)
                 (let ((settled outcome))
                   (cond
                     (settled (values (cdr settled) (car settled) leaked))
                     (timed-out (values timeout-seconds :timeout leaked))
                     (t (values (make-condition
                                 'simple-error
                                 :format-control
                                 "the ~A thread exited without a result"
                                 :format-arguments (list name))
                                :error leaked))))))
          ;; The whole UNWIND-PROTECT sits under WITHOUT-INTERRUPTS, with only
          ;; the waiting re-enabled: SBCL requires that for a cleanup to be
          ;; safe against an asynchronous unwind.  Nested calls make it
          ;; concrete -- a run thread may itself run a deadline of its own --
          ;; and an outer DESTROY-THREAD landing mid-cleanup would drop the
          ;; inner one, leaving its thread running with nobody tracking it.
          (sb-sys:without-interrupts
            (unwind-protect
                 (progn
                   ;; Spawned with interrupts deferred, so no asynchronous
                   ;; exit can land between the thread existing and THREAD
                   ;; naming it -- the cleanup would then have nothing to stop
                   ;; and the thread would run on unnoticed.
                   (setf thread
                         (make-thread
                          (lambda ()
                            (catch tag
                              (sb-sys:without-interrupts
                                (handler-case
                                    (setf outcome
                                          (cons :ok
                                                (multiple-value-list
                                                 (sb-sys:with-local-interrupts
                                                   (funcall thunk)))))
                                  (serious-condition (e)
                                    (setf outcome (cons :error e)))))))
                          :name name))
                   (sb-sys:with-local-interrupts
                     (%wait-until-dead thread timeout-seconds)
                     (let ((timed-out (thread-alive-p thread)))
                       (stop)
                       ;; STOP has already recorded the thread if it survived;
                       ;; the caller is told here so it can say so in its own
                       ;; result.
                       (multiple-value-prog1 (finish timed-out
                                                     (thread-alive-p thread))
                         (setf answered t)))))
              ;; A non-local exit from the caller -- an outer deadline, a
              ;; kill -- must not leave the run thread executing unnoticed.
              ;; Guarded so the normal path does not pay for a second
              ;; interrupt-and-destroy cycle it has already completed.
              (unless answered (stop))))))))
