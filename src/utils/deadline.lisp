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
                #:interrupt-thread)
  (:export #:call-with-deadline-thread
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
out of its own result."
  (if (not (and timeout-seconds (realp timeout-seconds) (plusp timeout-seconds)))
      (values (multiple-value-list (funcall thunk)) :ok nil)
      (let* ((tag (list :deadline))
             (outcome nil)
             (thread (make-thread
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
                      :name name)))
        (flet ((stop ()
                 ;; Cooperative unwind first: it runs the thread's
                 ;; UNWIND-PROTECT cleanups and releases the locks it holds.
                 (when (thread-alive-p thread)
                   (ignore-errors
                    (interrupt-thread
                     thread
                     ;; Checked in the interrupted thread, where publication
                     ;; cannot be in progress: it runs under
                     ;; WITHOUT-INTERRUPTS, so an OUTCOME seen here is
                     ;; complete.  A run that finished just as the deadline
                     ;; expired is therefore left to return normally instead
                     ;; of being unwound out of its own result.
                     (lambda () (unless outcome (throw tag :deadline)))))
                   (%wait-until-dead thread *unwind-grace-seconds*))
                 (when (thread-alive-p thread)
                   (ignore-errors (destroy-thread thread))
                   (%wait-until-dead thread *destroy-grace-seconds*)))
               (finish (leaked)
                 (let ((settled outcome))
                   (if settled
                       (values (cdr settled) (car settled) leaked)
                       (values timeout-seconds :timeout leaked)))))
          (let ((answered nil))
            (unwind-protect
                 (progn
                   (%wait-until-dead thread timeout-seconds)
                   (stop)
                   (multiple-value-prog1 (finish (thread-alive-p thread))
                     (setf answered t)))
              ;; A non-local exit from the caller -- an outer deadline, a
              ;; kill -- must not leave the run thread executing unnoticed.
              ;; Guarded so the normal path does not pay for a second
              ;; interrupt-and-destroy cycle it has already completed.
              (unless answered (stop))))))))
