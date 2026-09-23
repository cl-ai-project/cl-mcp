;;;; src/reset-events.lisp
;;;;
;;;; The ledger of state-loss events: each time a worker holding a session's
;;;; Lisp state ends, why it ended, and whether the session has been told.
;;;;
;;;; One worker's end is one event, recorded once.  The first account of why
;;;; it ended is the one kept: a worker stopped to cancel a request, or by
;;;; pool-kill-worker, is recorded as such before it is signalled, so the EOF,
;;;; the stream error or the dead process that follows -- each of which would
;;;; otherwise read as a crash -- adds nothing but exit details.
;;;;
;;;; An event is owed to the session the worker was bound to when it ended,
;;;; and delivered to it exactly once, by whichever response claims it first:
;;;; the failed request that met the death, or the session's next request.
;;;; Several pending events may be claimed together and told in one response.
;;;; A worker that ended bound to no session -- a standby, a released
;;;; session's, a pool shutting down -- owes nobody anything.
;;;;
;;;; Kept per session rather than on the worker, because the worker that
;;;; owes a reset is being thrown away, and not every path that throws one
;;;; away has a replacement in hand to pass it to.

(defpackage #:cl-mcp/src/reset-events
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held)
  (:export #:reset-event-worker-id
           #:reset-event-session-id
           #:reset-event-cause
           #:reset-event-reason
           #:reset-event-exit-status
           #:reset-event-exit-code
           #:reset-event-status
           #:*termination-causes*
           #:record-termination
           #:amend-termination-exit
           #:worker-termination
           #:termination-cause
           #:pending-session-resets
           #:claim-session-resets
           #:discard-session-resets
           #:discard-all-resets))

(in-package #:cl-mcp/src/reset-events)

(defparameter *termination-causes*
  '(:crashed :timeout :retired :cancelled :killed :released :shutdown :stopped)
  "Why a worker ended.

  :CRASHED    it died, or its connection failed, without being asked to;
  :TIMEOUT    it did not answer within the proxy's deadline, and was abandoned;
  :RETIRED    it exited on its own, rather than serve a request while carrying
              a thread an earlier deadline could not stop;
  :CANCELLED  it was stopped to cancel the request it was running;
  :KILLED     it was stopped by pool-kill-worker;
  :RELEASED   its session was released;
  :SHUTDOWN   the pool was shut down;
  :STOPPED    the pool ended it for any other reason.

The first four are the worker's doing or the deadline's; the rest are
decisions, recorded before the signal that carries them out.")

(defstruct (reset-event (:conc-name reset-event-))
  "One worker's end: which worker, whose state it held, why it ended, and
whether that session has been told."
  (worker-id nil)
  (session-id nil)
  (cause :crashed :type keyword)
  (reason nil)
  (exit-status nil)
  (exit-code nil)
  (status :pending :type keyword))

;;; A DEFSTRUCT slot takes no documentation, so the exported readers get theirs
;;; here.
(progn
  (setf (documentation 'reset-event-worker-id 'function)
        "The id of the worker whose end EVENT records."
        (documentation 'reset-event-session-id 'function)
        "The session whose state EVENT's worker held, or NIL when it held none."
        (documentation 'reset-event-cause 'function)
        "Why EVENT's worker ended: one of *TERMINATION-CAUSES*."
        (documentation 'reset-event-reason 'function)
        "The transport's account of EVENT's worker's end, such as \"eof\" or
\"timeout\", or NIL."
        (documentation 'reset-event-exit-status 'function)
        "The process status observed when EVENT's worker ended, or NIL."
        (documentation 'reset-event-exit-code 'function)
        "The process exit code observed when EVENT's worker ended, or NIL."
        (documentation 'reset-event-status 'function)
        "Whether EVENT's session has been told: :PENDING until it is,
:DELIVERED once it was, :DISCARDED when the session went away first, and
:UNOWED when the worker held no session's state.  Read and written under the
ledger's lock.")
  'reset-event)

(defvar *reset-events-lock* (make-lock "reset-events-lock")
  "Guards the ledger: every event's status, and both tables below.")

(defvar *terminations* (make-hash-table :test 'eq :weakness :key)
  "Each ended worker's event, keyed by the worker.  Held weakly: the event
outlives the worker only in its session's queue.")

(defvar *pending* (make-hash-table :test 'equal)
  "Sessions owed a reset, each mapped to its undelivered events, oldest first.")

(defun record-termination (worker cause &key worker-id session-id owed
                                              reason exit-status exit-code)
  "Record that WORKER ended, for CAUSE, and return its event.

Only the first record for a worker counts: a later one returns the existing
event unchanged, so a cause decided before the worker was signalled is not
replaced by the crash its signal then looks like.  The second value is true
when this call made the record.

OWED says the worker held SESSION-ID's state when it ended, so the session
is owed a reset; otherwise the event is recorded as owing nobody."
  (assert (member cause *termination-causes*) (cause))
  (with-lock-held (*reset-events-lock*)
    (let ((existing (gethash worker *terminations*)))
      (when existing
        (return-from record-termination (values existing nil))))
    (let ((event (make-reset-event
                  :worker-id worker-id
                  :session-id session-id
                  :cause cause
                  :reason reason
                  :exit-status exit-status
                  :exit-code exit-code
                  :status (if (and owed session-id) :pending :unowed))))
      (setf (gethash worker *terminations*) event)
      (when (eq :pending (reset-event-status event))
        (setf (gethash session-id *pending*)
              (append (gethash session-id *pending*) (list event))))
      (values event t))))

(defun amend-termination-exit (worker exit-status exit-code)
  "Fill in the exit details of WORKER's event where it has none that say how
the process ended.  The cause is never changed.  Returns the event, or NIL
when WORKER has none."
  (with-lock-held (*reset-events-lock*)
    (let ((event (gethash worker *terminations*)))
      (when (and event
                 (member exit-status '("exited" "signaled") :test #'equal)
                 (not (member (reset-event-exit-status event)
                              '("exited" "signaled") :test #'equal)))
        (setf (reset-event-exit-status event) exit-status
              (reset-event-exit-code event) exit-code))
      event)))

(defun worker-termination (worker)
  "Return WORKER's event, or NIL while it has not ended."
  (with-lock-held (*reset-events-lock*)
    (gethash worker *terminations*)))

(defun termination-cause (worker)
  "Return why WORKER ended, or NIL while it has not."
  (let ((event (worker-termination worker)))
    (and event (reset-event-cause event))))

(defun pending-session-resets (session-id)
  "Return SESSION-ID's undelivered events, oldest first, without claiming them."
  (with-lock-held (*reset-events-lock*)
    (copy-list (gethash session-id *pending*))))

(defun claim-session-resets (session-id)
  "Claim every event SESSION-ID is owed and has not been told, oldest first,
for the response about to tell it.  Each is claimed once: a second call
returns only what was recorded in between."
  (with-lock-held (*reset-events-lock*)
    (let ((events (gethash session-id *pending*)))
      (remhash session-id *pending*)
      (dolist (event events)
        (setf (reset-event-status event) :delivered))
      events)))

(defun discard-session-resets (session-id)
  "Drop what SESSION-ID is owed, as when the session itself is going away:
nobody is left to tell, and a later session reusing the id must not be told
about another's workers.  Returns the events dropped."
  (with-lock-held (*reset-events-lock*)
    (let ((events (gethash session-id *pending*)))
      (remhash session-id *pending*)
      (dolist (event events)
        (setf (reset-event-status event) :discarded))
      events)))

(defun discard-all-resets ()
  "Drop what every session is owed, as a pool shutdown does."
  (with-lock-held (*reset-events-lock*)
    (maphash (lambda (session-id events)
               (declare (ignore session-id))
               (dolist (event events)
                 (setf (reset-event-status event) :discarded)))
             *pending*)
    (clrhash *pending*)))
