;;;; src/request-lifecycle.lisp
;;;;
;;;; The lifecycle of one proxied tool request, kept apart from the lifecycle
;;;; of the worker that serves it.
;;;;
;;;; A request is registered under its session and its JSON-RPC id, and moves
;;;; through phases:
;;;;
;;;;   :registered -> :acquiring -> :waiting-to-send -> :executing -> :responded
;;;;
;;;; :EXECUTING begins at the moment its bytes are sent to the worker, and
;;;; :RESPONDED at the moment the worker's answer has been read -- both under
;;;; the worker's stream lock, through the hooks WORKER-RPC calls.  That is
;;;; what lets a cancellation act on the request it names and nothing else:
;;;;
;;;; - before :EXECUTING the request has not reached the worker; cancelling it
;;;;   only marks it, and it stops at its next boundary without running;
;;;; - at :EXECUTING the worker is running exactly this request -- nothing
;;;;   else holds its stream -- so stopping the worker stops this request;
;;;; - at :RESPONDED it is done, and a cancellation changes nothing.
;;;;
;;;; A worker stopped to cancel a request is remembered, so a request queued
;;;; behind it on the same worker is refused before sending rather than sent
;;;; into a dying process.
;;;;
;;;; The phase a request reached also decides what may be said about it
;;;; (REQUEST-OUTCOME): it did not run, it ran to an answer, or whether it
;;;; ran -- and what it changed -- is unknown.  Nothing here, or in the proxy,
;;;; runs a request twice.

(defpackage #:cl-mcp/src/request-lifecycle
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock #:with-lock-held)
  (:export #:request-session-id
           #:request-external-id
           #:request-phase
           #:request-worker
           #:request-cancel-requested
           #:*requests*
           #:*requests-lock*
           #:request-key
           #:register-request
           #:unregister-request
           #:find-request
           #:clear-requests
           #:note-request-phase
           #:note-request-worker
           #:cancellation-requested-p
           #:begin-send
           #:note-response
           #:cancel-request-record
           #:worker-stopped-for-cancel-p
           #:request-outcome))

(in-package #:cl-mcp/src/request-lifecycle)

(defstruct (request-record (:conc-name request-))
  "One proxied request: whose it is, which id the client gave it, how far it
got, the worker it was handed to, and whether its cancellation was asked for."
  (session-id nil)
  (external-id nil)
  (phase :registered :type keyword)
  (worker nil)
  (cancel-requested nil))

;;; A DEFSTRUCT slot takes no documentation, so the exported readers get theirs
;;; here.
(progn
  (setf (documentation 'request-session-id 'function)
        "The session RECORD's request came from; part of its identity."
        (documentation 'request-external-id 'function)
        "The JSON-RPC id the client gave RECORD's request; the other part of its
identity."
        (documentation 'request-phase 'function)
        "How far RECORD's request got: :REGISTERED, :ACQUIRING,
:WAITING-TO-SEND, :EXECUTING or :RESPONDED.  Read and written under
*REQUESTS-LOCK*."
        (documentation 'request-worker 'function)
        "The worker RECORD's request was handed, or NIL before one was found."
        (documentation 'request-cancel-requested 'function)
        "True once RECORD's cancellation was asked for.  Read and written under
*REQUESTS-LOCK*.")
  'request-record)

(defvar *requests* (make-hash-table :test 'equal)
  "In-flight requests, keyed by REQUEST-KEY: the session and the id together.
An id alone is not an identity -- every session numbers its requests from
the same small integers.")

(defvar *requests-lock* (make-lock "request-lifecycle-lock")
  "Guards *REQUESTS*, every record's phase and cancellation flag, and the
record of workers stopped for a cancellation.")

(defvar *workers-stopped-for-cancel*
  (make-hash-table :test 'eq :weakness :key)
  "Workers stopped to cancel the request they were running.  A request queued
behind one must not be sent to it.")

(defun request-key (session-id external-id)
  "Return the registry key of the request EXTERNAL-ID in SESSION-ID.  The id is
printed readably, so the integer 1 and the string \"1\" stay two requests."
  (cons session-id (prin1-to-string external-id)))

(defun register-request (session-id external-id)
  "Register a new request and return its record, in phase :REGISTERED.  A
second request under the same key replaces the first in the registry; each
unregisters only itself."
  (let ((record (make-request-record :session-id session-id
                                     :external-id external-id)))
    (with-lock-held (*requests-lock*)
      (setf (gethash (request-key session-id external-id) *requests*) record))
    record))

(defun unregister-request (record)
  "Remove RECORD from the registry, if the registry still holds it."
  (with-lock-held (*requests-lock*)
    (let ((key (request-key (request-session-id record)
                            (request-external-id record))))
      (when (eq record (gethash key *requests*))
        (remhash key *requests*)))))

(defun find-request (session-id external-id)
  "Return the in-flight record of EXTERNAL-ID in SESSION-ID, or NIL."
  (with-lock-held (*requests-lock*)
    (gethash (request-key session-id external-id) *requests*)))

(defun clear-requests ()
  "Forget every in-flight request, as a pool shutdown does."
  (with-lock-held (*requests-lock*)
    (clrhash *requests*)))

(defun note-request-phase (record phase)
  "Move RECORD to PHASE, one of :ACQUIRING or :WAITING-TO-SEND."
  (with-lock-held (*requests-lock*)
    (setf (request-phase record) phase)))

(defun note-request-worker (record worker)
  "Record that RECORD was handed WORKER, and is waiting to be sent."
  (with-lock-held (*requests-lock*)
    (setf (request-worker record) worker
          (request-phase record) :waiting-to-send)))

(defun cancellation-requested-p (record)
  "True when RECORD's cancellation was asked for."
  (with-lock-held (*requests-lock*)
    (request-cancel-requested record)))

(defun worker-stopped-for-cancel-p (worker)
  "True when WORKER was stopped to cancel the request it was running."
  (with-lock-held (*requests-lock*)
    (gethash worker *workers-stopped-for-cancel*)))

(defun begin-send (record)
  "Decide, just before RECORD's bytes go to its worker, whether they may.
Called with the worker's stream held, so nothing else is running on it.
Returns :SEND after moving RECORD to :EXECUTING; :CANCELLED when its
cancellation was asked for; :WORKER-STOPPED when its worker was stopped for
another request's cancellation.  Only :SEND lets the request run."
  (with-lock-held (*requests-lock*)
    (cond ((request-cancel-requested record) :cancelled)
          ((gethash (request-worker record) *workers-stopped-for-cancel*)
           :worker-stopped)
          (t (setf (request-phase record) :executing)
             :send))))

(defun note-response (record)
  "Decide, once the worker's answer to RECORD has been read, whether it is
delivered -- the one point at which an answer and a cancellation are ordered.

Under the registry lock, as CANCEL-REQUEST-RECORD is: if RECORD's
cancellation got there first, its worker has been signalled to stop, so the
answer is withheld (:WITHDRAWN) and RECORD stays :EXECUTING -- the request
reached the worker, and it is reported cancelled, not answered.  Otherwise
RECORD becomes :RESPONDED (:PUBLISH), and a cancellation from here on is too
late.  Never both: an answer delivered from a worker a cancellation stopped
would report a success in a session whose state was just lost."
  (with-lock-held (*requests-lock*)
    (cond ((request-cancel-requested record) :withdrawn)
          (t (setf (request-phase record) :responded)
             :publish))))

(defun cancel-request-record (record stop-worker)
  "Ask for RECORD's cancellation and return what that did:

  :MARKED     it had not reached its worker; it will stop at its next
              boundary without running;
  :STOPPING   it was running; STOP-WORKER was called on its worker, which is
              now remembered as stopped for a cancellation;
  :TOO-LATE   it had already been answered; nothing was done.

STOP-WORKER is called while the registry is held, so the request cannot pass
from :EXECUTING to :RESPONDED, and no other request can begin sending, in
between -- the worker stopped is the one running this request.  It should
only signal the process; ending the worker, which waits for its stream,
belongs outside."
  (with-lock-held (*requests-lock*)
    (case (request-phase record)
      (:responded :too-late)
      (:executing
       (setf (request-cancel-requested record) t
             (gethash (request-worker record) *workers-stopped-for-cancel*) t)
       (funcall stop-worker (request-worker record))
       :stopping)
      (t
       (setf (request-cancel-requested record) t)
       :marked))))

(defun request-outcome (record)
  "Return what may be said about RECORD's execution, from the phase it
reached: :COMPLETED once the worker answered, :EXECUTION-UNKNOWN once it was
sent and no answer came -- it may have run, partly or wholly -- and
:NOT-EXECUTED before it was sent."
  (with-lock-held (*requests-lock*)
    (case (request-phase record)
      (:responded :completed)
      (:executing :execution-unknown)
      (t :not-executed))))
