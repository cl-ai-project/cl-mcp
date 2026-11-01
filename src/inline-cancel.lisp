;;;; src/inline-cancel.lisp
;;;;
;;;; Cancellation for work a tool does inline, in the parent process.  A
;;;; proxied request is cancelled by stopping its worker (src/proxy.lisp); an
;;;; inline one has no worker, so the region that may run long registers its
;;;; thread here, and a cancellation interrupts that thread.

(defpackage #:cl-mcp/src/inline-cancel
  (:use #:cl)
  (:import-from #:bordeaux-threads)
  (:export #:call-cancellable
           #:cancel-inline-request
           #:inline-request-cancelled))

(in-package #:cl-mcp/src/inline-cancel)

(define-condition inline-request-cancelled (serious-condition)
  ()
  (:documentation "Signalled in a thread running a CALL-CANCELLABLE region when
its request is cancelled.  A SERIOUS-CONDITION, not an ERROR, so the region's
own HANDLER-CASE on ERROR -- a search skipping a file it cannot read, say --
does not swallow it.")
  (:report "The request was cancelled."))

(defvar *regions* (make-hash-table :test #'equal)
  "Maps (SESSION-ID . REQUEST-ID) to (THREAD . TOKEN): the thread running that
request's cancellable region, and the token naming the region.")

(defvar *regions-lock* (bordeaux-threads:make-lock "inline-cancel-regions")
  "Guards *REGIONS*.")

(defvar *in-cancellable-region* nil
  "In a thread running a CALL-CANCELLABLE region, the token that names that
region; NIL elsewhere.  An interruption compares it with the token it was sent
for, so a cancellation that arrives after the region ended -- even while the
same thread runs another request's region -- does nothing.")

(defun %region-key (session-id request-id)
  "Return the key of a request's region in *REGIONS*.  The id is compared as
the client wrote it, so a number and a string id are different requests."
  (cons session-id request-id))

(defun call-cancellable (session-id request-id thunk)
  "Call THUNK as the cancellable region of request REQUEST-ID of SESSION-ID and
return its values.  CANCEL-INLINE-REQUEST for that request then signals
INLINE-REQUEST-CANCELLED in this thread -- but only at a cancellation point:
THUNK runs with interrupts disabled, and only a step it wraps in
SB-SYS:WITH-INTERRUPTS can be interrupted (a regex match, say).  Everything
else, foreign calls such as opendir and read included, runs to its end, and a
cancellation that arrives meanwhile lands at the next point or as the region
ends.  Unwinding out of foreign code that holds a lock of the C library would
hang every thread of the server.  Without both ids there is nothing to cancel
by, and THUNK just runs."
  (if (and session-id request-id)
      (let* ((key (%region-key session-id request-id))
             (token (list key))
             (*in-cancellable-region* token))
        ;; Bound before registering, so an interruption that finds the entry
        ;; also finds the binding; the cleanup removes only this region's.
        (unwind-protect
             (progn
               (bordeaux-threads:with-lock-held (*regions-lock*)
                 (setf (gethash key *regions*)
                       (cons (bordeaux-threads:current-thread) token)))
               (sb-sys:without-interrupts
                 (sb-sys:allow-with-interrupts
                   (funcall thunk))))
          ;; Interrupts stay off here (WITH-LOCK-HELD would otherwise allow
          ;; them), so a cancellation pending at the end cannot unwind out
          ;; before the entry is gone; it is taken after, still in the region.
          (sb-sys:without-interrupts
            (bordeaux-threads:with-lock-held (*regions-lock*)
              (when (eq (cdr (gethash key *regions*)) token)
                (remhash key *regions*))))))
      (funcall thunk)))

(defun %interrupt-region (thread token)
  "Interrupt THREAD so that it signals INLINE-REQUEST-CANCELLED if it is still
in the region TOKEN names.  The thread may have left that region and entered
another by the time the interruption runs -- a Hunchentoot thread serves a
connection's requests one after another -- and that other region is not the
one cancelled."
  (bordeaux-threads:interrupt-thread
   thread
   (lambda ()
     (when (eq *in-cancellable-region* token)
       (error 'inline-request-cancelled)))))

(defun cancel-inline-request (session-id request-id)
  "Cancel request REQUEST-ID of SESSION-ID if it is running a cancellable
region: interrupt its thread, which then signals INLINE-REQUEST-CANCELLED at
the region's next cancellation point.  Return T when a region was found, NIL
otherwise."
  (let ((entry (bordeaux-threads:with-lock-held (*regions-lock*)
                 (gethash (%region-key session-id request-id) *regions*))))
    (when (and entry (bordeaux-threads:thread-alive-p (car entry)))
      ;; The thread can end between the check and the interruption.
      (handler-case (progn (%interrupt-region (car entry) (cdr entry)) t)
        (error () nil)))))
