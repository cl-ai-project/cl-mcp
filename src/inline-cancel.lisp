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
  "Maps (SESSION-ID . REQUEST-ID) to the thread running that request's
cancellable region.")

(defvar *regions-lock* (bordeaux-threads:make-lock "inline-cancel-regions")
  "Guards *REGIONS*.")

(defvar *in-cancellable-region* nil
  "True, in a thread, while it runs a CALL-CANCELLABLE region.  An interrupt
checks it, so a cancellation that arrives just after the region has ended does
nothing.")

(defun %region-key (session-id request-id)
  "Return the key of a request's region in *REGIONS*.  The id is compared as
the client wrote it, so a number and a string id are different requests."
  (cons session-id request-id))

(defun call-cancellable (session-id request-id thunk)
  "Call THUNK as the cancellable region of request REQUEST-ID of SESSION-ID and
return its values.  While it runs, CANCEL-INLINE-REQUEST for that request
signals INLINE-REQUEST-CANCELLED in this thread, from wherever THUNK is.
Without both ids there is nothing to cancel by, and THUNK just runs.  Only
work that can be abandoned at any point belongs in THUNK: a search, not a
file write."
  (if (and session-id request-id)
      (let ((key (%region-key session-id request-id)))
        (bordeaux-threads:with-lock-held (*regions-lock*)
          (setf (gethash key *regions*) (bordeaux-threads:current-thread)))
        (unwind-protect
             (let ((*in-cancellable-region* t))
               (funcall thunk))
          (bordeaux-threads:with-lock-held (*regions-lock*)
            (when (eq (gethash key *regions*) (bordeaux-threads:current-thread))
              (remhash key *regions*)))))
      (funcall thunk)))

(defun cancel-inline-request (session-id request-id)
  "Cancel request REQUEST-ID of SESSION-ID if it is running a cancellable
region: interrupt its thread, which then signals INLINE-REQUEST-CANCELLED.
Return T when a region was found, NIL otherwise."
  (let ((thread (bordeaux-threads:with-lock-held (*regions-lock*)
                  (gethash (%region-key session-id request-id) *regions*))))
    (when (and thread (bordeaux-threads:thread-alive-p thread))
      (bordeaux-threads:interrupt-thread
       thread
       (lambda ()
         (when *in-cancellable-region*
           (error 'inline-request-cancelled))))
      t)))
