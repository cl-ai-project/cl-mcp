(defpackage #:cl-mcp/src/utils/request-debugger-boundary-protocol
  (:use #:cl)
  (:export #:*request-debugger-boundary-active*
           #:call-with-request-debugger-boundary
           #:request-debugger-deadline-interrupt
           #:request-debugger-result-status
           #:request-debugger-result-error))

(in-package #:cl-mcp/src/utils/request-debugger-boundary-protocol)

(declaim (optimize (debug 3) (safety 3)))

;; Deadline enforcement is also used by the diagnostic dependency chain.
;; Keep this internal protocol independent of the concrete boundary, whose
;; methods are loaded by request owners that enable the Boolean policy.
(defvar *request-debugger-boundary-active* nil
  "Boolean policy enabled only around authenticated request execution.
Managed children may inherit this policy, but must create their own boundary.")

(declaim (type boolean *request-debugger-boundary-active*))

(defgeneric call-with-request-debugger-boundary (thunk)
  (:documentation
   "Run THUNK and return the concrete request boundary's settled result."))

(defgeneric request-debugger-result-status (result)
  (:documentation "Return :OK, :DEBUGGER, or :TIMEOUT for the settled boundary result."))

(defgeneric request-debugger-result-error (result)
  (:documentation "Return the saved escape error only for a :DEBUGGER result."))

(defgeneric request-debugger-deadline-interrupt (deadline-tag deadline-marker)
  (:documentation
   "Select a live deadline transfer when a request boundary is installed.
Outside a request, retain the guarded transfer used by ordinary deadlines.")
  (:method (deadline-tag deadline-marker)
    (ignore-errors (throw deadline-tag deadline-marker))))
