(defpackage #:cl-mcp/src/utils/request-debugger-boundary-protocol
  (:use #:cl)
  (:export #:*request-debugger-boundary-active*
           #:*request-debugger-config*
           #:make-request-debugger-config
           #:request-debugger-capture-options
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

(defstruct request-debugger-config
  "Immutable diagnostic settings, safe to share with managed deadline children."
  (print-level 3 :read-only t)
  (print-length 10 :read-only t)
  (locals-preview-frames 0 :read-only t)
  (preview-max-depth 1 :read-only t)
  (preview-max-elements 5 :read-only t)
  (locals-preview-skip-internal t :read-only t))

(defvar *request-debugger-config* nil
  "Request diagnostic settings only; never contains a hook, tag, context, or snapshot.")

(defun request-debugger-capture-options (config)
  "Return capture keyword arguments for CONFIG, or NIL for the existing defaults."
  (when config
    (list :print-level (request-debugger-config-print-level config)
          :print-length (request-debugger-config-print-length config)
          :locals-preview-frames (request-debugger-config-locals-preview-frames config)
          :preview-max-depth (request-debugger-config-preview-max-depth config)
          :preview-max-elements (request-debugger-config-preview-max-elements config)
          :locals-preview-skip-internal
          (request-debugger-config-locals-preview-skip-internal config))))

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
