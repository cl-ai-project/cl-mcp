;;;; src/yason-compat.lisp

(defpackage #:cl-mcp/src/yason-compat
  (:use #:cl)
  (:import-from #:yason)
  (:documentation "Make yason 0.7.x behave like 0.8 where cl-mcp depends on it.
Older Quicklisp dists still pin 0.7.x, and a project loading cl-mcp through
its own dist gets that version."))

(in-package #:cl-mcp/src/yason-compat)

;; 0.8 binds these to themselves.  0.7.x exports only the symbols, so every
;; (if x t yason:false) signals UNBOUND-VARIABLE.  DEFVAR leaves 0.8's value.
(defvar yason:true 'yason:true)
(defvar yason:false 'yason:false)

;; The parent relays worker results parsed with :json-nulls-as-keyword, and
;; 0.7.x has no ENCODE method for :NULL (only for CL:NULL), so every relayed
;; null fails the response.  Defined only when absent to keep 0.8's own method.
(unless (find-method #'yason:encode '()
                     (list (sb-mop:intern-eql-specializer :null))
                     nil)
  (defmethod yason:encode ((object (eql :null)) &optional (stream *standard-output*))
    (write-string "null" stream)
    object))

;;; 0.7.x also writes control characters other than \b \f \n \r \t raw (#342).
;;; That is not patched here: changing yason's escaping table would also change
;;; the yason a user's own code runs in the worker, and a reload of yason would
;;; undo it.  The encoded text is fixed instead, by TRANSPORT-SAFE-JSON in
;;; src/utils/sanitize.lisp, on every path that sends it.
