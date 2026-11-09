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

;;; 0.7.x escapes only \b \f \n \r \t and writes the other characters below
;;; U+0020 raw, which no JSON parser accepts (RFC 8259 section 7): one NUL in a
;;; repl-eval result_preview made the client drop the connection (#342).
;;; Changed only when yason writes one raw, so 0.8's own escaping stays as is.
(let ((table (let ((symbol (find-symbol "*CHAR-REPLACEMENTS*" '#:yason)))
               (and symbol (boundp symbol) (symbol-value symbol)))))
  (when (and (hash-table-p table)
             (find (code-char 0)
                   (with-output-to-string (s)
                     (yason:encode (string (code-char 0)) s))))
    (loop for code below #x20
          for char = (code-char code)
          unless (gethash char table)
            do (setf (gethash char table) (format nil "\\u~4,'0X" code)))))
