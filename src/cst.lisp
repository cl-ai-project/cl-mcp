;;;; src/cst.lisp

(defpackage #:cl-mcp/src/cst
  (:use #:cl)
  (:import-from #:eclector.parse-result
                #:parse-result-client
                #:make-expression-result
                #:make-skipped-input-result)
  (:export #:cst-node
           #:cst-node-kind
           #:cst-node-value
           #:cst-node-children
           #:cst-node-start
           #:cst-node-end
           #:cst-node-start-line
           #:cst-node-end-line
           #:parse-top-level-forms))

(in-package #:cl-mcp/src/cst)

(defstruct cst-node
  kind
  value
  children
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (start-line 1 :type fixnum)
  (end-line 1 :type fixnum))

(declaim (type (simple-array fixnum (*)) *line-table*))
(defvar *line-table* (make-array 1 :element-type 'fixnum :initial-element 1))

(defun %build-line-table (text)
  "Return a vector mapping character offsets in TEXT to 1-based line numbers."
  (let* ((len (length text))
         (table (make-array (1+ len) :element-type 'fixnum))
         (line 1))
    (loop for i fixnum from 0 below len
          for ch = (char text i) do
            (setf (aref table i) line)
            (when (char= ch #\Newline)
              (incf line)))
    (setf (aref table len) line)
    table))

(defun %pos->line (pos)
  (aref *line-table* (min pos (1- (length *line-table*)))))

(defun %in-readtable-form-p (form)
  "Return the readtable designator if FORM is an IN-READTABLE form, NIL otherwise."
  (and (consp form)
       (let ((head (car form)))
         (and (symbolp head)
              (string= (symbol-name head) "IN-READTABLE")
              (consp (cdr form))
              (second form)))))

(defun %try-switch-readtable (designator)
  "Try to get the named readtable for DESIGNATOR.
Returns the readtable if found, NIL if named-readtables is not loaded
or the readtable is not found."
  (let ((pkg (or (find-package :named-readtables)
                 (find-package :editor-hints.named-readtables))))
    (when pkg
      (let ((find-fn (find-symbol "FIND-READTABLE" pkg)))
        (when (and find-fn (fboundp find-fn))
          (funcall find-fn designator))))))

(defun %read-remaining-with-cl-reader (stream nodes custom-readtable)
  "Read remaining forms from STREAM using standard CL reader with CUSTOM-READTABLE.
Returns the complete list of nodes (including previously collected NODES)."
  (let ((*readtable* custom-readtable)
        (*read-eval* nil))
    (loop
      (let ((start-pos (file-position stream)))
        ;; Skip whitespace
        (loop for ch = (peek-char nil stream nil :eof)
              while (and (characterp ch)
                         (member ch '(#\Space #\Tab #\Newline #\Return)))
              do (read-char stream))
        (setf start-pos (file-position stream))
        (let ((form (handler-case (read stream nil :eof)
                      (error (e)
                        ;; On read error, return what we have so far
                        (declare (ignore e))
                        (return (nreverse nodes))))))
          (when (eq form :eof)
            (return (nreverse nodes)))
          (let* ((end-pos (file-position stream))
                 (node (make-cst-node :kind :expr
                                      :value form
                                      :children nil
                                      :start start-pos
                                      :end end-pos
                                      :start-line (%pos->line start-pos)
                                      :end-line (%pos->line end-pos))))
            (push node nodes)))))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod make-expression-result ((client parse-result-client)
                                     result children source)
    (declare (ignore client children))
    (destructuring-bind (start . end) source
      (make-cst-node :kind :expr
                     :value result
                     :children children
                     :start start
                     :end end
                     :start-line (%pos->line start)
                     :end-line (%pos->line end))))

  (defmethod make-skipped-input-result ((client parse-result-client)
                                        stream reason children source)
    (declare (ignore client stream children))
    (destructuring-bind (start . end) source
      (make-cst-node :kind :skipped
                     :value reason
                     :children nil
                     :start start
                     :end end
                     :start-line (%pos->line start)
                     :end-line (%pos->line end)))))

(defun parse-top-level-forms (text &key readtable)
  "Parse TEXT into CST-NODE values. When READTABLE is provided, use that named readtable."
  (let ((*line-table* (%build-line-table text)))
    (if readtable
        (let ((custom-rt (%try-switch-readtable readtable)))
          (if custom-rt
              (with-input-from-string (stream text)
                (%read-remaining-with-cl-reader stream nil custom-rt))
              (error "Readtable ~S not found." readtable)))
        (let ((*readtable* (copy-readtable))
              (*read-eval* nil)
              (nodes '())
              (client (make-instance 'parse-result-client)))
          (with-input-from-string (stream text)
            (handler-case
                (loop
                  (multiple-value-bind (result orphan-results)
                      (eclector.parse-result:read client stream nil :eof)
                    (dolist (orphan orphan-results)
                      (push orphan nodes))
                    (when (eq result :eof)
                      (return (nreverse nodes)))
                    (push result nodes)
                    (when (and (typep result 'cst-node)
                               (eq (cst-node-kind result) :expr))
                      (let ((designator (%in-readtable-form-p (cst-node-value result))))
                        (when designator
                          (let ((custom-rt (%try-switch-readtable designator)))
                            (when custom-rt
                              (return (%read-remaining-with-cl-reader
                                       stream nodes custom-rt)))))))))
              (reader-error (e)
                (let ((msg (format nil "~A" e)))
                  (if (search "READ-EVAL" msg)
                      ;; #. read-time evaluation error
                      (error "Reader error: ~A~%~%Read-time evaluation (#.) is disabled for security. ~
                              If you need to parse files containing #., consider removing or replacing ~
                              the #. forms, or use a separate evaluation step."
                             e)
                      ;; Other reader errors (unknown reader macros, etc.)
                      (error "Reader error: ~A~%~%If this file uses custom reader macros (e.g., cl-interpol's #?), ~
                              specify the 'readtable' parameter with the named-readtable designator ~
                              (e.g., readtable: \"interpol-syntax\")."
                             e))))))))))
