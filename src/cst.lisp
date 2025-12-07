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

(defstruct (cst-node
             (:documentation "Concrete syntax tree node wrapping Eclector parse results with offsets and line numbers."))
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

(defun parse-top-level-forms (text)
  "Parse TEXT into a list of CST-NODE values with offset and line information.
Reader evaluation is disabled for safety."
  (let ((*readtable* (copy-readtable nil))
        (*read-eval* nil)
        (*line-table* (%build-line-table text))
        (nodes '())
        (client (make-instance 'parse-result-client)))
    (with-input-from-string (stream text)
      (loop
        (multiple-value-bind (result orphan-results)
            (eclector.parse-result:read client stream nil :eof)
          (dolist (orphan orphan-results)
            (push orphan nodes))
          (when (eq result :eof)
            (return (nreverse nodes)))
          (push result nodes))))))
