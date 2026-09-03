;;;; src/cst.lisp

(defpackage #:cl-mcp/src/cst
  (:use #:cl)
  (:import-from #:eclector.parse-result
                #:parse-result-client
                #:make-expression-result
                #:make-skipped-input-result)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-file-package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages)
  (:export #:cst-node
           #:cst-node-kind
           #:cst-node-value
           #:cst-node-children
           #:cst-node-start
           #:cst-node-end
           #:cst-node-start-line
           #:cst-node-end-line
           #:parse-top-level-forms
           #:unterminated-source
           #:stray-right-parenthesis
           ;; Exported for cl-mcp/src/lisp-macroexpand, which has to answer
           ;; the same question this file answers while parsing: which
           ;; IN-PACKAGE is in effect at a given point in the file.
           #:%in-package-form-p))

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

(defun %in-package-form-p (form)
  "Return the package designator string if FORM is an IN-PACKAGE form, NIL otherwise."
  (and (consp form)
       (let ((head (car form)))
         (and (symbolp head)
              (string= (symbol-name head) "IN-PACKAGE")
              (consp (cdr form))
              (let ((designator (second form)))
                (cond ((stringp designator) designator)
                      ((symbolp designator) (symbol-name designator))
                      (t nil)))))))

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
Returns two values: the complete list of nodes (including previously collected
NODES) and the read error that stopped reading early, or NIL when the stream
was consumed to its end. A \")\" met where a form should start is reported as
STRAY-RIGHT-PARENTHESIS without consulting the implementation's reader, so the
classification does not depend on any implementation's error wording.
Reading stays lenient so a partially broken file can still be displayed, but
callers that need to know the file is unparseable (lisp-edit-form, the
fs-write-file overwrite guard) inspect the second value."
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
        (when (eql (peek-char nil stream nil :eof) #\))
          (return (values (nreverse nodes)
                          (make-condition
                           'stray-right-parenthesis
                           :stream stream
                           :message (format nil "Unmatched closing parenthesis ~
character ). Remove the extra \")\" (lisp-check-parens reports its line and column).")))))
        (let ((form (handler-case (read stream nil :eof)
                      (error (e)
                        ;; On read error, return what we have so far and
                        ;; report the error as the second value.
                        (return (values (nreverse nodes) e))))))
          (when (eq form :eof)
            (return (values (nreverse nodes) nil)))
          (let* ((end-pos (file-position stream))
                 (node (make-cst-node :kind :expr
                                      :value form
                                      :children nil
                                      :start start-pos
                                      :end end-pos
                                      :start-line (%pos->line start-pos)
                                      :end-line (%pos->line end-pos))))
            ;; Track in-package to activate local-nicknames
            (let ((pkg-name (%in-package-form-p form)))
              (when pkg-name
                (let ((pkg (find-package pkg-name)))
                  (when pkg
                    (setf *package* pkg)))))
            (push node nodes)))))))

;; Eclector's MAKE-SKIPPED-INPUT-RESULT generic function changed shape
;; between releases:
;;   - Eclector <= 0.10 (and the Feb-2024 release vendored under
;;     local-projects): (client stream reason source)           -- 4 args
;;   - Eclector >= 0.11 (Quicklisp 2024-10-12 and later):
;;     (client stream reason children source)                   -- 5 args
;;
;; Rather than committing to one API and breaking the other, we inspect
;; the installed generic function at compile time and push the feature
;; :CL-MCP-ECLECTOR-SKIPPED-HAS-CHILDREN when the 5-argument signature is
;; in effect. The reader conditional below then emits the matching
;; DEFMETHOD. Note that we push the feature inside EVAL-WHEN with
;; :COMPILE-TOPLEVEL so the reader sees it on the *next* top-level form
;; in the same file.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((gf (fdefinition 'make-skipped-input-result))
         (ll (#+sbcl sb-mop:generic-function-lambda-list
              #+ccl ccl:generic-function-lambda-list
              #+(or ecl clasp) clos:generic-function-lambda-list
              #-(or sbcl ccl ecl clasp) closer-mop:generic-function-lambda-list
              gf))
         (required (loop for p in ll
                         until (and (symbolp p)
                                    (member p lambda-list-keywords))
                         count 1)))
    (cond
      ((= required 5)
       (pushnew :cl-mcp-eclector-skipped-has-children *features*))
      ((= required 4)
       (setf *features* (remove :cl-mcp-eclector-skipped-has-children *features*)))
      (t
       (warn "Unexpected arity ~D for ECLECTOR.PARSE-RESULT:MAKE-SKIPPED-INPUT-RESULT; ~
              cl-mcp/src/cst assumes 4 or 5 required arguments."
             required)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod make-expression-result ((client parse-result-client)
                                     result children source)
    (declare (ignore client))
    (destructuring-bind (start . end) source
      (make-cst-node :kind :expr
                     :value result
                     :children children
                     :start start
                     :end end
                     :start-line (%pos->line start)
                     :end-line (%pos->line end))))

  #+cl-mcp-eclector-skipped-has-children
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
                     :end-line (%pos->line end))))

  #-cl-mcp-eclector-skipped-has-children
  (defmethod make-skipped-input-result ((client parse-result-client)
                                        stream reason source)
    (declare (ignore client stream))
    (destructuring-bind (start . end) source
      (make-cst-node :kind :skipped
                     :value reason
                     :children nil
                     :start start
                     :end end
                     :start-line (%pos->line start)
                     :end-line (%pos->line end)))))

(define-condition unterminated-source (end-of-file)
  ((message :initarg :message :reader unterminated-source-message))
  (:report (lambda (c s) (write-string (unterminated-source-message c) s)))
  (:documentation "Signaled when the reader hit end of input inside a form.
Carries the same explanatory message as other reader errors but stays a
subtype of END-OF-FILE, so callers that treat a premature end of input
specially (lisp-read-file's unbalanced-parentheses hint) keep working."))

(define-condition stray-right-parenthesis (reader-error)
  ((message :initarg :message :reader stray-right-parenthesis-message))
  (:report (lambda (c s) (write-string (stray-right-parenthesis-message c) s)))
  (:documentation "Signaled when the reader met a \")\" with no open form.
Like UNTERMINATED-SOURCE this keeps the failure identifiable as a delimiter
problem, which no readtable can fix, so the fs-write-file overwrite guard can
tell it apart from reader-macro failures that a readtable might resolve."))

(defun %parse-top-level-forms-core (text readtable)
  "Parse TEXT into CST nodes assuming *PACKAGE* and *LINE-TABLE* are already bound."
  (if readtable
      (let ((custom-rt (%try-switch-readtable readtable)))
        (if custom-rt
            (call-with-lenient-packages
             (lambda ()
               (with-input-from-string (stream text)
                 (%read-remaining-with-cl-reader stream nil custom-rt))))
            (error "Readtable ~S not found." readtable)))
      (let ((*readtable* (copy-readtable))
            (*read-eval* nil)
            (nodes '())
            (client (make-instance 'parse-result-client)))
        (with-input-from-string (stream text)
          (handler-case
              (call-with-lenient-packages
               (lambda ()
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
                       (let ((pkg-name (%in-package-form-p
                                        (cst-node-value result))))
                         (when pkg-name
                           (let ((pkg (find-package pkg-name)))
                             (when pkg
                               (setf *package* pkg)))))
                       (let ((designator
                               (%in-readtable-form-p (cst-node-value result))))
                         (when designator
                           (let ((custom-rt (%try-switch-readtable designator)))
                             (when custom-rt
                               (return
                                 (%read-remaining-with-cl-reader
                                  stream nodes custom-rt)))))))))))
            (reader-error (e)
              (let ((msg (format nil "~A" e)))
                (cond
                  ((search "READ-EVAL" msg)
                   (error
                    "Reader error: ~A~%~%Read-time evaluation (#.) is disabled for security. ~
If you need to parse files containing #., consider removing or replacing the #. forms, ~
or use a separate evaluation step."
                    e))
                  ((typep e 'eclector.reader:invalid-context-for-right-parenthesis)
                   ;; A ")" with no open form: a delimiter failure no
                   ;; readtable can fix, kept identifiable for the overwrite
                   ;; guard in lisp-edit-form-core.
                   (error 'stray-right-parenthesis
                          :stream stream
                          :message (format nil "Reader error: ~A~%~%Remove the extra \")\" ~
(lisp-check-parens reports its line and column)."
                                           e)))
                  ((typep e 'end-of-file)
                   ;; Keep the END-OF-FILE type: lisp-read-file turns it into
                   ;; an unbalanced-parentheses hint.
                   (error 'unterminated-source
                          :stream stream
                          :message (format nil "Reader error: ~A~%~%If this file uses custom ~
reader macros (e.g., cl-interpol's #?), specify the 'readtable' parameter with the ~
named-readtable designator (e.g., readtable: \"interpol-syntax\")."
                                           e)))
                  (t
                   (error
                    "Reader error: ~A~%~%If this file uses custom reader macros ~
(e.g., cl-interpol's #?), specify the 'readtable' parameter with the ~
named-readtable designator (e.g., readtable: \"interpol-syntax\")."
                    e))))))))))

(defun parse-top-level-forms (text &key readtable source-path initial-package)
  "Parse TEXT into CST-NODE values. When READTABLE is provided, use that named readtable.
Unknown package-qualified symbols are handled leniently by creating ephemeral
stub packages that are cleaned up after parsing.
When an IN-PACKAGE form is encountered, *PACKAGE* is updated so that
package-local-nicknames activate for subsequent forms.
Returns a second value: the read error that stopped the lenient CL-reader
pass early (after an IN-READTABLE switch or when READTABLE is given), or NIL.
In that mode a malformed later form does not signal; the nodes read so far
are returned and the error is reported here instead."
  (let ((*line-table* (%build-line-table text))
        (*package* *package*))
    (cond
      (initial-package
       (call-with-package-context
        initial-package
        (lambda ()
          (%parse-top-level-forms-core text readtable))
        :source-path source-path))
      (source-path
       (call-with-file-package-context
        source-path
        (lambda ()
          (%parse-top-level-forms-core text readtable))
        :text text))
      (t
       (%parse-top-level-forms-core text readtable)))))
