;;;; src/lisp-read-file.lisp

(defpackage #:cl-mcp/src/lisp-read-file
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-resolve-read-path)
  (:import-from #:cl-ppcre
                #:scan
                #:create-scanner)
  (:import-from #:uiop
                #:ensure-pathname
                #:pathname-type
                #:native-namestring)
  (:export #:lisp-read-file
           #:lisp-source-path-p))

(in-package #:cl-mcp/src/lisp-read-file)

(defparameter *lisp-source-extensions*
  '("lisp" "lsp" "cl" "asd" "ros")
  "File extensions treated as Lisp source.")

(defparameter *default-line-limit* 2000
  "Default maximum number of lines to return when LIMIT is not supplied.")

(defparameter *text-context-lines* 5
  "Number of surrounding lines to include for text filtering with patterns.")

(defun lisp-source-path-p (path)
  "Return T when PATH designator refers to a Lisp source file by extension."
  (let* ((pn (uiop:ensure-pathname path :want-relative nil))
         (type (pathname-type pn)))
    (and type (member (string-downcase type) *lisp-source-extensions*
                      :test #'string=))))

(defun %compile-scanner (pattern)
  (when pattern
    (unless (stringp pattern)
      (error "Pattern must be a string or NIL"))
    (create-scanner pattern)))

(defun %normalize-path (pathname)
  (uiop:native-namestring pathname))

(defun %docstring-first-line (string)
  (when (stringp string)
    (let* ((pos (or (position #\Newline string) (length string)))
           (slice (subseq string 0 pos)))
      (string-trim '(#\Space #\Tab) slice))))

(defun %truncate-doc (docstring)
  (let* ((line (%docstring-first-line docstring)))
    (when line
      (if (> (length line) 80)
          (concatenate 'string (subseq line 0 77) "...")
          line))))

(defun %definition-names (form)
  "Return a list of stringified definition names for FORM, when applicable."
  (when (consp form)
    (let* ((head (car form))
           (name (second form)))
      (when (member head '(defun defmacro defvar defparameter defconstant defclass
                           defstruct defgeneric defmethod defpackage))
        (list (string-downcase (prin1-to-string name)))))))

(defun %form->string (form)
  (let ((*print-pretty* t)
        (*print-case* :downcase)
        (*print-right-margin* 80))
    (with-output-to-string (out)
      (write form :stream out :pretty t :right-margin 80))))

(defun %collapse-def-form (form)
  (let* ((*print-case* :downcase)
         (head (car form))
         (name (second form))
         (args (case head
                 ((defmethod)
                  (or (find-if #'listp (cddr form))
                      (third form)))
                 (t (third form))))
         (args-display (if args
                           (with-output-to-string (out)
                             (write args :stream out :pretty nil :case :downcase))
                           "()"))
         (doc (%truncate-doc (find-if #'stringp (cddr form)))))
    (format nil "(~(~A~) ~(~A~) ~A ...~@[ ;; ~A~])"
            head
            (if name name "")
            args-display
            doc)))

(defun %collapse-generic (form)
  (cond
    ((consp form)
     (format nil "(~(~A~) ...)" (car form)))
    (t (prin1-to-string form))))

(defun %format-lisp-form (form name-scanner content-scanner)
  "Return two values: display string and whether FORM was expanded."
  (let* ((head (and (consp form) (car form)))
         (names (%definition-names form))
         (full-string nil)
         (name-match (and name-scanner
                          (some (lambda (n) (scan name-scanner n)) names)))
         (content-match (and content-scanner
                             (let ((repr (or full-string
                                             (setf full-string (%form->string form)))))
                               (scan content-scanner repr))))
         (expand? (or name-match content-match (eq head 'in-package))))
    (values (cond
              (expand? (or full-string (%form->string form)))
              ((member head '(defun defmacro defvar defparameter defconstant defclass
                               defstruct defgeneric defmethod))
               (%collapse-def-form form))
              ((eq head 'in-package)
               (%form->string form))
              (t (%collapse-generic form)))
            expand?)))

(defun %read-top-level-forms (text)
  "Return a list of top-level forms read from TEXT.
Reader evaluation is disabled for safety."
  (let ((*readtable* (copy-readtable nil))
        (*read-eval* nil))
    (loop with pos = 0
          with forms = '()
          do (multiple-value-bind (form next) (read-from-string text nil :eof :start pos)
               (if (eq form :eof)
                   (return (nreverse forms))
                   (progn
                     (when (= next pos)
                       (error "Reader made no progress at position ~D" pos))
                     (push form forms)
                     (setf pos next)))))))

(defun %format-lisp-file (text name-scanner content-scanner)
  (let* ((forms (%read-top-level-forms text))
         (expanded 0)
         (display (with-output-to-string (out)
                    (dolist (form forms)
                      (multiple-value-bind (line expanded?) (%format-lisp-form form name-scanner content-scanner)
                        (when expanded? (incf expanded))
                        (write-string line out)
                        (terpri out))))))
    (values display
            (let ((meta (make-hash-table :test #'equal)))
              (setf (gethash "total_forms" meta) (length forms)
                    (gethash "expanded_forms" meta) expanded
                    (gethash "truncated" meta) nil)
              meta))))

(defun %read-lines-slice (pathname offset limit)
  "Return three values: sliced text, truncated?, and total line count."
  (with-open-file (in pathname :direction :input :element-type 'character)
    (let ((lines '())
          (count 0)
          (line-idx 0)
          (hit-limit nil))
      (loop for line = (read-line in nil :eof)
            until (eq line :eof) do
              (when (>= line-idx offset)
                (when (or (null limit) (< count limit))
                  (push line lines)
                  (incf count))
                (when (and limit (>= count limit))
                  (setf hit-limit t)
                  (return)))
              (incf line-idx)
              )
      (when hit-limit
        (loop for line = (read-line in nil :eof)
              until (eq line :eof) do (incf line-idx)))
      (let ((total line-idx)
            (truncated (or (> offset 0)
                           (and limit hit-limit))))
        (values (format nil "~{~A~%~}" (nreverse lines))
                truncated
                total)))))

(defun %text-filter-with-context (pathname scanner limit)
  "Return two values: filtered text and truncated flag."
  (with-open-file (in pathname :direction :input :element-type 'character)
    (let ((lines (loop for line = (read-line in nil :eof)
                       until (eq line :eof)
                       collect line)))
      (let* ((len (length lines))
             (context *text-context-lines*)
             (selected '())
             (seen (make-hash-table :test #'eql))
             (truncated nil))
        (loop for idx from 0 below len
              for line in lines do
                (when (and (not truncated) (scan scanner line))
                  (loop for j from (max 0 (- idx context))
                        below (min len (+ idx context 1)) do
                          (unless (gethash j seen)
                            (setf (gethash j seen) t)
                            (push (format nil "~4D: ~A" (1+ j) (nth j lines)) selected)
                            (when (and limit (>= (length selected) limit))
                              (setf truncated t)
                              (return))))))
        (values (format nil "~{~A~%~}" (nreverse selected)) truncated)))))

(defun lisp-read-file (path &key (collapsed t) name-pattern content-pattern offset limit)
  "Read PATH with Lisp-aware collapsed formatting.
Returns a hash-table payload with keys \"content\", \"path\", \"mode\", and \"meta\"."
  (unless (stringp path)
    (error "path must be a string"))
  (unless (member collapsed '(t nil))
    (error "collapsed must be boolean"))
  (when (and offset (not (integerp offset)))
    (error "offset must be an integer when provided"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (not (integerp limit)))
    (error "limit must be an integer when provided"))
  (let* ((resolved (fs-resolve-read-path path))
         (mode nil)
         (line-limit (or limit *default-line-limit*))
         (name-scanner (%compile-scanner name-pattern))
         (content-scanner (%compile-scanner content-pattern)))
    (multiple-value-bind (content meta)
        (cond
          ((and collapsed (lisp-source-path-p resolved))
           (let ((text (fs-read-file resolved)))
             (multiple-value-bind (display meta-table)
                 (%format-lisp-file text name-scanner content-scanner)
               (setf mode "lisp-collapsed")
               (values display meta-table))))
          ((not collapsed)
           (multiple-value-bind (text truncated total)
               (%read-lines-slice resolved (or offset 0) line-limit)
             (let ((meta (make-hash-table :test #'equal)))
               (setf mode "raw"
                     (gethash "truncated" meta) truncated
                     (gethash "total_lines" meta) total)
               (values text meta))))
          ((and content-scanner (not (lisp-source-path-p resolved)))
           (multiple-value-bind (text truncated)
               (%text-filter-with-context resolved content-scanner line-limit)
             (let ((meta (make-hash-table :test #'equal)))
               (setf mode "text-filtered"
                     (gethash "truncated" meta) truncated)
               (values text meta))))
          (t
           (multiple-value-bind (text truncated total)
               (%read-lines-slice resolved 0 line-limit)
             (let ((meta (make-hash-table :test #'equal)))
               (setf mode (if (lisp-source-path-p resolved)
                              "lisp-snippet"
                              "text-snippet")
                     (gethash "truncated" meta) truncated
                     (gethash "total_lines" meta) total)
               (values text meta)))))
      (let ((payload (make-hash-table :test #'equal)))
        (setf (gethash "content" payload) content
              (gethash "path" payload) (%normalize-path resolved)
              (gethash "mode" payload) mode
              (gethash "meta" payload) meta)
        payload))))
