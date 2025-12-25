;;;; src/lisp-read-file.lisp

(defpackage #:cl-mcp/src/lisp-read-file
  (:use #:cl)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
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
  (let ((line (%docstring-first-line docstring)))
    (when line
      (if (> (length line) 80)
          (concatenate 'string (subseq line 0 77) "...")
          line))))

(defun %definition-names (form)
  "Return a list of stringified definition names for FORM, when applicable."
  (when (consp form)
    (let ((head (car form))
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
                 (otherwise (third form))))
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

(defun %comment-node-p (node)
  (and (typep node 'cst-node)
       (eq (cst-node-kind node) :skipped)
       (let ((reason (cst-node-value node)))
         (or (eq reason :block-comment)
             (eq reason :s-expression-comment)
             (and (consp reason) (eq (car reason) :line-comment))))))

(defun %comment-text (node text)
  (subseq text (cst-node-start node) (cst-node-end node)))

(defun %ensure-trailing-newline (string)
  (if (and (> (length string) 0)
           (char= (char string (1- (length string))) #\Newline))
      string
      (concatenate 'string string (string #\Newline))))

(defun %line-number-width (line-count)
  (max 1 (length (write-to-string line-count))))

(defun %add-line-numbers (text start-line width)
  (with-output-to-string (out)
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil nil)
            while line
            for idx from start-line do
              (format out "~VD: ~A~%" width idx line)))))

(defun %format-lisp-form (node name-scanner content-scanner line-width)
  "Return two values: display string and whether NODE was expanded."
  (let* ((form (cst-node-value node))
         (head (and (consp form) (car form)))
         (names (%definition-names form))
         (full-string nil)
         (name-match (and name-scanner
                          (some (lambda (n) (scan name-scanner n)) names)))
         (content-match (and content-scanner
                             (let ((repr (or full-string
                                             (setf full-string (%form->string form)))))
                               (scan content-scanner repr))))
         (expand? (or name-match content-match (eq head 'in-package)))
         (display (cond
                    (expand?
                     (%add-line-numbers (or full-string (%form->string form))
                                        (cst-node-start-line node)
                                        line-width))
                    ((member head '(defun defmacro defvar defparameter defconstant defclass
                                     defstruct defgeneric defmethod))
                     (%collapse-def-form form))
                    ((eq head 'in-package)
                     (%form->string form))
                    (t (%collapse-generic form)))))
    (values display expand?)))

(defun %line-stats (text)
  "Return three values: source lines, comment lines, and blank lines for TEXT."
  (let ((source 0)
        (comment 0)
        (blank 0))
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil nil)
            while line do
              (incf source)
              (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line))
                     (empty? (string= trimmed "")))
                (cond
                  (empty? (incf blank))
                  ((and (> (length trimmed) 0)
                        (or (char= (char trimmed 0) #\;)
                            (and (>= (length trimmed) 2)
                                 (char= (char trimmed 0) #\#)
                                 (char= (char trimmed 1) #\|))
                            (and (>= (length trimmed) 2)
                                 (char= (char trimmed 0) #\|)
                                 (char= (char trimmed 1) #\#))))
                   (incf comment))))))
    (values source comment blank)))

(defun %format-lisp-file (text name-scanner content-scanner include-comments comment-context)
  (multiple-value-bind (source-lines-count comment-lines blank-lines)
      (%line-stats text)
    (let* ((nodes (parse-top-level-forms text))
           (line-width (%line-number-width source-lines-count))
           (expanded 0)
           (total-forms 0)
           (display (with-output-to-string (out)
                      (let ((pending-comments '()))
                        (dolist (node nodes)
                          (cond
                            ((and include-comments (%comment-node-p node))
                             (let ((comment (%ensure-trailing-newline
                                             (%comment-text node text))))
                               (cond
                                 ((string= comment-context "all")
                                  (write-string comment out))
                                 ((string= comment-context "preceding")
                                  (push comment pending-comments)))))
                            ((and (typep node 'cst-node)
                                  (eq (cst-node-kind node) :expr))
                             (incf total-forms)
                             (when (and include-comments pending-comments)
                               (dolist (comment (nreverse pending-comments))
                                 (write-string comment out))
                               (setf pending-comments '()))
                             (multiple-value-bind (line expanded?)
                                 (%format-lisp-form node name-scanner content-scanner line-width)
                               (when expanded? (incf expanded))
                               (write-string (%ensure-trailing-newline line) out)))
                            (t
                             (setf pending-comments '()))))
                        (when (and include-comments
                                   (string/= comment-context "none")
                                   pending-comments)
                          (dolist (comment (nreverse pending-comments))
                            (write-string comment out)))))))
      (values display
              (let ((meta (make-hash-table :test #'equal)))
                (setf (gethash "total_forms" meta) total-forms
                      (gethash "expanded_forms" meta) expanded
                      (gethash "truncated" meta) nil
                      (gethash "comment_lines" meta) comment-lines
                      (gethash "blank_lines" meta) blank-lines
                      (gethash "source_lines" meta) source-lines-count)
                meta)))))

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
      (let ((len (length lines))
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

(defun %lisp-read-file-content (resolved collapsed name-scanner content-scanner offset line-limit
                                 include-comments comment-context)
  (cond
    ((and collapsed (lisp-source-path-p resolved))
     (let ((text (fs-read-file resolved)))
       (multiple-value-bind (display meta-table)
           (%format-lisp-file text
                              name-scanner
                              content-scanner
                              include-comments
                              comment-context)
         (values display meta-table "lisp-collapsed"))))
    ((not collapsed)
     (multiple-value-bind (text truncated total)
         (%read-lines-slice resolved (or offset 0) line-limit)
       (let ((meta (make-hash-table :test #'equal)))
         (setf (gethash "truncated" meta) truncated
               (gethash "total_lines" meta) total)
         (values text meta "raw"))))
    ((and content-scanner (not (lisp-source-path-p resolved)))
     (multiple-value-bind (text truncated)
         (%text-filter-with-context resolved content-scanner line-limit)
       (let ((meta (make-hash-table :test #'equal)))
         (setf (gethash "truncated" meta) truncated)
         (values text meta "text-filtered"))))
    (t
     (multiple-value-bind (text truncated total)
         (%read-lines-slice resolved 0 line-limit)
       (let ((meta (make-hash-table :test #'equal)))
         (setf (gethash "truncated" meta) truncated
               (gethash "total_lines" meta) total)
         (values text
                 meta
                 (if (lisp-source-path-p resolved)
                     "lisp-snippet"
                     "text-snippet")))))))
(defun lisp-read-file (path &key (collapsed t) name-pattern content-pattern offset limit
                             (include-comments nil) (comment-context "preceding"))
  "Read PATH with Lisp-aware collapsed formatting.
When COLLAPSED is true, NAME-PATTERN or CONTENT-PATTERN expand matching forms,
and INCLUDE-COMMENTS controls whether preceding or all comments are kept based
on COMMENT-CONTEXT. Returns a hash-table payload with keys \"content\", \"path\",
\"mode\", and \"meta\"."
  (unless (stringp path)
    (error "path must be a string"))
  (unless (member collapsed '(t nil))
    (error "collapsed must be boolean"))
  (unless (member include-comments '(t nil))
    (error "include-comments must be boolean"))
  (unless (member comment-context '("preceding" "all" "none") :test #'string=)
    (error "comment-context must be \"preceding\", \"all\", or \"none\""))
  (when (and offset (not (integerp offset)))
    (error "offset must be an integer when provided"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (not (integerp limit)))
    (error "limit must be an integer when provided"))
  (let ((resolved (fs-resolve-read-path path))
        (line-limit (or limit *default-line-limit*))
        (name-scanner (%compile-scanner name-pattern))
        (content-scanner (%compile-scanner content-pattern)))
    (multiple-value-bind (content meta mode)
        (%lisp-read-file-content resolved
                                 collapsed
                                 name-scanner
                                 content-scanner
                                 offset
                                 line-limit
                                 include-comments
                                 comment-context)
      (let ((payload (make-hash-table :test #'equal)))
        (setf (gethash "content" payload) content
              (gethash "path" payload) (%normalize-path resolved)
              (gethash "mode" payload) mode
              (gethash "meta" payload) meta)
        payload))))

(define-tool "lisp-read-file"
  :description "Read a file with Lisp-aware collapsed view to save context window tokens.
ALWAYS prefer this tool over 'fs-read-file' when reading .lisp or .asd files,
unless you need exact raw bytes.
Use 'name_pattern' to locate specific definitions (e.g., functions, classes)
without reading the entire file.
Use 'collapsed=true' (default) to see only signatures, or 'collapsed=false'
for full source."
  :args ((path :type :string :required t
               :description "Path to read; absolute inside project or registered ASDF system,
or relative to project root")
         (collapsed :type :boolean :default t
                    :description "When true (default) collapse Lisp definitions to signatures")
         (name-pattern :type :string
                       :description "Regex to match definition names to expand (CL-PPCRE syntax)")
         (content-pattern :type :string
                          :description "Regex to match form bodies or text lines to expand")
         (offset :type :integer
                 :description "0-based line offset when collapsed=false (raw mode only)")
         (limit :type :integer
                :description "Maximum lines to return; defaults to 2000"))
  :body
  (let ((file-result
         (lisp-read-file path
                         :collapsed collapsed
                         :name-pattern name-pattern
                         :content-pattern content-pattern
                         :offset offset
                         :limit limit)))
    (result id
            (make-ht "content" (text-content (gethash "content" file-result))
                     "text" (gethash "content" file-result)
                     "path" (gethash "path" file-result)
                     "mode" (gethash "mode" file-result)
                     "meta" (gethash "meta" file-result)))))
