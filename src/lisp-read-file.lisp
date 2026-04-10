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
                #:make-ht #:result #:text-content #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:*homeless-due-to-teardown*)
  (:import-from #:cl-mcp/src/utils/paths
                #:normalize-path-for-display)
  (:import-from #:cl-mcp/src/utils/strings
                #:ensure-trailing-newline)
  (:import-from #:cl-ppcre
                #:scan
                #:create-scanner)
  (:import-from #:uiop
                #:ensure-pathname
                #:pathname-type)
  (:export #:lisp-read-file
           #:lisp-source-path-p))

(in-package #:cl-mcp/src/lisp-read-file)

(defparameter *lisp-source-extensions*
  '("lisp" "lsp" "cl" "asd" "ros")
  "File extensions treated as Lisp source.")

(defparameter *default-line-limit* 500
  "Default maximum number of lines to return when LIMIT is not supplied.")

(defparameter *text-context-lines* 5
  "Number of surrounding lines to include for text filtering with patterns.")

(defun %print-homeless-symbol-name (stream name)
  "Write NAME to STREAM with case conversion honoring *PRINT-CASE*."
  (write-string (ecase *print-case*
                  (:downcase (string-downcase name))
                  (:upcase name)
                  (:capitalize (string-capitalize name)))
                stream))

(defun %print-source-symbol (stream sym)
  "Pprint-dispatch handler for symbols in lisp-read-file output.

A symbol with no home package is either:
  (a) genuinely uninterned in the source — e.g. written as `#:foo'.
      In that case it is NOT in *HOMELESS-DUE-TO-TEARDOWN*, and we
      preserve the `#:' prefix so the display remains a faithful
      reproduction of the source.
  (b) homeless only because a synthesized package was torn down
      after reading. In that case it IS in the teardown set, and we
      strip the `#:' so expanded views are readable.

Interned symbols (keywords, ordinary package symbols) fall through
to the default printer with *PRINT-CIRCLE* forced to NIL. Rebinding
*PRINT-CIRCLE* avoids a subtle interaction with the pretty printer
that would otherwise label shared symbol objects with `#1=#1#'
self-references when the outer tree is walked under *PRINT-PRETTY*."
  (cond
    ((and (symbolp sym) (null (symbol-package sym)))
     (unless (gethash sym *homeless-due-to-teardown*)
       (write-string "#:" stream))
     (%print-homeless-symbol-name stream (symbol-name sym)))
    (t
     (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
           (*print-circle* nil))
       (prin1 sym stream)))))

(defvar *source-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'symbol #'%print-source-symbol 0 table)
    table)
  "Pprint dispatch used by %FORM->STRING and %COLLAPSE-DEF-FORM so
homeless-due-to-teardown symbols render without `#:' while genuine
uninterned symbols retain their prefix. See %PRINT-SOURCE-SYMBOL.")

(defun lisp-source-path-p (path)
  "Return T when PATH designator refers to a Lisp source file by extension."
  (let* ((pn (uiop:ensure-pathname path :want-relative nil))
         (type (pathname-type pn)))
    (and type (member (string-downcase type) *lisp-source-extensions*
                      :test #'string=))))

(defun %compile-scanner (pattern param-name)
  "Compile PATTERN into a CL-PPCRE scanner.
Returns NIL for NIL or empty PATTERN.  Raises ARG-VALIDATION-ERROR when
PATTERN is a non-empty string that is not valid regex syntax."
  (when (and (stringp pattern) (plusp (length pattern)))
    (handler-case (create-scanner pattern)
      (ppcre:ppcre-syntax-error (e)
        (error 'arg-validation-error
               :arg-name param-name
               :message (format nil "invalid regex: ~A" e))))))

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
      (when (and (symbolp head)
                 (let ((n (symbol-name head)))
                   (or (and (>= (length n) 3) (string= n "DEF" :end1 3))
                       (and (>= (length n) 7) (string= n "DEFINE-" :end1 7)))))
        (list (string-downcase
               (if (symbolp name) (symbol-name name) (prin1-to-string name))))))))

(defun %form->string (form)
  "Return FORM printed as Lisp source text for display.
Uses *SOURCE-PPRINT-DISPATCH* so symbols made homeless by a
synthesized package's teardown print without the `#:' prefix, while
symbols that were genuinely uninterned in the source (e.g., `#:foo'
inside a `defpackage') retain their prefix.

Forces *PRINT-CIRCLE* to NIL because the CST reader may intern a
single symbol object once and share it across multiple positions in
the form tree. Under *PRINT-CIRCLE* T, that would cause the pretty
printer to label the first occurrence with `#1=' and emit `#1#' on
reuse — producing nonsense like `(in-package #1=#1#)' when the
shared symbol is the sole argument of a form."
  (let ((*print-pretty* t)
        (*print-case* :downcase)
        (*print-right-margin* 80)
        (*print-circle* nil)
        (*print-pprint-dispatch* *source-pprint-dispatch*))
    (with-output-to-string (out)
      (write form :stream out :pretty t :right-margin 80))))

(defun %collapse-def-form (form)
  "Collapse a definition form to a signature line.
For defmethod, includes qualifiers like :before, :after, :around.
Uses *SOURCE-PPRINT-DISPATCH* so homeless-due-to-teardown symbols
print without a spurious `#:' prefix while genuinely uninterned
source symbols keep theirs. Forces *PRINT-CIRCLE* to NIL to avoid
spurious `#1=#1#' self-references when the CST reader shares a
single symbol object across multiple positions in the form tree."
  (let* ((*print-case* :downcase)
         (*print-circle* nil)
         (*print-pprint-dispatch* *source-pprint-dispatch*)
         (head (car form))
         (name (second form))
         (qualifiers
          (when (eq head 'defmethod)
            (loop for part in (cddr form)
                  while (and part (symbolp part))
                  collect part)))
         (args
          (case head
            ((defmethod) (or (find-if #'listp (cddr form)) (third form)))
            (otherwise (third form))))
         (args-display
          (if args
              (with-output-to-string (out)
                (write args :stream out :pretty nil :case :downcase))
              "()"))
         (doc (%truncate-doc (find-if #'stringp (cddr form))))
         ;; Use ~S to preserve colon for keyword qualifiers
         (qual-str (when qualifiers (format nil "~{~(~S~)~^ ~}" qualifiers))))
    (if qual-str
        (format nil "(~(~A~) ~(~A~) ~A ~A ...~@[ ;; ~A~])" head name qual-str
                args-display doc)
        (format nil "(~(~A~) ~(~A~) ~A ...~@[ ;; ~A~])" head
                (if name
                    name
                    "")
                args-display doc))))

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
         (start-line (cst-node-start-line node))
         (display (cond
                    (expand?
                     (%add-line-numbers (or full-string (%form->string form))
                                        start-line line-width))
                    ((member head '(defun defmacro defvar defparameter defconstant defclass
                                     defstruct defgeneric defmethod))
                     (format nil "~VD: ~A" line-width start-line
                             (%collapse-def-form form)))
                    ((eq head 'in-package)
                     (%form->string form))
                    (t (format nil "~VD: ~A" line-width start-line
                               (%collapse-generic form))))))
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

(defun %format-lisp-file (text name-scanner content-scanner include-comments
                          comment-context &key readtable source-path)
  (multiple-value-bind (source-lines-count comment-lines blank-lines)
      (%line-stats text)
    (let* ((nodes (parse-top-level-forms text
                                         :readtable readtable
                                         :source-path source-path))
           (line-width (%line-number-width source-lines-count))
           (expanded 0)
           (total-forms 0)
           (*package* *package*)
           (display
            (with-output-to-string (out)
              (let ((pending-comments nil))
                (dolist (node nodes)
                  (cond
                    ((and include-comments (%comment-node-p node))
                     (let ((comment
                            (ensure-trailing-newline
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
                       (setf pending-comments nil))
                     (multiple-value-bind (line expanded?)
                         (%format-lisp-form node name-scanner
                                            content-scanner line-width)
                       (when expanded? (incf expanded))
                       (write-string (ensure-trailing-newline line) out))
                     ;; Track in-package to set *package* for correct
                     ;; symbol printing of subsequent forms.
                     (let* ((form (cst-node-value node))
                            (head (and (consp form) (car form))))
                       (when (and (symbolp head)
                                  (string= (symbol-name head) "IN-PACKAGE")
                                  (consp (cdr form)))
                         (let* ((designator (second form))
                                (pkg-name
                                 (cond ((stringp designator) designator)
                                       ((symbolp designator)
                                        (symbol-name designator)))))
                           (when pkg-name
                             (let ((pkg (find-package pkg-name)))
                               (when pkg
                                 (setf *package* pkg))))))))
                    (t (setf pending-comments nil))))
                (when (and include-comments
                           (string/= comment-context "none")
                           pending-comments)
                  (dolist (comment (nreverse pending-comments))
                    (write-string comment out)))))))
      (values display
              (let ((meta (make-hash-table :test #'equal)))
                (setf (gethash "total_forms" meta) total-forms
                      (gethash "expanded_forms" meta) expanded
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
              (incf line-idx))
      (when hit-limit
        (incf line-idx)
        (loop for line = (read-line in nil :eof)
              until (eq line :eof) do (incf line-idx)))
      (let ((total line-idx))
        (values (format nil "~{~A~%~}" (nreverse lines))
                total)))))

(defun %text-filter-with-context (pathname scanner limit)
  "Return two values: filtered text and truncated flag."
  (with-open-file (in pathname :direction :input :element-type 'character)
    (let ((lines (coerce (loop for line = (read-line in nil :eof)
                               until (eq line :eof)
                               collect line)
                         'vector)))
      (let ((len (length lines))
            (context *text-context-lines*)
            (selected '())
            (seen (make-hash-table :test #'eql))
            (truncated nil))
        (loop for idx from 0 below len
              for line = (aref lines idx) do
                (when (and (not truncated) (scan scanner line))
                  (loop for j from (max 0 (- idx context))
                        below (min len (+ idx context 1)) do
                          (unless (gethash j seen)
                            (setf (gethash j seen) t)
                            (push (format nil "~4D: ~A" (1+ j) (aref lines j)) selected)
                            (when (and limit (>= (length selected) limit))
                              (setf truncated t)
                              (return))))))
        (values (format nil "~{~A~%~}" (nreverse selected)) truncated)))))

(defun %lisp-read-file-content (resolved collapsed name-scanner content-scanner offset line-limit
                                 include-comments comment-context &key readtable)
  (cond
    ((and collapsed (lisp-source-path-p resolved))
     (let ((text (fs-read-file resolved)))
       (handler-case
           (multiple-value-bind (display meta-table)
               (%format-lisp-file text name-scanner content-scanner include-comments comment-context
                                  :source-path resolved
                                  :readtable readtable)
             (values display meta-table "lisp-collapsed"))
         (end-of-file ()
           (error "Unexpected end of file while parsing ~A; ~
                   check for unbalanced parentheses (use lisp-check-parens)"
                  (file-namestring resolved))))))
    ((not collapsed)
     (multiple-value-bind (text total)
         (%read-lines-slice resolved (or offset 0) line-limit)
       (let* ((meta (make-hash-table :test #'equal))
              (start-line (1+ (or offset 0)))
              (end-line (min (+ (or offset 0) line-limit) total))
              (footer (when (< end-line total)
                        (format nil "[Showing lines ~D-~D of ~D. ~
                                     Use offset=~D to read more.]~%"
                                start-line end-line total end-line)))
              (eof-msg (when (and (string= text "") (> (or offset 0) 0))
                         (format nil "[Offset ~D is past end of file (~D total line~:P).]~%"
                                 (or offset 0) total)))
              (content (cond
                         (footer (concatenate 'string text footer))
                         (eof-msg eof-msg)
                         (t text))))
         (setf (gethash "truncated" meta) (if footer t nil)
               (gethash "total_lines" meta) total)
         (values content meta "raw"))))
    ((and content-scanner (not (lisp-source-path-p resolved)))
     (multiple-value-bind (text truncated)
         (%text-filter-with-context resolved content-scanner line-limit)
       (let ((meta (make-hash-table :test #'equal)))
         (setf (gethash "truncated" meta) truncated)
         (values text meta "text-filtered"))))
    (t
     (multiple-value-bind (text total)
         (%read-lines-slice resolved (or offset 0) line-limit)
       (let* ((meta (make-hash-table :test #'equal))
              (start-line (1+ (or offset 0)))
              (end-line (min (+ (or offset 0) line-limit) total))
              (footer (when (< end-line total)
                        (format nil "[Showing lines ~D-~D of ~D. ~
                                     Use offset=~D to read more.]~%"
                                start-line end-line total end-line)))
              (content (if footer
                           (concatenate 'string text footer)
                           text)))
         (setf (gethash "truncated" meta) (if footer t nil)
               (gethash "total_lines" meta) total)
         (values content meta
                 (if (lisp-source-path-p resolved)
                     "lisp-snippet"
                     "text-snippet")))))))

(defun lisp-read-file (path &key (collapsed t) name-pattern content-pattern offset limit
                            (include-comments nil) (comment-context "preceding") readtable)
  "Read PATH with Lisp-aware collapsed formatting.
When COLLAPSED is true, NAME-PATTERN or CONTENT-PATTERN expand matching forms,
and INCLUDE-COMMENTS controls whether preceding or all comments are kept based
on COMMENT-CONTEXT.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing the file. This is useful for files that use custom reader macros
without an explicit IN-READTABLE form.

Returns a hash-table payload with keys \"content\", \"path\", \"mode\", and \"meta\"."
  (unless (stringp path)
    (error 'arg-validation-error :arg-name "path" :message "path must be a string"))
  (when (zerop (length path))
    (error 'arg-validation-error :arg-name "path" :message "path must not be empty"))
  (unless (member collapsed '(t nil))
    (error 'arg-validation-error :arg-name "collapsed" :message "collapsed must be boolean"))
  (unless (member include-comments '(t nil))
    (error 'arg-validation-error :arg-name "include_comments"
           :message "include-comments must be boolean"))
  (unless (member comment-context '("preceding" "all" "none") :test #'string=)
    (error 'arg-validation-error :arg-name "comment_context"
           :message "comment-context must be \"preceding\", \"all\", or \"none\""))
  (when (and offset (not (integerp offset)))
    (error 'arg-validation-error :arg-name "offset"
           :message "offset must be an integer when provided"))
  (when (and offset (< offset 0))
    (error 'arg-validation-error :arg-name "offset" :message "offset must be non-negative"))
  (when (and limit (not (integerp limit)))
    (error 'arg-validation-error :arg-name "limit"
           :message "limit must be an integer when provided"))
  (when (and limit (<= limit 0))
    (error 'arg-validation-error :arg-name "limit"
           :message "limit must be a positive integer when provided"))
  (let ((resolved (fs-resolve-read-path path))
        (line-limit (or limit *default-line-limit*))
        (name-scanner (%compile-scanner name-pattern "name_pattern"))
        (content-scanner (%compile-scanner content-pattern "content_pattern")))
    (multiple-value-bind (content meta mode)
        (%lisp-read-file-content resolved collapsed name-scanner content-scanner
                                 offset line-limit include-comments comment-context
                                 :readtable readtable)
      (let ((payload (make-hash-table :test #'equal)))
        (setf (gethash "content" payload) content
              (gethash "path" payload) (normalize-path-for-display resolved)
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
for full source.
When reading in raw mode (collapsed=false) and output is truncated, a
'[Showing lines A-B of N. Use offset=B to read more.]' footer is appended
to guide pagination. Use the suggested offset value in a follow-up call."
  :args ((path :type :string :required t
               :description "Path to read; absolute inside project or registered ASDF system,
or relative to project root")
         (collapsed :type :boolean :default t
                    :description "When true (default) collapse Lisp definitions to signatures")
         (name_pattern :type :string
                       :description "Regex to match definition names to expand (CL-PPCRE syntax)")
         (content_pattern :type :string
                          :description "Regex to match form bodies or text lines to expand")
         (offset :type :integer
                 :description "0-based line offset when collapsed=false (raw mode only)")
         (limit :type :integer
                :description "Maximum lines to return; defaults to 500")
         (readtable :type :string
                    :description "Named-readtable designator for files using custom reader macros.
Supports both keyword style ('interpol-syntax') and package-qualified style
('pokepay-syntax:pokepay-syntax'). NOTE: When specified, the standard CL reader
is used instead of Eclector, which means comments are NOT preserved."))
  :body
  (let ((file-result
         (lisp-read-file path
                         :collapsed collapsed
                         :name-pattern name_pattern
                         :content-pattern content_pattern
                         :offset offset
                         :limit limit
                         :readtable (when readtable
                                      (let ((colon-pos (position #\: readtable)))
                                        (if colon-pos
                                            ;; Package-qualified: "pkg:sym" or "pkg::sym"
                                            (let* ((pkg-name (subseq readtable 0 colon-pos))
                                                   (sym-start (if (and (< (1+ colon-pos) (length readtable))
                                                                       (char= (char readtable (1+ colon-pos)) #\:))
                                                                  (+ colon-pos 2)
                                                                  (1+ colon-pos)))
                                                   (sym-name (subseq readtable sym-start))
                                                   (pkg (find-package (string-upcase pkg-name))))
                                              (if pkg
                                                  (intern (string-upcase sym-name) pkg)
                                                  (error "Package ~A not found for readtable ~A"
                                                         pkg-name readtable)))
                                            ;; Keyword symbol (no colon prefix)
                                            (intern (string-upcase readtable) :keyword)))))))
    (result id
            (make-ht "content" (text-content (gethash "content" file-result))
                     "text" (gethash "content" file-result)
                     "path" (gethash "path" file-result)
                     "mode" (gethash "mode" file-result)
                     "meta" (gethash "meta" file-result)))))
