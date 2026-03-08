;;;; src/lisp-edit-form.lisp

(defpackage #:cl-mcp/src/lisp-edit-form
  (:use #:cl)
  (:shadowing-import-from #:cl-mcp/src/cst
                          #:cst-node
                          #:cst-node-kind
                          #:cst-node-value
                          #:cst-node-start
                          #:cst-node-end)
  (:import-from #:cl-ppcre
                #:scan-to-strings)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages)
  (:import-from #:cl-mcp/src/utils/strings
                #:ensure-trailing-newline)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:enough-pathname
                #:native-namestring
                #:subpathp)
  (:export #:lisp-edit-form))

(in-package #:cl-mcp/src/lisp-edit-form)

(defun %multiple-top-level-forms-error-message ()
  "Return the user-facing error message for multiple top-level form content."
  "content must contain exactly one top-level form; multiple forms are not supported in a single call")

(define-condition multiple-top-level-forms-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string (%multiple-top-level-forms-error-message) stream))))

(defun %multiple-top-level-forms-error-data ()
  "Return machine-readable remediation guidance for multiple-form content errors."
  (make-ht "code" "multiple_forms_not_supported"
           "next_tool" "lisp-edit-form"
           "action" "split_into_multiple_calls"
           "example_operation_sequence" (vector "insert_after" "insert_after")
           "required_args"
           (vector "file_path" "form_type" "form_name" "operation" "content")))

(defun %normalize-string (thing)
  "Normalize THING to a lowercase string for form matching.
Uses SYMBOL-NAME for symbols to avoid package prefix in the output."
  (string-downcase
   (if (symbolp thing)
       (symbol-name thing)
       (princ-to-string thing))))

(defun %defmethod-candidates (form)
  "Return candidate signature strings for a DEFMETHOD FORM.
Candidates are generated in order of specificity:
1. name only: \"resize\"
2. name + qualifier: \"resize :after\"
3. name + lambda-list: \"resize ((s shape) factor)\"
4. name + qualifier + lambda-list: \"resize :after ((s shape) factor)\""
  (destructuring-bind (_ name &rest rest) form
    (declare (ignore _))
    (let ((qualifiers '())
          (lambda-list nil))
      (dolist (part rest)
        (when (listp part)
          (setf lambda-list part)
          (return))
        (push part qualifiers))
      (let ((name-str (%normalize-string name))
            (lambda-str (and lambda-list
                             (%normalize-string
                              (with-output-to-string (s)
                                (prin1 lambda-list s)))))
            ;; Use ~S to preserve colon prefix for keywords like :after
            (qual-str (and qualifiers
                           (%normalize-string
                            (format nil "~{~S~^ ~}" (nreverse qualifiers))))))
        (remove nil
                (list name-str
                      ;; name + qualifier (without lambda-list)
                      (and qual-str (format nil "~A ~A" name-str qual-str))
                      (and lambda-str (format nil "~A ~A" name-str lambda-str))
                      (and (and qual-str lambda-str)
                           (format nil "~A ~A ~A" name-str qual-str lambda-str))))))))

(defun %definition-candidates (form form-type)
  "Return candidate strings that identify FORM with FORM-TYPE."
  (let ((name (second form)))
    (cond
      ((string= form-type "defmethod")
       (%defmethod-candidates form))
      ((symbolp name)
       (list (%normalize-string name)))
      (t (list (%normalize-string name))))))

(defun %ensure-blank-separation (prefix between)
  "Return BETWEEN extended so PREFIX+BETWEEN ends with at least two newlines.
Keeps existing whitespace intact and adds the minimal number of newlines
necessary to leave one blank line between top-level forms."
  (flet ((trailing-newlines (str)
           (loop for i downfrom (1- (length str)) to 0
                 while (char= (char str i) #\Newline)
                 count 1)))
    (let* ((combined (concatenate 'string prefix between))
           (missing (max 0 (- 2 (trailing-newlines combined)))))
      (if (zerop missing)
          between
          (concatenate 'string between
                       (make-string missing :initial-element #\Newline))))))

(defun %whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return)))

(defun %split-leading-whitespace (text)
  "Split TEXT into two values: leading whitespace and the remaining text."
  (let ((ws-end (or (position-if-not #'%whitespace-char-p text)
                    (length text))))
    (values (subseq text 0 ws-end)
            (subseq text ws-end))))

(defun %split-trailing-whitespace (text)
  "Split TEXT into two values: text without trailing whitespace and trailing whitespace."
  (let ((last-non-ws (position-if-not #'%whitespace-char-p text :from-end t)))
    (if last-non-ws
        (values (subseq text 0 (1+ last-non-ws))
                (subseq text (1+ last-non-ws)))
        (values "" text))))

(defun %normalized-separator (left-text right-text)
  "Return normalized separator between LEFT-TEXT and RIGHT-TEXT at top-level.
No separator is emitted before the first form. Between top-level forms use one
blank line. For EOF boundary use a single newline."
  (cond
    ((zerop (length left-text)) "")
    ((zerop (length right-text)) (string #\Newline))
    (t (format nil "~%~%"))))

(defun %trim-outer-whitespace (text)
  "Trim leading/trailing horizontal and vertical whitespace from TEXT."
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun %normalize-paths (file-path)
  "Return two values: absolute path (pathname) and relative namestring for FS tools."
  (let ((resolved (fs-resolve-read-path file-path))
        (root (ensure-directory-pathname *project-root*)))
    (unless (subpathp resolved root)
      (error "Write path ~A is outside project root ~A" file-path root))
    (let* ((relative (enough-pathname resolved root))
           (rel-namestring (native-namestring relative)))
      (values resolved rel-namestring))))

(defun %validate-and-repair-content (content &optional readtable-designator)
  "Ensure CONTENT is a single valid form. If parsing fails, attempt to repair
using parinfer:apply-indent-mode. Returns the validated (possibly repaired) content.
When READTABLE-DESIGNATOR is provided, use that named-readtable for parsing.
Unknown package prefixes are handled leniently via stub packages."
  (let* ((*read-eval* nil)
         (custom-rt
          (when readtable-designator
            (let ((pkg
                   (or (find-package :named-readtables)
                       (find-package :editor-hints.named-readtables))))
              (when pkg
                (let ((find-fn (find-symbol "FIND-READTABLE" pkg)))
                  (when (and find-fn (fboundp find-fn))
                    (funcall find-fn readtable-designator)))))))
         (*readtable*
          (if custom-rt
              custom-rt
              (copy-readtable nil))))
    (labels ((whitespace-char-p (ch)
               (member ch '(#\Space #\Tab #\Newline #\Return)))
             (rest-parses-as-complete-forms-p (text start)
               (let ((len (length text)))
                 (handler-case
                     (loop with cursor = start
                           with saw-form = nil
                           do (setf cursor
                                    (or (position-if-not #'whitespace-char-p
                                                         text :start cursor)
                                        len))
                              (when (>= cursor len)
                                (return saw-form))
                              (multiple-value-bind (next-form next-pos)
                                  (read-from-string text nil :eof
                                                    :start cursor :end len)
                                (when (eq next-form :eof)
                                  (return saw-form))
                                (setf saw-form t
                                      cursor next-pos)))
                   (error nil nil))))
             (try-parse (text)
               (handler-case
                   (call-with-lenient-packages
                    (lambda ()
                      (multiple-value-bind (form pos)
                          (read-from-string text nil :eof)
                        (when (eq form :eof) (error "content is empty"))
                        (let* ((len (length text))
                               (rest-start
                                (or (position-if-not #'whitespace-char-p
                                                     text :start pos)
                                    len)))
                          (when (< rest-start len)
                            (if (rest-parses-as-complete-forms-p text rest-start)
                                (error 'multiple-top-level-forms-error)
                                (error
                                 "content has trailing malformed characters after the first form"))))
                        text)))
                 (error (e) (values nil e)))))
      (multiple-value-bind (result err)
          (try-parse content)
        (if result
            (values result nil)
            (let ((repaired (apply-indent-mode content)))
              (multiple-value-bind (repaired-result repaired-err)
                  (try-parse repaired)
                (cond
                  (repaired-result
                   (log-event :info "lisp.edit.form" "auto-repair" "success"
                              "original-error" (princ-to-string err))
                   ;; Return repaired content and a description of what changed
                   (let ((added-count (- (length repaired) (length content))))
                     (values repaired-result
                             (format nil "~D closing delimiter~:P ~
                                          ~[were~;was~:;were~] added by parinfer"
                                     added-count added-count))))
                  ((and (typep err 'multiple-top-level-forms-error)
                        (typep repaired-err 'multiple-top-level-forms-error))
                   (error err))
                  (t
                   (error "content parse error: ~A (repair also failed: ~A)"
                          err repaired-err))))))))))

(defun %find-target (nodes form-type form-name)
  "Find a target node matching FORM-TYPE and FORM-NAME.
If FORM-NAME ends with [N] (e.g., 'resize[1]'), select the Nth match (0-indexed).
If multiple matches exist without an index, signals an error with candidate info."
  (multiple-value-bind (base-name index)
      (let ((match (nth-value 1 (scan-to-strings "^(.+?)\\[(\\d+)\\]$" form-name))))
        (if match
            (values (aref match 0) (parse-integer (aref match 1)))
            (values form-name nil)))
    (let ((target (string-downcase base-name))
          (matches nil))
      (loop for node in nodes
            when (and (typep node 'cst-node)
                      (eq (cst-node-kind node) :expr))
              do (let ((value (cst-node-value node)))
                   (when (and (consp value)
                              (string= (string-downcase (symbol-name (car value))) form-type)
                              (some (lambda (cand) (string= cand target))
                                    (%definition-candidates value form-type)))
                     (push (cons node value) matches))))
      (setf matches (nreverse matches))
      (cond
        ((null matches)
         nil)
        ((and index (< index (length matches)))
         (car (nth index matches)))
        (index
         (error "Index [~D] out of range, only ~D match~:P found for ~A"
                index (length matches) form-name))
        ((= (length matches) 1)
         (car (first matches)))
        (t
         ;; Multiple matches without index - provide helpful error
         (let ((descriptions
                 (loop for (node . form) in matches
                       for i from 0
                       collect (format nil "[~D] ~A"
                                       i
                                       (let ((candidates (%definition-candidates form form-type)))
                                         (or (car (last candidates)) (first candidates)))))))
           (error "Multiple matches for ~A ~A. Specify an index:~%~{  ~A~%~}"
                  form-type form-name descriptions)))))))

(defun %apply-operation-preserve-spacing (text node operation content)
  (let ((start (cst-node-start node))
        (end (cst-node-end node))
        (snippet (ecase operation
                   ((:replace) content)
                   ((:insert-before :insert-after) (ensure-trailing-newline content)))))
    (ecase operation
      (:replace
       (concatenate 'string (subseq text 0 start) snippet (subseq text end)))
      (:insert-before
       (let* ((prefix (subseq text 0 start))
              (sep (if (zerop start)
                       ""
                       (%ensure-blank-separation prefix ""))))
         (concatenate 'string prefix sep snippet (subseq text start))))
      (:insert-after
       (let* ((suffix (subseq text end))
              (ws-end (or (position-if-not
                           (lambda (ch)
                             (member ch '(#\Space #\Tab #\Newline #\Return)))
                           suffix)
                          (length suffix)))
              (between (%ensure-blank-separation (subseq text 0 end)
                                                 (subseq suffix 0 ws-end)))
              (rest (subseq suffix ws-end))
              (prefix (subseq text 0 end)))
         (concatenate 'string prefix between snippet rest))))))

(defun %apply-operation-normalized (text node operation content)
  (let* ((start (cst-node-start node))
         (end (cst-node-end node))
         (snippet (%trim-outer-whitespace content)))
    (ecase operation
      (:replace
       (multiple-value-bind (prefix-core _)
           (%split-trailing-whitespace (subseq text 0 start))
         (declare (ignore _))
         (multiple-value-bind (_ suffix-core)
             (%split-leading-whitespace (subseq text end))
           (declare (ignore _))
           (concatenate 'string
                        prefix-core
                        (%normalized-separator prefix-core snippet)
                        snippet
                        (%normalized-separator snippet suffix-core)
                        suffix-core))))
      (:insert-before
       (multiple-value-bind (prefix-core _)
           (%split-trailing-whitespace (subseq text 0 start))
         (declare (ignore _))
         (let ((target (subseq text start end))
               (suffix (subseq text end)))
           (concatenate 'string
                        prefix-core
                        (%normalized-separator prefix-core snippet)
                        snippet
                        (%normalized-separator snippet target)
                        target
                        suffix))))
      (:insert-after
       (multiple-value-bind (_ suffix-core)
           (%split-leading-whitespace (subseq text end))
         (declare (ignore _))
         (let ((prefix (subseq text 0 end)))
           (concatenate 'string
                        prefix
                        (%normalized-separator prefix snippet)
                        snippet
                        (%normalized-separator snippet suffix-core)
                        suffix-core)))))))

(defun %apply-operation (text node operation content normalize-blank-lines)
  (if normalize-blank-lines
      (%apply-operation-normalized text node operation content)
      (%apply-operation-preserve-spacing text node operation content)))

(defun %apply-edit-operation (text node old-text new-text)
  "Replace OLD-TEXT with NEW-TEXT within the form at NODE in TEXT.
Returns two values: the modified full file text and the modified form text.
Signals an error if OLD-TEXT is not found or occurs multiple times within the form."
  (when (zerop (length old-text))
    (error "old_text must not be empty"))
  (let* ((start (cst-node-start node))
         (end (cst-node-end node))
         (form-text (subseq text start end))
         (match-pos (search old-text form-text)))
    (unless match-pos
      (let* ((form-value (cst-node-value node))
             (form-id
              (if (consp form-value)
                  (format nil "~A ~A" (car form-value) (second form-value))
                  "matched")))
        (error "old_text not found in ~A form. ~
                Note: matching is exact and whitespace-sensitive. ~
                If the file may have different line endings (CRLF vs LF), ~
                ensure old_text uses matching line endings. ~
                Use lisp-read-file with name_pattern to see the exact form text. ~
                old_text begins with: ~S"
               form-id (subseq old-text 0 (min (length old-text) 60)))))
    (let ((second-match
           (search old-text form-text :start2 (1+ match-pos))))
      (when second-match
        (let ((count
               (loop for pos = (search old-text form-text) then (search
                                                                  old-text
                                                                  form-text
                                                                  :start2
                                                                  (1+ pos))
                     while pos
                     count 1)))
          (error "old_text matches ~D times in the form; ~
                  provide more surrounding context to match exactly once"
                 count))))
    (let* ((modified-form
            (concatenate 'string (subseq form-text 0 match-pos) new-text
                         (subseq form-text (+ match-pos (length old-text)))))
           (modified-file
            (concatenate 'string (subseq text 0 start) modified-form
                         (subseq text end))))
      (values modified-file modified-form))))

(defun %validate-form-parseable (form-text &optional readtable-designator)
  "Validate that FORM-TEXT parses as a single complete Lisp form.
Unlike %validate-and-repair-content, does NOT attempt parinfer repair.
Signals an error if the text does not parse correctly."
  (let* ((*read-eval* nil)
         (custom-rt
           (when readtable-designator
             (let ((pkg
                     (or (find-package :named-readtables)
                         (find-package :editor-hints.named-readtables))))
               (when pkg
                 (let ((find-fn (find-symbol "FIND-READTABLE" pkg)))
                   (when (and find-fn (fboundp find-fn))
                     (funcall find-fn readtable-designator)))))))
         (*readtable*
           (if custom-rt
               custom-rt
               (copy-readtable nil))))
    (handler-case
        (call-with-lenient-packages
         (lambda ()
           (multiple-value-bind (form pos)
               (read-from-string form-text nil :eof)
             (when (eq form :eof)
               (error "edit produced an empty form"))
             (let ((rest-start (or (position-if-not
                                    (lambda (ch)
                                      (member ch '(#\Space #\Tab #\Newline #\Return)))
                                    form-text :start pos)
                                   (length form-text))))
               (when (< rest-start (length form-text))
                 (error "edit produced malformed form text (trailing content after form)")))
             form-text)))
      (error (e)
        (error "edit operation produced invalid Lisp: ~A. ~
                The form could not be parsed after replacement. ~
                No changes were written to disk."
               e)))))

(defun %detect-readtable-from-nodes (nodes)
  "Scan parsed NODES for an IN-READTABLE form and return its designator, or NIL.
This mirrors the auto-detection logic in parse-top-level-forms."
  (dolist (node nodes)
    (when (and (typep node 'cst-node)
               (eq (cst-node-kind node) :expr))
      (let ((value (cst-node-value node)))
        (when (and (consp value)
                   (symbolp (car value))
                   (string= (symbol-name (car value)) "IN-READTABLE")
                   (consp (cdr value)))
          (return (second value)))))))

(defun lisp-edit-form (&key file-path form-type form-name operation content
                            old-text new-text
                            dry-run (normalize-blank-lines t) readtable)
  "Structured edit of a top-level Lisp form.
FILE-PATH may be absolute or relative to the project root. FORM-TYPE,
FORM-NAME, and OPERATION are always required.

For OPERATION \"replace\", \"insert_before\", or \"insert_after\": CONTENT is required
and specifies the full Lisp form. Missing closing parentheses are auto-repaired
using parinfer.

For OPERATION \"edit\": OLD-TEXT and NEW-TEXT are required. Performs an exact text
replacement within the matched form. CONTENT must not be provided. Does NOT
auto-repair parentheses; if the edit breaks form structure, an error is signaled
immediately.

When DRY-RUN is true, no changes are written; a preview hash-table is returned.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing both the file and the new content."
  (unless (and (stringp file-path) (stringp form-type) (stringp form-name)
               (stringp operation))
    (error "file_path, form_type, form_name, and operation must be strings"))
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (unless (member normalize-blank-lines '(t nil))
    (error "normalize-blank-lines must be boolean"))
  (let* ((op-normalized (string-downcase operation))
         (op-key (cond ((string= op-normalized "replace") :replace)
                       ((string= op-normalized "insert_before") :insert-before)
                       ((string= op-normalized "insert_after") :insert-after)
                       ((string= op-normalized "edit") :edit)
                       (t (error "Unsupported operation: ~A" operation))))
         (form-type-str (string-downcase form-type)))
    ;; Operation-specific parameter validation
    (case op-key
      (:edit
       (unless (and (stringp old-text) (stringp new-text))
         (error "old_text and new_text are required for edit operation"))
       (when content
         (error "content must not be provided for edit; use old_text and new_text")))
      (otherwise
       (unless (stringp content)
         (error "content is required for ~A operation" operation))))
    (if (eq op-key :edit)
        ;; Edit operation: scoped text replacement within matched form
        (multiple-value-bind (abs rel)
            (%normalize-paths file-path)
          (let* ((original (fs-read-file abs))
                 (nodes (parse-top-level-forms original :readtable readtable))
                 (target (%find-target nodes form-type-str form-name)))
            (unless target
              (error "Form ~A ~A not found in ~A" form-type form-name abs))
            (let ((target-snippet (subseq original
                                         (cst-node-start target)
                                         (cst-node-end target))))
              (multiple-value-bind (updated modified-form)
                  (%apply-edit-operation original target old-text new-text)
                (let ((would-change (not (string= original updated))))
                  ;; Validate modified form parses correctly (fail-fast, no parinfer)
                  (when would-change
                    (%validate-form-parseable
                     modified-form
                     (or readtable (%detect-readtable-from-nodes nodes))))
                  (log-event :debug "lisp.edit.form"
                             "path" (namestring abs)
                             "operation" op-normalized
                             "form_type" form-type
                             "form_name" form-name
                             "bytes" (length updated)
                             "dry_run" dry-run
                             "would_change" would-change)
                  (cond
                    (dry-run
                     (let ((result (make-hash-table :test #'equal)))
                       (setf (gethash "would_change" result) would-change
                             (gethash "original" result) target-snippet
                             (gethash "preview" result) updated
                             (gethash "file_path" result) (namestring abs)
                             (gethash "operation" result) op-normalized)
                       result))
                    (would-change
                     (fs-write-file rel updated)
                     updated)
                    (t updated)))))))
        ;; Existing operations: replace, insert_before, insert_after
        (multiple-value-bind (validated-content parinfer-warning)
            (%validate-and-repair-content content readtable)
          (multiple-value-bind (abs rel)
              (%normalize-paths file-path)
            (let* ((original (fs-read-file abs))
                   (nodes (parse-top-level-forms original :readtable readtable))
                   (target (%find-target nodes form-type-str form-name)))
              (unless target
                (error "Form ~A ~A not found in ~A" form-type form-name abs))
              (let* ((start (cst-node-start target))
                     (end (cst-node-end target))
                     (target-snippet (subseq original start end))
                     (updated (%apply-operation original target op-key validated-content
                                               normalize-blank-lines))
                     (would-change (not (string= original updated))))
                (log-event :debug "lisp.edit.form"
                           "path" (namestring abs)
                           "operation" op-normalized
                           "form_type" form-type
                           "form_name" form-name
                           "normalize_blank_lines" normalize-blank-lines
                           "bytes" (length updated)
                           "dry_run" dry-run
                           "would_change" would-change)
                (cond
                  (dry-run
                   (let ((result (make-hash-table :test #'equal)))
                     (setf (gethash "would_change" result) would-change
                           (gethash "original" result) target-snippet
                           (gethash "preview" result) updated
                           (gethash "file_path" result) (namestring abs)
                           (gethash "operation" result) op-normalized)
                     (when parinfer-warning
                       (setf (gethash "parinfer_warning" result) parinfer-warning))
                     result))
                  (t
                   (fs-write-file rel updated)
                   (values updated parinfer-warning))))))))))

(define-tool "lisp-edit-form"
  :description "Structure-aware edit of a top-level Lisp form using Eclector CST parsing.
Supports replace, insert_before, insert_after, and edit operations while preserving
formatting and comments.
PREFERRED METHOD for editing existing Lisp source code.
For replace/insert_before/insert_after: automatically repairs missing closing parentheses
using parinfer.
For edit: performs a scoped text replacement within the matched form using old_text/new_text.
The old_text must match exactly once within the form (whitespace-sensitive).
This is the most token-efficient way to make small changes to large forms.
Does NOT auto-repair parentheses for edit — if the edit breaks form structure, it fails
immediately and no changes are written to disk.
ALWAYS use this tool instead of 'fs-write-file' when modifying Lisp forms to ensure
safety and structure preservation."
  :args ((file_path :type :string :required t
                    :description "Target file path (absolute recommended)")
         (form_type :type :string :required t
                    :description "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\"")
         (form_name :type :string :required t
                    :description "Form name to match; for defmethod include specializers,
e.g., \"print-object (my-class t)\"")
         (operation :type :string :required t
                    :enum ("replace" "insert_before" "insert_after" "edit")
                    :description "Operation to perform")
         (content :type :string
                  :description "Full Lisp form for replace/insert_before/insert_after operations.
Must contain exactly ONE top-level form. Not used for edit operation.")
         (old_text :type :string
                   :description "Text to find within the matched form (required for edit operation).
Performs exact raw text matching (whitespace-sensitive). Must occur exactly once in the form.")
         (new_text :type :string
                   :description "Replacement text (required for edit operation)")
         (dry_run :type :boolean
                  :description "When true, return a preview without writing to disk")
         (normalize_blank_lines :type :boolean
                                :default t
                                :description "When true (default), normalize blank lines around edited top-level forms.
Applies to replace, insert_before, and insert_after operations only; ignored for edit.")
         (readtable :type :string
                    :description "Named-readtable designator for files using custom reader macros.
Supports both keyword style ('interpol-syntax') and package-qualified style
('pokepay-syntax:pokepay-syntax'). NOTE: When specified, the standard CL reader
is used instead of Eclector, which means comments are NOT preserved."))
  :body
  (let ((op-lower (string-downcase operation)))
    ;; Cross-parameter validation using arg-validation-error for -32602 codes
    (cond
      ((string= op-lower "edit")
       (unless old_text
         (error 'arg-validation-error :arg-name "old_text"
                :message "old_text is required for edit operation"))
       (when (zerop (length old_text))
         (error 'arg-validation-error :arg-name "old_text"
                :message "old_text must not be empty"))
       (unless new_text
         (error 'arg-validation-error :arg-name "new_text"
                :message "new_text is required for edit operation"))
       (when content
         (error 'arg-validation-error :arg-name "content"
                :message "content must not be provided for edit operation; use old_text and new_text")))
      (t
       (unless content
         (error 'arg-validation-error :arg-name "content"
                :message (format nil "content is required for ~A operation" operation)))))
    (handler-case
        (multiple-value-bind (updated parinfer-warning)
            (lisp-edit-form :file-path file_path
                            :form-type form_type
                            :form-name form_name
                            :operation operation
                            :content content
                            :old-text old_text
                            :new-text new_text
                            :dry-run dry_run
                            :normalize-blank-lines normalize_blank_lines
                            :readtable (when readtable
                                         (let ((colon-pos (position #\: readtable)))
                                           (if colon-pos
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
                                               (intern (string-upcase readtable) :keyword)))))
          (if dry_run
              (let* ((preview (gethash "preview" updated))
                     (would-change (gethash "would_change" updated))
                     (original-form (gethash "original" updated))
                     (pw (gethash "parinfer_warning" updated))
                     (summary (format nil "Dry-run ~A on ~A ~A (~:[no change~;would change~])~@[~%WARNING: ~A~]"
                                      operation form_type file_path would-change pw)))
                (result id
                        (make-ht "path" file_path
                                 "operation" operation
                                 "form_type" form_type
                                 "form_name" form_name
                                 "would_change" would-change
                                 "original" original-form
                                 "preview" preview
                                 "content" (text-content summary))))
              (let ((summary (format nil "Applied ~A to ~A ~A (~D chars)~@[~%WARNING: ~A~]"
                                     operation form_type file_path (length updated) parinfer-warning)))
                (result id
                        (make-ht "path" file_path
                                 "operation" operation
                                 "form_type" form_type
                                 "form_name" form_name
                                 "bytes" (length updated)
                                 "content" (text-content summary))))))
      (multiple-top-level-forms-error ()
        (cl-mcp/src/tools/helpers:rpc-error
         id -32602 (%multiple-top-level-forms-error-message)
         (%multiple-top-level-forms-error-data))))))
