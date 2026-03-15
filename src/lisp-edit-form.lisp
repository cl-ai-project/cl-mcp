;;;; src/lisp-edit-form.lisp

(defpackage #:cl-mcp/src/lisp-edit-form
  (:use #:cl)
  (:shadowing-import-from #:cl-mcp/src/cst
                          #:cst-node
                          #:cst-node-kind
                          #:cst-node-value
                          #:cst-node-start
                          #:cst-node-end)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:cl-mcp/src/state
                #:protocol-version)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content
                #:arg-validation-error #:json-bool)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/utils/strings
                #:ensure-trailing-newline)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%resolve-named-readtable
                #:%parse-readtable-designator
                #:%whitespace-char-p
                #:%locate-target-form)
  (:documentation "Structure-aware editing of top-level Lisp forms.")
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

(defun %validate-and-repair-content (content &optional readtable-designator
                                             package-name source-path)
  "Ensure CONTENT is a single valid form. If parsing fails, attempt to repair
using parinfer:apply-indent-mode. Returns the validated (possibly repaired) content.
When READTABLE-DESIGNATOR is provided, use that named-readtable for parsing.
Unknown package prefixes are handled leniently via stub packages."
  (let* ((*read-eval* nil)
         (custom-rt (%resolve-named-readtable readtable-designator))
         (*readtable* (if custom-rt custom-rt (copy-readtable nil))))
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
                   (call-with-package-context
                    package-name
                    (lambda ()
                      (multiple-value-bind (form pos)
                          (read-from-string text nil :eof)
                        (when (eq form :eof)
                          (error "content is empty"))
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
                        text))
                    :source-path source-path)
                 (error (e)
                   (values nil e)))))
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
  (let ((start (cst-node-start node))
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

(defun lisp-edit-form (&key file-path form-type form-name operation content
                            dry-run (normalize-blank-lines t) readtable)
  "Structured edit of a top-level Lisp form.
FILE-PATH may be absolute or relative to the project root. FORM-TYPE,
FORM-NAME, and OPERATION are always required. CONTENT is always required
and specifies the full Lisp form.

OPERATION must be one of: \"replace\", \"insert_before\", \"insert_after\".
Missing closing parentheses are auto-repaired using parinfer.

When DRY-RUN is true, no changes are written; a preview hash-table is returned.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing both the file and the new content."
  (unless (and (stringp file-path) (stringp form-type) (stringp form-name)
               (stringp operation))
    (error "file_path, form_type, form_name, and operation must be strings"))
  (unless (stringp content)
    (error "content is required for ~A operation" operation))
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (unless (member normalize-blank-lines '(t nil))
    (error "normalize-blank-lines must be boolean"))
  (let* ((op-normalized (string-downcase operation))
         (op-key (cond ((string= op-normalized "replace") :replace)
                       ((string= op-normalized "insert_before") :insert-before)
                       ((string= op-normalized "insert_after") :insert-after)
                       (t (error "Unsupported operation: ~A" operation)))))
    (multiple-value-bind (abs rel original nodes target target-snippet _ file-package-name)
        (%locate-target-form file-path form-type form-name readtable)
      (declare (ignore nodes _))
      (multiple-value-bind (validated-content parinfer-warning)
          (%validate-and-repair-content content readtable
                                        file-package-name abs)
        (let* ((updated (%apply-operation original target op-key
                                          validated-content
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
            (would-change
             (fs-write-file rel updated)
             (values updated parinfer-warning t))
            (t
             (values updated parinfer-warning nil))))))))

(define-tool "lisp-edit-form"
  :description "Structure-aware edit of a top-level Lisp form using Eclector CST parsing.
Supports replace, insert_before, and insert_after operations while preserving
formatting and comments.
PREFERRED METHOD for editing existing Lisp source code.
Automatically repairs missing closing parentheses using parinfer.
ALWAYS use this tool instead of 'fs-write-file' when modifying Lisp forms to ensure
safety and structure preservation."
  :args ((file_path :type :string :required t
                    :description "Target file path (absolute recommended)")
         (form_type :type :string :required t
                    :description "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\"")
         (form_name :type :string :required t
                    :description "Form name to match; for defmethod include specializers,
e.g., \"print-object ((obj my-class) stream)\"")
         (operation :type :string :required t
                    :enum ("replace" "insert_before" "insert_after")
                    :description "Operation to perform")
         (content :type :string :required t
                  :description "Full Lisp form for the operation. Must contain exactly ONE top-level form.
Missing closing parentheses are automatically repaired using parinfer.")
         (dry_run :type :boolean
                  :description "When true, return a preview without writing to disk")
         (normalize_blank_lines :type :boolean
                                :default t
                                :description "When true (default), normalize blank lines around edited top-level forms.
Applies to replace, insert_before, and insert_after operations.")
         (readtable :type :string
                    :description "Named-readtable designator for files using custom reader macros.
Supports both keyword style ('interpol-syntax') and package-qualified style
('pokepay-syntax:pokepay-syntax'). NOTE: When specified, the standard CL reader
is used instead of Eclector, which means comments are NOT preserved."))
  :body
  (progn
    (unless content
      (error 'arg-validation-error :arg-name "content"
             :message (format nil "content is required for ~A operation" operation)))
    (handler-case
        (multiple-value-bind (updated parinfer-warning changed-p)
            (lisp-edit-form :file-path file_path
                            :form-type form_type
                            :form-name form_name
                            :operation operation
                            :content content
                            :dry-run dry_run
                            :normalize-blank-lines normalize_blank_lines
                            :readtable (%parse-readtable-designator readtable))
          (if dry_run
              (let* ((preview (gethash "preview" updated))
                     (would-change (eq t (gethash "would_change" updated)))
                     (original-form (gethash "original" updated))
                     (pw (gethash "parinfer_warning" updated))
                     (summary (format nil "Dry-run ~A on ~A ~A in ~A (~:[no change~;would change~])~@[~%WARNING: ~A~]"
                                      operation form_type form_name file_path would-change pw)))
                (result id
                        (apply #'make-ht
                               "path" file_path
                               "operation" operation
                               "form_type" form_type
                               "form_name" form_name
                               "would_change" (json-bool would-change)
                               "original" original-form
                               "preview" preview
                               "content" (text-content summary)
                               (when pw
                                 (list "parinfer_warning" pw)))))
              (let ((summary
                     (cond
                       ((not changed-p)
                        (format nil "No change to ~A ~A in ~A (content matches existing form)~@[~%WARNING: ~A~]"
                                form_type form_name file_path parinfer-warning))
                       (t
                        (format nil "Applied ~A to ~A ~A in ~A (~D chars)~@[~%WARNING: ~A~]"
                                operation form_type form_name file_path (length updated) parinfer-warning)))))
                (result id
                        (make-ht "path" file_path
                                 "operation" operation
                                 "form_type" form_type
                                 "form_name" form_name
                                 "would_change" (json-bool changed-p)
                                 "bytes" (length updated)
                                 "content" (text-content summary))))))
      (multiple-top-level-forms-error ()
        (if (and (protocol-version state)
                 (string>= (protocol-version state) "2025-11-25"))
            (result id (make-ht "content"
                                (text-content (%multiple-top-level-forms-error-message))
                                "isError" t
                                "remediation" (%multiple-top-level-forms-error-data)))
            (rpc-error id -32602 (%multiple-top-level-forms-error-message)
                       (%multiple-top-level-forms-error-data))))
      (error (e)
        (let ((msg (sanitize-for-json
                    (sanitize-error-message (format nil "~A" e)))))
          (if (and (protocol-version state)
                   (string>= (protocol-version state) "2025-11-25"))
              (result id (make-ht "content" (text-content msg) "isError" t))
              (rpc-error id -32603 msg)))))))
