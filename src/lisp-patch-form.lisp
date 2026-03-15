;;;; src/lisp-patch-form.lisp
;;;;
;;;; Scoped text replacement within a matched top-level Lisp form.
;;;; For structural operations (replace/insert), see lisp-edit-form.lisp.

(defpackage #:cl-mcp/src/lisp-patch-form
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
  (:import-from #:cl-mcp/src/state
                #:protocol-version)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content
                #:arg-validation-error #:tool-error #:json-bool)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%resolve-named-readtable
                #:%parse-readtable-designator
                #:%detect-readtable-before-node
                #:%whitespace-char-p
                #:%locate-target-form)
  (:documentation "Scoped text replacement within a matched top-level Lisp form.")
  (:export #:lisp-patch-form))

(in-package #:cl-mcp/src/lisp-patch-form)

(define-condition patch-operation-error (error)
  ((reason :initarg :reason :reader patch-operation-reason))
  (:report (lambda (c s) (write-string (patch-operation-reason c) s)))
  (:documentation "Raised for expected patch failures (not-found, multiple-match, invalid result)."))

(defun %apply-patch-operation (text node old-text new-text)
  "Replace OLD-TEXT with NEW-TEXT within the form at NODE in TEXT.
Returns two values: the modified full file text and the modified form text.
Signals PATCH-OPERATION-ERROR if OLD-TEXT is not found or occurs multiple times."
  (when (zerop (length old-text))
    (error 'arg-validation-error :arg-name "old_text"
           :message "old_text must not be empty"))
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
        (error 'patch-operation-error
               :reason (format nil "old_text not found in ~A form. ~
                Note: matching is exact and whitespace-sensitive. ~
                If the file may have different line endings (CRLF vs LF), ~
                ensure old_text uses matching line endings. ~
                Use lisp-read-file with name_pattern to see the exact form text. ~
                old_text begins with: ~S~:[~;...~]"
                       form-id (subseq old-text 0 (min (length old-text) 60))
                       (> (length old-text) 60)))))
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
          (error 'patch-operation-error
                 :reason (format nil "old_text matches ~D times in the form; ~
                  provide more surrounding context to match exactly once"
                                 count)))))
    (let* ((modified-form
            (concatenate 'string (subseq form-text 0 match-pos) new-text
                         (subseq form-text (+ match-pos (length old-text)))))
           (modified-file
            (concatenate 'string (subseq text 0 start) modified-form
                         (subseq text end))))
      (values modified-file modified-form))))

(defun %validate-form-parseable (form-text &optional readtable-designator
                                           package-name source-path)
  "Validate that FORM-TEXT parses as a single complete Lisp form.
Does NOT attempt parinfer repair. Signals PATCH-OPERATION-ERROR if the text
does not parse correctly."
  (let* ((*read-eval* nil)
         (custom-rt (%resolve-named-readtable readtable-designator))
         (*readtable*
           (if custom-rt
               custom-rt
               (copy-readtable nil))))
    (handler-case
        (call-with-package-context
         package-name
         (lambda ()
           (multiple-value-bind (form pos)
               (read-from-string form-text nil :eof)
             (when (eq form :eof)
               (error 'patch-operation-error
                      :reason "patch produced an empty form"))
             (let ((rest-start (or (position-if-not #'%whitespace-char-p
                                                    form-text :start pos)
                                   (length form-text))))
               (when (< rest-start (length form-text))
                 (error 'patch-operation-error
                        :reason "patch produced malformed form text (trailing content after form)")))
             form-text))
         :source-path source-path)
      (patch-operation-error (e)
        (error e))
      (error (e)
        (error 'patch-operation-error
               :reason (format nil "patch operation produced invalid Lisp: ~A. ~
                The form could not be parsed after replacement. ~
                No changes were written to disk."
                               e))))))

(defun lisp-patch-form (&key file-path form-type form-name old-text new-text
                              dry-run readtable)
  "Scoped text replacement within a matched top-level Lisp form.
FILE-PATH may be absolute or relative to the project root. FORM-TYPE and
FORM-NAME identify the target form. OLD-TEXT and NEW-TEXT specify the replacement.

OLD-TEXT must match exactly once within the form (whitespace-sensitive).
Does NOT auto-repair parentheses; if the patch breaks form structure, an error
is signaled immediately and no changes are written to disk.

When DRY-RUN is true, no changes are written; a preview hash-table is returned.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing the file."
  (unless (and (stringp file-path) (stringp form-type) (stringp form-name))
    (error "file_path, form_type, and form_name must be strings"))
  (unless (and (stringp old-text) (stringp new-text))
    (error "old_text and new_text must be strings"))
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (multiple-value-bind (abs rel original nodes target target-snippet _ file-package-name)
      (handler-case
          (%locate-target-form file-path form-type form-name readtable)
        (error (e)
          (error 'patch-operation-error
                 :reason (format nil "~A" e))))
    (multiple-value-bind (updated modified-form)
        (%apply-patch-operation original target old-text new-text)
      (let ((would-change (not (string= original updated))))
        (when would-change
          (%validate-form-parseable
           modified-form
           (or readtable
               (%detect-readtable-before-node nodes target))
           file-package-name
           abs))
        (log-event :debug "lisp.patch.form"
                   "path" (namestring abs)
                   "form_type" form-type
                   "form_name" form-name
                   "dry_run" dry-run
                   "would_change" would-change)
        (cond
          (dry-run
           (let ((result (make-hash-table :test #'equal)))
             (setf (gethash "would_change" result) would-change
                   (gethash "original" result) target-snippet
                   (gethash "preview" result) modified-form
                   (gethash "file_path" result) (namestring abs)
                   (gethash "operation" result) "patch")
             result))
          (would-change
           (fs-write-file rel updated)
           (values updated t))
          (t
           (values updated nil)))))))

(define-tool "lisp-patch-form"
  :description "Scoped text replacement within a matched top-level Lisp form.
Finds old_text (exact, whitespace-sensitive match) within the form identified
by form_type and form_name, and replaces it with new_text. old_text must match
exactly once within the form.
Most token-efficient way to make small changes to large forms.
Does NOT auto-repair parentheses — if the patch breaks form structure, it fails
immediately and no changes are written to disk.
Use 'lisp-edit-form' instead when replacing or inserting entire forms."
  :args ((file_path :type :string :required t
                    :description "Target file path (absolute recommended)")
         (form_type :type :string :required t
                    :description "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\"")
         (form_name :type :string :required t
                    :description "Form name to match; for defmethod include specializers,
e.g., \"print-object ((obj my-class) stream)\"")
         (old_text :type :string :required t
                   :description "Text to find within the matched form.
Performs exact raw text matching (whitespace-sensitive). Must occur exactly once in the form.")
         (new_text :type :string :required t
                   :description "Replacement text")
         (dry_run :type :boolean
                  :description "When true, return a preview without writing to disk")
         (readtable :type :string
                    :description "Named-readtable designator for files using custom reader macros.
Supports both keyword style ('interpol-syntax') and package-qualified style
('pokepay-syntax:pokepay-syntax'). NOTE: When specified, the standard CL reader
is used instead of Eclector, which means comments are NOT preserved."))
  :body
  (let ((readtable-designator
         (handler-case (%parse-readtable-designator readtable)
           (error (e)
             (error 'arg-validation-error :arg-name "readtable"
                    :message (format nil "~A" e))))))
    (handler-case
        (multiple-value-bind (updated changed-p)
            (lisp-patch-form :file-path file_path
                             :form-type form_type
                             :form-name form_name
                             :old-text old_text
                             :new-text new_text
                             :dry-run dry_run
                             :readtable readtable-designator)
          (if dry_run
              (let* ((preview (gethash "preview" updated))
                     (would-change (eq t (gethash "would_change" updated)))
                     (original-form (gethash "original" updated))
                     (summary (format nil "Dry-run patch on ~A ~A in ~A (~:[no change~;would change~])~
                                      ~%~%--- original ---~%~A~%~%--- preview ---~%~A"
                                      form_type form_name file_path would-change
                                      original-form preview)))
                (result id
                        (make-ht "path" file_path
                                 "operation" "patch"
                                 "form_type" form_type
                                 "form_name" form_name
                                 "would_change" (json-bool would-change)
                                 "original" original-form
                                 "preview" preview
                                 "content" (text-content summary))))
              (let ((summary
                     (if (not changed-p)
                         (format nil "No change to ~A ~A in ~A (old_text already matches new_text)"
                                 form_type form_name file_path)
                         (format nil "Applied patch to ~A ~A in ~A (~D chars → ~D chars)"
                                 form_type form_name file_path
                                 (length old_text) (length new_text)))))
                (result id
                        (apply #'make-ht
                               "path" file_path
                               "form_type" form_type
                               "form_name" form_name
                               "would_change" (json-bool changed-p)
                               "bytes" (length updated)
                               "content" (text-content summary)
                               (when changed-p
                                 (list "delta" (- (length new_text) (length old_text)))))))))
      (patch-operation-error (e)
        (tool-error id
                    (sanitize-for-json
                     (sanitize-error-message (format nil "~A" e)))
                    :protocol-version (protocol-version state))))))
