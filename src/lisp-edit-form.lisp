;;;; src/lisp-edit-form.lisp

(defpackage #:cl-mcp/src/lisp-edit-form
  (:use #:cl)
  (:shadowing-import-from #:cl-mcp/src/cst
                          #:cst-node
                          #:cst-node-kind
                          #:cst-node-value
                          #:cst-node-start
                          #:cst-node-end)
  (:import-from #:alexandria
                #:string-trim)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/state
                #:protocol-version)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content #:tool-error)
  (:import-from #:cl-mcp/src/tools/registry
                #:register-tool)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:enough-pathname
                #:native-namestring
                #:subpathp)
  (:export #:lisp-edit-form))

(in-package #:cl-mcp/src/lisp-edit-form)

(defun %normalize-string (thing)
  (string-downcase (princ-to-string thing)))

(defun %defmethod-candidates (form)
  "Return candidate signature strings for a DEFMETHOD FORM."
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
            (qual-str (and qualifiers
                           (%normalize-string
                            (format nil "~{~A~^ ~}" (nreverse qualifiers))))))
        (remove nil
                (list name-str
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

(defun %ensure-trailing-newline (string)
  (if (and (> (length string) 0)
           (char= (char string (1- (length string))) #\Newline))
      string
      (concatenate 'string string (string #\Newline))))

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

(defun %normalize-paths (file-path)
  "Return two values: absolute path (pathname) and relative namestring for FS tools."
  (let ((resolved (fs-resolve-read-path file-path))
        (root (ensure-directory-pathname *project-root*)))
    (unless (subpathp resolved root)
      (error "Write path ~A is outside project root ~A" file-path root))
    (let* ((relative (enough-pathname resolved root))
           (rel-namestring (native-namestring relative)))
      (values resolved rel-namestring))))

(defun %validate-and-repair-content (content)
  "Ensure CONTENT is a single valid form. If parsing fails, attempt to repair
using parinfer:apply-indent-mode. Returns the validated (possibly repaired) content."
  (let ((*read-eval* nil)
        (*readtable* (copy-readtable nil)))
    (flet ((try-parse (text)
             (handler-case
                 (multiple-value-bind (form pos)
                     (cl:read-from-string text nil :eof)
                   (when (eq form :eof)
                     (error "content is empty"))
                   (let ((rest (string-trim '(#\Space #\Tab #\Newline)
                                            (subseq text pos))))
                     (when (> (length rest) 0)
                       (error "content contains extra characters after the first form")))
                   text)
               (error (e)
                 (values nil e)))))
      (multiple-value-bind (result err)
          (try-parse content)
        (if result
            result
            ;; First parse failed, try parinfer repair
            (let ((repaired (apply-indent-mode content)))
              (multiple-value-bind (repaired-result repaired-err)
                  (try-parse repaired)
                (cond
                  (repaired-result
                   (log-event :info "lisp-edit-form"
                              "auto-repair" "success"
                              "original-error" (princ-to-string err))
                   repaired-result)
                  (t
                   ;; Repair failed, signal the original error
                   (error "content parse error: ~A (repair also failed: ~A)"
                          err repaired-err))))))))))

(defun %find-target (nodes form-type form-name)
  (let ((target (string-downcase form-name)))
    (loop for node in nodes
          when (and (typep node 'cst-node)
                    (eq (cst-node-kind node) :expr))
            do (let ((value (cst-node-value node)))
                 (when (and (consp value)
                            (string= (string-downcase (symbol-name (car value))) form-type)
                            (some (lambda (cand) (string= cand target))
                                  (%definition-candidates value form-type)))
                   (return node))))))

(defun %apply-operation (text node operation content)
  (let ((start (cst-node-start node))
        (end (cst-node-end node))
        (snippet (ecase operation
                   ((:replace) content)
                   ((:insert-before :insert-after) (%ensure-trailing-newline content)))))
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

(defun lisp-edit-form (&key file-path form-type form-name operation content dry-run)
  "Structured edit of a top-level Lisp form.
FILE-PATH may be absolute or relative to the project root. FORM-TYPE,
FORM-NAME, OPERATION (\"replace\" | \"insert_before\" | \"insert_after\"), and
CONTENT are required. If CONTENT has missing closing parentheses, they will
be automatically added using parinfer. When DRY-RUN is true, no changes are
written; instead, a preview hash-table is returned."
  (unless (and (stringp file-path) (stringp form-type) (stringp form-name)
               (stringp operation) (stringp content))
    (error "All parameters (file_path, form_type, form_name, operation, content) must be strings"))
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (let* ((op-normalized (string-downcase operation))
         (op-key (cond
                   ((string= op-normalized "replace") :replace)
                   ((string= op-normalized "insert_before") :insert-before)
                   ((string= op-normalized "insert_after") :insert-after)
                   (t (error "Unsupported operation: ~A" operation))))
         (form-type-str (string-downcase form-type))
         (validated-content (%validate-and-repair-content content)))
    (multiple-value-bind (abs rel) (%normalize-paths file-path)
      (let* ((original (fs-read-file abs))
             (nodes (parse-top-level-forms original))
             (target (%find-target nodes form-type-str form-name)))
        (unless target
          (error "Form ~A ~A not found in ~A" form-type form-name abs))
        (let* ((start (cst-node-start target))
               (end (cst-node-end target))
               (target-snippet (subseq original start end))
               (updated (%apply-operation original target op-key validated-content))
               (would-change (not (string= original updated))))
          (log-event :debug "lisp-edit-form" "path" (namestring abs)
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
            (t
             (fs-write-file rel updated)
             updated)))))))

(defun lisp-edit-form-descriptor ()
  "Return the MCP tool descriptor for lisp-edit-form."
  (make-ht
   "name" "lisp-edit-form"
   "description"
   "Structure-aware edit of a top-level Lisp form using Eclector CST parsing.
Supports replace, insert_before, and insert_after while preserving formatting and comments.
PREFERRED METHOD for editing existing Lisp source code - automatically repairs missing closing
parentheses using parinfer.
ALWAYS use this tool instead of 'fs-write-file' when modifying Lisp forms to ensure
safety and structure preservation."
   "inputSchema"
   (let ((p (make-hash-table :test #'equal)))
     (setf (gethash "file_path" p)
           (make-ht "type" "string"
                    "description"
                    "Target file path (absolute recommended)"))
     (setf (gethash "form_type" p)
           (make-ht "type" "string"
                    "description"
                    "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\""))
     (setf (gethash "form_name" p)
           (make-ht "type" "string"
                    "description"
                    "Form name to match; for defmethod include specializers,
e.g., \"print-object (my-class t)\""))
     (setf (gethash "operation" p)
           (make-ht "type" "string"
                    "enum" (vector "replace" "insert_before" "insert_after")
                    "description"
                    "Operation to perform"))
     (setf (gethash "content" p)
           (make-ht "type" "string"
                    "description"
                    "Full Lisp form to insert or replace with"))
     (setf (gethash "dry_run" p)
           (make-ht "type" "boolean"
                    "description"
                    "When true, return a preview without writing to disk"))
     (make-ht "type" "object"
              "properties" p
              "required" (vector "file_path" "form_type" "form_name"
                                 "operation" "content")))))

(defun %lisp-edit-form-dry-run-result (id path form-type form-name operation updated)
  "Format dry-run result for lisp-edit-form."
  (let* ((preview (gethash "preview" updated))
         (would-change (gethash "would_change" updated))
         (original-form (gethash "original" updated))
         (summary (format nil "Dry-run ~A on ~A ~A (~:[no change~;would change~])"
                          operation form-type path would-change)))
    (result id
            (make-ht "path" path
                     "operation" operation
                     "form_type" form-type
                     "form_name" form-name
                     "would_change" would-change
                     "original" original-form
                     "preview" preview
                     "content" (text-content summary)))))

(defun %lisp-edit-form-apply-result (id path form-type form-name operation updated)
  "Format apply result for lisp-edit-form."
  (result id
          (make-ht "path" path
                   "operation" operation
                   "form_type" form-type
                   "form_name" form-name
                   "bytes" (length updated)
                   "content" (text-content
                              (format nil "Applied ~A to ~A ~A (~D chars)"
                                      operation form-type path (length updated))))))

(defun lisp-edit-form-handler (state id args)
  "Handle the lisp-edit-form MCP tool call."
  (handler-case
      (let ((path (and args (gethash "file_path" args)))
            (form-type (and args (gethash "form_type" args)))
            (form-name (and args (gethash "form_name" args)))
            (operation (and args (gethash "operation" args)))
            (content (and args (gethash "content" args)))
            (dry-run-present nil)
            (dry-run (multiple-value-bind (val presentp)
                        (and args (gethash "dry_run" args))
                      (setf dry-run-present presentp)
                      (if presentp val nil))))
        (unless (and (stringp path) (stringp form-type) (stringp form-name)
                     (stringp operation) (stringp content))
          (return-from lisp-edit-form-handler
            (tool-error id "file_path, form_type, form_name, operation, and content must be strings"
                        :protocol-version (protocol-version state))))
        (when (and dry-run-present (not (member dry-run '(t nil))))
          (return-from lisp-edit-form-handler
            (tool-error id "dry_run must be boolean"
                        :protocol-version (protocol-version state))))
        (let ((updated (lisp-edit-form :file-path path
                                       :form-type form-type
                                       :form-name form-name
                                       :operation operation
                                       :content content
                                       :dry-run dry-run)))
          (if dry-run
              (%lisp-edit-form-dry-run-result id path form-type form-name operation updated)
              (%lisp-edit-form-apply-result id path form-type form-name operation updated))))
    (error (e)
      (rpc-error id -32603
                 (format nil "Internal error during lisp-edit-form: ~A" e)))))

;;; Tool Registration

(register-tool "lisp-edit-form"
               (lisp-edit-form-descriptor)
               #'lisp-edit-form-handler)
