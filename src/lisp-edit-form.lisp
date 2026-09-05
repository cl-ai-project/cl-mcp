;;;; src/lisp-edit-form.lisp

(defpackage #:cl-mcp/src/lisp-edit-form
  (:use #:cl)
  (:shadowing-import-from #:cl-mcp/src/cst
                          #:cst-node
                          #:cst-node-kind
                          #:cst-node-value
                          #:cst-node-start
                          #:cst-node-end)
  (:import-from #:cl-mcp/src/cst
                #:%skip-whitespace-and-comments
                #:stray-right-parenthesis
                #:*standard-readtable*)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis
                #:repair-line-differences
                #:format-repair-lines
                #:format-bracket-warning
                #:opener-ambiguous-p
                #:format-opener-caveat
                #:format-relocation-note
                #:scan-delimiters)
  (:import-from #:cl-mcp/src/state
                #:protocol-version)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content
                #:arg-validation-error #:json-bool #:tool-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message
                #:sanitize-condition-text
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/utils/strings
                #:ensure-trailing-newline)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%resolve-named-readtable
                #:%nonstandard-readtable-p
                #:%parse-readtable-designator
                #:%whitespace-char-p
                #:%locate-target-form
                #:%reader-level-failure-p
                #:%detect-readtable-before-node
                #:file-unparseable-error)
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

(define-condition content-unrepairable-error (error)
  ((message :initarg :message :reader content-unrepairable-message))
  (:report (lambda (c s) (write-string (content-unrepairable-message c) s)))
  (:documentation "Signaled when CONTENT is unbalanced and parinfer cannot make it readable."))

(defun %repair-warning (fixes &optional repaired nonstandard-rt)
  "Describe FIXES (from REPAIR-LINE-DIFFERENCES) as a parinfer warning string,
or NIL when there are none. Added and dropped closing delimiters are summed
from each fix's gross :added and :removed counts (not the net :delta, which
hides a relocation such as \")(defun f () 1\" -> \"(defun f () 1)\") and
reported separately; the count is never negative. When REPAIRED (the
repaired content) still opens a [ or { that never closes, the warning ends
with FORMAT-OPENER-CAVEAT -- the same sentence lisp-check-parens prints --
naming the bracket's position and saying the ) fixes are wrong if it was
meant as (. That is a standard-syntax verdict, so it is not given under a
readtable that changes the syntax (NONSTANDARD-RT)."
  (when fixes
    (let* ((added (loop for fix in fixes sum (getf fix :added 0)))
           (dropped (loop for fix in fixes sum (getf fix :removed 0)))
           (scan (and repaired (not nonstandard-rt) (scan-delimiters repaired)))
           (caveat (and scan (opener-ambiguous-p scan)
                        (format-opener-caveat scan :action "edit"))))
      (format nil "~{~A~^; ~}~@[. ~A~]"
              (remove nil
                      (list (when (plusp added)
                              (format nil "~D closing delimiter~:P added by parinfer"
                                      added))
                            (when (plusp dropped)
                              (format nil "~D extra closing delimiter~:P dropped by ~
                                           parinfer"
                                      dropped))))
              caveat))))

(defun %bracket-warning (text nonstandard-rt)
  "Return the shared bracket warning (FORMAT-BRACKET-WARNING) for TEXT, or
NIL under a readtable that changes the syntax (NONSTANDARD-RT), where the
scan is not evidence."
  (unless nonstandard-rt
    (format-bracket-warning text :target "the content")))

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
using parinfer:apply-indent-mode. Returns four values: the validated
(possibly repaired) content, a parinfer warning string or NIL, the repair
line diff or NIL, and a bracket warning (FORMAT-BRACKET-WARNING) or NIL.
When READTABLE-DESIGNATOR is provided, use that named-readtable for parsing.
Unknown package prefixes are handled leniently via stub packages.

As a convenience, CONTENT consisting entirely of comments (and whitespace)
is accepted verbatim. This allows `replace' to delete a form by replacing
it with a `;; removed' comment marker, and `insert_*' to place bare
comments near a target form."
  (let* ((*read-eval* nil)
         (custom-rt (%resolve-named-readtable readtable-designator))
         (*readtable* (if custom-rt custom-rt (copy-readtable nil)))
         ;; Standard-syntax verdicts are withheld only for a readtable that
         ;; really changes the syntax; :standard (or a plain copy of it) must
         ;; not become a loophole around the ] refusal.
         (nonstandard-rt (%nonstandard-readtable-p readtable-designator)))
    (labels ((whitespace-char-p (ch)
               (member ch '(#\Space #\Tab #\Newline #\Return)))
             (comment-only-p (text)
               ;; Return T when TEXT contains at least one `;' line comment
               ;; or `#|...|#' block comment and NO readable forms.
               (and (stringp text)
                    (some (lambda (ch) (not (whitespace-char-p ch))) text)
                    (handler-case
                        (multiple-value-bind (form pos)
                            (read-from-string text nil :eof)
                          (declare (ignore pos))
                          (eq form :eof))
                      (error () nil))
                    ;; Require that a `;' or `#|' token is actually present
                    ;; so that mis-balanced junk doesn't accidentally pass.
                    (or (find #\; text)
                        (search "#|" text))))
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
             (stray-close-check (text)
               ;; The same structural evidence cst uses: after whitespace and
               ;; comments, a ) where a form should start is a stray ) -- a
               ;; delimiter failure by condition type, not by the reader's
               ;; wording (SBCL's own unmatched-close error is a plain
               ;; reader-error that %DELIMITER-FAILURE-P cannot recognise).
               ;; An open #| comment reported by the skip is a delimiter
               ;; failure too. Skipped when the readtable changes what )
               ;; means.
               (when (eq (get-macro-character #\) *readtable*)
                         (get-macro-character #\) *standard-readtable*))
                 (with-input-from-string (s text)
                   (let ((open-comment (%skip-whitespace-and-comments s *readtable*)))
                     (when open-comment
                       (error open-comment))
                     (when (eql (peek-char nil s nil :eof) #\))
                       (error 'stray-right-parenthesis
                              :stream s
                              :message "Unmatched closing parenthesis character )."))))))
             (try-parse (text)
               (handler-case
                   (call-with-package-context
                    package-name
                    (lambda ()
                      (stray-close-check text)
                      (multiple-value-bind (form pos)
                          (read-from-string text nil :eof)
                        (when (eq form :eof)
                          (if (comment-only-p text)
                              (return-from try-parse text)
                              (error "content is empty")))
                        (let* ((len (length text))
                               (rest-start
                                 (or (position-if-not #'whitespace-char-p
                                                      text :start pos)
                                     len)))
                          (when (< rest-start len)
                            (cond
                              ;; A trailing comment after the form is part
                              ;; of the content, not malformed text.
                              ((comment-only-p (subseq text rest-start)) nil)
                              ((rest-parses-as-complete-forms-p text rest-start)
                               (error 'multiple-top-level-forms-error))
                              (t
                               (error "content has trailing malformed characters ~
                                       after the first form")))))
                        text))
                    :source-path source-path)
                 (error (e)
                   (values nil e)))))
      (when (comment-only-p content)
        (return-from %validate-and-repair-content (values content nil nil nil)))
      (multiple-value-bind (result err)
          (try-parse content)
        (if result
            (values result nil nil (%bracket-warning result nonstandard-rt))
            (let ((diagnosis (diagnose-delimiters content)))
              ;; Under a custom readtable the standard delimiter scan is not
              ;; trustworthy (a reader macro may consume raw parentheses as
              ;; data), so its verdicts are not used to refuse or explain;
              ;; only the reader's own outcome counts then.
              ;; An unmatched [ or { (EXPECTED "]" or "}") is never grounds
              ;; for refusal: it may be a symbol character, in which case
              ;; parinfer's output reads fine and is written as before.
              ;; A repair rejected because it would change text inside a
              ;; string or comment (:outside-code) is refused even for an
              ;; ambiguous opener: what the tool would not suggest, it does
              ;; not write.
              (when (and (not nonstandard-rt)
                         (not (getf diagnosis :ok))
                         (getf diagnosis :repair-failed)
                         (or (eq (getf diagnosis :repair-failed) :outside-code)
                             (not (opener-ambiguous-p diagnosis))))
                ;; Keep the reader's own error too: for an ambiguous [ or ]
                ;; the scan may be a false positive, and the reader error
                ;; (an unknown #? macro, say) is then the actionable part.
                ;; Sanitized so no SBCL stream object reaches the client.
                ;; When the reader stopped on something other than a
                ;; delimiter (a disabled #., an unknown #?), the bracket may
                ;; well be a symbol character: describe it, but do not
                ;; instruct; the reader's own complaint is the actionable part.
                (error 'content-unrepairable-error
                       :message (format nil "~A (reader: ~A)"
                                        (format-delimiter-diagnosis
                                         diagnosis :target "content"
                                                   :false-positive
                                                   (%reader-level-failure-p err))
                                        (sanitize-condition-text err))))
              ;; Parinfer already ran inside DIAGNOSE-DELIMITERS when the
              ;; scan found a delimiter problem; reuse its output. Only a
              ;; balanced text that still fails to read runs it here.
              (let ((repaired (or (getf diagnosis :repaired)
                                  (apply-indent-mode content))))
                (multiple-value-bind (repaired-result repaired-err)
                    (try-parse repaired)
                  (cond
                    (repaired-result
                     (log-event :info "lisp.edit.form" "auto-repair" "success"
                                "original-error" (princ-to-string err))
                     (let ((fixes (repair-line-differences content repaired)))
                       (values repaired-result
                               (%repair-warning fixes repaired-result nonstandard-rt)
                               fixes
                               (%bracket-warning repaired-result nonstandard-rt))))
                    ((and (typep err 'multiple-top-level-forms-error)
                          (typep repaired-err 'multiple-top-level-forms-error))
                     (error err))
                    ((and (not nonstandard-rt) (not (getf diagnosis :ok)))
                     ;; Keep the reader error too: a paren problem often hides
                     ;; a second, unrelated read error that the user still
                     ;; needs -- unless the finding is an open string or
                     ;; comment, which the reader's "end of input" would only
                     ;; restate. A reader stopped elsewhere makes the
                     ;; bracket verdict a finding, not an instruction.
                     (error 'content-unrepairable-error
                            :message
                            (format nil "~A~@[ (repair also failed: ~A)~]"
                                    (format-delimiter-diagnosis
                                     diagnosis :target "content"
                                               :false-positive
                                               (%reader-level-failure-p repaired-err))
                                    (and (not (member (getf diagnosis :kind)
                                                      '("unclosed-string"
                                                        "unclosed-block-comment")
                                                      :test #'string=))
                                         (sanitize-condition-text repaired-err)))))
                    (t
                     (error "content parse error: ~A (repair also failed: ~A)"
                            (sanitize-condition-text err)
                            (sanitize-condition-text repaired-err))))))))))))

(defun %apply-operation-preserve-spacing (text node operation content)
  (let ((start (cst-node-start node))
        (end (cst-node-end node)))
    (ecase operation
      ((:replace)
       (concatenate 'string (subseq text 0 start) content (subseq text end)))
      ((:insert-before)
       (let* ((snippet (ensure-trailing-newline content))
              (prefix (subseq text 0 start))
              (sep
               (if (zerop start)
                   ""
                   (%ensure-blank-separation prefix ""))))
         (concatenate 'string prefix sep snippet (subseq text start))))
      ((:insert-after)
       (let* ((snippet (ensure-trailing-newline content))
              (suffix (subseq text end))
              (ws-end
               (or
                (position-if-not
                 (lambda (ch) (member ch '(#\Space #\Tab #\Newline #\Return)))
                 suffix)
                (length suffix)))
              (between
               (%ensure-blank-separation (subseq text 0 end)
                                         (subseq suffix 0 ws-end)))
              (rest (subseq suffix ws-end))
              (prefix (subseq text 0 end)))
         (concatenate 'string prefix between snippet rest)))
      ((:delete)
       (let* ((suffix (subseq text end))
              (ws-end
               (or
                (position-if-not
                 (lambda (ch) (member ch '(#\Space #\Tab #\Newline #\Return)))
                 suffix)
                (length suffix))))
         (concatenate 'string (subseq text 0 start)
                      (subseq suffix ws-end)))))))

(defun %apply-operation-normalized (text node operation content)
  (let ((start (cst-node-start node))
        (end (cst-node-end node)))
    (ecase operation
      ((:replace)
       (let ((snippet (%trim-outer-whitespace content)))
         (multiple-value-bind (prefix-core _)
             (%split-trailing-whitespace (subseq text 0 start))
           (declare (ignore _))
           (multiple-value-bind (_ suffix-core)
               (%split-leading-whitespace (subseq text end))
             (declare (ignore _))
             (concatenate 'string prefix-core
                          (%normalized-separator prefix-core snippet) snippet
                          (%normalized-separator snippet suffix-core)
                          suffix-core)))))
      ((:insert-before)
       (let ((snippet (%trim-outer-whitespace content)))
         (multiple-value-bind (prefix-core _)
             (%split-trailing-whitespace (subseq text 0 start))
           (declare (ignore _))
           (let ((target (subseq text start end)) (suffix (subseq text end)))
             (concatenate 'string prefix-core
                          (%normalized-separator prefix-core snippet) snippet
                          (%normalized-separator snippet target) target
                          suffix)))))
      ((:insert-after)
       (let ((snippet (%trim-outer-whitespace content)))
         (multiple-value-bind (_ suffix-core)
             (%split-leading-whitespace (subseq text end))
           (declare (ignore _))
           (let ((prefix (subseq text 0 end)))
             (concatenate 'string prefix (%normalized-separator prefix snippet)
                          snippet (%normalized-separator snippet suffix-core)
                          suffix-core)))))
      ((:delete)
       (multiple-value-bind (prefix-core _)
           (%split-trailing-whitespace (subseq text 0 start))
         (declare (ignore _))
         (multiple-value-bind (_ suffix-core)
             (%split-leading-whitespace (subseq text end))
           (declare (ignore _))
           (cond
            ((and (zerop (length prefix-core))
                  (zerop (length suffix-core)))
             "")
            ((zerop (length prefix-core))
             suffix-core)
            ((zerop (length suffix-core))
             (concatenate 'string prefix-core (string #\Newline)))
            (t
             (concatenate 'string prefix-core
                          (%normalized-separator prefix-core suffix-core)
                          suffix-core)))))))))

(defun %apply-operation (text node operation content normalize-blank-lines)
  "Apply OPERATION to NODE within TEXT, optionally normalizing blank lines."
  (if normalize-blank-lines
      (%apply-operation-normalized text node operation content)
      (%apply-operation-preserve-spacing text node operation content)))

(defconstant +dry-run-snippet-limit+ 2048
  "Maximum characters of one form snippet inlined into a dry-run summary.")

(defun %truncate-snippet (text)
  "Return TEXT bounded to +DRY-RUN-SNIPPET-LIMIT+ characters for summary text.
Longer input is cut at the limit and annotated with the number of characters
dropped, so a dry-run summary never echoes an unbounded amount of source.
Non-string input (a missing key) is returned unchanged."
  (if (and (stringp text) (> (length text) +dry-run-snippet-limit+))
      (concatenate 'string
                   (subseq text 0 +dry-run-snippet-limit+)
                   (format nil "~%... [~D more characters truncated]"
                           (- (length text) +dry-run-snippet-limit+)))
      text))

(defun %preview-form-text (operation content normalize-blank-lines)
  "Return the form text OPERATION splices into the file, for dry-run previews.
CONTENT is the validated (possibly parinfer-repaired) replacement text, so the
result is exactly what %APPLY-OPERATION writes at the edit site. This lets a
dry-run summary show the edited form instead of the whole updated file.
:DELETE writes no form, so a short marker is returned instead."
  (ecase operation
    ((:delete) "(form removed)")
    ((:replace)
     (if normalize-blank-lines
         (%trim-outer-whitespace content)
         content))
    ((:insert-before :insert-after)
     (if normalize-blank-lines
         (%trim-outer-whitespace content)
         (ensure-trailing-newline content)))))

(defun %repair-summary (warning fixes repaired-form &key include-form)
  "Return the text appended to a success summary when parinfer repaired the
content, or NIL when WARNING is NIL. Lists the changed lines and, when
INCLUDE-FORM is true, the repaired form itself (bounded by %TRUNCATE-SNIPPET).
The relocation note (FORMAT-RELOCATION-NOTE: a closer inserted on a line
whose next code line sits at the same indentation) is the one
lisp-check-parens prints, so the two tools describe the same repair in the
same words. The bracket-opener reminder is part of WARNING, built by
%REPAIR-WARNING where the readtable is known."
  (when warning
    (with-output-to-string (s)
      (format s "~%WARNING: ~A" warning)
      (when fixes
        (format s "~%Changed lines:~A" (format-repair-lines fixes)))
      (let ((note (format-relocation-note fixes repaired-form)))
        (when note
          (format s "~%~A" note)))
      (when include-form
        (format s "~%~%--- repaired form ---~%~A" (%truncate-snippet repaired-form))))))

(defun lisp-edit-form
       (&key file-path form-type form-name operation content dry-run
        (normalize-blank-lines t) readtable)
  "Structured edit of a top-level Lisp form.
FILE-PATH may be absolute or relative to the project root. FORM-TYPE,
FORM-NAME, and OPERATION are always required. CONTENT is required for
replace/insert_before/insert_after but ignored for delete.

OPERATION must be one of: \"replace\", \"insert_before\", \"insert_after\", \"delete\".
Missing closing parentheses are auto-repaired using parinfer (non-delete ops).

When DRY-RUN is true, no changes are written; a preview hash-table is returned.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing both the file and the new content.

For non-delete operations without DRY-RUN, returns six values: the updated
file text, the parinfer warning or NIL, whether the file changed, the repair
line diff or NIL, the validated content that was spliced in, and a bracket
warning (a ] or } found where ) was expected, in content that still reads)
or NIL."
  (unless
      (and (stringp file-path) (stringp form-type) (stringp form-name)
           (stringp operation))
    (error "file_path, form_type, form_name, and operation must be strings"))
  (unless (member dry-run '(t nil)) (error "dry-run must be boolean"))
  (unless (member normalize-blank-lines '(t nil))
    (error "normalize-blank-lines must be boolean"))
  (let* ((op-normalized (string-downcase operation))
         (op-key
          (cond ((string= op-normalized "replace") :replace)
                ((string= op-normalized "insert_before") :insert-before)
                ((string= op-normalized "insert_after") :insert-after)
                ((string= op-normalized "delete") :delete)
                (t (error "Unsupported operation: ~A" operation)))))
    (unless (or (eq op-key :delete) (stringp content))
      (error "content is required for ~A operation" operation))
    (multiple-value-bind
        (abs rel original nodes target target-snippet _ file-package-name)
        (%locate-target-form file-path form-type form-name readtable)
      (declare (ignore _))
      (if (eq op-key :delete)
          ;; Delete path: no content validation needed
          (let* ((updated
                  (%apply-operation original target op-key nil
                                    normalize-blank-lines))
                 (would-change (not (string= original updated))))
            (log-event :debug "lisp.edit.form" "path" (namestring abs)
                       "operation" op-normalized "form_type" form-type
                       "form_name" form-name "normalize_blank_lines"
                       normalize-blank-lines "bytes" (length updated) "dry_run"
                       dry-run "would_change" would-change)
            (cond
             (dry-run
              (let ((result (make-hash-table :test #'equal)))
                (setf (gethash "would_change" result) would-change
                      (gethash "original" result) target-snippet
                      (gethash "preview" result) updated
                      (gethash "preview_form" result)
                      (%preview-form-text op-key nil normalize-blank-lines)
                      (gethash "file_path" result) (namestring abs)
                      (gethash "operation" result) op-normalized)
                result))
             (would-change (fs-write-file rel updated)
              (values updated nil t))
             (t (values updated nil nil))))
          ;; Non-delete path: validate and repair content
          ;; Content is validated under the readtable in effect at the target:
          ;; the caller's argument, or an (in-readtable ...) earlier in the
          ;; file, as lisp-patch-form does.
          (multiple-value-bind (validated-content parinfer-warning repair-fixes
                                bracket-warning)
              (%validate-and-repair-content
               content
               (or readtable (%detect-readtable-before-node nodes target))
               file-package-name abs)
            (let* ((updated
                    (%apply-operation original target op-key validated-content
                                      normalize-blank-lines))
                   (would-change (not (string= original updated))))
              (log-event :debug "lisp.edit.form" "path" (namestring abs)
                         "operation" op-normalized "form_type" form-type
                         "form_name" form-name "normalize_blank_lines"
                         normalize-blank-lines "bytes" (length updated) "dry_run"
                         dry-run "would_change" would-change)
              (cond
               (dry-run
                (let ((result (make-hash-table :test #'equal)))
                  (setf (gethash "would_change" result) would-change
                        (gethash "original" result) target-snippet
                        (gethash "preview" result) updated
                        (gethash "preview_form" result)
                        (%preview-form-text op-key validated-content
                                            normalize-blank-lines)
                        ;; The untrimmed content the repair line numbers
                        ;; refer to, for the relocation note in the summary.
                        (gethash "validated_content" result) validated-content
                        (gethash "file_path" result) (namestring abs)
                        (gethash "operation" result) op-normalized)
                  (when parinfer-warning
                    (setf (gethash "parinfer_warning" result) parinfer-warning
                          (gethash "repair_fixes" result) repair-fixes))
                  (when bracket-warning
                    (setf (gethash "bracket_warning" result) bracket-warning))
                  result))
               (would-change (fs-write-file rel updated)
                (values updated parinfer-warning t repair-fixes validated-content
                        bracket-warning))
               (t (values updated parinfer-warning nil repair-fixes
                          validated-content bracket-warning)))))))))

(define-tool "lisp-edit-form"
  :description "Structure-aware edit of a top-level Lisp form using Eclector CST parsing.
Supports replace, insert_before, insert_after, and delete operations while preserving
formatting and comments.
PREFERRED METHOD for editing existing Lisp source code.
Automatically repairs missing closing parentheses using parinfer (non-delete ops).
ALWAYS use this tool instead of 'fs-write-file' when modifying Lisp forms to ensure
safety and structure preservation."
  :args ((file_path :type :string :required t
                    :description "Target file path (absolute recommended)")
         (form_type :type :string :required t
                    :description "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\"")
         (form_name :type :string :required t
                    :description "Form name to match; for defmethod include specializers,
e.g., \"print-object ((obj my-class) stream)\". For defstruct with
options \"(defstruct (name opts...) ...)\", use just the bare struct name.
Reader macro prefixes #: and : are stripped automatically, so
\"#:my-pkg\" and \"my-pkg\" both match \"(defpackage #:my-pkg ...).\"")
         (operation :type :string :required t
                    :enum ("replace" "insert_before" "insert_after" "delete")
                    :description "Operation to perform")
         (content :type :string
                  :description "Full Lisp form for the operation. Required for replace/insert_before/insert_after.
Ignored for delete. Must contain exactly ONE top-level form.
Missing closing parentheses are automatically repaired using parinfer.")
         (dry_run :type :boolean
                  :description "When true, return a preview without writing to disk")
         (normalize_blank_lines :type :boolean
                                :default t
                                :description "When true (default), normalize blank lines around edited top-level forms.
Applies to replace, insert_before, insert_after, and delete operations.")
         (readtable :type :string
                    :description "Named-readtable designator for files using custom reader macros.
Supports both keyword style ('interpol-syntax') and package-qualified style
('pokepay-syntax:pokepay-syntax'). NOTE: When specified, the standard CL reader
is used instead of Eclector, which means comments are NOT preserved."))
  :body
  (progn
    (when (and (not content) (string/= (string-downcase operation) "delete"))
      (error 'arg-validation-error :arg-name "content"
             :message (format nil "content is required for ~A operation" operation)))
    (handler-case
        (multiple-value-bind (updated parinfer-warning changed-p repair-fixes
                              repaired-form bracket-warning)
            (lisp-edit-form :file-path file_path
                            :form-type form_type
                            :form-name form_name
                            :operation operation
                            :content content
                            :dry-run dry_run
                            :normalize-blank-lines normalize_blank_lines
                            :readtable (%parse-readtable-designator readtable))
          (if dry_run
              ;; The summary inlines only the edited FORM (preview_form), never
              ;; the whole updated file: "preview" holds the full file and is
              ;; kept as a sibling JSON field for backward compatibility. The
              ;; relocation note is computed against the untrimmed content the
              ;; repair line numbers refer to, not the trimmed preview form.
              (let* ((preview (gethash "preview" updated))
                     (preview-form (gethash "preview_form" updated))
                     (would-change (eq t (gethash "would_change" updated)))
                     (original-form (gethash "original" updated))
                     (pw (gethash "parinfer_warning" updated))
                     (bw (gethash "bracket_warning" updated))
                     (summary
                      (format nil "Dry-run ~A on ~A ~A in ~A (~:[no change~;would change~])~
                                   ~@[~A~]~@[~%WARNING: ~A~]~
                                   ~@[~%~%--- original ---~%~A~]~
                                   ~@[~%~%--- preview ---~%~A~]"
                              operation form_type form_name file_path would-change
                              (%repair-summary pw (gethash "repair_fixes" updated)
                                               (or (gethash "validated_content" updated)
                                                   preview-form))
                              bw
                              (%truncate-snippet original-form)
                              (%truncate-snippet preview-form))))
                (result id
                        (apply #'make-ht
                               "path" file_path
                               "operation" operation
                               "form_type" form_type
                               "form_name" form_name
                               "would_change" (json-bool would-change)
                               "original" original-form
                               "preview" preview
                               "preview_form" preview-form
                               "content" (text-content summary)
                               (append
                                (when pw
                                  (list "parinfer_warning" pw))
                                (when bw
                                  (list "bracket_warning" bw))))))
              (let ((summary
                     (cond
                       ((not changed-p)
                        (format nil
                                "No change to ~A ~A in ~A (content matches existing form)~
                                 ~@[~A~]~@[~%WARNING: ~A~]"
                                form_type form_name file_path
                                (%repair-summary parinfer-warning repair-fixes
                                                 repaired-form :include-form t)
                                bracket-warning))
                       (t
                        (format nil "Applied ~A to ~A ~A in ~A (~D chars)~@[~A~]~
                                     ~@[~%WARNING: ~A~]"
                                operation form_type form_name file_path (length updated)
                                (%repair-summary parinfer-warning repair-fixes
                                                 repaired-form :include-form t)
                                bracket-warning)))))
                (result id
                        (apply #'make-ht
                               "path" file_path
                               "operation" operation
                               "form_type" form_type
                               "form_name" form_name
                               "would_change" (json-bool changed-p)
                               "bytes" (length updated)
                               "content" (text-content summary)
                               (when bracket-warning
                                 (list "bracket_warning" bracket-warning)))))))
      (content-unrepairable-error (e)
        (tool-error id (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
      (file-unparseable-error (e)
        (tool-error id (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
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
