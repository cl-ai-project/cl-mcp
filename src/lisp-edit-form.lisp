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
                #:fs-write-file
                #:with-file-lock)
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
                #:reparented-forms
                #:format-reparent-note
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
  (:import-from #:cl-mcp/src/utils/nesting
                #:check-lisp-nesting)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%resolve-named-readtable
                #:%nonstandard-readtable-p
                #:%parse-readtable-designator
                #:%whitespace-char-p
                #:%normalize-paths
                #:%locate-target-form
                #:%reader-level-failure-p
                #:%detect-readtable-before-node
                #:file-unparseable-error
                #:edit-guard-conflict-error
                #:edit-guard-conflict)
  (:documentation "Structure-aware editing of top-level Lisp forms.")
  (:export #:lisp-edit-form))

(in-package #:cl-mcp/src/lisp-edit-form)

(defun %multiple-top-level-forms-error-message ()
  "Return the user-facing error message for multiple top-level form content.
Only replace signals it: insert_before and insert_after take several forms."
  (concatenate 'string
               "replace takes exactly one top-level form; replace the form with the "
               "first one, then insert the rest with one insert_after on it "
               "(insert_before and insert_after take several forms)"))

(define-condition multiple-top-level-forms-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string (%multiple-top-level-forms-error-message) stream))))

(defun %multiple-top-level-forms-error-data ()
  "Return machine-readable remediation guidance for multiple-form content errors."
  (make-ht "code" "multiple_forms_not_supported"
           "next_tool" "lisp-edit-form"
           "action" "replace_then_insert_after"
           "example_operation_sequence" (vector "replace" "insert_after")
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
using parinfer:apply-indent-mode. Returns five values: the validated
(possibly repaired) content, a parinfer warning string or NIL, the repair
line diff or NIL, a bracket warning (FORMAT-BRACKET-WARNING) or NIL, and the
forms the repair moved out of the form CONTENT's own parens put them in
(REPARENTED-FORMS; always NIL under a readtable that changes the syntax).
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
                               (%bracket-warning repaired-result nonstandard-rt)
                               ;; The parens reading is a standard-syntax one:
                               ;; under a readtable that changes what ( and )
                               ;; mean it would name "moves" that are not.
                               (and (not nonstandard-rt)
                                    (reparented-forms content repaired-result)))))
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

(defun %block-spans (text package-name source-path)
  "Read TEXT as a sequence of complete top-level forms under the current
*READTABLE*, and return a list of (START . END), one per form, in order.
Whitespace and comments between the forms are not forms.  When any part of
TEXT does not read -- an unterminated form, a stray ), a disabled #., an
unterminated block comment -- return NIL and the condition as a second value:
a block that reads only in part is not a block of forms.

Each form is read by READ from the end of the previous one, so what counts as
whitespace or a comment is the readtable's own call.  START is where the form
itself begins only under standard gap syntax (%GAP-SYNTAX-STANDARD-P); under
any other readtable it is the previous form's END, the gaps are empty, and
nothing between the forms is ever rewritten."
  (let ((eof (list :eof))
        (scan-gaps (%gap-syntax-standard-p *readtable*)))
    (handler-case
        (call-with-package-context
         package-name
         (lambda ()
           (let ((spans '())
                 (pos 0))
             (loop
               (let ((start pos))
                 (when scan-gaps
                   (let ((stream (make-string-input-stream text pos)))
                     (let ((open-comment (%skip-whitespace-and-comments stream *readtable*)))
                       (when open-comment
                         (error open-comment)))
                     (incf start (file-position stream))))
                 (multiple-value-bind (form end)
                     (read-from-string text nil eof :start pos :preserve-whitespace t)
                   ;; Only whitespace and comments were left (or a trailing
                   ;; #+nil form, which reads as end of input).
                   (when (eq form eof)
                     (return (nreverse spans)))
                   (push (cons start end) spans)
                   (setf pos end))))))
         :source-path source-path)
      (error (e)
        (values nil e)))))

(defun %gap-segments (gap)
  "Split GAP, the text between two top-level forms read under standard gap
syntax, into (KIND START END) runs of :WHITESPACE or :COMMENT, the comments
being ; line comments and nested #| |# block comments.  Return NIL when GAP
holds anything else, so the caller keeps it as it is."
  (let ((len (length gap))
        (i 0)
        (segments '()))
    (flet ((pair-at-p (index first second)
             (and (< (1+ index) len)
                  (char= (char gap index) first)
                  (char= (char gap (1+ index)) second))))
      (loop while (< i len)
            do (let ((start i)
                     (ch (char gap i)))
                 (cond
                   ((%whitespace-char-p ch)
                    (loop while (and (< i len) (%whitespace-char-p (char gap i)))
                          do (incf i))
                    (push (list :whitespace start i) segments))
                   ((char= ch #\;)
                    (setf i (or (position #\Newline gap :start i) len))
                    (push (list :comment start i) segments))
                   ((pair-at-p i #\# #\|)
                    (let ((depth 0))
                      (loop while (< i len)
                            do (cond ((pair-at-p i #\# #\|)
                                      (incf depth)
                                      (incf i 2))
                                     ((pair-at-p i #\| #\#)
                                      (decf depth)
                                      (incf i 2)
                                      (when (zerop depth)
                                        (return)))
                                     (t (incf i))))
                      (unless (zerop depth)
                        (return-from %gap-segments nil)))
                    (push (list :comment start i) segments))
                   (t
                    (return-from %gap-segments nil))))))
    (nreverse segments)))

(defun %normalized-gap (gap)
  "Return GAP, the text between two top-level forms of an inserted block, with
its whitespace normalised and its comments kept in place.  Every comment that
starts on the previous form's line -- a ; comment, or a #| |# comment even when
it runs on over several lines -- stays right after that form.  The forms are
then separated by one blank line, and any other comments stay between them,
directly above the next form.  Only whitespace outside the comments changes;
a comment's own text, trailing spaces included, is copied as it is.  A gap %GAP-SEGMENTS cannot split is
returned unchanged."
  (let ((segments (%gap-segments gap)))
    (if (and (null segments) (plusp (length gap)))
        gap
        (let* ((first-newline
                 ;; A newline inside a block comment does not end the line the
                 ;; comment started on; only one in whitespace does.
                 (loop for (kind start end) in segments
                       for newline = (and (eq kind :whitespace)
                                          (position #\Newline gap :start start :end end))
                       when newline return newline))
               (same-line-end
                 ;; MAXIMIZE over no values is unspecified: start from 0.
                 (reduce #'max
                         (loop for (kind start end) in segments
                               when (and (eq kind :comment)
                                         (or (null first-newline) (< start first-newline)))
                                 collect end)
                         :initial-value 0))
               (same-line (if (plusp same-line-end) (subseq gap 0 same-line-end) ""))
               ;; The comments after that line, from the first one's start to
               ;; the last one's end: a comment keeps its own trailing spaces,
               ;; only the whitespace segments around them are dropped.
               (body-comments (loop for segment in segments
                                    when (and (eq (first segment) :comment)
                                              (>= (second segment) same-line-end))
                                      collect segment))
               (body (if body-comments
                         (subseq gap (second (first body-comments))
                                 (third (car (last body-comments))))
                         "")))
          (concatenate 'string same-line (format nil "~%~%")
                       (if (plusp (length body))
                           (concatenate 'string body (string #\Newline))
                           ""))))))

(defun %normalize-block-gaps (text spans)
  "Return TEXT with each gap between the top-level forms SPANS gives
normalised by %NORMALIZED-GAP.  The forms themselves, and the text before the
first and after the last, are copied unchanged, so nothing inside a form, a
string or a comment is touched."
  (with-output-to-string (out)
    (write-string text out :end (car (first spans)))
    (loop for (span . rest) on spans
          do (write-string text out :start (car span) :end (cdr span))
             (when rest
               (write-string (%normalized-gap (subseq text (cdr span) (car (first rest))))
                             out)))
    (write-string text out :start (cdr (car (last spans))))))

(defun %gap-syntax-standard-p (readtable)
  "True when READTABLE reads the text between two top-level forms the standard
way: Space, Tab, Newline, Return and Page are whitespace, and ; and #| are the
standard comments.  Only then may the gaps of an inserted block be scanned and
normalised.  Whitespace is tested by reading, not by GET-MACRO-CHARACTER: a
character can be neither a macro character nor whitespace (a Tab made a single
escape reads <Tab>1 as the symbol |1|, and dropping the Tab would make it 1)."
  (flet ((whitespace-p (ch)
           (and (null (get-macro-character ch readtable))
                (let ((*readtable* readtable)
                      (*read-suppress* t))
                  ;; A token stops at whitespace, so X ends at index 1.
                  (eql 1 (ignore-errors
                          (nth-value 1 (read-from-string (format nil "x~Cy" ch) nil nil
                                                         :preserve-whitespace t))))))))
    (and (every #'whitespace-p '(#\Space #\Tab #\Newline #\Return #\Page))
         (eq (get-macro-character #\; readtable)
             (get-macro-character #\; *standard-readtable*))
         (eq (ignore-errors (get-dispatch-macro-character #\# #\| readtable))
             (get-dispatch-macro-character #\# #\| *standard-readtable*)))))

(defun %repair-block (content nonstandard-rt package-name source-path)
  "Repair CONTENT as one block with the repair %VALIDATE-AND-REPAIR-CONTENT
uses, and read the result again.  Return the repaired text and its form spans
when it reads as complete forms, or NIL.  The same refusal applies as for one
form: under standard syntax, a delimiter problem the repair cannot fix (and
that is not an ambiguous [ or {) is not repaired at all."
  (let ((diagnosis (diagnose-delimiters content)))
    (unless (and (not nonstandard-rt)
                 (not (getf diagnosis :ok))
                 (getf diagnosis :repair-failed)
                 (or (eq (getf diagnosis :repair-failed) :outside-code)
                     (not (opener-ambiguous-p diagnosis))))
      (let ((repaired (or (getf diagnosis :repaired) (apply-indent-mode content))))
        (multiple-value-bind (spans read-error)
            (%block-spans repaired package-name source-path)
          (unless read-error
            (values repaired spans)))))))

(defun %validate-and-repair-block (content &optional readtable-designator
                                             package-name source-path)
  "Validate CONTENT for insert_before/insert_after, which take one or more
top-level forms.  Returns the five values of %VALIDATE-AND-REPAIR-CONTENT,
then the number of forms and, for a block of several, their spans in the
validated text.

A block that reads as complete forms is taken as given: parinfer never runs on
it, since it would change content that is accepted unchanged today.  One form,
comment-only content, and content that does not read but is one form once
repaired go through %VALIDATE-AND-REPAIR-CONTENT exactly as before.  Anything
else that does not read is repaired as a whole block and read again; it is
accepted only when every part of the result reads.  Under a readtable, a read
that stops partway is a failed read like any other, so the forms before the
error are never taken on their own."
  (let* ((*read-eval* nil)
         (custom-rt (%resolve-named-readtable readtable-designator))
         (*readtable* (if custom-rt custom-rt (copy-readtable nil)))
         (nonstandard-rt (%nonstandard-readtable-p readtable-designator)))
    (flet ((one-form ()
             (multiple-value-bind (validated warning fixes bracket reparented)
                 (%validate-and-repair-content content readtable-designator
                                               package-name source-path)
               (values validated warning fixes bracket reparented 1 nil))))
      (multiple-value-bind (spans read-error)
          (%block-spans content package-name source-path)
        (cond
          ((and (null read-error) (rest spans))
           (values content nil nil (%bracket-warning content nonstandard-rt) nil
                   (length spans) spans))
          ((null read-error)
           (one-form))
          (t
           (handler-case (one-form)
             (error (one-form-error)
               (multiple-value-bind (repaired repaired-spans)
                   (%repair-block content nonstandard-rt package-name source-path)
                 (unless (rest repaired-spans)
                   (error one-form-error))
                 (log-event :info "lisp.edit.form" "auto-repair" "success"
                            "forms" (length repaired-spans)
                            "original-error" (princ-to-string read-error))
                 (let ((fixes (repair-line-differences content repaired)))
                   (values repaired
                           (%repair-warning fixes repaired nonstandard-rt)
                           fixes
                           (%bracket-warning repaired nonstandard-rt)
                           (and (not nonstandard-rt)
                                (reparented-forms content repaired))
                           (length repaired-spans)
                           repaired-spans)))))))))))

(defun %skip-blank-and-comments (text start)
  "Return the index of the first character at or after START in TEXT that is
neither whitespace nor inside a comment, reading the standard syntax; START when
a block comment there is never closed."
  (with-input-from-string (in text :start start)
    (if (%skip-whitespace-and-comments in *standard-readtable*)
        start
        ;; A string input stream counts its position from START.
        (+ start (file-position in)))))

(defun %feature-prefix-end (text start)
  "Return the index in TEXT where the form at START begins once the #+ and #-
feature expressions in front of it are passed, with the whitespace and comments
after each; START when there are none.  The expressions are read with
*READ-SUPPRESS* on, which interns nothing, and under the standard syntax, as
feature expressions are read whatever the file's readtable."
  (let ((pos start)
        (length (length text)))
    (loop while (and (< (1+ pos) length)
                     (char= (char text pos) #\#)
                     (member (char text (1+ pos)) '(#\+ #\-)))
          do (let ((after (handler-case
                              (let ((*read-suppress* t)
                                    ;; #. is not evaluated while suppressing;
                                    ;; this says so and keeps it that way.
                                    (*read-eval* nil)
                                    (*readtable* *standard-readtable*))
                                (nth-value 1 (read-from-string text t nil
                                                               :start (+ pos 2)
                                                               :preserve-whitespace t)))
                            (error () nil))))
               (unless after
                 (return-from %feature-prefix-end start))
               (setf pos (%skip-blank-and-comments text after))))
    pos))

(defun %target-feature-prefix (text node)
  "Return the #+/#- feature expressions written in front of NODE's form in TEXT,
with the whitespace and comments after them, as they appear; NIL when there are
none.  NODE's span starts at its first #, so a replace of the whole span by the
bare form would drop them, making a definition meant for one implementation or
configuration unconditional."
  (let* ((start (cst-node-start node))
         (end (%feature-prefix-end text start)))
    (and (/= start end) (subseq text start end))))

(defun %split-feature-prefix (content)
  "Return (values STRIPPED PREFIX) for CONTENT: PREFIX the #+/#- feature
expressions in front of its form, as written, and STRIPPED CONTENT without them
-- each replaced by the line breaks it held, so the lines a repair reports keep
their numbers.  Content is validated by its form, which the reader skips when
the new condition is false in this process: #-sbcl (defun ...) on SBCL read as
no form at all.  CONTENT and NIL when it starts with none."
  (let* ((lead-end (%skip-blank-and-comments content 0))
         (prefix-end (%feature-prefix-end content lead-end)))
    (if (= lead-end prefix-end)
        (values content nil)
        (let ((prefix (subseq content lead-end prefix-end)))
          (values (concatenate 'string
                               (subseq content 0 lead-end)
                               (remove #\Newline prefix :test-not #'char=)
                               (subseq content prefix-end))
                  prefix)))))

(defun %feature-expression-problem (expression &optional (depth 0))
  "Return why EXPRESSION, a feature expression as read, breaks the grammar of
CLHS 24.1.2.1 -- a symbol, or a proper list headed by AND, OR or NOT, NOT with
exactly one argument, whose arguments are feature expressions in turn -- or NIL
when it keeps it.  Operators are compared by name, so :NOT and a NOT read in
another package both count, as readers accept both.  Nothing is evaluated: a
condition false in this process is as valid as a true one."
  (cond
    ((> depth 64) "it is nested too deeply")
    ((symbolp expression) nil)
    ((not (and (consp expression) (ignore-errors (list-length expression))))
     (format nil "~S is neither a symbol nor a proper list" expression))
    ((not (and (symbolp (first expression))
               (member (symbol-name (first expression)) '("AND" "OR" "NOT")
                       :test #'string=)))
     ;; By name: its package was the reader's scratch one, deleted by now.
     (format nil "~A is not AND, OR or NOT"
             (let ((operator (first expression)))
               (if (symbolp operator) (symbol-name operator) (prin1-to-string operator)))))
    ((and (string= (symbol-name (first expression)) "NOT")
          (/= 1 (length (rest expression))))
     "NOT takes exactly one feature expression")
    (t (some (lambda (argument) (%feature-expression-problem argument (1+ depth)))
             (rest expression)))))

(defun %check-feature-prefix (prefix)
  "Signal an error naming the first malformed feature expression in PREFIX, the
#+/#- expressions %SPLIT-FEATURE-PREFIX set aside from a replace's content.
Setting them aside is what lets a condition false here through, so they are not
read with the form and have to be checked on their own: a malformed one would
be written and break the file's next read.  Each is read in a package of its
own, deleted afterwards, so its feature names are interned nowhere."
  (let ((pos 0))
    (loop while (and (< (1+ pos) (length prefix))
                     (char= (char prefix pos) #\#)
                     (member (char prefix (1+ pos)) '(#\+ #\-)))
          do (let ((package (make-package (symbol-name (gensym "CL-MCP-FEATURE-CHECK-"))
                                          :use '())))
               (multiple-value-bind (expression end)
                   (unwind-protect
                        (handler-case
                            (let ((*package* package)
                                  (*readtable* *standard-readtable*)
                                  (*read-eval* nil)
                                  (*read-suppress* nil))
                              (read-from-string prefix t nil :start (+ pos 2)
                                                             :preserve-whitespace t))
                          (error (e)
                            (error "content's feature expression ~A cannot be read: ~A"
                                   (subseq prefix pos) (sanitize-condition-text e))))
                     (delete-package package))
                 (let ((problem (%feature-expression-problem expression)))
                   (when problem
                     (error "content's feature expression ~A is malformed: ~A"
                            (subseq prefix pos end) problem)))
                 (setf pos (%skip-blank-and-comments prefix end)))))))

(defun %form-start (content)
  "Return the index of CONTENT's first form, past its leading whitespace and
comments, or NIL when it holds comments only."
  (let ((start (%skip-blank-and-comments content 0)))
    (and (< start (length content)) start)))

(defun %put-feature-prefix (content prefix &optional (held-breaks 0))
  "Return CONTENT, validated replacement text, with PREFIX -- feature
expressions and the whitespace after them -- right before its form.  When
%SPLIT-FEATURE-PREFIX took PREFIX out of CONTENT, HELD-BREAKS is the number of
line breaks it left in PREFIX's place, which PREFIX now takes back.  CONTENT
itself when it holds no form: expressions with nothing after them would apply
to whatever follows in the file, or break its read at the end."
  (let ((form-start (%form-start content)))
    (if (null form-start)
        content
        (let ((start (if (and (>= form-start held-breaks)
                              (every (lambda (ch) (char= ch #\Newline))
                                     (subseq content (- form-start held-breaks) form-start)))
                         (- form-start held-breaks)
                         form-start)))
          (concatenate 'string (subseq content 0 start) prefix (subseq content form-start))))))

(defun %kept-feature-note (kept)
  "Return the summary line saying a replace kept KEPT, the target's feature
expressions (%TARGET-FEATURE-PREFIX) put back, or NIL when it kept none."
  (when kept
    (format nil "~%Kept ~A in front of the form: the content did not carry it. ~
                 To change or drop the condition, start the content with the ~
                 feature expression it should have."
            kept)))

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

(defun %repair-summary (warning fixes repaired-form &key include-form moved)
  "Return the text appended to a success summary when parinfer repaired the
content, or NIL when WARNING is NIL. Lists the changed lines and, when
INCLUDE-FORM is true, the repaired form itself (bounded by %TRUNCATE-SNIPPET).
MOVED is %VALIDATE-AND-REPAIR-CONTENT's fifth value: the forms the repair moved
out of the form the content's own parens put them in. When there are any they
are named (FORMAT-REPARENT-NOTE, issue #183): the repair follows indentation,
and where that disagrees with the parens the caller has to say which was
meant. Otherwise the relocation note (FORMAT-RELOCATION-NOTE: a closer
inserted on a line whose next code line sits at the same indentation) is the
one lisp-check-parens prints, so the two tools describe the same repair in the
same words. The bracket-opener reminder is part of WARNING, built by
%REPAIR-WARNING where the readtable is known."
  (when warning
    (with-output-to-string (s)
      (format s "~%WARNING: ~A" warning)
      (when fixes
        (format s "~%Changed lines:~A" (format-repair-lines fixes)))
      (let ((note (if moved
                      (format-reparent-note moved :target :content)
                      (format-relocation-note fixes repaired-form))))
        (when note
          (format s "~%~A" note)))
      (when include-form
        (format s "~%~%--- repaired form ---~%~A" (%truncate-snippet repaired-form))))))

(defun lisp-edit-form
       (&key file-path form-type form-name operation content dry-run
        (normalize-blank-lines t) readtable guard)
  "Structured edit of a top-level Lisp form.
FILE-PATH may be absolute or relative to the project root. FORM-TYPE,
FORM-NAME, and OPERATION are always required. CONTENT is required for
replace/insert_before/insert_after but ignored for delete.

OPERATION must be one of: \"replace\", \"insert_before\", \"insert_after\", \"delete\".
Missing closing parentheses are auto-repaired using parinfer (non-delete ops).
replace takes one top-level form; insert_before and insert_after take one or
more, inserted in order as one block (%VALIDATE-AND-REPAIR-BLOCK), with only
the gaps between them normalised when NORMALIZE-BLANK-LINES is true.

When DRY-RUN is true, no changes are written; a preview hash-table is returned.
The same GUARD validation runs whether DRY-RUN is true or not.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing both the file and the new content.

GUARD, if provided, is an edit_guard JSON object (clos-describe's edit_guard,
design doc 2026-09-16-clos-describe-fail-closed section 4.1), or the compact
token clos-describe prints for it in its content text -- the form a client
that renders only content[].text can actually get hold of. Either way it must
still describe the current file and target form; CL-MCP/SRC/LISP-EDIT-FORM-CORE:
%LOCATE-TARGET-FORM signals EDIT-GUARD-CONFLICT-ERROR, before any value is
returned and before anything is written, when it does not. Without GUARD,
this call behaves as before -- there is no guarantee the located form still
matches what an earlier read observed.

The whole read -> locate -> guard -> build -> write span runs under
CL-MCP/SRC/FS:WITH-FILE-LOCK for the target file, so cl-mcp's own concurrent
edits of one file are serialised: a second edit reads what the first wrote
instead of overwriting it from a stale copy. A DRY-RUN call takes the same
lock -- it reads the file, and excluding it would let it report a preview of a
file another thread is halfway through replacing -- but of course writes
nothing. The lock lives in this image, so only ONE cl-mcp process's calls are
ordered: an external editor, and equally a second cl-mcp server over the same
checkout, is not coordinated. GUARD remains the only check against one, and it
is a precondition, not a lock.

A replace keeps the #+/#- feature expressions in front of the target form
(%TARGET-FEATURE-PREFIX) when CONTENT does not start with its own and holds a
form for them.  CONTENT's own expressions are set aside while its form is
validated (%SPLIT-FEATURE-PREFIX), so a condition false in this process is
accepted, and put back in front of it.

For non-delete operations without DRY-RUN, returns nine values: the updated
file text, the parinfer warning or NIL, whether the file changed, the repair
line diff or NIL, the validated content that was spliced in, a bracket
warning (a ] or } found where ) was expected, in content that still reads)
or NIL, the forms the repair moved out of the form the content's own
parens put them in (REPARENTED-FORMS) or NIL, the number of forms an
insert put in when it was more than one, or NIL, and the feature expressions
a replace kept, or NIL. A dry run carries the reparented forms as
\"repair_reparented\", that number as \"forms\" and the expressions as
\"kept_feature_expression\"."
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
    ;; Content is read, and repaired, by recursive readers.
    (when (stringp content)
      (check-lisp-nesting content))
    (with-file-lock ((%normalize-paths file-path))
      (multiple-value-bind
          (abs rel original nodes target target-snippet _ file-package-name)
          (%locate-target-form file-path form-type form-name readtable guard)
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
            (let* ((content-readtable
                     (or readtable (%detect-readtable-before-node nodes target)))
                   ;; A replace's own feature expressions are set aside so that
                   ;; its form is validated whether or not their condition
                   ;; holds in this process.
                   (split (if (eq op-key :replace)
                              (multiple-value-list (%split-feature-prefix content))
                              (list content nil)))
                   (form-content (first split))
                   (own-prefix (second split)))
              (when own-prefix
                (%check-feature-prefix own-prefix))
              (when (and own-prefix (null (%form-start form-content)))
                (error "content has the feature expression ~A but no form after it"
                       (string-right-trim '(#\Space #\Tab #\Newline #\Return) own-prefix)))
              (multiple-value-bind (validated-content parinfer-warning repair-fixes
                                    bracket-warning reparented form-count spans)
                  ;; insert_before/insert_after take one or more forms (issue
                  ;; #189); replace keeps its one-form rule.
                  (if (member op-key '(:insert-before :insert-after))
                      (%validate-and-repair-block content content-readtable
                                                  file-package-name abs)
                      (%validate-and-repair-content form-content content-readtable
                                                    file-package-name abs))
                (let* ((block-spliced
                         ;; Normalise only the gaps between a block's forms, and
                         ;; only where the gaps read the standard way.
                         (if (and spans normalize-blank-lines
                                  (%gap-syntax-standard-p
                                   (or (%resolve-named-readtable content-readtable)
                                       *standard-readtable*)))
                             (%normalize-block-gaps validated-content spans)
                             validated-content))
                       ;; The target's expressions are kept only when the
                       ;; content brings none and holds a form for them.
                       (target-prefix (and (eq op-key :replace) (null own-prefix)
                                           (%target-feature-prefix original target)))
                       (spliced (cond
                                  (own-prefix
                                   (%put-feature-prefix block-spliced own-prefix
                                                        (count #\Newline own-prefix)))
                                  (target-prefix
                                   (%put-feature-prefix block-spliced target-prefix))
                                  (t block-spliced)))
                       (kept-feature (and target-prefix (%form-start block-spliced)
                                          (string-right-trim '(#\Space #\Tab #\Newline
                                                               #\Return)
                                                             target-prefix)))
                       (several-forms (and form-count (> form-count 1) form-count))
                       (updated
                         (%apply-operation original target op-key spliced
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
                            (%preview-form-text op-key spliced normalize-blank-lines)
                            ;; The untrimmed content the repair line numbers
                            ;; refer to, for the relocation note in the summary.
                            (gethash "validated_content" result) validated-content
                            (gethash "file_path" result) (namestring abs)
                            (gethash "operation" result) op-normalized)
                      (when parinfer-warning
                        (setf (gethash "parinfer_warning" result) parinfer-warning
                              (gethash "repair_fixes" result) repair-fixes
                              (gethash "repair_reparented" result) reparented))
                      (when bracket-warning
                        (setf (gethash "bracket_warning" result) bracket-warning))
                      (when several-forms
                        (setf (gethash "forms" result) several-forms))
                      (when kept-feature
                        (setf (gethash "kept_feature_expression" result) kept-feature))
                      result))
                   (would-change (fs-write-file rel updated)
                    (values updated parinfer-warning t repair-fixes validated-content
                            bracket-warning reparented several-forms kept-feature))
                   (t (values updated parinfer-warning nil repair-fixes
                              validated-content bracket-warning reparented
                              several-forms kept-feature)))))))))))

(defun %resolve-guard-argument (args guard guard-token)
  "Return the guard LISP-EDIT-FORM should run with, or NIL for an unguarded
call.  ARGS is the tool's raw JSON argument table; GUARD and GUARD-TOKEN are
what EXTRACT-ARG pulled out of it.

Decided on whether the KEYS are present, not on whether their values are
true.  EXTRACT-ARG sees only a value, and YASON decodes both `null` and
`false` to NIL, so `\"guard_token\": null` is indistinguishable there from a
key that was never sent: the type check is skipped and the call proceeds
unguarded.  That is the one outcome a caller who asked for a guard must never
get -- it is the same downgrade PARSE-EDIT-GUARD-TOKEN refuses to make for a
token it cannot read -- so a key that is present is checked here even when its
value is NIL, and only a call that sent neither key runs unguarded."
  (flet ((present-p (key) (and args (nth-value 1 (gethash key args)))))
    (let ((guard-present (present-p "guard"))
          (token-present (present-p "guard_token")))
      (cond
        ;; Two spellings of one guard: refuse both rather than pick a winner,
        ;; so a caller that sent a stale one alongside a fresh one is told.
        ((and guard-present token-present)
         (error 'arg-validation-error :arg-name "guard_token"
                :message "pass either guard or guard_token, not both"))
        (token-present
         (unless (and (stringp guard-token) (plusp (length guard-token)))
           (error 'arg-validation-error :arg-name "guard_token"
                  :message (concatenate
                            'string
                            "guard_token must be the [guard: ...] token clos-describe "
                            "printed, as a string; omit the argument entirely to edit "
                            "without a guard")))
         guard-token)
        (guard-present
         (unless (hash-table-p guard)
           (error 'arg-validation-error :arg-name "guard"
                  :message (concatenate
                            'string
                            "guard must be an edit_guard object; omit the argument "
                            "entirely to edit without a guard")))
         guard)
        (t nil)))))

(define-tool "lisp-edit-form"
  :description "Structure-aware edit of a top-level Lisp form using Eclector CST parsing.
Supports replace, insert_before, insert_after, and delete operations while preserving
formatting and comments. insert_before/insert_after take several top-level forms in
one call.
PREFERRED METHOD for editing existing Lisp source code.
Automatically repairs missing closing parentheses using parinfer (non-delete ops).
ALWAYS use this tool instead of 'fs-write-file' when modifying Lisp forms to ensure
safety and structure preservation."
  :args ((file_path :type :string :required t
                    :description "Target file path: relative to the project root, or absolute inside
it (absolute recommended). Files outside the project root, registered ASDF sources included, are
refused")
         (form_type :type :string :required t
                    :description "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\".
A package prefix is ignored: \"asdf:defsystem\" and \"defsystem\" both match
(asdf:defsystem ...). A colon inside the name itself still matches as written
(\"def:thing\" or \"|def:thing|\" for (|DEF:THING| ...)); when the two readings
name different forms, the call is refused as ambiguous.")
         (form_name :type :string :required t
                    :description "Form name to match; for defmethod include specializers,
e.g., \"print-object ((obj my-class) stream)\" (a method whose name is unique matches
by name alone). A name[N] suffix, 0-based, picks the Nth of several matches. For defstruct with
options \"(defstruct (name opts...) ...)\", use just the bare struct name.
Reader macro prefixes #: and : are stripped automatically, so
\"#:my-pkg\" and \"my-pkg\" both match \"(defpackage #:my-pkg ...).\"")
         (operation :type :string :required t
                    :enum ("replace" "insert_before" "insert_after" "delete")
                    :description "Operation to perform")
         (content :type :string
                  :description "Lisp source for the operation. Required for replace/insert_before/insert_after.
Ignored for delete. replace takes exactly ONE top-level form. insert_before and
insert_after take one or more, inserted in the order given as one block, so
several new definitions go in with one call; comments between them stay where
they are. Comment-only content is accepted too.
replace keeps the #+/#- feature expressions written in front of the target form
when content starts with none of its own, and says so (kept_feature_expression);
start content with the expression it should have to change or drop the condition
(its form is checked whether or not that condition holds here). Comment-only
content keeps none.
Missing closing parentheses are automatically repaired using parinfer; a block
is repaired as a whole and must then read as complete forms, or nothing is
written.")
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
is used to locate forms instead of Eclector. Only needed when the file does not
declare its own (in-readtable ...): one earlier in the file is honoured automatically.
The edit is spliced into the file by position, so text outside the edited form,
comments included, is kept.")
         (guard :type :object
                :description "Edit guard from clos-describe's edit_guard (design doc
2026-09-16-clos-describe-fail-closed section 4.1): {version, path, abs_path,
file_digest, form_start, form_end, form_digest}. When given, the edit (including
dry_run) is refused with a conflict object, and nothing is written, unless the
file and the matched form still look exactly as observed. Without it, this call
behaves as before: the located form may not be the one an earlier read saw.")
         (guard-token :type :string
                      :description "The same edit guard in the compact one-line form
clos-describe prints beside a matched definition as [guard: ...]:
version|file_digest|form_start|form_end|form_digest|abs_path. Copy that token
verbatim; it is checked exactly as the guard object is. Use this rather than
'guard' when reading clos-describe's content text, which is where the token
appears. Passing both is an error, and so is sending either one as null,
false or any other non-token value: omit the argument entirely to edit
without a guard."))
  :body
  (progn
    (when (and (not content) (string/= (string-downcase operation) "delete"))
      (error 'arg-validation-error :arg-name "content"
             :message (format nil "content is required for ~A operation" operation)))
    (handler-case
        (multiple-value-bind (updated parinfer-warning changed-p repair-fixes
                              repaired-form bracket-warning reparented forms
                              kept-feature)
            (lisp-edit-form :file-path file_path
                            :form-type form_type
                            :form-name form_name
                            :operation operation
                            :content content
                            :dry-run dry_run
                            :normalize-blank-lines normalize_blank_lines
                            :readtable (%parse-readtable-designator readtable)
                            :guard (%resolve-guard-argument args guard guard-token))
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
                     (block-forms (gethash "forms" updated))
                     (dry-kept (gethash "kept_feature_expression" updated))
                     (summary
                      (format nil "Dry-run ~A~@[ of ~D forms~] on ~A ~A in ~A ~
                                   (~:[no change~;would change~])~
                                   ~@[~A~]~@[~A~]~@[~%WARNING: ~A~]~
                                   ~@[~%~%--- original ---~%~A~]~
                                   ~@[~%~%--- preview ---~%~A~]"
                              operation block-forms form_type form_name file_path
                              would-change
                              (%kept-feature-note dry-kept)
                              (%repair-summary pw (gethash "repair_fixes" updated)
                                               (or (gethash "validated_content" updated)
                                                   preview-form)
                                               :moved (gethash "repair_reparented" updated))
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
                                  (list "bracket_warning" bw))
                                (when block-forms
                                  (list "forms" block-forms))
                                (when dry-kept
                                  (list "kept_feature_expression" dry-kept))))))
              (let ((summary
                     (cond
                       ((not changed-p)
                        (format nil
                                "No change to ~A ~A in ~A (content matches existing form)~
                                 ~@[~A~]~@[~%WARNING: ~A~]"
                                form_type form_name file_path
                                (%repair-summary parinfer-warning repair-fixes
                                                 repaired-form :include-form t
                                                 :moved reparented)
                                bracket-warning))
                       (t
                        (format nil "Applied ~A~@[ of ~D forms~] to ~A ~A in ~A ~
                                     (~D chars)~@[~A~]~@[~A~]~@[~%WARNING: ~A~]"
                                operation forms form_type form_name file_path
                                (length updated)
                                (%kept-feature-note kept-feature)
                                (%repair-summary parinfer-warning repair-fixes
                                                 repaired-form :include-form t
                                                 :moved reparented)
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
                               (append
                                (when bracket-warning
                                  (list "bracket_warning" bracket-warning))
                                (when forms
                                  (list "forms" forms))
                                (when kept-feature
                                  (list "kept_feature_expression" kept-feature))))))))
      (content-unrepairable-error (e)
        (tool-error id (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
      (edit-guard-conflict-error (e)
        (let* ((conflict (edit-guard-conflict e))
               (message (sanitize-for-json (princ-to-string e)))
               (conflict-ht (make-ht "reason" (getf conflict :reason)
                                     "expected" (getf conflict :expected)
                                     "actual" (getf conflict :actual))))
          (if (and (protocol-version state)
                   (string>= (protocol-version state) "2025-11-25"))
              (result id (make-ht "content" (text-content message)
                                  "isError" t
                                  "conflict" conflict-ht))
              (rpc-error id -32602 message conflict-ht))))
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
      ;; A rejected guard argument is an argument error, not an internal one.
      ;; Without this clause the generic ERROR clause below would relabel it
      ;; -32603, unlike every other argument this tool refuses -- and
      ;; %RESOLVE-GUARD-ARGUMENT signals from inside the call below, so
      ;; DEFINE-TOOL's own ARG-VALIDATION-ERROR handler never sees it.
      (arg-validation-error (e)
        (tool-error id (princ-to-string e)
                    :protocol-version (protocol-version state)))
      (error (e)
        (let ((msg (sanitize-for-json
                    (sanitize-error-message (format nil "~A" e)))))
          (if (and (protocol-version state)
                   (string>= (protocol-version state) "2025-11-25"))
              (result id (make-ht "content" (text-content msg) "isError" t))
              (rpc-error id -32603 msg)))))))
