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
                #:make-ht #:result #:rpc-error #:text-content
                #:arg-validation-error #:tool-error #:json-bool)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message
                #:sanitize-condition-text
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:count-delimiter-depth
                #:diagnose-delimiters
                #:format-delimiter-diagnosis
                #:lexical-state-at
                #:scan-delimiters
                #:bracket-ambiguous-p)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%resolve-named-readtable
                #:%nonstandard-readtable-p
                #:%parse-readtable-designator
                #:%detect-readtable-before-node
                #:%whitespace-char-p
                #:%locate-target-form
                #:file-unparseable-error)
  (:documentation "Scoped text replacement within a matched top-level Lisp form.")
  (:export #:lisp-patch-form))

(in-package #:cl-mcp/src/lisp-patch-form)

(define-condition patch-operation-error (error)
  ((reason :initarg :reason :reader patch-operation-reason))
  (:report (lambda (c s) (write-string (patch-operation-reason c) s)))
  (:documentation "Raised for expected patch failures (not-found, multiple-match, invalid result)."))

(defun %bound-snippet (text)
  "Return TEXT cut to 2048 characters plus a marker when it is longer, so a
dry-run summary of a large form does not echo the whole form twice."
  (if (> (length text) 2048)
      (concatenate 'string (subseq text 0 2048)
                   (format nil "~%... [truncated, ~D characters total]" (length text)))
      text))

(defun %bracket-mismatch-p (form-text)
  "Return T when the delimiter scan of FORM-TEXT stops at a mismatch that
rests on a bracket (see BRACKET-AMBIGUOUS-P): the typo case where a net
parenthesis count would advise adding or removing a \")\" and thereby write
code that merely reads (as a symbol ending in ]) instead of naming the
mistyped bracket."
  (let ((scan (scan-delimiters form-text)))
    (and (equal (getf scan :kind) "mismatch")
         (bracket-ambiguous-p scan))))

(defun %check-depth-balance (form-text modified-form match-pos old-text new-text)
  "Return a message describing the net parenthesis difference the patch makes,
or NIL when it makes none. OLD-TEXT occupies FORM-TEXT from MATCH-POS and
NEW-TEXT occupies MODIFIED-FORM from the same position; each region is
counted in its real lexical context, so a parenthesis inside a string or a
comment is not mistaken for code. The message is also withheld when the
lexical state at the end of the replacement differs between the two texts
(NEW-TEXT opened a string or comment that swallows the unchanged suffix):
the region counts would then describe a reclassified suffix, not a real
parenthesis difference, and the reader's own failure is the better report.
A net difference in code guarantees the form will not parse, but the caller
still uses this message only when the patched form actually fails to parse."
  (let ((old-end (+ match-pos (length old-text)))
        (new-end (+ match-pos (length new-text))))
    ;; Both values matter: the state keyword and, for block comments, the
    ;; nesting depth -- one open #| more or less reclassifies the suffix. A
    ;; :pending boundary is never trusted either: different unfinished
    ;; constructs (a trailing | versus a trailing #, say) all report
    ;; :pending yet combine differently with the unchanged suffix.
    (multiple-value-bind (old-state old-depth) (lexical-state-at form-text old-end)
      (multiple-value-bind (new-state new-depth) (lexical-state-at modified-form new-end)
        (unless (and (eq old-state new-state)
                     (eql old-depth new-depth)
                     (not (eq old-state :pending)))
          (return-from %check-depth-balance nil))))
    (multiple-value-bind (old-open old-close)
        (count-delimiter-depth form-text :start match-pos :end old-end)
      (multiple-value-bind (new-open new-close)
          (count-delimiter-depth modified-form :start match-pos :end new-end)
        (let ((diff (- (- new-open new-close) (- old-open old-close))))
          (unless (zerop diff)
            (let ((n (abs diff)))
              (if (plusp diff)
                  (format nil "new_text closes ~D fewer \")\" than old_text ~
                               (old_text: ~D open / ~D close, new_text: ~D open / ~D close). ~
                               The patch would leave the form unclosed. ~
                               Add ~D \")\" to new_text, or remove ~D \"(\". ~
                               No changes were written to disk."
                          n old-open old-close new-open new-close n n)
                  (format nil "new_text closes ~D more \")\" than old_text ~
                               (old_text: ~D open / ~D close, new_text: ~D open / ~D close). ~
                               The patch would add an extra closing parenthesis. ~
                               Remove ~D \")\" from new_text, or add ~D \"(\". ~
                               No changes were written to disk."
                          n old-open old-close new-open new-close n n)))))))))

(defun %apply-patch-operation (text node old-text new-text)
  "Replace OLD-TEXT with NEW-TEXT within the form at NODE in TEXT.
Returns four values: the modified full file text, the modified form text,
the original form text, and the position of OLD-TEXT within the form (which
is also the position of NEW-TEXT within the modified form).
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
      (values modified-file modified-form form-text match-pos))))

(defun %diagnosed-reason (form-text fallback &optional reader-text)
  "Return the patch failure reason for FORM-TEXT. When the delimiter scan
finds the breakage, the shared diagnosis is used; otherwise FALLBACK, the
complete message built from the failure. A mismatch that rests on a ] or }
(see BRACKET-AMBIGUOUS-P) may be a false positive, so READER-TEXT -- the
reader's own words alone, when the caller has them -- is kept alongside the
diagnosis instead of discarded; without READER-TEXT nothing is attributed to
the reader."
  (let ((diagnosis (diagnose-delimiters form-text)))
    (if (getf diagnosis :ok)
        fallback
        (format nil "patch operation produced invalid Lisp (line numbers below are ~
                     within the patched form). ~A~
                     ~@[~%The reader itself reported: ~A.~] ~
                     No changes were written to disk."
                (format-delimiter-diagnosis diagnosis :target "the patched form")
                (and (bracket-ambiguous-p diagnosis) reader-text)))))

(defun %validate-form-parseable (form-text &key readtable-designator
                                             package-name source-path
                                             depth-reason
                                             (nonstandard-rt
                                              (%nonstandard-readtable-p
                                               readtable-designator)))
  "Validate that FORM-TEXT parses as a single complete Lisp form.
Does NOT attempt parinfer repair. Signals PATCH-OPERATION-ERROR, carrying a
delimiter diagnosis when one applies, if the text does not parse correctly.
DEPTH-REASON, when non-NIL, is a function of no arguments returning the
net-parenthesis message from %CHECK-DEPTH-BALANCE or NIL; it is called only
once parsing has failed, so a successful patch pays for no extra scans, and
its message takes precedence over the delimiter diagnosis because it names
the exact number of \")\" to add or remove. A depth message alone never
rejects the patch: a parenthesis added inside a string or a comment is a
legitimate edit and still parses.
Under a READTABLE-DESIGNATOR that changes the syntax the standard delimiter
diagnosis is not consulted at all (a reader macro may consume raw
parentheses as data), so the reader's own failure is reported."
  (let* ((*read-eval* nil)
         (custom-rt (%resolve-named-readtable readtable-designator))
         (*readtable*
           (if custom-rt
               custom-rt
               (copy-readtable nil))))
    (flet ((diagnosed (fallback &optional reader-text)
             ;; A readtable that is standard in all but name keeps the
             ;; standard diagnosis; only a syntax change withholds it.
             (if nonstandard-rt
                 fallback
                 (%diagnosed-reason form-text fallback reader-text)))
           (depth-message ()
             (and depth-reason (funcall depth-reason))))
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
                          :reason (or (depth-message)
                                      (diagnosed
                                       (format nil "patch produced malformed form text ~
                                                    (trailing content after form). ~
                                                    No changes were written to disk."))))))
               form-text))
           :source-path source-path)
        (patch-operation-error (e)
          (error e))
        (error (e)
          (error 'patch-operation-error
                 :reason (or (depth-message)
                             ;; Sanitized here, where the reader's text is
                             ;; embedded, so the diagnosis around it keeps
                             ;; its line breaks.
                             (let ((reader-text (sanitize-condition-text e)))
                               (diagnosed
                                (format nil "patch operation produced invalid Lisp: ~A. ~
                                             The form could not be parsed after ~
                                             replacement. No changes were written to disk."
                                        reader-text)
                                reader-text)))))))))

(defun lisp-patch-form (&key file-path form-type form-name old-text new-text
                              dry-run readtable)
  "Scoped text replacement within a matched top-level Lisp form.
FILE-PATH may be absolute or relative to the project root. FORM-TYPE and
FORM-NAME identify the target form. OLD-TEXT and NEW-TEXT specify the replacement.

OLD-TEXT must match exactly once within the form (whitespace-sensitive).
Does NOT auto-repair parentheses; if the patch breaks form structure, an error
is signaled and no changes are written to disk. When NEW-TEXT changes the net
parenthesis count in code (parentheses inside strings and comments do not
count), that difference is reported as the failure reason, but only if the
patched form really does fail to parse.

When DRY-RUN is true, no changes are written; a preview hash-table is returned.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing the file.

Returns three values: the updated file text, whether the file changed, and a
bracket warning or NIL. The warning is set when the patched form reads but
its delimiter scan stops at a ] or } (a possible ) typo that standard syntax
silently accepts as part of a symbol), so the caller can check it."
  (unless (and (stringp file-path) (stringp form-type) (stringp form-name))
    (error "file_path, form_type, and form_name must be strings"))
  (unless (and (stringp old-text) (stringp new-text))
    (error "old_text and new_text must be strings"))
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (when (zerop (length old-text))
    (error 'arg-validation-error :arg-name "old_text"
           :message "old_text must not be empty"))
  (multiple-value-bind (abs rel original nodes target target-snippet _
                        file-package-name)
      (%locate-target-form file-path form-type form-name readtable)
    (declare (ignore _))
    (multiple-value-bind (updated modified-form form-text match-pos)
        (%apply-patch-operation original target old-text new-text)
      (let* ((would-change (not (string= original updated)))
             (readtable-designator
               (or readtable (%detect-readtable-before-node nodes target)))
             ;; Computed once: the readtable comparison probes 256 characters.
             (nonstandard-rt (%nonstandard-readtable-p readtable-designator))
             ;; Counted in the form's lexical context: a ")" inside a string
             ;; or comment is not code and must not produce a depth message.
             ;; Under a readtable that changes the syntax the standard lexical
             ;; rules cannot be trusted (a reader macro may consume raw
             ;; parentheses as data), so no depth message is offered at all;
             ;; the reader's own failure is reported through the normal
             ;; diagnosis path.
             ;; A ] or } typed for ) changes the net count too, but "add 1 )"
             ;; would then write code that reads (as a symbol ending in ]);
             ;; let the bracket diagnosis speak instead.
             ;; Deferred: the scans behind it run only once the patched form
             ;; has failed to parse, so a successful patch pays nothing.
             (depth-reason
               (lambda ()
                 (and (not nonstandard-rt)
                      (not (%bracket-mismatch-p modified-form))
                      (%check-depth-balance form-text modified-form
                                            match-pos old-text new-text)))))
        (when would-change
          (%validate-form-parseable
           modified-form
           :readtable-designator readtable-designator
           :package-name file-package-name
           :source-path abs
           :depth-reason depth-reason
           :nonstandard-rt nonstandard-rt))
        ;; The form reads, but a ] or } where ) was meant reads too (as part
        ;; of a symbol). Written as asked -- the caller may mean it -- but
        ;; flagged, since silently accepting it is how such typos survive.
        ;; Only the found side: an unmatched [ or { opener is a symbol
        ;; character in standard syntax and carries no ) typo to flag.
        (let ((bracket-warning
                (and would-change
                     (not nonstandard-rt)
                     (let ((scan (scan-delimiters modified-form)))
                       (and (equal (getf scan :kind) "mismatch")
                            (member (getf scan :found) '("]" "}") :test #'equal)
                            (format nil "the patched form reads, but its delimiter ~
                                         scan finds ~S where ~S was expected (line ~D, ~
                                         column ~D within the form); in standard ~
                                         syntax that ~S is part of a symbol name, so ~
                                         check that it is what you meant."
                                    (getf scan :found) (getf scan :expected)
                                    (getf scan :line) (getf scan :column)
                                    (getf scan :found)))))))
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
               (when bracket-warning
                 (setf (gethash "bracket_warning" result) bracket-warning))
               result))
            (would-change
             (fs-write-file rel updated)
             (values updated t bracket-warning))
            (t
             (values updated nil nil))))))))

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
e.g., \"print-object ((obj my-class) stream)\". For defstruct with
options \"(defstruct (name opts...) ...)\", use just the bare struct name.
Reader macro prefixes #: and : are stripped automatically, so
\"#:my-pkg\" and \"my-pkg\" both match \"(defpackage #:my-pkg ...).\"")
         (old_text :type :string :required t
                   :description "Text to find within the matched form.
Performs exact raw text matching (whitespace-sensitive). Must occur exactly once in the form.")
         (new_text :type :string :required t
                   :description "Replacement text.
If new_text opens and closes a different net number of parentheses than old_text
and the patched form no longer parses, the error says exactly how many \")\" to
add or remove.")
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
        (multiple-value-bind (updated changed-p bracket-warning)
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
                                      ~@[~%WARNING: ~A~]~
                                      ~%~%--- original ---~%~A~%~%--- preview ---~%~A"
                                      form_type form_name file_path would-change
                                      (gethash "bracket_warning" updated)
                                      (%bound-snippet original-form)
                                      (%bound-snippet preview))))
                (result id
                        (apply #'make-ht
                               "path" file_path
                               "operation" "patch"
                               "form_type" form_type
                               "form_name" form_name
                               "would_change" (json-bool would-change)
                               "original" original-form
                               "preview" preview
                               "content" (text-content summary)
                               (let ((warning (gethash "bracket_warning" updated)))
                                 (when warning
                                   (list "bracket_warning" warning))))))
              (let ((summary
                     (if (not changed-p)
                         (format nil "No change to ~A ~A in ~A (old_text already matches new_text)"
                                 form_type form_name file_path)
                         (format nil "Applied patch to ~A ~A in ~A (~D chars → ~D chars)~
                                      ~@[~%WARNING: ~A~]"
                                 form_type form_name file_path
                                 (length old_text) (length new_text)
                                 bracket-warning))))
                (result id
                        (apply #'make-ht
                               "path" file_path
                               "form_type" form_type
                               "form_name" form_name
                               "would_change" (json-bool changed-p)
                               "bytes" (length updated)
                               "content" (text-content summary)
                               (append
                                (when changed-p
                                  (list "delta" (- (length new_text) (length old_text))))
                                (when bracket-warning
                                  (list "bracket_warning" bracket-warning))))))))
      ;; CLAUSE ORDER IS LOAD-BEARING. PATCH-OPERATION-ERROR and
      ;; ARG-VALIDATION-ERROR are both subtypes of ERROR, so both must stay
      ;; ahead of the generic clause; reordering silently changes the response
      ;; shape of every expected failure rather than breaking the build.
      (file-unparseable-error (e)
        (tool-error id
                    (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
      ;; Not passed through SANITIZE-ERROR-MESSAGE: that would fold the
      ;; multi-line "Likely fix" block into one line and cut the message at
      ;; 500 characters, losing "No changes were written to disk". The reader
      ;; text embedded by %VALIDATE-FORM-PARSEABLE is sanitized at its source.
      (patch-operation-error (e)
        (tool-error id
                    (sanitize-for-json (princ-to-string e))
                    :protocol-version (protocol-version state)))
      ;; Re-signal so DEFINE-TOOL's own ARG-VALIDATION-ERROR clause formats it;
      ;; otherwise the generic clause below would swallow genuine argument
      ;; errors and misreport them as internal failures. HANDLER-CASE unwinds
      ;; before running a handler body, so this re-signal cannot re-enter the
      ;; clauses above or below it — it necessarily reaches DEFINE-TOOL's
      ;; generated HANDLER-CASE.
      (arg-validation-error (e)
        (error e))
      (error (e)
        (let ((msg (sanitize-for-json
                    (sanitize-error-message (format nil "~A" e)))))
          (if (and (protocol-version state)
                   (string>= (protocol-version state) "2025-11-25"))
              (result id (make-ht "content" (text-content msg) "isError" t))
              (rpc-error id -32603 msg)))))))
