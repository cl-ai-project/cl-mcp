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
  (:import-from #:cl-mcp/src/cst
                #:*standard-readtable*
                #:%feature-expression-problem)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:with-file-lock)
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
  (:import-from #:cl-mcp/src/utils/printing
                #:bounded-princ-to-string
                #:circular-p)
  (:import-from #:cl-mcp/src/source-reader
                #:*read-with-cl-reader*
                #:source-read-from-string)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-package-context
                #:extract-in-package-name-from-text)
  (:import-from #:cl-mcp/src/utils/nesting
                #:lisp-too-deep-p
                #:+max-lisp-nesting+)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:format-source-line-endings
                #:source-line-break-mask
                #:count-delimiter-depth
                #:diagnose-delimiters
                #:format-delimiter-diagnosis
                #:lexical-state-at
                #:scan-delimiters
                #:bracket-ambiguous-p
                #:format-bracket-warning)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:source-newline
                #:%resolve-named-readtable
                #:%nonstandard-readtable-p
                #:%parse-readtable-designator
                #:%detect-readtable-before-node
                #:%whitespace-char-p
                #:%normalize-paths
                #:%locate-target-form
                #:locate-form-in-text
                #:%feature-prefix-end
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

(defun %check-depth-balance (form-text modified-form match-pos old-end new-end)
  "Return a message describing the net parenthesis difference the patch makes,
or NIL when it makes none. OLD-END and NEW-END bound the regions in FORM-TEXT
and MODIFIED-FORM starting at MATCH-POS. Each region is
counted in its real lexical context, so a parenthesis inside a string or a
comment is not mistaken for code. The message is also withheld when the
lexical state at the end of the replacement differs between the two texts
(NEW-TEXT opened a string or comment that swallows the unchanged suffix):
the region counts would then describe a reclassified suffix, not a real
parenthesis difference, and the reader's own failure is the better report.
A net difference in code guarantees the form will not parse, but the caller
still uses this message only when the patched form actually fails to parse."
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
                        n old-open old-close new-open new-close n n))))))))

(defun %patch-matches (form-text old-text standard-syntax-p)
  "Return the first raw match's bounds and the count, including overlapping matches.
In standard Lisp a bare LF can stand for a formatting CRLF. Other readtables
match raw source. Explicit CRs always match literally."
  (let* ((first-break (and standard-syntax-p (position #\Newline old-text)))
         (breaks (and first-break (find #\Return form-text) (source-line-break-mask form-text)))
         (size (length form-text))
         (from nil) (to nil) (matches 0))
    (labels ((crlf-at-p (i)
               (and breaks (< (1+ i) size)
                    (char= (char form-text i) #\Return)
                    (char= (char form-text (1+ i)) #\Newline)
                    (plusp (sbit breaks i)) (plusp (sbit breaks (1+ i)))))
             (match-end (pos)
               (loop with i = (+ pos first-break)
                     for j from first-break below (length old-text)
                     for ch = (char old-text j)
                     do (when (and (char= ch #\Newline)
                                   (or (zerop j) (char/= (char old-text (1- j)) #\Return)))
                          (cond ((crlf-at-p i)
                                 (incf i))
                                ;; A match cannot begin halfway through CRLF.
                                ((and (plusp i) (crlf-at-p (1- i)))
                                 (return nil))))
                        (unless (and (< i size) (char= ch (char form-text i)))
                          (return nil))
                        (incf i)
                     finally (return i))))
      ;; Search for the literal first line (the entire needle on the usual
      ;; single-line path), then check each candidate's remaining lines.
      (loop for cursor = 0 then (1+ pos)
            while (< cursor size)
            for pos = (search old-text form-text :end1 first-break :start2 cursor)
            while pos
            for end = (if first-break (match-end pos) (+ pos (length old-text)))
            when end
              do (unless from (setf from pos to end))
                 (incf matches)))
    (values from to matches)))

(defun %apply-patch-operation (text node old-text new-text format-lines-p)
  "Patch NODE in TEXT. Return the updated file, updated and original form, and
the raw start, original end and updated end of the matched span in the form."
  (when (zerop (length old-text))
    (error 'arg-validation-error :arg-name "old_text"
           :message "old_text must not be empty"))
  (let* ((start (cst-node-start node))
         (end (cst-node-end node))
         (form-text (subseq text start end)))
    (multiple-value-bind (from to matches) (%patch-matches form-text old-text format-lines-p)
      (unless from
        (let* ((form-value (cst-node-value node))
               (form-id
                 (if (consp form-value)
                     (let ((*print-circle* (circular-p (second form-value))))
                       (format nil "~A ~A" (car form-value) (second form-value)))
                     "matched")))
          (error 'patch-operation-error
                 :reason (format nil "old_text not found in ~A form. ~
                            Note: matching is exact and whitespace-sensitive. ~
                            Use lisp-read-file with name_pattern to see the exact form text. ~
                            old_text begins with: ~S~:[~;...~]"
                                 form-id (subseq old-text 0 (min (length old-text) 60))
                                 (> (length old-text) 60)))))
      (when (> matches 1)
        (error 'patch-operation-error
               :reason (format nil "old_text matches ~D times in the form; ~
                                    provide more surrounding context to match exactly once"
                               matches)))
      (let ((new-end (+ from (length new-text))))
        (when (string= old-text new-text)
          (return-from %apply-patch-operation (values text form-text form-text from to to)))
        (let ((raw (concatenate 'string (subseq form-text 0 from) new-text
                                (subseq form-text to))))
          (multiple-value-bind (modified delta)
              (if (or (not format-lines-p) (not (find #\Newline new-text)))
                  (values raw 0)
                  (format-source-line-endings raw (source-newline text)
                                              :start from :end new-end))
            (values (concatenate 'string (subseq text 0 start) modified (subseq text end))
                    modified form-text from to (+ new-end delta))))))))

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
                     ~@[~%The reader itself reported: ~A.~]~%~
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
  ;; The reader recurses once per level; see CHECK-LISP-NESTING.
  (when (lisp-too-deep-p form-text)
    (error 'patch-operation-error
           :reason (format nil "the patched form nests lists and prefixes more than ~D levels ~
deep, which is deeper than cl-mcp will read"
                           +max-lisp-nesting+)))
  (let* ((*read-eval* nil)
         (custom-rt (%resolve-named-readtable readtable-designator))
         (*readtable*
           (if custom-rt
               custom-rt
               (copy-readtable nil)))
         ;; The file is read with CL:READ under any named readtable, so the
         ;; patched form is too (SOURCE-READ-FROM-STRING).
         (*read-with-cl-reader* (and custom-rt t)))
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
                 (source-read-from-string form-text :eof-error-p nil :eof-value :eof)
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

(defun %patch-located (abs text nodes target old-text new-text readtable file-package-name
                       &optional inactive-p)
  "Replace OLD-TEXT by NEW-TEXT within TARGET, a form of NODES parsed from TEXT
(the contents of ABS), and check the patched form still reads.  Returns
(values UPDATED WOULD-CHANGE BRACKET-WARNING MODIFIED-FORM OLD-SIZE NEW-SIZE); signals
PATCH-OPERATION-ERROR when OLD-TEXT is not found exactly once or the patched
form does not read.  Nothing is written.  INACTIVE-P says TARGET is a form under
a feature expression false here (issue #321): it is then checked past its
expressions (%INACTIVE-FORM-TEXT), which would otherwise skip it again."
  (let* ((readtable-designator (or readtable (%detect-readtable-before-node nodes target)))
         (nonstandard-rt (%nonstandard-readtable-p readtable-designator)))
    (multiple-value-bind (updated modified-form form-text match-pos old-end new-end)
        (%apply-patch-operation text target old-text new-text
                                (member readtable-designator '(nil :standard)))
      (let ((would-change (not (string= text updated)))
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
                                            match-pos old-end new-end)))))
        (when would-change
          (%validate-form-parseable
           (if inactive-p
               (%inactive-form-text modified-form)
               modified-form)
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
        (values updated
                would-change
                (and would-change
                     (not nonstandard-rt)
                     (format-bracket-warning modified-form
                                             :target "the patched form"))
                modified-form (- old-end match-pos) (- new-end match-pos))))))
(defun %inactive-form-text (modified-form)
  "Return MODIFIED-FORM, the patched text of a form under a feature expression
false here (issue #321), past its expressions, for the reader to check -- read
whole, the expressions would skip it again.  The expressions themselves are
checked first, as lisp-edit-form checks a replace's own: each must read and
keep the feature expression grammar (%FEATURE-EXPRESSION-PROBLEM), and a form
must follow them.  Signals PATCH-OPERATION-ERROR otherwise, so a patch never
writes a prefix that would make the file unreadable."
  (multiple-value-bind (form-start spans) (%feature-prefix-end modified-form 0)
    (when (and (null spans)
               (< 1 (length modified-form))
               (char= (char modified-form 0) #\#)
               (member (char modified-form 1) '(#\+ #\-)))
      (error 'patch-operation-error
             :reason (format nil "the patched feature expression cannot be read. No ~
                                  changes were written to disk.")))
    (dolist (span spans)
      (let* ((package (make-package (symbol-name (gensym "CL-MCP-PATCH-FEATURE-")) :use '()))
             (expression
               (unwind-protect
                    (handler-case
                        (let ((*package* package)
                              (*readtable* *standard-readtable*)
                              (*read-eval* nil)
                              (*read-suppress* nil))
                          ;; As the file's parser reads it: a #. is read,
                          ;; never evaluated (#319).
                          (source-read-from-string modified-form
                                                   :start (+ (car span) 2)
                                                   :end (cdr span)))
                      (error (e)
                        (error 'patch-operation-error
                               :reason (format nil "the patched feature expression ~A cannot ~
                                                    be read: ~A. No changes were written to disk."
                                               (subseq modified-form (car span) (cdr span))
                                               (sanitize-condition-text e)))))
                 (delete-package package)))
             (problem (%feature-expression-problem expression)))
        (when problem
          (error 'patch-operation-error
                 :reason (format nil "the patched feature expression ~A is malformed: ~A. ~
                                      No changes were written to disk."
                                 (subseq modified-form (car span) (cdr span)) problem)))))
    (when (>= form-start (length modified-form))
      (error 'patch-operation-error
             :reason (format nil "the patched text has feature expressions and no form ~
                                  after them. No changes were written to disk.")))
    (subseq modified-form form-start)))

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

The whole read -> locate -> patch -> write span runs under
CL-MCP/SRC/FS:WITH-FILE-LOCK for the target file, so cl-mcp's own concurrent
edits of one file are serialised: two patches to two different forms of one
file both survive, instead of the second overwriting the first from a stale
copy. A DRY-RUN call takes the same lock -- it reads the file -- but writes
nothing. The lock lives in this image, so only ONE cl-mcp process's calls are
ordered: an external editor, and equally a second cl-mcp server over the same
checkout, is not coordinated by it.

READTABLE, if provided, specifies a named-readtable designator (e.g., :interpol-syntax)
to use for parsing the file.

Returns the updated file text, whether the file changed, and a
bracket warning or NIL. The warning is set when the patched form reads but
its delimiter scan stops at a ] or } (a possible ) typo that standard syntax
silently accepts as part of a symbol), so the caller can check it. When the
file changed, two further values give the old and new span sizes in characters."
  (unless (and (stringp file-path) (stringp form-type) (stringp form-name))
    (error "file_path, form_type, and form_name must be strings"))
  (unless (and (stringp old-text) (stringp new-text))
    (error "old_text and new_text must be strings"))
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (when (zerop (length old-text))
    (error 'arg-validation-error :arg-name "old_text"
           :message "old_text must not be empty"))
  (with-file-lock ((%normalize-paths file-path))
    (multiple-value-bind (abs rel original nodes target target-snippet _
                          file-package-name inactive-p)
        (%locate-target-form file-path form-type form-name readtable)
      (declare (ignore _))
      (multiple-value-bind (updated would-change bracket-warning modified-form old-size new-size)
          (%patch-located abs original nodes target old-text new-text readtable
                          file-package-name inactive-p)
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
           (values updated t bracket-warning old-size new-size))
          (t
           (values updated nil nil)))))))

(defun lisp-patch-form-many (&key file-path patches dry-run readtable)
  "Apply PATCHES to the file at FILE-PATH together: all of them or none.
Each patch is a plist (:FORM-TYPE :FORM-NAME :OLD-TEXT :NEW-TEXT) and does what
LISP-PATCH-FORM does with those arguments, to the text the patches before it
left -- so a later patch finds its form, and may patch a form an earlier one
changed, as the file will be once written.  The first patch that cannot be
applied (its form not found, OLD-TEXT not found exactly once, a form that no
longer reads) signals PATCH-OPERATION-ERROR naming its number, and nothing is
written: a rename across a function and its callers is never left half done
(issue #326).  The file is read once and written once, under one
WITH-FILE-LOCK.

Returns three values: the updated file text, whether it changed, and a list of
(NUMBER . BRACKET-WARNING) for the patches whose form reads with a ] or } where
) may have been meant.  With DRY-RUN nothing is written and a hash table is
returned instead: would_change, preview (the whole file), file_path, operation
\"patch\", patches (their count), and bracket_warnings when there are any."
  (unless (stringp file-path)
    (error "file_path must be a string"))
  (unless (and (listp patches) patches)
    (error 'arg-validation-error :arg-name "patches"
           :message "patches must be a non-empty array of {form_type, form_name, old_text, new_text}"))
  (unless (member dry-run '(t nil))
    (error "dry-run must be boolean"))
  (loop for patch in patches
        for number from 1
        do (destructuring-bind (&key form-type form-name old-text new-text) patch
             (unless (and (stringp form-type) (stringp form-name)
                          (stringp old-text) (stringp new-text))
               (error 'arg-validation-error :arg-name "patches"
                      :message (format nil "patch ~D needs string form_type, form_name, ~
                                            old_text and new_text"
                                       number)))
             (when (zerop (length old-text))
               (error 'arg-validation-error :arg-name "patches"
                      :message (format nil "patch ~D: old_text must not be empty" number)))))
  (with-file-lock ((%normalize-paths file-path))
    (multiple-value-bind (abs rel) (%normalize-paths file-path)
     ;; Read once, before any patch: a file that cannot be read is about the
     ;; file, not about patch 1, and is reported as it is.
     (multiple-value-bind (original truncated file-length) (fs-read-file abs)
      (when truncated
        (error "~A exceeds the read limit (~@[~D bytes, ~]only ~D characters read); ~
                lisp-patch-form cannot edit files this large."
               (namestring abs) file-length (length original)))
      (let ((total (length patches))
            (warnings '())
            (previews '())
            (text original)
            (file-package-name (extract-in-package-name-from-text original)))
      (loop for patch in patches
            for number from 1
            do (destructuring-bind (&key form-type form-name old-text new-text) patch
                 (flet ((fail (message)
                          (error 'patch-operation-error
                                 :reason (format nil "Patch ~D of ~D (~A ~A): ~A~%Nothing was ~
                                                      written: the patches are applied ~
                                                      together or not at all."
                                                 number total form-type form-name message))))
                   (handler-case
                       (multiple-value-bind (nodes target type inactive-p)
                           (locate-form-in-text abs text form-type form-name readtable)
                         (declare (ignore type))
                         (multiple-value-bind (updated would-change bracket-warning
                                               modified-form)
                             (%patch-located abs text nodes target old-text new-text readtable
                                             file-package-name inactive-p)
                           (declare (ignore would-change))
                           (when bracket-warning
                             (push (cons number bracket-warning) warnings))
                           (push (list number form-type form-name modified-form) previews)
                           (setf text updated)))
                     ;; The file itself does not parse: about the file, not
                     ;; about this patch, so it is reported as it is.
                     (file-unparseable-error (e) (error e))
                     (patch-operation-error (e) (fail (patch-operation-reason e)))
                     (error (e)
                       (fail (sanitize-error-message (bounded-princ-to-string e))))))))
      (let ((would-change (not (string= original text)))
            (warnings (nreverse warnings)))
        (log-event :debug "lisp.patch.form"
                   "path" (namestring abs)
                   "patches" total
                   "dry_run" dry-run
                   "would_change" would-change)
        (cond
          (dry-run
           (let ((result (make-hash-table :test #'equal)))
             (setf (gethash "would_change" result) would-change
                   (gethash "preview" result) text
                   ;; Each patched form as it would be: what a client that
                   ;; renders only the content text gets to see.
                   (gethash "preview_forms" result) (reverse previews)
                   (gethash "file_path" result) (namestring abs)
                   (gethash "operation" result) "patch"
                   (gethash "patches" result) total)
             (when warnings
               (setf (gethash "bracket_warnings" result) warnings))
             result))
          (would-change
           (fs-write-file rel text)
           (values text t warnings))
          (t (values text nil warnings)))))))))

(defun %check-patch-arguments (patches form-type form-name old-text new-text)
  "Signal ARG-VALIDATION-ERROR unless the tool was given either PATCHES or all
four of FORM-TYPE, FORM-NAME, OLD-TEXT and NEW-TEXT, and not both."
  (let ((single-arguments (remove nil (list form-type form-name old-text new-text))))
    (cond
      ((and patches single-arguments)
       (error 'arg-validation-error :arg-name "patches"
              :message "pass either patches or form_type/form_name/old_text/new_text, not both"))
      ((and (not patches) (/= 4 (length single-arguments)))
       (error 'arg-validation-error
              :arg-name (cond ((null form-type) "form_type")
                              ((null form-name) "form_name")
                              ((null old-text) "old_text")
                              (t "new_text"))
              :message "form_type, form_name, old_text and new_text are required (or pass patches)")))))

(defun %patch-plists (patches)
  "Convert the validated PATCHES array of objects to plists for LISP-PATCH-FORM-MANY."
  (map 'list (lambda (patch)
               (list :form-type (gethash "form_type" patch)
                     :form-name (gethash "form_name" patch)
                     :old-text (gethash "old_text" patch)
                     :new-text (gethash "new_text" patch)))
       patches))

(defun %patches-result (id file-path dry-run count outcome)
  "Return the tool result for LISP-PATCH-FORM-MANY's OUTCOME, the list of its
values, for COUNT patches on FILE-PATH."
  (if dry-run
      (let* ((preview (first outcome))
             (would-change (eq t (gethash "would_change" preview)))
             (warnings (gethash "bracket_warnings" preview)))
        (result id
                (apply #'make-ht
                       "path" file-path
                       "operation" "patch"
                       "patches" count
                       "would_change" (json-bool would-change)
                       "preview" (gethash "preview" preview)
                       "content" (text-content
                                  (format nil "Dry-run of ~D patches on ~A (~:[no change~;would ~
                                               change~])~{~%WARNING (patch ~D): ~A~}~
                                               ~{~%~%--- patch ~D (~A ~A) ---~%~A~}"
                                          count file-path would-change
                                          (loop for (number . warning) in warnings
                                                collect number collect warning)
                                          (loop for (number type name form)
                                                  in (gethash "preview_forms" preview)
                                                collect number collect type collect name
                                                collect (%bound-snippet form))))
                       (when warnings
                         (list "bracket_warnings"
                               (map 'vector (lambda (entry)
                                              (make-ht "patch" (car entry)
                                                       "warning" (cdr entry)))
                                    warnings))))))
      (destructuring-bind (updated changed-p warnings) outcome
        (result id
                (apply #'make-ht
                       "path" file-path
                       "operation" "patch"
                       "would_change" (json-bool changed-p)
                       "bytes" (length updated)
                       "content" (text-content
                                  (format nil "~A~{~%WARNING (patch ~D): ~A~}"
                                          (if changed-p
                                              (format nil "Applied ~D patches to ~A"
                                                      count file-path)
                                              (format nil "No change to ~A (every old_text ~
                                                           already matches its new_text)"
                                                      file-path))
                                          (loop for (number . warning) in warnings
                                                collect number collect warning)))
                       (when warnings
                         (list "bracket_warnings"
                               (map 'vector (lambda (entry)
                                              (make-ht "patch" (car entry)
                                                       "warning" (cdr entry)))
                                    warnings))))))))

(define-tool "lisp-patch-form"
  :description "Scoped text replacement within a matched top-level Lisp form.
Finds old_text (exact, whitespace-sensitive match) within the form identified
by form_type and form_name, and replaces it with new_text. old_text must match
exactly once within the form. Under the standard readtable, an LF in old_text can match a formatting CRLF.
Most token-efficient way to make small changes to large forms.
Does NOT auto-repair parentheses — if the patch breaks form structure, it fails
immediately and no changes are written to disk.
Use 'lisp-edit-form' instead when replacing or inserting entire forms."
  :args ((file_path :type :string :required t
                    :description "Target file path: relative to the project root, or absolute inside
it (absolute recommended). Files outside the project root, registered ASDF sources included, are
refused")
         (form_type :type :string
                    :description "Form type to search, e.g., \"defun\", \"defmacro\", \"defmethod\".
A package prefix is ignored: \"asdf:defsystem\" and \"defsystem\" both match
(asdf:defsystem ...). A colon inside the name itself still matches as written
(\"def:thing\" or \"|def:thing|\" for (|DEF:THING| ...)); when the two readings
name different forms, the call is refused as ambiguous.")
         (form_name :type :string
                    :description "Form name to match; for defmethod include specializers,
e.g., \"print-object ((obj my-class) stream)\" (a method whose name is unique matches
by name alone). A name[N] suffix, 0-based, picks the Nth of several matches. For defstruct with
options \"(defstruct (name opts...) ...)\", use just the bare struct name.
Reader macro prefixes #: and : are stripped automatically, so
\"#:my-pkg\" and \"my-pkg\" both match \"(defpackage #:my-pkg ...).\"
A form under a #+/#- expression false in this server (another implementation's
branch) is named by its expressions in front of the name, as the file writes them:
\"#+ecl impl\". A plain name means the form this server reads, and reaches a
skipped one only when no read form has that name.")
         (old_text :type :string
                   :description "Text to find within the matched form.
Whitespace-sensitive and unique within the form. A bare LF also matches a formatting CRLF under the standard
readtable. Literal data and text under other named readtables match exactly.")
         (new_text :type :string
                   :description "Replacement text. With the standard readtable, formatting line breaks use
the file's dominant LF or CRLF ending. Newlines in literal data are kept as supplied.
If new_text opens and closes a different net number of parentheses than old_text
and the patched form no longer parses, the error says exactly how many \")\" to
add or remove.")
         (patches :type :array :item-type :object
                  :description "Several patches to apply to file_path together, in place of
form_type/form_name/old_text/new_text: an array of objects each holding those four keys.
They are applied in order, each to the text the ones before it left (so a later patch may
patch a form an earlier one changed), and the file is written once: if any patch cannot be
applied -- its form not found, its old_text not found exactly once, a form that no longer
reads -- nothing is written and the error names that patch's number. Use it for a rename
across a function and its callers, or a parameter added to a definition and its calls.")
         (dry_run :type :boolean
                  :description "When true, return a preview without writing to disk")
         (readtable :type :string
                    :description "Named-readtable designator for files using custom reader macros.
Supports both keyword style ('interpol-syntax') and package-qualified style
('pokepay-syntax:pokepay-syntax'). NOTE: When specified, the standard CL reader
is used to locate forms instead of Eclector. Only needed when the file does not
declare its own (in-readtable ...): one earlier in the file is honoured automatically.
The patch replaces text in place, so comments are kept."))
  :body
  (let ((readtable-designator
         (handler-case (%parse-readtable-designator readtable)
           (error (e)
             (error 'arg-validation-error :arg-name "readtable"
                    :message (format nil "~A" e))))))
    (%check-patch-arguments patches form_type form_name old_text new_text)
    (handler-case
        (if patches
            (%patches-result id file_path dry_run (length patches)
                             (multiple-value-list
                              (lisp-patch-form-many :file-path file_path
                                                    :patches (%patch-plists patches)
                                                    :dry-run dry_run
                                                    :readtable readtable-designator)))
        (multiple-value-bind (updated changed-p bracket-warning old-size new-size)
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
                                 old-size new-size
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
                                  (list "delta" (- new-size old-size)))
                                (when bracket-warning
                                  (list "bracket_warning" bracket-warning)))))))))
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
                    (sanitize-error-message (bounded-princ-to-string e)))))
          (if (and (protocol-version state)
                   (string>= (protocol-version state) "2025-11-25"))
              (result id (make-ht "content" (text-content msg) "isError" t))
              (rpc-error id -32603 msg)))))))
