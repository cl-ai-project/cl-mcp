;;;; src/validate.lisp

(defpackage #:cl-mcp/src/validate
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-resolve-read-path)
  ;; The edit tools' parser itself, not the hook fs installs at load time:
  ;; a direct dependency so the verdict is there in any image that has this
  ;; file, not only when lisp-edit-form-core happened to load first.
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%file-unparseable-by-edit-tools-p)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:*repair-lines-limit*
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content
                #:arg-validation-error #:json-bool)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-error-message)
  (:export #:lisp-check-parens
           #:*check-parens-max-bytes*))

(in-package #:cl-mcp/src/validate)

(defparameter *check-parens-max-bytes* (* 2 1024 1024)
  "Maximum number of characters lisp-check-parens will scan in one call.")

(defun %maybe-add-lisp-edit-guidance (result kind &key overwritable)
  "Attach machine-readable remediation hints for broken Lisp delimiters.
The default next step is lisp-edit-form, which repairs and writes a form.
When OVERWRITABLE -- the file was judged by the edit tools' own parser (the
same verdict fs-write-file's guard uses) to fail on a delimiter no readtable
can fix -- the structural tools cannot locate any form in it, so the next
step is the overwrite path: fs-read-file, apply the fix, fs-write-file with
allow_unparseable_overwrite. Keying this on the parser rather than on the
scan keeps the three tools' verdicts consistent: a file that parses (a
symbol such as a[b), fails for a reader-level reason (#., #?), or was only
read in part never receives an instruction the guard would then refuse."
  (when (member kind '("extra-close" "mismatch" "unclosed"
                       "unclosed-string" "unclosed-block-comment")
                :test #'string=)
    (if overwritable
        (setf (gethash "fix_code" result) "overwrite_with_allow_unparseable"
              (gethash "next_tool" result) "fs-write-file"
              (gethash "required_args" result)
              (vector "path" "content" "allow_unparseable_overwrite"))
        (setf (gethash "fix_code" result) "use_lisp_edit_form"
              (gethash "next_tool" result) "lisp-edit-form"
              (gethash "required_args" result)
              (vector "file_path" "form_type" "form_name" "operation" "content"))))
  result)

(defun %custom-readtable-p (text)
  "Return T if TEXT contains a named-readtable activation.
When a custom readtable is active, the standard CL reader would produce
false-positive reader errors on valid custom syntax."
  (not (null (search "in-readtable" text))))

(defun %project-root-truename ()
  "Return the project root as a resolved directory pathname, or NIL when it is
unset or does not exist. Both sides of a containment test must be resolved:
FS-RESOLVE-READ-PATH returns a truename, and a project root that is itself a
symlink (macOS /tmp, a git worktree) would otherwise never contain it."
  (let ((root cl-mcp/src/project-root:*project-root*))
    (and root
         (ignore-errors
          (uiop:ensure-directory-pathname
           (truename (uiop:ensure-directory-pathname root)))))))

(defun %under-project-root-p (path)
  "Return T when PATH resolves to a file under the project root, i.e. one that
fs-write-file could rewrite. A file elsewhere on the read allow-list (another
ASDF system's source) can be checked but not overwritten, so it must not be
sent to the overwrite path."
  (let ((root (%project-root-truename)))
    (and root
         (ignore-errors (uiop:subpathp (fs-resolve-read-path path) root))
         t)))

(defun %code-verdict (text)
  "Return :PARSED when the editing tools' reader accepts inline TEXT (so a
scan verdict against it is a false positive), else :UNPARSED. The same
question %FILE-UNPARSEABLE-BY-EDIT-TOOLS-P answers for a file, asked of a
snippet: *read-eval* stays off and unknown packages are stubbed, as in the
edit tools."
  (multiple-value-bind (nodes swallowed)
      (ignore-errors (cl-mcp/src/cst:parse-top-level-forms text))
    (declare (ignore nodes))
    ;; IGNORE-ERRORS returns the condition as its second value on failure,
    ;; and PARSE-TOP-LEVEL-FORMS returns a swallowed error there on success
    ;; of its lenient pass: either way a non-NIL second value means "did not
    ;; read cleanly".
    (if swallowed :unparsed :parsed)))

(defun %truncate-message (condition)
  "Extract CONDITION's message for the client: SBCL stream representations
and the trailing \"Stream:\" section are removed (SANITIZE-ERROR-MESSAGE),
then the text is truncated to 200 characters."
  (let ((msg (sanitize-error-message (princ-to-string condition))))
    (if (> (length msg) 200)
        (concatenate 'string (subseq msg 0 197) "...")
        msg)))

(defun %try-reader-check (text base-offset)
  "Attempt to fully read TEXT using the standard CL reader with *READ-EVAL* nil.
Returns a plist with reader error info if a genuine syntax error is detected,
or NIL if the text is clean (or if checking is skipped for known safe reasons).

Plist keys when non-nil: :KIND \"reader-error\", :MESSAGE string,
:OFFSET integer, :LINE integer-or-nil, :COLUMN integer-or-nil.

Skips the reader check (returns NIL) when TEXT contains \"in-readtable\",
because the standard CL reader does not know about custom readtables and would
produce false positives on valid custom syntax (e.g. cl-interpol #?\"...\").

Also returns NIL for package-not-found errors: a missing package is not a
syntax error in the file itself."
  ;; Skip reader check for files using custom readtables.
  (when (%custom-readtable-p text)
    (return-from %try-reader-check nil))
  (with-input-from-string (stream text)
    (handler-case
        (let ((*read-eval* nil))
          (loop (when (eq :eof (read stream nil :eof)) (return nil))))
      (reader-error (e)
        ;; SB-INT:SIMPLE-READER-PACKAGE-ERROR is a subtype of both
        ;; reader-error and package-error in SBCL, so it arrives here
        ;; before the package-error clause below can fire.
        ;; Treat it the same way: a missing package is not a file syntax error.
        (when (typep e 'package-error)
          (return-from %try-reader-check nil))
        (let* ((pos       (or (ignore-errors (file-position stream)) 0))
               (safe-pos  (min pos (length text)))
               (pre       (subseq text 0 safe-pos))
               (line      (1+ (count #\Newline pre)))
               (nl-pos    (position #\Newline pre :from-end t))
               (col-start (or nl-pos -1))
               (col       (- safe-pos col-start)))
          (list :kind    "reader-error"
                :message (%truncate-message e)
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (end-of-file (e)
        ;; end-of-file is NOT a subtype of reader-error in SBCL.
        ;; Capture stream position to give an accurate error location.
        (declare (ignore e))
        (let* ((pos      (or (ignore-errors (file-position stream)) (length text)))
               (safe-pos (min pos (length text)))
               (pre      (subseq text 0 safe-pos))
               (line     (1+ (count #\Newline pre)))
               (nl-pos   (position #\Newline pre :from-end t))
               (col      (- safe-pos (or nl-pos -1))))
          (list :kind    "reader-error"
                :message "unexpected end of file while reading"
                :offset  (+ base-offset pos)
                :line    line
                :column  col)))
      (package-error (e)
        ;; Package-not-found is not a syntax error in the file.
        ;; Return NIL to avoid false positives on valid files that reference
        ;; packages not loaded in the current image.
        (declare (ignore e))
        nil)
      (error (e)
        ;; Catch-all for unexpected non-reader errors.
        ;; Report without position since we have no reliable stream position.
        (list :kind    "reader-error"
              :message (%truncate-message e)
              :offset  base-offset
              :line    nil
              :column  nil)))))

(defun %fix->hash (fix)
  "Convert one (:line :original :repaired :delta :added :removed) plist into a
string-keyed hash. ADDED and REMOVED are the gross edit counts; DELTA is
their difference, so a relocation (\")(a\" -> \"(a)\") is not mistaken for a
no-op by a client reading only delta."
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "line" h) (getf fix :line)
          (gethash "original" h) (getf fix :original)
          (gethash "repaired" h) (getf fix :repaired)
          (gethash "delta" h) (getf fix :delta)
          (gethash "added" h) (getf fix :added 0)
          (gethash "removed" h) (getf fix :removed 0))
    h))

(defun lisp-check-parens (&key path code offset limit)
  "Check balanced parentheses/brackets in CODE or PATH slice.
Also checks for reader errors (e.g. unknown dispatch characters, #. with
*read-eval* nil) even when parentheses are balanced.
Returns a hash table with key \"ok\" and, when not ok, \"kind\", and
either \"expected\"/\"found\" (delimiter mismatch) or \"message\" (reader error),
plus a \"position\" hash with \"line\", \"column\", \"offset\" (absent for
\"too-large\", where nothing was scanned).
Delimiter failures also carry \"likely_fixes\" (vector of line/original/
repaired/delta/added/removed hashes inferred by parinfer, capped at
*REPAIR-LINES-LIMIT* entries with the rest counted in
\"likely_fixes_omitted\"; \"original\" and \"repaired\" are cut to 120
characters plus \"...\" and are then descriptive, not text to write back),
\"next_top_level_line\" when a later top-level form was swallowed, and
\"diagnosis_text\" (the guidance the MCP summary appends; not part of the
MCP payload).
For a PATH that fails the scan, the file is also parsed with the editing
tools' own reader (the verdict fs-write-file's overwrite guard uses): a file
it accepts makes the scan a false positive, so the text says so first and no
fix, field or instruction is attached; a file that fails on a delimiter no
readtable can fix gets the overwrite path as its next step. A window into a
file (OFFSET, or a LIMIT with input remaining) is diagnosed for its kind only."
  (when (and path code)
    (error "Provide either PATH or CODE, not both"))
  (when (and (null path) (null code))
    (error "Either PATH or CODE is required"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (let* ((truncated nil)
         (remaining nil)
         (text (or code
                   (multiple-value-bind (slice truncated-p file-length remaining-p)
                       (fs-read-file path :offset offset :limit limit)
                     (declare (ignore file-length))
                     (setf truncated truncated-p
                           remaining remaining-p)
                     slice)))
         (base-off (or offset 0))
         ;; A window into a file (an offset, or a limit the file filled with
         ;; input still remaining) is a prefix like a truncated read: a slice
         ;; of a valid file looks unbalanced, so no repair hint may be built
         ;; from it.
         (partial (and path
                       (or (plusp base-off)
                           (and limit (= (length text) limit) remaining)))))
    ;; A read cut at the fs cap is a prefix of the file: a verdict on it would
    ;; describe text the file does not end with, so it is reported as too
    ;; large rather than diagnosed. No "position": nothing was scanned.
    (when (or truncated (> (length text) *check-parens-max-bytes*))
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "ok" h) nil
              (gethash "kind" h) "too-large"
              (gethash "expected" h) nil
              (gethash "found" h) nil)
        (return-from lisp-check-parens h)))
    (let* ((diagnosis (diagnose-delimiters text :base-offset base-off))
           ;; The reader check only matters when the delimiters balance.
           (reader-info (and (getf diagnosis :ok) (%try-reader-check text base-off))))
      (destructuring-bind (&key ok kind expected found
                                (offset base-off) (line 1) (column 1)
                                likely-fixes next-top-level-line
                           &allow-other-keys)
          diagnosis
        (let ((h (make-hash-table :test #'equal)))
          (cond
            ((not ok)
             ;; Paren error takes priority
             (setf (gethash "ok" h) nil
                   (gethash "kind" h) kind
                   (gethash "expected" h) expected
                   (gethash "found" h) found)
             (let ((pos (make-hash-table :test #'equal)))
               (setf (gethash "offset" pos) offset
                     (gethash "line" pos) line
                     (gethash "column" pos) column)
               (setf (gethash "position" h) pos))
             ;; The next-step hint and the wording rest on the verdict the
             ;; fs-write-file guard itself gives (the edit tools' parser),
             ;; never on the scan alone, and never for a window. It is
             ;; computed before any text, because a file that parser accepts
             ;; must not receive a likely fix or an instruction at all.
             (multiple-value-bind (overwritable verdict editable-prefix)
                 (cond ((and path (not partial))
                        (ignore-errors
                         (%file-unparseable-by-edit-tools-p
                          (fs-resolve-read-path path) text)))
                       ;; Inline code gets the same reader check: a snippet
                       ;; the editing reader accepts (a] or foo#|bar| as
                       ;; symbols) must not be told to change anything.
                       ((null path) (values nil (%code-verdict text) nil)))
               (let ((false-positive (eq verdict :parsed))
                     (overwrite-hint (and overwritable (%under-project-root-p path))))
                 (when false-positive
                   ;; Marked in the payload too, for a client that reads
                   ;; kind/next_tool and never the text.
                   (setf (gethash "false_positive" h) t))
                 ;; The summary's next-step sentence, built here where the
                 ;; parser's verdict and the project root are known: the
                 ;; path is given relative to the root because that is the
                 ;; only form fs-write-file accepts, and a file whose forms
                 ;; before the breakage were parsed is not called unlocatable.
                 (when overwrite-hint
                   (setf (gethash "guidance_text" h)
                         (format nil ". The file does not parse~:[, so lisp-edit-form ~
                                      and lisp-patch-form cannot locate any form in ~
                                      it~; past its broken form (the forms before it ~
                                      can still be edited with lisp-edit-form; the ~
                                      broken tail needs the overwrite path)~]: read ~
                                      it with fs-read-file, apply the fix below, and ~
                                      write it back with fs-write-file (path=~S, ~
                                      allow_unparseable_overwrite=true). If the file ~
                                      uses custom reader syntax the default reader ~
                                      cannot parse, pass the readtable parameter to ~
                                      lisp-edit-form instead of overwriting."
                                 editable-prefix
                                 (namestring
                                  (uiop:enough-pathname (fs-resolve-read-path path)
                                                        (%project-root-truename))))))
                 (cond
                   (partial
                    ;; A slice of the file: say what was seen, never how to
                    ;; fix it.
                    (setf (gethash "diagnosis_text" h)
                          (format nil "Only a window of ~A was checked (offset ~D, ~D ~
                                       characters), so this may be an artifact of the ~
                                       window and no repair hint is offered; check the ~
                                       whole file for one."
                                  path base-off (length text))))
                   (t
                    (setf (gethash "diagnosis_text" h)
                          ;; The false clause skips the two arguments the true
                          ;; clause's nested directives would consume.
                          (format nil "~:[~2*~;The editing tools' reader parses this ~
                                       ~:[snippet~;file~], so the finding below is ~
                                       most likely a false positive of the ~
                                       standard-syntax scan (a token such as ~
                                       foo#|bar| or a[b reads as one symbol, or a ~
                                       reader macro from an in-readtable consumes ~
                                       that text); no repair is suggested~:[~;, and ~
                                       lisp-edit-form can still edit the file~].~%~]~
                                       ~A~:[~;~%The editing tools' reader also fails ~
                                       on this file, but not on a delimiter (a ~
                                       reader macro or #.), so the overwrite path ~
                                       does not apply; lisp-edit-form will report ~
                                       the reader's complaint.~]"
                                  false-positive (and path t) (and path t)
                                  (format-delimiter-diagnosis
                                   diagnosis :target (or path "code")
                                             :false-positive false-positive)
                                  (eq verdict :reader-level)))
                    ;; Parinfer fixes exist only for paren problems, not for
                    ;; an open #| comment or string, and never for a file the
                    ;; reader accepts.
                    (unless (or false-positive
                                (member kind '("unclosed-block-comment" "unclosed-string")
                                        :test #'string=))
                      (let* ((total (length likely-fixes))
                             (kept (min total *repair-lines-limit*)))
                        (setf (gethash "likely_fixes" h)
                              (map 'vector #'%fix->hash (subseq likely-fixes 0 kept)))
                        (when (> total kept)
                          (setf (gethash "likely_fixes_omitted" h) (- total kept))))
                      ;; Only meaningful for an unclosed form, which is the
                      ;; only kind whose guidance text explains the number.
                      (when (and next-top-level-line (string= kind "unclosed"))
                        (setf (gethash "next_top_level_line" h) next-top-level-line)))))
                 ;; fs-write-file only writes under the project root, so the
                 ;; overwrite hint is promised only for a file it could write.
                 (%maybe-add-lisp-edit-guidance h kind
                                                :overwritable (and overwrite-hint t)))))
            (reader-info
             ;; Parens OK but reader error detected
             (setf (gethash "ok" h) nil
                   (gethash "kind" h) (getf reader-info :kind)
                   (gethash "message" h) (getf reader-info :message))
             (let ((pos (make-hash-table :test #'equal))
                   (r-line (getf reader-info :line))
                   (r-col  (getf reader-info :column)))
               (setf (gethash "offset" pos) (getf reader-info :offset))
               (when r-line   (setf (gethash "line" pos) r-line))
               (when r-col    (setf (gethash "column" pos) r-col))
               (setf (gethash "position" h) pos)))
            (t
             ;; Both checks passed
             (setf (gethash "ok" h) t)))
          h)))))

(define-tool "lisp-check-parens"
  :description "Check balanced parentheses/brackets in a file slice or provided code.
Use this to DIAGNOSE syntax errors in existing files or validate code snippets
before/after editing. Returns the first mismatch position if unbalanced, or
success if balanced. Unbalanced delimiter results include guidance to use
lisp-edit-form for existing Lisp files.

Also detects reader errors (e.g. unknown dispatch characters, #. read-time eval
when *read-eval* is nil) even when parentheses are balanced. In that case the
result has kind: \"reader-error\" and a message field describing the error,
instead of expected/found fields. Files using named-readtables:in-readtable are
exempt from reader checking to avoid false positives.

When a file fails the delimiter scan it is also parsed with the editing tools'
reader (*read-eval* off; an in-file in-readtable is honoured, so its reader
macros run) to pick the next step: a file that reader accepts is reported as a
likely false positive with no fix attached, and a file broken on a delimiter is
sent to the fs-write-file overwrite path."
  :args ((path :type :string
               :description "Absolute path inside project or registered ASDF system
(mutually exclusive with code)")
         (code :type :string
               :description "Raw code string to check (mutually exclusive with path)")
         (offset :type :integer
                 :description "0-based character offset when reading from path")
         (limit :type :integer
                :description "Maximum characters to read from path"))
  :body
  (progn
    (when (and path code)
      (error 'arg-validation-error
             :arg-name "path/code"
             :message "Provide either path or code, not both"))
    (when (and (null path) (null code))
      (error 'arg-validation-error
             :arg-name "path/code"
             :message "Either path or code is required"))
    (handler-case
        (let* ((check-result (lisp-check-parens :path path
                                                :code code
                                                :offset offset
                                                :limit limit))
               (ok (gethash "ok" check-result))
               (next-tool (gethash "next_tool" check-result))
               (summary
                (if ok
                    "Parentheses are balanced"
                    (let* ((kind     (gethash "kind" check-result))
                           (message  (gethash "message" check-result))
                           (expected (gethash "expected" check-result))
                           (found    (gethash "found" check-result))
                           (pos      (gethash "position" check-result))
                           (line     (and pos (gethash "line" pos)))
                           (col      (and pos (gethash "column" pos))))
                      (if (string= kind "reader-error")
                          (format nil "Reader error~@[ at line ~D~]~@[, column ~D~]: ~A"
                                  line col (or message "unknown"))
                          (let ((ef (if (and expected found)
                                        (format nil " (expected ~A, found ~A)" expected found)
                                        ""))
                                ;; The headline must not call an open string
                                ;; or comment a parenthesis problem, since
                                ;; the diagnosis below it says otherwise.
                                (label (cond ((string= kind "unclosed-string")
                                              "Unterminated string")
                                             ((string= kind "unclosed-block-comment")
                                              "Unterminated block comment")
                                             ((string= kind "too-large")
                                              ;; A file hits this through the
                                              ;; fs-read-file cap; inline code
                                              ;; through the 2 MB check limit.
                                              (if path
                                                  (format nil "Input too large to check: ~
                                                   nothing was scanned. The read was ~
                                                   cut at the fs-read-file cap, so the ~
                                                   structural tools and the ~
                                                   fs-write-file overwrite path are ~
                                                   closed for it too; split the file ~
                                                   or edit it outside cl-mcp")
                                                  (format nil "Input too large to check: ~
                                                   nothing was scanned (the code ~
                                                   exceeds the check limit); check a ~
                                                   smaller region")))
                                             (t (format nil "Unbalanced parentheses: ~A"
                                                        kind)))))
                            (format nil
                                    ;; No position for too-large: nothing was
                                    ;; scanned, so line 1 would be noise.
                                    "~A~:[~A at line ~D, column ~D~;~*~*~*~]~A~@[~%~A~]"
                                    label (string= kind "too-large") ef line col
                                    (cond
                                      ;; The overwrite path, worded by
                                      ;; LISP-CHECK-PARENS where the parser's
                                      ;; verdict and the relative path are
                                      ;; known, rather than sending the caller
                                      ;; into a loop with lisp-edit-form.
                                      ((gethash "guidance_text" check-result))
                                      (next-tool
                                       ". Use lisp-edit-form for existing Lisp files.")
                                      (t ""))
                                    (gethash "diagnosis_text" check-result))))))))
          (let* ((kind     (gethash "kind" check-result))
                 (expected (gethash "expected" check-result))
                 (found    (gethash "found" check-result))
                 (message  (gethash "message" check-result))
                 (position (gethash "position" check-result))
                 (payload
                    (make-ht "content" (text-content summary)
                             "ok" (json-bool ok)
                             "kind" kind))
                 (fix-code (gethash "fix_code" check-result))
                 (required-args (gethash "required_args" check-result)))
            (when expected (setf (gethash "expected" payload) expected))
            (when found    (setf (gethash "found" payload) found))
            (when message  (setf (gethash "message" payload) message))
            (when position (setf (gethash "position" payload) position))
            (when fix-code
              (setf (gethash "fix_code" payload) fix-code))
            (when next-tool
              (setf (gethash "next_tool" payload) next-tool))
            (when required-args
              (setf (gethash "required_args" payload) required-args))
            (let ((fixes (gethash "likely_fixes" check-result))
                  (omitted (gethash "likely_fixes_omitted" check-result))
                  (next-line (gethash "next_top_level_line" check-result)))
              (when fixes
                (setf (gethash "likely_fixes" payload) fixes))
              (when omitted
                (setf (gethash "likely_fixes_omitted" payload) omitted))
              (when next-line
                (setf (gethash "next_top_level_line" payload) next-line))
              (when (gethash "false_positive" check-result)
                (setf (gethash "false_positive" payload) t)))
            (result id payload)))
      (error (e)
        (result id (make-ht "content" (text-content (format nil "Error: ~A" e))
                            "isError" t))))))
