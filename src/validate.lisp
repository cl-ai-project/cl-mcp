;;;; src/validate.lisp

(defpackage #:cl-mcp/src/validate
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:*repair-lines-limit*
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content
                #:arg-validation-error #:json-bool)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:lisp-check-parens
           #:*check-parens-max-bytes*))

(in-package #:cl-mcp/src/validate)

(defparameter *check-parens-max-bytes* (* 2 1024 1024)
  "Maximum number of characters lisp-check-parens will scan in one call.")

(defun %maybe-add-lisp-edit-guidance (result kind)
  "Attach machine-readable remediation hints for broken Lisp delimiters."
  (when (member kind '("extra-close" "mismatch" "unclosed") :test #'string=)
    (setf (gethash "fix_code" result) "use_lisp_edit_form"
          (gethash "next_tool" result) "lisp-edit-form"
          (gethash "required_args" result)
          (vector "file_path" "form_type" "form_name" "operation" "content")))
  result)

(defun %custom-readtable-p (text)
  "Return T if TEXT contains a named-readtable activation.
When a custom readtable is active, the standard CL reader would produce
false-positive reader errors on valid custom syntax."
  (not (null (search "in-readtable" text))))

(defun %truncate-message (condition)
  "Extract condition message string, truncating to 200 chars to prevent
SBCL stream representation leakage (e.g. reader-error ~A includes stream content)."
  (let ((msg (format nil "~A" condition)))
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
plus a \"position\" hash with \"line\", \"column\", \"offset\".
Delimiter failures also carry \"likely_fixes\" (vector of line/original/
repaired/delta/added/removed hashes inferred by parinfer, capped at
*REPAIR-LINES-LIMIT* entries with the rest counted in
\"likely_fixes_omitted\"; \"original\" and \"repaired\" are cut to 120
characters plus \"...\" and are then descriptive, not text to write back),
\"next_top_level_line\" when a later top-level form was swallowed, and
\"diagnosis_text\" (the guidance the MCP summary appends; not part of the
MCP payload)."
  (when (and path code)
    (error "Provide either PATH or CODE, not both"))
  (when (and (null path) (null code))
    (error "Either PATH or CODE is required"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (let ((text (or code (fs-read-file path :offset offset :limit limit)))
        (base-off (or offset 0)))
    (when (> (length text) *check-parens-max-bytes*)
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "ok" h) nil
              (gethash "kind" h) "too-large"
              (gethash "expected" h) nil
              (gethash "found" h) nil)
        (let ((pos (make-hash-table :test #'equal)))
          (setf (gethash "offset" pos) base-off
                (gethash "line" pos) 1
                (gethash "column" pos) 1)
          (setf (gethash "position" h) pos))
        (return-from lisp-check-parens h)))
    (let ((diagnosis (diagnose-delimiters text :base-offset base-off))
          (reader-info (%try-reader-check text base-off)))
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
             ;; Every delimiter failure gets guidance text; parinfer fixes
             ;; exist only for paren problems, not for an open #| comment.
             (setf (gethash "diagnosis_text" h)
                   (format-delimiter-diagnosis diagnosis :target (or path "code")))
             (unless (member kind '("unclosed-block-comment" "unclosed-string")
                             :test #'string=)
               (let* ((total (length likely-fixes))
                      (kept (min total *repair-lines-limit*)))
                 (setf (gethash "likely_fixes" h)
                       (map 'vector #'%fix->hash (subseq likely-fixes 0 kept)))
                 (when (> total kept)
                   (setf (gethash "likely_fixes_omitted" h) (- total kept))))
               ;; Only meaningful for an unclosed form, which is the only kind
               ;; whose guidance text explains the number.
               (when (and next-top-level-line (string= kind "unclosed"))
                 (setf (gethash "next_top_level_line" h) next-top-level-line)))
             (%maybe-add-lisp-edit-guidance h kind))
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
exempt from reader checking to avoid false positives."
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
                                        "")))
                            (format nil
                                    "Unbalanced parentheses: ~A~A at line ~D, column ~D~A~@[~%~A~]"
                                    kind ef line col
                                    (if next-tool
                                        " Use lisp-edit-form for existing Lisp files."
                                        "")
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
                (setf (gethash "next_top_level_line" payload) next-line)))
            (result id payload)))
      (error (e)
        (result id (make-ht "content" (text-content (format nil "Error: ~A" e))
                            "isError" t))))))
