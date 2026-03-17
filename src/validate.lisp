;;;; src/validate.lisp

(defpackage #:cl-mcp/src/validate
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
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

(defun %closing (opener)
  (ecase opener
    (#\( #\))
    (#\[ #\])
    (#\{ #\})))

(defun %scan-parens-push-open (stack line col base-offset ch idx)
  (cons (list ch line col (+ base-offset idx)) stack))

(defun %scan-parens-pop-open (stack line col base-offset ch idx)
  (if (null stack)
      (values stack
              (list :ok nil
                    :kind "extra-close"
                    :expected nil
                    :found (string ch)
                    :offset (+ base-offset idx)
                    :line line
                    :column col))
      (destructuring-bind (top-ch top-line top-col top-off) (car stack)
        (declare (ignore top-line top-col top-off))
        (let ((expected (%closing top-ch)))
          (if (char= expected ch)
              (values (cdr stack) nil)
              (values stack
                      (list :ok nil
                            :kind "mismatch"
                            :expected (string expected)
                            :found (string ch)
                            :offset (+ base-offset idx)
                            :line line
                            :column col)))))))

(defstruct scan-state
  (line 1 :type fixnum)
  (col 1 :type fixnum)
  (stack '() :type list)
  (in-string nil :type boolean)
  (escape nil :type boolean)
  (line-comment nil :type boolean)
  (block-depth 0 :type fixnum)
  (block-open-pos 0 :type fixnum))

(defun %scan-handle-line-comment (state ch)
  (when (char= ch #\Newline)
    (setf (scan-state-line-comment state) nil)))

(defun %scan-handle-string (state ch)
  (cond
    ((scan-state-escape state)
     (setf (scan-state-escape state) nil))
    ((char= ch #\\)
     (setf (scan-state-escape state) t))
    ((char= ch #\")
     (setf (scan-state-in-string state) nil))))

(defun %scan-handle-block-comment (state ch next)
  (when (and (char= ch #\|) next (char= next #\#))
    (decf (scan-state-block-depth state))
    t))

(defun %scan-handle-normal (state ch next idx base-offset text)
  "Handle a character in normal (non-string, non-comment) context.
Returns (VALUES err consumed) where CONSUMED is NIL or a positive integer
indicating how many additional characters past CH were consumed."
  (cond
   ((char= ch #\;) (setf (scan-state-line-comment state) t) (values nil nil))
   ((char= ch #\") (setf (scan-state-in-string state) t) (values nil nil))
   ;; Character literal: #\x or #\Space etc.  Skip past entirely so that
   ;; delimiter characters like #\( are not treated as open-parens.
   ((and (char= ch #\#) next (char= next #\\))
    (let ((skip 1))  ; at minimum skip the backslash
      (let ((char-pos (+ idx 2)))
        (when (< char-pos (length text))
          (incf skip)  ; skip the character after backslash
          ;; Named character literals: consume remaining alpha chars
          (when (alpha-char-p (char text char-pos))
            (loop for k from (1+ char-pos) below (length text)
                  while (alpha-char-p (char text k))
                  do (incf skip)))))
      (values nil skip)))
   ((and (char= ch #\#) next (char= next #\|))
    (when (zerop (scan-state-block-depth state))
      (setf (scan-state-block-open-pos state) (+ base-offset idx)))
    (incf (scan-state-block-depth state))
    (values nil 1))
   ((or (char= ch #\() (char= ch #\[) (char= ch #\{))
    (setf (scan-state-stack state)
            (%scan-parens-push-open (scan-state-stack state)
             (scan-state-line state) (scan-state-col state) base-offset ch
             idx))
    (values nil nil))
   ((or (char= ch #\)) (char= ch #\]) (char= ch #\}))
    (multiple-value-bind (new-stack err)
        (%scan-parens-pop-open (scan-state-stack state) (scan-state-line state)
         (scan-state-col state) base-offset ch idx)
      (setf (scan-state-stack state) new-stack)
      (values err nil)))
   (t (values nil nil))))

(defun %scan-advance-position (state ch)
  (cond
    ((char= ch #\Newline)
     (incf (scan-state-line state))
     (setf (scan-state-col state) 1))
    (t
     (incf (scan-state-col state)))))

(defun %scan-parens (text &key (base-offset 0))
  "Return a plist describing balance of delimiters in TEXT.
Keys: :ok (boolean), :kind (string|nil), :expected, :found, :offset, :line, :column."
  (let ((state (make-scan-state)))
    (loop for idx from 0 below (length text)
          for ch = (char text idx)
          for next = (and (< (1+ idx) (length text))
                          (char text (1+ idx))) do
            (cond
              ((scan-state-line-comment state)
               (%scan-handle-line-comment state ch))
              ((scan-state-in-string state)
               (%scan-handle-string state ch))
              ((plusp (scan-state-block-depth state))
               (when (%scan-handle-block-comment state ch next)
                 (incf idx)
                 (incf (scan-state-col state))))
              (t
               (multiple-value-bind (err consumed)
                   (%scan-handle-normal state ch next idx base-offset text)
                 (when err
                   (return-from %scan-parens err))
                 (when consumed
                   (let ((n (if (integerp consumed) consumed 1)))
                     (incf idx n)
                     (incf (scan-state-col state) n))))))
            (%scan-advance-position state ch))
    (when (plusp (scan-state-block-depth state))
      (let* ((open-pos  (scan-state-block-open-pos state))
             (local-pos (- open-pos base-offset))
             (pre       (subseq text 0 (min local-pos (length text))))
             (r-line    (1+ (count #\Newline pre)))
             (col-start (or (position #\Newline pre :from-end t) -1))
             (r-col     (- local-pos col-start)))
        (return-from %scan-parens
          (list :ok nil
                :kind "unclosed-block-comment"
                :expected nil
                :found nil
                :offset open-pos
                :line r-line
                :column r-col))))
    (when (scan-state-stack state)
      (destructuring-bind (ch l c off) (pop (scan-state-stack state))
        (return-from %scan-parens
          (list :ok nil
                :kind "unclosed"
                :expected (string (%closing ch))
                :found nil
                :offset off
                :line l
                :column c))))
    (list :ok t)))

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

(defun lisp-check-parens (&key path code offset limit)
  "Check balanced parentheses/brackets in CODE or PATH slice.
Also checks for reader errors (e.g. unknown dispatch characters, #. with
*read-eval* nil) even when parentheses are balanced.
Returns a hash table with key \"ok\" and, when not ok, \"kind\", and
either \"expected\"/\"found\" (delimiter mismatch) or \"message\" (reader error),
plus a \"position\" hash with \"line\", \"column\", \"offset\"."
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
    (let ((paren-result (%scan-parens text :base-offset base-off))
          (reader-info  (%try-reader-check text base-off)))
      (destructuring-bind (&key ok kind expected found
                                (offset base-off) (line 1) (column 1))
          paren-result
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
                                    "Unbalanced parentheses: ~A~A at line ~D, column ~D~A"
                                    kind ef line col
                                    (if next-tool
                                        " Use lisp-edit-form for existing Lisp files."
                                        ""))))))))
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
            (result id payload)))
      (error (e)
        (result id (make-ht "content" (text-content (format nil "Error: ~A" e))
                            "isError" t))))))
