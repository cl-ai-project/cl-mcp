;;;; src/paren-diagnostics.lisp
;;;;
;;;; Delimiter diagnostics shared by lisp-check-parens, lisp-edit-form and
;;;; lisp-patch-form: a balance scanner, a parinfer-based "likely fix" line
;;;; diff, a column-0 heuristic, an open/close counter, and one formatter so
;;;; all three tools describe the same breakage with the same words.

(defpackage #:cl-mcp/src/paren-diagnostics
  (:use #:cl)
  (:import-from #:cl-mcp/src/parinfer
                #:apply-indent-mode)
  (:import-from #:uiop
                #:split-string)
  (:documentation "Delimiter balance diagnostics with repair hints.")
  (:export #:scan-delimiters
           #:diagnose-delimiters
           #:count-delimiter-depth
           #:repair-line-differences
           #:format-repair-lines
           #:format-delimiter-diagnosis))

(in-package #:cl-mcp/src/paren-diagnostics)

;;; ---------------------------------------------------------------------------
;;; Balance scanner (moved verbatim from validate.lisp)
;;; ---------------------------------------------------------------------------

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

(defun scan-delimiters (text &key (base-offset 0))
  "Return a plist describing balance of delimiters in TEXT.
Keys: :ok (boolean), :kind (string|nil), :expected, :found, :offset, :line, :column.
:kind is one of \"extra-close\", \"mismatch\", \"unclosed\",
\"unclosed-block-comment\". BASE-OFFSET is added to :offset only; :line and
:column are always relative to the start of TEXT."
  (let ((state (make-scan-state))
        (len (length text))
        (idx 0))
    (loop while (< idx len)
          for ch = (char text idx)
          for next = (and (< (1+ idx) len) (char text (1+ idx)))
          do
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
                   (return-from scan-delimiters err))
                 (when consumed
                   (let ((n (if (integerp consumed) consumed 1)))
                     (incf idx n)
                     (incf (scan-state-col state) n))))))
            (%scan-advance-position state ch)
            (incf idx))
    (when (plusp (scan-state-block-depth state))
      (let* ((open-pos  (scan-state-block-open-pos state))
             (local-pos (- open-pos base-offset))
             (pre       (subseq text 0 (min local-pos (length text))))
             (r-line    (1+ (count #\Newline pre)))
             (col-start (or (position #\Newline pre :from-end t) -1))
             (r-col     (- local-pos col-start)))
        (return-from scan-delimiters
          (list :ok nil
                :kind "unclosed-block-comment"
                :expected nil
                :found nil
                :offset open-pos
                :line r-line
                :column r-col))))
    (when (scan-state-stack state)
      (destructuring-bind (ch l c off) (pop (scan-state-stack state))
        (return-from scan-delimiters
          (list :ok nil
                :kind "unclosed"
                :expected (string (%closing ch))
                :found nil
                :offset off
                :line l
                :column c))))
    (list :ok t)))

;;; ---------------------------------------------------------------------------
;;; Stubs (replaced in Tasks 2 and 3)
;;; ---------------------------------------------------------------------------

(defun diagnose-delimiters (text &key (base-offset 0))
  "Scan TEXT like SCAN-DELIMITERS and, when it is unbalanced, add repair hints:
:likely-fixes (parinfer line diff), :repair-failed, :next-top-level-line,
and for kind \"unclosed\" also :unclosed-form-line and :unclosed-form-head.
A balanced TEXT or an unclosed block comment returns the plain scan plist."
  (let* ((scan (scan-delimiters text :base-offset base-offset))
         (kind (getf scan :kind)))
    (if (or (getf scan :ok)
            (string= kind "unclosed-block-comment"))
        scan
        (multiple-value-bind (fixes failed) (%likely-fixes text)
          (append scan
                  (list :likely-fixes fixes
                        :repair-failed failed
                        :next-top-level-line (%next-top-level-line text))
                  (when (string= kind "unclosed")
                    (let ((line (getf scan :line)))
                      (list :unclosed-form-line line
                            :unclosed-form-head (%form-head text line)))))))))

(defun %map-code-characters (text function)
  "Call FUNCTION with (CH IDX LINE COL) for every character of TEXT that is
outside strings, line comments, block comments and character literals.
LINE and COL are 1-based."
  (let ((len (length text)) (idx 0) (line 1) (col 1)
        (in-string nil) (escape nil) (line-comment nil) (block-depth 0))
    (loop while (< idx len)
          do (let ((ch (char text idx))
                   (next (and (< (1+ idx) len) (char text (1+ idx)))))
               (cond
                 (line-comment
                  (when (char= ch #\Newline) (setf line-comment nil)))
                 (in-string
                  (cond (escape (setf escape nil))
                        ((char= ch #\\) (setf escape t))
                        ((char= ch #\") (setf in-string nil))))
                 ((plusp block-depth)
                  (cond ((and (char= ch #\|) next (char= next #\#))
                         (decf block-depth) (incf idx) (incf col))
                        ((and (char= ch #\#) next (char= next #\|))
                         (incf block-depth) (incf idx) (incf col))))
                 ((char= ch #\;) (setf line-comment t))
                 ((char= ch #\") (setf in-string t))
                 ((and (char= ch #\#) next (char= next #\|))
                  (incf block-depth) (incf idx) (incf col))
                 ((and (char= ch #\#) next (char= next #\\))
                  ;; #\x or #\Name: skip the backslash and the literal itself.
                  (let ((skip 1))
                    (when (< (+ idx 2) len)
                      (incf skip)
                      (when (alpha-char-p (char text (+ idx 2)))
                        (loop for k from (+ idx 3) below len
                              while (alpha-char-p (char text k))
                              do (incf skip))))
                    (incf idx skip)
                    (incf col skip)))
                 (t (funcall function ch idx line col)))
               (if (char= ch #\Newline)
                   (setf line (1+ line) col 1)
                   (incf col))
               (incf idx)))))

(defun %next-top-level-line (text)
  "Return the 1-based line of the first \"(\" in column 1 that appears while an
earlier form is still open, or NIL. Such a line almost always means the
previous top-level form was never closed."
  (let ((depth 0))
    (%map-code-characters
     text
     (lambda (ch idx line col)
       (declare (ignore idx))
       (case ch
         (#\( (when (and (= col 1) (plusp depth))
                (return-from %next-top-level-line line))
              (incf depth))
         (#\) (when (plusp depth) (decf depth))))))
    nil))

(defun %line-text (text line)
  "Return the LINE-th (1-based) line of TEXT, or \"\" when out of range."
  (let ((lines (split-string text :separator '(#\Newline))))
    (if (<= 1 line (length lines))
        (nth (1- line) lines)
        "")))

(defun %form-head (text line)
  "Return the trimmed first 40 characters of LINE in TEXT, for naming a form."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return) (%line-text text line))))
    (if (> (length trimmed) 40)
        (subseq trimmed 0 40)
        trimmed)))

(defun %stray-bracket-p (text)
  "Return T when TEXT contains ] or } outside strings, comments and char literals."
  (%map-code-characters
   text
   (lambda (ch idx line col)
     (declare (ignore idx line col))
     (when (or (char= ch #\]) (char= ch #\}))
       (return-from %stray-bracket-p t))))
  nil)

(defun %likely-fixes (text)
  "Run parinfer on TEXT and return (VALUES fixes repair-failed-p).
FIXES is the line diff from REPAIR-LINE-DIFFERENCES. REPAIR-FAILED-P is T
when the repaired text still has a stray ] or }, or is still unbalanced;
FIXES is NIL in that case."
  (let ((repaired (apply-indent-mode text)))
    (if (or (%stray-bracket-p repaired)
            (not (getf (scan-delimiters repaired) :ok)))
        (values nil t)
        (values (repair-line-differences text repaired) nil))))

(defun count-delimiter-depth (text)
  "Return two values: the number of \"(\" and the number of \")\" in TEXT
outside strings, comments and character literals. Only round parentheses
are counted; [ and { are constituent characters in Common Lisp."
  (let ((opens 0) (closes 0))
    (%map-code-characters
     text
     (lambda (ch idx line col)
       (declare (ignore idx line col))
       (case ch
         (#\( (incf opens))
         (#\) (incf closes)))))
    (values opens closes)))

(defun repair-line-differences (original repaired)
  "Compare ORIGINAL and REPAIRED (parinfer output) line by line.
Return a list of (:line n :original str :repaired str :delta d) for every
line that changed, where D is the number of \")\" added (negative if removed).
Both texts must have the same number of lines, which parinfer guarantees."
  (loop for orig in (split-string original :separator '(#\Newline))
        for rep in (split-string repaired :separator '(#\Newline))
        for line from 1
        unless (string= orig rep)
          collect (list :line line
                        :original orig
                        :repaired rep
                        :delta (- (count #\) rep) (count #\) orig)))))

(defun format-repair-lines (fixes)
  "Render FIXES (from REPAIR-LINE-DIFFERENCES) as indented lines, each
preceded by a newline, e.g. \"  line 2: \\\"  (let ((y 1)\\\"  ->  add 1 \\\")\\\"\"."
  (with-output-to-string (s)
    (dolist (fix fixes)
      (let ((delta (getf fix :delta)))
        (format s "~%  line ~D: ~S  ->  ~A ~D \")\""
                (getf fix :line)
                (getf fix :original)
                (if (minusp delta) "remove" "add")
                (abs delta))))))

(defun format-delimiter-diagnosis (diagnosis &key (target "code"))
  "Render DIAGNOSIS (from DIAGNOSE-DELIMITERS) as guidance text.
TARGET is the subject of the first sentence: \"code\", \"content\", \"new_text\",
or a file path. The likely-fix block is included only when parinfer produced
one; otherwise a repair-failed sentence is printed instead."
  (let ((kind (getf diagnosis :kind))
        (line (getf diagnosis :line))
        (column (getf diagnosis :column))
        (expected (getf diagnosis :expected))
        (found (getf diagnosis :found))
        (fixes (getf diagnosis :likely-fixes))
        (failed (getf diagnosis :repair-failed))
        (next-line (getf diagnosis :next-top-level-line)))
    (with-output-to-string (s)
      (cond
        ((string= kind "unclosed")
         (format s "Unbalanced parentheses in ~A: unclosed (form starting at line ~D: ~S)."
                 target (getf diagnosis :unclosed-form-line)
                 (getf diagnosis :unclosed-form-head)))
        ((string= kind "extra-close")
         (format s "Unbalanced parentheses in ~A: extra ~S at line ~D, column ~D.~%~
                    Either remove that ~S or check for a form opened earlier that was never closed."
                 target found line column found))
        ((string= kind "mismatch")
         (format s "Unbalanced parentheses in ~A: expected ~S but found ~S at line ~D, column ~D.~%~
                    \"]\" and \"}\" are ordinary symbol characters in Common Lisp and cannot be auto-repaired.~%~
                    Replace it with ~S."
                 target expected found line column expected))
        (t
         (format s "Unbalanced parentheses in ~A: ~A at line ~D, column ~D."
                 target kind line column)))
      (cond
        (fixes
         (format s "~%Likely fix, inferred from indentation:~A" (format-repair-lines fixes)))
        (failed
         (format s "~%Automatic repair could not produce a readable form; fix the delimiters by hand.")))
      (when (and next-line (string= kind "unclosed"))
        (format s "~%Next top-level form begins at line ~D, so the missing \")\" must come before it."
                next-line)))))
