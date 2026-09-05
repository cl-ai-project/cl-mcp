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
  (:export #:*repair-lines-limit*
           #:scan-delimiters
           #:diagnose-delimiters
           #:count-delimiter-depth
           #:lexical-state-at
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
  "Handle a character inside a #| ... |# comment. Returns T when a two-character
sequence was consumed: |# closes one level, and a nested #| opens another, so
an inner |# does not end the outer comment early."
  (cond
    ((and (char= ch #\|) next (char= next #\#))
     (decf (scan-state-block-depth state))
     t)
    ((and (char= ch #\#) next (char= next #\|))
     (incf (scan-state-block-depth state))
     t)
    (t nil)))

(defun %scan-handle-normal (state ch next idx base-offset text)
  "Handle a character in normal (non-string, non-comment) context.
Returns (VALUES err consumed) where CONSUMED is NIL or a positive integer
indicating how many additional characters past CH were consumed."
  (cond
   ((char= ch #\;) (setf (scan-state-line-comment state) t) (values nil nil))
   ((char= ch #\") (setf (scan-state-in-string state) t) (values nil nil))
   ;; Single escape outside a string: the next character belongs to a symbol,
   ;; so \( and \) are not delimiters. An escaped newline is left to the
   ;; normal path so the line counter still advances over it.
   ((and (char= ch #\\) next (char/= next #\Newline)) (values nil 1))
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

;; Known limitation: |...| multiple-escape symbols are not recognised here, so
;; a "(" or ")" inside such a symbol name is counted as code and can produce a
;; spurious imbalance report.
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

;; Known limitation: |...| multiple-escape symbols are not recognised here, so
;; a "(" or ")" inside such a symbol name is treated as code and reaches
;; FUNCTION like any other delimiter.

(defun %map-code-characters (text function &key end state-fn)
  "Call FUNCTION with (CH IDX LINE COL) for every character of TEXT that is
outside strings, line comments, block comments, character literals and
single-escaped characters (a \\ outside a string makes the next character
part of a symbol, so \\) is not a delimiter). LINE and COL are 1-based.
When STATE-FN is given it is called with (IDX STATE) for every position the
scan visits, STATE being the lexical state in effect just before IDX; this
lets a caller classify many positions in one pass instead of rescanning.
Scanning stops at position END (default: the end of TEXT) and returns two
values: the lexical state reached there (:code, :string, :string-escape,
:line-comment, :block-comment or :pending) and the block-comment nesting
depth at that point. Nothing at or past END is ever consulted, so the state
at END depends only on the text before it: a construct that would need the
character at END (\\x, #\\x, #|, |#, or a reader prefix such as ' ` , @ #
that needs a following object) is reported as :pending instead.
Known limitation: |...| multiple-escape symbols are not recognised, so a
parenthesis inside one is still reported as code."
  (let ((len (min (length text) (or end (length text))))
        (idx 0) (line 1) (col 1)
        (in-string nil) (escape nil) (line-comment nil) (block-depth 0)
        (pending nil))
    (flet ((state ()
             (cond (pending :pending)
                   (line-comment :line-comment)
                   ((and in-string escape) :string-escape)
                   (in-string :string)
                   ((plusp block-depth) :block-comment)
                   (t :code))))
      (loop while (< idx len)
            do (let* ((ch (char text idx))
                      (last-p (>= (1+ idx) len))
                      (next (and (not last-p) (char text (1+ idx)))))
                 (when state-fn
                   (funcall state-fn idx (state)))
                 (cond
                   (line-comment
                    (when (char= ch #\Newline) (setf line-comment nil)))
                   (in-string
                    (cond (escape (setf escape nil))
                          ((char= ch #\\) (setf escape t))
                          ((char= ch #\") (setf in-string nil))))
                   ((plusp block-depth)
                    (cond ((and last-p (or (char= ch #\|) (char= ch #\#)))
                           ;; |# or a nested #| would need the character at END.
                           (setf pending t)
                           (return))
                          ((and (char= ch #\|) next (char= next #\#))
                           (decf block-depth) (incf idx) (incf col))
                          ((and (char= ch #\#) next (char= next #\|))
                           (incf block-depth) (incf idx) (incf col))))
                   ((and last-p (find ch "\\#'`,@"))
                    ;; A two-character construct or a reader prefix would reach
                    ;; past END: stop here and report the token as pending
                    ;; instead of consulting text the caller asked us not to.
                    (setf pending t)
                    (return))
                   ((char= ch #\;) (setf line-comment t))
                   ((char= ch #\") (setf in-string t))
                   ((char= ch #\\)
                    ;; Single escape outside a string: skip the escaped character.
                    (incf idx)
                    (if (char= next #\Newline)
                        (setf line (1+ line) col 0)
                        (incf col)))
                   ((and (char= ch #\#) (char= next #\|))
                    (incf block-depth) (incf idx) (incf col))
                   ((and (char= ch #\#) (char= next #\\))
                    ;; #\x or #\Name: skip the backslash and the literal itself,
                    ;; but never past END -- a literal cut off by the scan limit
                    ;; is reported as pending, like a lone \ or # would be.
                    (when (>= (+ idx 2) len)
                      (setf pending t)
                      (return))
                    (let ((skip 2))
                      (when (alpha-char-p (char text (+ idx 2)))
                        (loop for k from (+ idx 3) below len
                              while (alpha-char-p (char text k))
                              do (incf skip)))
                      (incf idx skip)
                      (incf col skip)))
                   (t (funcall function ch idx line col)))
                 (if (char= ch #\Newline)
                     (setf line (1+ line) col 1)
                     (incf col))
                 (incf idx)))
      (when state-fn
        (funcall state-fn len (state)))
      (values (state) block-depth))))

(defun lexical-state-at (text pos)
  "Return the lexical state in effect just before position POS of TEXT, as
scanned from its beginning: :code, :string, :string-escape (inside a string
with a backslash pending, so the next character is not a delimiter),
:line-comment, :block-comment, or :pending when the character just before
POS starts a two-character construct (\\x, #\\x, #|) that would consume the
character at POS. The second value is the block-comment nesting depth, so
two :block-comment states at different depths can be told apart.
lisp-patch-form compares this at the end of a replacement in the original
and patched form texts; a mismatch means new_text opened a string or comment
that swallows the unchanged suffix, so region parenthesis counts no longer
describe a real delimiter difference."
  (%map-code-characters text
                        (lambda (ch idx line col)
                          (declare (ignore ch idx line col))
                          nil)
                        :end pos))

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

(defun %code-state-mask (text)
  "Return a simple-vector of length (1+ (length TEXT)) whose element I is the
lexical state in effect just before position I (:code, :string,
:string-escape, :line-comment, :block-comment, :pending), computed in one
pass. Positions the scan skips as parts of a token (the character behind a
\\, the body of a #\\x literal) are marked :token, which counts as non-code.
The final element is the state at the end of TEXT."
  (let ((mask (make-array (1+ (length text)) :initial-element :token)))
    (%map-code-characters text
                          (lambda (ch idx line col)
                            (declare (ignore ch idx line col))
                            nil)
                          :state-fn (lambda (idx state) (setf (svref mask idx) state)))
    mask))

(defun %any-fix-outside-code-p (text repaired fixes)
  "Return T when any of FIXES (from REPAIR-LINE-DIFFERENCES on TEXT and
REPAIRED) changes a position that is not code in TEXT: inside a string or
a comment, or within a token. Parinfer is not comment-aware, so such a
change would be ignored by the reader and must not be offered as a fix.
The change position is the first differing character of the line (a removed
\")\" ) or the end of the line (an appended one). Linear in the size of TEXT:
the lexical states come from one %CODE-STATE-MASK pass and the lines from
one split each."
  (when fixes
    (let ((mask (%code-state-mask text))
          (orig-lines (coerce (split-string text :separator '(#\Newline)) 'vector))
          (rep-lines (coerce (split-string repaired :separator '(#\Newline)) 'vector))
          (line-starts (make-array (1+ (count #\Newline text)) :fill-pointer 0)))
      (vector-push 0 line-starts)
      (loop for i from 0 below (length text)
            when (char= (char text i) #\Newline)
              do (vector-push (1+ i) line-starts))
      (loop for fix in fixes
            for line = (getf fix :line)
            for orig = (aref orig-lines (1- line))
            for rep = (aref rep-lines (1- line))
            for offset = (or (mismatch orig rep) (length orig))
            for pos = (min (+ (aref line-starts (1- line)) offset) (length text))
            unless (member (svref mask pos) '(:code :pending))
              do (return t)))))

(defun %likely-fixes (text)
  "Run parinfer on TEXT and return (VALUES fixes repair-failed-p).
FIXES is the line diff from REPAIR-LINE-DIFFERENCES. REPAIR-FAILED-P is T
when the repaired text is still unbalanced per SCAN-DELIMITERS, which also
covers a ] or } closing a paren, or when parinfer changed text that is not
code (see %ANY-FIX-OUTSIDE-CODE-P). FIXES is NIL in either case.
Balanced [...] or {...} pairs, as used by some reader macros, are accepted."
  (let ((repaired (apply-indent-mode text)))
    (if (getf (scan-delimiters repaired) :ok)
        (let ((fixes (repair-line-differences text repaired)))
          (if (%any-fix-outside-code-p text repaired fixes)
              (values nil t)
              (values fixes nil)))
        (values nil t))))

(defun count-delimiter-depth (text &key (start 0) end)
  "Return two values: the number of \"(\" and the number of \")\" in TEXT
outside strings, comments and character literals. Only round parentheses
are counted; [ and { are constituent characters in Common Lisp.
When START/END are given, only characters at positions START <= i < END are
counted, but TEXT is still scanned from its beginning, so the region is
judged in its real lexical context: a parenthesis inside a string or comment
that opened before START is not code."
  (let ((opens 0)
        (closes 0)
        (end (or end (length text))))
    (%map-code-characters
     text
     (lambda (ch idx line col)
       (declare (ignore line col))
       (when (and (<= start idx) (< idx end))
         (case ch
           (#\( (incf opens))
           (#\) (incf closes))))))
    (values opens closes)))

(defun %strip-trailing-cr (line)
  "Return LINE without a trailing #\\Return, so CRLF text does not leak a raw
carriage return into the rendered guidance."
  (let ((len (length line)))
    (if (and (plusp len) (char= (char line (1- len)) #\Return))
        (subseq line 0 (1- len))
        line)))

(defun %bound-line (line)
  "Return LINE cut to 120 characters plus \"...\" when it is longer, so a
single very long physical line cannot inflate a diagnosis or the
likely_fixes payload."
  (let ((limit 120))
    (if (> (length line) limit)
        (concatenate 'string (subseq line 0 limit) "...")
        line)))

(defun %paren-edit-counts (orig rep)
  "Return (VALUES added removed) when REP is ORIG with some \")\" characters
deleted and/or \")\" characters appended, which is parinfer's edit model:
ADDED is the number appended, REMOVED the number deleted. Returns NIL NIL
when the lines differ in any other way (whitespace, a rewrapped token), so
the caller can fall back to a plain count difference."
  (let ((i 0) (j 0) (removed 0)
        (n (length orig)) (m (length rep)))
    (loop while (and (< i n) (< j m))
          do (cond ((char= (char orig i) (char rep j)) (incf i) (incf j))
                   ((char= (char orig i) #\)) (incf removed) (incf i))
                   (t (return-from %paren-edit-counts (values nil nil)))))
    (loop while (< i n)
          do (if (char= (char orig i) #\))
                 (progn (incf removed) (incf i))
                 (return-from %paren-edit-counts (values nil nil))))
    (let ((added 0))
      (loop while (< j m)
            do (if (char= (char rep j) #\))
                   (progn (incf added) (incf j))
                   (return-from %paren-edit-counts (values nil nil))))
      (values added removed))))

(defun repair-line-differences (original repaired)
  "Compare ORIGINAL and REPAIRED (parinfer output) line by line.
Return a list of (:line n :original str :repaired str :delta d :added a
:removed r) for every line that changed, where A is the number of \")\"
parinfer appended, R the number it removed, and D = A - R. A line whose net
D is 0 is still reported when it really changed (\")(a\" -> \"(a)\"); only
differences with no parenthesis edits at all (whitespace, a trailing
carriage return) are skipped. A trailing #\\Return is stripped from both
sides before comparing. The stored :original and :repaired are bounded by
%BOUND-LINE so a pathological single-line input cannot inflate the response.
Both texts must have the same number of lines, which parinfer guarantees."
  (loop for raw-orig in (split-string original :separator '(#\Newline))
        for raw-rep in (split-string repaired :separator '(#\Newline))
        for line from 1
        for orig = (%strip-trailing-cr raw-orig)
        for rep = (%strip-trailing-cr raw-rep)
        for (added removed) = (multiple-value-list (%paren-edit-counts orig rep))
        for delta = (if added
                        (- added removed)
                        (- (count #\) rep) (count #\) orig)))
        unless (or (string= orig rep)
                   (if added
                       (zerop (+ added removed))
                       (zerop delta)))
          collect (list :line line
                        :original (%bound-line orig)
                        :repaired (%bound-line rep)
                        :delta delta
                        :added (or added (max delta 0))
                        :removed (or removed (max (- delta) 0)))))

(defparameter *repair-lines-limit* 10
  "Maximum number of likely-fix entries rendered by FORMAT-REPAIR-LINES and
serialized by lisp-check-parens; the rest are summarized as an omitted count
so a response stays bounded however many lines parinfer changed.")

(defun format-repair-lines (fixes)
  "Render FIXES (from REPAIR-LINE-DIFFERENCES) as indented lines, each
preceded by a newline, e.g. \"  line 2: \\\"  (let ((y 1)\\\"  ->  add 1 \\\")\\\"\".
At most 10 entries are rendered in full; when more lines changed, a trailing
\"  ... and N more changed lines\" names the remainder, so a wholesale
reindentation cannot flood the guidance."
  (let* ((limit *repair-lines-limit*)
         (total (length fixes))
         (shown (if (> total limit) (subseq fixes 0 limit) fixes)))
    (with-output-to-string (s)
      (dolist (fix shown)
        (let ((delta (getf fix :delta)))
          (if (zerop delta)
              ;; One ) removed and one appended (\")(a\" -> \"(a)\"): the net
              ;; count says nothing useful, so show the resulting line.
              (format s "~%  line ~D: ~S  ->  ~S"
                      (getf fix :line)
                      (getf fix :original)
                      (getf fix :repaired))
              (format s "~%  line ~D: ~S  ->  ~A ~D \")\""
                      (getf fix :line)
                      (getf fix :original)
                      (if (minusp delta) "remove" "add")
                      (abs delta)))))
      (when (> total limit)
        (format s "~%  ... and ~D more changed lines" (- total limit))))))

(defun format-delimiter-diagnosis (diagnosis &key (target "code"))
  "Render DIAGNOSIS (from DIAGNOSE-DELIMITERS) as guidance text.
Only failure plists are rendered: when DIAGNOSIS is balanced (:ok true) this
returns NIL, because there is nothing to explain.
TARGET is the subject of the first sentence: \"code\", \"content\", \"new_text\",
or a file path. The likely-fix block is included only when parinfer produced
one; otherwise a repair-failed sentence is printed instead."
  (when (getf diagnosis :ok)
    (return-from format-delimiter-diagnosis nil))
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
        ((and (string= kind "mismatch") (equal expected ")"))
         (format s "Unbalanced parentheses in ~A: expected ~S but found ~S at line ~D, column ~D.~%~
                    \"]\" and \"}\" are ordinary symbol characters in Common Lisp and cannot be ~
                    auto-repaired.~%~
                    Replace it with ~S."
                 target expected found line column expected))
        ((string= kind "mismatch")
         ;; EXPECTED is "]" or "}": the opener was a bracket or brace, which in
         ;; Common Lisp may legitimately be part of a symbol name.  Advising a
         ;; replacement here would break valid code such as (list [a b).
         (format s "Unbalanced parentheses in ~A: expected ~S but found ~S at line ~D, column ~D.~%~
                    The ~S opened earlier is being treated as an opening delimiter; if it is ~
                    part of a symbol name this diagnosis is a false positive."
                 target expected found line column
                 (if (equal expected "]") "[" "{")))
        ((string= kind "unclosed-block-comment")
         (format s "Unterminated block comment in ~A: the #| opened at line ~D, ~
                    column ~D was never closed. Close it with |#."
                 target line column))
        (t
         (format s "Unbalanced parentheses in ~A: ~A at line ~D, column ~D."
                 target kind line column)))
      (cond
        (fixes
         (format s "~%Likely fix, inferred from indentation:~A" (format-repair-lines fixes)))
        (failed
         (format s "~%Automatic repair could not produce a readable form; ~
                    fix the delimiters by hand.")))
      (when (and next-line (string= kind "unclosed"))
        (format s "~%Next top-level form begins at line ~D, ~
                   so the missing ~S must come before it."
                next-line (or expected ")"))))))
