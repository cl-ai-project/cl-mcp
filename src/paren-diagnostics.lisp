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
           #:format-delimiter-diagnosis
           #:bracket-ambiguous-p
           #:opener-ambiguous-p
           #:format-bracket-warning
           #:format-overwrite-recovery
           #:format-relocation-note
           #:relocating-fix-lines))

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
  ;; Where the current string literal opened, so an unterminated one can be
  ;; reported at its opening quote rather than as a paren problem.
  (string-open-line 1 :type fixnum)
  (string-open-col 1 :type fixnum)
  (string-open-pos 0 :type fixnum)
  ;; Inside a |...| multiple-escape symbol: delimiters there are symbol text.
  (in-multi-escape nil :type boolean)
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

(defvar *scan-brackets* t
  "When true (the default), SCAN-DELIMITERS tracks [ ] and { } as delimiters
alongside ( ). Bound to NIL for a rescan that must judge the parentheses
alone, treating brackets as the symbol characters they are in standard
syntax.")

(defun %scan-handle-normal (state ch next idx base-offset text)
  "Handle a character in normal (non-string, non-comment) context.
Returns (VALUES err consumed) where CONSUMED is NIL or a positive integer
indicating how many additional characters past CH were consumed."
  (cond
   ((char= ch #\;) (setf (scan-state-line-comment state) t) (values nil nil))
   ((char= ch #\")
    (setf (scan-state-in-string state) t
          (scan-state-string-open-line state) (scan-state-line state)
          (scan-state-string-open-col state) (scan-state-col state)
          (scan-state-string-open-pos state) (+ base-offset idx))
    (values nil nil))
   ;; Single escape outside a string: the next character belongs to a symbol,
   ;; so \( and \) are not delimiters. An escaped newline is left to the
   ;; normal path so the line counter still advances over it.
   ((and (char= ch #\\) next (char/= next #\Newline)) (values nil 1))
   ;; Multiple escape: everything up to the matching | is symbol text. A
   ;; block-comment opener never reaches here as |, since #| is recognised
   ;; at its # below, and |# only occurs inside a block comment.
   ((char= ch #\|) (setf (scan-state-in-multi-escape state) t) (values nil nil))
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
   ((or (char= ch #\()
        (and *scan-brackets* (or (char= ch #\[) (char= ch #\{))))
    (setf (scan-state-stack state)
            (%scan-parens-push-open (scan-state-stack state)
             (scan-state-line state) (scan-state-col state) base-offset ch
             idx))
    (values nil nil))
   ((or (char= ch #\))
        (and *scan-brackets* (or (char= ch #\]) (char= ch #\}))))
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

(defun %scan-handle-multi-escape (state ch)
  "Handle a character inside a |...| multiple-escape symbol: a \\ escapes the
next character (so \\| does not end the symbol), an unescaped | ends it,
and everything else -- parentheses included -- is symbol text."
  (cond
    ((scan-state-escape state)
     (setf (scan-state-escape state) nil))
    ((char= ch #\\)
     (setf (scan-state-escape state) t))
    ((char= ch #\|)
     (setf (scan-state-in-multi-escape state) nil))))

(defun scan-delimiters (text &key (base-offset 0) (brackets t))
  "Return a plist describing balance of delimiters in TEXT.
Keys: :ok (boolean), :kind (string|nil), :expected, :found, :offset, :line, :column.
With BRACKETS true (the default) [ ] and { } are tracked as delimiters as
well; with BRACKETS NIL only ( ) count and brackets are symbol characters.
:kind is one of \"extra-close\", \"mismatch\", \"unclosed\",
\"unclosed-block-comment\", \"unclosed-string\". BASE-OFFSET is added to
:offset only; :line and :column are always relative to the start of TEXT.
Known divergence from the reader, shared by every lexer here: #| is taken
to open a block comment even directly after token constituents, where the
reader would read foo#|bar| as one symbol."
  (let ((*scan-brackets* brackets)
        (state (make-scan-state))
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
              ((scan-state-in-multi-escape state)
               (%scan-handle-multi-escape state ch))
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
                     ;; #\<Newline> is the one skipped construct that spans
                     ;; a line: keep the line counter honest over it.
                     (if (find #\Newline text :start (1+ idx)
                                              :end (min len (+ idx n 1)))
                         (setf (scan-state-line state) (1+ (scan-state-line state))
                               (scan-state-col state) 0)
                         (incf (scan-state-col state) n))
                     (incf idx n))))))
            (%scan-advance-position state ch)
            (incf idx))
    (when (scan-state-in-string state)
      ;; Input ended inside a string literal: report the opening quote, not
      ;; the enclosing form, so the caller is not sent chasing parentheses.
      (return-from scan-delimiters
        (list :ok nil
              :kind "unclosed-string"
              :expected "\""
              :found nil
              :offset (scan-state-string-open-pos state)
              :line (scan-state-string-open-line state)
              :column (scan-state-string-open-col state))))
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
;;; Diagnosis and repair hints
;;; ---------------------------------------------------------------------------

(defun diagnose-delimiters (text &key (base-offset 0))
  "Scan TEXT like SCAN-DELIMITERS and, when it is unbalanced, add repair hints:
:likely-fixes (parinfer line diff), :repair-failed, :repaired (parinfer's
output, so a caller that goes on to try it need not run parinfer again),
:next-top-level-line for kind \"unclosed\" (the only kind whose guidance
explains it), and for that kind also :unclosed-form-line and
:unclosed-form-head. A balanced TEXT, an unclosed block comment or an
unclosed string returns the plain scan plist."
  (let* ((scan (scan-delimiters text :base-offset base-offset))
         (kind (getf scan :kind)))
    (if (or (getf scan :ok)
            ;; Parinfer only reasons about parentheses: an open comment or
            ;; string gets its own sentence and no likely-fix diff.
            (string= kind "unclosed-block-comment")
            (string= kind "unclosed-string"))
        scan
        (multiple-value-bind (fixes failed repaired) (%likely-fixes text)
          (append scan
                  (list :likely-fixes fixes
                        :repair-failed failed
                        :repaired repaired)
                  (when (string= kind "unclosed")
                    (let ((line (getf scan :line)))
                      (list :next-top-level-line (%next-top-level-line text)
                            :unclosed-form-line line
                            :unclosed-form-head (%form-head text line)))))))))

(defun %map-code-characters (text function &key end state-fn)
  "Call FUNCTION with (CH IDX LINE COL) for every character of TEXT that is
outside strings, line comments, block comments, character literals,
single-escaped characters (a \\ outside a string makes the next character
part of a symbol, so \\) is not a delimiter) and |...| multiple-escape
symbols (whose parentheses are symbol text). LINE and COL are 1-based.
When STATE-FN is given it is called with (IDX STATE) for every position the
scan visits, STATE being the lexical state in effect just before IDX; this
lets a caller classify many positions in one pass instead of rescanning.
Scanning stops at position END (default: the end of TEXT) and returns two
values: the lexical state reached there (:code, :string, :string-escape,
:symbol, :symbol-escape, :line-comment, :block-comment or :pending) and the
block-comment nesting depth at that point. Nothing at or past END is ever
consulted, so
the state at END depends only on the text before it: a construct that would
need the character at END (\\x, #\\x, #|, |#, or a reader prefix such as
' ` , @ # that needs a following object) is reported as :pending instead."
  (let ((len (min (length text) (or end (length text))))
        (idx 0) (line 1) (col 1)
        (in-string nil) (in-symbol nil) (escape nil) (line-comment nil)
        (block-depth 0) (pending nil))
    (flet ((state ()
             (cond (pending :pending)
                   (line-comment :line-comment)
                   ((and in-string escape) :string-escape)
                   (in-string :string)
                   ((and in-symbol escape) :symbol-escape)
                   (in-symbol :symbol)
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
                   (in-symbol
                    ;; |...|: \ escapes the next character, an unescaped |
                    ;; ends the symbol, everything else is symbol text.
                    (cond (escape (setf escape nil))
                          ((char= ch #\\) (setf escape t))
                          ((char= ch #\|) (setf in-symbol nil))))
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
                    (if (char= (char text (+ idx 2)) #\Newline)
                        ;; #\<Newline>: the literal itself ends the line.
                        (progn (incf idx 2)
                               (setf line (1+ line) col 0))
                        (let ((skip 2))
                          (when (alpha-char-p (char text (+ idx 2)))
                            (loop for k from (+ idx 3) below len
                                  while (alpha-char-p (char text k))
                                  do (incf skip)))
                          (incf idx skip)
                          (incf col skip))))
                   ((char= ch #\|)
                    ;; Multiple escape (checked after #| above).
                    (setf in-symbol t))
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
:symbol (inside a |...| multiple-escape symbol), :symbol-escape (inside one
with a backslash pending), :line-comment, :block-comment, or :pending when the character just before
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
  "Return the LINE-th (1-based) line of TEXT, or \"\" when out of range.
A bounded scan for the line's newlines, so naming one line of a large text
does not split the whole of it."
  (let ((start 0))
    (loop repeat (1- line)
          do (let ((nl (position #\Newline text :start start)))
               (if nl
                   (setf start (1+ nl))
                   (return-from %line-text ""))))
    (if (< line 1)
        ""
        (subseq text start (or (position #\Newline text :start start) (length text))))))

(defun %form-head (text line)
  "Return the trimmed first 40 characters of LINE in TEXT, for naming a form."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return) (%line-text text line))))
    (if (> (length trimmed) 40)
        (subseq trimmed 0 40)
        trimmed)))

(defun %code-state-mask (text)
  "Return a simple-vector of length (1+ (length TEXT)) whose element I is the
lexical state in effect just before position I (:code, :string,
:string-escape, :symbol, :symbol-escape, :line-comment, :block-comment,
:pending), computed in one pass. Positions the scan skips as parts of a
token (the character behind a \\, the body of a #\\x literal) are marked
:token, which counts as non-code.
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
Every edit on a changed line is checked: each removed \")\" at its own
position, and an append at the end of the line. Lines whose difference does
not fit parinfer's edit model are checked at their first differing
character. Linear in the size of TEXT: the lexical states come from one
%CODE-STATE-MASK pass and the lines from one split each."
  (when fixes
    (let ((mask (%code-state-mask text))
          (orig-lines (coerce (split-string text :separator '(#\Newline)) 'vector))
          (rep-lines (coerce (split-string repaired :separator '(#\Newline)) 'vector))
          (line-starts (make-array (1+ (count #\Newline text)) :fill-pointer 0)))
      (vector-push 0 line-starts)
      (loop for i from 0 below (length text)
            when (char= (char text i) #\Newline)
              do (vector-push (1+ i) line-starts))
      (flet ((outside-code-p (line offset)
               (let ((pos (min (+ (aref line-starts (1- line)) offset) (length text))))
                 ;; :pending (a # or \ cut off at the end of TEXT) is not a
                 ;; place a ) can follow either: (a #) does not read. Nor is
                 ;; the spot right after a reader prefix or escape that ends
                 ;; a line mid-text ((list ' then a newline): the walker calls
                 ;; that :code, but (list ')) does not read.
                 (or (not (eq (svref mask pos) :code))
                     (and (plusp pos)
                          (find (char text (1- pos)) "\\#'`,@")
                          (eq (svref mask (1- pos)) :code))))))
        (loop for fix in fixes
              for line = (getf fix :line)
              ;; Compared without a trailing #\Return, exactly as
              ;; REPAIR-LINE-DIFFERENCES does, so CRLF lines still get the
              ;; precise per-position check rather than the coarse fallback.
              for orig = (%strip-trailing-cr (aref orig-lines (1- line)))
              for rep = (%strip-trailing-cr (aref rep-lines (1- line)))
              do (multiple-value-bind (added removed positions)
                     (%paren-edit-counts orig rep)
                   (declare (ignore removed))
                   (when (if added
                             (or (some (lambda (p) (outside-code-p line p)) positions)
                                 (and (plusp added)
                                      (outside-code-p line (length orig))))
                             (outside-code-p line (or (mismatch orig rep) (length orig))))
                     (return t))))))))

(defun %likely-fixes (text)
  "Run parinfer on TEXT and return (VALUES fixes repair-failed repaired).
FIXES is the line diff from REPAIR-LINE-DIFFERENCES. REPAIR-FAILED is NIL
when the fixes can be offered, :UNBALANCED when the repaired text is still
unbalanced, or :OUTSIDE-CODE when parinfer changed text that is not code
(see %ANY-FIX-OUTSIDE-CODE-P) -- a repair that may well read but must not be
offered. FIXES is NIL in either failure. REPAIRED is parinfer's output
either way, so a caller that goes on to try it does not run parinfer a
second time.
The repaired text is accepted when its parentheses balance with brackets
read as the symbol characters they are in standard syntax (a rescan with
BRACKETS NIL) and the bracket-aware rescan complains of nothing, or only of
an unclosed [ or { opener (OPENER-AMBIGUOUS-P): that opener may be a symbol
character, parinfer's \")\" fixes are what lisp-edit-form writes for such
content, and the finding is reported as a possible false positive. A ] or }
found where ) was expected is a likely typo and still fails the repair.
SCAN-DELIMITERS stops at its first complaint, which is why the bracket-free
rescan is needed to see past an opener. Balanced [...] or {...} pairs, as
used by some reader macros, are accepted."
  (let* ((repaired (apply-indent-mode text))
         (rescan (scan-delimiters repaired)))
    (if (or (getf rescan :ok)
            (and (opener-ambiguous-p rescan)
                 (getf (scan-delimiters repaired :brackets nil) :ok)))
        (let ((fixes (repair-line-differences text repaired)))
          (if (%any-fix-outside-code-p text repaired fixes)
              (values nil :outside-code repaired)
              (values fixes nil repaired)))
        (values nil :unbalanced repaired))))

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
  "Return (VALUES added removed removed-positions) when REP is ORIG with some
\")\" characters deleted and/or \")\" characters appended, which is parinfer's
edit model: ADDED is the number appended, REMOVED the number deleted, and
REMOVED-POSITIONS their indices in ORIG (ascending). Returns NIL NIL NIL when
the lines differ in any other way (whitespace, a rewrapped token), so the
caller can fall back to a plain count difference."
  (let ((i 0) (j 0) (removed '())
        (n (length orig)) (m (length rep)))
    (loop while (and (< i n) (< j m))
          do (cond ((char= (char orig i) (char rep j)) (incf i) (incf j))
                   ((char= (char orig i) #\)) (push i removed) (incf i))
                   (t (return-from %paren-edit-counts (values nil nil nil)))))
    (loop while (< i n)
          do (if (char= (char orig i) #\))
                 (progn (push i removed) (incf i))
                 (return-from %paren-edit-counts (values nil nil nil))))
    (let ((added 0))
      (loop while (< j m)
            do (if (char= (char rep j) #\))
                   (progn (incf added) (incf j))
                   (return-from %paren-edit-counts (values nil nil nil))))
      (values added (length removed) (nreverse removed)))))

(defun repair-line-differences (original repaired)
  "Compare ORIGINAL and REPAIRED (parinfer output) line by line.
Return a list of (:line n :original str :repaired str :delta d :added a
:removed r :append-only p :column c :removed-columns cs :before-comment b
:truncated t) for every line that changed, where A is the number of \")\"
parinfer appended, R the number it removed, D = A - R, P is T only when the
repaired line is the original with closers appended at its very end (the
one shape a reader can apply from \"add N )\" alone; never for a CRLF line,
whose closers go before the #\\Return), C is the 1-based column of the first
character that differs (where an insertion goes), CS the 1-based columns of
the removed \")\" when the edit is a pure removal, B is T when the rest of
the original line from column C is a ; comment, and the truncated flag says
that :original/:repaired were cut by %BOUND-LINE and so are not text to
write back. A line whose net D is 0 is still reported when it really changed
(\")(a\" -> \"(a)\"); only differences with no parenthesis edits at all
(whitespace, a trailing carriage return) are skipped. A trailing #\\Return is
stripped from both sides before comparing. Both texts must have the same
number of lines, which parinfer guarantees."
  (loop for raw-orig in (split-string original :separator '(#\Newline))
        for raw-rep in (split-string repaired :separator '(#\Newline))
        for line from 1
        for orig = (%strip-trailing-cr raw-orig)
        for rep = (%strip-trailing-cr raw-rep)
        for (added removed removed-positions)
          = (multiple-value-list (%paren-edit-counts orig rep))
        for delta = (if added
                        (- added removed)
                        (- (count #\) rep) (count #\) orig)))
        for first-diff = (mismatch orig rep)
        unless (or (string= orig rep)
                   (if added
                       (zerop (+ added removed))
                       (zerop delta)))
          collect (let ((rest (and first-diff
                                   (string-left-trim '(#\Space #\Tab)
                                                     (subseq orig (min first-diff
                                                                       (length orig)))))))
                    (list :line line
                          :original (%bound-line orig)
                          :repaired (%bound-line rep)
                          :delta delta
                          :added (or added (max delta 0))
                          :removed (or removed (max (- delta) 0))
                          :append-only (and added (plusp added) (zerop removed)
                                            (not (find #\Return raw-orig))
                                            (string= rep
                                                     (concatenate
                                                      'string orig
                                                      (make-string
                                                       added :initial-element #\))))
                                            t)
                          :column (and first-diff (1+ first-diff))
                          :removed-columns (mapcar #'1+ removed-positions)
                          ;; A CRLF line: the CR-stripped :repaired is not the
                          ;; line to write back verbatim.
                          :crlf (and (find #\Return raw-orig) t)
                          :before-comment (and rest (plusp (length rest))
                                               (char= (char rest 0) #\;)
                                               t)
                          :truncated (and (or (string/= (%bound-line orig) orig)
                                              (string/= (%bound-line rep) rep))
                                          t)))))

(defparameter *repair-lines-limit* 10
  "Maximum number of likely-fix entries rendered by FORMAT-REPAIR-LINES and
serialized by lisp-check-parens; the rest are summarized as an omitted count
so a response stays bounded however many lines parinfer changed.")

(defun format-repair-lines (fixes)
  "Render FIXES (from REPAIR-LINE-DIFFERENCES) as indented lines, each
preceded by a newline, in a form that can be applied verbatim:
  - a pure end-of-line append: \"  line 2: \\\"  (let ((y 1)\\\"  ->  add 1 \\\")\\\"\";
  - a pure removal: \"remove N \\\")\\\" at column C\" (every removed column named);
  - an insertion elsewhere (before a trailing ; comment, or on a CRLF line):
    \"insert N \\\")\\\" at column C (before the trailing ; comment)\", plus the
    resulting line when it is short enough to be shown whole;
  - anything else (a relocation): the resulting line when it is whole,
    otherwise the counts and the column of the first change.
A line cut by %BOUND-LINE is never presented as text to write back, and no
fix is ever rendered as an unchanged X -> X. A fix plist without the newer
keys (built by hand) is rendered tersely, as before those keys existed. At
most 10 entries are rendered in full; when more lines changed, a trailing
\"  ... and N more changed lines\" names the remainder, so a wholesale
reindentation cannot flood the guidance."
  (let* ((limit *repair-lines-limit*)
         (total (length fixes))
         (shown (if (> total limit) (subseq fixes 0 limit) fixes)))
    (with-output-to-string (s)
      (dolist (fix shown)
        (let ((line (getf fix :line))
              (original (getf fix :original))
              (repaired (getf fix :repaired))
              (delta (getf fix :delta))
              (added (getf fix :added 0))
              (removed (getf fix :removed 0))
              ;; A plist without the key (built by hand) is not assumed to be
              ;; an append: it falls through to showing the resulting line.
              (append-only (getf fix :append-only))
              (column (getf fix :column))
              (removed-columns (getf fix :removed-columns))
              ;; A truncated or CRLF line is described, never shown as the
              ;; line to write back (the CR is stripped from :repaired).
              (no-line (or (getf fix :truncated) (getf fix :crlf))))
          (cond
            ((and (plusp delta) append-only)
             (format s "~%  line ~D: ~S  ->  add ~D \")\"" line original delta))
            ((and (minusp delta) (zerop added))
             (format s "~%  line ~D: ~S  ->  remove ~D \")\"~@[ at column~P ~{~D~^, ~}~]"
                     line original (- delta)
                     (and removed-columns (length removed-columns)) removed-columns))
            ((and (plusp delta) (zerop removed) column)
             (format s "~%  line ~D: ~S  ->  insert ~D \")\" at column ~D~
                        ~:[~; (before the trailing ; comment)~]~:[, giving ~S~;~*~]"
                     line original delta column (getf fix :before-comment)
                     no-line repaired))
            ((not no-line)
             ;; A relocation (")(a" -> "(a)"): the net count says nothing
             ;; useful, so show the resulting line.
             (format s "~%  line ~D: ~S  ->  ~S" line original repaired))
            (t
             (format s "~%  line ~D: ~S  ->  remove ~D \")\" and add ~D \")\" ~
                        (first change at column ~D)"
                     line original removed added column)))))
      (when (> total limit)
        (format s "~%  ... and ~D more changed lines" (- total limit))))))

(defun format-delimiter-diagnosis (diagnosis &key (target "code") false-positive)
  "Render DIAGNOSIS (from DIAGNOSE-DELIMITERS) as guidance text.
Only failure plists are rendered: when DIAGNOSIS is balanced (:ok true) this
returns NIL, because there is nothing to explain.
TARGET is the subject of the first sentence: \"code\", \"content\", \"new_text\",
or a file path. The text is a finding sentence per kind, followed -- unless
FALSE-POSITIVE -- by the instruction for that kind (remove, replace, close),
then the likely-fix block when parinfer produced one (with a note when a fix
closes a form whose body continues at the same indentation, and, for an
unclosed [ or {, the reminder that the ) fixes are wrong if that bracket was
meant as a paren) or a repair-failed sentence when it did not, then the
next-top-level hint. A verdict that rests on a bracket (BRACKET-AMBIGUOUS-P:
an unclosed [ or {, or a ] or } found where ) was expected) is described with
its caveat and never called unrepairable, since the bracket may be a symbol
character.
FALSE-POSITIVE, when true, means a caller with better evidence (the editing
tools' reader accepted the text) has judged the verdict a false positive of
the standard-syntax scan: the finding is still described, but nothing that
tells the caller to change anything is attached to it. That is enforced in
one place, below, rather than per kind."
  (when (getf diagnosis :ok)
    (return-from format-delimiter-diagnosis nil))
  (let* ((kind (getf diagnosis :kind))
         (line (getf diagnosis :line))
         (column (getf diagnosis :column))
         (expected (getf diagnosis :expected))
         (found (getf diagnosis :found))
         (fixes (getf diagnosis :likely-fixes))
         (failed (getf diagnosis :repair-failed))
         (next-line (getf diagnosis :next-top-level-line))
         (ambiguous (bracket-ambiguous-p diagnosis))
         (opener-ambiguous (opener-ambiguous-p diagnosis))
         (opener (if (equal expected "]") "[" "{")))
    (with-output-to-string (s)
      ;; The finding.
      (cond
        ((string= kind "unclosed")
         (format s "Unbalanced parentheses in ~A: unclosed (form starting at line ~D: ~S).~
                    ~:[~;~%The ~S opened at line ~D, column ~D is being treated as an ~
                    opening delimiter; if it is part of a symbol name this diagnosis ~
                    is a false positive.~]"
                 target (getf diagnosis :unclosed-form-line)
                 (getf diagnosis :unclosed-form-head)
                 opener-ambiguous opener line column))
        ((string= kind "extra-close")
         (format s "Unbalanced parentheses in ~A: extra ~S at line ~D, column ~D."
                 target found line column))
        ((and (string= kind "mismatch") (equal expected ")"))
         ;; The "cannot be auto-repaired" sentence is an instruction's premise,
         ;; so it lives with the instruction below, not in the finding.
         (format s "Unbalanced parentheses in ~A: expected ~S but found ~S at line ~D, column ~D."
                 target expected found line column))
        ((string= kind "mismatch")
         ;; EXPECTED is "]" or "}": the opener was a bracket or brace, which in
         ;; Common Lisp may legitimately be part of a symbol name.  Advising a
         ;; replacement here would break valid code such as (list [a b).
         (format s "Unbalanced parentheses in ~A: expected ~S but found ~S at line ~D, column ~D.~%~
                    The ~S opened earlier is being treated as an opening delimiter; if it is ~
                    part of a symbol name this diagnosis is a false positive."
                 target expected found line column opener))
        ((string= kind "unclosed-block-comment")
         (format s "Unterminated block comment in ~A: the #| opened at line ~D, ~
                    column ~D was never closed."
                 target line column))
        ((string= kind "unclosed-string")
         (format s "Unterminated string in ~A: the \" opened at line ~D, ~
                    column ~D was never closed."
                 target line column))
        (t
         (format s "Unbalanced parentheses in ~A: ~A at line ~D, column ~D."
                 target kind line column)))
      ;; What to do about it: nothing, when the verdict is a false positive.
      (unless false-positive
        (cond
          ((string= kind "extra-close")
           (format s "~%Either remove that ~S or check for a form opened earlier ~
                      that was never closed."
                   found))
          ((and (string= kind "mismatch") (equal expected ")"))
           (format s "~%\"]\" and \"}\" are ordinary symbol characters in Common Lisp and ~
                      cannot be auto-repaired. Replace it with ~S."
                   expected))
          ((string= kind "unclosed-block-comment")
           (format s " Close it with |#."))
          ((string= kind "unclosed-string")
           (format s " Close it with \".")))
        (cond
          (fixes
           (format s "~%Likely fix, inferred from indentation:~A" (format-repair-lines fixes))
           ;; A closer placed on a line whose body continues at the same
           ;; indentation has probably cut that body off: say so, as
           ;; lisp-edit-form's summary does.
           (let* ((repaired (getf diagnosis :repaired))
                  (note (and repaired (format-relocation-note fixes repaired))))
             (when note
               (format s "~%~A" note)))
           ;; The actionable-looking part must not outrank the caveat: for an
           ;; unclosed bracket, say what the fix assumes right after it.
           (when opener-ambiguous
             (format s "~%If the ~A at line ~D, column ~D was meant as \"(\", this fix is ~
                        wrong: replace it and check again."
                     opener line column)))
          ((and (eq failed :outside-code) (not ambiguous))
           ;; The repair may well read, but it would change text that is not
           ;; code, so it is withheld rather than offered.
           (format s "~%Automatic repair would have changed text inside a string ~
                      or comment, or would follow an unfinished token at the end of ~
                      the text (a lone # \\ ' ` , or @), so no repair is offered; fix ~
                      the delimiters by hand."))
          ((and failed (not ambiguous))
           (format s "~%Automatic repair could not produce a readable form; ~
                      fix the delimiters by hand.")))
        (when (and next-line (string= kind "unclosed"))
          ;; A column-0 "(" while a form is open is a strong hint, not proof
          ;; (an unindented continuation line looks the same), so hedge. An
          ;; unclosed [ or { is not asked for as ]: it may be a symbol
          ;; character, and the ) fixes above are what would be written.
          (format s "~%Next top-level form begins at line ~D, ~
                     so the missing ~S most likely belongs before it."
                  next-line (if opener-ambiguous ")" (or expected ")"))))))))

(defun bracket-ambiguous-p (diagnosis)
  "Return T when DIAGNOSIS (from SCAN-DELIMITERS or DIAGNOSE-DELIMITERS) rests
on a [ or { as an opener or a ] or } as a closer: in standard syntax those are
symbol characters, so the verdict may be a false positive and the reader has
the final word. The tools share this one rule: an unmatched opener (EXPECTED
\"]\" or \"}\") is never grounds for refusing an edit, a FOUND \"]\" or \"}\"
gets the reader's own error alongside the diagnosis."
  (and (not (getf diagnosis :ok))
       (or (member (getf diagnosis :expected) '("]" "}") :test #'equal)
           (member (getf diagnosis :found) '("]" "}") :test #'equal))
       t))

(defun opener-ambiguous-p (diagnosis)
  "Return T when DIAGNOSIS rests on an unclosed [ or { opener (EXPECTED \"]\"
or \"}\"): the one-sided rule that an opener is never grounds for refusing an
edit, since in standard syntax it may be a symbol character. The other side,
a ] or } found where ) was expected, is a likely typo and is judged by
BRACKET-AMBIGUOUS-P together with this one."
  (and (not (getf diagnosis :ok))
       (member (getf diagnosis :expected) '("]" "}") :test #'equal)
       t))

(defun format-bracket-warning (text &key (target "the content"))
  "Return a warning string when TEXT reads but its delimiter scan stops at a
] or } where ) was expected, or NIL. In standard syntax that character is
part of a symbol, so a ) typo survives silently: the tools write TEXT as
asked -- the caller may mean it -- but flag it. Shared by lisp-edit-form and
lisp-patch-form so the two cannot drift; TARGET names the text in the
sentence. Only the found side is flagged: an unmatched [ or { opener is a
symbol character with no ) typo behind it."
  (let ((scan (scan-delimiters text)))
    (and (equal (getf scan :kind) "mismatch")
         (member (getf scan :found) '("]" "}") :test #'equal)
         (format nil "~A reads, but its delimiter scan finds ~S where ~S was expected ~
                      (line ~D, column ~D within it); in standard syntax that ~S is ~
                      part of a symbol name, so check that it is what you meant."
                 target (getf scan :found) (getf scan :expected)
                 (getf scan :line) (getf scan :column) (getf scan :found)))))

(defun format-overwrite-recovery (relative-path &key have-fix (where "below") form-line)
  "Return the recovery steps for a file that fails on a delimiter no readtable
can fix, worded once for both lisp-check-parens and file-unparseable-error:
read it with fs-read-file, apply the fix (HAVE-FIX: the one shown under
\"Likely fix\"; otherwise the change described WHERE -- \"below\" or
\"above\" -- optionally to the form starting at FORM-LINE), and write it
back with fs-write-file. RELATIVE-PATH is the project-relative path that
fs-write-file requires. Ends with the custom-reader-syntax caveat."
  (format nil "read it with fs-read-file, apply the ~:[change described ~A~;fix shown under ~
               \"Likely fix\"~*~]~@[ to the form starting at line ~D~], and write the ~
               whole file back with fs-write-file (path=~S, ~
               allow_unparseable_overwrite=true; it refuses to overwrite an existing ~
               Lisp file otherwise). If the file uses custom reader syntax that the ~
               default reader cannot parse, pass the readtable parameter to ~
               lisp-edit-form instead of overwriting."
          have-fix where form-line relative-path))

(defun format-relocation-note (fixes text)
  "Return the NOTE sentence for those FIXES (from REPAIR-LINE-DIFFERENCES over
TEXT, parinfer's output) that RELOCATING-FIX-LINES judges to have closed a
form above a body that continues at the same indentation, or NIL when there
is none. At most *REPAIR-LINES-LIMIT* lines are listed and the rest counted,
so a wholesale reindentation cannot flood the note. One wording, used by
lisp-check-parens' diagnosis and lisp-edit-form's summary alike."
  (let ((relocations (relocating-fix-lines fixes text)))
    (when relocations
      (let* ((shown (if (> (length relocations) *repair-lines-limit*)
                        (subseq relocations 0 *repair-lines-limit*)
                        relocations))
             (more (- (length relocations) (length shown)))
             (several (cdr relocations)))
        (format nil "NOTE: the fix~:[~;es~] on line~:[~;s~] ~{~D~^, ~}~[~:;, and ~:*~D ~
                     more~] close~:[s~;~] a form there, so the lines below ~:[it~;them~] ~
                     are no longer inside that form; verify the nesting (indentation ~
                     decides where a form ends)."
                several several shown more several several)))))

(defun relocating-fix-lines (fixes text)
  "Return the 1-based lines of those FIXES (from REPAIR-LINE-DIFFERENCES over
TEXT) that added closers on a line whose next code line sits at the same
indentation. That is the shape of a body meant to stay inside the form --
`(when x' followed by `(g x)' at the same column -- which parinfer, going by
indentation alone, has just closed above it. A deeper next line is a body
that stays inside; a shallower one is an explicit dedent; neither is a
relocation worth a note, and for the common missing-) repair neither fires.
Blank and comment-only lines are skipped when looking for the next code
line."
  (let* ((lines (coerce (split-string text :separator '(#\Newline)) 'vector))
         (count (length lines)))
    (flet ((indent (i)
             ;; The same measure parinfer uses to decide where a closer goes
             ;; (%count-leading-spaces: every leading space or tab is one
             ;; column), so the note reasons about the geometry that produced
             ;; the repair, not a different one.
             (loop for ch across (aref lines i)
                   while (member ch '(#\Space #\Tab))
                   count ch))
           (code-line-p (i)
             (let ((trimmed (string-trim '(#\Space #\Tab #\Return) (aref lines i))))
               (and (plusp (length trimmed)) (char/= (char trimmed 0) #\;)))))
      (loop for fix in fixes
            for line = (getf fix :line)
            when (and (plusp (getf fix :added 0))
                      (<= 1 line count)
                      (let ((next (loop for i from line below count
                                        when (code-line-p i) return i)))
                        (and next (= (indent next) (indent (1- line))))))
              collect line))))
