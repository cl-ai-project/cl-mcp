;;;; src/parinfer.lisp

(defpackage #:cl-mcp/src/parinfer
  (:use #:cl)
  (:import-from #:uiop #:split-string)
  (:export #:apply-indent-mode))

(in-package #:cl-mcp/src/parinfer)

(defstruct (state (:constructor %make-state))
  (stack nil :type list)
  (in-string nil :type boolean)
  ;; Inside a |...| multiple-escape symbol: its parentheses are symbol text.
  (in-symbol nil :type boolean)
  (escape nil :type boolean)
  (sharp-seen nil :type boolean)
  (char-literal nil :type boolean)
  ;; Block comments: nesting depth, and whether the previous character was a
  ;; | (so |# can be recognised) -- bar-seen is per-line like sharp-seen.
  (block-depth 0 :type fixnum)
  (bar-seen nil :type boolean))


(defun %count-leading-spaces (line)
  (loop for ch across line
        while (member ch '(#\Space #\Tab))
        count 1))

(defun %line-empty-or-comment-p (line)
  "Return T when LINE is blank or holds only a ; comment. A trailing #\\Return
(CRLF input) is ignored, so a blank CRLF line is not mistaken for a code line
at indentation 0 that would close every open form."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
    (or (string= trimmed "")
        (char= (char trimmed 0) #\;))))

(defun %block-comment-opener-line-p (line)
  "Return T when LINE's first non-blank characters open a #| block comment.
Such a line is not code: its column says nothing about the open forms, so a
column-0 #| inside a function must not dedent the function shut."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (and (>= (length trimmed) 2)
         (char= (char trimmed 0) #\#)
         (char= (char trimmed 1) #\|))))

(defun %dedent-closes (state indent)
  "Return number of close parens needed when indentation decreases."
  (let ((pending 0))
    (loop while (and (state-stack state)
                     (> (car (state-stack state)) indent))
          do (pop (state-stack state))
             (incf pending))
    pending))

(defun %append-closes-to-previous (processed-lines code-flags count)
  "Append COUNT closing parens to the most recent code line in PROCESSED-LINES
(newest first). CODE-FLAGS parallels it and says which lines were code when
processed: not blank, not a comment, not starting inside a string or a block
comment. Those other lines are skipped so the closers land on the line that
ends the form, not on an empty line or inside a comment or string; when no
line was code the newest line is used. A trailing #\\Return stays after the
inserted closers, so CRLF text remains CRLF."
  (when (and (plusp count) processed-lines)
    (let* ((cell (nthcdr (or (position t code-flags) 0) processed-lines))
           (line (car cell))
           (cr-p (and (plusp (length line))
                      (char= (char line (1- (length line))) #\Return)))
           (body (if cr-p (subseq line 0 (1- (length line))) line)))
      (setf (car cell)
            (concatenate 'string body
                         (make-string count :initial-element #\))
                         (if cr-p (string #\Return) "")))))
  processed-lines)

(defun %process-line-characters (line state)
  "Process characters in LINE tracking parens, strings, comments, |...| symbols
and char literals. Handles #\\( and #\\) character literals so they are not
counted as real parens, treats the inside of a |...| multiple-escape symbol
as symbol text, and skips #| ... |# block comments (nested) so that a paren
inside one is neither counted nor \"repaired\"."
  (let ((output (make-string-output-stream)))
    (loop for ch across line
          for col from 0
          do (cond
               ;; Inside a #| ... |# block comment: only track its end (and
               ;; nested openers); nothing here is code.
               ((plusp (state-block-depth state))
                (write-char ch output)
                (cond ((and (state-sharp-seen state) (char= ch #\|))
                       (incf (state-block-depth state))
                       (setf (state-sharp-seen state) nil
                             (state-bar-seen state) nil))
                      ((and (state-bar-seen state) (char= ch #\#))
                       (decf (state-block-depth state))
                       (setf (state-sharp-seen state) nil
                             (state-bar-seen state) nil))
                      (t
                       (setf (state-sharp-seen state) (char= ch #\#)
                             (state-bar-seen state) (char= ch #\|)))))
               ;; Skip the character after #\ (it's a char literal, not a paren)
               ((state-char-literal state)
                (write-char ch output)
                (setf (state-char-literal state) nil))
               ;; Previous char was # outside string: check for \
               ((and (state-sharp-seen state) (char= ch #\\))
                (write-char ch output)
                (setf (state-sharp-seen state) nil)
                (setf (state-char-literal state) t))
               ;; Previous char was # : #| opens a block comment.
               ((and (state-sharp-seen state) (char= ch #\|))
                (write-char ch output)
                (setf (state-sharp-seen state) nil)
                (incf (state-block-depth state)))
               ;; Previous char was # but next is not \ or |: reset flag and
               ;; fall through to normal processing (e.g. #( vector literals
               ;; must still push onto the paren stack).
               ((state-sharp-seen state)
                (setf (state-sharp-seen state) nil)
                ;; Re-process this character through normal branches
                (cond
                  ((char= ch #\")
                   (write-char ch output)
                   (setf (state-in-string state) (not (state-in-string state))))
                  ((char= ch #\;)
                   (loop for i from col below (length line)
                         do (write-char (char line i) output))
                   (return))
                  ((char= ch #\()
                   (write-char ch output)
                   (push (1+ col) (state-stack state)))
                  ((char= ch #\))
                   (cond
                     ((state-stack state)
                      (pop (state-stack state))
                      (write-char ch output))
                     (t nil)))
                  (t (write-char ch output))))
               ;; Inside a |...| symbol: \ escapes, | ends, all else is text.
               ((state-in-symbol state)
                (write-char ch output)
                (cond ((state-escape state) (setf (state-escape state) nil))
                      ((char= ch #\\) (setf (state-escape state) t))
                      ((char= ch #\|) (setf (state-in-symbol state) nil))))
               ;; Escape in string
               ((state-escape state)
                (write-char ch output)
                (setf (state-escape state) nil))
               ;; Backslash in string
               ((and (state-in-string state) (char= ch #\\))
                (write-char ch output)
                (setf (state-escape state) t))
               ;; String delimiter
               ((char= ch #\")
                (write-char ch output)
                (setf (state-in-string state) (not (state-in-string state))))
               ;; Single escape outside a string: the next character is part
               ;; of a symbol (so \( and \) are not parens); reuse the string
               ;; escape flag, which the branch above consumes.
               ((and (not (state-in-string state)) (char= ch #\\))
                (write-char ch output)
                (setf (state-escape state) t))
               ;; Multiple-escape symbol start (outside string)
               ((and (not (state-in-string state)) (char= ch #\|))
                (write-char ch output)
                (setf (state-in-symbol state) t))
               ;; # outside string: set flag for next char
               ((and (not (state-in-string state)) (char= ch #\#))
                (write-char ch output)
                (setf (state-sharp-seen state) t))
               ;; Comment
               ((and (not (state-in-string state)) (char= ch #\;))
                (loop for i from col below (length line)
                      do (write-char (char line i) output))
                (return))
               ;; Open paren (outside string)
               ((and (not (state-in-string state)) (char= ch #\())
                (write-char ch output)
                (push (1+ col) (state-stack state)))
               ;; Close paren (outside string)
               ((and (not (state-in-string state)) (char= ch #\)))
                (cond
                  ((state-stack state)
                   (pop (state-stack state))
                   (write-char ch output))
                  (t nil)))
               (t (write-char ch output))))
    ;; Reset per-line transient flags
    (setf (state-escape state) nil
          (state-sharp-seen state) nil
          (state-bar-seen state) nil
          (state-char-literal state) nil)
    (get-output-stream-string output)))

(defun %append-remaining-closes (state processed-lines code-flags)
  "Close every form still open at the end of the input, on the last code line."
  (%append-closes-to-previous processed-lines code-flags
                              (length (state-stack state))))

(defun apply-indent-mode (text)
  "Apply a minimal Parinfer-like indent mode to TEXT.
Closes open forms when indentation decreases, drops excessive closing parens,
and ignores parentheses inside strings or comments. Only code lines take
part: a line that is blank, holds only a ; comment, opens a #| block comment,
or starts inside a string or block comment neither triggers a dedent (its
indentation says nothing about the forms open around it) nor receives the
closers."
  (let ((ends-with-newline (and (plusp (length text))
                                (char= (char text (1- (length text))) #\Newline)))
        ;; split-string yields a trailing "" for text ending in a newline;
        ;; drop it so closers land on the last real line and the newline is
        ;; restored below exactly once.
        (lines (let ((parts (uiop:split-string text :separator '(#\Newline))))
                 (if (and (cdr parts) (string= (car (last parts)) ""))
                     (butlast parts)
                     parts)))
        (state (%make-state))
        (processed-lines '())
        (code-flags '()))
    (dolist (line lines)
      (let ((indent (%count-leading-spaces line))
            (is-code-line (and (not (state-in-string state))
                               (zerop (state-block-depth state))
                               (not (%line-empty-or-comment-p line))
                               (not (%block-comment-opener-line-p line)))))
        (when is-code-line
          (let ((pending (%dedent-closes state indent)))
            (%append-closes-to-previous processed-lines code-flags pending)))
        (push (%process-line-characters line state) processed-lines)
        (push is-code-line code-flags)))

    ;; close any remaining open parens at EOF
    (%append-remaining-closes state processed-lines code-flags)

    ;; Format output, preserving whether input ended with newline
    (let ((result (format nil "~{~A~^~%~}" (nreverse processed-lines))))
      (if ends-with-newline
          (concatenate 'string result (string #\Newline))
          result))))
