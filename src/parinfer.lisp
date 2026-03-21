;;;; src/parinfer.lisp

(defpackage #:cl-mcp/src/parinfer
  (:use #:cl)
  (:import-from #:uiop #:split-string)
  (:export #:apply-indent-mode))

(in-package #:cl-mcp/src/parinfer)

(defstruct (state (:constructor %make-state))
  (stack nil :type list)
  (in-string nil :type boolean)
  (escape nil :type boolean)
  (sharp-seen nil :type boolean)
  (char-literal nil :type boolean))


(defun %count-leading-spaces (line)
  (loop for ch across line
        while (member ch '(#\Space #\Tab))
        count 1))

(defun %line-empty-or-comment-p (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (or (string= trimmed "")
        (char= (char trimmed 0) #\;))))

(defun %dedent-closes (state indent)
  "Return number of close parens needed when indentation decreases."
  (let ((pending 0))
    (loop while (and (state-stack state)
                     (> (car (state-stack state)) indent))
          do (pop (state-stack state))
             (incf pending))
    pending))

(defun %append-closes-to-previous (processed-lines count)
  (when (and (plusp count) processed-lines)
    (setf (first processed-lines)
          (format nil "~A~A"
                  (first processed-lines)
                  (make-string count :initial-element #\)))))
  processed-lines)

(defun %process-line-characters (line state)
  "Process characters in LINE tracking parens, strings, comments, and char literals.
Handles #\\( and #\\) character literals so they are not counted as real parens."
  (let ((output (make-string-output-stream)))
    (loop for ch across line
          for col from 0
          do (cond
               ;; Skip the character after #\ (it's a char literal, not a paren)
               ((state-char-literal state)
                (write-char ch output)
                (setf (state-char-literal state) nil))
               ;; Previous char was # outside string: check for \
               ((and (state-sharp-seen state) (char= ch #\\))
                (write-char ch output)
                (setf (state-sharp-seen state) nil)
                (setf (state-char-literal state) t))
               ;; Previous char was # but next is not \: reset flag and
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
          (state-char-literal state) nil)
    (get-output-stream-string output)))

(defun %append-remaining-closes (state processed-lines)
  (let ((remaining (length (state-stack state))))
    (when (and (plusp remaining) processed-lines)
      (setf (first processed-lines)
            (format nil "~A~A"
                    (first processed-lines)
                    (make-string remaining :initial-element #\))))))
  processed-lines)

(defun apply-indent-mode (text)
  "Apply a minimal Parinfer-like indent mode to TEXT.
Closes open forms when indentation decreases, drops excessive closing parens,
and ignores parentheses inside strings or comments."
  (let ((ends-with-newline (and (plusp (length text))
                                (char= (char text (1- (length text))) #\Newline)))
        (lines (uiop:split-string text :separator '(#\Newline)))
        (state (%make-state))
        (processed-lines '()))
    (dolist (line lines)
      (let ((indent (%count-leading-spaces line))
            (is-code-line (not (%line-empty-or-comment-p line))))
        (when is-code-line
          (let ((pending (%dedent-closes state indent)))
            (%append-closes-to-previous processed-lines pending)))
        (push (%process-line-characters line state) processed-lines)))

    ;; close any remaining open parens at EOF
    (%append-remaining-closes state processed-lines)

    ;; Format output, preserving whether input ended with newline
    (let ((result (format nil "~{~A~^~%~}" (nreverse processed-lines))))
      (if ends-with-newline
          (concatenate 'string result (string #\Newline))
          result))))
