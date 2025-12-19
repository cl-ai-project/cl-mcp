;;;; src/parinfer.lisp

(defpackage #:cl-mcp/src/parinfer
  (:use #:cl)
  (:import-from #:uiop #:split-string)
  (:export #:apply-indent-mode))

(in-package #:cl-mcp/src/parinfer)

(defstruct (state (:constructor %make-state))
  (stack nil :type list)        ; indentation levels of open forms
  (in-string nil :type boolean) ; currently inside a string?
  (escape nil :type boolean))   ; previous char was backslash?

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
  (let ((output (make-string-output-stream)))
    (loop for ch across line
          for col from 0
          do (cond
               ((state-escape state)
                (write-char ch output)
                (setf (state-escape state) nil))
               ((char= ch #\\)
                (write-char ch output)
                (setf (state-escape state) t))
               ((char= ch #\")
                (write-char ch output)
                (setf (state-in-string state)
                      (not (state-in-string state))))
               ((and (not (state-in-string state)) (char= ch #\;))
                ;; Comment: output rest of line and stop
                (loop for i from col below (length line)
                      do (write-char (char line i) output))
                (return))
               ((and (not (state-in-string state)) (char= ch #\())
                (write-char ch output)
                ;; Push the expected minimum indent for content inside this paren
                ;; which is col + 1 (next column after the opening paren)
                (push (1+ col) (state-stack state)))
               ((and (not (state-in-string state)) (char= ch #\)))
                ;; Only output closing paren if stack is not empty
                (cond
                  ((state-stack state)
                   (pop (state-stack state))
                   (write-char ch output))
                  (t nil)))
               (t
                (write-char ch output))))
    (setf (state-escape state) nil)
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
