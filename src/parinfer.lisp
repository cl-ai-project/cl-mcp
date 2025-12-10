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

(defun apply-indent-mode (text)
  "Apply a minimal Parinfer-like indent mode to TEXT.
Closes open forms when indentation decreases, preserves user-supplied
parens, and ignores parentheses inside strings or comments."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (state (%make-state))
         (processed-lines '())
         (pending-closes 0))

    (dolist (line lines)
      (let* ((indent (%count-leading-spaces line))
             (is-code-line (not (%line-empty-or-comment-p line))))

        ;; close forms when dedenting
        (when is-code-line
          (loop while (and (state-stack state)
                           (>= (car (state-stack state)) indent))
                do (pop (state-stack state))
                   (incf pending-closes)))

        ;; append any needed close parens to previous processed line
        (when (plusp pending-closes)
          (when processed-lines
            (setf (first processed-lines)
                  (format nil "~A~A"
                          (first processed-lines)
                          (make-string pending-closes :initial-element #\)))))
          (setf pending-closes 0))

        ;; scan line, tracking strings/escapes and user parens
        (loop for ch across line
              for i from 0
              do (cond
                   ((state-escape state)
                    (setf (state-escape state) nil))
                   ((char= ch #\\)
                    (setf (state-escape state) t))
                   ((char= ch #\")
                    (setf (state-in-string state)
                          (not (state-in-string state))))
                   ((and (not (state-in-string state)) (char= ch #\;))
                    (return)) ; comment starts; ignore rest
                   ((and (not (state-in-string state)) (char= ch #\())
                    (push i (state-stack state)))
                   ((and (not (state-in-string state)) (char= ch #\)))
                    (when (state-stack state)
                      (pop (state-stack state))))))

        (setf (state-escape state) nil)
        (push line processed-lines)))

    ;; close any remaining open parens at EOF
    (let ((remaining (length (state-stack state))))
      (when (plusp remaining)
        (when processed-lines
          (setf (first processed-lines)
                (format nil "~A~A"
                        (first processed-lines)
                        (make-string remaining :initial-element #\)))))))

    (format nil "~{~A~%~}" (nreverse processed-lines))))
