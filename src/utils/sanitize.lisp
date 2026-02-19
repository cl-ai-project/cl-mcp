;;;; src/utils/sanitize.lisp

(defpackage #:cl-mcp/src/utils/sanitize
  (:use #:cl)
  (:export #:sanitize-for-json))

(in-package #:cl-mcp/src/utils/sanitize)

(defun sanitize-for-json (string)
  "Remove characters that are invalid in JSON strings.
Strips ANSI escape sequences, control characters (codes 0-31 except tab,
newline, carriage return), and the DEL character (code 127).
Returns NIL when given NIL."
  (when (null string) (return-from sanitize-for-json nil))
  (let ((result
         (make-array (length string) :element-type 'character :fill-pointer 0
                     :adjustable t))
        (i 0)
        (len (length string)))
    (loop while (< i len)
          do (let ((char (char string i)))
               (cond
                ;; Strip ANSI escape sequences: ESC [ ... <alpha>
                ((and (char= char (code-char 27)) (< (1+ i) len)
                      (char= (char string (1+ i)) #\[))
                 (incf i 2)
                 (loop while (and (< i len)
                                  (let ((c (char string i)))
                                    (not (alpha-char-p c))))
                       do (incf i))
                 (when (< i len) (incf i)))
                ;; Preserve allowed whitespace
                ((member char '(#\Tab #\Newline #\Return))
                 (vector-push-extend char result) (incf i))
                ;; Strip control characters (0-31)
                ((< (char-code char) 32) (incf i))
                ;; Strip DEL (127)
                ((= (char-code char) 127) (incf i))
                ;; Pass through everything else
                (t (vector-push-extend char result) (incf i)))))
    (coerce result 'string)))
