;;;; Keep #. source bytes and comments.
(in-package #:cl-user)
(defun source-reader-fixture ()
  (list #.(char-code #\A)
        #.(progn (error "must not execute") 42)
        #()
        #(1 2)
        :before))
;;;; Unrelated trailing comment.
