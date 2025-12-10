;;;; src/validate.lisp

(defpackage #:cl-mcp/src/validate
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
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

(defun %scan-parens (text &key (base-offset 0))
  "Return a plist describing balance of delimiters in TEXT.
Keys: :ok (boolean), :kind (string|nil), :expected, :found, :offset, :line, :column."
  (let ((line 1)
        (col 1)
        (stack '())
        (in-string nil)
        (escape nil)
        (line-comment nil)
        (block-depth 0))
    (flet ((push-open (ch idx)
             (push (list ch line col (+ base-offset idx)) stack))
           (pop-open (ch idx)
             (if (null stack)
                 (return-from %scan-parens (list :ok nil :kind "extra-close"
                                                 :expected nil :found (string ch) :offset (+ base-offset idx)
                                                 :line line :column col))
                 (destructuring-bind (top-ch top-line top-col top-off) (pop stack)
                   (declare (ignore top-line top-col top-off))
                   (let ((expected (%closing top-ch)))
                     (unless (char= expected ch)
                       (return-from %scan-parens (list :ok nil :kind "mismatch"
                                                       :expected (string expected) :found (string ch) :offset (+ base-offset idx)
                                                       :line line :column col))))))))
      (loop for idx from 0 below (length text)
            for ch = (char text idx)
            for next = (and (< (1+ idx) (length text))
                            (char text (1+ idx))) do
               (cond
                 (line-comment
                  (when (char= ch #\Newline) (setf line-comment nil)))

                 (in-string
                  (cond
                    (escape (setf escape nil))
                    ((char= ch #\\) (setf escape t))
                    ((char= ch #\") (setf in-string nil))))

                 ((plusp block-depth)
                  (when (and (char= ch #\|) next (char= next #\#))
                    (decf block-depth)
                    (incf idx)
                    (incf col)))

                 (t
                  (cond
                    ((char= ch #\;) (setf line-comment t))
                    ((char= ch #\") (setf in-string t))
                    ((and (char= ch #\#) next (char= next #\|))
                     (incf block-depth)
                     (incf idx)
                     (incf col))
                    ((or (char= ch #\() (char= ch #\[) (char= ch #\{))
                     (push-open ch idx))
                    ((or (char= ch #\)) (char= ch #\]) (char= ch #\}))
                     (pop-open ch idx)))))

               (if (char= ch #\Newline)
                   (progn (incf line) (setf col 1))
                   (incf col))))
    (when stack
      (destructuring-bind (ch l c off) (pop stack)
        (return-from %scan-parens
          (list :ok nil :kind "unclosed"
                :expected (string (%closing ch))
                :found nil
                :offset off :line l :column c))))
    (list :ok t)))

(defun lisp-check-parens (&key path code offset limit)
  "Check balanced parentheses/brackets in CODE or PATH slice.
Returns a hash table with keys \"ok\" and, when not ok, \"kind\", \"expected\", \"found\", and \"position\"."
  (when (and path code)
    (error "Provide either PATH or CODE, not both"))
  (when (and (null path) (null code))
    (error "Either PATH or CODE is required"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (let* ((text (or code (fs-read-file path :offset offset :limit limit)))
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
    (destructuring-bind (&key ok kind expected found offset line column) (%scan-parens text :base-offset base-off)
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "ok" h) (and ok t))
        (unless ok
          (setf (gethash "kind" h) kind
                (gethash "expected" h) expected
                (gethash "found" h) found)
          (let ((pos (make-hash-table :test #'equal)))
            (setf (gethash "offset" pos) offset
                  (gethash "line" pos) line
                  (gethash "column" pos) column)
            (setf (gethash "position" h) pos)))
        h))))
