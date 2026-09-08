;;;; src/utils/bounded-stream.lisp
;;;;
;;;; A character output stream that stops retaining past a limit.
;;;;
;;;; Capturing a test suite's output into a STRING-OUTPUT-STREAM and truncating
;;;; the result afterwards bounds what is *reported* but not what is *held*: a
;;;; suite emitting 40 million characters costs 367 MB of heap to report 50 KB
;;;; of it, and on a worker with a smaller dynamic space the run dies with
;;;; HEAP-EXHAUSTED-ERROR while materializing the string.  A suite can reach
;;;; that in a fraction of a second, so the run deadline does not help.

(defpackage #:cl-mcp/src/utils/bounded-stream
  (:use #:cl)
  (:export #:bounded-output-stream
           #:make-bounded-output-stream
           #:bounded-output-string
           #:bounded-output-dropped))

(in-package #:cl-mcp/src/utils/bounded-stream)

(defclass bounded-output-stream (sb-gray:fundamental-character-output-stream)
  ((sink :initform (make-string-output-stream) :reader %sink)
   (limit :initarg :limit :initform 0 :reader %limit)
   (kept :initform 0 :accessor %kept)
   (dropped :initform 0 :accessor %dropped)
   (column :initform 0 :accessor %column))
  (:documentation "A character sink that keeps at most LIMIT characters.

Writes past the limit are counted and discarded rather than stored, so the
memory a capture costs is bounded by the limit instead of by how much the
writer produced.  Everything written is still accepted -- the writer never
sees an error or a short write -- which matters because the writer here is
arbitrary test code.

The column is tracked across every write, dropped ones included, because
FRESH-LINE, ~T and the pretty printer all ask for it: a stream that answers
NIL makes ~& emit a newline unconditionally, mis-tabulates ~T, and breaks
pretty-printed lines at the wrong places, silently rewriting the output it
was only supposed to be bounding.  Counting past the limit as well keeps
those decisions matching what the writer would have seen.

Not synchronized, and neither was the STRING-OUTPUT-STREAM it replaces:
concurrent writers can overshoot the limit by roughly one write each, where
the string stream instead lost characters outright.  Suites that spawn
threads do not reach these bindings anyway -- in SBCL a new thread starts
from a special's global value."))

(defun make-bounded-output-stream (limit)
  "Return a character output stream retaining at most LIMIT characters."
  (make-instance 'bounded-output-stream :limit (max 0 limit)))

(defmethod sb-gray:stream-write-char ((stream bounded-output-stream) character)
  (if (< (%kept stream) (%limit stream))
      (progn (write-char character (%sink stream))
             (incf (%kept stream)))
      (incf (%dropped stream)))
  (if (char= character #\Newline)
      (setf (%column stream) 0)
      (incf (%column stream)))
  character)

(defmethod sb-gray:stream-write-string ((stream bounded-output-stream) string
                                        &optional (start 0) end)
  (let* ((end (or end (length string)))
         (length (max 0 (- end start)))
         (room (max 0 (- (%limit stream) (%kept stream))))
         (taken (min length room)))
    (when (plusp taken)
      (write-string string (%sink stream) :start start :end (+ start taken))
      (incf (%kept stream) taken))
    (incf (%dropped stream) (- length taken))
    (when (plusp length)
      (let ((last-newline (position #\Newline string :from-end t
                                                     :start start :end end)))
        (if last-newline
            (setf (%column stream) (- end last-newline 1))
            (incf (%column stream) length)))))
  string)

(defmethod sb-gray:stream-line-column ((stream bounded-output-stream))
  ;; Answering NIL here is legal and nothing errors on it, but it is not
  ;; faithful: SBCL's STREAM-START-LINE-P is (eql (stream-line-column s) 0),
  ;; so ~& would emit a newline even at the start of a line, ~T would
  ;; mis-tabulate, and the pretty printer would break lines as though every
  ;; write began at column zero.  Capture would then quietly rewrite the
  ;; output it exists to record.
  (%column stream))

(defun bounded-output-dropped (stream)
  "Number of characters STREAM discarded for exceeding its limit."
  (%dropped stream))

(defun bounded-output-string (stream)
  "Return what STREAM retained, noting the total when anything was dropped.
Drains the stream, as GET-OUTPUT-STREAM-STRING does, so calling it twice
yields the retained text once."
  (let ((kept (get-output-stream-string (%sink stream)))
        (dropped (%dropped stream)))
    (setf (%kept stream) 0
          (%dropped stream) 0)
    (cond
      ((zerop dropped) kept)
      ;; No separating newline when nothing was kept: the note would otherwise
      ;; start with a blank line, which reads as retained output.
      ((zerop (length kept))
       (format nil "... (truncated, ~D total chars)" dropped))
      (t
       (format nil "~A~%... (truncated, ~D total chars)"
               kept (+ (length kept) dropped))))))
