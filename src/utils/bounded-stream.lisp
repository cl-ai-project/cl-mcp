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
   (limit :initarg :limit :reader %limit)
   (kept :initform 0 :accessor %kept)
   (dropped :initform 0 :accessor %dropped))
  (:documentation "A character sink that keeps at most LIMIT characters.

Writes past the limit are counted and discarded rather than stored, so the
memory a capture costs is bounded by the limit instead of by how much the
writer produced.  Everything written is still accepted -- the writer never
sees an error or a short write -- which matters because the writer here is
arbitrary test code."))

(defun make-bounded-output-stream (limit)
  "Return a character output stream retaining at most LIMIT characters."
  (make-instance 'bounded-output-stream :limit (max 0 limit)))

(defmethod sb-gray:stream-write-char ((stream bounded-output-stream) character)
  (if (< (%kept stream) (%limit stream))
      (progn (write-char character (%sink stream))
             (incf (%kept stream)))
      (incf (%dropped stream)))
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
    (incf (%dropped stream) (- length taken)))
  string)

(defmethod sb-gray:stream-line-column ((stream bounded-output-stream))
  ;; Not tracked: the pretty printer only uses this as a hint, and tracking it
  ;; would mean re-scanning every write for newlines.
  nil)

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
    (if (plusp dropped)
        (format nil "~A~%... (truncated, ~D total chars)"
                kept (+ (length kept) dropped))
        kept)))
