(defpackage #:cl-mcp/src/utils/random
  (:use #:cl)
  (:export #:generate-random-hex-string))

(in-package #:cl-mcp/src/utils/random)

(defun generate-random-hex-string (n-bytes)
  "Generate N-BYTES of random data and return as a lowercase hex string."
  (let ((buf (make-array n-bytes :element-type '(unsigned-byte 8))))
    #-win32
    (with-open-file (s #P"/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence buf s))
    #+win32
    (let ((state (make-random-state t)))
      (dotimes (i n-bytes)
        (setf (aref buf i) (random 256 state))))
    (format nil "~{~(~2,'0x~)~}" (coerce buf 'list))))
