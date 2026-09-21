;;;; specs/fixtures.lisp
;;;;
;;;; Character pools and draw primitives shared by the cl-mcp/specs bundle.
;;;;
;;;; Every draw uses CL:RANDOM and nothing else.  cl-spec binds *RANDOM-STATE*
;;;; from a run's seed before it calls a generator, so a seed replays the same
;;;; inputs; nothing here reads the clock, the filesystem or a random state of
;;;; its own.  Every draw returns a freshly consed string, so a trial never
;;;; sees an object an earlier trial was handed.
;;;;
;;;; No pool holds a surrogate code point (#xD800-#xDFFF).  SBCL refuses to
;;;; encode one as UTF-8, so a counterexample carrying one cannot be printed to
;;;; a client, and printing it from an MCP worker takes the worker down.

(defpackage #:cl-mcp/specs/fixtures
  (:use #:cl)
  (:export #:code-character
           #:*printable-ascii*
           #:*non-ascii-text*
           #:*allowed-whitespace*
           #:pick
           #:chance
           #:draw-string
           #:draw-allowed-text
           #:shrink-text-candidates))

(in-package #:cl-mcp/specs/fixtures)

(defun code-character (code)
  "Return the character whose code is CODE, or signal when this Lisp has none.
The bundle names characters by code point; a Lisp without one of them should
refuse to load the bundle rather than generate a different domain."
  (or (code-char code)
      (error "This Lisp has no character with code #x~X." code)))

(defparameter *printable-ascii*
  (coerce (loop for code from 32 to 126 collect (code-character code)) 'string)
  "Space through tilde: every printable ASCII character.")

(defparameter *non-ascii-text*
  (concatenate 'string "日本語テスト"
               (map 'string #'code-character '(#xE9 #x20AC #xFFFD)))
  "Characters above ASCII and inside the Basic Multilingual Plane: Japanese,
e-acute, the euro sign and U+FFFD itself.")

(defparameter *allowed-whitespace*
  (coerce (list #\Tab #\Newline #\Return) 'string)
  "The three control characters every sanitizer here keeps.")

(defun pick (sequence)
  "Return a random element of the non-empty SEQUENCE."
  (elt sequence (random (length sequence))))

(defun chance (percent)
  "Return true with probability PERCENT/100."
  (< (random 100) percent))

(defun draw-string (pool length)
  "Return a fresh string of LENGTH characters drawn from the string POOL."
  (let ((string (make-string length)))
    (dotimes (index length string)
      (setf (char string index) (pick pool)))))

(defun draw-allowed-text (&key (max-length 16))
  "Return a fresh string of 0 to MAX-LENGTH characters that no sanitizer here
rewrites: mostly printable ASCII, some non-ASCII BMP text, and tab, newline or
carriage return."
  (let* ((length (random (1+ max-length)))
         (string (make-string length)))
    (dotimes (index length string)
      (setf (char string index)
            (let ((roll (random 100)))
              (cond ((< roll 75) (pick *printable-ascii*))
                    ((< roll 90) (pick *non-ascii-text*))
                    (t (pick *allowed-whitespace*))))))))

(defun shrink-text-candidates (text)
  "Return strictly shorter strings to try in place of TEXT when shrinking a
failure: the empty string, each half, and TEXT without its first or last
character.  Every candidate is a substring, so a pool-restricted domain stays
closed, and none is as long as TEXT, so shrinking always terminates."
  (let* ((length (length text))
         (half (floor length 2)))
    (remove-duplicates
     (remove-if-not (lambda (candidate) (< (length candidate) length))
                    (when (plusp length)
                      (list (make-string 0)
                            (subseq text 0 half)
                            (subseq text half)
                            (subseq text 1)
                            (subseq text 0 (1- length)))))
     :test #'string=
     :from-end t)))
