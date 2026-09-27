;;;; src/utils/nesting.lisp
;;;;
;;;; Depth checks run before a recursive parser sees untrusted text.  Parsers
;;;; recurse once per level of nesting, and text nested deeply enough exhausts
;;;; the thread's control stack.  SBCL signals that as STORAGE-CONDITION when
;;;; it can, but an exhaustion that lands inside an allocation is fatal to the
;;;; whole process -- every session of the server with it -- so the depth is
;;;; counted first, by a loop that does not recurse.

(defpackage #:cl-mcp/src/utils/nesting
  (:use #:cl)
  (:export #:+max-json-nesting+
           #:+max-lisp-nesting+
           #:json-too-deep-p
           #:lisp-too-deep-p
           #:check-lisp-nesting))

(in-package #:cl-mcp/src/utils/nesting)

(defconstant +max-json-nesting+ 1000
  "Deepest nesting of arrays and objects a JSON message may have.  MCP
messages nest a few levels; this leaves room for any real tool argument while
staying far below the depth that exhausts a thread's control stack.")

(defconstant +max-lisp-nesting+ 500
  "Deepest nesting of lists Lisp source may have before the reader is let at
it.  Eclector uses about 1KB of control stack per level, so a 2MB thread stack
runs out between 1,700 and 2,000 levels; hand-written code stays far below 500.")

(defun json-too-deep-p (text &optional (limit +max-json-nesting+))
  "Return T when TEXT, JSON text, opens more than LIMIT arrays and objects
inside one another.  Brackets inside string literals do not count; the text
need not be valid JSON."
  (declare (type string text) (type fixnum limit))
  (let ((depth 0)
        (in-string nil)
        (escaped nil))
    (declare (type fixnum depth))
    (loop for c across text
          do (cond
               (in-string
                (cond (escaped (setf escaped nil))
                      ((char= c #\\) (setf escaped t))
                      ((char= c #\") (setf in-string nil))))
               ((char= c #\") (setf in-string t))
               ((or (char= c #\[) (char= c #\{))
                (when (> (incf depth) limit)
                  (return-from json-too-deep-p t)))
               ((or (char= c #\]) (char= c #\}))
                (when (plusp depth) (decf depth)))))
    nil))

(defun lisp-too-deep-p (text &optional (limit +max-lisp-nesting+))
  "Return T when TEXT, Lisp source in standard syntax, opens more than LIMIT
lists inside one another.  Parentheses in strings, |symbols|, comments and
character names such as #\\( do not count, nor does one after a single escape.
The text need not read; custom reader syntax is counted as standard syntax, so
the answer is an estimate, which is all a limit this far from real code needs."
  (declare (type string text) (type fixnum limit))
  (let ((depth 0)
        (i 0)
        (n (length text)))
    (declare (type fixnum depth i n))
    (flet ((skip-delimited (close)
             ;; I is on the opening delimiter; leave it just past CLOSE.
             (incf i)
             (loop while (< i n)
                   do (let ((c (char text i)))
                        (cond ((char= c #\\) (incf i 2))
                              ((char= c close) (incf i) (return))
                              (t (incf i)))))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((char= c #\\) (incf i 2))
                   ((char= c #\") (skip-delimited #\"))
                   ((char= c #\|) (skip-delimited #\|))
                   ((char= c #\;)
                    (setf i (or (position #\Newline text :start i) n)))
                   ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                    (let ((level 1))
                      (declare (type fixnum level))
                      (incf i 2)
                      (loop while (and (< i n) (plusp level))
                            do (cond ((and (char= (char text i) #\|) (< (1+ i) n)
                                           (char= (char text (1+ i)) #\#))
                                      (decf level) (incf i 2))
                                     ((and (char= (char text i) #\#) (< (1+ i) n)
                                           (char= (char text (1+ i)) #\|))
                                      (incf level) (incf i 2))
                                     (t (incf i))))))
                   ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
                    ;; #\x: the character after the backslash is data.
                    (incf i 3))
                   ((char= c #\()
                    (when (> (incf depth) limit)
                      (return-from lisp-too-deep-p t))
                    (incf i))
                   ((char= c #\))
                    (when (plusp depth) (decf depth))
                    (incf i))
                   (t (incf i))))))
    nil))

(defun check-lisp-nesting (text)
  "Signal an ERROR when TEXT nests lists deeper than +MAX-LISP-NESTING+, before
a reader recurses into it; otherwise return TEXT."
  (when (lisp-too-deep-p text)
    (error "The source nests lists more than ~D levels deep, which is deeper ~
than cl-mcp will read: a reader recursing that far can exhaust the server's ~
stack.  Split or flatten the form."
           +max-lisp-nesting+))
  text)
