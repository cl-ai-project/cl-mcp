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
  "Deepest reader recursion -- lists, and prefixes such as ' and #+ -- Lisp
source may need before the reader is let at it.  Eclector uses about 1KB of control stack per level, so a 2MB thread stack
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
  "Return T when reading TEXT, Lisp source in standard syntax, would recurse
more than LIMIT levels deep.

The reader recurses once per open list and once per prefix that reads the
object after it -- ' ` , ,@ #' #. #+ #- and #n= -- so a chain of prefixes
counts like nested lists: ''''x is four levels, and a prefix before a list
stays counted until that list closes.  Parentheses and prefixes in strings,
|symbols|, comments and character names such as #\\( do not count, nor does
one after a single escape.  The text need not read; custom reader syntax is
counted as standard syntax, so the answer is an estimate, which is all a limit
this far from real code needs."
  (declare (type string text) (type fixnum limit))
  (let ((depth 0)                       ; levels held by open lists
        (run 0)                         ; prefixes waiting for their object
        (held '())                      ; levels each open list holds
        (i 0)
        (n (length text)))
    (declare (type fixnum depth run i n))
    (labels ((peek (k)
               (and (< (+ i k) n) (char text (+ i k))))
             (prefix ()
               (incf run)
               (when (> (+ depth run) limit)
                 (return-from lisp-too-deep-p t)))
             (object-done ()
               ;; An atom completes the object every waiting prefix reads.
               (setf run 0))
             (skip-delimited (close)
               ;; I is on the opening delimiter; leave it just past CLOSE.
               (incf i)
               (loop while (< i n)
                     do (let ((c (char text i)))
                          (cond ((char= c #\\) (incf i 2))
                                ((char= c close) (incf i) (return))
                                (t (incf i))))))
             (skip-feature-expression ()
               ;; After #+ or #-: the feature expression is not the object the
               ;; prefix waits for, so it neither counts nor completes it.
               (loop while (and (< i n) (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf i))
               (if (eql (peek 0) #\()
                   (let ((level 0))
                     (declare (type fixnum level))
                     (loop while (< i n)
                           do (let ((c (char text i)))
                                (incf i)
                                (cond ((char= c #\() (incf level))
                                      ((char= c #\))
                                       (when (zerop (decf level)) (return)))))))
                   (loop while (and (< i n)
                                    (not (member (char text i)
                                                 '(#\Space #\Tab #\Newline #\Return
                                                   #\( #\) #\" #\; #\'))))
                         do (incf i)))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((char= c #\\) (incf i 2) (object-done))
                   ((char= c #\") (skip-delimited #\") (object-done))
                   ((char= c #\|) (skip-delimited #\|) (object-done))
                   ((char= c #\;)
                    (setf i (or (position #\Newline text :start i) n)))
                   ((member c '(#\' #\`))
                    (prefix) (incf i))
                   ((char= c #\,)
                    (prefix)
                    (incf i (if (member (peek 1) '(#\@ #\.)) 2 1)))
                   ((char= c #\#)
                    (let ((next (peek 1)))
                      (cond
                        ((eql next #\|)
                         (let ((level 1))
                           (declare (type fixnum level))
                           (incf i 2)
                           (loop while (and (< i n) (plusp level))
                                 do (cond ((and (char= (char text i) #\|) (eql (peek 1) #\#))
                                           (decf level) (incf i 2))
                                          ((and (char= (char text i) #\#) (eql (peek 1) #\|))
                                           (incf level) (incf i 2))
                                          (t (incf i))))))
                        ((eql next #\\)
                         ;; #\x: the character after the backslash is data.
                         (incf i 3) (object-done))
                        ((member next '(#\' #\.))
                         (prefix) (incf i 2))
                        ((member next '(#\+ #\-))
                         (prefix) (incf i 2) (skip-feature-expression))
                        ((and next (digit-char-p next))
                         ;; #n= labels the object after it; #n# is an atom.
                         (let ((j (position-if-not #'digit-char-p text :start (1+ i))))
                           (cond ((and j (char= (char text j) #\=))
                                  (prefix) (setf i (1+ j)))
                                 ((and j (char= (char text j) #\#))
                                  (setf i (1+ j)) (object-done))
                                 (t (setf i (or j n))))))
                        ;; #( #S( #C( ...: the ( that follows is counted as a list.
                        (t (incf i)))))
                   ((char= c #\()
                    (let ((levels (1+ run)))
                      (declare (type fixnum levels))
                      (incf depth levels)
                      (push levels held)
                      (setf run 0)
                      (when (> depth limit)
                        (return-from lisp-too-deep-p t)))
                    (incf i))
                   ((char= c #\))
                    (when held (decf depth (pop held)))
                    (object-done)
                    (incf i))
                   ((member c '(#\Space #\Tab #\Newline #\Return #\Page))
                    (incf i))
                   (t (object-done) (incf i))))))
    nil))

(defun check-lisp-nesting (text)
  "Signal an ERROR when reading TEXT would recurse deeper than
+MAX-LISP-NESTING+ (LISP-TOO-DEEP-P), before
a reader recurses into it; otherwise return TEXT."
  (when (lisp-too-deep-p text)
    (error "The source nests lists and prefixes such as ' or #+ more than ~D ~
levels deep, which is deeper than cl-mcp will read: a reader recursing that ~
far can exhaust the server's stack.  Split or flatten the form."
           +max-lisp-nesting+))
  text)
