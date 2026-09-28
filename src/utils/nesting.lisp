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

The scan mirrors the reader's recursion with an explicit stack of the reads
still waiting for an object: an open list waits for its ), a prefix such as
' ` , ,@ #' or #. waits for one object, and #+ or #- for two -- the feature
expression, then the form.  Any other dispatch (#C, #S, #P, #A, #n=, one a
readtable defines ...) is taken to wait for one object too, so an unknown
macro can only make the count larger.  An atom completes the read on top of
the stack, and a read that completes completes the one below it.  Strings,
|symbols|, comments and #\\x are atoms or skipped, so a ( inside them does
not count.  The text need not read; custom syntax is counted as standard
syntax, so the answer is an estimate -- one that errs towards a larger
depth, which is all a limit this far from real code needs."
  (declare (type string text) (type fixnum limit))
  (let ((stack '())                     ; :list, or a cons (:need . n)
        (depth 0)
        (i 0)
        (n (length text)))
    (declare (type fixnum depth i n))
    (labels ((peek (k)
               (and (< (+ i k) n) (char text (+ i k))))
             (push-frame (frame)
               (push frame stack)
               (when (> (incf depth) limit)
                 (return-from lisp-too-deep-p t)))
             (pop-frame ()
               (pop stack)
               (decf depth))
             (complete ()
               ;; An object was read: it satisfies the read on top of the
               ;; stack, and a prefix read that is satisfied is itself an
               ;; object for the read below it.
               (loop while (and stack (consp (first stack)))
                     do (if (zerop (decf (cdr (first stack))))
                            (pop-frame)
                            (return))))
             (terminating-p (c)
               (member c '(#\Space #\Tab #\Newline #\Return #\Page
                           #\( #\) #\" #\' #\` #\, #\;)))
             (skip-delimited (close)
               ;; I is on the opening delimiter; leave it just past CLOSE.
               (incf i)
               (loop while (< i n)
                     do (let ((c (char text i)))
                          (cond ((char= c #\\) (incf i 2))
                                ((char= c close) (incf i) (return))
                                (t (incf i))))))
             (skip-token ()
               ;; A token runs to a terminating character; escapes and
               ;; |...| inside it are part of it.
               (loop while (< i n)
                     do (let ((c (char text i)))
                          (cond ((char= c #\\) (incf i 2))
                                ((char= c #\|) (skip-delimited #\|))
                                ((terminating-p c) (return))
                                (t (incf i))))))
             (skip-block-comment ()
               (let ((level 1))
                 (declare (type fixnum level))
                 (incf i 2)
                 (loop while (and (< i n) (plusp level))
                       do (cond ((and (char= (char text i) #\|) (eql (peek 1) #\#))
                                 (decf level) (incf i 2))
                                ((and (char= (char text i) #\#) (eql (peek 1) #\|))
                                 (incf level) (incf i 2))
                                (t (incf i))))))
             (dispatch ()
               ;; I is on #.  Leave I past the dispatch characters.
               (let* ((j (or (position-if-not #'digit-char-p text :start (1+ i)) n))
                      (sub (and (< j n) (char text j)))
                      (digits-p (> j (1+ i))))
                 (cond
                   ((null sub) (setf i n) (complete))
                   ((and (not digits-p) (char= sub #\|)) (skip-block-comment))
                   ((char= sub #\\)
                    ;; #\x, #\Space: the character after \ is data.
                    (setf i (+ j 2)) (skip-token) (complete))
                   ((char= sub #\()
                    ;; #( and #n( read a list.
                    (setf i (1+ j)) (push-frame :list))
                   ((and digits-p (char= sub #\#))
                    ;; #n# refers to a labelled object: an atom.
                    (setf i (1+ j)) (complete))
                   ((and (not digits-p) (char= sub #\:))
                    (setf i (1+ j)) (skip-token) (complete))
                   ((and (not digits-p) (member sub '(#\+ #\-)))
                    (setf i (1+ j)) (push-frame (cons :need 2)))
                   (t
                    ;; #' #. #C #S #P #A #nA #n= #B #X ... and any macro a
                    ;; readtable adds: counted as reading one object.  For #B
                    ;; #X #R the token after them is that object.
                    (setf i (1+ j)) (push-frame (cons :need 1)))))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((member c '(#\Space #\Tab #\Newline #\Return #\Page))
                    (incf i))
                   ((char= c #\;)
                    (setf i (or (position #\Newline text :start i) n)))
                   ((char= c #\") (skip-delimited #\") (complete))
                   ((member c '(#\' #\`))
                    (incf i) (push-frame (cons :need 1)))
                   ((char= c #\,)
                    (incf i (if (member (peek 1) '(#\@ #\.)) 2 1))
                    (push-frame (cons :need 1)))
                   ((char= c #\#) (dispatch))
                   ((char= c #\()
                    (incf i) (push-frame :list))
                   ((char= c #\))
                    (incf i)
                    ;; Close the innermost list, abandoning any prefix left
                    ;; waiting inside it; a stray ) closes nothing.
                    (when (member :list stack)
                      (loop until (eq (first stack) :list) do (pop-frame))
                      (pop-frame)
                      (complete)))
                   (t (skip-token) (complete))))))
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
