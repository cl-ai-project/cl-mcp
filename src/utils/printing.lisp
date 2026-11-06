(defpackage #:cl-mcp/src/utils/printing
  (:use #:cl)
  (:export #:safe-prin1
           #:circular-p
           #:bounded-princ-to-string))

(in-package #:cl-mcp/src/utils/printing)

(declaim (ftype (function (t &key (:level (or null fixnum))
                                  (:length (or null fixnum))
                                  (:circle boolean))
                          string)
                safe-prin1))

(defun safe-prin1 (object &key (level 3) (length 10) (circle nil))
  "Safely convert OBJECT to a printed string representation.
Handles errors during printing gracefully, returning an error description
instead of signaling.

Arguments:
  OBJECT -- Any Lisp object to print.
  LEVEL  -- Maximum depth for nested structures (default: 3).
  LENGTH -- Maximum number of elements to print (default: 10).
  CIRCLE -- If true, detect circular structures (default: nil).

Returns:
  A string representation of OBJECT, or an error message if printing fails.

Examples:
  (safe-prin1 '(1 2 3))
  => \"(1 2 3)\"

  (safe-prin1 (make-hash-table))
  => \"#<HASH-TABLE ...>\""
  (declare (type t object)
           (type (or null fixnum) level length)
           (type boolean circle))
  (handler-case
      (let ((*print-level* level)
            (*print-length* length)
            (*print-readably* nil)
            (*print-circle* circle))
        (prin1-to-string object))
    (error (e)
      (format nil "#<error printing: ~A>" e))))

(defun %walked-container-p (object conses-only)
  "True for the objects CIRCULAR-P walks into: conses and, unless CONSES-ONLY,
arrays whose elements can be any object and structure instances.  A string or
a specialized array holds no object that could lead back to it; a standard
object or a hash table (a structure in SBCL, whose storage a weak table points
back from) prints as #<...> without printing what it holds."
  (or (consp object)
      (and (not conses-only)
           (or (and (arrayp object) (eq (array-element-type object) t))
               (and (typep object 'structure-object)
                    (not (hash-table-p object)))))))

(defun %walked-children (node)
  "Return the objects inside NODE, an array or a structure instance, that
CIRCULAR-P walks: every element of an array (fill pointer ignored), every
tagged slot of a structure (raw slots hold numbers)."
  (if (arrayp node)
      (loop for i below (array-total-size node)
            collect (row-major-aref node i))
      (let ((children '()))
        (sb-kernel:do-instance-tagged-slot (i node)
          (push (sb-kernel:%instance-ref node i) children))
        children)))

(defun circular-p (object &key conses-only)
  "Return T when OBJECT contains a cycle that printing it would follow.

Source text can be circular: #n= is standard reader syntax, so
\"#1=(a b . #1#)\", \"#1=#(a #1#)\" and a #S(...) that refers to itself read
fine.  Printing such an object with *PRINT-CIRCLE* NIL never ends (or exhausts
the stack); callers that keep *PRINT-CIRCLE* off for readability -- labels on
merely shared structure are noise -- switch it on when this says so.  The walk
follows conses, arrays whose elements can be any object (fill pointers
ignored) and the tagged slots of structure instances, which the default
printer writes as #S(...).  A standard object or a hash table prints as
#<...> and is not entered.  A structure with a print method of its own is
entered all the same -- a package, whose use and used-by lists point at each
other, answers T -- which only turns on *PRINT-CIRCLE* for nothing.

With CONSES-ONLY, only conses are followed: the question is then whether a
walk of the list structure, such as MACROEXPAND-ALL's, would loop.

The walk is depth-first with its own stack on the heap, not the control
stack: a long list, or an object nested deep through cars, vectors or
structures -- which a macro can build, so no check on source text sees it --
is ordinary input, and asking this before printing it under *PRINT-LEVEL*
must not exhaust the control stack.  A node is on the path from the time it
is entered until all its children are finished, and a child found on the path
is a cycle.  Finished nodes are memoized, so a heavily shared but acyclic
object costs one visit per distinct node instead of blowing up exponentially."
  (let ((on-path (make-hash-table :test #'eq))
        (finished (make-hash-table :test #'eq))
        ;; Each frame is (NODE . CHILDREN-NOT-YET-ENTERED).
        (stack '()))
    (flet ((enter (node)
             "Push a frame for NODE unless it is a leaf or finished; return T
when NODE is on the path, which closes a cycle."
             (cond ((not (%walked-container-p node conses-only)) nil)
                   ((gethash node on-path) t)
                   ((gethash node finished) nil)
                   (t
                    (setf (gethash node on-path) t)
                    (push (cons node (if (consp node)
                                         (list (car node) (cdr node))
                                         (%walked-children node)))
                          stack)
                    nil))))
      (when (enter object)
        (return-from circular-p t))
      (loop while stack
            do (let ((frame (first stack)))
                 (if (cdr frame)
                     (when (enter (pop (cdr frame)))
                       (return-from circular-p t))
                     (progn
                       (pop stack)
                       (remhash (car frame) on-path)
                       (setf (gethash (car frame) finished) t)))))
      nil)))

(defun bounded-princ-to-string (object)
  "Return OBJECT PRINCed for a message -- a condition's report, say -- with
*PRINT-CIRCLE* on and *PRINT-LEVEL* 8 and *PRINT-LENGTH* 50.  What a report
prints can be anything it was given, circular structure included, and only
all three bound together guarantee the printing ends soon: circle notation
stops a cycle, and the level and length stop a merely huge object (the bounds
alone do not stop #1=(#1# . #1#), which holds 50^8 elements within them).
The price: an object the report prints twice is labelled, #1=x and #1#."
  (let ((*print-circle* t)
        (*print-level* 8)
        (*print-length* 50))
    (princ-to-string object)))
