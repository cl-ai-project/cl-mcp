(defpackage #:cl-mcp/src/utils/printing
  (:use #:cl)
  (:export #:safe-prin1
           #:circular-p))

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

(defun %walked-container-p (object)
  "True for the objects CIRCULAR-P walks into: conses, and arrays whose
elements can be any object.  A string or a specialized array holds no object
that could lead back to it."
  (or (consp object)
      (and (arrayp object) (eq (array-element-type object) t))))

(defun circular-p (object)
  "Return T when OBJECT contains a cycle reachable through conses and arrays.

Source text can be circular: #n= is standard reader syntax, so
\"#1=(a b . #1#)\" and \"#1=#(a #1#)\" read fine.  Printing such an object with
*PRINT-CIRCLE* NIL never ends; callers that keep *PRINT-CIRCLE* off for
readability (labels on merely shared structure are noise) switch it on when
this says so.  Structure instances are not walked: their slots are not reached
by the reader's #n= the way list and vector elements are, and a cycle through
one is not something this guards against.

The cdr spine is walked iteratively and only cars and array elements recurse,
so stack depth is bounded by nesting rather than by list length: a long quoted
literal such as a generated lookup table is ordinary input.  Finished nodes are
memoized, so a heavily shared but acyclic object costs one visit per distinct
node instead of blowing up exponentially."
  (let ((on-path (make-hash-table :test #'eq))
        (finished (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (let ((spine '())
                     (found nil))
                 (loop
                   (cond
                     ((not (%walked-container-p node)) (return))
                     ((gethash node on-path) (setf found t) (return))
                     ((gethash node finished) (return))
                     (t
                      (setf (gethash node on-path) t)
                      (push node spine)
                      (cond
                        ((consp node)
                         (when (walk (car node))
                           (setf found t)
                           (return))
                         (setf node (cdr node)))
                        (t
                         (setf found (loop for i below (array-total-size node)
                                           thereis (walk (row-major-aref node i))))
                         (return))))))
                 (dolist (visited spine)
                   (remhash visited on-path)
                   (setf (gethash visited finished) t))
                 found)))
      (and (walk object) t))))
