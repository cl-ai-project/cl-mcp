(defpackage #:cl-mcp/src/utils/printing
  (:use #:cl)
  (:export #:safe-prin1))

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
