(defpackage #:cl-mcp/src/utils/strings
  (:use #:cl)
  (:export #:ensure-trailing-newline))

(in-package #:cl-mcp/src/utils/strings)

(declaim (ftype (function (string) string) ensure-trailing-newline))

(defun ensure-trailing-newline (string)
  "Ensure STRING ends with a newline character.
Returns STRING unchanged if it already ends with newline, otherwise
returns a new string with newline appended.

Arguments:
  STRING -- A string to check and potentially modify.

Returns:
  A string guaranteed to end with a newline character."
  (declare (type string string))
  (if (and (> (length string) 0)
           (char= (char string (1- (length string))) #\Newline))
      string
      (concatenate 'string string (string #\Newline))))

