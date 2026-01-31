(defpackage #:cl-mcp/src/utils/system
  (:use #:cl)
  (:export #:fd-count))

(in-package #:cl-mcp/src/utils/system)

(declaim (ftype (function () (or null fixnum)) fd-count))

(defun fd-count ()
  "Return the count of open file descriptors for the current process.
Returns NIL on non-Linux systems or when the count cannot be determined.

This is a debugging utility useful for detecting file descriptor leaks.
It works by counting entries in /proc/self/fd/ on Linux systems."
  (ignore-errors (length (directory #P"/proc/self/fd/*"))))
