#!/usr/bin/env sbcl --script
;; Simple TCP client to send MCP initialize and print the response line.

(require :asdf)
(let ((ok nil))
  (handler-case
      (progn (asdf:load-system :usocket) (setf ok t))
    (error ()
      (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
        (when (probe-file setup)
          (load setup)
          (asdf:load-system :usocket)
          (setf ok t)))))
  (unless ok
    (format *error-output* "Unable to load :usocket. Ensure Quicklisp is installed.~%")
    (uiop:quit 3)))

(defun argv-get (name &optional default)
  (let* ((argv (or (ignore-errors sb-ext:*posix-argv*) (list)))
         (pos (position name argv :test #'string=)))
    (or (and pos (let ((idx (1+ pos))) (when (< idx (length argv)) (nth idx argv))))
        default)))

(let ((host (or (argv-get "--host") (or (uiop:getenv "MCP_HOST") "127.0.0.1")))
      (port (parse-integer (or (argv-get "--port") (or (uiop:getenv "MCP_PORT") "12345"))))
      (sock nil)
      (stream nil))
  (handler-case
      (progn
        (setf sock (usocket:socket-connect host port :element-type 'character))
        (setf stream (usocket:socket-stream sock))
        (write-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n" stream)
        (finish-output stream)
        (let ((line (read-line stream nil nil)))
          (when line (format t "~A~%" line)))
        (uiop:quit 0))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 2))))
