;;;; src/log.lisp

(defpackage #:cl-mcp/src/log
  (:use #:cl)
  (:import-from #:uiop #:getenv)
  (:import-from #:yason #:encode #:*parse-json-arrays-as-vectors*)
  (:export
   #:log-event
   #:set-log-level-from-env
   #:setup-log-file
   #:should-log-p
   #:*log-level*
   #:*log-stream*
   #:*log-file-stream*
   #:*log-context*))

(in-package #:cl-mcp/src/log)

(defparameter *log-level* :debug)
(defparameter *log-stream* *error-output*)
(defparameter *log-file-stream* nil
  "File stream opened by setup-log-file, kept for cleanup.")
(defparameter *log-context* nil
  "Optional list of alternating key/value pairs appended to every log line.")

(defun %level->int (level)
  (ecase level
    (:debug 10)
    (:info 20)
    (:warn 30)
    (:error 40)))

(defun %parse-level (s)
  (cond
    ((null s) nil)
    ((string= s "debug") :debug)
    ((string= s "info") :info)
    ((string= s "warn") :warn)
    ((string= s "warning") :warn)
    ((string= s "error") :error)
    (t nil)))

(defun set-log-level-from-env ()
  (let* ((env (uiop:getenv "MCP_LOG_LEVEL"))
         (lvl (%parse-level (and env (string-downcase env)))))
    (when lvl (setf *log-level* lvl))
    *log-level*))

(defun %ts-iso8601 ()
  (multiple-value-bind (sec min hour day mon year) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year mon day hour min sec)))

(defun should-log-p (level)
  (>= (%level->int level) (%level->int *log-level*)))

(defun log-event (level event &rest kvs)
  "Emit a JSON log line to *log-stream* with LEVEL and EVENT.
Additional key-values KV can be provided as alternating strings and values.
Stream errors are silently ignored to prevent recursive error cascades
when *log-stream* becomes a broken pipe."
  (when (should-log-p level)
    (let ((obj (make-hash-table :test #'equal)))
      (setf (gethash "ts" obj) (%ts-iso8601))
      (setf (gethash "level" obj) (string-downcase (symbol-name level)))
      (setf (gethash "event" obj) event)
      (when *log-context*
        (loop for (k v) on *log-context* by #'cddr
              when k
              do (setf (gethash k obj) v)))
      (loop for (k v) on kvs by #'cddr
            when k
            do (setf (gethash k obj) v))
      (ignore-errors
        (yason:encode obj *log-stream*)
        (terpri *log-stream*)
        (finish-output *log-stream*)))))

(defun %ts-filename ()
  "Return a timestamp string safe for filenames (no colons)."
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D-~2,'0D-~2,'0D"
            year mon day hour min sec)))

(defun setup-log-file ()
  "If MCP_LOG_FILE is set, open a timestamped log file and set *log-stream*
to a broadcast stream writing to both stderr and the file.
Example: MCP_LOG_FILE=/tmp/cl-mcp.log creates /tmp/cl-mcp-2026-03-01T09-15-30.log"
  (let ((path-template (uiop:getenv "MCP_LOG_FILE")))
    (when (and path-template (plusp (length path-template)))
      (let* ((path (pathname path-template))
             (stem (pathname-name path))
             (ts (%ts-filename))
             (actual (make-pathname :defaults path
                                    :name (format nil "~A-~A" stem ts))))
        (handler-case
            (let ((file-stream (open actual :direction :output
                                           :if-exists :append
                                           :if-does-not-exist :create
                                           :external-format :utf-8)))
              (setf *log-file-stream* file-stream)
              (setf *log-stream*
                    (make-broadcast-stream *error-output* file-stream))
              (format *error-output* "Log file: ~A~%" (namestring actual))
              (finish-output *error-output*))
          (error (e)
            (format *error-output* "WARNING: Failed to open log file ~A: ~A~%"
                    (namestring actual) e)
            (finish-output *error-output*)))))))

;; initialize level from env at load
(set-log-level-from-env)

;; setup file logging from env at load
(setup-log-file)

;; Ensure JSON arrays are decoded as vectors so downstream consumers (tests and
;; tools) see ARRAYP results from `yason:parse`.
(setf yason:*parse-json-arrays-as-vectors* t)
