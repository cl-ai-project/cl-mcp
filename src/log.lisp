;;;; src/log.lisp

(defpackage #:lisp-mcp-server/src/log
  (:use #:cl)
  (:import-from #:uiop #:getenv)
  (:import-from #:yason #:encode #:*parse-json-arrays-as-vectors*)
  (:export
   #:log-event
   #:set-log-level-from-env
   #:should-log-p
   #:*log-level*
   #:*log-stream*
   #:*log-context*))

(in-package #:lisp-mcp-server/src/log)

(defparameter *log-level* :debug)
(defparameter *log-stream* *error-output*)
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
  (<= (%level->int level) (%level->int *log-level*)))

(defun log-event (level event &rest kvs)
  "Emit a JSON log line to *log-stream* with LEVEL and EVENT.
Additional key-values KV can be provided as alternating strings and values."
  (when (should-log-p level)
    (let ((obj (make-hash-table :test #'equal)))
      (setf (gethash "ts" obj) (%ts-iso8601))
      (setf (gethash "level" obj) (string-downcase (symbol-name level)))
      (setf (gethash "event" obj) event)
      (when *log-context*
        (loop for (k v) on *log-context* by #'cddr
              when k do (setf (gethash k obj) v)))
      (loop for (k v) on kvs by #'cddr
            when k do (setf (gethash k obj) v))
      (yason:encode obj *log-stream*)
      (terpri *log-stream*)
      (finish-output *log-stream*))))

;; initialize level from env at load
(set-log-level-from-env)

;; Ensure JSON arrays are decoded as vectors so downstream consumers (tests and
;; tools) see ARRAYP results from `yason:parse`.
(setf yason:*parse-json-arrays-as-vectors* t)
