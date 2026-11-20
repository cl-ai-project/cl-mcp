;;;; Loaded as source before the worker system, by both launchers.
(require :asdf)
(require :sb-posix)

(defpackage #:cl-mcp/worker-bootstrap
  (:use #:cl)
  (:export #:release))

(in-package #:cl-mcp/worker-bootstrap)

(defvar *lock-fd* nil)

(defun %open-lock-file (path)
  (sb-posix:open (uiop:native-namestring (ensure-directories-exist path))
                 (logior sb-posix:o-creat sb-posix:o-rdwr) #o666))

(defun %open-bootstrap-lock ()
  ;; Independently configured servers can share the same ASDF destination.
  ;; Keep the file there even after the lock is released: unlinking it would
  ;; let another worker lock a new inode while someone still holds the old one.
  (or (ignore-errors
        (let* ((source (asdf:system-relative-pathname :cl-mcp "src/worker/main.lisp"))
               (output (asdf:apply-output-translations (compile-file-pathname source))))
          (%open-lock-file (make-pathname :name "cl-mcp-worker-bootstrap" :type "lock"
                                          :defaults output))))
      ;; An installed set of current FASLs may be read-only, or the launcher's
      ;; loader may still need to locate the system. Its user cache is writable.
      (%open-lock-file (uiop:xdg-cache-home "cl-mcp" "worker-bootstrap.lock"))))

(defun %set-lock (fd byte type)
  (sb-posix:fcntl fd sb-posix:f-setlkw
                  (make-instance 'sb-posix:flock :type type :whence sb-posix:seek-set
                                 :start byte :len 1)))

(defun %outputs-to-build-p ()
  ;; In a fresh worker even an up-to-date system has many LOAD-OPs. Ask
  ;; whether the configured ASDF plan must actually produce any files.
  (handler-case
      (asdf/session:with-asdf-session ()
        (some (lambda (action) (asdf:output-files (car action) (cdr action)))
              (asdf/plan:plan-actions
               (asdf:make-plan nil 'asdf:load-op (asdf:find-system "cl-mcp/src/worker/main")))))
    ;; The launcher's normal loader (notably Roswell/Quicklisp) may still
    ;; discover or install a dependency. Let it do that with exclusive access.
    (error () t)))

(defun release ()
  "Release the worker's bootstrap lock after the launcher's system load."
  (when *lock-fd*
    (sb-posix:close *lock-fd*)
    (setf *lock-fd* nil)))

(let ((fd (%open-bootstrap-lock))
      (ready nil))
  (unwind-protect
       (progn
         ;; Byte 0 admits one planner at a time. Byte 1 protects the actual
         ;; load. A follower of a cold builder sees the completed cache and
         ;; can join other readers, even when many workers arrived together.
         (%set-lock fd 0 sb-posix:f-wrlck)
         (%set-lock fd 1 sb-posix:f-rdlck)
         (when (%outputs-to-build-p)
           (%set-lock fd 1 sb-posix:f-wrlck))
         (%set-lock fd 0 sb-posix:f-unlck)
         (setf *lock-fd* fd ready t))
    (unless ready (sb-posix:close fd))))
