;;;; src/utils/fasls.lisp
;;;;
;;;; Where a system's cached fasls live, and which of them cannot be trusted.
;;;; Shared by load-system's clear_fasls and run-tests' reload, which depend on
;;;; nothing of each other.

(defpackage #:cl-mcp/src/utils/fasls
  (:use #:cl)
  (:export #:fasl-source-directory
           #:delete-same-second-fasls))

(in-package #:cl-mcp/src/utils/fasls)

(defun fasl-source-directory (system-name)
  "Return (values DIRECTORY SYSTEM): the source directory whose cached fasls
belong to SYSTEM-NAME, and the system it is the directory of.  A package-inferred
subsystem such as \"my-app/src/contracts\" has no source directory of its own, so
its primary system's (\"my-app\") is used: that directory holds every fasl in the
tree, the subsystem's dependencies included.  NIL when neither is known."
  (flet ((source-dir-of (name)
           (let ((system (ignore-errors (asdf:find-system name nil))))
             (and system (asdf:system-source-directory system)))))
    (let* ((primary (asdf:primary-system-name system-name))
           (owner (cond ((source-dir-of system-name) system-name)
                        ((and (string/= primary system-name) (source-dir-of primary))
                         primary))))
      (values (and owner (source-dir-of owner)) owner))))

(defun delete-same-second-fasls (system-name)
  "Delete each cached fasl under SYSTEM-NAME's source tree (FASL-SOURCE-DIRECTORY)
whose source file was written in the same second as the fasl, or later.  Return
the number deleted.

ASDF judges a fasl current when its FILE-WRITE-DATE is not older than its
source's, and those dates are whole seconds: an edit landing in the second the
fasl was written leaves the two equal, so ASDF keeps the fasl and loads the code
from before the edit.  Such a fasl cannot be told from a current one, so it is
deleted and ASDF compiles the file again; one written a second or more after
its source is kept, so a run recompiles only what may be stale, unlike
clear_fasls, which drops them all."
  (let ((source-dir (fasl-source-directory system-name))
        (deleted 0))
    (when source-dir
      (dolist (source (directory (merge-pathnames "**/*.lisp" source-dir)))
        (let* ((fasl (ignore-errors
                      (asdf:apply-output-translations (compile-file-pathname source))))
               (fasl-date (and fasl (ignore-errors (file-write-date fasl))))
               (source-date (ignore-errors (file-write-date source))))
          (when (and fasl-date source-date (>= source-date fasl-date)
                     (ignore-errors (delete-file fasl) t))
            (incf deleted)))))
    deleted))
