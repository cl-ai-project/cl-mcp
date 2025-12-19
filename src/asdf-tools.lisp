;;;; src/asdf-tools.lisp

(defpackage #:cl-mcp/src/asdf-tools
  (:use #:cl)
  (:import-from #:asdf
                #:find-system
                #:system-depends-on
                #:system-defsystem-depends-on
                #:component-name
                #:component-version
                #:system-source-file
                #:system-source-directory
                #:system-description
                #:system-author
                #:system-license
                #:registered-systems
                #:component-loaded-p)
  (:import-from #:uiop
                #:native-namestring
                #:ensure-directory-pathname)
  (:export #:asdf-system-info
           #:asdf-list-systems))

(in-package #:cl-mcp/src/asdf-tools)

(declaim (inline %coerce-system-name %maybe-string %deps->vector))

(defmacro %with-quiet-asdf-io (() &body body)
  "Run BODY while suppressing ASDF/UIOP chatter on standard streams.

In :stdio transport mode, *standard-output* carries JSON-RPC. Any stray text
(e.g., ASDF compile/load messages) can corrupt the protocol and disconnect the
client, so we discard output during ASDF operations."
  `(let ((null (make-broadcast-stream)))
     (let ((*standard-output* null)
           (*error-output* null)
           (*trace-output* null)
           (*load-verbose* nil)
           (*load-print* nil)
           (*compile-verbose* nil)
           (*compile-print* nil))
       ,@body)))

(defun %coerce-system-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))))

(defun %maybe-string (x)
  (when x
    (typecase x
      (string x)
      (otherwise (princ-to-string x)))))

(defun %dep-name (dep)
  (let ((d (if (consp dep) (car dep) dep)))
    (string-downcase
     (etypecase d
       (string d)
       (symbol (symbol-name d))))))

(defun %deps->vector (deps)
  (coerce (mapcar #'%dep-name deps) 'vector))

(defun asdf-system-info (system-name)
  "Return detailed information about SYSTEM-NAME as a hash-table.

SYSTEM-NAME is a string or symbol naming a registered ASDF system.

The returned hash-table uses string keys:
  name, version, description, author, license,
  depends_on, defsystem_depends_on,
  source_file, source_directory, loaded.

Missing optional fields are returned as NIL (encoded as null)."
  (let ((coerced (%coerce-system-name system-name)))
    (handler-case
        (%with-quiet-asdf-io ()
          (let* ((sys (find-system coerced))
                 (h (make-hash-table :test #'equal))
                 (depends (ignore-errors (system-depends-on sys)))
                 (defsystem-depends
                   (ignore-errors (system-defsystem-depends-on sys)))
                 (src-file (ignore-errors (system-source-file sys)))
                 (src-dir (ignore-errors (system-source-directory sys))))
            (setf (gethash "name" h) (component-name sys)
                  (gethash "version" h) (component-version sys)
                  (gethash "description" h)
                  (%maybe-string (ignore-errors (system-description sys)))
                  (gethash "author" h)
                  (%maybe-string (ignore-errors (system-author sys)))
                  (gethash "license" h)
                  (%maybe-string (ignore-errors (system-license sys)))
                  (gethash "depends_on" h)
                  (%deps->vector (or depends '()))
                  (gethash "defsystem_depends_on" h)
                  (%deps->vector (or defsystem-depends '()))
                  (gethash "source_file" h)
                  (and src-file (native-namestring src-file))
                  (gethash "source_directory" h)
                  (and src-dir
                       (native-namestring
                        (ensure-directory-pathname src-dir)))
                  (gethash "loaded" h) (component-loaded-p sys))
            h))
      (error (e)
        (error "Failed to find system ~A: ~A" coerced e)))))

(defun asdf-list-systems ()
  "Return a vector of all registered ASDF system names.

Names are lower-case strings."
  (%with-quiet-asdf-io ()
    (coerce
     (mapcar (lambda (s)
               (string-downcase
                (etypecase s
                  (string s)
                  (symbol (symbol-name s)))))
             (registered-systems))
     'vector)))
