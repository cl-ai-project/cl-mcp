;;;; src/project-scaffold.lisp
;;;;
;;;; MCP tool entry for project-scaffold. Thin I/O layer on top of the pure
;;;; logic in project-scaffold-core. Runs in the parent (inline) process
;;;; alongside other fs-* tools. Registers itself with the tool registry
;;;; at load time via define-tool.

(defpackage #:cl-mcp/src/project-scaffold
  (:use #:cl)
  (:import-from #:cl-mcp/src/project-scaffold-core
                #:validate-project-name
                #:validate-destination
                #:validate-text-field
                #:plan-scaffold
                #:invalid-argument-error)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-project-root
                #:path-inside-p)
  (:export #:write-scaffold))

(in-package #:cl-mcp/src/project-scaffold)

(defun %uuid-suffix ()
  "Return a short pseudo-random suffix for temp directory naming."
  (format nil "~8,'0X" (random #xFFFFFFFF)))

(defun %absolute-scaffold-paths (root destination name)
  "Return (values TARGET-DIR TEMP-DIR) as absolute directory pathnames.
Both paths are inside ROOT. TEMP-DIR has a random suffix to avoid
collisions between concurrent calls."
  (let* ((dest-dir (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (uiop:ensure-directory-pathname destination)
                     root)))
         (target-dir (uiop:ensure-directory-pathname
                      (merge-pathnames
                       (uiop:ensure-directory-pathname name)
                       dest-dir)))
         (temp-dir (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (uiop:ensure-directory-pathname
                      (format nil ".tmp-project-scaffold-~A" (%uuid-suffix)))
                     dest-dir))))
    (values target-dir temp-dir)))

(defun %assert-within-project-root (pathname)
  "Signal error if PATHNAME is not inside *project-root*."
  (unless (path-inside-p pathname *project-root*)
    (error 'invalid-argument-error
           :field "destination" :value (namestring pathname)
           :reason "resolves outside project root")))

(defun %write-files-to-temp (temp-dir plan)
  "Write all PLAN entries into TEMP-DIR. Caller cleans up on error."
  (ensure-directories-exist temp-dir)
  (dolist (entry plan)
    (let* ((rel (car entry))
           (content (cdr entry))
           (abs (merge-pathnames rel temp-dir)))
      (ensure-directories-exist abs)
      (with-open-file (out abs :direction :output
                               :if-exists :error
                               :if-does-not-exist :create
                               :element-type 'character)
        (write-sequence content out)))))

(defun write-scaffold (&key name description author license destination)
  "Generate the scaffold project atomically. Returns a plist with:
  :target-dir (absolute pathname)
  :relative-path (namestring relative to *project-root*)
  :files (list of relative path strings, in manifest order)

On any failure, signals INVALID-ARGUMENT-ERROR or propagates the
underlying error after cleaning up the temp directory."
  (ensure-project-root)
  (validate-project-name name)
  (validate-destination destination)
  (validate-text-field "description" (or description ""))
  (validate-text-field "author" (or author ""))
  (validate-text-field "license" (or license ""))
  (multiple-value-bind (target-dir temp-dir)
      (%absolute-scaffold-paths *project-root* destination name)
    (%assert-within-project-root target-dir)
    (%assert-within-project-root temp-dir)
    (when (uiop:directory-exists-p target-dir)
      (error 'invalid-argument-error
             :field "name" :value name
             :reason (format nil "target directory already exists: ~A"
                             (namestring target-dir))))
    (let ((plan (plan-scaffold :name name
                               :description (or description "")
                               :author (or author "")
                               :license (or license "")
                               :destination destination))
          (committed nil))
      (unwind-protect
           (progn
             (%write-files-to-temp temp-dir plan)
             (rename-file temp-dir target-dir)
             (setf committed t)
             (list :target-dir target-dir
                   :relative-path (enough-namestring target-dir *project-root*)
                   :files (mapcar #'car plan)))
        (unless committed
          (when (uiop:directory-exists-p temp-dir)
            (ignore-errors
             (uiop:delete-directory-tree temp-dir :validate t))))))))
