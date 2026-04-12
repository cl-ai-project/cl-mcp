;;;; src/project-scaffold.lisp
;;;;
;;;; MCP tool entry for project-scaffold. Thin I/O layer on top of the pure
;;;; logic in project-scaffold-core. Runs in the parent (inline) process
;;;; alongside other fs-* tools. Registers itself with the tool registry
;;;; at load time via define-tool.

(defpackage #:cl-mcp/src/project-scaffold
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:result
                #:text-content)
  (:import-from #:cl-mcp/src/project-scaffold-core
                #:validate-project-name
                #:validate-destination
                #:validate-text-field
                #:plan-scaffold
                #:invalid-argument-error)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-project-root
                #:path-inside-p)
  (:export #:project-scaffold
           #:write-scaffold))

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

(defun %resolve-deepest-existing (pathname)
  "Return the deepest existing ancestor of PATHNAME as a resolved truename.
Walks up parent-by-parent through PATHNAME's directory components until
TRUENAME succeeds, then returns that ancestor as a directory pathname.
Used to detect symlink-based path traversal before any filesystem write:
even when the target itself does not exist yet, its deepest existing
ancestor must still resolve inside *PROJECT-ROOT*."
  (labels ((walk (dir)
             (or (ignore-errors
                  (uiop/pathname:ensure-directory-pathname (truename dir)))
                 (let ((parent (uiop/pathname:pathname-parent-directory-pathname
                                dir)))
                   (cond
                     ((null parent) nil)
                     ((equal parent dir) nil)
                     (t (walk parent)))))))
    (walk (uiop/pathname:ensure-directory-pathname pathname))))

(defun %assert-within-project-root (pathname)
  "Signal error if PATHNAME cannot be resolved inside *PROJECT-ROOT*.
Resolves the deepest existing ancestor of PATHNAME via TRUENAME to
neutralize symlinks planted anywhere along the path. Even if PATHNAME
itself does not exist yet, its nearest existing ancestor must be inside
the truenamed *PROJECT-ROOT* or this function signals
INVALID-ARGUMENT-ERROR."
  (let ((resolved-ancestor (%resolve-deepest-existing pathname))
        (resolved-root
         (or (ignore-errors
              (uiop/pathname:ensure-directory-pathname
               (truename *project-root*)))
             *project-root*)))
    (unless (and resolved-ancestor
                 (path-inside-p resolved-ancestor resolved-root))
      (error 'invalid-argument-error
             :field "destination"
             :value (namestring pathname)
             :reason "resolves outside project root"))))

(defun %write-files-to-temp (temp-dir plan)
  "Write all PLAN entries into TEMP-DIR via FS-WRITE-FILE.
TEMP-DIR must already be inside *PROJECT-ROOT*; the caller is
responsible for that containment check. Using FS-WRITE-FILE keeps the
file-write path inside the same sandbox wrapper as the other MCP file
tools, so scaffold writes go through ENSURE-WRITE-PATH validation and
atomic write-to-temp-then-rename for each file. Caller cleans up
TEMP-DIR if any intermediate write fails."
  (ensure-directories-exist temp-dir)
  (let ((temp-relative (enough-namestring temp-dir *project-root*)))
    (dolist (entry plan)
      (let ((rel (car entry))
            (content (cdr entry)))
        (fs-write-file (concatenate 'string temp-relative rel) content)))))

(defun write-scaffold (&key name description author license destination overwrite)
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
    (when (and (uiop:directory-exists-p target-dir) (not overwrite))
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
             ;; Delete existing target AFTER temp is ready, preserving
             ;; atomicity: if %write-files-to-temp fails, the original
             ;; scaffold survives.
             (when (and overwrite (uiop:directory-exists-p target-dir))
               (uiop:delete-directory-tree target-dir :validate t))
             (rename-file temp-dir target-dir)
             (setf committed t)
             (list :target-dir target-dir
                   :relative-path (enough-namestring target-dir *project-root*)
                   :files (mapcar #'car plan)))
        (unless committed
          (when (uiop:directory-exists-p temp-dir)
            (ignore-errors
             (uiop:delete-directory-tree temp-dir :validate t))))))))

(define-tool "project-scaffold"
  :description
  "Generate a minimal Common Lisp project skeleton under the project root.

The generated project uses package-inferred-system + Rove and ships with
CLAUDE.md/AGENTS.md templates referencing cl-mcp's existing prompts via
relative @-include paths. On success, returns the list of created files and
a 'next_steps' array with concrete REPL commands the agent can invoke to
register the project with ASDF and run its tests.

Fails if the target directory already exists; choose a unique 'name' per
generation. Intended for creating throwaway sample projects to exercise
cl-mcp's tool surface."
  :args
  ((name :type :string :required t :description
         "Project name in lisp-case (e.g. foo-lib). Must match ^[a-z][a-z0-9-]*$ and be 1-64 chars.")
   (description :type :string :description
                "One-line project description for .asd and README. No newlines.")
   (author :type :string :description
           "Author string for .asd :author. No newlines.")
   (license :type :string :description
            "License string for .asd :license. No newlines.")
   (destination :type :string :description
                "Relative parent directory under project root where <name>/ is created. Default: scaffolds.")
   (overwrite :type :boolean :description
              "When true, replace an existing scaffold directory instead of failing."))
  :body
  (handler-case
      (let* ((result-plist
              (write-scaffold
               :name name
               :description (or description "A Common Lisp project scaffolded by cl-mcp.")
               :author (or author "Unknown")
               :license (or license "MIT")
               :destination (or destination "scaffolds")
               :overwrite overwrite))
             (target-dir (getf result-plist :target-dir))
             (relative (getf result-plist :relative-path))
             (files (getf result-plist :files))
             (abs-asd (namestring
                       (merge-pathnames (format nil "~A.asd" name) target-dir)))
             (next-steps
              (vector
               (format nil
                       "Auto-registered with ASDF. To re-register: (asdf:load-asd ~S)"
                       abs-asd)
               (format nil
                       "To load: run load-system with {\"system\": ~S}"
                       name)
               (format nil
                       "To test: run run-tests with {\"system\": ~S}"
                       (format nil "~A/tests" name))
               (format nil
                       "To edit: use lisp-edit-form with paths under ~A"
                       relative))))
        ;; Auto-register with ASDF
        (ignore-errors (asdf/find-system:load-asd abs-asd))
        (result id
                (make-ht
                 "created" t
                 "path" relative
                 "absolute_path" (namestring target-dir)
                 "files" (coerce files 'vector)
                 "next_steps" next-steps
                 "content"
                 (text-content
                  (format nil "Scaffolded ~A at ~A (~D files)~%Path: ~A~%~{~A~%~}"
                          name relative (length files)
                          (namestring target-dir)
                          (coerce next-steps 'list))))))
    (invalid-argument-error (e)
      (result id
              (make-ht
               "created" nil
               "error" (princ-to-string e)
               "content" (text-content (princ-to-string e)))))))
