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
                #:validate-framework
                #:plan-scaffold
                #:invalid-argument-error
                #:*scaffold-marker-file*)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:cl-mcp/src/project-scaffold-ownership
                #:scaffold-entry-kind
                #:signed-scaffold-marker
                #:discard-scaffold-backup
                #:scaffold-replacement-problem)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-project-root
                #:native-path-namestring
                #:path-inside-p)
  (:export #:project-scaffold
           #:write-scaffold))

(in-package #:cl-mcp/src/project-scaffold)

(defun %uuid-suffix ()
  "Return a short pseudo-random suffix for scratch directory naming."
  (format nil "~8,'0X" (random #xFFFFFFFF)))

(defvar *scaffold-lock* (make-lock "project-scaffold")
  "Serialize scaffold generation and replacement in the server.")

(defun %absolute-scaffold-target (root destination name)
  "Resolve the requested scaffold path under ROOT using native path components."
  ;; The CL pathname reader interprets a name such as demo[old] as a wildcard.
  (merge-pathnames
   (uiop:parse-unix-namestring name :ensure-directory t)
   (merge-pathnames (uiop:parse-unix-namestring destination :ensure-directory t) root)))

(defun %resolve-deepest-existing (pathname)
  "Return the deepest existing ancestor of PATHNAME as a truename and as originally spelled.
The two values locate the existing prefix and the suffix still to be created.
Used to detect symlink-based path traversal before any filesystem write:
even when the target itself does not exist yet, its deepest existing
ancestor must still resolve inside *PROJECT-ROOT*."
  (labels ((walk (dir)
             (let ((resolved (ignore-errors (truename dir))))
               (if resolved
                   (values (uiop:ensure-directory-pathname resolved) dir)
                   (let ((parent (uiop:pathname-parent-directory-pathname dir)))
                     (when (and parent (not (equal parent dir)))
                       (walk parent)))))))
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
             :value (native-path-namestring pathname)
             :reason "resolves outside project root"))))

(defun %write-files-to-temp (temp-dir location name plan)
  "Write the planned files and their ownership receipt into TEMP-DIR."
  (ensure-directories-exist temp-dir)
  ;; FS-WRITE-FILE expects native paths, including under bracketed project roots.
  (let ((relative (native-path-namestring (uiop:enough-pathname temp-dir *project-root*))))
    (dolist (entry plan)
      (fs-write-file (concatenate 'string relative (car entry)) (cdr entry)))
    (fs-write-file (concatenate 'string relative *scaffold-marker-file*)
                   (signed-scaffold-marker temp-dir location name (mapcar #'car plan)))))

(defun %scaffold-owned-p (directory &optional location)
  "Return whether DIRECTORY can be replaced, and the reason for any refusal."
  (let ((problem (scaffold-replacement-problem directory location)))
    (values (null problem) problem)))

(defun %make-scaffold-scratch (target)
  "Allocate a private, empty transaction directory beside TARGET."
  (let ((parent (uiop:pathname-parent-directory-pathname target)))
    (ensure-directories-exist parent)
    (loop for relative = (uiop:ensure-directory-pathname
                          (format nil ".tmp-project-scaffold-~A" (%uuid-suffix)))
          for scratch = (merge-pathnames relative parent)
          do (%assert-within-project-root scratch)
             (handler-case
                 (progn (sb-posix:mkdir (native-path-namestring scratch) #o700)
                        (return scratch))
               (sb-posix:syscall-error (condition)
                 (unless (= (sb-posix:syscall-errno condition) sb-posix:eexist)
                   (error condition)))))))

(defun %physical-scaffold-location (target)
  "Return the native identity of TARGET beneath its resolved parent."
  (let ((leaf (car (last (pathname-directory target))))
        (parent (truename (uiop:pathname-parent-directory-pathname target))))
    (native-path-namestring
     (merge-pathnames (uiop:parse-unix-namestring leaf :ensure-directory t) parent))))

(defun %require-scaffold-ownership (directory target &optional location)
  "Refuse replacement of TARGET if DIRECTORY cannot be verified."
  (multiple-value-bind (owned problem) (%scaffold-owned-p directory location)
    (unless owned
      (error 'invalid-argument-error
             :field "overwrite" :value (native-path-namestring target)
             :reason (format nil "cannot overwrite ~A because ~A"
                             (native-path-namestring target) problem)))))

(defun %restore-scaffold-backup (backup target)
  "Restore BACKUP after a rejected transaction, or report where it was preserved."
  (unless (ignore-errors
            (unless (scaffold-entry-kind target)
              (%assert-within-project-root target)
              (rename-file backup target)
              t))
    (error 'invalid-argument-error
           :field "overwrite" :value (native-path-namestring target)
           :reason (format nil "the original files are preserved at ~A"
                           (native-path-namestring backup)))))

(defun %delete-scaffold-tree (scratch)
  "Remove SCRATCH, which this call created exclusively for its transaction."
  (%assert-within-project-root scratch)
  (uiop:delete-directory-tree
   scratch :validate (lambda (path)
                       (and (equal path scratch)
                            (eq :directory (scaffold-entry-kind path))))))

(defun %prompt-destination (target-dir)
  "Return the destination relative to the physical root, including its uncreated suffix."
  (let ((destination (uiop:pathname-parent-directory-pathname target-dir)))
    (multiple-value-bind (resolved ancestor) (%resolve-deepest-existing destination)
      (let ((physical (merge-pathnames (uiop:enough-pathname destination ancestor) resolved)))
        (native-path-namestring (uiop:enough-pathname physical (truename *project-root*)))))))

(defun %missing-prompt-files (root)
  "Return manifest entries for the bundled guides ROOT does not already provide."
  (loop for path in '("prompts/repl-driven-development.md" "prompts/common-lisp-expert.md")
        unless (uiop:file-exists-p (merge-pathnames path root))
          collect (cons path (uiop:read-file-string
                              (asdf:system-relative-pathname :cl-mcp path)
                              :external-format :utf-8))))

(defun write-scaffold (&key name description author license destination overwrite framework)
  "Generate a scaffold and return its target path, generated files, and framework.
OVERWRITE replaces only a directory whose ownership and unchanged contents
this server can verify. The old tree is renamed aside and verified again
before the new one is published. On failure, restore the original directory.
If the new tree was published but the backup could not be safely removed,
return its pathname under :LEFTOVER-BACKUP for the caller to report."
  (ensure-project-root)
  (validate-project-name name)
  (validate-destination destination)
  (validate-text-field "description" (or description ""))
  (validate-text-field "author" (or author ""))
  (validate-text-field "license" (or license ""))
  (setf framework (validate-framework framework))
  (with-lock-held (*scaffold-lock*)
    (let ((target (%absolute-scaffold-target *project-root* destination name)))
      (%assert-within-project-root target)
      (let ((exists (scaffold-entry-kind target)))
        (when exists
          (unless overwrite
            (error 'invalid-argument-error
                   :field "name" :value name
                   :reason (format nil "target directory already exists at ~A"
                                   (native-path-namestring target))))
          (%require-scaffold-ownership target target))
        (let* ((plan (plan-scaffold :name name
                                    :description (or description "")
                                    :author (or author "")
                                    :license (or license "")
                                    :destination (%prompt-destination target)
                                    :framework framework
                                    :bundled-prompts (%missing-prompt-files *project-root*)))
               (scratch (%make-scaffold-scratch target))
               (temp (merge-pathnames "new/" scratch))
               (backup (merge-pathnames "old/" scratch))
               (moved-aside nil)
               (committed nil))
          (unwind-protect
               (let ((location (%physical-scaffold-location target)))
                 (%write-files-to-temp temp location name plan)
                 (%assert-within-project-root target)
                 (when exists
                   (rename-file target backup)
                   (setf moved-aside t)
                   (%require-scaffold-ownership backup target location))
                 (when (scaffold-entry-kind target)
                   (error 'invalid-argument-error
                          :field "name" :value name
                          :reason "the destination appeared during generation"))
                 (rename-file temp target)
                 (setf committed t)
                 (let ((keep-backup (and moved-aside
                                         (not (ignore-errors
                                                (discard-scaffold-backup backup location))))))
                   (unless keep-backup
                     (ignore-errors (sb-posix:rmdir (native-path-namestring scratch))))
                   (list :target-dir target
                         :relative-path (native-path-namestring
                                         (uiop:enough-pathname target *project-root*))
                         :files (mapcar #'car plan)
                         :framework framework
                         :leftover-backup (and keep-backup backup))))
            (unless committed
              (when moved-aside
                (%restore-scaffold-backup backup target))
              (ignore-errors (%delete-scaffold-tree scratch)))))))))

(define-tool "project-scaffold"
  :description
  "Generate a minimal Common Lisp project skeleton under the project root.

The generated project uses package-inferred-system. Its CLAUDE.md and AGENTS.md
include guides from the project root's prompts/ directory.
Each missing guide is copied from cl-mcp into the generated project's prompts/ directory
and listed in files. Its test suite is written with Rove by default;
pass 'framework' to get a FiveAM project instead. Either way run-tests
detects the framework from the generated .asd, so no extra wiring is needed.

On success, returns the list of created files, the resolved framework, and
a 'next_steps' array with concrete REPL commands the agent can invoke to
register the project with ASDF and run its tests.

An existing target is replaceable only with 'overwrite' and a signed
ownership manifest from this server process. Its generated files must match
the manifest and the directory must contain no extra entries. If verification
fails, the existing files are kept. Use a new name for an older or changed
scaffold. A refusal is an ordinary result with created=false and an error message.

The generated .asd is NOT loaded or registered with ASDF: this tool runs in
the cl-mcp parent process, so loading generated code here would escape the
worker isolation boundary. Follow 'next_steps' and call load-system to
register it inside the session's worker.

Intended for creating throwaway sample projects to exercise cl-mcp's tool
surface."
  :args
  ((name :type :string :required t :description
         "Project name in lisp-case (e.g. foo-lib). Must match ^[a-z][a-z0-9-]*$ and be 1-64 chars.")
   (description :type :string :description
                "One-line project description for .asd and README. No newlines, double quotes or
backslashes. Default: \"A Common Lisp project scaffolded by cl-mcp.\"")
   (author :type :string :description
           "Author string for .asd :author. No newlines, double quotes or backslashes. Default:
Unknown.")
   (license :type :string :description
            "License string for .asd :license. No newlines, double quotes or backslashes. Default:
MIT.")
   (destination :type :string :description
                "Relative parent directory under project root where <name>/ is created: non-empty,
no leading /, no .. segment. Default: scaffolds.")
   (framework :type :string :description
              "Test framework the generated tests are written with: 'rove' (default) or
'fiveam'. Selects the .asd :depends-on entry, the test-op hook and the
tests/main-test.lisp template, so run-tests picks the framework up from the
generated project without further configuration.")
   (overwrite :type :boolean :description
              "Replace an unchanged scaffold that this server process generated. Its signed
.cl-mcp-scaffold manifest and every directory entry must be verified."))
  :body
  (handler-case
      (let* ((result-plist
              (write-scaffold
               :name name
               :description (or description "A Common Lisp project scaffolded by cl-mcp.")
               :author (or author "Unknown")
               :license (or license "MIT")
               :destination (or destination "scaffolds")
               :framework framework
               :overwrite overwrite))
             (target-dir (getf result-plist :target-dir))
             (relative (getf result-plist :relative-path))
             (files (getf result-plist :files))
             (framework-name (string-downcase
                              (symbol-name (getf result-plist :framework))))
             (leftover (getf result-plist :leftover-backup))
             ;; NAMESTRING on purpose, against the rule the rest of this file
             ;; follows. This string is not a tool's path argument: it is
             ;; embedded with ~S into (asdf:load-asd ~S) for the caller to run
             ;; through repl-eval, so it is read back by the CL reader and
             ;; handed to LOAD-ASD, which takes a pathname designator.
             ;; Reader-escaping is what survives that trip; the native form
             ;; reads back as a wild pathname. For /tmp/demo[old]/x.asd:
             ;;   namestring -> "...demo\\[old]..." -> /tmp/demo[old]/x.asd
             ;;   native     -> "...demo[old]..."   -> WILD, load-asd fails
             (abs-asd (namestring
                       (merge-pathnames (format nil "~A.asd" name) target-dir)))
             (next-steps
              (vector
               (format nil
                       "The generated system is not registered with ASDF ~
                        yet; run load-system to register it in this ~
                        session's worker (or (asdf:load-asd ~S) via ~
                        repl-eval)"
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
        ;; The generated .asd is deliberately NOT loaded here: this tool runs
        ;; in the cl-mcp parent process, and load-asd evaluates the file, so
        ;; loading it would execute freshly generated code outside the worker
        ;; isolation boundary. Registration happens in the worker instead,
        ;; when the agent follows next_steps and calls load-system.
        (let* ((warning
                (when leftover
                  (format nil
                          "The previous scaffold was preserved at ~A. ~
                           Review and move it before loading because ASDF ~
                           may resolve ~A to the backup."
                          (native-path-namestring leftover) name)))
               (ht (make-ht
                    "created" t
                    "path" relative
                    "absolute_path" (native-path-namestring target-dir)
                    "files" (coerce files 'vector)
                    "framework" framework-name
                    "next_steps" next-steps
                    "content"
                    (text-content
                     (format nil
                             "Scaffolded ~A at ~A (~D files, ~A tests)~%~
                              Path: ~A~%~{~A~%~}~@[~%⚠ ~A~%~]"
                             name relative (length files) framework-name
                             (native-path-namestring target-dir)
                             (coerce next-steps 'list)
                             warning)))))
          (when warning (setf (gethash "warning" ht) warning))
          (result id ht)))
    (invalid-argument-error (e)
      (result id
              (make-ht
               "created" nil
               "error" (princ-to-string e)
               "content" (text-content (princ-to-string e)))))))
