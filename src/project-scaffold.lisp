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
                #:*scaffold-marker-file*
                #:scaffold-marker-content)
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
  "Return a short pseudo-random suffix for scratch directory naming."
  (format nil "~8,'0X" (random #xFFFFFFFF)))

(defun %absolute-scaffold-paths (root destination name)
  "Return (values TARGET-DIR TEMP-DIR BACKUP-DIR) as absolute directory pathnames.
All three paths are inside ROOT. TEMP-DIR holds the freshly generated
files before they are moved into place; BACKUP-DIR receives the previous
TARGET-DIR contents during an overwrite so the replacement is a pair of
renames rather than a recursive delete. Both share one random suffix so
that leftovers from a crashed run are traceable to the same call, and so
that concurrent calls never collide."
  (let* ((suffix (%uuid-suffix))
         (dest-dir (uiop:ensure-directory-pathname
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
                      (format nil ".tmp-project-scaffold-~A" suffix))
                     dest-dir)))
         (backup-dir (uiop:ensure-directory-pathname
                      (merge-pathnames
                       (uiop:ensure-directory-pathname
                        (format nil ".bak-project-scaffold-~A" suffix))
                       dest-dir))))
    (values target-dir temp-dir backup-dir)))

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

(defun %write-files-to-temp (temp-dir name plan)
  "Write the ownership marker and all PLAN entries into TEMP-DIR.
TEMP-DIR must already be inside *PROJECT-ROOT*; the caller is
responsible for that containment check. Using FS-WRITE-FILE keeps the
file-write path inside the same sandbox wrapper as the other MCP file
tools, so scaffold writes go through ENSURE-WRITE-PATH validation and
atomic write-to-temp-then-rename for each file. The marker is written
first so that a temp directory abandoned by a mid-way failure is still
recognizable as cl-mcp-owned and therefore still deletable by
%DELETE-SCAFFOLD-TREE. Caller cleans up TEMP-DIR if any intermediate
write fails."
  (ensure-directories-exist temp-dir)
  (let ((temp-relative (enough-namestring temp-dir *project-root*)))
    (fs-write-file (concatenate 'string temp-relative *scaffold-marker-file*)
                   (scaffold-marker-content name (mapcar #'car plan)))
    (dolist (entry plan)
      (let ((rel (car entry))
            (content (cdr entry)))
        (fs-write-file (concatenate 'string temp-relative rel) content)))))

(defun %scaffold-owned-p (directory)
  "Return T when DIRECTORY carries the cl-mcp scaffold ownership marker.
Only directories this tool generated hold the marker, so it is what
separates a replaceable scaffold from an arbitrary directory that merely
happens to sit at the requested path."
  (and (uiop:directory-exists-p directory)
       (probe-file (merge-pathnames *scaffold-marker-file* directory))
       t))

(defparameter *scaffold-scratch-prefixes*
  '(".tmp-project-scaffold-" ".bak-project-scaffold-")
  "Directory-name prefixes reserved for this tool's own scratch directories.
Every such directory is created by %ABSOLUTE-SCAFFOLD-PATHS with a fresh
random suffix, so a caller can never steer the tool at a pre-existing one.
They count as deletable even without the ownership marker, which is what
lets a run that died before the marker was written still clean up.")

(defun %scaffold-scratch-dir-p (directory)
  "Return T when DIRECTORY's last segment uses a reserved scratch prefix.
See *SCAFFOLD-SCRATCH-PREFIXES* for why these are safe to delete."
  (let ((last-segment (car (last (pathname-directory
                                  (uiop:ensure-directory-pathname directory))))))
    (and (stringp last-segment)
         (some (lambda (prefix) (uiop:string-prefix-p prefix last-segment))
               *scaffold-scratch-prefixes*)
         t)))

(defun %delete-scaffold-tree (directory)
  "Recursively delete DIRECTORY, but only if this tool owns it.
UIOP's :VALIDATE T is a validator that always answers yes, which turns
DELETE-DIRECTORY-TREE into an unguarded rm -rf. This passes a real
predicate instead: the directory must truename inside *PROJECT-ROOT*,
must not be *PROJECT-ROOT* itself, and must either carry the ownership
marker or be one of this tool's reserved scratch directories. Signals
INVALID-ARGUMENT-ERROR rather than deleting when any of those fail."
  (let ((resolved-root
         (or (ignore-errors
              (uiop/pathname:ensure-directory-pathname
               (truename *project-root*)))
             *project-root*)))
    (flet ((safe-to-delete-p (dir)
             (let ((resolved (ignore-errors
                              (uiop/pathname:ensure-directory-pathname
                               (truename dir)))))
               (and resolved
                    (path-inside-p resolved resolved-root)
                    (not (equal resolved resolved-root))
                    (or (%scaffold-owned-p resolved)
                        (%scaffold-scratch-dir-p resolved))
                    t))))
      (unless (safe-to-delete-p directory)
        (error 'invalid-argument-error
               :field "overwrite"
               :value (namestring directory)
               :reason (format nil "refusing to delete ~A: not a cl-mcp-generated scaffold"
                               (namestring directory))))
      (uiop:delete-directory-tree directory :validate #'safe-to-delete-p))))

(defun write-scaffold (&key name description author license destination overwrite
                            framework)
  "Generate the scaffold project atomically. Returns a plist with:
  :target-dir (absolute pathname)
  :relative-path (namestring relative to *project-root*)
  :files (list of relative path strings, in manifest order)
  :framework (keyword) -- the test framework the project was generated
    for, resolved from the FRAMEWORK designator
  :leftover-backup (pathname, or NIL) -- set when the replaced tree could
    not be removed after the commit.  The caller must surface it: the
    leftover carries the same .asd, and ASDF's tree scan reaches
    dot-directories, so FIND-SYSTEM can resolve the system to the stale
    copy.  The generation itself succeeded, so this is a warning rather
    than an error.

OVERWRITE only ever replaces a directory this tool generated, proven by
the ownership marker; anything else is refused so that a name colliding
with a real source directory cannot delete it. The replacement itself is
done by renaming the old tree aside and only deleting the backup once the
new tree is in place, so an interrupted overwrite is recoverable.

On any failure, signals INVALID-ARGUMENT-ERROR or propagates the
underlying error after cleaning up the temp directory."
  (ensure-project-root)
  (validate-project-name name)
  (validate-destination destination)
  (validate-text-field "description" (or description ""))
  (validate-text-field "author" (or author ""))
  (validate-text-field "license" (or license ""))
  ;; Normalizes as well as validates: everything downstream -- the template
  ;; selection and the tool's response field -- wants the resolved keyword
  ;; rather than the designator the caller passed.
  (setf framework (validate-framework framework))
  (multiple-value-bind (target-dir temp-dir backup-dir)
      (%absolute-scaffold-paths *project-root* destination name)
    (%assert-within-project-root target-dir)
    (%assert-within-project-root temp-dir)
    (%assert-within-project-root backup-dir)
    (let ((target-exists (and (uiop:directory-exists-p target-dir) t)))
      (when (and target-exists (not overwrite))
        (error 'invalid-argument-error
               :field "name" :value name
               :reason (format nil "target directory already exists: ~A"
                               (namestring target-dir))))
      (when (and target-exists (not (%scaffold-owned-p target-dir)))
        (error 'invalid-argument-error
               :field "overwrite" :value name
               :reason (format nil "refusing to overwrite ~A: not a cl-mcp-generated ~
                                    scaffold; delete it manually"
                               (namestring target-dir))))
      (let ((plan (plan-scaffold :name name
                                 :description (or description "")
                                 :author (or author "")
                                 :license (or license "")
                                 :destination destination
                                 :framework framework))
            (moved-aside nil)
            (committed nil)
            (leftover-backup nil))
        (unwind-protect
             (progn
               (%write-files-to-temp temp-dir name plan)
               ;; Move the existing target aside AFTER temp is ready, preserving
               ;; atomicity: if %write-files-to-temp fails, the original
               ;; scaffold survives untouched.
               (when target-exists
                 (rename-file target-dir backup-dir)
                 (setf moved-aside t))
               (rename-file temp-dir target-dir)
               (setf committed t)
               ;; The scaffold is already in place, so a failure to remove the
               ;; backup must not turn a successful generation into a reported
               ;; error -- the unwind cleanup below cannot undo the commit
               ;; either.  But the leftover is NOT inert: it is a complete
               ;; copy carrying the same .asd, and ASDF's tree scan walks
               ;; dot-directories, so FIND-SYSTEM can resolve the system to the
               ;; stale copy instead of the new one.  Report it rather than
               ;; returning a bare success the caller cannot act on.
               (when moved-aside
                 (unless (ignore-errors (%delete-scaffold-tree backup-dir) t)
                   (setf leftover-backup backup-dir)))
               (list :target-dir target-dir
                     :relative-path (enough-namestring target-dir *project-root*)
                     :files (mapcar #'car plan)
                     :framework framework
                     :leftover-backup leftover-backup))
          (unless committed
            (when (uiop:directory-exists-p temp-dir)
              (ignore-errors (%delete-scaffold-tree temp-dir)))
            ;; Put the original tree back if it was renamed aside but the
            ;; commit never happened.
            (when (and moved-aside (not (uiop:directory-exists-p target-dir)))
              (ignore-errors (rename-file backup-dir target-dir)))))))))

(define-tool "project-scaffold"
  :description
  "Generate a minimal Common Lisp project skeleton under the project root.

The generated project uses package-inferred-system and ships with
CLAUDE.md/AGENTS.md templates referencing cl-mcp's existing prompts via
relative @-include paths. Its test suite is written with Rove by default;
pass 'framework' to get a FiveAM project instead. Either way run-tests
detects the framework from the generated .asd, so no extra wiring is needed.

On success, returns the list of created files, the resolved framework, and
a 'next_steps' array with concrete REPL commands the agent can invoke to
register the project with ASDF and run its tests.

Fails if the target directory already exists, unless 'overwrite' is true AND
that directory was itself generated by this tool (it carries a
.cl-mcp-scaffold marker file). A directory cl-mcp did not generate is never
deleted, so pointing this tool at an existing source directory is refused
rather than destructive; choose a unique 'name' per generation.

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
                "One-line project description for .asd and README. No newlines.")
   (author :type :string :description
           "Author string for .asd :author. No newlines.")
   (license :type :string :description
            "License string for .asd :license. No newlines.")
   (destination :type :string :description
                "Relative parent directory under project root where <name>/ is created. Default: scaffolds.")
   (framework :type :string :description
              "Test framework the generated tests are written with: 'rove' (default) or
'fiveam'. Selects the .asd :depends-on entry, the test-op hook and the
tests/main-test.lisp template, so run-tests picks the framework up from the
generated project without further configuration.")
   (overwrite :type :boolean :description
              "When true, replace an existing directory instead of failing -- but only
if cl-mcp itself generated that directory (it must contain a .cl-mcp-scaffold marker
file). Any other existing directory is refused, never deleted."))
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
                          "The replaced scaffold could not be removed and ~
                           remains at ~A.  It carries the same .asd, and ~
                           ASDF's tree scan reaches it, so FIND-SYSTEM may ~
                           resolve ~A to that stale copy -- delete it before ~
                           loading."
                          (namestring leftover) name)))
               (ht (make-ht
                    "created" t
                    "path" relative
                    "absolute_path" (namestring target-dir)
                    "files" (coerce files 'vector)
                    "framework" framework-name
                    "next_steps" next-steps
                    "content"
                    (text-content
                     (format nil
                             "Scaffolded ~A at ~A (~D files, ~A tests)~%~
                              Path: ~A~%~{~A~%~}~@[~%⚠ ~A~%~]"
                             name relative (length files) framework-name
                             (namestring target-dir)
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
