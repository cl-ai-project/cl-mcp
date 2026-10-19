;;;; src/utils/fasls.lisp
;;;;
;;;; Where a system's cached fasls live, and which of them cannot be trusted.
;;;; Shared by load-system's clear_fasls and run-tests' reload, which depend on
;;;; nothing of each other.

(defpackage #:cl-mcp/src/utils/fasls
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:collect-target-files)
  (:import-from #:cl-mcp/src/utils/project-files
                #:project-ignored-predicate)
  (:export #:fasl-source-directory
           #:delete-same-second-fasls
           #:registered-source-fasl-pairs
           #:registered-system-files
           #:registered-fasl-sources))

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

(defun %project-systems (system-name)
  "Return the registered systems of SYSTEM-NAME's project: its primary system and
every system named after it, PRIMARY/..., the package-inferred subsystems among
them.  Only systems already registered: one never loaded has no fasl to doubt."
  (let ((primary (asdf:primary-system-name system-name)))
    (loop for name in (asdf:registered-systems)
          for system = (and (string= primary (asdf:primary-system-name name))
                            (asdf:registered-system name))
          when system collect system)))

(defun %source-file-components (component)
  "Return the CL source file components in COMPONENT's tree."
  (cond ((typep component 'asdf:cl-source-file) (list component))
        ((typep component 'asdf:parent-component)
         (loop for child in (asdf:component-children component)
               append (%source-file-components child)))))

(defun %compiled-fasl (component)
  "Return the fasl ASDF compiles COMPONENT, a source file, into, or NIL."
  (find "fasl" (ignore-errors (asdf:output-files 'asdf:compile-op component))
        :key #'pathname-type :test #'equal))

(defun registered-source-fasl-pairs (system-name)
  "Return (SOURCE . FASL) for each source file component of SYSTEM-NAME's
project's registered systems (%PROJECT-SYSTEMS), which name a source of any
extension -- (:file \"main\" :type \"cl\") -- with the fasl the output
translations give it.  No directory is walked, so it is cheap; after a load
every subsystem the load reached is registered."
  (let ((pairs '()))
    (dolist (system (%project-systems system-name) pairs)
      (dolist (file (%source-file-components system))
        (let ((source (asdf:component-pathname file))
              (fasl (%compiled-fasl file)))
          (when (and source fasl)
            (push (cons source fasl) pairs)))))))

(defun registered-system-files (system-name)
  "Return the .asd files that define SYSTEM-NAME's project's registered systems
(%PROJECT-SYSTEMS), each once.  A warning signalled while one of them loads, such
as ASDF's about a system named against its conventions, is the project's."
  (remove-duplicates
   (remove nil (mapcar (lambda (system) (ignore-errors (asdf:system-source-file system)))
                       (%project-systems system-name)))
   :test #'equal :key #'namestring))

(defun registered-fasl-sources ()
  "Return an EQUAL table from the namestring of the FASL each source file
component of every registered system compiles to, to that source.  It tells
which file a FASL being loaded was compiled from, which the output translations
cannot be asked in reverse."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (name (asdf:registered-systems) table)
      (let ((system (ignore-errors (asdf:registered-system name))))
        (when system
          (dolist (file (%source-file-components system))
            (let ((source (asdf:component-pathname file))
                  (fasl (%compiled-fasl file)))
              (when (and source fasl)
                (setf (gethash (namestring fasl) table) source)))))))))

(defun %source-fasl-pairs (system-name &key (ignored-p (project-ignored-predicate)))
  "Return (SOURCE . FASL) for each source file of SYSTEM-NAME's project, from two
places, since neither alone sees every file: REGISTERED-SOURCE-FASL-PAIRS, and
every .lisp file under the project's source directory (FASL-SOURCE-DIRECTORY),
which reaches a package-inferred subsystem this worker has not registered or has
just cleared.  The walk leaves out what the directory's .gitignore does
(COLLECT-TARGET-FILES) and what IGNORED-P, by default the project root's
.gitignore (PROJECT-IGNORED-PREDICATE), does: a dependency vendored there is not
the project's, and walking it cost seconds on every load of a large project.
When the directory itself is left out -- the system being loaded is such a
dependency -- IGNORED-P would leave out everything, so it is not applied."
  (let* ((pairs (registered-source-fasl-pairs system-name))
         (source-dir (fasl-source-directory system-name))
         (filter-p (and source-dir (not (funcall ignored-p source-dir)))))
    (when source-dir
      (dolist (source (ignore-errors (collect-target-files source-dir)))
        (when (and (equal "lisp" (pathname-type source))
                   (not (and filter-p (funcall ignored-p source))))
          (let ((fasl (ignore-errors
                       (asdf:apply-output-translations (compile-file-pathname source)))))
            (when fasl
              (push (cons source fasl) pairs))))))
    (remove-duplicates pairs :key (lambda (pair) (namestring (cdr pair))) :test #'string=)))

(defun %forget-file-stamp (file)
  "Tell an ASDF session in progress that FILE no longer exists.  A session caches
the stamps of the files it has looked at, so one taken before FILE was deleted
would have ASDF load FILE rather than compile its source again -- which a load
run inside another ASDF operation, such as a test-op, does.  Outside a session
this does nothing."
  (let ((register (uiop:find-symbol* '#:register-file-stamp '#:asdf/session nil)))
    (when (and register (fboundp register))
      (ignore-errors (funcall register file nil)))))

(defun delete-same-second-fasls (system-name)
  "Delete the fasl of each source file of SYSTEM-NAME's project (%SOURCE-FASL-PAIRS)
whose source was written in the same second as the fasl, or later.  Return the
number deleted of the first kind, written in the same second.  Call it before
clearing the project's systems from ASDF: the registered components are half of
what it reads.

ASDF judges a fasl current when its FILE-WRITE-DATE is not older than its
source's, and those dates are whole seconds: an edit landing in the second the
fasl was written leaves the two equal, so ASDF keeps the fasl and loads the code
from before the edit.  Such a fasl cannot be told from a current one, so it is
deleted and ASDF compiles the file again; one written a second or more after
its source is kept, so a run recompiles only what may be stale, unlike
clear_fasls, which drops them all.  A source written later than its fasl is
one ASDF recompiles by itself -- unless an ASDF session in progress still has
the source's older date -- so its fasl goes too, but is not counted: the count
is of the fasls ASDF would have kept."
  (let ((deleted 0))
    (loop for (source . fasl) in (%source-fasl-pairs system-name)
          for fasl-date = (ignore-errors (file-write-date fasl))
          for source-date = (ignore-errors (file-write-date source))
          when (and fasl-date source-date (>= source-date fasl-date)
                    (ignore-errors (delete-file fasl) t))
            do (%forget-file-stamp fasl)
               (when (= source-date fasl-date)
                 (incf deleted)))
    deleted))
