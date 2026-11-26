;;;; src/utils/project-files.lisp

(defpackage #:cl-mcp/src/utils/project-files
  (:use #:cl)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:path-inside-p)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:root-ignore-patterns
                #:path-ignored-p)
  (:export #:project-file-predicate
           #:project-ignored-predicate))

(in-package #:cl-mcp/src/utils/project-files)

(defun %absolute-path (designator)
  "Return DESIGNATOR, a pathname or namestring, as an absolute pathname, or NIL."
  (let ((path (and designator (ignore-errors (pathname designator)))))
    (and path (uiop:absolute-pathname-p path) path)))

(defun project-ignored-predicate (&optional (root *project-root*))
  "Return a function of a pathname that is true when the file is under ROOT but
left out by ROOT's .gitignore or .git/ -- the rules the source scan collects
files by (COLLECT-TARGET-FILES), so the two agree.  A dependency qlot vendors
under the root is such a file.  With no ROOT nothing is left out.

The .gitignore is read once, here, so a caller testing many files makes one
predicate and keeps it."
  (if (null root)
      (constantly nil)
      (let* ((root-dir (uiop:ensure-directory-pathname root))
             (patterns (ignore-errors (root-ignore-patterns root-dir))))
        (lambda (pathname)
          (let ((path (%absolute-path pathname)))
            (and path
                 (path-inside-p path root-dir)
                 (path-ignored-p path root-dir patterns)
                 t))))))

(defun project-file-predicate (&optional (root *project-root*))
  "Return a function of a pathname that is true when the file is the project's
own: under ROOT and not left out by its .gitignore (PROJECT-IGNORED-PREDICATE).
With no ROOT every file is the project's."
  (if (null root)
      (lambda (pathname) (and pathname t))
      (let ((root-dir (uiop:ensure-directory-pathname root))
            (ignored-p (project-ignored-predicate root)))
        (lambda (pathname)
          (let ((path (%absolute-path pathname)))
            (and path
                 (path-inside-p path root-dir)
                 (not (member ".qlot" (pathname-directory (enough-namestring path root-dir))
                              :test #'equal))
                 (not (funcall ignored-p path))))))))
