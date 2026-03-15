;;;; src/package-context.lisp
;;;;
;;;; Parent-side package context synthesis for structural file tools.
;;;; Reconstructs enough package metadata from source files to activate
;;;; package-local nicknames while keeping the parent process isolated from
;;;; user system runtime state.

(defpackage #:cl-mcp/src/package-context
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages
                #:call-with-managed-packages)
  (:import-from #:uiop
                #:directory-exists-p
                #:directory-files
                #:ensure-directory-pathname
                #:ensure-pathname
                #:subdirectories)
  (:export #:extract-in-package-name-from-text
           #:discover-package-spec
           #:call-with-package-context
           #:call-with-file-package-context))

(in-package #:cl-mcp/src/package-context)

(defparameter *package-context-source-extensions*
  '("lisp" "lsp" "cl" "asd" "ros")
  "Source file extensions scanned for package definitions.")

(defparameter *package-context-skip-directories*
  '(".git" ".qlot" "qlot" ".cache")
  "Directory basenames skipped during package spec discovery.")

(defstruct package-spec
  "Minimal package metadata needed to reconstruct reader context."
  (name nil :type (or null string))
  (nicknames nil :type list)
  (use nil :type list)
  (local-nicknames nil :type list)
  source-path
  source-form)

(defstruct synthesized-package-context
  "Temporary package objects created for a single reader operation."
  root-package
  (created-packages nil :type list))

(defun %designator-name (designator)
  "Return DESIGNATOR normalized to an uppercase package-name string."
  (typecase designator
    (null nil)
    (string (string-upcase designator))
    (symbol (string-upcase (symbol-name designator)))
    (t nil)))

(defun %definition-head-p (head)
  "Return T when HEAD names a package definition form we can interpret."
  (and (symbolp head)
       (member (symbol-name head) '("DEFPACKAGE" "DEFINE-PACKAGE")
               :test #'string=)))

(defun %in-package-head-p (head)
  "Return T when HEAD names an IN-PACKAGE form."
  (and (symbolp head)
       (string= (symbol-name head) "IN-PACKAGE")))

(defun %header-form-p (form)
  "Return T when FORM is part of the package header section."
  (and (consp form)
       (let ((head (car form)))
         (or (%definition-head-p head)
             (%in-package-head-p head)))))

(defun %source-file-p (pathname)
  "Return T when PATHNAME looks like a Lisp source file worth scanning."
  (let ((type (pathname-type pathname)))
    (and type
         (member (string-downcase type) *package-context-source-extensions*
                 :test #'string=))))

(defun %directory-basename (pathname)
  "Return the last directory component of PATHNAME, or NIL."
  (let ((dir (pathname-directory (ensure-directory-pathname pathname))))
    (when (consp dir)
      (let ((last (car (last dir))))
        (when (stringp last)
          last)))))

(defun %skip-directory-p (pathname)
  "Return T when PATHNAME should be skipped during recursive scans."
  (let ((base (%directory-basename pathname)))
    (and base
         (member base *package-context-skip-directories* :test #'string=))))

(defun %collect-source-files (root)
  "Return a list of source files under ROOT, recursively."
  (labels ((walk (dir)
             (let ((results nil))
               (dolist (entry (directory-files dir))
                 (when (%source-file-p entry)
                   (push entry results)))
               (dolist (subdir (subdirectories dir))
                 (unless (%skip-directory-p subdir)
                   (setf results
                         (nconc results
                                (walk (ensure-directory-pathname subdir))))))
               results)))
    (walk (ensure-directory-pathname root))))

(defun %read-header-forms-from-text (text)
  "Read package-related header forms from TEXT using the CL reader.
Stops after the first non-header top-level form once package header forms
have begun, to avoid descending into the rest of the file."
  (call-with-lenient-packages
   (lambda ()
     (let ((*read-eval* nil))
       (with-input-from-string (stream text)
         (let ((forms nil)
               (saw-header nil))
           (loop for form = (read stream nil :eof)
                 until (eq form :eof)
                 do (cond
                      ((%header-form-p form)
                       (push form forms)
                       (setf saw-header t))
                      (saw-header
                       (return))))
           (nreverse forms)))))))

(defun %extract-in-package-name-from-forms (forms)
  "Return the first package name mentioned in FORMS' IN-PACKAGE declaration."
  (loop for form in forms
        when (and (consp form)
                  (%in-package-head-p (car form))
                  (consp (cdr form)))
          do (return (%designator-name (second form)))))

(defun extract-in-package-name-from-text (text)
  "Return the package named by the first IN-PACKAGE form in TEXT, or NIL."
  (%extract-in-package-name-from-forms (%read-header-forms-from-text text)))

(defun %normalize-local-nickname-entries (entries)
  "Convert local nickname ENTRIES into an alist of (nickname . package-name)."
  (let ((items (if (and (= (length entries) 1)
                        (listp (first entries))
                        (every #'listp (first entries)))
                   (first entries)
                   entries))
        (result nil))
    (dolist (entry items (nreverse result))
      (when (and (consp entry)
                 (consp (cdr entry)))
        (let ((nickname (%designator-name (first entry)))
              (target (%designator-name (second entry))))
          (when (and nickname target)
            (push (cons nickname target) result)))))))

(defun %extract-package-spec (form source-path)
  "Extract a PACKAGE-SPEC from package definition FORM, or NIL."
  (when (and (consp form)
             (%definition-head-p (car form))
             (consp (cdr form)))
    (let ((name (%designator-name (second form)))
          (nicknames nil)
          (use nil)
          (local-nicknames nil))
      (dolist (clause (cddr form))
        (when (and (consp clause) (symbolp (car clause)))
          (let ((key (symbol-name (car clause))))
            (cond
              ((string= key "NICKNAMES")
               (setf nicknames
                     (remove nil (mapcar #'%designator-name (cdr clause)))))
              ((string= key "USE")
               (setf use
                     (remove nil (mapcar #'%designator-name (cdr clause)))))
              ((string= key "LOCAL-NICKNAMES")
               (setf local-nicknames
                     (%normalize-local-nickname-entries (cdr clause))))))))
      (when name
        (make-package-spec
         :name name
         :nicknames nicknames
         :use use
         :local-nicknames local-nicknames
         :source-path source-path
         :source-form form)))))

(defun %package-spec-matches-p (spec package-name)
  "Return T when SPEC defines PACKAGE-NAME or one of its nicknames."
  (let ((target (%designator-name package-name)))
    (and target
         (or (string= (package-spec-name spec) target)
             (member target (package-spec-nicknames spec) :test #'string=)))))

(defun %package-specs-in-file (pathname)
  "Return a list of PACKAGE-SPEC values extracted from PATHNAME."
  (handler-case
      (let* ((text (fs-read-file pathname))
             (forms (%read-header-forms-from-text text))
             (specs nil))
        (dolist (form forms (nreverse specs))
          (let ((spec (%extract-package-spec form pathname)))
            (when spec
              (push spec specs)))))
    (reader-error ()
      nil)
    #+sbcl
    (sb-int:simple-reader-package-error ()
      nil)))

(defun discover-package-spec (package-name &key source-path)
  "Discover a PACKAGE-SPEC for PACKAGE-NAME from project source files."
  (let ((target (%designator-name package-name)))
    (when target
      (when source-path
        (let ((source (ensure-pathname source-path :want-pathname t)))
          (when (probe-file source)
            (let ((match
                    (find-if (lambda (spec)
                               (%package-spec-matches-p spec target))
                             (%package-specs-in-file source))))
              (when match
                (return-from discover-package-spec match))))))
      (let ((root (or *project-root*
                      (and source-path
                           (make-pathname :name nil :type nil
                                          :defaults (ensure-pathname source-path
                                                                     :want-pathname t))))))
        (when root
          (dolist (file (%collect-source-files root))
            (unless (and source-path
                         (equal (probe-file file)
                                (probe-file (ensure-pathname source-path
                                                             :want-pathname t))))
              (let ((match
                      (find-if (lambda (spec)
                                 (%package-spec-matches-p spec target))
                               (%package-specs-in-file file))))
                (when match
                  (return-from discover-package-spec match))))))))))

(defun %make-package-with-guard (name &key nicknames use)
  "Create package NAME with optional NICKNAMES and USE list."
  (handler-case
      (make-package name :use use :nicknames nicknames)
    (error (e)
      (error "Failed to synthesize package ~A in parent process: ~A"
             name e))))

(defun %ensure-package-object (name created-packages-cell)
  "Return package NAME, creating a temporary stub if needed."
  (or (find-package name)
      (let ((pkg (%make-package-with-guard name :use nil)))
        (push pkg (car created-packages-cell))
        pkg)))

(defun %safe-use-list (package-names)
  "Return USE entries whose packages already exist in the parent."
  (loop for name in package-names
        for pkg = (find-package name)
        when pkg
          collect (package-name pkg)))

(defun %synthesize-package-context (spec)
  "Create temporary packages needed to realize SPEC."
  (let ((created-packages-cell (list nil)))
    (let ((root
            (or (find-package (package-spec-name spec))
                (progn
                  (dolist (entry (package-spec-local-nicknames spec))
                    (%ensure-package-object (cdr entry) created-packages-cell))
                  (let ((pkg (%make-package-with-guard
                              (package-spec-name spec)
                              :nicknames (package-spec-nicknames spec)
                              :use (%safe-use-list (package-spec-use spec)))))
                    (push pkg (car created-packages-cell))
                    #+sbcl
                    (dolist (entry (package-spec-local-nicknames spec))
                      (sb-ext:add-package-local-nickname
                       (car entry)
                       (%ensure-package-object (cdr entry) created-packages-cell)
                       pkg))
                    #-sbcl
                    (when (package-spec-local-nicknames spec)
                      (error "Package-local nickname synthesis requires SBCL"))
                    pkg)))))
      (make-synthesized-package-context
       :root-package root
       :created-packages (nreverse (car created-packages-cell))))))

(defun %cleanup-synthesized-package-context (context)
  "Delete temporary packages created for CONTEXT."
  (dolist (pkg (reverse (synthesized-package-context-created-packages context)))
    (let ((name (ignore-errors (package-name pkg))))
      (when (and name (find-package name))
        (ignore-errors (delete-package pkg))))))

(defun call-with-package-context (package-name thunk &key source-path)
  "Call THUNK with PACKAGE-NAME available as the current package context.
If PACKAGE-NAME is absent in the parent image, attempt to reconstruct it from
project source and bind *PACKAGE* to the synthesized package while THUNK runs."
  (let ((name (%designator-name package-name)))
    (cond
      ((null name)
       (call-with-lenient-packages thunk))
      ((find-package name)
       (let ((*package* (find-package name)))
         (call-with-lenient-packages thunk)))
      (t
       (let ((spec (discover-package-spec name :source-path source-path)))
         (if (null spec)
             (call-with-lenient-packages thunk)
             (let ((context (%synthesize-package-context spec)))
               (unwind-protect
                    (let ((*package* (synthesized-package-context-root-package context)))
                      (call-with-managed-packages
                       (synthesized-package-context-created-packages context)
                       thunk))
                 (%cleanup-synthesized-package-context context)))))))))

(defun call-with-file-package-context (file-path thunk &key text)
  "Call THUNK with the package context implied by FILE-PATH's IN-PACKAGE form."
  (let ((file-text
          (or text
              (fs-read-file file-path))))
    (call-with-package-context
     (extract-in-package-name-from-text file-text)
     thunk
     :source-path file-path)))
