;;;; src/code.lisp

(defpackage #:cl-mcp/src/code
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/fs #:*project-root*)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:read-file-string
                #:getcwd
                #:ensure-pathname)
  (:export
   #:code-find-definition
   #:code-describe-symbol))

(in-package #:cl-mcp/src/code)

(defun %ensure-package (package)
  "Resolve PACKAGE designator to a package object.
Signals an error when the package does not exist."
  (cond
    ((null package) *package*)
    ((and (stringp package) (string= package ""))
     *package*)
    ((packagep package) package)
    ((symbolp package)
     (or (find-package package)
         (error "Package ~S does not exist" package)))
    ((stringp package)
     (or (find-package package)
         (error "Package ~A does not exist" package)))
    (t (error "Invalid package designator ~S" package))))

(defun %parse-symbol (symbol-name &key package)
  "Read SYMBOL-NAME as a symbol without permitting evaluation.
PACKAGE is used only when SYMBOL-NAME is unqualified; when a package marker
appears in SYMBOL-NAME (e.g., \"pkg:sym\"), PACKAGE is ignored."
  (unless (stringp symbol-name)
    (error "symbol must be a string"))
  (let* ((qualified-p (position #\: symbol-name))
         (*package* (if qualified-p
                        *package*
                        (handler-case
                            (%ensure-package package)
                          (error () *package*))))
         (*readtable* (copy-readtable nil))
         (*read-eval* nil))
    (multiple-value-bind (obj end) (read-from-string symbol-name nil :eof)
      (declare (ignore end))
      (when (eq obj :eof)
        (error "Symbol name ~S is empty" symbol-name))
      (unless (symbolp obj)
        (error "~S is not a symbol name" symbol-name))
      obj)))

(defun %ensure-sb-introspect ()
  "Load and return the SB-INTROSPECT package when available."
  #+sbcl
  (or (find-package :sb-introspect)
      (ignore-errors
        (require :sb-introspect)
        (find-package :sb-introspect)))
  #-sbcl
  nil)

(defun %sb-introspect-symbol (name)
  "Return symbol NAME from SB-INTROSPECT package or NIL."
  (let ((pkg (%ensure-sb-introspect)))
    (and pkg (find-symbol name pkg))))

(defun %offset->line (pathname offset)
  "Convert character OFFSET within PATHNAME to a 1-based line number.
Returns NIL when the file cannot be read."
  (when (and pathname offset)
    (handler-case
        (let* ((content (uiop:read-file-string pathname))
               (end (min (max offset 0) (length content))))
          (1+ (count #\Newline content :end end)))
      (error (e)
        (log-event :warn "code.find.line-error"
                   "path" (uiop:native-namestring pathname)
                   "error" (princ-to-string e))
        nil))))

(defun %normalize-path (pathname)
  "Return a namestring, relative to *project-root* when possible.
Falls back to CWD, then cl-mcp system source directory, else absolute."
  (when pathname
    (let* ((pn (uiop:ensure-pathname pathname))
           (bases (remove nil
                          (list *project-root*
                                (uiop:getcwd)
                                (ignore-errors (asdf:system-source-directory :cl-mcp))))))
      (dolist (base bases (uiop:native-namestring pn))
        (when (uiop:subpathp pn base)
          (return (uiop:native-namestring (uiop:enough-pathname pn base))))))))

(declaim (ftype (function (string &key (:package (or null package symbol string)))
                          (values (or null string) (or null integer) &optional))
                code-find-definition))
(defun code-find-definition (symbol-name &key package)
  "Return the definition location for SYMBOL-NAME.
Values are PATH (string) and LINE (integer), or NILs when not found."
  (let* ((qualified (position #\: symbol-name))
         (pkg (if qualified nil package))
         (sym (%parse-symbol symbol-name :package pkg)))
    #+sbcl
    (let* ((pkg (%ensure-sb-introspect))
           (find-by-name (and pkg (find-symbol "FIND-DEFINITION-SOURCES-BY-NAME" pkg)))
           (find (and pkg (find-symbol "FIND-DEFINITION-SOURCE" pkg)))
           (path-fn (and pkg (find-symbol "DEFINITION-SOURCE-PATHNAME" pkg)))
           (offset (and pkg (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" pkg)))
           (source (or (and find-by-name
                            (first (ignore-errors (funcall find-by-name sym :function))))
                        (and find (ignore-errors (funcall find sym))))))
      (when (and source path-fn)
        (let* ((pathname (funcall path-fn source))
               (char-offset (and offset (funcall offset source)))
               (line (%offset->line pathname char-offset))
               (path (%normalize-path pathname)))
          (return-from code-find-definition (values path line))))
      (log-event :warn "code.find.not-found" "symbol" symbol-name)
      (values nil nil))
    #-sbcl
    (error "code-find-definition requires SBCL")))

(declaim (ftype (function (string &key (:package (or null package symbol string)))
                          (values string string (or null string) (or null string) &optional))
                code-describe-symbol))
(defun code-describe-symbol (symbol-name &key package)
  "Return NAME, TYPE, ARGLIST string and DOCUMENTATION for SYMBOL-NAME.
Signals an error when the symbol is unbound."
  (let* ((sym (%parse-symbol symbol-name :package package))
         (name (princ-to-string sym))
         (type (cond
                 ((macro-function sym) "macro")
                 ((fboundp sym) "function")
                 ((boundp sym) "variable")
                 (t "unbound"))))
    (when (string= type "unbound")
      (error "Symbol ~A is not bound as a function or variable" sym))
    #+sbcl
    (%ensure-sb-introspect)
    (let* ((fn (cond
                 ((macro-function sym))
                 ((fboundp sym) (symbol-function sym))
                 (t nil)))
           (arglist (when fn
                      (handler-case
                          (let* ((fn-ll (%sb-introspect-symbol "FUNCTION-LAMBDA-LIST"))
                                 (args (and fn-ll (funcall fn-ll fn))))
                            (cond
                              ((null args) "()")
                              ((listp args) (princ-to-string args))
                              (t (princ-to-string args))))
                        (error (e)
                          (log-event :warn "code.describe.arglist-error" "symbol" symbol-name
                                     "error" (princ-to-string e))
                          "()"))))
           (doc (cond
                  ((or (macro-function sym) (fboundp sym))
                   (documentation sym 'function))
                  ((boundp sym) (documentation sym 'variable))
                  (t nil))))
      (values name type arglist doc))))
