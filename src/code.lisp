;;;; src/code.lisp

(defpackage #:cl-mcp/src/code
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/project-root #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:normalize-path-for-display
                #:path-inside-p)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:uiop
                #:read-file-string
                #:ensure-pathname
                #:ensure-directory-pathname
                #:absolute-pathname-p)
  (:export
   #:code-find-definition
   #:code-describe-symbol
   #:code-find-references))

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
Delegates to normalize-path-for-display from paths module."
  (normalize-path-for-display pathname))

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
                          (values string string (or null string) (or null string)
                                  (or null string) (or null integer) &optional))
                code-describe-symbol))
(defun code-describe-symbol (symbol-name &key package)
  "Return NAME, TYPE, ARGLIST, DOCUMENTATION, PATH, and LINE for SYMBOL-NAME.
Signals an error when the symbol is unbound. PATH/LINE may be NIL when unknown."
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
      (multiple-value-bind (path line)
          (code-find-definition symbol-name :package package)
        (values name type arglist doc path line)))))

(defun %path-inside-project-p (pathname)
  "Return T when PATHNAME is inside *project-root*.
Returns T for NIL or relative paths when *project-root* is not set."
  (and pathname
       (if *project-root*
           (if (uiop:absolute-pathname-p pathname)
               (path-inside-p (uiop:ensure-pathname pathname :want-relative nil)
                              (uiop:ensure-directory-pathname *project-root*))
               t)
           t)))

(defun %line-snippet (pathname line)
  "Return LINE text (1-based) from PATHNAME, or NIL when unavailable."
  (when (and pathname line (> line 0))
    (handler-case
        (with-open-file (in pathname :direction :input :element-type 'character)
          (loop for idx from 1
                for l = (read-line in nil :eof)
                until (eq l :eof)
                do (when (= idx line) (return l))))
      (file-error () nil))))

(defun %definition->path/line (source path-fn offset-fn)
  "Return PATH and LINE for an SB-INTROSPECT definition SOURCE."
  (let* ((pathname (and path-fn (funcall path-fn source)))
         (char-offset (and offset-fn (funcall offset-fn source)))
         (line (%offset->line pathname char-offset))
         (path (%normalize-path pathname)))
    (values pathname path (or line (and pathname char-offset 1)))))

(defun %finder->type (name)
  "Map SB-INTROSPECT XREF function name to output type."
  (cond
    ((string= name "WHO-CALLS") "call")
    ((string= name "WHO-MACROEXPANDS") "macro")
    ((string= name "WHO-BINDS") "bind")
    ((string= name "WHO-REFERENCES") "reference")
    ((string= name "WHO-SETS") "set")
    (t (string-downcase name))))

(declaim (ftype (function (string &key (:package (or null package symbol string))
                                 (:project-only (member t nil)))
                          (values vector fixnum &optional))
                code-find-references))
(defun code-find-references (symbol-name &key package (project-only t))
  "Return a vector of reference objects and the count for SYMBOL-NAME.
Each element is a hash-table with keys \"path\", \"line\", \"type\", \"context\"."
  (let ((sym (%parse-symbol symbol-name :package package))
        (results '()))
    #+sbcl
    (let* ((pkg (%ensure-sb-introspect))
           (finders '("WHO-CALLS"
                      "WHO-MACROEXPANDS"
                      "WHO-BINDS"
                      "WHO-REFERENCES"
                      "WHO-SETS"))
           (path-fn (and pkg (find-symbol "DEFINITION-SOURCE-PATHNAME" pkg)))
           (offset-fn (and pkg (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" pkg)))
           (seen (make-hash-table :test #'equal)))
      (dolist (finder finders)
        (let ((fn (and pkg (find-symbol finder pkg))))
          (when fn
            (dolist (source (ignore-errors (funcall fn sym)))
              (let ((definition (if (consp source) (cdr source) source)))
                (multiple-value-bind (pathname path line)
                    (%definition->path/line definition path-fn offset-fn)
                  (when (and path line
                             (or (not project-only)
                                 (%path-inside-project-p pathname)))
                    (let* ((type (%finder->type finder))
                           (context (%line-snippet pathname line))
                           (key (format nil "~A:~A:~A" path line type)))
                      (unless (gethash key seen)
                        (setf (gethash key seen) t)
                        (let ((h (make-hash-table :test #'equal)))
                          (setf (gethash "path" h) path
                                (gethash "line" h) line
                                (gethash "type" h) type
                                (gethash "context" h) (or context ""))
                          (push h results)))))))))))
      #-sbcl
      (error "code-find-references requires SBCL")
      (let ((vec (coerce (nreverse results) 'vector)))
        (values vec (length vec))))))

(define-tool "code-find"
  :description "Locate the definition of a symbol (path and line) using sb-introspect.

PREREQUISITE: The defining system MUST be loaded first (ql:quickload).
Prefer package-qualified symbols or supply the package argument.

NOTE: If the symbol is not found, the system might not be loaded yet.
For code exploration WITHOUT loading systems, use 'clgrep-search' instead.
Fallback: Use 'lisp-read-file' with 'name_pattern' to search the file system."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl:mapcar\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified; ensure the package exists
and is loaded"))
  :body
  (multiple-value-bind (path line)
      (code-find-definition symbol :package package)
    (if path
        (result id
                (make-ht "path" path
                         "line" line
                         "content" (text-content
                                    (format nil "~A defined in ~A at line ~D"
                                            symbol path line))))
        (rpc-error id -32004
                   (format nil "Definition not found for ~A" symbol)))))

(define-tool "code-describe"
  :description "Describe a symbol: type, arglist, and documentation.

PREREQUISITE: The defining system MUST be loaded first (ql:quickload).
Pass a package or a package-qualified symbol to avoid resolution errors.

NOTE: If the symbol is not found, the system might not be loaded yet.
For code exploration WITHOUT loading systems, use 'clgrep-search' instead.
Fallback: Use 'lisp-read-file' with 'name_pattern' to search the file system."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl:mapcar\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified; ensure the package exists
and is loaded"))
  :body
  (multiple-value-bind (name type arglist doc path line)
      (code-describe-symbol symbol :package package)
    (result id
            (make-ht "name" name
                     "type" type
                     "arglist" arglist
                     "documentation" doc
                     "path" path
                     "line" line
                     "content" (text-content
                                (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]~@[~%Defined at ~A:~D~]"
                                        name type arglist doc path line))))))

(define-tool "code-find-references"
  :description "Find where a symbol is referenced using SBCL xref (calls, macroexpands, binds,
references, sets).

PREREQUISITE: The defining system MUST be loaded first (ql:quickload).
Use package-qualified symbols when possible; set projectOnly=false to include
external libs.

For simple text-based usage search WITHOUT loading systems, use 'clgrep-search' instead."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl-mcp:run\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified")
         (project-only :type :boolean :json-name "project_only" :default t
                       :description "When true (default), only include references under the project root"))
  :body
  (multiple-value-bind (refs count)
      (code-find-references symbol :package package :project-only project-only)
    (let* ((summary-lines
            (map 'list
                 (lambda (h)
                   (format nil "~A:~D ~A ~A"
                           (gethash "path" h)
                           (gethash "line" h)
                           (gethash "type" h)
                           (gethash "context" h)))
                 refs))
           (summary (if summary-lines
                        (format nil "~{~A~%~}" summary-lines)
                        "")))
      (result id
              (make-ht "refs" refs
                       "count" count
                       "symbol" symbol
                       "project_only" project-only
                       "content" (text-content summary))))))
