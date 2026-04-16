;;;; src/code-core.lisp
;;;;
;;;; Core code intelligence logic (sb-introspect), shared between parent and worker.

(defpackage #:cl-mcp/src/code-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/project-root #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:normalize-path-for-display #:path-inside-p)
  (:import-from #:uiop
                #:read-file-string #:ensure-pathname
                #:ensure-directory-pathname #:absolute-pathname-p)
  (:export #:code-find-definition
           #:code-describe-symbol
           #:code-find-references
           #:%offset->line
           #:%ensure-sb-introspect))

(in-package #:cl-mcp/src/code-core)

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
     (or (find-package (string-upcase package))
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
SBCL's DEFINITION-SOURCE-CHARACTER-OFFSET typically points at a
whitespace character or reader-conditional directive that precedes
the actual `(def...)' form. Walk forward from OFFSET across:
  - whitespace
  - `;' line comments
  - `#|...|#' block comments
  - `#+feature' / `#-feature' reader conditionals (with atom or
    list feature expressions)
and stop at the first `(' that begins the next definition form.
Scan is capped at 1024 characters of look-ahead to stay safe if
the offset is spurious. Returns NIL when the file cannot be read."
  (when (and pathname offset)
    (handler-case
        (let* ((physical (translate-logical-pathname pathname))
               (content (uiop:read-file-string physical))
               (len (length content))
               (start (min (max offset 0) len))
               (limit (min len (+ start 1024))))
          (labels ((ws-p (ch)
                     (or (char= ch #\Space) (char= ch #\Tab)
                         (char= ch #\Newline) (char= ch #\Return)))
                   (skip-balanced-list (i)
                     (let ((depth 0))
                       (loop while (< i limit) do
                         (let ((c (char content i)))
                           (incf i)
                           (cond
                             ((char= c #\() (incf depth))
                             ((char= c #\))
                              (decf depth)
                              (when (zerop depth) (return))))))
                       i))
                   (skip-atom (i)
                     (loop while (< i limit) do
                       (let ((c (char content i)))
                         (when (or (ws-p c) (char= c #\() (char= c #\))
                                   (char= c #\;))
                           (return))
                         (incf i)))
                     i)
                   (skip-conditional (i)
                     (incf i 2)
                     (loop while (and (< i limit) (ws-p (char content i)))
                           do (incf i))
                     (if (< i limit)
                         (if (char= (char content i) #\()
                             (skip-balanced-list i)
                             (skip-atom i))
                         i))
                   (skip-block-comment (i)
                     (let ((end (search "|#" content :start2 (+ i 2)
                                        :end2 limit)))
                       (if end (+ end 2) limit))))
            (let ((i start))
              (loop while (< i limit) do
                (let ((ch (char content i)))
                  (cond
                    ((ws-p ch) (incf i))
                    ((char= ch #\;)
                     (let ((nl (position #\Newline content :start i
                                         :end limit)))
                       (setf i (if nl (1+ nl) limit))))
                    ((char= ch #\#)
                     (cond
                       ((and (< (1+ i) limit)
                             (char= (char content (1+ i)) #\|))
                        (setf i (skip-block-comment i)))
                       ((and (< (1+ i) limit)
                             (or (char= (char content (1+ i)) #\+)
                                 (char= (char content (1+ i)) #\-)))
                        (setf i (skip-conditional i)))
                       (t (return))))
                    (t (return)))))
              (1+ (count #\Newline content :end (min i len))))))
      (error (e)
        (log-event :warn "code.find.line-error"
                   "path" (princ-to-string pathname)
                   "error" (princ-to-string e))
        nil))))

(declaim (ftype (function (string &key (:package (or null package symbol string)))
                          (values (or null string) (or null integer) &optional))
                code-find-definition))

(defun code-find-definition (symbol-name &key package)
  "Return the definition location for SYMBOL-NAME.
Values are PATH (string) and LINE (integer), or NILs when not found.
Searches multiple SB-INTROSPECT definition kinds so that classes,
structures, conditions, generic functions, macros, and variables are
all locatable, not only ordinary functions."
  (let* ((qualified (position #\: symbol-name))
         (pkg (if qualified nil package))
         (sym (%parse-symbol symbol-name :package pkg)))
    #+sbcl
    (let* ((pkg (%ensure-sb-introspect))
           (find-by-name (and pkg (find-symbol "FIND-DEFINITION-SOURCES-BY-NAME" pkg)))
           (find (and pkg (find-symbol "FIND-DEFINITION-SOURCE" pkg)))
           (path-fn (and pkg (find-symbol "DEFINITION-SOURCE-PATHNAME" pkg)))
           (offset (and pkg (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" pkg)))
           (kinds '(:function :generic-function :method :macro
                    :class :condition :structure :type
                    :variable :constant :method-combination :package))
           (source
            (or ;; Prefer definitions with a known source file.
                ;; Implicitly-created GFs have NIL pathname; :method
                ;; entries carry the actual defmethod file location.
                (loop for kind in kinds
                      for src = (and find-by-name
                                     (first (ignore-errors
                                             (funcall find-by-name sym kind))))
                      when (and src path-fn (funcall path-fn src))
                        return src)
                ;; Fallback: accept any source even without pathname
                (loop for kind in kinds
                      for src = (and find-by-name
                                     (first (ignore-errors
                                             (funcall find-by-name sym kind))))
                      when src return src)
                (and find (ignore-errors (funcall find sym))))))
      (when (and source path-fn)
        (let* ((pathname (funcall path-fn source))
               (char-offset (and offset (funcall offset source)))
               (line (%offset->line pathname char-offset))
               (path (normalize-path-for-display pathname)))
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
Handles functions, macros, generic functions, variables, classes,
condition types, and structure types. Signals an error only when none
of those bindings resolve. PATH/LINE may be NIL when unknown.

TYPE is one of:
  \"function\", \"generic-function\", \"macro\", \"variable\",
  \"class\", \"condition\", \"structure\"."
  (let* ((sym (%parse-symbol symbol-name :package package))
         (name (princ-to-string sym))
         (class (find-class sym nil))
         (type
          (cond
            ((macro-function sym) "macro")
            ((and (fboundp sym)
                  (typep (symbol-function sym) 'generic-function))
             "generic-function")
            ((fboundp sym) "function")
            ((boundp sym) "variable")
            ((and class
                  (subtypep (class-name class) 'condition))
             "condition")
            #+sbcl
            ((and class
                  (typep class (find-class 'structure-class)))
             "structure")
            (class "class")
            (t "unbound"))))
    (when (string= type "unbound")
      (error "Symbol ~A is not bound as a function, variable, class, or condition"
             sym))
    #+sbcl
    (%ensure-sb-introspect)
    (let* ((fn (cond
                 ((macro-function sym))
                 ((fboundp sym) (symbol-function sym))
                 (t nil)))
           (arglist
            (cond
              (fn
               (handler-case
                   (let* ((fn-ll (%sb-introspect-symbol "FUNCTION-LAMBDA-LIST"))
                          (args (and fn-ll (funcall fn-ll fn))))
                     (cond
                       ((null args) "()")
                       ((listp args) (princ-to-string args))
                       (t (princ-to-string args))))
                 (error (e)
                   (log-event :warn "code.describe.arglist-error"
                              "symbol" symbol-name
                              "error" (princ-to-string e))
                   "()")))
              (class
               (handler-case
                   (let* ((slots-fn
                            #+sbcl (find-symbol "CLASS-DIRECT-SLOTS" "SB-MOP")
                            #-sbcl nil)
                          (slots (and slots-fn
                                      (ignore-errors (funcall slots-fn class))))
                          (slot-name-fn
                            #+sbcl (find-symbol "SLOT-DEFINITION-NAME" "SB-MOP")
                            #-sbcl nil))
                     (if (and slots slot-name-fn)
                         (format nil "(~{~(~A~)~^ ~})"
                                 (mapcar (lambda (s)
                                           (funcall slot-name-fn s))
                                         slots))
                         "()"))
                 (error () "()")))
              (t nil)))
           (doc
            (cond
              ((or (macro-function sym) (fboundp sym))
               (documentation sym 'function))
              ((boundp sym) (documentation sym 'variable))
              (class (documentation sym 'type))
              (t nil))))
      (multiple-value-bind (path line)
          (code-find-definition symbol-name :package package)
        (values name type arglist doc path line)))))

(defun %path-inside-project-p (pathname)
  "Return T when PATHNAME is inside *project-root*.
For relative paths, verifies the file exists under project root.
Returns T for any path when *project-root* is not set."
  (and pathname
       (if *project-root*
           (if (uiop:absolute-pathname-p pathname)
               (path-inside-p (uiop:ensure-pathname pathname :want-relative nil)
                              (uiop:ensure-directory-pathname *project-root*))
               ;; Relative path: must exist under project root
               (and (probe-file (merge-pathnames pathname *project-root*)) t))
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
         (path (normalize-path-for-display pathname)))
    (values pathname path (or line (and pathname char-offset 1)))))

(defun %format-xref-caller (name)
  "Render an SB-INTROSPECT xref caller NAME as a short human-readable string.

Normalizes several SBCL-internal shapes into the form the user would
type to locate the call site:

  FOO                               -> \"foo\"
  (PKG::FOO)                        -> \"pkg::foo\"
  (SB-PCL::FAST-METHOD NAME ...)    -> \"(defmethod name ...)\"
  (:METHOD NAME ...)                -> \"(defmethod name ...)\"
  (METHOD NAME ...)                 -> \"(defmethod name ...)\"
  (FLET INNER :IN OUTER)            -> \"flet inner :in outer\"
  (LABELS INNER :IN OUTER)          -> \"labels inner :in outer\"
  (LAMBDA () :IN /abs/path)         -> \"(lambda)\"
  (:LAMBDA ...)                     -> \"(lambda)\"
  (SOMETHING ...)                   -> downcased, absolute path stripped

Returns NIL for NIL input."
  (when name
    (handler-case
        (let ((*print-case* :downcase)
              (*print-readably* nil)
              (*print-gensym* nil))
          (cond
            ((symbolp name)
             (princ-to-string name))
            ((not (consp name))
             (princ-to-string name))
            ;; SBCL-specific fast method wrapper
            ((and (symbolp (car name))
                  (or (string= (symbol-name (car name)) "FAST-METHOD")
                      (string= (symbol-name (car name)) "SLOW-METHOD")))
             (format nil "(defmethod ~{~(~A~)~^ ~})" (cdr name)))
            ;; Keyword :method / plain method
            ((and (symbolp (car name))
                  (or (string= (symbol-name (car name)) "METHOD")
                      (eq (car name) :method)))
             (format nil "(defmethod ~{~(~A~)~^ ~})" (cdr name)))
            ;; (lambda ...) or (:lambda ...) — drop absolute file paths
            ((and (symbolp (car name))
                  (or (string= (symbol-name (car name)) "LAMBDA")
                      (eq (car name) :lambda)))
             "(lambda)")
            ;; (flet name :in parent) / (labels name :in parent)
            ((and (symbolp (car name))
                  (or (string= (symbol-name (car name)) "FLET")
                      (string= (symbol-name (car name)) "LABELS"))
                  (consp (cdr name)))
             (format nil "~(~A~) ~(~A~)~@[ :in ~(~A~)~]"
                     (car name)
                     (second name)
                     (let ((in (member :in name))) (and in (second in)))))
            (t
             ;; Generic form: strip any absolute path strings from pieces.
             (format nil "(~{~A~^ ~})"
                     (mapcar
                      (lambda (piece)
                        (cond
                          ((and (stringp piece)
                                (or (uiop:string-prefix-p "/" piece)
                                    (uiop:string-prefix-p "\\" piece)))
                           "...")
                          (t (format nil "~(~A~)" piece))))
                      name)))))
      (error () nil))))

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
Each element is a hash-table with keys \"path\", \"line\", \"type\",
\"caller\", and \"context\".

SBCL xref reports references at the granularity of the *enclosing
function's definition location*, not the exact call-site line, so
the LINE field points at the start of the function that contains
the reference. The CALLER field surfaces the enclosing function's
fully-qualified name so callers can locate the actual usage: read
the caller's definition with LISP-READ-FILE name_pattern=<caller>,
or grep the file starting from LINE to find the call site."
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
              (let ((caller-name (and (consp source) (car source)))
                    (definition (if (consp source) (cdr source) source)))
                (multiple-value-bind (pathname path line)
                    (%definition->path/line definition path-fn offset-fn)
                  (when (and path line
                             (or (not project-only)
                                 (%path-inside-project-p pathname)))
                    (let* ((type (%finder->type finder))
                           (caller-str (%format-xref-caller caller-name))
                           (context (%line-snippet pathname line))
                           (key (format nil "~A:~A:~A:~A"
                                        path line type (or caller-str ""))))
                      (unless (gethash key seen)
                        (setf (gethash key seen) t)
                        (let ((h (make-hash-table :test #'equal)))
                          (setf (gethash "path" h) path
                                (gethash "line" h) line
                                (gethash "type" h) type
                                (gethash "caller" h) (or caller-str "")
                                (gethash "context" h) (or context ""))
                          (push h results)))))))))))
      #-sbcl
      (error "code-find-references requires SBCL")
      (let ((vec (coerce (nreverse results) 'vector)))
        (values vec (length vec))))))
