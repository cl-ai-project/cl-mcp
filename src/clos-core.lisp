;;;; src/clos-core.lisp
;;;;
;;;; CLOS introspection for clos-describe: a generic function's methods and a
;;;; class's hierarchy, slots and specialized methods, read from the running
;;;; image.  Runs in the worker; loads no eclector.  Reads only: it never
;;;; finalizes a class, calls an initfunction or interns a symbol.

(defpackage #:cl-mcp/src/clos-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-core
                #:definition-source-location
                #:with-definition-source-cache
                #:%ensure-sb-introspect
                #:%sbcl-function)
  (:import-from #:cl-mcp/src/code-refs-core
                #:resolve-target
                #:qualified-symbol-name
                #:symbol-kind
                #:%status-string)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:path-inside-p)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:json-bool)
  (:export #:clos-describe-report
           #:*note-not-finalized*))

(in-package #:cl-mcp/src/clos-core)

(defparameter *note-not-finalized*
  "not finalized; precedence list and slots were computed without finalizing the class"
  "Note added when a class's precedence list and slots were computed rather
than read, because the class has not been finalized.")

(defun %introspect (name)
  "Return SB-INTROSPECT's function NAME, or NIL when it is unavailable."
  (let ((pkg (%ensure-sb-introspect)))
    (and pkg (%sbcl-function (package-name pkg) name))))

(defun %home-package (name)
  "Return the package whose reader prints NAME -- a symbol or (SETF symbol) --
without a prefix: its symbol's home package, or COMMON-LISP-USER."
  (let ((symbol (if (consp name) (second name) name)))
    (or (and (symbolp symbol) (symbol-package symbol))
        (find-package "COMMON-LISP-USER"))))

(defun %form-text (form package)
  "Return FORM, unevaluated code from a definition, printed on one line as
PACKAGE's reader would read it, or a placeholder when it cannot be printed."
  (let ((*package* (or package (find-package "COMMON-LISP-USER")))
        (*print-pretty* t)
        (*print-right-margin* most-positive-fixnum)
        (*print-length* 10)
        (*print-level* 4)
        (*print-circle* t)
        (*print-readably* nil))
    (handler-case (prin1-to-string form)
      (error () "#<unprintable>"))))

(defun %datum-text (object)
  "Return OBJECT printed with every symbol fully qualified, keywords as :NAME."
  (%form-text object (find-package "KEYWORD")))

(defun %name-string (name)
  "Return a function or class NAME fully qualified: PKG::NAME or (SETF PKG::NAME)."
  (cond
    ((and (consp name) (eq (first name) 'setf) (symbolp (second name)))
     (format nil "(SETF ~A)" (qualified-symbol-name (second name))))
    ((symbolp name) (qualified-symbol-name name))
    (t (%datum-text name))))

(defun %qualifier-string (qualifier)
  "Return a method QUALIFIER as written: :AROUND for a keyword, the bare name
of any other symbol (+ for the + method combination)."
  (cond
    ((keywordp qualifier) (format nil ":~A" (symbol-name qualifier)))
    ((symbolp qualifier) (symbol-name qualifier))
    (t (%datum-text qualifier))))

(defun %proper-class-name (class)
  "Return CLASS's name when it is a symbol that names CLASS, else NIL."
  (let ((name (ignore-errors (class-name class))))
    (and name (symbolp name) (eq (find-class name nil) class) name)))

(defun %specializer-string (specializer)
  "Return SPECIALIZER as text: a class's qualified name or (EQL object)."
  (cond
    ((typep specializer 'sb-mop:eql-specializer)
     (format nil "(EQL ~A)" (%datum-text (sb-mop:eql-specializer-object specializer))))
    ((and (typep specializer 'class) (%proper-class-name specializer))
     (qualified-symbol-name (%proper-class-name specializer)))
    (t (%datum-text specializer))))

(defun %standard-combination-p (gf)
  "True when GF uses the STANDARD method combination."
  (let ((name-fn (%sbcl-function "SB-PCL" "METHOD-COMBINATION-TYPE-NAME")))
    (and name-fn
         (eq (ignore-errors
              (funcall name-fn (sb-mop:generic-function-method-combination gf)))
             'standard))))

(defun %method-combination-string (gf)
  "Return GF's method combination as its name and options: STANDARD,
+ :MOST-SPECIFIC-FIRST.  NIL when this SBCL does not say."
  (let* ((combination (ignore-errors (sb-mop:generic-function-method-combination gf)))
         (name-fn (%sbcl-function "SB-PCL" "METHOD-COMBINATION-TYPE-NAME"))
         (options-fn (%sbcl-function "SB-PCL" "METHOD-COMBINATION-OPTIONS"))
         (name (and combination name-fn (ignore-errors (funcall name-fn combination))))
         (options (and combination options-fn
                       (ignore-errors (funcall options-fn combination)))))
    (and (symbolp name) name
         (format nil "~A~{ ~A~}" (symbol-name name) (mapcar #'%datum-text options)))))

(defun %role-rank (method)
  "Rank METHOD by its role in the standard method combination:
:AROUND, :BEFORE, primary, :AFTER, anything else."
  (let ((qualifiers (method-qualifiers method)))
    (cond
      ((equal qualifiers '(:around)) 0)
      ((equal qualifiers '(:before)) 1)
      ((null qualifiers) 2)
      ((equal qualifiers '(:after)) 3)
      (t 4))))

(defun %method-source (method)
  "Return METHOD's SB-INTROSPECT definition source, or NIL."
  (let ((find (%introspect "FIND-DEFINITION-SOURCE")))
    (and find (ignore-errors (funcall find method)))))

(defun %source-sort-key (source)
  "Return (PLACE NAMESTRING FORM-NUMBER) ordering SOURCE without reading its
file: PLACE is 0 inside the project root, 1 elsewhere, 2 without a file."
  (let* ((path-fn (%introspect "DEFINITION-SOURCE-PATHNAME"))
         (form-path-fn (%introspect "DEFINITION-SOURCE-FORM-PATH"))
         (pathname (and source path-fn (ignore-errors (funcall path-fn source))))
         (form-path (and source form-path-fn (ignore-errors (funcall form-path-fn source))))
         (absolute (and pathname (ignore-errors (uiop:absolute-pathname-p pathname)))))
    (list (cond ((null absolute) 2)
                ((and *project-root*
                      (ignore-errors (path-inside-p pathname *project-root*)))
                 0)
                (t 1))
          (if pathname (namestring pathname) "")
          (if (and (consp form-path) (integerp (first form-path)))
              (first form-path)
              0))))

(defun %key< (a b)
  "Compare two lists of numbers and strings element by element."
  (loop for x in a
        for y in b
        do (cond ((and (numberp x) (numberp y))
                  (cond ((< x y) (return t)) ((> x y) (return nil))))
                 ((string< x y) (return t))
                 ((string> x y) (return nil)))
        finally (return nil)))

(defun %set-location (ht source)
  "Store SOURCE's location in HT -- abs_path, path, line and stale -- with null
form_type, form_name and note for the parent to fill from the source file."
  (multiple-value-bind (abs-path path line stale)
      (if source (definition-source-location source) (values nil nil nil nil))
    (setf (gethash "abs_path" ht) abs-path
          (gethash "path" ht) path
          (gethash "line" ht) line
          (gethash "stale" ht) (json-bool stale)
          (gethash "form_type" ht) nil
          (gethash "form_name" ht) nil
          (gethash "note" ht) nil)
    ht))

(defun %first-line (condition)
  "Return CONDITION's report, cut at its first line."
  (let ((text (handler-case (princ-to-string condition)
                (error () "unprintable error"))))
    (subseq text 0 (or (position #\Newline text) (length text)))))

(defun %method-entry (method &key via)
  "Return the JSON object for METHOD.  VIA, a class, is recorded when METHOD
was found through that class's specialized methods."
  (let ((ht (%set-location (make-ht) (%method-source method))))
    (setf (gethash "generic_function" ht) nil
          (gethash "qualifiers" ht) (vector)
          (gethash "specializers" ht) (vector)
          (gethash "kind" ht) "method"
          (gethash "slot" ht) nil)
    (when via
      (setf (gethash "via" ht) (%name-string (%proper-class-name via))))
    (handler-case
        (let ((gf (sb-mop:method-generic-function method)))
          (setf (gethash "generic_function" ht)
                (and gf (%name-string (sb-mop:generic-function-name gf)))
                (gethash "qualifiers" ht)
                (map 'vector #'%qualifier-string (method-qualifiers method))
                (gethash "specializers" ht)
                (map 'vector #'%specializer-string (sb-mop:method-specializers method)))
          (when (typep method 'sb-mop:standard-accessor-method)
            (setf (gethash "kind" ht)
                  (if (typep method 'sb-mop:standard-reader-method) "reader" "writer")
                  (gethash "slot" ht)
                  (qualified-symbol-name
                   (sb-mop:slot-definition-name
                    (sb-mop:accessor-method-slot-definition method))))))
      (error (e)
        (setf (gethash "note" ht)
              (format nil "could not read this method: ~A" (%first-line e)))))
    ht))

(defun %sorted-methods (methods standard-p)
  "Return METHODS ordered for display: by role when STANDARD-P, then project
files, other files and no file, then by file and top-level form."
  (mapcar #'car
          (stable-sort (mapcar (lambda (method)
                                 (cons method
                                       (cons (if standard-p (%role-rank method) 0)
                                             (%source-sort-key (%method-source method)))))
                               methods)
                       #'%key< :key #'cdr)))

(defun %generic-function-entry (gf limit)
  "Return the JSON object for GF with at most LIMIT of its methods.  Methods are
sorted before any is located, so only the listed ones read their files."
  (let* ((name (sb-mop:generic-function-name gf))
         (package (%home-package name))
         (methods (sb-mop:generic-function-methods gf))
         (sorted (%sorted-methods methods (%standard-combination-p gf)))
         (by-name (%introspect "FIND-DEFINITION-SOURCES-BY-NAME"))
         (source (and by-name
                      (first (ignore-errors (funcall by-name name :generic-function)))))
         (ht (%set-location (make-ht) source)))
    (setf (gethash "name" ht) (%name-string name)
          (gethash "lambda_list" ht)
          (%form-text (ignore-errors (sb-mop:generic-function-lambda-list gf)) package)
          (gethash "documentation" ht) (ignore-errors (documentation gf t))
          (gethash "method_combination" ht) (%method-combination-string gf)
          (gethash "method_count" ht) (length methods)
          (gethash "truncated" ht) (json-bool (> (length methods) limit))
          (gethash "methods" ht)
          (map 'vector #'%method-entry (subseq sorted 0 (min limit (length sorted)))))
    ht))

(defun clos-describe-report (symbol-name &key package (limit 50))
  "Return the clos-describe payload for SYMBOL-NAME, everything but its content
text and the form_type, form_name and note fields the parent fills in.

SYMBOL-NAME is resolved like code-find-references' symbol (RESOLVE-TARGET):
nothing is interned.  The report holds a generic_functions entry for the
function SYMBOL-NAME names and one for its SETF function, when either is a
generic function.  At most LIMIT methods are listed per entry.  docs/tools.md
describes every field."
  (multiple-value-bind (symbol status lookup-package lookup-name)
      (resolve-target symbol-name :package package)
    (let ((report (make-ht "symbol" symbol-name
                           "symbol_status" (%status-string status)
                           "resolved_symbol" (and symbol (qualified-symbol-name symbol))
                           "symbol_kind" (and (eq status :found) (symbol-kind symbol))
                           "lookup_package" lookup-package
                           "lookup_name" lookup-name
                           "generic_functions" (vector)
                           "class" nil
                           "limit" limit
                           "notes" (vector))))
      (when (eq status :found)
        (with-definition-source-cache
          (setf (gethash "generic_functions" report)
                (coerce (loop for name in (list symbol (list 'setf symbol))
                              for function = (ignore-errors
                                              (and (fboundp name) (fdefinition name)))
                              when (typep function 'generic-function)
                                collect (%generic-function-entry function limit))
                        'vector))))
      report)))
