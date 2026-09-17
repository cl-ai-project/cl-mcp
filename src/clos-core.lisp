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

(defun %identity-symbol (symbol)
  "Return SYMBOL's identity as {package, name}: its home package's name, or
NIL when SYMBOL is uninterned, and SYMBOL-NAME exactly as it prints -- case
kept, no reader-macro prefix."
  (make-ht "package" (let ((package (symbol-package symbol)))
                       (and package (package-name package)))
           "name" (symbol-name symbol)))

(defun %identity-function-name (name)
  "Return the identity of a function NAME -- a symbol, or (SETF symbol) -- as
{package, name, setf}: %IDENTITY-SYMBOL of the base symbol plus whether NAME
is a SETF function name."
  (let* ((setf-p (consp name))
         (symbol (if setf-p (second name) name))
         (identity (%identity-symbol symbol)))
    (setf (gethash "setf" identity) (json-bool setf-p))
    identity))

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

(defun %decimal-string (integer)
  "Return INTEGER printed in base 10 without a radix marker, so a bignum
travels as JSON text instead of a float."
  (let ((*print-base* 10) (*print-radix* nil))
    (princ-to-string integer)))

(defun %eql-unverifiable-reason (object)
  "Return one sentence explaining why OBJECT cannot be an EQL datum: it is not
on the tagged allow-list of spec 3.3 (integer, ratio, character, keyword,
boolean, or an interned symbol)."
  (cond
    ((floatp object) "a float compares unsoundly by printed value")
    ((complexp object) "a complex number compares unsoundly by printed value")
    ((stringp object) "a string is not EQL-comparable by content")
    ((consp object) "a list is not EQL-comparable by content")
    ((arrayp object) "an array is not EQL-comparable by content")
    ((symbolp object) "an uninterned symbol has no package to resolve it by")
    (t (format nil "a value of type ~(~A~) is not on the EQL datum allow-list"
               (type-of object)))))

(defun %eql-datum-identity (object)
  "Return OBJECT's EQL datum, tagged per spec 3.3, built from the live object
an EQL specializer holds -- never from source text or a printed
representation.  Anything off the allow-list (float, complex, string, list,
array, uninterned symbol, or any other object) comes back
{\"kind\": \"unverifiable\", \"reason\": ...} instead of a guess."
  (cond
    ((eq object t) (make-ht "kind" "boolean" "value" "T"))
    ((null object) (make-ht "kind" "boolean" "value" "NIL"))
    ((keywordp object) (make-ht "kind" "keyword" "name" (symbol-name object)))
    ((integerp object) (make-ht "kind" "integer" "value" (%decimal-string object)))
    ((typep object 'ratio)
     (make-ht "kind" "ratio"
              "numerator" (%decimal-string (numerator object))
              "denominator" (%decimal-string (denominator object))))
    ((characterp object) (make-ht "kind" "character" "value" (string object)))
    ((and (symbolp object) (symbol-package object))
     (make-ht "kind" "symbol"
              "package" (package-name (symbol-package object))
              "name" (symbol-name object)))
    (t (make-ht "kind" "unverifiable" "reason" (%eql-unverifiable-reason object)))))

(defun %specializer-identity (specializer)
  "Return SPECIALIZER's identity: {kind: \"class\", package, name} for a class
with a proper name, {kind: \"eql\", datum: <spec 3.3 tagged datum>} for an EQL
specializer, or {kind: \"unverifiable\", reason} for a class with none -- an
anonymous or forward-referenced superclass."
  (cond
    ((typep specializer 'sb-mop:eql-specializer)
     (make-ht "kind" "eql"
              "datum" (%eql-datum-identity (sb-mop:eql-specializer-object specializer))))
    ((typep specializer 'class)
     (let ((name (%proper-class-name specializer)))
       (if name
           (let ((identity (%identity-symbol name)))
             (setf (gethash "kind" identity) "class")
             identity)
           (make-ht "kind" "unverifiable" "reason" "anonymous class has no name to match"))))
    (t (make-ht "kind" "unverifiable" "reason" "specializer is neither a class nor EQL"))))

(defun %accessor-owner-name (method)
  "Return the symbol naming the class METHOD, a standard accessor method, was
generated for: its last specializer -- the sole one for a reader, the second
of two for a writer -- or NIL when that class has no proper name."
  (%proper-class-name (car (last (sb-mop:method-specializers method)))))

(defun %condition-class-p (class)
  "True when CLASS is certainly a subtype of CONDITION.  Asks the type system
rather than CLASS's name or its metaclass, and demands SUBTYPEP's certainty
flag, so an unfinalized or forward-referenced specializer -- or a metaclass
whose SUBTYPEP signals -- answers NIL, fail-closed."
  (multiple-value-bind (subtype-p certain-p)
      (ignore-errors (subtypep class 'condition))
    (and subtype-p certain-p t)))

(defun %condition-accessor-slot (method qualifiers)
  "Return (values SLOT-NAME ACCESS OWNER) when METHOD, a plain, unqualified
method specialized on a CONDITION class, is unambiguously identifiable as one
of that condition's slot accessors: QUALIFIERS (METHOD's own, passed in so
this need not recompute what %METHOD-ENTRY already has) is empty -- an
accessor is never :BEFORE/:AFTER/:AROUND-qualified, a guarantee the
STANDARD-ACCESSOR-METHOD branch gets for free from its type but this fallback
must check directly, since a hand-written qualified method can otherwise share
an accessor's generic function and sole specializer without being one -- its
sole specializer is a proper class OWNER that is certainly a subtype of
CONDITION, and exactly one of OWNER's direct slots' READERS or WRITERS names
METHOD's generic function.

The CONDITION restriction is what this fallback exists for and all it is for.
SBCL never makes a DEFINE-CONDITION slot reader or writer a
STANDARD-ACCESSOR-METHOD, so a genuine condition accessor can only be
recognised this way; on an ordinary class a genuine accessor always arrives as
a STANDARD-ACCESSOR-METHOD and never reaches here.  Anything that does reach
here on an ordinary class is therefore a hand-written DEFMETHOD merely sharing
the accessor's generic function and specializer -- typically one that replaced
the generated reader -- and is reported as the plain method it is.  On a
condition the two stay indistinguishable from the image alone, so this still
fills in the accessor fields there and CL-MCP/SRC/CLOS-VERIFY-CORE settles
which of the two it was from the source form.

Returns NIL fail-closed otherwise: any qualifier at all, more than one
specializer, an anonymous specializer, a specializer that is not certainly a
condition, no matching slot, or a slot matching in more than one role or more
than one slot matching at all -- never a guess between them.  Any MOP read
that signals (a metaclass whose accessors misbehave) is caught the same way,
degrading to NIL instead of failing the report."
  (ignore-errors
    (when (null qualifiers)
      (let ((specializers (sb-mop:method-specializers method)))
        (when (= (length specializers) 1)
          (let* ((class (first specializers))
                 (owner (%proper-class-name class))
                 (gf (sb-mop:method-generic-function method))
                 (name (and gf (sb-mop:generic-function-name gf)))
                 (matches '()))
            (when (and owner name (%condition-class-p class))
              (dolist (slot (sb-mop:class-direct-slots class))
                (when (member name (sb-mop:slot-definition-readers slot) :test #'equal)
                  (push (cons (sb-mop:slot-definition-name slot) "reader") matches))
                (when (member name (sb-mop:slot-definition-writers slot) :test #'equal)
                  (push (cons (sb-mop:slot-definition-name slot) "writer") matches)))
              (when (= (length matches) 1)
                (values (caar matches) (cdar matches) owner)))))))))

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
  (let ((ht (%set-location (make-ht) (%method-source method)))
        (identity (make-ht "kind" "method" "generic_function" nil
                           "qualifiers" (vector) "specializers" (vector)
                           "class" nil "slot" nil "access" nil)))
    (setf (gethash "generic_function" ht) nil
          (gethash "qualifiers" ht) (vector)
          (gethash "specializers" ht) (vector)
          (gethash "kind" ht) "method"
          (gethash "slot" ht) nil
          (gethash "identity" ht) identity)
    (when via
      (setf (gethash "via" ht) (%class-name-string via)))
    (handler-case
        (let ((gf (sb-mop:method-generic-function method))
              (qualifiers (method-qualifiers method))
              (specializers (sb-mop:method-specializers method)))
          (setf (gethash "generic_function" ht)
                (and gf (%name-string (sb-mop:generic-function-name gf)))
                (gethash "qualifiers" ht) (map 'vector #'%qualifier-string qualifiers)
                (gethash "specializers" ht) (map 'vector #'%specializer-string specializers)
                (gethash "generic_function" identity)
                (and gf (%identity-function-name (sb-mop:generic-function-name gf)))
                (gethash "qualifiers" identity) (map 'vector #'%identity-symbol qualifiers)
                (gethash "specializers" identity)
                (map 'vector #'%specializer-identity specializers))
          (cond
            ((typep method 'sb-mop:standard-accessor-method)
             (let ((slot-name (sb-mop:slot-definition-name
                               (sb-mop:accessor-method-slot-definition method)))
                   (owner (%accessor-owner-name method))
                   (access (if (typep method 'sb-mop:standard-reader-method)
                               "reader" "writer")))
               (setf (gethash "kind" ht) access
                     (gethash "slot" ht) (qualified-symbol-name slot-name)
                     (gethash "access" identity) access
                     (gethash "slot" identity) (%identity-symbol slot-name)
                     (gethash "class" identity) (and owner (%identity-symbol owner)))))
            (t
             (multiple-value-bind (slot-name access owner)
                 (%condition-accessor-slot method qualifiers)
               (when slot-name
                 (setf (gethash "kind" ht) access
                       (gethash "slot" ht) (qualified-symbol-name slot-name)
                       (gethash "access" identity) access
                       (gethash "slot" identity) (%identity-symbol slot-name)
                       (gethash "class" identity) (and owner (%identity-symbol owner))))))))
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
          (map 'vector #'%method-entry (subseq sorted 0 (min limit (length sorted))))
          (gethash "identity" ht)
          (make-ht "kind" "generic-function"
                   "generic_function" (%identity-function-name name)))
    ht))

(defun %class-name-string (class)
  "Return CLASS's name fully qualified when it is a symbol naming CLASS, else
an unreadable representation naming its metaclass,
#<anonymous COMMON-LISP:STANDARD-CLASS {10045A8F33}>, so a class without a
proper name never reads as NIL.  SBCL's own PRINT-OBJECT for a class is not
used: it writes an anonymous class's name as COMMON-LISP:NIL whatever
*PACKAGE* is."
  (let ((name (%proper-class-name class)))
    (if name
        (%name-string name)
        (with-output-to-string (stream)
          (print-unreadable-object (class stream :identity t)
            (format stream "anonymous ~A" (%class-name-string (class-of class))))))))

(defun %language-class-p (class)
  "True when CLASS belongs to the language or the implementation: its name's
package is COMMON-LISP or an SB- package.  Their methods are the standard
protocol every class inherits."
  (let* ((name (%proper-class-name class))
         (package (and name (symbol-package name)))
         (package-name (and package (package-name package))))
    (and package-name
         (or (string= package-name "COMMON-LISP")
             (uiop:string-prefix-p "SB-" package-name)))))

(defun %undefined-ancestors (class)
  "Return the forward-referenced classes among CLASS's ancestors."
  (let ((seen '())
        (undefined '()))
    (labels ((walk (c)
               (unless (member c seen)
                 (push c seen)
                 (if (typep c 'sb-mop:forward-referenced-class)
                     (pushnew c undefined)
                     (mapc #'walk (ignore-errors (sb-mop:class-direct-superclasses c)))))))
      (walk class))
    (nreverse undefined)))

(defun %precedence-list (class)
  "Return CLASS's precedence list without finalizing it, or NIL when an
ancestor is undefined."
  (if (sb-mop:class-finalized-p class)
      (sb-mop:class-precedence-list class)
      (ignore-errors (sb-mop:compute-class-precedence-list class))))

(defun %direct-slots-named (cpl name)
  "Return (CLASS . DIRECT-SLOT) for each class in CPL defining a slot NAME."
  (loop for class in cpl
        for slot = (find name (ignore-errors (sb-mop:class-direct-slots class))
                         :key #'sb-mop:slot-definition-name)
        when slot collect (cons class slot)))

(defun %names (names)
  "Return a vector of NAMES, function names, fully qualified."
  (map 'vector #'%name-string names))

(defun %slot-type-text (types package)
  "Return the type of a slot declared TYPES along the precedence list: T, the
one declared type, or the conjunction of several."
  (let ((declared (remove-duplicates (remove t types) :test #'equal :from-end t)))
    (%form-text (cond ((null declared) t)
                      ((null (rest declared)) (first declared))
                      (t (cons 'and declared)))
                package)))

(defun %allocation-text (allocation)
  "Return a slot ALLOCATION as lower-case text: instance or class."
  (if (symbolp allocation)
      (string-downcase (symbol-name allocation))
      "class"))

(defun %slot-documentation (slot)
  "Return SLOT's documentation, or NIL.  Only standard slot definitions are
asked: SBCL warns \"unsupported DOCUMENTATION\" for condition and structure
slots, and that warning would reach the caller's stderr."
  (and (typep slot 'sb-mop:standard-slot-definition)
       (ignore-errors (documentation slot t))))

(defun %direct-slot-entry (slot package)
  "Return the JSON object for SLOT, a direct slot definition."
  (make-ht "name" (qualified-symbol-name (sb-mop:slot-definition-name slot))
           "initargs" (map 'vector #'%datum-text (sb-mop:slot-definition-initargs slot))
           "initform" (and (sb-mop:slot-definition-initfunction slot)
                           (%form-text (sb-mop:slot-definition-initform slot) package))
           "type" (%form-text (sb-mop:slot-definition-type slot) package)
           "allocation" (%allocation-text (sb-mop:slot-definition-allocation slot))
           "readers" (%names (ignore-errors (sb-mop:slot-definition-readers slot)))
           "writers" (%names (ignore-errors (sb-mop:slot-definition-writers slot)))
           "documentation" (%slot-documentation slot)))

(defun %effective-slot-entry (name cpl package &optional effective)
  "Return the JSON object for the slot NAME as CPL's classes define it.

EFFECTIVE, the finalized class's effective slot definition, supplies initargs,
initform, type and allocation when given.  Otherwise they are merged from the
direct slots the standard way (CLHS 7.5.3): allocation and documentation from
the most specific, the initform from the most specific that has one, the
initargs from all of them, the type as the conjunction of their types.
Readers and writers are those of every direct slot NAME."
  (let* ((pairs (%direct-slots-named cpl name))
         (slots (mapcar #'cdr pairs))
         (with-initform (find-if #'sb-mop:slot-definition-initfunction slots)))
    (flet ((all (reader)
             (remove-duplicates (mapcan (lambda (slot)
                                          (copy-list (ignore-errors (funcall reader slot))))
                                        slots)
                                :test #'equal :from-end t)))
      (make-ht "name" (qualified-symbol-name name)
               "from" (and pairs (%class-name-string (car (first pairs))))
               "initargs" (map 'vector #'%datum-text
                               (if effective
                                   (sb-mop:slot-definition-initargs effective)
                                   (all #'sb-mop:slot-definition-initargs)))
               "initform" (let ((source (or effective with-initform)))
                            (and source (sb-mop:slot-definition-initfunction source)
                                 (%form-text (sb-mop:slot-definition-initform source) package)))
               "type" (if effective
                          (%form-text (sb-mop:slot-definition-type effective) package)
                          (%slot-type-text (mapcar #'sb-mop:slot-definition-type slots)
                                           package))
               "allocation" (%allocation-text
                             (sb-mop:slot-definition-allocation (or effective (first slots))))
               "readers" (%names (all #'sb-mop:slot-definition-readers))
               "writers" (%names (all #'sb-mop:slot-definition-writers))
               "documentation" (some #'%slot-documentation slots)))))

(defun %effective-slots (class cpl package)
  "Return the JSON objects for CLASS's effective slots, or NIL without CPL.
A finalized class's own effective slots are used, so a metaclass that
computes them differently is respected; an unfinalized class's are merged
from the direct slots, the most general class's first."
  (cond
    ((null cpl) nil)
    ((sb-mop:class-finalized-p class)
     (map 'vector
          (lambda (effective)
            (%effective-slot-entry (sb-mop:slot-definition-name effective) cpl package effective))
          (sb-mop:class-slots class)))
    (t
     (let ((names (remove-duplicates
                   (loop for c in (reverse cpl)
                         append (mapcar #'sb-mop:slot-definition-name
                                        (ignore-errors (sb-mop:class-direct-slots c))))
                   :from-end t)))
       (map 'vector (lambda (name) (%effective-slot-entry name cpl package)) names)))))

(defun %default-initargs (cpl package)
  "Return the JSON objects for the default initargs of the class whose
precedence list is CPL, each with the class that supplies it, or NIL without
CPL."
  (when cpl
    (let ((seen '())
          (entries '()))
      (dolist (c cpl)
        (dolist (initarg (ignore-errors (sb-mop:class-direct-default-initargs c)))
          (unless (member (first initarg) seen)
            (push (first initarg) seen)
            (push (make-ht "initarg" (%datum-text (first initarg))
                           "form" (%form-text (second initarg) package)
                           "from" (%class-name-string c))
                  entries))))
      (coerce (nreverse entries) 'vector))))

(defun %class-methods (class cpl)
  "Return (values PAIRS OMITTED) for the methods specialized on CLASS or its
superclasses.  PAIRS are (METHOD . CLASS-SPECIALIZED), ordered by that class's
place in CPL, then by generic function name -- X before (SETF X) -- and role.
OMITTED lists the language-level superclasses (%LANGUAGE-CLASS-P) whose
methods were left out; CLASS itself is never left out."
  (let ((seen (make-hash-table :test #'eq))
        (entries '())
        (omitted '()))
    (loop for c in (or cpl (list class))
          for rank from 0
          for methods = (ignore-errors (sb-mop:specializer-direct-methods c))
          do (if (and (not (eq c class)) (%language-class-p c))
                 (when methods (push c omitted))
                 (dolist (method methods)
                   (unless (gethash method seen)
                     (setf (gethash method seen) t)
                     (push (list method c rank) entries)))))
    (flet ((key (entry)
             (destructuring-bind (method c rank) entry
               (declare (ignore c))
               (let* ((gf (ignore-errors (sb-mop:method-generic-function method)))
                      (name (and gf (sb-mop:generic-function-name gf)))
                      (setf-p (consp name))
                      (base (if setf-p (second name) name)))
                 (list rank
                       (if (symbolp base) (symbol-name base) "")
                       (if setf-p 1 0)
                       (%role-rank method))))))
      (values (mapcar (lambda (entry) (cons (first entry) (second entry)))
                      (stable-sort (nreverse entries) #'%key< :key #'key))
              (nreverse omitted)))))

(defun %class-entry (class limit)
  "Return (values ENTRY NOTES): the JSON object for CLASS, with at most LIMIT
methods, and the notes the report should carry about it."
  (let* ((name (%proper-class-name class))
         (package (%home-package name))
         (finalized (sb-mop:class-finalized-p class))
         (cpl (%precedence-list class))
         (by-name (%introspect "FIND-DEFINITION-SOURCES-BY-NAME"))
         (source (and by-name name
                      (loop for kind in '(:class :condition :structure)
                            thereis (first (ignore-errors (funcall by-name name kind))))))
         (ht (%set-location (make-ht) source))
         (notes '()))
    (multiple-value-bind (pairs omitted) (%class-methods class cpl)
      (setf (gethash "name" ht) (%name-string name)
            (gethash "metaclass" ht) (%class-name-string (class-of class))
            (gethash "documentation" ht) (ignore-errors (documentation class t))
            (gethash "finalized" ht) (json-bool finalized)
            (gethash "direct_superclasses" ht)
            (map 'vector #'%class-name-string (sb-mop:class-direct-superclasses class))
            (gethash "direct_subclasses" ht)
            (%names (remove nil (mapcar #'%proper-class-name
                                        (sb-mop:class-direct-subclasses class))))
            (gethash "precedence_list" ht) (and cpl (map 'vector #'%class-name-string cpl))
            (gethash "undefined_superclasses" ht)
            (if cpl
                (vector)
                (map 'vector #'%class-name-string (%undefined-ancestors class)))
            (gethash "direct_slots" ht)
            (map 'vector (lambda (slot) (%direct-slot-entry slot package))
                 (ignore-errors (sb-mop:class-direct-slots class)))
            (gethash "effective_slots" ht) (%effective-slots class cpl package)
            (gethash "default_initargs" ht) (%default-initargs cpl package)
            (gethash "method_count" ht) (length pairs)
            (gethash "truncated" ht) (json-bool (> (length pairs) limit))
            (gethash "methods" ht)
            (map 'vector (lambda (pair) (%method-entry (car pair) :via (cdr pair)))
                 (subseq pairs 0 (min limit (length pairs))))
            (gethash "omitted_classes" ht) (map 'vector #'%class-name-string omitted)
            (gethash "identity" ht)
            (make-ht "kind" "class" "class" (and name (%identity-symbol name))))
      (cond
        ((null cpl)
         (let ((undefined (coerce (gethash "undefined_superclasses" ht) 'list)))
           (push (cond
                   ((null undefined)
                    (format nil "precedence list and effective slots unavailable: ~
the precedence list could not be computed"))
                   ;; %UNDEFINED-ANCESTORS lists CLASS itself exactly when it is
                   ;; forward-referenced: named as a superclass, never defined.
                   ((typep class 'sb-mop:forward-referenced-class)
                    "this class is referenced as a superclass but not defined")
                   (t
                    (format nil "precedence list and effective slots unavailable: ~
undefined superclass ~{~A~^, ~}"
                            undefined)))
                 notes)))
        ((not finalized)
         (push *note-not-finalized* notes))))
    (values ht (nreverse notes))))

(defun clos-describe-report (symbol-name &key package (limit 50))
  "Return the clos-describe payload for SYMBOL-NAME, everything but its content
text and the form_type, form_name and note fields the parent fills in.

SYMBOL-NAME is resolved like code-find-references' symbol (RESOLVE-TARGET):
nothing is interned.  The report holds a generic_functions entry for the
function SYMBOL-NAME names and one for its SETF function, when either is a
generic function, and a class entry when it names a class.  At most LIMIT
methods are listed per entry.  docs/tools.md describes every field."
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
                        'vector))
          (let ((class (find-class symbol nil)))
            (when class
              (multiple-value-bind (entry notes) (%class-entry class limit)
                (setf (gethash "class" report) entry
                      (gethash "notes" report) (coerce notes 'vector)))))))
      report)))
