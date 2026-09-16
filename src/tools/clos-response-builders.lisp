;;;; src/tools/clos-response-builders.lisp
;;;;
;;;; Parent-side half of clos-describe: annotate the worker's report with the
;;;; form_type and form_name of each definition, read from its source file, and
;;;; render the content text.  Loads eclector (through code-refs-scan), so the
;;;; worker must not import this file.

(defpackage #:cl-mcp/src/tools/clos-response-builders
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:top-level-forms-at)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list
                #:*note-stale*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:text-content)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:quote-meta-chars)
  (:export #:clos-report-p
           #:annotate-report-forms
           #:build-clos-describe-response
           #:*note-no-form-at-line*
           #:*note-unparseable*
           #:*note-different-definition*))

(in-package #:cl-mcp/src/tools/clos-response-builders)

(defparameter *note-no-form-at-line* "no top-level form starts at this line"
  "Note on a definition whose recorded line starts no top-level form in its file.")

(defparameter *note-unparseable* "file could not be parsed"
  "Note on a definition whose source file does not parse.")

(defparameter *note-different-definition*
  (concatenate 'string "the form on this line is a different definition; the file no longer "
               "has this one as loaded (reload for accurate results)")
  "Note on a definition whose recorded line starts a form defining something else:
the file was edited and reloaded, and this definition, still in the image, is no
longer in it.")

(defun %true-p (value)
  "True when VALUE, a JSON boolean, is true.  False arrives as YASON:FALSE
in-process and as NIL after the worker's JSON round trip."
  (and value (not (eq value yason:false))))

(defun clos-report-p (object)
  "True when OBJECT is a clos-describe report rather than an error result.
PROXY-TO-WORKER returns a crash notice or a worker error as a hash-table with
isError and content, which must reach the caller untouched."
  (and (hash-table-p object)
       (nth-value 1 (gethash "symbol_status" object))
       (not (%true-p (gethash "isError" object)))))

(defun %located-entries (report)
  "Return every object in REPORT that carries a source location: each generic
function and its methods, the class and its methods."
  (let ((entries '()))
    (dolist (gf (sequence->list (gethash "generic_functions" report)))
      (push gf entries)
      (dolist (method (sequence->list (gethash "methods" gf)))
        (push method entries)))
    (let ((class (gethash "class" report)))
      (when (hash-table-p class)
        (push class entries)
        (dolist (method (sequence->list (gethash "methods" class)))
          (push method entries))))
    (nreverse entries)))

(defun %add-note (entry note)
  "Append NOTE to ENTRY's note, separated by '; '."
  (let ((old (gethash "note" entry)))
    (setf (gethash "note" entry)
          (if (and (stringp old) (plusp (length old)))
              (format nil "~A; ~A" old note)
              note))))

(defun %base-name (text)
  "Return TEXT, a symbol name or (SETF name) as a report or a signature writes
it, without its package prefix: the text after the last colon, and
(SETF PKG::X) as (SETF X)."
  (let ((end (length text)))
    (if (and (> end 7)
             (string-equal "(SETF " text :end2 6)
             (char= #\) (char text (1- end))))
        (format nil "(SETF ~A)" (%base-name (subseq text 6 (1- end))))
        (let ((colon (position #\: text :from-end t)))
          (if colon (subseq text (1+ colon)) text)))))

(defun %same-name-p (a b)
  "True when A and B, names as strings, are equal without package prefixes and
ignoring case."
  (and (stringp a) (stringp b)
       (string-equal (%base-name a) (%base-name b))))

(defun %eql-inner-text (text)
  "Return the datum text inside TEXT, an (EQL ...) specializer string --
\"(EQL)\" or \"(EQL <datum>)\" -- between \"(EQL \" and the final \")\"; NIL
when TEXT does not have that shape.  \"(EQL)\" gives the empty string: an
older signature, or one whose datum could not be printed."
  (when (and (stringp text) (>= (length text) 5)
             (string-equal "(EQL" text :end2 4)
             (char= #\) (char text (1- (length text)))))
    (string-trim " " (subseq text 4 (1- (length text))))))

(defun %eql-datum-comparable-p (text)
  "True when TEXT, an (EQL ...) specializer's inner datum text, holds only
characters that can appear in symbol, keyword or number syntax, so it can be
compared against another such text reliably.  A string, a list or a character
literal holds a double quote, a parenthesis, `#', a backslash, or a space
(from more than one token), and returns NIL for those."
  (every (lambda (ch) (and (not (find ch "\"()#\\ ")) (not (char= ch #\Tab)))) text))

(defun %same-specializers-p (entry-specializers form-specializers)
  "True when ENTRY-SPECIALIZERS, a method entry's, match FORM-SPECIALIZERS, a
defmethod signature's, pairwise: class names by %SAME-NAME-P, and an EQL
specializer -- both texts start \"(EQL \" -- by its datum: the inner texts
(%EQL-INNER-TEXT), compared like a name (package prefixes dropped token-wise,
case-insensitively) when both hold only symbol/keyword/number syntax
(%EQL-DATUM-COMPARABLE-P).  Either inner text empty, or either not comparable
that way -- a string, a list, or a character literal -- counts as a match:
those cases cannot be told apart reliably and must not report a false
mismatch.  A specializer that is EQL on only one side never matches."
  (and (= (length entry-specializers) (length form-specializers))
       (every (lambda (entry-specializer form-specializer)
                (and (stringp entry-specializer)
                     (let ((entry-inner (%eql-inner-text entry-specializer)))
                       (if entry-inner
                           (let ((form-inner (and (stringp form-specializer)
                                                  (%eql-inner-text form-specializer))))
                             (and form-inner
                                  (or (zerop (length entry-inner))
                                      (zerop (length form-inner))
                                      (not (%eql-datum-comparable-p entry-inner))
                                      (not (%eql-datum-comparable-p form-inner))
                                      (%same-name-p entry-inner form-inner))))
                           (%same-name-p entry-specializer form-specializer)))))
              entry-specializers form-specializers)))

(defun %form-describes-entry-p (form entry)
  "True unless FORM, a TOP-LEVEL-FORMS-AT value (FORM-TYPE FORM-NAME SIGNATURE)
for the line ENTRY was recorded on, is a definition that cannot be ENTRY.

The line comes from the image, the form from the file as it is now, so after a
method is renamed in place and the file reloaded, the old method still in the
image points at the new one's form.  Names are compared by %SAME-NAME-P:
- a method (it has specializers) of kind method: a defmethod needs its generic
  function's name, its qualifiers and its specializers (%SAME-SPECIALIZERS-P);
  a defgeneric, holding the method as a :method option, needs the name
- a slot reader or writer: a defclass or define-condition needs the name of the
  class specialized on, the first specializer of a reader, the second of a writer
- a generic function (it has a lambda_list): a defgeneric needs its name
- a class (it has a metaclass): a defclass, define-condition or defstruct needs
  its name
Any other form -- a user macro, a PROGN, a form too malformed for a signature --
cannot be checked and is accepted, as is a method whose generic function the
worker could not read."
  (destructuring-bind (form-type form-name &optional signature) form
    (declare (ignore form-name))
    (flet ((type-p (&rest types)
             (and signature (member form-type types :test #'equal)))
           (named-p (name)
             (%same-name-p name (getf signature :name))))
      (cond
        ((nth-value 1 (gethash "specializers" entry))
         (let ((kind (gethash "kind" entry))
               (generic-function (gethash "generic_function" entry))
               (specializers (sequence->list (gethash "specializers" entry))))
           (cond
             ((not (stringp generic-function)) t)
             ((equal kind "method")
              (cond
                ((type-p "defmethod")
                 (let ((qualifiers (sequence->list (gethash "qualifiers" entry))))
                   (and (named-p generic-function)
                        (= (length qualifiers) (length (getf signature :qualifiers)))
                        (every #'equalp qualifiers (getf signature :qualifiers))
                        (%same-specializers-p specializers (getf signature :specializers)))))
                ((type-p "defgeneric") (named-p generic-function))
                (t t)))
             ((and (member kind '("reader" "writer") :test #'equal)
                   (type-p "defclass" "define-condition"))
              (named-p (nth (if (equal kind "reader") 0 1) specializers)))
             (t t))))
        ((nth-value 1 (gethash "lambda_list" entry))
         (if (type-p "defgeneric") (named-p (gethash "name" entry)) t))
        ((nth-value 1 (gethash "metaclass" entry))
         (if (type-p "defclass" "define-condition" "defstruct")
             (named-p (gethash "name" entry))
             t))
        (t t)))))

(defun %legacy-eql-datum-text (datum)
  "Return DATUM, a CODE-REFS-SCAN %SOURCE-EQL-DATUM tagged plist (spec 3.3),
rendered the way this file's predecessor (value-based %DEFINITION-SIGNATURE)
used to render an EQL specializer's datum -- close enough for
%SAME-SPECIALIZERS-P's text-based, case-insensitive, prefix-stripping
comparison -- or NIL when DATUM cannot be rendered that way (UNVERIFIABLE),
matching the old \"cannot print\" case and its safe (EQL) fallback."
  (case (getf datum :kind)
    (:keyword (format nil ":~A" (getf datum :name)))
    (:integer (getf datum :value))
    (:ratio (format nil "~A/~A" (getf datum :numerator) (getf datum :denominator)))
    (:character (format nil "#\\~A" (getf datum :value)))
    (:boolean (getf datum :value))
    (:symbol (getf datum :token))
    (t nil)))

(defun %legacy-specializer-text (specializer)
  "Return SPECIALIZER, a CODE-REFS-SCAN %SOURCE-SPECIALIZER tagged plist,
rendered the way this file's predecessor rendered a DEFMETHOD specializer,
or NIL when SPECIALIZER is UNVERIFIABLE -- the caller then treats the whole
form as un-checkable, as the old code did for anything it could not render."
  (case (getf specializer :kind)
    (:class (getf specializer :token))
    (:eql (let ((text (%legacy-eql-datum-text (getf specializer :datum))))
            (if text (format nil "(EQL ~A)" text) "(EQL)")))
    (t nil)))

(defun %legacy-name-text (name)
  "Return NAME, a CODE-REFS-SCAN %SOURCE-NAME tagged plist, rendered the way
this file's predecessor rendered a definition's name, or NIL when NAME is
absent (a malformed definer %DEFINITION-SOURCE-SIGNATURE could not name)."
  (and name
       (if (getf name :setf)
           (format nil "(SETF ~A)" (getf name :token))
           (getf name :token))))

(defun %legacy-signature (signature)
  "Return SIGNATURE, a CODE-REFS-SCAN %DEFINITION-SOURCE-SIGNATURE plist
(spec 3.2), as the (:NAME :QUALIFIERS :SPECIALIZERS) plist
%FORM-DESCRIBES-ENTRY-P still expects -- a provisional bridge kept only
until A4 replaces that predicate with worker-verified identity matching.
NIL when SIGNATURE's kind is :OTHER, its name is missing, or (for a
DEFMETHOD) any specializer is UNVERIFIABLE: %FORM-DESCRIBES-ENTRY-P then
accepts the form unchecked, as the old code did for anything it could not
confidently render as text."
  (let ((name (%legacy-name-text (getf signature :name))))
    (case (getf signature :kind)
      (:defmethod
        (and name
             (let ((specializers (mapcar #'%legacy-specializer-text
                                         (getf signature :specializers))))
               (and (notany #'null specializers)
                    (list :name name
                          :qualifiers (mapcar (lambda (q) (getf q :token))
                                              (getf signature :qualifiers))
                          :specializers specializers)))))
      ((:defgeneric :defclass :define-condition :defstruct)
       (and name (list :name name)))
      (t nil))))

(defun annotate-report-forms (report)
  "Fill in the form_type, form_name and note of every located object in
REPORT from its source file, then remove abs_path from each; return REPORT.

Each file is read once (TOP-LEVEL-FORMS-AT), which now returns every
top-level form starting on a line, not just one.  An object gets the form
that starts on its line only when exactly one form starts there and that
form describes something else (%FORM-DESCRIBES-ENTRY-P, fed a
%LEGACY-SIGNATURE bridge from the new token-based source_signature): the
object then gets *NOTE-DIFFERENT-DEFINITION* and no form, so its form_name
never leads an edit to another definition.  Zero forms on the line, more
than one (ambiguous), or no note-worthy match falls through the same way:
the file does not parse, or it changed since it was loaded (stale), or
neither, in which case the recorded line simply starts no (uniquely
identifiable) form.  A file the read policy refuses gets neither form nor
note -- the text still gives path:line.  A4 replaces this provisional
one-form-per-line rule with worker-verified identity matching across every
candidate on the line (spec 3.4)."
  (let ((by-file (make-hash-table :test #'equal)))
    (dolist (entry (%located-entries report))
      (let ((abs-path (gethash "abs_path" entry)))
        (when (and (stringp abs-path) (integerp (gethash "line" entry)))
          (push entry (gethash abs-path by-file)))))
    (maphash (lambda (abs-path entries)
               (multiple-value-bind (table failure)
                   (top-level-forms-at abs-path
                                       (mapcar (lambda (entry) (gethash "line" entry))
                                               entries))
                 (dolist (entry entries)
                   (let* ((forms (gethash (gethash "line" entry) table))
                          (form (and forms (null (rest forms))
                                    (list (getf (first forms) :form-type)
                                          (getf (first forms) :form-name)
                                          (%legacy-signature (getf (first forms) :signature))))))
                     (cond
                       ((and form (%form-describes-entry-p form entry))
                        (setf (gethash "form_type" entry) (first form)
                              (gethash "form_name" entry) (second form)))
                       (form (%add-note entry *note-different-definition*))
                       ((eq failure :denied))
                       (failure
                        (%add-note entry (format nil "~A: ~A" *note-unparseable* failure)))
                       ((%true-p (gethash "stale" entry)) (%add-note entry *note-stale*))
                       (t (%add-note entry *note-no-form-at-line*)))))))
             by-file)
    (dolist (entry (%located-entries report))
      (remhash "abs_path" entry))
    report))

(defun %home-package-name (report)
  "Return the package name of REPORT's resolved symbol, or NIL."
  (let* ((resolved (gethash "resolved_symbol" report))
         (colon (and (stringp resolved) (position #\: resolved))))
    (and colon (plusp colon) (subseq resolved 0 colon))))

(defun %short (text home)
  "Return TEXT, names printed fully qualified, without the prefixes of HOME and
COMMON-LISP, as a reader in HOME would write them."
  (let ((result (or text "")))
    (dolist (package (remove nil (list home "COMMON-LISP")) result)
      (setf result (regex-replace-all
                    (format nil "(?<![^\\s(])~A::?" (quote-meta-chars package))
                    result "")))))

(defun %location-text (entry)
  "Return where ENTRY is defined: PATH:LINE (FORM-TYPE FORM-NAME), with the
note in brackets, or (no source)."
  (let ((path (gethash "path" entry))
        (line (gethash "line" entry))
        (form-type (gethash "form_type" entry))
        (form-name (gethash "form_name" entry))
        (note (gethash "note" entry)))
    (concatenate 'string
                 (if path
                     (format nil "~A~@[:~D~]~@[ (~A)~]"
                             path line
                             (and form-type (format nil "~A~@[ ~A~]" form-type form-name)))
                     "(no source)")
                 (if note (format nil "  [~A]" note) ""))))

(defun %method-signature (method home &key with-name class-name)
  "Return METHOD's signature: [NAME] QUALIFIERS (SPECIALIZERS) [kind], plus
'via CLASS' when it was found through a superclass of CLASS-NAME."
  (let ((qualifiers (sequence->list (gethash "qualifiers" method)))
        (specializers (sequence->list (gethash "specializers" method)))
        (kind (gethash "kind" method))
        (via (gethash "via" method)))
    (%short (format nil "~@[~A ~]~{~A ~}(~{~A~^ ~})~:[ [~A]~;~*~]~@[ via ~A~]"
                    (and with-name (gethash "generic_function" method))
                    qualifiers specializers
                    (equal kind "method") kind
                    (and via (not (equal via class-name)) via))
            home)))

(defun %write-methods (stream methods home &key with-name class-name)
  "Write one aligned line per method in METHODS to STREAM."
  (let* ((signatures (mapcar (lambda (method)
                               (%method-signature method home
                                                  :with-name with-name
                                                  :class-name class-name))
                             methods))
         (width (reduce #'max signatures :key #'length :initial-value 0)))
    (loop for method in methods
          for signature in signatures
          do (format stream "  ~vA  ~A~%" width signature (%location-text method)))))

(defun %write-more (stream entry)
  "Write how many of ENTRY's methods were left out, when any were."
  (let ((count (or (gethash "method_count" entry) 0))
        (shown (length (sequence->list (gethash "methods" entry)))))
    (when (> count shown)
      (format stream "  … and ~D more (raise limit to see them)~%" (- count shown)))))

(defun %write-generic-function (stream gf home)
  "Write the text for GF, one generic_functions entry, to STREAM."
  (let ((methods (sequence->list (gethash "methods" gf))))
    (format stream "Generic function ~A ~A — ~@[~(~A~) combination, ~]~D method~:P~%"
            (gethash "name" gf)
            (%short (gethash "lambda_list" gf) home)
            (gethash "method_combination" gf)
            (or (gethash "method_count" gf) 0))
    (when (gethash "documentation" gf)
      (format stream "~A~%" (gethash "documentation" gf)))
    (if (gethash "path" gf)
        (format stream "Defined at ~A~%" (%location-text gf))
        (format stream "No defgeneric: created implicitly by a defmethod or a slot accessor.~%"))
    (%write-methods stream methods home)
    (%write-more stream gf)))

(defun %accessor-words (slot home)
  "Return the reader, writer and accessor words for SLOT: accessor X when X
reads it and (SETF X) writes it."
  (let ((readers (mapcar (lambda (name) (%short name home))
                         (sequence->list (gethash "readers" slot))))
        (writers (mapcar (lambda (name) (%short name home))
                         (sequence->list (gethash "writers" slot))))
        (words '()))
    (dolist (reader readers)
      (let ((writer (format nil "(SETF ~A)" reader)))
        (if (member writer writers :test #'string=)
            (progn (push (format nil "accessor ~A" reader) words)
                   (setf writers (remove writer writers :test #'string=)))
            (push (format nil "reader ~A" reader) words))))
    (dolist (writer writers)
      (push (format nil "writer ~A" writer) words))
    (nreverse words)))

(defun %slot-parts (slot class-name home)
  "Return (NAME ORIGIN ATTRIBUTES), the three columns of SLOT's text line:
its name, 'direct' or 'from CLASS' when a superclass of CLASS-NAME defines it,
and its initargs, initform, type, class allocation and accessors."
  (let ((from (gethash "from" slot))
        (type (gethash "type" slot)))
    (list (%short (gethash "name" slot) home)
          (if (and from (not (equal from class-name)))
              (format nil "from ~A" (%short from home))
              "direct")
          (format nil "~{ :initarg ~A~}~@[ :initform ~A~]~@[ :type ~A~]~
~:[~; :allocation :class~]~{  ~A~}"
                  (sequence->list (gethash "initargs" slot))
                  (gethash "initform" slot)
                  (and type (not (equal type "T")) type)
                  (equal (gethash "allocation" slot) "class")
                  (%accessor-words slot home)))))

(defun %write-class (stream class home)
  "Write the text for CLASS, the report's class entry, to STREAM."
  (let* ((name (gethash "name" class))
         (cpl (gethash "precedence_list" class))
         ;; Effective slots exist exactly when the precedence list does.  Test
         ;; that, not the slots: after the worker's JSON round trip an empty
         ;; array and null are both NIL.
         (slots (sequence->list (gethash (if cpl "effective_slots" "direct_slots") class)))
         (initargs (sequence->list (gethash "default_initargs" class)))
         (omitted (sequence->list (gethash "omitted_classes" class))))
    (format stream "Class ~A (~(~A~)~:[, not finalized~;~]) — ~A~%"
            name
            (%short (gethash "metaclass" class) home)
            (%true-p (gethash "finalized" class))
            (%location-text class))
    (when (gethash "documentation" class)
      (format stream "~A~%" (gethash "documentation" class)))
    (format stream "Superclasses: ~:[(none)~;~:*~{~A~^ ~}~]~%"
            (mapcar (lambda (c) (%short c home))
                    (sequence->list (gethash "direct_superclasses" class))))
    (format stream "Subclasses: ~:[(none)~;~:*~{~A~^ ~}~]~%"
            (mapcar (lambda (c) (%short c home))
                    (sequence->list (gethash "direct_subclasses" class))))
    (if cpl
        (format stream "Precedence: ~{~A~^ ~}~%"
                (mapcar (lambda (c) (%short c home)) (sequence->list cpl)))
        (format stream "Precedence: unavailable (undefined superclass ~{~A~^, ~})~%"
                (mapcar (lambda (c) (%short c home))
                        (sequence->list (gethash "undefined_superclasses" class)))))
    (format stream "~:[Direct slots~;Slots~] (~D):~%" cpl (length slots))
    (let* ((parts (mapcar (lambda (slot) (%slot-parts slot name home)) slots))
           (name-width (reduce #'max parts :key (lambda (p) (length (first p)))
                                           :initial-value 0))
           (origin-width (reduce #'max parts :key (lambda (p) (length (second p)))
                                             :initial-value 0)))
      (dolist (part parts)
        (format stream "~A~%"
                (string-right-trim " " (format nil "  ~vA  ~vA~A"
                                               name-width (first part)
                                               origin-width (second part)
                                               (third part))))))
    (format stream "Default initargs:~:[ (none)~;~]~%" initargs)
    (dolist (initarg initargs)
      (format stream "  ~A ~A~:[~; from ~A~]~%"
              (gethash "initarg" initarg)
              (%short (gethash "form" initarg) home)
              (not (equal (gethash "from" initarg) name))
              (%short (gethash "from" initarg) home)))
    (format stream "Methods (~D~@[; standard protocol on ~{~A~^, ~} omitted~]):~%"
            (or (gethash "method_count" class) 0)
            (and omitted (mapcar (lambda (c) (%short c home)) omitted)))
    (%write-methods stream (sequence->list (gethash "methods" class)) home
                    :with-name t :class-name name)
    (%write-more stream class)))

(defun %format-clos-report (report)
  "Return the content text for REPORT, an annotated clos-describe payload."
  (let ((status (gethash "symbol_status" report))
        (home (%home-package-name report))
        (gfs (sequence->list (gethash "generic_functions" report)))
        (class (gethash "class" report)))
    (with-output-to-string (s)
      (cond
        ((equal status "package_not_found")
         (format s "Package ~S not found (nothing was interned). ~
                    Load the system that defines it with load-system.~%"
                 (gethash "lookup_package" report)))
        ((equal status "not_found")
         (format s "Symbol ~S not found in ~A (nothing was interned). ~
                    Is the system loaded? Run load-system first.~%"
                 (gethash "lookup_name" report) (gethash "lookup_package" report)))
        ((and (null gfs) (not (hash-table-p class)))
         (if (equal (gethash "symbol_kind" report) "unbound")
             (format s "~A names nothing in this image. Is the system loaded?~%"
                     (gethash "resolved_symbol" report))
             (format s "~A names a ~A, not a generic function or class; ~
                        code-describe describes it.~%"
                     (gethash "resolved_symbol" report)
                     (gethash "symbol_kind" report))))
        (t
         (loop for (gf . more) on gfs
               do (%write-generic-function s gf home)
                  (when (or more (hash-table-p class))
                    (terpri s)))
         (when (hash-table-p class)
           (%write-class s class home))))
      (dolist (note (sequence->list (gethash "notes" report)))
        (format s "Note: ~A~%" note)))))

(defun build-clos-describe-response (report)
  "Return REPORT, a clos-describe payload, annotated and with its content text.

A result that is not a report (CLOS-REPORT-P), such as the error PROXY-TO-WORKER
returns when the worker crashed, is returned unchanged."
  (if (clos-report-p report)
      (progn
        (annotate-report-forms report)
        (setf (gethash "content" report)
              (text-content (%format-clos-report report)))
        report)
      report))
