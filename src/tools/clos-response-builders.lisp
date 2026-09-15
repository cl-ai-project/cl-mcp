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
           #:*note-unparseable*))

(in-package #:cl-mcp/src/tools/clos-response-builders)

(defparameter *note-no-form-at-line* "no top-level form starts at this line"
  "Note on a definition whose recorded line starts no top-level form in its file.")

(defparameter *note-unparseable* "file could not be parsed"
  "Note on a definition whose source file does not parse.")

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

(defun annotate-report-forms (report)
  "Fill in the form_type, form_name and note of every located object in
REPORT from its source file, then remove abs_path from each; return REPORT.

Each file is read once (TOP-LEVEL-FORMS-AT).  An object gets the form that
starts on its line.  When none does it gets a note instead: the file does not
parse, or it changed since it was loaded (stale), or neither, in which case the
recorded line simply starts no form.  A file the read policy refuses gets
neither form nor note -- the text still gives path:line."
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
                   (let ((form (gethash (gethash "line" entry) table)))
                     (cond
                       (form
                        (setf (gethash "form_type" entry) (car form)
                              (gethash "form_name" entry) (cdr form)))
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
        (format stream "No defgeneric: created by its first defmethod.~%"))
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
