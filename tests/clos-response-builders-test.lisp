;;;; tests/clos-response-builders-test.lisp
;;;;
;;;; Tests for cl-mcp/src/tools/clos-response-builders: annotating a
;;;; clos-describe report from its source files and rendering its text.

(defpackage #:cl-mcp/tests/clos-response-builders-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/clos-response-builders
                #:build-clos-describe-response
                #:annotate-report-forms
                #:clos-report-p
                #:*note-no-form-at-line*
                #:*note-unparseable*
                #:*note-different-definition*)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list
                #:*note-stale*)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:text-content)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*))

(in-package #:cl-mcp/tests/clos-response-builders-test)

(defparameter *fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-fixture.lisp")
  "CLOS definitions compiled so that SBCL records their source locations.")

(defun %load-fixture ()
  "Compile and load the fixture with its truename as the source namestring.
repl-eval's compilation unit would otherwise name the file \"repl-eval\"."
  (let ((truename (truename *fixture*)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %text (response)
  "Return the text of RESPONSE's first content part."
  (gethash "text" (elt (gethash "content" response) 0)))

(defun %method (&key (gf "PKG::AREA") qualifiers specializers (kind "method") slot via
                  path line form-type form-name note)
  "Return a synthetic method object."
  (make-ht "generic_function" gf
           "qualifiers" (coerce qualifiers 'vector)
           "specializers" (coerce specializers 'vector)
           "kind" kind "slot" slot "via" via
           "path" path "line" line "stale" yason:false
           "form_type" form-type "form_name" form-name "note" note))

(defun %slot (&key name from initargs initform (type "T") (allocation "instance")
                readers writers)
  "Return a synthetic slot object."
  (make-ht "name" name "from" from "initargs" (coerce initargs 'vector)
           "initform" initform "type" type "allocation" allocation
           "readers" (coerce readers 'vector) "writers" (coerce writers 'vector)
           "documentation" nil))

(defun %gf-report ()
  "Return a synthetic report on PKG::AREA, with 2 of its 3 methods."
  (make-ht "symbol" "pkg::area" "symbol_status" "found" "resolved_symbol" "PKG::AREA"
           "symbol_kind" "generic-function" "lookup_package" "PKG" "lookup_name" "AREA"
           "generic_functions"
           (vector (make-ht "name" "PKG::AREA" "lambda_list" "(SHAPE)" "documentation" "Area."
                            "method_combination" "STANDARD"
                            "path" "src/shapes.lisp" "line" 3 "stale" yason:false
                            "form_type" "defgeneric" "form_name" "area" "note" nil
                            "method_count" 3 "truncated" t
                            "methods"
                            (vector (%method :qualifiers '(":AROUND") :specializers '("PKG::CIRCLE")
                                             :path "src/shapes.lisp" :line 9
                                             :form-type "defmethod"
                                             :form-name "area :around ((s circle))")
                                    (%method :specializers '("PKG::SQUARE")
                                             :note "could not read this method: boom"))))
           "class" nil "limit" 2 "notes" (vector)))

(defun %class-report ()
  "Return a synthetic report on the unfinalized class PKG:CIRCLE."
  (make-ht "symbol" "pkg:circle" "symbol_status" "found" "resolved_symbol" "PKG:CIRCLE"
           "symbol_kind" "unbound" "lookup_package" "PKG" "lookup_name" "CIRCLE"
           "generic_functions" (vector)
           "class"
           (make-ht "name" "PKG:CIRCLE" "metaclass" "COMMON-LISP:STANDARD-CLASS"
                    "documentation" nil "finalized" yason:false
                    "path" "src/shapes.lisp" "line" 20 "stale" yason:false
                    "form_type" "defclass" "form_name" "circle" "note" nil
                    "direct_superclasses" (vector "PKG::SHAPE") "direct_subclasses" (vector)
                    "precedence_list" (vector "PKG:CIRCLE" "PKG::SHAPE"
                                              "COMMON-LISP:STANDARD-OBJECT" "COMMON-LISP:T")
                    "undefined_superclasses" (vector)
                    "direct_slots" (vector)
                    "effective_slots"
                    (vector (%slot :name "PKG::NAME" :from "PKG::SHAPE" :initargs '(":NAME")
                                   :initform "\"anon\"" :readers '("PKG:SHAPE-NAME"))
                            (%slot :name "PKG:RADIUS" :from "PKG:CIRCLE" :initargs '(":RADIUS")
                                   :initform "(RANDOM 10)" :type "REAL"
                                   :readers '("PKG:RADIUS") :writers '("(SETF PKG:RADIUS)"))
                            (%slot :name "PKG::REGISTRY" :from "PKG::SHAPE" :allocation "class"))
                    "default_initargs" (vector (make-ht "initarg" ":NAME" "form" "\"round\""
                                                        "from" "PKG::SHAPE"))
                    "method_count" 2 "truncated" yason:false
                    "methods"
                    (vector (%method :gf "PKG:RADIUS" :specializers '("PKG:CIRCLE") :kind "reader"
                                     :slot "PKG:RADIUS" :via "PKG:CIRCLE"
                                     :path "src/shapes.lisp" :line 20
                                     :form-type "defclass" :form-name "circle")
                            (%method :gf "(SETF PKG::LABEL)"
                                     :specializers '("COMMON-LISP:T" "PKG::SHAPE")
                                     :via "PKG::SHAPE" :path "src/shapes.lisp" :line 30
                                     :form-type "defmethod"
                                     :form-name "(setf label) (value (s shape))"))
                    "omitted_classes" (vector "COMMON-LISP:STANDARD-OBJECT" "COMMON-LISP:T"))
           "limit" 50
           "notes" (vector (concatenate 'string "not finalized; precedence list and slots "
                                        "were computed without finalizing the class"))))

(defun %round-trip (report)
  "Return REPORT as it arrives from the worker: JSON-encoded and parsed back,
so arrays are lists and false is NIL."
  (yason:parse (with-output-to-string (out) (yason:encode report out))))

(defun %lines (&rest lines)
  "Return LINES joined, each ended by a newline."
  (format nil "~{~A~%~}" lines))

(deftest clos-text-for-a-generic-function
  (testing "header, documentation, definition, aligned methods and the rest counted"
    (ok (equal (%lines
                "Generic function PKG::AREA (SHAPE) — standard combination, 3 methods"
                "Area."
                "Defined at src/shapes.lisp:3 (defgeneric area)"
                "  :AROUND (CIRCLE)  src/shapes.lisp:9 (defmethod area :around ((s circle)))"
                "  (SQUARE)          (no source)  [could not read this method: boom]"
                "  … and 1 more (raise limit to see them)")
               (%text (build-clos-describe-response (%gf-report)))))))

(deftest clos-text-for-a-class
  (testing "hierarchy, aligned slots, initargs and methods with their origin"
    (ok (equal (%lines
                (concatenate 'string "Class PKG:CIRCLE (standard-class, not finalized) — "
                             "src/shapes.lisp:20 (defclass circle)")
                "Superclasses: SHAPE"
                "Subclasses: (none)"
                "Precedence: CIRCLE SHAPE STANDARD-OBJECT T"
                "Slots (3):"
                "  NAME      from SHAPE :initarg :NAME :initform \"anon\"  reader SHAPE-NAME"
                (concatenate 'string "  RADIUS    direct     :initarg :RADIUS "
                             ":initform (RANDOM 10) :type REAL  accessor RADIUS")
                "  REGISTRY  from SHAPE :allocation :class"
                "Default initargs:"
                "  :NAME \"round\" from SHAPE"
                "Methods (2; standard protocol on STANDARD-OBJECT, T omitted):"
                "  RADIUS (CIRCLE) [reader]          src/shapes.lisp:20 (defclass circle)"
                (concatenate 'string "  (SETF LABEL) (T SHAPE) via SHAPE  src/shapes.lisp:30 "
                             "(defmethod (setf label) (value (s shape)))")
                (concatenate 'string "Note: not finalized; precedence list and slots were computed "
                             "without finalizing the class"))
               (%text (build-clos-describe-response (%class-report)))))))

(deftest clos-text-is-the-same-after-the-worker-round-trip
  (testing "lists for vectors and NIL for false render the same text"
    (dolist (make (list #'%gf-report #'%class-report))
      (ok (equal (%text (build-clos-describe-response (funcall make)))
                 (%text (build-clos-describe-response (%round-trip (funcall make)))))))))

(deftest clos-text-explains-an-empty-answer
  (flet ((text-for (&rest pairs)
           (%text (build-clos-describe-response
                   (apply #'make-ht "symbol" "x" "generic_functions" (vector) "class" nil
                          "notes" (vector) pairs)))))
    (testing "missing symbol and missing package"
      (ok (search "Symbol \"FOO\" not found in PKG (nothing was interned)"
                  (text-for "symbol_status" "not_found" "lookup_name" "FOO"
                            "lookup_package" "PKG")))
      (ok (search "Package \"NOPE\" not found (nothing was interned)"
                  (text-for "symbol_status" "package_not_found" "lookup_package" "NOPE"))))
    (testing "a symbol naming neither a class nor a generic function"
      (ok (search (concatenate 'string "COMMON-LISP:CAR names a function, not a generic function "
                               "or class; code-describe describes it.")
                  (text-for "symbol_status" "found" "resolved_symbol" "COMMON-LISP:CAR"
                            "symbol_kind" "function")))
      (ok (search "PKG::X names nothing in this image. Is the system loaded?"
                  (text-for "symbol_status" "found" "resolved_symbol" "PKG::X"
                            "symbol_kind" "unbound"))))))

(deftest build-clos-describe-response-passes-errors-through
  (testing "a crash notice or worker error is returned untouched"
    (let ((error-result (make-ht "isError" t "content" (text-content "Worker error: boom"))))
      (ok (not (clos-report-p error-result)))
      (ok (eq error-result (build-clos-describe-response error-result))))))

(deftest annotate-report-forms-reads-the-fixture
  (testing "each definition gets the form starting on its line, and abs_path goes"
    (%load-fixture)
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (report (annotate-report-forms (clos-describe-report "cl-mcp-clos-fixture:circle")))
           (class (gethash "class" report)))
      (ok (equal '("defclass" "circle")
                 (list (gethash "form_type" class) (gethash "form_name" class))))
      (ok (not (nth-value 1 (gethash "abs_path" class))))
      (ok (equal `(("defmethod" "area :around ((shape circle))")
                   ("defmethod" "area ((shape circle))")
                   ("defclass" "circle")
                   ("defclass" "circle")
                   ("defmethod" ,(concatenate 'string "describe-shape ((shape shape) "
                                              "&optional (stream *standard-output*) verbose)"))
                   ("defmethod" "(setf label) (value (shape shape))")
                   ("defclass" "shape"))
                 (mapcar (lambda (method)
                           (list (gethash "form_type" method) (gethash "form_name" method)))
                         (sequence->list (gethash "methods" class))))))))

(deftest annotate-report-forms-explains-a-missing-form
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (flet ((annotated (abs-path line &key stale)
             (let ((entry (make-ht "abs_path" abs-path "path" "x.lisp" "line" line
                                   "stale" (if stale t yason:false)
                                   "form_type" nil "form_name" nil "note" nil)))
               (annotate-report-forms
                (make-ht "symbol_status" "found" "generic_functions" (vector)
                         "class" (make-ht "abs_path" abs-path "path" "x.lisp" "line" line
                                          "stale" (if stale t yason:false)
                                          "form_type" nil "form_name" nil "note" nil
                                          "methods" (vector entry))))
               entry)))
      (testing "a line that starts no form"
        (let ((entry (annotated (namestring (truename *fixture*)) 2)))
          (ok (null (gethash "form_name" entry)))
          (ok (equal *note-no-form-at-line* (gethash "note" entry)))))
      (testing "a stale file"
        (ok (equal *note-stale*
                   (gethash "note" (annotated (namestring (truename *fixture*)) 2 :stale t)))))
      (testing "a file that does not parse"
        (let ((file (asdf/system:system-relative-pathname
                     :cl-mcp "tests/tmp/clos-unparseable.lisp")))
          (ensure-directories-exist file)
          (with-open-file (out file :direction :output :if-exists :supersede)
            (format out "(defparameter *x* #.(+ 1 2))~%"))
          (unwind-protect
               (let ((note (gethash "note" (annotated (namestring (truename file)) 1))))
                 (ok (and note (eql 0 (search *note-unparseable* note))) note))
            (ignore-errors (delete-file file)))))
      (testing "a file outside the readable paths gets no note"
        (ok (null (gethash "note" (annotated "/nonexistent-cl-mcp-dir/x.lisp" 1))))))))

(deftest annotate-report-forms-rejects-a-different-definition
  (let ((*project-root* (asdf:system-source-directory :cl-mcp))
        (file (asdf/system:system-relative-pathname :cl-mcp "tests/tmp/clos-renamed.lisp")))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      ;; Lines 3, 6, 9 and 12 start forms.
      (format out "(in-package #:cl-user)~%~%~
(defmethod area ((s ellipse))~%  :ellipse)~%~%~
(defmethod area :around ((s circle))~%  (call-next-method))~%~%~
(defclass ellipse ()~%  ((radius :reader radius)))~%~%~
(defgeneric perimeter (shape))~%"))
    (unwind-protect
         (let ((abs-path (namestring (truename file))))
           (labels ((report-of (entry place)
                      (setf (gethash "abs_path" entry) abs-path)
                      (make-ht "symbol_status" "found"
                               "generic_functions" (if (eq place :generic-function)
                                                       (vector entry)
                                                       (vector))
                               "class" (case place
                                         (:class entry)
                                         (:method (make-ht "methods" (vector entry))))))
                    (annotated (entry place)
                      (annotate-report-forms (report-of entry place))
                      entry)
                    (form-of (entry)
                      (list (gethash "form_type" entry) (gethash "form_name" entry)
                            (gethash "note" entry)))
                    (method-at (line &rest keys)
                      (apply #'%method :path "x.lisp" :line line keys)))
             (let ((different (list nil nil *note-different-definition*)))
               (testing "a method whose specializer differs from the defmethod on its line"
                 (ok (equal different
                            (form-of (annotated (method-at 3 :specializers '("PKG::CIRCLE"))
                                                :method))))
                 (ok (equal '("defmethod" "area ((s ellipse))" nil)
                            (form-of (annotated (method-at 3 :specializers '("PKG:ELLIPSE"))
                                                :method)))
                     "the method the line does define keeps its form"))
               (testing "the same with lists for vectors, as from the worker"
                 (let* ((report (let ((yason:*parse-json-arrays-as-vectors* nil))
                                  (%round-trip (report-of (method-at 3 :specializers
                                                                     '("PKG::CIRCLE"))
                                                          :method))))
                        (entry (elt (gethash "methods" (gethash "class" report)) 0)))
                   (ok (listp (gethash "specializers" entry)))
                   (annotate-report-forms report)
                   (ok (equal different (form-of entry)))))
               (testing "a method whose qualifiers or generic function differ"
                 (ok (equal different
                            (form-of (annotated (method-at 6 :specializers '("PKG::CIRCLE"))
                                                :method)))
                     "a primary method on the :around method's line")
                 (ok (equal '("defmethod" "area :around ((s circle))" nil)
                            (form-of (annotated (method-at 6 :qualifiers '(":AROUND")
                                                             :specializers '("PKG::CIRCLE"))
                                                :method))))
                 (ok (equal different
                            (form-of (annotated (method-at 3 :gf "PKG::PERIMETER"
                                                             :specializers '("PKG::ELLIPSE"))
                                                :method)))))
               (testing "a slot reader of another class on a defclass line"
                 (ok (equal different
                            (form-of (annotated (method-at 9 :gf "PKG::RADIUS" :kind "reader"
                                                             :specializers '("PKG::CIRCLE"))
                                                :method))))
                 (ok (equal '("defclass" "ellipse" nil)
                            (form-of (annotated (method-at 9 :gf "PKG::RADIUS" :kind "writer"
                                                             :specializers
                                                             '("COMMON-LISP:T" "PKG::ELLIPSE"))
                                                :method)))
                     "a writer's class is its second specializer"))
               (testing "a class or generic function of another name"
                 (flet ((located (&rest pairs)
                          (apply #'make-ht "path" "x.lisp" "stale" yason:false
                                 "form_type" nil "form_name" nil "note" nil
                                 "methods" (vector) pairs)))
                   (ok (equal different
                              (form-of (annotated (located "name" "PKG:CIRCLE" "line" 9
                                                           "metaclass" "STANDARD-CLASS")
                                                  :class))))
                   (ok (equal different
                              (form-of (annotated (located "name" "PKG::AREA" "line" 12
                                                           "lambda_list" "(SHAPE)")
                                                  :generic-function))))
                   (ok (equal '("defgeneric" "perimeter" nil)
                              (form-of (annotated (located "name" "PKG::PERIMETER" "line" 12
                                                           "lambda_list" "(SHAPE)")
                                                  :generic-function)))))))))
      (ignore-errors (delete-file file)))))

(deftest annotate-report-forms-compares-the-eql-datum
  (let ((*project-root* (asdf:system-source-directory :cl-mcp))
        (file (asdf/system:system-relative-pathname :cl-mcp "tests/tmp/clos-eql-renamed.lisp")))
    (ensure-directories-exist file)
    (labels ((report-of (entry)
               (setf (gethash "abs_path" entry) (namestring (truename file)))
               (make-ht "symbol_status" "found" "generic_functions" (vector)
                        "class" (make-ht "methods" (vector entry))))
             (annotated (entry)
               (annotate-report-forms (report-of entry))
               entry)
             (form-of (entry)
               (list (gethash "form_type" entry) (gethash "form_name" entry)
                     (gethash "note" entry))))
      (unwind-protect
           (progn
             (testing "a method re-specialized in place is not handed to the old EQL entry"
               (with-open-file (out file :direction :output :if-exists :supersede)
                 (format out "(in-package #:cl-user)~%~%(defmethod area ((s (eql :new))) 1)~%"))
               (ok (equal (list nil nil *note-different-definition*)
                          (form-of (annotated (%method :specializers '("(EQL :OLD)")
                                                       :path "x.lisp" :line 3))))))
             (testing "the same entry against its own datum keeps its form_name"
               (with-open-file (out file :direction :output :if-exists :supersede)
                 (format out "(in-package #:cl-user)~%~%(defmethod area ((s (eql :old))) 1)~%"))
               (ok (equal (list "defmethod" "area ((s (eql :old)))" nil)
                          (form-of (annotated (%method :specializers '("(EQL :OLD)")
                                                       :path "x.lisp" :line 3)))))))
        (ignore-errors (delete-file file))))))

(deftest clos-describe-form-names-work-in-lisp-edit-form
  (testing "every form_name the fixture's reports carry finds that very form"
    (%load-fixture)
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (lines (uiop:read-file-lines *fixture*))
          (checked 0))
      (dolist (designator '("cl-mcp-clos-fixture:area" "cl-mcp-clos-fixture:circle"
                            "cl-mcp-clos-fixture:square" "cl-mcp-clos-fixture:label"
                            "cl-mcp-clos-fixture:combine" "cl-mcp-clos-fixture:describe-shape"
                            "cl-mcp-clos-fixture:probe-error" "cl-mcp-clos-fixture:point"
                            "cl-mcp-clos-fixture:radius"))
        (let* ((report (build-clos-describe-response (clos-describe-report designator)))
               (class (gethash "class" report))
               (entries (append (loop for gf
                                        in (sequence->list (gethash "generic_functions" report))
                                      collect gf
                                      append (sequence->list (gethash "methods" gf)))
                                (and (hash-table-p class)
                                     (cons class (sequence->list (gethash "methods" class)))))))
          (dolist (entry entries)
            (let ((path (gethash "path" entry)))
              (when (and path (search "tests/fixtures/clos-fixture.lisp" path))
                (incf checked)
                (let* ((result (lisp-edit-form
                                :file-path (namestring (truename *fixture*))
                                :form-type (gethash "form_type" entry)
                                :form-name (gethash "form_name" entry)
                                :operation "delete"
                                :dry-run t))
                       (original (gethash "original" result)))
                  (ok (equal (nth (1- (gethash "line" entry)) lines)
                             (subseq original 0 (or (position #\Newline original)
                                                    (length original))))
                      (format nil "~A ~A" (gethash "form_type" entry)
                              (gethash "form_name" entry)))))))))
      (ok (> checked 25) (format nil "~D definitions checked" checked)))))
