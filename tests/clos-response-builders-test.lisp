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
                #:*reason-verification-unavailable*
                #:*reason-not-locatable*
                #:*reason-not-readable*
                #:*reason-no-source-line*)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/clos-verify-core
                #:verify-entries)
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

(defun %compile-and-load (path)
  "Compile and load the fixture at PATH with its truename as the source
namestring.  repl-eval's compilation unit would otherwise name the file
\"repl-eval\"."
  (let ((truename (truename path)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %load-fixture ()
  "Compile and load the fixture with its truename as the source namestring."
  (%compile-and-load *fixture*))

(defun %verify-inline (entries)
  "A VERIFY-FN calling CLOS-VERIFY-CORE:VERIFY-ENTRIES directly, as
CLOS.LISP's inline (worker pool disabled) path does."
  (verify-entries entries))

(defun %text (response)
  "Return the text of RESPONSE's first content part."
  (gethash "text" (elt (gethash "content" response) 0)))

(defun %method (&key (gf "PKG::AREA") qualifiers specializers (kind "method") slot via
                  path line form-type form-name note edit-unit
                  (source-match (and form-type "matched")) source-match-reason)
  "Return a synthetic method object."
  (make-ht "generic_function" gf
           "qualifiers" (coerce qualifiers 'vector)
           "specializers" (coerce specializers 'vector)
           "kind" kind "slot" slot "via" via
           "path" path "line" line "stale" yason:false
           "form_type" form-type "form_name" form-name "note" note
           "source_match" source-match "source_match_reason" source-match-reason
           "edit_unit" edit-unit))

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
                            "source_match" "matched" "source_match_reason" nil
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
                    "source_match" "matched" "source_match_reason" nil
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
                                     :form-type "defclass" :form-name "circle"
                                     :edit-unit "defclass")
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
               (%text (build-clos-describe-response (%gf-report) #'%verify-inline))))))

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
                (concatenate 'string "  RADIUS (CIRCLE) [reader]          src/shapes.lisp:20 "
                             "(defclass circle)  [edit_unit: defclass]")
                (concatenate 'string "  (SETF LABEL) (T SHAPE) via SHAPE  src/shapes.lisp:30 "
                             "(defmethod (setf label) (value (s shape)))")
                (concatenate 'string "Note: not finalized; precedence list and slots were computed "
                             "without finalizing the class"))
               (%text (build-clos-describe-response (%class-report) #'%verify-inline))))))

(deftest clos-text-is-the-same-after-the-worker-round-trip
  (testing "lists for vectors and NIL for false render the same text"
    (dolist (make (list #'%gf-report #'%class-report))
      (ok (equal (%text (build-clos-describe-response (funcall make) #'%verify-inline))
                 (%text (build-clos-describe-response (%round-trip (funcall make))
                                                       #'%verify-inline)))))))

(deftest clos-text-explains-an-empty-answer
  (flet ((text-for (&rest pairs)
           (%text (build-clos-describe-response
                   (apply #'make-ht "symbol" "x" "generic_functions" (vector) "class" nil
                          "notes" (vector) pairs)
                   #'%verify-inline))))
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
      (ok (eq error-result (build-clos-describe-response error-result #'%verify-inline))))))

(deftest annotate-report-forms-reads-the-fixture
  (testing "each definition gets the form starting on its line, and abs_path goes"
    (%load-fixture)
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (report (annotate-report-forms (clos-describe-report "cl-mcp-clos-fixture:circle")
                                           #'%verify-inline))
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

(deftest annotate-report-forms-never-matches-a-stale-entry
  (testing "a stale entry never becomes matched even when its form does verify"
    (%load-fixture)
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (report (clos-describe-report "cl-mcp-clos-fixture:circle"))
           (class (gethash "class" report)))
      (setf (gethash "stale" class) t)
      (annotate-report-forms report #'%verify-inline)
      (ok (equal "unverified" (gethash "source_match" class)))
      (ok (equal *note-stale* (gethash "source_match_reason" class)))
      (ok (null (gethash "form_type" class))))))

(deftest annotate-report-forms-falls-back-when-verify-fn-is-unavailable
  (testing "a verify-fn that signals never blocks the report"
    (%load-fixture)
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (report (clos-describe-report "cl-mcp-clos-fixture:circle"))
           (class (gethash "class" report)))
      (annotate-report-forms report (lambda (entries)
                                       (declare (ignore entries))
                                       (error "boom")))
      (ok (equal "unverified" (gethash "source_match" class)))
      (ok (equal *reason-verification-unavailable* (gethash "source_match_reason" class)))
      (ok (null (gethash "form_type" class))))))

(deftest annotate-report-forms-reports-no-source-line
  (testing "an entry with a source file but no recorded line is unverified, not skipped"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (method (%method :path "src/shapes.lisp" :line nil)))
      (setf (gethash "abs_path" method) (namestring (truename *fixture*)))
      (let* ((report (make-ht "symbol" "pkg::area" "symbol_status" "found"
                              "resolved_symbol" "PKG::AREA" "symbol_kind" "generic-function"
                              "lookup_package" "PKG" "lookup_name" "AREA"
                              "generic_functions"
                              (vector (make-ht "name" "PKG::AREA" "lambda_list" "(SHAPE)"
                                               "documentation" nil "method_combination" "STANDARD"
                                               "path" "src/shapes.lisp" "line" 3
                                               "stale" yason:false
                                               "form_type" "defgeneric" "form_name" "area"
                                               "note" nil "source_match" "matched"
                                               "source_match_reason" nil
                                               "method_count" 1 "truncated" nil
                                               "methods" (vector method)))
                              "class" nil "limit" 50 "notes" (vector)))
             (text (%text (build-clos-describe-response report #'%verify-inline))))
        (ok (equal "unverified" (gethash "source_match" method)))
        (ok (equal *reason-no-source-line* (gethash "source_match_reason" method)))
        (ok (null (gethash "form_type" method)))
        (ok (null (gethash "form_name" method)))
        (ok (search "src/shapes.lisp [unverified: no source line recorded]" text))))))

(deftest annotate-report-forms-clamps-an-unexpected-verifier-status
  (testing "a status the three-word contract doesn't define is clamped to unverified"
    (%load-fixture)
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (report (clos-describe-report "cl-mcp-clos-fixture:circle"))
           (class (gethash "class" report)))
      (annotate-report-forms
       report
       (lambda (entries)
         (make-ht "results"
                  (map 'vector
                       (lambda (e) (make-ht "id" (gethash "id" e) "status" "weird-status"
                                            "reason" nil "candidate_index" nil))
                       (sequence->list entries)))))
      (ok (equal "unverified" (gethash "source_match" class)))
      (ok (search "unexpected verifier status" (gethash "source_match_reason" class)))
      (ok (null (gethash "form_type" class))))))

(deftest annotate-report-forms-reports-a-real-mismatch
  (testing "a method whose specializer changed on disk after compilation becomes mismatched"
    (let ((file (asdf/system:system-relative-pathname
                 :cl-mcp "tests/tmp/clos-mismatch-round-trip.lisp"))
          (header (concatenate 'string "(defpackage #:cl-mcp-clos-mismatch-fixture (:use #:cl))~%"
                               "(in-package #:cl-mcp-clos-mismatch-fixture)~%~%"
                               "(defclass box () ())~%~%"
                               "(defgeneric bulk (x))~%~%")))
      (ensure-directories-exist file)
      (unwind-protect
           (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
             (with-open-file (out file :direction :output :if-exists :supersede)
               (format out (concatenate 'string header "(defmethod bulk ((x box)) :boxed)~%")))
             (%compile-and-load file)
             ;; Rewrite the file so the recorded line now defines something else,
             ;; without reloading: the in-image identity is now stale relative to
             ;; what is on disk (a genuine mismatch, spec 3.1).
             (with-open-file (out file :direction :output :if-exists :supersede)
               (format out (concatenate 'string header "(defmethod bulk ((x integer)) :int)~%")))
             (let* ((report (clos-describe-report "cl-mcp-clos-mismatch-fixture:bulk"))
                    (gf (elt (sequence->list (gethash "generic_functions" report)) 0))
                    (method (elt (sequence->list (gethash "methods" gf)) 0))
                    (text (%text (build-clos-describe-response report #'%verify-inline))))
               (ok (equal "mismatched" (gethash "source_match" method)))
               (ok (stringp (gethash "source_match_reason" method)))
               (ok (null (gethash "form_type" method)))
               (ok (search "[mismatched:" text))
               (ok (not (search "(defmethod" text)))))
        (ignore-errors (delete-file file))))))

(deftest annotate-report-forms-falls-back-when-the-edit-tool-cannot-locate-it-uniquely
  (testing "a matched verdict whose form_name is ambiguous file-wide becomes unverified"
    (let ((file (asdf/system:system-relative-pathname
                 :cl-mcp "tests/tmp/clos-ambiguous-round-trip.lisp")))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out (concatenate 'string
                                 "(defpackage #:cl-mcp-clos-ambiguous-fixture (:use #:cl))~%"
                                 "(in-package #:cl-mcp-clos-ambiguous-fixture)~%~%"
                                 "(defclass box () ())~%~%"
                                 "(defgeneric bulk (x))~%~%"
                                 "(defmethod bulk ((x box)) :first)~%~%"
                                 "(defmethod bulk ((x box)) :second)~%")))
      (unwind-protect
           (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
             (%compile-and-load file)
             (let* ((report (clos-describe-report "cl-mcp-clos-ambiguous-fixture:bulk"))
                    (gf (elt (sequence->list (gethash "generic_functions" report)) 0))
                    (method (elt (sequence->list (gethash "methods" gf)) 0))
                    (text (%text (build-clos-describe-response report #'%verify-inline))))
               (ok (equal "unverified" (gethash "source_match" method)))
               (ok (equal *reason-not-locatable* (gethash "source_match_reason" method)))
               (ok (null (gethash "form_type" method)))
               (ok (not (search "(defmethod" text)))))
        (ignore-errors (delete-file file))))))

(deftest annotate-report-forms-explains-a-missing-form
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (flet ((annotated (abs-path line)
             (let ((entry (make-ht "abs_path" abs-path "path" "x.lisp" "line" line
                                   "stale" yason:false
                                   "form_type" nil "form_name" nil "note" nil)))
               (annotate-report-forms
                (make-ht "symbol_status" "found" "generic_functions" (vector)
                         "class" (make-ht "abs_path" abs-path "path" "x.lisp" "line" line
                                          "stale" yason:false
                                          "form_type" nil "form_name" nil "note" nil
                                          "methods" (vector entry)))
                #'%verify-inline)
               entry)))
      (testing "a line that starts no form"
        (let ((entry (annotated (namestring (truename *fixture*)) 2)))
          (ok (null (gethash "form_name" entry)))
          (ok (equal "unverified" (gethash "source_match" entry)))
          (ok (equal *note-no-form-at-line* (gethash "source_match_reason" entry)))))
      (testing "a file that does not parse"
        (let ((file (asdf/system:system-relative-pathname
                     :cl-mcp "tests/tmp/clos-unparseable.lisp")))
          (ensure-directories-exist file)
          (with-open-file (out file :direction :output :if-exists :supersede)
            (format out "(defparameter *x* #.(+ 1 2))~%"))
          (unwind-protect
               (let* ((entry (annotated (namestring (truename file)) 1))
                      (reason (gethash "source_match_reason" entry)))
                 (ok (equal "unverified" (gethash "source_match" entry)))
                 (ok (and reason (eql 0 (search *note-unparseable* reason))) reason))
            (ignore-errors (delete-file file)))))
      (testing "a file outside the readable paths"
        (let ((entry (annotated "/nonexistent-cl-mcp-dir/x.lisp" 1)))
          (ok (equal "unverified" (gethash "source_match" entry)))
          (ok (equal *reason-not-readable* (gethash "source_match_reason" entry))))))))

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
        (let* ((report (build-clos-describe-response (clos-describe-report designator)
                                                      #'%verify-inline))
               (class (gethash "class" report))
               (entries (append (loop for gf
                                        in (sequence->list (gethash "generic_functions" report))
                                      collect gf
                                      append (sequence->list (gethash "methods" gf)))
                                (and (hash-table-p class)
                                     (cons class (sequence->list (gethash "methods" class)))))))
          (dolist (entry entries)
            (let ((path (gethash "path" entry)))
              (when (and path (search "tests/fixtures/clos-fixture.lisp" path)
                         (equal "matched" (gethash "source_match" entry)))
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
