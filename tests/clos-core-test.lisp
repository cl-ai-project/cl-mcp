;;;; tests/clos-core-test.lisp
;;;;
;;;; Tests for cl-mcp/src/clos-core: the clos-describe report built from the
;;;; CLOS definitions in tests/fixtures/clos-fixture.lisp.

(defpackage #:cl-mcp/tests/clos-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report
                #:*note-not-finalized*)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*))

(in-package #:cl-mcp/tests/clos-core-test)

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

(defun %line (needle)
  "Return the 1-based line of the fixture on which NEEDLE starts."
  (let ((text (uiop:read-file-string *fixture*)))
    (1+ (count #\Newline text :end (search needle text)))))

(defun %report (designator &key (limit 50))
  "Load the fixture and return DESIGNATOR's report with the project root bound."
  (%load-fixture)
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (clos-describe-report designator :limit limit)))

(defun %gfs (report)
  "Return REPORT's generic function entries as a list."
  (sequence->list (gethash "generic_functions" report)))

(defun %methods (entry)
  "Return ENTRY's method objects as a list."
  (sequence->list (gethash "methods" entry)))

(defun %summary (method)
  "Return (QUALIFIERS SPECIALIZERS LINE) for METHOD."
  (list (sequence->list (gethash "qualifiers" method))
        (sequence->list (gethash "specializers" method))
        (gethash "line" method)))

(deftest generic-function-report-lists-methods-by-role
  (testing "a generic function's methods: :around first, then primaries in file order"
    (let* ((report (%report "cl-mcp-clos-fixture:area"))
           (gf (first (%gfs report))))
      (ok (equal "found" (gethash "symbol_status" report)))
      (ok (= 1 (length (%gfs report))))
      (ok (null (gethash "class" report)))
      (ok (equal "CL-MCP-CLOS-FIXTURE:AREA" (gethash "name" gf)))
      (ok (equal "(SHAPE)" (gethash "lambda_list" gf)))
      (ok (equal "Return the area of SHAPE." (gethash "documentation" gf)))
      (ok (equal "STANDARD" (gethash "method_combination" gf)))
      (ok (= 4 (gethash "method_count" gf)))
      (ok (eq yason:false (gethash "truncated" gf)))
      (ok (equal (namestring (truename *fixture*)) (gethash "abs_path" gf)))
      (ok (search "tests/fixtures/clos-fixture.lisp" (gethash "path" gf)))
      (ok (eql (%line "(defgeneric area") (gethash "line" gf)))
      (ok (equal (list (list '(":AROUND") '("CL-MCP-CLOS-FIXTURE:CIRCLE")
                             (%line "(defmethod area :around"))
                       (list '() '("(EQL :UNIT)") (%line "(defgeneric area"))
                       (list '() '("CL-MCP-CLOS-FIXTURE:CIRCLE")
                             (%line "(defmethod area ((shape circle"))
                       (list '() '("CL-MCP-CLOS-FIXTURE:SQUARE")
                             (%line "(defmethod area ((shape square")))
                 (mapcar #'%summary (%methods gf))))
      (dolist (method (%methods gf))
        (ok (equal "method" (gethash "kind" method)))
        (ok (equal "CL-MCP-CLOS-FIXTURE:AREA" (gethash "generic_function" method)))
        (ok (nth-value 1 (gethash "form_name" method)) "form_name is left for the parent")))))

(deftest generic-function-report-covers-setf-functions-and-accessors
  (testing "a symbol naming only a SETF generic function"
    (let* ((report (%report "cl-mcp-clos-fixture:label"))
           (gf (first (%gfs report))))
      (ok (equal "unbound" (gethash "symbol_kind" report)))
      (ok (equal "(SETF CL-MCP-CLOS-FIXTURE:LABEL)" (gethash "name" gf)))
      (ok (equal (list (list '() '("COMMON-LISP:T" "CL-MCP-CLOS-FIXTURE:SHAPE")
                             (%line "(defmethod (setf label)")))
                 (mapcar #'%summary (%methods gf))))))
  (testing "an accessor: the reader and its SETF writer, located at the defclass"
    (let* ((report (%report "cl-mcp-clos-fixture:radius"))
           (gfs (%gfs report)))
      (ok (equal '("CL-MCP-CLOS-FIXTURE:RADIUS" "(SETF CL-MCP-CLOS-FIXTURE:RADIUS)")
                 (mapcar (lambda (gf) (gethash "name" gf)) gfs)))
      (ok (null (gethash "path" (first gfs))) "no defgeneric")
      (ok (equal '(("reader" "CL-MCP-CLOS-FIXTURE:RADIUS") ("writer" "CL-MCP-CLOS-FIXTURE:RADIUS"))
                 (mapcar (lambda (gf)
                           (let ((method (first (%methods gf))))
                             (list (gethash "kind" method) (gethash "slot" method))))
                         gfs)))
      (ok (every (lambda (gf)
                   (eql (%line "(defclass circle") (gethash "line" (first (%methods gf)))))
                 gfs)))))

(deftest generic-function-report-shows-other-method-combinations
  (testing "a + combination keeps its qualifier and lists methods in file order"
    (let ((gf (first (%gfs (%report "cl-mcp-clos-fixture:combine")))))
      (ok (equal "+ :MOST-SPECIFIC-FIRST" (gethash "method_combination" gf)))
      (ok (equal (list (list '("+") '("COMMON-LISP:INTEGER" "COMMON-LISP:T")
                             (%line "(defmethod combine + ((a integer)"))
                       (list '("+") '("COMMON-LISP:NUMBER" "COMMON-LISP:NUMBER")
                             (%line "(defmethod combine + ((a number)")))
                 (mapcar #'%summary (%methods gf)))))))

(deftest generic-function-report-honours-limit
  (testing "at most LIMIT methods are listed and the total is kept"
    (let ((gf (first (%gfs (%report "cl:print-object" :limit 2)))))
      (ok (> (gethash "method_count" gf) 2))
      (ok (= 2 (length (%methods gf))))
      (ok (eq t (gethash "truncated" gf))))))

(deftest clos-describe-report-resolves-without-interning
  (testing "a missing symbol is reported and not interned"
    (%load-fixture)
    (ok (null (find-symbol "NO-SUCH-CLOS-NAME" "CL-MCP-CLOS-FIXTURE")))
    (let ((report (%report "cl-mcp-clos-fixture::no-such-clos-name")))
      (ok (equal "not_found" (gethash "symbol_status" report)))
      (ok (null (gethash "resolved_symbol" report))))
    (ok (null (find-symbol "NO-SUCH-CLOS-NAME" "CL-MCP-CLOS-FIXTURE"))))
  (testing "a missing package"
    (ok (equal "package_not_found"
               (gethash "symbol_status" (%report "no-such-clos-package:thing")))))
  (testing "a plain function has no generic function or class"
    (let ((report (%report "cl:car")))
      (ok (equal "function" (gethash "symbol_kind" report)))
      (ok (null (%gfs report)))
      (ok (null (gethash "class" report))))))

(defun %class (designator)
  "Return DESIGNATOR's class entry."
  (gethash "class" (%report designator)))

(defun %slot-named (class name)
  "Return CLASS's effective slot whose name is NAME."
  (find name (sequence->list (gethash "effective_slots" class))
        :key (lambda (slot) (gethash "name" slot)) :test #'equal))

(defun %strings (entry key)
  "Return ENTRY's KEY as a list of strings."
  (sequence->list (gethash key entry)))

(deftest class-report-leaves-an-unfinalized-class-unfinalized
  (testing "precedence list and slots of a class nobody instantiated"
    (%load-fixture)
    (let ((circle (find-class (find-symbol "CIRCLE" "CL-MCP-CLOS-FIXTURE"))))
      (ok (not (sb-mop:class-finalized-p circle)) "precondition: CIRCLE is unfinalized")
      (let* ((report (%report "cl-mcp-clos-fixture:circle"))
             (class (gethash "class" report))
             (cpl (%strings class "precedence_list")))
        (ok (equal "CL-MCP-CLOS-FIXTURE:CIRCLE" (gethash "name" class)))
        (ok (equal "COMMON-LISP:STANDARD-CLASS" (gethash "metaclass" class)))
        (ok (eq yason:false (gethash "finalized" class)))
        (ok (eql (%line "(defclass circle") (gethash "line" class)))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:SHAPE") (%strings class "direct_superclasses")))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:CIRCLE" "CL-MCP-CLOS-FIXTURE:SHAPE")
                   (subseq cpl 0 2)))
        (ok (equal "COMMON-LISP:T" (car (last cpl))))
        (ok (member *note-not-finalized* (%strings report "notes") :test #'equal)))
      (ok (not (sb-mop:class-finalized-p circle)) "still unfinalized after the report"))))

(deftest class-report-merges-inherited-slots
  (testing "effective slots name the class that defines them"
    (let ((class (%class "cl-mcp-clos-fixture:circle")))
      (ok (equal '("CL-MCP-CLOS-FIXTURE::NAME" "CL-MCP-CLOS-FIXTURE::REGISTRY"
                   "CL-MCP-CLOS-FIXTURE:RADIUS")
                 (mapcar (lambda (slot) (gethash "name" slot))
                         (sequence->list (gethash "effective_slots" class)))))
      (let ((radius (%slot-named class "CL-MCP-CLOS-FIXTURE:RADIUS"))
            (name (%slot-named class "CL-MCP-CLOS-FIXTURE::NAME"))
            (registry (%slot-named class "CL-MCP-CLOS-FIXTURE::REGISTRY")))
        (ok (equal "CL-MCP-CLOS-FIXTURE:CIRCLE" (gethash "from" radius)))
        (ok (equal '(":RADIUS") (%strings radius "initargs")))
        (ok (equal "(RANDOM 10)" (gethash "initform" radius)) "the code, not a value")
        (ok (equal "REAL" (gethash "type" radius)))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:RADIUS") (%strings radius "readers")))
        (ok (equal '("(SETF CL-MCP-CLOS-FIXTURE:RADIUS)") (%strings radius "writers")))
        (ok (equal "CL-MCP-CLOS-FIXTURE:SHAPE" (gethash "from" name)))
        (ok (equal "\"anon\"" (gethash "initform" name)))
        (ok (equal "A label for the shape." (gethash "documentation" name)))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:SHAPE-NAME") (%strings name "readers")))
        (ok (equal "class" (gethash "allocation" registry)))))))

(deftest class-report-reads-a-finalized-class-and-its-initargs
  (testing "a finalized class reports as such and lists its subclasses"
    (%load-fixture)
    (let* ((shape (find-class (find-symbol "SHAPE" "CL-MCP-CLOS-FIXTURE")))
           (class (%class "cl-mcp-clos-fixture:shape")))
      (ok (eq (sb-mop:class-finalized-p shape) (eq t (gethash "finalized" class))))
      (ok (null (set-exclusive-or '("CL-MCP-CLOS-FIXTURE:CIRCLE" "CL-MCP-CLOS-FIXTURE:SQUARE")
                                  (%strings class "direct_subclasses")
                                  :test #'equal)))
      (ok (equal "The base of every shape." (gethash "documentation" class)))))
  (testing "default initargs with the class that supplies them"
    (let ((initargs (sequence->list (gethash "default_initargs"
                                             (%class "cl-mcp-clos-fixture:square")))))
      (ok (equal '((":NAME" "\"square\"" "CL-MCP-CLOS-FIXTURE:SQUARE"))
                 (mapcar (lambda (entry)
                           (list (gethash "initarg" entry) (gethash "form" entry)
                                 (gethash "from" entry)))
                         initargs))))))

(deftest class-report-lists-specialized-methods
  (testing "own methods first, then inherited ones, the standard protocol left out"
    (let ((class (%class "cl-mcp-clos-fixture:circle")))
      (ok (= 7 (gethash "method_count" class)))
      (ok (equal '(("CL-MCP-CLOS-FIXTURE:AREA" (":AROUND") "CL-MCP-CLOS-FIXTURE:CIRCLE" "method")
                   ("CL-MCP-CLOS-FIXTURE:AREA" () "CL-MCP-CLOS-FIXTURE:CIRCLE" "method")
                   ("CL-MCP-CLOS-FIXTURE:RADIUS" () "CL-MCP-CLOS-FIXTURE:CIRCLE" "reader")
                   ("(SETF CL-MCP-CLOS-FIXTURE:RADIUS)" () "CL-MCP-CLOS-FIXTURE:CIRCLE" "writer")
                   ("CL-MCP-CLOS-FIXTURE:DESCRIBE-SHAPE" () "CL-MCP-CLOS-FIXTURE:SHAPE" "method")
                   ("(SETF CL-MCP-CLOS-FIXTURE:LABEL)" () "CL-MCP-CLOS-FIXTURE:SHAPE" "method")
                   ("CL-MCP-CLOS-FIXTURE:SHAPE-NAME" () "CL-MCP-CLOS-FIXTURE:SHAPE" "reader"))
                 (mapcar (lambda (method)
                           (list (gethash "generic_function" method)
                                 (%strings method "qualifiers")
                                 (gethash "via" method)
                                 (gethash "kind" method)))
                         (%methods class))))
      (ok (member "COMMON-LISP:STANDARD-OBJECT" (%strings class "omitted_classes")
                  :test #'equal))
      (ok (member "COMMON-LISP:T" (%strings class "omitted_classes") :test #'equal))))
  (testing "LIMIT applies to a class's methods too"
    (let ((class (gethash "class" (%report "cl-mcp-clos-fixture:circle" :limit 3))))
      (ok (= 7 (gethash "method_count" class)))
      (ok (= 3 (length (%methods class))))
      (ok (eq t (gethash "truncated" class))))))

(deftest class-report-handles-an-undefined-superclass
  (testing "no precedence list or effective slots, and the missing class named"
    (let* ((report (%report "cl-mcp-clos-fixture:pending"))
           (class (gethash "class" report)))
      (ok (null (gethash "precedence_list" class)))
      (ok (null (gethash "effective_slots" class)))
      (ok (equal '("CL-MCP-CLOS-FIXTURE::NOT-YET-DEFINED")
                 (%strings class "undefined_superclasses")))
      (ok (some (lambda (note) (search "undefined superclass" note))
                (%strings report "notes"))))))

(deftest class-report-on-a-forward-referenced-class
  (testing "a class only named as a superclass is said not to be defined"
    (let* ((report (%report "cl-mcp-clos-fixture::not-yet-defined"))
           (class (gethash "class" report)))
      (ok (hash-table-p class))
      (ok (null (gethash "precedence_list" class)))
      (ok (equal '("this class is referenced as a superclass but not defined")
                 (%strings report "notes"))))))

(deftest class-report-covers-conditions-and-structures
  (testing "a condition's slot reader and its location"
    (let ((class (%class "cl-mcp-clos-fixture:probe-error")))
      (ok (equal "SB-PCL::CONDITION-CLASS" (gethash "metaclass" class)))
      (ok (eql (%line "(define-condition probe-error") (gethash "line" class)))
      (ok (equal '("CL-MCP-CLOS-FIXTURE:PROBE-ERROR-CODE")
                 (%strings (first (sequence->list (gethash "direct_slots" class))) "readers")))
      (ok (member "COMMON-LISP:CONDITION" (%strings class "omitted_classes") :test #'equal))))
  (testing "a structure's slots and initforms"
    (let ((class (%class "cl-mcp-clos-fixture:point")))
      (ok (equal "COMMON-LISP:STRUCTURE-CLASS" (gethash "metaclass" class)))
      (ok (eql (%line "(defstruct point") (gethash "line" class)))
      (ok (equal "0" (gethash "initform" (%slot-named class "CL-MCP-CLOS-FIXTURE::Y"))))))
  (testing "describing them signals no warning (SBCL warns on slot DOCUMENTATION)"
    (let ((warned nil))
      (handler-bind ((warning (lambda (w)
                                (setf warned (princ-to-string w))
                                (muffle-warning w))))
        (%report "cl-mcp-clos-fixture:probe-error")
        (%report "cl-mcp-clos-fixture:point"))
      (ok (null warned) warned))))

(deftest class-report-prints-anonymous-classes
  (testing "an anonymous superclass is printed, never named NIL"
    (let* ((anonymous (make-instance 'standard-class
                                     :direct-superclasses (list (find-class 'standard-object))))
           (child (sb-mop:ensure-class 'anonymous-superclass-probe
                                       :direct-superclasses (list anonymous)))
           (class (gethash "class" (clos-describe-report
                                    "cl-mcp/tests/clos-core-test::anonymous-superclass-probe")))
           (superclass (first (%strings class "direct_superclasses"))))
      (ok (not (sb-mop:class-finalized-p child)))
      (ok (eql 0 (search "#<anonymous COMMON-LISP:STANDARD-CLASS {" superclass)) superclass)
      (ok (equal superclass (second (%strings class "precedence_list"))))
      (ok (notany (lambda (name) (search "COMMON-LISP:NIL" name))
                  (%strings class "precedence_list"))))))

(defparameter *identity-fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-identity-fixture.lisp")
  "CLOS definitions across two packages, exercising identity's package/name
and EQL tagging apart from clos-fixture's display-string tests.")

(defun %load-identity-fixture ()
  "Compile and load the identity fixture with its truename as the source
namestring, as %LOAD-FIXTURE does for the display-string fixture."
  (let ((truename (truename *identity-fixture*)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %identity-report (designator &key (limit 50))
  "Load the identity fixture and return DESIGNATOR's report with the project
root bound."
  (%load-identity-fixture)
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (clos-describe-report designator :limit limit)))

(defun %identity (entry)
  "Return ENTRY's identity object."
  (gethash "identity" entry))

(defun %eql-datum-of (methods predicate)
  "Return the tagged EQL datum of the first of METHODS whose sole specializer
is an EQL specializer and whose datum PREDICATE accepts."
  (loop for method in methods
        for specializer = (first (sequence->list
                                  (gethash "specializers" (%identity method))))
        when (and specializer (equal "eql" (gethash "kind" specializer))
                 (funcall predicate (gethash "datum" specializer)))
          return (gethash "datum" specializer)))

(defun %method-with-specializer-name (methods name)
  "Return the entry in METHODS whose first specializer is a named class
matching NAME exactly (case-sensitive), found by identity, not display text."
  (find-if (lambda (method)
             (let ((specializer (first (sequence->list
                                        (gethash "specializers" (%identity method))))))
               (and specializer (equal "class" (gethash "kind" specializer))
                    (equal name (gethash "name" specializer)))))
           methods))

(deftest identity-distinguishes-generic-function-package-and-setf
  (testing "package A's ACT and its SETF function carry package and setf apart"
    (let* ((report (%identity-report "cl-mcp-identity-a:act"))
           (gfs (%gfs report))
           (act-fn (gethash "generic_function" (%identity (first gfs))))
           (setf-act-fn (gethash "generic_function" (%identity (second gfs)))))
      (ok (= 2 (length gfs)))
      (ok (equal "generic-function" (gethash "kind" (%identity (first gfs)))))
      (ok (equal "CL-MCP-IDENTITY-A" (gethash "package" act-fn)))
      (ok (equal "ACT" (gethash "name" act-fn)))
      (ok (eq yason:false (gethash "setf" act-fn)))
      (ok (equal "CL-MCP-IDENTITY-A" (gethash "package" setf-act-fn)))
      (ok (equal "ACT" (gethash "name" setf-act-fn)))
      (ok (eq t (gethash "setf" setf-act-fn)))))
  (testing "package B's ACT identity names package B, not A"
    (let* ((gf (first (%gfs (%identity-report "cl-mcp-identity-b:act"))))
           (fn (gethash "generic_function" (%identity gf))))
      (ok (equal "CL-MCP-IDENTITY-B" (gethash "package" fn)))
      (ok (equal "ACT" (gethash "name" fn)))
      (ok (eq yason:false (gethash "setf" fn))))))

(deftest identity-tags-qualifiers-by-package-and-name
  (testing "a keyword qualifier resolves to the KEYWORD package"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:area"))))
           (around (first (%methods gf)))
           (qualifier (first (sequence->list (gethash "qualifiers" (%identity around))))))
      (ok (equal "KEYWORD" (gethash "package" qualifier)))
      (ok (equal "AROUND" (gethash "name" qualifier)))))
  (testing "a symbol qualifier from the method-combination protocol"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:combine"))))
           (method (first (%methods gf)))
           (qualifier (first (sequence->list (gethash "qualifiers" (%identity method))))))
      (ok (equal "COMMON-LISP" (gethash "package" qualifier)))
      (ok (equal "+" (gethash "name" qualifier))))))

(deftest identity-tags-eql-data-by-kind
  (let ((methods (%methods (first (%gfs (%identity-report "cl-mcp-identity-a:act"))))))
    (testing "an integer literal carries its decimal value as text"
      (ok (%eql-datum-of methods (lambda (d) (and (equal "integer" (gethash "kind" d))
                                                  (equal "3" (gethash "value" d)))))))
    (testing "a ratio carries numerator and denominator as text"
      (let ((datum (%eql-datum-of methods (lambda (d) (equal "ratio" (gethash "kind" d))))))
        (ok (equal "1" (gethash "numerator" datum)))
        (ok (equal "3" (gethash "denominator" datum)))))
    (testing "character data keep case, as two distinct characters"
      (ok (%eql-datum-of methods (lambda (d) (and (equal "character" (gethash "kind" d))
                                                  (equal "A" (gethash "value" d))))))
      (ok (%eql-datum-of methods (lambda (d) (and (equal "character" (gethash "kind" d))
                                                  (equal "B" (gethash "value" d)))))))
    (testing "a keyword datum is named without a package"
      (let ((datum (%eql-datum-of methods (lambda (d) (equal "keyword" (gethash "kind" d))))))
        (ok (equal "UNIT" (gethash "name" datum)))))
    (testing "T and NIL are tagged boolean, not symbol or a missing value"
      (ok (%eql-datum-of methods (lambda (d) (and (equal "boolean" (gethash "kind" d))
                                                  (equal "T" (gethash "value" d))))))
      (ok (%eql-datum-of methods (lambda (d) (and (equal "boolean" (gethash "kind" d))
                                                  (equal "NIL" (gethash "value" d)))))))
    (testing "a quoted interned symbol carries its package and name"
      (let ((datum (%eql-datum-of methods (lambda (d) (equal "symbol" (gethash "kind" d))))))
        (ok (equal "CL-MCP-IDENTITY-A" (gethash "package" datum)))
        (ok (equal "SYM" (gethash "name" datum)))))
    (testing "a variable reference's evaluated value is tagged by its own type"
      (ok (%eql-datum-of methods (lambda (d) (and (equal "integer" (gethash "kind" d))
                                                  (equal "7" (gethash "value" d)))))))))

(deftest identity-marks-unverifiable-eql-data-with-a-reason
  (testing "a string EQL datum is unverifiable, with a one-sentence reason"
    (let* ((methods (%methods (first (%gfs (%identity-report "cl-mcp-identity-a:act")))))
           (datum (%eql-datum-of methods (lambda (d) (equal "unverifiable" (gethash "kind" d))))))
      (ok (stringp (gethash "reason" datum)))
      (ok (plusp (length (gethash "reason" datum))))
      (ok (null (position #\Newline (gethash "reason" datum))))
      (ok (nth-value 1 (gethash "reason" datum)))
      (ok (null (gethash "value" datum)))
      (ok (null (gethash "name" datum))))))

(deftest identity-tags-class-specializers-by-package-not-display-string
  (testing "same-named PROBE classes in two packages resolve to different identities"
    (let* ((a-methods (%methods (first (%gfs (%identity-report "cl-mcp-identity-a:act")))))
           (b-methods (%methods (first (%gfs (%identity-report "cl-mcp-identity-b:act")))))
           (a-probe (%method-with-specializer-name a-methods "PROBE"))
           (b-probe (%method-with-specializer-name b-methods "PROBE")))
      (ok a-probe)
      (ok b-probe)
      (ok (equal "CL-MCP-IDENTITY-A"
                 (gethash "package" (first (sequence->list
                                            (gethash "specializers" (%identity a-probe)))))))
      (ok (equal "CL-MCP-IDENTITY-B"
                 (gethash "package" (first (sequence->list
                                            (gethash "specializers" (%identity b-probe)))))))))
  (testing "|Foo| and |FOO| keep their exact case as distinct specializers"
    (let* ((methods (%methods (first (%gfs (%identity-report "cl-mcp-identity-a:act")))))
           (foo (%method-with-specializer-name methods "Foo"))
           (foo-upper (%method-with-specializer-name methods "FOO")))
      (ok foo)
      (ok foo-upper)
      (ok (not (eq foo foo-upper))))))

(deftest identity-reports-accessor-slot-class-and-access
  (testing "PROBE's reader and writer both carry access, slot, and owning class identity"
    (let* ((gfs (%gfs (%identity-report "cl-mcp-identity-a:probe-value")))
           (reader (first (%methods (first gfs))))
           (writer (first (%methods (second gfs))))
           (reader-identity (%identity reader))
           (writer-identity (%identity writer)))
      (ok (equal "method" (gethash "kind" reader-identity)))
      (ok (equal "method" (gethash "kind" writer-identity)))
      (ok (equal "reader" (gethash "access" reader-identity)))
      (ok (equal "writer" (gethash "access" writer-identity)))
      (dolist (identity (list reader-identity writer-identity))
        (ok (equal "CL-MCP-IDENTITY-A" (gethash "package" (gethash "slot" identity))))
        (ok (equal "VALUE" (gethash "name" (gethash "slot" identity))))
        (ok (equal "CL-MCP-IDENTITY-A" (gethash "package" (gethash "class" identity))))
        (ok (equal "PROBE" (gethash "name" (gethash "class" identity))))))))

(deftest identity-reports-class-identity
  (testing "a class's identity names its package and symbol, not a display string"
    (let* ((report (%identity-report "cl-mcp-identity-b:probe"))
           (class (gethash "class" report))
           (identity (%identity class)))
      (ok (equal "class" (gethash "kind" identity)))
      (ok (equal "CL-MCP-IDENTITY-B" (gethash "package" (gethash "class" identity))))
      (ok (equal "PROBE" (gethash "name" (gethash "class" identity)))))))

(deftest identity-defaults-accessor-fields-to-nil-for-plain-methods
  (testing "a non-accessor method's identity has no accessor fields, present but nil"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:combine"))))
           (identity (%identity (first (%methods gf)))))
      (ok (null (gethash "access" identity)))
      (ok (null (gethash "slot" identity)))
      (ok (null (gethash "class" identity))))))

(deftest identity-reports-a-condition-readers-slot-class-and-access
  (testing "PROBE-ERROR-CODE is not a standard-accessor-method in this SBCL, but its
identity is still filled from CLASS-DIRECT-SLOTS: exactly one slot of PROBE-ERROR
names it as a reader"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:probe-error-code"))))
           (identity (%identity (first (%methods gf)))))
      (ok (equal "method" (gethash "kind" identity)))
      (ok (equal "reader" (gethash "access" identity)))
      (ok (equal "CL-MCP-CLOS-FIXTURE" (gethash "package" (gethash "slot" identity))))
      (ok (equal "CODE" (gethash "name" (gethash "slot" identity))))
      (ok (equal "CL-MCP-CLOS-FIXTURE" (gethash "package" (gethash "class" identity))))
      (ok (equal "PROBE-ERROR" (gethash "name" (gethash "class" identity)))))))

(deftest identity-leaves-an-ambiguous-condition-reader-unfilled
  (testing "PROBE-ERROR-AMBIGUOUS names two of PROBE-ERROR's slots' readers, so
neither can be picked without a guess -- class/slot/access stay nil, fail-closed"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture::probe-error-ambiguous"))))
           (identity (%identity (first (%methods gf)))))
      (ok (equal "method" (gethash "kind" identity)))
      (ok (null (gethash "access" identity)))
      (ok (null (gethash "slot" identity)))
      (ok (null (gethash "class" identity))))))

(deftest identity-does-not-treat-a-qualified-method-as-an-accessor
  (testing "a :before method sharing GUARDED-ERROR-CODE's generic function and sole
specializer with the genuine condition reader is not mistaken for it: kind stays
\"method\", class/slot/access stay nil; the reader still gets them.  A condition is
the only place the qualifier guard decides anything, because a condition reader is
the only accessor that reaches the fallback at all"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:guarded-error-code"))))
           (methods (%methods gf))
           (before-method
             (find-if (lambda (m) (equal '(":BEFORE") (%strings m "qualifiers"))) methods))
           (reader-method
             (find-if (lambda (m) (null (%strings m "qualifiers"))) methods))
           (before-identity (%identity before-method))
           (reader-identity (%identity reader-method)))
      (ok before-method)
      (ok reader-method)
      (ok (equal "method" (gethash "kind" before-method)))
      (ok (null (gethash "access" before-identity)))
      (ok (null (gethash "slot" before-identity)))
      (ok (null (gethash "class" before-identity)))
      (ok (equal "reader" (gethash "kind" reader-method)))
      (ok (equal "reader" (gethash "access" reader-identity)))
      (ok (equal "CL-MCP-CLOS-FIXTURE" (gethash "package" (gethash "slot" reader-identity))))
      (ok (equal "CODE" (gethash "name" (gethash "slot" reader-identity))))
      (ok (equal "CL-MCP-CLOS-FIXTURE" (gethash "package" (gethash "class" reader-identity))))
      (ok (equal "GUARDED-ERROR" (gethash "name" (gethash "class" reader-identity))))))
  (testing "on an ordinary class the same :before method is a plain method too, and
WIDGET-SIZE's genuine accessor -- a real STANDARD-READER-METHOD -- still carries
class, slot and access"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:widget-size"))))
           (methods (%methods gf))
           (before-identity
             (%identity (find-if (lambda (m) (equal '(":BEFORE") (%strings m "qualifiers")))
                                 methods)))
           (reader-method (find-if (lambda (m) (null (%strings m "qualifiers"))) methods))
           (reader-identity (%identity reader-method)))
      (ok (null (gethash "access" before-identity)))
      (ok (null (gethash "slot" before-identity)))
      (ok (null (gethash "class" before-identity)))
      (ok (equal "reader" (gethash "kind" reader-method)))
      (ok (equal "reader" (gethash "access" reader-identity)))
      (ok (equal "SIZE" (gethash "name" (gethash "slot" reader-identity))))
      (ok (equal "WIDGET" (gethash "name" (gethash "class" reader-identity)))))))

(deftest identity-treats-an-overridden-ordinary-accessor-as-a-plain-method
  (testing "GADGET-SIZE's generated reader was replaced by a hand-written DEFMETHOD:
the sole live method is a plain STANDARD-METHOD, and on an ordinary class that is
proof it is not an accessor, so class/slot/access stay nil and the entry points at
the DEFMETHOD's own line"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:gadget-size"))))
           (methods (%methods gf))
           (identity (%identity (first methods))))
      (ok (= 1 (length methods)) "the override replaced the generated reader")
      (ok (equal "method" (gethash "kind" (first methods))))
      (ok (null (gethash "access" identity)))
      (ok (null (gethash "slot" identity)))
      (ok (null (gethash "class" identity)))
      (ok (eql (%line "(defmethod gadget-size") (gethash "line" (first methods)))))))

(deftest identity-still-fills-an-overridden-condition-readers-accessor-fields
  (testing "OVERRIDE-ERROR-CODE's generated reader was replaced the same way, but a
genuine condition reader is a plain STANDARD-METHOD too, so the image cannot tell
the two apart: the identity keeps class/slot/access -- source matching settles which
it was -- while the entry's line is the DEFMETHOD's own"
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:override-error-code"))))
           (methods (%methods gf))
           (identity (%identity (first methods))))
      (ok (= 1 (length methods)))
      (ok (equal "reader" (gethash "access" identity)))
      (ok (equal "CODE" (gethash "name" (gethash "slot" identity))))
      (ok (equal "OVERRIDE-ERROR" (gethash "name" (gethash "class" identity))))
      (ok (eql (%line "(defmethod override-error-code")
               (gethash "line" (first methods)))))))

(deftest identity-treats-an-overridden-writer-as-a-plain-method
  (testing "a hand-written (SETF BOX-W) method has two specializers, so the accessor
fallback's one-specializer guard rejects it on any class; BOX-W's untouched reader
half is still a genuine accessor"
    (let* ((gfs (%gfs (%report "cl-mcp-clos-fixture:box-w")))
           (reader-identity (%identity (first (%methods (first gfs)))))
           (writer-identity (%identity (first (%methods (second gfs))))))
      (ok (equal "reader" (gethash "access" reader-identity)))
      (ok (equal "W" (gethash "name" (gethash "slot" reader-identity))))
      (ok (null (gethash "access" writer-identity)))
      (ok (null (gethash "slot" writer-identity)))
      (ok (null (gethash "class" writer-identity))))))
