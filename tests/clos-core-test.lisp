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
