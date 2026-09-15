;;;; tests/clos-core-test.lisp
;;;;
;;;; Tests for cl-mcp/src/clos-core: the clos-describe report built from the
;;;; CLOS definitions in tests/fixtures/clos-fixture.lisp.

(defpackage #:cl-mcp/tests/clos-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
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
