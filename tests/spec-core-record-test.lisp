;;;; tests/spec-core-record-test.lisp
;;;;
;;;; The versioned-record layer, exercised with plain plists and no cl-spec in
;;;; the image.  Every function here is pure over data cl-spec would have
;;;; returned, which is what lets these cases cover the revisions this project
;;;; cannot install.

(defpackage #:cl-mcp/tests/spec-core-record-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-core-record
                #:safe-json-integer-p
                #:project-value
                #:project-record
                #:*projection-max-depth*
                #:*projection-max-length*))

(in-package #:cl-mcp/tests/spec-core-record-test)

(deftest safe-json-integer-covers-the-double-mantissa
  (testing "the boundary itself is safe"
    (ok (safe-json-integer-p (1- (expt 2 53))))
    (ok (safe-json-integer-p (- (1- (expt 2 53))))))
  (testing "one past it is not, although it is still a fixnum"
    (ok (not (safe-json-integer-p (expt 2 53))))
    (ok (not (safe-json-integer-p (- (expt 2 53)))))
    ;; The case this rule exists for: a cl-spec seed is a fixnum and is far
    ;; wider than a JSON consumer holds exactly.
    (ok (not (safe-json-integer-p 4611686018427387903)))))

(deftest project-value-tags-each-type-once
  (testing "a keyword is a scalar word, not a symbol node"
    (ok (equal '(:scalar "state-post") (project-value :state-post))))
  (testing "NIL is a scalar null, not the symbol COMMON-LISP::NIL"
    (ok (equal '(:scalar nil) (project-value nil))))
  (testing "any other symbol is a symbol node and keeps its package"
    (let ((node (project-value 'cl-user::widen)))
      (ok (eq :symbol (first node)))
      (ok (equal "WIDEN" (getf (second node) :name)))
      (ok (equal "COMMON-LISP-USER" (getf (second node) :package)))))
  (testing "a string is a scalar"
    (ok (equal '(:scalar "boom") (project-value "boom"))))
  (testing "a safe integer stays a number and a wider one becomes text"
    (ok (equal '(:scalar 42) (project-value 42)))
    (ok (equal '(:scalar "4611686018427387903")
               (project-value 4611686018427387903))))
  (testing "anything else is a value node from externalize-value"
    (let ((node (project-value (make-hash-table))))
      (ok (eq :value (first node)))
      (ok (stringp (getf (second node) :printed)))
      (ok (equal "hash-table" (getf (second node) :type))))))

(deftest object-descriptor-projects-only-declared-keys
  (let ((shape '(:object (:kind . :leaf) (:index . :leaf))))
    (multiple-value-bind (node issues unknown)
        (project-record '(:kind :state-postcondition :index 0 :surprise 7) shape)
      (ok (null issues))
      (testing "declared keys are projected under snake_case names"
        (ok (equal '(:scalar "state-postcondition")
                   (cdr (assoc "kind" (second node) :test #'equal))))
        (ok (equal '(:scalar 0)
                   (cdr (assoc "index" (second node) :test #'equal)))))
      (testing "an undeclared key is named and its value is not interpreted"
        (ok (equal '("surprise") unknown))
        (ok (null (assoc "surprise" (second node) :test #'equal)))))))

(deftest a-keyword-list-is-an-array-not-an-object
  ;; §6.2.1's own counterexample.  (:AT-LEAST :AT-MOST) is two case names;
  ;; read as a plist it becomes {"at-least": "at-most"}, a relation cl-spec
  ;; never declared.
  (let ((node (project-record '(:at-least :at-most) :word-list)))
    (ok (eq :array (first node)))
    (ok (equal '((:scalar "at-least") (:scalar "at-most")) (second node)))))

(deftest actual-and-expected-are-projected-differently
  ;; The same cons under two keys of one error datum: :ACTUAL is a value from
  ;; the code under test, :EXPECTED is a descriptor cl-spec built.
  (let* ((shape '(:object (:actual . :opaque) (:expected . (:object (:kind . :leaf)))))
         (node (project-record '(:actual (1 2 3) :expected (:kind :range)) shape))
         (fields (second node)))
    (testing ":actual is externalized, never structured"
      (let ((actual (cdr (assoc "actual" fields :test #'equal))))
        (ok (eq :value (first actual)))
        (ok (search "1 2 3" (getf (second actual) :printed)))))
    (testing ":expected keeps its structure"
      (let ((expected (cdr (assoc "expected" fields :test #'equal))))
        (ok (eq :object (first expected)))
        (ok (equal '(:scalar "range")
                   (cdr (assoc "kind" (second expected) :test #'equal))))))))

(deftest an-alist-of-capture-values-becomes-name-value-pairs
  ;; Measured shape: ((BALANCE-BEFORE . 30) (ID-BEFORE . 7)).  Dotted pairs are
  ;; not proper lists, so an array rule has nothing to say about them.
  (let* ((node (project-record (list (cons 'cl-user::balance-before 30))
                               '(:alist :opaque)))
         (entry (first (second node)))
         (fields (second entry)))
    (ok (eq :array (first node)))
    (ok (equal "BALANCE-BEFORE"
               (getf (second (cdr (assoc "name" fields :test #'equal))) :name)))
    (ok (eq :value (first (cdr (assoc "value" fields :test #'equal)))))))

(deftest a-length-cut-is-reported-not-hidden
  (let ((*projection-max-length* 2))
    (multiple-value-bind (node issues)
        (project-record '(:a :b :c :d :e) :word-list '("failure" "cases"))
      (ok (= 2 (length (second node))))
      (ok (= 1 (length issues)))
      (let ((issue (first issues)))
        (ok (equal '("failure" "cases") (getf issue :path)))
        (ok (eq :length-limit (getf issue :reason)))
        ;; Five errors must not arrive as two that look complete.
        (ok (eql 3 (getf issue :omitted-items)))))))

(deftest a-depth-cut-leaves-the-standard-value-node
  (let ((*projection-max-depth* 1)
        (shape '(:object (:inner . (:object (:deeper . :leaf))))))
    (multiple-value-bind (node issues)
        (project-record '(:inner (:deeper 1)) shape)
      (let ((inner (cdr (assoc "inner" (second node) :test #'equal))))
        ;; Not a marker invented for this: the externalized-value plist is what
        ;; already represents any Lisp value everywhere else in the record.
        (ok (eq :value (first inner))))
      (ok (eq :depth-limit (getf (first issues) :reason)))
      (ok (equal '("inner") (getf (first issues) :path))))))
