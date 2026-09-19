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
                #:project-value))

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
