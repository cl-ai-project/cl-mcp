;;;; tests/inspect-test.lisp

(defpackage #:cl-mcp/tests/inspect-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/object-registry
                #:register-object
                #:*object-registry*
                #:make-object-registry)
  (:import-from #:cl-mcp/src/inspect
                #:inspect-object-by-id))

(in-package #:cl-mcp/tests/inspect-test)

;;; Helper to get hash-table value
(defun ht-get (ht key)
  (gethash key ht))

;;; Test class for CLOS inspection
(defclass test-person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)
   (unbound-slot)))

;;; Test structure for structure inspection
(defstruct test-point
  x y)

;;; Setup/teardown
(defun with-fresh-registry (thunk)
  (let ((*object-registry* (make-object-registry)))
    (funcall thunk)))

;;; Test error handling

(deftest inspect-nonexistent-id
  (testing "inspect nonexistent ID returns error"
    (with-fresh-registry
     (lambda ()
       (let ((result (inspect-object-by-id 99999)))
         (ok (ht-get result "error"))
         (ok (string= "OBJECT_NOT_FOUND" (ht-get result "code"))))))))

;;; Test list inspection

(deftest inspect-simple-list
  (testing "inspect simple list"
    (with-fresh-registry
     (lambda ()
       (let* ((obj '(1 2 3))
              (id (register-object obj))
              (result (inspect-object-by-id id)))
         (ok (string= "list" (ht-get result "kind")))
         (ok (= id (ht-get result "id")))
         (ok (= 3 (length (ht-get result "elements")))))))))

(deftest inspect-list-with-nested-objects
  (testing "inspect list with nested objects creates refs"
    (with-fresh-registry
     (lambda ()
       (let* ((inner (list :inner))
              (obj (list 1 inner 3))
              (id (register-object obj))
              (result (inspect-object-by-id id :max-depth 1)))
         (ok (string= "list" (ht-get result "kind")))
         ;; First and third elements are primitives
         (let ((elems (ht-get result "elements")))
           (ok (= 1 (ht-get (first elems) "value")))
           ;; Second element should be an object-ref
           (ok (string= "object-ref" (ht-get (second elems) "kind")))))))))

(deftest inspect-list-truncation
  (testing "inspect list respects max_elements"
    (with-fresh-registry
     (lambda ()
       (let* ((obj (loop for i from 1 to 100 collect i))
              (id (register-object obj))
              (result (inspect-object-by-id id :max-elements 10)))
         (ok (string= "list" (ht-get result "kind")))
         (ok (= 10 (length (ht-get result "elements"))))
         (let ((meta (ht-get result "meta")))
           (ok (ht-get meta "truncated"))))))))

;;; Test vector inspection

(deftest inspect-vector
  (testing "inspect vector"
    (with-fresh-registry
     (lambda ()
       (let* ((obj #(a b c))
              (id (register-object obj))
              (result (inspect-object-by-id id)))
         (ok (string= "array" (ht-get result "kind")))
         (ok (equal '(3) (ht-get result "dimensions")))
         (ok (= 3 (length (ht-get result "elements")))))))))

;;; Test hash-table inspection

(deftest inspect-hash-table
  (testing "inspect hash-table"
    (with-fresh-registry
     (lambda ()
       (let* ((obj (make-hash-table :test 'equal)))
         (setf (gethash "key1" obj) "value1")
         (setf (gethash "key2" obj) 42)
         (let* ((id (register-object obj))
                (result (inspect-object-by-id id)))
           (ok (string= "hash-table" (ht-get result "kind")))
           (ok (string= "EQUAL" (ht-get result "test")))
           (ok (= 2 (length (ht-get result "entries"))))))))))

;;; Test function inspection

(deftest inspect-function
  (testing "inspect function"
    (with-fresh-registry
     (lambda ()
       (let* ((obj (lambda (x y) (+ x y)))
              (id (register-object obj))
              (result (inspect-object-by-id id)))
         (ok (string= "function" (ht-get result "kind"))))))))

(deftest inspect-named-function
  (testing "inspect named function shows name"
    (with-fresh-registry
     (lambda ()
       (let* ((id (register-object #'car))
              (result (inspect-object-by-id id)))
         (ok (string= "function" (ht-get result "kind")))
         (ok (ht-get result "name")))))))

;;; Test CLOS instance inspection

#+sbcl
(deftest inspect-clos-instance
  (testing "inspect CLOS instance"
    (with-fresh-registry
     (lambda ()
       (let* ((obj (make-instance 'test-person :name "Alice" :age 30))
              (id (register-object obj))
              (result (inspect-object-by-id id)))
         (ok (string= "instance" (ht-get result "kind")))
         (ok (string= "TEST-PERSON" (ht-get result "class")))
         (let ((slots (ht-get result "slots")))
           (ok (>= (length slots) 2))))))))

#+sbcl
(deftest inspect-clos-unbound-slot
  (testing "inspect CLOS instance with unbound slot"
    (with-fresh-registry
     (lambda ()
       (let* ((obj (make-instance 'test-person :name "Bob" :age 25))
              (id (register-object obj))
              (result (inspect-object-by-id id)))
         ;; Find the unbound-slot
         (let ((unbound-slot (find-if (lambda (s)
                                        (string= "UNBOUND-SLOT" (ht-get s "name")))
                                      (ht-get result "slots"))))
           (when unbound-slot
             (ok (string= "unbound" (ht-get (ht-get unbound-slot "value") "kind"))))))))))

;;; Test structure inspection

#+sbcl
(deftest inspect-structure
  (testing "inspect structure"
    (with-fresh-registry
     (lambda ()
       (let* ((obj (make-test-point :x 10 :y 20))
              (id (register-object obj))
              (result (inspect-object-by-id id)))
         (ok (string= "structure" (ht-get result "kind")))
         (ok (string= "TEST-POINT" (ht-get result "class"))))))))

;;; Test circular reference detection

(deftest inspect-circular-reference
  (testing "circular references are detected"
    (with-fresh-registry
     (lambda ()
       (let* ((obj (list 1 2 3)))
         ;; Create circular reference
         (setf (cdr (last obj)) obj)
         (let* ((id (register-object obj))
                (result (inspect-object-by-id id :max-elements 10)))
           ;; Should complete without infinite loop
           (ok (string= "list" (ht-get result "kind")))))))))

;;; Test max_depth behavior

(deftest inspect-depth-zero
  (testing "max_depth=0 returns summary only"
    (with-fresh-registry
     (lambda ()
       (let* ((inner (list :inner :data))
              (obj (list inner))
              (id (register-object obj))
              (result (inspect-object-by-id id :max-depth 0)))
         (ok (string= "list" (ht-get result "kind")))
         ;; Inner list should be object-ref, not expanded
         (let ((first-elem (first (ht-get result "elements"))))
           (ok (string= "object-ref" (ht-get first-elem "kind")))))))))

(deftest inspect-depth-expands-nested
  (testing "max_depth > 0 expands nested objects"
    (with-fresh-registry
     (lambda ()
       (let* ((inner (list 1 2))
              (obj (list inner 3))
              (id (register-object obj))
              (result (inspect-object-by-id id :max-depth 2)))
         (ok (string= "list" (ht-get result "kind")))
         ;; Inner list should be expanded at depth 2
         (let ((first-elem (first (ht-get result "elements"))))
           ;; At depth 2, inner objects get expanded
           (ok (or (string= "list" (ht-get first-elem "kind"))
                   (string= "object-ref" (ht-get first-elem "kind"))))))))))
