;;;; tests/object-registry-test.lisp

(defpackage #:cl-mcp/tests/object-registry-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p
                #:register-object
                #:lookup-object
                #:clear-registry
                #:registry-count
                #:make-object-registry
                #:+max-registry-size+))

(in-package #:cl-mcp/tests/object-registry-test)

;;; Test inspectable-p predicate

(deftest inspectable-p-primitives
  (testing "primitives are not inspectable"
    (ok (not (inspectable-p 42)))
    (ok (not (inspectable-p 3.14)))
    (ok (not (inspectable-p "hello")))
    (ok (not (inspectable-p 'symbol)))
    (ok (not (inspectable-p :keyword)))
    (ok (not (inspectable-p #\a)))))

(deftest inspectable-p-non-primitives
  (testing "non-primitives are inspectable"
    (ok (inspectable-p '(1 2 3)))
    (ok (inspectable-p #(1 2 3)))
    (ok (inspectable-p (make-hash-table)))
    (ok (inspectable-p (lambda (x) x)))
    (ok (inspectable-p (cons 1 2)))))

;;; Test basic registry operations

(deftest register-and-lookup
  (testing "register and lookup basic objects"
    (let ((registry (make-object-registry)))
      (let* ((obj (list 1 2 3))
             (id (register-object obj registry)))
        (ok (integerp id))
        (ok (eq obj (lookup-object id registry)))))))

(deftest register-returns-nil-for-primitives
  (testing "register returns NIL for primitives"
    (let ((registry (make-object-registry)))
      (ok (null (register-object 42 registry)))
      (ok (null (register-object "string" registry)))
      (ok (null (register-object 'symbol registry)))
      (ok (null (register-object #\x registry))))))

(deftest lookup-nonexistent-returns-nil
  (testing "lookup nonexistent ID returns NIL"
    (let ((registry (make-object-registry)))
      (ok (null (lookup-object 999 registry))))))

(deftest clear-registry-removes-all
  (testing "clear-registry removes all objects"
    (let ((registry (make-object-registry)))
      (register-object (list 1) registry)
      (register-object (list 2) registry)
      (register-object (list 3) registry)
      (ok (= 3 (registry-count registry)))
      (clear-registry registry)
      (ok (= 0 (registry-count registry))))))

(deftest registry-count-tracks-objects
  (testing "registry-count tracks object count"
    (let ((registry (make-object-registry)))
      (ok (= 0 (registry-count registry)))
      (register-object (list 1) registry)
      (ok (= 1 (registry-count registry)))
      (register-object (list 2) registry)
      (ok (= 2 (registry-count registry))))))

;;; Test FIFO eviction

(deftest fifo-eviction
  (testing "FIFO eviction when capacity exceeded"
    (let ((registry (make-object-registry)))
      ;; Register max objects
      (let ((first-id nil)
            (first-obj (list :first)))
        (setf first-id (register-object first-obj registry))
        ;; Fill to capacity
        (loop for i from 2 to +max-registry-size+
              do (register-object (list i) registry))
        (ok (= +max-registry-size+ (registry-count registry)))
        ;; First object should still be accessible
        (ok (eq first-obj (lookup-object first-id registry)))
        ;; Add one more - should evict the first
        (register-object (list :overflow) registry)
        (ok (= +max-registry-size+ (registry-count registry)))
        ;; First object should now be gone
        (ok (null (lookup-object first-id registry)))))))

;;; Test ID uniqueness

(deftest ids-are-unique
  (testing "each registration gets unique ID"
    (let ((registry (make-object-registry))
          (ids '()))
      (dotimes (i 100)
        (push (register-object (list i) registry) ids))
      ;; All IDs should be unique
      (ok (= 100 (length (remove-duplicates ids)))))))

(deftest ids-monotonically-increase
  (testing "IDs monotonically increase"
    (let ((registry (make-object-registry)))
      (let ((id1 (register-object (list 1) registry))
            (id2 (register-object (list 2) registry))
            (id3 (register-object (list 3) registry)))
        (ok (< id1 id2 id3))))))
