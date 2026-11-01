(defpackage #:cl-mcp/tests/utils-printing-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/printing
                #:safe-prin1
                #:circular-p))

(in-package #:cl-mcp/tests/utils-printing-test)

(deftest safe-prin1-simple-values
  (testing "prints simple values correctly"
    (ok (string= (safe-prin1 42) "42"))
    (ok (string= (safe-prin1 "hello") "\"hello\""))
    (ok (string= (safe-prin1 'foo) "FOO"))
    (ok (string= (safe-prin1 nil) "NIL"))))

(deftest safe-prin1-lists
  (testing "prints lists correctly"
    (ok (string= (safe-prin1 '(1 2 3)) "(1 2 3)"))
    (ok (string= (safe-prin1 '(a b c)) "(A B C)"))))

(deftest safe-prin1-respects-level
  (testing "respects print-level parameter"
    (let ((nested '((((deep))))))
      (ok (search "#" (safe-prin1 nested :level 2))))))

(deftest safe-prin1-respects-length
  (testing "respects print-length parameter"
    (let ((long-list '(1 2 3 4 5 6 7 8 9 10 11 12)))
      (ok (search "..." (safe-prin1 long-list :length 5))))))

(deftest safe-prin1-handles-errors
  (testing "returns error message on print failure"
    ;; Create an object that fails to print
    (let ((result (safe-prin1 (make-array 0 :element-type nil))))
      ;; Should return a string, not signal an error
      (ok (stringp result)))))

(deftest safe-prin1-default-parameters
  (testing "uses sensible defaults"
    ;; Default level is 3, length is 10
    (let ((deep '((((very deep nested structure))))))
      (ok (stringp (safe-prin1 deep))))))

(deftest circular-p-finds-cycles-through-conses-and-vectors
  (testing "a cycle through a cdr, a car or a vector is circular"
    (ok (circular-p (read-from-string "#1=(a b . #1#)")) "cdr")
    (ok (circular-p (read-from-string "#1=(#1#)")) "car")
    (ok (circular-p (read-from-string "#1=#(a #1#)")) "vector")
    (ok (circular-p (read-from-string "(x #1=#(a (b #1#)))")) "vector through a list")
    (ok (circular-p (read-from-string "#1=#2A((a b) (c #1#))")) "multidimensional array")))

(deftest circular-p-accepts-what-is-only-shared-or-long
  (testing "shared structure, strings and long lists are not circular"
    (ok (not (circular-p (read-from-string "(#1=(a b) #1# #1#)"))) "shared, acyclic")
    (ok (not (circular-p (read-from-string "#(#1=#(a) #1#)"))) "shared vector")
    (ok (not (circular-p "string")) "a string is a leaf")
    (ok (not (circular-p 42)) "an atom")
    (ok (not (circular-p (make-list 200000 :initial-element 'x)))
        "a long list does not exhaust the stack")
    (ok (not (circular-p (let ((tree nil))
                           (dotimes (i 40 tree)
                             (setf tree (list tree tree))))))
        "a heavily shared tree is walked once per cons")))

(defstruct circular-p-node
  "A structure for CIRCULAR-P's tests: its default printer writes its slots.
LABEL is any value; NEXT holds what the node refers to, which a test makes
lead back to the node."
  (label nil)  ; Any value.
  (next nil))  ; What the node refers to; a test points it back at the node.

(deftest circular-p-walks-structure-slots
  (testing "a cycle through a structure's slots is circular, a plain one is not"
    (let ((node (make-circular-p-node :label 1)))
      (setf (circular-p-node-next node) (list node))
      (ok (circular-p node) "through a slot and a list")
      (ok (circular-p (list 'quote node)) "inside a quoted literal"))
    (ok (not (circular-p (make-circular-p-node :label 1 :next (make-circular-p-node))))
        "a chain of structures")
    ;; A hash table is a structure in SBCL, and a weak one's storage points
    ;; back at it; it prints as #<...>, so it is not entered.
    (ok (not (circular-p (list (make-hash-table :weakness :key))))
        "a hash table is a leaf")))

(deftest circular-p-conses-only
  (testing "with :conses-only, only a cycle through conses counts"
    (ok (circular-p (read-from-string "#1=(a . #1#)") :conses-only t))
    (ok (not (circular-p (read-from-string "(a #1=#(b #1#))") :conses-only t))
        "a cycle through a vector is not one through conses")
    (let ((node (make-circular-p-node)))
      (setf (circular-p-node-next node) node)
      (ok (not (circular-p (list 'quote node) :conses-only t))
          "nor is one through a structure"))))
