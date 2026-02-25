;;;; tests/paredit-test.lisp
;;;;
;;;; Tests for paredit structural editing tools.
;;;; Tests the internal transform functions directly (no file I/O).

(defpackage #:cl-mcp/tests/paredit-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end))

(in-package #:cl-mcp/tests/paredit-test)

;;; We test the internal %transform-* functions by calling them through
;;; the package. Since they're not exported, we use the full symbol path.

(defun parse-single (text)
  "Parse TEXT and return the single top-level form node."
  (first (parse-top-level-forms text)))

(defun find-by-path (root path)
  "Navigate to a child node by PATH (list of indices) through :expr children."
  (let ((current root))
    (dolist (idx path current)
      (let ((children (remove-if-not
                       (lambda (c)
                         (and (typep c 'cst-node) (eq (cst-node-kind c) :expr)))
                       (cst-node-children current))))
        (setf current (nth idx children))))))

(defun resolve-and-transform (text target-path transform-fn &rest args)
  "Helper: parse TEXT, navigate to TARGET-PATH, apply TRANSFORM-FN."
  (let* ((nodes (parse-top-level-forms text))
         (root (if (= 1 (length nodes))
                   (first nodes)
                   (make-cst-node :kind :expr :value nil :children nodes
                                  :start 0 :end (length text))))
         (target (find-by-path root target-path)))
    (apply transform-fn text root target args)))

;;;; ====================================================================
;;;; Addressing Tests
;;;; ====================================================================

(deftest parse-simple-form
  (testing "parse a single defun and verify children"
    (let* ((text "(defun foo (x) (+ x 1))")
           (node (parse-single text)))
      (ok (typep node 'cst-node))
      (ok (eq (cst-node-kind node) :expr))
      (ok (consp (cst-node-value node)))
      (ok (= (cst-node-start node) 0))
      (ok (= (cst-node-end node) (length text))))))

(deftest navigate-by-path
  (testing "path navigation reaches correct child"
    (let* ((text "(defun foo (x) (+ x 1))")
           (root (parse-single text)))
      ;; [0] = foo symbol
      (let ((name-node (find-by-path root '(0))))
        (ok (eq (cst-node-value name-node) 'foo)))
      ;; [1] = (x) lambda list
      (let ((ll (find-by-path root '(1))))
        (ok (consp (cst-node-value ll))))
      ;; [2] = (+ x 1) body
      (let ((body (find-by-path root '(2))))
        (ok (consp (cst-node-value body))))
      ;; [2, 0] = + symbol
      (let ((plus (find-by-path root '(2 0))))
        (ok (eq (cst-node-value plus) '+))))))

;;;; ====================================================================
;;;; Wrap / Unwrap Tests
;;;; ====================================================================

(deftest transform-wrap-simple
  (testing "wrap a single sexp in parens"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper nil :head nil)))
      (ok (string= result "(a (b) c)")))))

(deftest transform-wrap-with-head
  (testing "wrap with a head operator"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper nil :head "progn")))
      (ok (string= result "(a (progn b) c)")))))

(deftest transform-wrap-multiple
  (testing "wrap multiple consecutive siblings"
    (let* ((text "(a b c d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 2 :wrapper nil :head nil)))
      (ok (string= result "(a (b c) d)")))))

(deftest transform-unwrap-all
  (testing "unwrap keeping all children"
    (let* ((text "(a (b c) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "all")))
      (ok (string= result "(a b c d)")))))

(deftest transform-unwrap-body
  (testing "unwrap keeping only body (drop operator)"
    (let* ((text "(a (progn x y) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "body")))
      (ok (string= result "(a x y d)")))))

;;;; ====================================================================
;;;; Slurp / Barf Tests
;;;; ====================================================================

(deftest transform-slurp-forward
  (testing "slurp pulls next sibling into list"
    (let* ((text "(a (b) c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-SLURP-FORWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a (b c))")))))

(deftest transform-slurp-backward
  (testing "slurp pulls previous sibling into list"
    (let* ((text "(a b (c))")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "%TRANSFORM-SLURP-BACKWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a (b c))")))))

(deftest transform-barf-forward
  (testing "barf pushes last child out of list"
    (let* ((text "(a (b c) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-BARF-FORWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a (b) c d)")))))

(deftest transform-barf-backward
  (testing "barf pushes first child out of list"
    (let* ((text "(a (b c) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-BARF-BACKWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a b (c) d)")))))

;;;; ====================================================================
;;;; Raise / Kill / Transpose Tests
;;;; ====================================================================

(deftest transform-raise
  (testing "raise replaces parent with child"
    (let* ((text "(a (if t x nil) b)")
           ;; raise x (path [1, 2]) — x inside (if t x nil)
           (result (resolve-and-transform
                    text '(1 2)
                    (symbol-function (find-symbol "%TRANSFORM-RAISE" :cl-mcp/src/paredit)))))
      (ok (string= result "(a x b)")))))

(deftest transform-kill
  (testing "kill removes a sexp"
    (let* ((text "(a b c d)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "%TRANSFORM-KILL" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a b d)")))))

(deftest transform-transpose
  (testing "transpose swaps adjacent siblings"
    (let* ((text "(list alpha beta)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "%TRANSFORM-TRANSPOSE" :cl-mcp/src/paredit)))))
      (ok (string= result "(list beta alpha)")))))

;;;; ====================================================================
;;;; Split / Join Tests
;;;; ====================================================================

(deftest transform-split
  (testing "split a list into two"
    (let* ((text "(progn a b c)")
           ;; split at b (child index 2 of progn form)
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "%TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head nil)))
      (ok (string= result "(progn a) (b c)")))))

(deftest transform-split-clone-head
  (testing "split with cloned head"
    (let* ((text "(progn a b c)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "%TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head t)))
      (ok (string= result "(progn a) (progn b c)")))))

(deftest transform-join
  (testing "join two adjacent lists"
    (let* ((text "((a b) (c d))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "%TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head nil)))
      (ok (string= result "((a b c d))")))))

(deftest transform-join-drop-head
  (testing "join with drop-head"
    (let* ((text "((progn a) (progn b))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "%TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head t)))
      (ok (string= result "((progn a b))")))))

;;;; ====================================================================
;;;; Query Tests
;;;; ====================================================================

(deftest show-structure-basic
  (testing "show-structure returns tree with paths"
    (let* ((text "(defun foo (x) (+ x 1))")
           (root (parse-single text))
           (structure (funcall (symbol-function
                                (find-symbol "%SHOW-STRUCTURE" :cl-mcp/src/paredit))
                               root text :depth 2)))
      (ok (hash-table-p structure))
      (ok (stringp (gethash "form" structure)))
      (let ((tree (gethash "tree" structure)))
        (ok (vectorp tree))
        (ok (> (length tree) 0))))))

(deftest get-enclosing-basic
  (testing "get-enclosing returns parent info"
    (let* ((text "(defun foo (x) (+ x 1))")
           (root (parse-single text))
           (target (find-by-path root '(2 1))) ; x inside (+ x 1)
           (info (funcall (symbol-function
                           (find-symbol "%GET-ENCLOSING" :cl-mcp/src/paredit))
                          root target text :levels 1)))
      (ok (hash-table-p info))
      (ok (string= (gethash "head" info) "+"))
      (ok (= (gethash "sibling_count" info) 3)))))
