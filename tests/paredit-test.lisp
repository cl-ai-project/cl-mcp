;;;; tests/paredit-test.lisp

(defpackage #:cl-mcp/tests/paredit-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node
                #:make-cst-node
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end))

(in-package #:cl-mcp/tests/paredit-test)

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
      ;; [0] = defun symbol
      (let ((head-node (find-by-path root '(0))))
        (ok (eq (cst-node-value head-node) 'defun)))
      ;; [1] = foo symbol
      (let ((name-node (find-by-path root '(1))))
        (ok (eq (cst-node-value name-node) 'foo)))
      ;; [2] = (x) lambda list
      (let ((ll (find-by-path root '(2))))
        (ok (consp (cst-node-value ll))))
      ;; [3] = (+ x 1) body
      (let ((body (find-by-path root '(3))))
        (ok (consp (cst-node-value body))))
      ;; [3, 0] = + symbol
      (let ((plus (find-by-path root '(3 0))))
        (ok (eq (cst-node-value plus) '+))))))

(deftest transform-wrap-simple
  (testing "wrap a single sexp in parens"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper nil :head nil)))
      (ok (string= result "(a (b) c)")))))

(deftest transform-wrap-with-head
  (testing "wrap with a head operator"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper nil :head "progn")))
      (ok (string= result "(a (progn b) c)")))))

(deftest transform-wrap-multiple
  (testing "wrap multiple consecutive siblings"
    (let* ((text "(a b c d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 2 :wrapper nil :head nil)))
      (ok (string= result "(a (b c) d)")))))

(deftest transform-unwrap-all
  (testing "unwrap keeping all children"
    (let* ((text "(a (b c) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "all")))
      (ok (string= result "(a b c d)")))))

(deftest transform-unwrap-body
  (testing "unwrap keeping only body (drop operator)"
    (let* ((text "(a (progn x y) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "body")))
      (ok (string= result "(a x y d)")))))

(deftest transform-slurp-forward
  (testing "slurp pulls next sibling into list"
    (let* ((text "(a (b) c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-SLURP-FORWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a (b c))")))))

(deftest transform-slurp-forward-multi
  (testing "slurp pulls multiple following siblings into list (inspired by Emacs paredit tests)"
    (let* ((text "(a (b) c d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-SLURP-FORWARD" :cl-mcp/src/paredit))
                    :count 2)))
      (ok (string= result "(a (b c d))")))))

(deftest transform-slurp-backward
  (testing "slurp pulls previous sibling into list"
    (let* ((text "(a b (c))")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-SLURP-BACKWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a (b c))")))))

(deftest transform-slurp-backward-multi
  (testing "slurp pulls multiple preceding siblings into list (inspired by Emacs paredit tests)"
    (let* ((text "(w x y (z))")
           (result (resolve-and-transform
                    text '(3)
                    (symbol-function (find-symbol "TRANSFORM-SLURP-BACKWARD" :cl-mcp/src/paredit))
                    :count 2)))
      (ok (string= result "(w (x y z))")))))

(deftest transform-barf-forward
  (testing "barf pushes last child out of list"
    (let* ((text "(a (b c) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-BARF-FORWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a (b) c d)")))))

(deftest transform-barf-forward-multi
  (testing "barf pushes multiple trailing children out of list (inspired by Emacs paredit tests)"
    (let* ((text "(a (b c d e) f)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-BARF-FORWARD" :cl-mcp/src/paredit))
                    :count 2)))
      (ok (string= result "(a (b c) d e f)")))))

(deftest transform-barf-backward
  (testing "barf pushes first child out of list"
    (let* ((text "(a (b c) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-BARF-BACKWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a b (c) d)")))))

(deftest transform-barf-backward-multi
  (testing "barf pushes multiple leading children out of list (inspired by Emacs paredit tests)"
    (let* ((text "(a (b c d e) f)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-BARF-BACKWARD" :cl-mcp/src/paredit))
                    :count 2)))
      (ok (string= result "(a b c (d e) f)")))))

(deftest transform-raise
  (testing "raise replaces parent with child"
    (let* ((text "(a (if t x nil) b)")
           ;; raise x (path [1, 2]) — x inside (if t x nil)
           (result (resolve-and-transform
                    text '(1 2)
                    (symbol-function (find-symbol "TRANSFORM-RAISE" :cl-mcp/src/paredit)))))
      (ok (string= result "(a x b)")))))

(deftest transform-kill
  (testing "kill removes a sexp"
    (let* ((text "(a b c d)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-KILL" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a b d)")))))

(deftest transform-kill-first
  (testing "kill removes first child and normalizes leading whitespace"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-KILL" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(b c)")))))

(deftest transform-kill-last
  (testing "kill removes last child and trims trailing whitespace"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-KILL" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(a b)")))))

(deftest transform-kill-multiple
  (testing "kill removes multiple consecutive children (inspired by Emacs paredit-kill tests)"
    (let* ((text "(a b c d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-KILL" :cl-mcp/src/paredit))
                    :count 2)))
      (ok (string= result "(a d)")))))

(deftest transform-transpose
  (testing "transpose swaps adjacent siblings"
    (let* ((text "(list alpha beta)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-TRANSPOSE" :cl-mcp/src/paredit)))))
      (ok (string= result "(list beta alpha)")))))

(deftest transform-split
  (testing "split a list into two"
    (let* ((text "(progn a b c)")
           ;; split at b (child index 2 of progn form)
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head nil)))
      (ok (string= result "(progn a) (b c)")))))

(deftest transform-split-clone-head
  (testing "split with cloned head"
    (let* ((text "(progn a b c)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head t)))
      (ok (string= result "(progn a) (progn b c)")))))

(deftest transform-join
  (testing "join two adjacent lists"
    (let* ((text "((a b) (c d))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head nil)))
      (ok (string= result "((a b c d))")))))

(deftest transform-join-drop-head
  (testing "join with drop-head"
    (let* ((text "((progn a) (progn b))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head t)))
      (ok (string= result "((progn a b))")))))

;;; ----------------------------------------------------------------
;;; Additional tests inspired by Emacs paredit test.el
;;; (Taylor R. Campbell, paredit.org)
;;; ----------------------------------------------------------------

;;; --- Wrap edge cases ---

(deftest wrap-square-brackets
  (testing "wrap with square brackets (from Emacs paredit-wrap-square)"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper "square" :head nil)))
      (ok (string= result "(a [b] c)")))))

(deftest wrap-curly-brackets
  (testing "wrap with curly brackets (from Emacs paredit-wrap-curly)"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper "curly" :head nil)))
      (ok (string= result "(a {b} c)")))))

(deftest wrap-all-remaining
  (testing "wrap count exceeding remaining siblings caps gracefully"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 5 :wrapper nil :head nil)))
      (ok (string= result "(a (b c))")))))

(deftest wrap-last-element
  (testing "wrap the last element of a list"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper nil :head nil)))
      (ok (string= result "(a b (c))")))))

(deftest wrap-nested-list
  (testing "wrap a nested list form"
    (let* ((text "(a (b c) d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 1 :wrapper nil :head nil)))
      (ok (string= result "(a ((b c)) d)")))))

(deftest wrap-with-head-multiple
  (testing "wrap multiple siblings with head operator"
    (let* ((text "(a b c d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 2 :wrapper nil :head "when")))
      (ok (string= result "(a (when b c) d)")))))

;;; --- Unwrap/Splice edge cases (from Emacs paredit-splice-sexp) ---

(deftest unwrap-single-child
  (testing "unwrap a list with one child — splice identity"
    (let* ((text "(a (b) c)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "all")))
      (ok (string= result "(a b c)")))))

(deftest unwrap-deeply-nested
  (testing "unwrap innermost form of deeply nested structure (from Emacs splice tests)"
    (let* ((text "(f (g (h x)))")
           (result (resolve-and-transform
                    text '(1 1)
                    (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "all")))
      (ok (string= result "(f (g h x))")))))

(deftest unwrap-body-multi-child
  (testing "unwrap body with multiple body forms (from Emacs splice tests)"
    (let* ((text "(outer (let ((x 1)) body1 body2))")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "body")))
      (ok (string= result "(outer ((x 1)) body1 body2)")))))

(deftest unwrap-nested-lists
  (testing "unwrap a form containing nested lists"
    (let* ((text "(a (b (c d) (e f)) g)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "all")))
      (ok (string= result "(a b (c d) (e f) g)")))))

;;; --- Raise edge cases (from Emacs paredit-raise-sexp) ---

(deftest raise-list-form
  (testing "raise a list form, not just an atom (from Emacs raise tests)"
    (let* ((text "(a (b (c d)) e)")
           (result (resolve-and-transform
                    text '(1 1)
                    (symbol-function (find-symbol "TRANSFORM-RAISE" :cl-mcp/src/paredit)))))
      (ok (string= result "(a (c d) e)")))))

(deftest raise-first-child
  (testing "raise the first child of a subform"
    (let* ((text "(a (if test then else) b)")
           (result (resolve-and-transform
                    text '(1 0)
                    (symbol-function (find-symbol "TRANSFORM-RAISE" :cl-mcp/src/paredit)))))
      (ok (string= result "(a if b)")))))

(deftest raise-from-deep-nesting
  (testing "raise from three levels deep (from Emacs raise tests)"
    (let* ((text "(let ((x 5)) (let ((y 3)) (foo bar baz)))")
           (result (resolve-and-transform
                    text '(2 2 1)
                    (symbol-function (find-symbol "TRANSFORM-RAISE" :cl-mcp/src/paredit)))))
      (ok (string= result "(let ((x 5)) (let ((y 3)) bar))")))))

(deftest raise-replaces-entire-parent
  (testing "raise single atom replaces entire enclosing list"
    (let* ((text "((when t (do-something) (do-other)))")
           (result (resolve-and-transform
                    text '(0 2)
                    (symbol-function (find-symbol "TRANSFORM-RAISE" :cl-mcp/src/paredit)))))
      (ok (string= result "((do-something))")))))

;;; --- Transpose edge cases ---

(deftest transpose-list-forms
  (testing "transpose two list forms (not just atoms)"
    (let* ((text "((a b) (c d))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-TRANSPOSE" :cl-mcp/src/paredit)))))
      (ok (string= result "((c d) (a b))")))))

(deftest transpose-mixed-atom-and-list
  (testing "transpose atom with following list"
    (let* ((text "(foo x (bar baz))")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-TRANSPOSE" :cl-mcp/src/paredit)))))
      (ok (string= result "(foo (bar baz) x)")))))

(deftest transpose-preserves-surrounding
  (testing "transpose in the middle preserves siblings on both sides"
    (let* ((text "(a b c d e)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-TRANSPOSE" :cl-mcp/src/paredit)))))
      (ok (string= result "(a b d c e)")))))

;;; --- Split edge cases ---

(deftest split-at-second-element
  (testing "split at second element yields two halves"
    (let* ((text "(a b c d)")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head nil)))
      (ok (string= result "(a) (b c d)")))))

(deftest split-at-last-element
  (testing "split at last element"
    (let* ((text "(a b c)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head nil)))
      (ok (string= result "(a b) (c)")))))

(deftest split-nested-form
  (testing "split inside a parent preserves outer structure"
    (let* ((text "(outer (a b c d))")
           (result (resolve-and-transform
                    text '(1 2)
                    (symbol-function (find-symbol "TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head nil)))
      (ok (string= result "(outer (a b) (c d))")))))

(deftest split-clone-head-let
  (testing "split a let-like form cloning head (from Emacs paredit split semantics)"
    (let* ((text "(let bindings body1 body2)")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head t)))
      (ok (string= result "(let bindings) (let body1 body2)")))))

;;; --- Join edge cases (from Emacs paredit-join-sexps) ---

(deftest join-adjacent-no-space
  (testing "join adjacent lists (from Emacs: (x)|(y) → (x y))"
    (let* ((text "((x) (y))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head nil)))
      (ok (string= result "((x y))")))))

(deftest join-nested-lists
  (testing "join lists containing nested structures (from Emacs join tests)"
    (let* ((text "((a (b c)) ((d e) f))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head nil)))
      (ok (string= result "((a (b c) (d e) f))")))))

(deftest join-drop-head-matching
  (testing "join with drop-head when both lists share head (from Emacs join + convolute patterns)"
    (let* ((text "((cond (a b)) (cond (c d)))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head t)))
      (ok (string= result "((cond (a b) (c d)))")))))

(deftest join-single-element-lists
  (testing "join two single-element lists"
    (let* ((text "((a) (b))")
           (result (resolve-and-transform
                    text '(0)
                    (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head nil)))
      (ok (string= result "((a b))")))))

;;; --- Error conditions (from Emacs paredit error tests) ---

(defun expect-error (thunk)
  "Call THUNK and return T if it signals a tool-operation-error, NIL otherwise."
  (handler-case (progn (funcall thunk) nil)
    (cl-mcp/src/tools/helpers:tool-operation-error () t)))

(deftest slurp-forward-no-sibling-errors
  (testing "slurp forward with no next sibling signals error (from Emacs slurp error cases)"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a (b c))" '(1)
            (symbol-function (find-symbol "TRANSFORM-SLURP-FORWARD" :cl-mcp/src/paredit))
            :count 1))))))

(deftest slurp-forward-atom-errors
  (testing "slurp forward on atom signals error"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a b c)" '(1)
            (symbol-function (find-symbol "TRANSFORM-SLURP-FORWARD" :cl-mcp/src/paredit))
            :count 1))))))

(deftest slurp-backward-no-sibling-errors
  (testing "slurp backward with no preceding sibling signals error"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "((a b) c)" '(0)
            (symbol-function (find-symbol "TRANSFORM-SLURP-BACKWARD" :cl-mcp/src/paredit))
            :count 1))))))

(deftest barf-forward-insufficient-children-errors
  (testing "barf forward when list has only one child signals error (from Emacs barf error cases)"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a (b) c)" '(1)
            (symbol-function (find-symbol "TRANSFORM-BARF-FORWARD" :cl-mcp/src/paredit))
            :count 1))))))

(deftest barf-backward-insufficient-children-errors
  (testing "barf backward when list has only one child signals error"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a (b) c)" '(1)
            (symbol-function (find-symbol "TRANSFORM-BARF-BACKWARD" :cl-mcp/src/paredit))
            :count 1))))))

(deftest raise-on-root-errors
  (testing "raise on root node signals error"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a b c)" '()
            (symbol-function (find-symbol "TRANSFORM-RAISE" :cl-mcp/src/paredit))))))))

(deftest unwrap-atom-errors
  (testing "unwrap an atom signals error"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a b c)" '(1)
            (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
            :keep "all"))))))

(deftest transpose-no-next-errors
  (testing "transpose with no next sibling signals error (from Emacs transpose error cases)"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a b c)" '(2)
            (symbol-function (find-symbol "TRANSFORM-TRANSPOSE" :cl-mcp/src/paredit))))))))

(deftest join-no-next-errors
  (testing "join with no next sibling signals error (from Emacs join error cases)"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "((a b))" '(0)
            (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
            :drop-head nil))))))

(deftest join-next-not-list-errors
  (testing "join when next sibling is atom signals error (from Emacs join type mismatch)"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "((a b) c)" '(0)
            (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
            :drop-head nil))))))

(deftest kill-on-root-errors
  (testing "kill on root node (no parent) signals error"
    (ok (expect-error
         (lambda ()
           (resolve-and-transform
            "(a b c)" '()
            (symbol-function (find-symbol "TRANSFORM-KILL" :cl-mcp/src/paredit))
            :count 1))))))

;;; --- Realistic Lisp form transformations ---

(deftest slurp-into-let-binding
  (testing "slurp a form into a let body (realistic editing pattern)"
    (let* ((text "(let ((x 1)) (print x) (cleanup))")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-SLURP-FORWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(let ((x 1)) (print x (cleanup)))")))))

(deftest wrap-body-in-handler-case
  (testing "wrap body forms in handler-case (common Lisp editing pattern)"
    (let* ((text "(defun foo () (do-risky) (do-more))")
           (result (resolve-and-transform
                    text '(3)
                    (symbol-function (find-symbol "TRANSFORM-WRAP" :cl-mcp/src/paredit))
                    :count 2 :wrapper nil :head "handler-case")))
      (ok (string= result "(defun foo () (handler-case (do-risky) (do-more)))")))))

(deftest raise-then-body-from-when
  (testing "raise body from (when test body) to just body"
    (let* ((text "(progn (when t (important-thing)) other)")
           (result (resolve-and-transform
                    text '(1 2)
                    (symbol-function (find-symbol "TRANSFORM-RAISE" :cl-mcp/src/paredit)))))
      (ok (string= result "(progn (important-thing) other)")))))

(deftest barf-last-from-progn
  (testing "barf last form out of progn (common refactoring)"
    (let* ((text "(handler-case (progn (setup) (run) (teardown)) (error (e) (log e)))")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-BARF-FORWARD" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(handler-case (progn (setup) (run)) (teardown) (error (e) (log e)))")))))

(deftest kill-cond-clause
  (testing "kill a clause from a cond form"
    (let* ((text "(cond (a 1) (b 2) (t 3))")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-KILL" :cl-mcp/src/paredit))
                    :count 1)))
      (ok (string= result "(cond (a 1) (t 3))")))))

(deftest transpose-cond-clauses
  (testing "transpose two cond clauses (reorder priority)"
    (let* ((text "(cond (a 1) (b 2) (t 3))")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-TRANSPOSE" :cl-mcp/src/paredit)))))
      (ok (string= result "(cond (b 2) (a 1) (t 3))")))))

(deftest split-defun-body
  (testing "split a progn-like body into two forms"
    (let* ((text "(progn (setup) (run) (cleanup))")
           (result (resolve-and-transform
                    text '(2)
                    (symbol-function (find-symbol "TRANSFORM-SPLIT" :cl-mcp/src/paredit))
                    :clone-head t)))
      (ok (string= result "(progn (setup)) (progn (run) (cleanup))")))))

(deftest join-two-progn-forms
  (testing "join two progn forms dropping duplicate head"
    (let* ((text "(outer (progn a b) (progn c d))")
           (result (resolve-and-transform
                    text '(1)
                    (symbol-function (find-symbol "TRANSFORM-JOIN" :cl-mcp/src/paredit))
                    :drop-head t)))
      (ok (string= result "(outer (progn a b c d))")))))

(deftest unwrap-redundant-progn
  (testing "unwrap single-body progn (common cleanup)"
    (let* ((text "(defun foo () (progn (bar)))")
           (result (resolve-and-transform
                    text '(3)
                    (symbol-function (find-symbol "TRANSFORM-UNWRAP" :cl-mcp/src/paredit))
                    :keep "body")))
      (ok (string= result "(defun foo () (bar))")))))

(deftest show-structure-basic
  (testing "show-structure returns tree with paths"
    (let* ((text "(defun foo (x) (+ x 1))")
           (root (parse-single text))
           (structure (funcall (symbol-function
                                (find-symbol "SHOW-STRUCTURE" :cl-mcp/src/paredit))
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
           (target (find-by-path root '(3 1))) ; x inside (+ x 1)
           (info (funcall (symbol-function
                           (find-symbol "GET-ENCLOSING" :cl-mcp/src/paredit))
                          root target text :levels 1)))
      (ok (hash-table-p info))
      (ok (string= (gethash "head" info) "+"))
      (ok (= (gethash "sibling_count" info) 3)))))
