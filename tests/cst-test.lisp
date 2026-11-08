;;;; tests/cst-test.lisp

(defpackage #:cl-mcp/tests/cst-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/source-reader
                #:read-time-evaluation-p
                #:read-time-evaluation-form
                #:structure-literal-p
                #:structure-literal-name
                #:stand-in-symbol-package-name)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node-kind
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line
                #:cst-node-end-line
                #:cst-node-value
                #:text-reached-in-readtable))

(in-package #:cl-mcp/tests/cst-test)

(deftest parse-top-level-forms-captures-comments-and-lines
  (testing "comment nodes are preserved with accurate line numbers"
    (let* ((text (format nil ";; heading~%~%(defun sample ()~%  :ok)~%"))
           (nodes (parse-top-level-forms text))
           (comment (find-if (lambda (n) (eq (cst-node-kind n) :skipped)) nodes))
           (expr (find-if (lambda (n) (eq (cst-node-kind n) :expr)) nodes)))
      (ok comment)
      (ok expr)
      (ok (= 1 (cst-node-start-line comment)))
      (ok (= 3 (cst-node-start-line expr)))
      (ok (<= (cst-node-end-line expr) 4))
      (ok (string= "sample" (string-downcase (second (cst-node-value expr))))))))

(deftest cl-reader-pass-records-a-form-behind-a-false-feature-expression
  ;; Issue #316 review: READ consumed a false #+ and its form without a trace,
  ;; so the CL-reader pass returned no node for it and started the next form's
  ;; span at its #; a replace of the next form overwrote both.
  (let* ((text (format nil "(in-package :cl-user)~%#+cl-mcp-no-such-feature~%~
                            (defun twin () 1)~%#-cl-mcp-no-such-feature~%~
                            (defun twin () 2)~%(defun plain () 3)~%"))
         (nodes (parse-top-level-forms text :readtable :standard))
         (described (mapcar (lambda (node)
                              (list (cst-node-kind node) (cst-node-start-line node)
                                    (subseq text (cst-node-start node) (cst-node-end node))))
                            nodes)))
    (testing "the skipped form is a node of its own, with its reason"
      (let ((skipped (find :skipped nodes :key #'cst-node-kind)))
        (ok (and skipped (= 2 (cst-node-start-line skipped))) (format nil "~S" described))
        (ok (and skipped (eq :sharpsign-plus (car (cst-node-value skipped))))
            "recorded as the Eclector pass records it")))
    (testing "the form that is read starts at its own expression"
      (ok (member (list :expr 4 (format nil "#-cl-mcp-no-such-feature~%(defun twin () 2)"))
                  described :test #'equal)
          (format nil "~S" described))
      (ok (member (list :expr 6 "(defun plain () 3)") described :test #'equal)
          "and a plain form after it as before"))
    (testing "a malformed expression is a read error, as it is to SBCL"
      (ok (nth-value 1 (parse-top-level-forms (format nil "(a)~%#+(not) (b)~%")
                                              :readtable :standard))
          "reported as the second value"))
    (testing "an operator UIOP does not evaluate is a read error, not a signal"
      (ok (nth-value 1 (parse-top-level-forms (format nil "#+(cl:and sbcl) (defun a () 1)~%")
                                              :readtable :standard))
          "reported as the second value"))
    (testing "stacked expressions with no form for the outer one are a read error"
      (let ((err (nth-value 1 (parse-top-level-forms
                               (format nil "(a)~%#+sbcl #+cl-mcp-no-such-feature ~
                                            (defun impl (x) 2)~%")
                               :readtable :standard))))
        (ok (typep err 'cl-mcp/src/cst:unterminated-source)
            "an unterminated source, as the Eclector pass refuses it")))
    (testing "an unterminated block comment there is reported as one"
      (let ((err (nth-value 1 (parse-top-level-forms
                               (format nil "(a)~%#+sbcl #+cl-mcp-no-such-feature (b) #| open")
                               :readtable :standard))))
        (ok (and err (search "block comment" (princ-to-string err)))
            (format nil "~A" err))))
    (testing "the skipped form ends where its form does, a comment after it notwithstanding"
      (let* ((source (format nil "(a)~%#+sbcl #+cl-mcp-no-such-feature (b) ; c~%(d)~%"))
             (skipped (find :skipped (parse-top-level-forms source :readtable :standard)
                            :key #'cst-node-kind)))
        (ok (and skipped (string= "#+sbcl #+cl-mcp-no-such-feature (b)"
                                  (subseq source (cst-node-start skipped)
                                          (cst-node-end skipped))))
            "its span stops after (b)")))
    (testing "an expression with no form after it is a read error"
      (ok (nth-value 1 (parse-top-level-forms (format nil "(a)~%#+sbcl~%")
                                              :readtable :standard))
          "reported as the second value"))))

(deftest both-passes-split-a-twin-off-stacked-expressions
  ;; Second review of #331: behind #+sbcl #+cl-mcp-no-such-feature the reader
  ;; skips the guarded form and the outer #+sbcl takes the next one, which the
  ;; Eclector pass returned as one node starting at the skipped form's #.
  (let ((text (format nil "(in-package :cl-user)~%~%#+sbcl #+cl-mcp-no-such-feature~%~
                           (defun impl (x) 2)~%~%#+sbcl #-cl-mcp-no-such-feature~%~
                           (defun impl (x) 1)~%~%(defun z () 3)~%")))
    (dolist (readtable '(nil :standard))
      (let ((described (mapcar (lambda (node)
                                 (list (cst-node-kind node) (cst-node-start-line node)))
                               (if readtable
                                   (parse-top-level-forms text :readtable readtable)
                                   (parse-top-level-forms text)))))
        (ok (equal '((:expr 1) (:skipped 3) (:expr 6) (:expr 9)) described)
            (format nil "readtable ~S: ~S" readtable described))))))

(deftest parse-top-level-forms-error-suggests-readtable
  (testing "reader error message suggests using readtable parameter"
    (let ((error-message
            (handler-case
                (progn
                  (parse-top-level-forms "(defun foo () #?\"test\")")
                  nil)
              (error (e)
                (format nil "~A" e)))))
      (ok error-message)
      (ok (search "readtable" error-message))
      (ok (search "interpol-syntax" error-message)))))

(defun %expr-values (text)
  "The values of the :EXPR nodes PARSE-TOP-LEVEL-FORMS reads from TEXT, failing
the test when the parse reports an error."
  (multiple-value-bind (nodes swallowed) (parse-top-level-forms text)
    (ok (null swallowed) (format nil "parsed without an error: ~A" swallowed))
    (mapcar #'cst-node-value (remove :expr nodes :key #'cst-node-kind :test-not #'eq))))

(deftest parse-top-level-forms-reads-sharpsign-dot-without-evaluating
  (dolist (prefix (list "" (format nil "(named-readtables:in-readtable :standard)~%")))
    (testing (format nil "#. reads as its form, unevaluated~:[~; (CL-reader pass)~]"
                     (plusp (length prefix)))
      (if (and (plusp (length prefix)) (not (find-package "NAMED-READTABLES")))
          (skip "named-readtables not available")
          (let* ((forms (%expr-values
                         (format nil "~A(defvar *x* #.(error \"evaluated\"))~%(defun y () 1)"
                                 prefix)))
                 (defvar-form (car (last forms 2)))
                 (stand (third defvar-form)))
            (ok (= (if (plusp (length prefix)) 3 2) (length forms))
                "the form after the #. is read too")
            (ok (read-time-evaluation-p stand))
            (ok (string= "ERROR" (symbol-name (first (read-time-evaluation-form stand)))))
            (ok (string= "#.(error \"evaluated\")"
                         (let ((*print-case* :downcase))
                           (prin1-to-string stand)))))))))

(deftest parse-top-level-forms-reads-a-sharpsign-dot-feature-expression-as-holding
  (dolist (prefix (list "" (format nil "(named-readtables:in-readtable :standard)~%")))
    (testing (format nil "#+#.(...) keeps its form, #-#.(...) skips it~:[~; (CL-reader pass)~]"
                     (plusp (length prefix)))
      (if (and (plusp (length prefix)) (not (find-package "NAMED-READTABLES")))
          (skip "named-readtables not available")
          (let ((nodes (parse-top-level-forms
                        (format nil "~A#+#.(cl:if (cl:find-package \"NO-SUCH-319\") '(:and) '(:or))~%~
                                     (defun kept () 1)~%~
                                     #-(:and :sbcl #.(error \"never\"))~%(defun skipped () 2)~%~
                                     (defun after () 3)"
                                prefix))))
            (ok (equal '("KEPT" "AFTER")
                       (loop for node in nodes
                             when (and (eq :expr (cst-node-kind node))
                                       (consp (cst-node-value node))
                                       (string= "DEFUN" (symbol-name
                                                         (first (cst-node-value node)))))
                               collect (symbol-name (second (cst-node-value node)))))
                "the form under #+ is read; the one under #- is skipped"))))))

(deftest parse-top-level-forms-splits-a-twin-behind-a-sharpsign-dot-expression
  (testing "#+#.(...) #+ecl (defun impl) is a skipped twin, not part of the next form"
    (let* ((nodes (parse-top-level-forms
                   (format nil "#+#.(cl:if t '(:and) '(:or)) #+ecl (defun impl () 1)~%~
                                (defun next () 2)")))
           (exprs (remove :expr nodes :key #'cst-node-kind :test-not #'eq))
           (next (find-if (lambda (node)
                            (let ((value (cst-node-value node)))
                              (and (consp value) (consp (cdr value))
                                   (symbolp (second value))
                                   (string= "NEXT" (symbol-name (second value))))))
                          exprs)))
      (ok next)
      (ok (and next (= 2 (cst-node-start-line next))
               (< 50 (cst-node-start next)))
          "NEXT starts at its own text, not at the #+ of the skipped twin")
      (ok (some (lambda (node)
                  (and (eq :skipped (cst-node-kind node))
                       (consp (cst-node-value node))))
                nodes)
          "the twin is listed as skipped"))))

(deftest parse-top-level-forms-reads-sbcl-character-names-and-sharpsign-s
  (dolist (prefix (list "" (format nil "(named-readtables:in-readtable :standard)~%")))
    (testing (format nil "SBCL's character names and #S~:[~; (CL-reader pass)~]"
                     (plusp (length prefix)))
      (if (and (plusp (length prefix)) (not (find-package "NAMED-READTABLES")))
          (skip "named-readtables not available")
          (let* ((forms (%expr-values
                         (format nil "~A(defparameter *c* '(#\\Nul #\\Esc #\\U+2192 ~
                                      #\\RIGHTWARDS_DOUBLE_ARROW #\\Greek_Small_Letter_Lambda))~%~
                                      (defparameter *s* #S(no-such-struct-319 :x 1))"
                                 prefix)))
                 (chars (second (third (car (last forms 2)))))
                 (literal (third (car (last forms)))))
            (ok (equal chars (mapcar #'name-char '("Nul" "Esc" "U+2192"
                                                   "RIGHTWARDS_DOUBLE_ARROW"
                                                   "Greek_Small_Letter_Lambda"))))
            (ok (structure-literal-p literal))
            (ok (string= "NO-SUCH-STRUCT-319"
                         (symbol-name (structure-literal-name literal)))))))))

(deftest parse-top-level-forms-never-interns-into-a-real-package
  (testing "a symbol a real package lacks reads as a stand-in, interned nowhere"
    (let* ((form (first (%expr-values
                         "(defun f (l) (uiop:no-such-function-320 l) (cl::no-such-symbol-320))")))
           (missing (first (fourth form)))
           (locked (first (fifth form))))
      (ok (null (find-symbol "NO-SUCH-FUNCTION-320" "UIOP")))
      (ok (null (find-symbol "NO-SUCH-SYMBOL-320" "CL")) "CL's package lock was never met")
      (ok (string= "NO-SUCH-FUNCTION-320" (symbol-name missing)))
      (ok (null (symbol-package missing)))
      (ok (equal "UIOP" (stand-in-symbol-package-name missing)))
      (ok (equal "CL" (stand-in-symbol-package-name locked))
          "the package is named as the token wrote it")))
  (testing "a symbol the package has reads as itself, internal ones included"
    (let ((form (first (%expr-values "(list uiop:getenv cl-mcp/src/cst:%pos->line)"))))
      (ok (eq (second form) 'uiop:getenv))
      (ok (eq (third form) 'cl-mcp/src/cst::%pos->line)))))

(deftest source-read-from-string-uses-the-parser-s-reader-for-standard-syntax
  (testing "a copy of the standard readtable is standard syntax"
    ;; SBCL gives every copy a fresh # dispatcher closure, so comparing that
    ;; function by EQ sent every validation to the CL reader (Codex, PR #336).
    (ok (cl-mcp/src/source-reader:standard-syntax-readtable-p (copy-readtable nil)))
    (let ((custom (copy-readtable nil)))
      (set-macro-character #\! (lambda (stream char) (declare (ignore stream char)) :bang)
                           nil custom)
      (ok (not (cl-mcp/src/source-reader:standard-syntax-readtable-p custom))
          "a readtable with a macro character of its own is not")))
  (testing "under a copy of the standard readtable, content reads as files do"
    (let ((*readtable* (copy-readtable nil)))
      (cl-mcp/src/utils/lenient-read:call-with-lenient-packages
       (lambda ()
         (let ((missing (cl-mcp/src/source-reader:source-read-from-string
                         "(uiop:no-such-function-336)")))
           (ok (null (symbol-package (first missing))) "a stand-in, not an error")
           (ok (null (find-symbol "NO-SUCH-FUNCTION-336" "UIOP"))))
         (ok (equal '(:kept)
                    (cl-mcp/src/source-reader:source-read-from-string
                     "(#+#.(cl:if t '(:and) '(:or)) :kept)"))
             "a #. feature expression holds"))))))

(deftest parse-top-level-forms-unknown-package-succeeds
  (testing "files with unknown package-qualified symbols parse successfully"
    (let* ((text (format nil "(defpackage #:demo (:use #:cl))~%~%(in-package #:demo)~%~%(defun process-data (x)~%  (unknown-dep:transform x))~%"))
           (nodes (parse-top-level-forms text))
           (expr-nodes (remove-if-not
                        (lambda (n) (eq (cst-node-kind n) :expr))
                        nodes)))
      (ok (= 3 (length expr-nodes))
          (format nil "Expected 3 expr nodes, got ~A" (length expr-nodes))))))

(deftest parse-top-level-forms-defmethod-unknown-class
  (testing "defmethod with unknown package-qualified specializer parses successfully"
    (let* ((text (format nil "(defmethod render ((widget ui-pkg:button))~%  (format nil \"<button>~~A</button>\" (ui-pkg:label widget)))~%"))
           (nodes (parse-top-level-forms text))
           (expr-nodes (remove-if-not
                        (lambda (n) (eq (cst-node-kind n) :expr))
                        nodes)))
      (ok (= 1 (length expr-nodes))
          (format nil "Expected 1 expr node, got ~A" (length expr-nodes))))))

(deftest parse-top-level-forms-classifies-stray-close-paren
  (testing "a stray ) on the Eclector path is a stray-right-parenthesis"
    (ok (handler-case (progn (parse-top-level-forms "(a) )") nil)
          (cl-mcp/src/cst::stray-right-parenthesis () t)
          (error () nil))))
  (testing "a stray ) after an in-readtable switch is classified the same way"
    (if (find-package "NAMED-READTABLES")
        (multiple-value-bind (nodes swallowed)
            (parse-top-level-forms
             (format nil "(named-readtables:in-readtable :standard)~%(a) )"))
          (ok (= 2 (count :expr nodes :key #'cst-node-kind))
              "the forms before the stray ) are still returned")
          (ok (typep swallowed 'cl-mcp/src/cst::stray-right-parenthesis)
              "the swallowed error is the classified condition"))
        (rove:skip "named-readtables not available")))
  (testing "a macro that reads a balanced list and then fails keeps its own error"
    ;; #C(...) consumes "(1 2 3)" and signals afterwards: the text before that )
    ;; does not read as nothing, so it is not a stray ).
    (ok (handler-case (progn (parse-top-level-forms "(a) #C(1 2 3)") nil)
          (cl-mcp/src/cst::stray-right-parenthesis () nil)
          (end-of-file () nil)
          (error () t)))))

(deftest text-reached-in-readtable-follows-the-reader
  (let ((broken (format nil "(defun clamp (count room)~%  (if (< room 0)~%      0~%  (min count room))")))
    (testing "a top-level IN-READTABLE the reader reaches yields its designator"
      (ok (eq :foo (text-reached-in-readtable (format nil "(in-readtable :foo)~%~A" broken))))
      (ok (eq :standard
              (text-reached-in-readtable
               (format nil "(named-readtables:in-readtable :standard)~%~A" broken)))
          "the designator is returned as written, so a caller can tell :standard apart")
      (ok (eq :foo (text-reached-in-readtable
                    (format nil "(in-package :cl-user)~%(NAMED-READTABLES:IN-READTABLE :FOO)~%~A"
                            broken))))
      (ok (eq :foo (text-reached-in-readtable
                    (format nil "(~%  ;; choose syntax~%  NAMED-READTABLES:IN-READTABLE :FOO)~%~A"
                            broken)))
          "a line comment inside the form is whitespace to the reader")
      (ok (eq :foo (text-reached-in-readtable
                    (format nil "(#| comment |# NAMED-READTABLES:IN-READTABLE :FOO)~%~A" broken)))
          "so is a block comment"))
    (testing "PR #184 third review: data, text and unreachable forms do not count"
      (ok (null (text-reached-in-readtable
                 (format nil "(defparameter *example*~%  '(in-readtable :foo))~%~%~A" broken)))
          "a quoted list is data")
      (ok (null (text-reached-in-readtable (format nil "'(in-readtable :foo)~%~A" broken))))
      (ok (null (text-reached-in-readtable
                 (format nil ";; This file does not use in-readtable.~%~A" broken))))
      (ok (null (text-reached-in-readtable
                 (format nil "(defvar *doc* \"(in-readtable :foo)\")~%~A" broken))))
      (ok (null (text-reached-in-readtable (format nil "~A~%~%(in-readtable :foo)~%" broken)))
          "the reader stops at the broken form and never reaches the declaration")
      (ok (null (text-reached-in-readtable "(when x (in-readtable :foo))"))
          "only a top-level form switches the readtable"))
    (testing "reading leaves no symbols behind"
      (let ((name "READER-PROBE-SYMBOL-FOR-183"))
        (text-reached-in-readtable (format nil "(~A 1)~%(in-readtable :foo)" name))
        (ok (null (find-symbol name :cl-user)))
        (ok (null (find-symbol name *package*)))))))

(defun %spans (text &rest options)
  "Return the (START END) of every :expr node PARSE-TOP-LEVEL-FORMS gives TEXT."
  (loop for node in (apply #'parse-top-level-forms text options)
        when (eq :expr (cst-node-kind node))
          collect (list (cst-node-start node) (cst-node-end node))))

(deftest reader-pass-ends-a-node-where-the-form-ends
  ;; Issue #191: READ consumed the whitespace after a top-level form, so under
  ;; the CL-reader pass END was one past the form and every caller editing
  ;; next to it saw the newline as part of the form.
  (let ((form "(defun a () 1)")
        (tails (list (string #\Newline)
                     (format nil "~C~C" #\Return #\Newline)
                     " "
                     (string #\Tab)
                     "")))
    (dolist (tail tails)
      (let* ((text (concatenate 'string form tail))
             (expected (%spans text)))
        (ok (equal '((0 14)) expected) (format nil "standard syntax, tail ~S" tail))
        (ok (equal expected (%spans text :readtable :standard))
            (format nil "explicit :standard, tail ~S" tail))
        (ok (equal '((42 56))
                   (rest (%spans (concatenate 'string
                                              "(named-readtables:in-readtable :standard)"
                                              (string #\Newline) text))))
            (format nil "after in-readtable, tail ~S" tail))))))
