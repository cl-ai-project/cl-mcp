;;;; tests/nesting-test.lisp

(defpackage #:cl-mcp/tests/nesting-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/nesting
                #:json-too-deep-p
                #:lisp-too-deep-p
                #:check-lisp-nesting
                #:+max-lisp-nesting+
                #:+max-json-nesting+)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens)
  (:import-from #:cl-mcp/src/worker-client))

(in-package #:cl-mcp/tests/nesting-test)

(defun %opens (n &optional (char #\())
  (make-string n :initial-element char))

(deftest lisp-depth-counts-only-code-parens
  (testing "the limit itself is allowed and one more is not"
    (ok (not (lisp-too-deep-p (%opens +max-lisp-nesting+))))
    (ok (lisp-too-deep-p (%opens (1+ +max-lisp-nesting+)))))
  (testing "parens that are data do not count"
    (ok (not (lisp-too-deep-p (format nil "\"~A\"" (%opens 900)))) "in a string")
    (ok (not (lisp-too-deep-p (format nil "|~A|" (%opens 900)))) "in a |symbol|")
    (ok (not (lisp-too-deep-p (format nil "; ~A~%x" (%opens 900)))) "in a line comment")
    (ok (not (lisp-too-deep-p (format nil "#| #| |# ~A |# x" (%opens 900))))
        "in a nested block comment")
    (ok (not (lisp-too-deep-p (with-output-to-string (s)
                                (dotimes (i 900) (write-string "#\\( " s)))))
        "as character names")
    (ok (not (lisp-too-deep-p (with-output-to-string (s)
                                (dotimes (i 900) (write-string "\\(" s)))))
        "after a single escape"))
  (testing "a closed list gives its depth back"
    (ok (not (lisp-too-deep-p (with-output-to-string (s)
                                (dotimes (i 900) (write-string "(a)" s))))))))

(deftest check-lisp-nesting-refuses-with-an-error
  (ok (string= "(a)" (check-lisp-nesting "(a)")))
  (ok (handler-case (progn (check-lisp-nesting (%opens 20000)) nil)
        (error (e) (search "levels deep" (princ-to-string e))))))

(deftest json-depth-counts-only-structure
  (ok (not (json-too-deep-p (%opens +max-json-nesting+ #\[))))
  (ok (json-too-deep-p (%opens (1+ +max-json-nesting+) #\{)))
  (ok (not (json-too-deep-p (format nil "[\"\\\"~A\"]" (%opens 5000 #\[))))
      "brackets inside a string, after an escaped quote, are data"))

(deftest lisp-check-parens-reports-deep-code-instead-of-reading-it
  (let ((result (lisp-check-parens
                 :code (concatenate 'string (%opens 20000) (%opens 20000 #\))))))
    (ok (hash-table-p result) "an answer, not a stopped thread")))

(defun %chain (n prefix)
  (with-output-to-string (s)
    (dotimes (i n) (write-string prefix s))
    (write-string "x" s)))

(deftest lisp-depth-counts-prefix-chains
  ;; From review: only ( was counted, and the reader recurses once per prefix
  ;; as well, so 20000 quotes exhausted the parent's reader unrefused.
  (testing "each prefix that reads the object after it is a level"
    (dolist (prefix '("'" "`" "," ",@" "#'" "#+sbcl " "#-(or a b) " "#1=" "'("))
      (ok (lisp-too-deep-p (%chain 20000 prefix)) prefix))
    (ok (not (lisp-too-deep-p (%chain (1- +max-lisp-nesting+) "'")))
        "a chain within the limit is allowed"))
  (testing "prefixes whose object is complete give their level back"
    (ok (not (lisp-too-deep-p (%chain 20000 "'a "))))
    (ok (not (lisp-too-deep-p (%chain 20000 "'(a) "))))
    (ok (not (lisp-too-deep-p (%chain 20000 "#+sbcl a "))))))

(deftest lisp-check-parens-refuses-a-prefix-chain
  (let ((result (lisp-check-parens :code (%chain 20000 "'"))))
    (ok (hash-table-p result) "an answer, not an exhausted stack")))

(deftest worker-answer-too-deep-is-an-error-answer
  ;; The parent parses what the worker encodes, and a client chooses how
  ;; deep that is (preview_max_depth, max_depth).
  (let ((line (format nil "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":~A~A}~%"
                      (%opens 5000 #\[) (%opens 5000 #\]))))
    (with-input-from-string (s line)
      (ok (handler-case
              (progn (cl-mcp/src/worker-client::%read-json-rpc-response s 7 nil) nil)
            (cl-mcp/src/worker-client:worker-rpc-error (e)
              (search "levels deep" (princ-to-string e))))))))
