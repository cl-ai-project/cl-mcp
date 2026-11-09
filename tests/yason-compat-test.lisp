;;;; tests/yason-compat-test.lisp

(defpackage #:cl-mcp/tests/yason-compat-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:yason)
  (:import-from #:cl-mcp/src/yason-compat)
  (:import-from #:cl-mcp/src/tools/helpers
                #:json-bool))

(in-package #:cl-mcp/tests/yason-compat-test)

(defun %encode (object)
  (with-output-to-string (s) (yason:encode object s)))

(defun %null-methods ()
  (remove-if-not (lambda (m)
                   (let ((s (first (sb-mop:method-specializers m))))
                     (and (typep s 'sb-mop:eql-specializer)
                          (eq (sb-mop:eql-specializer-object s) :null))))
                 (sb-mop:generic-function-methods #'yason:encode)))

(deftest yason-literals-are-bound
  (testing "yason:true and yason:false evaluate to themselves"
    (ok (eq yason:true 'yason:true))
    (ok (eq yason:false 'yason:false)))
  (testing "json-bool encodes as a JSON boolean"
    (ok (string= "true" (%encode (json-bool t))))
    (ok (string= "false" (%encode (json-bool nil))))))

(deftest keyword-null-encodes-as-null
  (testing ":null encodes as JSON null, alone and nested"
    (ok (string= "null" (%encode :null)))
    (ok (string= "[1,null,false]" (%encode (vector 1 :null yason:false)))))
  (testing "exactly one ENCODE method handles :null"
    (ok (= 1 (length (%null-methods))))))

(deftest preserved-literals-round-trip
  (testing "parsing with literal types kept and encoding again gives the same JSON"
    (let ((json "{\"a\":[null,false,true]}"))
      (ok (string= json
                   (%encode (yason:parse json
                                         :json-arrays-as-vectors t
                                         :json-booleans-as-symbols t
                                         :json-nulls-as-keyword t)))))))
