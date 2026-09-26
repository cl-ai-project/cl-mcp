;;;; tests/server-instructions-test.lisp

(defpackage #:cl-mcp/tests/server-instructions-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/registry
                #:*enabled-tool-groups*
                #:register-tool-group-instructions
                #:enabled-tool-group-instructions))

(in-package #:cl-mcp/tests/server-instructions-test)

(deftest group-instructions-follow-the-enabled-groups
  (register-tool-group-instructions :test-instructions-group "Test group text.")
  (testing "a group that is off contributes nothing"
    (let ((*enabled-tool-groups* '()))
      (ok (not (member "Test group text." (enabled-tool-group-instructions)
                       :test #'string=)))))
  (testing "a group that is on contributes its text"
    (let ((*enabled-tool-groups* (list "TEST-INSTRUCTIONS-GROUP")))
      (ok (member "Test group text." (enabled-tool-group-instructions)
                  :test #'string=))))
  (testing "re-registering replaces the text instead of adding a second one"
    (register-tool-group-instructions "test-instructions-group" "Replaced.")
    (let ((*enabled-tool-groups* (list "TEST-INSTRUCTIONS-GROUP")))
      (ok (equal '("Replaced.") (enabled-tool-group-instructions))))))
