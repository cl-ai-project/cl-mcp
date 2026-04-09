;;;; tests/project-scaffold-test.lisp

(defpackage #:cl-mcp/tests/project-scaffold-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/project-scaffold-core))

(in-package #:cl-mcp/tests/project-scaffold-test)

(deftest project-scaffold-smoke
  (testing "project-scaffold-core package loads"
    (ok (find-package '#:cl-mcp/src/project-scaffold-core))))

(deftest validate-project-name-valid
  (testing "accepts valid names"
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-lib"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "a"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-123"))))

(deftest validate-project-name-rejects
  (testing "rejects empty string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects uppercase"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "FooLib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects leading digit"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "1foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects underscore"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo_lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects slash"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo/lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects overlong name"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name
                  (make-string 65 :initial-element #\a))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name :foo)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest validate-destination
  (testing "accepts a plain relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds")))
  (testing "accepts a nested relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "work/samples")))
  (testing "rejects absolute path"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "/tmp/foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "../outside")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects embedded parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds/../..")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects empty"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest validate-text-field
  (testing "accepts normal text"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "author" "Ada Lovelace")))
  (testing "accepts empty string"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "description" "")))
  (testing "rejects newline"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "author" (format nil "Ada~%Lovelace"))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects CR"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "description" (format nil "foo~Abar" #\Return))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field "license" 42)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))
