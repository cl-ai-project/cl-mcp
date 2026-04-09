;;;; tests/project-scaffold-test.lisp

(defpackage #:cl-mcp/tests/project-scaffold-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/project-scaffold-core))

(in-package #:cl-mcp/tests/project-scaffold-test)

(deftest project-scaffold-smoke
  (testing "project-scaffold-core package loads"
    (ok (find-package '#:cl-mcp/src/project-scaffold-core))))
