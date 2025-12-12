;;;; tests/asdf-tools-test.lisp

(defpackage #:cl-mcp/tests/asdf-tools-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/asdf-tools
                #:asdf-system-info
                #:asdf-list-systems))

(in-package #:cl-mcp/tests/asdf-tools-test)

(deftest asdf-system-info-cl-mcp
  (testing "asdf-system-info returns expected metadata for cl-mcp"
    (let ((info (asdf-system-info "cl-mcp")))
      (ok (hash-table-p info))
      (ok (string= "cl-mcp" (gethash "name" info)))
      (ok (vectorp (gethash "depends_on" info)))
      (let ((deps (coerce (gethash "depends_on" info) 'list)))
        (ok (find "alexandria" deps :test #'string=)))
      (ok (vectorp (gethash "defsystem_depends_on" info)))
      (ok (stringp (gethash "source_file" info)))
      (ok (search "cl-mcp.asd" (gethash "source_file" info)))
      (ok (stringp (gethash "source_directory" info)))
      (ok (member (gethash "loaded" info) '(t nil))))))

(deftest asdf-system-info-unknown
  (testing "asdf-system-info signals an error for unknown systems"
    (ok (handler-case
            (progn (asdf-system-info "no-such-system") nil)
          (error () t)))))

(deftest asdf-list-systems-includes-cl-mcp
  (testing "asdf-list-systems returns a vector including cl-mcp"
    (let* ((systems (asdf-list-systems))
           (lst (coerce systems 'list)))
      (ok (vectorp systems))
      (ok (find "cl-mcp" lst :test #'string=)))))
