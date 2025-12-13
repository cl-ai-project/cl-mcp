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

(deftest asdf-list-systems-suppresses-asdf-noise
  (testing "asdf-list-systems suppresses stdout/stderr noise from ASDF"
    (let ((orig (symbol-function 'asdf:registered-systems)))
      (unwind-protect
           (progn
             (setf (symbol-function 'asdf:registered-systems)
                   (lambda ()
                     (format t "NOISE")
                     (format *error-output* "ERR")
                     (funcall orig)))
             (let* ((err (make-string-output-stream))
                    (*error-output* err)
                    (*trace-output* err)
                    systems
                    (out (with-output-to-string (*standard-output*)
                           (setf systems (asdf-list-systems)))))
               (ok (vectorp systems))
               (ok (string= "" out))
               (ok (string= "" (get-output-stream-string err)))))
        (setf (symbol-function 'asdf:registered-systems) orig)))))

(deftest asdf-system-info-suppresses-asdf-noise
  (testing "asdf-system-info suppresses stdout/stderr noise from ASDF"
    (let ((orig (symbol-function 'asdf:find-system)))
      (unwind-protect
           (progn
             (setf (symbol-function 'asdf:find-system)
                   (lambda (&rest args)
                     (format t "NOISE")
                     (format *error-output* "ERR")
                     (apply orig args)))
             (let* ((err (make-string-output-stream))
                    (*error-output* err)
                    (*trace-output* err)
                    info
                    (out (with-output-to-string (*standard-output*)
                           (setf info (asdf-system-info "cl-mcp")))))
               (ok (hash-table-p info))
               (ok (string= "" out))
               (ok (string= "" (get-output-stream-string err)))))
        (setf (symbol-function 'asdf:find-system) orig)))))
