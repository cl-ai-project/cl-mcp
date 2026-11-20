;;;; tests/asdf-tools-test.lisp

(defpackage #:cl-mcp/tests/asdf-tools-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
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

(deftest asdf-test-system-propagates-rove-failures
  ;; The enclosing Rove run may already be inside this ASDF test operation.
  (let* ((asdf/session:*asdf-session* nil)
         (run (symbol-function 'rove:run))
         (report (make-string-output-stream))
         (*error-output* (make-string-output-stream))
         (rove:*rove-error-output* *error-output*)
         (rove:*default-reporter* :spec)
         (rove:*last-suite-report* nil)
         (rove:*report-stream* report))
    (flet ((test-fixture (fixture)
             ;; Run one real Rove suite to avoid recursively running this test
             ;; or starting the full tier's worker processes.
             (unwind-protect
                  (progn
                    (setf (symbol-function 'rove:run)
                          (lambda (suites)
                            (declare (ignore suites))
                            (funcall run fixture)))
                    (handler-case
                        (progn (asdf:test-system "cl-mcp") nil)
                      (error (condition) condition)))
               (setf (symbol-function 'rove:run) run))))
      (testing "ASDF reports a Rove failure after Rove names the failing assertion"
        (let ((failure (test-fixture "cl-mcp/tests/test-runner-test-failures")))
          (ok (typep failure 'error))
          (ok (search "1 should equal 2" (get-output-stream-string report)))))
      (testing "A passing Rove suite lets ASDF finish normally"
        (ok (null (test-fixture "cl-mcp/tests/utils-strings-test")))
        (ok (search "CL-MCP/TESTS/UTILS-STRINGS-TEST"
                    (get-output-stream-string report) :test #'char-equal))))))
