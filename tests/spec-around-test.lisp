;;;; tests/spec-around-test.lisp

(defpackage #:cl-mcp/tests/spec-around-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok #:skip)
  (:import-from #:cl-mcp/tests/spec-input-test #:input-check #:field)
  (:import-from #:cl-mcp/tests/spec-integration-test #:%contracts-available-p)
  (:import-from #:cl-mcp/tests/spec-tools-test #:%call))

(in-package #:cl-mcp/tests/spec-around-test)

(declaim (optimize (debug 3) (safety 3)))

(deftest around-fixtures-report-case-and-lifecycle-through-spec-tools
  (if (and (%contracts-available-p)
           (fboundp (find-symbol "SCHEMA-INFO" "CL-SPEC"))
           (member :around (getf (getf (uiop:symbol-call :cl-spec :schema-info)
                                      :fixture-protocol) :isolations)))
      (progv (list (find-symbol "*REGISTRY*" "CL-SPEC"))
             (list (uiop:symbol-call :cl-spec :make-hash-table-registry))
        (load (asdf:system-relative-pathname "cl-mcp" "tests/fixtures/spec-around-contracts.lisp"))
        (loop for suffix in '("identity" "wrapper-error" "cleanup-error")
              for phase in '(nil "around" "cleanup")
              for name = (concatenate 'string "cl-mcp/tests/spec-around-test::around-" suffix)
              for direct = (input-check name "recipe" 3)
              for check = (gethash "result" (%call "spec-check" (format nil
                            "{\"function\":~S,\"trials\":2,\"seed\":\"42\"}" name)))
              for batch = (gethash "result" (%call "spec-check-batch" (format nil
                            "{\"functions\":[~S],\"trials\":2}" name)))
              for result = (aref (gethash "results" check) 0)
              for batch-item = (aref (gethash "results" batch) 0)
              for data = (field result "core_result" "data")
              for lifecycle = (field direct "lifecycle")
              do (ok (equal "around" (field data "fixture" "isolation")))
                 (ok (field data "fixture" "around"))
                 (ok (equal (field result "status") (field batch-item "status")))
                 (ok (equal "around" (field batch-item "core_result" "data" "fixture" "isolation")))
                 (ok (equal "around" (field lifecycle "isolation")))
                 (ok (equal "positive" (field direct "selected_case")))
                 (if phase
                     (progn
                       (ok (equal "error" (field direct "status")))
                       (ok (equal phase (field (aref (gethash "errors" lifecycle) 0) "phase")))
                       (ok (equal "error" (field result "status")))
                       (ok (eql 1 (field data "case_report" "fixture_errors")))
                       (ok (equal "positive" (field data "run_error" "call_evidence" "case")))
                       (ok (equal phase (field (aref (field data "run_error" "lifecycle" "errors") 0)
                                               "phase")))
                       (ok (not (eq t (field check "verified"))))
                       (ok (not (eq t (field batch "verified")))))
                     (progn
                       (ok (equal "passed" (field direct "status")))
                       (ok (equal "completed" (field lifecycle "around")))
                       (ok (equal "passed" (field result "status")))
                       (ok (eq t (field check "verified")))
                       (ok (eq t (field batch "verified")))))))
      (skip "This cl-spec predates around isolation.")))
