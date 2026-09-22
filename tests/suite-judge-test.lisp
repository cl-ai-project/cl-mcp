;;;; tests/suite-judge-test.lisp
;;;;
;;;; The judge of specs/suite-judge.lisp, on results built from Rove's own
;;;; result classes -- the objects ROVE:RUN leaves in *LAST-SUITE-REPORT* --
;;;; so the checks do not depend on running a nested Rove suite inside this
;;;; one.  In the default suite: needs Rove, not cl-spec.

(defpackage #:cl-mcp/tests/suite-judge-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:rove/core/result
                #:passed-assertion
                #:failed-assertion
                #:pending-assertion
                #:passed-test
                #:failed-test
                #:failed
                #:pending)
  (:import-from #:cl-mcp/specs/suite-judge
                #:judge-suite-results))

(in-package #:cl-mcp/tests/suite-judge-test)

(defun %pass () (make-instance 'passed-assertion :form t))
(defun %fail () (make-instance 'failed-assertion :form nil :reason "it did not hold"))
(defun %skip () (make-instance 'pending-assertion :form t :desc "cl-spec is not loaded"))

(defun %test (name &rest children)
  "Return a test result NAME holding CHILDREN, filed the way Rove files them:
failures under failed, skips under pending, the rest under passed, and a
failed test exactly when something under it failed."
  (let ((failures (remove-if-not (lambda (child) (typep child 'failed)) children))
        (skips (remove-if-not (lambda (child) (typep child 'pending)) children)))
    (make-instance (if failures 'failed-test 'passed-test)
                   :name name
                   :passed (set-difference children (append failures skips))
                   :failed failures
                   :pending skips)))

(defun %judge (required &rest tests)
  "Judge one suite holding TESTS against the REQUIRED names."
  (judge-suite-results (list (apply #'%test "suite" tests)) required))

(deftest every-required-test-ran-and-passed
  (let ((verdict (%judge '("FIRST" "SECOND")
                         (%test 'first (%pass)) (%test 'second (%pass) (%pass)))))
    (ok (getf verdict :ok))
    (ok (equal '("FIRST" "SECOND") (getf verdict :passed)))))

(deftest a-skip-is-not-a-pass
  (let ((skipped (%test 'first (%skip))))
    (ok (typep skipped 'passed-test) "Rove itself files a test that only skipped as passed")
    (let ((verdict (%judge '("FIRST") skipped)))
      (ok (not (getf verdict :ok)))
      (ok (equal '("FIRST") (getf verdict :skipped)))))
  (testing "however deep the skip sits"
    (let ((verdict (%judge '("FIRST")
                           (%test 'first (%pass) (%test "a testing block" (%skip))))))
      (ok (not (getf verdict :ok)))
      (ok (equal '("FIRST") (getf verdict :skipped))))))

(deftest a-failure-fails-the-suite
  (let ((verdict (%judge '("FIRST") (%test 'first (%pass) (%fail)))))
    (ok (not (getf verdict :ok)))
    (ok (equal '("FIRST") (getf verdict :failed))))
  (testing "a failure inside a testing block"
    (let ((verdict (%judge '("FIRST") (%test 'first (%test "block" (%fail))))))
      (ok (not (getf verdict :ok)))
      (ok (equal '("FIRST") (getf verdict :failed))))))

(deftest a-missing-required-test-fails-the-suite
  (let ((verdict (%judge '("FIRST" "SECOND") (%test 'first (%pass)))))
    (ok (not (getf verdict :ok)))
    (ok (equal '("SECOND") (getf verdict :missing)))))

(deftest nothing-ran-is-not-a-pass
  (ok (not (getf (judge-suite-results '() '()) :ok)) "no suite at all")
  (ok (not (getf (%judge '()) :ok)) "a suite with no tests"))

(deftest a-test-that-asserted-nothing-fails-the-suite
  (let ((verdict (%judge '("FIRST") (%test 'first))))
    (ok (not (getf verdict :ok)))
    (ok (equal '("FIRST") (getf verdict :empty)))))

(deftest an-extra-test-is-judged-too
  (testing "passing, it is allowed"
    (let ((verdict (%judge '("FIRST") (%test 'first (%pass)) (%test 'extra (%pass)))))
      (ok (getf verdict :ok))
      (ok (equal '("FIRST" "EXTRA") (getf verdict :ran)))))
  (testing "skipping, it fails the suite like a required one"
    (let ((verdict (%judge '("FIRST") (%test 'first (%pass)) (%test 'extra (%skip)))))
      (ok (not (getf verdict :ok)))
      (ok (equal '("EXTRA") (getf verdict :skipped))))))

(deftest names-match-without-regard-to-case
  (ok (getf (%judge '("real-first") (%test 'real-first (%pass))) :ok))
  (ok (getf (%judge '("REAL-FIRST") (%test 'real-first (%pass))) :ok)))

(deftest tests-are-found-under-a-system-and-a-suite
  ;; The shape ROVE:RUN leaves for a package-inferred system: the system's
  ;; result, holding the package suite's, holding one result per DEFTEST.
  ;; The two outer ones are named by strings, a DEFTEST's by its symbol.
  (let ((report (list (%test "cl-mcp/tests/example"
                             (%test "cl-mcp/tests/example"
                                    (%test 'first (%pass)) (%test 'second (%skip)))))))
    (let ((verdict (judge-suite-results report '("FIRST" "SECOND"))))
      (ok (equal '("FIRST" "SECOND") (getf verdict :ran)))
      (ok (equal '("SECOND") (getf verdict :skipped)))
      (ok (not (getf verdict :ok))))))

(deftest a-skip-outside-any-test-fails-the-suite
  (let ((verdict (judge-suite-results
                  (list (%test "cl-mcp/tests/example" (%test 'first (%pass)) (%skip)))
                  '("FIRST"))))
    (ok (not (getf verdict :ok)))
    (ok (equal '("cl-mcp/tests/example") (getf verdict :skipped)))))
