;;;; specs/suite-judge.lisp
;;;;
;;;; Judge a Rove run by the results Rove recorded, test by test, rather than
;;;; by whether ROVE:RUN returned true.  Needs Rove and nothing else, so the
;;;; default suite can test it.
;;;;
;;;; ROVE:RUN's answer cannot tell a test that ran from one that skipped: Rove
;;;; records a skip as a pending assertion, and a test holding only pending
;;;; assertions is a PASSED-TEST.  A suite of real-cl-spec checks that each
;;;; skip when cl-spec cannot be found therefore passes, in full, in an image
;;;; without cl-spec -- which is how the real-cl-spec integration tests went
;;;; unrun in CI while every job was green.  JUDGE-SUITE-RESULTS reads the
;;;; per-test results instead: every required test must have run, failed
;;;; nothing, skipped nothing and asserted something.
;;;;
;;;; scripts/check-specs.lisp runs it (CL_MCP_SPECS_MODE=integration) in a
;;;; process of its own; tests/suite-judge-test.lisp checks it on results
;;;; built from Rove's own classes.

(defpackage #:cl-mcp/specs/suite-judge
  (:use #:cl)
  (:import-from #:rove/core/result
                #:test
                #:test-name
                #:passed-tests
                #:failed-tests
                #:pending-tests
                #:failed
                #:pending)
  (:export #:judge-suite-results
           #:print-suite-verdict))

(in-package #:cl-mcp/specs/suite-judge)

(defun %name (name)
  "Return a test NAME as text: a symbol's name, or the string itself."
  (if (symbolp name) (symbol-name name) (princ-to-string name)))

(defun %children (node)
  "Return everything Rove recorded under NODE: assertions and nested tests."
  (append (passed-tests node) (failed-tests node) (pending-tests node)))

(defun %tally (node)
  "Return (values ASSERTIONS SKIPS FAILURES) over NODE and every test nested
in it, whether by TESTING or otherwise."
  (let ((assertions 0) (skips 0) (failures 0))
    (labels ((walk (node)
               (dolist (child (%children node))
                 (cond ((typep child 'test)
                        (when (typep child 'failed) (incf failures))
                        (walk child))
                       ((typep child 'pending) (incf skips))
                       ((typep child 'failed) (incf failures))
                       (t (incf assertions))))))
      (walk node))
    (values assertions skips failures)))

(defun %deftest-p (node)
  "Return true when NODE is the result of one DEFTEST.  Rove names that result
with the test's symbol; a system's, a suite's and a TESTING block's results
carry a string or no name at all."
  (and (typep node 'test)
       (let ((name (test-name node)))
         (and name (symbolp name)))))

(defun judge-suite-results (results required)
  "Return a verdict plist for the Rove RESULTS of one run, against the test
names REQUIRED (strings, compared without regard to case).

RESULTS is the list Rove keeps in ROVE:*LAST-SUITE-REPORT*.  The tests are the
DEFTEST results found in it at any depth -- under a system's result, a
suite's, or both, which is how Rove versions differ.  Each is classed as
:FAILED (any failed assertion or nested test), :SKIPPED (any pending
assertion, however deep -- Rove calls such a test passed), :EMPTY (no
assertion at all) or :PASSED.  A failure or skip recorded outside any test is
classed under the name of the result holding it.  :MISSING names every
required test that did not run.  :OK is true only when some test ran, none is
missing, and every test that ran passed."
  (let ((ran '()) (passed '()) (failed '()) (skipped '()) (empty '()))
    (labels ((classify (test)
               (let ((name (%name (test-name test))))
                 (push name ran)
                 (multiple-value-bind (assertions skips failures) (%tally test)
                   (cond ((or (typep test 'failed) (plusp failures)) (push name failed))
                         ((plusp skips) (push name skipped))
                         ((zerop assertions) (push name empty))
                         (t (push name passed))))))
             (visit (node holder)
               (dolist (child (%children node))
                 (cond ((%deftest-p child) (classify child))
                       ((typep child 'test)
                        (visit child (if (test-name child) (%name (test-name child)) holder)))
                       ((typep child 'failed) (pushnew holder failed :test #'string=))
                       ((typep child 'pending) (pushnew holder skipped :test #'string=))))))
      (dolist (result results)
        (cond ((%deftest-p result) (classify result))
              ((typep result 'test) (visit result (%name (test-name result)))))))
    (let ((missing (remove-if (lambda (name) (member name ran :test #'string-equal))
                              required)))
      (list :ok (and ran (null missing) (null failed) (null skipped) (null empty) t)
            :ran (reverse ran)
            :passed (reverse passed)
            :failed (reverse failed)
            :skipped (reverse skipped)
            :empty (reverse empty)
            :missing missing))))

(defun print-suite-verdict (system verdict &optional (stream *standard-output*))
  "Print VERDICT for the suite SYSTEM on STREAM, naming every test that did
not pass and every required one that did not run."
  (destructuring-bind (&key ok ran passed failed skipped empty missing) verdict
    (format stream "~&~A: ~D test~:P ran, ~D passed~%" system (length ran) (length passed))
    (loop for (label names) in (list (list "failed" failed) (list "skipped" skipped)
                                     (list "asserted nothing" empty)
                                     (list "required but did not run" missing))
          when names
            do (format stream "  ~A:~{ ~A~}~%" label names))
    (format stream "~A: ~:[FAILED~;PASSED~]~%" system ok)))
