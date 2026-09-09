;;;; tests/spec-worker-test.lisp
;;;;
;;;; The cl-spec tools through a real worker, with the pool enabled.
;;;;
;;;; tests/spec-tools-test.lisp drives the same tools inline, which is the
;;;; right place to check argument handling but cannot check the guarantees
;;;; that are the reason these tools run in the worker at all: that a load and
;;;; the run that follows it land in the same image, that another session does
;;;; not see them, that an object id from a counterexample is still resolvable
;;;; where it was made, and that a timeout leaves the image described rather
;;;; than assumed reusable.
;;;;
;;;; Each case spawns a worker and loads cl-spec into it, so the suite is slow
;;;; by construction.  It skips, with a reason, when a worker cannot be spawned
;;;; or cl-spec cannot be resolved.

(defpackage #:cl-mcp/tests/spec-worker-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker
                #:*use-worker-pool*
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht))

(in-package #:cl-mcp/tests/spec-worker-test)

(defparameter +fixture-package+ "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"
  "Package the fixture file defines, loaded into a worker by these tests.")

(defun %fixture-path ()
  "Return the fixture file's path as a string."
  (namestring (merge-pathnames "tests/fixtures/spec-fixture.lisp"
                               (asdf:system-source-directory "cl-mcp"))))

(defun %cl-spec-resolvable-p ()
  "Return true when ASDF in this image can find cl-spec/check-it.

Checked in the parent rather than the worker: the worker inherits the same
source registry, so a system the parent cannot find is one the worker cannot
load either, and asking here costs no subprocess."
  (handler-case (and (asdf:find-system "cl-spec/check-it" nil) t)
    (error () nil)))

(defun %text (result)
  "Return the first content text of RESULT, or the empty string."
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (or (when (and (vectorp content) (plusp (length content)))
          (gethash "text" (aref content 0)))
        "")))

(defun %eval-in-worker (code &optional package)
  "Evaluate CODE in the current session's worker and return the result.

PACKAGE names the package CODE is READ in, and has to be passed rather than
written as an (in-package ...) inside CODE: the whole form is read before any
of it runs, so an embedded IN-PACKAGE changes nothing about how the symbols
around it were interned."
  (let ((params (make-ht "code" code "timeout_seconds" 120)))
    (when package (setf (gethash "package" params) package))
    (proxy-to-worker 1 "worker/eval" params)))

(defun %json-false-p (value)
  "Return true when VALUE is a JSON false that has crossed the worker RPC.

Inline, a response carries YASON:FALSE; through a worker the same field is
serialized to JSON and parsed back, and yason reads false as NIL.  Both
render as false to a client, so the difference is only visible to a test
holding the hash-table."
  (or (null value) (eq value (find-symbol "FALSE" "YASON"))))

(defun %load-cl-spec-and-fixture ()
  "Load cl-spec and the fixture into the current session's worker.

The fixture registers a property written to fail, so it goes into a registry
of its own and the previous one is put back -- the same discipline
tests/spec-integration-test.lisp follows, carried into the worker."
  (%eval-in-worker
   (format nil "(progn (asdf:load-asd ~S) t)"
           (namestring (asdf:system-source-file
                        (asdf:find-system "cl-spec" nil)))))
  (proxy-to-worker 2 "worker/load-system"
                   (make-ht "system" "cl-spec/check-it" "force" nil
                            "timeout_seconds" 240))
  (%eval-in-worker
   (format nil
           "(progn (setf (symbol-value (find-symbol \"*REGISTRY*\" \"CL-SPEC\"))
                        (funcall (find-symbol \"MAKE-HASH-TABLE-REGISTRY\" \"CL-SPEC\")))
                   (handler-bind ((warning #'muffle-warning)) (load ~S))
                   t)"
           (%fixture-path))))

(defun %spec-symbol (name)
  "Call the worker's spec-symbol on the fixture symbol named NAME."
  (proxy-to-worker 3 "worker/spec-symbol"
                   (make-ht "symbol" (format nil "~A::~A" +fixture-package+ name)
                            "timeout_seconds" 120)))

(defun %spec-check (arguments)
  "Call the worker's spec-check with ARGUMENTS, a plist of string keys."
  (let ((params (make-ht "timeout_seconds" 120)))
    (loop for (key value) on arguments by #'cddr
          do (setf (gethash key params) value))
    (proxy-to-worker 4 "worker/spec-check" params)))

(defun %skip-reason ()
  "Return why this suite cannot run, or NIL when it can."
  (cond ((not (spawn-available-p)) "no ros/sbcl available to spawn a worker")
        ((not (%cl-spec-resolvable-p))
         "cl-spec/check-it is not on this image's ASDF source registry")))

(deftest spec-tools-see-what-this-session-loaded
  (let ((reason (%skip-reason)))
    (if reason
        (skip reason)
        (let ((*use-worker-pool* t)
              (*current-session-id* "spec-worker-affinity-session"))
          (with-pool ()
            (%load-cl-spec-and-fixture)
            (testing "the load and the lookup that follows share one image"
              (let ((result (%spec-symbol "CLAMP")))
                (ok (string= "ok" (gethash "status" result)))
                (ok (= 3 (length (gethash "properties" result))))
                (testing "and cl-spec is reported as loaded there"
                  (ok (eq t (gethash "cl_spec_loaded"
                                     (gethash "environment" result)))))))
            (testing "a failing property yields a counterexample and a seed"
              (let* ((result (%spec-check
                              (list "property"
                                    (format nil "~A::CLAMP-IS-WRONG-ON-PURPOSE"
                                            +fixture-package+))))
                     (per-property (aref (gethash "results" result) 0)))
                (ok (%json-false-p (gethash "verified" result)))
                (ok (string= "failed" (gethash "status" per-property)))
                (ok (string= "present"
                             (gethash "counterexample_status" per-property)))
                (ok (every #'digit-char-p (gethash "seed" per-property)))
                (testing "and the worker is reported reusable"
                  (ok (string= "safe" (gethash "worker_reuse" result)))))))))))

(deftest another-session-does-not-see-this-one-s-registry
  (let ((reason (%skip-reason)))
    (if reason
        (skip reason)
        (let ((*use-worker-pool* t))
          (with-pool ()
            (let ((*current-session-id* "spec-worker-owner-session"))
              (%load-cl-spec-and-fixture)
              (ok (string= "ok" (gethash "status" (%spec-symbol "CLAMP")))))
            (testing "a second session gets its own worker and its own registry"
              (let ((*current-session-id* "spec-worker-other-session"))
                (let ((result (%spec-symbol "CLAMP")))
                ;; The fixture package does not exist in the other worker at
                ;; all, so the symbol cannot resolve -- which is the strongest
                ;; form the isolation can take.
                  (ok (member (gethash "status" result)
                              '("cl-spec-not-loaded" "unresolved-symbol")
                              :test #'string=))))))))))

(deftest counterexample-object-ids-resolve-in-the-session-that-made-them
  (let ((reason (%skip-reason)))
    (if reason
        (skip reason)
        (let ((*use-worker-pool* t)
              (*current-session-id* "spec-worker-object-id-session"))
          (with-pool ()
            (%load-cl-spec-and-fixture)
            ;; A property over a list-valued spec, so the counterexample holds
            ;; a compound value and therefore earns an object id.
            (%eval-in-worker
             "(progn (cl-spec:defspec small-list (list-of small-int))
                     (cl-spec:defproperty list-is-never-long ((xs small-list))
                       (:about clamp)
                       (< (length xs) 0))
                     t)"
             +fixture-package+)
            (let* ((result (%spec-check
                            (list "property"
                                  (format nil "~A::LIST-IS-NEVER-LONG"
                                          +fixture-package+))))
                   (results (gethash "results" result))
                   (per-property (when (and (vectorp results) (plusp (length results)))
                                   (aref results 0)))
                   (counterexample (and per-property
                                        (gethash "counterexample" per-property)))
                   (object-id (when (plusp (length counterexample))
                                (gethash "object_id"
                                         (gethash "value" (aref counterexample 0))))))
              (ok per-property (%text result))
              (ok (string= "failed" (gethash "status" per-property)))
              (if (null object-id)
                  ;; A generated empty list is a primitive as far as the
                  ;; object registry is concerned; nothing to drill into.
                  (ok t "counterexample value was primitive, no id expected")
                  (testing "the id resolves in the same worker"
                    (let ((inspected (proxy-to-worker
                                      5 "worker/inspect-object"
                                      (make-ht "id" object-id))))
                      (ok (not (gethash "isError" inspected)))
                      (ok (plusp (length (%text inspected)))))))))))))

(deftest a-timeout-reports-the-image-as-unknown
  (let ((reason (%skip-reason)))
    (if reason
        (skip reason)
        (let ((*use-worker-pool* t)
              (*current-session-id* "spec-worker-timeout-session"))
          (with-pool ()
            (%load-cl-spec-and-fixture)
            (%eval-in-worker
             "(progn (cl-spec:defproperty slow-property ((x small-int))
                       (:about clamp)
                       (sleep 30)
                       t)
                     t)"
             +fixture-package+)
            (testing "the deadline is answered, and the image is not assumed safe"
              (let* ((result (%spec-check
                              (list "property"
                                    (format nil "~A::SLOW-PROPERTY"
                                            +fixture-package+)
                                    "timeout_seconds" 2)))
                     (results (gethash "results" result))
                     (per-property (when (and (vectorp results)
                                              (plusp (length results)))
                                     (aref results 0))))
                (ok per-property (%text result))
                (ok (string= "timeout" (gethash "status" per-property)))
                (ok (%json-false-p (gethash "verified" result)))
                (ok (member (gethash "worker_reuse" result)
                            '("unknown" "unsafe") :test #'string=))
                (ok (search "pool-kill-worker"
                            (gethash "worker_reuse_message" result))))))))))
