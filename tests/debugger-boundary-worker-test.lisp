(defpackage #:cl-mcp/tests/debugger-boundary-worker-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok #:skip)
  (:import-from #:cl-mcp/src/project-root #:*project-root*)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool* #:proxy-to-worker)
  (:import-from #:cl-mcp/src/state #:*current-session-id*)
  (:import-from #:cl-mcp/src/pool #:get-or-assign-worker)
  (:import-from #:cl-mcp/src/worker-client
                #:worker-pid #:worker-state #:worker-process-info
                #:worker-last-crash-reason #:worker-last-exit-status #:worker-last-exit-code
                #:*reaper-threads* #:*reaper-threads-lock*)
  (:import-from #:cl-mcp/src/test-runner-core #:*test-debug-output*)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:cl-mcp/tests/test-helpers #:with-pool #:spawn-available-p)
  (:import-from #:bordeaux-threads #:with-lock-held #:thread-alive-p))

(in-package #:cl-mcp/tests/debugger-boundary-worker-test)

(declaim (optimize (safety 3) (debug 3)))

(defvar *worker-source-root* (asdf:system-source-directory "cl-mcp")
  "Source checkout fresh acceptance workers must load, also bindable for the RED control.")

(defvar *rpc-id* 0)

(defun %worker-eval (code &key (timeout 5) options)
  (let ((params (make-ht "code" code "package" "CL-USER" "timeout_seconds" timeout)))
    (loop for (key value) on options by #'cddr
          do (setf (gethash key params) value))
    (proxy-to-worker (incf *rpc-id*) "worker/eval" params)))

(defun %worker-text (result)
  (let ((content (gethash "content" result)))
    (if (and (vectorp content) (plusp (length content)))
        (or (gethash "text" (aref content 0)) "")
        "")))

(defun %context-type (result)
  (let ((context (gethash "error_context" result)))
    (if (hash-table-p context) (or (gethash "condition_type" context) "") "")))

(defun %note (control &rest arguments)
  (apply #'format *test-debug-output* (concatenate 'string control "~%") arguments))

(defun %assert-worker-source (worker)
  ;; Inspect the loaded function, not just a registry that can be repointed later.
  (%worker-eval "(require :sb-introspect)")
  (let* ((root (namestring (truename *worker-source-root*)))
         (result
           (%worker-eval
            "(list (namestring (truename (asdf:system-source-directory \"cl-mcp\")))
                   (namestring
                    (sb-introspect:definition-source-pathname
                     (sb-introspect:find-definition-source
                      #'cl-mcp/src/worker/handlers::%handle-eval))))"))
         (text (%worker-text result))
         (paths (let ((*read-eval* nil)) (read-from-string text nil nil))))
    (%note "source pid=~D state=~S ~A" (worker-pid worker) (worker-state worker) text)
    (ok (not (gethash "isError" result)))
    (ok (not (gethash "error_context" result)))
    (ok (and (listp paths) (= 2 (length paths))) "worker returned two independent paths")
    (ok (and (listp paths) (equal root (first paths)))
        "fresh worker ASDF root is the requested checkout")
    (ok (and (listp paths)
             (equal (namestring (merge-pathnames "src/worker/handlers.lisp" root))
                    (second paths)))
        "loaded worker handler was compiled from the requested checkout")))

(defmacro with-boundary-worker ((worker session-id) &body body)
  `(if (spawn-available-p)
       (let ((old-registry (uiop:getenv "CL_SOURCE_REGISTRY"))
             (*project-root* *worker-source-root*)
             (*use-worker-pool* t)
             (*current-session-id* ,session-id)
             (*rpc-id* 0))
         ;; Roswell's -s lookup does not inherit the controller's ASDF registry.
         ;; Keep the child-visible override scoped to this serial test pool.
         (unwind-protect
              (progn
                (sb-posix:setenv
                 "CL_SOURCE_REGISTRY"
                 (format nil "(:source-registry (:directory ~S) :inherit-configuration)"
                         (namestring *worker-source-root*))
                 1)
                (with-pool ()
                  (let ((,worker (get-or-assign-worker *current-session-id*)))
                    (%assert-worker-source ,worker)
                    ,@body)))
           (if old-registry
               (sb-posix:setenv "CL_SOURCE_REGISTRY" old-registry 1)
               (sb-posix:unsetenv "CL_SOURCE_REGISTRY"))))
       (skip "worker spawning unavailable")))

(defun %assert-success (code expected)
  (let ((result (%worker-eval code)))
    (ok (not (gethash "isError" result)))
    (ok (not (gethash "error_context" result)))
    (ok (string-equal expected
                      (subseq (%worker-text result) 0
                              (position #\Newline (%worker-text result)))))
    result))

(defun %assert-survival (worker pid)
  (ok (= pid (worker-pid worker)) "worker PID did not change")
  (ok (eq :bound (worker-state worker)) "worker remains bound")
  (ok (eq worker (get-or-assign-worker *current-session-id*))
      "the session still owns the original worker")
  (let ((result (%assert-success "*boundary-state*" "73")))
    (%note "survival check pid=~D state=~S preserved-state=~A"
           (worker-pid worker) (worker-state worker) (%worker-text result))))

(defun %assert-local-failure (worker code condition-type)
  (let* ((pid (worker-pid worker))
         (start (get-internal-real-time))
         (result (%worker-eval code :timeout 5))
         (elapsed (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))
    (%note "failure pid=~D state=~S elapsed=~,3F type=~A text=~A"
           pid (worker-state worker) elapsed (%context-type result)
           (subseq (%worker-text result) 0 (position #\Newline (%worker-text result))))
    (ok (< elapsed 5) "debugger failure returns before the five-second deadline")
    (ok (hash-table-p (gethash "error_context" result)))
    (ok (search condition-type (%context-type result)))
    (ok (not (gethash "isError" result)) "not a proxy crash notification")
    (ok (not (search "crashed" (%worker-text result))))
    (%assert-survival worker pid)
    (when (gethash "isError" result)
      (%note "unexpected RPC failure pid=~D reason=~S process=~S/~S live-reapers=~D"
             pid (worker-last-crash-reason worker)
             (sb-ext:process-status (worker-process-info worker))
             (sb-ext:process-exit-code (worker-process-info worker))
             (with-lock-held (*reaper-threads-lock*)
               (count-if #'thread-alive-p *reaper-threads*))))
    result))

(deftest pooled-worker-survives-direct-condition-errors
  (with-boundary-worker (worker "debugger-boundary-eval")
    (%worker-eval "(defparameter *boundary-state* 73)")
    (%assert-local-failure
     worker
     "(progn
        (define-condition worker-direct-condition (condition) ())
        (error 'worker-direct-condition))"
     "WORKER-DIRECT-CONDITION")))

(deftest pooled-worker-survives-direct-simple-condition-errors
  (with-boundary-worker (worker "debugger-boundary-simple")
    (%worker-eval "(defparameter *boundary-state* 73)")
    (let ((result
            (%assert-local-failure
             worker
             "(progn
                (define-condition worker-direct-simple-condition (simple-condition) ())
                (error 'worker-direct-simple-condition
                       :format-control \"simple boundary message\"))"
             "WORKER-DIRECT-SIMPLE-CONDITION")))
      (ok (equal "simple boundary message"
                 (gethash "message" (gethash "error_context" result)))))))

(deftest pooled-worker-survives-explicit-invoke-debugger
  (with-boundary-worker (worker "debugger-boundary-invoke")
    (%worker-eval "(defparameter *boundary-state* 73)")
    (%assert-local-failure
     worker
     "(progn
        (define-condition worker-invoked-condition (condition) ())
        (invoke-debugger (make-condition 'worker-invoked-condition)))"
     "WORKER-INVOKED-CONDITION")))

(deftest pooled-debugger-escape-keeps-captured-output
  (with-boundary-worker (worker "debugger-boundary-output")
    (%worker-eval "(defparameter *boundary-state* 73)
                  (define-condition worker-output-condition (condition) ())")
    (let ((pid (worker-pid worker))
          (result
            (%worker-eval
             "(progn
                (write-string \"stdout before debugger\")
                (write-string \"stderr before debugger\" *error-output*)
                (invoke-debugger (make-condition 'worker-output-condition)))")))
      (ok (equal "stdout before debugger" (gethash "stdout" result)))
      ;; SBCL may append its compilation-unit abort note during the unwind.
      (ok (eql 0 (search "stderr before debugger" (gethash "stderr" result))))
      (ok (search "WORKER-OUTPUT-CONDITION" (%context-type result)))
      (ok (not (gethash "isError" result)))
      (%assert-survival worker pid))))

(deftest pooled-debugger-capture-honors-request-settings
  (with-boundary-worker (worker "debugger-boundary-settings")
    (%worker-eval
     "(defparameter *boundary-state* 73)
      (defparameter *boundary-config-locals* nil)
      (define-condition worker-config-condition (condition) ()
        (:report (lambda (condition stream)
                   (declare (ignore condition))
                   (write-string \"configured debugger capture\" stream))))
      (defun worker-config-frame (structured printed)
        (declare (optimize (debug 3) (speed 0)))
        (unwind-protect
             (invoke-debugger (make-condition 'worker-config-condition))
          (setf *boundary-config-locals* (list structured printed))))")
    (let* ((pid (worker-pid worker))
           (result
             (%worker-eval
              "(worker-config-frame #(#(10 20 30) #(40 50 60) #(70 80 90))
                                    '((1 2 3) (4 5 6) (7 8 9)))"
              :options '("print_level" 1 "print_length" 2
                         "locals_preview_frames" 1 "locals_preview_max_depth" 2
                         "locals_preview_max_elements" 2)))
           (context (gethash "error_context" result))
           (frame (and context
                       (find "WORKER-CONFIG-FRAME" (gethash "frames" context)
                             :key (lambda (frame) (gethash "function" frame)) :test #'search)))
           (locals (and frame (gethash "locals" frame)))
           (structured (find "STRUCTURED" locals
                             :key (lambda (local) (gethash "name" local)) :test #'equal))
           (printed (find "PRINTED" locals
                          :key (lambda (local) (gethash "name" local)) :test #'equal))
           (preview (and structured (gethash "preview" structured))))
      (ok (search "WORKER-CONFIG-CONDITION" (%context-type result)))
      (ok (equal "configured debugger capture" (gethash "message" context)))
      (ok frame "the real debug-3 user frame is captured")
      (ok (hash-table-p preview) "the requested user local preview is present")
      (when (hash-table-p preview)
        (let* ((elements (gethash "elements" preview))
               (child (elt elements 0)))
          (ok (= 2 (length elements)) "the requested outer element limit is honored")
          (ok (equal "array" (gethash "kind" child)) "depth two expands the nested array")
          (ok (= 2 (length (gethash "elements" child))))
          (ok (eql 2 (gethash "max_elements" (gethash "meta" child))))))
      (ok (and printed (equal "(# # ...)" (gethash "value" printed)))
          "requested print-level one and print-length two bound the local value")
      (ok (not (gethash "isError" result)))
      (%assert-survival worker pid))))

(deftest pooled-worker-preserves-user-diagnostics
  (with-boundary-worker (worker "debugger-boundary-diagnostics")
    (%worker-eval
     "(defparameter *boundary-state* 73)
      (define-condition worker-snapshot-condition (condition) ()
        (:report (lambda (condition stream)
                   (declare (ignore condition))
                   (write-string \"original worker diagnostic\" stream))))
      (defun worker-boundary-user-frame ()
        (declare (optimize (debug 3) (speed 0)))
        (restart-case (invoke-debugger (make-condition 'worker-snapshot-condition))
          (worker-boundary-recovery () :recovered)))")
    (let* ((result (%assert-local-failure worker "(worker-boundary-user-frame)"
                                          "WORKER-SNAPSHOT-CONDITION"))
           (context (gethash "error_context" result)))
      (when (hash-table-p context)
        (%note "diagnostics message=~S restarts=~S frames=~S"
               (gethash "message" context)
               (map 'list (lambda (restart) (gethash "name" restart))
                    (gethash "restarts" context))
               (map 'list (lambda (frame) (gethash "function" frame))
                    (gethash "frames" context)))
        (ok (equal "original worker diagnostic" (gethash "message" context)))
        (ok (find "WORKER-BOUNDARY-RECOVERY" (gethash "restarts" context)
                  :key (lambda (restart) (gethash "name" restart)) :test #'search))
        #+sbcl
        (ok (find "WORKER-BOUNDARY-USER-FRAME" (gethash "frames" context)
                  :key (lambda (frame) (gethash "function" frame)) :test #'search)
            "the debug-3 user frame survives the JSON response")))))

(deftest pooled-worker-survives-unavailable-frame-metadata
  (with-boundary-worker (worker "debugger-boundary-unavailable-metadata")
    (%worker-eval
     "(defparameter *boundary-state* 73)
      (define-condition worker-metadata-condition (condition) ()
        (:report (lambda (condition stream)
                   (declare (ignore condition))
                   (write-string \"metadata diagnostic\" stream))))
      (defun worker-metadata-frame ()
        (declare (optimize (debug 3) (speed 0)))
        (restart-case (error 'worker-metadata-condition)
          (worker-metadata-recovery () :recovered)))")
    ;; Some SBCL versions have an unknown code location in the compiler's
    ;; WITH-SIMPLE-CONDITION-RESTARTS frame for ERROR within RESTART-CASE.
    ;; The every-secondary-condition policy then returns the safe minimal record.
    ;; Explicit INVOKE-DEBUGGER above separately requires the full diagnostics.
    (let* ((result (%assert-local-failure worker "(worker-metadata-frame)"
                                          "WORKER-METADATA-CONDITION"))
           (context (gethash "error_context" result)))
      (when (hash-table-p context)
        (let ((message (gethash "message" context)))
          (ok (member message '("metadata diagnostic"
                                "Debugger entered; diagnostic capture unavailable.")
                      :test #'equal))
          (when (equal message "Debugger entered; diagnostic capture unavailable.")
            (ok (zerop (length (gethash "frames" context))))
            (ok (zerop (length (gethash "restarts" context))))))))))

(deftest pooled-worker-survives-report-secondary-error
  (with-boundary-worker (worker "debugger-boundary-report-error")
    (%worker-eval "(defparameter *boundary-state* 73)")
    (%assert-local-failure
     worker
     "(progn
        (define-condition worker-report-error-condition (condition) ()
          (:report (lambda (condition stream)
                     (declare (ignore condition stream))
                     (error \"secondary report error\"))))
        (error 'worker-report-error-condition))"
     "WORKER-REPORT-ERROR-CONDITION")))

(deftest pooled-worker-survives-report-debugger-reentry
  (with-boundary-worker (worker "debugger-boundary-report-reentry")
    (%worker-eval "(defparameter *boundary-state* 73)")
    (%assert-local-failure
     worker
     "(progn
        (define-condition worker-report-reentry-condition (condition) ()
          (:report (lambda (condition stream)
                     (declare (ignore condition stream))
                     (invoke-debugger
                      (make-condition 'simple-condition
                                      :format-control \"secondary debugger entry\")))))
        (error 'worker-report-reentry-condition))"
     "WORKER-REPORT-REENTRY-CONDITION")))

(deftest pooled-worker-preserves-condition-semantics
  (with-boundary-worker (worker "debugger-boundary-semantics")
    (%worker-eval
     "(defparameter *boundary-state* 73)
      (define-condition worker-direct-condition (condition) ())")
    (%assert-success "(signal 'worker-direct-condition)" "nil")
    (let ((warning (%assert-success "(progn (warn \"ordinary warning\") :ok)" ":ok")))
      (ok (search "ordinary warning" (gethash "stderr" warning ""))))
    (let ((muffled
            (%assert-success
             "(handler-bind ((warning #'muffle-warning)) (warn \"muffled\") :ok)" ":ok")))
      (ok (not (search "muffled" (gethash "stderr" muffled "")))))
    (%assert-success "(handler-case (error \"handled\") (error () :handled))" ":handled")
    (%assert-success
     "(handler-bind ((error (lambda (condition)
                              (declare (ignore condition))
                              (invoke-restart :recover))))
        (restart-case (error \"recover\") (:recover () :ok)))"
     ":ok")
    (let ((result
            (%assert-local-failure
             worker
             "(handler-case (error 'worker-direct-condition)
                (error () :incorrect-success))"
             "WORKER-DIRECT-CONDITION")))
      (ok (not (string-equal ":incorrect-success" (%worker-text result)))))))

(defun %worker-run-fixture (test)
  (proxy-to-worker
   (incf *rpc-id*) "worker/run-tests"
   (make-ht "system" "cl-mcp/tests/debugger-boundary-run-tests-fixture"
            "test" (concatenate 'string
                                 "cl-mcp/tests/debugger-boundary-run-tests-fixture::" test)
            "framework" "rove" "timeout_seconds" 5)))

(deftest pooled-run-tests-distinguishes-assertion-failure-from-debugger-escape
  (with-boundary-worker (worker "debugger-boundary-run-tests")
    (%worker-eval "(defparameter *boundary-state* 73)")
    (let ((pid (worker-pid worker))
          (ordinary (%worker-run-fixture "ordinary-rove-failure")))
      (%note "ordinary run-tests pid=~D passed=~S failed=~S text=~A"
             pid (gethash "passed" ordinary) (gethash "failed" ordinary)
             (%worker-text ordinary))
      (ok (eql 0 (gethash "passed" ordinary)))
      (ok (eql 1 (gethash "failed" ordinary)))
      (ok (not (gethash "isError" ordinary)))
      (let* ((failures (gethash "failed_tests" ordinary))
             (failure (and (vectorp failures) (= 1 (length failures)) (aref failures 0))))
        (ok (hash-table-p failure) "the selected assertion has one structured failure")
        (when (hash-table-p failure)
          (ok (equal "ordinary Rove assertion failure" (gethash "description" failure)))
          (ok (not (gethash "reason" failure)) "no framework-crash fallback reason")))
      (let* ((source (%worker-eval
                      "(mapcar (lambda (component)
                                 (namestring (asdf:component-pathname component)))
                               (asdf:component-children
                                (asdf:find-system
                                 \"cl-mcp/tests/debugger-boundary-run-tests-fixture\")))"))
             (text (%worker-text source)))
        (%note "run-tests fixture source pid=~D ~A" pid text)
        (ok (search (namestring *worker-source-root*) text))
        (ok (search "debugger-boundary-run-tests-fixture" text)))
      (let* ((start (get-internal-real-time))
             (escaped (%worker-run-fixture "direct-condition-reaches-worker-boundary"))
             (elapsed (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)))
        (%note "escaped run-tests pid=~D state=~S elapsed=~,3F text=~A"
               pid (worker-state worker) elapsed (%worker-text escaped))
        (ok (< elapsed 5))
        (ok (gethash "isError" escaped) "existing worker RPC error form")
        (ok (not (gethash "passed" escaped)) "not a zero-test success result")
        (ok (search "RUN-TESTS-BOUNDARY-CONDITION" (%worker-text escaped)))
        (ok (not (search "crashed" (%worker-text escaped))))
        (%assert-survival worker pid)))))

(deftest pooled-real-process-exit-is-reaped-and-replaced
  (with-boundary-worker (worker "debugger-boundary-real-exit")
    (let ((pid (worker-pid worker))
          (process (worker-process-info worker))
          (result (%worker-eval "(sb-ext:exit :code 71)"))
          (deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
      (loop until (or (eq :exited (sb-ext:process-status process))
                      (>= (get-internal-real-time) deadline))
            do (sleep 0.01))
      (%note "real exit pid=~D state=~S reason=~S observed=~S/~S process=~S/~S text=~A"
             pid (worker-state worker) (worker-last-crash-reason worker)
             (worker-last-exit-status worker) (worker-last-exit-code worker)
             (sb-ext:process-status process) (sb-ext:process-exit-code process)
             (%worker-text result))
      (ok (gethash "isError" result))
      (ok (search "crashed" (%worker-text result)))
      (ok (not (gethash "error_context" result)))
      (ok (equal "eof" (worker-last-crash-reason worker)))
      (ok (member (worker-state worker) '(:crashed :dead)))
      ;; The public fields are the immutable EOF-time snapshot. The original
      ;; process object exposes the terminal status after the async reaper runs.
      (ok (eq :exited (sb-ext:process-status process)))
      (ok (eql 71 (sb-ext:process-exit-code process)))
      (loop while (and (< (get-internal-real-time) deadline)
                       (with-lock-held (*reaper-threads-lock*)
                         (some #'thread-alive-p *reaper-threads*)))
            do (sleep 0.01))
      (ok (with-lock-held (*reaper-threads-lock*)
            (notany #'thread-alive-p *reaper-threads*))
          "the exited child's asynchronous reaper has finished")
      ;; A crash notification can consume the next request; retry only that form.
      (let ((recovered nil))
        (loop repeat 3
              for retry = (%worker-eval "(+ 20 22)")
              do (%note "exit retry text=~A" (%worker-text retry))
              if (not (gethash "isError" retry))
                do (setf recovered retry) and return nil
              else do (ok (search "crashed" (%worker-text retry))))
        (ok recovered "documented retry reaches a replacement")
        (when recovered
          (ok (string= "42" (%worker-text recovered)))
          (ok (not (gethash "error_context" recovered)))
          (let ((replacement (get-or-assign-worker *current-session-id*)))
            (ok (/= pid (worker-pid replacement)))
            (ok (eq :bound (worker-state replacement)))
            (%assert-worker-source replacement)
            (%note "replacement pid=~D state=~S result=~A"
                   (worker-pid replacement) (worker-state replacement)
                   (%worker-text recovered))))))))
