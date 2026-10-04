;;;; tests/system-loader-test.lisp

(defpackage #:cl-mcp/tests/system-loader-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/system-loader
                #:load-system)
  (:import-from #:cl-mcp/src/system-loader-core
                #:%load-with-timeout)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-load-system-response)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:*request-debugger-boundary-active*
                #:request-debugger-escape-error-p
                #:request-debugger-escape-error-display-text))

(in-package #:cl-mcp/tests/system-loader-test)

(define-condition loader-boundary-condition (condition)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "loader debugger snapshot" stream))))

(deftest load-debugger-escape-keeps-error-result
  (let ((*request-debugger-boundary-active* t))
    (multiple-value-bind (result timed-out-p errored-p leaked-p)
        (%load-with-timeout
         (lambda () (invoke-debugger (make-condition 'loader-boundary-condition)))
         2)
      (ok errored-p)
      (ok (not timed-out-p))
      (ok (not leaked-p))
      (ok (request-debugger-escape-error-p (first result)))
      (let ((text (request-debugger-escape-error-display-text (first result))))
        (ok (search "LOADER-BOUNDARY-CONDITION" text))
        (ok (search "loader debugger snapshot" text))))))

(deftest load-debugger-escape-uses-existing-error-response
  (let ((*request-debugger-boundary-active* t)
        (cl-mcp/src/system-loader-core:*system-load-lock-wrapper*
          (lambda (thunk)
            (declare (ignore thunk))
            (invoke-debugger (make-condition 'loader-boundary-condition)))))
    (let ((response (load-system "cl-mcp" :force nil :timeout-seconds 2)))
      (ok (equal "error" (gethash "status" response)))
      (ok (search "LOADER-BOUNDARY-CONDITION" (gethash "message" response)))
      (ok (search "loader debugger snapshot" (gethash "message" response)))
      (ok (null (gethash "load_not_started" response))))))

(deftest load-system-basic
  (testing "loads an already-available system and returns structured result"
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (string= (gethash "system" ht) "cl-mcp"))
      (ok (integerp (gethash "duration_ms" ht)))
      (ok (integerp (gethash "warnings" ht))))))

(deftest load-system-force-reload
  (testing "force=true clears and reloads the system"
    (let ((ht (load-system "cl-mcp" :force t)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (eq t (gethash "forced" ht))))))

(deftest load-system-nonexistent
  (testing "returns error status for nonexistent system"
    (let ((ht (load-system "nonexistent-system-that-does-not-exist-12345"
                           :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "error"))
      (ok (stringp (gethash "message" ht))))))

(deftest load-system-timeout
  (testing "returns timeout status when worker is still running at deadline"
    ;; Use %load-with-timeout directly with a thunk that sleeps well beyond
    ;; the polling window to guarantee the worker is still alive at deadline.
    (multiple-value-bind (result-list timed-out-p errored-p)
        (%load-with-timeout
         (lambda () (sleep 10) :never-reached)
         0.05)
      (ok timed-out-p "should report timeout")
      (ok (not errored-p))
      (ok (null result-list)))))

(deftest load-system-clear-fasls-flag
  (testing "clear_fasls flag is reflected in response"
    (let ((ht (load-system "cl-mcp" :force t :clear-fasls nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (null (gethash "clear_fasls" ht))))))

(deftest load-system-warning-fields
  (testing "warning fields are properly typed in response"
    ;; Verify the warning-related fields exist and have correct types.
    ;; An already-loaded system should produce zero warnings.
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (integerp (gethash "warnings" ht)))
      (ok (zerop (gethash "warnings" ht)))
      ;; No warning_details key when warnings are zero
      (ok (null (gethash "warning_details" ht))))))

(deftest load-system-response-includes-warning-text
  (testing "build-load-system-response inlines warning_details in content text"
    ;; Build a fake load-system core result with warnings captured.
    ;; build-load-system-response should render them into content[].text
    ;; (the only field MCP clients display), not just the structured
    ;; warning_details field.
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "status" ht) "loaded"
            (gethash "system" ht) "fake-system"
            (gethash "duration_ms" ht) 42
            (gethash "warnings" ht) 1
            (gethash "warning_details" ht)
            "redefining FOO in DEFUN")
      (let* ((built (cl-mcp/src/tools/response-builders:build-load-system-response
                     "fake-system" ht))
             (content (gethash "content" built))
             (text (and (vectorp content)
                        (plusp (length content))
                        (gethash "text" (aref content 0)))))
        (ok (stringp text))
        (ok (search "System fake-system loaded successfully" text))
        (ok (search "(1 warning)" text))
        ;; The actual warning message must now be visible in content text.
        (ok (search "redefining FOO" text))))))

(deftest load-system-response-truncates-long-warnings
  (testing "warning text longer than the cap is truncated with a marker"
    (let ((ht (make-hash-table :test #'equal))
          (long-text (make-string 3000 :initial-element #\W)))
      (setf (gethash "status" ht) "loaded"
            (gethash "system" ht) "fake-system"
            (gethash "duration_ms" ht) 1
            (gethash "warnings" ht) 1
            (gethash "warning_details" ht) long-text)
      (let* ((built (cl-mcp/src/tools/response-builders:build-load-system-response
                     "fake-system" ht))
             (content (gethash "content" built))
             (text (gethash "text" (aref content 0))))
        (ok (stringp text))
        (ok (search "more characters truncated" text))))))

(deftest load-system-force-false-no-clear
  (testing "force=false skips clearing and uses quickload"
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (null (gethash "forced" ht))))))

(deftest timeout-returns-completed-work
  (testing "worker completing within polling granularity returns success, not timeout"
    ;; timeout=0.001s → ceiling(0.001/0.05)=1 → effective deadline ≈ 50ms.
    ;; Worker sleeps 10ms, exceeding the nominal 1ms timeout but finishing
    ;; well before the 50ms polling deadline (~40ms slack).  This avoids
    ;; flaky failures on slow CI runners (macOS) where the previous 30ms
    ;; slack was insufficient.
    (multiple-value-bind (result-list timed-out-p errored-p)
        (%load-with-timeout
         (lambda () (sleep 0.01) :done)
         0.001)
      (ok (not timed-out-p) "completed work should not be reported as timeout")
      (ok (not errored-p))
      (ok (equal result-list '(:done))))))

(deftest load-system-force-preserves-local-asd-registration
  (testing "force=true re-registers locally-loaded systems after clear-system"
    ;; Simulate a system registered only via asdf:load-asd (not on any
    ;; standard search path).  Without the fix, clear-system drops it from
    ;; the in-memory registry and the subsequent load-system errors with
    ;; "System definition for ... not found".
    (let* ((tmp-dir (uiop:ensure-directory-pathname
                     (uiop:merge-pathnames*
                      (format nil "cl-mcp-test-local-~A/" (get-universal-time))
                      (uiop:temporary-directory))))
           (asd-path (uiop:merge-pathnames* "cl-mcp-test-local.asd" tmp-dir))
           (system-name "cl-mcp-test-local"))
      (unwind-protect
           (progn
             ;; Create a minimal .asd file in a fresh temp directory.
             (ensure-directories-exist tmp-dir)
             (with-open-file (s asd-path :direction :output
                                         :if-exists :supersede)
               (format s "(asdf:defsystem ~S :description \"test\" :components ())~%"
                       system-name))
             ;; Register it locally (not on ASDF search path).
             (asdf:load-asd asd-path)
             (ok (asdf:find-system system-name nil)
                 "system should be registered after load-asd")
             ;; Reload with force=t — this is the scenario that used to fail.
             (let ((ht (load-system system-name :force t)))
               (ok (hash-table-p ht))
               (ok (string= "loaded" (gethash "status" ht))
                   "force=true should not lose local .asd registration")))
        ;; Cleanup: remove from registry and delete temp files.
        (ignore-errors (asdf:clear-system system-name))
        (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t))))))

(deftest suppress-redefinition-warning-predicate
  (testing "%redefinition-warning-p recognizes the printed SBCL shape"
    (ok (cl-mcp/src/system-loader-core::%redefinition-warning-p
         (make-condition 'simple-warning
                         :format-control "redefining FOO in DEFUN")))
    (ok (not (cl-mcp/src/system-loader-core::%redefinition-warning-p
              (make-condition 'simple-warning
                              :format-control "variable X unused")))))
  (testing "textual fallback requires ' in ' to avoid false positives"
    (ok (not (cl-mcp/src/system-loader-core::%redefinition-warning-p
              (make-condition 'simple-warning
                              :format-control "redefining makes no sense"))))
    (ok (not (cl-mcp/src/system-loader-core::%redefinition-warning-p
              (make-condition 'simple-warning
                              :format-control "not a redefining at all")))))
  #+sbcl
  (testing "primary typep path recognizes a real SBCL redefinition warning"
    (let ((cls (find-class 'sb-kernel:redefinition-warning nil)))
      (ok cls "sb-kernel:redefinition-warning class is present on SBCL")
      (let ((captured nil))
        (handler-bind ((warning
                         (lambda (w)
                           (push w captured)
                           (muffle-warning w))))
          (eval '(defun %rfp-typep-probe () 1))
          (eval '(defun %rfp-typep-probe () 2)))
        (let ((hit (find-if (lambda (w) (typep w cls)) captured)))
          (ok hit "handler-bind captured a sb-kernel:redefinition-warning")
          (when hit
            (ok (cl-mcp/src/system-loader-core::%redefinition-warning-p hit)
                "primary typep branch of %redefinition-warning-p matches")))))))

(deftest suppress-redefinition-warning-filter-behavior
  (testing "%call-with-suppressed-output drops redefining-warnings and keeps the rest"
    (multiple-value-bind (result warning-count details)
        (cl-mcp/src/system-loader-core::%call-with-suppressed-output
         (lambda ()
           (warn "redefining FOO in DEFUN")
           (warn "redefining BAR in DEFMACRO")
           (warn "something real and bad")
           :done))
      (ok (eq result :done))
      (ok (= warning-count 1))
      (ok (null (search "redefining FOO" details)))
      (ok (search "something real" details)))))

(deftest redefinition-notices-are-dropped-and-other-warnings-kept
  ;; Redefining is ordinary Common Lisp development, and a reload exists to
  ;; do it: every redefinition notice is dropped -- a first load's, a
  ;; reload's, another file's -- and no other warning is touched.  Each case
  ;; is first run bare, to show SBCL does signal the notices it drops.
  (let ((root (uiop:ensure-directory-pathname
               (uiop:merge-pathnames*
                (format nil "clmcp-redef-~36R/"
                        (random most-positive-fixnum (make-random-state t)))
                (uiop:temporary-directory))))
        (package-name "CLMCP-REDEF-PROBE"))
    (labels ((write-file* (name text)
               (let ((path (uiop:merge-pathnames* name root)))
                 (ensure-directories-exist path)
                 (with-open-file (out path :direction :output :if-exists :supersede)
                   (write-string text out))
                 path))
             (source (text)
               (format nil "(in-package #:clmcp-redef-probe)~%~A~%" text))
             (fresh-package ()
               (ignore-errors (delete-package package-name))
               (make-package package-name :use '(#:cl)))
             (bare (thunk)
               ;; The redefinition notices THUNK signals, counted and muffled.
               (let ((count 0)
                     (*error-output* (make-broadcast-stream)))
                 (handler-bind ((warning (lambda (w)
                                           (when (typep w 'sb-kernel:redefinition-warning)
                                             (incf count))
                                           (muffle-warning w))))
                   (funcall thunk))
                 count))
             (suppressed (thunk)
               (multiple-value-bind (result count details)
                   (cl-mcp/src/system-loader-core::%call-with-suppressed-output
                    (lambda ()
                      (let ((*error-output* (make-broadcast-stream)))
                        (funcall thunk))))
                 (declare (ignore result))
                 (values count details))))
      (unwind-protect
           (let* ((a (write-file* "a.lisp" (source "(defun probe () 1)
(defmacro probe-macro () 1)
(defgeneric probe-gf (x))
(defmethod probe-gf ((x integer)) x)")))
                  (b (write-file* "b.lisp" (source "(defun probe () 2)")))
                  (other (write-file* "other.lisp" (source "(defun dup () 1)
(defun dup () 2)
(defun calls-missing () (clmcp-redef-missing-function))")))
                  (load-a (lambda () (load (compile-file a))))
                  (reload-a (lambda () (load (compile-file-pathname a))))
                  (load-b (lambda () (load (compile-file b)))))
             (testing "a first load: compile-file defines the macro, the fasl again"
               (fresh-package)
               (ok (plusp (bare load-a)) "SBCL signals it")
               (fresh-package)
               (multiple-value-bind (count details) (suppressed load-a)
                 (ok (zerop count) (format nil "dropped (~A)" details))))
             (testing "a reload"
               (ok (<= 4 (bare reload-a)) "SBCL signals one per definition")
               (multiple-value-bind (count details) (suppressed reload-a)
                 (ok (zerop count) (format nil "dropped (~A)" details))))
             (testing "another file's redefinition"
               (ok (= 1 (bare load-b)) "SBCL signals it")
               (bare reload-a)
               (multiple-value-bind (count details) (suppressed load-b)
                 (ok (zerop count) (format nil "dropped (~A)" details))))
             (testing "warnings of other kinds are kept"
               (multiple-value-bind (count details)
                   (suppressed (lambda () (load (compile-file other))))
                 (ok (not (search "redefining" details))
                     (format nil "the second DUP's notice is dropped (~A)" details))
                 (ok (search "Duplicate definition" details) "Duplicate definition is kept")
                 (ok (search "CLMCP-REDEF-MISSING-FUNCTION" details)
                     "an undefined function is kept")
                 (ok (<= 2 count) (format nil "and counted (~A)" details)))))
        (ignore-errors (delete-package package-name))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(deftest load-system-drops-redefinition-notices-on-first-load-and-reload
  ;; Through load-system itself: a first load (force=false) and a reload
  ;; (force=true) of a system whose second file redefines its first file's
  ;; function, with a warning of another kind signalled while it loads.
  (let ((root (uiop:ensure-directory-pathname
               (uiop:merge-pathnames*
                (format nil "clmcp-redef-system-~36R/"
                        (random most-positive-fixnum (make-random-state t)))
                (uiop:temporary-directory))))
        (system "clmcp-redef-fixture")
        (package-name "CLMCP-REDEF-FIXTURE"))
    (flet ((write-file* (name text)
             (let ((path (uiop:merge-pathnames* name root)))
               (ensure-directories-exist path)
               (with-open-file (out path :direction :output :if-exists :supersede)
                 (write-string text out))
               path))
           (forget ()
             (asdf:clear-system system)
             (ignore-errors (delete-package package-name))
             (asdf:load-asd (uiop:merge-pathnames* "clmcp-redef-fixture.asd" root)))
           (bare-redefinitions (reload-p)
             ;; What ASDF's own load signals, with nothing dropped.  A reload
             ;; clears the system as load-system does: ASDF refuses :force in
             ;; a call nested in another operation, such as rove's test-op.
             (let ((count 0)
                   (*error-output* (make-broadcast-stream))
                   (*standard-output* (make-broadcast-stream)))
               (when reload-p
                 (asdf:clear-system system)
                 (asdf:load-asd (uiop:merge-pathnames* "clmcp-redef-fixture.asd" root)))
               (handler-bind ((warning (lambda (w)
                                         (when (typep w 'sb-kernel:redefinition-warning)
                                           (incf count))
                                         (muffle-warning w))))
                 (asdf:load-system system))
               count)))
      (unwind-protect
           (progn
             (write-file* "clmcp-redef-fixture.asd"
                          "(defsystem \"clmcp-redef-fixture\" :serial t
  :components ((:file \"package\") (:file \"a\") (:file \"b\") (:file \"other\")))")
             (write-file* "package.lisp" "(defpackage #:clmcp-redef-fixture (:use #:cl))")
             (write-file* "a.lisp" "(in-package #:clmcp-redef-fixture)
(defun probe () 1)")
             (write-file* "b.lisp" "(in-package #:clmcp-redef-fixture)
(defun probe () 2)")
             (write-file* "other.lisp" "(in-package #:clmcp-redef-fixture)
(warn \"clmcp-redef-fixture: a warning of another kind\")")
             (forget)
             (ok (plusp (bare-redefinitions nil)) "a bare first load signals a redefinition")
             (ok (plusp (bare-redefinitions t)) "and so does a bare reload")
             (forget)
             (dolist (force '(nil t))
               (let* ((ht (load-system system :force force))
                      (details (or (gethash "warning_details" ht) "")))
                 (ok (string= "loaded" (gethash "status" ht))
                     (format nil "~:[first load~;reload~] loaded" force))
                 (ok (search "a warning of another kind" details)
                     (format nil "~:[first load~;reload~]: the other warning is kept" force))
                 (ok (and (= 1 (gethash "warnings" ht))
                          (not (search "redefining" details)))
                     (format nil "~:[first load~;reload~]: no redefinition (~A)"
                             force details)))))
        (asdf:clear-system system)
        (ignore-errors (delete-package package-name))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(deftest load-system-force-reload-reports-no-redefinitions
  (testing "force=true on an already-loaded system reports zero warnings
(the redefinitions a reload makes are dropped)"
    (let ((ht (load-system "cl-mcp" :force t)))
      (ok (hash-table-p ht))
      (ok (string= "loaded" (gethash "status" ht)))
      (ok (integerp (gethash "warnings" ht)))
      (ok (zerop (gethash "warnings" ht))
          "reloading an already-loaded system must not surface noise"))))

(deftest load-system-clear-fasls-recompiles-package-inferred
  (testing "clear_fasls recompiles dependency subsystems regardless of timestamps"
    ;; ASDF's :FORCE T only forces the NAMED system, not its
    ;; dependencies.  For package-inferred systems the actual code
    ;; lives in dependency subsystems (\"fixture/src/main\"), so a
    ;; source rewrite landing in the same second as the previous
    ;; compile is masked by second-granularity FILE-WRITE-DATE and
    ;; the stale fasl gets reloaded.  clear_fasls must delete the
    ;; cached fasls so recompilation happens unconditionally.
    (let* ((dir (merge-pathnames "clmcp-clear-fasls-fixture/"
                                 (uiop:temporary-directory)))
           (src-dir (merge-pathnames "src/" dir))
           (asd (merge-pathnames "clmcp-clear-fasls-fixture.asd" dir))
           (main (merge-pathnames "main.lisp" src-dir)))
      (unwind-protect
          (flet ((write-main (value)
                   (with-open-file (out main :direction :output
                                             :if-exists :supersede)
                     (format out "(defpackage #:clmcp-clear-fasls-fixture/src/main~%~
                                    (:use #:cl)~%  (:export #:answer))~%~
                                  (in-package #:clmcp-clear-fasls-fixture/src/main)~%~
                                  (defun answer () ~A)~%"
                             value))))
            (uiop:delete-directory-tree dir :validate t
                                            :if-does-not-exist :ignore)
            (ensure-directories-exist src-dir)
            (with-open-file (out asd :direction :output
                                     :if-exists :supersede)
              (write-string "(asdf:defsystem \"clmcp-clear-fasls-fixture\"
  :class :package-inferred-system
  :depends-on (\"clmcp-clear-fasls-fixture/src/main\"))" out))
            (write-main 1)
            (asdf:load-asd asd)
            (ok (string= "loaded"
                         (gethash "status"
                                  (load-system "clmcp-clear-fasls-fixture"))))
            (ok (= 1 (funcall (find-symbol
                               "ANSWER" "CLMCP-CLEAR-FASLS-FIXTURE/SRC/MAIN"))))
            ;; Rewrite the dependency subsystem's source immediately —
            ;; almost always within the same second as the compile above,
            ;; which is exactly the case clear_fasls must defeat.
            (write-main 2)
            (ok (string= "loaded"
                         (gethash "status"
                                  (load-system "clmcp-clear-fasls-fixture"
                                               :clear-fasls t))))
            (ok (= 2 (funcall (find-symbol
                               "ANSWER" "CLMCP-CLEAR-FASLS-FIXTURE/SRC/MAIN")))
                "clear_fasls must pick up a same-second source rewrite"))
        (uiop:delete-directory-tree dir :validate t
                                        :if-does-not-exist :ignore)))))

(deftest load-system-clear-fasls-on-a-subsystem-clears-its-primary
  (testing "clear_fasls given a package-inferred subsystem recompiles its dependencies (#167)"
    ;; A subsystem such as fixture/src/contracts has no source directory of
    ;; its own, so clear_fasls used to delete nothing and still report a
    ;; successful load: an edit to a dependency whose FASL looked newer than
    ;; its source stayed unloaded.  The FASLs are dated into the future here,
    ;; so the case does not depend on landing in the same second.
    (let* ((name "clmcp-clear-fasls-sub")
           (dir (merge-pathnames (format nil "~A/" name) (uiop:temporary-directory)))
           (src-dir (merge-pathnames "src/" dir))
           (impl (merge-pathnames "impl.lisp" src-dir)))
      (unwind-protect
           (flet ((write-impl (body)
                    (with-open-file (out impl :direction :output :if-exists :supersede)
                      (format out "(defpackage #:~A/src/impl (:use #:cl) (:export #:twice))~%~
                                   (in-package #:~A/src/impl)~%~
                                   (defun twice (x) ~A)~%"
                              name name body)))
                  (twice (x)
                    (funcall (find-symbol "TWICE" (format nil "~:@(~A~)/SRC/IMPL" name)) x))
                  (date-fasls-ahead ()
                    (let ((future (- (+ (get-universal-time) 120) 2208988800)))
                      (dolist (fasl (directory
                                     (merge-pathnames
                                      "**/*.fasl" (asdf:apply-output-translations dir))))
                        (uiop:symbol-call :sb-posix :utimes
                                          (namestring fasl) future future)))))
             (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
             (ensure-directories-exist src-dir)
             (with-open-file (out (merge-pathnames (format nil "~A.asd" name) dir)
                                  :direction :output :if-exists :supersede)
               (format out "(asdf:defsystem ~S :class :package-inferred-system~%~
                             :depends-on (~S))~%"
                       name (format nil "~A/src/impl" name)))
             (with-open-file (out (merge-pathnames "contracts.lisp" src-dir)
                                  :direction :output :if-exists :supersede)
               (format out "(defpackage #:~A/src/contracts (:use #:cl)~%~
                             (:import-from #:~A/src/impl #:twice))~%~
                            (in-package #:~A/src/contracts)~%"
                       name name name))
             (write-impl "(+ x x 1)")
             (asdf:load-asd (merge-pathnames (format nil "~A.asd" name) dir))
             (let ((subsystem (format nil "~A/src/contracts" name)))
               (ok (string= "loaded" (gethash "status" (load-system subsystem))))
               (ok (= 7 (twice 3)) "precondition: the wrong definition is loaded")
               (write-impl "(* 2 x)")
               (date-fasls-ahead)
               (let ((ht (load-system subsystem :clear-fasls t)))
                 (ok (string= "loaded" (gethash "status" ht)))
                 (ok (plusp (gethash "fasls_deleted" ht 0))
                     "the FASLs of the tree were deleted")
                 (ok (equal name (gethash "fasls_cleared_from" ht))
                     "from the primary system's directory")
                 (ok (search "clear_fasls: deleted"
                             (gethash "text"
                                      (aref (gethash "content"
                                                     (build-load-system-response
                                                      subsystem ht))
                                            0)))
                     "and the response says how many"))
               (ok (= 6 (twice 3)) "the edited dependency was recompiled")))
        (ignore-errors (asdf:clear-system name))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest load-system-response-says-when-clear-fasls-deleted-nothing
  (testing "a clear_fasls that deleted nothing is not silent"
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "status" ht) "loaded"
            (gethash "duration_ms" ht) 5
            (gethash "warnings" ht) 0
            (gethash "fasls_deleted" ht) 0)
      (let ((text (gethash "text" (aref (gethash "content"
                                                 (build-load-system-response "nowhere" ht))
                                        0))))
        (ok (search "clear_fasls deleted no FASLs" text) text)
        (ok (search "no source directory found for nowhere" text) text)))))
