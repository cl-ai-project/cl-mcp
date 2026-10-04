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

(define-condition loader-probe-warning (warning)
  ()
  (:report "loader probe: a full warning"))

(define-condition loader-probe-style-warning (style-warning)
  ()
  (:report "loader probe: a style warning"))

(defun %call-with-probe-files (files thunk)
  "Write FILES, an alist of (name . text), into a fresh temporary directory, call
THUNK with a function from a name to its path, and remove the directory.  The
package CLMCP-WARN-PROBE exists, empty, for the files to be in."
  (let ((root (uiop:ensure-directory-pathname
               (uiop:merge-pathnames*
                (format nil "clmcp-warn-~36R/"
                        (random most-positive-fixnum (make-random-state t)))
                (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (ignore-errors (delete-package "CLMCP-WARN-PROBE"))
           (make-package "CLMCP-WARN-PROBE" :use '(#:cl))
           (loop for (name . text) in files
                 for path = (uiop:merge-pathnames* name root)
                 do (ensure-directories-exist path)
                    (with-open-file (out path :direction :output :if-exists :supersede
                                              :external-format :utf-8)
                      (write-string text out)))
           (funcall thunk (lambda (name) (uiop:merge-pathnames* name root))))
      (ignore-errors (delete-package "CLMCP-WARN-PROBE"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun %suppressed (thunk)
  "Call THUNK under %CALL-WITH-SUPPRESSED-OUTPUT.  Return THUNK's values as a
list, and the warning records."
  (multiple-value-bind (result count details stderr records)
      (cl-mcp/src/system-loader-core::%call-with-suppressed-output
       (lambda () (multiple-value-list (funcall thunk))))
    (declare (ignore count details stderr))
    (values result records)))

(deftest a-full-warning-is-left-to-the-compiler
  ;; compile-file's failure flag is what ASDF fails a build on.  A handler that
  ;; muffles a full WARNING clears it, and load-system then loads what ASDF --
  ;; and run-tests -- refuse.
  (%call-with-probe-files
   '(("full.lisp" . "(in-package #:clmcp-warn-probe)
(defun two-arguments (a b) (+ a b))
(defun wrong-arity () (two-arguments 1 2 3))
")
     ("style.lisp" . "(in-package #:clmcp-warn-probe)
(defun unused-argument (x y) (* x 2))
"))
   (lambda (path)
     (testing "a full warning: the compile is reported as failed"
       (let ((values (%suppressed (lambda () (compile-file (funcall path "full.lisp"))))))
         (ok (eq t (third values)) "failure-p")))
     (testing "a style warning: the compile is reported as clean"
       (let ((values (%suppressed (lambda () (compile-file (funcall path "style.lisp"))))))
         (ok (null (second values)) "warnings-p")
         (ok (null (third values)) "failure-p"))))))

(deftest a-warning-is-recorded-with-its-class-and-severity
  (multiple-value-bind (values records)
      (%suppressed (lambda ()
                     (warn 'loader-probe-warning)
                     (warn 'loader-probe-style-warning)
                     :done))
    (ok (equal '(:done) values))
    (ok (= 2 (length records)))
    (let ((full (first records))
          (style (second records)))
      (ok (equal "warning" (gethash "severity" full)))
      (ok (equal "CL-MCP/TESTS/SYSTEM-LOADER-TEST::LOADER-PROBE-WARNING"
                 (gethash "class" full)))
      (ok (equal "loader probe: a full warning" (gethash "message" full)))
      (ok (null (gethash "file" full)) "signalled outside a compile: no place")
      (ok (equal "style-warning" (gethash "severity" style)))
      (ok (equal "loader probe: a style warning" (gethash "message" style))))))

(deftest a-compile-time-warning-is-recorded-with-its-place
  ;; The first line is in Japanese: SBCL gives the place in octets, and a
  ;; position read as characters lands on another line.
  (%call-with-probe-files
   '(("place.lisp" . ";;;; 警告の位置を数える — 日本語のコメントが先頭にあるファイル
(in-package #:clmcp-warn-probe)

(defun unused-argument (x y)
  (* x 2))

;;; A comment between two forms: the place is the form's, not the comment's.
(defun two-arguments (a b)
  (+ a b))

(defun wrong-arity ()
  (two-arguments 1 2 3))
"))
   (lambda (path)
     (multiple-value-bind (values records)
         (%suppressed (lambda () (compile-file (funcall path "place.lisp"))))
       (declare (ignore values))
       (ok (= 2 (length records)))
       (let ((style (first records))
             (full (second records)))
         (ok (equal "style-warning" (gethash "severity" style)))
         (ok (eql 4 (gethash "line" style)))
         (ok (equal "(defun unused-argument)" (gethash "form" style)))
         (ok (search "place.lisp" (gethash "file" style)))
         (ok (equal "warning" (gethash "severity" full)))
         (ok (eql 11 (gethash "line" full)))
         (ok (equal "(defun wrong-arity)" (gethash "form" full))))))))

(deftest a-warning-signalled-again-at-load-is-recorded-once
  (%call-with-probe-files
   '(("twice.lisp" . "(in-package #:clmcp-warn-probe)
(defun dup () 1)
(defun dup () 2)
")
     ("same-text.lisp" . "(in-package #:clmcp-warn-probe)
(defun first-one (y) 1)
(defun second-one (y) 2)
(defun third-one () (clmcp-warn-missing-function))
"))
   (lambda (path)
     (testing "SBCL signals a duplicate definition compiling the file and again loading it"
       (multiple-value-bind (values records)
           (%suppressed (lambda () (load (compile-file (funcall path "twice.lisp")))))
         (declare (ignore values))
         (ok (= 1 (count-if (lambda (record)
                              (search "Duplicate definition" (gethash "message" record)))
                            records)))))
     (testing "warnings that read the same in two places are two, of one kind"
       (multiple-value-bind (values records)
           (%suppressed (lambda () (compile-file (funcall path "same-text.lisp"))))
         (declare (ignore values))
         (ok (equal '(2 3 4) (mapcar (lambda (record) (gethash "line" record)) records)))
         (ok (eql (gethash "kind" (first records)) (gethash "kind" (second records)))
             "the two unused variables")
         (ok (not (eql (gethash "kind" (first records)) (gethash "kind" (third records))))
             "the undefined function is another kind"))))))

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

(defun %call-with-warning-system (body thunk)
  "Make the ASDF system clmcp-warn-fixture, whose one source file holds BODY in
the package CLMCP-WARN-FIXTURE, call THUNK with the system's name and a function
that makes ASDF forget what it loaded of it, and remove everything again."
  (let ((root (uiop:ensure-directory-pathname
               (uiop:merge-pathnames*
                (format nil "clmcp-warn-system-~36R/"
                        (random most-positive-fixnum (make-random-state t)))
                (uiop:temporary-directory))))
        (system "clmcp-warn-fixture")
        (package-name "CLMCP-WARN-FIXTURE"))
    (flet ((write-file* (name text)
             (let ((path (uiop:merge-pathnames* name root)))
               (ensure-directories-exist path)
               (with-open-file (out path :direction :output :if-exists :supersede)
                 (write-string text out))))
           (forget ()
             (asdf:clear-system system)
             (ignore-errors (delete-package package-name))
             (asdf:load-asd (uiop:merge-pathnames* "clmcp-warn-fixture.asd" root))))
      (unwind-protect
           (progn
             (write-file* "clmcp-warn-fixture.asd"
                          "(defsystem \"clmcp-warn-fixture\" :serial t
  :components ((:file \"package\") (:file \"body\")))")
             (write-file* "package.lisp" "(defpackage #:clmcp-warn-fixture (:use #:cl))")
             (write-file* "body.lisp"
                          (format nil "(in-package #:clmcp-warn-fixture)~%~A~%" body))
             (forget)
             (funcall thunk system #'forget))
        (asdf:clear-system system)
        (ignore-errors (delete-package package-name))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(defun %response-text (system ht)
  "Return the text a client reads for HT, load-system's result for SYSTEM."
  (gethash "text" (aref (gethash "content" (build-load-system-response system ht)) 0)))

(deftest load-system-fails-on-a-full-warning-as-asdf-does
  (%call-with-warning-system
   "(defun two-arguments (a b) (+ a b))
(defun wrong-arity () (two-arguments 1 2 3))"
   (lambda (system forget)
     (testing "ASDF's own load refuses the file"
       (ok (typep (nth-value 1 (ignore-errors
                                (let ((*error-output* (make-broadcast-stream))
                                      (*standard-output* (make-broadcast-stream)))
                                  (asdf:load-system system))))
                  'uiop:compile-file-error)))
     (funcall forget)
     (testing "and so does load-system, naming the warning and its place"
       (let* ((ht (load-system system :force nil))
              (records (gethash "warning_records" ht))
              (text (%response-text system ht)))
         (ok (equal "error" (gethash "status" ht)))
         (ok (eql 1 (length records)))
         (ok (equal "warning" (gethash "severity" (aref records 0))))
         (ok (eql 3 (gethash "line" (aref records 0))))
         (ok (search "called with three arguments" text))
         (ok (search "body.lisp:3 (defun wrong-arity)" text))
         (ok (search "ASDF refuses a file that compiles with a WARNING" text))
         (ok (null (nth-value 1 (gethash "compile_failed" ht)))
             "what the builder was told about the error is not left in the response"))))))

(deftest load-system-loads-style-warnings-and-sums-them-up-by-kind
  (%call-with-warning-system
   "(defun first-one (y) 1)
(defun second-one (y) 2)
(defun third-one () (clmcp-warn-missing-function))"
   (lambda (system forget)
     (declare (ignore forget))
     (let* ((ht (load-system system :force nil))
            (text (%response-text system ht)))
       (ok (equal "loaded" (gethash "status" ht)))
       (ok (eql 3 (gethash "warnings" ht)))
       (ok (search "(3 style warnings)" text))
       (ok (search "2x The variable Y is defined but never used." text))
       (ok (search "body.lisp:2 (defun first-one)" text))
       (ok (search "body.lisp:3 (defun second-one)" text))
       (ok (search "CLMCP-WARN-MISSING-FUNCTION" text))))))

(defun %warning-record (&rest fields)
  "Return a warning record as load-system's core makes them, from FIELDS, a
property list of its JSON keys and values."
  (let ((record (make-hash-table :test #'equal)))
    (loop for (key value) on fields by #'cddr
          do (setf (gethash key record) value))
    record))

(defun %loaded-with (records)
  "Return a successful load-system result that carries RECORDS."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "status" ht) "loaded"
          (gethash "duration_ms" ht) 7
          (gethash "warnings" ht) (length records)
          (gethash "warning_records" ht) (coerce records 'vector))
    ht))

(deftest load-system-response-shows-a-full-warning-whole
  (let ((text (%response-text
               "fake-system"
               (%loaded-with
                (list (%warning-record "severity" "warning"
                                       "class" "SIMPLE-WARNING"
                                       "message" (format nil "first line of it~%second line of it")
                                       "kind" 1))))))
    (ok (search "(1 warning)" text))
    (ok (search "first line of it" text))
    (ok (search "second line of it" text))))

(deftest load-system-response-bounds-what-it-lists-of-style-warnings
  (flet ((style (kind message &optional (line kind))
           (%warning-record "severity" "style-warning"
                            "class" "SB-INT:SIMPLE-STYLE-WARNING"
                            "message" message
                            "kind" kind
                            "file" "src/a.lisp"
                            "line" line
                            "form" (format nil "(defun f~D)" line))))
    (testing "kinds past the limit are counted, not listed"
      (let ((text (let ((cl-mcp/src/tools/response-builders::*load-style-kinds-shown* 3))
                    (%response-text
                     "fake-system"
                     (%loaded-with
                      (loop for kind from 1 to 5
                            collect (style kind (format nil "style kind ~D" kind))))))))
        (ok (search "(5 style warnings)" text))
        (ok (search "style kind 3" text))
        (ok (null (search "style kind 4" text)))
        (ok (search "2 more kinds" text))))
    (testing "places past the limit are counted, not listed"
      (let ((text (let ((cl-mcp/src/tools/response-builders::*load-warning-places-shown* 2))
                    (%response-text
                     "fake-system"
                     (%loaded-with (loop for line from 11 to 14
                                         collect (style 1 "the same kind" line)))))))
        (ok (search "4x the same kind" text))
        (ok (search "src/a.lisp:12 (defun f12)" text))
        (ok (null (search "src/a.lisp:13" text)))
        (ok (search "2 more" text))))
    (testing "a style warning is shown by its first line"
      (let ((text (%response-text
                   "fake-system"
                   (%loaded-with
                    (list (style 1 (format nil "the headline~%a paragraph of advice")))))))
        (ok (search "the headline" text))
        (ok (null (search "a paragraph of advice" text)))))))

(defun %failed-with (records &key compile-failed)
  "Return a failed load-system result that carries RECORDS, as the core builds
one: COMPILE-FAILED when the error was the compiler's verdict on a file."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "status" ht) "error"
          (gethash "duration_ms" ht) 7
          (gethash "message" ht) "the load stopped"
          (gethash "warnings" ht) (length records)
          (gethash "warning_records" ht) (coerce records 'vector))
    (when compile-failed
      (setf (gethash "compile_failed" ht) t))
    ht))

(deftest load-system-response-bounds-a-flood-of-full-warnings
  (let ((text (let ((cl-mcp/src/tools/response-builders::*load-full-warnings-shown* 2))
                (%response-text
                 "fake-system"
                 (%loaded-with
                  (loop for n from 1 to 5
                        collect (%warning-record
                                 "severity" "warning"
                                 "class" "COMMON-LISP:SIMPLE-WARNING"
                                 "message" (format nil "full warning number ~D" n)
                                 "kind" 1)))))))
    (ok (search "full warning number 2" text))
    (ok (null (search "full warning number 3" text)))
    (ok (search "3 more warnings" text))))

(deftest load-system-response-tells-a-refused-compile-from-another-error
  (flet ((full ()
           (%warning-record "severity" "warning"
                            "class" "COMMON-LISP:SIMPLE-WARNING"
                            "message" "a warning signalled while loading"
                            "kind" 1))
         (style ()
           (%warning-record "severity" "style-warning"
                            "class" "SB-INT:SIMPLE-STYLE-WARNING"
                            "message" "a style warning"
                            "kind" 2)))
    (testing "the compiler's verdict: the warning is the cause, the style warnings are counted"
      (let ((text (%response-text
                   "fake-system"
                   (%failed-with (list (full) (style)) :compile-failed t))))
        (ok (search "ASDF refuses a file that compiles with a WARNING" text))
        (ok (search "a warning signalled while loading" text))
        (ok (search "1 style warning" text))
        (ok (search "fix the warning above" text))))
    (testing "a package at variance: nothing in the file is wrong, the worker is stale"
      (let ((text (%response-text
                   "fake-system"
                   (%failed-with
                    (list (%warning-record
                           "severity" "warning"
                           "class" "SB-INT:PACKAGE-AT-VARIANCE"
                           "message" "FAKE also exports the following symbols: (FAKE:GONE)"
                           "kind" 1))
                    :compile-failed t))))
        (ok (search "FAKE also exports the following symbols" text))
        (ok (search "the running image has stale exports" text))
        (ok (search "pool-kill-worker" text))))
    (testing "another error: the warning came before it and is not blamed for it"
      (let ((text (%response-text "fake-system" (%failed-with (list (full))))))
        (ok (search "Warnings before the error (1)" text))
        (ok (search "a warning signalled while loading" text))
        (ok (null (search "ASDF refuses" text)))
        (ok (search "pool-kill-worker" text))))))

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

(deftest load-system-force-recompiles-a-same-second-edit
  (testing "force=true recompiles a file written in the same second as its fasl"
    ;; ASDF judges a fasl current when its source is not newer, to the second,
    ;; so an edit landing in the second of the last compile ran the old code
    ;; -- while run-tests, which deletes such a fasl, ran the new.  The
    ;; source is given its fasl's own timestamp, so the case is not left to
    ;; how fast the test runs.
    (require :sb-posix)
    (let* ((name "clmcp-same-second-fixture")
           (dir (merge-pathnames (format nil "~A/" name) (uiop:temporary-directory)))
           (main (merge-pathnames "src/main.lisp" dir))
           (package (format nil "~:@(~A~)/SRC/MAIN" name)))
      (unwind-protect
          (flet ((write-main (value)
                   (with-open-file (out main :direction :output :if-exists :supersede)
                     (format out "(defpackage #:~A/src/main (:use #:cl) (:export #:answer))~%~
                                  (in-package #:~A/src/main)~%~
                                  (defun answer () ~A)~%"
                             name name value)))
                 (answer ()
                   (funcall (find-symbol "ANSWER" package))))
            (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
            (ensure-directories-exist main)
            (with-open-file (out (merge-pathnames (format nil "~A.asd" name) dir)
                                 :direction :output :if-exists :supersede)
              (format out "(asdf:defsystem ~S :class :package-inferred-system ~
                             :depends-on (~S))~%"
                      name (format nil "~A/src/main" name)))
            (write-main 1)
            (asdf:load-asd (merge-pathnames (format nil "~A.asd" name) dir))
            (ok (string= "loaded" (gethash "status" (load-system name))))
            (ok (= 1 (answer)))
            (write-main 2)
            (let* ((fasl (asdf:apply-output-translations (compile-file-pathname main)))
                   (unix (- (file-write-date fasl) (encode-universal-time 0 0 0 1 1 1970 0))))
              (uiop:symbol-call :sb-posix :utimes (namestring main) unix unix))
            (let ((ht (load-system name)))
              (ok (string= "loaded" (gethash "status" ht)))
              (ok (= 2 (answer)) "the edit is what runs")
              (ok (eql 1 (gethash "same_second_fasls_deleted" ht))
                  "the response counts the fasl it deleted")
              (ok (search "Deleted 1 FASL whose source was written in the same second"
                          (let ((content (gethash "content"
                                                  (build-load-system-response name ht))))
                            (gethash "text" (aref content 0))))
                  "and the text says so")))
        (ignore-errors (asdf:clear-system name))
        (ignore-errors (asdf:clear-system (format nil "~A/src/main" name)))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

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
