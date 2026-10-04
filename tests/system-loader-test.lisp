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

(deftest decide-suppress-redefinition-helper
  (testing "%decide-suppress-redefinition honors :auto, T, and NIL"
    (testing ":auto keeps only the conflicts a project can act on"
      (ok (eq :conflicts
              (cl-mcp/src/system-loader-core::%decide-suppress-redefinition :auto))))
    (testing "explicit T always suppresses"
      (ok (eq t (cl-mcp/src/system-loader-core::%decide-suppress-redefinition t))))
    (testing "explicit NIL never suppresses"
      (ok (null (cl-mcp/src/system-loader-core::%decide-suppress-redefinition nil))))))

(deftest suppress-redefinition-warning-filter-behavior
  (testing "%call-with-suppressed-output drops redefining-warnings when asked"
    (let ((thunk
           (lambda ()
             (warn "redefining FOO in DEFUN")
             (warn "redefining BAR in DEFMACRO")
             (warn "something real and bad")
             :done)))
      (testing "without suppress: all three warnings counted"
        (multiple-value-bind (result warning-count details)
            (cl-mcp/src/system-loader-core::%call-with-suppressed-output thunk)
          (ok (eq result :done))
          (ok (= warning-count 3))
          (ok (search "redefining FOO" details))
          (ok (search "something real" details))))
      (testing "with suppress: redefining-warnings filtered, real one remains"
        (multiple-value-bind (result warning-count details)
            (cl-mcp/src/system-loader-core::%call-with-suppressed-output
             thunk :suppress-redefinition t)
          (ok (eq result :done))
          (ok (= warning-count 1))
          (ok (null (search "redefining FOO" details)))
          (ok (search "something real" details)))))))

(deftest redefinitions-are-reported-only-as-project-conflicts
  ;; A fresh worker's first load-system cl-mcp reported about 2000
  ;; redefinitions: its dependencies read again, SBCL's UIOP replaced by
  ;; Quicklisp's.  A reload says nothing either.  What is worth a word is a
  ;; file of the project replacing what another file defined.
  (let* ((root (uiop:ensure-directory-pathname
                (uiop:merge-pathnames*
                 (format nil "clmcp-redef-~36R/"
                         (random most-positive-fixnum (make-random-state t)))
                 (uiop:temporary-directory))))
         (project (uiop:merge-pathnames* "proj/" root))
         (library (uiop:merge-pathnames* "lib/" root))
         ;; Where a Quicklisp dist keeps a release of the same project.
         (release (uiop:merge-pathnames* "proj-20240101-git/" root))
         (link (uiop:merge-pathnames* "link" root))
         (package-name "CLMCP-REDEF-PROBE"))
    (labels ((write-file* (dir name text)
               (let ((path (uiop:merge-pathnames* name dir)))
                 (ensure-directories-exist path)
                 (with-open-file (out path :direction :output :if-exists :supersede)
                   (write-string text out))
                 path))
             (source (name)
               (format nil "(in-package #:clmcp-redef-probe)~%~A~%" name))
             (compile* (path)
               (let ((*error-output* (make-broadcast-stream)))
                 (handler-bind ((warning #'muffle-warning))
                   (compile-file path))))
             (load* (fasl &key (directories (list (truename project))))
               (cl-mcp/src/system-loader-core::%call-with-suppressed-output
                (lambda () (load fasl))
                :suppress-redefinition
                (cl-mcp/src/system-loader-core::%decide-suppress-redefinition :auto)
                :project-directories directories
                :project-name "proj"))
             (conflicts (fasl)
               (multiple-value-bind (result count details) (load* fasl)
                 (declare (ignore result))
                 (values count details))))
      (unwind-protect
           (let* ((base (compile* (write-file* library "base.lisp"
                                               (format nil "(defpackage #:clmcp-redef-probe ~
                                                            (:use #:cl))~%~
                                                            (in-package #:clmcp-redef-probe)~%~
                                                            (defun lib-fn () 1)~%"))))
                  (copy-text (source "(defun copied () 1)
(defgeneric copied-gf (x))
(defmethod copied-gf ((x integer)) x)"))
                  (release-copy (progn (load base)
                                       (compile* (write-file* release "src/copy.lisp"
                                                              copy-text))))
                  (lib-lists (compile* (write-file* library "lists.lisp"
                                                    (source "(defun lib-flatten () 1)"))))
                  (a (compile* (write-file* project "a.lisp"
                                            (source "(defun probe () 1)
(defmacro probe-macro () 1)
(defgeneric probe-gf (x))
(defmethod probe-gf ((x integer)) x)
(defun (setf probe-place) (value) value)"))))
                  (b (compile* (write-file* project "b.lisp" (source "(defun probe () 2)"))))
                  (m (compile* (write-file* project "m.lisp"
                                            (source "(defmethod probe-gf ((x integer)) (1+ x))"))))
                  (g (compile* (write-file* project "g.lisp" (source "(defgeneric probe-gf (x))"))))
                  (setter (compile* (write-file* project "setter.lisp"
                                                 (source "(defun (setf probe-place) (value)
  (1+ value))"))))
                  (clobber (compile* (write-file* project "clobber.lisp"
                                                  (source "(defun lib-fn () 2)"))))
                  (project-lists (compile* (write-file* project "lists.lisp"
                                                        (source "(defun lib-flatten () 2)"))))
                  (late (compile* (write-file* library "late.lisp" (source "(defun probe () 3)"))))
                  (project-copy (compile* (write-file* project "src/copy.lisp" copy-text))))
             (testing "a reload of a project file says nothing"
               (load* a)
               (multiple-value-bind (count details) (conflicts a)
                 (ok (zerop count) (format nil "defun, defmacro, defgeneric, defmethod, ~
                                                (setf f) (~A)"
                                           details))))
             (testing "another project file redefining a function is reported, with both files"
               (multiple-value-bind (count details) (conflicts b)
                 (ok (= 1 count) "b.lisp replaces a.lisp's PROBE")
                 (ok (and (search "PROBE" details)
                          (search "a.lisp" details)
                          (search "b.lisp" details))
                     "the details name the old file and the new one")))
             (testing "a method, a generic function and a (setf f) are reported too"
               (multiple-value-bind (count details) (conflicts m)
                 (ok (and (= 1 count) (search "DEFMETHOD" details)) "m.lisp replaces a method"))
               (multiple-value-bind (count details) (conflicts g)
                 (ok (and (= 1 count) (search "DEFGENERIC" details))
                     "g.lisp replaces a.lisp's generic function"))
               (multiple-value-bind (count details) (conflicts setter)
                 (ok (and (= 1 count) (search "a.lisp" details))
                     "setter.lisp replaces a.lisp's (setf probe-place)")))
             (testing "a macro is reported, and so is a function replacing it"
               ;; Review of the rework: (fdefinition 'macro) is SBCL's guard,
               ;; defined in SYS:SRC;, and (macro-function 'function) is NIL.
               (let ((macro (compile* (write-file* project "macro.lisp"
                                                   (source "(defmacro probe-macro () 2)"))))
                     (function (compile* (write-file* project "function.lisp"
                                                      (source "(defun probe-macro () 3)")))))
                 (load* a)
                 (multiple-value-bind (count details) (conflicts macro)
                   (ok (and (= 1 count) (search "a.lisp" details))
                       "macro.lisp replaces a.lisp's PROBE-MACRO"))
                 (multiple-value-bind (count details) (conflicts function)
                   (ok (and (plusp count) (search "macro.lisp" details)
                            (not (search "SYS:" details)))
                       (format nil "function.lisp replaces macro.lisp's macro (~A)" details)))))
             (testing "a project file clobbering a library's function is reported"
               (ok (= 1 (conflicts clobber)) "clobber.lisp replaces lib-fn")
               (load* lib-lists)
               (ok (= 1 (conflicts project-lists))
                   "proj/lists.lisp replaces lib/lists.lisp's function: a shared path is no copy"))
             (testing "what a dependency redefines is not the project's to act on"
               (ok (zerop (conflicts late)) "lib/late.lisp replaces PROBE"))
             (testing "a copy of the same file from another release is a reload"
               (load* release-copy)
               (multiple-value-bind (count details) (conflicts project-copy)
                 (ok (zerop count) (format nil "proj/src/copy.lisp over ~
                                                proj-20240101-git/src/copy.lisp (~A)"
                                           details))))
             (testing "without a project directory nothing is a conflict"
               (load* a)
               (multiple-value-bind (result count) (load* b :directories nil)
                 (declare (ignore result))
                 (ok (zerop count))))
             (testing "the replacing file is the new definition's, not the one compiling"
               ;; Review of #220: b.lisp loaded while a.lisp compiles replaces
               ;; a.lisp's PROBE: still two files, one name.
               (load* a)
               (let ((nested (write-file* project "nested.lisp"
                                          (format nil "(eval-when (:compile-toplevel) (load ~S))~%"
                                                  (namestring b)))))
                 (multiple-value-bind (result count)
                     (cl-mcp/src/system-loader-core::%call-with-suppressed-output
                      (lambda ()
                        (let ((*error-output* (make-broadcast-stream)))
                          (compile-file nested)))
                      :suppress-redefinition :conflicts
                      :project-directories (list (truename project)))
                   (declare (ignore result))
                   (ok (= 1 count)))))
             (testing "a source name that is no file proves nothing"
               ;; Review of #220 (160da27): code compiled inside repl-eval's
               ;; compilation unit records "repl-eval" for every file.
               (flet ((compile-as-repl (path)
                        (with-compilation-unit (:override t :source-namestring "repl-eval")
                          (compile* path))))
                 (load* (compile-as-repl (uiop:merge-pathnames* "a.lisp" project)))
                 (ok (zerop (conflicts (compile-as-repl
                                        (uiop:merge-pathnames* "b.lisp" project)))))))
             (testing "a recorded name with a character a namestring escapes is still a file"
               (let ((odd (uiop:merge-pathnames* (uiop:parse-native-namestring "odd[1].lisp")
                                                 project)))
                 (with-open-file (out odd :direction :output :if-exists :supersede)
                   (write-string "nil" out))
                 (ok (cl-mcp/src/system-loader-core::%source-file (namestring odd))
                     "SBCL records odd\\[1].lisp")))
             (testing "a project reached through a symbolic link is still one project"
               (uiop:run-program (list "ln" "-s"
                                       (uiop:native-namestring
                                        (string-right-trim "/" (namestring project)))
                                       (uiop:native-namestring link)))
               (let* ((via (uiop:ensure-directory-pathname link))
                      (a-via (compile* (uiop:merge-pathnames* "a.lisp" via)))
                      (m-via (compile* (uiop:merge-pathnames* "m.lisp" via))))
                 (load* a-via)
                 (multiple-value-bind (count details) (conflicts a-via)
                   (ok (zerop count) (format nil "reloaded through the link (~A)" details)))
                 (ok (= 1 (conflicts m-via)) "m.lisp through the link replaces a.lisp's method")))
             (testing "a name defined twice in one file is still SBCL's own warning"
               (let ((dup (write-file* project "dup.lisp"
                                       (source "(defun dup () 1)
(defun dup () 2)"))))
                 (multiple-value-bind (result count details)
                     (cl-mcp/src/system-loader-core::%call-with-suppressed-output
                      (lambda () (load (compile-file dup)))
                      :suppress-redefinition :conflicts
                      :project-directories (list (truename project)))
                   (declare (ignore result))
                   (ok (and (plusp count) (search "Duplicate definition" details)))))))
        (ignore-errors (delete-package package-name))
        ;; rm, not DELETE-FILE: the link itself goes, never what it names.
        (uiop:run-program (list "rm" "-f" (uiop:native-namestring link))
                          :ignore-error-status t)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(deftest project-directories-cover-sources-the-asd-moves-elsewhere
  ;; Review of the rework: an .asd in systems/ with :pathname "../src/" has
  ;; every source outside its own directory.
  (let* ((root (uiop:ensure-directory-pathname
                (uiop:merge-pathnames*
                 (format nil "clmcp-redef-dirs-~36R/"
                         (random most-positive-fixnum (make-random-state t)))
                 (uiop:temporary-directory))))
         (asd (uiop:merge-pathnames* "systems/clmcp-redef-moved.asd" root))
         (source (uiop:merge-pathnames* "src/one.lisp" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist asd)
           (ensure-directories-exist source)
           (with-open-file (out asd :direction :output :if-exists :supersede)
             (write-string "(defsystem \"clmcp-redef-moved\" :pathname \"../src/\"
  :components ((:file \"one\")))" out))
           (with-open-file (out source :direction :output :if-exists :supersede)
             (write-string "nil" out))
           (asdf:load-asd asd)
           (let ((directories (cl-mcp/src/system-loader-core::%project-directories
                               "clmcp-redef-moved/sub")))
             (ok (equal (truename (uiop:merge-pathnames* "systems/" root)) (first directories))
                 "the .asd's directory first")
             (ok (member (truename (uiop:merge-pathnames* "src/" root)) directories
                         :test #'equal)
                 "and the one :pathname moves the sources to")))
      (asdf:clear-system "clmcp-redef-moved")
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(deftest load-system-force-default-auto-suppresses-redefinition
  (testing "force=true on an already-loaded system reports zero warnings
(the implied redefining-warnings are now auto-filtered)"
    (let ((ht (load-system "cl-mcp" :force t)))
      (ok (hash-table-p ht))
      (ok (string= "loaded" (gethash "status" ht)))
      (ok (integerp (gethash "warnings" ht)))
      (ok (zerop (gethash "warnings" ht))
          "reloading an already-loaded system must not surface noise"))))

(deftest load-system-explicit-suppress-nil-is-honored
  (testing "explicit :suppress-redefinition-warnings nil is wired through without errors"
    (let ((ht (load-system "cl-mcp"
                           :force t
                           :suppress-redefinition-warnings nil)))
      (ok (hash-table-p ht))
      (ok (string= "loaded" (gethash "status" ht))))))

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
