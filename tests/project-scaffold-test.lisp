;;;; tests/project-scaffold-test.lisp

(defpackage #:cl-mcp/tests/project-scaffold-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/project-scaffold-core
                #:escape-lisp-string
                #:*scaffold-marker-file*)
  (:import-from #:cl-mcp/src/project-scaffold)
  (:import-from #:cl-mcp/src/project-root)
  (:import-from #:cl-mcp/src/test-runner
                #:detect-test-framework))

(in-package #:cl-mcp/tests/project-scaffold-test)

(deftest project-scaffold-exports
  (testing "public core symbols are fbound"
    (ok (fboundp 'cl-mcp/src/project-scaffold-core:validate-project-name))
    (ok (fboundp 'cl-mcp/src/project-scaffold-core:validate-destination))
    (ok (fboundp 'cl-mcp/src/project-scaffold-core:validate-text-field))
    (ok (fboundp 'cl-mcp/src/project-scaffold-core:render-template))
    (ok (fboundp 'cl-mcp/src/project-scaffold-core:plan-scaffold))
    (ok (fboundp 'cl-mcp/src/project-scaffold:write-scaffold)))
  (testing "invalid-argument-error is a subclass of error"
    (ok (subtypep 'cl-mcp/src/project-scaffold-core:invalid-argument-error 'error))))

(deftest validate-project-name-valid
  (testing "accepts valid names"
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-lib"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "a"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-123"))))

(deftest validate-project-name-rejects
  (testing "rejects empty string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects uppercase"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "FooLib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects leading digit"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "1foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects underscore"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo_lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects slash"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo/lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects overlong name"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name
                  (make-string 65 :initial-element #\a))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name :foo)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest validate-destination
  (testing "accepts a plain relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds")))
  (testing "accepts a nested relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "work/samples")))
  (testing "rejects absolute path"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "/tmp/foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "../outside")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects embedded parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds/../..")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects empty"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest validate-text-field
  (testing "accepts normal text"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "author" "Ada Lovelace")))
  (testing "accepts empty string"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "description" "")))
  (testing "rejects newline"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "author" (format nil "Ada~%Lovelace"))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects CR"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "description" (format nil "foo~Abar" #\Return))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects double quote"
    ;; A quote closes the Lisp string literal these fields are pasted into
    ;; inside the generated .asd, turning the rest of the value into code.
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "description" "A \"cool\" parser")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects backslash"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "author" "C:\\Ada")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field "license" 42)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest render-template
  (testing "substitutes a single placeholder"
    (ok (equal "hello foo"
               (cl-mcp/src/project-scaffold-core:render-template
                "hello {{name}}" '(("name" . "foo"))))))
  (testing "substitutes multiple placeholders"
    (ok (equal "foo by Ada (MIT)"
               (cl-mcp/src/project-scaffold-core:render-template
                "{{name}} by {{author}} ({{license}})"
                '(("name" . "foo") ("author" . "Ada") ("license" . "MIT"))))))
  (testing "leaves unknown placeholder unchanged"
    (ok (equal "hello {{unknown}}"
               (cl-mcp/src/project-scaffold-core:render-template
                "hello {{unknown}}" '(("name" . "foo"))))))
  (testing "handles repeated placeholder"
    (ok (equal "foo-foo"
               (cl-mcp/src/project-scaffold-core:render-template
                "{{name}}-{{name}}" '(("name" . "foo"))))))
  (testing "passes through regex-sensitive characters in values"
    (ok (equal "value: a.b*c"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}}" '(("v" . "a.b*c"))))))
  (testing "leaves format directive ~A unchanged in value"
    (ok (equal "value: ~A done"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}} done" '(("v" . "~A"))))))
  (testing "handles backslash in value"
    (ok (equal "value: a\\b"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}}" '(("v" . "a\\b"))))))
  (testing "handles dollar sign in value"
    (ok (equal "value: $1.00"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}}" '(("v" . "$1.00")))))))

(deftest compute-parent-prompts-path
  (testing "scaffolds destination"
    (ok (equal "../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "scaffolds"))))
  (testing "two-segment destination"
    (ok (equal "../../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "work/samples"))))
  (testing "three-segment destination"
    (ok (equal "../../../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "a/b/c")))))

(deftest plan-scaffold
  (let ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
               :name "foo-lib"
               :description "A demo library"
               :author "Ada Lovelace"
               :license "MIT"
               :destination "scaffolds")))
    (testing "returns seven entries"
      (ok (= 7 (length plan))))
    (testing "each entry is (relative-path . content) of strings"
      (ok (every (lambda (entry)
                   (and (consp entry)
                        (stringp (car entry))
                        (stringp (cdr entry))))
                 plan)))
    (testing "asd file is keyed by <name>.asd and has substitutions"
      (let ((asd (cdr (assoc "foo-lib.asd" plan :test #'string=))))
        (ok asd)
        (ok (search "foo-lib" asd))
        (ok (search "Ada Lovelace" asd))
        (ok (search "A demo library" asd))
        (ok (search "MIT" asd))))
    (testing "CLAUDE.md has resolved parent-prompts path"
      (let ((md (cdr (assoc "CLAUDE.md" plan :test #'string=))))
        (ok md)
        (ok (search "../../prompts/repl-driven-development.md" md))))
    (testing "main.lisp uses the project-qualified package name"
      (let ((src (cdr (assoc "src/main.lisp" plan :test #'string=))))
        (ok src)
        (ok (search "#:foo-lib/src/main" src))))
    (testing "main-test.lisp has a scaffold-smoke deftest and no greet reference"
      (let ((test (cdr (assoc "tests/main-test.lisp" plan :test #'string=))))
        (ok test)
        (ok (search "scaffold-smoke" test))
        (ok (search ":foo-lib/src/main" test))
        (ok (null (search "greet" test)))))
    (testing "main.lisp scaffold has no :export clause"
      (let ((src (cdr (assoc "src/main.lisp" plan :test #'string=))))
        (ok src)
        (ok (null (search ":export" src)))
        (ok (null (search "defun greet" src)))))
    (testing "no unresolved placeholders remain"
      (dolist (entry plan)
        (ok (null (search "{{" (cdr entry))))))))

(deftest plan-scaffold-deeper-destination
  (let ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
               :name "demo"
               :description "d"
               :author "a"
               :license "MIT"
               :destination "work/samples")))
    (testing "parent-prompts resolves with extra ../"
      (let ((md (cdr (assoc "CLAUDE.md" plan :test #'string=))))
        (ok (search "../../../prompts/repl-driven-development.md" md))))))

(deftest validate-framework
  (testing "NIL selects the default framework"
    (ok (eq :rove (cl-mcp/src/project-scaffold-core:validate-framework nil))))
  (testing "accepts the supported framework names"
    (ok (eq :rove (cl-mcp/src/project-scaffold-core:validate-framework "rove")))
    (ok (eq :fiveam (cl-mcp/src/project-scaffold-core:validate-framework "fiveam"))))
  (testing "accepts a keyword designator"
    (ok (eq :fiveam (cl-mcp/src/project-scaffold-core:validate-framework :fiveam))))
  (testing "is case-insensitive, so FiveAM's own spelling works"
    (ok (eq :fiveam (cl-mcp/src/project-scaffold-core:validate-framework "FiveAM"))))
  (testing "rejects a framework with no template"
    ;; run-tests can execute Prove suites, but project-scaffold has no Prove
    ;; template, so accepting the name would generate a Rove project under a
    ;; Prove label.
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-framework "prove")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects an empty string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-framework "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects a non-string, non-symbol"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-framework 42)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest plan-scaffold-defaults-to-rove
  ;; The framework argument is optional, and every caller that predates it
  ;; must keep getting the Rove project it used to get.
  (let* ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
                :name "rove-default"
                :description "d" :author "a" :license "MIT"
                :destination "scaffolds"))
         (asd (cdr (assoc "rove-default.asd" plan :test #'string=)))
         (test (cdr (assoc "tests/main-test.lisp" plan :test #'string=))))
    (testing "the test system depends on rove, not fiveam"
      (ok (search "\"rove\"" asd))
      (ok (null (search "fiveam" asd))))
    (testing "the smoke test is written against Rove"
      (ok (search "#:rove" test))
      (ok (search "(deftest scaffold-smoke" test)))))

(deftest plan-scaffold-fiveam
  (let* ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
                :name "fa-demo"
                :description "A FiveAM demo"
                :author "Ada Lovelace"
                :license "MIT"
                :destination "scaffolds"
                :framework :fiveam))
         (asd (cdr (assoc "fa-demo.asd" plan :test #'string=)))
         (test (cdr (assoc "tests/main-test.lisp" plan :test #'string=)))
         (claude (cdr (assoc "CLAUDE.md" plan :test #'string=)))
         (readme (cdr (assoc "README.md" plan :test #'string=)))
         (agents (cdr (assoc "AGENTS.md" plan :test #'string=))))
    (testing "the manifest still holds the same seven files"
      (ok (= 7 (length plan)))
      (ok (assoc "src/main.lisp" plan :test #'string=)))
    (testing "the test system depends on fiveam and never on rove"
      (ok (search "\"fiveam\"" asd))
      (ok (null (search "rove" asd))))
    (testing "test-op runs the project's own suite rather than every suite"
      ;; FiveAM's suite registry is global, so a blanket run would execute
      ;; other systems' suites too.
      (ok (search ":fiveam :run!" asd))
      (ok (search ":fa-demo" asd)))
    (testing "the smoke test is written against FiveAM"
      (ok (search "#:fiveam" test))
      (ok (search "(def-suite :fa-demo" test))
      (ok (search "(in-suite :fa-demo)" test))
      (ok (search "scaffold-smoke" test))
      (ok (search ":fa-demo/src/main" test))
      (ok (null (search "deftest" test)))
      (ok (null (search "rove" test))))
    (testing "the generated docs name FiveAM, not Rove"
      (ok (search "FiveAM" claude))
      (ok (null (search "Rove" claude)))
      (ok (search "FiveAM" readme))
      (ok (null (search "Rove" readme)))
      (ok (null (search "Rove" agents))))
    (testing "src/main.lisp is framework-independent and unchanged"
      (ok (equal (cdr (assoc "src/main.lisp" plan :test #'string=))
                 (cdr (assoc "src/main.lisp"
                             (cl-mcp/src/project-scaffold-core:plan-scaffold
                              :name "fa-demo" :description "A FiveAM demo"
                              :author "Ada Lovelace" :license "MIT"
                              :destination "scaffolds")
                             :test #'string=)))))
    (testing "no unresolved placeholders remain"
      (dolist (entry plan)
        (ok (null (search "{{" (cdr entry))))))))

(defmacro with-temp-project-root ((root-var) &body body)
  "Bind cl-mcp/src/project-root:*project-root* to a fresh temp directory.
Deletes the directory on exit. ROOT-VAR is always declared IGNORABLE so
callers do not need to reference it."
  `(let* ((,root-var
           (uiop:ensure-directory-pathname
            (uiop:merge-pathnames*
             (format nil "cl-mcp-scaffold-test-~A/" (random #xFFFFFFFF))
             (uiop:temporary-directory)))))
     (declare (ignorable ,root-var))
     (ensure-directories-exist ,root-var)
     (unwind-protect
          (let ((cl-mcp/src/project-root:*project-root* ,root-var))
            ,@body)
       (ignore-errors (uiop:delete-directory-tree ,root-var :validate t)))))

(deftest write-scaffold-creates-all-files
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "foo-lib"
     :description "demo"
     :author "Ada"
     :license "MIT"
     :destination "scaffolds")
    (let ((target (uiop:merge-pathnames* "scaffolds/foo-lib/" root)))
      (testing "target directory exists"
        (ok (uiop:directory-exists-p target)))
      (testing "all seven files exist"
        (dolist (rel '("foo-lib.asd" "CLAUDE.md" "AGENTS.md" "README.md"
                       ".gitignore" "src/main.lisp" "tests/main-test.lisp"))
          (ok (probe-file (uiop:merge-pathnames* rel target))))))))

(deftest write-scaffold-rejects-existing-target
  (with-temp-project-root (root)
    (let ((target (uiop:merge-pathnames* "scaffolds/foo-lib/" root)))
      (ensure-directories-exist target)
      (testing "generating into existing dir errors"
        (ok (signals
             (cl-mcp/src/project-scaffold:write-scaffold
              :name "foo-lib" :description "d" :author "a" :license "MIT"
              :destination "scaffolds")
             'cl-mcp/src/project-scaffold-core:invalid-argument-error))))))

(deftest write-scaffold-validates-inputs
  (with-temp-project-root (root)
    (testing "invalid name errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "BadName" :description "d" :author "a" :license "MIT"
            :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
    (testing "newline in author errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "ok" :description "d" :author (format nil "a~%b") :license "MIT"
            :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))))

(deftest write-scaffold-no-temp-dir-on-failure
  (with-temp-project-root (root)
    (ignore-errors
     (cl-mcp/src/project-scaffold:write-scaffold
      :name "BadName" :description "d" :author "a" :license "MIT"
      :destination "scaffolds"))
    (let ((scaffolds (uiop:merge-pathnames* "scaffolds/" root)))
      (testing "no .tmp-project-scaffold-* directory remains"
        (let ((remnants
               (when (uiop:directory-exists-p scaffolds)
                 (remove-if-not
                  (lambda (p)
                    (let ((last-seg (car (last (pathname-directory p)))))
                      (and (stringp last-seg)
                           (uiop:string-prefix-p ".tmp-project-scaffold-"
                                                 last-seg))))
                  (uiop:subdirectories scaffolds)))))
          (ok (null remnants)))))))

(deftest scaffold-e2e-load-and-test
  (with-temp-project-root (root)
    ;; The scaffold's CLAUDE.md uses ../../prompts/... which, relative to
    ;; root/scaffolds/foo-lib-e2e/, resolves to root/prompts/. Create stubs
    ;; so file-based @-includes would resolve if anything ever opened them.
    (let ((prompts (uiop:ensure-directory-pathname
                    (merge-pathnames "prompts/" root))))
      (ensure-directories-exist prompts)
      (with-open-file (s (merge-pathnames "repl-driven-development.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s))
      (with-open-file (s (merge-pathnames "common-lisp-expert.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s)))
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "foo-lib-e2e"
     :description "e2e demo"
     :author "Test"
     :license "MIT"
     :destination "scaffolds")
    (let ((asd-path (merge-pathnames "scaffolds/foo-lib-e2e/foo-lib-e2e.asd" root))
          (system-name "foo-lib-e2e")
          (test-system-name "foo-lib-e2e/tests"))
      (unwind-protect
           (progn
             (testing "asd is loadable"
               (ok (asdf:load-asd asd-path)))
             (testing "system loads cleanly"
               (ok (asdf:load-system system-name)))
             (testing "bundled test system loads"
               (ok (asdf:load-system test-system-name)))
             (testing "generated main package exists with no dangling exports"
               (let ((pkg (find-package "FOO-LIB-E2E/SRC/MAIN")))
                 (ok pkg)
                 (let ((externals '()))
                   (do-external-symbols (s pkg) (push s externals))
                   (ok (null externals)
                       "scaffold main package starts with zero exports")))))
        (ignore-errors (asdf:clear-system test-system-name))
        (ignore-errors (asdf:clear-system system-name))))))

(deftest scaffold-asd-is-readable
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "readable-asd" :description "d" :author "a" :license "MIT"
     :destination "scaffolds")
    (let ((asd-path (merge-pathnames "scaffolds/readable-asd/readable-asd.asd" root)))
      (testing "both defsystem forms parse via CL reader"
        (with-open-file (in asd-path :direction :input)
          (let ((form1 (read in nil nil))
                (form2 (read in nil nil))
                (form3 (read in nil nil)))
            (ok (and (consp form1) (eq (first form1) 'asdf:defsystem)))
            (ok (and (consp form2) (eq (first form2) 'asdf:defsystem)))
            (ok (null form3))))))))

(deftest scaffold-lisp-files-have-balanced-parens
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "balanced" :description "d" :author "a" :license "MIT"
     :destination "scaffolds")
    (let ((main (merge-pathnames "scaffolds/balanced/src/main.lisp" root))
          (test (merge-pathnames "scaffolds/balanced/tests/main-test.lisp" root)))
      (testing "generated main.lisp has balanced parens"
        (ok (gethash "ok"
                     (cl-mcp/src/validate:lisp-check-parens
                      :code (alexandria:read-file-into-string main)))))
      (testing "generated main-test.lisp has balanced parens"
        (ok (gethash "ok"
                     (cl-mcp/src/validate:lisp-check-parens
                      :code (alexandria:read-file-into-string test))))))))

(deftest scaffold-tool-response-shape
  (with-temp-project-root (root)
    (declare (ignorable root))
    (let ((handler (cl-mcp/src/tools/registry:get-tool-handler "project-scaffold"))
          (args (make-hash-table :test #'equal)))
      (setf (gethash "name" args) "shape-test")
      (setf (gethash "description" args) "tool response shape test")
      (setf (gethash "author" args) "Shape")
      (setf (gethash "license" args) "MIT")
      (setf (gethash "destination" args) "scaffolds")
      (testing "project-scaffold is registered in the tool registry"
        (ok handler))
      (let* ((response (funcall handler nil 42 args))
             (result (gethash "result" response))
             (created (gethash "created" result))
             (path (gethash "path" result))
             (absolute-path (gethash "absolute_path" result))
             (files (gethash "files" result))
             (next-steps (gethash "next_steps" result)))
        (testing "response has created t"
          (ok (eq created t)))
        (testing "response has path string"
          (ok (stringp path)))
        (testing "response has absolute_path string"
          (ok (stringp absolute-path)))
        (testing "response has files vector of seven entries"
          (ok (vectorp files))
          (ok (= 7 (length files)))
          (ok (every #'stringp (coerce files 'list))))
        (testing "response has next_steps vector of four non-empty strings"
          (ok (vectorp next-steps))
          (ok (= 4 (length next-steps)))
          (ok (every (lambda (s) (and (stringp s) (plusp (length s))))
                     (coerce next-steps 'list))))
        (testing "next_steps mentions asdf:load-asd, load-system, run-tests, lisp-edit-form"
          (ok (find-if (lambda (s) (search "asdf:load-asd" s))
                       (coerce next-steps 'list)))
          (ok (find-if (lambda (s) (search "load-system" s))
                       (coerce next-steps 'list)))
          (ok (find-if (lambda (s) (search "run-tests" s))
                       (coerce next-steps 'list)))
          (ok (find-if (lambda (s) (search "lisp-edit-form" s))
                       (coerce next-steps 'list))))
        (testing "next_steps does not claim any ASDF registration happened"
          ;; project-scaffold no longer loads the generated .asd: doing so
          ;; evaluated freshly generated code in the parent process, outside
          ;; the worker isolation boundary. Nothing is registered anywhere
          ;; until the agent calls load-system, and the message must say so.
          (ok (notany (lambda (s) (search "Auto-registered with ASDF" s))
                      (coerce next-steps 'list)))
          (ok (notany (lambda (s) (search "registration happened" s))
                      (coerce next-steps 'list)))
          (ok (find-if (lambda (s)
                         (and (search "not registered with ASDF" s)
                              (search "load-system" s)
                              (search "worker" s)))
                       (coerce next-steps 'list))
              "load-system is named as the way to register in the worker"))))))

(deftest scaffold-e2e-test-system-runs-smoke-test
  (with-temp-project-root (root)
    (let ((prompts (uiop:ensure-directory-pathname
                    (merge-pathnames "prompts/" root))))
      (ensure-directories-exist prompts)
      (with-open-file (s (merge-pathnames "repl-driven-development.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s))
      (with-open-file (s (merge-pathnames "common-lisp-expert.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s)))
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "test-op-demo" :description "d" :author "a" :license "MIT"
     :destination "scaffolds")
    (let ((asd-path (merge-pathnames "scaffolds/test-op-demo/test-op-demo.asd" root)))
      (asdf:load-asd asd-path)
      (unwind-protect
           (progn
             (asdf:load-system "test-op-demo/tests")
             (let* ((system (asdf:find-system "test-op-demo/tests"))
                    (deps (asdf:system-depends-on system))
                    (test-packages
                     (remove-if-not
                      (lambda (dep)
                        (and (stringp dep)
                             (uiop:string-prefix-p "test-op-demo/tests/" dep)))
                      deps)))
               (testing "perform hook would dispatch to the child test packages"
                 (ok (member "test-op-demo/tests/main-test" test-packages
                             :test #'string=)))
               (testing "rove:run on the child packages returns T (all pass)"
                 (let ((*standard-output* (make-broadcast-stream)))
                   (ok (rove:run test-packages))))))
        (ignore-errors (asdf:clear-system "test-op-demo/tests/main-test"))
        (ignore-errors (asdf:clear-system "test-op-demo/tests"))
        (ignore-errors (asdf:clear-system "test-op-demo"))))))

(defun read-top-level-forms (text)
  "Return the list of top-level forms READ from TEXT.
Lets the .asd tests assert on structure rather than on substrings: a
quote smuggled into a template value shows up as extra top-level forms,
which a SEARCH-based check would happily miss.

*READ-EVAL* is bound to NIL because TEXT is generated from the very
injection payloads these tests feed in.  Reading it with read-time eval
enabled would let a `#.' in a payload execute here, inside the test that
exists to prove such a payload cannot execute."
  (let ((*read-eval* nil))
    (with-input-from-string (in text)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            collect form))))

(deftest escape-lisp-string-escapes-quote-and-backslash
  (testing "leaves ordinary text alone"
    (ok (equal "Ada Lovelace" (escape-lisp-string "Ada Lovelace"))))
  (testing "escapes a double quote"
    (ok (equal "a\\\"b" (escape-lisp-string "a\"b"))))
  (testing "escapes a backslash"
    (ok (equal "a\\\\b" (escape-lisp-string "a\\b"))))
  (testing "escapes a trailing backslash"
    (ok (equal "MIT\\\\" (escape-lisp-string "MIT\\"))))
  (testing "escaped output reads back as the original string"
    ;; *READ-EVAL* NIL: these inputs are injection payloads, and reading them
    ;; with read-time eval enabled would let a `#.' among them execute inside
    ;; the test that exists to prove it cannot.
    (let ((*read-eval* nil))
      (dolist (raw (list "a\"b" "a\\b" "MIT\\" "x\") (defparameter *evil* 1) (\""
                         "#.(error \"read-time eval must not fire\")"))
        (ok (equal raw
                   (read-from-string
                    (concatenate 'string "\"" (escape-lisp-string raw) "\""))))))))

(deftest scaffold-asd-neutralizes-quote-injection
  ;; PLAN-SCAFFOLD is called directly (it performs no validation) so that
  ;; this exercises the escaping layer on its own, independently of
  ;; VALIDATE-TEXT-FIELD's up-front rejection of the same characters.
  (let* ((payload
          "x\") (defparameter *injected-marker* :executed) (asdf:defsystem \"junk")
         (plan (cl-mcp/src/project-scaffold-core:plan-scaffold
                :name "inject-demo"
                :description payload
                :author "a"
                :license "MIT"
                :destination "scaffolds"))
         (asd (cdr (assoc "inject-demo.asd" plan :test #'string=)))
         (forms (read-top-level-forms asd)))
    (testing "the .asd still reads as exactly two defsystem forms"
      (ok (= 2 (length forms)))
      (ok (every (lambda (form)
                   (and (consp form) (eq (first form) 'asdf:defsystem)))
                 forms)))
    (testing "the payload round-trips as the :description string, not as code"
      (ok (equal payload (getf (cddr (first forms)) :description))))))

(deftest scaffold-asd-neutralizes-backslashes
  (let* ((description "windows path C:\\temp\\x")
         (author "Ada\\Lovelace")
         (license "MIT\\")
         (plan (cl-mcp/src/project-scaffold-core:plan-scaffold
                :name "slash-demo"
                :description description
                :author author
                :license license
                :destination "scaffolds"))
         (asd (cdr (assoc "slash-demo.asd" plan :test #'string=)))
         (forms (read-top-level-forms asd))
         (options (cddr (first forms))))
    (testing "the .asd still reads as exactly two defsystem forms"
      (ok (= 2 (length forms))))
    (testing "backslashes round-trip verbatim through read"
      ;; A trailing backslash is the nastiest case: unescaped it would eat
      ;; the closing quote of the literal and swallow the following keys.
      (ok (equal description (getf options :description)))
      (ok (equal author (getf options :author)))
      (ok (equal license (getf options :license))))))

(deftest scaffold-fiveam-asd-is-readable
  (let* ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
                :name "fa-readable" :description "d" :author "a" :license "MIT"
                :destination "scaffolds" :framework :fiveam))
         (asd (cdr (assoc "fa-readable.asd" plan :test #'string=)))
         (forms (read-top-level-forms asd)))
    (testing "the .asd reads as exactly two defsystem forms"
      (ok (= 2 (length forms)))
      (ok (every (lambda (form)
                   (and (consp form) (eq (first form) 'asdf:defsystem)))
                 forms)))
    (testing "the test system declares fiveam in :depends-on"
      (let ((deps (getf (cddr (second forms)) :depends-on)))
        (ok (member "fiveam" deps :test #'equal))
        (ok (member "fa-readable/tests/main-test" deps :test #'equal))))))

(deftest write-scaffold-rejects-quote-and-backslash-fields
  (with-temp-project-root (root)
    (testing "double quote in description errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "quoted" :description "A \"cool\" parser" :author "a"
            :license "MIT" :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
    (testing "backslash in author errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "quoted" :description "d" :author "C:\\Ada"
            :license "MIT" :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
    (testing "nothing was created for the rejected calls"
      (ok (null (uiop:directory-exists-p
                 (uiop:merge-pathnames* "scaffolds/quoted/" root)))))))

(deftest write-scaffold-writes-ownership-marker
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "marked" :description "d" :author "a" :license "MIT"
     :destination "scaffolds")
    (let* ((target (uiop:merge-pathnames* "scaffolds/marked/" root))
           (marker (uiop:merge-pathnames* *scaffold-marker-file* target)))
      (testing "the marker file exists in the generated scaffold"
        (ok (probe-file marker)))
      (testing "the marker holds a readable manifest plist"
        (let ((form (first (read-top-level-forms
                            (alexandria:read-file-into-string marker)))))
          (ok (equal "marked" (getf form :name)))
          (ok (member "marked.asd" (getf form :files) :test #'string=))
          (ok (member "src/main.lisp" (getf form :files) :test #'string=)))))))

(deftest write-scaffold-overwrite-replaces-own-scaffold
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "over-me" :description "first" :author "a" :license "MIT"
     :destination "scaffolds")
    (let ((target (uiop:merge-pathnames* "scaffolds/over-me/" root)))
      ;; A stale file proves the old tree really went away rather than
      ;; being merged into.
      (with-open-file (s (uiop:merge-pathnames* "stale.txt" target)
                         :direction :output :if-exists :supersede)
        (write-string "stale" s))
      (testing "overwriting a cl-mcp-generated scaffold succeeds"
        (ok (cl-mcp/src/project-scaffold:write-scaffold
             :name "over-me" :description "second" :author "a" :license "MIT"
             :destination "scaffolds" :overwrite t)))
      (testing "the tree was replaced, not merged"
        (ok (null (probe-file (uiop:merge-pathnames* "stale.txt" target))))
        (ok (search "second"
                    (alexandria:read-file-into-string
                     (uiop:merge-pathnames* "over-me.asd" target)))))
      (testing "no scratch directories are left behind"
        (ok (null (remove-if-not
                   (lambda (p)
                     (let ((last-seg (car (last (pathname-directory p)))))
                       (and (stringp last-seg)
                            (or (uiop:string-prefix-p ".tmp-project-scaffold-"
                                                      last-seg)
                                (uiop:string-prefix-p ".bak-project-scaffold-"
                                                      last-seg)))))
                   (uiop:subdirectories
                    (uiop:merge-pathnames* "scaffolds/" root)))))))))

(deftest write-scaffold-overwrite-refuses-foreign-directory
  (with-temp-project-root (root)
    (let* ((target (uiop:merge-pathnames* "scaffolds/precious/" root))
           (victim (uiop:merge-pathnames* "keep.txt" target)))
      (ensure-directories-exist target)
      (with-open-file (s victim :direction :output :if-exists :supersede)
        (write-string "do not delete" s))
      (testing "overwrite is refused for a directory cl-mcp did not generate"
        (ok (handler-case
                (progn
                  (cl-mcp/src/project-scaffold:write-scaffold
                   :name "precious" :description "d" :author "a" :license "MIT"
                   :destination "scaffolds" :overwrite t)
                  nil)
              (cl-mcp/src/project-scaffold-core:invalid-argument-error () t))))
      (testing "the original contents survive untouched"
        (ok (uiop:directory-exists-p target))
        (ok (probe-file victim))
        (ok (equal "do not delete" (alexandria:read-file-into-string victim)))))))

(deftest write-scaffold-refuses-existing-source-directory
  (with-temp-project-root (root)
    (let* ((src (uiop:merge-pathnames* "src/" root))
           (victim (uiop:merge-pathnames* "main.lisp" src)))
      (ensure-directories-exist src)
      (with-open-file (s victim :direction :output :if-exists :supersede)
        (write-string ";;;; precious source" s))
      (testing "name colliding with a real source dir at destination . is refused"
        (ok (handler-case
                (progn
                  (cl-mcp/src/project-scaffold:write-scaffold
                   :name "src" :description "d" :author "a" :license "MIT"
                   :destination "." :overwrite t)
                  nil)
              (cl-mcp/src/project-scaffold-core:invalid-argument-error () t))))
      (testing "the source tree survives"
        (ok (uiop:directory-exists-p src))
        (ok (probe-file victim))
        (ok (equal ";;;; precious source"
                   (alexandria:read-file-into-string victim)))))))

(deftest scaffold-tool-does-not-register-generated-system
  (with-temp-project-root (root)
    ;; Loading the generated .asd would evaluate freshly generated code in
    ;; the cl-mcp parent process, outside the worker isolation boundary.
    ;; The tool must leave this process's ASDF registry untouched.
    (let ((handler (cl-mcp/src/tools/registry:get-tool-handler "project-scaffold"))
          (args (make-hash-table :test #'equal))
          (system-name "no-autoload-demo"))
      (setf (gethash "name" args) system-name)
      (setf (gethash "destination" args) "scaffolds")
      (unwind-protect
           (let* ((response (funcall handler nil 7 args))
                  (result (gethash "result" response)))
             (testing "generation still reports success"
               (ok (eq t (gethash "created" result))))
             (testing "the generated system is not registered in this process"
               (ok (null (asdf:find-system system-name nil)))))
        (ignore-errors (asdf:clear-system system-name))))))

(defmacro when-fiveam-available (&body body)
  "Evaluate BODY only when the FiveAM system can be found, else report a skip.
FiveAM is deliberately not a declared dependency of this test system: cl-mcp's
own tests are Rove tests, and the production FiveAM backend resolves its
symbols at run time for the same reason.  A generated FiveAM project cannot be
*run* without it though, so the tests that do so are skipped rather than failed
when it is absent."
  `(if (null (asdf:find-system "fiveam" nil))
       (skip "FiveAM is not installed; a generated FiveAM project cannot run here")
       (progn
         (asdf:load-system "fiveam")
         ,@body)))

(defun fiveam-run-suite (suite-name)
  "Run SUITE-NAME through FiveAM with its progress output muted.
Every FiveAM symbol is resolved at run time so that this file compiles in an
image where FiveAM was never loaded. Only call inside WHEN-FIVEAM-AVAILABLE."
  (let ((run (uiop:find-symbol* '#:run :fiveam))
        (dribble (uiop:find-symbol* '#:*test-dribble* :fiveam)))
    (progv (list dribble) (list (make-broadcast-stream))
      (funcall run suite-name))))

(defun forget-fiveam-toplevel-suite (suite-name)
  "Drop SUITE-NAME from FiveAM's global toplevel-suite registry.
That registry outlives the temporary project a test generated, so a suite left
in it would stay a selection candidate for every later system whose name
happens to nest below it."
  (let ((var (uiop:find-symbol* '#:*toplevel-suites* :fiveam)))
    (setf (symbol-value var) (remove suite-name (symbol-value var)))))

(defun break-the-generated-smoke-test (root name)
  "Rewrite NAME's generated smoke assertion in ROOT so that it fails.
Replaces the (find-package ...) call the scaffold asserts on with a false
comparison. One replacement covers both framework templates, because only the
assertion macro wrapped around that call differs between them. Returns the
path, and signals rather than silently doing nothing when the call is absent."
  (let* ((path (merge-pathnames
                (format nil "scaffolds/~A/tests/main-test.lisp" name) root))
         (text (alexandria:read-file-into-string path))
         (broken (cl-ppcre:regex-replace "\\(find-package [^)]*\\)" text "(= 1 2)")))
    (assert (string/= text broken) ()
            "no (find-package ...) assertion to break in ~A" path)
    (alexandria:write-string-into-file broken path :if-exists :supersede)
    path))

(deftest scaffold-fiveam-e2e-suite-is-discoverable-and-green
  (when-fiveam-available
    (with-temp-project-root (root)
      (let ((prompts (uiop:ensure-directory-pathname
                      (merge-pathnames "prompts/" root))))
        (ensure-directories-exist prompts)
        (dolist (stub '("repl-driven-development.md" "common-lisp-expert.md"))
          (with-open-file (s (merge-pathnames stub prompts)
                             :direction :output :if-exists :supersede)
            (write-string "stub" s))))
      (cl-mcp/src/project-scaffold:write-scaffold
       :name "fiveam-e2e-demo" :description "d" :author "a" :license "MIT"
       :destination "scaffolds" :framework "fiveam")
      (let ((asd-path (merge-pathnames
                       "scaffolds/fiveam-e2e-demo/fiveam-e2e-demo.asd" root)))
        (unwind-protect
             (progn
               (asdf:load-asd asd-path)
               (asdf:load-system "fiveam-e2e-demo/tests")
               (testing "run-tests detects FiveAM from the generated :depends-on"
                 (ok (eq :fiveam (detect-test-framework "fiveam-e2e-demo/tests"))))
               (testing "run-tests' suite matcher finds the generated suite"
                 ;; The matcher is what decides which suites run-tests executes.
                 ;; A suite it cannot see means a run of zero tests, and zero
                 ;; tests render as a pass -- the failure mode a scaffold must
                 ;; never ship with.
                 (ok (cl-mcp/src/test-runner-core::%find-fiveam-suites-for-system
                      "fiveam-e2e-demo/tests")))
               (testing "the generated smoke test runs and passes"
                 (let ((results (fiveam-run-suite :fiveam-e2e-demo)))
                   (ok (plusp (length results))
                       "the suite ran at least one check")
                   (ok (funcall (uiop:find-symbol* '#:results-status :fiveam)
                                results)
                       "and every check passed"))))
          (forget-fiveam-toplevel-suite :fiveam-e2e-demo)
          (ignore-errors (asdf:clear-system "fiveam-e2e-demo/tests/main-test"))
          (ignore-errors (asdf:clear-system "fiveam-e2e-demo/tests"))
          (ignore-errors (asdf:clear-system "fiveam-e2e-demo")))))))

(deftest scaffold-rove-test-op-signals-when-the-suite-fails
  ;; The runner's return value is the only evidence ASDF gets that a suite
  ;; went red. Discard it and test-op completes normally, so `asdf:test-system`
  ;; -- the command the generated README tells the user to run -- reports
  ;; success over a failing suite, and so does any CI built on it.
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "rove-red" :description "d" :author "a" :license "MIT"
     :destination "scaffolds")
    (break-the-generated-smoke-test root "rove-red")
    (let ((asd-path (merge-pathnames "scaffolds/rove-red/rove-red.asd" root)))
      (unwind-protect
           (progn
             (asdf:load-asd asd-path)
             (testing "test-op signals rather than reporting success"
               (ok (handler-case
                       (let ((*standard-output* (make-broadcast-stream))
                             (*error-output* (make-broadcast-stream)))
                         ;; Own compilation unit: ASDF signals from inside one,
                         ;; and a deliberate failure here would otherwise be
                         ;; tallied into the whole suite's closing summary as
                         ;; "caught N fatal ERROR conditions".
                         (with-compilation-unit (:override t)
                           (asdf:test-system "rove-red"))
                         nil)
                     (error () t)))))
        (ignore-errors (asdf:clear-system "rove-red/tests/main-test"))
        (ignore-errors (asdf:clear-system "rove-red/tests"))
        (ignore-errors (asdf:clear-system "rove-red"))))))

(deftest scaffold-fiveam-test-op-signals-when-the-suite-fails
  ;; Same contract as the Rove case: fiveam:run! reports the failure to the
  ;; console and returns false, and a test-op that drops that value turns a red
  ;; suite into a green build.
  (when-fiveam-available
    (with-temp-project-root (root)
      (cl-mcp/src/project-scaffold:write-scaffold
       :name "fiveam-red" :description "d" :author "a" :license "MIT"
       :destination "scaffolds" :framework "fiveam")
      (break-the-generated-smoke-test root "fiveam-red")
      (let ((asd-path (merge-pathnames "scaffolds/fiveam-red/fiveam-red.asd"
                                       root)))
        (unwind-protect
             (progn
               (asdf:load-asd asd-path)
               (testing "test-op signals rather than reporting success"
                 (ok (handler-case
                         (let ((*standard-output* (make-broadcast-stream))
                               (*error-output* (make-broadcast-stream)))
                           ;; See the Rove twin for why this gets its own
                           ;; compilation unit.
                           (with-compilation-unit (:override t)
                             (asdf:test-system "fiveam-red"))
                           nil)
                       (error () t)))))
          (forget-fiveam-toplevel-suite :fiveam-red)
          (ignore-errors (asdf:clear-system "fiveam-red/tests/main-test"))
          (ignore-errors (asdf:clear-system "fiveam-red/tests"))
          (ignore-errors (asdf:clear-system "fiveam-red")))))))

(deftest scaffold-tool-accepts-framework-argument
  (with-temp-project-root (root)
    (let ((handler (cl-mcp/src/tools/registry:get-tool-handler "project-scaffold"))
          (args (make-hash-table :test #'equal)))
      (setf (gethash "name" args) "fa-tool-demo")
      (setf (gethash "destination" args) "scaffolds")
      (setf (gethash "framework" args) "fiveam")
      (let* ((response (funcall handler nil 11 args))
             (result (gethash "result" response)))
        (testing "generation succeeds and echoes the resolved framework"
          (ok (eq t (gethash "created" result)))
          (ok (equal "fiveam" (gethash "framework" result))))
        (testing "the generated .asd is a FiveAM one"
          (let ((asd (alexandria:read-file-into-string
                      (uiop:merge-pathnames*
                       "scaffolds/fa-tool-demo/fa-tool-demo.asd" root))))
            (ok (search "\"fiveam\"" asd))
            (ok (null (search "\"rove\"" asd)))))))))

(deftest scaffold-tool-rejects-unknown-framework
  (with-temp-project-root (root)
    (let ((handler (cl-mcp/src/tools/registry:get-tool-handler "project-scaffold"))
          (args (make-hash-table :test #'equal)))
      (setf (gethash "name" args) "bad-fw")
      (setf (gethash "destination" args) "scaffolds")
      (setf (gethash "framework" args) "prove")
      (let* ((response (funcall handler nil 12 args))
             (result (gethash "result" response)))
        (testing "the call is rejected with a diagnostic naming the field"
          (ok (null (gethash "created" result)))
          (ok (search "framework" (gethash "error" result))))
        (testing "and nothing is written for the rejected call"
          (ok (null (uiop:directory-exists-p
                     (uiop:merge-pathnames* "scaffolds/bad-fw/" root)))))))))
