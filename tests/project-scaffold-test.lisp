;;;; tests/project-scaffold-test.lisp

(defpackage #:cl-mcp/tests/project-scaffold-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/project-scaffold-core)
  (:import-from #:cl-mcp/src/project-scaffold)
  (:import-from #:cl-mcp/src/project-root))

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
    (testing "main-test.lisp imports the greet symbol"
      (let ((test (cdr (assoc "tests/main-test.lisp" plan :test #'string=))))
        (ok test)
        (ok (search "#:foo-lib/src/main" test))
        (ok (search "#:greet" test))))
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
    (let* ((asd-path (merge-pathnames "scaffolds/foo-lib-e2e/foo-lib-e2e.asd" root))
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
             (testing "generated greet function works"
               (ok (equal "Hello, world!"
                          (funcall (find-symbol "GREET" "FOO-LIB-E2E/SRC/MAIN")
                                   "world")))))
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
          (let* ((form1 (read in nil nil))
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
    (let* ((handler (cl-mcp/src/tools/registry:get-tool-handler "project-scaffold"))
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
                       (coerce next-steps 'list))))))))

(deftest scaffold-e2e-test-system-runs-greet-test
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
